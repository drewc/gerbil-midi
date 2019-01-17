(import
  :gerbil/gambit
  :gerbil/gambit/ports
  :scheme/base
  :std/iter
  :std/format
  :std/generic)

(export #t)
(defstruct midi-file 
 (header-chunk track-chunks))

(def (read-variable-length-quantity p)
    (let loop ((result 0))
      (let (byte (read-u8 p))
        (if (eof-object? byte)
          result
          (let (result (bitwise-ior
            (arithmetic-shift result 7)
                        (##fxand byte #x7f)))
            (if (##fx< byte #x80)
              result
              (loop result)))))))


(def (big-endian->integer list)
  (let loop ((list list)
             (result 0)
             (shift (* 8 (- (length list) 1))))
    (if (null? list)
      result
      (loop (cdr list)
            (bitwise-ior result (arithmetic-shift (car list) shift))
            (- shift 8)))))

(def (read-big-endian-integer bytes port)
  (let loop ((bytes bytes)
             (result '()))
    (if (= bytes 0)
      (big-endian->integer (reverse result))
      (loop (- bytes 1)
            (cons (read-u8 port) result)))))


(defstruct chunk
  ;; type is four ascii characters
  (type
   ;; a 32-bit representation of a number
   length))

(def (read-chunk-length port)
 (read-big-endian-integer 4 port))

(def (read-chunk-type port)
  (let loop ((bytes 4)
             (result '()))
    (if (= bytes 0)
      (list->string (map integer->char (reverse result)))
      (loop (- bytes 1)
            (cons (read-u8 port) result)))))

(def (read-chunk p)
  (let* ((type (read-chunk-type p))
         (length (read-chunk-length p)))
    (if (equal? "MTrk" type)
      (read-track-chunk type length p)
      (let ((bytes (make-u8vector length)))
        (read-u8vector bytes p)
        (make-alien-chunk type length bytes)))))

(def (read-chunks p)
  (let ((chunk (read-chunk p))
        (next (peek-u8 p)))
    (if (eof-object? next)
      [chunk]
      (cons chunk (read-chunks p)))))

(defstruct (alien-chunk chunk)
  (bytes))

(defstruct (header-chunk chunk)
   (format number-of-tracks division)) 
(def (read-header-chunk p)
  (let* ((type (read-chunk-type p)) 
         (length (read-chunk-length p))
         (format (read-big-endian-integer 2 p))
         (number-of-tracks (read-big-endian-integer 2 p))
         (division (read-big-endian-integer 2 p)))
    (make-header-chunk type length format number-of-tracks division)))


(defstruct (track-chunk chunk)
  (events))

(def (read-track-chunk type length p)
  (let ((events (read-events p)))
    (make-track-chunk type length events)))


(defstruct track-event
  (delta-time))

(defmethod {print track-event}
  (lambda (self)
    (printf "Event Time: ~A"
            (track-event-delta-time self))))


(def (read-delta-time p)
  (read-variable-length-quantity p))
(def (status-byte? byte)
  (<= #x80 byte))

(defstruct (unknown-track-event track-event)
 (status bytes))

(def (read-unknown-track-event time status p)
  (def (%read-bytes)
    (let ((next (peek-u8 p)))
      (if (and (not (eof-object? next))
               (not (status-byte? next)))
        (let ((byte (read-u8 p)))
          (cons byte (%read-bytes)))
        [])))
  (let ((bytes (%read-bytes)))
    (make-unknown-track-event time status (list->u8vector (%read-bytes)))))


(defmethod {print unknown-track-event}
  (lambda (self)
    (printf "Unknown ~X ~A ~%"
            (unknown-track-event-status self)
            (unknown-track-event-bytes self))))

(define (between-or-=? x y z)
  (and (<=  x y)
       (<= y z)))

(def (read-event p (delta-time #f))
  (let ((delta-time (or delta-time (read-delta-time p)))
        (status (peek-u8 p)))
    ;;(printf "event status? ~X ~%" status )
    (cond ((equal? status #xff)
           (read-meta-event delta-time p))
          ((between-or-=? #x00 status #xEF)
           (read-midi-event delta-time p))
          (else 
           (read-unknown-track-event delta-time status p)))))

(def (read-events p (delta-time #f))
  (let* ((event (read-event p))
         (next-unknown-delta (if (unknown-track-event? event)
                               (track-event-delta-time event)
                               #f)))
    (if (end-of-track-meta-event? event)
      [event]
      (cons event (read-events p next-unknown-delta)))))


(def current-running-status
  (make-parameter #f))

(defstruct (midi-event track-event)
  (status))

(def (midi-event-channel midi-event)
  (bitwise-and (midi-event-status midi-event) #b00001111))

(defmethod {print midi-event}
  (lambda (self) 
    (printf "Channel ~A " (midi-event-channel self))))

(def (read-midi-event delta-time p)
  (let* ((status (peek-u8 p))
         (status
          (begin (when (>= status #x80)
                   (current-running-status (read-u8 p)))
                 (current-running-status))))
    (cond ((between-or-=? #x80 status #x8F)
            (let* ((n (read-u8 p))
                   (v (read-u8 p)))
              (make-note-off delta-time status n v)))
          ((between-or-=? #x90 status #x9F)
            (let* ((n (read-u8 p))
                   (v (read-u8 p)))
              (make-note-on delta-time status n v)))
          ((between-or-=? #xA0 status #xAF)
           (let* ((n (read-u8 p))
                  (v (read-u8 p)))
             (make-key-aftertouch delta-time status n v)))
          ((between-or-=? #xB0 status #xBF)
           (let* ((n (read-u8 p))
                  (v (read-u8 p)))
             (make-control-change delta-time status n v)))
          ((between-or-=? #xC0 status #xCF)
           (let* ((pp (read-u8 p)))
             (make-patch-change delta-time status pp)))
           ((between-or-=? #xD0 status #xDF)
            (let* ((cc (read-u8 p)))
              (make-channel-aftertouch delta-time status cc)))
            ((between-or-=? #xE0 status #xEF)
             (let* ((bb (read-u8 p))
                    (tt (read-u8 p)))
              (make-pitch-wheel-change delta-time status bb tt)))
          (else (error (format "Not a known midi event: #x~X" status))))))

(defstruct (note-off midi-event)
  (number velocity))

(defmethod {print note-off}
  (lambda (self)
    (@next-method self)
    (printf "Note OFF ~A velocity ~A~%"
            (note-off-number self)
            (note-off-velocity self))))

(defstruct (note-on midi-event)
   (number velocity))
(defmethod {print note-on}
  (lambda (self) (printf "Note ON ~A velocity ~A~%"
                    (note-on-number self)
                    (note-on-velocity self))))
(defstruct (key-aftertouch midi-event)
  (note velocity))

(defstruct (control-change midi-event)
  (controller value))

(defmethod {print control-change}
  (lambda (self) (printf "Control Change: ~A ~A~%"
                    (control-change-controller self)
                    (control-change-value self))))

(defstruct (patch-change midi-event)
  (number))

(defmethod {print patch-change}
  (lambda (self) (printf "Patch (Program) Change: ~A ~%"
                    (patch-change-number self))))

(defstruct (channel-aftertouch midi-event)
  (number))

(defmethod {print channel-aftertouch}
  (lambda (self) (printf "Channel After Touch ~A ~%"
                    (channel-aftertouch-number self))))

(defstruct (pitch-wheel-change midi-event)
  (bottom top))

(defmethod {print pitch-wheel-change}
  (lambda (self) (printf "Channel After Touch bottom ~A top ~A ~%"
                    (pitch-wheel-change-bottom self)
                    (pitch-wheel-change-top self))))




(defstruct (meta-event track-event)
  (type length))

(defmethod {print meta-event}
  (lambda (self) (printf "Meta Event: type ~X length ~A type ~A "
                    (meta-event-type self)
                    (meta-event-length self)
                    (type-of self))))

(defstruct (unknown-meta-event meta-event)
  (bytes))

(defmethod {print unknown-meta-event}
  (lambda (self) (printf "Unknown Meta Event: ~X ~A ~A~%"
                    (meta-event-type self)
                    (meta-event-length self)
                    (unknown-meta-event-bytes self))))

(def meta-event-constructors [])

(def (find-meta-event-constructor delta-time type length bytes)
  ;;(printf "Lookibgf cons type ~A ~A ~%" type length)
  (def make (assoc type meta-event-constructors eqv?))
  (if make
    (cdr make)
    make-unknown-meta-event))

(def (read-meta-event delta-time port)
  (let* ((type (begin
                 ;; throwaway #xFF
                 (read-u8 port)
                 (read-u8 port)))
         (length (read-variable-length-quantity port))
         (bytes (make-u8vector length)))
    (read-u8vector bytes port)
    ((find-meta-event-constructor delta-time type length bytes)
     delta-time type length bytes)))



 (defstruct (text-meta-event meta-event)
  (text))

(def (construct-text-meta-event time type length bytes)
  (make-text-meta-event time type length (bytes->string bytes)))

(let lp ((i #x01))
  (set! meta-event-constructors
    (cons (cons i construct-text-meta-event)
          meta-event-constructors))
 (when (< i #x0f)
     (lp (+ 1 i))))
  (def text-meta-events
   [[01 . "Text Event"]
    [02 . "Copyright Notice"]
    [03 . "Sequence/Track Name"]
    [04 . "Instrument Name"]
    [05 . "Lyric"]
    [06 . "Marker"]
    [07 . "Cue Point"]])

(def (text-meta-event-type text-meta-event)
  (let ((string (assoc (meta-event-type text-meta-event) text-meta-events eqv?)))
    (if string (cdr string) "")))

(defmethod {print text-meta-event}
    (lambda (self)
;     (@next-method self)
      (printf "Time: ~A Text ~A: ~A~%"
              (track-event-delta-time self)
              (text-meta-event-type self)
              (text-meta-event-text self))))

(defstruct (end-of-track-meta-event meta-event) ())

(def (construct-end-of-track-meta-event time type length bytes)
  (printf "End of track. Time: ~A type ~X length ~A bytes ~A"
          time type length bytes)
   (make-end-of-track-meta-event time type length))

(set! meta-event-constructors
    (cons (cons #x2F construct-end-of-track-meta-event) meta-event-constructors))

(defmethod {print end-of-track-meta-event}
    (lambda (self)
      (printf "End of Track~%")))

(defstruct (set-tempo-meta-event meta-event)
  (time))

(def (construct-set-tempo-meta-event time type length bytes)
  (make-set-tempo-meta-event time type length (big-endian->integer
                                               (u8vector->list bytes))))

(set! meta-event-constructors
  (cons (cons #x51 construct-set-tempo-meta-event) meta-event-constructors))

(defmethod {print set-tempo-meta-event}
  (lambda (self)
    (printf "Set Tempo: ~1,1Fbpm ~Amu (microseconds) per quarter-note ~%" 
            (/ 60000000 (set-tempo-meta-event-time self))
            (set-tempo-meta-event-time self))))
 

(defstruct (time-signature-meta-event meta-event)
  (numerator denominator clocks-click 32nd-per-click))

(def (construct-time-signature-meta-event time type length bytes)
  (make-time-signature-meta-event time type length 
   (u8vector-ref bytes 0)(u8vector-ref bytes 1)(u8vector-ref bytes 2)(u8vector-ref bytes 3)))

(set! meta-event-constructors 
  (cons (cons #x58 construct-time-signature-meta-event) meta-event-constructors))

(defmethod {print time-signature-meta-event}
  (lambda (self) 
    (printf "Time Signature: ~A/~A ~A MIDI clocks per dotted-quarter, ~A notated 32nd-notes per quarter-note.~%"

        (time-signature-meta-event-numerator self)
        (denominator (expt 2 (* (time-signature-meta-event-denominator self) -1)))
        (time-signature-meta-event-clocks-click self)
        (time-signature-meta-event-32nd-per-click self))))
