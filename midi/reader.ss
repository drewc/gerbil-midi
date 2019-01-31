(import
    :gerbil/gambit
    :gerbil/gambit/ports
    :scheme/base
    :std/iter
    :std/format
    :std/generic
    :drewc/midi/chunks
    :drewc/midi/events)

(export read-chunk-length
        read-chunk-type
        read-header-chunk
        read-chunk
        read-chunks
        read-MTrk-event
        read-meta-event)

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


(def (read-chunk-type port)
  (let ((type (make-u8vector 4)))
    (begin0 type
      (read-u8vector type port))))

(def (read-chunk-length port)
   (read-big-endian-integer 4 port))

(def (read-chunk p)
  (let* ((type (read-chunk-type p))
         (length (read-chunk-length p)))
    ((cond
      ;; "MTrk" => #u8(77 84 114 107)
      ((equal? #u8(77 84 114 107) type) read-track-chunk)
      ;; "MTrk" => #u8(77 84 114 107)
     ((equal? #u8(77 84 104 100) type) read-header-chunk)
     (else read-alien-chunk))
     type length p)))


(def (read-chunks p)
  (let ((chunk (read-chunk p))
        (next (peek-u8 p)))
    (if (eof-object? next)
      [chunk]
      (cons chunk (read-chunks p)))))

(def (read-alien-chunk type length port)
  (let ((bytes (make-u8vector length)))
    (read-u8vector bytes port)
    (make-alien-chunk type length bytes)))

(def (read-header-chunk type length p)
  (let* ((format (read-big-endian-integer 2 p))
         (number-of-tracks (read-big-endian-integer 2 p))
         (division (read-big-endian-integer 2 p)))
    ;; Also, more parameters may be added to the MThd chunk in the future: it is
    ;; important to read and honour the length, even if it is longer than 6.
    ;; We'll throw any extras away.
    (when (> length 6)
      (for ((x (in-range (- length 6))))
        (read-u8 p)))
    (make-header-chunk type length format number-of-tracks division)))


(def (read-track-chunk type length p)
  (let (events (read-MTrk-events p))
    (make-track-chunk type length events)))

(def (read-delta-time p)
  (read-variable-length-quantity p))


;; HACK: If there's ever an invalid-event in our midi file, which hopefully
;; never occurs but during hacking does until I make it valid, we cannot tell
;; the delta-time.

;; So, we'll pass one over, and keep on reading.

(def (read-MTrk-event p (delta-time #f))
 (let ((delta-time (or delta-time (read-delta-time p)))
       (event (read-event p)))
   (make-MTrk-event delta-time event)))

(def (read-MTrk-events p (delta-time #f))
  (def (%read delta-time)
    (let* ((MTrk-event (read-MTrk-event p delta-time))
           (event (MTrk-event-event MTrk-event)))
      (if (end-of-track? event)
        [MTrk-event]
        (cons MTrk-event
              (%read (if (invalid-event? event)
                         (MTrk-event-delta-time event)
                         #f))))))
  (parameterize ((current-running-status (current-running-status)))
    (%read delta-time)))

(def (status-byte? byte)
  (<= #x80 byte))

(def current-running-status
  (make-parameter #f))
(def (new-current-running-status! byte)
  (current-running-status
   (if (midi-status-byte? byte) byte #f)))



(def (read-event p)
  (let* ((potential-status (peek-u8 p))
         (status-byte
          ;;If the first (status) byte is less than 128 (hex 80), this implies
          ;;that running status is in effect, and that this byte is actually the
          ;;first data byte (the status carrying over from the previous MIDI
          ;;event).
          (if (not (status-byte? potential-status))
            (current-running-status)
            ;; This can only be the case if the immediately previous event was
            ;; also a MIDI event, i.e. SysEx and Meta events interrupt (clear)
            ;; running status.
            (let ((byte (read-u8 p)))
              (begin0 byte
                (new-current-running-status! byte))))))
    (cond 
     ;; First midi
     ((midi-status-byte? status-byte)

      (read-midi-event status-byte p))
     ;; Now META
     ((= status-byte #xff)
      (read-meta-event status-byte p))
     ;; Sysex
     ((or (= status-byte #xF0)
          (= status-byte #xF7))
      (read-sysex-event status-byte p))
     ;; Else
     (else 
      (read-invalid-event status-byte p)))))

(def (read-invalid-event status p)
    (def (%read-bytes)
      (let ((next (peek-u8 p)))
        (if (and (not (eof-object? next))
                 (not (status-byte? next)))
          (let ((byte (read-u8 p)))
            (cons byte (%read-bytes)))
          [])))
    (let ((bytes (%read-bytes)))
      (make-invalid-event status (list->u8vector (%read-bytes)))))

(def (midi-status-byte? byte)
  (and (<= #x80 byte) (<= byte #xEF)))

(def (read-midi-event status-byte p)
  (def (two-bytes maker)
    (let* ((one (read-u8 p)) (two (read-u8 p)))
      (maker status-byte one two)))
  (case  (bitwise-and status-byte #xF0)
    ;; | 8x    | 1000xxxx | /nn vv/ | Note off (key is released)      
    ((#x80) (two-bytes make-note-off))
    ;; | 9x    | 1001xxxx | /nn vv/ | Note on (key is pressed)   
    ((#x90) (two-bytes make-note-on))
    ;; | Ax    | 1010xxxx | /nn vv/ | Key after-touch
    ((#xA0) (two-bytes make-key-aftertouch))
    ;; | Bx    | 1011xxxx | /cc vv/ | Control Change
    ((#xB0) (two-bytes make-control-change))
    ;; | Cx    | 1100xxxx | /pp/    | Program (patch) change
    ((#xC0) (make-program-change status-byte (read-u8 p)))
    ;; | Dx    | 1101xxxx | /cc/    | Channel after-touch
    ((#xD0) (make-channel-aftertouch status-byte (read-u8 p)))
    ;; | Ex    | 1110xxxx | /bb tt/ | Pitch wheel change (2000H is normal or no change)
    ((#xE0) (two-bytes make-pitchwheel-change))
    ;; ELSE no soup for you!
    (else (error (format "Not a known midi event: #x~X" status-byte)))))

(def (read-meta-event status-byte p)
  (let* ((type (read-u8 p))
         (length (read-variable-length-quantity p))
         (bytes (make-u8vector length)))
    (read-u8vector bytes p)
    ((case type 
       ;;Meta-event types 01 through 0F are reserved for various types of text
       ;;events, each of which meets the specification of text events:

       ;; #xFF type length text

       ((#x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08
              #x09 #x0A #x0B #x0C #x0D #x0E #x0F)
        construct-text-event)
       ((#x7F) make-sequencer-specific)
       ((#x2f) make-end-of-track)
       ((#x51) make-set-tempo)
       ((#x58) make-time-signature)
       ((#x59) make-key-signature)
     (else make-unknown-meta-event)) 
     status-byte type bytes)))


(def (read-sysex-event status-byte p)
  (let* ((length (read-variable-length-quantity p))
         (bytes (make-u8vector length)))
    (read-u8vector bytes p)
    (make-sysex-event status-byte bytes)))
