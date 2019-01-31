(import
      :gerbil/gambit
      :gerbil/gambit/ports
      :scheme/base
      :std/iter
      (phi: +1 :std/iter :std/format)
      :std/format
      :std/generic)
(export #t)

(defstruct MTrk-event
  (delta-time event))

(defstruct event 
 (status-byte))

(defstruct (invalid-event event)
 (bytes))

(defstruct (sysex-event event)
 (bytes))

(def (sysex-event-length sysex-event)
  (u8vector-length bytes))

(defstruct (midi-event event) ())

(def (midi-event-status midi-event)
  (bitwise-and (event-status-byte midi-event) #xF0))

(def (midi-event-channel midi-event)
  (bitwise-and (event-status-byte midi-event) #x0F))

(defstruct (note-off midi-event)
  (number velocity))
(defstruct (note-on midi-event)
  (number velocity))
(defstruct (key-aftertouch midi-event)
  (note velocity))
(defstruct (control-change midi-event)
  (controller value))
(defstruct (program-change midi-event)
  (number))
(defstruct (channel-aftertouch midi-event)
  (number))
(defstruct (pitchwheel-change midi-event)
  (bottom top))

(defstruct (meta-event event)
  (type bytes))

(def (meta-event-length meta-event)
  (u8vector-length (meta-event-bytes meta-event)))


;; This is simple.
;; (def-event-bytes foo bar baz) => (def (foo-bar foo) (u8vector-ref foo 0))
;; Same thing for set!

(defsyntax (def-meta-event-bytes stx)
  (def (%def-event-bytes name args)
  (cons 'begin  
        (for/fold (d []) (n (in-range (length args)))
          (cons* `(def (,(string->symbol (format "~A-~A" name (list-ref args n))) ,name)
                    (u8vector-ref (meta-event-bytes ,name) ,n))
                 `(def (,(string->symbol (format "~A-~A-set!" name (list-ref args n))) ,name val)
                    (u8vector-set! (meta-event-bytes ,name) ,n val))
                 d))))
  (syntax-case stx ()
    ((macro name . args)
     (let* ((n (syntax->datum #'name))
            (bs (syntax->datum #'args))
            (new-beginning (%def-event-bytes n bs)))
       (with-syntax ((begin-event (datum->syntax #'macro new-beginning)))
         #'begin-event))))) 

(defstruct (unknown-meta-event meta-event) ())

(defstruct (text-event meta-event) ())

(def (text-event->string meta-event)
  (bytes->string (meta-event-bytes meta-event)))

(def text-events
    [[01 . "Text"]
     [02 . "Copyright Notice"]
     [03 . "Sequence/Track Name"]
     [04 . "Instrument Name"]
     [05 . "Lyric"]
     [06 . "Marker"]
     [07 . "Cue Point"]])

(def (text-event-description text-event)
  (let ((string (assoc (meta-event-type text-event) text-events eqv?)))
    (if string (cdr string) (number->string (meta-event-type text-event)))))

(defstruct (unknown-text-event text-event) ())

(def (construct-text-event FF type bytes)
 ((case type
    ((#x01) make-text-event)
    ((#x03) make-track-name)
    ((#x05) make-lyric)
    (else make-unknown-text-event))
  FF type bytes))

(defstruct (track-name text-event) ())

(defstruct (time-signature meta-event) ())

(def-meta-event-bytes time-signature
  numerator denominator pulses-per-quarter 32nd-per-click)

(def (time-signature-beats time-signature)
  (time-signature-numerator time-signature))

(def (time-signature-bars time-signature)
  (denominator (expt 2 (* (time-signature-denominator time-signature) -1))))

(defstruct (key-signature meta-event) ())

(def-meta-event-bytes key-signature
  displacement MI)

(def key-signature-info
  '((7 "7 sharps" "C#" "A#")
    (6 "6 sharps" "F#" "D#")
    (5 "5 sharps" "B" "G#")
    (4 "4 sharps" "E" "C#")
    (3 "3 sharps" "A" "F#")
    (2 "2 sharps" "D" "B")
    (1 "1 sharp" "G" "E")
    (0 "" "C" "A")
    (-1 "1 flat" "F" "D")
    (-2 "2 flats" "Bb" "G")
    (-3 "3 flats" "Eb" "C")
    (-4 "4 flats" "Ab" "F")
    (-5 "5 flats" "Db" "Bb")
    (-6 "6 flats" "Gb" "Eb")
    (-7 "7 flats" "Cb" "Ab")))

(def (key-signature-notation ks)
  (let (i (assoc (key-signature-displacement ks) key-signature-info eqv?))
    (cadr i)))

(def (key-signature-note ks)
  (let (i (assoc (key-signature-displacement ks) key-signature-info eqv?))
    ((if (= (key-signature-MI ks) 0)
       caddr
       cadddr) i)))

(def (key-signature-type ks)
  (if (= (key-signature-MI ks) 0)
    "Major"
    "Minor"))


(defstruct (set-tempo meta-event) ())

(def (set-tempo-microseconds meta-event)
  (def (big-endian->integer list)
    (let loop ((list list)
               (result 0)
               (shift (* 8 (- (length list) 1))))
      (if (null? list)
        result
        (loop (cdr list)
              (bitwise-ior result (arithmetic-shift (car list) shift))
              (- shift 8)))))
  (big-endian->integer
   (u8vector->list (meta-event-bytes meta-event))))

(defstruct (lyric text-event) ())

(defstruct (end-of-track meta-event) ())

(defstruct (sequencer-specific meta-event) ())
