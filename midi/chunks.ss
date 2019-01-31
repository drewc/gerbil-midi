(import
  :drewc/midi/events
  :gerbil/gambit
  :gerbil/gambit/ports
  :scheme/base
  :std/iter
  :std/format
  :std/generic)

(export #t)

(defstruct chunk
  ;; type is four ascii characters
  (type
   ;; a 32-bit representation of a number
   length))

(def (chunk-type->string chunk)
  (bytes->string (chunk-type chunk)))

(defstruct (alien-chunk chunk)
  (bytes))

(defstruct (header-chunk chunk)
   (format number-of-tracks division)) 

(defstruct (track-chunk chunk)
  (events))

(def (track-chunk-track-name track-chunk)
  (let (e (find (lambda (e) (track-name? (MTrk-event-event e)))
                (track-chunk-events track-chunk)))
    (if e
      (text-event->string (MTrk-event-event e))
      #f)))
