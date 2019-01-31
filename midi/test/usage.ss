(import :drewc/midi :std/iter :gerbil/gambit)

(export test-sequence)

(def (ticks-per-quarter-note SMF-file)
  (SMF-file-division SMF-file))

(def (MTrk-event-event? pred)
  (lambda (e) (pred (MTrk-event-event e))))

(def (microseconds-per-quarter-note SMF-file)
  (set-tempo-microseconds
   (MTrk-event-event 
    (find (MTrk-event-event? set-tempo?)
         ;; A track chunk has events
         (track-chunk-events
          ;; which we ref as follows
          (SMF-file-track-ref SMF-file 0))))))

(def (delta-time-in-seconds delta-time microseconds-per-quarter-note ticks-per-quarter-note)
  (let* ((seconds-per-quarter-note (/ microseconds-per-quarter-note 1000000))
         (seconds-per-tick (/ seconds-per-quarter-note ticks-per-quarter-note)))
    (* delta-time seconds-per-tick)))

(def test-sequence-SMF-filename
  "~/.gerbil/pkg/github.com/drewc/gerbil-midi/midi/test/sultans/DIRE STRAITS.Sultans of swing K.mid")

(def (test-sequence (file test-sequence-SMF-filename))
  (def SMF-file (read-SMF-file file))
  (def mus (microseconds-per-quarter-note SMF-file))
  (def ticks (ticks-per-quarter-note SMF-file))

  (def (sleep-delta-time! event)
    (thread-sleep! (delta-time-in-seconds (MTrk-event-delta-time event) mus ticks))) 

  ;; Now, loop over the events and sleep the required time, only outputting the
  ;; lyrics.
  (displayln "Introduction Instrumental... take a while")
  (for (e (track-chunk-events (SMF-file-track-ref SMF-file 0)))
    (sleep-delta-time! e)
    (when (lyric? (MTrk-event-event e))
      (display (text-event->string (MTrk-event-event e))))))
