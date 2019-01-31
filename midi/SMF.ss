(import :drewc/midi/chunks
        :drewc/midi/events
        :drewc/midi/reader

        :std/iter
        :gerbil/gambit/ports)

(export #t)

(defstruct SMF-file 
  (;; a single chunk
   header
   ;; a list of chunks
   tracks))

(def (read-SMF-file from)
  (if (port? from)
    (let ((header (read-chunk from))
          (tracks (read-chunks from)))
      (make-SMF-file header tracks))
    (call-with-input-file from read-SMF-file)))

(def (SMF-file-track-ref file n)
  (list-ref (SMF-file-tracks file) n))

(def (SMF-file-format SMF-file)
    (header-chunk-format (SMF-file-header SMF-file)))
(def (SMF-file-number-of-tracks SMF-file)
    (header-chunk-number-of-tracks (SMF-file-header SMF-file)))
(def (SMF-file-division SMF-file)
    (header-chunk-division (SMF-file-header SMF-file)))

(def (SMF-file-tempo-map SMF-file)
  (case (SMF-file-format SMF-file)
    ((0 1) 
     (for/collect ((e (track-chunk-events (SMF-file-track-ref SMF-file 0))
                      when (let ((ee (MTrk-event-event e)))
                             (or (set-tempo? ee) (time-signature? ee)))))
       e))))

(def (SMF-file-sequence-name SMF-file)
  (track-chunk-track-name (SMF-file-track-ref SMF-file 0)))
