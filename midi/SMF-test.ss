(import :drewc/midi/reader 
        :drewc/midi/chunks
        :drewc/midi/events
        :drewc/midi/SMF
        :std/error
        :gerbil/gambit/os
        :gerbil/gambit
        :std/iter 
        :std/format
        :std/sugar
        :std/generic)
(export #t)
(defrules test> ()
  ((_ form => value format-args ...)
   (let ((expected value)
         (f 'form)
         (evaled form)
         (args (list format-args ...)))
     (if (equal? expected evaled)
       evaled
       (error (apply format (if (null? args)
                              (list "Test Failed ~A gave ~A,  != ~A" form evaled expected)
                              (if (null? (cdr args))
                                (list (car args) form evaled expected)
                                args))))))))

(def test-path "~/.gerbil/pkg/github.com/drewc/gerbil-midi/test/")


(def (find-test-midi-files (path test-path) (ext ".mid"))
  (def (%fdir type f)
    (let (t (file-info-type (file-info f)))
      (if (equal? t type)
        f
        #f)))
  (def paths (map (cut path-expand <> path) (directory-files path)))
  (def files (filter (cut %fdir 'regular <>) paths))
  (def directories (filter (cut %fdir 'directory <>) paths))

  (apply append (filter (lambda (file) (equal? ext (path-extension file))) files)
         (map find-test-midi-files directories)))



(def global-test-file "/home/user/src/gerbil-midi/test/sultans/SultansOfSwing7.mid")

(def (print-header-chunk chunk)
  (printf "Header Format ~A, ~A tracks, ~A division"
          (header-chunk-format chunk)
          (header-chunk-number-of-tracks chunk)
          (header-chunk-division chunk)))


(def (test-header-chunk (file global-test-file))
  (print-header-chunk (test-read-header-chunk file))
  (test-read-first-file-chunk file)
  (test-second-chunk-is-track))

(def (test-read-header-chunk (file global-test-file))
  "=> header-chunk"
  (call-with-input-file file 
    (lambda (p)
    (let* ((type (read-chunk-type p))
           (length (read-chunk-length p))
           (header (read-header-chunk type length p))
           (format (header-chunk-format header)))
      (begin0 header
        (test> type => #u8(77 84 104 100))
        (test> length => 6)
        (test> (case format
                 ((0 1 2) #t)
                 (else #f))
               => #t "Header Format is ~A, not 0, 1 or 2" format))))))
(def (test-read-first-file-chunk (file global-test-file))
  "=> header-chunk"
  (let (chunk (call-with-input-file file read-chunk))
        (begin0 chunk 
          (test> (header-chunk? chunk) => #t)  
          (test> (chunk-type->string chunk) => "MThd")
          (test> (chunk-length chunk) => 6)
          (test> (case (header-chunk-format chunk)
                   ((0 1 2) #t)
                   (else #f))
                 => #t
                 "Header Format is ~A, not 0, 1 or 2" (header-chunk-format chunk)))))
(def (test-second-chunk-is-track (file global-test-file))
  "=> \"MTrk\" "
  (call-with-input-file file
    (lambda (p)
      (read-chunk p)
      (let (label 
             (bytes->string
              (list->u8vector (for/collect ((_ (in-range 4)))
                                (read-u8 p)))))
        (begin0 label
          (test> label => "MTrk"))))))

(def (print-event event (nl #t))
  (begin0 event
    (try
     {print event}
     (catch (e) (display-exception e)))
    (when (midi-event? event) (set! nl #t))
    (when nl (printf "~%"))))

(def (print-MTrk-event e)
 (printf "T:~A " (MTrk-event-delta-time e))
 (print-event (MTrk-event-event e) #f))
(defmethod {print MTrk-event}
  (lambda (self) (print-MTrk-event self)))
(def (print-unknown-meta-event e)
  (printf "Meta? #x~X length ~A"
          (meta-event-type e)
          (meta-event-length e)))

(defmethod {print unknown-meta-event} (lambda (e) (print-unknown-meta-event e)))
(def (print-time-signature self)
  (printf "Time Signature: ~A/~A ~A MIDI clocks per dotted-quarter, ~A notated 32nd-notes per quarter-note.~%"

            (time-signature-beats self)
            (time-signature-bars self)
            (time-signature-pulses-per-quarter self)
            (time-signature-32nd-per-click self)))

(defmethod {print time-signature}
  (lambda (self) (print-time-signature self)))
(def (print-key-signature self)
  (printf "Key Signature: ~A ~A"
          (key-signature-note self)
          (key-signature-type self)))

(defmethod {print key-signature}
  (lambda (self) (print-key-signature self)))
(def (print-set-tempo self)
  (printf "Set Tempo: ~1,1Fbpm ~Amu (microseconds) per quarter-note." 
          (/ 60000000 (set-tempo-microseconds self))
          (set-tempo-microseconds self)))

(defmethod {print set-tempo}
  (lambda (self) (print-set-tempo self)))

(def (print-sequencer-specific self)
  (printf "Sequencer Specific Length ~A"
          (meta-event-length self)))

(defmethod {print sequencer-specific}
  (lambda (self) (print-sequencer-specific self)))
(def (print-end-of-track e)
  (printf "End of Track"))

(defmethod {print end-of-track} (cut print-end-of-track <>))

(def (print-text-event e)
  (printf "~A ~A: ~A"
          (type-of e)
          (text-event-description e) (text-event->string e)))

(defmethod {print text-event}
  (lambda (e) (print-text-event e)))
(defmethod {print unknown-text-event}
  (lambda (e) (printf "Unknown: ~X"
                 (meta-event-type e))
     (print-text-event e)))

(def (print-track-name e)
  (printf "Track Name: ~A" (text-event->string e)))

(defmethod {print track-name} (cut print-track-name <>))
  (def (print-control-change self)
    (printf "Control Change: ~A ~A~%"
            (control-change-controller self)
            (control-change-value self)))
  (defmethod {print control-change}
      (lambda (self) (print-control-change self)))


(defmethod {print midi-event}
    (lambda (self) 
      (printf "Channel ~A " (midi-event-channel self))))

(defmethod {print note-off}
  (lambda (self)
    (@next-method self)
    (printf "Note OFF ~A velocity ~A"
            (note-off-number self)
            (note-off-velocity self))))


(defmethod {print note-on}
  (lambda (self) 
    (@next-method self)
    (printf "Note ON ~A velocity ~A"
            (note-on-number self)
            (note-on-velocity self))))


(defmethod {print program-change}
  (lambda (self) (printf "Patch (Program) Change: ~A" 
                    (program-change-number self))))



(defmethod {print channel-aftertouch}
  (lambda (self) (printf "Channel After Touch ~A"
                    (channel-aftertouch-number self))))



(defmethod {print pitchwheel-change}
  (lambda (self) (printf "pitchwheel change bottom ~A top ~A"
                    (pitchwheel-change-bottom self)
                    (pitchwheel-change-top self))))

(def (test-read-chunks (file global-test-file))
    "=> track-chunk"
    (call-with-input-file file
      (lambda (p)
        (printf "Testing File ~A " file)
        ;; header
        (print-header-chunk (read-chunk p))
        ;; first track
        (read-chunks p))))

(def (test-SMF-file (file global-test-file) (print? #t))
  (def SMF-file (read-SMF-file file))
  (def SMF-header (SMF-file-header SMF-file))

  (def (e? test e)
    (test (MTrk-event-event e)))

  ;;; We should have the same number of tracks as we were given.
  (test> (length (SMF-file-tracks SMF-file))
         => (header-chunk-number-of-tracks SMF-header))

  ;;; I'd love to see an alien chunk in our massive collection!
  (for ((t (SMF-file-tracks SMF-file)))
    (test> (not (alien-chunk? t)) => #t))


  (begin0 SMF-file
    (printf "Midi File ~A, format ~A, ~A tracks, Sequence name ~A~%"
            (path-strip-directory file)
            (SMF-file-format SMF-file)
            (SMF-file-number-of-tracks SMF-file)
            (SMF-file-sequence-name SMF-file))
    (for (n (in-range (SMF-file-number-of-tracks SMF-file)))
      (let* ((track (SMF-file-track-ref SMF-file n))
             (meta (filter (cut e? meta-event? <>)
                           (track-chunk-events track)))
             (sysex (filter (cut e? sysex-event? <>)
                            (track-chunk-events track)))
             (midi (filter (cut e? midi-event? <>)
                           (track-chunk-events track)))
             (invalid (filter (cut e? invalid-event? <>)
                              (track-chunk-events track))))
        (test> (null? invalid) =? #t
               "Invalid Events ~A" invalid)


        (when print? (printf "--- Track ~A : ~A ----~%"
                             n (track-chunk-track-name track))
              (printf "~A Midi Events," (length midi))
              (printf "~A Sysex Events~%" (length sysex))
              (map print-event meta))))))


(def global-test-sequence-SMF-file "~/src/gerbil-midi/test/sultans/DIRE STRAITS.Sultans of swing K.mid")

(def (delta-time-in-seconds delta-time microseconds-per-quarter-note ticks-per-quarter-note)
  (let* ((seconds-per-quarter-note (/ microseconds-per-quarter-note 1000000))
         (seconds-per-tick (/ seconds-per-quarter-note ticks-per-quarter-note)))
    (* delta-time seconds-per-tick)))

(def (test-sequence (file global-test-sequence-SMF-file))
  (def SMF-file (test-SMF-file file #f))
  (def (e? test e) (test (MTrk-event-event e)))
  (def track0 (SMF-file-track-ref SMF-file 0))
  (def events (track-chunk-events track0))
  (def timesig (MTrk-event-event (find (cut e? time-signature? <>) events)))
  (def set-tempo (MTrk-event-event (find (cut e? set-tempo? <>) events)))

  (for ((l events))
    (let (wait (delta-time-in-seconds
                (MTrk-event-delta-time l)
                (set-tempo-microseconds set-tempo)
                (header-chunk-division (SMF-file-header SMF-file))))
      (thread-sleep! wait)
      (if (lyric? (MTrk-event-event l))
        (printf "~1,5F: ~A~%" wait
                (text-event->string (MTrk-event-event l)))

        (printf ".")))))
