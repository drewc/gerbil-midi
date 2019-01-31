#!/usr/bin/env gxi

(import :std/build-script
        :std/make)

(defbuild-script
  `("midi/chunks" "midi/events" "midi/reader" "midi/SMF" "midi" "midi/SMF-test"
    "midi/test/usage"))
