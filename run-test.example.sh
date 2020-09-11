#!/usr/bin/env bash

set -e

emacs_bins=(
    ~/.local/bin/emacs
    # /Applications/Emacs-27.1.app//Contents/MacOS/Emacs
    # /Applications/Emacs-26.1.app//Contents/MacOS/Emacs
    # /Applications/Emacs-25.3.app//Contents/MacOS/Emacs
    # /Applications/Emacs-25.2.app//Contents/MacOS/Emacs
    # /Applications/Emacs-25.1.app//Contents/MacOS/Emacs
)

for emacs_bin in "${emacs_bins[@]}"; do
    export EMACS=$emacs_bin; make clean; make test $@

done

make clean
