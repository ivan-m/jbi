#!/usr/bin/env bash

set -evuo pipefail
IFS=$'\n\t'

curl -fsSkL https://raw.github.com/cask/cask/master/go | python

export PATH=/home/travis/.cask/bin:$PATH

cd elisp && cask install
