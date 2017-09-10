#!/usr/bin/env bash

set -evuo pipefail
IFS=$'\n\t'

# Here starts the actual work to be performed for the package under test;
# any command which exits with a non-zero exit code causes the build to fail.

if [ -f configure.ac ]; then autoreconf -i; fi
cabal configure --enable-tests --enable-benchmarks -v2  # -v2 provides useful information for debugging
cabal build   # this builds all libraries and executables (including tests/benchmarks)
cabal test
cabal check
cabal haddock # tests that documentation can be generated
cabal sdist   # tests that a source-distribution can be generated

# Check that the resulting source distribution can be built & installed.
# If there are no other `.tar.gz` files in `dist`, this can be even simpler:
# `cabal install --force-reinstalls dist/*-*.tar.gz`
SRC_TGZ=$(cabal info . | awk '{print $2;exit}').tar.gz &&
    (cd dist && cabal install --force-reinstalls "$SRC_TGZ")
