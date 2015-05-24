#!/usr/bin/env bash

THIS=$(basename ${0})
echo "${THIS}: Initial attempt at coarse-grained parallelism."
echo "${THIS}: Top-level words and character counts are done in parallel."
echo "${THIS}: Same list-based algorithm as before."
.cabal-sandbox/bin/freq2 ${@} +RTS -N2 -ls -s
