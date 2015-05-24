#!/usr/bin/env bash

THIS=$(basename ${0})
echo "${THIS}: Parallel. Split list in half recursively, with each half being run in parallel."
.cabal-sandbox/bin/freq4 ${@} +RTS -N2 -ls -s
