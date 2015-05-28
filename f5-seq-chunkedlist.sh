#!/usr/bin/env bash

THIS=$(basename ${0})
echo "${THIS}: Sequential. Split list in chunks of 5000 so we can parallelise the chunks later."
.cabal-sandbox/bin/freq5 ${@} +RTS -ls -s
