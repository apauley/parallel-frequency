#!/usr/bin/env bash

THIS=$(basename ${0})
echo "${THIS}: Sequential. Frequency implementation based on Map.fromListWith"
.cabal-sandbox/bin/freq3 ${@} +RTS -ls -s
