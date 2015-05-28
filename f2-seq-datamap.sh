#!/usr/bin/env bash

THIS=$(basename ${0})
echo "${THIS}: Sequential. Frequency implementation based on Map.fromListWith"
.cabal-sandbox/bin/freq-datamap ${@} +RTS -ls -s
