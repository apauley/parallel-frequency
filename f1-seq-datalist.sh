#!/usr/bin/env bash

THIS=$(basename ${0})
echo "${THIS}: Sequential. Frequency implementation based on list grouping/sorting."
.cabal-sandbox/bin/freq1 ${@} +RTS -ls -s
