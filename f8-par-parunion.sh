#!/usr/bin/env bash

THIS=$(basename ${0})
echo "${THIS}: Parallel. Try to run Map.unionWith in parallel, in addition to the parList strategy."
.cabal-sandbox/bin/freq-parunion ${@} +RTS -N4 -ls -s
