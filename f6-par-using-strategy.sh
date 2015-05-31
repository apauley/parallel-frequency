#!/usr/bin/env bash

THIS=$(basename ${0})
echo "${THIS}: Parallel. Use the parList strategy instead of a custom parMap."
.cabal-sandbox/bin/freq-using ${@} +RTS -N4 -ls -s
