#!/usr/bin/env bash

THIS=$(basename ${0})
echo "${THIS}: Parallel. Use parMap which creates a spark for each list element."
.cabal-sandbox/bin/freq-parmap ${@} +RTS -N3 -ls -s