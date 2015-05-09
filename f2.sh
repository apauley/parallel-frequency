#!/usr/bin/env bash

.cabal-sandbox/bin/freq2 ${@} +RTS -N4 -ls -s
