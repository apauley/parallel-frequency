#!/usr/bin/env bash

.cabal-sandbox/bin/freq2 ${@} +RTS -N2 -ls -s
