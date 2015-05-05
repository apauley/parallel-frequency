#!/bin/bash

.cabal-sandbox/bin/frequency ${@} +RTS -N2 -ls -s
