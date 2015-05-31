#!/usr/bin/env bash

THIS=$(basename ${0})
echo "${THIS}: Sequential. Split file into chunks so we can parallelise the chunks later."
echo "${THIS}: Speedup mostly due to using Data.ByteString and Data.Text for chunking instead of lists."
.cabal-sandbox/bin/freq-chunkedlist ${@} +RTS -ls -s
