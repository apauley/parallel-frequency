#!/usr/bin/env bash

for SCRIPT in f[0-9]*.sh
do
  ./${SCRIPT} ${@} 2>&1 |grep "${SCRIPT}\|total memory\|GC work balance\|Total\|TASKS\|SPARKS"
  echo
done
