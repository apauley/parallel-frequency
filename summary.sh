#!/usr/bin/env bash

for SCRIPT in f[0-9]*.sh
do
  echo ${SCRIPT}
  ./${SCRIPT} ${@} 2>&1 |grep 'total memory\|GC work balance\|Total\|TASKS\|SPARKS'
  echo
done
