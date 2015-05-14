#!/usr/bin/env bash

for SCRIPT in f[0-9]*.sh
do
  echo ${SCRIPT}
  ./${SCRIPT} ${@} 2>&1 |grep 'Total\|TASKS\|SPARKS'
  echo
done
