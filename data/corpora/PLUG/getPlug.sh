#!/bin/bash

# 2002-February.txt.gz

baseURL=http://lists.pdxlinux.org/pipermail/plug/

declare -a months=(February March April May June July August September October November December January)

for year in {2002..2013}; do
  for month in ${months[@]}; do
    wget ${baseURL}${year}-${month}.txt.gz
  done
done
