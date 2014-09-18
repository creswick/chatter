#!/bin/bash

brownzip=$1

echo ${brownzip}

excludes="brown/cats.txt brown/CONTENTS brown/README brown/ca01 brown/ca03 brown/cb02 brown/cc01 brown/cc03 brown/cd02 brown/ce01 brown/ce03 brown/cf02 brown/cg01 brown/cg03 brown/ch02 brown/cj01 brown/cj03 brown/ck02 brown/cl01 brown/cl03 brown/cm02 brown/cn01 brown/cn03 brown/cp02 brown/cr01 brown/cr03 brown/ca02 brown/cb01 brown/cb03 brown/cc02 brown/cd01 brown/cd03 brown/ce02 brown/cf01 brown/cf03 brown/cg02 brown/ch01 brown/ch03 brown/cj02 brown/ck01 brown/ck03 brown/cl02 brown/cm01 brown/cm03 brown/cn02 brown/cp01 brown/cp03 brown/cr02"

TEMP=`mktemp -d /tmp/chatter.XXXXX`

echo "Extracting corpus..."
unzip ${brownzip} -x ${excludes} -d ${TEMP}

echo "Training..."
time ./dist/build/train/train ${TEMP}/brown/c* brown-train.model

echo "Compressing brown model..."
gzip brown-train.model

rm -rf ${TEMP}
