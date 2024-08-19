#!/usr/bin/env bash

make -C codegen all

mkdir -p tmp
cp src/Data/C/Integer.idr tmp/

codegen/integer_gen >>src/Data/C/Integer.idr
