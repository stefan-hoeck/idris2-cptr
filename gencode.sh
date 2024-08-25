#!/usr/bin/env bash

make -C codegen all
make -C support

cat >src/Data/C/Integer.idr <<EOT
module Data.C.Integer

%default total
EOT

codegen/integer_gen >>src/Data/C/Integer.idr
