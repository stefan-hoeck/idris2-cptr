#!/usr/bin/env bash

make -C codegen all

codegen/integer_gen >>src/Data/C/Integer.idr
