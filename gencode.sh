#!/usr/bin/env bash

make -C codegen all
make -C support

cat >src/Data/C/Integer.idr <<EOT
-- Note: This module is automatically generated when Idris builds
-- the library and the constants will be replaced with values
-- matching the system this is generated on.
--
-- The placeholders are here so that it works with tools like the LSP
-- without first compiling the library. They were generated on an x86_64
-- GNU/Linux system with GCC. If you are on a similar system, your numbers
-- might very well be identical.
module Data.C.Integer

%default total
EOT

codegen/integer_gen >>src/Data/C/Integer.idr
