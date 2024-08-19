#!/usr/bin/env bash

echo "cleaning up generated code"

mv tmp/Integer.idr src/Data/C/Integer.idr
rm -r tmp

echo "cleaned up generated code"
