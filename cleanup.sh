#!/usr/bin/env bash

echo "cleaning up generated code"

if [ "$(git status >/dev/null 2>&1; echo $?)" -eq 0 ]; then
    git restore src/Data/C/Integer.idr
fi

echo "cleaned up generated code"

exit 0
