#!/usr/bin/env bash

if [ "$(
    git status >/dev/null 2>&1
    echo $?
)" -eq 0 ]; then
    git restore src/Data/C/Integer.idr
fi
