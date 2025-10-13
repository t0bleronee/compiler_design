#!/bin/bash

set -e

# Build project from top-level Makefile
make

# Run compiler on all testcases
for file in testcases/*; do
    echo "=============================="
    echo "Running test: $file"
    echo "------------------------------"
    ./compiler < "$file"
    echo -e "\n"
done
