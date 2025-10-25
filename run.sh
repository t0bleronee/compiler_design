#!/bin/bash

# Build project from top-level Makefile
make || { echo "Build failed"; exit 1; }

# Ensure outputs directory exists
mkdir -p outputs

# Run compiler on all testcases and write full outputs; do not stop on failures
for file in testcases/*; do
    name=$(basename "$file")
    echo "=============================="
    echo "Running test: $file"
    echo "------------------------------"
    # Capture output to per-test file and also print to console
    ./compiler < "$file" | tee "outputs/${name}.txt"
    echo
done

echo "All test outputs saved under outputs/"
