#!/bin/bash
# Quick compile and run script

if [ -z "$1" ]; then
    echo "Usage: ./quick_run.sh <source_file.cpp>"
    exit 1
fi

echo "Compiling $1..."
./compiler "$1" 2>&1

# Check if output.s was generated (ignore segfault if it was)
if [ ! -f "output.s" ]; then
    echo "Error: Compilation failed - output.s not generated"
    exit 1
fi

echo ""
echo "✅ Assembly generated successfully"
echo "Running with SPIM..."
./run_spim.sh
