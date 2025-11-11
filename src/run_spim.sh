#!/bin/bash
# Run MIPS assembly with SPIM simulator

if [ -z "$1" ]; then
    FILE="output.s"
else
    FILE="$1"
fi

if [ ! -f "$FILE" ]; then
    echo "Error: File $FILE not found"
    exit 1
fi

echo "=========================================="
echo "Running $FILE with SPIM"
echo "=========================================="
echo ""

# Run SPIM non-interactively
echo "run" | spim -file "$FILE" 2>&1
