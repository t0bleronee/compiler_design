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

# Note: SPIM command-line version has limitations with scanf/input
# For programs requiring input, use one of these methods:
#   1. Run manually: spim -file output.s (then type 'run' and provide input)
#   2. Use QtSPIM (GUI version) which handles input better
#   3. Pre-create input file and redirect: spim -file output.s < input.txt

# Check if stdin is a terminal (interactive) or pipe (non-interactive)
if [ -t 0 ]; then
    # Interactive mode - stdin is terminal
    echo "NOTE: For programs with scanf, after typing 'run', enter your input values"
    echo "      Press Ctrl+C to exit if program hangs"
    echo ""
    spim -file "$FILE"
else
    # Non-interactive mode - stdin is pipe or file  
    # WARNING: This may not work correctly for scanf - SPIM reads from terminal by default
    echo "WARNING: Piped input may not work with scanf in command-line SPIM"
    echo "         Consider running interactively or using QtSPIM instead"
    echo ""
    (echo "run"; cat) | spim -file "$FILE" 2>&1
fi
