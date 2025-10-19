#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

# Build project
echo "Building compiler..."
make

# Prepare output directories
mkdir -p test_outputs/ir test_outputs/logs

echo "Running tests and generating TAC outputs..."

for file in testcases/*; do
    [ -f "$file" ] || continue
    
    base=$(basename "$file")
    
    # Skip non-source files
    case "$base" in
        *.out|*.3ac|*.log|*.o|*.exe) continue;;
    esac
    
    echo "=============================="
    echo "Test: $base"
    echo "------------------------------"

    # Run compiler and capture ALL output
    ./compiler "$file" > "test_outputs/logs/${base}.log" 2>&1 || true

    # STRATEGY 1: If compiler produces output.3ac file, use that
    if [ -s output.3ac ]; then
        cp output.3ac "test_outputs/ir/${base}.3ac"
        echo "✓ Generated IR from output.3ac"
    
    # STRATEGY 2: Otherwise, extract TAC from log output
    else
        # Extract TAC section (adjust pattern based on your TAC format)
        awk '
            /^main:/ || /^[A-Z_]+:/ || /^[[:space:]]*[%t][0-9]+[[:space:]]*=/ { 
                if (!started) { started=1 } 
                print 
            }
            started && /^[[:space:]]*$/ { print }  # Keep empty lines within TAC
        ' "test_outputs/logs/${base}.log" > "test_outputs/ir/${base}.3ac"
        
        if [ -s "test_outputs/ir/${base}.3ac" ]; then
            echo "✓ Extracted IR from compiler output"
        else
            echo "✗ No IR generated"
            rm -f "test_outputs/ir/${base}.3ac"
        fi
    fi
    
    # Clean up
    rm -f output.3ac 2>/dev/null || true
done

echo "=============================="
echo "Done! Outputs in test_outputs/"
echo "  - IR files: test_outputs/ir/"
echo "  - Full logs: test_outputs/logs/"
