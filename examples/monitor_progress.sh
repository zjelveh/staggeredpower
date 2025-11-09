#!/bin/bash
# Monitor test_strangulation.R progress

while true; do
    clear
    echo "=== Power Analysis Progress Monitor ==="
    echo ""
    echo "Last 20 lines of output:"
    echo "------------------------"
    tail -20 test_output.log
    echo ""
    echo "------------------------"
    echo "Press Ctrl+C to exit"
    sleep 10
done
