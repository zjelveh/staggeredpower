#!/bin/bash
# Quick progress checker for the test run

echo "=== Test Progress Check ==="
echo ""
echo "Last 15 lines of output:"
tail -15 examples/test_output.log 2>/dev/null || echo "No log file yet"
echo ""
echo "Database size:"
ls -lh examples/*.sqlite 2>/dev/null || echo "No database yet"
echo ""
echo "Process status:"
ps aux | grep "[R]script examples/test_strangulation.R" && echo "✓ Test is running" || echo "✗ Test not running"
