#!/bin/bash
# Monitor V3 power analysis progress

LOG_FILE="/home/zjelveh/Dropbox/research/base_rate_fallacy/repo/staggeredpower/examples/run_v3.log"
DB_FILE="/home/zjelveh/Dropbox/research/base_rate_fallacy/repo/staggeredpower/examples/power_analysis_results_V3.sqlite"

echo "=== V3 Power Analysis Progress ==="
echo

# Check if running
if pgrep -f "run_power_analysis_v3.R" > /dev/null; then
    echo "Status: RUNNING"
else
    echo "Status: NOT RUNNING or COMPLETED"
fi
echo

# Runtime
if [ -f "$LOG_FILE" ]; then
    START_TIME=$(head -20 "$LOG_FILE" | grep "Starting Power Grid" -A 1 | tail -1 | awk '{print $1, $2}')
    if [ ! -z "$START_TIME" ]; then
        echo "Started: $START_TIME"
        ELAPSED=$(($(date +%s) - $(date -d "$START_TIME" +%s 2>/dev/null || echo 0)))
        if [ $ELAPSED -gt 0 ]; then
            HOURS=$((ELAPSED / 3600))
            MINS=$(((ELAPSED % 3600) / 60))
            echo "Elapsed: ${HOURS}h ${MINS}m"
        fi
    fi
    echo
fi

# Latest log entries
if [ -f "$LOG_FILE" ]; then
    echo "Latest progress:"
    tail -10 "$LOG_FILE" | grep -E "Processing outcome|Running spec|complete"
    echo
fi

# Database size and rows (if exists)
if [ -f "$DB_FILE" ]; then
    echo "Database stats:"
    echo "  Size: $(du -h $DB_FILE | cut -f1)"

    ROWS=$(sqlite3 "$DB_FILE" "SELECT COUNT(*) FROM final_power" 2>/dev/null)
    if [ ! -z "$ROWS" ]; then
        echo "  Rows in final_power: $ROWS"
        EXPECTED=308000
        PCT=$((ROWS * 100 / EXPECTED))
        echo "  Progress: ${PCT}% (${ROWS}/${EXPECTED})"
    fi
    echo
fi

# Estimated completion
if [ -f "$LOG_FILE" ] && [ -f "$DB_FILE" ]; then
    ROWS=$(sqlite3 "$DB_FILE" "SELECT COUNT(*) FROM final_power" 2>/dev/null || echo 0)
    if [ $ROWS -gt 1000 ] && [ $ELAPSED -gt 0 ]; then
        RATE=$((ROWS / ELAPSED))
        REMAINING=$((308000 - ROWS))
        ETA_SECS=$((REMAINING / RATE))
        ETA_HOURS=$((ETA_SECS / 3600))
        echo "Estimated time remaining: ${ETA_HOURS} hours"
    fi
fi
