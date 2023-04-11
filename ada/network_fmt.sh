#!/bin/bash

# Set default values for host and port
HOST="localhost"
PORT=10573

# Check if host and port were specified as command line arguments
if [[ $# -ge 1 ]]; then
    HOST="$1"
fi
if [[ $# -ge 2 ]]; then
    PORT="$2"
fi

# Set block size to 2KiB - 3
BLOCK_SIZE=2045

# Define function to send message over UDP
send_message() {
    # Send message over UDP
    printf '>'$(printf '\\x%02x\\x%02x' $((counter >> 8)) $((counter & 0xff)))"%s" "$1" | nc -q0 -u "$HOST" "$PORT"

    # Increment counter
    ((counter++))
}

# Read input data and split into blocks or lines
counter=0
while read -r -n "$BLOCK_SIZE" -u 0 data || [[ -n $data ]]; do
   send_message "$data"
done

# Close file descriptor 0
exec 0<&- 