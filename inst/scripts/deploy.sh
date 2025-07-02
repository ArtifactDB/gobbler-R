#!/bin/sh

# We shovel this into a shell script as system(2) doesn't like multiple
# statements, especially if one of them is backgrounded. We need to do
# multiple statements so that we can run `$!` to get the PID.
"$@" > /dev/null &
PID=$!
echo $PID
