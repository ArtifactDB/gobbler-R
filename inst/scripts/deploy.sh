#!/bin/sh

# We shovel this into a shell script as system(2) doesn't like multiple
# statements, especially if one of them is backgrounded. We need to do
# multiple statements so that we can run `$!` to get the PID.
$1 -staging $2 -registry $3 -admin $4 > /dev/null &
PID=$!

sleep 1 # Give it a second to set up before it's considered 'ready'.
echo $PID
