#!/bin/bash

# intercept ctrl-c and cleanup before exiting

function cleanup () {
    echo "Hold on a minute..."
    sleep 3
    echo "Ok, all done."
    # if we don't exit here, we'll just keep looping
    exit 0
}

trap cleanup SIGINT

while true ; do
    echo "tick"
    sleep 1
done

