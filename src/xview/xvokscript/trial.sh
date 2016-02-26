#!/bin/sh

# This script runs a program for you.

# create a dummy script to run
echo 'date>/dev/console' > /tmp/trial$$

# Now run it. No slash to work locally or from TOUR
xvokscript /tmp/trial$$
