#!/bin/sh

# this runs a demo of KL Group's demo vmgraph.

if [ -x /bin/sun ] && sun; then
	${CDROMBASE}/demos/klg/vmgraph
else
	echo "Sorry, this demo is only available on SunOS"
fi
