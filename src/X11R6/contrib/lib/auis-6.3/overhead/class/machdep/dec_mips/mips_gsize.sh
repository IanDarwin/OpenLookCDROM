#!/bin/sh
## ###################################################################### ##
##         Copyright IBM Corporation 1988,1991 - All Rights Reserved      ##
##        For full copyright information see:'andrew/config/COPYRITE'     ##
## ###################################################################### ##

size -A $1 | awk '/rdata|sdata|sbss|lit8|lit4/ { sum += $2 }; END {print 65536 - sum}'
