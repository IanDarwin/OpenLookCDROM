#!/bin/csh -f

if (! $?METAMAIL_TMPDIR) then
    set METAMAIL_TMPDIR=/tmp
endif

cd ${METAMAIL_TMPDIR}
uudecode < $1
audiotool audio-file
rm -f audio-file $1
