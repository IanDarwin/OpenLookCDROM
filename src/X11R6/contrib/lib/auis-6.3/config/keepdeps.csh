#!/bin/csh -f
## ###################################################################### ##
##         Copyright IBM Corporation 1988,1991 - All Rights Reserved      ##
##        For full copyright information see:'andrew/config/COPYRITE'     ##
## ###################################################################### ##

echo >! tmp.dep
mv Makefile Makefile.BAK
set DEPFILES=`/bin/ls *.[cH]`
if ($#DEPFILES != 0) then
echo "## Keeping Old Dependency Information"
ed Makefile.BAK <<!  >>& /dev/null
/##### DEPENDENCY LINE - DO NOT DELETE #####/
1,.d
w tmp.dep
q
!
endif

