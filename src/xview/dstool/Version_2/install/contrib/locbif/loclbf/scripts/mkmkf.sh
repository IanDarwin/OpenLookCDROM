:
SCRIPTS_DIR=`dirname $0`
LIBRARY=`pwd`
LIBRARY=`basename $LIBRARY`
echo "Library name = $LIBRARY.a"

MAKEFILE=makefile
cp /dev/null $MAKEFILE
DATE=`date`

( echo "# Created at $DATE"
echo "FFLAGS=-O -cg92"
echo -n  "OBJECTS=" ) >>$MAKEFILE

for F_FILE in `/bin/ls *.F`
do
   FILE=`basename $F_FILE .F`
   echo -n "$FILE.o "
done >>$MAKEFILE

( echo; echo; echo "LIBRARY=$LIBRARY.a"
echo; cat $SCRIPTS_DIR/make.arch ) >> $MAKEFILE
