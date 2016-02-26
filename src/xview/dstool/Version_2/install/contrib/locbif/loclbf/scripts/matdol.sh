:
for FILE in `/bin/ls *.F`
do
   echo $FILE
   TMPF=/tmp/$$.f
   cp $FILE $TMPF
   cat $TMPF | sed -e 's/^#if/\$if/' -e 's/^#endif/\$endif/' -e 's/^c#if/\$if/' -e 's/^c#endif/\$endif/' >$TMPF.new
   cp $TMPF.new $FILE
   rm $TMPF
done
