#!/bin/sh
#
# Convert a HBF font file to a BDF font file
#
# Should be able to run on most Unix system.
# Use mostly awk, bc, sed, od, dd.
#
# <<< For HBF version 1.0 >>>
# 
# Author:	Yongguang Zhang  (ygz@cs.purdue.edu)
# Create date:	Apr/25/93
# Last update:	Apr/30/93


# usage
usage() {
  (
    echo "Usage: $0 hbf-filename bdf-filename"
    echo "      please check the input hbf file by hbf-check.sh first"
  ) 1>&2
}


# get command line arguments
#
if [ $# -ne 2 ]; then
    usage; exit;
fi
HBFFILE=$1
BDFFILE=$2
me=`basename $0`

# check the arguments
#
if [ ! -r $HBFFILE ]; then
    echo "unable to open hbf-file \"$HBFFILE\"." 1>&2
    exit 1
fi
if [ "`dd if=$HBFFILE bs=1 count=14 2>/dev/null`" != "HBF_START_FONT" ]; then
    echo "File \"$HBFFILE\" doesn't seem to be a HBF file."  1>&2
    echo "Its very first line should start with \"HBF_START_FONT\"."  1>&2
    exit 1
fi

if cat /dev/null > $BDFFILE ; then
    : OK, succeeded in initializing the .bdf file.
else
    exit 1
fi


TMPFILE1=/tmp/hbftobdf.$$.1
TMPFILE2=/tmp/hbftobdf.$$.2
trap "rm -f - $TMPFILE1 $TMPFILE2; exit 1" 1 2 3 15

################################################
# Round 1:  output the bdf header and font name
################################################

# function todec:  turn hex/oct/dec into dec
todec () {
    if [ $# -eq 1 ]; then
	echo $1
    else
	head -1
    fi | awk '
	BEGIN {	c["0"] = 0; c["1"] = 1; c["2"] = 2; c["3"] = 3; c["4"] = 4;
		c["5"] = 5; c["6"] = 6; c["7"] = 7; c["8"] = 8; c["9"] = 9;
		c["a"] =10; c["b"] =11; c["c"] =12;
		c["d"] =13; c["e"] =14; c["f"] =15;
		c["A"] =10; c["B"] =11; c["C"] =12;
		c["D"] =13; c["E"] =14; c["F"] =15;
		s["+"] = 1; c["-"] =-1;
	}
	/^0[xX][0-9a-fA-F][0-9a-fA-F]*$/	{	# hex
		    h = 0;
		    for (i = 3; i <= length($1); i++)
			h = h*16 + c[substr($1,i,1)];
		    print h; exit 0
		}
	/^[+-]0[xX][0-9a-fA-F][0-9a-fA-F]*$/	{	# hex
		    h = 0;
		    for (i = 4; i <= length($1); i++)
			h = h*16 + c[substr($1,i,1)];
		    print h*s[substr($1,1,1)]; exit 0
		}
	/^0[0-7][0-7]*$/	{			# oct
		    h = 0;
		    for (i = 2; i <= length($1); i++)
			h = h*8 + substr($1,i,1);
		    print h; exit 0
		}
	/^[+-]0[0-7][0-7]*$/	{			# oct
		    h = 0;
		    for (i = 3; i <= length($1); i++)
			h = h*8 + substr($1,i,1);
		    print h*s[substr($1,1,1)]; exit 0
		}
	/^[1-9][0-9]*$/		{ print $1; exit 0 }	# dec
	/^-[1-9][0-9]*$/	{ print $1; exit 0 }	# dec
	/^+[1-9][0-9]*$/	{			# dec
		    print substr($1,2,length($1)-1); exit 0
		}
		{   print 0; exit 0  }			# dec
    '
}

# defaults
WEIGHT_NAME="Medium"
SLANT="R"
SETWIDTH_NAME="Normal"
ADD_STYLE_NAME=""
SPACING="C"
PIXEL_SIZE=""
POINT_SIZE=""
RESOLUTION_X=72
RESOLUTION_Y=72
AVERAGE_WIDTH=""
FONT_ASCENT=""
FONT_DESCENT=""
CHARSET_REGISTRY=""
CHARSET_ENCODING=""
SIZE=""

sec="hdr"

# (tr -d '\015')  is to remove the ^M, in case it is a MS-DOS file
#
cat $HBFFILE | tr -d '\015' | \
while read keyword arg; do

  case "$keyword" in

    HBF_START_FONT )
	if [ "$arg" != "1.0" ]; then
	    echo "Sorry.  This program only understands HBF version 1.0"  1>&2
	    exit 1
	else
	    echo "output BDF header ..." 1>&2
	    echo "STARTFONT 2.1" >> $BDFFILE
	fi
	;;

    HBF_CODE_SCHEME )
	encode=`echo "$arg" | awk '{print $1}'`
	;;

    FONT )
	hbf_fontname=$arg
	;;

    SIZE )
	SIZE="$arg"
	PIXEL_SIZE=`echo "$arg" | awk '{print $1}' | todec`
	RESOLUTION_X=`echo "$arg" | awk '{print $2}' | todec`
	RESOLUTION_Y=`echo "$arg" | awk '{print $3}' | todec`
	;;

    HBF_BITMAP_BOUNDING_BOX )
	w=`echo "$arg" | awk '{print $1}' | todec`
	h=`echo "$arg" | awk '{print $2}' | todec`
	xd=`echo "$arg" | awk '{print $3}' | todec`
	yd=`echo "$arg" | awk '{print $4}' | todec`
	;;

    FONTBOUNDINGBOX )
	wF=`echo "$arg" | awk '{print $1}' | todec`
	hF=`echo "$arg" | awk '{print $2}' | todec`
	xdF=`echo "$arg" | awk '{print $3}' | todec`
	ydF=`echo "$arg" | awk '{print $4}' | todec`
	;;

    STARTPROPERTIES )
	sec="prop"
	n_prop=0
	cat /dev/null > $TMPFILE1
	;;

    FOUNDRY )
	FOUNDRY=`echo $arg | sed 's/^"//;s/"$//;s/""/"/g'`
	;;

    FAMILY_NAME )
	FAMILY_NAME=`echo $arg | sed 's/^"//;s/"$//;s/""/"/g'`
	;;

    WEIGHT_NAME )
	WEIGHT_NAME=`echo $arg | sed 's/^"//;s/"$//;s/""/"/g'`
	;;

    SLANT )
	SLANT=`echo $arg | sed 's/^"//;s/"$//;s/""/"/g'`
	;;

    SETWIDTH_NAME )
	SETWIDTH_NAME=`echo $arg | sed 's/^"//;s/"$//;s/""/"/g'`
	;;

    ADD_STYLE_NAME )
	ADD_STYLE_NAME=`echo $arg | sed 's/^"//;s/"$//;s/""/"/g'`
	if echo "$encode" | egrep -s -i '^GB2312' ; then
	    if echo "$ADD_STYLE_NAME" | egrep -s -i '^[js]' ; then
		ADD_STYLE_NAME=""
	    fi
	elif echo "$encode" | egrep -s -i '^Big5' ; then
	    if echo "$ADD_STYLE_NAME" | egrep -s -i '^[ft]' ; then
		ADD_STYLE_NAME=""
	    fi
	fi
	;;

    PIXEL_SIZE )
	PIXEL_SIZE=`todec $arg`
	;;

    POINT_SIZE )
	POINT_SIZE=`todec $arg`
	;;

    RESOLUTION_X )
	RESOLUTION_X=`todec $arg`
	;;

    RESOLUTION_Y )
	RESOLUTION_Y=`todec $arg`
	;;

    SPACING )
	SPACING=`echo $arg | sed 's/^"//;s/"$//;s/""/"/g'`
	;;

    AVERAGE_WIDTH )
	AVERAGE_WIDTH=`todec $arg`
	;;

    CHARSET_REGISTRY )
	CHARSET_REGISTRY=`echo $arg | sed 's/^"//;s/"$//;s/""/"/g'`
	;;

    CHARSET_ENCODING )
	CHARSET_ENCODING=`echo $arg | sed 's/^"//;s/"$//;s/""/"/g'`
	;;

    FONT_ASCENT )
	FONT_ASCENT=`todec $arg`
	;;

    FONT_DESCENT )
	FONT_DESCENT=`todec $arg`
	;;

    DEFAULT_CHAR )
	DEFAULT_CHAR=`todec $arg`
	;;

    ENDPROPERTIES )
	if [ "$POINT_SIZE" = "" ]; then
	    POINT_SIZE=`expr $hF '*' 10`	# height of the font
	fi
	if [ "$PIXEL_SIZE" = "" ]; then
	    PIXEL_SIZE=`echo "$RESOLUTION_Y" "$POINT_SIZE" | \
		    awk '{ printf "%d\n", ($1 * $2 / 722.7 + 0.5) }'`
	fi
	if [ "$AVERAGE_WIDTH" = "" ]; then
	    AVERAGE_WIDTH=`expr "$w" '*' 10`
	fi
	if [ "$FONT_ASCENT" = "" ]; then
	    FONT_ASCENT=`expr "$hF" + "$ydF"`
	fi
	if [ "$FONT_DESCENT" = "" ]; then
	    FONT_DESCENT=`expr 0 - "$ydF"`
	fi
	if [ "$CHARSET_REGISTRY" = "" ]; then
	    CHARSET_REGISTRY=`echo "$encode" | tr '-' '.'`
	fi
	if [ "$CHARSET_ENCODING" = "" ]; then
	    CHARSET_ENCODING=0
	    if echo "$CHARSET_REGISTRY" | grep -s -i '^GB' ; then
		if [ "$DEFAULT_CHAR" -ge 32768 ]; then
		    CHARSET_ENCODING=1
		fi
	    fi
	fi
	if [ "$SIZE" = "" ]; then
	    SIZE="$PIXEL_SIZE $RESOLUTION_X $RESOLUTION_Y"
	fi

	# now, ouput the header

	(
	  echo "$FOUNDRY" ;
	  echo "$FAMILY_NAME";
	  echo "$WEIGHT_NAME";
	  echo "$SLANT";
	  echo "$SETWIDTH_NAME";
	  echo "$ADD_STYLE_NAME";
	  echo "$PIXEL_SIZE";
	  echo "$POINT_SIZE";
	  echo "$RESOLUTION_X";
	  echo "$RESOLUTION_Y";
	  echo "$SPACING";
	  echo "$AVERAGE_WIDTH";
	  echo "$CHARSET_REGISTRY";
	  echo "$CHARSET_ENCODING";
	) | awk 'BEGIN	{ printf "FONT " }
		 	{ printf "-%s", $0 }
		 END	{ printf "\n" }'  >> $BDFFILE

	echo "COMMENT  converted by $me from $HBFFILE"  >> $BDFFILE
	echo "COMMENT  HBF font name: $hbf_fontname"  >> $BDFFILE

	echo "SIZE $SIZE"  >> $BDFFILE
	echo "FONTBOUNDINGBOX $wF $hF $xdF $ydF"  >> $BDFFILE
	echo "STARTPROPERTIES `expr $n_prop + 17`"  >> $BDFFILE
	echo "FOUNDRY \"$FOUNDRY\""  >> $BDFFILE
	echo "FAMILY_NAME \"$FAMILY_NAME\""  >> $BDFFILE
	echo "WEIGHT_NAME \"$WEIGHT_NAME\""  >> $BDFFILE
	echo "SLANT \"$SLANT\""  >> $BDFFILE
	echo "SETWIDTH_NAME \"$SETWIDTH_NAME\""  >> $BDFFILE
	echo "ADD_STYLE_NAME \"$ADD_STYLE_NAME\""  >> $BDFFILE
	echo "PIXEL_SIZE $PIXEL_SIZE"  >> $BDFFILE
	echo "POINT_SIZE $POINT_SIZE"  >> $BDFFILE
	echo "RESOLUTION_X $RESOLUTION_X"  >> $BDFFILE
	echo "RESOLUTION_Y $RESOLUTION_Y"  >> $BDFFILE
	echo "SPACING \"$SPACING\""  >> $BDFFILE
	echo "AVERAGE_WIDTH $AVERAGE_WIDTH"  >> $BDFFILE
	echo "CHARSET_REGISTRY \"$CHARSET_REGISTRY\""  >> $BDFFILE
	echo "CHARSET_ENCODING \"$CHARSET_ENCODING\""  >> $BDFFILE
	echo "FONT_ASCENT $FONT_ASCENT"  >> $BDFFILE
	echo "FONT_DESCENT $FONT_DESCENT"  >> $BDFFILE
	echo "DEFAULT_CHAR $DEFAULT_CHAR"  >> $BDFFILE
	cat $TMPFILE1  >> $BDFFILE
	echo "ENDPROPERTIES"  >> $BDFFILE

	rm -f - "$TMPFILE1"
	sec="range"
	;;

    COMMENT )
	if [ "$sec" = "prop" ]; then
	    echo "$keyword $arg"  >> $TMPFILE1
	fi
	;;

    CHARS )
	echo "CHARS `todec $arg`"  >> $BDFFILE
	# calculate per character information
	echo "$AVERAGE_WIDTH $POINT_SIZE $RESOLUTION_X $wF" | awk '{
		printf "SWIDTH %d 0\n", $1 * 1000 * 72 / $2 / $3;
		printf "DWIDTH %d 0\n", $4;
	    }'  > $TMPFILE2
	echo "BBX $w $h $xd $yd"  >> $TMPFILE2
	;;

    HBF_CODE_RANGE )
	bf=`echo "$arg" | awk '{ print $(NF-1) }'`
	if [ ! -r "$bf" ]; then
	    echo "Cannot access the bitmap file \"$bf\"."  1>&2
	    echo "Only the BDF header is generated."   1>&2
	    rm -f - "$TMPFILE2"
	    exit 1
	fi
	;;

    * )
	if [ "$sec" = "prop" ]; then
	    n_prop=`expr $n_prop + 1`
	    if echo "$arg" | egrep -s '^"' ; then
		echo "$keyword $arg"  >> $TMPFILE1
	    else
		# the parameter should be in decimal in BDF
		echo "$keyword `todec $arg`"  >> $TMPFILE1
	    fi
	fi
	;;

  esac
done

if [ $? -ne 0 ]; then
    rm -f - "$TMPFILE2"
    exit 1
fi

#############################################
# Round 2: output the bitmap for each glyph.
#############################################

b2_ranges=""
num_b2_ranges=0

cat $HBFFILE | tr -d '\015' | \
while read keyword a1 a2 a3 a4 a5 ; do

  case "$keyword" in

    HBF_START_FONT )
	echo "output BDF bitmaps ... (will take a long time)" 1>&2
	swidth=`awk '/SWIDTH/ {print}' $TMPFILE2`
	dwidth=`awk '/DWIDTH/ {print}' $TMPFILE2`
	bbx=`awk '/BBX/ {print}' $TMPFILE2`
	rm -f - "$TMPFILE2"
	;;

    HBF_BITMAP_BOUNDING_BOX )
	w=`todec $a1` ;  h=`todec $a2` 
	bpl=`echo "scale=0; ($w + 7)/8" | bc`
	;;

    HBF_BYTE_2_RANGE )
	rs=`echo ${a1}${a2}${a3} | sed 's/-.*$//' | todec`
	re=`echo ${a1}${a2}${a3} | sed 's/^.*-//' | todec`
	b2_ranges="$b2_ranges  $rs $re"
	num_b2_ranges=`expr $num_b2_ranges + 1`
	;;

    HBF_CODE_RANGE )
	if [ "$a4" = "" ]; then
	    cs=`echo ${a1} | sed 's/-.*$//' | todec`
	    ce=`echo ${a1} | sed 's/^.*-//' | todec`
	    F=$a2;  co=`todec $a3`
	elif [ "$a5" = "" ]; then
	    cs=`echo ${a1}${a2} | sed 's/-.*$//' | todec`
	    ce=`echo ${a1}${a2} | sed 's/^.*-//' | todec`
	    F=$a3;  co=`todec $a4`
	else
	    cs=`echo ${a1}${a2}${a3} | sed 's/-.*$//' | todec`
	    ce=`echo ${a1}${a2}${a3} | sed 's/^.*-//' | todec`
	    F=$a4;  co=`todec $a5`
	fi

	echo "generate range $cs-$ce from file \"$F\", offset = $co ..." 1>&2

	if [ "$co" -eq 0 ]; then
	    od -bvw"$bpl" "$F"
	else
	    dd if="$F" bs=$co skip=1  2>/dev/null | od -bvw"$bpl"
	fi | awk '
	    BEGIN  {
		code = '"$cs"' ; cend = '"$ce"' ; h = '"$h"'
		split("'"$b2_ranges"'", range);
		for (i = 0; i < 256; i++)  a[i] = 0;
		for (j = 0; j < '"$num_b2_ranges"'; j++)
		    for (i = range[j*2+1]; i <= range[j*2+2]; i++)
			a[i] = 1
		d=0
		for (b1 = 0; b1 <= 7; b1++)
		    for (b2 = 0; b2 <= 7; b2++)
			for (b3 = 0; b3 <= 7; b3++) {
			    o2h[ b1 b2 b3 ] = sprintf("%02x", d);
			    d++
			}
	    }
	    NR % h == 1  {
		while (1) {
		    if (code > cend)  exit;		# done 
		    if (a[code % 256] == 1) {
			printf "STARTCHAR %04x\n", code
			printf "ENCODE %d\n", code
			print "'"$swidth"'"
			print "'"$dwidth"'"
			print "'"$bbx"'"
			print "BITMAP"
			break;
		    } else
			code++
		}
	    }
	    {
		s=""
		for (i = 2; i <= NF; i++)  s = s o2h[$i];
		print s
	    }
	    NR % h == 0  {
		print "ENDCHAR"
		code++;
	    }
	'  >> $BDFFILE

	;;

    HBF_END_FONT )
	echo "ENDFONT"  >> $BDFFILE
	;;

  esac

done
