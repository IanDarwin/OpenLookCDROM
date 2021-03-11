#!/bin/sh
#
# Extract the bitmap of a hanzi code from an HBF font.
#
# Given an .hbf file and a hanzi internal code as arguments,
# this program outputs the bitmap of the glyph on screen.
# Options can be used to change the pixel characters of the bitmap, or
# to produce a file in EPS, X11 bitmap, or PBM portable bitmap format.
#
# Type "hbf-show.sh -help" for usage.
#
# Should be able to run on most Unix system.
# Use mostly awk, bc, sed, od, dd.
#
# <<< For HBF version 1.0 >>>
# 
# Author:	Yongguang Zhang  (ygz@cs.purdue.edu)
# Create date:	Apr/25/93
# Last update:	Jun/25/93


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
	}
	/^0[xX][0-9a-fA-F][0-9a-fA-F]*$/	{	# hex
		    h = 0;
		    for (i = 3; i <= length($1); i++)
			h = h*16 + c[substr($1,i,1)];
		    print h; exit 0
		}
	/^0[0-7][0-7]*$/	{			# oct
		    h = 0;
		    for (i = 2; i <= length($1); i++)
			h = h*8 + substr($1,i,1);
		    print h; exit 0
		}
	/^[1-9][0-9]*$/		{ print $1; exit 0 }	# dec
	/^0$/			{ print 0; exit 0 }	# dec
		{   print 0; exit 1  }
    '
}

# number of valid codes from 0x00 to (byte2)
offset_byte2 () {
    echo $* | awk '{
	b2=$1; offset = 0;
	for (i=2;i<=NF;i+=2) { 
	    if ($i <= b2 && b2 <= $(i+1))
		offset += b2 - $i;
	    else if ($(i+1) < b2)
		offset += $(i+1)-$i+1;
	}
	print offset
    }'
}

# dump the bitmap in "black"/"white" format
dumpbitmap() {
    od -bvw$3 | head -$2 | awk '
	BEGIN {
		w = '"$1"' ;  black = "'"$4"'" ;  white = "'"$5"'" ;
		s["0"]="000"; s["1"]="001"; s["2"]="010"; s["3"]="011";
		s["4"]="100"; s["5"]="101"; s["6"]="110"; s["7"]="111";
		t["0"]="00";  t["1"]="01";  t["2"]="10";  t["3"]="11"; 
	}
	{
	    zo = ""; bm = "";
	    for (i = 2; i <= NF; i++) {
		zo = zo t[substr($i,1,1)] s[substr($i,2,1)] s[substr($i,3,1)]
	    }
	    for (i = 1; i <= w; i++) {
		if ( substr(zo,i,1) == "1" )  bm = bm black
		else  bm = bm white
	    }
	    printf "%s\n", bm
	}'
}

# dump the bitmap in EPSF
bitmap_eps () {
    BM=`od -bvw$3 | head -$2 | awk '{
	for (i = 2; i <= NF; i++) {
	    n = substr($i,1,1) * 64 + substr($i,2,1) * 8 + substr($i,3,1)
	    printf "%02x", n
	}
	printf "\n"
    }'`
    echo "%!PS-Adobe-2.0 EPSF-1.2"
    echo "%%BoundingBox: 0 1 `expr $1 - 1` $2"
    echo "%%BeginPreview: $1 $2 1 $2"
    echo "$BM" | sed 's/^/% /'
    echo "%%EndImage"
    echo "%%EndPreview"
    echo "$1 $2 scale"
    echo "$1 $2 true [ $1 0 0 -$2 0 $2 ] {<"
    echo "$BM"
    echo ">} imagemask"
}

# dump the bitmap in X11 bitmap format
bitmap_xbm () {
    echo "#define ${4}_width  $1"
    echo "#define ${4}_height $2"
    echo "static char ${4}_bits[] = {"
    od -bvw$3 | head -$2 | awk '
	BEGIN {
		# X11 bitmap is LSB first.
		rs["0"]=0;  rs["1"]=4;  rs["2"]=2;  rs["3"]=6;
		rs["4"]=1;  rs["5"]=5;  rs["6"]=3;  rs["7"]=7;
		rt["0"]=0;  rt["1"]=2;  rt["2"]=1;  rt["3"]=3;
	}
	{
	    for (i = 2; i <= NF; i++) {
		n = rt[substr($i,1,1)] + rs[substr($i,2,1)] * 4	\
			+ rs[substr($i,3,1)] * 32
		printf " 0x%02x, ", n
	    }
	    printf "\n"
        }'
    echo "};"
}

# dump the bitmap in PBM P4 format
bitmap_pbm () {
    echo "P4"
    echo "$1 $2"
    dd bs=1 count=$3  2>/dev/null
}

# usage
usage() {
  (
    echo "Usage: $0 [ options ] hbf-filename hanzi-code"
    echo " "
    echo "options:"
    echo "    -p black white"
    echo "        Dump the bitmap on screen using human readable text,"
    echo "        using the two strings to show bit 1 and 0."
    echo "        (Note that the two strings must have the same length.)"
    echo "    -eps filename.eps"
    echo "        Write the bitmap in EPS encapsulate postscript format"
    echo "        into the given file."
    echo "    -xbm filename.xbm"
    echo "        Write the bitmap in X11 bitmap format"
    echo "        into the given file."
    echo "    -pbm filename.pbm"
    echo "        Write the bitmap in PBM portable bitmap format"
    echo "        into the given file."
    echo " "
    echo "The above 4 options are exclusive.  By default, -p \"#\" \".\""
    echo "The hanzi-code should be in hex (starts with 0x), oct, or dec."
    echo " "
    echo "Example:        $0 cclib16st.hbf 0xb0a1"
    echo "                $0 -p \"**\" \". \" cclib16st.hbf 0xb0a1"
    echo "                $0 -eps hz_b0a1.eps cclib16st.hbf 0xb0a1"
    echo " "
  ) 1>&2
}


# default options
FORMAT="dump"
BLACK="#"
WHITE="."

# get command line arguments
#
while [ "$1" != "" ]; do
    case "$1" in
	-p )	if [ $# -lt 3 ]; then  usage; exit; fi
		FORMAT="dump";  BLACK="$2";  WHITE="$3";
		shift 3
		;;
	-eps )	if [ $# -lt 2 ]; then  usage; exit; fi
		FORMAT="eps";  OUTPUT="$2"
		shift 2
		;;
	-xbm )	if [ $# -lt 2 ]; then  usage; exit; fi
		FORMAT="xbm";  OUTPUT="$2"
		shift 2
		;;
	-pbm )	if [ $# -lt 2 ]; then  usage; exit; fi
		FORMAT="pbm";  OUTPUT="$2"
		shift 2
		;;
	-* )	usage; exit
		;;
	* )	break;
		;;
    esac
done
if [ $# -ne 2 ]; then
    usage; exit;
fi
HBFFILE=$1
HZCODE=$2

# check the arguments
#
if [ ! -r $HBFFILE ]; then
    echo "unable to open hbf-file \"$HBFFILE\"." 1>&2
    exit 1
fi
if [ "`dd if=$HBFFILE bs=1 count=14 2>/dev/null`" != "HBF_START_FONT" ]; then
    echo "File \"$HBFFILE\" doesn't seem to be a HBF file."
    echo "Its very first line should start with \"HBF_START_FONT\"."
    exit 1
fi

hz=`todec $HZCODE`
if [ $? -ne 0 ]; then
    echo "hanzi code \"$HZCODE\" is not an integer (hex, oct, or dec)." 1>&2
    exit 1
fi
hz_b1=`echo "scale=0; $hz / 256" | bc`
hz_b2=`echo "$hz % 256" | bc`
if [ "$hz_b1" -gt 255 ]; then
    echo "hanzi code \"$HZCODE\" is out of range (0x0000 -- 0xffff)." 1>&2
    exit 1
fi

if [ `echo "$BLACK" | wc -c` -ne `echo "$WHITE" | wc -c` ]; then
    echo "pixels \"$BLACK\" and \"$WHITE\" must have the same length." 1>&2
    exit 1
fi

# processing the file.
#
valid_b2=0

# (tr -d '\015')  is to remove the ^M, in case it is a MS-DOS file
#
cat $HBFFILE | tr -d '\015' | \
while read f1 f2 f3 f4 f5 f6 f7 f8 f9; do

  case "$f1" in

    HBF_BITMAP_BOUNDING_BOX )
	w=`todec $f2`;  h=`todec $f3`
	bpl=`echo "scale=0; ($w + 7)/8" | bc`		# trunc to int
	bpc=`echo "scale=0; $h * $bpl" | bc`
	echo "$f1 $f2 $f3 $f4 $f5    => byte/row=$bpl, byte/char=$bpc" 1>&2
	;;

    HBF_BYTE_2_RANGE )
	rs=`echo ${f2}${f3}${f4} | sed 's/-.*$//' | todec`
	re=`echo ${f2}${f3}${f4} | sed 's/^.*-//' | todec`
	B2RANGES="$B2RANGES  $rs $re"
	if [ "(" $rs -le $hz_b2 ")" -a "(" $hz_b2 -le $re ")" ]; then
	    valid_b2=1
	    echo "$f1 $f2        => byte2 range here" 1>&2
	else
	    echo "$f1 $f2" 1>&2
	fi
	;;

    HBF_END_BYTE_2_RANGES )
	if [ $valid_b2 -eq 0 ]; then
	    echo "no such byte2 range in \"$HBFFILE\" for \"$HZCODE\"" 1>&2
	    exit 1
	fi
	;;

    HBF_CODE_RANGE )
	if [ "$f5" = "" ]; then
	    cs=`echo ${f2} | sed 's/-.*$//' | todec`
	    ce=`echo ${f2} | sed 's/^.*-//' | todec`
	    F=$f3;  co=`todec $f4`
	elif [ "$f6" = "" ]; then
	    cs=`echo ${f2}${f3} | sed 's/-.*$//' | todec`
	    ce=`echo ${f2}${f3} | sed 's/^.*-//' | todec`
	    F=$f4;  co=`todec $f5`
	else
	    cs=`echo ${f2}${f3}${f4} | sed 's/-.*$//' | todec`
	    ce=`echo ${f2}${f3}${f4} | sed 's/^.*-//' | todec`
	    F=$f5;  co=`todec $f6`
	fi
	if [ "(" $cs -le $hz ")" -a "(" $hz -le $ce ")" ]; then
	    echo "$f1 $f2 $f3 $f4 $f5 $f6       => \"$HZCODE\" is here" 1>&2

	    BMFILE="$F"
	    if [ ! -r "$F" ]; then
		BMFILE="`dirname $HBFFILE`/$F"
		if [ ! -r "$BMFILE" ]; then
		    echo "cannot find bitmap file \"$F\"." 1>&2
		    exit
		fi
	    fi

	    cs_b1=`echo "scale=0; $cs / 256" | bc`
	    cs_b2=`echo "$cs % 256" | bc`

	    cdt=`offset_byte2 $cs_b2 $B2RANGES`
	    hdt=`offset_byte2 $hz_b2 $B2RANGES`
	    maxdt=`offset_byte2  256 $B2RANGES`

	    C=`echo "($hz_b1 - $cs_b1) * $maxdt + $hdt - $cdt" | bc`
	    B=`echo "$C * $bpc + $co" | bc`

	    echo "bitmap is in \"$BMFILE\", offset = $B." 1>&2
	    fl=`/bin/ls -lL "$BMFILE" | awk '{print $4;exit}'`
	    if [ "$fl" -lt "`expr $B + $bpc`" ]; then
		echo "bitmap file \"$BMFILE\" is too short." 1>&2
		exit 1
	    fi

	    if [ "$B" -eq 0 ]; then
		B=$bpc; skp=0
	    else
		skp=1
	    fi

	    dd if="$BMFILE" bs=$B skip=$skp  2>/dev/null |	\
		case $FORMAT in
		  eps)	bitmap_eps "$w" "$h" "$bpl" > $OUTPUT
			    ;;
		  xbm)	bitmap_xbm "$w" "$h" "$bpl" "HZ_$HZCODE" > $OUTPUT
			    ;;
		  pbm)	bitmap_pbm "$w" "$h" "$bpc" > $OUTPUT
			    ;;
		  * )	dumpbitmap "$w" "$h" "$bpl" "$BLACK" "$WHITE"
			    ;;
		esac

	    exit
	else
	    echo "$f1 $f2 $f3 $f4 $f5 $f6       => no \"$HZCODE\"" 1>&2
	fi
	;;
		
    HBF_END_CODE_RANGES )
	echo "no such code range in $HBFFILE for $HZCODE" 1>&2
	exit
	;;

  esac

done
