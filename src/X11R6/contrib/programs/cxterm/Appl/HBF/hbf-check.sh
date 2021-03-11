#!/bin/sh
#
# Try to check every line of the HBF file.
#
# <<< For HBF version 1.0 >>>
#
# Should be able to just run on most Unix system.  Mostly awk,bc,sed,egrep.
# Warning: The code is from hell!  Extremely ad hoc, slow, and illegible. :-)
#
# Author:		Yongguang Zhang  (ygz@cs.purdue.edu)
# Create date:		Apr/25/93
# Last update:		Jun/06/93


PASS1AWK="hbf-chk1.awk"

if [ "$1" = "" ]; then
    echo "Usage: $0 hbf_file" 1>&2
    exit 0
fi
HBF_FILE=$1
if [ ! -r "$HBF_FILE" ]; then
    echo "Cannot access \"$HBF_FILE\"." 1>&2
    exit 1
fi


########### SYNTAX ###########

echo " " 1>&2
echo "pass 1: syntax checking ... " 1>&2

# (tr -d '\015')  is to remove the ^M, in case it is a MS-DOS file
#
cat "$HBF_FILE" | tr -d '\015' | awk -f $PASS1AWK  1>&2

if [ "$?" -ne 0 ]; then
    echo " " 1>&2
    echo "Error(s) found in \"$HBF_FILE\".  Stop." 1>&2
    exit 1
fi



########### INTEGER & RANGE ###########

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
		s["+"] = 1; s["-"] =-1;
	}
	/^0[xX]/	{				# hex
		    h = 0;
		    for (i = 3; i <= length($1); i++)
			h = h*16 + c[substr($1,i,1)];
		    print h; exit
		}
	/^[+-]0[xX]/	{				# hex
		    h = 0;
		    for (i = 4; i <= length($1); i++)
			h = h*16 + c[substr($1,i,1)];
		    print s[substr($1,1,1)] * h; exit
		}
	/^0/	{					# oct
		    h = 0;
		    for (i = 1; i <= length($1); i++)
			h = h*8 + substr($1,i,1);
		    print h; exit
		}
	/^[+-]0/	{				# oct
		    h = 0;
		    for (i = 1; i <= length($1); i++)
			h = h*8 + substr($1,i,1);
		    print s[substr($1,1,1)] * h; exit
		}
		{   print $1 }				# dec
    '
}
# function to check the type
int_type () {	
    if echo "$1" | egrep -s -i '^0[x][0-9a-f]+$'; then
	echo "hexadecimal"
    elif echo "$1" | egrep -s '^0[0-7]+$'; then
	echo "octal"
    elif echo "$1" | egrep -s '^[0-9]+$'; then
	echo "decimal"
    else
	echo "error"
    fi
}
type_check () {
  case "$2" in
    "unsigned" )
	t=`int_type "$1"`
	if [ "$t" = "error" ]; then
	    echo "** Type error: Line $4: $5"
	    echo "   \"$1\" should be an unsigned integer"
	    echo "--------"
	    exit 1
	fi
	if [ "$t" != "$3" ]; then
	    echo "** Line $4: $5"
	    echo "   \"$1\" is better to be in \"$3\""
	    echo "--------"
	    return 2
	fi
	return 0
	;;
    "integer" )
	ui=`echo $1 | sed 's/^+//;s/^-//'`
	t=`int_type $ui`
	if [ "$t" = "error" ]; then
	    echo "** Type error: Line $4: $5"
	    echo "   \"$1\" should be an integer"
	    echo "--------"
	    exit 1
	fi
	if [ "$t" != "$3" ]; then
	    echo "** Line $4: $5"
	    echo "   \"$1\" is better to be in \"$3\""
	    echo "--------"
	    return 2
	fi
	return 0
	;;
  esac
}
# offset_byte2:  count the number of codes within the range
offset_byte2 () {
    echo $* | awk '{
	b2=$1; offset = 0;
	for (i=2; i<=NF; i+=2) {
	    if ($i <= b2 && b2 <= $(i+1))
		offset += b2 - $i;
	    else if ($(i+1) < b2)
		offset += $(i+1)-$i+1;
	}
	print offset
    }'
}
out_range () {
    echo $* | awk '{
	b2=$1
	for (i=2;i<=NF;i+=2) {
	    if ($i <= b2 && b2 <= $(i+1))  exit 1;
	}
	exit 0;
    }'
}


echo " " 1>&2
echo "pass 2: integer and range checking ... (slow, please be patient)" 1>&2

line=0
codecnt=0
b2ranges=""
coderanges=""
in_prop=""

cat "$HBF_FILE" | tr -d '\015' |	\
while read keyword a1 a2 a3 a4 a5; do

  line=`expr $line + 1`
  case "$keyword" in

    SIZE )
	echo "checking SIZE ..."
	type_check $a1 "unsigned" "decimal" $line "$keyword $a1 $a2 $a3"
	type_check $a2 "unsigned" "decimal" $line "$keyword $a1 $a2 $a3"
	type_check $a3 "unsigned" "decimal" $line "$keyword $a1 $a2 $a3"
	;;

    HBF_BITMAP_BOUNDING_BOX )
	echo "checking BOUNDING BOX ......"
	type_check $a1 "unsigned" "decimal" $line "$keyword $a1 $a2 $a3 $a4"
	type_check $a2 "unsigned" "decimal" $line "$keyword $a1 $a2 $a3 $a4"
	type_check $a3  "integer" "decimal" $line "$keyword $a1 $a2 $a3 $a4"
	type_check $a4  "integer" "decimal" $line "$keyword $a1 $a2 $a3 $a4"
	w=`todec $a1`;  h=`todec $a2`
	bpl=`echo "scale=0; ($w + 7)/8" | bc`		# trunc to int
	bpc=`echo "scale=0; $h * $bpl" | bc`
	;;

    FONTBOUNDINGBOX )
	type_check $a1 "unsigned" "decimal" $line "$keyword $a1 $a2 $a3 $a4"
	type_check $a2 "unsigned" "decimal" $line "$keyword $a1 $a2 $a3 $a4"
	type_check $a3  "integer" "decimal" $line "$keyword $a1 $a2 $a3 $a4"
	type_check $a4  "integer" "decimal" $line "$keyword $a1 $a2 $a3 $a4"
	wf=`todec $a1`;  hf=`todec $a2`
	if [ "(" $wf -lt $w ")" -o "(" $hf -lt $h ")" ]; then
	    echo "** Line $line:  $keyword $a1 $a2 $a3 $a4"
	    echo "   FONT bbx is smaller than HBF_BITMAP bbx.  Are you sure?"
	    echo "--------"
	fi
	;;

    CHARS )
	echo "checking CHARS ..."
	type_check $a1 "unsigned" "decimal" $line "$keyword $a1"
	num_code=`todec $a1`
	;;

    HBF_START_BYTE_2_RANGES )
	last_re=0
	;;

    HBF_BYTE_2_RANGE )
	echo "checking BYTE_2 RANGE ......"
	c1=`echo ${a1}${a2}${a3} | sed 's/-.*$//'`
	c2=`echo ${a1}${a2}${a3} | sed 's/^.*-//'`
	type_check $c1 "unsigned" "hexadecimal" $line "$keyword $a1 $a2 $a3"
	type_check $c2 "unsigned" "hexadecimal" $line "$keyword $a1 $a2 $a3"
	rs=`todec $c1`;  re=`todec $c2`
	if [ "(" $rs -gt $re ")" -o "(" $re -gt 255 ")" ]; then
	    echo "** Line $line:  $keyword $a1 $a2 $a3"
	    echo "   Invalid range.  (0x00 <= range1 <= range2 <= 0xff)"
	    echo "--------"
	    exit 1
	fi
	if [ $rs -lt $last_re ]; then
	    echo "** Style error: Line $line:  $keyword $a1 $a2 $a3"
	    echo "   byte2 ranges should be sorted and disjoined!"
	    echo "--------"
	    exit 1
	fi
	if [ `expr $last_re + 1` -eq $rs ]; then
	    echo "** Line $line:  $keyword $a1 $a2 $a3"
	    echo "   Why don't you merge the adjunct byte2 ranges?"
	    echo "--------"
	fi
	b2ranges="$b2ranges  $rs $re"
	last_re=$re
	;;

    HBF_CODE_RANGE )
	echo "checking CODE RANGE ......"
	if [ "$a4" = "" ]; then
	    c1=`echo ${a1} | sed 's/-.*$//'`
	    c2=`echo ${a1} | sed 's/^.*-//'`
	    bf="$a2";  c3=$a3;
	elif [ "$a5" = "" ]; then
	    c1=`echo ${a1}${a2} | sed 's/-.*$//'`
	    c2=`echo ${a1}${a2} | sed 's/^.*-//'`
	    bf="$a3";  c3=$a4;
	else
	    c1=$a1;  c2=$a3;  bf="$a4";  c3=$a5;
	fi
	type_check $c1 "unsigned" "hexadecimal" $line \
		"$keyword $a1 $a2 $a3 $a4 $a5"
	type_check $c2 "unsigned" "hexadecimal" $line \
		"$keyword $a1 $a2 $a3 $a4 $a5"
	type_check $c3 "unsigned"     "decimal" $line \
		"$keyword $a1 $a2 $a3 $a4 $a5"
	cs=`todec $c1`;  ce=`todec $c2`;  co=`todec $c3`
	coderanges="$coderanges  $cs $ce"
	if [ "(" $cs -gt $ce ")" -o "(" $ce -gt 65535 ")" ]; then
	    echo "** Range Error: Line $line:  $keyword $a1 $a2 $a3 $a4 $a5"
	    echo "   Invalid range.  (0x0000 <= range1 <= range2 <= 0xffff)"
	    echo "--------"
	    exit 1
	fi
	cs_b1=`echo "scale=0; $cs / 256" | bc`
	cs_b2=`echo "$cs % 256" | bc`
	ce_b1=`echo "scale=0; $ce / 256" | bc`
	ce_b2=`echo "$ce % 256" | bc`
	if out_range $cs_b2 $b2ranges ; then
	    echo "** Range Error: Line $line:  $keyword $a1 $a2 $a3 $a4 $a5"
	    echo "   \"$c1\" is not valid in any of the BYTE_2_RANGE." 
	    echo "--------"
	    exit 1
	fi
	if out_range $ce_b2 $b2ranges ; then
	    echo "** Range Error: Line $line:  $keyword $a1 $a2 $a3 $a4 $a5"
	    echo "   \"$c2\" is not valid in any of the BYTE_2_RANGE." 
	    echo "--------"
	    exit 1
	fi
	sdt=`offset_byte2 $cs_b2 $b2ranges`
	edt=`offset_byte2 $ce_b2 $b2ranges`
	mdt=`offset_byte2    256 $b2ranges`
	num_ch=`echo "($ce_b1 - $cs_b1) * $mdt + $edt - $sdt + 1" | bc`
	codecnt=`expr $codecnt + $num_ch`
	if [ -r "$bf" ]; then
	    num_b=`echo "$num_ch * $bpc + $co" | bc`
	    fl=`/bin/ls -lL "$bf" | awk '{print $4;exit}'`
	    if [ "$fl" -lt "$num_b" ]; then
		echo "** Warning: Line $line:  $keyword $a1 $a2 $a3 $a4 $a5"
		echo "   bitmap file \"$bf\" is too short for the range."
		echo "--------"
	    fi
	else
	    echo "** Warning: Line $line:  $keyword $a1 $a2 $a3 $a4 $a5"
	    echo "   bitmap file \"$bf\" is not present or readable."
	    echo "--------"
	fi
	;;

    HBF_END_CODE_RANGES )
	if [ "$codecnt" -ne "$num_code" ]; then
	    echo "** Warning: number of characters mismatched!"
	    echo "   $num_code chars are declared by 'CHARS' statement."
	    echo "   But totally $codecnt chars are found in all CODE_RANGEs."
	    echo "--------"
	fi
	;;

    # X11 optional properties

    STARTPROPERTIES )
	echo "checking PROPERTIES ..."
	in_prop=1
	prop_err=0
	;;

    DEFAULT_CHAR )
	type_check $a1 "unsigned" "hexadecimal" $line "$keyword $a1"
	def_char=$a1
	dc_line="$line:  $keyword $a1" 
	;;

    AVERAGE_WIDTH | CAP_HEIGHT | DESTINATION | \
    END_SPACE | MAX_SPACE | MIN_SPACE | NORM_SPACE | \
    PIXEL_SIZE | POINT_SIZE | RELATIVE_SETWIDTH | RELATIVE_WEIGHT | \
    RESOLUTION | RESOLUTION_X | RESOLUTION_Y | \
    SMALL_CAP_SIZE | SUBSCRIPT_SIZE | SUPERSCRIPT_SIZE | \
    UNDERLINE_THICKNESS | WEIGHT )
	if [ "$a1" != "" ]; then
	    ui=`echo $a1 | sed 's/^+//;s/^-//'`
	    if [ "`int_type $ui`" = "error" ]; then
		echo "** Type error: Line $line:  $keyword $a1 ${a2:+'...'}"
		echo "   \"$a1\" should be an integer"
		echo "--------"
		prop_err=`expr $prop_err + 1`
	    fi
	fi
	;;

    AVG_CAPITAL_WIDTH | AVG_LOWERCASE_WIDTH | FIGURE_WIDTH | \
    FONT_ASCENT | FONT_DESCENT | ITALIC_ANGLE | QUAD_WIDTH | \
    STRIKEOUT_ASCENT | STRIKEOUT_DESCENT | SUBSCRIPT_X | SUBSCRIPT_Y | \
    SUPERSCRIPT_X | SUPERSCRIPT_Y | UNDERLINE_POSITION | X_HEIGHT )
	if [ "$a1" != "" ]; then
	    if [ "`int_type $a1`" = "error" ]; then
		echo "** Type error: Line $line:  $keyword $a1 ${a2:+'...'}"
		echo "   \"$a1\" should be an integer"
		echo "--------"
		prop_err=`expr $prop_err + 1`
	    fi
	fi
	;;

    FONTNAME_REGISTRY | CHARSET_ENCODING | CHARSET_REGISTRY | FOUNDRY | \
    ADD_STYLE_NAME | FACE_NAME | FAMILY_NAME | SETWIDTH_NAME | WEIGHT_NAME | \
    SLANT | SPACING | FULL_NAME | COPYRIGHT | NOTICE )
	if echo "$a1$a2$a3$a4$a5" | egrep -s '^".*"$' ; then
	    if echo "$a1 $a2 $a3 $a4 $a5" | sed 's/^"//;s/"[ ]*$//' \
		    | egrep -s '[^"]"[^"]'
	    then
		echo "** Error: Line $line:  $keyword $a1 $a2 $a3 $a4 $a5"
		echo "   the quote character (\") inside the quotedString"
		echo "   should be represented as two in a row (\"\")."
		echo "--------"
		exit 1
	    fi
	else
	    echo "** Type error: Line $line:  $keyword $a1 $a2 $a3 $a4 $a5"
	    echo "   the parameter value should be a quotedString"
	    echo "--------"
	    prop_err=`expr $prop_err + 1`
	fi
	;;

    ENDPROPERTIES )
	if [ "$prop_err" -gt 0 ]; then
	    echo "** Total $prop_err property error(s) have been found."
	    echo "   Although the format of a property is not compulsory,"
	    echo "   it is better to follow the convention."
	    echo "--------"
	fi
	in_prop=""
	;;

    HBF_END_FONT )
	dc=`todec $def_char`
	dc_b2=`echo "$dc % 256" | bc`
	if out_range $dc_b2 $b2ranges ; then
	    echo "** Range Error: Line $dc_line"
	    echo "   \"$def_char\" is not valid in any of the BYTE_2_RANGE." 
	    echo "--------"
	    exit 1
	fi
	if out_range $dc $coderanges ; then
	    echo "** Range Error: Line $dc_line"
	    echo "   \"$def_char\" is not in any of the CODE_RANGE."
	    echo "--------"
	    exit 1
	fi
	;;

    COMMENT )
	: ignore
	;;

    * )
	if [ "$in_prop" != "" ]; then	# in the PROPERTIES section
	    if echo "$a1$a2$a3$a4$a5" | egrep -s '^".*"$' ; then
		if echo "$a1 $a2 $a3 $a4 $a5" | sed 's/^"//;s/"[ ]*$//' \
			| egrep -s '[^"]"[^"]'
		then
		    echo "** Error: Line $line:  $keyword $a1 $a2 $a3 $a4 $a5"
		    echo "   the quote character (\") inside the quotedString"
		    echo "   should be represented as two in a row (\"\")."
		    echo "--------"
		    exit 1
		fi
	    else	# should be an integer
		if [ "$a2" != "" ]; then
		    echo "** Error: Line $line:  $keyword $a1 $a2 $a3 $a4 $a5"
		    echo "   the value should be either interger or string."
		    echo "--------"
		    exit 1
		fi
		ui=`echo $a1 | sed 's/^+//;s/^-//'`
		if [ "`int_type $ui`" = "error" ]; then
		    echo "** Error: Line $line:  $keyword $a1"
		    echo "   the value should be either interger or string."
		    echo "--------"
		    exit 1
		fi
	    fi
	fi
	;;

  esac

done 1>&2

if [ "$?" -ne 0 ]; then
    echo "Error(s) found in \"$HBF_FILE\".  Stop."
    exit 1
fi



########### PRAGMATIC ###########

echo " " 1>&2
echo "pass 3: pragmatic checking ... (slow, please be patient)" 1>&2

CODE_SCHEME='GB2312-1980    Big5    Unicode'
FAMILY_NAME='"Zhuan"  "Li"  "Kai"  "Song"  "Ming"  "Xing"  "Cao"  '\
'"Yuan"  "Hei"  "Xiaozhuan"  "FangSong"'
WEIGHT_NAME='"SingleWidth"    "Thin"    "Medium"   "Bold"'
SLANT='"R"    "I"    "S"    "SI"'
SETWIDTH_NAME='"Normal"'

line=0
cat "$HBF_FILE" | tr -d '\015' |	\
while read keyword a1 ; do

  line=`expr $line + 1`
  case "$keyword" in

    HBF_START_FONT )
	echo "checking FONT header ..."
	if [ "$a1" != "1.0" ]; then
	    echo "** Line $line:  $keyword $a1"
	    echo "   I guess the font version number should be '1.0'."
	    echo "--------"
	fi
	;;

    HBF_CODE_SCHEME )
	code_scheme=""
	a=`echo "$a1" | awk '{print $1}'`
	for i in $CODE_SCHEME ; do
	    if echo $a | egrep -s -i '^'"$i"'$' ; then
		code_scheme=$a
		break;
	    fi
	done
	if [ "$code_scheme" = "" ]; then
	    echo "** Line $line:  $keyword $a1"
	    echo "   I don't seem to know the code scheme \"$a\"."
	    echo "   Current acceptable code schemes are:"
	    echo "	$CODE_SCHEME"
	    echo "--------"
	    code_scheme=$a
	fi
	;;

    DEFAULT_CHAR )
	def_char=$a1
	;;

    CHARSET_REGISTRY )
	echo "checking CHARSET ..."
	char_set=`echo \"$code_scheme\" | tr '-' '.'`
	if [ "$char_set" != "$a1" ]; then
	    echo "** Line $line:  $keyword $a1"
	    echo "   CHARSET should match code scheme \"$code_scheme\"."
	    echo "   Suggestion:  CHARSET_REGISTRY \"$code_scheme\""
	    echo "--------"
	fi
	;;

    CHARSET_ENCODING )
	chs_enc_line="$line:  $keyword $a1"
	chs_enc="$a1"
	;;

    FAMILY_NAME )
	echo "checking FONT styles ..."
	found=""
	for i in $FAMILY_NAME ; do
	    if echo $a1 | egrep -s -i '^'"$i"'$' ; then
		found=$a1
		break;
	    fi
	done
	if [ "$found" = "" ]; then
	    echo "** Line $line:  $keyword $a1"
	    echo "   I don't seem to know the family name $a1."
	    echo "   Commonly used family names are:"
	    echo "	$FAMILY_NAME"
	    echo "--------"
	fi
	;;
    WEIGHT_NAME )
	found=""
	for i in $WEIGHT_NAME ; do
	    if echo $a1 | egrep -s -i '^'"$i"'$' ; then
		found=$a1
		break;
	    fi
	done
	if [ "$found" = "" ]; then
	    echo "** Line $line:  $keyword $a1"
	    echo "   I don't seem to know the weight name $a1."
	    echo "   Commonly used weight names are:"
	    echo "	$WEIGHT_NAME"
	    echo "--------"
	fi
	;;

    SLANT )
	found=""
	for i in $SLANT ; do
	    if echo $a1 | egrep -s -i '^'"$i"'$' ; then
		found=$a1
		break;
	    fi
	done
	if [ "$found" = "" ]; then
	    echo "** Line $line:  $keyword $a1"
	    echo "   I don't seem to know the slant $a1."
	    echo "   Commonly used slants are:"
	    echo "	$SLANT"
	    echo "--------"
	fi
	;;

    SETWIDTH_NAME )
	found=""
	for i in $SETWIDTH_NAME ; do
	    if echo $a1 | egrep -s -i '^'"$i"'$' ; then
		found=$a1
		break;
	    fi
	done
	if [ "$found" = "" ]; then
	    echo "** Line $line:  $keyword $a1"
	    echo "   I don't seem to know the set-width name $a1."
	    echo "   Commonly used set-width names are:"
	    echo "	$SETWIDTH_NAME"
	    echo "--------"
	fi
	;;

    ADD_STYLE_NAME )
	if echo "$code_scheme" | egrep -s -i "^GB2312"; then
	    if echo "$a1" | egrep -s -i '^"[js]' ; then
		echo "** Line $line:  $keyword $a1"
		echo "   By default a GB2312 font is simplified (jianti)."
		echo "   Suggestion:  ADD_STYLE_NAME \"\""
		echo "--------"
	    fi
	elif echo "$code_scheme" | egrep -s -i "^Big5"; then
	    if echo "$a1" | egrep -s -i '^"[ft]' ; then
		echo "** Line $line:  $keyword $a1"
		echo "   By default a GB2312 font is traditional (fangti)."
		echo "   Suggestion:  ADD_STYLE_NAME \"\""
		echo "--------"
	    fi
	fi
	;;

    HBF_END_FONT )
	if [ "$chs_enc" != "" ] && echo "$code_scheme" | egrep -s -i "^GB"
	then
	    # GB encoding are likely to have "1" as CHARSET_ENCODING 
	    dc=`todec $def_char`
	    if [ "$dc" -ge 32768 ]; then
		if [ "$chs_enc" != '"1"' ]; then
		    echo "** Line $chs_enc_line"
		    echo "   It looks like a GR-encoding font to me."
		    echo "   Suggestion:  CHARSET_ENCODING \"1\""
		    echo "--------"
		fi
	    else
		if [ "$chs_enc" != '"0"' ]; then
		    echo "** Line $chs_enc_line"
		    echo "   It looks like a GL-encoding font to me."
		    echo "   Suggestion:  CHARSET_ENCODING \"0\""
		    echo "--------"
		fi
	    fi
	fi
	;;

  esac

done 1>&2

if [ "$?" -ne 0 ]; then
    echo "Error(s) found in \"$HBF_FILE\".  Stop."
    exit 1
fi


echo " " 1>&2
echo "Done with all checking.  No fatal error found." 1>&2

exit 0

