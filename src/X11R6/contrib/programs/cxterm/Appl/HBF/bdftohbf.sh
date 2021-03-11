#!/bin/sh

# Generate an HBF file from a BDF file.
# All characters must have the same BBX lines.
# Usage:
#	bdftohbf <stem>.bdf.gz
# creates files <stem>.bin and <stem>.hbf

# Ross Paterson <rap@doc.ic.ac.uk>

# Requires Mark Leisher's bdftobin

bdftobin=bdftobin

case $# in
1)	;;
*)	echo "usage: $0 <stem>.bdf.gz" >&2
	exit 1 ;;
esac

if [ ! -f "$1" ]
then	echo "$0: can't find '$1'" >&2
	exit 1
fi

case "$1" in
*.bdf.gz) ;;
*)	echo "$0: '$1' is not a gzipped BDF file" >&2
	exit 1 ;;
esac
stem=`echo "$1" | sed 's/\.bdf\.gz$//'`
base=`basename $stem`

# extract font bounding box and code scheme

set `gzcat $stem.bdf.gz | sed -n '/^BBX/ {
p
q
}'`
width=$2
height=$3
xoff=$4
yoff=$5

code_scheme=`gzcat $stem.bdf.gz | sed -n '/^CHARSET_REGISTRY/ {
s/.*"\(.*\)".*/\1/p
q
}'`

# temporary files to hold ranges

byte2_tmp=/tmp/btoh.b$$
code_tmp=/tmp/btoh.c$$
trap "rm -f $byte2_tmp $code_tmp; exit" 0 1 2 13 15

#determine byte-2 ranges

gzcat $stem.bdf.gz |
	grep '^ENCODING' |
	awk '{ seen[$2%256] = "x" }
	END {
		started = 0
		for (b = 0; b < 256; b++)
			if (seen[b] != "") {
				if (! started) {
					started = 1
					start = b
				}
			}
			else if (started) {
				print start, b-1
				started = 0
			}
		if (started)
			print start, b-1
	}' >$byte2_tmp

num_byte2_ranges=`wc -l <$byte2_tmp`

# determine code ranges

gzcat $stem.bdf.gz |
	grep '^ENCODING' |
	awk 'BEGIN {
		height = '$height'
		width = '$width'
		bmsize = int((width+7)/8) * height
		num_byte2_ranges = '"$num_byte2_ranges"'
		'"`cat -n $byte2_tmp | sed 's/ *\(.*\)	\(.*\) \(.*\)/start[\1] = \2; finish[\1] = \3/'`"'
	}
	{
		this_code = $2
		if (started == 0) {
			start_code = this_code
			byte2 = this_code%256
			for (range = 1; start[range] > byte2; range++)
				;
			offset = 0
			started = 1
		}
		else {
			if (last_code%256 == finish[range]) {
				if (++range > num_byte2_ranges) {
					range = 1
					last_code += 256
				}
				next_code = int(last_code/256)*256 + start[range]
			}
			else
				next_code = last_code + 1
			if (this_code != next_code) {
				printf("HBF_CODE_RANGE 0x%04x-0x%04x '$base'.bin %d\n", start_code, last_code, offset)
				start_code = this_code
				byte2 = this_code%256
				for (range = 1; start[range] > byte2; range++)
					;
				offset = (NR-1)*bmsize
				started = 1
			}
		}
		last_code = this_code
	}
	END {
		if (started)
			printf("HBF_CODE_RANGE 0x%04x-0x%04x '$base'.bin %d\n", start_code, last_code, offset)
	}' >$code_tmp

# build the HBF file

(
	echo HBF_START_FONT 1.0
	echo HBF_CODE_SCHEME $code_scheme
	echo HBF_BITMAP_BOUNDING_BOX $width $height $xoff $yoff
	gzcat $stem.bdf.gz | sed '/^CHARS[ 	]/q' | grep -v '^STARTFONT'
	echo HBF_START_BYTE_2_RANGES $num_byte2_ranges
	awk '{printf("HBF_BYTE_2_RANGE 0x%02x-0x%02x\n", $1, $2)}' $byte2_tmp
	echo HBF_END_BYTE_2_RANGES
	echo HBF_START_CODE_RANGES `wc -l <$code_tmp`
	cat $code_tmp
	echo HBF_END_CODE_RANGES
	echo HBF_END_FONT
) >$stem.hbf

# build the corresponding binary

gzcat $stem.bdf.gz | $bdftobin >$stem.bin
