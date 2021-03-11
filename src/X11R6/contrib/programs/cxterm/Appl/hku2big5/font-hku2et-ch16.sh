#!/bin/sh
if [ "$1" = "" ]; then
	echo "Which directory did you put the file hku-ch16.bdf?"  1>&2
	echo "usage: $0 directory_name" 1>&2
	exit 1;
fi
if [ -r "$1/hku-ch16.bdf" ]; then
	INPUT="cat $1/hku-ch16.bdf"
elif [ -r "$1/hku-ch16.bdf.Z" ]; then
	INPUT="zcat $1/hku-ch16.bdf.Z"
else
	echo "Cannot open $1/hku-ch16.bdf or $1/hku-ch16.bdf.Z"  1>&2
	exit 1;
fi
if $INPUT | head -10 | grep -s 'BIG5\.HKU'; then
	echo "start converting ... " 1>&2
else
	echo "$1/hku-ch16.bdf(.Z) is not a hku-ch16 font.  Sorry." 1>&2
	exit 1;
fi

$INPUT | sed \
  -e '/^FONT/s/-BIG5\.HKU-/-BIG5.ET-/' \
  -e '/^CHARSET_REGISTRY/s/BIG5\.HKU/BIG5.ET/' \
| awk '
/^STARTCHAR/ {	next; }
/^ENCODING/  {	c = $2;  h = int(c / 256);  l = c % 256;
		if ((h > 198) || ((h == 198) && (l >= 161))) {
			if ((l >= 224) && (l <= 254))
				{ c = (h + 3) * 256 + (l - 224 + 161); }
			else if ((l >= 161) && (l <= 223))
				{ c = (h + 3) * 256 + (l - 161 +  64); }
			else if ((l >=  64) && (l <= 126))
				{ c = (h + 2) * 256 + (l -  64 + 192); }
		}
		printf ("STARTCHAR %x\n", c);
		printf ("ENCODING %d\n", c);
		next;
	      }
{ print }
' > $1/et-ch16.bdf
echo "$1/et-ch16.bdf  generated."  1>&2
