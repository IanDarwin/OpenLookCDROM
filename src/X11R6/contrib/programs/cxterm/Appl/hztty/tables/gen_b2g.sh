#!/bin/sh

GBTOBIG="g2b"
BIGTOGB="b2g"

echo "" | awk '
BEGIN {
	for ( q = 161; q <= 169; q++ ) {	# 0xa1 -- 0xa9
		for ( w = 161; w <= 254; w++ ) { printf "%c%c\n", q, w; }
	}
	for ( q = 176; q <= 247; q++ ) {	# 0xb0 -- 0xf7
		for ( w = 161; w <= 254; w++ ) { printf "%c%c\n", q, w; }
	}
}' | eval "$GBTOBIG" > mapGBtoBIG.b5

echo "" | awk '
BEGIN {
	for ( q = 161; q <= 246; q++ ) {		# 0xa1 -- 0xf6
		for ( w =  64; w <= 126; w++ ) { printf "%c%c\n", q, w; }
		for ( w = 161; w <= 254; w++ ) { printf "%c%c\n", q, w; }
	}
	q = 247; {				# 0xf7
		for ( w =  64; w <=  85; w++ ) { printf "%c%c\n", q, w; }
	}
}' | eval "$BIGTOGB" > mapBIGtoGB.gb

make	# generate the gen_ctab program

./gen_ctab mapGBtoBIG.b5 GtoB >  b2g_tables.c
./gen_ctab mapBIGtoGB.gb BtoG >> b2g_tables.c
echo "A new \"b2g_tables.c\" is generated.  Replace the old one with this."
