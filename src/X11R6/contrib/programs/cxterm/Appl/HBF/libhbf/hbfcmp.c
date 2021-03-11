/*
 * Compare the bitmaps in two fonts referenced by HBF files.
 *
 *	hbfcmp hbf-file1 hbf-file2
 *
 * BUG: codes present in the second file but not the first are not reported.
 *
 * Ross Paterson <rap@doc.ic.ac.uk>
 */
#include "defs.h"
#include "hbf.h"

typedef enum {
	STATE_EXTRA,	/* last char was missing from font 2 */
	STATE_NULL_1,	/* last char was null in font 1 only */
	STATE_NULL_2,	/* last char was null in font 2 only */
	STATE_DIFF,	/* last chars were different */
	STATE_SAME	/* last chars were identical */
} STATE;

local	bool	different;
local	STATE	state;
local	unsigned first_code;	/* first code in current state */
local	unsigned last_code;

local	char	*font1_name, *font2_name;

local	HBF	*font2;
local	int	total_non_null;
local	long	diff_bits;
local	int	extras;
local	int	nulls1, nulls2;

local	void	set_state	ARGS((unsigned code, STATE new_state));
local	void	cmp_chars	ARGS((HBF *font1, unsigned code));

int
main(argc, argv)
	int	argc;
	char	*argv[];
{
reg	HBF	*font1;

	if (argc != 3) {
		fprintf(stderr, "usage: %s font1 font2\n", argv[0]);
		exit(2);
	}
	if ((font1 = hbfOpen(argv[1])) == NULL) {
		fprintf(stderr, "%s: can't find font '%s'\n",
			argv[0], argv[1]);
		exit(2);
	}
	if ((font2 = hbfOpen(argv[2])) == NULL) {
		fprintf(stderr, "%s: can't find font '%s'\n",
			argv[0], argv[2]);
		exit(2);
	}

	if (hbfBitmapBBox(font1)->hbf_width !=
			hbfBitmapBBox(font2)->hbf_width ||
	    hbfBitmapBBox(font1)->hbf_height !=
			hbfBitmapBBox(font2)->hbf_height) {
		printf("fonts have differing dimensions\n");
		exit(1);
	}

	font1_name = argv[1];
	font2_name = argv[2];

	different = FALSE;
	state = STATE_SAME;
	total_non_null = 0;
	extras = nulls1 = nulls2 = 0;
	hbfForEach(font1, cmp_chars);

	if (extras > 0)
		printf("\"%s\": %d missing chars\n", font2_name, extras);
	if (nulls2 > 0)
		printf("\"%s\": %d extra bitmaps\n", font1_name, nulls2);
	if (nulls1 > 0)
		printf("\"%s\": %d extra bitmaps\n", font2_name, nulls1);
	if (diff_bits > 0)
		printf("Non-null bitmaps are %g%% different.\n",
			(100.0*diff_bits)/(total_non_null *
				hbfBitmapBBox(font2)->hbf_width *
				hbfBitmapBBox(font1)->hbf_height));

	if (! different)
		printf("The fonts are identical.\n");

	return different;
}

local void
cmp_chars(font1, code)
	HBF	*font1;
	unsigned code;
{
reg	const	unsigned char	*p1, *p2, *p1end;
reg	int	c1, c2;
	bool	null1, null2;
reg	int	diff;
reg	unsigned int i;

	if ((p1 = hbfGetBitmap(font1, code)) == NULL)
		return;
	if ((p2 = hbfGetBitmap(font2, code)) == NULL) {
		set_state(code, STATE_EXTRA);
		return;
	}
	null1 = null2 = TRUE;
	diff = 0;
	p1end = p1 + HBF_BitmapSize(font1);
	while (p1 != p1end) {
		c1 = *p1++;
		c2 = *p2++;
		if (c1 != '\0')
			null1 = FALSE;
		if (c2 != '\0')
			null2 = FALSE;
		if (c1 != c2)	/* Count the different bits */
			for (i = ((c1 ^ c2)&0xff); i != 0; i >>= 1)
				diff += (i & 01);
	}
	if (null1) {
		if (null2)
			set_state(code, STATE_SAME);
		else
			set_state(code, STATE_NULL_1);
	}
	else if (null2)
		set_state(code, STATE_NULL_2);
	else {
		set_state(code, diff > 0 ? STATE_DIFF : STATE_SAME);
		diff_bits += diff;
		total_non_null++;
	}
}

local void
set_state(code, new_state)
	unsigned code;
	STATE	new_state;
{
	if (new_state != STATE_SAME)
		different = TRUE;
	if (new_state != state) {
		switch (state) {
		when STATE_EXTRA:
			if (last_code > first_code)
				printf("\"%s\", %04x-%04x: missing chars\n",
					font2_name, first_code, last_code);
			else
				printf("\"%s\", %04x: missing char\n",
					font2_name, last_code, font2_name);
			extras += last_code - first_code + 1;
		when STATE_NULL_1:
			if (last_code > first_code)
				printf("\"%s\", %04x-%04x: extra bitmaps\n",
					font2_name, first_code, last_code);
			else
				printf("\"%s\", %04x: extra bitmap\n",
					font2_name, last_code);
			nulls1 += last_code - first_code + 1;
		when STATE_NULL_2:
			if (last_code > first_code)
				printf("\"%s\", %04x-%04x: extra bitmaps\n",
					font1_name, first_code, last_code);
			else
				printf("\"%s\", %04x: extra bitmap\n",
					font1_name, last_code);
			nulls2 += last_code - first_code + 1;
		when STATE_DIFF	or STATE_SAME:
			;
		}
		state = new_state;
		first_code = code;
	}
	last_code = code;
}
