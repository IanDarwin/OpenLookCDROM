/*
 * Test harness for HBF functions:
 *
 *	tester hbf-file char ...
 *
 * where 'font' is the name of a HBF file (minus it's extension), and
 * each 'char' is a hex code of a character in the font, prints ASCII
 * representations of the character bitmaps on standard output.
 */
#include <stdio.h>
#include "hbf.h"

void
show_bitmap(font, bitmap)
	HBF	*font;
	const	unsigned char	*bitmap;
{
	int	x, y;

	for (y = 0; y < hbfBitmapBBox(font)->hbf_height; y++) {
		for (x = 0; x < hbfBitmapBBox(font)->hbf_width; x++)
			putchar(HBF_GetBit(font, bitmap, x, y) ? '#' : ' ');
		putchar('\n');
	}
}

int
main(argc, argv)
	int	argc;
	char	*argv[];
{
	HBF	*font;
	int	code;
	int	i;
	const	unsigned char	*bitmap;

	hbfDebug = 1;
	if (argc < 3) {
		fprintf(stderr, "Usage: %s font char ...\n", argv[0]);
		exit(1);
	}
	if ((font = hbfOpen(argv[1])) == NULL) {
		fprintf(stderr, "%s: can't open font '%s'\n",
			argv[0], argv[1]);
		exit(1);
	}

	printf("HBF_START_FONT %s\n",
		hbfProperty(font, "HBF_START_FONT"));
	printf("HBF_CODE_SCHEME %s\n",
		hbfProperty(font, "HBF_CODE_SCHEME"));
	printf("FONT %s\n",
		hbfProperty(font, "FONT"));
	printf("HBF_BITMAP_BOUNDING_BOX %d %d %d %d\n",
		hbfBitmapBBox(font)->hbf_width,
		hbfBitmapBBox(font)->hbf_height,
		hbfBitmapBBox(font)->hbf_xDisplacement,
		hbfBitmapBBox(font)->hbf_yDisplacement);
	printf("FONTBOUNDINGBOX %d %d %d %d\n",
		hbfFontBBox(font)->hbf_width,
		hbfFontBBox(font)->hbf_height,
		hbfFontBBox(font)->hbf_xDisplacement,
		hbfFontBBox(font)->hbf_yDisplacement);
	printf("FAMILY_NAME \"%s\"\n",
		hbfProperty(font, "FAMILY_NAME"));
	printf("CHARS %s\n",
		hbfProperty(font, "CHARS"));

	for (i = 2; i < argc; i++) {
		sscanf(argv[i], "%x", &code);
		if ((bitmap = hbfGetBitmap(font, (HBF_CHAR)code)) != NULL)
			show_bitmap(font, bitmap);
		else
			fprintf(stderr, "%s: unknown char %s (%x)\n",
				argv[0], argv[i], code);
	}

	(void)hbfClose(font);
	return 0;
}
