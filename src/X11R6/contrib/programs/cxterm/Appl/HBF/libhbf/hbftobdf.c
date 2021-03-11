/*
 * Convert an HBF file to the X Windows BDF format.
 *
 *	hbftobdf hbf-file
 *
 * Ross Paterson <rap@doc.ic.ac.uk>
 *
 * DEFAULT_CHAR fix by Fung Fung Lee <lee@simd.stanford.edu>
 */
#include "defs.h"
#include "hbf.h"

#define	MAXLINE	1024

local	char	*pname;

local void
put_char(font, code)
reg	HBF	*font;
reg	unsigned code;
{
reg	const	unsigned char	*bp;
reg	int	linewidth;
reg	int	i, j;

	if ((bp = hbfGetBitmap(font, code)) == NULL) {
		fprintf(stderr, "%s: can't read char %04x\n", pname, code);
		return;
	}

	printf("STARTCHAR C%x\n", code);
	printf("ENCODING %u\n", code);
	printf("SWIDTH 666 0\n");	/* I'm not sure about this part */
	printf("DWIDTH %d 0\n", hbfFontBBox(font)->hbf_width);
	printf("BBX %d %d %d %d\n",
		hbfFontBBox(font)->hbf_width,
		hbfFontBBox(font)->hbf_height,
		hbfFontBBox(font)->hbf_xDisplacement,
		hbfFontBBox(font)->hbf_yDisplacement);
	printf("BITMAP\n");
	linewidth = HBF_RowSize(font);
	for (i = hbfBitmapBBox(font)->hbf_height; i > 0; i--) {
		for (j = linewidth; j > 0; j--)
			printf("%02x%", *bp++&0xff);
		putchar('\n');
	}
	printf("ENDCHAR\n");
}

int
main(argc, argv)
	int	argc;
	char	*argv[];
{
reg	HBF	*font;
	char	line[MAXLINE];
	FILE	*f;
	long	code;

	pname = argv[0];
	if (argc != 2) {
		fprintf(stderr, "Usage: %s hbffile\n", argv[0]);
		return 1;
	}
	if ((font = hbfOpen(argv[1])) == NULL ||
	    (f = fopen(hbfFileName(font), "r")) == NULL) {
		fprintf(stderr, "%s: can't find font '%s'\n",
			argv[0], argv[1]);
		return 1;
	}

	printf("STARTFONT 2.1\n");
	/* copy header, turning HBF lines into comments */
	while (fgets(line, MAXLINE, f) != NULL)
		if (strncmp(line, "HBF_", 4) == 0)
			printf("COMMENT %s", line);
		else if (sscanf(line, "DEFAULT_CHAR %li", &code) == 1)
			printf("DEFAULT_CHAR %ld\n", code);
		else
			fputs(line, stdout);

	/* print bitmaps */
	hbfForEach(font, put_char);
	hbfClose(font);

	printf("ENDFONT\n");
	return 0;
}
