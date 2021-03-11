/*
 * Convert an HBF file to a stream of (code, bitmap) pairs
 *
 *	hbflist hbf-file
 *
 * Ross Paterson <rap@doc.ic.ac.uk>
 */
#include "defs.h"
#include "hbf.h"

#define	MAXLINE	1024

local	char	*pname;

local void
put_char(font, code)
reg	HBF	*font;
reg	unsigned int	code;
{
reg	const	unsigned char	*bp;
reg	int	n;

	if ((bp = hbfGetBitmap(font, code)) == NULL) {
		fprintf(stderr, "%s: can't read char %04x\n", pname, code);
		return;
	}
	printf("%04X\t", code);
	for (n = HBF_BitmapSize(font); n != 0; n--)
		printf("%02x", *bp++);
	printf("\n");
}

int
main(argc, argv)
	int	argc;
	char	*argv[];
{
reg	HBF	*font;

	pname = argv[0];
	if (argc != 2) {
		fprintf(stderr, "Usage: %s hbffile\n", argv[0]);
		exit(1);
	}
	if ((font = hbfOpen(argv[1])) == NULL) {
		fprintf(stderr, "%s: can't find font '%s'\n",
			argv[0], argv[1]);
		exit(1);
	}

	/* print bitmaps */
	hbfForEach(font, put_char);
	hbfClose(font);

	return 0;
}
