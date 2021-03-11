
#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/image/tiff/RCS/mkspans.c,v 1.4 1992/12/16 17:04:55 rr2b R6tape $";
#endif


#include "tiffcompat.h"



dumparray(name, runs)
	char *name;
	unsigned char runs[256];
{
	register int i;
	register char *sep;
	printf("static u_char %s[256] = {\n", name);
	sep = "    ";
	for (i = 0; i < 256; i++) {
		printf("%s%d", sep, runs[i]);
		if (((i + 1) % 16) == 0) {
			printf(",	/* 0x%02x - 0x%02x */\n", i-15, i);
			sep = "    ";
		} else
			sep = ", ";
	}
	printf("\n};\n");
}

main()
{
	unsigned char runs[2][256];

	bzero(runs[0], 256*sizeof (char));
	bzero(runs[1], 256*sizeof (char));
	{ register int run, runlen, i;
	  runlen = 1;
	  for (run = 0x80; run != 0xff; run = (run>>1)|0x80) {
		for (i = run-1; i >= 0; i--) {
			runs[1][run|i] = runlen;
			runs[0][(~(run|i)) & 0xff] = runlen;
		}
		runlen++;
	  }
	  runs[1][0xff] = runs[0][0] = 8;
	}
	dumparray("bruns", runs[0]);
	dumparray("wruns", runs[1]);
}
