/* $Header: file.c,v 1.4 88/12/02 10:43:11 bvs Exp $ */
/* Copyright (C) 1988 by Sun Microsystems. All rights reserved. */
#include <stdio.h>

#define PRINTFILE "file.ps"
Print()
  {
	FILE *pfile = fopen("newsdraw.ps", "w");

	fprintf(pfile, "%%!\n");
	fprintf(pfile, "%%%%!PS-Adobe-1.0\n");
	fprintf(pfile, "%%%%Creator: NewsDraw\n");
	fprintf(pfile, "%%%%Title: \n");
	fprintf(pfile, "%%%%CreationDate: \n");
	fprintf(pfile, "%%%%DocumentFonts: Times-Roman\n");
	fprintf(pfile, "%%%%Pages: (atend)\n");
	fprintf(pfile, "%%%%EndComments\n");
	fprintf(pfile, "%%\n%%\n%%\n");

	fprintf(pfile, "/Times-Roman findfont 36 scalefont setfont\n");
	fprintf(pfile, "/rect {dup 0 exch rlineto exch 0 rlineto neg 0 exch rlineto closepath } def\n");
	fprintf(pfile, "/rectpath { 4 2 roll moveto rect } def\n");
	fprintf(pfile, "/linepath { moveto lineto } def\n");
	fprintf(pfile, "/circpath { 0 360 arc } def\n");
	fprintf(pfile, "/ovalpath {  matrix currentmatrix 5 1 roll 4 2 roll translate scale .5 .5 translate 0 0 .5 0 360 arc closepath setmatrix } def\n");
	fprintf(pfile, "%%%%EndProlog\n");
	fprintf(pfile, "%%%%Page: ? 1\n");

	GOPrintItems(pfile);
	fprintf(pfile, "showpage\n");

	fprintf(pfile, "%%%%Pages: 1\n");

	fclose(pfile);

	/* system("lpr .drawprint"); */
  }

#define WRITEFILE "file.nd"
Write()
  {
	FILE *pfile = fopen(WRITEFILE, "w");
	GOWriteItems(pfile);
	fclose(pfile);
  }

Read()
  {
	FILE *pfile = fopen(WRITEFILE, "r");

	GOReadItems(pfile);
	fclose(pfile);
  }

