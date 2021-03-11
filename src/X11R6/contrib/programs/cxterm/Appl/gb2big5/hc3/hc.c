/*
 * NAME:
 *	hc.c - Hanzi Converter Version 3.0 main program
 *	Copyright (C) 1988,1989,1990,1993  by Fung F. Lee & Ricky Yeung
 *
 * DESCRIPTION:
 *	hc converts a GB file to a BIG-5 file, or a BIG-5 file to a GB file.
 *	GB (GuoBiao) refers to the standard implementation of GB2312-80
 *	of Mainland China, in which the two bytes representing a GB code
 *	have their most significant bit set to 1.  BIG-5 refers to the Big
 *	Five standard published in 1984 by Taiwan's Institute for Information
 *	Industry. Currently, most popular Chinese systems use either
 *	GB or BIG-5.
 *
 * API:
 *	See hclib.h.
 *
 * FORMAT OF THE CONVERSION TABLE FILE
 *	The table file contains newline-terminated mapping entries.
 *	Each entry is a two-byte GB code followed by a list of two-byte
 *	BIG5 codes.  Each entry line cannot exceed BUFSIZE characters.
 *	See hc.tab.  Users may build their own separate table files.
 *
 * AUTHORS:
 *	Ricky Yeung (Ricky.Yeung@eng.sun.com)
 *	Fung F. Lee (lee@umunhum.stanford.edu)
 *
 * ACKNOWLEDGEMENT:
 *	Thanks to Mr. Edmund Lai (lai@apple.com) for providing most of
 *	the mapping data for the less-frequently-used hanzi of GB.
 *
 * DISTRIBUTION:
 *
 *	This program and the table file are NOT in the public domain.
 *	All Rights Reserved.
 *
 *	You may copy and distribute verbatim copies of hc source code
 *	files, table file(s), and documentation files as you receive it
 *	for non-commercial purposes.
 *
 *	If you wish to incorporate parts of hc into other programs,
 *	write to the authors.  We have not yet worked out a simple rule
 *	that can be stated here, but we will often permit this.
 *
 *	This software is provided "as is" without warranty of any kind,
 *	either expressed or implied, including, but not limited to,
 *	the implied warranty of fitness for a particular purpose.
 *
 * DISCLAIMER
 *
 *	This software has no connection with our employers.
 *
 */


#include <stdio.h>
#include "hclib.h"

static usage(prog)
char *prog;
{
    fprintf(stderr,"usage: %s -m mode -map mapping [-t t1 t2 ...] < file\n", 
	    prog);
    fprintf(stderr, "\tmode is g2b or b2g\n");
    fprintf(stderr, "\tmapping is one, all, or allbutsymbols\n");
    fprintf(stderr, "\tti is a conversion table filename\n");
    exit(1);
}


main(argc, argv)
int argc;
char *argv[];
{
    int	mode = 0, i, mapping1 = HC_DO_SINGLE;
    char *pname;
    long miss;
    
    pname = argv[0];
    for( i=1; i<argc; i++)
    {
	if ((!strcmp(argv[i], "-mode")) || (!strcmp(argv[i], "-m")))
	{
	    i++;
	    if (!strcmp(argv[i], "b2g"))
		mode = HC_BIGtoGB;
	    else if (!strcmp(argv[i], "g2b"))
		mode = HC_GBtoBIG;
	    else usage(pname);
	}
	else if (!strcmp(argv[i], "-map"))
	{
	    i++;
	    if (!strcmp(argv[i], "one"))
		mapping1 = HC_DO_SINGLE;
	    else if (!strcmp(argv[i], "all"))
		mapping1 = HC_DO_ALL;
	    else if (!strcmp(argv[i], "allbutsymbols"))
		mapping1 = HC_DO_ALL_BUT_SYMBOLS;
	    else usage(pname);
	}
	else if (!strcmp(argv[i], "-t"))
	{
	    while (1)
	    {
		if (++i >= argc) break;
		if (argv[i][0]=='-') /* other option, put it back */
		{
		    i--;
		    break;
		}
		else hc_readtab(argv[i]);
	    }
	}
	else usage(pname);
    }
    if ((mode!=HC_BIGtoGB)&&(mode!=HC_GBtoBIG)) usage(pname);
    miss = hc_convert_fp(stdin, stdout, mode, mapping1); 
    if (miss > 0)
	fprintf(stderr, "number of hanzi not converted = %ld\n", miss);

    return 0;
}

