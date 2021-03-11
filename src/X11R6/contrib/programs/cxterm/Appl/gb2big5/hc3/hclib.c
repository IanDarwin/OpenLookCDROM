/*
 * NAME:
 *	hclib.c - Hanzi Converter Version 3.0 library implementation file
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
#include <ctype.h>
#include <malloc.h>
#include "hclib.h"

#define GBfirst	 0xA1A1	/* first code of GB */
#define GBlast	 0xFEFE	/* last code of GB */
#define GBsize	 0x5E5E	/* GBlast - GBfirst + 1 */
#define BIGfirst 0xA140	/* first code of BIG */
#define BIGlast	 0xF9FE	/* last code of BIG */
#define	BIGsize	 0x58BF	/* BIGlast - BIGfirst + 1 */

#define GBbox	0xA1F5	/* GB code for the empty box symbol */
#define BIGbox	0xA1BC	/* BIG code for the empty box symbol */

#define BUFSIZE 256	/* Buffer size for each table entry. */

#define DB(hi,lo)	(((hi)&0xFF) << 8 | (lo)&0xFF)
#define inGBrange(x)	(((x)>=GBfirst) && ((x)<=GBlast))
#define inBIGrange(x)	(((x)>=BIGfirst) && ((x)<=BIGlast))

/* Code mapping tables. */
static u_int16 BtoG[BIGsize], GtoB[GBsize];	

/* Arrays to store multiple mapping codes.  */
static u_int16 *mBtoG[BIGsize], *mGtoB[GBsize];

static u_int16 b5_default_code = BIGbox;
static u_int16 gb_default_code = GBbox;

u_int16 hc_set_default_code(mode, code)
    int mode;
    u_int16 code;
{
    u_int16 result;
    
    if (mode==HC_GBtoBIG)
    {
	result = gb_default_code;
	gb_default_code = code;
    }
    else if (mode==HC_BIGtoGB)
    {
	result = b5_default_code;
	b5_default_code = code;
    }
    return result;
}

void hc_clear_tabs()
{
    register int i;

    for (i=0; i<BIGsize; i++)
    {
	mBtoG[i] = NULL;
	BtoG[i] = 0;
    }
    for (i=0; i<GBsize; i++)
    {
	mGtoB[i] = NULL;
	GtoB[i] = 0;
    }
}    


void hc_clear_tab_entry(mode, code)
    int mode;
    u_int16 code;
{
    int i;
    
    if (mode==HC_GBtoBIG)
    {
	i = code - GBfirst;
	if (mGtoB[i])
	    free(mGtoB[i]);
	mGtoB[i] = NULL;
	GtoB[i] = 0;
    }
    else if (mode==HC_BIGtoGB)
    {
	i = code - BIGfirst;
	if (mBtoG[i])
	    free(mBtoG[i]);
	mBtoG[i] = NULL;
	BtoG[i] = 0;
    }
}

    
/* Add a code to the table or the multiple mapping table. */
static void add(a1, am, i, code)
    u_int16 *a1, **am, i, code;
{
    int n = 0;
    u_int16 x;

    if (!a1[i]) a1[i] = code;		/* no code, just add it */
    else if (a1[i]==code) return;	/* already there, return */
    else if (am[i])			/* already has multiple mappings */
    {
	/* Check multiple mapping list, if there, return. */
	while (x=am[i][n])
	{
	    if (x==code) return;
	    else n++;
	}

	/* Append to multiple mapping list, expand the array.
	   After the above check, n now contains the number of mappings
	   in the array, not counting the terminating zero.
	   Needs two extra spaces, one for the terminating 0. */
	am[i] = (u_int16 *) realloc(am[i], sizeof(u_int16) * (n+2));
	am[i][n] = code;
	am[i][n + 1] = 0;
    }
    else
    {
	/* First multiple mapping, allocate new list. 
	   Needs two spaces, one for the terminating 0. */
	am[i] = (u_int16 *) malloc(2 * sizeof(u_int16));
	am[i][0] = code;
	am[i][1] = 0;
    }
}


/* Process the mapping entry line.  */
static int do_line (lcnt, buffer)
    long lcnt;
    char *buffer;
{
    int c1 = buffer[0], c2 = buffer[1];
    int i = 2, total = 0;
    u_int16 gb_code = DB(c1,c2), big_code;

    if (!inGBrange(gb_code))
    {
	fprintf(stderr, "Invalid GB code in line %d\n", lcnt);
	return(0);
    }
    while (c1=buffer[i++])
    {
	c2 = buffer[i++];
	if (!(c1&&c2)) break;
	big_code = DB(c1,c2);
	if (!inBIGrange(big_code))
	{
	    fprintf(stderr, "Invalid BIG5 code in line %d\n", lcnt);
	    return(0);
	}
	add(GtoB, mGtoB, gb_code - GBfirst, big_code);
	add(BtoG, mBtoG, big_code - BIGfirst, gb_code);
	total++;
    }
    return(total);
}


long  hc_readtab(fn)
char *fn;
{
    static char buffer[BUFSIZE];
    
    long total = 0;
    long lcnt = 0;
    FILE *fp = fopen(fn,"r");

    if (!fp)
    {
	fprintf(stderr, "can't open table file: %s\n", fn);
	return(-1);
    }
    
    for (;;)
    {
	if (!fgets(buffer, BUFSIZE, fp)) break;
	if (HC_ISFIRSTBYTE(buffer[0]))
	    total += do_line(lcnt, buffer);
	lcnt++;
    }
    fclose(fp);
    return(total);
}

void hc_add_tab_entry(mode, code, mapping)
    int mode;
    u_int16 code, mapping;
{
    if (mode==HC_GBtoBIG)
	add(GtoB, mGtoB, code - GBfirst, mapping);
    else if (mode==HC_BIGtoGB)
	add(BtoG, mBtoG, code - BIGfirst, mapping);
}

/*
  Look up the code in the single/multiple mapping table for index i,
  and put the result in the result array of size n.
*/
static int cvrt(a1, am, i, result, n)
    u_int16 *a1, **am, i, *result;
{
    int k = 0;
    u_int16 x, codeDes = a1[i];
    
    if (codeDes == 0) return(0);
    result[0] = codeDes;
    if (am[i])
	while (x = am[i][k])
	{
	    if (k>=n) break;
	    result[++k] = x;
	}
    return k + 1;
}


int hc_convert(mode, codeSrc, result, n)
    int mode;
    u_int16 codeSrc, *result;
    int n;
{
    if (n<=0) return -2;
     if (mode == HC_GBtoBIG)
    {
	result[0] = b5_default_code;
	if (inGBrange(codeSrc)) 
	    return cvrt(GtoB, mGtoB, codeSrc - GBfirst, result, n);
    }	
    else if (mode == HC_BIGtoGB)
    {
	result[0] = gb_default_code;
	if (inBIGrange(codeSrc))
	    return cvrt(BtoG, mBtoG, codeSrc - BIGfirst, result, n);
    }
    return(-1);		/* unconverted due to error */
}


int hc_convert_fp(ifp, ofp, mode, do_mult)
    FILE *ifp, *ofp;
    int mode, do_mult;
{
    int c1, c2, n;
    long unconverted = 0;
    u_int16 result[BUFSIZE];

    while ((c1=fgetc(ifp))!=EOF)
    {
	if (!HC_ISFIRSTBYTE(c1)) fputc(c1, ofp);
	else
	{
	    c2 = fgetc(ifp);
	    if ((n=hc_convert(mode, DB(c1, c2), result, BUFSIZE))<=0)
		++unconverted;
	    if ((n<=1) || (do_mult==HC_DO_SINGLE) ||
		((do_mult==HC_DO_ALL_BUT_SYMBOLS) &&
		 (((mode == HC_GBtoBIG) && (HC_IS_GB_SYMBOL(DB(c1,c2)))) ||
		  ((mode == HC_BIGtoGB) && (HC_IS_BIG_SYMBOL(DB(c1,c2)))))))
	    {
		fputc(HC_HB(result[0]), ofp);
		fputc(HC_LB(result[0]), ofp);
	    }
	    else 
	    {
		fprintf(ofp, "<<");
		for (c1=0; c1<n; c1++)
		{
		    fputc(HC_HB(result[c1]), ofp);
		    fputc(HC_LB(result[c1]), ofp);
		}
		fprintf(ofp, ">>");
	    }
	}
    }
    return(unconverted);
}


u_int16 hc_convert1(mode, code)
    int mode;
    u_int16 code;
{
    u_int16 result[BUFSIZE];

    hc_convert(mode, code, result, BUFSIZE);
    return(result[0]);
}

    
