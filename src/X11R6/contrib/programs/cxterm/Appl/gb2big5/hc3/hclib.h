/*
 * NAME:
 *	hclib.h - Hanzi Converter Version 3.0 library header file
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
 *	See below.
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

#ifndef _HC_H
#define _HC_H

#define	HC_GBtoBIG	1
#define HC_BIGtoGB	2

#define HC_DO_SINGLE		0
#define HC_DO_ALL		1
#define HC_DO_ALL_BUT_SYMBOLS	2

#define HC_HB(x)	(((x)>>8) & 0xFF)
#define HC_LB(x)	((x) & 0xFF)
#define HC_ISFIRSTBYTE(c)	(((unsigned char)c)>=0xA1)

#define HC_GB1	0xB0A1  /* starting code of GB Level 1 hanzi */
#define HC_BIG1	0xA440  /* starting code of BIG5 frequently-used hanzi */

/* Macros to determine if a code is BIG5 or GB symbol. */
#define HC_IS_GB_SYMBOL(x)	((x)<HC_GB1)
#define HC_IS_BIG_SYMBOL(x)	((x)<HC_BIG1)


typedef unsigned int u_int16;

/* Read in HC conversion table. */
extern long hc_readtab(/* char *filename */);

/* Clear all table entries. */
extern void hc_clear_tabs();

/* Clear a table entry, code is considered in GB if mode=HC_GBtoBIG */
/* and in BIG5 if mode=HC_BIGtoGB. */
extern void hc_clear_tab_entry(/* int mode, u_int16 code */);


/* Add a table entry, code is considered in GB if mode=HC_GBtoBIG */
/* and in BIG5 if mode=HC_BIGtoGB. */
extern void hc_add_tab_entry(/* int mode, u_int16 code, u_int16 mapping */);


/* Set default code and returns the current one;
   code is considered GB if mode=HC_GBtoBIG and in BIG5 if mode=HC_BIGtoGB. */
extern u_int16 hc_set_default_code(/* int mode, u_int code */);


/* Convert a hanzi code according to mode, and put the resulting mappings
   into the (user allocated) array 'result' of size 'n' (a small array, like
   size 16 or 32 would be more than enough since usually there are not many
   multiple mappings).
   
   Meaning of return code:
   > 0 - the number of mappings;
   = 0 - no conversion and the code of the default code
         is stored into result[0]; and
   < 0 - for invalid code.
*/
extern int hc_convert(/* int mode, u_int16 code, u_int16* result, int n */);

/* Convert a file stream of hanzi, depends on mode (HC_GBtoBIG or HC_BIGtoGB).
   Hanzi that cannot be converted is replaced by a "box" character code. 
   If do_mult is true, all multiple mappings will be shown in <<...>>.
*/
extern hc_convert_fp(/* FILE *ifp, FILE *ofp, int mode, int do_mult */);

/* Convert a hanzi code according to mode, and return the converted code.
   In case of multiple mappings, an arbitrary one is returned.
   Returns the "box" code for hanzi that cannot be converted or in case
   of invalid code.
*/
extern u_int16 hc_convert1(/* int mode, u_int16 code */);

#endif /* _HC_H */


