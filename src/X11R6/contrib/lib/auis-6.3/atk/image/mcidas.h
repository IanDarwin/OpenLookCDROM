/* mcidas.h
 */

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/


/* Argh, looks like the numbers are in vax byte order */

#define TYPELEN		4 /* Short strings used as identifiers */
#define COMMENTLEN	32	/* longer strings */
/*
 * McIdas AREA DIRECTORY, based on documentation dated 5/87 by R. Dengal
 */
struct area_dir {
/*  1 */ unsigned long	status ;
/*  2 */ unsigned long	type ;
/*  3 */ unsigned long	satid ;
/*  4 */ unsigned long	ndate ; /* YYDDD */
/*  5 */ unsigned long	ntime ; /* HHMMSS */
/*  6 */ unsigned long	lcor ;
/*  7 */ unsigned long	ecor ;
/*  8 */ unsigned long	zcor ;
/*  9 */ unsigned long	lsiz ;
/* 10 */ unsigned long	esiz ;
/* 11 */ unsigned long	zsiz ;
/* 12 */ unsigned long	lres ;
/* 13 */ unsigned long	eres ;
/* 14 */ unsigned long	bands ;
/* 15 */ unsigned long	yzprefix ;
/* 16 */ unsigned long	projnum ;
/* 17 */ unsigned long	cdate ;
/* 18 */ unsigned long	ctime ;
/* 19 */ unsigned long	filtmap ;
/* 20 */ unsigned long	imageid ;
/* 21 */ unsigned long	resvid[4] ;
#define AREA_COMMENTS 24
/* 25 */ char	comments[COMMENTLEN] ;
#define AREA_CALKEY 32
/* 33 */ unsigned long	calkey ;
/* 34 */ unsigned long	navkey ;
/* 35 */ unsigned long	navkey2 ;
/* 36 */ unsigned long	lprefix ;
/* 37 */ unsigned long	pdl[8] ;
/* 45 */ unsigned long	band8 ;
/* 46 */ unsigned long idate ;
/* 47 */ unsigned long itime ;
/* 48 */ unsigned long	startscan ;
/* 49 */ unsigned long	doclen ;
/* 50 */ unsigned long	callen ;
/* 51 */ unsigned long	levlen ;
#define AREA_STYPE 51
/* 52 */ char	stype[TYPELEN] ;
/* 53 */ char	ctype[TYPELEN] ;
/* 54 */ unsigned long	reserved[11] ;
} ;

/*
 * McIdas NAVIGATION CODICIL, based on documentation dated 5/87 by D. Santek
 *  Only type 'GOES' used here currently
 */
struct navigation {
/*   1 */ char	type[TYPELEN] ;
#define NAV_DATA 1
/*   2 */ unsigned long iddate ;
/*   3 */ unsigned long itime ;
/*   4 */ unsigned long fill[37] ; /* expand this later, if needed */
#define NAV_RESERVED 40
/*  41 */ unsigned long	reserved[80] ;
/* 121 */ char	memo[COMMENTLEN] ;
} ;

struct mc_area {
	struct area_dir *dir ;
	struct navigation *nav ;
	unsigned char *image ;
	unsigned char *priv ; /* conveninence pointer */
} ;
