
/*
 * @(#)def.h	1.16 12/16/94
 *
 *	(c) Copyright 1993-1994 by Mark Grant. All right reserved.
 *	The author assumes no liability for damages resulting from the 
 *	use of this software, even if the damage results from defects in
 *	this software. No warranty is expressed or implied.
 *
 *	This software is being distributed under the GNU Public Licence,
 *	see the file COPYING for more details.
 *
 *			- Mark Grant (mark@unicorn.com) 29/6/94
 *
 */

#ifndef TRUE
#define TRUE	1
#define FALSE	0
#endif

#if (!defined(PGPTOOLS) || !defined(PGPLIB))
typedef unsigned char	byte;
typedef unsigned short	word16;
typedef unsigned long	word32;
#endif
typedef int		int32;

#define FL_ASCII	0x01
#define FL_SIGN		0x02
#define FL_ENCRYPT	0x04

#define SIG_NONE	0
#define SIG_GOOD	1
#define SIG_BAD		(-1)
#define SIG_NO_KEY	(-2)
#define DEC_BAD_PHRASE	(-3)
#define DEC_NO_KEY	(-4)
#define DEC_BAD_FILE	(-5)

#define ERR_NONE	0
#define ERR_NO_KEY	(-1)
#define ERR_NO_SECRET_KEY	(-2)
/* Following will be (-3) */
#define ERR_BAD_PHRASE	DEC_BAD_PHRASE

#define MORE_SECURE

#define DEFAULT_SECURITY	3
#define MAX_SECURITY		4
#define MIN_SECURITY		1

extern char	*pgp_path();

#define DEFAULT_CHECK_TIME	300

#ifdef SYSV
#define bzero(a,b)	memset((a),0,(b))
#endif
