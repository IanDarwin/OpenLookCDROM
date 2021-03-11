/* @(#)$Header: /crl/audio/AF/lib/AFSox/RCS/st.h,v 1.4 1994/04/05 21:31:40 tml Exp $ */
/***********************************************************
$Copyright$,1994 by Tektronix, Inc., Wilsonville, Oregon.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Tektronix or Tek not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

TEKTRONIX DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
TEKTRONIX BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/
/*
 * July 5, 1991
 * Copyright 1991 Lance Norskog And Sundry Contributors
 * This source code is freely redistributable and may be used for
 * any purpose.  This copyright notice must be maintained. 
 * Lance Norskog And Sundry Contributors are not responsible for 
 * the consequences of using this software.
 */

#ifndef _st_H_
#define _st_H_

#include <AF/audio.h>

#ifdef VAXC
#define IMPORT  globalref
#define EXPORT  globaldef
/*
 * use the VAX C optimized functions 
 */ 
#define calloc  VAXC$CALLOC_OPT
#define cfree   VAXC$CFREE_OPT
#define free    VAXC$FREE_OPT
#define malloc  VAXC$MALLOC_OPT
#define realloc VAXC$REALLOC_OPT
#else
#define IMPORT  extern
#define EXPORT 
#endif


/*
 * Sound Tools sources header file.
 */

#include <stdio.h>

#ifdef AMIGA
#include "amiga.h"
#endif /* AMIGA */

/*
 * Handler structure for each format.
 */

typedef struct format {
	char	**names;	/* file type names */
	int	(*startread)();			
	int	(*read)();			
	int	(*stopread)();		
	int	(*cvt)();
	int	(*startwrite)();			
	int	(*write)();
	int	(*stopwrite)();		
} format_t;

IMPORT format_t formats[];

/* Signal parameters */

struct  signalinfo {
	AEncodeType 	type;		/* sample type, MU255 etc */
	long		rate;		/* sampling rate */
	int		size;		/* word length of data */
	int		style;		/* format of sample numbers */
	int		channels;	/* number of sound channels */
	int 		mode;		/* play or record */
};

/* Pipe parameters */

struct	pipeinfo {
	FILE	*pout;			/* Output file */
	FILE	*pin;			/* Input file */
};

#define	PRIVSIZE	100

/*
 * Handler structure for each effect.
 */

typedef struct {
	char	*name;			/* effect name */
	int	flags;			/* this and that */
	int	(*getopts)();		/* process arguments */
	int	(*start)();		/* start off effect */
	int	(*flow)();		/* do a buffer */
	int	(*drain)();		/* drain out at end */
	int	(*stop)();		/* finish up effect */
} effect_t;

IMPORT effect_t effects[];

struct effect {
	char		*name;		/* effect name */
#if 0
	struct signalinfo ininfo;	/* input signal specifications */
	struct signalinfo outinfo;	/* output signal specifications */
#endif /* 0 */
	effect_t 	h;		/* effects driver */
	char		priv[PRIVSIZE];	/* private area for effect */
};

typedef struct effect *eff_t;

/*
 *  Format information for input and output files.
 */

struct soundstream {
	struct	signalinfo info;	/* signal specifications for input */
	struct	signalinfo outfo;	/* signal specifications for output */
	struct effect eff;
	int hasHeader;
	char *ibuf;
	int  isize;
	char *obuf;
	int  osize;
	char	swap;			/* do byte- or word-swap */
	char	seekable;		/* can seek on this file */
	char	*filename;		/* file name */
	char	*filetype;		/* type of file */
	char	*comment;		/* comment string */
	FILE	*fp;			/* File stream pointer */
	void	(*errfunc)();		/* error handling routine */
	format_t h;			/* format struct for this file */
	char	priv[PRIVSIZE];		/* format's private inp data area */
	char	opriv[PRIVSIZE];	/* format's private outp data area */
};

IMPORT struct soundstream informat, outformat;
typedef struct soundstream *ft_t;

/* Size field */
#define	BYTE	1
#define	WORD	2
#define	LONG	4
#define	FLOAT	5
#define DOUBLE	6
#define IEEE	7		/* IEEE 80-bit floats.  Is it necessary? */

/* Style field */
#define UNSIGNED	1	/* unsigned linear: Sound Blaster */
#define SIGN2		2	/* signed linear 2's comp: Mac */
#define	ULAW		3	/* U-law signed logs: US telephony, SPARC */
#define ALAW		4	/* A-law signed logs: non-US telephony */

/* mode field */
#define PLAYMODE	0
#define RECORDMODE	1

IMPORT char *sizes[], *styles[];

#define	EFF_CHAN	1		/* Effect can mix channels up/down */
#define EFF_RATE	2		/* Effect can alter data rate */
#define EFF_MCHAN	4		/* Effect can handle multi-channel */

#define SAMPLE_RATE_DIFF	200	/* Acceptable sample rate delta */

#define AF_SUCCESS	1
#define AF_FAILURE	-1

#ifdef	__STDC__
#define	P1(x) x
#define	P2(x,y) x, y
#define	P3(x,y,z) x, y, z
#define	P4(x,y,z,w) x, y, z, w
#else
#define P1(x)
#define P2(x,y)
#define P3(x,y,z)
#define P4(x,y,z,w)
#endif

/* Utilities to read and write shorts and longs little-endian and big-endian */
unsigned short rlshort(P1(ft_t ft));			/* short little-end */
unsigned short rbshort(P1(ft_t ft));			/* short big-end    */
unsigned short wlshort(P2(ft_t ft, unsigned short us));	/* short little-end */
unsigned short wbshort(P2(ft_t ft, unsigned short us));	/* short big-end    */
unsigned long  rllong(P1(ft_t ft));			/* long little-end  */
unsigned long  rblong(P1(ft_t ft));			/* long big-end     */
unsigned long  wllong(P2(ft_t ft, unsigned long ul));	/* long little-end  */
unsigned long  wblong(P2(ft_t ft, unsigned long ul));	/* long big-end     */
/* Read and write words and longs in "machine format".  Swap if indicated.  */
unsigned short rshort(P1(ft_t ft));			
unsigned short wshort(P2(ft_t ft, unsigned short us));
unsigned long  rlong(P1(ft_t ft));		
unsigned long  wlong(P2(ft_t ft, unsigned long ul));
/* Utilities to byte-swap values */
unsigned short swapw(P1(unsigned short us));		/* Swap short */
unsigned long  swapl(P1(unsigned long ul));		/* Swap long */

IMPORT void report(P2(char *, ...));
IMPORT int fail(P3(ft_t,char *, ...));
IMPORT void fatal(P3(ft_t,char *, ...));

typedef	unsigned int u_i;
typedef	unsigned long u_l;
typedef	unsigned short u_s;

#define	MAXRATE	50L * 1024			/* maximum sample rate */

#if  defined(unix) || defined (__OS2__)
/* Some wacky processors don't have arithmetic down shift, so do divs */
#define LEFT(datum, bits)	(datum << bits)
/* Most compilers will turn this into a shift if they can, don't worry */
/* #define RIGHT(datum, bits)	(datum / (1L << bits)) /* use maybe? */
#define RIGHT(datum, bits)	(datum >> bits)
#else
/* x86 & 68k PC's have arith shift ops and dumb compilers */
#define LEFT(datum, bits)	(datum << bits)
#define RIGHT(datum, bits)	(datum >> bits)
#endif

#ifndef	M_PI
#define M_PI	3.14159265358979323846
#endif

#if	defined(__osf__) || defined(unix) || defined(AMIGA) \
     || defined (__OS2__) || defined(OS9)
#define READBINARY	"r"
#define WRITEBINARY	"w"
#endif
#ifdef	VMS
#define READBINARY      "r", "mbf=16", "ctx=stm" 
#define WRITEBINARY     "w", "ctx=stm"
#endif
#ifdef	DOS
#define READBINARY	"rb"
#define WRITEBINARY	"wb"
#endif

/* Error code reporting */
#ifdef	QNX
#include <errno.h>
#endif

#if defined(unix) || defined(__OS2__)
#include <errno.h>
extern errno;
#if defined(i386) || !defined(__STDC__)
/* usually be a const in stdlib.h ?? */
/* this is turning out to be a headache */
extern char *sys_errlist[];
#define strerror(errno)	sys_errlist[errno]
#endif
#endif

#ifdef	__OS2__
#define REMOVE remove
#else
#define REMOVE unlink
#endif

/* ummmm??? */

#ifdef VXWORKS
#define strrchr	rindex
#endif

#define FREQ(ft,aDev) ((ft)->info.mode==PLAYMODE?(aDev)->playSampleFreq:(aDev)->recSampleFreq)
#define CHAN(ft,aDev) ((ft)->info.mode==PLAYMODE?(aDev)->playNchannels:(aDev)->recNchannels)
#define TYPE(ft,aDev) ((ft)->info.mode==PLAYMODE?(aDev)->playBufType:(aDev)->recBufType)
#define NBUF(ft,aDev) ((ft)->info.mode==PLAYMODE?(aDev)->playNSamplesBuf:(aDev)->recNSamplesBuf)
#define STYLE(ft) ((ft)->info.mode==PLAYMODE?(ft)->info.style:(ft)->outfo.style)

#if 0
#define fread(buf,s,c,fp)	read(fp,buf,(s)*(c))
#define fseek(fp,off,whence)	lseek(fp,off,whence)
#undef getc
#define getc(fp)	readc(fp)
#endif /* 0 */

#endif /* ! _st_H_ */
