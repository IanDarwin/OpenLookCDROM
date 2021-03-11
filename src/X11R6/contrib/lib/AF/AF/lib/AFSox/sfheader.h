/* @(#)$Header: /home/wesw/nwd/terminator/src/av/crl/audio/AF/lib/AFSox/RCS/sfheader.h,v 1.4 1994/01/12 21:07:53 marvinw Exp $ */
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
# define SIZEOF_BSD_HEADER 1024
# define SF_MAGIC 107364
# define SF_LINK 107414
# define SF_SHORT sizeof(short)
# define SF_FLOAT sizeof(float)
# define SF_BUFSIZE	(16*1024)
# define SF_MAXCHAN	4
# define MAXCOMM 512
# define MINCOMM 256

/* Codes for sfcode */
# define SF_END 0
# define SF_MAXAMP 1
# define SF_COMMENT 2
# define SF_LINKCODE 3

typedef struct sfcode {
	short	code;
	short	bsize;
} SFCODE;

typedef struct sfmaxamp {
	float	value[SF_MAXCHAN];
	long	samploc[SF_MAXCHAN];
	long	timetag;
} SFMAXAMP;

typedef struct sfcomment {
	char 	comment[MAXCOMM];
} SFCOMMENT;

typedef struct sflink {
	char 	reality[50];
	long 	startsamp;
	long	endsamp;
} SFLINK;

struct sfinfo {	
	long	  sf_magic;
	float	  sf_srate;
	long	  sf_chans;
	long	  sf_packmode;
/*	char	  sf_codes;		/* BOGUS! */
	SFCODE    sf_codes;
} ;

typedef union sfheader {
	struct  sfinfo sfinfo;
	char	filler[SIZEOF_BSD_HEADER];
} SFHEADER;

static SFCODE	ampcode = {
SF_MAXAMP,
sizeof(SFMAXAMP) + sizeof(SFCODE)
};

# define sfchans(x) (x)->sfinfo.sf_chans
# define sfmagic(x) (x)->sfinfo.sf_magic
# define sfsrate(x) (x)->sfinfo.sf_srate
# define sfclass(x) (x)->sfinfo.sf_packmode
# define sfbsize(x) ((x)->st_size - sizeof(SFHEADER))
# define sfcodes(x) (x)->sfinfo.sf_codes

# define ismagic(x) ((x)->sfinfo.sf_magic == SF_MAGIC)
# define islink(x)  ((x)->sfinfo.sf_magic == SF_LINK)

# define sfmaxamp(mptr,chan) (mptr)->value[chan]
# define sfmaxamploc(mptr,chan) (mptr)->samploc[chan]
# define sfmaxamptime(x) (x)->timetag
# define ismaxampgood(x,s) (sfmaxamptime(x) + 2  >= (s)->st_mtime)

# define sfcomm(x,n) (x)->comment[n]

# define realname(x) (x)->reality
# define startsmp(x) (x)->startsamp
# define endsmp(x) (x)->endsamp
# define sfoffset(x,h) ((x)->startsamp * sfchans(h) * sfclass(h))
# define sfendset(x,h) ((x)->endsamp * sfchans(h) * sfclass(h))

# define sflseek(x,y,z) lseek(x,(z != 0) ? y : ((y) + sizeof(SFHEADER)),z)
# define rheader(x,y) read(x,(char *) y,sizeof(SFHEADER)) != sizeof(SFHEADER)

#define readopensf(name,fd,sfh,sfst,prog,result) \
if ((fd = open(name, 0))  < 0) {  \
	fprintf(stderr,"%s: cannot access file %s\n",prog,name); \
	result = -1;  \
} \
else if (stat(name,&sfst)){ \
	fprintf(stderr,"%s: cannot get status on %s\n",prog,name); \
	result = -1;  \
} \
else if (rheader(fd,&sfh)){ \
	fprintf(stderr,"%s: cannot read header from %s\n",prog,name); \
	result = -1;  \
} \
else if (!ismagic(&sfh)){ \
	fprintf(stderr,"%s: %s not a bsd soundfile\n",prog,name); \
	result = -1;  \
} \
else result = 0;

#define rwopensf(name,fd,sfh,sfst,prog,result,code) \
if ((fd = open(name, code))  < 0) {  \
	fprintf(stderr,"%s: cannot access file %s\n",prog,name); \
	result = -1;  \
} \
else if (rheader(fd,&sfh)){ \
	fprintf(stderr,"%s: cannot read header from %s\n",prog,name); \
	result = -1;  \
} \
else if (!ismagic(&sfh)){ \
	fprintf(stderr,"%s: %s not a bsd soundfile\n",prog,name); \
	result = -1;  \
} \
else if (stat(name,&sfst)){ \
	fprintf(stderr,"%s: cannot get status on %s\n",prog,name); \
	result = -1;  \
} \
else result = 0;

