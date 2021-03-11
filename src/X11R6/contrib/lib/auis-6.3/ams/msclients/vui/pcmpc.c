/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

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

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/msclients/vui/RCS/pcmpc.c,v 1.13 1992/12/15 21:23:32 rr2b R6tape $";
#endif


 

/* 
 * This package contains more C library routines either 1.missing from those
 * supplied with the MSC/IBM compilers, or 2. included in libmail or libitc.
 */

#include <andrewos.h>
#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#ifdef IBMPC
#include <bios.h>
#include <conio.h>
#endif /* IBMPC */
#include <vui.h>
#ifdef AFS_ENV
#include <afs/param.h>
#include <afs/errors.h>
#endif /* #ifdef AFS_ENV */
#include <vuidebug.h>

/* This routine to fix a Microsoft Compiler bug that prevents EOF from
   being cleared.
 */
void clearerr(stream)
FILE * stream;
{
        extern unsigned char _osfile[_NFILE];
        stream->_flag &= ~ (_IOERR | _IOEOF );
        _osfile[stream->_file] &= ~2;
}

GetAndPackAllTokens()
{
    return(0);
}

fork()
{
      return(0);
}

SetMallocCheckLevel(n)
int n;
{
}

getuid()
{
    srand(17);
    return(rand());      /* For cuilib.c */
}

getaddr()
{
    srand(17);
    return(rand());      /* For cuilib.c */
}

/******  Shouldn't need. If we really do, use CUI_GetProfileSwitch 
getprofileswitch(s,z)
char *s; int z;
{
        return(0);      * For cuisnap.c *
}
 ********************/

char *rindex (s, c)
char *s, c;
    {
    char *strrchr();
    return (strrchr (s, c));
    }

/* 
        writeall.c -- Do write, resuming if interrupted

        Author: Craig F. Everhart and Nathaniel Borenstein, ITC, CMU
        Written: July, 1986

        (c) Copyright IBM Corporation, 1986
 */
int ULstrcmp(s1, s2)
register char *s1, *s2;
{
/* case INSENSITIVE:  Compare strings:  s1>s2: >0  s1==s2: 0  s1<s2: <0 */

    register char c1,c2;

    for(;;) {
	c1 = *s1++; if (c1 <= 'Z') if (c1 >= 'A') c1 += 040;
	c2 = *s2++; if (c2 <= 'Z') if (c2 >= 'A') c2 += 040;
	if (c1 != c2) break;
	if (c1 == '\0') return(0);
    }
    return(c1 - c2);
}

int ULstrncmp(s1, s2,count)
int count;
register char *s1, *s2;
{
/* case INSENSITIVE:  Compare strings:  s1>s2: >0  s1==s2: 0  s1<s2: <0 */

    register char i,c1,c2;

    for(i=0;i<count;i++) {
	c1 = *s1++; if (c1 <= 'Z') if (c1 >= 'A') c1 += 040;
	c2 = *s2++; if (c2 <= 'Z') if (c2 >= 'A') c2 += 040;
	if (c1 != c2) break;
	if (c1 == '\0') return(0);
    }
    return(c1 - c2);
}

ULsubstr(s1, s2)
char *s1, *s2;
{
    while (*s2 && *s1 &&
	   ((isupper(*s1) ? tolower(*s1) : *s1) ==
	    (isupper(*s2) ? tolower(*s2) : *s2))) {
	++s1; ++s2;
    }
    if (!*s2) return(0);
    return((isupper(*s1) ? tolower(*s1) : *s1) - (isupper(*s2) ? tolower(*s2) : *s2));
}


extern int errno;

int writeall(fd, Buf, NBytes)
int fd;
char *Buf;
int NBytes;
{
        int Code, ToWrite;

        Code = 0;
        ToWrite = NBytes;
        errno = 0;
        while (ToWrite > 0) {
                Code = write(fd, Buf, ToWrite);
                if (Code < 0) return(Code);
                if (Code == ToWrite) return(NBytes);
                if (Code > ToWrite || errno != 0) return(Code + NBytes - ToWrite);
                ToWrite -= Code;
                Buf += Code;
        }
        return(Code);
}

/* 
        fwriteallchars.c -- Do fwrite, resuming if interrupted

        Author: Craig F. Everhart and Nathaniel Borenstein, ITC, CMU
        Written: July, 1986

        (c) Copyright IBM Corporation, 1986
 */


int fwriteallchars(Thing, NItems, stream)
char *Thing;
int NItems;
FILE *stream;
{
        int Code, ToWrite;

        Code = 0;
        ToWrite = NItems;
        errno = 0;
        while (ToWrite > 0) {
                Code = fwrite(Thing, sizeof(char), ToWrite, stream);
                if (Code < 0) return(Code);
                if (Code == 0 && (errno != 0 || ferror(stream) || feof(stream)))
                                return(Code);
                if (Code == ToWrite) return(NItems);
                if (Code > ToWrite || errno != 0 || ferror(stream) || feof(stream))
                        return(Code + NItems - ToWrite);
                ToWrite -= Code;
                Thing += Code;
        }
        return(Code);
}
/* ***************************************************************

 errprntf.c:  Routine for printing Andrew-standard errors.

 author -- Nathaniel Solomon Borenstein, CMU-ITC

                ********************************************
                **** For documentation, see errprntf.h ****
                ********************************************

   Written in February, 1986

   (c) Copyright IBM Corporation, 1986

 */

#include <errprntf.h>

#define CONTROLMAX 1000  /* Longest printf control string */

errprintf(application, type, log, id, format, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18, s19, s20)

int type;
char *application, *log, *id, *format, *s1, *s2, *s3, *s4, *s5, *s6, *s7, *s8,
    *s9, *s10, *s11, *s12, *s13, *s14, *s15, *s16, *s17, *s18, *s19, *s20;
{
    char ControlString[CONTROLMAX], *typestr;
    int numfields;

    if (type < 0 || type > 9) type = 0;
    numfields = 1;
    if (application) numfields = 2;
    if (log) numfields = 3;
    if (id) numfields = 4;
    sprintf(ControlString, "<%%s%%s%%s%%s%%s%%s%%s>%s\n", format);
    if (type == ERR_CRITICAL) {
        typestr = "critical";
    } else if (type <= ERR_WARNING) {
        typestr = "warning";
    } else if (type <= ERR_MONITOR) {
        typestr = "monitor";
    } else {
        typestr = "debug";
    }
    return(safefprintf(stderr, ControlString,
        typestr,
        (numfields > 1) ? ":" : "",
        application ? application : "",
        (numfields > 2) ? ":" : "",
        log ? log : "",
        (numfields > 3) ? ":" : "",
        id ? id : "",
         s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11,
        s12, s13, s14, s15, s16, s17, s18, s19, s20));
}

safefprintf(fp, control, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18, s19, s20)
FILE *fp;
char *control, *s1, *s2, *s3, *s4, *s5, *s6, *s7, *s8, *s9, *s10, *s11, *s12, *s13, *s14, *s15, *s16, *s17, *s18, *s19, *s20;
{
    fprintf(fp, control, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18, s19, s20);
    fflush(fp);
    if (ferror(fp)) {
        fp = freopen("/dev/console", "w", fp);
        if (fp == NULL) return(-1);
        fprintf(fp, control, s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18, s19, s20);
        fflush(fp);
        if (ferror(fp)) return(-1);
    }
    return(0);
}

int vdown(err)
    int err;
{/* 
	ETIMEDOUT:	Venus has timed out the connection to the file server
	ENXIO:	the Venus process handling the kernel device has terminated
	ENOTTY:	Venus doesn't know about this file descriptor;
			probably it's left over from a previous Venus run
	ENODEV:	Volume off line (Venus error code mapping)
	VOFFLINE:	Volume is off line, for reason given in offline message
	VBUSY:	Volume temporarily unavailable; try again; not usually
			propagated to application level.
 */
#ifdef AFS_ENV
	return (err == ETIMEDOUT || err == ENXIO || err == ENOTTY
		|| err == EIO || err == ENODEV || err == VOFFLINE
		|| err == VBUSY || err == 255 || err == -1);
#else /* #ifdef AFS_ENV */
	return (0);
#endif /* #ifdef AFS_ENV */
}

/*
	base64.c -- Routines for packing long ints into ascii base 64 strings

	No guarantees are made about the operations being invertible for
	negative numbers.

	Author: Nathaniel Solomon Borenstein
		Information Technology Center
		Carnegie-Mellon University

	Copyright (c) 1985, IBM Corporation
*/

static char Basis[65] = "0123456789:=ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

#define digitval(c) (isupper(c) ? (12 + (c) - 'A') : (islower(c) ? (38 + (c) - 'a') : ((c) == ':' ? 10 : ((c) == '=' ? 11 : ((c) - '0')))))

char *
convlongto64(num, pad)
/* unsigned */ long num;
/* unsigned */ int pad;
{
    static char Answer[7];

    Answer[6] =	0;	/* Initialize null termination */
    Answer[5] = Basis[(num & 077)];
    Answer[4] = Basis[(num >> 6) & 077];
    Answer[3] = Basis[(num >> 12) & 077];
    Answer[2] = Basis[(num >> 18) & 077];
    Answer[1] = Basis[(num >> 24) & 077];
    Answer[0] = Basis[((num >> 30) & 077) | (pad & 017) << 2];

    return(Answer);
}

/* Note that the following routine throws away the first 4 of 36 bits */

unsigned long
conv64tolong(xnum)
char *xnum;
{
    register int digits;
    unsigned long Answer = 0;

    digits = strlen(xnum);
    if (digits > 6) digits = 6;
    switch(digits) {
	case 6: Answer |= digitval(*xnum) << 30; ++xnum;
	case 5: Answer |= digitval(*xnum) << 24; ++xnum;
	case 4: Answer |= digitval(*xnum) << 18; ++xnum;
	case 3: Answer |= digitval(*xnum) << 12; ++xnum;
	case 2: Answer |= digitval(*xnum) << 6; ++xnum;
	case 1: Answer |= digitval(*xnum);
    }
    return(Answer);
}

char *AndrewDir(str)
char *str;
{
    char fullstr[256];
    
    /* For now return '/usr/andrew'||str */
    
    sprintf(fullstr,"/usr/andrew%s",str);
    return (fullstr);
}

char *memcpy_preserve_overlap(dest, source, len)
char *dest, *source;
int   len;
{
    /* memcpy in MS C 5.0 doesn't handle overlaps */

    memmove(dest, source, len);
    return dest;
}

PrinterAvailable()
{
#define LPT1 0
#define LPT2 1
#define LPT3 2

unsigned printer_status;
unsigned printer_data = 36;

    printer_status = _bios_printer(_PRINTER_STATUS, LPT1, printer_data);
    if ((printer_status & 0x0F) == 0) return TRUE;

    printer_status = _bios_printer(_PRINTER_STATUS, LPT2, printer_data);
    if ((printer_status & 0x0F) == 0) return TRUE;

    printer_status = _bios_printer(_PRINTER_STATUS, LPT3, printer_data);
    if ((printer_status & 0x0F) == 0) return TRUE;

    return FALSE;
}
