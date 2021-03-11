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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/lib/RCS/verbose.c,v 1.5 1992/12/15 21:11:36 rr2b R6tape $";
#endif

#include <stdio.h>
#include <varargs.h>

static int     *Threshold = NULL;
static unsigned int *Mask = NULL;
static char    *Prefix = NULL;
static FILE    *Stream = NULL;

void            Verbose_SetUp(stream, prefix, threshold, mask)
FILE           *stream;
char           *prefix;
int            *threshold;
unsigned int   *mask;
{
    Stream = stream;
    Prefix = prefix;
    Threshold = threshold;
    Mask = mask;
}

/*
 * How to call this function:
 *
 * Verbose([stream], [level], [bits], fmt, datum1, datum2, ..., datumN)
 *
 * Stream is given iff no stream was specified with Verbose_SetUp
 * Level is given iff a threshold was specified with Verbose_SetUp
 * Bits is given iff a mask was specified with Verbose_SetUp
 * The rest works mostly like printf, with no modifiers to the % controls
 * (no "%15s" or "%ld", etc).  This restriction is likely to change.
 */

int             Verbose(va_alist) va_dcl
{
    va_list         ap;
    FILE           *stream;
    int             level, numConverted = -1;
    unsigned int    bits;
    char           *fmt, *p;
    long            dint;
    double          dfloat;
    char            dchar, *dstring;
    unsigned long   duint;

    va_start(ap);

    bits = 0;
    level = 0;
    if (Stream)
        stream = Stream;
    else
        stream = va_arg(ap, FILE *);
    if (Threshold)
        level = va_arg(ap, int);
    if (Mask)
        bits = va_arg(ap, unsigned int);
    if (((!Threshold) && (!Mask))
        || (Threshold && (level <= (*Threshold)))
        || (Mask && (bits & (*Mask)))) {
        numConverted = 0;
        fmt = va_arg(ap, char *);
        if (Prefix)
            fputs(Prefix, stream);
        for (p = fmt; *p; ++p) {
            if ((*p) == '%') {
                switch (*(++p)) {
                    case 'd':
                        dint = va_arg(ap, long);
                        fprintf(stream, "%d", dint);
                        ++numConverted;
                        break;
                    case 'o':
                        dint = va_arg(ap, long);
                        fprintf(stream, "%o", dint);
                        ++numConverted;
                        break;
                    case 'x':
                        dint = va_arg(ap, long);
                        fprintf(stream, "%x", dint);
                        ++numConverted;
                        break;
                    case 'X':
                        dint = va_arg(ap, long);
                        fprintf(stream, "%X", dint);
                        ++numConverted;
                        break;
                    case 'f':
                        dfloat = va_arg(ap, double);
                        fprintf(stream, "%f", dfloat);
                        ++numConverted;
                        break;
                    case 'e':
                        dfloat = va_arg(ap, double);
                        fprintf(stream, "%e", dfloat);
                        ++numConverted;
                        break;
                    case 'E':
                        dfloat = va_arg(ap, double);
                        fprintf(stream, "%E", dfloat);
                        ++numConverted;
                        break;
                    case 'g':
                        dfloat = va_arg(ap, double);
                        fprintf(stream, "%g", dfloat);
                        ++numConverted;
                        break;
                    case 'G':
                        dfloat = va_arg(ap, double);
                        fprintf(stream, "%G", dfloat);
                        ++numConverted;
                        break;
                    case 'c':
                        dchar = va_arg(ap, char);
                        fputc(dchar, stream);
                        ++numConverted;
                        break;
                    case 's':
                        dstring = va_arg(ap, char *);
                        fputs(dstring, stream);
                        ++numConverted;
                        break;
                    case 'u':
                        duint = va_arg(ap, unsigned long);
                        fprintf(stream, "%u", duint);
                        ++numConverted;
                        break;
                    case '\0':
                        break;
                    default:
                        fputc(*p, stream);
                        break;
                }
            }
            else {
                fputc(*p, stream);
            }
        }
        fflush(stream);
    }

    va_end(ap);
    return (numConverted);
}
