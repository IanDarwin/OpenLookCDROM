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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/text/RCS/textref.c,v 1.5 1993/10/26 22:43:58 gk5g Exp $";
#endif

#include <andrewos.h>
#include <class.h>
#include <textref.eh>

char * textref__ViewName(self)
struct textref *self;
{
    return "textrefv";
}
static char *textref__GetRef(self,size,buf)
struct textref *self;
long size;
char *buf;
{
    char *c,*name;
    long realsize,i;
    i = 0;
    realsize = textref_GetLength(self);
    while(realsize > 0 && textref_GetChar(self,i) == ' ') {i++; realsize--;}
    if(size > realsize) size = realsize;
    textref_CopySubString(self,i,size,buf, FALSE);
    c = buf;
    if(*c == '#'){
	while(*c != ' ' && *c != '\t' && *c != '\0')c++;
	if (*c == '\0') c = buf;
	else c++;
	if (*c == '\0') c = buf;
    }
    for(name = c; *c != '\0'; c++) if(*c == ' ' || *c == '\t') *c = '-';
    c--;
    while(*c == '-') *c-- = '\0';
    return name;
}
