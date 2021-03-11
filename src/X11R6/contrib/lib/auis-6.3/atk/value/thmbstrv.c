/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1989 - All Rights Reserved      *
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/value/RCS/thmbstrv.c,v 2.8 1992/12/15 21:47:14 rr2b R6tape $";
#endif


 

#include <class.h>
#include <value.ih>
#include <thmbstrv.eh>

char *thumbstrV__GetValueString(self)
struct thumbstrV *self;
{
    struct value *w = thumbstrV_Value(self);
    long len,val;
    char **arr;
    if(((len = value_GetArraySize(w)) == 0) || 
	((arr = value_GetStringArray(w)) == NULL) || 
	  ((val = thumbstrV_GetTmpVal(self))< 0) ||
	  val >= len){
	return super_GetValueString(self);
    }
    return(arr[val]);
}
struct thumbstrV * thumbstrV__DoHit( self,type,x,y,hits )
struct thumbstrV * self;
enum view_MouseAction type;
long x,y,hits;
{
    int mv = value_GetArraySize(thumbstrV_Value(self));
    struct thumbV *sf = (struct thumbV *) self;
    if(mv > 0 && mv != sf->maxval + 1 ){
	int diff ;
	sf->maxval = mv -1 ;
	diff = sf->maxval - sf->minval;
	if(diff < 20) sf->granular = 6;
	else if(diff < 50) sf->granular = 4;
	else if(diff < 100) sf->granular = 2;
	else sf->granular = 0;
    }
    return super_DoHit( self,type,x,y,hits );
}
