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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/snap2/pfio/RCS/pfpack.c,v 1.13 1993/09/22 19:44:14 gk5g Exp $";
#endif

#include <andrewos.h>
#include <pfio.h>
#include <varargs.h>
#include <pfioseg.h>

/*
 * pack arguments for network transfer
 */
void PFpack(va_alist)
va_dcl
{
    va_list ap;
    register atype;
    register along;
    char *astr;
    PFM_pt o;
    va_start(ap);
    o=va_arg(ap,PFM_pt);
    atype=va_arg(ap,int);  
    along=va_arg(ap,int);
    if(atype!=LS_alocop)
	(*(o->parerr))(o,"PFIO:invalid opcode %d",atype);
    PF_aloc(o,along);
    o->buf+=MIN_PAK;		/*skip the length we will come back to it*/
    
    /* pack up the argument list*/
    while(TRUE) {
        if(PFM_remaining(*o)<4)
	  (*(o->parerr))(o,"PFIO:output reserve too small");
	atype=va_arg(ap,int);
   	switch(atype) {
	    case LS_dropoff:
		goto end_of_arg_list;
	    case LS_int32:
		along=va_arg(ap,long);
		*(o->buf++) = along >> 24;
		*(o->buf++) = along >> 16;
		*(o->buf++) = along >> 8;
		*(o->buf++) = along ;
		break;
	    case LS_int16:
		along=va_arg(ap,long);
   		*(o->buf++) = along >> 8;
		*(o->buf++) = along ;
		break;
	    case LS_int8:
		along=va_arg(ap,long);
		*(o->buf++) = along ;
		break;
	    case LS_cstr:
		along=va_arg(ap,long);	/*string length*/
		if(PFM_remaining(*o)<(along+2))
		  (*(o->parerr))(o,"PFIO:output reserve too small");
   		*(o->buf++) = along >> 8;
		*(o->buf++) = along ;
		astr=va_arg(ap,char *);	/*string to send*/
		bcopy(astr,o->buf,((int)along)); /*put on the string*/
		o->buf+=along;	/*and advace output*/
		*(o->buf++)=0;	/*null terminate it*/
		break;
	    case LS_rstr:		/*rest of packet as string*/
		along=va_arg(ap,long);	/*string length*/
		astr=va_arg(ap,char *);	/*string to send*/
		if(PFM_remaining(*o)<(along+2))
		  (*(o->parerr))(o,"PFIO:output reserve too small");
		bcopy(astr,o->buf,((int)along)); /*put on the string*/
		o->buf+=along;	/*and advace output*/
		*(o->buf++)=0;	/*null terminate it*/
		goto end_of_arg_list;
	    default:
		(*(o->parerr))(o,"invalid send opcode %d",atype);
	}
    }

/*
 * all the arguments have been packed
 * backpatch the length
 */
end_of_arg_list:
    PF_dropoff(o);	/*queue this block up for writing*/
}
