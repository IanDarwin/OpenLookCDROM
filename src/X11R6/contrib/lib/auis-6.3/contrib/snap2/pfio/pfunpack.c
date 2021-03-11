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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/snap2/pfio/RCS/pfunpack.c,v 1.17 1993/09/22 19:43:52 gk5g Exp $";
#endif

#include <andrewos.h>
#include <pfio.h>
#include <varargs.h>
#include <pfioseg.h>

/*
 * read some bytes from a stream as an unsigned long
 */
ulong PFslurp(amsg,size)
register PFM_pt amsg;
register int size;
{
    register ulong result=0;
    while((size--)>0) {
	if(PFM_remaining(*amsg)<1)
	    (*(amsg->parerr))(amsg,"PFIO:message too short");
	result<<= 8;
	result|=(*(amsg->buf++));
    }
    return result;
}

/*
 * unpack a buffer transmited through the network
 */
void PFunpack(va_alist)
va_dcl
{
    int atype;
    va_list ap;
    PFM_pt amsg;
    char *astr_pt;
    long along;
    va_start(ap);
    amsg=va_arg(ap,PFM_pt);	/*message to read from*/
    /*unpack the arguments*/
    while(TRUE) {
	atype=va_arg(ap,int);
	switch(LR_get_stream(atype)) {
	    case LR_int8:
		along=PFslurp(amsg,1);
		break;
	    case LR_int16:
		along=PFslurp(amsg,2);
		break;
	    case LR_int24:
		along=PFslurp(amsg,3);
		break;
	    case LR_int32:
		along=PFslurp(amsg,4);
		break;
	    case LR_strlen:
		{
		  /*peek ahead to get the length of the following string*/
		  uint8 *curbuf=amsg->buf;
		  along=PFslurp(amsg,2);
		  amsg->buf=curbuf;
		}
		break;
	    case LR_strr:
		astr_pt=((char*)amsg->buf);
		along=PFM_remaining(*amsg)-1;
   		break;
	    case LR_str:
		along=PFslurp(amsg,2);
		if((along<0)||
		   (along>PFM_remaining(*amsg)))
		  (*(amsg->parerr))(amsg,"PFIO:bogus packed string length");
		astr_pt=((char*)amsg->buf);
   		break;
	    case LR_end:
		return;
	    default:
   		(*(amsg->parerr))(amsg,"PFIO:bogus stream list format");
	}

	switch(LR_get_mach(atype)) {
	    case LRh_str:
		{
		register char *outstr;
		outstr=((char*)va_arg(ap,uint8*));
		bcopy(astr_pt,outstr,((int)(along+1)));
		amsg->buf+=along;
		if((*(amsg->buf++)!=0))	/*insure null termination*/
		   (*(amsg->parerr))(amsg,"PFIO:string isn't null terminated");
		}
		break;
	    case LRh_str_pt:
		{
		register char **outstr;
		outstr=va_arg(ap,char**);
		*outstr=astr_pt;
		amsg->buf+=along;
		if((*(amsg->buf++)!=0))	/*insure null termination*/
		   (*(amsg->parerr))(amsg,"PFIO:string isn't null terminated");
		}
		break;
	    case LRh_int8:
		astr_pt=((char*)(va_arg(ap,uint8*)));
		(*astr_pt)=along;
		break;
	    case LRh_int16:
		{
		register uint16 *auint16_pt;
		auint16_pt=va_arg(ap,uint16*);
		(*auint16_pt)=along;
		}
		break;
	    case LRh_int32:
		{
		register uint32 *auint32_pt;
		auint32_pt=va_arg(ap,uint32*);
		(*auint32_pt)=along;
		}
		break;
	    default:
   		(*(amsg->parerr))(amsg,"PFIO:bogus list host format");
	}
    }
}
