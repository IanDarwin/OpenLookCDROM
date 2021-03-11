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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/examples/ex16/RCS/hello.c,v 2.8 1992/12/15 21:33:36 rr2b R6tape $";
#endif

#include <stdio.h>
#include <class.h>

#include "hello.eh"

boolean helloworld__InitializeObject(classID,hw)
struct classheader *classID;
struct helloworld *hw;   
{
    hw->x=POSUNDEF;
    hw->y=POSUNDEF;
    hw->blackOnWhite=TRUE;
    return TRUE;
}

long helloworld__Read(hw,file,id)
struct helloworld *hw;
FILE *file;
long id;
{
    char buf[100];

    helloworld_SetID(hw,helloworld_UniqueID(hw));

    if(fgets(buf,sizeof(buf),file)==NULL)
	return dataobject_PREMATUREEOF;
    /* the %hd tells scanf that blackOnWhite is a short, not an int */
    if(sscanf(buf,"%d %d %hd\n",&hw->x,&hw->y,&hw->blackOnWhite)<3)
	return dataobject_PREMATUREEOF;

    if(fgets(buf,sizeof(buf),file)==NULL) /* read in the \enddata{...} */
	return dataobject_MISSINGENDDATAMARKER;

    return dataobject_NOREADERROR;
}

long helloworld__Write(hw,file,writeId,level)
struct helloworld *hw;
FILE *file;
long writeId;
int level;
{
    if(writeId!=helloworld_GetWriteID(hw)){ /* only write a given version once */
	helloworld_SetWriteID(hw,writeId);
	fprintf(file,"\\begindata{%s,%d}\n",
		class_GetTypeName(hw), helloworld_UniqueID(hw));
	fprintf(file,"%d %d %d\n",hw->x,hw->y,hw->blackOnWhite);
	fprintf(file,"\\enddata{%s,%d}\n",
		class_GetTypeName(hw), helloworld_UniqueID(hw));
    }

    return helloworld_UniqueID(hw);
}
