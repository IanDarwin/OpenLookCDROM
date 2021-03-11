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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/examples/ex17/RCS/hello.c,v 2.8 1992/12/15 21:33:36 rr2b R6tape $";
#endif

#include <stdio.h>
#include <class.h>

#include "hello.eh"

#include "dataobj.ih"
#include "text.ih"
#include "style.ih"
#include "fontdesc.ih"

struct dataobject *createInitialDobj()
{
    struct text *text=text_New();
    struct style *bold=style_New(),*italic=style_New();

    style_SetName(bold,"bold");
    style_AddNewFontFace(bold,fontdesc_Bold);

    style_SetName(italic,"italic");
    style_AddNewFontFace(italic,fontdesc_Italic);

    text_InsertCharacters(text,0,"Hello world!",sizeof("Hello world!")-1);

    text_AddStyle(text,0,5,bold);
    text_AddStyle(text,6,5,italic);

    return (struct dataobject *)text;
}

boolean helloworld__InitializeObject(classID,hw)
struct classheader *classID;
struct helloworld *hw;   
{
    hw->x = POSUNDEF;
    hw->y = POSUNDEF;
    hw->blackOnWhite = TRUE;
    hw->dobj=createInitialDobj();

    return TRUE;
}

void helloworld__FinalizeObject(classID,hw)
struct classheader *classID;
struct helloworld *hw;
{
    dataobject_Destroy(hw->dobj);
}

long helloworld__Read(hw,file,id)
struct helloworld *hw;
FILE *file;
long id;
{
    char buf[100],classNameBuf[100];
    long retVal,dobjObjId;

    helloworld_SetID(hw,helloworld_UniqueID(hw));

    if(fgets(buf,sizeof(buf),file)==NULL ||
       /* the %hd tells scanf that blackOnWhite is a short, not an int */
       sscanf(buf,"%d %d %hd\n",&hw->x,&hw->y,&hw->blackOnWhite)<3 ||
       fgets(buf,sizeof(buf),file)==NULL ||
       sscanf(buf,"\\begindata{%[^,],%d}\n",classNameBuf,&dobjObjId)<2)
	retVal=dataobject_PREMATUREEOF;
    else{
	retVal=dataobject_Read(hw->dobj,file,id);
	if(retVal==dataobject_NOREADERROR)
	    if(fgets(buf,sizeof(buf),file)==NULL) /* read in the \enddata{...} */
		retVal=dataobject_MISSINGENDDATAMARKER;
    }

    return retVal;
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
	dataobject_Write(hw->dobj,file,writeId,level);
	fprintf(file,"\\enddata{%s,%d}\n",
		class_GetTypeName(hw), helloworld_UniqueID(hw));
    }

    return helloworld_UniqueID(hw);
}
