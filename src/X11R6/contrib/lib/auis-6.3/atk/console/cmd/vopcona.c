/*LIBS: -lconsole -lerrors -lutil -lcont -lvint -lr -llwp -lm
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/console/cmd/RCS/vopcona.c,v 1.19 1993/11/22 20:39:06 gk5g Exp $";
#endif


 

/* 
 * console application class - to run dynamically loaded console with
 * shared libraries.
 */

#include <andrewos.h>
#include <class.h>
#include <app.ih>
#include <view.ih>
#include <im.ih>
#include <conclass.ih>
#include <logv.ih>
#include <initglbs.h>
#include <convers.h>
#include <proctbl.ih>
#include <vopcona.eh>


boolean vopconapp__InitializeObject(classID,self)
struct classheader *classID;
struct vopconapp *self;
{
    self->consoleName = NULL;
    vopconapp_SetMajorVersion(self, MAJORVERSION);
    vopconapp_SetMinorVersion(self, MINORVERSION);
    return TRUE;
}

void vopconapp__FinalizeObject(classID,self)
struct classheader *classID;
struct vopconapp *self;
{
    if(self->consoleName != NULL){
	free(self->consoleName);
    }
}

/*
 * These 3 routines are the heart of the vopconapp interface
 */
int ForceErrorMonitoring = FALSE;

boolean vopconapp__ParseArgs(self,argc,argv)
struct vopconapp *self;
int argc;
char **argv;
{
    if(!super_ParseArgs(self,argc,argv))
	return FALSE;

#define GETARGSTR(var)\
{\
    if((*argv)[2]!='\0')\
        var= ((*argv)[2]=='=' ? &(*argv)[3] : &(*argv)[2]);\
    else if(argv[1]==NULL){\
	fprintf(stderr,"%s: %s switch requires an argument.\n",vopconapp_GetName(self),*argv);\
        return FALSE;\
    }else\
    	var= *++argv;\
}

    while(*++argv!=NULL)
	if(**argv=='-')
	    switch((*argv)[1]){
		case 'c':
		    ForceErrorMonitoring = TRUE;
		    break;
		case 'D':
		    MYDEBUGGING = TRUE;
		    mydbg(("Entering my debugging mode\n"));
		    break;
		case 'q':
		    HighestPriority = 0;
		    break;
		case 'v':
		    HighestPriority = 6;
		    break;
		case 'V':
		    HighestPriority = 9;
		    break;
		default:
		    fprintf(stderr,"%s: unrecognized switch: %s\n", vopconapp_GetName(self), *argv);
		    return FALSE;
	    }
	else {
	    if(self->consoleName!=NULL){
		fprintf(stderr,"%s: only one console at a time\n", vopconapp_GetName(self));
		return FALSE;
	    }
	    self->consoleName = (char *)malloc(strlen(*argv)+1);
	    strcpy(self->consoleName, *argv);
	}

    return TRUE;
}


boolean vopconapp__Start(self)
struct vopconapp *self;
{
    struct consoleClass *con;
    struct im *im;

    if(!super_Start(self))
	return FALSE;

    PostParseArgs(self->consoleName);
    con = consoleClass_New();
    OneTimeInit(con);
    SetupFromConsoleFile(con,TRUE);
    if (MaxWidth < MinWidth) MaxWidth = MinWidth;
    if (MaxHeight < MinHeight) MaxHeight = MinHeight;
    im_SetPreferedDimensions(0, 0, MaxWidth, MaxHeight);
    if ((im = im_Create(NULL)) == NULL) {
        fprintf(stderr,"Could not create new window.\nexiting.\n");
        return FALSE;
    }
    im_SetView(im, con);
    consoleClass_WantInputFocus(con,con);
    return(TRUE);
}



boolean vopconapp__InitializeClass(classID)
struct classheader *classID;
{
    consoleClass_StaticEntry;
    logview_StaticEntry;
    return(TRUE);
}
