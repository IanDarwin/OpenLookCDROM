

/*LIBS: -lconsole -lerrors -lutil -lm
*/

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/console/cmd/RCS/consolea.c,v 1.21 1993/12/01 23:05:23 gk5g Exp $";
#endif

/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

 

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
#include <environ.ih>
#include <consolea.eh>

/*
 * This callback is called when the window
 * manager requests to destroy the im.
 */
static void delete_console_win(im, rock)
struct im *im;
long rock;
{
     im_KeyboardExit();
}

boolean consoleapp__InitializeObject(classID,self)
struct classheader *classID;
struct consoleapp *self;
{
    self->consoleName = NULL;
    /* NOTE: the following defines for the version info come from ../lib/convers.h */
    consoleapp_SetMajorVersion(self, MAJORVERSION); 
    consoleapp_SetMinorVersion(self, MINORVERSION);
    return TRUE;
}

void consoleapp__FinalizeObject(classID,self)
struct classheader *classID;
struct consoleapp *self;
{
    if(self->consoleName != NULL){
	free(self->consoleName);
    }
}

/*
 * These 3 routines are the heart of the consoleapp interface
 */
int ForceErrorMonitoring = FALSE;
int InhibitErrorMonitoring = FALSE;

boolean consoleapp__ParseArgs(self,argc,argv)
struct consoleapp *self;
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
	fprintf(stderr,"%s: %s switch requires an argument.\n",consoleapp_GetName(self),*argv);\
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
 		case 'C':
 		    InhibitErrorMonitoring = TRUE;
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
		    fprintf(stderr,"%s: unrecognized switch: %s\n", consoleapp_GetName(self), *argv);
		    return FALSE;
	    }
	else {
	    if(self->consoleName!=NULL){
		fprintf(stderr,"%s: only one console at a time\n", consoleapp_GetName(self));
		return FALSE;
	    }
	    self->consoleName = (char *)malloc(strlen(*argv)+1);
	    strcpy(self->consoleName, *argv);
	}

    return TRUE;
}



boolean consoleapp__Start(self)
struct consoleapp *self;
{
    struct consoleClass *con;
    struct im *im;

    if(!super_Start(self))
	return FALSE;
    PostParseArgs(self->consoleName);
    con = consoleClass_New();
    OneTimeInit(con);
    SetupFromConsoleFile(con,TRUE);
    InitializeGetStats(con);
    if (MaxWidth < MinWidth) MaxWidth = MinWidth;
    if (MaxHeight < MinHeight) MaxHeight = MinHeight;
    im_SetPreferedDimensions(0, 0, MaxWidth, MaxHeight);
    if ((im = im_Create(NULL)) == NULL) {
        fprintf(stderr,"Could not create new window.\nexiting.\n");
        return FALSE;
    }
    im_SetDeleteWindowCallback(im, delete_console_win, 0);
    im_SetView(im, con);
    consoleClass_WantInputFocus(con,con);
    return(TRUE);
}



boolean consoleapp__InitializeClass(classID)
struct classheader *classID;
{
    char *s;
    consoleClass_StaticEntry;
    logview_StaticEntry;
    if ((s = environ_GetConfiguration("InhibitErrorMonitoring")) != NULL) 
	InhibitErrorMonitoring = TRUE;
    if ((s = environ_GetConfiguration("ForceErrorMonitoring")) != NULL) 
	ForceErrorMonitoring = TRUE;
    return(TRUE);
}
