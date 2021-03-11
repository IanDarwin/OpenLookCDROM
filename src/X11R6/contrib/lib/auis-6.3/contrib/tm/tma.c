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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/tm/RCS/tma.c,v 2.10 1992/12/15 21:57:11 rr2b R6tape $";
#endif


 

#include <andrewos.h>
#include <class.h>

#include <tma.eh>

#include <app.ih>
#include <im.ih>
#include <frame.ih>
#include <tm19.ih>
#include <tmv.ih>
#include <environ.ih>

boolean tmapp__InitializeObject(classID,self)
struct classheader *classID;
struct tmapp *self;
{
    self->args=NULL;
    self->title=NULL;
    self->fileMenus=FALSE;
    self->menufile=NULL;
    self->rows=self->cols=0;
    return  TRUE;
}

boolean tmapp__ParseArgs(self,argc,argv)
struct tmapp *self;
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
	fprintf(stderr,"%s: %s switch requires an argument.\n",tmapp_GetName(self),*argv);\
        return FALSE;\
    }else\
    	var= *++argv;\
}

    while(*++argv!=NULL && **argv=='-')
	switch((*argv)[1]){
		char *temp;
	    case 't':
		GETARGSTR(self->title);
		break;
	    case 'F':
		self->fileMenus=TRUE;
		break;
	    case 'm':
		GETARGSTR(self->menufile);
		break;
	    case 'r':
		GETARGSTR(temp);
		self->rows=atoi(temp);
		break;
	    case 'c':
		GETARGSTR(temp);
		self->cols=atoi(temp);
		break;
	    default:
		fprintf(stderr,"%s: unrecognized switch: %s\n", tmapp_GetName(self), *argv);
		return FALSE;
	}

    if(*argv!=NULL)
	self->args=argv;

    return TRUE;
}

boolean tmapp__Start(self)
struct tmapp *self;
{
    struct tm19 *tm;
    struct tmview *tmv;
    struct im *im;
    struct frame *frame;

    if(!super_Start(self))
	return FALSE;

    if(self->args==NULL){
	static char *args[2];
	args[0]=environ_Get("SHELL");
	if(args[0]==NULL)
	    args[0]="/bin/csh";
	args[1]=NULL;
	self->args=args;
    }

    {
	char *p=strrchr(self->args[0],'/');
	if(p==NULL)
	    im_SetProgramName(self->args[0]);
	else
	    im_SetProgramName(p+1);
    }

    tm=tm19_New();
    if(tm==NULL)
	return FALSE;

    if(self->rows>0 || self->cols>0){
	if(self->rows==0)
	    self->rows=tm19_GetHeight(tm);
	if(self->cols==0)
	    self->cols=tm19_GetWidth(tm);
	tm19_SetScreenSize(tm,self->cols,self->rows);
    }

    tm19_StartProcess(tm,self->args);

    tmv=tmview_New();
    if(tmv==NULL)
	goto destroytm;

    tmview_SetFileMenus(tmv,self->fileMenus);
    tmview_SetTitle(tmv,self->title);

    if(self->menufile!=NULL)
	tmview_ReadShMenus(tmv,self->menufile);
    else{
	char *menupref=environ_GetProfile("shmenu");
	char *home=environ_Get("HOME");
	char buf[500];

	if(menupref!=NULL)
	   tmview_ReadShMenus(tmv,menupref);
	else if(home==NULL ||
		!(sprintf(buf,"%s/.shmenu",home),
		  tmview_ReadShMenus(tmv,buf))){
	    
	    char *fileName;
	    
	    fileName = environ_AndrewDir("/lib/shmenu");
	    tmview_ReadShMenus(tmv,fileName);
	}
    }

    tmview_SetDataObject(tmv,tm);

    frame=frame_New();
    if(frame==NULL) {
	fprintf(stderr,"tm: Could not allocate enough memory; exiting.\n");
	goto destroytmv;
    }

    frame_SetView(frame,tmview_GetApplicationLayer(tmv));
    frame_SetCommandEnable(frame,TRUE);

    im=im_Create(NULL);
    if(im==NULL) {
	fprintf(stderr,"tm: Could not create new window; exiting.\n");
	goto destroyframe;
    }

    im_SetView(im,frame);

    tmview_WantInputFocus(tmv,tmv);

    return TRUE;

destroyframe:
    frame_Destroy(frame);
destroytmv:
    tmview_Destroy(tmv);
destroytm:
    tm19_Destroy(tm);

    return FALSE;
}
