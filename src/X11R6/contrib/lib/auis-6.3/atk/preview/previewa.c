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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/preview/RCS/previewa.c,v 2.17 1993/09/24 21:26:19 gk5g Exp $";
#endif


 

/*
*
*	Main module for BE2 preview.
*		A program for previewing dvitroff input
*
*/
#include <andrewos.h>
#include <class.h>
#include <previewa.eh>
#include <app.ih>
#include <preview.ih>
#include <im.ih>
#include <signal.h>

char *DviBaseName=NULL;
char *DviFileName=NULL;
boolean DoScaling = TRUE;
FILE *filein;
boolean DviFileComplete;
boolean debug;

boolean previewapp__InitializeObject(classID,self)
struct classheader *classID;
struct previewapp *self;
{
    previewapp_SetMajorVersion(self, 7);
    previewapp_SetMinorVersion(self, 0);
    return TRUE;
}

boolean previewapp__ParseArgs(self,argc,argv)
struct previewapp *self;
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
	fprintf(stderr,"%s: %s switch requires an argument.\n",previewapp_GetName(self),*argv);\
        return FALSE;\
    }else\
    	var= *++argv;\
}

    while(*++argv!=NULL){
	if(**argv=='-')
	    switch((*argv)[1]){
		case 'f':
		    DoScaling = FALSE;
		    break;
		case 'o':
		    GETARGSTR(DviBaseName);
		    break;
		default:
		    fprintf(stderr,"%s: unrecognized switch: %s\n", previewapp_GetName(self), *argv);
		    return FALSE;
	    }
	else{
	    DviFileName= *argv;
	    if(DviBaseName==NULL)
		DviBaseName= *argv;
	    DviFileComplete = TRUE;
	    if (debug)
		fprintf(stderr, "Dvi File is %s\n", DviFileName);
	}	    
    }

    return TRUE;
}

static struct preview *pv;
#if POSIX_ENV
void FinishUp
#ifdef __STDC__
(int sig)
#else
(sig)
int sig;
#endif
#else
FinishUp()
#endif
{
    preview_FinalizeObject(pv);
    signal(SIGTERM, SIG_DFL);
}

boolean previewapp__Start(self)
struct previewapp *self;
{
    struct view *v;
    struct im *im;
#ifdef POSIX_ENV
    struct sigaction termVector;
    sigset_t sigset;
#else
    struct sigvec termVector;
#endif
#ifdef USEFRAME
    struct frame *fr;
#endif /* USEFRAME */

    if(!super_Start(self))
	return FALSE;

    debug = FALSE;

    if(DviFileName==NULL)
	filein=stdin;
    else{
	filein=fopen(DviFileName,"r");
	if(filein==NULL){
	    fprintf(stderr, "Can't open %s\n", DviFileName);
	    return FALSE;
	}
    }

    /*     print_StaticEntry; */
    pv = preview_Create(filein, (DviFileName==NULL ? "" : DviFileName), (DviBaseName==NULL ? "" : DviBaseName), DviFileComplete, DoScaling);
    if(pv == NULL) exit(-1);
    /* pv->debug = debug; */
    v= preview_GetApplicationLayer(pv);
    if((im = im_Create(NULL)) == NULL) {
	fprintf(stderr,"preview: Could not create new window; exiting.\n");
	return(FALSE);
    }
#ifdef USEFRAME
    if((fr = frame_New()) == NULL) {
	fprintf(stderr,"preview: Could not allocate enough memory; exiting.\n");
	return(FALSE);
    }
    frame_SetView(fr, v);
#else /* USEFRAME */
    im_SetView(im,v);
#endif /* USEFRAME */
    preview_WantInputFocus(pv,pv);
#ifdef POSIX_ENV
    termVector.sa_handler = FinishUp;
    sigemptyset(&sigset);
    sigaddset(&sigset, SIGURG);
    termVector.sa_mask = sigset; /* Block redraw signals while doing cleanup. */
    termVector.sa_flags = 0;
    sigaction(SIGTERM, &termVector, NULL);
#else
    termVector.sv_handler = FinishUp;
    termVector.sv_mask = (1 << (SIGURG - 1)); /* Block redraw signals while doing cleanup. */
    termVector.sv_onstack = 0;
    sigvec(SIGTERM, &termVector, NULL);
#endif
    return TRUE;
}

previewapp__Run(self)
struct previewapp *self;
{
    if(!previewapp_Fork(self))
	return -1;
    while(TRUE){
	im_KeyboardProcessor();
	if(preview_ReadyToQuit(pv)) break;
    }
    preview_FinalizeObject(pv);
    exit(0);
}
