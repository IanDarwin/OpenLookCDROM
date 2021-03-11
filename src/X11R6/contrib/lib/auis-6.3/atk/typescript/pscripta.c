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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/typescript/RCS/pscripta.c,v 2.12 1992/12/15 21:45:38 rr2b R6tape $";
#endif


 

#include <class.h>
#include <pscripta.eh>
#include <app.ih>
#include <tscript.ih>
#include <style.ih>
#include <frame.ih>
#include <im.ih>
#include <fontdesc.ih>

static FILE *outfile = NULL;

boolean pipescriptapp__ParseArgs(self,argc,argv)
struct pipescriptapp *self;
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
	fprintf(stderr,"%s: %s switch requires an argument.\n",pipescriptapp_GetName(self),*argv);\
        return FALSE;\
    }else\
    	var= *++argv;\
}

    while(*++argv!=NULL && **argv=='-')
	switch((*argv)[1]){
	    case 't':
		GETARGSTR(self->titleLine);
		break;
            case 'f':
		GETARGSTR(self->font);
		break;
	    case 'o':
		outfile = stdout;
		break;
	    default:
		fprintf(stderr,"%s: unrecognized switch: %s\n", pipescriptapp_GetName(self), *argv);
		return FALSE;
	    }

    if(*argv!=NULL){
	fprintf(stderr,"%s: unrecognized argument: %s\n", pipescriptapp_GetName(self), *argv);
	return FALSE;
	}

 return TRUE;
}

boolean pipescriptapp__Start(self)
struct pipescriptapp *self;
{
    struct view *vs;
    struct im *im;

    if(!super_Start(self))
	return FALSE;

    self->ss =typescript_CreatePipescript(stdin, outfile, TRUE);

    if (self->font != NULL) {
        char family[256];
        long size, style;
        struct style *defaultStyle = typescript_GetDefaultStyle(self->ss);

        if (defaultStyle == NULL)
            defaultStyle = style_New();
        if (fontdesc_ExplodeFontName(self->font, family, sizeof(family), &style, &size)) {
            style_SetFontFamily(defaultStyle, family);
            style_SetFontSize(defaultStyle, style_ConstantFontSize, size);
            style_ClearNewFontFaces(defaultStyle);
            style_AddNewFontFace(defaultStyle, style);
            typescript_SetDefaultStyle(self->ss, defaultStyle);
        }
    }

    vs = typescript_GetApplicationLayer(self->ss);
    if((self->frame = frame_New()) == NULL) {
	fprintf(stderr,"pipescript: Could not allocate enough memory; exiting.\n");
	return(FALSE);
    }
    if((im = im_Create(NULL)) == NULL) {
	fprintf(stderr,"pipescript: Could not create new window; exiting.\n");
	return(FALSE);
    }
    if (self->titleLine != NULL)
	frame_SetTitle(self->frame, self->titleLine);
    im_SetView(im, self->frame);
    frame_SetView(self->frame,vs);
    typescript_WantInputFocus(self->ss,self->ss);

    return TRUE;
}

boolean pipescriptapp__InitializeObject(classID,self)
struct classheader *classID;
struct pipescriptapp *self;
{
    pipescriptapp_SetName(self, "pipescript");
    self->filemenu = 1;
    self->titleLine = NULL;
    self->font = NULL;
    self->ShellMenu = 0;
    self->df = NULL;
    self->ss = NULL;
    self->Menu = NULL;
    self->frame = NULL;
    self->argv=NULL;
    pipescriptapp_SetMajorVersion(self, 7);
    pipescriptapp_SetMinorVersion(self, 2);

    return TRUE;
}
