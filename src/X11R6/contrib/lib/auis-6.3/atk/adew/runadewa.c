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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/adew/RCS/runadewa.c,v 1.8 1993/01/08 16:30:59 rr2b R6tape $";
#endif


 

#include <andrewos.h> /* sys/types.h sys/file.h */

#include <andyenv.h>
#include <class.h>

#include <im.ih>
#include <frame.ih>
#include <dataobj.ih>
#include <buffer.ih>
#include <message.ih>
#include <environ.ih>
#include <app.ih>
#include <proctbl.ih>

#include <runadewa.eh>


static char **Gargv;
static int Gargc;
extern int errno;

boolean runadewapp__InitializeObject(classID,self)
struct classheader *classID;
struct runadewapp *self;
{
    self->initFile=TRUE;
    self->files=NULL;
    self->fileLink= &self->files;
    self->haveBufferInWindow = FALSE;
    self->im = NULL;
    self->frame = NULL;
    self->buffer = NULL;
    runadewapp_SetMajorVersion(self, 7);
    runadewapp_SetMinorVersion(self, 0);
    self->cls = NULL;
    self->func = "go";
    self->title = NULL;
    self->objectname = NULL;
    return TRUE;
}


static void StartupError( string)
    char *string;
{
	fprintf(stderr,"%s\n",string);
	fflush(stderr);
}


static void addFile(self,name,newWin,ro)
struct runadewapp *self;
char *name;
boolean newWin,ro;
{
    struct runadewapp_fileList *fileEntry=
      (struct runadewapp_fileList *) malloc(sizeof(struct runadewapp_fileList));

    fileEntry->filename=name;
    fileEntry->newWindow=newWin;
    fileEntry->readOnly=ro;
    fileEntry->next=NULL;
    *self->fileLink=fileEntry;
    self->fileLink=(&(fileEntry->next));
}
static char *getarg(argv,argc)
char **argv;
int *argc;
{
    int cnt;
    char *opt;
    if(argv[0][2] == '\0'){
	opt = argv[1];
	cnt = 2;
    }
    else {
	opt = (*argv) + 2;
	cnt = 1;
    }
    application_DeleteArgs(argv,cnt);
    *argc -= cnt;
    return opt;
}
boolean runadewapp__ParseArgs(self,argc,argv)
struct runadewapp *self;
int argc;
char **argv;
{
    int maxInitWindows=environ_GetProfileInt("MaxInitWindows", 2);
    boolean useNewWindow = FALSE;
    boolean pendingReadOnly = TRUE;
    Gargc = argc;
    Gargv = argv;

    if(!super_ParseArgs(self,argc,argv))
	return FALSE;


    if (maxInitWindows < 2)
	maxInitWindows = 1;
    argv++;
    while(*argv!=NULL){
	if(**argv=='-')
	    switch((*argv)[1]){
		case 'C': 
		    self->cls = getarg(argv,&Gargc);
		    break;
		case 'F':
		    self->func =getarg(argv,&Gargc);
		    break;
		case 'T': 
		    self->title =getarg(argv,&Gargc);
		    break;
		case 'S':
		    addFile(self,
			    getarg(argv,&Gargc),
			    (useNewWindow || maxInitWindows-->0),
			    pendingReadOnly);
		    break;		
		case 'I':
		    self->objectname = getarg(argv,&Gargc);
		    break;
		default:
		    argv++;
	    }
	else{
	    argv++;
	}
    }
    return TRUE;
}



boolean runadewapp__Start(self)
struct runadewapp *self;
{
    struct runadewapp_fileList *fileEntry, *next;
    char iname[256];
    struct proctable_Entry *pr;
    int (*proc)();
    if(!super_Start(self))
	return FALSE;
    if(self->objectname){
	if((self->buffer = buffer_Create("", NULL, self->objectname, NULL)) == NULL){
	    fprintf(stderr,"adew: Could not create a buffer with %s; exiting.\n",self->objectname);
	    exit(-1);
	}

	if((self->frame = frame_New()) == NULL) {
	    fprintf(stderr,"adew: Could not allocate enough memory; exiting.\n");
	    exit(-1);
	}
	if((self->im = im_Create(NULL)) == NULL) {
	    fprintf(stderr,"adew: Could not create new window; exiting.\n");
	    exit(-1);
	}
	im_SetView(self->im, self->frame);
	frame_PostDefaultHandler(self->frame, "message", frame_WantHandler(self->frame, "message"));
	frame_SetBuffer(self->frame, self->buffer, TRUE);
	buffer_SetScratch(self->buffer,TRUE);
	frame_SetCommandEnable(self->frame, FALSE);
	if(self->title)
	    frame_SetTitle(self->frame,self->title);
	self->haveBufferInWindow = TRUE;
    }
    else for (fileEntry = self->files; fileEntry != NULL; fileEntry = next) {
        self->buffer = buffer_GetBufferOnFile(fileEntry->filename, fileEntry->readOnly ? buffer_ReadOnly : 0);

	if (self->buffer != NULL && fileEntry->newWindow) {
	    if((self->frame = frame_New()) == NULL) {
		fprintf(stderr,"adew: Could not allocate enough memory; exiting.\n");
		exit(-1);
	    }
	    if((self->im = im_Create(NULL)) == NULL) {
		fprintf(stderr,"adew: Could not create new window; exiting.\n");
		exit(-1);
	    }
	    im_SetView(self->im, self->frame);
	    frame_PostDefaultHandler(self->frame, "message", frame_WantHandler(self->frame, "message"));
	    frame_SetBuffer(self->frame, self->buffer, TRUE);
	    buffer_SetScratch(self->buffer,TRUE);
	    frame_SetCommandEnable(self->frame, FALSE);
	    if(self->title)
		frame_SetTitle(self->frame,self->title);
	    im_ForceUpdate();
	    if(self->cls){
		strcpy(iname,self->cls);
		if(class_Load(iname) != NULL){
		    strcat(iname,"-");
		    strcat(iname,self->func);
		    if((pr = proctable_Lookup(iname)) != NULL && proctable_Defined(pr) ){
			proc = proctable_GetFunction(pr) ;
			(*proc)(frame_GetView(self->frame),0);
		    }
		}
	    }
	    self->haveBufferInWindow = TRUE;
	}
        else {
            char errorMessage[200];
            sprintf(errorMessage, "File %s does not exist and could not be created.", fileEntry->filename);
            StartupError(errorMessage);
        }
        next = fileEntry->next;
        free(fileEntry);
    }

    if(self->haveBufferInWindow == FALSE){
	return FALSE;
    }
    return TRUE;
}


boolean runadewapp__InitializeClass()
{
    Gargv = NULL;
    Gargc = 0;
    return TRUE;
}
char **runadewapp__GetArguments(classID,argc)
struct classheader *classID;
int *argc;
{
    if(argc != NULL) *argc = Gargc;
    return Gargv;
}
