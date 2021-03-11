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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/common/RCS/app.c,v 2.31 1993/11/16 21:24:54 gk5g Exp $";
#endif

#include <andrewos.h>
#include <class.h>

#include <im.ih>
#include <init.ih>
#include <environ.ih>
#include <graphic.ih>
#include <profile.ih>
#include <app.eh>

static void errorProc(rock, str)
pointer rock;
char *str;
{
    fprintf(stderr, "%s", str);
    fflush(stderr);
}

boolean application__InitializeObject(classID,self)
struct classheader *classID;
struct application *self;
{
    self->fork=TRUE; /* do it by default */
    self->name=NULL;
    self->forceload=FALSE;
    self->readInitFile=TRUE;
    self->errorProc=errorProc;
    self->errorRock=NULL;
    self->fgcolor=self->bgcolor=self->geometry=NULL;
    self->profile=NULL;
    self->majorversion = -42;
    self->minorversion = -42;
    self->printversioninfo = TRUE;
    self->iconic=environ_GetProfileSwitch("StartIconic", FALSE);
    return TRUE;
}

void application__FinalizeObject(classID,self)
struct classheader *classID;
struct application *self;
{
}
void application__PrintVersionNumber(self)
struct application *self;
{
    if (!environ_GetProfileSwitch("PrintVersionNumber", TRUE))
	return;
    switch(application_GetMajorVersion(self)){
	case -42:
	    fprintf(stderr,
		    "Starting %s (Version Unknown, ATK %s); please wait...\n",
		    application_GetName(self), application_GetATKVersion());
	    break;
	default:
	    fprintf(stderr,
		    "Starting %s (Version %d.%d, ATK %s); please wait...\n",
		    application_GetName(self),
		    application_GetMajorVersion(self),
		    application_GetMinorVersion(self),
		    application_GetATKVersion());
    }
    fflush(stderr);
}
/*
 * These 4 routines are the heart of the application interface
 */

boolean application__ParseArgs(self,argc,argv)
struct application *self;
int argc;
char **argv;
{
    char *host;

    if(self->name==NULL){
	char *t=rindex(*argv,'/');
	if(t==NULL)
	    self->name= *argv;
	else
	    self->name=t+1;
    }

#define GETARGSTR(var, pos)\
{\
    if((*argv)[pos]!='\0')\
        var= ((*argv)[pos]=='=' ? &(*argv)[pos+1] : &(*argv)[pos]);\
    else if(argv[1]==NULL){\
	fprintf(stderr,"%s: %s switch requires an argument.\n",application_GetName(self),*argv);\
        return FALSE;\
    }else{\
        argc--;\
        application_DeleteArgs(argv,1);\
    	var= *argv;\
    }\
}

    im_SetDefaultIconic(self->iconic);
    while(*++argv!=NULL)
	if(**argv=='-'){
	    switch((*argv)[1]){
		case 'b':
		    if((*argv)[2]!='g')
			continue;

		    GETARGSTR(self->bgcolor, 3);
		    break;
		case 'd':
		    if ((*argv)[2] == '\0')  {
			self->fork=FALSE;
		    }
		    else if (strncmp(&((*argv)[2]), "isplay", 6) == 0)  {
			GETARGSTR(host, 8);
			im_SetDefaultServerHost(host);
		    }
		    else
			continue;
  		    break;
		case 'f':
		    if((*argv)[2]=='g'){
			GETARGSTR(self->fgcolor, 3);
		    }else if((*argv)[2]=='l')
			self->forceload=TRUE;
  		    else
			continue;
  		    break;
		case 'g':
		    if (strncmp(&((*argv)[2]), "eometry", 7) == 0)  {
			GETARGSTR(self->geometry, 9);
			im_SetGeometrySpec(self->geometry);
		    }
		    else
			continue;
		    break;
		case 'h':
		    if (strncmp(&((*argv)[2]), "ost", 3) == 0)  {
			GETARGSTR(host, 5);
			im_SetDefaultServerHost(host);
		    }
		    else
			continue;
		    break;
		case 'i':
		    if(strncmp(*argv+1, "iconic", 6)==0) {
			if((*argv)[7]=='o') {
			    im_SetDefaultIconic(FALSE);
			} else {
			    im_SetDefaultIconic(TRUE);
			}
		    }
		    break;
		case 'n':
		    if((*argv)[2]=='i')
			self->readInitFile=FALSE;
  		    else
			continue;
  		    break;
		case 'p':
		    GETARGSTR(self->profile, 2);
		    break;
  		default:
		    /* skip over the DeleteArgs */
		    continue;
  	    }

	    application_DeleteArgs(argv--,1);
	}
    return TRUE;
}


void 
application__ReadInitFile(self)
struct application *self;
{
    char buffer[256], *andrewDir, *sitename,*name=self->name;
    struct init *init;
    boolean HadGlobalNameInit = FALSE;
    boolean siteinit = FALSE,sitegloinit = FALSE;
    char *home, *localLib;

    if (!self->readInitFile) return;

    init = init_New();
    im_SetGlobalInit(init);	/* tell im about it.    -wjh
			The init can then be modified by called procs.
			(retract below if no inits found)  */

    home = environ_Get("HOME");
    andrewDir = environ_AndrewDir(NULL);

    if((sitename = environ_GetConfiguration("SiteConfigName")) != NULL){
	sprintf(buffer, "%s/lib/%s.atkinit", andrewDir,sitename);
	sitegloinit = init_Load(init, buffer, (procedure) self->errorProc, (long) self->errorRock,self->forceload) >= 0;
	if(name != NULL){
	    sprintf(buffer, "%s/lib/%s.%sinit", andrewDir,sitename, name);
	    siteinit = init_Load(init, buffer, (procedure) self->errorProc,
				 (long) self->errorRock, self->forceload) >= 0;
	}
    }

    /* try for .NAMEinit and quit if succeed */
    if (home != NULL  &&  name != NULL)  {
	sprintf(buffer, "%s/.%sinit", home, name);
	if ((init_Load(init, buffer, (procedure) self->errorProc, 
		       (long) self->errorRock, self->forceload)) >= 0) 
	    return;
    }

    /* try for LocalLib/global.NAMEinit */
    if(name != NULL && (localLib = environ_GetConfiguration("LocalLib")) != NULL){
	char lib[1000];
	char *thisStr, *nextStr;
	thisStr = lib;
	strcpy(lib, localLib);
	while (thisStr != NULL) {
	    nextStr = index(thisStr, ':');
	    if (nextStr != NULL) *nextStr++ = '\0';
	    sprintf(buffer, "%s/global.%sinit", thisStr, name);
	    if (init_Load(init, buffer, (procedure) self->errorProc, (long) self->errorRock, self->forceload) >= 0) {
		HadGlobalNameInit = TRUE;
		if(localLib = (char*) malloc(strlen(thisStr) + 1))
		    strcpy(localLib, thisStr);
		else
		    localLib = NULL;
		break;
	    }
	    thisStr = nextStr;
	}
    }

    /* try for andrew/lib/global.NAMEinit and continue even if succeed */
    if (! HadGlobalNameInit && (name != NULL)) {
	sprintf(buffer, "%s/lib/global.%sinit", andrewDir, name);
	HadGlobalNameInit = (init_Load(init, buffer,	(procedure) self->errorProc,(long) self->errorRock, self->forceload)) >= 0;
    }

    /* try for ~/.atkinit or ~/.be2init  either alone or extending
	global.NAMEinit quit if succeed */
    if (home != NULL) {
	sprintf(buffer, "%s/.atkinit", home);
	if ((init_Load(init, buffer, (procedure) self->errorProc, 
		       (long) self->errorRock, self->forceload)) >= 0) 
	    return;
	sprintf(buffer, "%s/.be2init", home);
	if ((init_Load(init, buffer, (procedure) self->errorProc, 
		       (long) self->errorRock, self->forceload)) >= 0 ) 
	    return;
    }

    /* as a last resort, try for global.atkinit, but only if there was no global.NAMEinit */
    if (! HadGlobalNameInit) {
	if (localLib) {
	    sprintf(buffer, "%s/global.atkinit", localLib);
	    if ((init_Load(init, buffer, (procedure) self->errorProc, (long) self->errorRock,self->forceload)) >= 0)
		return;
	}
	else {
	    sprintf(buffer, "%s/lib/global.atkinit", andrewDir);
	    if ((init_Load(init, buffer, (procedure) self->errorProc, (long) self->errorRock,self->forceload)) >= 0)
		return;
	}
    }
 
    /* If we failed to find initialization, discard the data structure we might have used */
    if(!HadGlobalNameInit && !siteinit && !sitegloinit){
	im_SetGlobalInit(NULL);
	init_Destroy(init);
    }
}


boolean application__Start(self)
struct application *self;
{
    char *t;

    if(self->fgcolor==NULL)  {
	t = environ_GetProfile("ForeGroundColor");
	if (t != NULL)  {
	    self->fgcolor = (char *) malloc(strlen(t) + 1);
	    strcpy(self->fgcolor, t);
	}
    }
    if(self->bgcolor==NULL)  {
	t = environ_GetProfile("BackGroundColor");
	if (t != NULL)  {
	    self->bgcolor = (char *) malloc(strlen(t) + 1);
	    strcpy(self->bgcolor, t);
	}
    }

    graphic_SetDefaultColors(self->fgcolor,self->bgcolor);

    /* get a geometry spec if specified */
    if (self->geometry == NULL){
	t = environ_GetProfile("Geometry");
	if (t != NULL){
	    im_SetGeometrySpec(t);
	}
    }

    if(self->profile){
	/* this code should probably be in Start, but then who would call
	 * profile_Stop?
	 */
	char filename[1000];

	if(*self->profile=='\0')
	    self->profile=class_GetTypeName(self);

	strcpy(filename,self->profile);
	strcat(filename,".mon.out");

	if(!profile_StartClass(self->profile,filename)){
	    fprintf(stderr,"%s: Couldn't start profiling %s\n",
		    application_GetName(self),self->profile);
	    return FALSE;
	}
    }

    return TRUE;
}

void application__Stop(self)
struct application *self;
{
    if (class_IsLoaded("profile"))
        profile_Stop(); /* stop current profiling, if any */
}

/* this assumes interaction; maybe should be in a subclass, like
 * interactiveapp or something
 */
int application__Run(self)
struct application *self;
{
    if(!application_Fork(self))
	return -1;

    im_KeyboardProcessor();

    return 0;
}

boolean application__Fork(self)
struct application *self;
{
    if(self->fork){
	self->fork=FALSE; /* just in case */
	switch(fork()){
	    case 0:
		NEWPGRP();
		return TRUE;
	    case -1:
		perror(self->name);
		return FALSE;
	    default:
		exit(0);
	}
    }
    return TRUE;
}


/*
 * Some utility routines
 */

void application__DeleteArgs(classID,argv,num)
struct classheader *classID;
char **argv;
int num;
{
    int i;

    for(i=0;i<num && argv[i]!=NULL; i++)
	;

    while(*(argv+i)!=NULL){
	*argv= *(argv+i);
	argv++;
    }

    *argv=NULL;
}

char *application__GetATKVersion(c)
struct classheader *c;
{
    FILE *fp;
    char *andrewDir, fname[1200], *s;
    static char VerNo[40] = "ATK-No-Version-Number";
    static boolean HasChecked = FALSE;

    if (!HasChecked) {
	andrewDir = environ_AndrewDir(NULL);
	sprintf(fname, "%s/lib/atkvers.num", andrewDir);
	fp = fopen(fname, "r");
	if (!fp) return(VerNo);
	if (fgets(fname, sizeof(fname), fp) == NULL) {
	    fclose(fp);
	    return(VerNo);
	}
	fclose(fp);
	s = index(fname, '\n');
	if (s) *s = '\0';
	strncpy(VerNo, fname, sizeof(VerNo) - 1);
	HasChecked = TRUE;
    }
    return(VerNo);
}
