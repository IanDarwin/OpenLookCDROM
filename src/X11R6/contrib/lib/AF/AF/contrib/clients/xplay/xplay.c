/*
 * xplay.c
 *
 * $Header: /crl/audio/AF/contrib/clients/xplay/RCS/xplay.c,v 1.17 1994/06/03 18:11:51 jg Exp $
 */
/*              Copyright 1990, 1991, 1992, 1993 Digital Equipment Corporation
 *			 Maynard, Massachusetts.
 * 
 * Permission to use, copy, modify, distribute, and sell this software and its 
 * documentation for any purpose is hereby granted without fee, provided that 
 * the above copyright notice appear in all copies and that both that 
 * copyright notice and this permission notice appear in supporting 
 * documentation, and that the name of Digital not be used in advertising or 
 * publicity pertaining to distribution of the software without specific, 
 * written prior permission.  Digital makes no representations about the 
 * suitability of this software for any purpose.  It is provided "as is" 
 * without express or implied warranty.
 * 
 * DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL 
 * DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN 
 * AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF 
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

/* Header Files */

#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <sys/limits.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/stat.h>

#include <AF/AFlib.h>

/* needed for all apps using widgets */
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <Xm/Xm.h>
#include <Mrm/MrmAppl.h>	/* for Mrm */

#include <Xm/PanedW.h>		/* for Paned Window */
#include <Xm/Label.h>		/* for Label */
#include <Xm/FileSB.h>		/* for File Selection Box */
#include <Xm/RowColumn.h>	/* for RowColumn */
#include <Xm/PushBG.h>		/* for Push Button Gadget */
#include <Xm/CascadeB.h>	/* for Cascade button */
#include <Xm/ToggleBG.h>	/* for Toggle Button Gadget */
#include <Xm/Form.h>		/* for Form */

/* Constants & macros */
#define ZERO 0
#define DEFAULT_FILE "/crl/audio/sound_files/magic.snd"
#define SAMPLES_PER_SEC 8000.0
#define SPEAKER 1
#define	MSTOTICKS(x)	((x)<<3)
#define BUFSIZE 1024
#define SBUFSIZE 512
#define LBUFSIZE 2048

#ifndef UIDFILE
#define UIDFILE "xplay.uid"
#endif

static char uid_file[] = UIDFILE;

/* Globals */

String fallback_resources[] = {
    "Xplay*foreground:			SteelBlue",
    "*background:			Wheat",
    "*menuButton.label:			File",
    "*SimpleMenu.open.label:		Open",
    "*SimpleMenu.quit.label:		Quit",
    "*Dialog.label:			Enter the Filename",
    "*Dialog*value.translations: #override \\n <Key>Return: Ok()",
    NULL,
};

#define REW 0
#define STOP 1
#define PLAY 2
#define PAUSE 3
#define FF 4
#define wcr_scale 5

#define NUM_TOGGLES 5

#define menu_open 1
#define menu_quit 2

extern char *version;

Widget toggleButton[NUM_TOGGLES];
Widget topshell;
Widget fsb;
Widget scaleFilePos = NULL;
Widget open_err;
char audioFileName[PATH_MAX];
int toffset = 1000;
int device;
int afd = -1;
int playStatus = STOP;
AFSetACAttributes attributes;
AC ac;
ATime atm;
XtAppContext app_con;
XtWorkProcId curId = 0;
XmStringCharSet charset = (XmStringCharSet) XmSTRING_DEFAULT_CHARSET;
off_t FileLen = 0;
off_t FilePos = 0;

void FileMenuSelect();
void ProcessName();
void CancelPrompt();
void CenterWidgetOverCursor();
void SetupForm();
void Syntax();
int OpenFile();
int PlaySome();
int PlayFast();
int PlayRew();
void ChangeState();
void SetCurrentToggle();
void WidgetCreate();
void UpdateScale();
void UpdateFilePos();
XtErrorHandler DummyHandler();

typedef struct _AppResources 
{
    String geometry;
    int device;
    float offset;
    int gain;
    String file;
} AppResources;

float def_offset = 0.175;

static XtResource resources[] = {
{"geometry", "Geometry", XtRString, sizeof(char *),
     XtOffset(AppResources *,geometry), XtRString, NULL},
{"device", "Device", XtRInt, sizeof(int),
     XtOffset(AppResources *,device), XtRImmediate, (XtPointer)SPEAKER},
{"offset", "Offset", XtRFloat, sizeof(float),
     XtOffset(AppResources *,offset), XtRFloat, (XtPointer)&def_offset},
{"gain", "Gain", XtRInt, sizeof(int),
     XtOffset(AppResources *,gain), XtRImmediate, (XtPointer)0},
{"file", "File", XtRString, sizeof(char *),
     XtOffset(AppResources *,file), XtRString, NULL},
};

static XrmOptionDescRec options[] = {
{"-geometry", "geometry", XrmoptionSepArg, NULL},
{"-device", "device", XrmoptionSepArg, NULL},
{"-offset", "offset", XrmoptionSepArg, NULL},
{"-gain", "gain", XrmoptionSepArg, NULL},
{"-file", "file", XrmoptionSepArg, NULL},
};

static MrmHierarchy s_MrmHierarchy;     /* MRM database hierarchy ID */
static char *db_filename_vec[] = {
    "xplay.uid"			/* There is only one UID file for xplay */
};
static int db_filename_num = 
    (sizeof db_filename_vec / sizeof db_filename_vec[0]);
static char *db_backup_vec[] = {
    uid_file
};
static int db_backup_num = 
    (sizeof db_backup_vec / sizeof db_backup_vec[0]);

static MrmRegisterArg reglist[] = {
{"ChangeState", (caddr_t) ChangeState},
{"ProcessName", (caddr_t) ProcessName},
{"FileMenuSelect", (caddr_t) FileMenuSelect},
{"CancelPrompt", (caddr_t) CancelPrompt},
{"WidgetCreate", (caddr_t) WidgetCreate},
{"UpdateFilePos", (caddr_t) UpdateFilePos},
};
static int reglist_num = (sizeof reglist / sizeof reglist[0]);

main (argc,argv)
int argc;
char **argv;
{
    Widget playmain;
    MrmType class;
    AppResources app_resources;
    AFAudioConn *aud;
    int i;
    Arg arg[1];

    MrmInitialize();

    topshell = XtAppInitialize(&app_con,"Xplay",options,XtNumber(options),
			       &argc,argv,fallback_resources,NULL,ZERO);
    i=0;
    XtSetArg(arg[i],XtNtitle,version); i++;
    XtSetValues(topshell,arg,i);

    XtAppSetWarningHandler(app_con,DummyHandler);
    /* Open the UID files (the output of the UIL compiler) in the hierarchy*/
    if ( MrmOpenHierarchy(db_filename_num,db_filename_vec,NULL,
			  &s_MrmHierarchy) != MrmSUCCESS) {
	if ( MrmOpenHierarchy(db_backup_num,db_backup_vec,NULL,
			      &s_MrmHierarchy) != MrmSUCCESS) {
	    fprintf(stderr,"Can't open UID hierarchy.\n");
	    exit(1);
	}
    }
    /* calling XtAppSetWarningHandler with null restores the default handler */
    XtAppSetWarningHandler(app_con,NULL);

    if (MrmRegisterNames (reglist, reglist_num) != MrmSUCCESS) {
	fprintf(stderr, "Can't register names\n");
	exit(1);
    }

    /* XtAppAddActions(app_con, actionTable, XtNumber(actionTable)); */

    if (argc != 1)
        Syntax(app_con, argv[0]);

    /* TBD: do something about -c option and DEFAULT_FILE */

    XtGetApplicationResources( topshell, (XtPointer) &app_resources,
                               resources, XtNumber(resources), NULL, ZERO );

    /* open audio connection */
    if ( (aud = AFOpenAudioConn("")) == NULL) {
        fprintf(stderr, "%s: can't open connection.\n", argv[0]);
        exit(1);
    }

    attributes.preempt = Mix;
    attributes.start_timeout = 0;
    attributes.end_silence = 0;
    attributes.play_gain = app_resources.gain;
    attributes.rec_gain = 0;
    ac = AFCreateAC(aud, app_resources.device, ACPlayGain, &attributes);

    afd = OpenFile(app_resources.file);
    if (afd >= 0) {
	curId = XtAppAddWorkProc(app_con, PlaySome, NULL);
	playStatus = PLAY;
	atm = AFGetTime(ac) + (int)(SAMPLES_PER_SEC * app_resources.offset);
    }

    if ( MrmFetchWidget(s_MrmHierarchy, "play_main", topshell,
			&playmain,&class) != MrmSUCCESS) {
	fprintf(stderr, "Can't fetch widget\n");
	exit(1);
    }
    if ( MrmFetchWidget(s_MrmHierarchy, "fs_box", topshell,
			&fsb,&class) != MrmSUCCESS) {
	fprintf(stderr, "Can't fetch widget\n");
	exit(1);
    }
    if ( MrmFetchWidget(s_MrmHierarchy, "open_error", topshell,
			&open_err,&class) != MrmSUCCESS) {
	fprintf(stderr, "Can't fetch widget\n");
	exit(1);
    }

    XtManageChild(playmain);

    XtRealizeWidget(topshell);
    SetCurrentToggle();
    XtAppMainLoop(app_con);
}

XtErrorHandler DummyHandler()
{
    return(NULL);
}

int PlaySome()
{
    unsigned char buf[BUFSIZE];
    int nbytes;

    nbytes = read(afd, buf, BUFSIZE);
    if (nbytes == 0) {
	/* this should remove the work procedure */
	if (afd >= 0) {
	    close(afd);
	    afd = -1;
	}
	playStatus = STOP;
	SetCurrentToggle();
	FilePos = 0;
	UpdateScale();
	return(TRUE);
    }
    AFPlaySamples(ac, atm, nbytes, buf);
    atm += nbytes;
    FilePos += nbytes;
    UpdateScale();
    return(FALSE);
}

int PlayFast()
{
    unsigned char buf[BUFSIZE];
    unsigned char buf2[SBUFSIZE];
    int nbytes,half,i;

    nbytes = read(afd, buf, BUFSIZE);
    half = nbytes / 2;

    i = 0;
    while (i < half) {
	buf2[i] = buf[2*i];
	i++;
    }
    FilePos +=nbytes;
    if (nbytes == 0) {
	if (afd >= 0) {
	    close(afd);
	    afd = -1;
	}
	playStatus = STOP;
	SetCurrentToggle();
	FilePos = 0;
	UpdateScale();
	return(TRUE);
    }
    AFPlaySamples(ac, atm, half, buf2);
    atm += half;
    UpdateScale();
    return(FALSE);
}

int PlayRew()
{
    off_t offset;
    unsigned char buf[BUFSIZE];
    unsigned char buf2[SBUFSIZE];
    int nbytes;
    int half,i,j;

    nbytes = read(afd, buf, BUFSIZE);
    half = nbytes / 2;
    i = half - 1;
    j = 0;
    while (i >= 0) {
	buf2[i] = buf[j];
	j += 2;
	i--;
    }
    FilePos += nbytes;
    /* TBD: this doesn't work for rewind */
    if (nbytes == 0) {
	return(TRUE);
    }
    AFPlaySamples(ac, atm, nbytes, buf);
    offset = 0 - nbytes - LBUFSIZE;
    (void) lseek(afd,offset,SEEK_CUR);
    FilePos += offset;
    atm += nbytes;
    UpdateScale();
    return(FALSE);
}

/* ARGSUSED */
void UpdateFilePos(w,client_data,call_data)
Widget w;
XtPointer client_data,call_data;
{
    int newpos;

    XmScaleGetValue(scaleFilePos,&newpos);
    (void) lseek(afd,newpos,SEEK_SET);
    FilePos = newpos;
}

void UpdateScale()
{
    XmScaleSetValue(scaleFilePos,FilePos);
}

void Syntax(app_con, call)
XtAppContext app_con;
char *call;
{
    XtDestroyApplicationContext(app_con);
    if (call != NULL) {
	fprintf( stderr, "Usage: %s\n", call);
    }
}

/* ARGSUSED */
void ChangeState(w, client_data, call_data)
Widget w;
XtPointer client_data, call_data;
{
    Arg qarg[1];
    Boolean togState;

    /* get state of button, if unset, just return */
    XtSetArg(qarg[0],XmNset,&togState);
    XtGetValues(w,qarg,1);
    if (togState == FALSE) {
	return;
    }
    if (w == toggleButton[REW]) {
	if (playStatus == REW) {
	    SetCurrentToggle();
	    return;
	}
	if (curId != 0) {
	    XtRemoveWorkProc(curId);
	}
	/* if afd is -1, then there is no open audio file, so attempt to
	   open one. */
	if (afd == -1) {
	    afd = OpenFile(audioFileName);
	    if (afd < 0) {
		afd = -1;
		SetCurrentToggle();
		return;
	    }
	}
	curId = XtAppAddWorkProc(app_con, PlayRew, NULL);
	playStatus = REW;
	SetCurrentToggle();
	atm = AFGetTime(ac) + (int) (SAMPLES_PER_SEC * 0.175);
    } else if (w == toggleButton[PLAY]) {
	if (playStatus == PLAY) {
	    SetCurrentToggle();
	    return;
	}
	if (curId != 0) {
	    XtRemoveWorkProc(curId);
	}
	/* if afd is -1, then there is no open audio file, so attempt to
	   open one. */
	if (afd == -1) {
	    afd = OpenFile(audioFileName);
	    if (afd < 0) {
		afd = -1;
		SetCurrentToggle();
		return;
	    }
	}
	curId = XtAppAddWorkProc(app_con, PlaySome, NULL);
	/* start up after 1/8 second pause */
	atm = AFGetTime(ac) + (int) (SAMPLES_PER_SEC * 0.175);
	playStatus = PLAY;
	SetCurrentToggle();	
    } else if (w == toggleButton[STOP]) {
	if (playStatus != STOP && playStatus != PAUSE) {
	    XtRemoveWorkProc(curId);
	    curId = 0;
	    FilePos = 0;
	    UpdateScale();
	}
	if (playStatus != STOP) {
	    if (afd >= 0) {
		close(afd);
		afd = -1;
	    }
	    playStatus = STOP;
	}
	SetCurrentToggle();
    } else if (w == toggleButton[PAUSE]) {
	if (playStatus == PLAY || playStatus == FF || playStatus == REW) {
	    if (curId == 0) {
		fprintf(stderr,"Can't remove non-existant work proc\n");
	    }
	    XtRemoveWorkProc(curId);
	}
	curId = 0;
	playStatus = PAUSE;
	SetCurrentToggle();
    } else if (w == toggleButton[FF]) {
	if (playStatus == FF) {
	    SetCurrentToggle();
	    return;
	}
	if (curId != 0) {
	    XtRemoveWorkProc(curId);
	}
	/* if afd is -1, then there is no open audio file, so attempt to
	   open one. */
	if (afd == -1) {
	    afd = OpenFile(audioFileName);
	    if (afd < 0) {
		afd = -1;
		SetCurrentToggle();
		return;
	    }
	}
	curId = XtAppAddWorkProc(app_con, PlayFast, NULL);
	playStatus = FF;
	SetCurrentToggle();
	atm = AFGetTime(ac) + (int) (SAMPLES_PER_SEC * 0.175);
    }
}

void SetCurrentToggle()
{
    switch(playStatus) {
    case FF:
	XmToggleButtonGadgetSetState(toggleButton[FF],TRUE,TRUE);
	break;
    case REW:
	XmToggleButtonGadgetSetState(toggleButton[REW],TRUE,TRUE);
	break;
    case PLAY:
	XmToggleButtonGadgetSetState(toggleButton[PLAY],TRUE,TRUE);
	break;
    case STOP:
	XmToggleButtonGadgetSetState(toggleButton[STOP],TRUE,TRUE);
	break;
    case PAUSE:
	XmToggleButtonGadgetSetState(toggleButton[PAUSE],TRUE,TRUE);
    }
}

/* ARGSUSED */
void WidgetCreate(w,tag,garbage)
Widget w;
int *tag;
XtPointer garbage;
{
    Arg args[3];
    int i;

    switch (*tag) {
    case FF:
	toggleButton[FF] = w;
	break;
    case REW:
	toggleButton[REW] = w;
	break;
    case STOP:
	toggleButton[STOP] = w;
	break;
    case PLAY:
	toggleButton[PLAY] = w;
	break;
    case PAUSE:
	toggleButton[PAUSE] = w;
	break;
    case wcr_scale:
	scaleFilePos = w;
	i = 0;
	XtSetArg(args[i],XmNminimum,0); i++;
	/* if file is already open, set the maximum */
	if (FileLen > 0) {
	    XtSetArg(args[i],XmNmaximum,FileLen); i++;
	}
	XtSetArg(args[i],XmNvalue,0); i++;
	XtSetValues(w,args,i);
	break;
    }
}

/* ARGSUSED */
void FileMenuSelect(w,tag,garbage)
Widget w;
int *tag;
XtPointer garbage;
{
    static int fsbexists = FALSE;

    if (*tag == menu_quit) {
	XtDestroyApplicationContext(XtWidgetToApplicationContext(w));
	/* TBD: call cleanup routine? */
	exit(0);
    }
    /* if not quit, must be open */
    if (*tag == menu_open) {
	if (fsbexists == FALSE) {
	    fsbexists = TRUE;
	    XtRealizeWidget(fsb);
	}
	CenterWidgetOverCursor(fsb);
	XtManageChild(fsb);
    }
}

/* copied this routine from xrn */
void CenterWidgetOverCursor(widget)
Widget widget;
{
    Window root, child;
    int x, y, dummy;
    unsigned int mask;
    Arg cargs[2];

    /* get cursor position */
    (void) XQueryPointer(XtDisplay(widget), XtWindow(widget),
                         &root, &child,
                         &x, &y, &dummy, &dummy,
                         &mask);
    XtSetArg(cargs[0], XtNwidth, 0);
    XtSetArg(cargs[1], XtNheight, 0);
    XtGetValues(widget, cargs, XtNumber(cargs));

    /* calculate new x and y */
    x -= ((int) cargs[0].value) / 2;
    y -= ((int) cargs[1].value) / 2;
    if (x + (int) cargs[0].value > WidthOfScreen(XtScreen(widget))) {
        x = WidthOfScreen(XtScreen(widget)) - (int) cargs[0].value;
    }
    if (y + (int) cargs[1].value > HeightOfScreen(XtScreen(widget))) {
        y = HeightOfScreen(XtScreen(widget)) - (int) cargs[1].value;
    }
    if (x < 0) {
        x = 0;
    }
    if (y < 0) {
        y = 0;
    }

    /* set it for the widget */
    XtSetArg(cargs[0], XtNx, x);
    XtSetArg(cargs[1], XtNy, y);
    XtSetValues(widget, cargs, XtNumber(cargs));
    return;
}

/* ARGSUSED */
void CancelPrompt(widget, client_data, call_data)
Widget  widget;
XtPointer client_data, call_data;
{
    XtUnmanageChild(fsb);
}

/* ARGSUSED */
void ProcessName(widget, client_data, call_data)
Widget  widget;
XtPointer client_data, call_data;
{
    XmFileSelectionBoxCallbackStruct *cdat;
    char *fname;

    cdat = (XmFileSelectionBoxCallbackStruct *)call_data;
    XmStringGetLtoR(cdat->value, charset, &fname);
    afd = OpenFile(fname);
    if (afd >= 0) {
	if (curId != 0) {
	    XtRemoveWorkProc(curId);
	} 
	curId = XtAppAddWorkProc(app_con, PlaySome, NULL);
	playStatus = PLAY;
	SetCurrentToggle();
	atm = AFGetTime(ac) + (int) (SAMPLES_PER_SEC * 0.175);
    } else {
	/* create popup */
	XtRealizeWidget(open_err);
	XtManageChild(open_err);
    }
    XtUnmanageChild(fsb);
}

int OpenFile(name)
String name;
{
    int fd;
    struct stat buf;
    int i;
    Arg args[1];

    if (name != NULL && (strlen(name) != 0) ) {
	strcpy(audioFileName,name);
    } else {
	return (-1);
    }
#ifdef DEBUG
    fprintf(stderr,"about to open %s.\n",audioFileName);
#endif
    /* interpret - as stdin */
    if (name[0] == '-' && name[1] == '\0') {
	return(0);
    }
    /* TBD: this should really be an X popup error message */
    if ((fd = open(audioFileName, O_RDONLY, 0)) < 0) {
	return(-1);
    } 
    (void)fstat(fd,&buf);
    FileLen = buf.st_size;
    FilePos = 0;
    if (scaleFilePos != NULL) {
	i=0;
	XtSetArg(args[i],XmNmaximum,FileLen); i++;
	XtSetValues(scaleFilePos,args,i);
    }
    return(fd);
}
