/************************************************************************
 *       Copyright 1990, 1991, 1992, 1993 Digital Equipment Corporation
 *		Maynard, Massachusetts
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
 ************************************************************************/
/*
 * Copyright 1989 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#include <stdio.h>
#include <AF/AFlib.h>
#include <X11/Xatom.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Clock.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/List.h>
#include <X11/Xaw/Logo.h>
#include <X11/Xaw/MenuButton.h>
#include <X11/Xaw/Scrollbar.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Xaw/SmeBSB.h>
#include <X11/Xaw/SmeLine.h>
#include <X11/Xaw/StripChart.h>
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/Toggle.h>
#include <X11/Xaw/Viewport.h>
#include <X11/Xaw/Cardinals.h>


char		*ebuf = (char *) NULL;
int		mebuf=256;
Widget		text;
int		epos=0;

char *malloc();
char *realloc();
void HandleEvent();
void RingAndLoopEvent();
void HookEvent();
void DTMFEvent();
void DSP56KEvent();
char *tod();
void eprintf();

extern void fatal();

AFInitEvent(aud, outer)
AFAudioConn 	*aud;
Widget outer;
{
    int i;
    int arg_cnt;
    Arg	args[16];

    AFFlush(aud);
    for (i = 0; i < 2; i++) {
	    AMask mask;
	    AC ac;
	    ac = AFCreateAC(aud, 0, 0, NULL);
	    if (ac == NULL) {
		    fatal("can't create AC!\n");
	    }
	    mask = (APhoneRingMask|APhoneDTMFMask|APhoneLoopMask|
		    APhoneHookSwitchMask|ADSPMask);
	    AFSelectEvents(ac, mask);
    }
    if ( (ebuf = malloc(mebuf)) == (char *)NULL) 
	fatal("Could not malloc event buffer");
    ebuf[0] = '\0';

    arg_cnt = 0;
    XtSetArg(args[arg_cnt], XtNtype, XawAsciiString); arg_cnt++;
    XtSetArg(args[arg_cnt], XtNstring, ebuf); arg_cnt++; 
    XtSetArg(args[arg_cnt], XtNuseStringInPlace, True); arg_cnt++;
    XtSetArg(args[arg_cnt], XtNlength, 0); arg_cnt++;
    XtSetArg(args[arg_cnt], XtNdisplayPosition, epos); arg_cnt++;
    text = XtCreateManagedWidget("text", asciiTextWidgetClass, outer, 
				 args, arg_cnt);
    eprintf("Event Log\n");
}


void
eprintf(s)
char *s;
{
    int arg_cnt;
    Arg	args[16];

    if ((epos=strlen(s)+strlen(ebuf)) >= mebuf) {
	mebuf *= 2;
	ebuf = realloc(ebuf, mebuf);
	if (ebuf == (char *)NULL) fatal("Could not realloc event buf");
    }
    (void)strcat(ebuf,s);
    while(ebuf[epos] != '\n')
	epos--;

    arg_cnt = 0;
    XtSetArg(args[arg_cnt], XtNstring, ebuf); arg_cnt++;
    XtSetArg(args[arg_cnt], XtNlength, strlen(ebuf)); arg_cnt++;
    XtSetValues(text, args, arg_cnt);
    arg_cnt = 0;
    XtSetArg(args[arg_cnt], XtNinsertPosition, epos); arg_cnt++;
    XtSetValues(text, args, arg_cnt);
}

void
AFeventHandler(audp, afdp, idp)
AFAudioConn **audp;
int *afdp;
XtInputId *idp;
{
    AFEvent event;

    (void) AFNextEvent(*audp, &event);
    (void)HandleEvent(&event);
}

void HandleEvent(ep)
AFEvent *ep;
{
	switch(ep->type){
	case APhoneRingEvent:
		RingAndLoopEvent(ep);
		break;
	case APhoneDTMFEvent:
		DTMFEvent(ep);
		break;
	case APhoneLoopEvent:
		RingAndLoopEvent(ep);
		break;
	case APhoneHookSwitchEvent:
		HookEvent(ep);
		break;
	case ADSPEvent:
		DSP56KEvent(ep);
		break;
	default:
		break;
	};
}

void RingAndLoopEvent(ep)
AFEvent *ep;
{
	static char	buf[128];
	if(ep->type == APhoneRingEvent)
	{
		sprintf(buf, "%s ", tod(ep->aring.stime)); eprintf(buf);
		sprintf(buf, "%-20s\n","Ringing"); eprintf(buf);
		sprintf(buf, "%-20s %8u\n","  ATime", ep->aring.time); eprintf(buf);
		sprintf(buf, "%-20s %8d\n","  ADevice", ep->aring.device); eprintf(buf);
		sprintf(buf, "%-20s %8d\n","  State", ep->aring.state); eprintf(buf);
	}
	 else
	{
		sprintf(buf, "%s ", tod(ep->aloop.stime)); eprintf(buf);
		sprintf(buf, "%-20s\n","Loop current"); eprintf(buf);
		sprintf(buf, "%-20s %8u\n","  ATime", ep->aloop.time); eprintf(buf);
		sprintf(buf, "%-20s %8d\n","  ADevice", ep->aloop.device); eprintf(buf);
		sprintf(buf, "%-20s %8d\n","  State", ep->aloop.state); eprintf(buf);
	}
}

void HookEvent(ep)
AFEvent *ep;
{
	static char	buf[128];
	sprintf(buf, "%s ", tod(ep->ahook.stime)); eprintf(buf);
	sprintf(buf, "%-20s\n","Hook Switch"); eprintf(buf);
	sprintf(buf, "%-20s %8u\n","  ATime", ep->ahook.time); eprintf(buf);
	sprintf(buf, "%-20s %8d\n","  ADevice", ep->ahook.device); eprintf(buf);
	sprintf(buf, "%-20s %8d\n","  State", ep->ahook.state); eprintf(buf);
}

void DTMFEvent(ep)
AFEvent *ep;
{
	static char	buf[128];

	sprintf(buf, "%s ", tod(ep->adtmf.stime)); eprintf(buf);
	sprintf(buf, "%-20s\n","DTMF"); eprintf(buf);
	sprintf(buf, "%-20s %8u\n","  ATime", ep->adtmf.time); eprintf(buf);
	sprintf(buf, "%-20s %8d\n","  ADevice", ep->adtmf.device); eprintf(buf);
	sprintf(buf, "%-20s %8d\n","  State", ep->adtmf.state); eprintf(buf);
	sprintf(buf, "%-20s %c\n","  Key", ep->adtmf.digit); eprintf(buf);
}

void DSP56KEvent(ep)
AFEvent *ep;
{
	static char	buf[128];

	sprintf(buf, "%s ", tod(ep->adsp.stime)); eprintf(buf);
	sprintf(buf, "%-20s\n","DSP"); eprintf(buf);
	sprintf(buf, "%-20s %8u\n","  ATime", ep->adsp.time); eprintf(buf);
	sprintf(buf, "%-20s %8d\n","  ADevice", ep->adsp.device); eprintf(buf);
	sprintf(buf, "%-20s %02x\n","  HD[0]", ep->adsp.hd[0]); eprintf(buf);
	sprintf(buf, "%-20s %02x\n","  HD[1]", ep->adsp.hd[1]); eprintf(buf);
	sprintf(buf, "%-20s %02x\n","  HD[2]", ep->adsp.hd[2]); eprintf(buf);
	sprintf(buf, "%-20s %02x\n","  HD[3]", ep->adsp.hd[3]); eprintf(buf);
	sprintf(buf, "%-20s %02x\n","  HD[4]", ep->adsp.hd[4]); eprintf(buf);
	sprintf(buf, "%-20s %02x\n","  HD[5]", ep->adsp.hd[5]); eprintf(buf);
	sprintf(buf, "%-20s %02x\n","  HD[6]", ep->adsp.hd[6]); eprintf(buf);
	sprintf(buf, "%-20s %02x\n","  HD[7]", ep->adsp.hd[7]); eprintf(buf);
}

char *tod(t)
struct timeval t;
{
	struct tm *lt;
	static char	buf[128];

	lt = localtime(&t.tv_sec);
	sprintf(buf,"%s", ctime((time_t *) &t.tv_sec));
	sprintf(&buf[strlen(buf)-1]," %2d:%02d:%02d %4d ms",
		lt->tm_hour,
		lt->tm_min,
		lt->tm_sec,
		t.tv_usec/1000
		);
	return buf;
}
