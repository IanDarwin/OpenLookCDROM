/***********************************************************
Copyright 1993 by Tektronix, Inc., Wilsonville, Oregon.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Tektronix or Tek not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

TEKTRONIX DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
TEKTRONIX BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

#include <stdio.h>
#include <signal.h>
#include <AF/AFlib.h>
#include <AF/extensions/aftime.h>
#include <setjmp.h>

AFAudioConn *aud;

static jmp_buf openAbort;

static void
abortOpen (int xxx)
{
    longjmp (openAbort, 1);
}

main(argc, argv)
int argc;
char *argv[];
{
    char *aserver = NULL;
    int first_event, first_error;
    ATime AFTime;
    int hrs, min, sec, msec;

    if (argc == 2) aserver = argv[1];

    (void) signal (SIGALRM, abortOpen);
    (void) alarm ((unsigned) 30);
    if (!setjmp (openAbort)) {
	if (!(aud= AFOpenAudioConn(aserver))) {
	    printf("Cannot open audio server\n");
	    exit(1);
	}
	if (AFTimeQueryExtension(aud, &first_event, &first_error)) {
	    if (AFEGetTime(aud, &AFTime)) {
		msec = AFTime;
		sec = msec / 1000;
		msec -= sec * 1000;
		min = sec / 60;
		sec -= min * 60;
		hrs = min / 60;
		min -= hrs * 60;
		printf("AFTime = %2d:%02d:%02d.%03d\n",hrs,min,sec,msec);
	    } else {
		printf("Unable to get time.\n");
	    }
	} else {
	    printf("AFTime extension not loaded.\n");
	}
	AFCloseAudioConn(aud);
    }
    else {
	printf("Audio server grabbed for over 30 seconds.\n");
    }
    (void) signal (SIGALRM, SIG_DFL);
    (void) alarm ((unsigned) 0);
}
