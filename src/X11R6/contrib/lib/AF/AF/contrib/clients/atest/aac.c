/*
 * Copyright 1993 by Digital Equipment Corporation, Maynard, Massachusetts.
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
#include <stdio.h>
#include <AF/AFlib.h>

main()
{
	ATime t;
	AC ac;
        AFSetACAttributes attributes;
	AFAudioConn *aud;
	attributes.preempt = Preempt;
	attributes.start_timeout = 2500;
	attributes.end_silence = 2500;
	attributes.play_gain = 10;
        attributes.rec_gain =  10;

	aud = AFOpenAudioConn("");
        if (aud != NULL) {
	}
	else {
		printf("can't open connection\n");
		exit(1);
	}
	ac = AFCreateAC(aud, 0, ACRecordGain|ACPlayGain|ACStartTimeout|
		ACEndSilence|ACPreemption,
			&attributes);
	while (1) {
		ac = AFCreateAC(aud, 0, 
			ACRecordGain|ACPlayGain|ACEndSilence|ACStartTimeout|
			ACPreemption, &attributes);
	  	printf("getting time...\n");
		t = AFGetTime(ac);
		printf("time is %d\n", t);
		AFChangeACAttributes(ac,
			 ACRecordGain|ACPlayGain|ACEndSilence|ACPreemption,
			&attributes);
		AFFreeAC(ac);
	  	printf("getting time...\n");
	}
	/* NOTREACHED */
}
