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
#include <string.h>				/* strcmp proto, etc. 	*/
#include <stdlib.h>				/* exit prot, etc.	*/
#include <time.h>
#include <AF/AFlib.h>

void HandleEvent(AFEvent *ep);
void CountRings(AFEvent *ep);
void RingAndLoopEvent(AFEvent *ep);
void HookEvent(AFEvent *ep);
void DTMFEvent(AFEvent *ep);
void DSP56KEvent(AFEvent *ep);
void PropertyEvent(AFEvent *ep);
char *tod(struct timeval t);

int ring_count_flag = 0;
int noprint_flag = 0;
double ring_count = 0.0;


main(int argc, char **argv)
{
  	int i;
	AFAudioConn *aud;
	AC ac;
	AFEvent event;


	/* Parse the command line. 					*/
	for ( i = 1; i < argc; i++ ) {
		if ( strcmp( argv[i], "-ringcount" ) == 0)
		{
			if(++i < argc)
				if (sscanf(argv[i],"%lf",&ring_count) == 1)
					ring_count_flag = 1;

			else
			{
				fprintf(stderr,"%s: missing ringcount\n",
					argv[0]);
				exit(1);
			}
		} 
		 else if ( strcmp( argv[i], "-noprint" ) == 0)
			noprint_flag = 1;
	}


	aud = AFOpenAudioConn("");


        if (aud != NULL) {
	}
	else {
		printf("can't open connection\n");
		exit(1);
	}
	AFFlush(aud);
	for (i = 0; i < 2; i++) {
		AMask mask;
		ac = AFCreateAC(aud, 0, 0, NULL);
		if (ac == NULL) {
			fprintf(stderr, "%s: can't create AC!\n", argv[0]);
			exit(1);
		}
		mask = (APhoneRingMask|APhoneDTMFMask|APhoneLoopMask|
			APhoneHookSwitchMask|ADSPMask|APropertyChangeMask);
		AFSelectEvents(ac, mask);
	}
	while (1) {
		(void) AFNextEvent(aud, &event);
		if (!noprint_flag) (void)HandleEvent(&event);
		if (ring_count_flag) (void)CountRings(&event);
		fflush(stdout);
	}
}

long last_event_time = 0;
int this_ring_count = 0;

void CountRings(AFEvent *ep)
{
	long event_time;
	switch(ep->type){
	case APhoneRingEvent:
		event_time = ep->aring.stime.tv_sec;
		if (ep->aring.state == 1)
		{
		  if ((event_time - last_event_time) > 7) this_ring_count = 1;
		  else this_ring_count += 1;
                }
                else /* ep->aring.state == 0 */
                {
                    if (this_ring_count >= ring_count) exit(0);
                }
                last_event_time = event_time;
		break;
	default:
		break;
	};
}

void HandleEvent(AFEvent *ep)
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
	case APropertyEvent:
		PropertyEvent(ep);
		break;
	default:
		break;
	};
}

void RingAndLoopEvent(AFEvent *ep)
{
	if(ep->type == APhoneRingEvent)
	{
		printf("%s ", tod(ep->aring.stime));
		printf("%-20s\n","Ringing");
		printf("%-20s %8d\n","  ATime", ep->aring.time);
		printf("%-20s %8d\n","  ADevice", ep->aring.device);
		printf("%-20s %8d\n","  State", ep->aring.state);
	}
	 else
	{
		printf("%s ", tod(ep->aloop.stime));
		printf("%-20s\n","Loop current");
		printf("%-20s %8d\n","  ATime", ep->aloop.time);
		printf("%-20s %8d\n","  ADevice", ep->aloop.device);
		printf("%-20s %8d\n","  State", ep->aloop.state);
	}
}

void HookEvent(AFEvent *ep)
{
	printf("%s ", tod(ep->ahook.stime));
	printf("%-20s\n","Hook Switch");
	printf("%-20s %8d\n","  ATime", ep->ahook.time);
	printf("%-20s %8d\n","  ADevice", ep->ahook.device);
	printf("%-20s %8d\n","  State", ep->ahook.state);
}

void DTMFEvent(AFEvent *ep)
{
	printf("%s ", tod(ep->adtmf.stime));
	printf("%-20s\n","DTMF");
	printf("%-20s %8d\n","  ATime", ep->adtmf.time);
	printf("%-20s %8d\n","  ADevice", ep->adtmf.device);
	printf("%-20s %8d\n","  State", ep->adtmf.state);
	printf("%-20s %c\n","  Key", ep->adtmf.digit);
}

void DSP56KEvent(AFEvent *ep)
{
	printf("%s ", tod(ep->adsp.stime));
	printf("%-20s\n","DSP");
	printf("%-20s %8d\n","  ATime", ep->adsp.time);
	printf("%-20s %8d\n","  ADevice", ep->adsp.device);
	printf("%-20s %02x\n","  HD[0]", ep->adsp.hd[0]);
	printf("%-20s %02x\n","  HD[1]", ep->adsp.hd[1]);
	printf("%-20s %02x\n","  HD[2]", ep->adsp.hd[2]);
	printf("%-20s %02x\n","  HD[3]", ep->adsp.hd[3]);
	printf("%-20s %02x\n","  HD[4]", ep->adsp.hd[4]);
	printf("%-20s %02x\n","  HD[5]", ep->adsp.hd[5]);
	printf("%-20s %02x\n","  HD[6]", ep->adsp.hd[6]);
	printf("%-20s %02x\n","  HD[7]", ep->adsp.hd[7]);
}

void PropertyEvent(AFEvent *ep)
{
	printf("%s ", tod(ep->aproperty.stime));
	printf("%-20s\n","Property Change");
	printf("%-20s %8d\n","  ATime", ep->aproperty.time);
	printf("%-20s %8d\n","  ADevice", ep->aproperty.device);
	printf("%-20s %8d\n","  Atom", ep->aproperty.atom);
	printf("%-20s %8d\n","  State", ep->aproperty.state);
}

char *
tod(struct timeval t)
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
