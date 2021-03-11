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
#include <string.h>
#include <stdlib.h>
#include <AF/AFlib.h>
#include <AF/AFUtils.h>


void usage(char *s);

main(int argc, char **argv)
{

	AFAudioConn *aud;
	AC ac;
	int onoff = OnHook;
        int hs,loop;
	int device = 0;		/* default phone device */
	int set = 0;
	int i;

	/* parse the command line */
	for(i=1; i<argc; i++) {
		if(!strcmp(argv[i], "-d")) {
			if(++i < argc)
				device = atoi(argv[i]);
			else
				fprintf(stderr,"%s: missing device\n",
					argv[0]);
		} else if(!strcmp(argv[i], "on")) {
			onoff = OnHook;
			set = 1;
		} else if(!strcmp(argv[i], "off")) {
			onoff = OffHook;
			set = 1;
		} else
			usage(argv[0]);
	} 
	if((aud = AFOpenAudioConn(""))==NULL) {
	    fprintf(stderr, "Connection failed.\n");
	    exit (1);
	}
	if(AAudioDeviceDescriptor(aud, device)->inputsFromPhone == 0) {
		fprintf(stderr, "%s: device %d isn't a phone device.\n",
			argv[0], device);
		exit(1);
	}
	ac = AFCreateAC(aud, device, 0, NULL);
	if (ac == NULL) {
	    fprintf(stderr, "Can't open phone device\n");
	    exit (1);
	  }
	if (set) {
		printf("hook switch %s\n", (onoff == OnHook) ? "on" : "off");
		AFHookSwitch(ac, onoff);
		AFFlush(aud);
		}
	else {
		AFQueryPhone(ac, &hs, &loop);
		printf("The phone is currently %s\n",(hs==OnHook ? "OnHook" : "OffHook"));	
		}
	AFCloseAudioConn(aud);

	return 0;
}

void usage(char *s)
{
   fprintf(stderr,"%s {on off}\n", s);
   exit(0);
}
