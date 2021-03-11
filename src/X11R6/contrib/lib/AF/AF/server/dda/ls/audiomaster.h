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
#ifndef _AUDIOMASTER_H_
#define _AUDIOMASTER_H_

#ifndef _TIME_H_
#include <sys/time.h>
#endif

/*
  Structure describing the LineServer<-->workstation connection
*/
typedef struct ASstruct {
	int	s;			/* UDP socket */
	struct sockaddr_in	a;	/* address family, protocol type */
	int	seq;			/* sequence number */
} AS;

#define AS struct ASstruct

#define SAMPLE_RATE	8192		/* samples per second */
#define SILENCE		255		/* ulaw silence */

/* ----- Function Prototypes ----- */
struct audio_command *AudioPlayReq(AS *ac, int when, int len);
int AudioPlay(AS *ac, int when, char *sample, int len, int option, int *time);
struct audio_command *AudioRecReq(AS *ac, int when, int len);
int AudioRecord(AS *ac, int when, char *sample, int len, int option, int *time);
AS *OpenConnection(char *host);
void CloseConnection(AS *ac);
struct audio_command *AudioReadCodecReq(AS *ac, int reg, int len);
int AudioReadCodec(AS *ac, int reg, int len, void *data);
struct audio_command *AudioWriteCodecReq(AS *ac, int reg, int len, void *data);
int AudioWriteCodec(AS *ac, int reg, int len, void *data);
int AudioSetCodec(AS *ac, int reg, int mask, int val);
int AudioReset(AS *ac);
void usleep(int t);

#define AudioTime(ac,t) (AudioPlay(ac, 0, NULL, 0, 0, t))
#define AudioSleep(t) (usleep((t) * (1000000 / SAMPLE_RATE)))

/* ----- Time Relations ----- */
#define LATER(a,b)	(((a)-(b)) > 0)	/* a later than b */
#define EARLIER(a,b)	(((a)-(b)) < 0)	/* a earlier than b */

#endif
