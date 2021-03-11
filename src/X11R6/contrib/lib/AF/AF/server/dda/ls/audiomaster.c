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
/*
  AudioMaster

	The workstation end of the LineServer <--> workstation audio 
	connection.

  June, 1991  Andrew C. payne
*/

#include <stdio.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/time.h>
#include <netdb.h>

#include "audioproto.h"
#include "audiomaster.h"

#define TRUE	1
#define FALSE	0

#define RETRY_COUNT	5		/* number of retries */
#define RETRY_TIMEOUT	250000		/* length of each retry */

#define MAX_SAMPLE	1024		/* maximum samples in single packet */

/* ----- Timer subroutines ----- */

static int	timeout;		/* TRUE if timer fired */

/*
  Signal handler for timeouts.
*/
void timeout_handler(void)
{
	timeout = TRUE;
}

/*
  Start the timeout timer for the interval specified.  Sets 'timeout' to 
  FALSE.  When the timer fires, 'timeout' is set to TRUE.
*/
void start_timer(int interval)
{
	struct itimerval	it;

	it.it_interval.tv_sec = 0;
	it.it_interval.tv_usec = 0;

	it.it_value.tv_sec = 0;
	it.it_value.tv_usec = interval;

	signal(SIGALRM, timeout_handler);
	timeout = 0;
	setitimer(ITIMER_REAL, &it, NULL);
}

/*
  Kill the timeout timer.
*/
void stop_timer(void)
{
	struct itimerval	it;

	it.it_interval.tv_sec = 0;
	it.it_interval.tv_usec = 0;

	it.it_value.tv_sec = 0;
	it.it_value.tv_usec = 0;

	signal(SIGALRM, SIG_DFL);
	setitimer(ITIMER_REAL, &it, NULL);
}

/*
  Sleep for a fractional second time (time specified in microseconds).
*/
void usleep(int ti)
{
	struct timeval	t;

	t.tv_sec = 0;
	t.tv_usec = ti;

	select(31, 0, 0, 0, &t);
}
	
/* ----- Audio Subroutines ----- */

/*
  Creates a play packet to send to the slave.  Returns pointer to packet.
*/
struct audio_command *AudioPlayReq(AS *ac, int when, int len)
{
	struct audio_command	*c;

	c = (struct audio_command *)malloc(sizeof(struct audio_command) + len);

	if(c != NULL) {
        	c->opcode = htonl(AUDIO_PLAY);
	        c->sequence = htonl(ac->seq++);
	        c->time = htonl(when);
	}
	return c;
}

/*
  Play a sample on the LineServer.  Returns current time in *time, if non-NULL.
  We can be more clever about the header, don't need to copy bytes.
  Returns number of bytes successfully played, or -1 if error.
*/
int AudioPlay(AS *ac, int when, char *sample, int len, int option, int *time)
{
        struct audio_command    *c;
	int	dif;
	int	i,j;
	int	seq;

	if(len > MAX_SAMPLE)
		len = MAX_SAMPLE;

	c = AudioPlayReq(ac, when, len);
	memcpy(c->data, sample, len);
	seq = c->sequence;

	for(i=0; i<RETRY_COUNT; i++) {
		if(send(ac->s, c, sizeof(struct audio_command) + len, 0) !=
			sizeof(struct audio_command) + len) {
				free(c);
				return -1;
		}

		start_timer(RETRY_TIMEOUT);
		while(!timeout && recv(ac->s, c, sizeof(struct audio_command), 0) > 0) {
			if(c->sequence == seq) {
				if(time != NULL)
					*time = ntohl(c->time);
				i = ntohl(c->param);
				free(c);
				return i;
			}
		}
		stop_timer();
	}
	free(c);
	return -1;		/* retry count exceeded */
}

/*
  Creates a record packet to send to the slave.  Returns pointer to packet.
*/
struct audio_command *AudioRecReq(AS *ac, int when, int len)
{
	struct audio_command	*c;

	c = (struct audio_command *)malloc(sizeof(struct audio_command) + len);

	if(c != NULL) {
        	c->opcode = htonl(AUDIO_RECORD);
	        c->sequence = htonl(ac->seq++);
	        c->time = htonl(when);
		c->param = htonl(len);
	}
	return c;
}

/*
  Record a sample on the LineServer.  Returns current time in *time, if 
  non-NULL.
  Returns number of bytes recorded, or -1 if error.
  We can be more clever about the header, don't need to copy bytes.
*/
int AudioRecord(AS *ac, int when, char *sample, int len, int option, int *time)
{
        struct audio_command    *c;
	int	i,j;
	int	seq;

	if(len > MAX_SAMPLE)
		len = MAX_SAMPLE;

	c = AudioRecReq(ac, when, len);
	seq = c->sequence;

	for(i=0; i<RETRY_COUNT; i++) {
		if(send(ac->s, c, sizeof(struct audio_command), 0) !=
			sizeof(struct audio_command)) {
				free(c);
				return -1;
		}

		start_timer(RETRY_TIMEOUT);
		while(!timeout && recv(ac->s, c, sizeof(struct audio_command) + len, 0) > 0) {
			if(c->sequence == seq) {
				i = ntohl(c->param);
				if(EARLIER(when, ntohl(c->time))) {
					memset(sample, SILENCE, len-i);
					memcpy(sample+len-i, c->data, i);
					i = len;
				} else if(i > 0)
					memcpy(sample, c->data, i);
				if(time != NULL)
					*time = ntohl(c->time);
				free(c);
				return i;
			}
		}
		stop_timer();
	}
	free(c);
	return -1;		/* retry count exceeded */
}

/*
  Open UDP socket to remote LineServer.  Return handle to audio connection
  structure.
*/
AS *OpenConnection(char *host)
{
	AS	*ac;
	struct hostent	*hp;

	if((ac = (AS *)malloc(sizeof(AS))) == NULL)
		return NULL;

	if((ac->s = socket(AF_INET, SOCK_DGRAM, 0)) == -1)
		return NULL;

	ac->a.sin_family = AF_INET;
	ac->a.sin_port = htons(AUDIO_SLAVE_PORT);

/* convert the hostname to an IP address */
	if((ac->a.sin_addr.S_un.S_addr = inet_addr(host)) == INADDR_NONE) {
		if((hp = gethostbyname(host)) == NULL) {
			free(ac);
			printf("%s: unknown host\n", host);
			return NULL;
		}
		memcpy(&ac->a.sin_addr, hp->h_addr, hp->h_length);
	}

	if(connect(ac->s, &ac->a, sizeof(struct sockaddr_in)) == -1) {
		free(ac);
		return NULL;
	}

	ac->seq = 0;

/* see if slave is alive... */
	if(AudioTime(ac, NULL) == -1) {
		free(ac);
		return NULL;
	}

	return ac;
}

/*
  Close an LineServer connection
*/
void CloseConnection(AS *ac)
{
	close(ac->s);
	free(ac);
}

/*
  Create a read CODEC request packet.  Returns pointer to packet.
*/
struct audio_command *AudioReadCodecReq(AS *ac, int reg, int len)
{
	struct audio_command *c;

	c = (struct audio_command *)malloc(sizeof(struct audio_command) + len);

	if(c != NULL) {
        	c->opcode = htonl(AUDIO_READ_CODEC);
		c->param = htonl(reg);
        	c->sequence = htonl(ac->seq++);
	}
	return c;
}

/*
  Read CODEC paramters.  Reads the specified number of parameters into the
  data area.

  Returns -1 if error (retry timeout).
*/
int AudioReadCodec(AS *ac, int reg, int len, void *data)
{
        struct audio_command    *c;
	int	i;
	int	seq;

	c = AudioReadCodecReq(ac, reg, len);
	seq = c->sequence;

	for(i=0; i<RETRY_COUNT; i++) {
		if(send(ac->s, c, sizeof(struct audio_command)+len, 0) !=
			sizeof(struct audio_command)+len) {
				free(c);
				return -1;
		}

		start_timer(RETRY_TIMEOUT);
		while(!timeout && recv(ac->s, c, sizeof(struct audio_command)+len, 0) > 0) {
			if(c->sequence == seq) {
				memcpy(data, c->data, len);
				free(c);
				return 0;
			}
		}
		stop_timer();
	}
	free(c);
	return -1;		/* retry count exceeded */
}

/*
  Create a read CODEC request packet.  Returns pointer to packet.
*/
struct audio_command *AudioWriteCodecReq(AS *ac, int reg, int len, void *data)
{
	struct audio_command *c;

	c = (struct audio_command *)malloc(sizeof(struct audio_command) + len);

	if(c != NULL) {
        	c->opcode = htonl(AUDIO_WRITE_CODEC);
		c->param = htonl(reg);
        	c->sequence = htonl(ac->seq++);
		memcpy(c->data, data, len);
	}
	return c;
}

/*
  Write CODEC paramters.  Writes the specified number of parameters from the
  data area.
*/
int AudioWriteCodec(AS *ac, int reg, int len, void *data)
{
        struct audio_command    *c;
	int	dif;
	int	i;
	int	seq;

	c = AudioWriteCodecReq(ac, reg, len, data);
	seq = c->sequence;

	for(i=0; i<RETRY_COUNT; i++) {
		if(send(ac->s, c, sizeof(struct audio_command) + len, 0) !=
			sizeof(struct audio_command) + len) {
				free(c);
				return -1;
		}

		start_timer(RETRY_TIMEOUT);
		while(!timeout && recv(ac->s, c, sizeof(struct audio_command)+len, 0) > 0) {
			if(c->sequence == seq) {
				free(c);
				return 0;
			}
		}
		stop_timer();
	}
	free(c);
	return -1;		/* retry count exceeded */
}

/*
  Convenience routine:  sets/clears bit in a single bit register.
*/
int AudioSetCodec(AS *ac, int reg, int mask, int val)
{
	char	c;

        if(AudioReadCodec(ac, reg, 1, &c) == -1)
		return -1;

        c = (c & ~mask) | val;
        return AudioWriteCodec(ac, reg, 1, &c);
}

/*
  Reset.  Sends a reset command to the LineServer audio slave.
*/
int AudioReset(AS *ac)
{
        struct audio_command    c;
	int	dif;
	int	i;
	int	seq;

/* compose packet */
        c.opcode = htonl(AUDIO_RESET);

	if(send(ac->s, &c, sizeof(struct audio_command), 0) !=
		sizeof(struct audio_command)) {
			free(c);
			return -1;
	}

	free(c);
	return -1;		/* retry count exceeded */
}
