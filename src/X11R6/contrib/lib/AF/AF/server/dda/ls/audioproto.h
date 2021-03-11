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
  defs for the UDP protocol used to talk to the LineServer audio slave

  June, 1991
  Andrew C. payne
*/

#define AUDIO_SLAVE_PORT 4102		/* UDP port used */

struct audio_command {
	int	sequence;		/* packet sequence number */
	int	time;			/* AudioServer time */
	int	opcode;			/* function to perform */
	int	param;			/* opcode parameter #1 */
	char	data[0];		/* packet data */
};

/* Opcodes */
#define AUDIO_PLAY		1	/* play a sample */
#define AUDIO_RECORD		2	/* get a sample */
#define AUDIO_READ_CODEC	3	/* read codec register */
#define AUDIO_WRITE_CODEC	4	/* write codec regsiter */
#define AUDIO_LOOPBACK		5	/* echo packet back to sender */
#define AUDIO_RESET		6	/* reset the audio system */
