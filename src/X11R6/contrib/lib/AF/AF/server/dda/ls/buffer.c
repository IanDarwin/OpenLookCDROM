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

#include <stdlib.h>
#include <server/include/dia.h>
#include <server/include/misc.h>
#include <server/include/acstruct.h>
#include <server/include/task.h>
#include "buffer.h"
#include "uatables.h"

/* ----- Buffer manipulation routines ----- */

/*
  Mixes data into buffer.
*/
static void memmix(unsigned char *dest, unsigned char *src, int len)
{
	while(len--) {
		*dest = MIX(*dest, *src++);
		dest++;
	}
}

/*
  Move bytes to (dir = 0) or from (dir = 1) a buffer.

  If 'mix' is true, the data is mixed in, otherwise it is copied in.
  'gain' specifies the gain adjustment done to the data before it is
  moved (applies to writes only).
*/
void buffer_move(int dir, ATime time, unsigned char *buf, int buflen, 
	unsigned char *dp, int len, int mix, int gain)
{
	int	slen;
	void	(*mem)();
	int	i;
	extern	void memcpy();
	unsigned char	*gp;			/* gain table pointer */

	slen = min(len, buflen - (time % buflen));
	mem = mix ? memmix : memcpy;

/* adjust gain, if necessary */
	if(dir == 0 && gain != 0) {
		gp = gain_table[gain + abs(GAIN_MIN)];
		for(i=0; i<len; i++)
			dp[i] = gp[dp[i]];
	}

/* copy in two parts... */
	if(dir)
		(*mem)(dp, buf + (time % buflen), slen);
	else
		(*mem)(buf + (time % buflen), dp, slen);

	if(slen < len) {
		if(dir)
			(*mem)(dp + slen, buf, len-slen);
		else
			(*mem)(buf, dp + slen, len-slen);
	}
}

/*
  Set a range of a buffer to the specified value.
*/
void buffer_set(ATime time, unsigned char *buf, int buflen, 
		int len, unsigned char val)
{
	int	slen;

	slen = min(len, buflen - (time % buflen));

	memset(buf + (time % buflen), val, slen);
	if(slen < len)
		memset(buf, val, len-slen);
}
