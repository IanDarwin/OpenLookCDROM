/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/


#ifndef _deamonlog_
#define _deamonlog_

extern char *deal_fixfield();
extern void deal_flush();
extern int deal_open();
extern void deal_close();
extern void deal_close_silently();
extern char *deal_get_log_name();
extern void deal_willfork();
extern void deal_log_your_self();


extern void deal_log();
#define DEALOG(xx_arg) {deal_log xx_arg ;}

/*convert an ip address into a comma seperated list of integers,
  suitable for use as a printf argument with the format string
  %d.%d.%d.%d
*/
#define low_eight_bits (0x000000ffl)
#define IP_ADDR_PARTS_INTERNAL(xx) \
  ((int)(((xx)>>24)&low_eight_bits)), \
  ((int)(((xx)>>16)&low_eight_bits)), \
  ((int)(((xx)>> 8)&low_eight_bits)), \
  ((int)((xx)&low_eight_bits))
#define IP_ADDR_PARTS(xx) IP_ADDR_PARTS_INTERNAL((htonl(xx)))
#endif /* _deamonlog_ */
