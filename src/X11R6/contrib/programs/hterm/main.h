/*
 *	$XConsortium: main.h,v 1.5 91/02/06 16:00:15 gildea Exp $
 */

/*
 * Copyright 1987 by Digital Equipment Corporation, Maynard, Massachusetts.
 *
 *                         All Rights Reserved
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notice appear in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation, and that the name of Digital Equipment
 * Corporation not be used in advertising or publicity pertaining to
 * distribution of the software without specific, written prior permission.
 *
 *
 * DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
 * DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
 * ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
 * ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 * SOFTWARE.
 */

/*
** Copyright 1993 by GUI Consortium, CAIR/KAIST
**
** KAIST disclaims all warranties with regard to this software,
** including all implied warranties of merchantability
** and fitness, in no event shall KAIST be liable for
** any special, indirect or consequential damages or any damages
** whatsoever resulting from loss of use, data or profits, whether in an
** action of contract, negligence or other tortious action, arising out of
** or in connection with the use or performance of this software.
*/

#ifdef I18N
#define DEFFONTSET	"-*-*-medium-r-normal--16-*-*-*-c-*-*-*"
#define DEFINPUTSTYLE	"OverTheSpot"
#else
#define	DEFFONT			"fixed"
#define	DEFBOLDFONT		NULL 	/* no bold font uses overstriking */
#endif
#define	DEFBORDER		2
#define	DEFBORDERWIDTH		2
