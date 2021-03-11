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


#ifdef IBMPC

#define KEYCODE_LEFT        (256*75)
#define KEYCODE_RIGHT       (256*77)
#define KEYCODE_UP          (256*72)
#define KEYCODE_DOWN        (256*80)
#define KEYCODE_TAB         (256*15)+9
#define KEYCODE_CTRL_TAB    (256*116)
#define KEYCODE_SHIFT_TAB   (256*15)
#define KEYCODE_CTRL_LEFT   (256*115)
#define KEYCODE_RETURN      (256*28)+13
#define KEYCODE_FAKERETURN  (256*28)+13
#define KEYCODE_INSERT      (256*82)
#define KEYCODE_DELETE      (256*83)
#define KEYCODE_HOME        (256*71)
#define KEYCODE_END         (256*79)
#define KEYCODE_BACKSPACE   (256*14)+8
#define KEYCODE_DEL         (256*14)+8	/* this matches BS and will be ignored */
#define KEYCODE_CTRL_END    (256*117)
#define KEYCODE_ESCAPE      (256+27)
#define KEYCODE_CTRL_HOME   (256*119)
#define KEYCODE_PAGE_DOWN   (256*81)
#define KEYCODE_PAGE_UP     (256*73)
#define KEYCODE_F1          (256*59)
#define KEYCODE_F2          (256*60)
#define	KEYCODE_F3	    (256*61)
#define KEYCODE_ALT_F1      (256*104)
#define KEYCODE_REDRAW	    (256*1)+1	/* mas V1.3 : this key value is fake! */

#else /* IBMPC */

#define KEYCODE_LEFT	    (2)
#define KEYCODE_RIGHT	    (6)
#define KEYCODE_UP	    (16)
#define KEYCODE_DOWN	    (14)
#define KEYCODE_TAB	    (9)
#define KEYCODE_CTRL_TAB    (256 + 9)
#define KEYCODE_SHIFT_TAB   (256 + 2)
#define KEYCODE_CTRL_LEFT   (256 + 2)
#define KEYCODE_RETURN	    (13)
#define KEYCODE_FAKERETURN  (10)
#define KEYCODE_INSERT	    (15)
#define KEYCODE_DELETE	    (4)
#define KEYCODE_HOME	    (1)
#define KEYCODE_END	    (5)
#define KEYCODE_BACKSPACE   (8)
#define KEYCODE_DEL	    (127)
#define KEYCODE_CTRL_END    (11)
#define KEYCODE_ESCAPE	    (7)
#define KEYCODE_CTRL_HOME   (256 + '<')
#define KEYCODE_PAGE_DOWN   (22)
#define KEYCODE_PAGE_UP	    (256 + 'v')
#define KEYCODE_F1	    (256 + '1')
#define KEYCODE_F2	    (256 + '2')
#define	KEYCODE_F3	    (256 + '3')
#define KEYCODE_ALT_F1	    (256 + '!')
#define KEYCODE_REDRAW	    (12)

#endif /* IBMPC */
