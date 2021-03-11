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

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/msclients/vui/RCS/andmach.c,v 2.8 1992/12/15 21:23:32 rr2b R6tape $";
#endif

#include <andrewos.h>
#include <curses.h>
#include <vui.h>
#include <panel.h>

char *ProgramPrefix = "";

/*
   INTRO panel taken out of vuiscrns to become machine dependent.
*/

PANEL intro[] = {
     0,  0, NORMAL,20, "01/01/87 00:00:00 ",
     0, 65, NORMAL,15, "                  ",
     1, 21, HILITE, 0, "VV    VV       UU    UU       IIIIII",
     2, 21, HILITE, 0, "VV    VV       UU    UU         II",
     3, 21, HILITE, 0, "VV    VV       UU    UU         II",
     4, 21, HILITE, 0, "VV    VV       UU    UU         II",
     5, 21, HILITE, 0, " V    V        UU    UU         II",
     6, 21, HILITE, 0, "  V  V         UU    UU         II",
     7, 21, HILITE, 0, "   VV           UUUUUU        IIIIII",
9, 16, HILITE, 0, "the   Visual          User        Interface",
    11, 24, HILITE, 0, "an Andrew Message System program",
    15, 10, HILITE, 0, "Keys: Arrows to select commands      Press the first letter of a",
    16, 10, HILITE, 0, "      Enter to execute a command     command to select that command.",
    17, 47, HILITE, 0, "Repeat the letter when there are",
    18, 47, HILITE, 0, "similar commands.",
	/* FIXING UP INVOKED BELOW FOR THIS PANEL, IF YOU CHANGE THIS, CHANGE FixUpMenus */
     /* #define INTRO_START 15 defined in vui.h */
    17, 16, NORMAL, 0, ESC_STRING,
    17, 41, HILITE, 0, "to return from a menu.",
    18, 16, NORMAL, 0, F1_STRING,
    18, 25, HILITE, 0, "for help",
     NULL_PANEL
   };
/*
  main is machine dependant because on the macintosh we need to load
  dynamicly bound strings before starting the real program
*/
int main(argc,argv)
int argc;
char **argv;
{return vuimain(argc,argv);
}

int vuirefresh()
{return refresh();
}
