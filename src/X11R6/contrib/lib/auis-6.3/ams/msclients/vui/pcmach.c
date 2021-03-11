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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/msclients/vui/RCS/pcmach.c,v 2.8 1992/12/15 21:23:32 rr2b R6tape $";
#endif

#include <vui.h>
#include <panel.h>
#include <errno.h>

extern int errno;

char *ProgramPrefix = "PC";

/*
   Introduction panel is now machine dependent. There should be a panel for each
   machine type.  This has been taken out of vuiscrns.c and it is made to be extern.
*/

PANEL intro[] = {
     0,  0, NORMAL,20, "01/01/87 00:00:00 ",
     0, 65, NORMAL,15, "                  ",
     1, 14, HILITE, 0, "PPPPPPP    CCCCCC      VV    VV  UU    UU  IIIIII",
     2, 14, HILITE, 0, "PP    PP  CC     C     VV    VV  UU    UU    II",
     3, 14, HILITE, 0, "PP    PP  CC           VV    VV  UU    UU    II",
     4, 14, HILITE, 0, "PPPPPPP   CC           VV    VV  UU    UU    II",
     5, 14, HILITE, 0, "PP        CC            V    V   UU    UU    II",
     6, 14, HILITE, 0, "PP        CC     C       V  V    UU    UU    II",
     7, 14, HILITE, 0, "PP         CCCCCC         VV      UUUUUU   IIIIII",
     9, 13, HILITE, 0, "Personal   Computer      Visual     User   Interface",
    11, 23, HILITE, 0, "an Andrew Message System program",
    15,  6, HILITE, 0, "Keys: Arrows to select commands      Press the first letter of a",
    16,  6, HILITE, 0, "      Enter to execute a command     command to select that command.",
    17, 43, HILITE, 0, "Repeat the letter when there are",
    18, 43, HILITE, 0, "similar commands.",
	/* FIXING UP INVOKED BELOW FOR THIS PANEL, IF YOU CHANGE THIS, CHANGE FixUpMenus */
     /* #define INTRO_START 15 defined in vui.h */
    17, 12, NORMAL, 0, ESC_STRING,
    17, 37, HILITE, 0, "to return from a menu.",
    18, 12, NORMAL, 0, F1_STRING,
    18, 21, HILITE, 0, "for help",
     NULL_PANEL
   };

/*
  main is machine dependant because on the macintosh we need to load
  dynamicly bound strings before starting the real program
*/
int main(argc,argv)
int argc;
char **argv;
{
    int exit_type;
    char *ExecVec[3];

    if ((exit_type = vuimain(argc,argv)) != FALSE) {
	switch (exit_type) {
	    case LOGOUT_ALL:
		ExecVec[1] = NIL;
		break;
	    case LOGOUT_MSSERVER:
		ExecVec[1] = "messageserver";
		break;
	    case LOGOUT_PCSERVER:
		ExecVec[1] = "pcserver";
	}
	ExecVec[0] = "logout";
	ExecVec[2] = NIL;
	execvp("LOGOUT.EXE", ExecVec);
	printf("[VUI]: Could not Execute LOGOUT.EXE program(error %d).\n",errno);
    }
}

/*
  dummy refresh for the pc
*/
int vuirefresh()
{
}
