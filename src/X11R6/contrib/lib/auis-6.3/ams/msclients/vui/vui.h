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

/* 
 *      Common declarations, typedefs, and constants for VUI
 */


/* Version number for CUI-client intrface.  If an end user interface
        wishes to, it can track major and minor changes to the
        CUI and issue warnings when it thinks it is out of date */

#include <cuivers.h>
#include <sys/param.h>
#include <stdio.h>
#include <errprntf.h>
#include <vuimsgs.h>

#ifndef TRUE
#define TRUE    1
#endif /* TRUE */

#ifndef FALSE
#define FALSE   0
#endif /* FALSE */

#ifndef NULL
#define NULL    0
#endif /* NULL */

#ifndef NIL
#define NIL     (char *)0
#endif /* NIL */

#ifndef PRIVATE
#define PRIVATE static
#endif /* PRIVATE */

#define Boolean char

extern int CUIDebugging;
extern FILE *debugfile;

struct dir_page_list {
        int path_entry;
        long map_offset;
        struct dir_page_list *nextpage, *prevpage;
   };

struct msg_page_list {
        long offset;
        struct msg_page_list *nextpage, *prevpage;
   };

struct addr_list {
    char *data;
    struct addr_list *nextaddr, *prevaddr;
    char longaddr; /* flag for long addresses broken into multiple lines */
};

struct head_list {
        char *header;
        struct head_list *next;
        };

#define MAIL 0
#define FOLDER 1
#define BBOARD 2

#define ALL 0
#define SUBSCRIBED 1
#define CHANGED 2

#define AUTO 0
#define MANUAL 1

#define WRITEFILECHUNK 2000

#define MAXBODY 5000            /* Size of chunks in which files and
                                   messages will be delivered via SNAP */

#define N_TCS_LINES 8  /* Number of lines for To, CC, Subj =1+3 to lines+1 subj line + 3 cc lines*/
#define LINE_LENGTH 80
#define TABLEN  8

/* Number of snapshots to request from ms in one gulp */
#define CUIMACH_GULPSIZE 10
 /* Size of header buffer for each SNAP transaction */
 /* Ought to remain a multiple of AMS_SNAPSHOTSIZE */
#define HEADBUFSIZE (CUIMACH_GULPSIZE * AMS_SNAPSHOTSIZE)
#define DATELEN  7
#define TIMELEN 10

#ifdef IBMPC
#define CUI_READLIBERALLY /* turn on fuzzy matching of file-read byte counts. */
#endif /* IBMPC */

#define MENU_DO_OVER (-99) 
#define MENU_ERROR (-89) 
#define MENU_EMPTY (-79) 

#define	VUI_NORMAL_EXIT	0
#define	VUI_EXIT_NOSAVE	1

#define	LOGOUT_ALL	1
#define	LOGOUT_MSSERVER	2
#define	LOGOUT_PCSERVER	3

#ifndef IBMPC
#define ESC_STRING   "^G"
#define F1_STRING    "<Esc>1"
#endif /* IBMPC */

#ifdef IBMPC
#define ESC_STRING   "Esc"
#define F1_STRING    "F1"
#endif /* IBMPC */

#define	INTRO_START 15	/* where the info start in main vui panel */
