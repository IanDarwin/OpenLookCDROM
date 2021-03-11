#ifndef _TERMP_H
#define _TERMP_H

/* TermP.h,v 1.2 1994/05/27 06:20:53 me Exp */

/*
 * This file is part of the Emu system.
 *
 * Copyright 1990 by PCS Computer Systeme, GmbH. Munich, West Germany.
 * 
 * Copyright 1994 by Jordan K. Hubbard and Michael W. Elbel
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL PCS, THE AUTHORS, OR THEIR HOUSEPETS BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. SO DON'T SUE US.
 * THANK YOU.
 */

/*
 * Term widget private include file.
 *
 * Author: Jordan K. Hubbard
 * Date: June 6th, 1990.
 * Description: Class specific and private data for the term widget.
 *
 * Revision History:
 *
 * TermP.h,v
 * Revision 1.2  1994/05/27  06:20:53  me
 * New copyright message
 *
 * Revision 1.1.1.1  1994/05/22  11:22:44  me
 * Initial import into CVS
 *
 * Revision 1.6  90/11/13  14:57:06  jkh
 * Double fd version.
 */

#include <X11/IntrinsicP.h>
#include <X11/CompositeP.h>

#include "common.h"
#include "Term.h"
#include "trie.h"

/* For convenience */
#define CANVAS_W(w)		((w)->term.canvas)
#define RESIZE_LOCKED(w)	((w)->term.resizing == TRUE)
#define RESIZE_UNLOCKED(w)	((w)->term.resizing == FALSE)
#define LOCK_RESIZE(w)		((w)->term.resizing = TRUE)
#define UNLOCK_RESIZE(w)	((w)->term.resizing = FALSE)

/*
 * So our class can keep track of processes. Too bad the toolkit has no
 * way of dealing with signals; it wouldn't be necessary to do things this
 * way.
 */
typedef struct _ptable {
     TermWidget w;
     struct _ptable *next;
} PTable;

typedef struct {
     int sig_cnt;
     PTable *procs;
} TermClassPart;

typedef struct _TermClassRec {
     CoreClassPart	core_class;
     CompositeClassPart composite_class;
     TermClassPart	term_class;
} TermClassRec;

typedef struct {
     /* Resources */
     String command;			/* command name			*/
     String *command_args;		/* command arguments		*/
     String term_type;			/* terminal type		*/
     String rops;			/* initial rops			*/
     Dimension w_inc, h_inc;		/* width and height incs	*/
     Widget canvas;			/* which widget's the canvas	*/
     XtCallbackList in_parse;		/* input parser(s)		*/
     XtCallbackList out_parse;		/* output parser(s)		*/
     XtCallbackList proc_death;		/* process death callback	*/
     XtCallbackList iop_req;		/* for iop requests to parent	*/
     GFuncPtr layout_kids;		/* for external layout purposes	*/
     int read_size;			/* number of chars to read in	*/
     Boolean utmp_inhibit;		/* don't add utmp entries	*/
     Boolean login_shell;		/* invoke shell as login shell	*/

     /* Private state */
     ComBlockPtr cb;			/* current communications block	*/
     ComBlockPtr save;			/* saved communications block	*/
     String tname;			/* name of allocated PTY	*/
     pid_t pid;				/* process ID of child process	*/
     uid_t uid;				/* uid of invoking user		*/
     gid_t gid;				/* gid of invoking user		*/
     int master;			/* pty master file descriptor	*/
     int slave;				/* pty slave file descriptor	*/
     XtInputId xi, xo;			/* toolkit input/output ID's	*/
     State *zero_state;			/* root of possible trie parser	*/
     Possible_state *current_states;	/* current trie possibilities	*/
     int chars_to_parse;		/* now many chars buffered up	*/
     int char_parse_mark;		/* point of interest in buffer	*/
     unsigned char buf[BUF_SZ];		/* buffer for unparsed chars	*/
     unsigned char immediate;		/* do next request right away	*/
     Rparse *rparse_list;		/* reverse parser operations	*/
     Generic init_tty;			/* parent's saved tty settings	*/
     Boolean resizing;			/* in the middle of resizing?	*/
     /**/
     int save_pos;
     char save_buf[BUF_SZ];
} TermPart;

typedef struct _TermRec {
     CorePart           core;
     CompositePart	composite;
     TermPart		term;
} TermRec;

/* Routines shared across multiple parts of the widget */

/* From the tty section */
Import Boolean tty_open(TermWidget);
Import void tty_set_nonblocking(TermWidget, int);
Import String tty_find_pty(TermWidget);
Import Generic tty_get_values(TermWidget);
Import Generic tty_get_sane(TermWidget);
Import void tty_close(TermWidget);
Import void tty_set_size(TermWidget w, int rows, int cols,
			 int width, int height);
Import void tty_add_utmp(TermWidget);
Import void tty_remove_utmp(TermWidget);
Import void tty_set_values(TermWidget, Generic);

/* From the process section */
Import void process_init(TermWidget);
Import void process_create(TermWidget);
Import void process_destroy(TermWidget, int);
Import void process_signal(TermWidget, int);

/* From the parser section */
Import Boolean parser_install(TermWidget);
Import void parser_remove(TermWidget, String);
Import void iparse(TermWidget, int *, XtInputId);
Export void oparse(XtPointer, int *, XtInputId *);
Import void rparse(TermWidget, ComBlockPtr);
Import void dispatch(TermWidget, ComBlockPtr);

/* From widget */
Import void XpNrememberWidget(TermWidget);
Import void XpNremoveRememberedWidget(TermWidget);
Import TermWidget XpNfindWidgetByPid(pid_t);

#endif /* _TERMP_H */
