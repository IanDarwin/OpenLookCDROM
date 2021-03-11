/* -*-C-*-
********************************************************************************
*
* File:         xlisp.c
* RCS:          $Header: /users/npm/src/winterp/src-server/xlisp/RCS/xlisp.c,v 2.4 1994/06/06 15:59:18 npm Exp $
* Description:  xlisp.c - a small implementation of lisp with object-oriented programming
* Author:       David Michael Betz. WINTERP portions by Niels Mayer;
*		XLISP-PLUS by Tom Almy with contributions from Johnny
*		Greenblatt, Neal Holtz, Niels Mayer, Blake McBride, Mikael
*		Pettersson, Luke Tierney, Ken Whedbee, Pete Yadlowsky.
* Created:      
* Modified:     Mon Jun  6 03:04:16 1994 (Niels Mayer) npm@indeed
* Language:     C
* Package:      N/A
* Status:       X11r6 contrib release
*
* Copyright (C) 1994, Enterprise Integration Technologies Corp. and Niels Mayer.
* WINTERP 1.15-1.99, Copyright (c) 1993, Niels P. Mayer.
* WINTERP 1.0-1.14, Copyright (c) 1989-1992 Hewlett-Packard Co. and Niels Mayer.
* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney
* XLISP version 2.1, Copyright (c) 1989, by David Betz.
*
* Permission to use, copy, modify, distribute, and sell this software and its
* documentation for any purpose is hereby granted without fee, provided that
* the above copyright notice appear in all copies and that both that
* copyright notice and this permission notice appear in supporting
* documentation, and that the name of Enterprise Integration Technologies,
* Hewlett-Packard Company, Niels Mayer, Luke Tierney and David Betz not be
* used in advertising or publicity pertaining to distribution of the software
* without specific, written prior permission.  Enterprise Integration
* Technologies, Hewlett-Packard Company, Niels Mayer, Luke Tierney and David
* Betz make no representations about the suitability of this software for any
* purpose. It is provided "as is" without express or implied warranty.
*
* ENTERPRISE INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY, NIELS MAYER,
* LUKE TIERNEY AND DAVID BETZ DISCLAIM ALL WARRANTIES WITH REGARD TO THIS
* SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS,
* IN NO EVENT SHALL ENTERPRISE INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD
* COMPANY, NIELS MAYER, LUKE TIERNEY NOR DAVID BETZ BE LIABLE FOR ANY SPECIAL,
* INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
* LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
* OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
* PERFORMANCE OF THIS SOFTWARE.
*
********************************************************************************
*/
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/xlisp/RCS/xlisp.c,v 2.4 1994/06/06 15:59:18 npm Exp $";

/*
*------------------------------------------------------------------------------
* See ./winterp/COPYRIGHT for information on contacting the authors.
* Please e-mail comments, modifications, questions, improvements and
* bugfixes to the WINTERP mailing list winterp@netcom.com. Please send 
* mailing list subscribe/unsubscribe notices to winterp-request@netcom.com .
* Post XLISP-specific questions/information to the USENET newsgroup
* comp.lang.lisp.x.
*------------------------------------------------------------------------------
*/

#include "xlisp.h"

/* define the banner line string */
#ifdef STRUCTS
#define BANNER	"XLISP version 2.1c, Copyright (c) 1985-1989, by David Betz\n\
As modified by Thomas Almy"
#else
#define BANNER	"XLISP version 2.0c, Copyright (c) 1985-1989, by David Betz\n\
As modified by Thomas Almy"
#endif

/* global variables */
#ifdef SAVERESTORE
jmp_buf top_level;
#endif

/* external variables */
extern LVAL s_stdin,s_stdout,s_evalhook,s_applyhook;
extern LVAL s_1plus,s_2plus,s_3plus,s_1star,s_2star,s_3star,s_minus;
extern int xltrcindent;
extern int xldebug;
extern LVAL true;
extern FILEP tfp;

/* usage - print command line usage, then quit TAA addition */
#ifdef ANSI
VOID usage(void) {
#else
VOID usage() {
#endif
#ifdef SAVERESTORE
    fprintf(stderr,"Valid Arguments:\n\t-?\tThis help\n\
\t-tfname\tOpen transcript (dribble) file fname\n\
\t-v\tLoad verbosely\n\
\t-w\tDon't restore from xlisp.wks\n\
\t-wfname\tRestore from fname\n\
\tfname\tLoad file fname\n");
#else
    fprintf(stderr,"Valid Arguments:\n\t-?\tThis help\n\
\t-tfname\tOpen transcript (dribble) file fname\n\
\t-v\tLoad verbosely\n\
\tfname\tLoad file fname\n");
#endif
    exit(1);
}

/* main - the main routine */
#ifdef ANSI
VOID CDECL main(int argc, char *argv[])
#else
VOID main(argc,argv)
  int argc; char *argv[];
#endif
{
    char *transcript;
    CONTEXT cntxt;
    int verbose,i;
    LVAL expr;
#ifdef SAVERESTORE
    char *resfile = "xlisp.wks";    /* TAA mod -- command line restore file */
#endif

    /* setup default argument values */
    transcript = NULL;
    verbose = FALSE;

    /* parse the argument list switches */
#ifndef LSC
    for (i = 1; i < argc; ++i)
	if (argv[i][0] == '-')
	    switch(tolower(argv[i][1])) {
	    case '?':	/* TAA MOD: added help */
		usage();
	    case 't':
		transcript = &argv[i][2];
		break;
	    case 'v':
		verbose = TRUE;
		break;
#ifdef SAVERESTORE
	    case 'w':
		resfile = &argv[i][2];
		break;
#endif
	    default: /* Added to print bad switch message */
		fprintf(stderr,"Bad switch: %s\n",argv[i]);
		usage();
	    }
#endif

    /* initialize and print the banner line */
    osinit(BANNER);

    /* setup initialization error handler */
    xlbegin(&cntxt,CF_TOPLEVEL|CF_CLEANUP|CF_BRKLEVEL,(LVAL)1);
    if (setjmp(cntxt.c_jmpbuf))
	xlfatal("fatal initialization error");
#ifdef SAVERESTORE
    if (setjmp(top_level))
	xlfatal("RESTORE not allowed during initialization");
#endif

    /* initialize xlisp */
#ifdef SAVERESTORE
    i = xlinit(resfile);
#else
    i = xlinit(NULL);
#endif

    /* reset the error handler, since we know what "true" is */
    xlend(&cntxt);
    xlbegin(&cntxt,CF_TOPLEVEL|CF_CLEANUP|CF_BRKLEVEL,true);

    /* open the transcript file */
    if (transcript!=NULL && (tfp = OSAOPEN(transcript,CREATE_WR)) == CLOSED) {
	/* TAA Mod -- quote name so "-t foo" will indicate no file name */
	sprintf(buf,"error: can't open transcript file: \"%s\"",transcript);
	stdputstr(buf);
    }

    /* load "init.lsp" */
    if (i && (setjmp(cntxt.c_jmpbuf) == 0))
	xlload("init.lsp",TRUE,FALSE);

    /* load any files mentioned on the command line */
    if (setjmp(cntxt.c_jmpbuf) == 0)
	for (i = 1; i < argc; i++)
	    if (argv[i][0] != '-' && !xlload(argv[i],TRUE,verbose))
		xlerror("can't load file",cvstring(argv[i]));

    /* target for restore */
#ifdef SAVERESTORE
    if (setjmp(top_level))
	xlbegin(&cntxt,CF_TOPLEVEL|CF_CLEANUP|CF_BRKLEVEL,true);
#endif

    /* protect some pointers */
    xlsave1(expr);

    /* main command processing loop */
    for (;;) {

	/* setup the error return */
	if (setjmp(cntxt.c_jmpbuf)) {
	    setvalue(s_evalhook,NIL);
	    setvalue(s_applyhook,NIL);
	    xltrcindent = 0;
	    xldebug = 0;
	    xlflush();
	}

	/* print a prompt */
/*	stdputstr("> "); */
	if (!redirectin) dbgputstr("> ");

	/* read an expression */
	if (!xlread(getvalue(s_stdin),&expr))
	    break;

	/* save the input expression */
	xlrdsave(expr);

	/* evaluate the expression */
	expr = xleval(expr);

	/* save the result */
	xlevsave(expr);

	/* Show result on a new line -- TAA MOD to improve display */
#ifdef BETTERIO
	xlfreshline(getvalue(s_stdout));
#endif

	/* print it */
	stdprint(expr);
    }
    xlend(&cntxt);

    /* clean up */
    wrapup();
}

/* xlrdsave - save the last expression returned by the reader */
VOID xlrdsave(expr)
  LVAL expr;
{
    setvalue(s_3plus,getvalue(s_2plus));
    setvalue(s_2plus,getvalue(s_1plus));
    setvalue(s_1plus,getvalue(s_minus));
    setvalue(s_minus,expr);
}

/* xlevsave - save the last expression returned by the evaluator */
VOID xlevsave(expr)
  LVAL expr;
{
    setvalue(s_3star,getvalue(s_2star));
    setvalue(s_2star,getvalue(s_1star));
    setvalue(s_1star,expr);
}

/* xlfatal - print a fatal error message and exit */
VOID xlfatal(msg)
  char *msg;
{
    xoserror(msg);
    wrapup();
}

/* wrapup - clean up and exit to the operating system */
VOID wrapup()
{
    if (tfp != CLOSED)
	OSCLOSE(tfp);
    osfinish();
    exit(0);
}
