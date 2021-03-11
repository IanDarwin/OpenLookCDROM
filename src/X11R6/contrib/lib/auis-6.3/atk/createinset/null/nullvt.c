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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/createinset/null/RCS/nullvt.c,v 1.8 1993/05/04 01:14:27 susan Exp $";
#endif


 


/* nullvt.c

	test the nullv object
*/

/*
 *    $Log: nullvt.c,v $
 * Revision 1.8  1993/05/04  01:14:27  susan
 * RCS Tree Split
 *
 * Revision 1.7.1.1  1993/02/02  01:41:20  rr2b
 * new R6tape branch
 *
 * Revision 1.7  1992/12/15  21:32:53  rr2b
 * more disclaimerization fixing
 *
 * Revision 1.6  1992/12/14  20:40:31  rr2b
 * disclaimerization
 *
 * Revision 1.5  1992/06/22  21:55:07  wjh
 * fix comment about debugging
 * remove a printout line.
 * .
 *
 * Revision 1.4  1991/09/12  16:11:48  bobg
 * Update copyright notice and rcsid
 *
 * Revision 1.3  1990/09/11  15:01:01  gk5g
 * Added code to deal with a NULL return value from im_Create and frame_New.
 *
 * Revision 1.2  89/12/12  15:00:22  ghoti
 * sync with MIT tape
 * 
 * Revision 1.1  89/07/31  15:35:47  wjh
 * Initial revision
 * 
 *  
 * Revision 1.0  88/05/14  15:40:36  wjh
 * Copied from /usr/andrew/lib/genericinset
 */


#include <stdio.h>

#include <null.ih>
#include <nullv.ih>

#include <im.ih>
#include <view.ih>
#include <frame.ih>
#include <lpair.ih>

/* include .ih files to be loaded with the binary of the file but none of whose
		methods are called in this file itself */
#define class_StaticEntriesOnly

/* required classes */
#include <environ.ih>
#include <graphic.ih>
#include <fontdesc.ih>
#include <cursor.ih>

/* classes that are anyway dynamically loaded by the
	required classes */
#include <dataobj.ih>
#include <event.ih>
#include <filetype.ih>
#include <keymap.ih>
#include <keystate.ih>
#include <menulist.ih>
#include <bind.ih>
#include <proctbl.ih>
#include <keyrec.ih>
#include <message.ih>
#include <msghndlr.ih>
#include <observe.ih>
#include <updlist.ih>

#undef class_StaticEntriesOnly


/* LL - generate a horizontal or vertical pair of views 
	first arg is H or V to choose direction, second is percent of split,
	third and fourth args are the views 
	all boundaries are movable */

enum dir {H, V};

	static struct lpair *
LL(dir, pct, top, bot)
	enum dir dir;
	short pct;
	struct lpair *top, *bot;
{
	register struct lpair *ret = lpair_New();
	if (dir == H) 
		lpair_HSplit(ret, top, bot, pct, TRUE);
	else
		lpair_VSplit(ret, top, bot, pct, TRUE);
	return ret;
}


main(argc, argv)
	register int	  argc;
	register char  **argv;
{
	register struct null *dobj;
	register struct nullview *dview, *dview2;
	boolean debug = TRUE;

	struct lpair *lpr;
	struct frame *frame;
	FILE *f;
	char *fnm;
	struct im *im;
	short i;
    
	printf("Start\n"); fflush(stdout);
  	class_Init(AndrewDir("/dlib/atk"));

	printf("Init done\n"); fflush(stdout);

/*	Cause static linking and loading of most ATK routines:
 */
	im_StaticEntry;
	view_StaticEntry;
	frame_StaticEntry;
	lpair_StaticEntry;

	environ_StaticEntry;
	graphic_StaticEntry;
	fontdesc_StaticEntry;
	cursor_StaticEntry;
	dataobject_StaticEntry;
	event_StaticEntry;
	filetype_StaticEntry;
	keymap_StaticEntry;
	keystate_StaticEntry;
	menulist_StaticEntry;
	bind_StaticEntry;
	proctable_StaticEntry;
	keyrec_StaticEntry;
	message_StaticEntry;
	msghandler_StaticEntry;
	observable_StaticEntry;
	updatelist_StaticEntry;

/* dynamically load null and nullv so nullvt need not be relinked when null is recompiled */

	im_SetProgramName("nullview test");

	fnm = NULL;
	for (i = 1; i < argc; i++) {
		if (argv[i][0] == '-')
			switch (argv[i][1]) {
				case 'f': /* Debugging. */
					debug = FALSE;
					break;
				default: /* Unknown switch. Treat it as a file... */
					fnm = argv[i]+1;
					break;
			}
		else { /* Its a file right? */
			fnm = argv[i];
		}
	}

	dobj = null_New();

	/* $$$ establish some initial contents for the data object */
	if (fnm && (f=fopen(fnm, "r")))
		null_Read(dobj, f, 0);
	else {
		null_AddDot(dobj, 10, 15);
	}

	printdata(dobj);
	dview = nullview_New();
	nullview_SetDataObject(dview, dobj);

	if((im = im_Create(NULL)) == NULL) {	/* (argument can be hostname) */
	    fprintf(stderr,"Could not create new window.\nexiting.\n");
	    exit(-1);
	}
	if((frame = frame_New()) == NULL) {	/* use a frame to get message line */
	    fprintf(stderr,"Could not allocate enough memory.\nexiting.\n");
	    exit(-1);
	}
	im_SetView(im, frame);
	frame_PostDefaultHandler(frame, "message", 
			frame_WantHandler(frame, "message"));

	printf("Start building lpair\n"); fflush(stdout);

	/* build image */
	/*  This code makes two views of the data object.  
		$$$ You may want to have some other image */
	dview2 = nullview_New();
	nullview_SetDataObject(dview2, dobj);
	lpr = LL(H, 40, (struct lpair *)dview, (struct lpair *)dview2);
	printf("Finish building lpair\n"); fflush(stdout);
	printdata(
		((struct nullview *)lpair_GetNth(lpr, 1))->header.view.dataobject
	);

	/* in general, the application layer may have a surround (eg. scrollbar) 
			for nullview, GetApplicationLayer returns its argument */
	frame_SetView(frame, lpair_GetApplicationLayer(lpr));

	/*  fork only if not debugging  
		(if it forks, the debugger cannot get at the core image)  */

	if (! debug) {
		int pid;
		if ((pid = fork()) < 0)   /* fork failed */
			exit(1);
      		else if (pid != 0)  /* exit from parent to release typescript */
			exit(0);
	}

	/* tell im to make the left view be the input focus */
	nullview_WantInputFocus(dview, dview);

	printf("Blastoff !!\n"); fflush(stdout);

	im_KeyboardProcessor();		/* Do it */
}

printdata(dobj)
	register struct null *dobj;
{
	/* $$$ same procedure as in nulltest.c */
	register struct dotlist *d;
	printf("null data object at 0x%lx\n", dobj);
	for (d = null_GetFirstDot(dobj);  d != NULL;  d = null_GetNextDot(dobj, d))
		printf("\t%d  \t%d\n", null_GetDotX(dobj, d), null_GetDotY(dobj, d));


	fflush(stdout);
}

