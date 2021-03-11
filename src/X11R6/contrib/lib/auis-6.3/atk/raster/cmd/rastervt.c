/*LIBS: -lframe -ltext -lsupviews -lsupport -lbasics -lclass -lmalloc -lerrors -lutil
*/

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/raster/cmd/RCS/rastervt.c,v 2.10 1992/12/15 21:40:02 rr2b R6tape $";
#endif

/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

 

/* testv.c

	test rasterview
*/

#include <andyenv.h>
#include <stdio.h>
#include <class.h>

#include <raster.ih>
#include <rasterv.ih>

#include <im.ih>
#include <view.ih>
#include <frame.ih>
#include <lpair.ih>

/* include the ones utilized, but not by this .c file itself */
#define class_StaticEntriesOnly

/* required classes */
#include <environ.ih>
#include <graphic.ih>
#include <fontdesc.ih>
#include <cursor.ih>
#ifdef WM_ENV
#include <wws.ih>
#include <wgraphic.ih>
#include <wim.ih>
#include <wcursor.ih>
#include <wfontd.ih>
#include <mrl.ih>
#endif /* WM_ENV */

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


main(argc, argv)
	register int	  argc;
	register char  **argv;
{
	register struct raster *dobj;
	register struct rasterview *dview;

	struct frame *frame;
	FILE *f;
	char *fnm;
	struct im *im;
	short i;

	printf("Start\n"); fflush(stdout);
	class_Init(".");		/* set up classpath */
	printf("Init done\n"); fflush(stdout);

	im_StaticEntry;
	view_StaticEntry;
	frame_StaticEntry;
	lpair_StaticEntry;

	environ_StaticEntry;
	graphic_StaticEntry;
	fontdesc_StaticEntry;
	cursor_StaticEntry;
#ifdef WM_ENV
	wmws_StaticEntry;
	wmim_StaticEntry;
	wmgraphic_StaticEntry;
	wmcursor_StaticEntry;
	wmfontdesc_StaticEntry;
	mrl_StaticEntry;
#endif /* WM_ENV */
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


	printf("About to New\n"); fflush(stdout);

	fnm = NULL;
	for (i = 1; i < argc; i++) {
		if (argv[i][0] == '-')
			switch (argv[i][1]) {
				case 'd': /* Debugging. */
					/* debug = TRUE; */
					break;
				default: /* Unknown switch. Treat it as a file... */
					fnm = argv[i]+1;
					break;
			}
		else { /* Its a file right? */
			fnm = argv[i];
		}
	}

	dobj = raster_New();
	printf("Newed\n"); fflush(stdout);

	if (fnm && (f=fopen(fnm, "r")))
		raster_Read(dobj, f, 0);
	else 
		raster_Read(dobj, fopen(environ_AndrewDir("/lib/rasters/people/doug.ras"), "r"), 0);
	printdata(dobj);
	dview = rasterview_New();
	printf("View Newed\n"); fflush(stdout);
	rasterview_SetDataObject(dview, dobj);
	printf("View SDOed\n"); fflush(stdout);

	if((im = im_Create(NULL)) == NULL) {	/* (argument can be hostname) */
	    fprintf(stderr,"rastervt: Could not create new window; exiting.\n");
	    exit(-1);
	}
	if((frame = frame_New()) == NULL) {	/* use a frame to get message line */
	    fprintf(stderr,"rastervt: Could not allocate enough memory; exiting.\n");
	    exit(-1);
	}
	im_SetView(im, frame);
	frame_PostDefaultHandler(frame, "message", 
			frame_WantHandler(frame, "message"));

	printf("SetView\n"); fflush(stdout);
	frame_SetView(frame, dview);

	printf("Blastoff !!\n"); fflush(stdout);

	im_KeyboardProcessor();		/* Do it */
}

printdata(dobj)
	register struct raster *dobj;
{
	printf ("%d x %d\n", raster_GetWidth(dobj), 
			raster_GetHeight(dobj));
	fflush(stdout);
}

