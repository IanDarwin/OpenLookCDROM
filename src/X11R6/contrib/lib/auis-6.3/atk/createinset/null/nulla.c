/* ********************************************************************** *\
 *         Copyright Carnegie Mellon University, 1992 - All Rights Reserved      *
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/createinset/null/RCS/nulla.c,v 1.5 1994/02/15 16:13:23 wjh Exp $";
#endif


 

/*
 * app for null
 *
 *	Program to execute the null inset as an application
 *
 *	usage:  null   [<saved null inset file>]
 *
 */

/*
 * $Log: nulla.c,v $
 * Revision 1.5  1994/02/15  16:13:23  wjh
 * define empty InitializeClass
 * remove override of Run
 * provide a filename in case Save is selected
 * clarify action of super_ParseArgs
 * call super_Start to get colors
 *
 * Revision 1.4  1993/05/04  01:14:27  susan
 * RCS Tree Split
 *
 * Revision 1.3.1.1  1993/02/02  01:41:28  rr2b
 * new R6tape branch
 *
 * Revision 1.3  1992/12/15  21:32:53  rr2b
 * more disclaimerization fixing
 *
 * Revision 1.2  1992/12/14  20:40:31  rr2b
 * disclaimerization
 *
 * Revision 1.1  1992/06/22  21:47:49  wjh
 * Initial revision
 *
 * Revision 1.1  1992/06/22   wjh
 * 	Created
 */

#include <andrewos.h>

#include <filetype.ih>
#include <view.ih>
#include <im.ih>
#include <frame.ih>
#include <buffer.ih>

#include <null.ih>
#include <nullv.ih>
#include <nulla.eh>

	boolean
nullapp__InitializeClass(classID)
	struct classheader *classID;
{
	return  TRUE;
}

	boolean
nullapp__InitializeObject(classID, self)
	struct classheader *classID;
	struct nullapp *self;
{
	self->inputfile = NULL;
	self->dobj = null_New();
	nullapp_SetMajorVersion(self, MAJORVERSION);
	nullapp_SetMinorVersion(self, MINORVERSION);
	nullapp_SetFork(self, TRUE);

	return  TRUE;
}

	void
nullapp__FinalizeObject(classID, self)
	struct classheader *classID;
	struct nullapp *self;
{
	null_Destroy(self->dobj);
	/* do not free self->inputfile because it is in argv */
}


/*
 * usage statement
 */
	static void
show_usage(self)
	struct nullapp *self;
{
	fprintf(stderr,  "Usage: %s [file]\n",  nullapp_GetName(self));
}


	boolean 
nullapp__ParseArgs(self, argc, argv)
	struct nullapp *self;
	int argc;
	char **argv;
{
	char *name;

	/* super_ParseArgs() passes across the "runapp" and its switches,
		leaving "nulla" as the first arg. 
		The following switches are also processed and removed:  
			-fg -bg -display -geometry -host  -iconic -ni -profile
	 */
	
	if( ! super_ParseArgs(self, argc, argv))
		return FALSE;

	while(*++argv != NULL && **argv == '-') {
		switch((*argv)[1]){
			case 'd':		
				nullapp_SetFork(self, FALSE);
				break;
			default:
				fprintf(stderr,"%s - unrecognized switch: %s\n",
					nullapp_GetName(self), *argv);
				show_usage(self);
				return FALSE;
		}
	}

	/* get the name of the null inset file, if any */
	self->inputfile = *argv++;

	/* testing nullapp_GetFork(self) determines if the -d switch 
		was set to start debugging */

	if ( ! nullapp_GetFork(self))
		printf("Args parsed.  dobj @ 0x%lx\n", self->dobj);

	return TRUE;
}

	boolean
nullapp__Start(self)
	struct nullapp *self;
{
	FILE *f;
	long objectID;
	long val;
	char *objectType;
	struct attributes *attributes;
	int c;

	/* we will use the frame/buffer system, 
			though it is not necessary */

	struct buffer *buffer;
	char tempName[100];
	struct frame *frame;
	struct im *im;
	struct view *v;

	if ( ! nullapp_GetFork(self))
		printf("Start.   file: %s\n", self->inputfile);

	super_Start(self);	/* get colors set up */

	if (self->inputfile != NULL) {
		f = fopen(self->inputfile, "r");
		objectType = filetype_Lookup(f, self->inputfile, 
				&objectID, &attributes);
		c = EOF;
		if (feof(f) == 0) c = getc(f);
		if (c != EOF) ungetc(c, f);
		if (c != EOF && objectType != NULL 
				&&  ! class_IsTypeByName(objectType,
					 "null")) {
			fprintf(stderr, "File is not a saved null object, it's a %s\n",
					objectType);  
			return FALSE;
		}

		if (attributes != NULL)
			null_SetAttributes(self->dobj, attributes);

		if (c != EOF)
			val = null_Read(self->dobj, f, objectID);
		else val = dataobject_NOREADERROR;

		if (val != dataobject_NOREADERROR) {
			fprintf(stderr, "Input file is corrupted (%d): %s\n",
					 val, self->inputfile);
			return FALSE;
		}
	}

	buffer = buffer_Create("nulldata", NULL, NULL, self->dobj);
	buffer_SetFilename(buffer, self->inputfile ? self->inputfile : "/tmp/nulldata");


/*	if ( ! nullapp_GetFork(self))
 *		nullv_SetDebug(TRUE);
 */

	if((im = im_Create(NULL)) == NULL) {
		/* no window manager found.  
				Could uses curses version here */
		exit(0);
	}

	frame = frame_New();
	if(buffer == NULL || frame == NULL) {
		fprintf(stderr,"Could not allocate enough memory.\nexiting.\n");
		exit(8);
	}
	frame_SetCommandEnable(frame, TRUE);  /* allow cmds */

	/* connect frame to window */
	im_SetView(im, frame);	
	frame_PostDefaultHandler(frame, "message", 
			frame_WantHandler(frame, "message"));
	message_DisplayString(frame, 0, "");

	/* connect the frame (container for view) 
			to the buffer (container for data object)
			create the nullview as a side effect !  */
	frame_SetBuffer(frame, buffer, TRUE);

	/* give view the input focus */
	v = frame_GetView(frame);
	view_WantInputFocus(v, v);

	if ( ! nullapp_GetFork(self))
		printf("Focussed.  nullv @ 0x%lx  im @ 0x%lx   frame @ 0x%lx\n", 
				v, im, frame);

	return TRUE;
}

/* super_Run forks and then calls im_KeyboardProcessor.
	It can be overridden if other processing is required here.
	However, that other processing should be at the end of nullapp_Start 
*/


