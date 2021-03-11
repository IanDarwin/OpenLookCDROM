/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/ness/objects/RCS/nesst.c,v 1.4 1992/12/15 21:38:20 rr2b R6tape $";
#endif


 


/* nesst.c

	test the ness object
*/


#include <stdio.h>

#include <ness.ih>


/* include .ih files to be loaded with the binary of the file but none of whose
		methods are called in this file itself */
#define class_StaticEntriesOnly

/* required classes */
#include <environ.ih>

/* classes that are anyway dynamically loaded by the
	required classes */
#include <dataobj.ih>
#include <event.ih>
#include <filetype.ih>
#include <proctbl.ih>
#include <observe.ih>

#undef class_StaticEntriesOnly



main(argc, argv)
	register int argc;
	register char **argv;
{
	register struct ness *dobj;
	boolean debug = TRUE;

	FILE *f;
	char *fnm;
	long i, id;
    
	printf("Start\n"); fflush(stdout);
  	class_Init(AndrewDir("/dlib/atk"));

	printf("Init done\n"); fflush(stdout);

/*	Cause static linking and loading of most ATK routines:
 */
	environ_StaticEntry;
	dataobject_StaticEntry;
	event_StaticEntry;
	filetype_StaticEntry;
	proctable_StaticEntry;
	observable_StaticEntry;

/* dynamically load ness so nesst need not be relinked when ness is recompiled */

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

	printf("About to New\n"); fflush(stdout);

	dobj = ness_New();
	printdata(dobj);

	/* $$$ establish some initial contents for the data object */
	if (fnm && (f=fopen(fnm, "r")))
		ness_Read(dobj, f, 0);
	else {
		static unsigned char *small = "function main(arg) printline(arg); end function";
		ness_InsertCharacters(dobj, 0, small, strlen(small));
	}

	printdata(dobj);
	printf("writing t1\n");  fflush(stdout);
	ness_Write(dobj, f=fopen("/tmp/t1", "w"), 12, 0);
	fclose(f);
	ness_Destroy(dobj);
	dobj = ness_New();
	printf("reading t1\n");  fflush(stdout);
	f = fopen("/tmp/t1", "r");
	fscanf(f, "\\begindata{ness,%d} ", &id);
	printf("read: %d\n",ness_Read(dobj, f, id));
	fclose(f);
	printdata(dobj);
	printf("writing t2\n");  fflush(stdout);
	ness_Write(dobj, f=fopen("/tmp/t2", "w"), 12, 0);
}

printdata(dobj)
	register struct ness *dobj;
{
	printf("Origin: %s\n", dobj->Origin);
	printf("OriginalModValue: %ld\n", dobj->OriginalModValue);
	printf("ScriptLoc: %ld\n", dobj->ScriptLoc);
	printf("AfterScriptLoc: %ld\n", dobj->AfterScriptLoc);
	printf("DeauthButtonLoc: %ld\n", dobj->DeauthButtonLoc);
	printf("ScanButtonLoc: %ld\n", dobj->ScanButtonLoc);
	printf("CompileButtonLoc: %ld\n", dobj->CompileButtonLoc);
	printf("accesslevel: %ld\n", dobj->accesslevel);
	printf("DisplayDialogBox: %ld\n", dobj->DisplayDialogBox);
	printf("hasWarningText: %ld\n", dobj->hasWarningText);
	fflush(stdout);
}

