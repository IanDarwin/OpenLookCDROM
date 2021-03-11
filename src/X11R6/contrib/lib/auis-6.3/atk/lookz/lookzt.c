/*LIBS: -lbasics -lclass -lerrors -lutil
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/lookz/RCS/lookzt.c,v 1.4 1992/12/15 21:37:21 rr2b R6tape $";
#endif

/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

 


/* testdo.c
	test the lookz data object
*/


#include <stdio.h>
#include <class.h>
#include <observe.ih>
#include <proctbl.ih>
#include <dataobj.ih>

#include <lookz.ih>

main( argc, argv )
	register int	  argc;
	register char  **argv;
{
	register struct lookz *st, *st2;
	FILE *f;

	printf("Start\n"); fflush(stdout);
	class_Init(".");		/* use current directory for dataobject path (???) */
	printf("Init done\n"); fflush(stdout);

	observable_StaticEntry;
	proctable_StaticEntry;
	dataobject_StaticEntry;
/*
	lookz_StaticEntry;
*/
	printf("About to New\n"); fflush(stdout);
	st = lookz_New();

	printdata(st);
	lookz_SetVisibility(st, ! lookz_GetVisibility(st));
	printdata(st);

	lookz_Destroy(st);

	printf("\n Phase II\n");  fflush(stdout);
	st = lookz_New();

	printdata(st);

	printf("\nWriting plain data stream\n");  fflush(stdout);
	f = fopen("/tmp/lookzfoo", "w");
	lookz_Write(st, f, 0, 0);
	fclose(f);

	printf("Reading\n");  fflush(stdout);
	f = fopen("/tmp/lookzfoo", "r");
	st2 = lookz_New();
	lookz_Read(st2, f, 0);
	fclose(f);

	printdata(st2);
	lookz_Destroy(st2);

	/* the next file should be empty because the
		WriteID is the same as above */
	printf("\nReWriting with same WriteID\n");  fflush(stdout);
	f = fopen("/tmp/lookzfoo2", "w");
	lookz_Write(st, f, 0, 2);
	fclose(f);

	lookz_SetVisibility(st, FALSE);
	printf("\nWriting with headers\n");  fflush(stdout);
	f = fopen("/tmp/lookzfoo3", "w");
	lookz_Write(st, f, 1, 2);
	fclose(f);


	printf("Reading\n");  fflush(stdout);
	f = fopen("/tmp/lookzfoo3", "r");
	while (TRUE) 
		if((fgetc(f)) == '}') break;
	st2 = lookz_New();
	lookz_Read(st2, f, 3);
	fclose(f);

	printdata(st2);
}

printdata(st)
	register struct lookz *st;
{
	printf("Image is %s\n", (lookz_GetVisibility(st) ? "visible" : "hidden"));
	fflush(stdout);
}
