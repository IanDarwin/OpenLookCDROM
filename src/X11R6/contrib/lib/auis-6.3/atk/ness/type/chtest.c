/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
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
\* ********************************************************************** */
#if !defined(lint) && !defined(LOCORE) && defined(RCS_HDRS)
char *chtest_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/ness/type/RCS/chtest.c,v 1.12 1994/01/05 02:51:11 wjh Exp $";
#endif

/*
 *	TEST PROGRAM FOR .CH FILE SYNTAX

	usage:  chtest  [-r] [-t] [-d classname [-p itemname] ]  filename...

	Reads the named files and parses them as .ch files, checking for errors.

		(for both -d and -p there is a space between switch and name)
	-d classname  prints the declarations for the named class
	-p itemname  prints the type structure contents 
			for the named method, classproc, or variable
		(Note that -p can only be given if -d is also.)
	-r causes a reset after each file.  This is more accurate, but slower.
	-t turns on parse debugging
 */

/*
 *   $Log: chtest.c,v $
 * Revision 1.12  1994/01/05  02:51:11  wjh
 * statically load parse and sym in chtest
 *
 * Revision 1.11  1993/12/15  19:19:52  wjh
 * extend -p to be able to give info on fields as well as methods
 *
 * Revision 1.10  1993/05/04  01:23:33  susan
 * RCS Tree Split
 *
 * Revision 1.9.1.1  1993/02/02  02:56:46  rr2b
 * new R6tape branch
 *
 * Revision 1.9  1992/12/14  20:48:33  rr2b
 * disclaimerization
 *
 * Revision 1.8  1992/12/02  21:20:28  wjh
 * remove static loading of parse, sym, and type from chtest
 * improved messages from chtest
 * cause type_Declare... classprocedures to properly return error message
 * fix bogus declaration of grammarScope and garbageScope (was erroneously 'static')
 * .
 *
 * Revision 1.7  1992/11/26  02:33:17  wjh
 * converted to bison/parse and tlex
 * chtest: added -t switch to dump parse transitions
 *
 * .
 *
. . .
 */

#include <type.ih>
#include <parse.ih>
#include <sym.ih>

struct type_ctypes *Ctypes;

	void
DoDumps(dumpclass, dumpitem)
	char *dumpclass, *dumpitem;
{
	struct type *classtype, *ttt, *elt;
	classtype = type_Lookup(Ctypes->basicobject, dumpclass);
	if (classtype != NULL) {
		type_Print(classtype);
		dumpclass = NULL;  /* do it only once */
		if (dumpitem != NULL) {
			ttt = type_Lookup(classtype, dumpitem);
			if (ttt == NULL)    /* try data fields */
				ttt = type_Lookup(
					type_GetClassInfo(classtype, data),
					dumpitem);
			if (ttt != NULL) {
				type_Print(ttt);
				printf("\nargs in reverse order:\n\n");
				for (elt = type_GetPrev(ttt);
						elt != NULL;
						elt = type_GetPrev(elt))
					type_Print(elt);
			}
		}
	}
}

main(argc, argv)
	register int argc;
	register char **argv;
{
	char *result;

	boolean rswitch = FALSE;
	char *dumpitem = NULL;
	char *dumpclass = NULL;

	initClass();

	argv++; argc--;	/* skip argv[0] */

	/* process switches */
	while (argc > 0 && **argv == '-') switch ((*argv)[1]) {
		case 'r':	rswitch = TRUE;  argv++; argc--;  break;
		case 'd':  argv++; argc--;  dumpclass = *argv; 
			argv++; argc--;  break;
		case 't': parse_SetDebug(TRUE);  argv++; argc--;  break;
		case 'p':  argv++; argc--;  dumpitem = *argv; 
			argv++; argc--;  
			if (dumpclass != NULL) 
				break;
			else {/* DROPTHRU for error */}
		default:	printf("usage: chtest  [-r]  [-d classname [-p itemname] ]  filename...\n");
			exit(1);
	}

	printf("testing %d .ch file%s\n\n", argc, (argc==1) ? "" : "s");

	Ctypes = type_GetCtypes();

	while (argc-- > 0) {
		printf("parsing %s", *argv);
		fflush (stdout);
		result = type_DeclareFromFile(*argv++);
		if (*result == '\0') {
			printf("    -> succeeded\n\n");
			if (dumpclass != NULL) 
				DoDumps(dumpclass, dumpitem);
		}
		else 
			printf("   -> failed:\n\t%s\n\n", result);
		if (rswitch)
			type_Reset();
	}
}

initClass()
{
	char *classPath;
    
	classPath = (char *) AndrewDir("/dlib/atk");
	class_Init(classPath);

	type_StaticEntry;
	parse_StaticEntry;  
	sym_StaticEntry; 
}
