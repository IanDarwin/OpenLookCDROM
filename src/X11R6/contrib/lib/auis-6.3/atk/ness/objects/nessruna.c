/* ********************************************************************** *\
 *         Copyright IBM Corporation 1991 - All Rights Reserved      *
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/ness/objects/RCS/nessruna.c,v 1.13 1993/05/04 01:23:55 susan Exp $";
#endif
 

/*
 * app for ness
 *
 *	Program to execute a Ness file.  
 *
 *	nessrun [-d]  programfilename args
 *
 *	programfilename is the name of a file containing the ness program
 *		it must have a function main(), which will be called
 *		to initiate execution
 *	remaining args are concatenated and passed as a single marker parameter to main()
 *
 *	The -d switch causes the compiler to dump the generated code.
 */

/*
 * $Log: nessruna.c,v $
 * Revision 1.13  1993/05/04  01:23:55  susan
 * RCS Tree Split
 *
 * Revision 1.11.1.1  1993/02/02  03:05:59  rr2b
 * new R6tape branch
 *
 * Revision 1.11  1992/12/17  19:39:37  rr2b
 * added #include of sys/param.h for MAXPATHLEN
 * .
 *
 * Revision 1.10  1992/12/16  03:57:28  wjh
 * the ness file argument name is canonicalized
 * .
 *
 * Revision 1.9  1992/12/15  21:38:20  rr2b
 * more disclaimerization fixing
 *
 * Revision 1.8  1992/12/14  20:50:00  rr2b
 * disclaimerization
 *
 * Revision 1.7  1992/11/26  02:42:25  wjh
 * converted CorrectGetChar to GetUnsignedChar
 * moved ExtendShortSign to interp.h
 * remove xgetchar.h; use simpletext_GetUnsignedChar
 * nessrun timing messages go to stderr
 * replaced curNess with curComp
 * replaced genPush/genPop with struct compilation
 * created compile.c to handle compilation
 * moved scope routines to compile.c
 * converted from lex to tlex
 * convert to use lexan_ParseNumber
 * truncated logs to 1992 only
 * use bison and gentlex instead of yacc and lexdef/lex
 *
 * .
 *
 * Revision 1.6  92/11/14  15:44:15  wjh
 * fix illegal pointer messages for stupid Sun char * conventions
 * 
 * Revision 1.5  91/12/18  22:26:29  wjh
 * fixed bug with empty argument list
 * simplified the timing output
 * .
 * 
 * Revision 1.4  1991/09/12  16:26:33  bobg
 * Update copyright notice and rcsid
 *
 * Revision 1.3  1990/08/01  16:27:53  wjh
 * fixed a bug introduced last week where it mistakenly included the file name in the args as given to main()
 *
 * Revision 1.2  90/07/25  16:35:22  wjh
 * fix core dump on Sun3
 * 
 * Revision 1.1  90/07/15  15:24:35  wjh
 * Initial revision
 * 
* 11 May 1990 WJH converted from old standalone nessrun program.
 */
#include <stdio.h>
#include <andrewos.h>
#include <sys/param.h>
#include <nessruna.eh>

#include <event.ih>
#include <mark.ih>
#include <nessmark.ih>
#include <ness.ih>
#include <filetype.ih>
#include <path.ih>
#include <text.ih>
#include <smpltext.ih>
#include <textv.ih>
#include <error.h>

struct ness *program;


	boolean
nessrunapp__InitializeObject(classID,self)
	struct classheader *classID;
	struct nessrunapp *self;
{
	self->inputfile = NULL;
	self->dump = FALSE;
	self->theNess = ness_New();
	nessrunapp_SetMajorVersion(self, CURRENTSYNTAXLEVEL);
	nessrunapp_SetMinorVersion(self, CURRENTMODIFICATIONLEVEL);
	nessrunapp_SetFork(self, FALSE);
	return  TRUE;
}

	void
nessrunapp__FinalizeObject(classID,self)
	struct classheader *classID;
	struct nessrunapp *self;
{
	ness_Destroy(self->theNess);
	/* do not free self->inputfile because it is in argv */
}


/*
 * usage statement
 */
	static void
show_usage(self)
	struct nessrunapp *self;
{
	fprintf(stderr,
		"Usage: %s  [-d]  programfilename  arguments\n",
		nessrunapp_GetName(self));
	fprintf(stderr,
"\
	-d: display generated code\n\
	programfilename: execute program in this files\n\
	args: all further text is passed as the arg to main() in the program\n\
");
}

	void
dumpall()
{
	struct ness *n;
	unsigned char *name;
	for (n = ness_GetList(); n != NULL; n = ness_GetNext(n)) {
		name = ness_GetName(n);
		printf("\nObject code for %s\n", 
			(name != NULL) ? name : (unsigned char *)"unknown");
		ness_dumpattrs(n, stdout);
	}
}


	boolean 
nessrunapp__ParseArgs(self, argc, argv)
	struct nessrunapp *self;
	int argc;
	char **argv;
{
	struct nessmark *arg, *args, *blank;

	if(!super_ParseArgs(self, argc, argv))
		return FALSE;

	/* super_ParseArgs() passes across the "runapp" and its switches,
		leaving "nessrun" as the first arg.   */

	while(*++argv != NULL && **argv == '-') {
		switch((*argv)[1]){
			case 'd':		
				self->dump = TRUE;
				break;
			case 'f':		
				nessrunapp_SetFork(self, TRUE);
				break;
			default:
				fprintf(stderr,"%s: unrecognized switch: %s\n",
					nessrunapp_GetName(self), *argv);
				show_usage(self);
				return FALSE;
		}
	}

	if (*argv == NULL) {
		fprintf(stderr,"%s: no programfilename specified\n",
				nessrunapp_GetName(self));
		show_usage(self);
		return FALSE;
	}

	/* get the name of the ness program file */
	self->inputfile = *argv++;

	/* concatenate args to pass to theNess */
	args = nessmark_New();
	nessmark_SetText(args, simpletext_New());
	blank = nessmark_New();
	nessmark_MakeConst(blank, " ");
	arg = nessmark_New();
	while (*argv != NULL) {
		nessmark_MakeConst(arg, *argv);
		nessmark_Next(args);
		nessmark_Replace(args, arg);
		nessmark_Next(args);
		nessmark_Replace(args, blank);
		argv++;
	}
	nessmark_Base(args);
	ness_SupplyMarkerArg(self->theNess, args);
	return TRUE;
}

	boolean
nessrunapp__Start(self)
	struct nessrunapp *self;
{
	char fullName[MAXPATHLEN+1];
	char *fname;

	fname = path_UnfoldFileName(self->inputfile, fullName, 0);
	if (ness_ReadNamedFile(self->theNess, fname) 
				!= dataobject_NOREADERROR) {
		fprintf(stderr, "Input file is neither plain text not ATK format: %s\n",
				fname);
		return FALSE;
	}
	return TRUE;
}


	int 
nessrunapp__Run(self)
	struct nessrunapp *self;
{
	struct errornode *result;
	long t0, t1;
	struct text *text;
	struct textview *textview;
	boolean forkit;

	text = text_New();
	textview = textview_New();
	textview_SetDataObject(textview, text);
	ness_SetDefaultText(self->theNess, textview);

	ness_SetName(self->theNess, (unsigned char *)self->inputfile);
	ness_SetAccessLevel(self->theNess, ness_codeUV);

			t0 = event_TUtoMSEC(event_Now());
	result = ness_Compile(self->theNess);
			t1 = event_TUtoMSEC(event_Now());

	if (self->dump)
		dumpall();

	if (ness_PrintAllErrors("Compile") != 0)
		return(1);
	else fprintf (stderr, "Compiled in %d.%02d sec.\n",
			(t1-t0)/1000, (t1-t0)%1000/10);

	forkit = nessrunapp_GetFork(self);
	if(!nessrunapp_Fork(self))
		return -1;

			t0 = event_TUtoMSEC(event_Now());
	result = ness_Execute(self->theNess, "main");
			t1 = event_TUtoMSEC(event_Now());

	if (result != NULL) 
		ness_PrintAllErrors("Execution");
	else if ( ! forkit)
		fprintf (stderr, "Executed in %d.%02d sec.\n",
			(t1-t0)/1000, (t1-t0)%1000/10);

	return ((result == NULL) ? 0 : 1);
}
