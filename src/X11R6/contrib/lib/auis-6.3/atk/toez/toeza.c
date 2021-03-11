/* ********************************************************************** *\
 *         Copyright IBM Corporation 1991 - All Rights Reserved      *
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/toez/RCS/toeza.c,v 1.10 1993/12/07 21:08:51 Zarf Exp $";
#endif


 

/*
 * app for toez
 *
 *	Program to convert from various file types to andrew object type.
 *
 *	The program attempts to analyze the input to determine what sort of
 *	data it might contain and to convert it to the appropriate ATK object file.
 *
 *	Main switches:
 *		-h, -help	print explanation
 *		-scribe	input IS Scribe
 *		-nroff	input IS nroff
 *		-troff	input IS troff
 *		-	input from stdin	(analysis is limited)
 *		-o FILE	output to file named FILE
 *
 *	Switches for nroff/troff
 *		-H	special mode for help system
 *		-b	completely bogus (?)
 *		-m MAC	{TMACPREFIX}{MAC}
 * 
 *	If no input file is given, input is from stdin.
 *	If no output file is given, stdout is assumed.
 */

#include <stdio.h>
#include <andrewos.h>
#include <toeza.eh>
#include <text.ih>
#include <rofftext.ih>

	boolean
toezapp__InitializeObject(classID,self)
	struct classheader *classID;
	struct toezapp *self;
{
	self->macrofile = NULL;
	self->inputtype = NULL;
	self->outputfile = NULL;
	self->inputfile = NULL;
	self->inf = NULL;
	self->outf = NULL;
	self->HelpMode = FALSE;
	self->BeCompletelyBogus = FALSE;
	toezapp_SetMajorVersion(self, 7);
	toezapp_SetMinorVersion(self, 0);
	toezapp_SetFork(self,FALSE);
	return  TRUE;
}

	void
toezapp__FinalizeObject(classID,self)
	struct classheader *classID;
	struct toezapp *self;
{
}


/*
 * usage statement
 */
	static void
show_usage(self)
	struct toezapp *self;
{
	fprintf(stderr,
		"Usage: toez [-help] [-scribe | [[-nroff | -troff][-m macro file][-H]]] [-o outputfile] [-] [file]\n");
	fprintf(stderr,
"	-help: show this usage statement\n\
	-scribe: assume Scribe input (default for .mss)\n\
	-nroff: pretend to be nroff (default for .n)\n\
	-troff: pretend to be troff (default for .t)\n\
	-m file: read in %sfile as a macro file\n\
	-H: format for use with the ATK Help system.  Crush initial blank space\n\
	-o file: send output file to 'file'.  (default is .d)\n\
	-: use standard input as the file input\n\
	file: read in this files\n\
", TMACPREFIX);
}

	static void
InsistOn(arg, assumed)
	char *arg, *assumed;
{
	if (strcmp(arg+1, assumed) != 0)
		fprintf(stderr, "Assuming -%s instead of \"%s\"\n", assumed, arg);
}


	boolean 
toezapp__ParseArgs(self,argc,argv)
	struct toezapp *self;
	int argc;
	char **argv;
{
	char temp2[128];

	if(!super_ParseArgs(self, argc, argv))
		return FALSE;

#define GETARGSTR(var)\
{\
	if((*argv)[2] != '\0')\
		var= ((*argv)[2] == '=' ? &(*argv)[3] : &(*argv)[2]);\
	else if(argv[1] == NULL){\
		fprintf(stderr,"%s: %s switch requires an argument.\n",\
				toezapp_GetName(self), *argv);\
		return FALSE;\
	}else {\
			var = *++argv;\
		argc--;\
	}\
}

	while(*++argv != NULL && **argv == '-') {
		boolean stop = FALSE;
		switch((*argv)[1]){
				char *temp;
			case 's':
				InsistOn(*argv, "scribe");			
				self->inputtype = "scribe";
				break;
			case 'n':
				InsistOn(*argv, "nroff");
				self->inputtype = "nroff";
				break;
			case 't':
				InsistOn(*argv, "troff");
				self->inputtype = "troff";
				break;
			case 'm':
				GETARGSTR(temp);
				sprintf(temp2,"%s%s", TMACPREFIX, temp);
				self->macrofile = StrDup(temp2);
				break;
			case 'o':
				GETARGSTR(self->outputfile);
				break;
			case 'h':
				show_usage(self);
				exit(0);
			case 'H':					
				self->HelpMode = TRUE;
				break;
			case 'b':
				self->BeCompletelyBogus = TRUE;
				break;
			case '\0':
				stop = TRUE;
				break;	/* for stdin, use '-' */
			default:
				fprintf(stderr,"%s: unrecognized switch: %s\n",
					toezapp_GetName(self), *argv);
				show_usage(self);
				return FALSE;
		}
		if (stop)
			break;
		argc--;
	}

	/* are there input filenames? */

	if (*argv != NULL)
		self->inputfile = *argv;

	return TRUE;
}

	boolean
toezapp__Start(self)
	struct toezapp *self;
{
	return TRUE;
}


	static void
AnalyzeInput(self)
	struct toezapp *self;
{
	long c;
	char *extension;

	if (self->inputfile == NULL)
		self->inf = stdin;
	else 
		self->inf = fopen(self->inputfile,"r");
	if (self->inf == NULL) {
		perror(self->inputfile);
		exit (1);
	}

	if (self->inputtype != NULL) 
		return;

	/* no input type given.  Try to get one from extension. */
	/* XXX we should call a test routine from each file type */

	if (self->inputfile != NULL) {
		extension = (char *)rindex(self->inputfile, '.');
		if (extension == NULL) {}
		else if (strcmp(extension, ".mss") == 0) 
			self->inputtype = "scribe";
		else if (strcmp(extension, ".n") == 0) 
			self->inputtype = "nroff";
		else if (strcmp(extension, ".t") == 0) 
			self->inputtype = "troff";
		if (self->inputtype != NULL) {
			fprintf(stderr, "Assuming -%s because of extension %s\n",
				self->inputtype, extension);
			return;
		}
	}

	/* extension didn't help.  Examine start of file. */

	c = getc(self->inf);
	ungetc(c, self->inf);
	
	switch (c) {
	case '@':   self->inputtype = "scribe";  return;
	case '#':    self->inputtype = "nroff";  return;
	case '.':    self->inputtype = "nroff";  return;
	}
	if (self->inputtype != NULL) {
		fprintf(stderr, "Assuming -%s because of first character %c\n",
			self->inputtype, c);
		return;
	}

	/* give up.  Assume ASCII or scribe */
	self->inputtype = "scribe";	
	fprintf(stderr, "Assuming -%s by default\n", self->inputtype);
}

	static void
OpenOutputFile(self)
	struct toezapp *self;
{
	/* XXX should check the extension on the file name versus
		the type deduced */
	if (self->outputfile)
		self->outf = fopen(self->outputfile, "w");
	else
		self->outf = stdout;
	if (self->outf == NULL) {
		perror(self->outputfile);
		exit(1);
	}
}


	static void
rofftext(self)
	struct toezapp *self;
{
	struct rofftext *r;
	struct text *t;

	if(!super_Start(self)) return;

	r = rofftext_New();
	if (r == NULL) return;

	r->inputfiles = (char **)malloc(2 * sizeof(char *));
	r->inputfiles[0] = NULL;
	r->inputfiles[1] = NULL;
	r->filename = self->inputfile;

	if (self->BeCompletelyBogus) {
		t = text_New();
		fprintf(stderr,"Reading roff into text...");
		fflush(stderr);
		rofftext_ReadRoffIntoText(t, self->inf, 0, r->inputfiles);
		fprintf(stderr, "done.\n");
		fflush(stderr);
		text_Write(t, self->outf, (long)t, 0);
	}
	else {
		if (self->macrofile == NULL) 
			fprintf(stderr, 
				"Warning: no macro file specified.  %s\n",
				"Valid options include -ms, -me, and -man.");
		r->macrofile = self->macrofile;
		r->RoffType = (*self->inputtype == 't');
		r->HelpMode = self->HelpMode;

		rofftext_Read(r, self->inf, (long)r);
		rofftext_Write(r, self->outf, (long)r, 0);
	}
}


/* = = = = = = = = = = = = = = = = = = = =
	ReadScribe
= = = = = = = = = = = = = = = = = = = = =*/


	static void
errhandler(lineno, msg)
	long lineno;
	char *msg;
{
	fprintf(stderr, "line %d: %s\n", lineno, msg);
}


	static void
describe(self)
	struct toezapp *self;
{
	struct text *txt = text_New();
	text_ReadTemplate(txt, "default", TRUE);

	ReadScribeFromFileToDoc(self->inf, txt, 0, errhandler);
	text_Write(txt, self->outf, 91, 0);
}


	int 
toezapp__Run(self)
	struct toezapp *self;
{
	/* if no type given, analyze the input file to determine type */
	AnalyzeInput(self);

	/* open the output file */
	OpenOutputFile(self);

	/* convert input to output */
	if (strcmp(self->inputtype, "nroff") == 0
			|| strcmp(self->inputtype, "troff") == 0) 
		rofftext(self);
	else
		describe(self);
	fclose(self->outf);
	fclose(self->inf);
	return (0);
}
