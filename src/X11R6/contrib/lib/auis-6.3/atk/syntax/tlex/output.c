/* output.c - generate .tlc file for gentlex */
/*
	Copyright Carnegie Mellon University 1992 - All rights reserved
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
	char *tlex_output_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/syntax/tlex/RCS/output.c,v 1.6 1992/12/19 21:51:43 wjh R6tape $";
#endif


#include <stdio.h>
#include <ctype.h>

#include <global.h>
#include <gentlex.h>

static int defaultaction;  /* action for character \377 */
static int HiChar;  /* last character for which there is an action other
		than defaultaction */


/* !!! Order here is same as order of numbers in gentlex.h and global.h !!! */
static char *(RecName[]) = {
	"undefined recognizer",
	"tlex_ERROR",
	"tlex_WHITESPACE",
	"tlex_COMMENT",
	"tlex_TOKEN",
	"tlex_ID",
	"tlex_NUMBER",
	"tlex_STRING",
	"-reservedwords-",
	"-global-",
	"-errorhandler-"
};


/* write the struct declaration, its initialization, and the C proc, if any

	struct <structname>_type {
		<<one line for each ->Elt>>
		type var;
	};
	<<if there is C code include the following>>
		static int
	<procname>(tlex, parm)
		struct tlex *tlex;
		struct <structname>_type *parm;
	{
		<<C lines from decl>>
	}
	static struct <structname>_type <structname> = {
		<<another instance of one line per ->Elt>>
		val,
	};
*/
	void
WriteClass(fout, hdr)
	FILE *fout;
	struct line *hdr;
{
	struct line *elt;
	char *procname = (hdr->u.h.Ccode)  ? GenSym()  : "NULL";

	/* output identification */
	if (hdr->u.h.tokennumber >0 && 
			hdr->u.h.tokennumber < numtokens) 
		fprintf(fout, "\n/* tokenclass %s   action %d */\n", 
				Token[hdr->u.h.tokennumber],
				hdr->u.h.action);
	else 
		fprintf(fout, "\n/* recognizer %s  action %d */\n", 
				RecName[hdr->u.h.recognizer],
				hdr->u.h.action);

	/* output struct type definition */
	fprintf(fout, "struct %s_type {\n", hdr->u.h.structname);
	for (elt = hdr->u.h.decls; elt; elt = elt->next)
		fprintf(fout, "\t%s %s;\n", 
				elt->u.d.type, elt->u.d.var);
	fprintf(fout, "};\n");

	/* output function definition, if needed */
	if (hdr->u.h.Ccode) {
		fprintf(fout,"\tstatic int\n%s(tlex, parm)\n", procname);
		fprintf(fout,"\tstruct tlex *tlex;\n\tstruct %s_type *parm;\n",
				hdr->u.h.structname);
		if (linenos)
			fprintf(fout,"#line %d \"%s\"\n", 
				hdr->u.h.Ccode->lineno, tlxFile);
		for (elt = hdr->u.h.Ccode; elt; elt = elt->next)
			fprintf(fout, "%s", elt->u.c.text);
	}

	/* output struct variables and initial values */
	fprintf(fout, "static struct %s_type %s = {\n", 
			hdr->u.h.structname, hdr->u.h.structname);
	for (elt = hdr->u.h.decls; elt; elt = elt->next) {
		if (*elt->u.d.val == '$') {
			if (elt->u.d.val[1] == '1') 
				fprintf(fout, "\t%s", procname);
			else	/* $2: tokennumber */
				fprintf(fout, "\t%d", hdr->u.h.tokennumber);
		}
		else 
			fprintf(fout, "\t%s", elt->u.d.val);
		fprintf(fout, "%s  /* %s */\n", (elt->next) ? "," : "", 
					elt->u.d.var);
	}
	fprintf(fout, "};\n");
}

/* WriteRectbl(f)
	write tlex_recparmtbl, an array of pointers to the 
		structs generated for each Hdr
	
	static struct tlex_Recparm *(tlex_recparmtbl[]) = {
		<<one line for each elt of Classes>>
		(struct tlex_Recparm *)&<structname>,
	};

	compute action value for each Hdr in Classes
		action is already set if u.h.recogniser == tlex_RESWD
		for others:  ACTSCAN | u.h.recognizer
		(at this time thongs and simple tokens are not in Classes list)

	Warning: The traversal order of Classes must be the same as in 
	assigning u.h.action values in defaults.c:ComputeDefaults.
*/
	static void
WriteRectbl(f)
	FILE *f;
{
	struct line *lx;
	int i;
	char *comma;

	fprintf(f, "\nstatic struct tlex_Recparm *(%s_recparmtbl[]) = {", 
			Prefix);

	comma = "";	/* no comma before the first entry */
	for (lx = Classes, i = 0; lx; lx = lx->next, i++) {
		fprintf(f, "%s\n\t(struct tlex_Recparm *)&%s",
			comma, lx->u.h.structname);
		comma = ",";
	}
	fprintf(f, "\n};\n");
}

/* WriteActions(f)
	write the Actions array as an array of shorts
	set HiChar and defaultaction

	static short tlex_actiontbl[] = {
		<<one integer for each element of Actions:  
			Actions[i]->u.h.action>>
		3, 1, ... 0
	};

*/
	static void
WriteActions(f)
	FILE *f;
{
	int i, action;
	struct line *act;

	act = Actions[255];
	for (i = 254; i >= 0 && Actions[i] == act; i--) {}
	HiChar = i;
	defaultaction = (act->type == Thong)
				? ThongAction(act)
				: act->u.h.action;
	if (defaultaction == 0) {
		char buf[2];
		buf[0] = HiChar;  buf[1] = '\0';
		LineNo = 999;
		ErrorA(WARNING, 
			"Parser will halt for input characters above", 
			Escapify(buf, NULL));
	}

	fprintf(f, "\nstatic short %s_actiontbl[] = {", Prefix);
	for (i = 0; i <= HiChar; i++) {
		if (i % 8 == 0) {
			char b[2];
			b[0] = i;  b[1] = '\0';
			fprintf(f, "\n  /* '%s' */\t", Escapify(b, NULL));
		}
		act = Actions[i];
		action = (act->type == Thong)
				? ThongAction(act) 
				: act->u.h.action;
		fprintf(f, "%d%s", action, (i == HiChar) ? "\n" : ", ");
		if (action == 0) {
			char buf[2];
			buf[0] = i;  buf[1] = '\0';
			LineNo = 999;
			ErrorA(WARNING,
				"Parser will halt for input character",
				Escapify(buf, NULL));
		}
	}
	fprintf(f, "};\n");
}


	void
WriteTlc(fout, fname)
	FILE *fout;
	char *fname;
{
	struct line *hdr;
	char **linep;
	int i;
	char buf[2];

	fprintf(fout, 
		"/* %s - Generated by `gentlex`.  Edit at your peril */\n",
		fname);
	fprintf(fout, "#include <gentlex.h>\n\n");

	if (ResWords) {
		fprintf(fout, "/* Actions for reserved words */\n");
		for (hdr = ResWords; hdr; hdr = hdr->next) 
			fprintf(fout, "\t/* %d  %s */\n", 
				hdr->u.h.action, (char *)hdr->u.h.decls);
		fprintf(fout, "\n");
	}
	CharsetOutputArrays(fout);
	if (GlobalHandler)
		WriteClass(fout, GlobalHandler);
	for (hdr = Classes;  hdr;  hdr = hdr->next) 
		if (hdr != GlobalHandler)
			WriteClass(fout, hdr);
	WriteRectbl(fout);
	fprintf(fout, "\n");
	ThongOut(fout);
	WriteActions(fout);
	buf[0] = HiChar;  buf[1] = '\0';

fprintf(fout, "\nstatic struct tlex_tables %s_tlex_tables = {\n", Prefix);
fprintf(fout, "\t/* rectbl; */		&%s_recparmtbl[0],\n", Prefix);
fprintf(fout, "\t/* short *action; */	%s_actiontbl,\n", Prefix);
fprintf(fout, "\t/* int hichar; */	UNSIGN('%s'),\n", Escapify(buf, NULL));
fprintf(fout, "\t/* int defaultaction;*/	%d,\n", defaultaction);
fprintf(fout, "\t/* reservedwordparm;*/	");
if (ResWordHandler)
	fprintf(fout, "(struct tlex_Recparm *)&%s,\n",
			ResWordHandler->u.h.structname);
else 
	fprintf(fout, "(struct tlex_Recparm *)NULL,\n");
fprintf(fout, "\t/* char **thongtbl; */	(char **)%s_thongtbl,\n", Prefix);
fprintf(fout, "\t/* char *thongsame; */	%s_thongsame,\n", Prefix);
fprintf(fout, "\t/* short *thongact; */	%s_thongact,\n", Prefix);
if (GlobalHandler)
	fprintf(fout, "\t/* global; */		(struct tlex_Recparm *)&%s,\n",
			GlobalHandler->u.h.structname);
else
	fprintf(fout, "\t/* global; */		(struct tlex_Recparm *)NULL,\n");
fprintf(fout, "\t/* ErrorHandler; */	(struct tlex_ErrorRecparm *)&%s\n",
			ErrorHandler->u.h.structname);
fprintf(fout, "};\n");

}
