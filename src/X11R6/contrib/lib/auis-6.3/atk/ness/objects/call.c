/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
	Copyright Carnegie Mellon University 1992 - All Rights Reserved 
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/ness/objects/RCS/call.c,v 1.64 1994/02/01 22:08:25 wjh Exp $";
#endif


#include <andrewos.h>
#include <stdio.h>
#include <ctype.h>
#include <sys/param.h>
#include <sys/types.h>
#include <filetype.ih>
#include <environ.ih>
#include <proctbl.ih>
#include <dataobj.ih>
#include <tlex.ih>
#include <im.ih>
#include <view.ih>
#include <text.ih>
#include <viewref.ih>
#include <arbiterv.ih>
#include <arbiter.ih>
#include <celv.ih>

#include <nesssym.ih>
#include <nessmark.ih>
#include <ness.ih>

#include <nodeclss.h>
#include <call.h>
#include <error.h>
#include <compdefs.h>	/* for flag_xxx */
#include <envt.h>
#include <nevent.h>	/* for ProperPtr */

#include <type.ih>

#define BUFLEN 300

/* defining occurrence for declaration in call.hn */
char *callvarietyname[] = {
	"callC",
	"callPE",
	"callSym",
	"callNess",
	"callMeth",
	"callClPr",
	"callGet",
	"callSet",
	NULL
};

static struct libnode *LibList = NULL;
nesssym_scopeType LibScope = nesssym_GLOBAL;

extern struct nessmark *getcurfuncmark();	/* in gen.c */
extern ness_access MapAccess();		/* in ness.c */
extern ness_access DefaultLibraryAccess;	/* in ness.c */

static struct type_ctypes *Ctypes = NULL;
static struct type *booleanType = NULL;


/* {{fields name and defn should be unsigned char, but the Sun considers
	character constants to be *signed* (bletch) }} */
static struct builtindef {
	char *name, *defn;
	Texpr types[8];	/* the first in this list is the return type,
				the rest is args in REVERSE ORDER
				the end of list indicator is Tend
				Tunk is special for an optional textview arg 
			   if the list has only one type, the name is a predefined
				identifier */
	ness_access minok;	/* will be allowed when accesslevel 
				is this level or higher */
} builtintable[] = {
	{"next", "n", {Tstr, Tstr, Tend}, ness_codeOrange},
	{"start", "o", {Tstr, Tstr, Tend}, ness_codeOrange},
	{"base", "p", {Tstr, Tstr, Tend}, ness_codeOrange},
	{"newbase", "q", {Tstr, Tend}, ness_codeOrange},
	{"replace", "r", {Tstr, Tstr, Tstr, Tend}, ness_codeYellow},
	{"extent", "x", {Tstr, Tstr, Tstr, Tend}, ness_codeOrange},

	{"previous", "wp", {Tstr, Tstr, Tend}, ness_codeOrange},
	{"finish", "no", {Tstr, Tstr, Tend}, ness_codeOrange},
	{"front", "on", {Tstr, Tstr, Tend}, ness_codeOrange},
	{"rest", "monnzx", {Tstr, Tstr, Tend}, ness_codeOrange},
	{"first", "mmonnzxox", {Tstr, Tstr, Tend}, ness_codeOrange},
	{"second", "monnzxmmonnzxox", {Tstr, Tstr, Tend}, ness_codeOrange},
	{"last", "Z", {Tstr, Tstr, Tend}, ness_codeOrange},
	{"allprevious", "ompzx", {Tstr, Tstr, Tend}, ness_codeOrange},
	{"allnext", "nmpx", {Tstr, Tstr, Tend}, ness_codeOrange},
	{"advance", "monnoznnox", {Tstr, Tstr, Tend}, ness_codeOrange},
	{"length", "wl", {Tlong, Tstr, Tend}, ness_codeOrange},
	{"nextn", "wn", {Tstr, Tlong, Tstr, Tend}, ness_codeOrange},
	{"toend", "mpx", {Tstr, Tstr, Tend}, ness_codeOrange},
	{"copy", "qzr", {Tstr, Tstr, Tend}, ness_codeOrange},

	/* F* search.c */
	/* SearchOp's with two marker args */
	{"search", "Fa", {Tstr, Tstr, Tstr, Tend}, ness_codeOrange},
	{"match", "Fb", {Tstr, Tstr, Tstr, Tend}, ness_codeOrange},
	{"anyof", "Fc", {Tstr, Tstr, Tstr, Tend}, ness_codeOrange},
	{"span", "Fd", {Tstr, Tstr, Tstr, Tend}, ness_codeOrange},
	{"token", "Fe", {Tstr, Tstr, Tstr, Tend}, ness_codeOrange},
	{"regsearch", "Ff", {Tstr, Tstr, Tstr, Tend}, ness_codeOrange},
	{"regsearchreverse", "Fg", {Tstr, Tstr, Tstr, Tend}, ness_codeOrange},
	{"searchforstyle", "Fh", {Tstr, Tstr, Tstr, Tend}, ness_codeOrange},
	{"definestyle", "zFi", {Tstr, Tstr, Tstr, Tend}, ness_codeOrange},
	{"addstyles", "Fj", {Tstr, Tstr, Tstr, Tend}, ness_codeYellow},
	{"hasstyles", "Fk", {Tbool, Tstr, Tstr, Tend}, ness_codeOrange},
	{"addstylebyname", "Fl", {Tstr, Tstr, Tstr, Tend}, ness_codeYellow},
	/* SearchOp's with one marker arg */
	{"parseint", "Fp", {Tlong, Tstr, Tend}, ness_codeOrange},
	{"parsereal", "Fq", {Tdbl, Tstr, Tend}, ness_codeOrange},
	{"firstobject", "Fr", {Tptr, Tstr, Tend}, ness_codeOrange},
	{"nextstylegroup", "Fs", {Tstr, Tstr, Tend}, ness_codeOrange},
	{"enclosingstylegroup", "Ft", {Tstr, Tstr, Tend}, ness_codeOrange},
	{"clearstyles", "Fu", {Tstr, Tstr, Tend}, ness_codeOrange},
	{"nextstylesegment", "Fv", {Tstr, Tstr, Tend}, ness_codeOrange},
	/* SearchOp with no arg */
	{"whereitwas", "Fw", {Tstr, Tend}, ness_codeOrange},
	/* SearchOp with three args */
	{"replacewithobject", "Fx", {Tstr, Tstr, Tptr, Tstr, Tend},
						ness_codeYellow},

	/* interp.c */
	{"textimage", "L", {Tstr, Tunk, Tend}, ness_codeOrange},
	{"readfile", "ia", {Tstr, Tstr, Tend}, ness_codeOrange},
	{"readrawfile", "ir", {Tstr, Tstr, Tend}, ness_codeOrange},
	{"writefile", "zWf", {Tstr, Tstr, Tstr, Tend}, ness_codeGreen},
	{"writerawfile", "zWr", {Tstr, Tstr, Tstr, Tend}, ness_codeGreen},
	{"writeobject", "zWo", {Tptr, Tptr, Tstr, Tend}, ness_codeGreen},
	{"print", "j", {Tstr, Tstr, Tend}, ness_codeOrange},
	{"printline", "jN", {Tstr, Tstr, Tend}, ness_codeOrange},
	{"system", "X", {Tstr, Tstr, Tend}, ness_codeGreen},
	{"launchapplication", "UP", {Tvoid, Tbool, Tstr, Tstr, Tstr, Tend}, ness_codeGreen},
	{"isreadonly", "UR", {Tbool, Tstr, Tend}, ness_codeOrange},
	{"telluser", "UT", {Tvoid, Tstr, Tend}, ness_codeOrange},
	{"askuser", "UU", {Tstr, Tstr, Tstr, Tend}, ness_codeOrange},

	{"currentselection", "Ss", {Tstr, Tunk, Tend}, ness_codeOrange},
	{"currentmark", "Sm", {Tstr, Tunk, Tend}, ness_codeOrange},
	{"setcurrentselection", "Y", {Tvoid, Tstr, Tptr, Tend}, ness_codeOrange},
	{"queueanswer", "UQ", {Tvoid, Tstr, Tend}, ness_codeGreen},
	{"queuecancellation", "UC", {Tvoid, Tend}, ness_codeGreen},
	{"dohit", "UH", {Tvoid, Tlong, Tlong, Tlong, Tptr, Tend}, ness_codeYellow},
	{"focus", "G", {Tvoid, Tptr, Tend}, ness_codeOrange},
	{"dokeys", "Kk", {Tvoid, Tstr, Tptr, Tend}, ness_codeGreen},
	{"domenu", "Km", {Tvoid, Tstr, Tptr, Tend}, ness_codeGreen},

	/* U*  nevent.c */
	{"value_getvalue", "Un", {Tlong, Tptr, Tend}, ness_codeOrange},
	{"value_getarraysize", "Uo", {Tlong, Tptr, Tend}, ness_codeOrange},
	{"value_getstring", "Up", {Tstr, Tptr, Tend}, ness_codeOrange},
	{"value_getarrayelt", "Uq", {Tstr, Tlong, Tptr, Tend}, ness_codeOrange},
	{"value_setvalue", "Ur", {Tvoid, Tlong, Tptr, Tend}, ness_codeYellow},
	{"value_setarraysize", "Us", {Tvoid, Tlong, Tptr, Tend}, ness_codeYellow},
	{"value_setstring", "Ut", {Tvoid, Tstr, Tptr, Tend}, ness_codeYellow},
	{"value_setarrayelt", "Uu", {Tvoid, Tstr, Tlong, Tptr, Tend}, ness_codeYellow},
	{"value_setnotify", "Uv", {Tvoid, Tbool, Tptr, Tend}, ness_codeYellow},

	/* J*  call.c */
	{"im_forceupdate", "Jq", {Tvoid, Tend}, ness_codeOrange},
	{"inset", "Jr", {Tptr, Tstr, Tend}, ness_codeOrange},
	{"new", "Js", {Tptr, Tptr, Tend}, ness_codeOrange},
	{"class", "Jt", {Tptr, Tunk, Tend}, ness_codeOrange},

	/* H*  real.c */
	/* unary real operators */
	{"acos", "Ha", {Tdbl, Tdbl, Tend}, ness_codeOrange},
	{"asin", "Hc", {Tdbl, Tdbl, Tend}, ness_codeOrange},
	{"atan", "He", {Tdbl, Tdbl, Tend}, ness_codeOrange},
	{"cos", "Hi", {Tdbl, Tdbl, Tend}, ness_codeOrange},
	{"cosh", "Hj", {Tdbl, Tdbl, Tend}, ness_codeOrange},
	{"erf", "Hk", {Tdbl, Tdbl, Tend}, ness_codeOrange},
	{"erfc", "Hl", {Tdbl, Tdbl, Tend}, ness_codeOrange},
	{"exp", "Hm", {Tdbl, Tdbl, Tend}, ness_codeOrange},
	{"fabs", "Ho", {Tdbl, Tdbl, Tend}, ness_codeOrange},
	{"j0", "Hr", {Tdbl, Tdbl, Tend}, ness_codeOrange},
	{"j1", "Hs", {Tdbl, Tdbl, Tend}, ness_codeOrange},
	{"log", "Hu", {Tdbl, Tdbl, Tend}, ness_codeOrange},
	{"log10", "Hv", {Tdbl, Tdbl, Tend}, ness_codeOrange},
	{"sin", "Hy", {Tdbl, Tdbl, Tend}, ness_codeOrange},
	{"sinh", "Hz", {Tdbl, Tdbl, Tend}, ness_codeOrange},
	{"sqrt", "HA", {Tdbl, Tdbl, Tend}, ness_codeOrange},
	{"tan", "HB", {Tdbl, Tdbl, Tend}, ness_codeOrange},
	{"tanh", "HC", {Tdbl, Tdbl, Tend}, ness_codeOrange},
	{"y0", "HD", {Tdbl, Tdbl, Tend}, ness_codeOrange},
	{"y1", "HE", {Tdbl, Tdbl, Tend}, ness_codeOrange},

#if (! SY_U5x && ! SY_AIXx)

#ifndef VAX_ENV
	{"lgamma", "Ht", {Tdbl, Tdbl, Tend}, ness_codeOrange},
#endif  /* VAX_ENV */

	{"acosh", "Hb", {Tdbl, Tdbl, Tend}, ness_codeOrange},
	{"asinh", "Hd", {Tdbl, Tdbl, Tend}, ness_codeOrange},
	{"atanh", "Hf", {Tdbl, Tdbl, Tend}, ness_codeOrange},
	{"cbrt",  "Hg", {Tdbl, Tdbl, Tend}, ness_codeOrange},
	{"expm1", "Hn", {Tdbl, Tdbl, Tend}, ness_codeOrange},
	{"log1p", "Hw", {Tdbl, Tdbl, Tend}, ness_codeOrange},
#endif /* (!SYSV && !AIX) */

	/* M*  real.c */
	/* binary and other real operators */
	{"atan2",  "Ma", {Tdbl, Tdbl, Tdbl, Tend}, ness_codeOrange},
	{"hypot",  "Mb", {Tdbl, Tdbl, Tdbl, Tend}, ness_codeOrange},
	{"pow", "Mx", {Tdbl, Tdbl, Tdbl, Tend}, ness_codeOrange},	/* Mx out of order */

	/* real to integer */
	{"round",  "Mc", {Tlong, Tdbl, Tend}, ness_codeOrange},
	{"floor",  "Md", {Tlong, Tdbl, Tend}, ness_codeOrange},
	{"ceil",  "Me", {Tlong, Tdbl, Tend}, ness_codeOrange},

	/* real to boolean */
#if !defined(VAX_ENV) && !defined(PMAX_ENV)
#if (! SY_U5x && ! SY_AIXx)
	{"isnan",  "Mf", {Tbool, Tdbl, Tend}, ness_codeOrange},
	{"finite",  "Mg", {Tbool, Tdbl, Tend}, ness_codeOrange},
#endif  /* (!SYSV && !AIX) */
#endif  /* !defined(VAX_ENV) && !defined(PMAX_ENV) */

	/* integer to real */
	{"float",  "Mj", {Tdbl, Tlong, Tend}, ness_codeOrange},
	{"double",  "Mj", {Tdbl, Tlong, Tend}, ness_codeOrange},

	/* (integer, real) => real */
	{"jn",  "Mk", {Tdbl, Tdbl, Tlong, Tend}, ness_codeOrange},
	{"yn",  "Ml", {Tdbl, Tdbl, Tlong, Tend}, ness_codeOrange},

	/* "Mx" is above */	

	/* rexf.c */
	{"centerx", "Da", {Tstr, Tlong, Tstr, Tend}, ness_codeOrange},
	{"centerpadx", "Db", {Tstr, Tstr, Tlong, Tstr, Tend}, ness_codeOrange},
	{"leftx", "Dc", {Tstr, Tlong, Tstr, Tend}, ness_codeOrange},
	{"padx", "Dd", {Tstr, Tstr, Tlong, Tstr, Tend}, ness_codeOrange},
	{"prepadx", "De", {Tstr, Tstr, Tlong, Tstr, Tend}, ness_codeOrange},
	{"prestripx", "Df", {Tstr, Tstr, Tstr, Tend}, ness_codeOrange},
	{"rightx", "Dg", {Tstr, Tlong, Tstr, Tend}, ness_codeOrange},
	{"stripx", "Dh", {Tstr, Tstr, Tstr, Tend}, ness_codeOrange},
	{"substrx", "Di", {Tstr, Tlong, Tlong, Tstr, Tend}, ness_codeOrange},
	{"subwordx", "Dj", {Tstr, Tlong, Tlong, Tstr, Tend}, ness_codeOrange},
	{"wordx", "Dk", {Tstr, Tlong, Tstr, Tend}, ness_codeOrange},
	{"copiesx", "Do", {Tstr, Tlong, Tstr, Tend}, ness_codeOrange},
	{"delstrx", "Dp", {Tstr, Tlong, Tlong, Tstr, Tend}, ness_codeOrange},
	{"delwordx", "Dq", {Tstr, Tlong, Tlong, Tstr, Tend}, ness_codeOrange},
	{"justifyx", "Dr", {Tstr, Tlong, Tstr, Tend}, ness_codeOrange},
	{"reversex", "Ds", {Tstr, Tstr, Tend}, ness_codeOrange},
	{"spacex", "Dt", {Tstr, Tstr, Tend}, ness_codeOrange},
	{"tolowerx", "Du", {Tstr, Tstr, Tend}, ness_codeOrange},
	{"toupperx", "Dv", {Tstr, Tstr, Tend}, ness_codeOrange},
	{"insertx", "DA", {Tstr, Tlong, Tstr, Tstr, Tend}, ness_codeOrange},
	{"overlayx", "DB", {Tstr, Tlong, Tstr, Tstr, Tend}, ness_codeOrange},
	{"translatex", "DC", {Tstr, Tstr, Tstr, Tstr, Tend}, ness_codeOrange},
	{"xrangex", "DH", {Tstr, Tstr, Tstr, Tend}, ness_codeOrange},
	{"findx", "DK", {Tlong, Tstr, Tstr, Tend}, ness_codeOrange},
	{"indexx", "DL", {Tlong, Tstr, Tstr, Tend}, ness_codeOrange},
	{"posx", "zDL", {Tlong, Tstr, Tstr, Tend}, ness_codeOrange},
	{"verifyx", "DO", {Tlong, Tstr, Tstr, Tend}, ness_codeOrange},
	{"wordindexx", "DS", {Tlong, Tlong, Tstr, Tend}, ness_codeOrange},
	{"wordsx", "DT", {Tlong, Tstr, Tend}, ness_codeOrange},
	{"abbrevx", "DW", {Tbool, Tstr, Tstr, Tend}, ness_codeOrange},


	{NULL, NULL}
};

struct builtindef  predefinedTable[] = {
	{"mousex", "Ux", {Tlong}, ness_codeOrange},
	{"mousey", "Uy", {Tlong}, ness_codeOrange},
	{"mouseaction", "Uw", {Tlong}, ness_codeOrange},
	{"mouseleftdown", "Ua", {Tlong}, ness_codeOrange}, 
	{"mouseleftup", "Ub", {Tlong}, ness_codeOrange}, 
	{"mouseleftmove", "Uc", {Tlong}, ness_codeOrange}, 
	{"mouserightdown", "Ud", {Tlong}, ness_codeOrange}, 
	{"mouserightup", "Ue", {Tlong}, ness_codeOrange}, 
	{"mouserightmove", "Uf", {Tlong}, ness_codeOrange}, 
	{"lastkeys", "Uk", {Tstr}, ness_codeOrange},
	{"lastmenu", "Um", {Tstr}, ness_codeOrange},
	{"defaulttext", "E", {Tptr}, ness_codeOrange},
	{"currentinputfocus", "UF", {Tptr}, ness_codeOrange},
	{"currentinset", "UI", {Tptr}, ness_codeOrange},
	{"currentwindow", "UJ", {Tptr}, ness_codeOrange},
	{NULL, NULL}
};

/* callInit(Gscope, idtok, proto)
	initialize the tables used by procedures in this file
	Only needs to be called once.
	'Gscope' is the GrammarScope in which to define builtins
	'idtok' is the token number for identifiers
	'proto' is a struct nesssym value
	defines the LibScope for storing names of ness library functions
*/
	void
callInit(Gscope, idtok, proto)
	nesssym_scopeType Gscope;
	int idtok;
	struct nesssym *proto;
{
	struct builtindef *b;
	struct nesssym *sym;

	for (b = builtintable; b->name != NULL; b++) {
		sym = nesssym_NDefine((unsigned char *)b->name, proto, Gscope);
		nesssym_NSetINode(sym, builtindef, b);
		sym->header.toksym.toknum = idtok;
		sym->type = b->types[0];
		sym->flags = flag_function | flag_builtin;
	}
	for (b = predefinedTable; b->name != NULL; b++) {
		sym = nesssym_NDefine((unsigned char *)b->name, proto, Gscope);
		nesssym_NSetINode(sym, builtindef, b);
		sym->header.toksym.toknum = idtok;
		sym->type = b->types[0];
		sym->flags = flag_var | flag_builtin;
	}
	LibScope = nesssym_NNewScope(nesssym_GLOBAL);
}


/* callPredefId(var)
	generate code for predefined identifier
*/
	void
callPredefId(var)
	struct nesssym *var;
{
	struct builtindef *b = nesssym_NGetINode(var, builtindef);
	unsigned char *defn;
	for (defn = (unsigned char *)b->defn ; *defn; defn++)
		genop(*defn);
}



static char *toomanyargs = "*function call has %d extra argument%s";
static char *toofewargs = "*function call needs %d more argument%s";

	static char *
argcounterror(format, n)
	char *format;
	long n;
{
	char *msg = malloc(50);
	sprintf(msg, format, n, (n>1) ? "s" : "");
	return msg;
}
	static char *
argtypeerror(n, formal, actual)
	long n;
	Texpr formal, actual;
{
	char *msg = malloc(60);
	if (formal == Tend) formal = Tunk;
	if (actual == Tend) actual = Tunk;
	sprintf(msg, "*argument %d should have type %s, but is type %s", 
			n, TypeName[formal], TypeName[actual]);
	return msg;
}


/* builtincall(fnode, argtypes)
	generate a call on builtin function referenced by sym
	check types
*/
	static Texpr
builtincall(fnode, argtypes)
	struct varnode *fnode;
	struct exprnode *argtypes;
{
	struct builtindef *b = nesssym_NGetINode(fnode->sym, builtindef);
	unsigned char *defn;
	register Texpr *type;
	register long nargs;
	register struct exprnode *targs;
	long loc, tloc, len;
	boolean sendnargs;

	loc = fnode->loc;	/* start of function name */
	tloc = tlex_RecentPosition(curComp->tlex, 0, &len);  /* closing ')' */
	len = tloc + len - loc;	/* length of call */
	sendnargs = FALSE;

	/* count number of args */
	nargs = 0;
	for (targs = argtypes;  targs != NULL;  targs = targs->next) 
		nargs++;

	/* the type list at b->types begins with the return type and then has
		argument types in reverse order.  We start with the
		last type and go right to left */
	type = b->types + 1;
	if (*type == Tunk) {
		/* XXX special case for  currentselection(), currentmark(),  textimage(),
			cheat_callmethod(),   cheat_callclassproc(),  class()*/
		if (strcmp(b->name, "textimage") == 0) {
			if (argtypes == NULL || argtypes->next != NULL) 
				SaveError(":textimage must have only one argument", 
						loc, len);
		}
		else if (strncmp(b->name, "current", 7) == 0) {
			if (argtypes == NULL)
				/*  no arg, insert defaulttext */
				genop('E');
			else if (argtypes->next != NULL) 
				SaveError(":cannot have more than one argument", 
						loc, len);
			else if (argtypes->type != Tptr) 
				ExprError(":argument should be a pointer value",
						argtypes);
		}
		else if (strncmp(b->name, "cheat_call", 10) == 0) {
			/* method or class proc call: send along arg count */
			sendnargs = TRUE;
		}
		else if (strcmp(b->name, "class") == 0) {
			/* class() arg is either Tptr or Tstr */
			if (argtypes == NULL || argtypes->next != NULL) 
				SaveError(":class() must have one argument", 
						loc, len);
			if (argtypes->type != Tptr  &&  argtypes->type != Tstr) 
				ExprError(":argument should be a marker or pointer",
						argtypes);
		}
		else {} 
	}
	else {
		/* check match of types in argtypes and type  */
		register long argnum = nargs;
		while (argtypes != NULL && *type != Tend) {
			/* check a type */
			if (*type != argtypes->type)
				SaveError(argtypeerror(argnum, 
					*type, argtypes->type),    loc, len);
			/* advance */
			argtypes = argtypes->next;
			type++;
			argnum --;
		}
		if (argtypes != NULL)
			SaveError(":too many args", loc, len);
		else if (*type != Tend)
			SaveError(":too few args", loc, len);
	}

	if (curComp->ness->accesslevel >= b->minok) {
		/* now (finally) we generate the code for the function */
		for (defn = (unsigned char *)b->defn ; *defn; defn++)
			genop(*defn);
		if (sendnargs) genop(nargs);
	}
	else {
		/* does not meet the security test.  Pop the args and return 0 */
		register long argnum;
		for (argnum = nargs; argnum > 0; argnum--)
			genop('y');
		genop ('0');
		if (b->minok >= ness_codeGreen)
			SaveError(":This function may destroy your files", 
					loc, len);	
		else
			SaveError(":This function may modify a file", 
					loc, len);	
	}
	return *b->types;
}


	void
checkargtypes(func, fexpr, formal, actual)
	struct nesssym *func;	/* the function symbol */
	struct exprnode *fexpr;	/* the function expr */
	struct nesssym *formal;
	struct exprnode *actual;
{
	struct nesssym *tformal;
	struct exprnode *tactual;
	long n, nargs;

	/* check number of arguments */
	tformal = formal;
	tactual = actual;
	nargs = 0;
	while (tformal != NULL && tactual != NULL) {
		tformal = tformal->next;
		tactual = tactual->next;
		nargs++;
	}
	if (tformal != NULL || tactual != NULL) {	/* wrong number of arguments */
		char *msg;
		n = 0;
		if (tformal != NULL)  {	/* too few arguments */
			while (tformal != NULL) n++, tformal = tformal->next;
			msg = argcounterror(toofewargs, n);
		}
		else {	/* too many arguments */
			while (tactual != NULL) n++, tactual = tactual->next;
			msg = argcounterror(toomanyargs, n);
		}
		ExprError(msg, fexpr);
		return;
	}
	/* check types of arguments  (note that lists are the same length) */
	tformal = formal;
	tactual = actual;
	n = 0;
	while (tformal != NULL) {
		if (tactual->type != tformal->type)
			ExprError(argtypeerror(nargs - n, tformal->type, tactual->type), tactual);
		tformal = tformal->next;
		tactual = tactual->next;
		n++;	/* update n late so that arg number is  (nargs - n)  */
	}
}



/* MapType(type, firstop, Pctype)
	determine the Texpr type corresponding to a type-class type
	expect 'firstop' as the first operator
	return the corresponding Ctype in *Pctype
*/
	static Texpr 
MapType(type, firstop, Pctype)
	struct type *type;
	operation firstop;
	Texpr *Pctype;
{
	struct type *super;
	operation *ops;

	ops = type_GetOps(type);
	if (ops[0] != firstop) return Tunk;
	super = type_GetSuper(type);

	if (super == Ctypes->charType || super == Ctypes->ucharType) {
		if (ops[1] == NULL)  {*Pctype = Tchr; return Tlong;}
		if (ops[1] == type_ptr && ops[2] == NULL)  
			{*Pctype = Tcstr; return Tstr;}
		return Tunk;
	}
	if (ops[1] == type_ptr) {*Pctype = Tptr;  return Tptr;}
	if (ops[1] != NULL) return Tunk;
	if (super == Ctypes->longType || super == Ctypes->ulongType)
		{*Pctype = Tlong;  return Tlong;}
	if (super == booleanType)  {*Pctype = Tbool;  return Tbool;}
	if (super == Ctypes->shortType || super == Ctypes->ushortType) 
		{*Pctype = Tshrt;  return Tlong;}
	if (super == Ctypes->intType || super == Ctypes->uintType) {
		*Pctype =  (type_GetSize(super) == 2)  ?  Tshrt  :  Tlong;
		return Tlong;
	}
	return Tunk;
}


/* callCheckClass (var, types)
	Checks for a class with a declaration of the desired name.
	The name is given by 'var'.  
	The argument types (already stacked) are in 'types'.
	return TRUE if succeed and FALSE otherwise

    Here are the processing steps:
	determine if there is such a class
	see if the desired function name exists  (else return FALSE)
	get rettype
	create callnode
	set callnode->variety to callMeth, callClPr, callGet, or callSet
	set where.offset, requiredclass, rettype
	set nargs and argtype[]
	return TRUE

	If the function is a obj_GetXyz(obj), 
		the value of callnode->argtype[2] is the type of the C value
		and callnode->rettype is the type Ness will use
		(callnode->arg[1] is Tunk to indicate one arg)
*/
	static boolean
callCheckClass(var, types)
	struct varnode *var;
	struct exprnode *types;
{	
	char classname[BUFLEN];
	char *dash;
	char *msg;
	struct type *t, *classtype, *a;
	struct callnode *callnode;

	Texpr rettype, extratype, argtypes[11];
	long nargs, offset, loc, len, i;
	enum callvariety variety;

	/* copy name from source code to get the capitalization */
	loc = var->loc;
	len = var->len;
	if (len > BUFLEN-1) len = BUFLEN - 1;
	for (i = 0; i < len; i++, loc++)
		classname[i] = ness_GetChar(curComp->ness, loc);
	classname[len] = '\0';

	dash = (char *)index(classname, '_');
	if (dash == NULL)
		return FALSE;
	*dash = '\0';		/* isolate classname */

	if (Ctypes == NULL) {
		/* one time initialization */
		Ctypes = type_GetCtypes();
		booleanType = type_Lookup(Ctypes->voidType, "boolean");
	}

	msg = type_DeclareClass(classname);
	if (msg && *msg != '\0')  {
		/* generate error message */
		char *emsg = (char *)malloc(strlen(msg)+2);
		*emsg = '*';
		strcpy(emsg+1, msg);
		SaveError(emsg, var->loc, var->len);
		return FALSE;
	}
	classtype = type_Lookup(Ctypes->basicobject, classname);
	if (classtype == NULL) return FALSE;

	/* try to find name as a method or class proc */
	t = type_Lookup(classtype, dash+1);
	if (t == NULL) {
		/* maybe it is Get<fieldname> or Set<fieldname> */
		if (strncmp(dash+1, "Get", 3) == 0) {
			variety = callGet;
			nargs = 1;
		}
		else if (strncmp(dash+1, "Set", 3) == 0) {
			variety = callSet;
			nargs = 2;
			rettype = Tvoid;
		}
		else
			return FALSE;   /* it isn't Getxyz or Setxyx */

		/* ok, process get/set */
		/* find the type struct for the field of interest */
		t = type_Lookup(type_GetClassInfo(classtype, data), dash+4);
		if (t == NULL) {
			/* the field requested is not in this Get or Set */
			SaveError(":No such field in this class", var->loc, var->len);
			nesssym_NSetINode(var->sym, callnode, (struct callnode *)1);
			return TRUE;	/* we found the right function, but it isn't there */
		}

		/* set offset, extratype, argtype, rettype */
		argtypes[0] = Tptr;		/* check type at runtime */
		offset = type_GetOffset(t);
		if (variety == callGet)
			rettype = MapType(t, type_field, &extratype);
		else 
			argtypes[1] = MapType(t, type_field, &extratype);
	}
	else if (*type_GetOps(t) != type_method  
				&& *type_GetOps(t) != type_classmethod)
		return FALSE;
	else {
		/* process a method or class procedure */
		if (*type_GetOps(t) == type_method) {
			variety = callMethod;
			nargs = 1;	/* initial value: count 'self' arg */
			argtypes[0] = Tptr;	/* at runtime we will check the type */
		}
		else {
			variety = callClProc;
			nargs = 0;	/* initial value */
		}
		offset = type_GetIndex(t);
		rettype = MapType(t, *type_GetOps(t), &extratype);
			/* the rettype value is not an exact match to the
			value actually returned.  Tshrt, Tchr, and Tlong
			all have rettype Tlong and will be Tlong values.
			Tstr as rettype implies that the value returned
			is really Tcstr and will be converted */

		/* count the args, nargs initialized above */
		for (a = type_GetPrev(t); a != NULL; a = type_GetPrev(a)) 
			nargs ++;
		/* map each type and store in argtypes[i] */
		for (a = type_GetPrev(t), i = nargs; 
				a != NULL; 
				a = type_GetPrev(a))  
			argtypes[--i] = MapType(a, type_param, &extratype);

		extratype = Tend;
	}

	/* at this point the code above has to have set the following values needed below:
		variety, offset, rettype, nargs, argtype[], extratype
		extratype will usually be Tend, but not for a Get or Set
	*/

	callnode = (struct callnode *)malloc(sizeof(struct callnode) 
			+ sizeof(Texpr) * 
				(nargs+ ((extratype == Tend) ? 1 : 2) - MAXARGS ));
	callnode->variety = variety;
	callnode->where.offset = offset;
	callnode->requiredclass = class_Load(classname);
	callnode->rettype = rettype;

	callnode->nargs = nargs;
	callnode->argtype[nargs] = Tend;
	if (extratype != Tend) 
		callnode->argtype[nargs+1] = extratype;
	while (nargs-- > 0)
		callnode->argtype[nargs] = argtypes[nargs];
	nesssym_NSetINode(var->sym, callnode, callnode);
	var->sym->flags = flag_function | flag_classdefn;

	return TRUE;
}

static struct hack {
	char procmod[15];
	char modname[9];
} ModuleHack [] = {
	/* names that prefix dashed entries in the proctable */
	{"bodies", "text822v"},
	{"folder", "folders"},
	{"frame", "framecmd"},
	{"self", "zipedit"},
	{"raster", "rasterv"},	/* obsolete */
	{0, 0}

};
/* names that appear as-is in the proctable 
 *	{"self_insert", "spread"},
 *	{"wraplook", "stylesht"},
 */
/* names it is silly to check for because im is always loaded 
 *		and these are obsolete names anyway
 *	{"redraw", "im"}, 	redraw-window
 *	{"start", "im"}, 	start-keyboard-macro
 *	{"stop", "im"},	stop-keyboard-macro
 *	{"play", "im"},	play-keyboard-macro
 *	{"exit", "im"},		exit
 */

/* callCheckProcTable(func)
	check to see if function called is in the proctable  
	(try loading if needed)
	if found, attach a callnode to var
	return TRUE if succeed and FALSE otherwise
*/
	static boolean
callCheckProcTable(varnode, argtypes)
	struct varnode *varnode;
	struct exprnode *argtypes;
{
	struct nesssym *func;
	struct proctable_Entry *pe;
	unsigned char procname[BUFLEN];
	unsigned char modname[BUFLEN];
	unsigned char *dash;
	struct callnode *call;
	long nargs, i;
	struct exprnode *e;
	struct hack *gack;

	func = varnode->sym;

	/* extract modname from front of symbol's name and
			copy entire name to procname, changing _ to - */
	strncpy(procname, nesssym_NGetName(func), BUFLEN);
	procname[BUFLEN-1] = '\0';	/* truncate to BUFLEN characters */
	dash = (unsigned char *)index(procname, '_');
	if (dash == NULL) {
		if (strcmp(procname, "wraplook") != 0)
			return FALSE;
		/* It is "wraplook" (GACK, sigh) */
		strcpy(modname, "stylesht");
	}
	else {
		strncpy(modname, nesssym_NGetName(func), dash - procname);
		modname[dash - procname] = '\0';
		for (dash=procname; (dash=(unsigned char *)index(dash, '_')); )
			*dash = '-';
	}
	/* look up procname in proctable */
	pe=proctable_Lookup(procname);
	if (pe == NULL) {
		/* try loading the implied module */
		if (strcmp(procname, "self-insert") == 0) {
			/* GACK (sigh) self_insert from table/keyboard.c */
			strcpy(modname, "spread");
			strcpy(procname, "self_insert");
		}
		else for (gack = &ModuleHack[0];  gack->procmod[0] != 0;  gack++)
			if (strcmp(modname, &gack->procmod[0]) == 0) {
				strcpy(modname, &gack->modname[0]);
				break;
			}
		class_Load(modname);
		pe=proctable_Lookup(procname);
		if (pe == NULL) 
			return FALSE;
	}

	if (curComp->ness->accesslevel < ness_codeGreen) 
		/* illegal at this accesslevel */
		SaveError(":This function may modify or destroy your files", 
			varnode->loc, varnode->len);

	for (nargs=0, e=argtypes; e != NULL;  nargs++, e = e->next) {}
	call = (struct callnode *)malloc(sizeof(struct callnode) 
				+ nargs * (sizeof(Texpr) - MAXARGS));

	/* XXX bogus: we set the argtypes from the first instance of this call */

	for (i = nargs, e=argtypes; e != NULL;  e = e->next) 
		call->argtype[--i] = e->type;
	call->nargs = nargs;
	call->variety = callPE;
	call->where.pe = pe;
	switch (pe->returntype) {
		case proctable_Boolean:   call->rettype = Tbool;  break;
		case proctable_Double:  call->rettype = Tvoid;  break;
		case proctable_Object:  call->rettype = Tptr;  break;
		case proctable_StaticString:  call->rettype = Tstr;  break;
		case proctable_DisposeString:  call->rettype = Tstr;  break;
			/* CORELEAK  XXX fail to discard */
			/* XXX we should use Tcstr and reserve Tstr
				 for proctable_NessMark */
		default:	call->rettype = Tlong;  break;
	}
	call->requiredclass = NULL;
	nesssym_NSetINode(func, callnode, call);
	func->flags = flag_function | flag_proctable;
	proctable_ForceLoaded(pe);
	if (!proctable_Defined(pe)) 
		SaveError(":could not load proctable procedure", 
				varnode->loc, varnode->len);
	return TRUE;
}


/* callFunc(varnode, argtypes)
	called during compilation to generate object code for a function call
	'varnode' is the function reference
	'argtypes' is a list of exprnodes, each giving the type of an arg (reverse order)
     Possible kinds of function:
	flag_function | flag_builtin		builtin - use builtincall
	flag_function | flag_ness		Ness library - find out so with callCheckLib
	flag_function | flag_undef		unknown - may be a forward reference
	flag_function | flag_ness		previously defined Ness function
	flag_function | flag_ness | flag_xfunc	defined in an 'extend'
	flag_function | flag_proctable  	proctable function
	flag_function | flag_classdefn  	method, class procedure, Get, or Set
*/
	struct exprnode *
callFunc(varnode, argtypes)
	struct varnode* varnode;
	struct exprnode *argtypes;
{
	struct nesssym *var = varnode->sym;
	struct exprnode *val;	/* return value */
	long loc, len;
	Texpr rettype = Tstr;
	struct funcnode *fnode;
	struct callnode *cnode;
	struct exprnode *e;
	long n;
	char *msg;

	loc = tlex_RecentPosition(curComp->tlex, 0, &len);
	val = exprnode_Create(rettype, NULL, FALSE, 
			varnode->loc, loc+len - varnode->loc);

	/* first try to find the definition if not previously encountered */

	if (var != NULL && (fnode=nesssym_NGetINode(var, funcnode)) == NULL ) {
		/* call to unknown function */

		/* move to the outerscope for the current compilation, 
			so other references to the same function will find it  */
		sym_SetScope((struct sym *)var, curComp->ness->outerScope); 

		if (callCheckLib(nesssym_NGetName(var), &fnode) == OK) {
			/* found a Ness library that might have the name */
			if (fnode != NULL) {
				nesssym_NSetINode(var, funcnode, fnode);
				var->flags = flag_function | flag_ness;
			}
			else {
				/* is a for-sure undefined func */
				SaveError(
	":the Ness library file for the given prefix, does not define this function", 
						varnode->loc, varnode->len);
				nesssym_NSetINode(var, funcnode, (struct funcnode *)1);
			}
		}
			/* We check to see if it is a proctable function
			  before going off to parse the .ch file to get the types */

		else if (callCheckProcTable(varnode, argtypes)) {}
			/* it is a call on the proctable */
		else if (curComp->ness->ClassEnabled 
				&&  callCheckClass(varnode, argtypes)) {}
			/* it is a method, classproc, or instance variable reference */
		else {
			/* first call to an undefined function:  build callnode */
			for (n=0, e=argtypes; e != NULL;  n++, e = e->next) {}
			cnode = (struct callnode *)malloc(sizeof(struct callnode)
				+  sizeof(Texpr) * (n - MAXARGS));
			cnode->nargs = n;
			for (e=argtypes; e != NULL; e = e->next) 
				cnode->argtype[--n] = e->type;
			cnode->variety = callSym;
			cnode->requiredclass = NULL;
			cnode->where.Sym = var;
			cnode->rettype = Tstr;	/* ASSUME MARKER FUNC
						for forward calls */
			nesssym_NSetINode(var, callnode, cnode);
			var->flags = flag_function | flag_forward;
		}
	}

	/* now check argument types and generate code */

	if (var != NULL)  {
		fnode = nesssym_NGetINode(var, funcnode);
		cnode = (struct callnode *)fnode;
		switch (var->flags) {
		case flag_function | flag_ness:
		case flag_function | flag_ness | flag_xfunc:
			/* call to previously defined ness function (or library func) */ 
			checkargtypes(var, val, fnode->parmlist, argtypes);
			refSysMark(getcurfuncmark(), 'O', fnode->SysMarkOffset);
			rettype = fnode->functype;
			break;
	
		case flag_function | flag_builtin:
			rettype = builtincall(varnode, argtypes);
			break;
	
		case flag_function | flag_proctable:
		case flag_function | flag_classdefn:
		case flag_function | flag_forward:
			/* count number of actual args */
			for (n=0, e=argtypes; e != NULL;  n++, e = e->next) {}
	
			/* check that there are the proper number of args */
			if (n < cnode->nargs)
				msg = argcounterror(toofewargs, cnode->nargs - n);
			else if (n > cnode->nargs)
				msg = argcounterror(toomanyargs, n - cnode->nargs);
			else msg = NULL;
			if (msg) 
				SaveError(msg, varnode->loc, varnode->len);
			else {
				/* check that the args have the proper types */
				for (n--, e = argtypes; e != NULL; n--, e = e->next) 
					if (e->type != cnode->argtype[n]) {
						msg = argtypeerror(n+1, 
								cnode->argtype[n], 
								e->type); 
						SaveError(msg, varnode->loc, varnode->len);
					}
			}
			refAddress(getcurfuncmark(), 'C', cnode);
			rettype = cnode->rettype;
			break;
	
		case flag_function | flag_undef:
			break;
		}
	}

	/* delete argtypes list elements */
	while (argtypes != NULL) {
		struct exprnode *t = argtypes->next;
		exprnode_Destroy(argtypes);
		argtypes = t;
	}
	varnode_Destroy(varnode);
	val->type = rettype;
	return val;
}

/* callUnknown(sym)
	make the sym a call on an unknown function
	the node value will later point to a callnode
	(called from varIsFunction)
*/
	void
callUnknown(sym)
	struct nesssym *sym;
{
	sym->flags = flag_function | flag_undef;
	nesssym_NSetINode(sym, callnode, NULL);
}





/* = = = = = = = = = = = = = = = = = = = = = = = = = = = = *\
 *
 *	Run Time
 *
\* = = = = = = = = = = = = = = = = = = = = = = = = = = = = */




/* callCheck (call, NSP, iar, ness)
	call->variety is callSym
	This is used only for forward function calls.
	Resolve the function reference indicated by 'call' and check argument types
	Use value of 'NSP' to find arguments.  Use 'iar' in RunError calls.
	Will convert to callNess if function was a forward reference;
	otherwise signals a runtime error.

	If the function was defined, the nesssym referenced by the four bytes 
	following the opcode will now have flags  flag_function|flag_ness

XXX this function is no longer needed: everything but forward references are done
at compile time.  Forward references should be done with fixups at the end of the compilation
(and should use 'O' instead of 'C')

XXX this function does not test the types of the values on the stack

*/
	void
callCheck (call, iar, ness)
	struct callnode *call;
	unsigned char *iar;
	struct ness *ness;
{
	struct nesssym *func = call->where.Sym;	/* the nesssym for desired function */
	register struct nesssym *formals;
	register long n;
	struct funcnode *fnode;

	if (func->flags != (flag_function | flag_ness))
		/* function not found, issue warning */
		RunError(":call to undefined proc", iar);
	fnode = nesssym_NGetINode(func, funcnode);

	/* count formals and check types against actuals */
	for (n=0, formals = fnode->parmlist; formals != NULL;
			formals = formals->next, n++) {}
	if (call->nargs > n)
		RunError(argcounterror(toomanyargs, call->nargs - n), iar);
	else if (call->nargs < n)
		RunError(argcounterror(toofewargs, n - call->nargs), iar);
	else for (formals = fnode->parmlist; --n, formals != NULL;
			formals = formals->next) 
		if (formals->type != call->argtype[n]) 
			RunError(argtypeerror(call->nargs - n, 
				formals->type,  call->argtype[n]),  iar);

	/* we have found the desired function.  Convert to callNess. */
	call->variety = callNess;
	call->where.Nproc = fnode->SysMarkOffset;
}

union anytype {
	long l;
	struct basicobject *p;
	struct view *v;
	unsigned char *s;
	struct nessmark *m;
	boolean b;
	double *dp;
	float f;
	char c;
	short h;
	struct callnode *cn;
};

/* getProc(object, call)
	from 'object' at offset 'call->where.offset'
	get the value of type 'call->argtype[2]' 
		and store in 'hold'
*/
	static long   /* actually, union anytype */
getProc(object, call)
	struct basicobject *object;
	struct callnode *call;
{
	union anytype ret;
	long p;		/* XXX KLUDGE.   Assume a long holds a pointer */
	p = ((long)object) + call->where.offset;
	switch (call->argtype[2]) {
	case Tbool:   ret.b = *((boolean *)p);   break;
	case Tchr:   ret.l = *((unsigned char *)p);   break;
	case Tshrt:   ret.l = *((short *)p);   break;
	case Tlong:   ret.l = *((long *)p);   break;
	case Tptr:   ret.p = *((struct basicobject  **)p);   break;
	case Tcstr:   ret.s = *((unsigned char  **)p);   
		if (ret.p == NULL) ret.s = (unsigned char *)"";  break;
	default:   ret.l = 0;   break;
	}
	return ret.l;
}

/* setProc(object, val, call)
	store into 'object' at location 'call->where.offset'
	a value of type 'call->argtype[3]' (a C type)
	the value 'val', which is of type 'call->argtype[1]' (a Ness type)
	no need to pop the stack
*/
	static long
setProc(object, val, call)
	struct basicobject *object;
	long val;	/* really any single word type */
	struct callnode *call;
{
	union anytype v;
	long p;		/* XXX KLUDGE.   Assume a long holds a pointer */
	p = ((long)object) + call->where.offset;
	v.l = val;
	switch (call->argtype[3]) {
	case Tbool:   *((boolean *)p) = v.b;   break;
	case Tchr:    *((unsigned char *)p) = v.c;   break;
	case Tshrt:   *((short *)p) = v.h;   break;
	case Tlong:   *((long *)p) = val;   break;
	case Tptr:     *((struct basicobject  **)p) = v.p;   break;
	case Tcstr:    *((unsigned char  **)p) = nessmark_ToC(v.m);   break;
				/* XXX CORELEAK WARNING: The existing 
				value is replaced, without being freed. 
				There is nothing that will free the new value. 
				XXX WARNING TROUBLE:  If the old value
				was a buffer known to some portion of the
				system, the assignment causes something to 
				NO LONGER refer to the buffer.  */
	default:          break;
	}
	return 0;
}

	static union stackelement *
startPush(NSP, size, hdr, v)
	register union stackelement *NSP;
	long size;
	TType hdr;
	long v;
{
	NSP = (union stackelement *)(((unsigned long)NSP) - size);
	NSP->l.hdr = hdr;
	NSP->l.v = v;
	NSPstore = NSP;
	return NSP;
}


/* callCfunc(call, NSP, iar, ness)
	call the C function or proctable entry indicated by callnode 'call'
	call is limited to 10 argument words
	FLOATS AND DOUBLES MAY NOT BE PASSED OR RETURNED
	call-by-address is not supported

	arg types supported:	Ness value:
		[unsigned] long		long
		enum(...)			long
		[unsigned] char *		marker
		struct xyz *		pointer
		char			long
		short			long
		boolean			boolean

	return types supported	Ness value:
		[unsigned] long		long
		enum(...)			long
		[unsigned] char *		marker
		struct xyz *		pointer
		char			long
		short			long
		boolean			boolean
		

	steps:
		1. get args into an array
		2. get the proc
		3. check first arg if req'd
		4. call the proc
		5. free allocated values
		6. push return value onto stack
*/
	void
callCfunc(call, iar, ness)
	struct callnode *call;
	unsigned char *iar;
	struct ness *ness;
{
	register union stackelement *NSP = NSPstore;
	union anytype arg[10];	/* arguments (from stack) */
	union anytype retval;	/* returned value (push to stack) */
	boolean malloced[10];	/* T if arg[i] pts to malloced space */
	long n;		/* local var */
	long nargs;	/* actual number of arg words in array */
	long (*proc)();		/* the function to call */
	TType lasttype;		/* local var */
	boolean checkFirstArg;	/* whether first arg must be object */
	struct view *v;		/* first arg if any */
	struct dataobject *d;
	struct text *text;
	boolean createdview;
	long argmax;		/* number of argument words actually passed */
	long FirstIndex;		/* kludge for class proc first arg */

	nargs = call->nargs;
	argmax = nargs;
	FirstIndex = 0;

	/* 1. get a pointer to the function to call */
	checkFirstArg = TRUE;
	createdview = FALSE;

	switch(call->variety) {
	case callPE:
		proc = (long(*)())proctable_GetFunction(call->where.pe);
		call->requiredclass = proctable_GetType(call->where.pe);
		checkFirstArg = (call->requiredclass != NULL);
		break;
	case callGet:
		proc = getProc;
		arg[argmax++].cn = call;
		break;	
	case callSet:
		proc = setProc;
		arg[argmax++].cn = call;
		break;	
	case callClProc:
		proc = call->requiredclass
			->procs->routines[call->where.offset];
		arg[0].p = NULL;  /* XXX should be classheader, 
					but there is no way to get it */
		argmax++;
		FirstIndex = 1;
		checkFirstArg = FALSE;
		break;	
	}

	/* 2. get C versions of arguments into array.  
		pop the stack
		XXX if we ever are able to pass nessmarks to C functions,
			we must be careful that popping the stack
			does not delete the text pointed at by a nessmark
	*/
	for (n = nargs-1+FirstIndex; n >= FirstIndex; n--)  {
		malloced[n] = FALSE;
		lasttype = NSP->l.hdr;
		/* load arg 'n' from stack */
		/* XXX should we check type versus call->argtype[n] ???  */
		switch (lasttype) {
		case ptrHdr:
			arg[n].p = NSP->p.v;
			NSPopSpace(ptrstkelt);
			break;
		case longHdr:
		case boolHdr:
			arg[n].l = NSP->l.v;
			NSPopSpace(longstkelt);
			break;
#ifdef notdef
		case dblHdr:  {
			/* KLUDGE ALERT:  We move the args to the right
				to make space for a double value */
			long i;
			union { double d;
				struct {long l1, l2;} l;
			} kludge;
			ndbl++;
			for (i = 8; i > n; i--) {
				arg[i+1] = arg[i];
				malloced[i+1] = malloced[i];
			}
			kludge.d = NSP->d.v;
#ifndef VAX_ENV 
			arg[n].l = kludge.l.l1;
			arg[n+1].l = kludge.l.l2;
#else  /* VAX_ENV */
	/* XXX does this word swapping work for vax??? */
			arg[n+1].l = kludge.l.l1;
			arg[n].l = kludge.l.l2;
#endif  /* VAX_ENV */
			NSPopSpace(dblstkelt);
		}	break;
#endif /* notdef */
		default:
	      		if (NSP->l.hdr == nessmarkHdr) {
				arg[n].s = nessmark_ToC(&NSP->m);
				malloced[n] = TRUE;
				nessmark_Finalize(&NSP->m);
				NSPopSpace(nessmark);
			}
			else
				/* ERROR: unknown arg type */
				RunError(":unknown arg type", iar);
			break;
		}
	}


	/* 3. if required, check first arg and perhaps invent it 
		(comparing classinfo values checks that it is the proper class 
		and also that it is the right version number ) */
	if (checkFirstArg) {
		v = arg[0].v;
		if (nargs == 0) {
			/* there are no args, use input focus or currentinset */
			v = (struct view *)im_GetLastUsed();
			if (call->requiredclass != imClass)
				v = (ness->CurrentInset != NULL) 
					? ness->CurrentInset 
				: (v != NULL) ? im_GetInputFocus((struct im *)v)
				: NULL;
			argmax++;
			lasttype = ptrHdr;
		}
		else if (call->argtype[0] != Tptr)  {
			/* oops, first arg should be a pointer, but isn't */
			RunError(":first argument should be a pointer, but isn't", iar);
		}
		if (v == NULL)  {
			/* XXX should somehow be able to deal with undisplayed insets */
			/* for the nonce we ignore proctable calls with NULL first
				arg on the notion that this probably comes
				from a call of inset() for an inset 
				whose view has never been exposed  */
			goto FreeTheMallocs;
		}

		/* find view appropriate to type required by proc */
		arg[0].p = ProperPtr((struct basicobject *)v, call->requiredclass);
		if (arg[0].p == NULL && class_IsType(v, dataobjectClass)  
				&& class_IsType(call->requiredclass, viewClass)) {
			/* wants a view on the data object.  Generate one. */
			/* XXX need to cache the generated views */
			/* XXX it is pretty bogus to insert the view into the im */
			d = (struct dataobject *)v;
			v = (struct view *)class_NewObject(dataobject_ViewName(d));
			if (class_IsType(v, call->requiredclass)) {
				static struct rectangle playpen = {0,0,0,0};
				view_SetDataObject(v, d);
				view_InsertView(v, (struct view *)im_GetLastUsed(),
						 &playpen);
				arg[0].v = v;
				createdview = TRUE;
			}
			else view_Destroy(v);
		}
		if (arg[0].p == NULL) {
			char *buf, *wantname;
			wantname = (char *)class_GetTypeName(
					proctable_GetType(call->where.pe)   );
			buf = malloc(60 + strlen(class_GetTypeName(v)) 
					+ strlen(wantname));
			sprintf(buf, 
				"*first arg to proctable call is  /%s/, but should be /%s/",
				class_GetTypeName(v),  wantname);
			RunError(buf, iar);
		}
	}

	/* 2. get the proc (reprise) */
	if (call->variety == callMethod) 
		proc = arg[0].v->header.view_methods->routines[call->where.offset];

	/* 4. call the function */
	switch (argmax) {
		case  0: retval.l = proc();   break;
		case  1: retval.l = proc(arg[0].l);   break;
		case  2: retval.l = proc(arg[0].l, arg[1].l);   break;
		case  3: retval.l = proc(arg[0].l, arg[1].l, arg[2].l);   break;
		case  4: retval.l = proc(arg[0].l, arg[1].l, arg[2].l, arg[3].l);   break;
		case  5: retval.l = proc(arg[0].l, arg[1].l, arg[2].l, arg[3].l, arg[4].l);   break;
		case  6: retval.l = proc(arg[0].l, arg[1].l, arg[2].l, arg[3].l, arg[4].l, 
				arg[5].l);   break;
		case  7: retval.l = proc(arg[0].l, arg[1].l, arg[2].l, arg[3].l, arg[4].l,
				arg[5].l, arg[6].l);   break;
		case  8: retval.l = proc(arg[0].l, arg[1].l, arg[2].l, arg[3].l, arg[4].l, 
				arg[5].l, arg[6].l, arg[7].l);   break;
		case  9: retval.l = proc(arg[0].l, arg[1].l, arg[2].l, arg[3].l, arg[4].l,
				arg[5].l, arg[6].l, arg[7].l, arg[8].l);   break;
		case 10: retval.l = proc(arg[0].l, arg[1].l, arg[2].l, arg[3].l, arg[4].l, 
				arg[5].l, arg[6].l, arg[7].l, arg[8].l, arg[9].l);   break;
	}

FreeTheMallocs:
	/* 5.  free allocated values */
	for (n = nargs-1; n >= 0; n--) 
		if (malloced[n]) 
			free((char *)arg[n].s);
	if (createdview) {
		/* XXX should cache the view */
		/*  XXX should set data object  in the view to NULL,
			but view.c is broken:
			view_SetDataObject(v, NULL);  */
		view_Destroy(v);
	}

	/* 6. push retval */
	switch (call->rettype) {
	case Tlong:
		NSP = startPush(NSP, sizeof(struct longstkelt), longHdr, retval.l);
		break;
	case Tbool:
		NSP = startPush(NSP, sizeof(struct boolstkelt), boolHdr, retval.l);
		break;
	case Tptr:
		NSP = startPush(NSP, sizeof(struct ptrstkelt), ptrHdr, retval.l);
		break;
	case Tstr:
		NSP = startPush(NSP, sizeof(struct nessmark), 0, 0);
		text = text_New();
		text_InsertCharacters(text, 0, retval.s, strlen(retval.s));
		nessmark_Initialize(&NSP->m);
		nessmark_Set(&NSP->m, text, 0, text_GetLength(text));
		break;
	default:
		NSP = startPush(NSP, sizeof(struct longstkelt), longHdr, 0);
		break;
	}
}


/* = = = = = = = = = = = = = = = = = = = = = = = = = = = = *\
 *
 *	The Cheat functions
 *
\* = = = = = = = = = = = = = = = = = = = = = = = = = = = = */

/* callCheat(op, iar, ness)
	implement the  cheat_  functions
*/
	void
callCheat(op, iar, ness)
	unsigned char op;
	unsigned char *iar;
	struct ness *ness;
{
	register union stackelement *NSP = NSPstore;
	switch (op) {

	/* eventually the following ought to move to nevent.c */

	case 'q':	im_ForceUpdate();  break;

	case 'r':	{				/* inset() */
		struct celview *v;
		/* load to stack top a pointer to the object named by the
			string now at stack top */
		if (ness_GetArbiter(ness) == NULL) 
			v = NULL;
		else {
			unsigned char *s;
			/* put name from mark at NSP into buf */ 
			s = nessmark_ToC(&NSP->m);
			/* get ptr to named inset */
			v = arbiterview_GetNamedCelview(ness_GetArbiter(ness), s);
			free(s);
		}
		NSP = popValue(NSP);		/* discard string */
		NSPushSpace(ptrstkelt);
		NSP->p.hdr = ptrHdr;
		NSP->p.v = (struct basicobject *)v;
	}	break;
	case 's':	{				/* new() */
		/* arg is ptr for the class */
		struct classinfo *ci;
		if (NSP->p.hdr != ptrHdr  ||  NSP->p.v == NULL)  
			RunError(":arg was not a non-NULL pointer", iar);
		ci = (struct classinfo *)NSP->p.v;
		NSP = popValue(NSP);
		NSPushSpace(ptrstkelt);
		NSP->p.hdr = ptrHdr;
		NSP->p.v = (struct basicobject *)(ci->procs->routines[0])(ci,
				ci->versionnumber);
	}	break;
	case 't':	{				/* class */
		/* arg is string naming the class */
		struct classinfo *cip;
		if ((TType)((struct nessmark *)NSP)->header.nessmark_methods 
						== nessmarkHdr)  {
			unsigned char *s;
			s = nessmark_ToC(&NSP->m);
			cip = class_Load(s);
			if (cip == NULL) {
				char msg[250];
				sprintf(msg, "*could not find class \"%s\"", s);
				RunError(freeze(msg), iar);
			}
			free(s);
		}
		else if (NSP->p.hdr == ptrHdr  &&  NSP->p.v != NULL) 
			cip = class_GetType(NSP->p.v);
		else
			RunError(":arg is neither a mark nor an object (is it initialized?)", iar);
		NSP = popValue(NSP);
		NSPushSpace(ptrstkelt);
		NSP->p.hdr = ptrHdr;
		NSP->p.v = (struct basicobject *)cip;
	}	break;

	} /* end switch(op) */
}


/* = = = = = = = = = = = = = = = = = = = = = = = = = = = = *\
 *
 *	Find all files in library path
 *
\* = = = = = = = = = = = = = = = = = = = = = = = = = = = = */


/* recordFiles(path, end)
	make a libnode for each .n file in the directory 'dir'
*/
	static void
recordFiles(dir, proto)
	unsigned char *dir;
	struct nesssym *proto;
{
	unsigned char *dirname;
	long len;
	DIR *dirp;
	DIRENT_TYPE *entry;

	len = strlen(dir);
	dirname = (unsigned char *)malloc(len+2);
	strcpy(dirname, dir);
	if (*(dirname+len-1) != '/')
		strcat(dirname+len, "/");
	dirp = opendir(dirname);
	if (dirp == NULL) return;
	while ((entry=readdir(dirp)) != NULL) {
		/* process a directory entry */
		unsigned char *fname, *symname, *t;
		struct nesssym *s;
		boolean new;

		t = (unsigned char *)(entry->d_name + DIRENT_NAMELEN(entry));
		if (*(t-2) != '.' ||  *(t-1) != 'n')
			/* skip all files other than *.n */
			continue;
		fname = (unsigned char *)malloc(DIRENT_NAMELEN(entry)+1);
		symname = (unsigned char *)malloc(DIRENT_NAMELEN(entry)+1);
		strncpy(fname, entry->d_name, DIRENT_NAMELEN(entry));
		fname[DIRENT_NAMELEN(entry)] = '\0';
		/* build symbol name */
		strcpy(symname, fname);
		symname[DIRENT_NAMELEN(entry)-2] = '\0';	/* remove .n */
		for (t=symname; *t; t++)		/* smash case */
			if (isupper(*t)) *t = tolower(*t);
		s = nesssym_NLocate(symname, proto, LibScope, &new);
		free(symname);
		if (new) {
			/* we only record this file name if we do
				not previously have a library file of
				the same name */
			LibList = libnode_Create(dirname, fname, NULL,
					NotRead, -1, LibList);
			nesssym_NSetINode(s, libnode, LibList);
		}
		else free (fname);
	}
	closedir(dirp);
}

/* callDirLibs()
	read directories on NessPath and find out what '.n' files exist
	build the LibList.
*/
	void
ness_callDirLibs()
{
	unsigned char *path, *colon;
	unsigned char dirname[MAXPATHLEN+1], tc;
	struct nesssym *proto = nesssym_New();

	path = (unsigned char *)environ_GetProfile("nesspath");
	if (path == NULL) 
		path = (unsigned char *)environ_AndrewDir("/lib/ness/");
	/* path is a colon separated list of directories */
	while (TRUE) {
		while (isspace(*path) || *path == ':') path++;
		if (*path == '\0') {
			nesssym_Destroy(proto);
			return;
		}
		colon = (unsigned char *)index(path, ':');
		if (colon == NULL)
			colon = path + strlen(path);
		/* colon points just beyond end of path */

		tc = *colon;
		*colon = '\0';


		filetype_CanonicalizeFilename(dirname, path, MAXPATHLEN);
		recordFiles(dirname, proto);

		*colon = tc;
		path = colon;
	}

/* xxxx need to check classpath and make a list of all .ch files available
for callCheckClass  */

}



/* = = = = = = = = = = = = = = = = = = = = = = = = = = = = *\
 *
 *	Compile Time: look for function in library
 *
\* = = = = = = = = = = = = = = = = = = = = = = = = = = = = */

/* callCompLib(lnode)
	create a ness for 'lnode'
	compile the file referenced by 'lnode'
	set the status field in 'lnode' appropriately
*/
	void
callCompLib(lnode)
	struct libnode *lnode;
{
	unsigned char fullname[MAXPATHLEN+1];

	if (lnode->status == ReadFailed)
		return;
	if (lnode->ness == NULL) {
		lnode->ness = ness_New();
		ness_SetName(lnode->ness, lnode->filename);
		lnode->ness->libnode = lnode;
	}

	ness_SetAccessLevel(lnode->ness, ness_codeUV);
	if (lnode->status == NotRead) {
		/* open the file and read it into the ness 
			(it may be a text data stream) */
		strcpy(fullname, lnode->path);
		strcat(fullname, lnode->filename);
		if (ness_ReadNamedFile(lnode->ness, fullname) 
				!= dataobject_NOREADERROR) {
			lnode->ness->ErrorList = errornode_Create(lnode->ness,
					0, 0, 0, ":file read failed", 
					FALSE, NULL);
			ness_Expose(lnode->ness);	
			lnode->status = ReadFailed;
			return;
		}
		lnode->status = NotCompiled;
	}

	/* compile the ness */
	if (lnode->status == NotCompiled) {
		lnode->status = Compiling;
		if (ness_Compile(lnode->ness) != NULL) {
			lnode->status = CompiledWithError;
			ness_Expose(lnode->ness);
		}
		else
			lnode->status = OK;
	}
}


/* callCheckLib(fun, fnode)
	looks to see if the function named 'fun' is in the library
	and loads it if so
	returns enum libstate value: OK if found and parsed the library
	If OK and find desired function, set *fnode to its funcnode
	(OK does not necessarily mean the function was found.)

	All function names are assumed to have an underline; the part 
	preceding the underline is taken to be the file name.
	If the file has a function named filename_Init, that function
	is called after a successful compile.
*/
	enum libstate
callCheckLib(fun, fnode)
	unsigned char *fun;
	struct funcnode **fnode;
{
	unsigned char fname[MAXNAMLEN+1];
	unsigned char *under;
	long nmlen;
	struct nesssym *libsym, *funcsym;
	struct libnode *libnode;
	struct libusenode *libuse;
	boolean found;

	*fnode = NULL;

	/* get file name */
	under = (unsigned char *)index(fun, '_');
	if (under == NULL)
		return NotRead;   /* not valid library function name */
	nmlen = MIN(MAXNAMLEN, under - fun);
	strncpy(fname, fun, nmlen);
	fname[nmlen] = '\0';
	libsym = nesssym_NFind(fname, LibScope);
	if (libsym == NULL) 
		return NotRead;		/* no such library file */
	libnode = nesssym_NGetINode(libsym, libnode);

	if (libnode->useid != curComp->ness->compilationid) {
		/* add it to the list of libraries used by curComp->ness */
		libnode->useid = curComp->ness->compilationid;

		/* check to see if it is on the libuselist yet */
		found = FALSE;
		for (libuse = curComp->ness->libuseList; libuse != NULL;
				libuse = libuse->next)
			if (libuse->used == libnode) {
				found = TRUE;
				break;
			}
		if ( ! found) 
			/* add it to the list of libraries used by curComp->ness */
			curComp->ness->libuseList = libusenode_Create(libnode,
					curComp->ness->libuseList);
	}

	if (libnode->status != OK)
		callCompLib(libnode);

		/* note: no 'else' here.  we test the outcome of this
		or an earlier compile */
	if (libnode->status != OK) 
		return libnode->status;
	
	/* find the desired function in the file */
	funcsym = nesssym_NFind(fun, libnode->ness->outerScope);
	if (funcsym != NULL && funcsym->flags == (flag_function | flag_ness))
		/* BINGO! */
		*fnode = nesssym_NGetINode(funcsym, funcnode);
	return OK;
}


static long InitId;	/* which initialization cycle are we doing */

/* callInitSubTree(ness)
	calls init() in subtree headed by ness
	does not call it if its compilationid is InitId, to avoid circular call graphs
*/
	struct errornode *
callInitSubTree(ness)
	struct ness *ness;
{
	struct libusenode *t;
	struct errornode *err;
	if (ness->ErrorList != NULL)
		return ness->ErrorList;
	if (ness->needsInit && InitId == ness->compilationid) {
		/* somewhere higher in the call chain, this ness is being 
			initialized,  But now it also needs to be initialized.
			Therefore there is a circularity.  Error 

			This situation can arise when there is a forward reference within 
			a library package to a routine in that same package.
		*/
		unsigned char buf[300];
		sprintf(buf, "*Circular initializations involving %s\n", ness->name);
		return ness->ErrorList = 
			errornode_Create(ness, 0, 0, 0, freeze(buf), TRUE, ness->ErrorList);
	}

	ness->compilationid = InitId;

	/* init all children on the off chance that the init calls a child */
	for (t = ness->libuseList;  t != NULL;  t = t->next) {
		err = callInitSubTree(t->used->ness);
		if (err != NULL)
			return err;
	}

	/* init this ness */
	if (ness->needsInit) {
		/* find and interpret init()
			MapRunErrors if needed
		*/
		ness->ErrorList = interpretNess(
				nesssym_NGetINode(ness->InitFunc, funcnode)
						->SysMarkOffset, 
				NULL, ness);

		if (ness->ErrorList == NULL) {
			/* COMPATABILITY with 00: accept init() for initialization */
			struct nesssym *funcsym;
			for (funcsym = ness->globals; funcsym != NULL; 
					funcsym = funcsym->next)
				if (strcmp(funcsym->header.sym.name, "init") == 0) 
					break;
			if (funcsym != NULL) 
				ness->ErrorList = interpretNess(
					nesssym_NGetINode(funcsym, funcnode)
						->SysMarkOffset, 
					NULL, ness);
		}

		if (ness->ErrorList != NULL) {
			neventUnpost(ness, FALSE);	/* remove old postings */
			MapRunError(ness);
			ness_Expose(ness);
		}
		ness->needsInit = FALSE;
	}
	return ness->ErrorList;
}


/* callInitAll(ness)
	call init() for 'ness' and all libraries it calls
*/
	struct errornode *
callInitAll(ness)
	struct ness *ness;
{
	InitId = im_GetWriteID();
	return callInitSubTree(ness);
}


/* ReadTextFileStream(text, name, f)
	read a text from an already open file
	the file name is required so it can be checked to see
		what the extension is, and thus what kind of object
	if the file is an arbiter object surrounding a text object, 
		the latter is extracted from the former
	if the file is a non-text object it is an error, unless objok is TRUE
	if the file is a non-text object and objok is TRUE,
		the object is returned as the only element in the text
*/
	long
ReadTextFileStream(text, name, f, objok)
	struct text *text;
	unsigned char *name;
	FILE *f;
	boolean objok;
{
	long objectID;
	long val;
	char *objectType;
	struct attributes *attributes;

	objectType = filetype_Lookup(f, name, &objectID, &attributes);
	if (objectType != NULL)
		if (class_IsTypeByName(objectType, "arbiter")) {
			/* XXX there is an arbiter wrapped around it */
			struct text *t;
			struct arbiter *shit = arbiter_New();
			val = arbiter_Read(shit, f, objectID);
			if (val != dataobject_NOREADERROR)
				return val;
			t = (struct text *)arbiter_GetObject(shit);
			if (class_IsType(t, textClass)) {
				if (attributes != NULL)
					text_SetAttributes(text, attributes);
				text_AlwaysCopyTextExactly(text, 0, 
					t, 0, text_GetLength(t));
				val = dataobject_NOREADERROR;
			}
			else 
				val = dataobject_BADFORMAT;
			arbiter_Destroy(shit);
			return val;
		}
		else if ( ! class_IsTypeByName(objectType, "text")) {
			if (objok) {
				struct viewref *vr;
				struct dataobject *dobj;
				vr = text_InsertObject(text, 0, 
						objectType, NULL);
				if (vr == NULL) 
					return dataobject_BADFORMAT;
				dobj = vr->dataObject;
				if (attributes != NULL)
					dataobject_SetAttributes(dobj, 
						attributes);
				return dataobject_Read(dobj, f, objectID);
			}
			else {
			  /*	fprintf(stderr, "%s, it's a %s\n", 
					"File is not a ness",
					objectType);  */
				return dataobject_BADFORMAT;
			}
		}
	if (attributes != NULL)
		text_SetAttributes(text, attributes);
	return text_Read(text, f, objectID);
}

