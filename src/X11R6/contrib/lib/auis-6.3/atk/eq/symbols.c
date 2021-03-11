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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/eq/RCS/symbols.c,v 2.7 1994/03/27 22:11:57 rr2b Exp $";
#endif


 

/*
 * symbols.c
 * This module handles the symbols for eq.
 */

#include <class.h>
#define AUXMODULE 1
#include <eq.eh>

#include <exfont.h>
#include <eqparse.h>

#define NONE -1
#define what (int)

struct symbol symbols[] = {

/*
    eq		fmt	what		space	font	yacc
    string		genre			genre	string	genre
*/

    "{",	BEGIN,	NONE,		INNER,	NIL,	Y_BEGIN,	NIL,
    "{ord",	BEGIN,	NONE,		ORD,	NIL,	Y_BEGIN,	NIL,
    "{op",	BEGIN,	NONE,		OP,	NIL,	Y_BEGIN,	NIL,
    "{bin",	BEGIN,	NONE,		BIN,	NIL,	Y_BEGIN,	NIL,
    "{rel",	BEGIN,	NONE,		REL,	NIL,	Y_BEGIN,	NIL,
    "{open",	BEGIN,	NONE,		OPEN,	NIL,	Y_BEGIN,	NIL,
    "{close",	BEGIN,	NONE,		CLOSE,	NIL,	Y_BEGIN,	NIL,
    "{punct",	BEGIN,	NONE,		PUNCT,	NIL,	Y_BEGIN,	NIL,
    "{inner",	BEGIN,	NONE,		INNER,	NIL,	Y_BEGIN,	NIL,

    "lpile",	ALIGN,	what LPILE,	ORD,	NIL,	Y_PILE,		NIL,
    "cpile",	ALIGN,	what CPILE,	ORD,	NIL,	Y_PILE,		NIL,
    "rpile",	ALIGN,	what RPILE,	ORD,	NIL,	Y_PILE,		NIL,

    "d_eqstyle",EQSTYLE,what D_EQSTYLE,	ORD,	NIL,	Y_EQSTYLE,	NIL,
    "t_eqstyle",EQSTYLE,what T_EQSTYLE,	ORD,	NIL,	Y_EQSTYLE,	NIL,
    "s_eqstyle",EQSTYLE,what S_EQSTYLE,	ORD,	NIL,	Y_EQSTYLE,	NIL,
    "ss_eqstyle",EQSTYLE,what SS_EQSTYLE,ORD,	NIL,	Y_EQSTYLE,	NIL,

    "sup",	SCRIPT,	what SUP,	ORD,	NIL,	Y_SUP,		NIL,
    "sub",	SCRIPT,	what SUB,	ORD,	NIL,	Y_SUB,		NIL,
    "above",	SCRIPT,	what ABOVE,	ORD,	NIL,	Y_SUP,		NIL,
    "below",	SCRIPT,	what BELOW,	ORD,	NIL,	Y_SUB,		NIL,

    "}",	END,	NONE,		ORD,	NIL,	Y_END,		NIL,

    "--",	SIMPLE,	what ITALIC,	ORD,	NIL,	Y_ATOM,		NIL,
    "bold-",	SIMPLE,	what BOLD,	ORD,	NIL,	Y_ATOM,		NIL,
    "roman-",	SIMPLE,	what ROMAN,	ORD,	NIL,	Y_ATOM,		NIL,

    "bold-a",	SIMPLE,	what BOLD,	ORD,	"a",	Y_ATOM,		NIL,

    "a",	SIMPLE,	what ITALIC,	ORD,	"a",	Y_ATOM,		NIL,
    "b",	SIMPLE,	what ITALIC,	ORD,	"b",	Y_ATOM,		NIL,
    "c",	SIMPLE,	what ITALIC,	ORD,	"c",	Y_ATOM,		NIL,
    "d",	SIMPLE,	what ITALIC,	ORD,	"d",	Y_D,		NIL,
    "e",	SIMPLE,	what ITALIC,	ORD,	"e",	Y_ATOM,		NIL,
    "f",	SIMPLE,	what ITALIC,	ORD,	"f",	Y_ATOM,		NIL,
    "g",	SIMPLE,	what ITALIC,	ORD,	"g",	Y_ATOM,		NIL,
    "h",	SIMPLE,	what ITALIC,	ORD,	"h",	Y_ATOM,		NIL,
    "i",	SIMPLE,	what ITALIC,	ORD,	"i",	Y_ATOM,		NIL,
    "j",	SIMPLE,	what ITALIC,	ORD,	"j",	Y_ATOM,		NIL,
    "k",	SIMPLE,	what ITALIC,	ORD,	"k",	Y_ATOM,		NIL,
    "l",	SIMPLE,	what ITALIC,	ORD,	"l",	Y_ATOM,		NIL,
    "m",	SIMPLE,	what ITALIC,	ORD,	"m",	Y_ATOM,		NIL,
    "n",	SIMPLE,	what ITALIC,	ORD,	"n",	Y_ATOM,		NIL,
    "o",	SIMPLE,	what ITALIC,	ORD,	"o",	Y_ATOM,		NIL,
    "p",	SIMPLE,	what ITALIC,	ORD,	"p",	Y_ATOM,		NIL,
    "q",	SIMPLE,	what ITALIC,	ORD,	"q",	Y_ATOM,		NIL,
    "r",	SIMPLE,	what ITALIC,	ORD,	"r",	Y_ATOM,		NIL,
    "s",	SIMPLE,	what ITALIC,	ORD,	"s",	Y_ATOM,		NIL,
    "t",	SIMPLE,	what ITALIC,	ORD,	"t",	Y_ATOM,		NIL,
    "u",	SIMPLE,	what ITALIC,	ORD,	"u",	Y_ATOM,		NIL,
    "v",	SIMPLE,	what ITALIC,	ORD,	"v",	Y_ATOM,		NIL,
    "w",	SIMPLE,	what ITALIC,	ORD,	"w",	Y_ATOM,		NIL,
    "x",	SIMPLE,	what ITALIC,	ORD,	"x",	Y_ATOM,		NIL,
    "y",	SIMPLE,	what ITALIC,	ORD,	"y",	Y_ATOM,		NIL,
    "z",	SIMPLE,	what ITALIC,	ORD,	"z",	Y_ATOM,		NIL,

    "A",	SIMPLE,	what ITALIC,	ORD,	"A",	Y_ATOM,		NIL,
    "B",	SIMPLE,	what ITALIC,	ORD,	"B",	Y_ATOM,		NIL,
    "C",	SIMPLE,	what ITALIC,	ORD,	"C",	Y_ATOM,		NIL,
    "D",	SIMPLE,	what ITALIC,	ORD,	"D",	Y_ATOM,		NIL,
    "E",	SIMPLE,	what ITALIC,	ORD,	"E",	Y_ATOM,		NIL,
    "F",	SIMPLE,	what ITALIC,	ORD,	"F",	Y_ATOM,		NIL,
    "G",	SIMPLE,	what ITALIC,	ORD,	"G",	Y_ATOM,		NIL,
    "H",	SIMPLE,	what ITALIC,	ORD,	"H",	Y_ATOM,		NIL,
    "I",	SIMPLE,	what ITALIC,	ORD,	"I",	Y_ATOM,		NIL,
    "J",	SIMPLE,	what ITALIC,	ORD,	"J",	Y_ATOM,		NIL,
    "K",	SIMPLE,	what ITALIC,	ORD,	"K",	Y_ATOM,		NIL,
    "L",	SIMPLE,	what ITALIC,	ORD,	"L",	Y_ATOM,		NIL,
    "M",	SIMPLE,	what ITALIC,	ORD,	"M",	Y_ATOM,		NIL,
    "N",	SIMPLE,	what ITALIC,	ORD,	"N",	Y_ATOM,		NIL,
    "O",	SIMPLE,	what ITALIC,	ORD,	"O",	Y_ATOM,		NIL,
    "P",	SIMPLE,	what ITALIC,	ORD,	"P",	Y_ATOM,		NIL,
    "Q",	SIMPLE,	what ITALIC,	ORD,	"Q",	Y_ATOM,		NIL,
    "R",	SIMPLE,	what ITALIC,	ORD,	"R",	Y_ATOM,		NIL,
    "S",	SIMPLE,	what ITALIC,	ORD,	"S",	Y_ATOM,		NIL,
    "T",	SIMPLE,	what ITALIC,	ORD,	"T",	Y_ATOM,		NIL,
    "U",	SIMPLE,	what ITALIC,	ORD,	"U",	Y_ATOM,		NIL,
    "V",	SIMPLE,	what ITALIC,	ORD,	"V",	Y_ATOM,		NIL,
    "W",	SIMPLE,	what ITALIC,	ORD,	"W",	Y_ATOM,		NIL,
    "X",	SIMPLE,	what ITALIC,	ORD,	"X",	Y_ATOM,		NIL,
    "Y",	SIMPLE,	what ITALIC,	ORD,	"Y",	Y_ATOM,		NIL,
    "Z",	SIMPLE,	what ITALIC,	ORD,	"Z",	Y_ATOM,		NIL,

    "!",	SIMPLE,	what ROMAN,	ORD,	"!",	Y_BAD,		NIL,
    ",",	SIMPLE,	what ROMAN,	ORD,	",",	Y_BAD,		NIL,
    ".",	SIMPLE,	what ROMAN,	ORD,	".",	Y_BAD,		NIL,

    "0",	SIMPLE,	what ROMAN,	ORD,	"0",	Y_ATOM,		NIL,
    "1",	SIMPLE,	what ROMAN,	ORD,	"1",	Y_ATOM,		NIL,
    "2",	SIMPLE,	what ROMAN,	ORD,	"2",	Y_ATOM,		NIL,
    "3",	SIMPLE,	what ROMAN,	ORD,	"3",	Y_ATOM,		NIL,
    "4",	SIMPLE,	what ROMAN,	ORD,	"4",	Y_ATOM,		NIL,
    "5",	SIMPLE,	what ROMAN,	ORD,	"5",	Y_ATOM,		NIL,
    "6",	SIMPLE,	what ROMAN,	ORD,	"6",	Y_ATOM,		NIL,
    "7",	SIMPLE,	what ROMAN,	ORD,	"7",	Y_ATOM,		NIL,
    "8",	SIMPLE,	what ROMAN,	ORD,	"8",	Y_ATOM,		NIL,
    "9",	SIMPLE,	what ROMAN,	ORD,	"9",	Y_ATOM,		NIL,

    "alpha",	SIMPLE,	what SYMBOL,	ORD,	"a",	Y_ATOM,		NIL,
    "beta",	SIMPLE,	what SYMBOL,	ORD,	"b",	Y_ATOM,		NIL,
    "gamma",	SIMPLE,	what SYMBOL,	ORD,	"g",	Y_ATOM,		NIL,
    "delta",	SIMPLE,	what SYMBOL,	ORD,	"d",	Y_ATOM,		NIL,
    "epsilon",	SIMPLE,	what SYMBOL,	ORD,	"e",	Y_ATOM,		NIL,
    "zeta",	SIMPLE,	what SYMBOL,	ORD,	"z",	Y_ATOM,		NIL,
    "eta",	SIMPLE,	what SYMBOL,	ORD,	"h",	Y_ATOM,		NIL,
    "theta",	SIMPLE,	what SYMBOL,	ORD,	"q",	Y_ATOM,		NIL,
    "vartheta",	SIMPLE,	what SYMBOL,	ORD,	"J",	Y_ATOM,		NIL,
    "iota",	SIMPLE,	what SYMBOL,	ORD,	"i",	Y_ATOM,		NIL,
    "kappa",	SIMPLE,	what SYMBOL,	ORD,	"k",	Y_ATOM,		NIL,
    "lambda",	SIMPLE,	what SYMBOL,	ORD,	"l",	Y_ATOM,		NIL,
    "mu",	SIMPLE,	what SYMBOL,	ORD,	"m",	Y_ATOM,		NIL,
    "nu",	SIMPLE,	what SYMBOL,	ORD,	"n",	Y_ATOM,		NIL,
    "xi",	SIMPLE,	what SYMBOL,	ORD,	"x",	Y_ATOM,		NIL,
    "omicron",	SIMPLE,	what SYMBOL,	ORD,	"o",	Y_ATOM,		NIL,
    "pi",	SIMPLE,	what SYMBOL,	ORD,	"p",	Y_ATOM,		NIL,
    "rho",	SIMPLE,	what SYMBOL,	ORD,	"r",	Y_ATOM,		NIL,
    "varsigma",	SIMPLE,	what SYMBOL,	ORD,	"V",	Y_ATOM,		NIL,
    "sigma",	SIMPLE,	what SYMBOL,	ORD,	"s",	Y_ATOM,		NIL,
    "tau",	SIMPLE,	what SYMBOL,	ORD,	"t",	Y_ATOM,		NIL,
    "upsilon",	SIMPLE,	what SYMBOL,	ORD,	"u",	Y_ATOM,		NIL,
    "phi",	SIMPLE,	what SYMBOL,	ORD,	"j",	Y_ATOM,		NIL,
    "varphi",	SIMPLE,	what SYMBOL,	ORD,	"f",	Y_ATOM,		NIL,
    "chi",	SIMPLE,	what SYMBOL,	ORD,	"c",	Y_ATOM,		NIL,
    "psi",	SIMPLE,	what SYMBOL,	ORD,	"y",	Y_ATOM,		NIL,
    "omega",	SIMPLE,	what SYMBOL,	ORD,	"w",	Y_ATOM,		NIL,
    "varomega",	SIMPLE,	what SYMBOL,	ORD,	"v",	Y_ATOM,		NIL,

    "Alpha",	SIMPLE,	what SYMBOL,	ORD,	"A",	Y_ATOM,		NIL,
    "Beta",	SIMPLE,	what SYMBOL,	ORD,	"B",	Y_ATOM,		NIL,
    "Gamma",	SIMPLE,	what SYMBOL,	ORD,	"G",	Y_ATOM,		NIL,
    "Delta",	SIMPLE,	what SYMBOL,	ORD,	"D",	Y_ATOM,		NIL,
    "Epsilon",	SIMPLE,	what SYMBOL,	ORD,	"E",	Y_ATOM,		NIL,
    "Zeta",	SIMPLE,	what SYMBOL,	ORD,	"Z",	Y_ATOM,		NIL,
    "Eta",	SIMPLE,	what SYMBOL,	ORD,	"H",	Y_ATOM,		NIL,
    "Theta",	SIMPLE,	what SYMBOL,	ORD,	"Q",	Y_ATOM,		NIL,
    "Iota",	SIMPLE,	what SYMBOL,	ORD,	"I",	Y_ATOM,		NIL,
    "Kappa",	SIMPLE,	what SYMBOL,	ORD,	"K",	Y_ATOM,		NIL,
    "Lambda",	SIMPLE,	what SYMBOL,	ORD,	"L",	Y_ATOM,		NIL,
    "Mu",	SIMPLE,	what SYMBOL,	ORD,	"M",	Y_ATOM,		NIL,
    "Nu",	SIMPLE,	what SYMBOL,	ORD,	"N",	Y_ATOM,		NIL,
    "Xi",	SIMPLE,	what SYMBOL,	ORD,	"X",	Y_ATOM,		NIL,
    "Omicron",	SIMPLE,	what SYMBOL,	ORD,	"O",	Y_ATOM,		NIL,
    "Pi",	SIMPLE,	what SYMBOL,	ORD,	"P",	Y_ATOM,		NIL,
    "Rho",	SIMPLE,	what SYMBOL,	ORD,	"R",	Y_ATOM,		NIL,
    "Sigma",	SIMPLE,	what SYMBOL,	ORD,	"S",	Y_ATOM,		NIL,
    "Tau",	SIMPLE,	what SYMBOL,	ORD,	"T",	Y_ATOM,		NIL,
    "Upsilon",	SIMPLE,	what SYMBOL,	ORD,	"U",	Y_ATOM,		NIL,
    "Phi",	SIMPLE,	what SYMBOL,	ORD,	"F",	Y_ATOM,		NIL,
    "Chi",	SIMPLE,	what SYMBOL,	ORD,	"C",	Y_ATOM,		NIL,
    "Psi",	SIMPLE,	what SYMBOL,	ORD,	"Y",	Y_ATOM,		NIL,
    "Omega",	SIMPLE,	what SYMBOL,	ORD,	"W",	Y_ATOM,		NIL,

    "log",	SIMPLE,	what ROMAN,	OP,	"log",	Y_FUNC,		"",
    "lim",	SIMPLE,	what ROMAN,	OP,	"lim",	Y_FUNC,		"",
    "sin",	SIMPLE,	what ROMAN,	OP,	"sin",	Y_FUNC,		"",
    "cos",	SIMPLE,	what ROMAN,	OP,	"cos",	Y_FUNC,		"",
    "tan",	SIMPLE,	what ROMAN,	OP,	"tan",	Y_FUNC,		"",
    "sinh",	SIMPLE,	what ROMAN,	OP,	"sinh",	Y_FUNC,		"",
    "cosh",	SIMPLE,	what ROMAN,	OP,	"cosh",	Y_FUNC,		"",
    "tanh",	SIMPLE,	what ROMAN,	OP,	"tanh",	Y_FUNC,		"",

    "cint",	SIMPLE,	what SYM,	OP,	"C",	Y_INT,		"circular integral",
    "int",	SIMPLE,	what SYM,	OP,	"I",	Y_INT,		"integral",

    "sum",	SIMPLE,	what SYM,	OP,	"S",	Y_ITER,		"summation op",
    "product",	SIMPLE,	what SYM,	OP,	"P",	Y_ITER,		"product op",
    "union_op",	SIMPLE,	what SYM,	OP,	"U",	Y_ITER,		"union op",
    "or_op",	SIMPLE,	what SYM,	OP,	"V",	Y_ITER,		"logical or op",
    "inter_op",	SIMPLE,	what SYM,	OP,	"W",	Y_ITER,		"intersection op",
    "and_op",	SIMPLE,	what SYM,	OP,	"X",	Y_ITER,		"logical and op",

    "inf",	SIMPLE,	what SYM,	ORD,	"%",	Y_ATOM,		"infinity",

    "+",	SIMPLE,	what SYM,	BIN,	"+",	Y_OP,		NIL,
    "+-",	SIMPLE,	what SYM,	BIN,	"p",	Y_OP,		"plus or minus",
    "-",	SIMPLE,	what SYM,	BIN,	"-",	Y_OP,		NIL,
    "-+",	SIMPLE,	what SYM,	BIN,	"m",	Y_OP,		"minus or plus",
    "*",	SIMPLE,	what SYM,	BIN,	"*",	Y_BINOP,	"times",
    "/",	SIMPLE,	what SYM,	BIN,	"/",	Y_BINOP,	NIL,

    "<",	SIMPLE, what SYM,	REL,	"<",	Y_BINOP,	NIL,
    "<=",	SIMPLE, what SYM,	REL,	"l",	Y_BINOP,	"less or equal",
    "=",	SIMPLE,	what SYM,	REL,	"=",	Y_EQ,		NIL,
    ">=",	SIMPLE, what SYM,	REL,	"g",	Y_BINOP,	"greater or equal",
    ">",	SIMPLE, what SYM,	REL,	">",	Y_BINOP,	NIL,

    "down",	SIMPLE, what SYM,	BIN,	"\\",	Y_BINOP,	"downward arrow",
    "->",	SIMPLE, what SYM,	BIN,	"]",	Y_GOESTO,	"right arrow",
    "up",	SIMPLE, what SYM,	BIN,	"^",	Y_BINOP,	"upward arrow",
    "<-",	SIMPLE, what SYM,	BIN,	"_",	Y_BINOP,	"left arrow",

/*
    "Ainf",	SIMPLE,	what SYMBOLA,	ORD,	"%",	Y_ATOM,		NIL,

    "A+",	SIMPLE,	what SYMBOL,	BIN,	"+",	Y_OP,		NIL,
    "A+-",	SIMPLE,	what SYMBOLA,	BIN,	"1",	Y_OP,		NIL,
    "A-",	SIMPLE,	what SYMBOL,	BIN,	"-",	Y_OP,		NIL,
    "A*",	SIMPLE,	what SYMBOLA,	BIN,	"4",	Y_BINOP,	NIL,
    "A/",	SIMPLE,	what SYMBOLA,	BIN,	"$",	Y_BINOP,	NIL,

    "A<",	SIMPLE, what SYMBOL,	REL,	"<",	Y_BINOP,	NIL,
    "A<=",	SIMPLE, what SYMBOLA,	REL,	"#",	Y_BINOP,	NIL,
    "A=",	SIMPLE,	what SYMBOL,	REL,	"=",	Y_EQ,		NIL,
    "A>=",	SIMPLE, what SYMBOLA,	REL,	"3",	Y_BINOP,	NIL,
    "A>",	SIMPLE, what SYMBOL,	REL,	">",	Y_BINOP,	NIL,

    "Adown",	SIMPLE, what SYMBOL,	BIN,	"\\",	Y_BINOP,	NIL,
    "A->",	SIMPLE, what SYMBOL,	BIN,	"]",	Y_GOESTO,	NIL,
    "Aup",	SIMPLE, what SYMBOL,	BIN,	"^",	Y_BINOP,	NIL,
    "A<-",	SIMPLE, what SYMBOL,	BIN,	"_",	Y_BINOP,	NIL,

    "prime",	SIMPLE, what SYMBOLA,	ORD,	"\"",	Y_ATOM,		NIL,
    "dprime",	SIMPLE, what SYMBOLA,	ORD,	"2",	Y_ATOM,		NIL,
*/

    "all",	SIMPLE, what SYMBOL,	ORD,	"\"",	Y_ATOM,		"for all",
    "exists",	SIMPLE, what SYMBOL,	ORD,	"$",	Y_ATOM,		"there exists",
    "st",	SIMPLE, what SYMBOL,	OP,	"'",	Y_OP,		"such that",
    "<>",	SIMPLE, what SYMBOLA,	BIN,	"9",	Y_BINOP,	"not equal",
    "approx",	SIMPLE, what SYMBOLA,	BIN,	".",	Y_BINOP,	"approximately",
    "and",	SIMPLE, what SYMBOLA,	BIN,	"Y",	Y_BINOP,	"logical and",
    "or",	SIMPLE, what SYMBOLA,	BIN,	"Z",	Y_BINOP,	"logical or",

    "'",	SIMPLE, what SYM,	ORD,	"'",	Y_PRIME,	NIL,
    "dot",	SIMPLE, what SYMBOL,	ORD,	".",	Y_DOT,		NIL,

    "zilch",	SIMPLE,	what SYM,	ORD,	"z",	Y_ATOM,		NIL,

    "cthin",	EXTEND,	what EX_CTHIN,	ORD,	NIL,	Y_SPACE,	NIL,
    "thin",	EXTEND,	what EX_THIN,	ORD,	NIL,	Y_SPACE,	"thin space",
    "cmed",	EXTEND, what EX_CMED,	ORD,	NIL,	Y_SPACE,	NIL,
    "med",	EXTEND,	what EX_MED,	ORD,	NIL,	Y_SPACE,	"medium space",
    "cthick",	EXTEND,	what EX_CTHICK,	ORD,	NIL,	Y_SPACE,	NIL,
    "thick",	EXTEND,	what EX_THICK,	ORD,	NIL,	Y_SPACE,	"thick space",

    "bar",	EXTEND,	what EX_BAR,	ORD,	NIL,	Y_BAD,		NIL,
    "over",	EXTEND,	what EX_OVER,	ORD,	NIL,	Y_OVER,		NIL,

    "(",	EXTEND,	what EX_LPAREN,	OPEN,	NIL,	Y_OPEN,		NIL,
    ")",	EXTEND,	what EX_RPAREN,	CLOSE,	NIL,	Y_CLOSE,	NIL,
    "[",	EXTEND,	what EX_LSQUARE,OPEN,	NIL,	Y_OPEN,		NIL,
    "]",	EXTEND,	what EX_RSQUARE,CLOSE,	NIL,	Y_CLOSE,	NIL,
    "lbrace",	EXTEND,	what EX_LBRACE,	OPEN,	NIL,	Y_OPEN,		NIL,
    "rbrace",	EXTEND,	what EX_RBRACE,	CLOSE,	NIL,	Y_CLOSE,	NIL,
    "langle",	EXTEND,	what EX_LANGLE,	OPEN,	NIL,	Y_OPEN,		NIL,
    "rangle",	EXTEND,	what EX_RANGLE,	CLOSE,	NIL,	Y_CLOSE,	NIL,

    "slash",	EXTEND, what EX_SLASH,	BIN,	NIL,	Y_BINOP,	"",

    "root",	EXTEND,	what EX_ROOT,	OPEN,	NIL,	Y_ROOT,		NIL,

    NIL

};


struct symbol *eq__Lookup(classID, name)
struct classheader *classID;
char *name;
{
    register struct symbol *s;
    for (s=symbols;  s->name;  s++)
	if (*name == *(s->name) && strcmp(name, s->name)==0)
	    break;
    if (s->name)
	return s;
    else
	return NIL;
}
