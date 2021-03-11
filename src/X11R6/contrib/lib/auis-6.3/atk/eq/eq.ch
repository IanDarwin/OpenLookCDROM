/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
*Permission to use, copy, modify, and distribute this software and its 
*documentation for any purpose is hereby granted without fee, 
*provided that the above copyright notice appear in all copies and that 
*both that copyright notice, this permission notice, and the following 
*disclaimer appear in supporting documentation, and that the names of 
*IBM, Carnegie Mellon University, and other copyright holders, not be 
*used in advertising or publicity pertaining to distribution of the software 
*without specific, written prior permission.
*
*IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
*DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
*ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
*SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
*BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
*DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
*WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
*ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
*OF THIS SOFTWARE.
* $
*/


 

/*
 * eq.H
 * class header file for eq.
 */



#include <mark.ih>

class eq : dataobject[dataobj] {
overrides:
    Read(FILE *file, long id) returns long;
    Write(FILE *file, long writeid, long level) returns long;

methods:
/*
 * The array of struct formulas in an equation is contained in a
 * struct eq, which is maintained by the following
 * routines.  The data structure itself is private.
 */

    Insert(long pos, struct formula *f);
    InsertTokens(long pos, char *s) returns long;
    InsertTokensCarefully(long pos, char *s) returns long;
    Delete(long pos);
    DeleteCarefully(long start, long stop) returns long;
    DoScript(long pos, enum script *script, char *string) returns long;
    CreateMark(long pos, long length) returns struct mark *;
    RemoveMark(struct mark *mark);
    Erase();
    Access(long n) returns struct formula *;
    Size() returns long;
    NextFormula(struct formula *f) returns struct formula *;
    GetTokens(long *startp, long stop, char *string, long size) returns long;   /* fix -- make static? */
    FindEndGroup(long i) returns long;
    FindBeginGroup(long i) returns long;
    FindLeftSibling(long i) returns long;
    FindRightSibling(long i) returns long;
    WriteFILE(FILE *f, long start, long stop, char sep);
    ReadFILE(FILE *f, long start) returns long;
    Dump(char *name);
    Parse(FILE *file, long w);
classprocedures:
    InitializeObject(struct eq *self) returns boolean;
    FinalizeObject(struct eq *self);
    Lookup(char *name) returns struct symbol *;
    InitializeClass() returns boolean;

data:
/*
 * An equation.  .formula points to an array of struct formulas.  This
 * array has p1 + gap + p2 entries. The first p1 elements of
 * the array contain valid information; the next gap elements
 * of the array contain garbage; and the final p2 elements of
 * the array contain the remainder of the valid information.
 * Thus p1 + p2 elements of the array are in use, and gap elements
 * of the array are free.
 */
    struct formula *f;
    short p1, gap, p2;
    struct mark *markList;
    struct mark *fence;
};

/* Enums--see struct symbol below. */
enum align {HLIST, LPILE, CPILE, RPILE, MAX_align };
enum eqstyle { D_EQSTYLE, T_EQSTYLE, S_EQSTYLE, SS_EQSTYLE, MAX_eqstyle };
enum script {SUP, SUB, ABOVE, BELOW, MAX_script };
enum simple {ITALIC, ROMAN, BOLD, SYMBOL, SYMBOLA, SYM, MAX_simple };
enum genre { ORD, OP, BIN, REL, OPEN, CLOSE, PUNCT, INNER, MAX_genre };

/*
 * Symbol table.  Each symbol has a general type (.type) and
 * a more specific type (.what).  This two-level description
 * makes certain formatting operations easier.
 */
struct symbol {
    char *name;					/* print name */
    enum type {
	BEGIN, END,				/* control tokens */
	ALIGN,					/* set alignment type */
	EQSTYLE,					/* set eqstyle */
	SCRIPT,					/* script follows */
	SIMPLE,					/* real symbols */
	EXTEND,					/* extendables */
	MAX_type
    } type;
    char what;					/* should be a union of: */
	/* enum align if type is ALIGN */
	/* enum eqstyle if type is EQSTYLE */
	/* enum script if type is SCRIPT */
	/* enum simple if type is SIMPLE */
    enum genre genre;				/* spacing genre */
    char *string;				/* for real symbols */
    long y_genre;				/* parse genre */
    char *doc;					/* online documentation string */
};

/*
 * An equation is (conceptually) an array of struct formula.
 * Each formula contains among other things a pointer to it's symbol.
 */
struct formula {
    struct pos {
	short x, y;
    } min, max;					/* ul and lr corners */
    short sup_y, sub_y, kern;			/* script positioning info */
    struct pos pos;				/* where relative to group */
    struct pos hot;				/* absolute hot spot */
    struct fontdesc *font;			/* for drawing */
    struct symbol *symbol;			/* symbol for this formula */
    char *string;				/* string for extendables */
    boolean has_hot_spot;			/* whether dot can be here */
    boolean transparent;
    boolean deletable;
    boolean verticalExtend;			/* TRUE if it extends vertically */
};

/* Miscellaneous */
#ifndef NIL
#define NIL 0
#endif

#ifndef ABS
#define ABS(x)   ((x)>0? (x) : -(x))
#endif

/* 
 * Knuth's spacing classifications for symbols.  See The TeXbook p. 170.
 * This is in eq.ch only so that symbols.c can be separately compiled.
 */
enum space {		/* Knuth:	Description:			     */
    EQBAD,		/*  *		 Can't happen			     */
    NOSPC,		/*  0		 No space 			     */
    CTHIN,		/* (1)		 Thin space only in t and d eqstyles   */
    THIN,		/*  1		 Thin space in any eqstyle	     */
    CMED,		/* (2)		 Medium space only in t and d eqstyles */
    MED,		/*  2		 Medium space in any eqstyle	     */
    CTHICK,		/* (3)		 Thick space only in t and d eqstyles  */
    THICK,		/*  3		 Thick space in any eqstyle	     */
    MAX_space
};


/* generally useful.  Initialized in eqview_InitializeClass. */
static struct symbol *zilch;
static struct symbol *root;

