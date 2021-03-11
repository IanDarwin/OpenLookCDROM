/* global.h  -  global variables for gentlex */
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

#ifndef boolean
#define boolean char
#define TRUE 1
#define FALSE 0
#endif

#ifndef NULL
#define NULL 0
#endif

/* severity values */
#define WARNING 1
#define ERROR	2
#define FATAL	3


/* add to the list of recognizers for processing purposes */
/* (see list of names in output.c) */
#define tlex_RESWD 8
#define tlex_GLOBAL 9
#define tlex_ERRHAND 10


struct symbol {
	char *s;
	int n;
};


/* A struct line value is created for each non-trivial line of the .tlx
	an enum linetype value distinguishes the lines:
*/
enum linetype {
	Hdr, 	/* "tokenclass" */
	Elt, 	/* type var value */
	C, 	/* a C code line */
	Thong	/* value stored for a thong in the Action array */
};
struct line {
	int lineno;		/* linenumber */
	struct line *next;	/* if in a linked list of lines */
	enum linetype type;
	union {
		/* Hdr */
		struct {
			struct line *decls;
			struct line *Ccode;
			int tokennumber;
			char subtoken;		/* disambiguating letter */
			int recognizer; 	/* tlex_XXXX */
			char *structname;	/* gensym */
				/* for defaults: lineno will be zero,
				   decls and structname will be NULL */
			int action;	/* flag & value for actions array */
		} h;
		/* Elt */
		struct {
			char *type;
			char *var;
			char *val;
			int toknum;	/* for action variables */
		} d;
		/* C */
		struct {
			char *text;	   /* line with newline */
		} c;
		/* Thong - dummy for Action array */
		struct {
			char *thong;
			struct line *hdr;
			int index;		/* location in output array */
		} g;
	} u;
};


/* gentlex.c */
extern struct line *Classes;		/* list of all Hdr lines */
extern struct line *Actions[256];	/* action for each char
						may be Hdr or Thong line */
extern char *Prefix;			/* prefix identifiers */
extern char *tlxFile;			/* name of .tlx file */
extern boolean linenos;			/* TRUE if #line lines are output */
extern struct line *GlobalHandler;
extern struct line *ErrorHandler;
extern struct line *ResWordHandler;
extern int MaxSeverity;   /* maximum severity passed to Error() */
extern int CurrSym;	  /* integer portion of next symbol from GenSym() */
extern int LineNo;	  /* current input file line number */
void Error();
void ErrorA();
char *GenSym();
char *freeze();
char *Escapify();

/* readtabc.c */
/* 'TokenNames' has all the token names parsed from ftabc
		each name is terminated with \0
	numtokens is the number of tokens (YYNTOKENS)
	Token[i] is a pointer to the i'th token name in TokenNames
*/
extern char *TokenNames;
extern int numtokens;
extern char **Token;
int TransEscape();
void ReadTabc();
char *ScanToken();
int ScanTokenToNumber();

/* readtlx.c */
void ReadTlx();

/* defaults.c */
extern struct line *ResWords;
void ComputeDefaults();

/* output.c */
void WriteTlc();

/* thongs.c */
struct line *ThongAdd();
void ThongReplaceNulls();
void ThongOut();
int ThongAction();

/* charset.c */
char *CharsetParse();
char *CharsetValue();
void CharsetOutputArrays();

