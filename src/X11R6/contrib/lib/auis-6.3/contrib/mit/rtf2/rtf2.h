/*
 rtf2.h of 

 Rtf2: a facility to convert RTF files to files compatible with the ATK format.

 Rtf2 is copyright (c) 1991 by the Massachusetts Institute of 
 Technology.

 RTF is a product of the Microsoft Corporation.

 Permission to use, copy, modify, and distribute this software and
 its documentation for any purpose and without fee is hereby granted,
 provided that the above copyright notice and the name of the author(s)
 appear in all copies; that both that copyright notice, the name of
 the author(s) and this permission notice appear in supporting
 documentation; and that the name of the Massachusetts Institute of
 Technology not be used in advertising or publicity pertaining to
 distribution of the software without specific, written prior
 permission.  The Massachusetts Institute of Technology makes no
 representations about the suitability of this software for any purpose.
 It is provided "as is" without express or implied warranty.
 
 Rtf2 was written entirely by Jeremy Paul Kirby, jpkirby@ATHENA.MIT.EDU

 $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/rtf2/RCS/rtf2.h,v 1.3 1994/04/22 17:41:16 rr2b Exp $
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


/* PROGRAM DEFINITIONS */
#define VERSION "1.1"

/* CHARACTER SET DEFINITIONS */
#define ANSI 0
#define MAC 1
#define PC 2


/* CHARACTER FLAGS FOR RTF2 COMMANDS IN TRANSFILE  */
#define COMMAND_LIST "@!#$^"

#define TRUE 1
#define FALSE 0
#define TMP_SIZE 255

/* Functions need to have a return value that isn't the same as tofind for any
 * invocation of ParseText().  0 will do nicely. */
#define CONTINUE 0


/* VALUES OF tofind for ParseText() */
/*   Must be greater than 255, save EOF   */
#define POP_JOB 4297

/* FOR USE AS BIT SETTINGS FOR mode: */
#define WORD 0
#define COMMAND 1
#define RTFCOMMAND 0
#define EZCOMMAND 2
#define PARAMETERIZED 0
#define NAKED 4
#define USEDELS 0
#define SUPPRESSDELS 8
#define ANYTHINGELSE 0
#define QUOTEDCHAR 16

/* SEARCH PATH FOR FindNode()   */
#define RTFCOLUMN 1
#define EZCOLUMN 2


/* VALUES FOR transform IN ParseText()  */
#define CAPS 2
#define LOWERCASE 1
#define NORMAL 0


/* ACTION CODES FOR ParseText()  */
#define PRINTTOFILE 0
#define PRINTTOSTRING 1  /* RESERVED FOR FUTURE USE */
#define NOP 2

#define DEFAULT 0
#define ANDY 1
#define ANDYSANS 2
#define ANDYTYPE 3
#define ANDYSYMBOL 4
#define NOFONT 255

#define ATTRIB_SIZE 18
#define FUNCTION_SIZE 24

typedef struct TableStruct
{
  char *rtfword;
  int mode;
  union
    {
      char *word;
      int (*fun)();
      char quote;
    } ez;
  struct TableStruct *next;
} *TABLE;

typedef struct ValuesStruct
{
  char *name;
  char *value;
  struct ValuesStruct *next;
} *VALUES;

typedef struct FileStackStruct
{
  FILE *ptr;
  long int line;
  struct FileStackStruct *next;
} *FILESTACK;

typedef struct FontStruct
{
  int number;
  int ind;
  struct FontStruct *next;
} *FONT;

struct func_words
{
   char *word;
   int (*fname)();
};

typedef int (*FP)();

char *me, *RTFchars;

int Token, MasterToken, errno, TextDSVersion;
int PopFile(), RTFverceiling, CharSet, Levels, FontSize;
int flag, left_indented, right_indented, fnote, tabs;
extern double LeftMargin, RightMargin;
long int CurrLine;

FILE *fin, *fout, *ftrans, *ferr;

void PushFile();

