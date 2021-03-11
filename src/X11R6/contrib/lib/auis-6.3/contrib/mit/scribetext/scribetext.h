/*
 scribetext.h of 

 Scribetext: a facility to convert Scribe manuscript files to files 
 compatible with the ATK format.

 Scribetext is copyright (c) 1989, 1990 by the Massachusetts Institute of 
 Technology.

 Scribe is a registered trademark of Scribe Systems, Inc.

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
 
 Scribetext was written entirely by Jeremy Paul Kirby, jpkirby@ATHENA.MIT.EDU

 $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/scribetext/RCS/scribetext.h,v 1.3 1993/12/07 02:13:10 rr2b Exp $
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
#define VERSION "1.2Alpha"
#define SCRIBESITELOCATION "/usr/lib/scribe.sit"

/* CHARACTER FLAGS FOR SCRIBETEXT COMMANDS IN TRANSFILE  */
#define COMMAND_LIST "@!#$^"

#define TRUE 1
#define FALSE 0
#define TMP_SIZE 255


/* VALUES OF tofind for ParseText() */
/*   Must be greater than 255, save EOF   */
#define POP_JOB 4297

/* FOR USE AS BIT SETTINGS FOR mode: */
#define WORD 0
#define COMMAND 1
#define SCRIBECOMMAND 0
#define EZCOMMAND 2
#define PARAMETERIZED 0
#define NAKED 4
#define USEDELS 0
#define SUPPRESSDELS 8
#define ANYTHINGELSE 0
#define QUOTEDCHAR 16

/* SEARCH PATH FOR FindNode()   */
#define SCRIBECOLUMN 1
#define EZCOLUMN 2


/* VALUES FOR transform IN ParseText()  */
#define GREEK 3  /* RESERVED FOR FUTURE USE */
#define CAPS 2
#define LOWERCASE 1
#define NORMAL 0


/* ACTION CODES FOR ParseText()  */
#define PRINTTOFILE 0
#define PRINTTOSTRING 1  /* RESERVED FOR FUTURE USE */
#define NOP 2


typedef struct TableStruct
{
  char *scribeword;
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


typedef int (*FP)();

char *me, *Scribechars, *Scribeopendelimiters, *Scribeclosedelimiters;

int Token, MasterToken, errno, verbatim, TextDSVersion, PopFile();
long int CurrLine;

FILE *fin, *fout, *ftrans, *ferr;

void PushFile();
