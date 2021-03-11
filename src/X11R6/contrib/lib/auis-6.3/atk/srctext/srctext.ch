/* File srctext.ch created by R L Quinn
   
   srctext.ch: Text subclass specialized for dealing with source code text views. */
/* Copyright1988, 1994 Carnegie Mellon University and IBM. All rights reserved.
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

#include <ctype.h>

/* for keyword hash-table lookup */
#define TABLESIZE 53
#define HASH(word) ((*(word)+word[strlen(word)-1]*strlen(word))%TABLESIZE)

/* arbitrary number for how many Tab Stops can be set */
#define MAX_TABSTOPS 80

/* kinds of styles that are used in hash table (other style numbers are defined in subclasses) */
#define MAX_STYLES 16
#define UPRCSE 0            /* a null style array entry for uppercase words */
                            /* values from 1 to MAX_STYLES-2 can be used for subclass-specific styles */
#define USRDEF MAX_STYLES-1 /* user-defined keywords */

typedef struct DictStruct {
    char    *stng;
    short   val;    /*bits contain indentation info*/
    short   kind;    /*used for multiple style types*/
    struct DictStruct *next;
} Dict;

class srctext: text {

  overrides:
    AlwaysInsertFile(FILE * file,char *filename,long position) returns long;
    Clear();
    GetLineForPos(long pos) returns long;
    GetModified() returns long;
    GetPosForLine(long line) returns long;
    InsertCharacters(long pos, char *str, long len) returns boolean; /*RSK91overstrike*/
    Read(FILE *file, long id) returns long;
    ReadTemplate(char *templateName, boolean inserttemplatetext) returns long;
    SetAttributes(struct attributes *atts);
    Write(FILE *file, long writeID, int level) returns long;

  methods:
    /* Polymorphism Enablers */
    BackwardCheckWord(long from,long to);
    CheckComment(long start) returns long;
    CheckLinecomment(long start) returns long;
    CheckString(long start) returns long;
    CheckWord(long i,long end) returns long;
    InCommentStart(long pos) returns long;
    Indentation(long pos) returns int;
    IsTokenChar(char ch) returns boolean;
    Keywordify(char *buff, boolean checkforceupper) returns char *;
    RedoStyles();

    /* Normal Methods */
    BackwardCheckLabel(long pos);
    BackwardCopyWord(long from,long to,char buffer[]) returns long;
    BackwardSkipString(long pos,char delim) returns long;
    Breakable(long pos) returns boolean;
    BreakLine(struct mark *endofline);
    CheckLineLengths(int maxlen,struct view *view);
    CopyWord(long pos,long end,char buffer[]) returns long;
    CurrentIndent(long pos) returns int;
    CurrentColumn(long pos) returns int;
    DoMatch(long pos,char *str,int len) returns boolean;
    ExtendToOutdent(int indent,long *pos,long *len) returns boolean;
    FindSrcInsets();
    GetEnvironment(long pos) returns struct environment *;
    GetStyle(long pos) returns struct style *;
    GetToken(long *pos, long *len);
    Indent(struct mark *range) returns long; /* override this if subclass does stuff to comments that are on lines with code (plxtext) */
    InString(long pos) returns boolean;
    JustInsertCharacters(long pos,char *str,long len) returns boolean; /*RSK91overstrike*/
    LinesTooLong(int maxlen) returns boolean;
    NextTabStop(int curcol) returns int;
    OutputSrcInset(FILE *file,long writeID,struct dataobject *inset) returns boolean;
    OverstrikeAChar(long pos); /*RSK91overstrike*/
    Quoted(long pos) returns boolean; /* override this if subclass can have backslash-quoted characters (like \" in C) */
    ReflowComment(long pos) returns boolean; /* override this if subclass' comments are column-sensitive or something (plxtext) */
    ReindentLine(long pos); /* override this if subclass pads end-of-comments to right margin (plxtext) */
    RemoveStyles();
    ReverseBalance(long pos) returns long; /* override this if subclass can have nested comments (modtext) */
    SetWriteCallbacks(procedure *pre,procedure *post);
    SetupStyles();
    SkipWhitespace(long pos,long end) returns long;
    TabAndOptimizeWS(long pos, int inc) returns long;
    TranslateSrcInset(long pos) returns long;
    WrapStyle(long pos,long len,struct style *style,boolean begflag,boolean endflag);
    WrapStyleNow(long pos,long len,struct style *style,boolean begflag,boolean endflag);

  classprocedures:
    InitializeObject(struct srctext *self) returns boolean;
    FinalizeObject(struct srctext *self);
    BuildTable(char *classname,Dict *hashTable[],Dict wordlist[]);
    HashInsert(Dict *hashTable[], Dict *word);
    Lookup(Dict hashTable, char *word) returns Dict *;

  macromethods:
    GetForceUpper() ((self)->forceUpper)
    SetForceUpper( boolean newValue ) (((self)->forceUpper) = (newValue))
    GetMaxLineLength() ((self)->maxlinelength)
    SetMaxLineLength(long newValue) (((self)->maxlinelength)= (newValue))
    IndentingEnabled() ((self)->indentingEnabled)
    IsInOverstrikeMode() ((self)->OverstrikeMode) /*RSK91overstrike*/
    ChangeOverstrikeMode(boolean newValue) (((self)->OverstrikeMode) = (newValue)) /*RSK91overstrike*/

  data:
    Dict **words;
    boolean forceUpper, useTabs, reindentComments, indentingEnabled;

    /* Generic styles. Specific styles go into kindStyle array. */
    struct style *comment_style, *linecomment_style, *function_style, *label_style, *string_style;
    struct style *kindStyle[MAX_STYLES];

    /* Variables used to control the indenting style. */
    int commentIndent, contIndent, levelIndent;
    int tabSize, numTabStops, tabStop[MAX_TABSTOPS];

    /* Variables used by Esc-1 and Esc-2 comment insertion */
    char *commentString, *linecommentString;
    int commentCol, linecommentCol;
    boolean commentFixed, linecommentFixed;
    int remarkPadding;

    /* Proc callback pointers. */
    procedure *preWriteFctn, *postWriteFctn;

    /* "Temporary" stuff. maxlinelength stuff should go away as soon as programmers stop using punch cards (and fixed-length records) for their source code. RSK91overstrike stuff would be better implemented in ATK's text(VIEW) object. */
    long maxlinelength;
    boolean OverstrikeMode; /*RSK91overstrike*/ /*ideally, this should be associated with the view; unfortunately, the view is not known to srctext_InsertCharacters, which is where the overstriking is being done. But doing it in _InsertCharacters was a mistake *anyway*, so...*/
};

static boolean is_whitespace(ch)
char ch;
{ return (ch==' ')||(ch=='\t'); }

static char *makeupper(str)
char *str;
{
    char *st;
    st=str;
    while(*str!=0) {
	if(islower(*str)) *str=toupper(*str);
	str++;
    }
    return st;
}
