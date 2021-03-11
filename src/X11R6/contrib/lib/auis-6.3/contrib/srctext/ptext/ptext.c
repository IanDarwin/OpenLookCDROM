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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/srctext/ptext/RCS/ptext.c,v 1.8 1993/01/08 16:36:06 rr2b R6tape $";
#endif


 

/*
 * ptext, a Pascal mode for ATK
 *
 * Originally taken from C-Text by Miles Bader
 *  Made into a MODULA-2 editor by Rob Ryan, Chris Newman
 * Then completely re-done to work for Pascal by Curt McDowell
 */

#include <class.h>
#include <andrewos.h>
#include <setjmp.h>
#include <ctype.h>

#include <envrment.ih>
#include <stylesht.ih>
#include <style.ih>
#include <attribs.h>
#include <nstdmark.ih>
#include <tree23.ih>
#include <ptext.eh>

static boolean isident(c)
char c;
{
    return (isalnum(c) || c == '_');
}

#define	TOUPPER(c) (islower(c) ? toupper(c) : (c))
#define	TOLOWER(c) (isupper(c) ? tolower(c) : (c))

struct keywd {
    char    *s;
    short   val;
};

#define new(type) (type *) malloc(sizeof(type))

#define NONE 0
#define SINGLE '\''
#define DOUBLE '\"'

#define PARENCOMMENT 0
#define BRACECOMMENT 1

#define ID_INDENT       (1 << 0)
#define ID_BEGIN        (1 << 1)
#define ID_END          (1 << 2)
#define ID_DO           (1 << 3)
#define ID_LEAVE        (1 << 4)

/*
 * Variables used to control the indenting style.
 */

static boolean useTabs = FALSE;
static int levelIndent=4,
    contIndent=2,
    caseLevelIndent=4,
    idStyle=idstyle_CAPIT;

static struct keywd words[]={
    { "abort", 0 },
    { "abs", 0 },
    { "adr", 0 },
    { "ads", 0 },
    { "and", 0 },
    { "arctan", 0 },
    { "array", 0 },
    { "assign", 0 },
    { "begin", ID_LEAVE | ID_BEGIN | ID_INDENT },
    { "boolean", 0 },
    { "break", 0 },
    { "byword", 0 },
    { "case", 0 },
    { "char", 0 },
    { "chr", 0 },
    { "close", 0 },
    { "concat", 0 },
    { "const", ID_LEAVE | ID_INDENT },
    { "copylst", 0 },
    { "copystr", 0 },
    { "cos", 0 },
    { "cycle", 0 },
    { "decode", 0 },
    { "delete", 0 },
    { "discard", 0 },
    { "dispose", 0 },
    { "div", 0 },
    { "do", ID_DO },
    { "downto", 0 },
    { "else", ID_DO },
    { "encode", 0 },
    { "end", ID_END },
    { "eol", 0 },
    { "eoln", 0 },
    { "eval", 0 },
    { "exit", 0 },
    { "exp", 0 },
    { "extern", 0 },
    { "external", 0 },
    { "false", 0 },
    { "file", 0 },
    { "filemodes", 0 },
    { "float", 0 },
    { "for", 0 },
    { "forward", 0 },
    { "function", ID_LEAVE },
    { "get", 0 },
    { "goto", 0 },
    { "halt", 0 },
    { "hibyte", 0 },
    { "if", 0 },
    { "implementation", 0 },
    { "in", 0 },
    { "input", 0 },
    { "insert", 0 },
    { "integer", 0 },
    { "interface", 0 },
    { "label", 0 },
    { "ln", 0 },
    { "lobyte", 0 },
    { "lower", 0 },
    { "lstring", 0 },
    { "maxint", 0 },
    { "maxword", 0 },
    { "mod", 0 },
    { "module", 0 },
    { "new", 0 },
    { "nil", 0 },
    { "not", 0 },
    { "null", 0 },
    { "odd", 0 },
    { "of", ID_BEGIN | ID_INDENT },
    { "or", 0 },
    { "ord", 0 },
    { "otherwise", 0 },
    { "output", 0 },
    { "pack", 0 },
    { "packed", 0 },
    { "positn", 0 },
    { "pred", 0 },
    { "procedure", ID_LEAVE },
    { "program", ID_LEAVE },
    { "public", 0 },
    { "pure", 0 },
    { "put", 0 },
    { "read", 0 },
    { "readfn", 0 },
    { "readln", 0 },
    { "readonly", 0 },
    { "readset", 0 },
    { "real", 0 },
    { "record", ID_BEGIN | ID_INDENT },
    { "repeat", ID_BEGIN | ID_INDENT },
    { "reset", 0 },
    { "result", 0 },
    { "return", 0 },
    { "rewrite", 0 },
    { "round", 0 },
    { "scaneq", 0 },
    { "scanne", 0 },
    { "sequential", 0 },
    { "set", 0 },
    { "sin", 0 },
    { "sizeof", 0 },
    { "sqr", 0 },
    { "sqrt", 0 },
    { "static", 0 },
    { "string", 0 },
    { "succ", 0 },
    { "super", 0 },
    { "terminal", 0 },
    { "text", 0 },
    { "then", ID_DO },
    { "to", 0 },
    { "true", 0 },
    { "trunc", 0 },
    { "type", ID_LEAVE | ID_INDENT },
    { "unit", 0 },
    { "unpack", 0 },
    { "until", ID_END },
    { "upper", 0 },
    { "uses", 0 },
    { "val", 0 },
    { "value", 0 },
    { "var", ID_LEAVE | ID_INDENT },
    { "vars", 0 },
    { "while", 0 },
    { "with", 0 },
    { "word", 0 },
    { "wrd", 0 },
    { "write", 0 },
    { "writeln", 0 },
    { "xor", 0 },
    { NULL, 0 }
};

static void stylizekeyword();

static int is_whitespace(ch)
char ch;
{
    return ((ch==' ')||(ch=='\t'));
}

static long skipwhitespace(ct,pos,end)
struct ptext *ct;
long pos,end;
{
    while ((pos<end)&&is_whitespace(ptext_GetChar(ct,pos)))
        pos++;
    return pos;
}

static void casify(s, style)
char *s;
int style;
{
    if (style == idstyle_PLAIN || *s == '\0')
        return;
    if (style == idstyle_LOWER)
        *s = TOLOWER(*s);
    else
        *s = TOUPPER(*s);
    for (s++; *s != '\0'; s++)
        if (style == idstyle_UPPER)
            *s = TOUPPER(*s);
        else
            *s = TOLOWER(*s);
}

/* copyWord: 'pos' points at the first char or somewhere in front
 * of the identifier; copies from the beginning of the identifier
 * to the end into the buffer, returning pos of 1 past last char in id */

static long copyWord(ct,pos,end,buffer)
struct ptext *ct;
long pos,end;
char buffer[];
{
    char ch;
    int count=0;
    buffer[0]=0;
    pos=skipwhitespace(ct,pos,end);
    while((count<=255)&&(pos<end)&& isident(ch=ptext_GetChar(ct,pos++)))
        buffer[count++]=ch;
    buffer[count]=0;
    return pos;
}

/* backCopyWord: 'from' points to the last character in an identifier,
 * to some position before that to greater than or equal to 'to'.
 * Copies the whole identifier into
 * the buffer and returns the position of the first char in the id. */

static long backCopyWord(pt, from, to, buffer)
struct ptext *pt;
long from,to;
char buffer[];
{
    int count=0,i=0,j;
    buffer[0]=0;
    while ((from-i>=to)&& isident(ptext_GetChar(pt,from-i)))
        i++;
    j=from-i;
    if (from-i>=to-1)
        for(;i>0 && count<255;i--,count++)
            buffer[count]=ptext_GetChar(pt,from-i+1);
    buffer[count]=0;
    return j;
}

/* Returns NULL if not a keyword */

static struct keywd *lookupKeyword(dict, word)
struct keywd *dict;
char *word;
{
    struct keywd *k;

    for (k = words; k->s != NULL; k++) {
        char *s = k->s, *t = word;
        for (; *s != '\0' && *t != '\0'; s++, t++)
            if (TOLOWER(*t) != *s)
                break;
        if (*s == '\0' && *t == '\0')
            return k;
    }

    return NULL;
}

struct environment *ptext__GetEnvironment(self,pos)
struct ptext *self;
long pos;
{
    struct environment *me;
    if (me=self->header.text.rootEnvironment)
        me=environment_GetEnclosing(me,pos);
    return me;
}

struct style *ptext__GetStyle(self,pos)
struct ptext *self;
long pos;
{
    struct environment *me;
    if (me=ptext_GetEnvironment(self,pos))
        return me->data.style;
    return NULL;
}

void ptext__InsertNewStyle(self,pos,len,style,begflag,endflag)
struct ptext *self;
long pos,len;
struct style *style;
boolean begflag,endflag;
{
    struct environment *newenv;
    if (style!= NULL) {
	newenv=environment_InsertStyle(self->header.text.rootEnvironment, pos, style, 1);
	environment_SetLength(newenv, len);
	environment_SetStyle(newenv,begflag,endflag);
    }
}

/* assumes that pos points to 1 BEFORE ending delimiter */
static long backwardSkipString(self,pos,delim)
struct ptext *self;
long pos;
char delim;
{
    while ((pos>=0)&& ptext_GetChar(self,pos--)!=delim)
        ;
    return pos;
}
static boolean InString(self,pos)
struct ptext *self;
long pos;
{
    char c,lastquote=0;
    boolean quotes=FALSE;
    while((pos>=0)&& (c=ptext_GetChar(self,pos--))!='\n') {
	if(c=='\'' || c=='\"') {
	    if(c==lastquote) {
		quotes=FALSE;
		lastquote=0;
	    } else if (!quotes) {
		quotes=TRUE;
		lastquote=c;
	    }
	}
    }
    return quotes;
}

static long backwardcheckword(self,from,to)
struct ptext *self;
long from,to;
{
    struct environment *me;
    char buf1[256], buf2[256];
    long j = from;
    if ((!InString(self, from)) &&
         isident(ptext_GetChar(self,from)) &&
         (((me = ptext_GetEnvironment(self, from)) ==
            self->header.text.rootEnvironment) ||
            (me->data.style==self->keyword_style))) {
	j = backCopyWord(self, from, to, buf1);
	if (lookupKeyword(words, buf1) != NULL) {
	    int l;
	    l = strlen(buf1);
            strcpy(buf2, buf1);
            casify(buf2, idStyle);
            if (strcmp(buf1, buf2))
                ptext_ReplaceCharacters(self, j + 1, l, buf2, l);
	    stylizekeyword(self, j + 1, l);
	}
    }
    return j;
}

void ptext__BackwardCheckWord(self,from,to)
struct ptext *self;
long from,to;
{
    (void) backwardcheckword(self,from,to);
}

static long checkword(self,i,end)
struct ptext *self;
long i,end;
{
    int j;
    struct keywd *word;
    char buf1[256], buf2[256];
    i = skipwhitespace(self, i, end);
    j = copyWord(self, i, end, buf1);
    word = lookupKeyword(words, buf1);
    if (word != NULL) {
	int l = strlen(word->s);
        strcpy(buf2, buf1);
        casify(buf2, idStyle);
        if (strcmp(buf1, buf2))
            ptext_ReplaceCharacters(self, i, l, buf2, l);
	stylizekeyword(self, i, l);
    }
    return j;
}

void ptext__SetAttributes(self,atts)
struct ptext	*self;
struct attributes   *atts;
{
    super_SetAttributes(self,atts);

    while(atts!=NULL)
    {
	if(strncmp(atts->key,"ptext-",6)==0)
	{
	    char    *key=atts->key+6;

	    if(strcmp(key,"level-indent")==0)
		levelIndent=atoi(atts->value.string);
	    else if(strcmp(key,"cont-indent")==0)
		contIndent=atoi(atts->value.string);
	    else if(strcmp(key,"case-level-indent")==0)
		caseLevelIndent=atoi(atts->value.string);
	    else if(strcmp(key,"use-tabs")==0)
		useTabs=atoi(atts->value.string);
            else if(strcmp(key,"id-style")==0) {
                if (strcmp(atts->value.string, "upper")==0)
                    idStyle = idstyle_UPPER;
                else if (strcmp(atts->value.string, "lower")==0)
                    idStyle = idstyle_LOWER;
                else if (strcmp(atts->value.string, "capit")==0)
                    idStyle = idstyle_CAPIT;
                else
                    idStyle = idstyle_PLAIN;
            }
	}    

	atts=atts->next;
    }
}

static void SetupStyles(self)
struct ptext *self;
{
    self->comment_style = stylesheet_Find(self->header.text.styleSheet, "comment");
    self->keyword_style = stylesheet_Find(self->header.text.styleSheet, "keyword");
    ptext_SetGlobalStyle(self, stylesheet_Find(self->header.text.styleSheet, "global"));
}

boolean ptext__InitializeObject(classID, self)
struct classheader *classID;
struct ptext *self;
{
    struct attributes templateAttribute;

    templateAttribute.key = "template";
    templateAttribute.value.string = "ptext";
    templateAttribute.next = NULL;
    ptext_SetAttributes(self, &templateAttribute);
    ptext_SetExportEnvironments(self, FALSE);

    SetupStyles(self);
    return TRUE;
}

void ptext__Clear(self)
struct ptext *self;
{
    super_Clear(self);      /* This destroyes all styles in the stylesht. */

    SetupStyles(self);
}

long ptext__Read(self, file, id)
struct ptext *self;
FILE *file;
long id;
{
    long tmpRetValue;

    tmpRetValue = super_Read(self, file, id);
    if (tmpRetValue == dataobject_NOREADERROR) {
	SetupStyles(self);
	ptext_RedoStyles(self);
    }
    return tmpRetValue;
}

/****************************************************************
  *
  *  Style parsing code.
  *
  ****************************************************************/

#ifdef NOTUSED
static long skipstring(self,start)
struct ptext *self;
long start;
{
    long len=ptext_GetLength(self);
    char quote=ptext_GetChar(self,start);
    while((++start<=len)&&(ptext_GetChar(self,start)!=quote));
    return start;
}
#endif /* NOTUSED */

/* comment returns the last character of the (possibly nested) comment.
 * The start character should be the '*' in a PARENCOMMENT or the
 * first character in the comment for a BRACECOMMENT. */

static long comment(self, start, comtype)
struct ptext *self;
long start;
int comtype;
{
    int end, len = ptext_GetLength(self);
    char prev, c = 0;

    for (end = start; end < len; end++) {
	prev = c;
	c = ptext_GetChar(self, end);
        if (c == '*' && prev == '(' && comtype == PARENCOMMENT)
            end = comment(self, end, comtype);
        else if (c == ')' && prev == '*' && comtype == PARENCOMMENT)
            break;
        else if (c == '{' && comtype == BRACECOMMENT)
            end = comment(self, end + 1, comtype);
        else if (c == '}' && comtype == BRACECOMMENT)
            break;
    }
    return end;
}

/* backComment returns the first character (paren or brace) of the (possibly nested)
 * comment.  The start character should be the '*' in a PARENCOMMENT
 * or the last character in the comment for a BRACECOMMENT. */

static long backComment(self, end, comtype)
struct ptext *self;
long end;
int comtype;
{
    int start;
    char past, c = 0;

    for (start = end; start > 0; start--) {
	past = c;
	c = ptext_GetChar(self, start);
        if (c == '*' && past == ')' && comtype == PARENCOMMENT)
            start = backComment(self, start, comtype);
        else if (c == '(' && past == '*' && comtype == PARENCOMMENT)
            break;
        else if (c == '}' && comtype == BRACECOMMENT)
            start = backComment(self, start - 1, comtype);
        else if (c == '{' && comtype == BRACECOMMENT)
            break;
    }
    return start;
}

static void stylizekeyword(self, posn, len)
struct ptext *self;
long posn, len;
{
    if (self->keyword_style == NULL)
        return;
    if (ptext_GetStyle(self, posn) == self->keyword_style)
        return;
    environment_Remove(self->header.text.rootEnvironment,
        posn, len, environment_Style, FALSE);
    ptext_InsertNewStyle(self, posn, len,
        self->keyword_style, FALSE, FALSE);
    ptext_SetModified(self);
}

/* returns the length of a *constant* string */

#define cstrlen(str) (sizeof(str)-1)

/* pos should pointer to 2nd from last character-- last character won't be
* matched; strconst *must* be a constant string, so that sizeof works
*/
#define backwardMatch(self,pos,strConst) \
(pos>=cstrlen(strConst) && match(self,pos-(cstrlen(strConst)-1)+1,strConst,cstrlen(strConst)))

#define match(self,pos,str,len) ((pos==0 || !isident(ptext_GetChar(self,pos-1))) && !isident(ptext_GetChar(self,pos+len)) && domatch(self,pos,str,len))

static domatch(self,pos,str,len)
struct ptext *self;
int pos;
char *str;
int len;
{
    while(len>0 && ptext_GetChar(self,pos++)==*str++)
	len--;

    return len==0;
}

static void DoFreeTree(self)
struct nestedmark *self;
{
    nestedmark_FreeTree(self);
}

void ptext__RedoStyles(self)
struct ptext *self;
{
    struct nestedmark *root = (struct nestedmark *)self->header.text.rootEnvironment;
    long posn, eposn, len = ptext_GetLength(self);
    int prev = 0, c = '\n', string = NONE;
    /* c is initialize to a newline so the start of the file looks like the start of line. */

    /* Remove the old styles, but leave the root environment in place. */
    if (root->children) {
	tree23int_Apply(root->children, DoFreeTree);
	tree23int_Destroy(root->children);
	root->children = NULL;
    }
    for (posn = 0; posn < (len=ptext_GetLength(self)); posn++) {
	prev = c;
	posn=skipwhitespace(self,posn,len);
	c = ptext_GetChar(self, posn);
	    switch (c) {
		case '\n':
		    break;
		case '*':
		    if (prev == '(' && string == NONE) {
			eposn = comment(self, posn, PARENCOMMENT);
                        if (self->comment_style != NULL)
                            ptext_InsertNewStyle(self, posn - 1, eposn - posn,
                                 self->comment_style, TRUE, TRUE);
                        posn = eposn;
                    }
		    break;
		case '{':
		    if (string == NONE) {
			eposn = comment(self, posn + 1, BRACECOMMENT);
                        if (self->comment_style != NULL)
                            ptext_InsertNewStyle(self, posn - 1,
                                 eposn - posn + 2,
                                 self->comment_style, TRUE, TRUE);
                        posn = eposn;
                    }
		    break;
		case '\'':
		case '\"':
		    if (string == NONE)
			if (c == '\"')
			    string = DOUBLE;
			else
			    string = SINGLE;
		    else
                        if ((string == DOUBLE && c == '\"')
                          || (string == SINGLE && c == '\''))
                            string = NONE;
		    break;
		default:
		    if (string==NONE)
                        posn = checkword(self, posn, len) - 1;
		    break;
	    }
    }
}

/****************************************************************
  *
  * Paren balancer.
  *
  ****************************************************************/


struct paren_node {
    long type;
    struct paren_node *next;
};


static boolean Quoted(self, pos)
struct ptext *self;
long pos;
{
    /* returns true iff the character at pos is quoted (ie. "\"). Takes into account the slash being quoted. (ie "\\"). */

    boolean retval = FALSE;

    pos--;
    while (ptext_GetChar(self, pos) == '\\' && pos > 0) {
	retval = !retval;
	pos--;
    }

    return FALSE;
}

long ptext__ReverseBalance(self, pos)
struct ptext *self;
long pos;
{
    boolean found = FALSE, incomment = FALSE, instring = FALSE, doublestring = FALSE, atleastone = FALSE;
    int thischar, prechar;
    char *parentype;
    struct paren_node *parenstack = NULL/*, *tmp */;
    static char *opens = "({[", *closes = ")}]";


    while ((parenstack != NULL || !atleastone) && (pos > 0)) {
	thischar = ptext_GetChar(self, --pos);
	prechar = pos > 0 ? ptext_GetChar(self, pos - 1) : 0;

	if (incomment) {
	    if(thischar == '*' && prechar == '(') {
		incomment = FALSE;
		if (parenstack == NULL) found = TRUE;
		else pos= pos > 1 ? pos -= 2 : 0;
	    }
	}
	else if (!Quoted(self, pos)) {
	    if (instring) {
		if ((thischar == '"' && doublestring) || (thischar == '\'' && !doublestring)) {
		    instring = FALSE;
		}
	    }
	    else if (thischar == '"') {
		instring = TRUE;
		doublestring = TRUE;
	    }
	    else if (thischar == '\'') {
		instring = TRUE;
		doublestring = FALSE;
	    }
	    else if (thischar == ')' && prechar == '*') {
		incomment = TRUE;
	    }
	    else if ((parentype = index(opens, thischar)) != NULL) {
		if (parenstack == NULL || parenstack->type != (parentype - opens)) {
		    break;
		}
		else {
		    struct paren_node *temp = parenstack;

		    parenstack = parenstack->next;
		    free(temp);
		    if ((prechar == '\n' || pos == '0') && parenstack != NULL) {
			break;
		    }
		    else if (parenstack == NULL) {
			found = TRUE;
			break;
		    }
		}
	    }
	    else if ((parentype = index(closes, thischar)) != NULL) {
		struct paren_node *temp = new(struct paren_node);

		temp->type = parentype - closes;
		temp->next = parenstack;
		parenstack = temp;
	    }
	}
    }

    if (found) {
	return pos;
    }
    else {
	return EOF;

    }
}

/*****************************************************************
  * Indenter.  This is a completely new one
  * 15 July 1987 - Miles Bader
  *****************************************************************/

long ptext__Indent(self,mark)
struct ptext *self;
struct mark *mark;
{
    long end,pos=mark_GetPos(mark);
    int c;

    while(pos>=0 && ptext_GetChar(self,--pos)!='\n');

    pos++;

    do{
	pos=ptext_TabAndOptimizeWS(self,pos,indentation(self,pos));

	for(end=pos; (c=ptext_GetChar(self,end))==' ' || c=='\t'; end++ );
	if(end>pos)
	    ptext_DeleteCharacters(self,pos,end-pos);

	end=mark_GetEndPos(mark);
	while(pos<end && (c=ptext_GetChar(self,pos++))!=EOF && c!='\n');
    }while(pos<end && c!=EOF);

    return pos;
}

void ptext__ReindentLine(self, pos)
struct ptext *self;
long pos;
{
    long end;
    int c;

    while(pos>0 && ptext_GetChar(self,--pos)!='\n');

    pos++;

    pos=ptext_TabAndOptimizeWS(self,pos,indentation(self,pos));

    for(end=pos; (c=ptext_GetChar(self,end))==' ' || c=='\t'; end++);
    if(end>pos)
	ptext_DeleteCharacters(self,pos,end-pos);
}

void ptext__StyleLine(self,pos)
struct ptext *self;
long pos;
{
    long i,start;
    int string=NONE;
    char ch=0,prev;
    start=i=pos;
    while((--start>=0)&&(ch=ptext_GetChar(self,start))!='\n');
    while((--i>=start)&&(ptext_GetChar(self,i))!='\n') {
	prev=ch;
	ch=ptext_GetChar(self,i);
	switch(ch) {
	    case '*':
		if (prev== ')' && (string==NONE)) i=backComment(self,i,PARENCOMMENT)+1;
		break;
            case '}':
                if (string == NONE)
                    i = backComment(self,i-1,BRACECOMMENT);
	    case '\'':
	    case '\"':
		if(string==NONE) string=ch;
		else if(string==ch) string=NONE;
	    break;
	default:
	    if(isident(ch)&& string==NONE) i=backwardcheckword(self,i,start)+1;
	}
    }
	/* if(i!=pos) {
	if(environment_Remove(self->header.text.rootEnvironment,i, pos-i,environment_Style,TRUE)) ptext_SetModified(self);
	while(i<pos) i=checkword(self,i,pos);
	ptext_RegionModified(self,i,pos-i);
    } */
    ptext__ReindentLine(self,pos);
}

/* skipJunk moves forward and returns the position of the first character
 * which is at or beyond the position, but is not is not white space, part of
 * a (possibly nested) comment, part of a single- or double- quoted string,
 * or greater than the length of the file. */

static long skipJunk(self,pos)
struct ptext *self;
long pos;
{
    int stringtype = 0;
    long len = ptext_GetLength(self);

    while (pos < len) {
        int c = ptext_GetChar(self, pos++);
        if (stringtype && c == stringtype) {
            stringtype = 0;
            continue;
        }
        switch (c) {
            case '(':
                if (ptext_GetChar(self, pos) == '*')
                    pos = comment(self, pos, PARENCOMMENT) + 1;
                else
                    return pos - 1;
                break;
            case '{':
                pos = comment(self, pos, BRACECOMMENT) + 1;
                break;
            case ' ':
            case '\t':
            case '\n':
                break;
            case '\'':
            case '\"':
                stringtype = c;                
                break;
            default:
                return pos - 1;
        }
    }

    return len - 1;
}

/* backSkipJunk returns returns the position of the first character
 * which is at or earlier than the position, but is not white space, part of
 * a (possibly nested) comment, part of a single- or double-quoted string,
 * or less than zero. */

static long backSkipJunk(self, pos)
struct ptext *self;
long pos;
{
    int stringtype;

    while (pos > 0) {
	int c = ptext_GetChar(self, pos--);
        if (stringtype && c == stringtype) {
            stringtype = 0;
            continue;
        }
        switch (c) {
            case '\'':
            case '\"':
                stringtype = c;
                break;
	    case ')':
                if (ptext_GetChar(self, pos) == '*')
                    pos = backComment(self, pos, PARENCOMMENT) - 1;
                else
                    return pos + 1;
                break;
            case '}':
                pos = backComment(self, pos, BRACECOMMENT) - 1;
                break;
	    case '\n':
	    case '\t':
	    case ' ':
                break;
            default:
                return pos + 1;
	}
    }

    return 0;
}

/* matchBegin: takes a position of the first character in a 'end'
 * type of identifier (end, until, ...) and searches backwards.  It
 * returns the indentation of the line containing a matching 'begin'
 * type of identifier (begin, repeat, record, of, ...) */

static int matchBegin(self, pos)
struct ptext *self;
long pos;
{
    int count = 1;
    struct keywd *word;
    char buff[256];

    while (count > 0 && --pos >= 0) {
	pos = backSkipJunk(self, pos);
        if (pos == 0)
            return 0;
        if (! isident(ptext_GetChar(self, pos)))
            continue;
        pos = backCopyWord(self, pos, 0, buff);
        word = lookupKeyword(words, buff);
        if (word != NULL) {
            if (word->val & ID_END)
                count++;
            else if (word->val & ID_BEGIN)
                count--;
        }
    }

    return currentIndent(self, pos);
}

/* indentation: pos must be the first character on a line. */

static int indentation(self, pos)
struct ptext *self;
long pos;
{
    char buff[256];
    struct keywd *word;
    long i, j;
    long endline, len;

    if (ptext_GetStyle(self, pos) == self->comment_style)
	return currentColumn(self,
            backComment(self, pos, PARENCOMMENT)); 

    len = ptext_GetLength(self);
    endline = pos;
    while (ptext_GetChar(self, endline) != '\n' && endline < len)
        endline++;

    /* Find the keyword on the line, if any */
    /* It must be before any other non-white-space or comments */

    j = skipJunk(self, pos);
    if (j >= endline || ! isident(ptext_GetChar(self, j)))
        word = NULL;
    else {
        (void) copyWord(self, j, len, buff);
        word = lookupKeyword(words, buff);
        if (word != NULL && word->val == 0)
            word = NULL;
    }

    if (word != NULL) {
        /* If the first word on the line is a 'leave' type of keyword,
         * then don't bother the indentation at all. */

        if (word->val & ID_LEAVE)
            return currentIndent(self, pos);

        /* If the first word on the line is an 'end' type of keyword,
         * the indentation is determined by a matching 'begin' type. */

        if (word->val & ID_END)
            return matchBegin(self, pos);
    }

    /* In other cases, the indentation of the line is determined entirely
     * by the first preceding relevant indentation-related keyword. */

    j = pos;

    do {
        j--;
        while (j > 0 && ! isident(ptext_GetChar(self, j)))
            j = backSkipJunk(self, j - 1);
        if (j <= 0) {
            word = NULL;
            break;
        }
        j = backCopyWord(self, j, 0, buff);
        word = lookupKeyword(words, buff);
    } while (word == NULL || word->val == 0);

    if (word == NULL)
        return currentIndent(self, pos);    /* Can't do much */

    if (word->val & ID_END)
        return currentIndent(self, j);

    if (word->val & ID_INDENT)
        return currentIndent(self, j) + levelIndent;

    if (word->val & ID_DO) {
        /* See if there's no semicolon between the DO and pos */
        for (i = j; i < pos; i++)
            if (ptext_GetChar(self, i) == ';')
                break;
        if (i == pos)
            return currentIndent(self, j) + levelIndent;   /* No semi */
        return currentIndent(self, j);
    }

    /* Default: indentation of previous line */

    return (pos < 2) ? 0 : currentIndent(self, pos - 2);
}

static int currentIndent(self,pos)
struct ptext *self;
long pos;
{
    int c, ind=0;

    while(pos>=0 && (c=ptext_GetChar(self,pos))!='\n')
	pos--;

    while(isspace(c=ptext_GetChar(self,pos++)))
	switch(c){
	    case ' ':
		ind++;
		break;
	    case '\t':
		ind=(ind+8)&~7;
		break;
	}
    return ind;
}


static int currentColumn(self,pos)
struct ptext *self;
long pos;
{
    int ind=0,oldPos=pos;

    while(pos>=0 && ptext_GetChar(self,pos)!='\n')
	pos--;

    while(pos<oldPos)
	if(ptext_GetChar(self,pos++)=='\t')
	    ind=(ind+8)&~7;
	else
	    ind++;

    return ind;
}

/*
  * optimized whitespace insertion
  * 7 June 1987 - Miles Bader
  */

static char *spaces="        ";

#define TABOVER(self,oldpos,pos,oldcol,col) \
if(col>oldcol){\
ptext_DeleteCharacters(self,oldpos,pos-oldpos);\
pos=oldpos;\
if(useTabs){\
int nc;\
while(col>=(nc=(oldcol+8)&~7)){\
oldcol=nc;\
ptext_InsertCharacters(self,pos++,"\t",1);\
}\
}else{\
int count=(col-oldcol)/8;\
while(count--){\
ptext_InsertCharacters(self,pos,spaces,8);\
oldcol+=8;\
pos+=8;\
}\
}\
ptext_InsertCharacters(self,pos,spaces,col-oldcol);\
pos+=(col-oldcol);\
}

long ptext__TabAndOptimizeWS(self,pos,inc)
struct ptext    *self;
long	pos;
int	inc;
{
    int     home=0, oldPos, col=0, oldCol=0, target;

    while(--pos>0 && ptext_GetChar(self,pos)!='\n')
	home++;

    oldPos= ++pos;
    while(home--){
	switch(ptext_GetChar(self,pos)){
	    case '\t':
		col=(col+8)&~7;
		break;
	    case ' ':
		col++;
		break;
	    default:
		if(col>oldCol)
		    TABOVER(self,oldPos,pos,oldCol,col);

		oldPos=pos+1;
		oldCol= ++col;
	}
	pos++;
    }

    /* the column that we are aiming for */
    if(inc==0)
	target=col;
    else
	target=(col+inc)-(col%inc);

    /* skip over existing white space */
    while(col<target){
	int	nc;
	char	c=ptext_GetChar(self,pos);

	if(c==' '){
	    col++;
	    pos++;
	}else if(c=='\t' && (nc=(col+8)&~7)<=target){
	    col=nc;
	    pos++;
	}else
	    break;
    }

    /* add new whitespace */
    TABOVER(self,oldPos,pos,oldCol,target);

    return pos;
}
