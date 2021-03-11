/* File modtext.c created by R L Quinn
  
   modtext, a Modula-X mode for ATK. */
/* Copyright 1988, 1994 Carnegie Mellon University and IBM. All rights reserved.
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
static char rcsid[] = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/srctext/RCS/modtext.c,v 1.11 1994/02/28 21:02:02 rr2b Exp $";
#endif

#include <andrewos.h>
#include <class.h>

#include <envrment.ih>

#include <stylesht.ih>
#include <style.ih>
#include <fontdesc.ih>
#include <attribs.h>

#include "srctext.ih"
#include "modtext.eh"

boolean modtext__IsTokenChar(self,ch)    
struct modtext *self;
char ch;
{
    return isalnum(ch)||ch=='_'||ch=='|';
}

boolean modtext__IsTokenOrPeriod(self,ch)
struct modtext *self;
char ch;
{
    return modtext_IsTokenChar(self,ch)||(ch=='.');
}

void modtext__SetAttributes(self,atts)
struct modtext *self;
struct attributes *atts;
{
    super_SetAttributes(self,atts);
    while (atts!=NULL) {
	char *key=atts->key;
	if (strcmp(key,"preprocessor")==0)
	    self->preprocessor=atoi(atts->value.string);
	else if (strcmp(key,"outdent-preproc")==0)
	    self->outdentPreproc=atoi(atts->value.string);
	atts=atts->next;
    }
}

/* modtext_Keywordify makes buff all uppercase IF it wasn't mixed case to begin with AND ForceUpper is ON.  The latter condition only applies if checkforceupper is TRUE. */
/* This function "keywordifies" a string. RedoStyles says checkforceupper is FALSE, so "begin" won't ever be found in the hash table. BackwardCheckWord says checkforceupper is TRUE, so, if ForceUpper is ON, "begin" changes to "BEGIN", hence it gets "keywordified". */
char *modtext__Keywordify(self, buff, checkforceupper)
struct modtext *self;
char *buff;
boolean checkforceupper;
{
    if (buff!=NULL && strlen(buff)>0 && checkforceupper && modtext_GetForceUpper(self)) {
	char *b= buff;
	while (*b!='\0' && !isupper(*b))
	    ++b;
	if (*b=='\0')
	    makeupper(buff);
    }
    return buff;
}

/* override */
/* modtext_BackwardCheckWord calls CheckWord to wrap procedure name whenever PROCEDURE_VAL is found */
void modtext__BackwardCheckWord(self,from,to)
struct modtext *self;
long from,to;
{
    Dict *word;
    char buff[1024];
    long j=from;
    if (modtext_IsTokenChar(self, modtext_GetChar(self, from)) && !modtext_GetStyle(self, from) && !modtext_InString(self, from)) {
	j=modtext_BackwardCopyWord(self,from,to,buff);
	modtext_Keywordify(self,buff,TRUE);
	if((word=srctext_Lookup(self->header.srctext.words,buff))->stng!=NULL) {
	    int l=strlen(buff);
	    if (modtext_GetForceUpper(self))
		modtext_ReplaceCharacters(self,j+1,l, word->stng,l);
	    modtext_WrapStyleNow(self, j+1,l, self->header.srctext.kindStyle[word->kind], FALSE,FALSE);
	}
    }
    return /*j*/;
}

/* override */
/* modtext_CheckWord does the procedure-name recognizing */
long modtext__CheckWord(self,i,end)
struct modtext *self;
long i,end;
{
    long j;
    Dict *word;
    char buff[1024];
    i=modtext_SkipWhitespace(self,i,end);
    j=modtext_CopyWord(self,i,end,buff);
    modtext_Keywordify(self,buff,FALSE);
    if((word=srctext_Lookup(self->header.srctext.words,buff))->stng!=NULL) {
	modtext_WrapStyle(self, i,strlen(word->stng), self->header.srctext.kindStyle[word->kind], FALSE,FALSE);
	if (word->val==PROCEDURE_VAL) {
	    i=modtext_SkipWhitespace(self,j+1,end);
	    j=modtext_CopyWord(self,i,end,buff);/*get procedure name*/
	    modtext_Keywordify(self,buff,FALSE);
	    if ((word=srctext_Lookup(self->header.srctext.words, buff))->stng==NULL) /*legit proc name*/
		modtext_WrapStyle(self, i,strlen(buff), self->header.srctext.function_style, TRUE,TRUE);
	    else /*not a proc name!! make appropriate style*/
		modtext_WrapStyle(self, i,strlen(word->stng), self->header.srctext.kindStyle[word->kind], FALSE,FALSE);
	}
    }
    return j;
}

static void SetupStyles(self)
struct modtext *self;
{
    if ((self->header.srctext.kindStyle[KEYWRD]= stylesheet_Find(self->header.text.styleSheet, "keyword")) == NULL) {
	self->header.srctext.kindStyle[KEYWRD]= style_New();
	style_SetName(self->header.srctext.kindStyle[KEYWRD], "keyword");
	style_AddNewFontFace(self->header.srctext.kindStyle[KEYWRD], fontdesc_Bold);
	stylesheet_Add(self->header.text.styleSheet, self->header.srctext.kindStyle[KEYWRD]);
    }
}

/* only setup styles that are common to both mtext and m3text! */
void modtext__SetupStyles(self)
struct modtext *self;
{
    super_SetupStyles(self);
    SetupStyles(self);
}

boolean modtext__InitializeObject(classID, self)
struct classheader *classID;
struct modtext *self;
{
    self->header.srctext.useTabs=FALSE;
    self->header.srctext.commentIndent= 3;
    self->header.srctext.commentString= "(*  *)";
    self->header.srctext.indentingEnabled= TRUE;
    self->preprocessor= FALSE;
    self->outdentPreproc= TRUE;
    SetupStyles(self);
    return TRUE;
}

/* override */
/* CheckComment will wrap the comment style around paren-star comments, taking nesting into account. */
long modtext__CheckComment(self, start)
struct modtext *self;
long start;
{
    int prev, c=0, comments=1;
    long end=start+1, len=modtext_GetLength(self);

    while (end<len) {
	prev= c;
	c= modtext_GetChar(self, ++end);
	switch(c) {
	    case '*':
		if(prev=='(') ++comments;
		break;
	    case ')':
		if(prev=='*') --comments;
		break;
	}
	if (comments<1) {
	    modtext_WrapStyle(self, start,end-start+1, self->header.srctext.comment_style, FALSE,FALSE);
	    break;
	}
    }
    return end;
}

/* checkPreproc wraps the preproc style around a preprocessor directive */
static long checkPreproc(self, start)
struct modtext *self;
long start;
{
    long end, len=modtext_GetLength(self);
    int prev, c;
    CheckSmore: ; /* jump here after we're done handling the comment or whatever */
    for (c=0, end=start; end<len; ++end) {
	prev= c;
	switch (c=modtext_GetChar(self,end)) {
	    case '\n':
		if (!modtext_Quoted(self,end)) {
		    if (end>start)
			modtext_WrapStyle(self, start,end-start, self->header.srctext.kindStyle[PREPRC], FALSE,TRUE);
		    return end-1;
		}
		break;
	    case '*':
		if (prev=='(') {
		    if (end-1>start)
			modtext_WrapStyle(self, start,end-start-1, self->header.srctext.kindStyle[PREPRC], FALSE,TRUE);
		    start= modtext_CheckComment(self, end-1) +1;
		    goto CheckSmore; /* continue at end of comment */
		}
		break;
	}
    }
    return end-1;
}

void modtext__RedoStyles(self)
struct modtext *self;
{
    long posn, len=modtext_GetLength(self);
    int prev=0, c='\n'; /* c is initialized to a newline so the start of the file looks like the start of line. */
    modtext_RemoveStyles(self); /* Remove the old styles, but leave the root environment in place. */
    for (posn=0; posn<len-1; ++posn) {
	prev = c;
	c = modtext_GetChar(self, posn);
	switch (c) {
	    case ' ': case '\t':
		break;
	    case '\n':
		posn= modtext_SkipWhitespace(self, posn+1,len) -1;
		break;
	    case '#':
		if (prev=='\n' && self->preprocessor)
		    posn= checkPreproc(self,posn);
		break;
	    case '*':
		if (prev == '(')
		    posn= modtext_CheckComment(self, posn-1);
		break;
	    case '\'':
	    case '\"':
		posn= modtext_CheckString(self,posn);
		break;
	    default:
		if (modtext_IsTokenChar(self,c))
		    posn= modtext_CheckWord(self,posn,len);
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

#define new(type) (type *) malloc(sizeof(type))

static char *opens = "({[";
static char *closes = ")}]";
long modtext__ReverseBalance(self, pos)
struct modtext *self;
long pos;
{
    boolean found = FALSE, /*RSKmod remove "incomment = FALSE,"*/ instring = FALSE, doublestring = FALSE, atleastone = FALSE;
    int incomment=0;/*RSKadd*/
    int inpragma=0;
    int thischar, prechar;
    char *parentype;
    struct paren_node *parenstack = NULL/*, *tmp */;

    while ((parenstack != NULL || !atleastone) && (pos > 0)) {
	thischar = modtext_GetChar(self, --pos);
	prechar = pos > 0 ? modtext_GetChar(self, pos - 1) : 0;
	if (incomment) {
	    if(thischar == '*' && prechar == '(') {
		--incomment;
		--pos;
		/*RSK91mod:*/
		if (incomment<1 && parenstack==NULL) {
		    found= TRUE;
		    break;
		}
	    }
	    else if (thischar==')' && prechar=='*') /*RSKmod*/
		++incomment;
	}
	else if (inpragma) {
	    if(thischar == '*' && prechar == '<') {
		--inpragma;
		if (parenstack == NULL) found = TRUE;
		else pos= pos > 1 ? pos -= 2 : 0;
	    }
	    else if (thischar=='>' && prechar=='*') 
		++inpragma;
	}
	else if (!modtext_Quoted(self, pos)) {
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
		++incomment;
	    }
	    else if (thischar == '>' && prechar == '*') {
		++inpragma;
	    }
	    else if ((parentype = index(opens, thischar)) != NULL) {
		if (parenstack == NULL || parenstack->type != (parentype - opens)) {
		    break;
		}
		else {
		    struct paren_node *temp = parenstack;

		    parenstack = parenstack->next;
		    free(temp);
		    if ((/*prechar == '\n' || pos == '0'*/ pos==0) && parenstack != NULL) {    /*RSK90fix*/
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

static long GetStyleCount(self,pos)
struct modtext *self;
long pos;
{
    struct environment *me;
    struct style * styletype;
    char * stylename;
    long count=0;
    pos=modtext_SkipWhitespace(self,pos,modtext_GetLength(self));
    if(me=modtext_GetEnvironment(self,pos)) {
	while (me != self->header.text.rootEnvironment) {
	    styletype = me->data.style;
	    stylename = style_GetName(styletype);
	    if (strcmp(stylename,"procedure")==0) ++count;
	    me = (struct environment *) me->header.nestedmark.parent;
	}   
	return count;
    }
    return NULL;
}

static long backwardSkipComment(self,pos)
struct modtext *self;
long pos;
{
    int count=1;
    int ch=0,prev;
    if(pos>=0)
	while((count>0)&&(--pos>0)) {
	    prev=ch;
	    if (((ch=modtext_GetChar(self,pos))=='(')&& (prev=='*')) --count;
	    else if ((ch=='*')&&(prev==')')) ++count;
	}
    return pos-1;
}

static long backwardSkipPragma(self,pos)
struct modtext *self;
long pos;
{
    int count=1;
    int ch=0,prev;
    if(pos>=0)
	while((count>0)&&(--pos>0)) {
	    prev=ch;
	    if(((ch=modtext_GetChar(self,pos))=='<')&& (prev=='*')) --count;
	    else if ((ch=='*')&&(prev=='>')) ++count;
	}
    return pos-1;
}

static long maybeBackwardSkipCppLines(self,pos)
struct modtext *self;
long pos;
{
    long currentEOL, len=modtext_GetLength(self);
    --pos;
    do  {
	currentEOL= pos;
	pos= modtext_BackwardSkipString(self,pos,'\n');
    } while (pos>=0 && modtext_GetChar(self,modtext_SkipWhitespace(self,pos+2,len))=='#');
    return currentEOL+1;
}

static long backwardSkipJunk(self,i)
struct modtext *self;
long i;
{
    int ch=0;
    boolean done=FALSE;
    while(!done && (i>=0)) {
	ch=modtext_GetChar(self,i);
	switch(ch) {
	    case '\'':
	    case '\"':
		i=modtext_BackwardSkipString(self,i-1,ch);
		break;
	    case ')':
		if(i<1 || modtext_GetChar(self,i-1)!='*') done=TRUE;
		else i=backwardSkipComment(self,i-1)+1;
		break;
	    case '>':
		if(i<1 || modtext_GetChar(self,i-1)!='*') done=TRUE;
		else i=backwardSkipPragma(self,i-1)+1;
		break;
	    case '\n':
		if (self->preprocessor)
		    i= maybeBackwardSkipCppLines(self,i);
		/* fall through */
	    case '\t':
	    case ' ':
		/* nothing */
		break;
	    default:
		done=TRUE;
	}
	ch=modtext_GetChar(self,i--);
    }
    return i+1;
}

static long matchend(self,pos)
struct modtext *self;
long pos;
{
    int count=1;
    while((count>0)&&(--pos>=0)) {
	pos=backwardSkipJunk(self,pos);
	if(pos>=0) {
	    if(modtext_IsTokenChar(self,modtext_GetChar(self,pos))) {
		Dict *word2;
		char buff[256];
		pos=modtext_BackwardCopyWord(self,pos,0,buff)+1;
		modtext_Keywordify(self,buff,FALSE);
		word2=srctext_Lookup(self->header.srctext.words,buff);    /*RSK90mod*/
		if (word2->val&16) ++count;
		else if(word2->val&1) --count;
	    }
	}
    }
    return modtext_CurrentIndent(self,pos);
}

static long SkipPragma(self,pos,end)
struct modtext *self;
long pos,end;
{
    int count=1;
    int ch=0,prev;
    while((count>0)&&(++pos<end)) {
	prev=ch;
	if(((ch=modtext_GetChar(self,pos))=='>')&& (prev=='*')) --count;
    }
    return pos+1;
}

static long backLevel(self,pos,word,count)   /*RSKmod*/
struct modtext *self;
long pos,count;
Dict *word;/*RSKadd*/
{
    Dict *word2;
    long ncount,tpos;
    char buff[256];
    boolean isbegin=(word->val&8)&&(word->val&1),
    isproc=word->val&128;
    if (isproc) return count;
    while(--pos>=0) {
	/*RSKmod:*/
	pos= modtext_GetBeginningOfLine(self,pos);
	if (pos>0) {
	    tpos=modtext_SkipWhitespace(self,pos,modtext_GetLength(self));
	    if (modtext_GetChar(self,tpos)=='<' && modtext_GetChar(self,tpos+1)=='*') {
		tpos=SkipPragma(self,tpos,modtext_GetLength(self));
		tpos= modtext_SkipWhitespace(self, tpos, modtext_GetLength(self)); /*RSK92fix- CopyWord doesn't skip whitespace itself anymore*/
		(void)modtext_CopyWord(self,tpos/*+1*/,modtext_GetLength(self),buff);
		ncount = GetStyleCount(self,tpos/*+1*/);
	    }
	    else {
		(void)modtext_CopyWord(self,/*pos*/tpos,modtext_GetLength(self),buff); /*RSK92fix- CopyWord doesn't skip whitespace itself anymore*/
		ncount = GetStyleCount(self,/*pos*/tpos);
	    }
	    ncount=ncount*self->header.srctext.levelIndent;
	    modtext_Keywordify(self,buff,FALSE);
	    word2=srctext_Lookup(self->header.srctext.words,buff);    /*RSK90mod*/
	    if (word2->val&32) { return 0;
		/*if (isbegin) return 0;
		else return self->header.srctext.levelIndent;*/
	    }
	    if ((word2->val&8)&&(word2->val&1)&&(!isbegin)&&(ncount==count)) return ncount;
	    if ((word2->val&128)&&(ncount==count)) {
		if (isbegin) return modtext_CurrentIndent(self,pos)+self->header.srctext.levelIndent;
		else return modtext_CurrentIndent(self,pos) + self->header.srctext.levelIndent;
	    }
	    if ((word2->val&8)&&(!isbegin)&&(ncount==count)) return modtext_CurrentIndent(self,pos);
	}
    }
    return 0;
}

static int backwardFirstnonwhitespace(self,pos)   
struct modtext *self;
long pos;
{
    int ch;
    while (pos>0) {
	if ((modtext_GetChar(self,pos)=='>')&&(modtext_GetChar(self,pos-1)=='*')) pos=backwardSkipPragma(self,pos);
	if (!is_whitespace(ch=modtext_GetChar(self,pos--)))
	    return ch;
    }
}

/* STYLESTARTCOL is a macro to find the current column of the beginning of the style wrapped around position pos.*/ /*RSK91add*/
#define STYLESTARTCOL(self,pos) (modtext_CurrentColumn((self),environment_Eval(modtext_GetEnvironment((self),(pos)))))

/* Indentation returns the proper indentation of the line that starts at pos */ /*RSK91rewrite*/
int modtext__Indentation(self,pos)
struct modtext *self;
long pos;
{
    char buff[256];
    Dict *word;
    long i,j,k,count,savcount;
    int indent=0;
    long oldj,temppos;/*RSKadd*/

    /* avoid potential problems with first line of file */ /*RSK91fix*/
    if (pos<1)
	return 0;

    if(modtext_GetStyle(self,pos)==self->header.srctext.comment_style)
	return STYLESTARTCOL(self,pos)+self->header.srctext.commentIndent;
    temppos=modtext_SkipWhitespace(self,pos,modtext_GetLength(self));
    if (modtext_GetChar(self,temppos)=='(' && modtext_GetChar(self,temppos+1)=='*' && !self->header.srctext.reindentComments) /*RSK92mod*/
	return modtext_CurrentIndent(self,pos);
    if (modtext_GetChar(self,temppos)=='<' && modtext_GetChar(self,temppos+1)=='*') {
	temppos= pos= SkipPragma(self,temppos,modtext_GetLength(self));
	temppos= modtext_SkipWhitespace(self, pos, modtext_GetLength(self)); /*RSK92fix*/
    }
    if (self->preprocessor && self->outdentPreproc && modtext_GetChar(self,temppos)=='#')
	return 0;
    (void) modtext_CopyWord(self,/*pos*/temppos,modtext_GetLength(self),buff); /*RSK92fix- CopyWord doesn't skip whitespace itself anymore*/
    modtext_Keywordify(self,buff,FALSE);
    word=srctext_Lookup(self->header.srctext.words,buff);    /*RSK90mod*/
    count = GetStyleCount(self,pos);
    savcount=count;
    if (count) {
	--count;
	if (strcmp(buff,"PROCEDURE")==0) ++count;
	count=count*self->header.srctext.levelIndent;
    }
    if (word->val&32) return 0;/*RSKadd*/
    if (word->val&16) return matchend(self,pos-1);/*RSKmod: change 32 to 16*/
    if ((word->val&8)&&(savcount==0)) return 0;
    if (word->val&8) return backLevel(self,pos-1,word,count);/*RSKmod: add word*/
    if (word->val&4) return matchend(self,pos-1);/*RSKmod: change "matchelse" to "matchend"*/
    j=backwardSkipJunk(self,pos-1);
    while ((modtext_GetChar(self,j)=='(')&&(modtext_GetChar(self,j+1)=='*')&&(j>0))
	j=backwardSkipJunk(self,--j);
    i=modtext_GetBeginningOfLine(self,j); /*RSK92mod*/
    k=modtext_SkipWhitespace(self,i,modtext_GetLength(self));
    if (modtext_GetChar(self,k)=='|'){
	return modtext_CurrentIndent(self,k)+self->header.srctext.levelIndent; 
    }
    /*RSKmod:*/
    oldj=j;
    while (j>i) {
	j=modtext_BackwardCopyWord(self,j,i,buff);
	modtext_Keywordify(self,buff,FALSE);
	word=srctext_Lookup(self->header.srctext.words,buff); /*RSK90mod*/
	if((word->val&1)||(word->val&4)||(word->val&64 && backwardFirstnonwhitespace(self,j)=='\n')) 
		indent=self->header.srctext.levelIndent;
	if(word->val&16)
	    return matchend(self,j)+indent;
	j=backwardSkipJunk(self,j-1);
    }
    if ((i=modtext_CurrentIndent(self,oldj))+indent<0) indent=(-i); /*RSKmod*/
    return i+indent;
}
