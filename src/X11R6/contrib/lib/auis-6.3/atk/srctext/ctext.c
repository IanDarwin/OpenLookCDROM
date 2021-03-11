/* File ctext.c created by R L Quinn
  
   ctext, a C mode for ATK. */

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
static char rcsid[] = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/srctext/RCS/ctext.c,v 1.5 1994/02/22 20:14:18 rr2b Exp $";
#endif

#include <andrewos.h>
#include <class.h>

#include <stylesht.ih>
#include <style.ih>
#include <fontdesc.ih>
#include <attribs.h>
#include <toolcnt.h>

#include "srctext.ih"
#include "ctext.eh"

static Dict *words[TABLESIZE];

boolean ctext__IsTokenChar(self,ch)    
struct ctext *self;
char ch;
{
    return (isalnum(ch) || (ch) == '_');
}
    
void ctext__SetAttributes(self,atts)
struct ctext	*self;
struct attributes   *atts;
{
    super_SetAttributes(self,atts);
    while (atts!=NULL) {
	char *key=atts->key;
	if (strcmp(key,"brace-indent")==0)
	    self->braceIndent=atoi(atts->value.string);
	else if (strcmp(key,"switch-label-undent")==0)
	    self->switchLabelUndent=atoi(atts->value.string);
	else if (strcmp(key,"switch-level-indent")==0)
	    self->switchLevelIndent=atoi(atts->value.string);
	else if (strcmp(key,"outdent-preproc")==0)
	    self->outdentPreproc=atoi(atts->value.string);
	atts=atts->next;
    }
}

static void SetupStyles(self)
struct ctext *self;
{
    if ((self->header.srctext.kindStyle[KEYWRD]= stylesheet_Find(self->header.text.styleSheet, "keyword")) == NULL) {
	self->header.srctext.kindStyle[KEYWRD]= style_New();
	style_SetName(self->header.srctext.kindStyle[KEYWRD], "keyword");
	style_AddNewFontFace(self->header.srctext.kindStyle[KEYWRD], fontdesc_Bold);
	stylesheet_Add(self->header.text.styleSheet, self->header.srctext.kindStyle[KEYWRD]);
    }
    if ((self->header.srctext.kindStyle[PREPRC]= stylesheet_Find(self->header.text.styleSheet, "preproc")) == NULL) {
	self->header.srctext.kindStyle[PREPRC]= style_New();
	style_SetName(self->header.srctext.kindStyle[PREPRC], "preproc");
	style_AddNewFontFace(self->header.srctext.kindStyle[PREPRC], fontdesc_Italic);
	stylesheet_Add(self->header.text.styleSheet, self->header.srctext.kindStyle[PREPRC]);
    }
}

void ctext__SetupStyles(self)
struct ctext *self;
{
    super_SetupStyles(self);
    SetupStyles(self);
}

boolean ctext__InitializeClass(classID)
struct classheader *classID;
{
    static Dict ckeywords[]={
	{"auto",0,KEYWRD},
	{"break",0,KEYWRD},
	{"case",0,KEYWRD},
	{"char",0,KEYWRD},
	{"const",0,KEYWRD},
	{"continue",0,KEYWRD},
	{"default",0,KEYWRD},
	{"do",0,KEYWRD},
	{"double",0,KEYWRD},
	{"else",0,KEYWRD},
	{"enum",0,KEYWRD},
	{"extern",0,KEYWRD},
	{"float",0,KEYWRD},
	{"for",0,KEYWRD},
	{"goto",0,KEYWRD},
	{"if",0,KEYWRD},
	{"int",0,KEYWRD},
	{"long",0,KEYWRD},
	{"register",0,KEYWRD},
	{"return",0,KEYWRD},
	{"short",0,KEYWRD},
	{"signed",0,KEYWRD},
	{"sizeof",0,KEYWRD},
	{"static",0,KEYWRD},
	{"struct",0,KEYWRD},
	{"switch",0,KEYWRD},
	{"typedef",0,KEYWRD},
	{"union",0,KEYWRD},
	{"unsigned",0,KEYWRD},
	{"void",0,KEYWRD},
	{"volatile",0,KEYWRD},
	{"while",0,KEYWRD},
	{NULL,0,0} };
    srctext_BuildTable("ctext",words,ckeywords);
    return TRUE;
}

boolean ctext__InitializeObject(classID, self)
struct classheader *classID;
struct ctext *self;
{
    self->header.srctext.words= (Dict **)words;
    self->braceIndent=0;
    self->switchLabelUndent=4;
    self->switchLevelIndent=8,
    self->outdentPreproc=1;
    self->header.srctext.commentString= "/*  */";
    self->header.srctext.indentingEnabled= TRUE;
    SetupStyles(self);
    ToolCount("EditViews-ctext",NULL);
    return TRUE;
}

/* returns the length of a *constant* string */
#define cstrlen(str) (sizeof(str)-1)

/* pos should pointer to 2nd from last character-- last character won't be
 * matched; strconst *must* be a constant string, so that sizeof works
 */
#define backwardMatch(self,pos,strConst) \
    (pos>=cstrlen(strConst)-1 && match(self,pos-(cstrlen(strConst)-1)+1,strConst))

#define match(self,pos,str) ((pos==0 || !ctext_IsTokenChar(self,ctext_GetChar(self,pos-1))) && !ctext_IsTokenChar(self,ctext_GetChar(self,pos+cstrlen(str))) && ctext_DoMatch(self,pos,str,cstrlen(str))) /*RSK91fix: use IsTokenChar, not isalnum*/

long ctext__MaybeBackwardSkipCppLines(self,pos) /*RSK91mod: returns the position <cr> preceding the Cpp-line, instead of the last character of the previous line (for //comment detection)*/
struct ctext *self;
long pos;
{
    long currentEOL;
    long len= ctext_GetLength(self);

    pos--;
    do{
	currentEOL=pos;
	pos=ctext_BackwardSkipString(self,pos,'\n');
    }while(pos>=0 && ctext_GetChar(self,ctext_SkipWhitespace(self,pos+2,len))=='#'); /*RSK91mod: allow possibility of *indented* Cpp lines*/

    return currentEOL+1;
}

long ctext__BackwardSkipJunk(self,pos)
struct ctext *self;
long pos;
{
    while (pos>=0) {
	register int c=ctext_GetChar(self,pos--);

	switch(c) {
	    case '/':
		if (ctext_GetChar(self,pos)=='*') {
		    long startcomment;
		    if ((startcomment=ctext_InCommentStart(self,pos)) > 0 || (startcomment=ctext_UnstyledCommentStart(self,pos)) > 0)
			pos= startcomment-1;
		    pos= ctext_MaybeBackwardSkipCppLines(self,pos);
		} else /* that slash ISN'T junk */
		    return pos+1;
		break;
	    case '\n':
		if (ctext_GetStyle(self,pos)==self->header.srctext.linecomment_style)
		    pos= ctext_InCommentStart(self,pos)-1;
		pos= ctext_MaybeBackwardSkipCppLines(self,pos);
		/* fall through */
	    case ' ':
	    case '\t': 
		/* nothing */
		break;
	    default:
		return pos+1;
	}
    }
    return -1;
}

/* beg and end must be constants (for switch) */
/* assumes ending delimiter AFTER pos */
long ctext__BackwardSkipDelimited(self,pos,beg,end)
struct ctext *self;
long pos;
char beg, end;
{
    int level=1;
    while(pos>=0 && level>0){
	int c;
	switch(c=ctext_GetChar(self,pos--)){
	    case '\'':
	    case '"':
		pos=ctext_BackwardSkipString(self,pos,c);
		break;
	    case '/':
		if (ctext_GetChar(self,pos)=='*') {
		    long startcomment; /*RSK92mod*/
		    if ((startcomment=ctext_InCommentStart(self,pos)) > 0 || (startcomment=ctext_UnstyledCommentStart(self,pos)) > 0)
			pos= startcomment-1;
		}
		break;
	    case '\n': /* for // comments in C++ */ /*RSK91add*/
		if (ctext_GetStyle(self,pos)==self->header.srctext.linecomment_style)
		    pos= ctext_InCommentStart(self,pos)-1;
		break;
	    default:
		if(c==beg)
		    level--;
		else if(c==end)
		    level++;
	}
    }
    return pos;
}

/* returns true iff the character at pos is quoted (ie. "\"). Takes into account the slash being quoted. (ie "\\"). */ /*RSK92mod*/
boolean ctext__Quoted(self, pos)
struct ctext *self;
long pos;
{
    boolean retval = FALSE;
    while (pos-->0 && ctext_GetChar(self,pos)=='\\')
	retval= !retval;
    return retval;
}

/* ctext_UnstyledCommentStart is a fallback for places where we KNOW we're at the end of a comment, but InCommentStart returns zero becuse there was no style there. */ /*RSK92add*/
long ctext__UnstyledCommentStart(self, pos)
struct ctext *self;
long pos;
{
    int prev, c=0;
    while (pos>0) {
	prev= c;
	c= ctext_GetChar(self,--pos);
	if (c=='/' && prev=='*')
	    /* found a slash-star. It COULD be *inside* the actual comment, but we'll guess that it's the *opening* slash-star. */
	    return pos;
	else if (c=='*' && prev=='/')
	    /* whoah! found ANOTHER end-of-comment */
	    return 0;
    }
    return 0;
}

/* CheckPreproc wraps the preproc style around a preprocessor directive */
long ctext__CheckPreproc(self, start)
struct ctext *self;
long start;
{
    long end, len=ctext_GetLength(self);
    int prev, c;
    CheckSmore: ; /* jump here after we're done handling the comment or whatever */
    for (c=0, end=start; end<len; end++) {
	prev= c;
	switch (c=ctext_GetChar(self,end)) {
	    case '\n':
		if (!ctext_Quoted(self,end)) {
		    if (end>start)
			ctext_WrapStyle(self, start,end-start, self->header.srctext.kindStyle[PREPRC], FALSE,TRUE);
		    return end-1;
		}
		break;
	    case '*':
		if (prev=='/') {
		    if (end-1>start)
			ctext_WrapStyle(self, start,end-start-1, self->header.srctext.kindStyle[PREPRC], FALSE,TRUE);
		    start= ctext_CheckComment(self, end-1) +1;
		    goto CheckSmore; /* continue at end of comment */
		}
		break;
	    case '/':
		if (prev=='/' && self->header.srctext.linecommentString) { /* cheated to detect C++ object */
		    if (end-1>start)
			ctext_WrapStyle(self, start,end-start-1, self->header.srctext.kindStyle[PREPRC], FALSE,TRUE);
		    return ctext_CheckLinecomment(self, end-1);
		}
		break;
	}
    }
    return end-1;
}

static void fn_name(self, posn, last_comment)
struct ctext *self;
long posn, last_comment;
{
    long end=posn, length=ctext_GetLength(self);
    int c, parencount=0;
    do switch (ctext_GetChar(self, end++)) {
	case '(':
	    parencount++;
	    break;
	case ')':
	    parencount--;
	    break;
    } while (parencount>0 && end<length);
    while ((c=ctext_GetChar(self,end))<=' ' && end<length)
	end++;
    if (c=='[' || c=='(')
	/* either it's a global array of pointers to functions, or a single pointer to a function. neither case is an actual function declaration */
	return;
    /* Scan backwards for the end of preceding whitespace */
    end= posn= ctext_BackwardSkipJunk(self,posn-1);
    /* Now find start of token */
    while (posn>=last_comment && ctext_IsTokenChar(self,ctext_GetChar(self, posn)))
	posn--;
    if (posn<end)
	ctext_WrapStyle(self, posn+1,end-posn, self->header.srctext.function_style, TRUE,TRUE);
}

void ctext__RedoStyles(self)
struct ctext *self;
{
    long last_comment = 0, posn, len = ctext_GetLength(self);
    int prev=0, c='\n', braces=0, escape=FALSE;
    /* c is initialized to a newline so the start of the file looks like the start of line. */
    ctext_RemoveStyles(self); /* Remove the old styles, but leave the root environment in place. */
    for (posn=0; posn<len-1; posn++) {
	prev = c;
	c = ctext_GetChar(self, posn);
	if(escape)
	    escape=FALSE;
	else
	    switch (c) {
		case ' ': case '\t':
		    break;
		case '\n':
		    posn= ctext_SkipWhitespace(self, posn+1,len) -1;
		    break;
		case '#':
		    if (prev == '\n')
			posn= ctext_CheckPreproc(self,posn);
		    break;
		case '\\':
		    escape=TRUE;
		    break;
		case '*':
		    if (prev == '/')
			last_comment= posn= ctext_CheckComment(self, posn-1);
		    break;
		case '{':
		    braces++;
		    break;
		case '}':
		    braces--;
		    break;
		case '(':
		    if (!braces)
			fn_name(self, posn, last_comment);
		    braces++;
		    break;
		case ')':
		    braces--;
		    break;
		case '\'':
		case '\"':
		    posn= ctext_CheckString(self,posn);
		    break;
		case ':':	/*wrn added 10/29/92*/
		    ctext_BackwardCheckLabel(self, posn);
		    break;		    
		default:
		    if (ctext_IsTokenChar(self,c))
			posn= ctext_CheckWord(self,posn,len);
		    break;
	    }
    }
}

/* Indentation returns the proper indentation of the line that starts at pos */ /*RSK91rewrite*/
int ctext__Indentation(self,pos)
struct ctext *self;
long pos;
/*boolean reindent,indentEmptyLines;*/ /*RSK91remove*/
{
    int c;
    long savedPos=(-1), parensFollowedBy=(-1), colonFollowedBy=(-1);
    int elseCatch=0;
    boolean onestatement=TRUE, skipend=FALSE, braceFlag=FALSE, commaFlag=FALSE;
    int braceExtra=0, levelExtra=self->header.srctext.levelIndent,
        switchLevelExtra=self->switchLevelIndent, labelExtra=0;

    /* avoid potential problems with first line of file */ /*RSK91fix*/
    if (pos<1)
	return 0;
    /*pos--; /* parse everything *before* pos RSK92remove*/

    /* see if indenting any special cases */
    {
        long newpos;
	if ((newpos=ctext_InCommentStart(self,pos)) > 0)
	    return ctext_CurrentIndent(self,newpos) + self->header.srctext.commentIndent;
	for (newpos=pos/*+1 RSK92remove*/; (c=ctext_GetChar(self,newpos))==' ' || c=='\t'; newpos++) ;
        switch(c){
            case '}':
		braceFlag=TRUE; /* kluge ? */
                pos= ctext_BackwardSkipDelimited(self,newpos-1,'{','}') +1/*RSK92add*/;
                /* fall through */
            case '{':
		braceFlag=TRUE; /* kluge ? */ /* the "kluge?" for '}' should apply for '{' too. RSK92add*/
                levelExtra=self->braceIndent;
		switchLevelExtra=self->braceIndent;
                break;
	    case '#':
		if (self->outdentPreproc)
		    return 0;
		break;
            case 'e':
                if(match(self,newpos,"else"))
		    elseCatch=1;
		break; /*RSK91add*/
            case 'c':
            case 'd':
		if(match(self,newpos,"case") || match(self,newpos,"default"))
                    labelExtra= -self->switchLabelUndent;
                break;
	    case '/':  /*RSKmod addition*/
		if (ctext_GetChar(self,newpos+1)=='*' && !self->header.srctext.reindentComments) /*RSK92mod*/
		    return ctext_CurrentIndent(self,newpos);
		break;
	    case '\n':
	    case EOF:
		/*if(!indentEmptyLines)
		    return 0;*/ /*RSK91remove*/
		break;
        }
    }

    /* sometimes, *only* want to re-indent special cases */
    /*if(reindent && levelExtra==self->header.srctext.levelIndent && labelExtra==0 && elseCatch==0)
        return ctext_CurrentIndent(self,pos);*/ /*RSK91remove*/

    pos= ctext_BackwardSkipJunk(self,pos-1); /*RSK92mod*/

    for(;;){
	switch(ctext_GetChar(self,pos)){
	    case '}':
		pos= ctext_BackwardSkipDelimited(self,--pos,'{','}'); /* avoid hitting the { */
		onestatement=FALSE;
		braceFlag=TRUE;
		pos= ctext_BackwardSkipJunk(self,pos);
		continue;
	    case '{':
		braceExtra+=self->header.srctext.levelIndent;
		braceFlag=TRUE;
		pos= ctext_BackwardSkipJunk(self,--pos);
		continue;
	}
	break;
    }

    if (ctext_GetChar(self,pos)==';') {
	savedPos=pos--;
	onestatement=FALSE;
    }

    for(;;){
        int c;

        if(pos<0)
            c=EOF;
        else
            c=ctext_GetChar(self,pos--);
        switch(c){
	    case '/':
		if (ctext_GetChar(self,pos)=='*') {
		    long startcomment; /*RSK92mod*/
		    if ((startcomment=ctext_InCommentStart(self,pos)) > 0 || (startcomment=ctext_UnstyledCommentStart(self,pos)) > 0)
			pos= startcomment-1;
		}
                break;
            case '"':
            case '\'':
                pos=ctext_BackwardSkipString(self,pos,c);
                savedPos=pos;
                break;
	    case '*':
		/* primitive comment support */
		if(pos>=0 && ctext_GetChar(self,pos)=='/')
		    return ctext_CurrentColumn(self,pos)+self->header.srctext.commentIndent; /*RSK91mod*/
		break;
	    case '\n':
		/* must check to avoid cpp lines */
		pos=ctext_MaybeBackwardSkipCppLines(self,pos);
		/* fall through */
            case ' ':
            case '\t':
                /* nothing */
                break;
	    case 'f': /* if */
		if((onestatement || elseCatch>0) && backwardMatch(self,pos,"if"))
                    if(elseCatch==0)
                        if(parensFollowedBy==-1)
                            return ctext_CurrentIndent(self,pos) +levelExtra +labelExtra;
                        else
                            return ctext_CurrentIndent(self,pos) +levelExtra +self->header.srctext.contIndent;
                    else if(--elseCatch==0)
                        return ctext_CurrentIndent(self,pos); /* line else up with if */
		    savedPos=pos;
                break;
            case 'o': /* do */
                if(onestatement && backwardMatch(self,pos,"do"))
                    if(savedPos==-1)
                        return ctext_CurrentIndent(self,pos)+levelExtra+labelExtra;
                    else
                        return ctext_CurrentIndent(self,pos) +levelExtra +self->header.srctext.contIndent;
                savedPos=pos;
                break;
            case 'e': /* else, case or while */
                if(backwardMatch(self,pos,"case"))
                    if(!onestatement || colonFollowedBy==-1)
                        return ctext_CurrentIndent(self,pos) + self->switchLevelIndent - (self->switchLevelIndent - self->switchLabelUndent) +labelExtra;
                    else
                        return ctext_CurrentIndent(self,pos) + (self->switchLevelIndent - self->switchLabelUndent) + self->header.srctext.contIndent;
                else if(onestatement && backwardMatch(self,pos,"while"))
                    if(parensFollowedBy==-1)
                        return ctext_CurrentIndent(self,pos) +levelExtra +labelExtra;
                    else
                        return ctext_CurrentIndent(self,pos) +levelExtra +self->header.srctext.contIndent;
                else if(backwardMatch(self,pos,"else"))
                    if(onestatement)
                        if(savedPos==-1)
                            return ctext_CurrentIndent(self,pos) +levelExtra +labelExtra;
                        else
                            return ctext_CurrentIndent(self,pos) +levelExtra +self->header.srctext.contIndent;
                    else{
                        skipend=TRUE;
                        if(elseCatch>0)
                            elseCatch++;
                    }
                savedPos=pos;
                break;
            case 'r':
                if(onestatement && backwardMatch(self,pos,"for"))
                    if(parensFollowedBy==-1)
                        return ctext_CurrentIndent(self,pos) +levelExtra +labelExtra;
                    else
                        return ctext_CurrentIndent(self,pos) +levelExtra +self->header.srctext.contIndent;
                savedPos=pos;
                break;
            case 'h':
                if(onestatement && backwardMatch(self,pos,"switch"))
                    if(parensFollowedBy==-1)
                        return ctext_CurrentIndent(self,pos) +switchLevelExtra +labelExtra;
                    else
                        return ctext_CurrentIndent(self,pos) +switchLevelExtra +self->header.srctext.contIndent;
                savedPos=pos;
                break;
            case 't':
                if(backwardMatch(self,pos,"default") && !ctext_IsTokenChar(self,ctext_GetChar(self,pos+2)))
                    if(!onestatement || colonFollowedBy==-1)
                        return ctext_CurrentIndent(self,pos) + self->switchLevelIndent - (self->switchLevelIndent - self->switchLabelUndent) + labelExtra;
                    else
                        return ctext_CurrentIndent(self,pos) + (self->switchLevelIndent - self->switchLabelUndent) + self->header.srctext.contIndent;
                savedPos=pos;
                break;
	    case ',':
		/* this is a kluge, which assumes that commas occuring outside of parens
		* imply a separate statement (i.e., equivalent indentation).  This is mostly true.
		*/
		commaFlag=TRUE;
		savedPos=pos;
		break;
            case ':':
                colonFollowedBy=savedPos;
                savedPos=pos;
                break;
            case '{':
                /* Since skipend being set here would indicate a syntax error in the code, we assume it isn't. */
                /* fall through */
            case '}':
                if(skipend)
                    pos= ctext_BackwardSkipDelimited(self,pos,'{','}');
                /* fall through */
	    case ';':
	    case EOF:
		if(skipend){
		    skipend=FALSE;
		    savedPos=pos;
		}else if(onestatement){
		    int cin=ctext_CurrentIndent(self,savedPos+1);
		    /* savedPos set at beginning */
		    if(braceFlag)
			return cin+braceExtra;
		    else if(cin==0)
			return 0;
		    else if(commaFlag)
			return cin;
		    else
			return cin +self->header.srctext.contIndent;
		}else { /* CAN'T get here until after something that sets savedPos */
		    /* first make sure the PREVIOUS line didn't ALREADY have 'labelExtra' added to it */ /*RSK91fix*/
		    long tpos= ctext_SkipWhitespace(self, ctext_GetBeginningOfLine(self,savedPos+1), ctext_GetLength(self));
		    if(match(self,tpos,"case") || match(self,tpos,"default"))
			labelExtra+= self->switchLabelUndent;
		    return ctext_CurrentIndent(self,savedPos+1)+labelExtra+braceExtra;
		}
		break;
	    case ')':
		pos= ctext_BackwardSkipDelimited(self,pos,'(',')');
		/* save to do continuation lines correctly */
		parensFollowedBy=savedPos;
		savedPos=pos;
		break;
	    case ']':
		pos= ctext_BackwardSkipDelimited(self,pos,'[',']');
		savedPos=pos;
		break;
	    case '(':
	    case '[':
		return ctext_CurrentColumn(self, ctext_SkipWhitespace(self, pos+2, savedPos+1));
	    default:
                savedPos=pos;
	}
    }
}
