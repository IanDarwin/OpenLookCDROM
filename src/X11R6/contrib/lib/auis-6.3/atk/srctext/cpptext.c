/* File cpptext.c created by R L Quinn
   

   cpptext, a C++ mode for ATK.  */

/* Copyright 1990, 1994 Carnegie Mellon University and IBM.  All rights reserved.
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
static char rcsid[] = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/srctext/RCS/cpptext.c,v 1.8 1994/02/22 20:14:18 rr2b Exp $";
#endif

#include <andrewos.h>

#include <class.h>

#include <envrment.ih>
#include <stylesht.ih>
#include <style.ih>
#include <fontdesc.ih>
#include <attribs.h>
#include <toolcnt.h>  

#include "srctext.ih"
#include "cpptext.eh"

static Dict *words[TABLESIZE];

void cpptext__SetAttributes(self,atts)
struct cpptext	*self;
struct attributes   *atts;
{
    super_SetAttributes(self,atts);
    while (atts!=NULL) {
	if (strcmp(atts->key,"acsctrl-outdent")==0)
	    self->acsctrlOutdent=atoi(atts->value.string);
	atts=atts->next;
    }
}

static void SetupStyles(self)
struct cpptext *self;
{
    if ((self->header.srctext.kindStyle[CLASS]= stylesheet_Find(self->header.text.styleSheet, "class")) == NULL) {
	self->header.srctext.kindStyle[CLASS]= style_New();
	style_SetName(self->header.srctext.kindStyle[CLASS], "class");
	style_AddNewFontFace(self->header.srctext.kindStyle[CLASS], fontdesc_Bold);
	style_SetFontSize(self->header.srctext.kindStyle[CLASS], style_PreviousFontSize, 2);
	stylesheet_Add(self->header.text.styleSheet, self->header.srctext.kindStyle[CLASS]);
    }
}

void cpptext__SetupStyles(self)
struct cpptext *self;
{
    super_SetupStyles(self);
    SetupStyles(self);
}

boolean cpptext__InitializeClass(classID)
struct classheader *classID;
{
    static Dict cppkeywords[]={
	{"asm",0,KEYWRD},
	{"auto",0,KEYWRD},
	{"break",0,KEYWRD},
	{"case",0,KEYWRD},
	{"catch",0,KEYWRD},
	{"char",0,KEYWRD},
	{"class",CLASS_BIT,KEYWRD},
	{"const",0,KEYWRD},
	{"continue",0,KEYWRD},
	{"default",0,KEYWRD},
	{"delete",0,KEYWRD},
	{"do",0,KEYWRD},
	{"double",0,KEYWRD},
	{"else",0,KEYWRD},
	{"enum",0,KEYWRD},
	{"extern",0,KEYWRD},
	{"float",0,KEYWRD},
	{"for",0,KEYWRD},
	{"friend",0,KEYWRD},
	{"goto",0,KEYWRD},
	{"if",0,KEYWRD},
	{"inline",0,KEYWRD},
	{"int",0,KEYWRD},
	{"long",0,KEYWRD},
	{"new",0,KEYWRD},
	{"operator",0,KEYWRD},
	{"overload",0,KEYWRD}, /* in FIRST, but not SECOND edition of Lippman's C++ Primer */
	{"private",ACSCTRL_BIT,KEYWRD},
	{"protected",ACSCTRL_BIT,KEYWRD},
	{"public",ACSCTRL_BIT,KEYWRD},
	{"register",0,KEYWRD},
	{"return",0,KEYWRD},
	{"short",0,KEYWRD},
	{"signed",0,KEYWRD},
	{"sizeof",0,KEYWRD},
	{"static",0,KEYWRD},
	{"struct",CLASS_BIT,KEYWRD}, /* synonym for "class", hence CLASS_BIT is set */
	{"switch",0,KEYWRD},
	{"this",0,KEYWRD},
	{"template",TEMPLATE_BIT,KEYWRD},
	{"throw",0,KEYWRD}, /* in SECOND, but not FIRST edition of Lippman's C++ Primer */
	{"try",0,KEYWRD},
	{"typedef",0,KEYWRD},
	{"union",0,KEYWRD},
	{"unsigned",0,KEYWRD},
	{"virtual",0,KEYWRD},
	{"void",0,KEYWRD},
	{"volatile",0,KEYWRD},
	{"while",0,KEYWRD},
	{NULL,0,0} };
    srctext_BuildTable("cpptext",words,cppkeywords);
    return TRUE;
}

boolean cpptext__InitializeObject(classID, self)
struct classheader *classID;
struct cpptext *self;
{
    self->header.srctext.words= (Dict **)words;
    self->acsctrlOutdent= 2;
    self->header.srctext.linecommentString= "// ";
    SetupStyles(self);
    ToolCount("EditViews-cpptext",NULL);
    return TRUE;
}

/* override */
/* cpptext_ReflowComment is identical to srctext's, but it will also flow double-slash comments.  pos is the beginning of the line we want to try to flow into the previous one */
boolean cpptext__ReflowComment(self,pos)
struct cpptext *self;
long pos;
{
    long startcomment=cpptext_InCommentStart(self,pos);
    long skipws= cpptext_SkipWhitespace(self,pos, cpptext_GetLength(self));
    if (cpptext_GetChar(self,skipws) == '\n')
	/* don't flow it if it's a blank line */
	return FALSE;
    if (startcomment && startcomment<pos) {
	/* this line IS a continued comment; flow it with the previous line */
	cpptext_ReplaceCharacters(self,pos-1, skipws-pos+1, " ",1);
	return TRUE;
    } else if ((startcomment= cpptext_InCommentStart(self,skipws+1)) > 0) {
	/* this line contains a separate comment that might be appropriate to flow with the previous comment */
	struct style *commenttype=cpptext_GetStyle(self,skipws+1);
	if (commenttype==self->header.srctext.linecomment_style) {
	    int startcol=cpptext_CurrentColumn(self,startcomment);
	    if (is_whitespace(cpptext_GetChar(self, startcomment+2)) && cpptext_GetStyle(self,pos-1)==commenttype && environment_GetLength(cpptext_GetEnvironment(self, pos-1)) > 2) {
		/* this is a flowable double-slash comment and there's a double-slash comment on the previous line too (len>2); flow them together */
		int spaces=1;
		while (is_whitespace(cpptext_GetChar(self, startcomment+2+spaces))) ++spaces;
		cpptext_ReplaceCharacters(self,pos-1, startcomment+3+spaces-pos, " ",1);
		cpptext_CheckLinecomment(self, environment_Eval(cpptext_GetEnvironment(self, pos-1)));
		return TRUE;
	    }
	}
    }
    return FALSE;
}

/* override */
/* BreakLine breaks up a line so it fits within the specified max-length. endofline is a mark (so it stays put during reindenting) pointing to the newline at the end of the line to be broken. */
void cpptext__BreakLine(self,endofline)
struct cpptext *self;
struct mark *endofline;
{
    long end;
    int c;
    boolean brokebang;
    int ccol= cpptext_CurrentColumn(self,mark_GetPos(endofline)-1);
    int prevlen= ccol+1; /* add 1 to ensure at least 1 iteration of while loop */
    /* ccol & prevlen keep track of how long the remainder of the line IS and WAS */
    /* this prevents infinite loops when a 'word' is too long to break up right */
    while (ccol>=cpptext_GetMaxLineLength(self) && ccol<prevlen) {
	/* line too long; fragment it */
	long stopline= mark_GetPos(endofline);
	int curcol= ccol+1;
	do  {
	    stopline--;
	    c= cpptext_GetChar(self,stopline);
	    if (c=='\t') /* recalculate */
		curcol= cpptext_CurrentColumn(self, stopline);
	    else /* decrement (it's a LOT quicker!) */
		curcol--;
	} while (c!='\n' && stopline>0 && (!cpptext_Breakable(self,stopline) || curcol>cpptext_GetMaxLineLength(self)));
	if (stopline<1 || c=='\n')
	    return; /* never mind */
	brokebang= (cpptext_GetStyle(self,stopline)==self->header.srctext.linecomment_style);
	end= cpptext_SkipWhitespace(self,stopline,cpptext_GetLength(self));
	while (stopline>0 && is_whitespace(cpptext_GetChar(self,stopline-1)))
	    stopline--;
	if (stopline<end)
	    cpptext_DeleteCharacters(self,stopline,end-stopline);
	/* pretend there's a blank line so it uses 'desired' _Indentation */
	cpptext_JustInsertCharacters(self,stopline,"\n\n",2);
	if (brokebang) {
	    /* remove style from the broken part */
	    struct environment *bangstyle= cpptext_GetEnvironment(self,stopline);
	    long bangstart= environment_Eval(bangstyle);
	    int bangcol= cpptext_CurrentColumn(self,bangstart);
	    int textoffset= cpptext_CurrentColumn(self, cpptext_SkipWhitespace(self, bangstart+2, stopline)) - bangcol;
	    environment_Remove(self->header.text.rootEnvironment, stopline, environment_GetLength(bangstyle) + bangstart - stopline, environment_Style, FALSE);
	    cpptext_ReplaceCharacters(self, stopline+1,1, "// ",3);
	    cpptext_CheckLinecomment(self,stopline+1);
	    cpptext_TabAndOptimizeWS(self,stopline+1,bangcol);
	    if (textoffset>2)
		/* reindent *within* comments to make a straight left margin for paragraph */
		cpptext_TabAndOptimizeWS(self, cpptext_SkipWhitespace(self,stopline+1, cpptext_GetLength(self)) +2, bangcol+textoffset);
	    /* reindent it to line up with the previous bang */
	} else {
	    /* reindent the "blank line" */
	    cpptext_ReindentLine(self,stopline+1);
	    stopline= cpptext_SkipWhitespace(self,stopline+1, cpptext_GetLength(self));
	    cpptext_DeleteCharacters(self, stopline, 1); /* remove extra newline */
	    /* reindent *again* in case we have to right-justify an end-of-comment */
	    cpptext_ReindentLine(self,stopline);
	}
	prevlen= ccol;
	ccol= cpptext_CurrentColumn(self,mark_GetPos(endofline)-1);
    }
}

/* skipJunk finds the position of the next "significant" character */
static long skipJunk(self, pos, len)
struct cpptext *self;
long pos, len;
{
    int ch=cpptext_GetChar(self,pos++), prev;
    int incomment=FALSE;
    while (pos<len) {
	prev= ch;
	ch= cpptext_GetChar(self,pos++);
	if (incomment) {
	    if (prev=='*' && ch=='/') {
		incomment= FALSE;
		ch= cpptext_GetChar(self,pos++);
	    }
	} else if (prev=='/') {
	    if (ch=='*')
		incomment= TRUE;
	    else if (ch=='/') {
		pos= cpptext_GetEndOfLine(self,pos)+1;
		ch= '\n';
	    }
	} else
	    if (prev>' ')
		return pos-2;
    }
    return len;
}

static boolean isOperatorOverload(self,ch)
struct cpptext *self;
char ch;
{
    return (index("!%&,^|~()[]*/+-<=>",ch) != NULL);
}

static void fn_name(self, posn, backtoofar)
struct cpptext *self;
long posn, backtoofar;
{
    long end=posn, length=cpptext_GetLength(self);
    int c, parencount=0;
    do switch (cpptext_GetChar(self, end++)) {
	case '(':
	    parencount++;
	    break;
	case ')':
	    parencount--;
	    break;
    } while (parencount>0 && end<length);
    while ((c=cpptext_GetChar(self,end))<=' ' && end<length)
	end++;
    if (c=='[' || c=='(')
	/* either it's a global array of pointers to functions, or a single pointer to a function. neither case is an actual function declaration */
	return;
    /* Scan backwards for the end of preceding whitespace */
    end= posn= cpptext_BackwardSkipJunk(self,posn-1);
    /* Now find start of token */
    /* Check for any operator overload characters */
    while (posn>backtoofar && isOperatorOverload(self,c=cpptext_GetChar(self,posn))) {
	--posn;
	if (c==')' && cpptext_GetChar(self,posn)!='(')
	    /* the "function name" is in parens, so it must be some global pointer to a function. */
	    return;
    }
    /* Check for fctn name tokens, or possibly an operator like "new" or maybe the keyword "operator" */
    while (posn>backtoofar && cpptext_IsTokenChar(self,cpptext_GetChar(self, posn)))
	--posn;
    /* Possible to have destructor op at this point */
    if (cpptext_GetChar(self,posn) == '~')
	--posn;
    { /* Make "operator" keyword part of function name, if present */
	long endop=posn;
	while (is_whitespace(cpptext_GetChar(self, endop))) --endop;
	if (endop>8 && cpptext_Strncmp(self,endop-7, "operator",8)==0)
	    posn= endop-8;
    }
    /* Test for scope resolution operator(s).  Include it and the class name if present. Also grab <template args> */
    while (posn>backtoofar && cpptext_GetChar(self,posn)==':' && cpptext_GetChar(self,posn-1)==':') {
	boolean inTplArgList= FALSE;
	posn-= 2;
	while (posn>backtoofar && (cpptext_IsTokenChar(self,c=cpptext_GetChar(self, posn)) || c=='<' || c=='>' || ( /* not so picky if inside <template arg list> */ inTplArgList && (is_whitespace(c) || c==',')))) {
	    --posn;
	    if (c=='<') inTplArgList= FALSE;
	    else if (c=='>') inTplArgList= TRUE;
	}
    }
    /* if we see an assignment in front of all this, it means it's a fctn CALL initializing a global var */
    if (cpptext_GetChar(self, cpptext_BackwardSkipJunk(self,posn)) == '=')
	return;
    if (posn<end)
	cpptext_WrapStyle(self, posn+1,end-posn, self->header.srctext.function_style, FALSE,TRUE);
}

/* static global data shared between RedoStyles and CheckWord */
static boolean inClassDef; /* TRUE to flag that "class" keyword was found.  reset to FALSE once inside definition itself */
static int inTplArgList; /* -1 to flag that "template" keyword was found.  + values count <> nesting */ /* this check is probably unnecessary, since the keyword "class" only implies a class decl if it has a : or { after the class name */

void cpptext__RedoStyles(self)
struct cpptext *self;
{
    long posn, len=cpptext_GetLength(self), readyForFName=1;
    int prev=0, c='\n', braces=0; /* c is initialized to a newline so the start of the file looks like the start of line. */
    boolean escape=FALSE, inClassDefBraces[64];
    inClassDef= inTplArgList= FALSE;
    cpptext_RemoveStyles(self); /* Remove the old styles, but leave the root environment in place. */
    for (posn= 0; posn<len-1; ++posn) {
	prev = c;
	c = cpptext_GetChar(self, posn);
	if(escape)
	    escape=FALSE;
	else
	    switch (c) {
		case ' ': case '\t':
		    break;
		case '\n':
		    posn= cpptext_SkipWhitespace(self, posn+1,len) -1;
		    break;
		case '#':
		    if (prev == '\n')
			posn= cpptext_CheckPreproc(self,posn);
		    break;
		case '\\':
		    escape=TRUE;
		    break;
		case '*':
		    if (prev == '/') {
			posn = cpptext_CheckComment(self, posn-1);
			if (readyForFName)
			    readyForFName= posn;
		    }
		    break;
		case '/':
		    if (prev=='/') {
			posn= cpptext_CheckLinecomment(self,posn-1);
			if (readyForFName)
			    readyForFName= posn;
		    }
		    break;
		case '{':
		    ++braces;
		    if (inClassDef) {
			if (braces<64) inClassDefBraces[braces]= TRUE;
			readyForFName= posn;
			inClassDef= FALSE;
		    } else { /* this must be the open brace for a function definition; quit looking for declarations */
			readyForFName= FALSE;
			if (braces<64) inClassDefBraces[braces]= FALSE;
		    }
		    break;
		case '}':
		    --braces;
		    if (braces<64 && inClassDefBraces[braces]) {
			/* end brace of member function definition */
			readyForFName= posn;
		    }
		    if (braces<=0) {
			braces= 0;
			readyForFName= posn;
		    }
		    break;
		case '<':
		    if (inTplArgList)
			inTplArgList= (inTplArgList<0) ? 1 : (inTplArgList-1);
		    break;
		case '>':
		    if (inTplArgList)
			--inTplArgList;
		    break;
		case '(':
		    if (readyForFName && (!braces || (braces<64 && inClassDefBraces[braces])))
			fn_name(self,posn,readyForFName);
		    ++braces;
		    if (braces<64) inClassDefBraces[braces]= FALSE;
		    break;
		case ')':
		    --braces;
		    if (braces<0) braces= 0;
		    if (cpptext_GetChar(self, skipJunk(self, posn+1,len))==':')
			/* some inline variable initializations are coming up, and they only LOOK like function names! */
			readyForFName= FALSE;
		    break;
		case '[':
		    ++braces; /* kludge to prevent styling [sizeof()] as a function name */
		    if (braces<64) inClassDefBraces[braces]= FALSE;
		    break;
		case ']':
		    --braces;
		    if (braces<0) braces= 0;
		    break;
		case ';':
		    if (inClassDef) /* no braces were found after the "class" keyword */
			inClassDef= FALSE;
		    if (!braces || (braces<64 && inClassDefBraces[braces]))
			readyForFName= posn;
		    break;
		case '\'':
		case '\"':
		    posn= cpptext_CheckString(self,posn);
		    break;
		case ':':
		    if (prev!=':' && cpptext_GetChar(self,posn+1)!=':') /* it's not a scope resolution operator */
			cpptext_BackwardCheckLabel(self, posn);
		    break;		    
		default:
		    if (cpptext_IsTokenChar(self,c))
			posn= cpptext_CheckWord(self,posn,len);
		    break;
	    }
    }
}

/* override */
/* cpptext_CheckWord does everything that srctext's would have, but also checks for class names */
long cpptext__CheckWord(self,i,end)
struct cpptext *self;
long i,end;
{
    long j, start, endstr;
    long filelen=cpptext_GetLength(self);
    Dict *word;
    char c, buff[1024];
    i=cpptext_SkipWhitespace(self,i,end);
    j=cpptext_CopyWord(self,i,end,buff);
    cpptext_Keywordify(self,buff,FALSE);
    if((word=srctext_Lookup(self->header.srctext.words,buff))->stng!=NULL) {
	cpptext_WrapStyle(self, i,strlen(word->stng), self->header.srctext.kindStyle[word->kind], FALSE,FALSE);
        /* Check for class definition keyword */
	if (((word->val) & CLASS_BIT) && !inTplArgList) {
	    /* it's a "class" or "struct" keyword, and isn't inside a <template-argument-list> */
	    start=endstr=cpptext_SkipWhitespace(self,j+1,end);
	    while((endstr<filelen) && (cpptext_IsTokenChar(self,cpptext_GetChar(self, endstr)))) endstr++;
	    if (endstr>start && ((c=cpptext_GetChar(self, skipJunk(self,endstr,end)))==':' || c=='{' || c==';')) {
		/* it's must be a real class decl if it's derived, or has a defn after it, or is just a decl */
		cpptext_WrapStyle(self, start,endstr-start, self->header.srctext.kindStyle[CLASS], TRUE,TRUE);
		j=endstr-1;
		inClassDef=TRUE;
	    }
	}
	if ((word->val) & TEMPLATE_BIT)
	    inTplArgList= TRUE;
    }
    return j;
}

/* override */
/* cpp's RedoStyles won't call this for scope resolution operators (::), but it DOES still get called when user TYPES a colon */
void cpptext__BackwardCheckLabel(self, pos)
struct cpptext *self;
long pos;
{
    if (cpptext_GetChar(self,pos-1)!=':')
	super_BackwardCheckLabel(self,pos);
    else { /* uh-oh. assume we mistakenly styled it as a label when that PRECEDING colon was typed. UN-style it */
	long start=pos-1;
	while (is_whitespace(cpptext_GetChar(self, --start))) ;
	if (cpptext_GetStyle(self,start)==self->header.srctext.label_style) {
	    start= cpptext_GetBeginningOfLine(self, start);
	    environment_Remove(self->header.text.rootEnvironment, start,pos-start, environment_Style,TRUE);
	    cpptext_RegionModified(self, start,pos-start);
	}
    }
}

/* returns the length of a *constant* string */
#define cstrlen(str) (sizeof(str)-1)

/* pos should pointer to 2nd from last character-- last character won't be
 * matched; strconst *must* be a constant string, so that sizeof works
 */
#define backwardMatch(self,pos,strConst) \
    (pos>=cstrlen(strConst)-1 && match(self,pos-(cstrlen(strConst)-1)+1,strConst))

#define match(self,pos,str) ((pos==0 || !cpptext_IsTokenChar(self, cpptext_GetChar(self, pos-1))) && !cpptext_IsTokenChar(self, cpptext_GetChar(self, pos+cstrlen(str))) && cpptext_DoMatch(self,pos,str,cstrlen(str))) /*RSK91fix: use IsTokenChar, not isalnum*/

/* Indentation returns the proper indentation of the line that starts at pos */ /*RSK91rewrite*/
int cpptext__Indentation(self,pos)
struct cpptext *self;
long pos;
/*boolean reindent,indentEmptyLines;*/ /*RSK91remove*/
{
    int c;
    long savedPos=(-1), parensFollowedBy=(-1), colonFollowedBy=(-1);
    int elseCatch=0;
    boolean onestatement=TRUE, skipend=FALSE, braceFlag=FALSE, commaFlag=FALSE;
    int braceExtra=0, levelExtra=self->header.srctext.levelIndent,
	switchLevelExtra=self->header.ctext.switchLevelIndent, labelExtra=0;

    /* avoid potential problems with first line of file */ /*RSK91fix*/
    if (pos<1)
	return 0;
    /*pos--; /* parse everything *before* pos RSK92remove*/
    
    /* see if indenting any special cases */
    {
        long newpos;
	if ((newpos=cpptext_InCommentStart(self,pos)) > 0 && cpptext_GetStyle(self,pos) != self->header.srctext.linecomment_style) /* does NOT apply for //-comments! */
	    return cpptext_CurrentIndent(self,newpos) + self->header.srctext.commentIndent;
	for (newpos=pos/*+1 RSK92remove*/; (c=cpptext_GetChar(self,newpos))==' ' || c=='\t'; newpos++) ;
	switch(c){
	    case '}':
		braceFlag=TRUE; /* kluge ? */
		pos= cpptext_BackwardSkipDelimited(self,newpos-1,'{','}') +1/*RSK92add*/;
		/* fall through */
	    case '{':
		braceFlag=TRUE; /* kluge ? */ /* the "kluge?" for '}' should apply for '{' too. RSK92add*/
		levelExtra=self->header.ctext.braceIndent;
		switchLevelExtra=self->header.ctext.braceIndent;
		break;
	    case '#':
		if (self->header.ctext.outdentPreproc)
		    return 0;
		break;
	    case 'e':
		if(match(self,newpos,"else"))
		    elseCatch=1;
		break; /*RSK91add*/
	    case 'c':
	    case 'd':
		if(match(self,newpos,"case") || match(self,newpos,"default"))
		    labelExtra= -self->header.ctext.switchLabelUndent;
		break;
	    case 'p': /*RSK91add*/
		if (match(self,newpos,"private") || match(self,newpos,"public") || match(self,newpos,"protected"))
		    labelExtra= -(self->acsctrlOutdent);
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
	return cpptext_CurrentIndent(self,pos);*/ /*RSK91remove*/

    pos= cpptext_BackwardSkipJunk(self,pos-1); /*RSK92mod*/

    for(;;){
	switch(cpptext_GetChar(self,pos)){
	    case '}':
		pos= cpptext_BackwardSkipDelimited(self,--pos,'{','}'); /* avoid hitting the { */
		onestatement=FALSE;
		braceFlag=TRUE;
		pos= cpptext_BackwardSkipJunk(self,pos);
		continue;
	    case '{':
		braceExtra+=self->header.srctext.levelIndent;
		braceFlag=TRUE;
		pos= cpptext_BackwardSkipJunk(self,--pos);
		continue;
	}
	break;
    }

    if (cpptext_GetChar(self,pos)==';') {
	savedPos=pos--;
	onestatement=FALSE;
    }

   for(;;){
	int c;

	if(pos<0)
	    c=EOF;
	else
	    c=cpptext_GetChar(self,pos--);
	switch(c){
	    case '/':
		if (cpptext_GetChar(self,pos)=='*') {
		    long startcomment; /*RSK92mod*/
		    if ((startcomment=cpptext_InCommentStart(self,pos)) > 0 || (startcomment=cpptext_UnstyledCommentStart(self,pos)) > 0)
			pos= startcomment-1;
		}
		break;
	    case '"':
	    case '\'':
		pos=cpptext_BackwardSkipString(self,pos,c);
		savedPos=pos;
		break;
	    case '*':
		/* primitive comment support */
		if(pos>=0 && cpptext_GetChar(self,pos)=='/')
		    return cpptext_CurrentColumn(self,pos)+self->header.srctext.commentIndent; /*RSK91mod*/
		break;
	    case '\n':
		/* must check to avoid cpp lines */
		if (cpptext_GetStyle(self,pos)==self->header.srctext.linecomment_style) /*RSK91add*/
		    pos= cpptext_InCommentStart(self,pos)-1; /*RSK91mod*/
		else
		    pos=cpptext_MaybeBackwardSkipCppLines(self,pos);
		/* fall through */
	    case ' ':
	    case '\t':
		/* nothing */
		break;
	    case 'f': /* if */
		if((onestatement || elseCatch>0) && backwardMatch(self,pos,"if"))
		    if(elseCatch==0)
			if(parensFollowedBy==-1)
			    return cpptext_CurrentIndent(self,pos) +levelExtra +labelExtra;
			else
			    return cpptext_CurrentIndent(self,pos) +levelExtra +self->header.srctext.contIndent;
		    else if(--elseCatch==0)
			return cpptext_CurrentIndent(self,pos); /* line else up with if */
		savedPos=pos;
		break;
	    case 'o': /* do */
		if(onestatement && backwardMatch(self,pos,"do"))
		    if(savedPos==-1)
			return cpptext_CurrentIndent(self,pos)+levelExtra+labelExtra;
		    else
			return cpptext_CurrentIndent(self,pos) +levelExtra +self->header.srctext.contIndent;
		savedPos=pos;
		break;
	    case 'e': /* else, case or while */
		if(backwardMatch(self,pos,"case"))
		    if(!onestatement || colonFollowedBy==-1)
			return cpptext_CurrentIndent(self,pos) + self->header.ctext.switchLevelIndent - (self->header.ctext.switchLevelIndent - self->header.ctext.switchLabelUndent) + labelExtra;
		    else
			return cpptext_CurrentIndent(self,pos) + (self->header.ctext.switchLevelIndent - self->header.ctext.switchLabelUndent) + self->header.srctext.contIndent;
		else if(onestatement && backwardMatch(self,pos,"while"))
		    if(parensFollowedBy==-1)
			return cpptext_CurrentIndent(self,pos) +levelExtra +labelExtra;
		    else
			return cpptext_CurrentIndent(self,pos) +levelExtra +self->header.srctext.contIndent;
		else if(backwardMatch(self,pos,"else"))
		    if(onestatement)
			if(savedPos==-1)
			    return cpptext_CurrentIndent(self,pos) +levelExtra +labelExtra;
			else
			    return cpptext_CurrentIndent(self,pos) +levelExtra +self->header.srctext.contIndent;
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
			return cpptext_CurrentIndent(self,pos) +levelExtra +labelExtra;
		    else
			return cpptext_CurrentIndent(self,pos) +levelExtra +self->header.srctext.contIndent;
		savedPos=pos;
		break;
	    case 'h':
		if(onestatement && backwardMatch(self,pos,"switch"))
		    if(parensFollowedBy==-1)
			return cpptext_CurrentIndent(self,pos) +switchLevelExtra +labelExtra;
		    else
			return cpptext_CurrentIndent(self,pos) +switchLevelExtra +self->header.srctext.contIndent;
		savedPos=pos;
		break;
	    case 's': /*RSK91add*/
		if (onestatement && backwardMatch(self,pos,"class"))
		    return cpptext_CurrentIndent(self,pos) +braceExtra +labelExtra;
		savedPos=pos;
		break;
	    case 't':
		if(backwardMatch(self,pos,"default") && !cpptext_IsTokenChar(self,cpptext_GetChar(self,pos+2)))
		    if(!onestatement || colonFollowedBy==-1)
			return cpptext_CurrentIndent(self,pos) + self->header.ctext.switchLevelIndent - (self->header.ctext.switchLevelIndent - self->header.ctext.switchLabelUndent) + labelExtra;
		    else
			return cpptext_CurrentIndent(self,pos) + (self->header.ctext.switchLevelIndent - self->header.ctext.switchLabelUndent) + self->header.srctext.contIndent;
		savedPos=pos;
		break;
	    case ',':
		/* this is a kluge, which assumes that commas occuring outside of parens
		 imply a separate statement (i.e., equivalent indentation).  This is mostly true.*/
		commaFlag=TRUE;
		savedPos=pos;
		break;
	    case ':':
		if(backwardMatch(self,pos,"private:") ||  backwardMatch(self,pos,"public:") || backwardMatch(self,pos,"protected:"))
		    return cpptext_CurrentIndent(self,pos)+(self->acsctrlOutdent)+labelExtra+braceExtra; /*RSK92mod*/
		/* make sure this is a ':' and not a '::' (C++ only) */ /*RSK91fix*/
		if (cpptext_GetChar(self,pos)==':' || cpptext_GetChar(self,pos+2)==':')
		    break;
		colonFollowedBy=savedPos;
		savedPos=pos;
		break;
	    case '{':
		/* Since skipend being set here would indicate a syntax error in the code, we assume it isn't.*/
		/* fall through */
	    case '}':
		if(skipend)
		    pos= cpptext_BackwardSkipDelimited(self,pos,'{','}');
		/* fall through */
	    case ';':
	    case EOF:
		if(skipend){
		    skipend=FALSE;
		    savedPos=pos;
		}else if(onestatement){
		    int cin=cpptext_CurrentIndent(self,savedPos+1);
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
		    long tpos= cpptext_SkipWhitespace(self, cpptext_GetBeginningOfLine(self,savedPos+1), cpptext_GetLength(self));
		    if(match(self,tpos,"case") || match(self,tpos,"default"))
			labelExtra+= self->header.ctext.switchLabelUndent;
		    return cpptext_CurrentIndent(self,savedPos+1)+labelExtra+braceExtra;
		}
		break;
	    case ')':
		pos= cpptext_BackwardSkipDelimited(self,pos,'(',')');
		/* save to do continuation lines correctly */
		parensFollowedBy=savedPos;
		savedPos=pos;
		break;
	    case ']':
		pos= cpptext_BackwardSkipDelimited(self,pos,'[',']');
		savedPos=pos;
		break;
	    case '(':
	    case '[':
		return cpptext_CurrentColumn(self, cpptext_SkipWhitespace(self, pos+2, savedPos+1));
	    default:
                savedPos=pos;
	}
    }
}
