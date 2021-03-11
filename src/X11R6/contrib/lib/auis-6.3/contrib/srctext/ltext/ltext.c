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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/srctext/ltext/RCS/ltext.c,v 2.13 1992/12/15 21:56:26 rr2b R6tape $";
#endif

/* LText, a ``Lisp'' mode for BE2. */


#include <class.h>
#include <andrewos.h> /* strings.h */
#include <setjmp.h>
#include <ctype.h>

#include <environ.ih>
#include <envrment.ih>
#include <stylesht.ih>
#include <style.ih>
#include <attribs.h>
#include <nstdmark.ih>
#include <tree23.ih>
#include <proctbl.ih>

#include <ltext.eh>

#define LOW(ch) (isupper(ch)?tolower(ch):(ch))

#define new(type) (type *)malloc(sizeof(type))

static struct indent {
    char *name;
    int args;
    struct indent *next;
} *Indents = NULL;

/* Varables used to control the indenting style. */
boolean useTabs = TRUE;

void ltext__SetAttributes(self,atts)
struct ltext *self;
struct attributes *atts;
{
    char *key;
    super_SetAttributes(self,atts);

    while(atts != NULL){
	if(strncmp(atts->key, "ltext-", 6) == 0){
	    key = atts->key+6;

            if (strcmp(key, "use-tabs")==0)
		useTabs=atoi(atts->value.string);
	}

	atts = atts->next;
    }
}

static void SetupStyles(self)
    struct ltext *self;
{
    self->comment_style = stylesheet_Find(self->header.text.styleSheet, "comment");
    self->fnname_style = stylesheet_Find(self->header.text.styleSheet, "function");

    ltext_SetGlobalStyle(self, stylesheet_Find(self->header.text.styleSheet, "global"));
}

boolean ltext__InitializeObject(classID, self)
    struct classheader *classID;
    struct ltext *self;
{
    struct attributes templateAttribute;

    templateAttribute.key = "template";
    templateAttribute.value.string = "ltext";
    templateAttribute.next = NULL;
    ltext_SetAttributes(self, &templateAttribute);
    ltext_SetExportEnvironments(self, FALSE);

    SetupStyles(self);
    return TRUE;
}

void ltext__Clear(self)
struct ltext *self;
{
    super_Clear(self); /* This destroyes all styles in the stylesheet. */

    SetupStyles(self);
}

long ltext__Read(self, file, id)
struct ltext *self;
FILE *file;
long id;
{
    long tmpRetValue;

    tmpRetValue = super_Read(self, file, id);
    if (tmpRetValue == dataobject_NOREADERROR) {
        SetupStyles(self);
        ltext_RedoStyles(self);
    }
    return tmpRetValue;
}

/****************************************************************
 *
 *  Style parsing code.
 *
 ****************************************************************/


static void DoFreeTree(self)
struct nestedmark *self;
{
    nestedmark_FreeTree(self);
}

void ltext__RedoStyles(self)
struct ltext *self;
{
    struct nestedmark *root = (struct nestedmark *)self->header.text.rootEnvironment;
    struct environment *newenv;
    long posn, len = ltext_GetLength(self);
    enum {normal, comment, string, vertbars} state = normal;
    enum {nothing, justin, foundD, foundE} substate = nothing;
    enum {nodef, indef, inwhitespace, inname} defwhatever = nodef;
    int parens = 0, c;
    boolean quoted = FALSE;
    long commentpos = 0, fnpos = 0;

    /* Remove the old styles, but leave the root environemt in place. */
    if (root->children) {
        tree23int_Apply(root->children, DoFreeTree);
        tree23int_Destroy(root->children);
        root->children = NULL;
    }

    /* Two things of interest:
      1 - semicolons. Can't be quoted or inside of vert bars.
      2 - "def". must be first thing after top level paren.
     */

    for (posn = 0; posn < len; posn++) {
        c = ltext_GetChar(self, posn);
        switch (state) {
            case normal:
                switch (substate) {
                    case nothing:
                        break;
                    case justin:
                        switch (c) {
                            case ' ':
                            case '\t':
                                break;
                            case 'd':
                            case 'D':
                                substate = foundD;
                                break;
                            default:
                                substate = nothing;
                                break;
                        }
                        break;
                    case foundD:
                        if (c == 'e' || c == 'E')
                            substate = foundE;
                        else
                            substate = nothing;
                        break;
                    case foundE:
                        if (c == 'f' || c == 'F') {
                            defwhatever = indef;
                            fnpos = -1;
                        }
                        substate = nothing;
                        break;
                }
                switch (defwhatever) {
                    case nodef:
                        break;
                    case indef:
                        switch (c) {
                            case ' ':
                            case '\t':
                            case '\n':
                                defwhatever = inwhitespace;
                                break;
                            default:
                                break;
                        }
                        break;
                    case inwhitespace:
                        switch (c) {
                            case ' ':
                            case '\t':
                            case '\n':
                                break;
                            case ';':
                                if (!quoted)
                                    break;
                                /* else fall through */
                            default:
                                defwhatever = inname;
                                fnpos = posn;
                                break;
                        }
                        break;

                    case inname:
                        if (c <= ' ' || c == '(' || c == ')' || c == ';') {
                            newenv = environment_InsertStyle(self->header.text.rootEnvironment, fnpos, self->fnname_style, 1);
                            environment_SetLength(newenv, posn - fnpos);
                            defwhatever = nodef;
                        }
                        break;
                }

                if (!quoted)
                    switch (c) {
                        case '(':
                            if (parens++ == 0)
                                substate = justin;
                            defwhatever = nodef;
                            break;
                        case ')':
                            parens--;
                            defwhatever = nodef;
                            break;
                        case '|':
                            state = vertbars;
                            break;
                        case '"':
                            state = string;
                            defwhatever = nodef;
                            break;
                        case ';':
                            commentpos = posn;
                            state = comment;
                            break;
                        default:
                            break;
                    }
                break;

            case comment:
                substate = nothing;
                if (c == '\n') {
                    newenv = environment_InsertStyle(self->header.text.rootEnvironment, commentpos, self->comment_style, 1);
                    environment_SetLength(newenv, posn - commentpos);
                    state = normal;
                }
                break;

            case string:
                substate = nothing;
                if (c == '"' && !quoted)
                    state = normal;
                break;

            case vertbars:
                substate = nothing;
                if (c == '|' && !quoted)
                    state = normal;
                break;
        }
        if (c == '\\')
            quoted = !quoted;
        else
            quoted = FALSE;
    }
}

 
/****************************************************************
 *
 * Paren balancer.
 *
 ****************************************************************/

/* Things that need to be delt with:
    Comments: start anywhere on a line and go to the end of the line.
    Quotes: between sets of ``|''
    Strings: Double Quotes. Can span lines. Backslash quotes.
    Paren-Types: Normal, Curly, Square. No Angle braces.

  Dealing with Quotes is hard, so it is punted until I think of something that will work. 
  */

struct paren_node {
    long type;
    struct paren_node *next;
};

static boolean Quoted(self, pos)
struct ltext *self;
long pos;
{
    /* returns true iff the character at pos is quoted (ie. "\"). Takes into account the slash being quoted. (ie "\\"). */

    boolean retval = FALSE;

    pos--;
    while (ltext_GetChar(self, pos) == '\\' && pos > 0) {
        retval = !retval;
	pos--;
    }

    return retval;
}

long ltext__ReverseBalance(self, pos, type)
struct ltext *self;
long pos;
int type;
{
    /* Balance parens backwards. If type is EOF, scan back to the start of the previous paren pair, otherwise scan back to the matching open paren. Never look at characters positioned at "pos" or later. */

    boolean found = FALSE, instring = FALSE, atleastone = FALSE;
    int thischar, prechar;
    char *parentype;
    struct paren_node *parenstack = NULL;
    static char *opens = "({[", *closes = ")}]";

    if (type == EOF)
        if (pos > 0)
            prechar = ltext_GetChar(self, --pos);
        else
            return EOF;
    else {
        if (index(closes, type) == NULL)
            /* Don't know anything about this type of paren. */
            return EOF;
        prechar = type;
    }

    while ((parenstack != NULL || !atleastone) && (pos >= 0)) {
        thischar = prechar;
        if (pos-- > 0)
            prechar = ltext_GetChar(self, pos);

        if (thischar == '\n') {
            /* Check for a comment. */
            long newline, semi = -1;
            int c;

            /* Scan backwards until we find a newline, checking for a ';' */
            for (newline = pos; newline >= 0 && (c = ltext_GetChar(self, newline)) != '\n'; newline--)
                if (c == ';' && !Quoted(self, newline))
                    semi = newline;

            /* Found a semi. */
            if (semi >= 0) {
                prechar = (pos = semi) > 0 ? ltext_GetChar(self, pos - 1) : 0;
                continue;
            }
        }

	if (!Quoted(self, pos)) {
            if (instring) {
                if (thischar == '"')
                    instring = FALSE;
            }
            else if (thischar == '"') {
                instring = TRUE;
            }
            else if ((parentype = index(opens, thischar)) != NULL) {
                if (parenstack == NULL || parenstack->type != (parentype - opens)) {
                    break;
                }
                else {
                    struct paren_node *temp = parenstack;

                    parenstack = parenstack->next;
                    free(temp);
                    if ((prechar == '\n' || pos <= 0) && parenstack != NULL) {
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
	return pos + 1;
    }
    else {
	return EOF;
    }
}

/*****************************************************************
 * Indenter.  
 *****************************************************************/

static int current_column(self,pos)
struct ltext *self;
long pos;
{
    int ind=0, oldPos=pos;

    while (pos >= 0 && (ltext_GetChar(self,pos)) != '\n')
        pos--;
    pos++;

    while (pos < oldPos)
        if (ltext_GetChar(self,pos++)=='\t')
            ind = (ind+8)&~7;
        else
            ind++;

    return ind;
}

static long nextline(self, pos)
struct ltext *self;
long pos;
{
    /* Returns the start of the line following this one, or EOF if this line doesn't end. */

    int c;

    while ((c = ltext_GetChar(self, pos)) != EOF && c != '\n')
        pos++;

    if (c == EOF)
        return EOF;
    else
        return pos+1;
}

static boolean inmark(mark, pos)
struct mark *mark;
long pos;
{
    /* Returns true if the pos is inside the mark. */

    if (pos < mark_GetPos(mark) || pos > mark_GetEndPos(mark))
        return FALSE;
    else
        return TRUE;
}

static long findsexpr(self, pos, limit)
struct ltext *self;
long pos, limit;
{
    /* Returns the start of the next sexpr before limit. Takes into account comments and ws. */

    int c;

    while (pos < limit && (c = ltext_GetChar(self, pos)) != EOF)
        if (c == ';' && !Quoted(self, pos))
            while ((c = ltext_GetChar(self, ++pos)) != EOF && c != '\n')
                ;
        else
            if (isspace(c))
                pos++;
            else
                return pos;

    return EOF;
}

static long skipsexpr(self, pos, limit)
struct ltext *self;
long pos, limit;
{
    /* Returns the first position after the end of the current sexpr. */

    int c;
    long nextexpr;

    switch (ltext_GetChar(self, pos)) {
        case '(':
            /* It's a list. */
            pos++;
            while (pos < limit) {
                nextexpr = findsexpr(self, pos, limit);
                if (nextexpr < 0 || nextexpr >= limit)
                    return EOF;
                if (ltext_GetChar(self, nextexpr) == ')')
                    return nextexpr + 1;
                pos = skipsexpr(self, nextexpr, limit);
            }
            break;

        case '\"':
            /* It's a string. */
            while (pos++ < limit && (c = ltext_GetChar(self, pos)) != EOF)
                if (c == '\"' && !Quoted(self, pos))
                    return pos;
            break;

        case '.':
            /* Might be a dotted pair. */
            c = ltext_GetChar(self, pos+1);
            if (c > ' ' && c != '(' && c != ')' && !Quoted(self, pos))
                return pos+1;
            /* Fall through. */
        default:
            /* Skip until ws, delimeter, or comment. */
            while (pos < limit && (c = ltext_GetChar(self, pos)) != EOF)
                if (isspace(c) || ((c == ';' || c == '(' || c == ')') && !Quoted(self, pos)))
                    return pos;
                else
                    pos++;
            break;
    }

    return EOF;
}

long indent(self, pos)
struct ltext *self;
long pos;
{
    /* Return the correct indentation for the line containing pos assuming nothing exists beyond pos. Note: Assumed that the text object is not modified while we are doing our stuff so that longs can be used instead of marks. */

    struct indent *ind;
    long linestart, sexprstart, functionstart, functionend, firstarg, argstart;
    boolean comment = FALSE;
    int c, args;
    char buf[256], *ptr;

    /* Find the start of the line, checking to see if we are in a left-flush comment. */
    while (pos > 0 && (c = ltext_GetChar(self, pos-1)) != '\n') {
        pos--;
        if (!isspace(c))
            comment = (c == ';');
    }
    linestart = pos;

    /* If we are in a comment, make it left-flush */
    if (comment)
        return 0;

    /* Find the start of the sexpr we are in. */
    sexprstart = ltext_ReverseBalance(self, linestart, ')');

    /* If we aren't in an sexpr, see if a previous sexpr can be scanned. If so, assume the same indentation, otherwise assume a top level form. */
    if (sexprstart < 0) {
        sexprstart = ltext_ReverseBalance(self, linestart, EOF);
        if (sexprstart < 0)
            return 0;
        else
            return current_column(self, sexprstart);
    }

    /* Find the start of the first sexpr (the function) inside our parent sexpr, but don't look at anything on this line. */
    functionstart = findsexpr(self, sexprstart+1, linestart);

    /* If we couldn't find one, assume that we will be the function and return one more than the paren. */
    if (functionstart < 0)
        return current_column(self, sexprstart) + 1;

    /* Determine the name of the function. Note: it is impossible for skipexpr to return a position on this line or further. This is because ReverseBalance would not have passed over this sexpr it was not totally before the start of this line. */
    functionend = skipsexpr(self, functionstart, linestart);
    args = -1;
    if (functionend - functionstart < sizeof(buf)) {
        /* Don't bother if the ``function'' is real long, cause then it's probably not a function */
        ptr = buf;
        for (pos = functionstart; pos < functionend; pos++)
            *ptr++ = LOW(ltext_GetChar(self, pos));
        *ptr = '\0';
        for (ind = Indents; ind != NULL; ind = ind->next)
            if (strcmp(buf, ind->name) == 0) {
                args = ind->args;
                break;
            }
    }

    /* If the function wants zero args indented, it's easy. */
    if (args == 0)
        return current_column(self, functionstart) + 1;

    /* Find the start of the second expr (the first arg). */
    argstart = firstarg = findsexpr(self, functionend, linestart);

    /* Try to find more than "args" args. If so, return two more than function indent. */
    if (args > 0)
        while (argstart > 0) {
            if (--args == 0)
                return current_column(self, functionstart) + 1;
            argstart = findsexpr(self, skipsexpr(self, argstart, linestart), linestart);
        }

    /* If the arg doesn't exist, assume that we are going to be it, and return the column of the function name. */
    if (firstarg < 0)
        return current_column(self, functionstart);

    /* Assume that we are another argument, and return the column of the first arg. */
    return current_column(self, firstarg);
}

long ltext__Indent(self, mark)
struct ltext *self;
struct mark *mark;
{
    /* Every line that overlaps the mark is indented. Returns the position of the first non-whitespace character on the last line indented. */

    long pos = mark_GetPos(mark), end;
    int c, in;

    /* Find the start of the first line. */
    while (pos > 0 && (c = ltext_GetChar(self, pos-1)) != '\n')
        pos--;

    while (1) {
        in = indent(self, pos);

        /* Insert spaces for the indentation. */
        pos = ltext_TabAndOptimizeWS(self, pos, in);

        /* Remove extra spaces. */
        for(end = pos; (c = ltext_GetChar(self, end)) == ' ' || c == '\t'; end++)
            ;
        if(end>pos)
            ltext_DeleteCharacters(self, pos, end - pos);

        /* More? */
        end = nextline(self, pos);
        if (inmark(mark, end))
            pos = end;
        else
            return pos;
    }
}

void ltext__ReindentLine(self, pos)
struct ltext *self;
long pos;
{
    /* Indents the line that pos is in. On top of the newline counts as the previous line. Acts as if the text ends at the end of the line. */

    long end;
    int c, in;

    in = indent(self, pos);

    /* Insert spaces for the indentation. */
    pos = ltext_TabAndOptimizeWS(self, pos, in);

    /* Remove extra spaces. */
    for(end = pos; (c = ltext_GetChar(self, end)) == ' ' || c == '\t'; end++)
        ;
}



/*
 * optimized whitespace insertion
 * 7 June 1987 - Miles Bader
 */

static char *spaces="        ";

#define TABOVER(self,oldpos,pos,oldcol,col) \
    if(col>oldcol){\
	ltext_DeleteCharacters(self,oldpos,pos-oldpos);\
	pos=oldpos;\
	if(useTabs){\
	    int nc;\
	    while(col>=(nc=(oldcol+8)&~7)){\
		oldcol=nc;\
		ltext_InsertCharacters(self,pos++,"\t",1);\
	    }\
	}else{\
	    int count=(col-oldcol)/8;\
	    while(count--){\
		ltext_InsertCharacters(self,pos,spaces,8);\
		oldcol+=8;\
		pos+=8;\
	    }\
	}\
	ltext_InsertCharacters(self,pos,spaces,col-oldcol);\
	pos+=(col-oldcol);\
    }

long ltext__TabAndOptimizeWS(self,pos,inc)
struct ltext    *self;
long	pos;
int	inc;
{
    int     home=0, oldPos, col=0, oldCol=0, target;

    while(--pos>=0 && ltext_GetChar(self,pos)!='\n')
        home++;

    oldPos= ++pos;
    while(home--){
        switch(ltext_GetChar(self,pos)){
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
        char	c=ltext_GetChar(self,pos);

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

/* --- */


static void addindent(name, args)
char *name;
int args;
{
    struct indent *newindent = new(struct indent);
    char *ptr;

    ptr = newindent->name = (char *)malloc(strlen(name) + 1);
    while ((*ptr++ = LOW(*name)) != '\0')
        name++;
    newindent->args = args;
    newindent->next = Indents;

    Indents = newindent;
}

boolean ltext__InitializeClass(classID)
struct classheader *classID;
{
    int indent;
    char *t, *t2, *s = environ_GetProfile("ltextindents");

    if (!s) s = "defun:2,defmacro:2,defconstant:1,defstruct:1,let:1,let*:1,do:2,if:1";
    while (isspace(*s)) ++s;
    while (t = s) {
        if (s = index(t, ',')) *s++ = NULL;
        if (t2=index(t, ':')) *t2++=NULL;
        addindent(t, t2 ? atoi(t2) : 1);
    }
      
    proctable_DefineProc("ltext-add-indent", addindent, NULL, "ltext", "Sets the number of args to indent normally.");

    return TRUE;
}
