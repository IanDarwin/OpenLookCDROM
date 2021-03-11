/* $Author: rr2b $ */

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/util/RCS/compat.c,v 1.3 1992/12/15 21:50:19 rr2b R6tape $";
#endif


 

/*************************************************
 * GNU Emacs Compatiblity package.
 *************************************************/

/*************************************************
 * Copyright (C) 1990 by the Massachusetts Institute of Technology
 * Permission to use, copy, modify, distribute, and sell this
 * software and its documentation for any purpose is hereby
 * granted without fee, provided that the above copyright notice
 * appear in all copies and that both that copyright notice and
 * this permission notice appear in supporting documentation,
 * and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without
 * specific, written prior permission.  M.I.T. makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied
 * warranty.
 *************************************************/

/* Contains the following functions to make ez compatible with */
/* GNU Emacs: */			

/* Fill Paragraph:  Removes hard newlines within a paragraph */

#include <andrewos.h>

#include <ctype.h>

#include <class.h>

#include <bind.ih>
#include <text.ih>
#include <textv.ih>
#include <im.ih>
#include <message.ih>
#include <environ.ih>
#include <envrment.ih>
#include <stylesht.ih>
#include <style.ih>
#include <txtstvec.h>

#include <compat.eh>

#define DIALOG 100
#define MESSAGE 0

/* Routines appear in this file in "bottom-up" order. */
/* This is so I don't have to deal with declaring forward */
/* references. */

/* Added friendly read-only behavior from txtvcmds.c */

static boolean ConfirmReadOnly(self, txt)
struct textview *self;
struct text *txt;
{
    if (text_GetReadOnly(txt)) {
        message_DisplayString(self, 0,
          "Document is read only.");
        return TRUE;
    } else
        return FALSE;
}


/* Travel backward until we get a bonafide paragraph break. */
/* Return buffer position of first character of this paragraph */
/* First character is defined as first non-whitespace char */
/* unless we are at the end of the buffer, in which case */
/* we point at the end of the buffer even though it be whitespace */

int back_to_start (txt, pos)
struct text *txt;
int pos;
{
    int cur;

    while (pos >=0 ) {
	cur = text_GetChar(txt, pos);
	if (cur == '\n') {
	    if (pos == 0) break;
	    else if (text_GetChar(txt, pos - 1)	== '\n')
		break;
	    else if (pos == text_GetLength (txt))
		break;
	    else if ((cur = text_GetChar(txt, pos + 1))	== '\t')
		return (pos + 1);
	    else if (cur == ' ') return (pos + 1);
	}
	pos--;
    }
    /* Skip over newline if we're on it. */
    if (pos == text_GetLength (txt)) return ((pos>0) ? pos-1 : 0);
    else if (text_GetChar(txt, pos) == '\n') return (pos + 1);
    else return pos;
}
	
/* Fill a paragraph.*/

/* This algorithm is careful to not damage an already filled */
/* paragraph. */

/* Here is the paragraphing algorithm: */
/* Backslash-newline is converted to a space */
/* Lines that begin with whitespace are preceded by a newline. */
/* Lines that contain tab or triple space after a non-white */
/*   character are preceded and followed by newlines. */
/* An empty line is given a real newline before and after. */
/* Sentence enders are : ; ? . !  If they occur at end of line,*/
/*   they will be followed by at least two blanks. */
/*  (Same if followed by quote or right parenthesis.) */

static void gcparafill (self, key)
struct textview *self;
long key;
{
    struct text *txt = (struct text *)self->header.view.dataobject;
    int pos, len, cur, npos, count, nextc, end;
    int one_only = 0, modified = 0, found_table;

    if (ConfirmReadOnly(self, txt))
        return;

    pos = textview_GetDotPosition(self);
    len = textview_GetDotLength(self);

    end = pos + len;

    if (len == 0) {
	/* No region, fill current paragraph. */
	pos = back_to_start (txt, pos);
	end = text_GetLength (txt);
	len = end - pos;
	one_only = 1;
    }

    while (pos < end) {
	/* for each newline */
	cur = text_GetChar(txt, pos);
	if (cur == '\n') {
	    /* If this is the last char in the region */
	    /* we're done */
	    if (pos + 1 == end)
		break;
	    /* keep newline preceded by newline */
	    nextc = text_GetChar(txt, pos + 1);
	    if ((pos > 0) && (text_GetChar(txt, pos-1) == '\n')) {
		pos++;
		if (one_only) break;
	    }
	    /* or followed by tab (save char in cur for next else) */
	    else if (nextc == '\t') {
		pos++;
		if (one_only) break;
	    }
	    /* or followed by space */
	    else if (nextc == ' ') {
		pos++;
		if (one_only) break;
	    }
	    /* or followed by a newline */
	    else if (nextc == '\n') {
		pos++;
		if (one_only) break;
	    }
	    /* replace backslash newline pair with a space */
	    else if ((pos > 0) &&
		     (text_GetChar(txt, pos-1) == '\\')) {
		text_ReplaceCharacters(txt, pos-1, 2, " ", 1);
		len--;
		end--;
		modified = 1;
	    }
	    else {
		npos = pos + 1;
		found_table = 0;
		while ((npos <= end) &&
		       ((cur = text_GetChar(txt, npos)) != '\n')){
		    if ((cur == '\t') ||
			((cur == ' ') &&
			 (text_GetChar(txt, npos+1 ) == ' ') &&
			 (text_GetChar(txt, npos+2 ) == ' '))) {
			found_table = 1;
		    }
		    npos++;
		}
		if ((cur == '\n') && (found_table == 1)) {
		    /* we're in a table keep newline at pos */
		    /* and process the one at npos */
		    pos = npos;
		    if (one_only) break;
		}
		else {
		    /* At last!  Our purpose for existance! */
		    /* compress whitespace */
		    count = 0;
		    while ((pos > 0) &&
			   ((cur = text_GetChar(txt, pos-1) == ' '))) {
			    pos--;
			    count++;
		    }
		    if (count>0) {
			text_DeleteCharacters(txt, pos, count);
			modified = 1;
			len -= count;
			end -= count;
		    }
		    /* find out how many spaces to replace */
		    /* newline with: punctuation followed by these: */
		    switch (cur) {
			case '\'':
			case '\\':
			case '"':
			case '}':
			case ']':
			case ')':
			    cur = text_GetChar(txt, pos-2);
			    break;
		    }
		    /* consistinge of these: */
		    switch (cur) {
			case '.':
			case ':':
			case ';':
			case '?':
			case '!':
			    /* get two spaces. */
			    text_ReplaceCharacters(txt, pos, 1, "  ", 2);
			    len++;
			    end++;
			    modified = 1;
			    break;
			default:
			    /* otherwise one space */
			    text_ReplaceCharacters(txt,	pos, 1,	" ", 1);
			    modified = 1;
		    }
		    pos = npos;
		}
	    }
	}
	else pos++;
    }
    if (modified) {
	text_NotifyObservers(txt, 0);
	if (!one_only) textview_SetDotLength (self, len);
    }
}

/*
 * Is what we're pointing at an item tag?
 *
 * It will return the count of characters scan'ed.
 * If this is not an item tag, the count is returned as zero.
 * A tag is currently defined as "7 ".
 * We could also test for the symbola environment, but users
 * might have done plainest before discovering de-itemize.
 */

static int is_itemp (txt, start, end)
struct text *txt;
int start, end;
{
    long c;

    if ((text_GetChar(txt, start) == '7') &&
	 (start < end) &&
	 ((c = text_GetChar(txt, start + 1)) == ' ' || c == '\t')) return 2;
    else return 0;
}


/*
 * itemize
 *
 * Put a bullet in front of every paragraph in the given region.
 * (Unless one is already there.)
 * If no region is given, put one at the begining of the current
 * paragraph
 */

static void insertBullets (self, key)
struct textview *self;
long key;
{
    struct text *txt = (struct text *)self->header.view.dataobject;
    struct style *style;
    
    int dot, pos, len, end;
    long cur, indent, left;
    int one_only = 0, modified = 0;
    struct text_statevector sv;

    if (ConfirmReadOnly(self, txt))
        return;

    style = stylesheet_Find(txt->styleSheet, "symbola");
    if (style == NULL)  {
        style = style_New();
	style_SetFontFamily (style, "AndySymbola");
	style_SetName(style, "symbola");
        stylesheet_Add(txt->styleSheet, style); 
    }

    dot = pos = textview_GetDotPosition(self);
    len = textview_GetDotLength(self);

    end = pos + len;
    (void) textview_GetStyleInformation(self, &sv, pos, NULL);
    indent = sv.CurIndentation;
    left = sv.CurLeftMargin;

    /* if len is zero, do current paragraph */
    if (len == 0) {
	/* back up to begining of paragraph */
	if (pos > 0) pos--;
	while (pos > 0 && (text_GetChar(txt, pos - 1)) != '\n') pos--;
	end = text_GetLength (txt);
	len = end - pos;
	one_only = 1;
    }

    while (pos < end) {
	cur = text_GetChar(txt, pos);
	if (cur == ' ' || cur == '\n' || cur == '\t') {
	    pos++;
	    continue;	/* go to first non-whitespace character */
	}
	else {
	    /* Only itemize lines at the same level */
	    (void) textview_GetStyleInformation(self, &sv, pos, NULL);
	    if (sv.CurLeftMargin == left && sv.CurIndentation == indent)
		if (is_itemp(txt, pos, end) == 0) {
		    /* Not already itemized */
		    unsigned char c = (unsigned char) cur;

		    text_AlwaysInsertCharacters(txt, pos+1, &c, 1);
		    text_AlwaysInsertCharacters(txt, pos+1, "7\t", 2);
		    text_AlwaysDeleteCharacters(txt, pos, 1);
		    environment_WrapStyle(txt->rootEnvironment,	pos, 1, style);
		    modified = 1;
		    len += 2;
		    end += 2;
		}
	}
	if (one_only) break;
	/* go to end of paragraph */
	while (pos < end) {
	    pos++;	/* always move at least one character */
	    if (text_GetChar(txt, pos) == '\n') break;
	}
	pos++;
    }
    if (modified) {
	text_NotifyObservers(txt, 0);
	if (!one_only) {
	    textview_SetDotPosition (self, dot);
	    textview_SetDotLength (self, len);
	}
    }
}

static void removeBullets (self, key)
struct textview *self;
long key;
{
    struct text *txt = (struct text *)self->header.view.dataobject;
    struct text_statevector sv;
    int pos, count, len, end;
    long cur, indent, left;
    int one_only = 0, modified = 0;

    if (ConfirmReadOnly(self, txt))
        return;

    pos = textview_GetDotPosition(self);
    len = textview_GetDotLength(self);

    end = pos + len;

    /* if len is zero, do current paragraph */
    if (len == 0) {
	/* back up to begining of paragraph */
	if (pos > 0) pos--;
	while (pos > 0 && (text_GetChar(txt, pos - 1)) != '\n') pos--;
	end = text_GetLength (txt);
	len = end - pos;
	one_only = 1;
    }

    (void) textview_GetStyleInformation(self, &sv, pos, NULL);
    indent = sv.CurIndentation;
    left = sv.CurLeftMargin;

    while (pos < end) {
	cur = text_GetChar(txt, pos);
	if (cur == ' ' || cur == '\n' || cur == '\t') {
	    pos++;
	    continue;	/* go to first non-whitespace character */
	}
	else {
	    (void) textview_GetStyleInformation(self, &sv, pos, NULL);	    
	    if (sv.CurLeftMargin == left && sv.CurIndentation == indent)
		if ((count = is_itemp (txt, pos, end)) > 0) {
		    text_DeleteCharacters(txt, pos, count);
		    end -= count;
		    len -= count;
		    modified = 1;
		}
	}
	if (one_only) break;
	/* go to end of paragraph */
	while (pos < end) {
	    pos++;	/* always move at least one character */
	    if (text_GetChar(txt, pos) == '\n') break;
	}
	pos++;
    }
    if (modified) {
	text_NotifyObservers(txt, 0);
	if (!one_only) textview_SetDotLength (self, len);
    }
}
/*
 * Parse a number out of a text object.
 *
 * Call this with start pointing at the first digit.
 * It will store the number parsed in numret.
 * It will return the count of characters scan'ed.
 * If the number is not immediately followed by a 
 * period and then a space or tab, the count is
 * returned as zero
 * signifying that this number is to be ignored.
 */

static int parse_num (txt, start, end, numret)
struct text *txt;
int start, end, *numret;
{
    int cur_num = 0, count = 0;
    long cur;

    while (start < end && isdigit (cur = text_GetChar(txt, start))) {
	cur_num = 10 * cur_num + (cur - '0');
	start++;
	count++;
    }
    if (count == 0) return 0;
    /* skip over trailing period and whitespace if present */
    if (start < end && (cur = text_GetChar(txt, start))	!= '.') return 0;	/* wrong format */
    if (start < end && ((cur = text_GetChar(txt, start+1))
			 != ' ' && cur != '\t')) 
	return 0;	/* wrong format */
    count += 2;
    *numret = cur_num;
    return count;
}

/*
 * enumerate
 *
 * Put a number in front of every paragraph in the given region.
 *
 * The tricky part is deciding how to sequence the numbers.
 */

static void enumerate (self, key)
struct textview *self;
long key;
{
    struct text *txt = (struct text *)self->header.view.dataobject;
    struct text_statevector sv;
    int dot, pos, npos, count, tlen, len, end;
    long cur, indent, left;
    int one_only = 0, modified = 0;
    int cur_num, the_number = 0;
    int found_the_number = 0;
    char numstring[20];

    if (ConfirmReadOnly(self, txt))
	return;

    removeBullets(self, 0L); /* Enumeration overrides bullets */

    dot = pos = textview_GetDotPosition(self);
    len = textview_GetDotLength(self);

    end = pos + len;

    /* if len is zero, do current paragraph */
    if (len == 0) {
	/* back up to begining of paragraph */
	if (pos > 0) pos--;
	while (pos > 0 && (text_GetChar(txt, pos - 1)) != '\n') pos--;
	end = text_GetLength (txt);
	len = end - pos;
	one_only = 1;

	/* See if the previous paragraph begins with a number. If so, set the_number */
	npos = pos;
	if (npos > 0) npos--; /* back up over newline we just saw */

	/* back up over additional whitespace between paragraphs */
	while (npos > 0) {
	    cur = text_GetChar(txt, npos);
	    if (!(cur == ' ' || cur == '\n' || cur == '\t'))
		break;
	    npos--;
	    continue;
	}
	    
	/* Now go to begining of the paragraph */
	while (npos > 0 && (text_GetChar(txt, npos - 1)) != '\n') npos--;
	if (parse_num (txt, npos, pos, &cur_num) > 0) {
	    found_the_number = 1;
	    the_number = cur_num + 1;
	}
    }
    
    (void) textview_GetStyleInformation(self, &sv, pos, NULL);
    indent = sv.CurIndentation;
    left = sv.CurLeftMargin;

    while (pos < end) {
	cur = text_GetChar(txt, pos);
	if (cur == ' ' || cur == '\n' || cur == '\t') {
	    pos++;
	    continue;	/* go to first non-whitespace character */
	}
	else {
	    (void) textview_GetStyleInformation(self, &sv, pos, NULL);
	    if (sv.CurLeftMargin == left && sv.CurIndentation == indent)
		if ((count = parse_num (txt, pos, end, &cur_num)) > 0) {
		    if (found_the_number) {
			if (the_number != cur_num) {
			    sprintf (numstring, "%d.\t", the_number);
			    tlen = strlen(numstring);
			    text_ReplaceCharacters(txt, pos, count, numstring, tlen);
			    count = tlen - count;
			    end += count;
			    len += count;
			    pos += tlen;
			    modified = 1;
			}
			the_number++;
		    } else {
			the_number = cur_num + 1;
			found_the_number = 1;
			pos += count;
		    }
		} else {
		    unsigned char c = (unsigned char) cur;

		    if (!found_the_number) {
			/* There was no digit seen Set the_number */
			the_number = 1;
			found_the_number = 1;
		    }
		    /* this paragraph has no number, add it */
		    sprintf (numstring, "%d.\t", the_number);
		    tlen = strlen(numstring);
		    text_AlwaysInsertCharacters(txt, pos+1, &c, 1);
		    text_AlwaysInsertCharacters(txt, pos+1, numstring, tlen);
		    text_AlwaysDeleteCharacters(txt, pos, 1);
		    pos += tlen;
		    len += tlen;
		    end += tlen;
		    modified = 1;
		    the_number++;
		}
	}
	if (one_only) break;
	/* go to end of paragraph */
	while (pos < end) {
	    pos++;	/* always move at least one character */
	    if (text_GetChar(txt, pos) == '\n') break;
	}
	pos++;
    }
    if (modified) {
	text_NotifyObservers(txt, 0);
	if (!one_only) {
	    textview_SetDotPosition (self, dot);
	    textview_SetDotLength (self, len);
	}
    }
}

static void denumerate (self, key)
struct textview *self;
long key;
{
    struct text *txt = (struct text *)self->header.view.dataobject;
    struct text_statevector sv;
    int pos, count, len, end;
    long cur, indent, left;
    int one_only = 0, modified = 0;
    int cur_num;

    if (ConfirmReadOnly(self, txt))
        return;

    pos = textview_GetDotPosition(self);
    len = textview_GetDotLength(self);

    end = pos + len;

    /* if len is zero, do current paragraph */
    if (len == 0) {
	/* back up to begining of paragraph */
	if (pos > 0) pos--;
	while (pos > 0 && (text_GetChar(txt, pos - 1)) != '\n') pos--;
	end = text_GetLength (txt);
	len = end - pos;
	one_only = 1;
    }

    (void) textview_GetStyleInformation(self, &sv, pos, NULL);
    indent = sv.CurIndentation;
    left = sv.CurLeftMargin;

    while (pos < end) {
	cur = text_GetChar(txt, pos);
	if (cur == ' ' || cur == '\n' || cur == '\t') {
	    pos++;
	    continue;	/* go to first non-whitespace character */
	}
	else {
	    (void) textview_GetStyleInformation(self, &sv, pos, NULL);
	    if (sv.CurLeftMargin == left && sv.CurIndentation == indent)
		if ((count = parse_num (txt, pos, end, &cur_num)) > 0) {
		    text_DeleteCharacters(txt, pos, count);
		    end -= count;
		    len -= count;
		    modified = 1;
		}
	}
	if (one_only) break;
	/* go to end of paragraph */
	while (pos < end) {
	    pos++;	/* always move at least one character */
	    if (text_GetChar(txt, pos) == '\n') break;
	}
	pos++;
    }
    if (modified) {
	text_NotifyObservers(txt, 0);
	if (!one_only) textview_SetDotLength (self, len);
    }
}

/*
 * Explicit object insertion code.
 *
 * This is so we can make menu items to insert objects
 * by name rather than prompting and asking for them.
  * for example:
      addmenu gnucompat-insert-inset "Special~4,Insert equation~41" textview gnucompat inherit "eq"
      addmenu gnucompat-insert-inset "My Cart, Insert Eq" textview gnucompat inherit "eq"
 * The flag is true if we should make sure there are newlines around the inset.
 */

static void
do_insert(self,typename, nl_flag)
struct textview *self;
char *typename;
boolean nl_flag;
{
    struct text *txt;
    char buf[80];
    int result;
    long pos;

    if ((txt = (struct text *) textview_GetDataObject(self)) == NULL) return;

    if (ConfirmReadOnly(self, txt))
	return;

    if(text_GetObjectInsertionFlag(txt) == FALSE){
	message_DisplayString(self, 0, "Object Insertion Not Allowed!");
	return;
    }
    /* Check to see if we are interactive or not... njw*/
    if (!typename || (int) typename < 256 /* Sigh */) {
	/* This is a bit of overkill, using completedString...
	 * But at some point, it might be nice to have either
	 * completion or help...
	 */
	result = message_AskForStringCompleted(self,
					       0,
					       "Object to insert: " ,
					       NULL, /* no default */
					       buf,
					       sizeof(buf),
					       NULL, /* no special keymap */
					       NULL,/* no completion */
					       NULL, /* no help */
					       0L,
					       message_NoInitialString);
	if (result<0) {
	    message_DisplayString(self, 0, "Object insertion cancelled.");
	    return;
	}
	typename = buf;
    }

    pos = textview_GetDotPosition(self) + textview_GetDotLength(self);
    /* for user convenience, if we don'txt have newlines before and after, we insert them. */
    if (nl_flag) {
	if (text_GetChar(txt, pos) != '\n') {
	    text_InsertCharacters(txt, pos, "\n", 1);
	}
	if (text_GetChar(txt, pos -	1) != '\n'){
	    text_InsertCharacters(txt, pos, "\n", 1);
	    pos++;
	}
    }
    self->currentViewreference = text_InsertObject(txt, pos, typename, NULL);
    text_NotifyObservers(txt, 0);
}

static void
insert(self,typename)
struct textview *self;
char *typename;
{
    do_insert (self, typename, TRUE);
}

static void
insert_no_nl(self,typename)
struct textview *self;
char *typename;
{
    do_insert (self, typename, FALSE);
}

static void
AddDefaultTpl(self, l)
struct textview *self;
long l;
{
    struct text *txt;

    if ((txt = (struct text *) textview_GetDataObject(self)) == NULL) return;

    if (ConfirmReadOnly(self, txt))
        return;

    if (text_ReadTemplate(txt, "default", text_GetLength(txt) == 0) < 0)
        message_DisplayString(self, 100, "Could not read template file.");
    else {
	text_RegionModified(txt, 0, text_GetLength(txt));
	text_NotifyObservers(txt, 0);
        message_DisplayString(self, 0, "Enabled Basic Styles.");
    }
}



boolean gnucompat__InitializeClass(classID)
struct classheader *classID;
{
    static struct bind_Description compat_fns[] = {
	{"gnucompat-fill-para", NULL, 0, NULL, 0, 0, gcparafill, "Fill Paragraph.", "gnucompat"},
	{"gnucompat-insert-bullets", NULL, 0, NULL, 0, 0, insertBullets, "Put a bullet at the begining of each paragraph in the region", "gnucompat"},
	{"gnucompat-remove-bullets", NULL, 0, NULL, 0, 0, removeBullets, "Remove bullets from the begining of each paragraph in the region", "gnucompat"},
	{"gnucompat-enumerate", NULL, 0, NULL, 0, 0, enumerate, "Put number in sequence at the begining of each paragraph in the region", "gnucompat"},
	{"gnucompat-denumerate", NULL, 0, NULL, 0, 0, denumerate, "Delete number from the begining of each paragraph in the region", "gnucompat"},
	/* Added by njw so that you can keybind all this junk: */
	{"gnucompat-insert-inset", NULL, 0, NULL, 0, 0, insert, "Insert a named inset here", "gnucompat"},
	{"gnucompat-insert-inset-no-nl", NULL, 0, NULL, 0, 0, insert_no_nl, "Insert a named inset without newlines around it", "gnucompat"},
	{"gnucompat-add-default-tpl", NULL, 0, NULL, 0, 0, AddDefaultTpl, "Enable basic styles by adding the template named default", "gnucompat"},

        {NULL},
    };
    struct classinfo *textviewClassinfo;

    textviewClassinfo = class_Load("textview");
    if (textviewClassinfo != NULL) {
        bind_BindList(compat_fns, NULL, NULL, textviewClassinfo);
        return TRUE;
    }
    else
        return FALSE;
}
