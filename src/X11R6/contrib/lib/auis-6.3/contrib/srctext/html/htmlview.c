/* $Id: htmlview.c,v 1.7 1994/05/16 15:50:48 rr2b Exp $ */
/*
 * Copyright 1993, City University
 * Copyright 1993, 1994, Nick Williams. 
 * 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * City University, Nick Williams, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 *
 * City University, Nick Williams, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL City University, Nick Williams, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 */

#include <andrewos.h>
#include <class.h>

#include <bind.ih>
#include <buffer.ih>
#include <ctype.h>
#include <envrment.ih>
#include <environ.ih>
#include <im.ih>
#include <keymap.ih>
#include <keystate.ih>
#include <menulist.ih>
#include <message.ih>
#include <proctbl.ih>
#include <textv.ih>
#include <txtstvec.h>
#include <style.ih>
#include <view.ih>

#include <html.ih>
#include <htmlview.eh>

#ifndef MAXPATHLEN
#define MAXPATHLEN 256
#endif

static struct keymap *Keymap;
static struct menulist *Menus;

void htmlview_SetTitle();
void htmlview_SetIndex();
void htmlview_SetLink();
void htmlview_EditAttributes();
void htmlview_unlistify();
void htmlview_itemize();
void htmlview_AddRandom();
void htmlview_AddImage();
void htmlview_AddHrule();
void htmlview_makeList();
void htmlview_SetImage();
void htmlview_modifyList();

static char* bulletChars = "*+";

/* These are bindings to commands only, not the menu bits themselves */
/* Place the menu entry for these commands in the stylesheet */
static struct bind_Description htmlBindings[]={
    {"htmlview-set-title",0,0,0,0, 0,htmlview_SetTitle,"Set title of document."},
    {"htmlview-set-indexable",0,0,0,0, 0,htmlview_SetIndex,"Set flag specifying if document is searchable."},
    {"htmlview-set-link",0,0,0,0,0,htmlview_SetLink,"Turn a region into an active anchor"},
    {"htmlview-edit-attributes",0,0,0,0,0,htmlview_EditAttributes,"Edit the attributes of tag at point"},
    {"htmlview-unlistify", 0,0,0,0,0, htmlview_unlistify,"Remove all listyness about point or region"},
    {"htmlview-add-random", 0,0,0,0,0, htmlview_AddRandom, "Add a random entity to a region of text"},
    {"htmlview-add-image", 0,0,0,0,0, htmlview_AddImage, "Add an image"},
    {"htmlview-add-hrule", 0,0,0,0,0, htmlview_AddHrule, "Add a horizontal rule"},
    {"htmlview-make-list",0,0,0,0,0, htmlview_makeList, "Make a region into a list"},
    {"htmlview-set-img-src",0,0,0,0,0, htmlview_SetImage, "Set the source of an image"},
    {"htmlview-modify-list",0,0,0,0,0, htmlview_modifyList, "Modify an attribute of a list"},
    NULL
};

/* Added friendly read-only behavior from txtvcmds.c */
static boolean ConfirmReadOnly(self, html)
struct htmlview *self;
struct html* html;
{
    if (html_GetReadOnly(html)) {
        message_DisplayString(self, 0,
          "Document is read only.");
        return TRUE;
    } else
        return FALSE;
}

boolean 
htmlview__InitializeClass(classID)
struct classheader *classID;
{
    Menus  = menulist_New();
    Keymap = keymap_New();

    bind_BindList(htmlBindings,Keymap,Menus,&htmlview_classinfo);
    return TRUE;
}


boolean 
htmlview__InitializeObject(classID, self)
struct classheader *classID;
struct htmlview *self;
{
    self->kstate = keystate_Create(self, Keymap);
    self->menus = menulist_DuplicateML(Menus, self);
    htmlview_SetBorder(self,5,5);
    return TRUE;
}

void 
htmlview__FinalizeObject(classID, self)
struct classheader *classID;
struct htmlview *self;
{
    keystate_Destroy(self->kstate);
    menulist_Destroy(self->menus);
}

void
htmlview__PostKeyState(self, keystate)
struct htmlview* self;
struct keystate* keystate;
{
    self->kstate->next = NULL;
    keystate_AddBefore(self->kstate, keystate);
    super_PostKeyState(self, self->kstate);
}

void
htmlview__PostMenus(self, menulist)
struct htmlview* self;
struct menulist* menulist;
{
    menulist_ClearChain(self->menus);

    if (menulist) {
	menulist_ChainAfterML(self->menus, menulist, 0);
    }

    super_PostMenus(self, self->menus);
}

void
htmlview_SetTitle(self, key)
struct htmlview* self;
long key;
{
    char reply[80];
    struct html* h = (struct html*) self->header.view.dataobject;
    if (message_AskForString(self, 0, "New title: ", html_GetTitle(h), reply, sizeof(reply)) < 0) {
	message_DisplayString(self, 0, "Cancelled");
	return;
    }
    html_ChangeTitle(h, reply);
    message_DisplayString(self, 0, "Done.");
}

void
htmlview_SetIndex(self, key)
struct htmlview* self;
long key;
{
    char reply[80];
    struct html* h = (struct html*) self->header.view.dataobject;

    if (message_AskForString(self, 0, "Is the document searchable? [yes/no] ", html_GetIsIndex(h) ? "yes" : "no", reply, sizeof(reply)) < 0) {
	message_DisplayString(self, 0, "Cancelled");
	return;
    }
    html_ChangeIndexable(h, reply[0] == 'y' || reply[0] == 'Y');
    message_DisplayString(self, 0, "Done.");
}

void
htmlview_SetLink(self, key)
struct htmlview* self;
long key;
{
    long pos, len;
    struct html* html = (struct html*) self->header.view.dataobject;
    char uri[MAXPATHLEN];

    pos = htmlview_GetDotPosition(self);
    len = htmlview_GetDotLength(self);
    if (len == 0) {
	/* XXX: set current insertion style... */
	message_DisplayString(self, 0, "Please mark some text first!");
	return;
    }
    if (message_AskForString(self, 100, "URI to link to: ", 0, uri, sizeof(uri)) < 0) {
	message_DisplayString(self, 0, "Cancelled");
	return;
    }

    html_AddLink(html, pos, len, uri);
}

struct view*
htmlview__Hit(self, action, x, y, numberOfClicks)
struct htmlview* self;
enum view_MouseAction action;
long x;
long y;
long numberOfClicks;
{
    struct html* html = (struct html*) self->header.view.dataobject;
    long pos, len;
    char* s;
    struct view* retv = super_Hit(self, action, x, y, numberOfClicks);
    /* Modified based on comments from Tom Neuendorffer, 28/04/94 */
    /* Now returns the correct view object */
    /* Will only determine anchor details if there was a mouse click */

    if (retv == (struct view*) self && 
	(action==view_LeftDown || action==view_RightDown)) {
	pos = htmlview_GetDotPosition(self);
	if (s = html_GetAnchorDest(html, pos)) {
	    message_DisplayString(self, 0, s);
	} else {
	    message_DisplayString(self, 0, "");
	}
    }
    return retv;
}

char* editOptions[] = {
    "Add new variable",
    "Next environment",
    "Done",
    0
};

void
htmlview_EditAttributes(self, key)
struct htmlview* self;
long key;
{
    long pos, len;
    struct html* html = (struct html*) self->header.view.dataobject;
    struct environment* startEnv;
    char* choices[16]; /* 3 internal, that leaves user with max of 13 */
    char answer[MAXPATHLEN];
    char buf[MAXPATHLEN];
    char titlebuf[MAXPATHLEN];
    int i;
    int count;
    long result;

    pos= htmlview_GetDotPosition(self);
    len = htmlview_GetDotLength(self);
    if (len == 0) {
	self->styleInQuestion = html_GetEntityEnvironment(html, pos, 0);
    } else {
	self->styleInQuestion = html_GetEntityEnvironment(html, pos, self->header.textview.displayEnvironment);
    }
    if (!self->styleInQuestion || self->styleInQuestion->type != environment_Style) {
	message_DisplayString(self, 0, "Select an entity first");
	return;
    }

    html_GetAttributeList(html, self->styleInQuestion, choices, &count);
    for (i = 0; i < sizeof(editOptions); i++) {
	choices[count+i] = editOptions[i];
    }
    startEnv = self->styleInQuestion;
    htmlview_SetDotPosition(self, environment_Eval(self->styleInQuestion));
    htmlview_SetDotLength(self, environment_GetLength(self->styleInQuestion));

    sprintf(titlebuf, "Attributes of this %s", self->styleInQuestion->data.style->name);
    while (message_MultipleChoiceQuestion(self, 100, titlebuf, count+2, &result, choices, NULL) >= 0) {
	if (result < count) {
	    /* Build up the prompt */
	    sprintf(buf, "Value for %s: ", choices[result]);
	    if (message_AskForString(self, 100, buf, html_GetAttribute(html, self->styleInQuestion,choices[result]), answer, sizeof(answer)) >= 0) {
		html_ChangeAttribute(html, self, self->styleInQuestion, choices[result], answer);
	    }
	} else if (result == count) {
	    /* ADD VAR */
	    if (message_AskForString(self, 100, "Give attribute (e.g. foo=bar): ", 0, answer, sizeof(answer)) >= 0) {
		char* s = (char*)strchr(answer, '=');
		if (s) {
		    *s++ = '\0';
		}
		html_ChangeAttribute(html, self, self->styleInQuestion, answer, s);
		/* Rebuild the options */
		html_GetAttributeList(html, self->styleInQuestion, choices, &count);
		for (i = 0; i < sizeof(editOptions); i++) {
		    choices[count+i] = editOptions[i];
		}
	    }
	} else if (result == (count+1)) {
	    /* NEXT */
	    /* first, free the old list */
	    for (i = 0; i < count; i++) {
		free(choices[i]);
	    }
	    /* Now find a new environment */
	    do {
		self->styleInQuestion = (struct environment*)self->styleInQuestion->header.nestedmark.parent;
	    } while (self->styleInQuestion->type != environment_Style);
	    /* And cycle back to the start if we get to the root */
	    if (self->styleInQuestion == html->header.text.rootEnvironment) {
		self->styleInQuestion = startEnv;
	    }

	    /* build the new list */
	    html_GetAttributeList(html, self->styleInQuestion, choices, &count);
	    for (i = 0; i < sizeof(editOptions); i++) {
		choices[count+i] = editOptions[i];
	    }
	    htmlview_SetDotPosition(self, environment_Eval(self->styleInQuestion));
	    htmlview_SetDotLength(self, environment_GetLength(self->styleInQuestion));
	    sprintf(titlebuf, "Attributes of this %s", self->styleInQuestion->data.style->name);
	} else {
	    /* DONE */
	    goto endfn;  
	    /*NOTREACHED*/
	}
    }
    message_DisplayString(self, 0, "Cancelled");
    endfn:
      for (i = 0; i < count; i++) {
	  free(choices[i]);
      }
    self->styleInQuestion = 0;
    return;
}


void
htmlview_AddRandom(self, key)
struct htmlview* self;
long key;
{
    struct html* html = (struct html *)self->header.view.dataobject;
    char ename[MAXPATHLEN];
    long pos, len;

    pos = htmlview_GetDotPosition(self);
    len = htmlview_GetDotLength(self);
    if (len == 0) {
	/* XXX: set current insertion style... */
	message_DisplayString(self, 0, "Please mark some text first!");
	return;
    }
    if (message_AskForString(self, 100, "Name of entity: ", 0, ename, sizeof(ename)) < 0) {
	message_DisplayString(self, 0, "Cancelled");
	return;
    }

    html_AddEntity(html, pos, len, ename, 0);
}

void
htmlview_AddImage(self, key)
struct htmlview* self;
long key;
{
    struct html* html = (struct html *)self->header.view.dataobject;
    struct environment* env;
    char ename[MAXPATHLEN];
    static char vars[256];
    long pos;

    pos = htmlview_GetDotPosition(self);

    if (message_AskForString(self, 100, "Source of image: ", 0, ename, sizeof(ename)) < 0) {
	message_DisplayString(self, 0, "Cancelled");
	return;
    }

    sprintf(vars, "src=%s", ename);
    html_AddEntity(html, pos, 1L, "img", vars);
}

void
htmlview_SetImage(self, key)
struct htmlview* self;
long key;
{
    struct html* html = (struct html *)self->header.view.dataobject;
    long pos = htmlview_GetDotPosition(self);
    char* newsrc;

    printf("in SetImage()\n");
    if (!self->styleInQuestion) {
	message_DisplayString(self, "This callback should be used only when editing attributes", 0);
	return;
    }
    printf("Getting attribute\n");
    newsrc = html_GetAttribute(html, self->styleInQuestion, "src");

    printf("Going for update\n");

    /* Find the image here and tell it to load using the new src. */
    /* XXX: Not yet Implemented */
    message_DisplayString(self, "Sorry, image updates not yet implemented", 0);
}

void
htmlview_AddHrule(self, key)
struct htmlview* self;
long key;
{
    struct html* html = (struct html*) self->header.view.dataobject;
    long pos = htmlview_GetDotPosition(self);
    html_AddEntity(html, pos, 0, "hr", 0);
}

/* The following functions are all for handling lists */

/*
 * Parse a number out of a html object.
 *
 * Call this with start pointing at the first digit.
 * It will store the number parsed in numret.
 * It will return the count of characters scan'ed.
 * If the number is not immediately followed by a 
 * period and then a space or tab, the count is
 * returned as zero
 * signifying that this number is to be ignored.
 */
static int parse_num (html, start, end, numret)
struct html *html;
int start, end, *numret;
{
    int cur_num = 0, count = 0;
    long cur;

    while (start < end && isdigit (cur = html_GetChar(html, start))) {
	cur_num = 10 * cur_num + (cur - '0');
	start++;
	count++;
    }
    if (count == 0) return 0;
    /* skip over trailing period and whitespace if present */
    if (start < end && (cur = html_GetChar(html, start))	!= '.') return 0;	/* wrong format */
    if (start < end && ((cur = html_GetChar(html, start+1))
			 != ' ' && cur != '\t')) 
	return 0;	/* wrong format */
    count += 2;
    *numret = cur_num;
    return count;
}


int
checkEnumerate(html, pos, end, the_number)
struct html* html;
long pos;
long end;
int* the_number;
{
    int num;
    if (parse_num(html, pos, end, &num) > 0) {
	*the_number = num + 1;
	return 1;
    } else {
	return 0;
    }
}


char*
stringEnumerate(the_number)
int* the_number; /* datum1 */
{
    static char numstring[16];
    sprintf(numstring, "%d.\t", *the_number);
    return numstring;
}

/*
 * parse the number at point.  If we have already found the
 * beginning of the list (i.e., we're redoing the numbers),
 * then we should check the number is correct and fix it if not
 */
int
lineEnumerate(html, pos, end, the_number) 
struct html* html;
long* pos;
long* end;
int* the_number;	 
{
    int count;
    int newnum;

    *the_number += 1;

    if ((count = parse_num(html, *pos, *end, &newnum)) <= 0) {
	return 0;
    } else {
	/* We need to replace the number at point. */
	char* numstring;
	int tlen;

	if (newnum != *the_number) {
	    numstring = stringEnumerate(the_number);
	    tlen = strlen(numstring);
	    html_ReplaceCharacters(html, *pos, count, numstring, tlen);
	    *end += (tlen-count);
	    *pos += (tlen-count);
	}
	return 1;
    }
}

int
checkBullet(html, pos, end, datum)
struct html* html;
long pos;
long end;
int* datum;
{
    if (strchr(bulletChars, html_GetChar(html, pos))) {
	return 1;
    } else {
	return 0;
    }
}



char*
stringBullet(datum)
int* datum;
{
    return "*\t";
}

int
lineBullet(html, pos, end, datum)
struct html* html;
long* pos;
long* end;
int* datum;
{
    return checkBullet(html, *pos, *end, datum);
}

int
checkGlossary(html, pos, end, datum)
struct html* html;
long pos;
long end;
int* datum;
{
    /* Need to check if the line has a glossary term at this point. */
    return 0;
}

char*
stringGlossary(html, pos, end, datum)
int* datum;
{
    return "";
}

int
lineGlossary(html, pos, end, datum)
struct html* html;
long* pos;
long* end;
int* datum;
{
    return 0;
}

struct listCompileTable {
      char* styleName;
      int(*lookAtFn)(); /* Check the tag at point to see relevance to new item */
      int(*lineFn)();   /* For each line, this is called at beginning of line */
      char*(*computeTagStringFn)(); /* When doing a tag, this is called to get the string */
      char* itemStyle;
} listTypes[] = {
    { "ol", checkEnumerate, lineEnumerate, stringEnumerate, 0 },
    { "ul", checkBullet, lineBullet, stringBullet, 0 },
    { "menu", checkBullet, lineBullet, stringBullet, 0 },
    { "dir", checkBullet, lineBullet, stringBullet, 0 },
    { "dl", checkGlossary, lineGlossary, stringGlossary, "data-description" },
    { 0 }
};

void 
htmlview_makeList (self, listStyleName)
struct htmlview *self;
char* listStyleName;
{
    struct html* html = (struct html *)self->header.view.dataobject;
    struct text_statevector sv;
    int dot, pos, npos, count, tlen, len, end, origLen;
    int startPos, itemPos;
    long cur, indent, left;
    int one_only = 0, modified = 0;
    int datum = 0;
    int oldList = 0;
    char* tagString;
    char* itemStyle;
    int diff;

    int i;
    int(*lookAtFn)() = 0;
    int(*lineFn)() = 0;
    char*(*computeTagStringFn)() = 0;

    if (ConfirmReadOnly(self, html)) {
	return;
    }

    /* Work out the functions we want... */
    for (i = 0; listTypes[i].styleName; i++) {
	if (strcmp(listTypes[i].styleName, listStyleName) == 0) {
	    itemStyle = listTypes[i].itemStyle;
	    lookAtFn	       = listTypes[i].lookAtFn;
	    lineFn	       = listTypes[i].lineFn;
	    computeTagStringFn = listTypes[i].computeTagStringFn;
	    break;
	}
    }
    if (lookAtFn == 0) {
	return;
    }

    dot = pos = htmlview_GetDotPosition(self);
    origLen = len = htmlview_GetDotLength(self);

    end = pos + len;

    /* if len is zero, do current paragraph */
    if (len == 0) {
	int newlines = 0;
	/* back up to begining of paragraph */
	if (pos > 0) pos--;
	while (pos > 0 && (html_GetChar(html, pos - 1)) != '\n') pos--;
	end = html_GetLength (html);
	len = end - pos;
	one_only = 1;

	npos = pos;
	if (npos > 0) 
	    npos--; /* back up over newline we just saw */

	/* back up over additional whitespace between paragraphs */
	while (npos > 0) {
	    cur = html_GetChar(html, npos);
	    if (cur == '\n') {
		if (++newlines > 1) {
		    npos = -1;
		    break;
		}
	    }
	    if (!(cur == ' ' || cur == '\n' || cur == '\t'))
		break;
	    npos--;
	    continue;
	}
	    
	/* Now go to begining of the paragraph */
	while (npos > 0 && (html_GetChar(html, npos - 1)) != '\n') 
	    npos--;

	/* See if the previous paragraph begins with a tag'd item. */
	if (datum = lookAtFn(html, npos, pos, &datum)) {
	    oldList = 1;
	}
    }
    
    (void) htmlview_GetStyleInformation(self, &sv, pos, NULL);
    indent = sv.CurIndentation;
    left = sv.CurLeftMargin;

    startPos = pos;
    itemPos = -1;
    while (pos < end) {
	cur = html_GetChar(html, pos);
	if (cur == ' ' || cur == '\n' || cur == '\t') {
	    pos++;
	    continue;	/* go to first non-whitespace character */
	}
	/* pos is the start of the line (first non-blank char) */
	(void) htmlview_GetStyleInformation(self, &sv, pos, NULL);
	if (sv.CurLeftMargin == left && sv.CurIndentation == indent) {
	    /* Do the line function... */
	    if (diff = lineFn(html, &pos, &end, &datum)) {
		/* They did it themselves */
		itemPos = -1;
	    } else {
		itemPos = pos;
	    }
	}

	while (pos < end) {
	    pos++;	/* always move at least one character */
	    if (html_GetChar(html, pos) == '\n') {
		break;
	    }
	}
	/* At end of line, here (pos == '\n') */
	if (itemPos >= 0) {
	    int tlen;

	    tagString = computeTagStringFn(&datum);
	    tlen = strlen(tagString);
	    html_TagItem(html, itemPos, pos-itemPos, tagString, itemStyle, 0);
	    pos += tlen;
	    len += tlen;
	    end += tlen;
	    itemPos = -1;
	    modified = 1;
	}
	pos++;
	if (one_only) break;
    }
    /* End of region */

    /* We only add the list over everything if we didn't find an old wrapping. */
    if (!oldList || origLen > 0) {
	html_AddEntity(html, startPos, pos-startPos, listStyleName, 0);
    }
    if (modified) {
	html_NotifyObservers(html, 0);
	if (!one_only) {
	    htmlview_SetDotPosition (self, dot);
	    htmlview_SetDotLength (self, len);
	}
    }
}


void htmlview_unlistify (self, key)
struct htmlview *self;
long key;
{
    struct html *html = (struct html *)self->header.view.dataobject;
    struct text_statevector sv;
    int pos, count, len, end;
    long cur, indent, left;
    int one_only = 0, modified = 0;
    int cur_num;

    if (ConfirmReadOnly(self, html)) {
        return;
    }

    html_SetView(html,self);
    pos = htmlview_GetDotPosition(self);
    len = htmlview_GetDotLength(self);

    end = pos + len;

    /* if len is zero, do current paragraph */
    if (len == 0) {
	/* back up to begining of paragraph */
	if (pos > 0) pos--;
	while (pos > 0 && (html_GetChar(html, pos - 1)) != '\n') pos--;
	end = html_GetLength (html);
	len = end - pos;
	one_only = 1;
    }

    (void) htmlview_GetStyleInformation(self, &sv, pos, NULL);
    indent = sv.CurIndentation;
    left = sv.CurLeftMargin;

    while (pos < end) {
	cur = html_GetChar(html, pos);
	if (cur == ' ' || cur == '\n' || cur == '\t') {
	    pos++;
	    continue;	/* go to first non-whitespace character */
	}
	else {
	    (void) htmlview_GetStyleInformation(self, &sv, pos, NULL);
	    if (sv.CurLeftMargin == left && sv.CurIndentation == indent)
		if (count = html_UntagItem(html, pos)) {
		    end -= count;
		    len -= count;
		    modified = 1;
		}
	}
	if (one_only) break;
	/* go to end of paragraph */
	while (pos < end) {
	    pos++;	/* always move at least one character */
	    if (html_GetChar(html, pos) == '\n') break;
	}
	pos++;
    }
    if (modified) {
	html_NotifyObservers(html, 0);
	if (!one_only) htmlview_SetDotLength (self, len);
    }
}


void
htmlview_modifyList(self, key)
struct htmlview* self;
long key;
{
    struct html *html = (struct html *)self->header.view.dataobject;
    char* ptr;
    if (!self->styleInQuestion) {
	message_DisplayString(self, "Need to use Edit Attributes to call this", 0);
	return;
    }
    if (ptr = html_GetAttribute(html, self->styleInQuestion, "compact")) {
	/* style_SetNewInterlineSpacing(self->styleInQuestion->data.style, style_InterlineSpacing, -2, style_Points); */
    } else {
	/* style_SetNewInterlineSpacing(self->styleInQuestion->data.style, style_InterlineSpacing, 0, style_Points); */
    }
    html_RegionModified(html, htmlview_GetDotPosition(self), htmlview_GetDotLength(self));
    html_NotifyObservers(html, 0);
}

