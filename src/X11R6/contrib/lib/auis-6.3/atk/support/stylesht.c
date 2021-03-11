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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/support/RCS/stylesht.c,v 2.9 1992/12/15 21:41:42 rr2b R6tape $";
#endif


 

#include <class.h>

#include <style.ih>
#include <menulist.ih>
#include <proctbl.ih>

#include <stylesht.eh>


#define iswhite(c) ((c) == ' ' || (c) == '\t' || (c) == '\n')

#define INITIALNUMSTYLES 20


boolean stylesheet__InitializeObject(classID, self)
struct classheader *classID;
struct stylesheet *self;
{
    self->nstyles = 0;
    self->maxStyles = 0;
    self->styles = NULL;
    self->templateName = NULL;
    self->styleMenu = NULL;
    self->version = 0;

    return TRUE;
}

void stylesheet__FinalizeObject(classID, self)
struct classheader *classID;
struct stylesheet *self;
{
    if (self->templateName)
	free(self->templateName);
    if (self->styleMenu != NULL)
        menulist_Destroy(self->styleMenu);
    stylesheet_FreeStyles(self);
    if (self->styles)
	free(self->styles);
}

void stylesheet__FreeStyles(self)
struct stylesheet *self;
{
    register int i;
    register struct style **styles;

    if (self->nstyles==0)
	return;

    for (i = 0, styles = self->styles; i < self->nstyles; i++, styles++)
	style_Destroy(*styles);

    self->nstyles = 0;
    self->version++;
}

static long FindStyle(self, styleptr)
struct stylesheet *self;
struct style *styleptr;
{
    register int i;
    register struct style **styles;
    
    for (i = 0, styles = self->styles; i < self->nstyles; i++, styles++)
        if (*styles == styleptr)
            return i;

    return -1;
}

void stylesheet__Add(self, styleptr)
struct stylesheet *self;
struct style *styleptr;
{
    if (self->maxStyles == 0) {
	self->maxStyles = INITIALNUMSTYLES;
	self->styles = (struct style **)
          malloc(INITIALNUMSTYLES * sizeof(struct style *));
    } else if (FindStyle(self, styleptr) != -1)
	return;
    else if (self->nstyles == self->maxStyles) {
	self->maxStyles += self->maxStyles / 2;
	self->styles = (struct style **) realloc(self->styles, self->maxStyles * sizeof(struct style *));
    }
    self->styles[self->nstyles++] = styleptr;
    self->version++;
}

void stylesheet__Delete(self, styleptr)
struct stylesheet *self;
struct style *styleptr;
{
    register int i;

    if ((i = FindStyle(self, styleptr)) != -1) {
	style_Destroy(styleptr);
	for (self->nstyles--; i < self->nstyles; i++)
	    self->styles[i] = self->styles[i+1];
    }
    self->version++;
}

struct style *stylesheet__Find(self, name)
struct stylesheet *self;
char *name;
{
    register int i;
    register struct style **styles;
    
    for (i = 0, styles = self->styles; i < self->nstyles; i++, styles++)
	if (strcmp((*styles)->name, name) == 0)
            return *styles;

    return NULL;
}

struct menulist *stylesheet__GetMenuList(self, procname, infotype)
struct stylesheet *self;
int (*procname)();
struct classinfo *infotype;
{
    struct proctable_Entry *proc;

    register int i;
    register struct style **styles;

    if (!self->styleMenu)
        self->styleMenu = menulist_New();
    else
        menulist_ClearML(self->styleMenu);

    proc = proctable_DefineProc("wraplook", procname, infotype, NULL, NULL);

    for (i = 0, styles = self->styles; i < self->nstyles; i++, styles++)
	menulist_AddToML(self->styleMenu, (*styles)->menuName, proc, i, 0);

    return self->styleMenu;
}

/* This routine parses the contents of a \define{} */
/* Assumes the "\define{" has already been read. */
/* The ending "}" is read by style_Read. */

long stylesheet__Read(self, fp, template)
struct stylesheet *self;
FILE *fp;
boolean template;
{
    int c, i;
    struct style *styleptr;
    char styleName[80];

    do {
        if ((c = getc(fp)) == EOF)
            return -1;
    } while (iswhite(c));

    i = 0;

    if (c == '}')   /* Empty define */
        return 0;

    /* The style name is terminated by white space, usually '\n'. */

    while (! iswhite(c)) {
        if (i < sizeof (styleName) - 1)
            styleName[i++] = c;
        if ((c = getc(fp)) == EOF)
            return -1;
    }

    styleName[i] = '\0';

    if ((styleptr = stylesheet_Find(self, styleName)) != NULL) {
        style_Finalize(styleptr);
        style_Initialize(styleptr);
    } else {
        styleptr = style_New();
        stylesheet_Add(self, styleptr);
    }

    style_SetName(styleptr, styleName);
    styleptr->template = template;

    style_Read(styleptr, fp);

    return 0;
}

void stylesheet__Write(self, fp)
struct stylesheet *self;
FILE *fp;
{
    register int i;
    register struct style **styles;
    
    for (i = 0, styles = self->styles; i < self->nstyles; i++, styles++)
	if ((*styles)->template == 0)  {
	    fprintf(fp, "\\define{%s\n", style_GetName(*styles));
	    style_Write(*styles, fp);
	    fprintf(fp, "}\n");
	}
}

void stylesheet__SetTemplateName(self, templateName)
struct stylesheet *self;
char *templateName;
{
    if (self->templateName != NULL) {
	if (strcmp(self->templateName, templateName)==0)
	    return;
	else
	    free(self->templateName);
    }

    if (templateName == NULL)
	self->templateName = NULL;
    else {
	self->templateName = (char *) malloc(strlen(templateName)+ 1);
	strcpy(self->templateName, templateName);
    }
    self->version++;
}

char *stylesheet__GetTemplateName(self)
struct stylesheet *self;
{
    return self->templateName;
}

 /* EnumerateStyles calls func(style, data) for each style in self.  
			The boolean value returned by func is True if the function is
			through enumerating;  EnumerateStyles then returns the last style 
			processed, othewise it returns NULL 
		WJHansen, 17 Aug, 1987 */

struct style *
stylesheet__EnumerateStyles(self, func, data)
struct stylesheet *self;
boolean (*func)();
long data;
{
	struct style **curr;
	for (curr = self->styles; curr < self->styles + self->nstyles; ) {
		struct style *t = *curr;
		if ((*func)(*curr, data))
			return *curr;
		if (t == *curr) curr++;	/* try to be reasonable if a style has been deleted */
	}
	return NULL;
}

struct style *stylesheet__GetGlobalStyle(self)
struct stylesheet *self;
{
    return stylesheet_Find(self, "global");
}

