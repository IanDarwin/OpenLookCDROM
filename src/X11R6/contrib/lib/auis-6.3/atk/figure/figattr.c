/* figattr.c - attributes for fig objects */
/*
	Copyright Carnegie Mellon University 1992 - All rights reserved
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
char *figattr_c_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/figure/RCS/figattr.c,v 1.2 1992/12/14 20:46:05 rr2b R6tape $";
#endif 

#include <andrewos.h>
#include <figattr.eh>

#include <class.h>

static char attribute_names[figattr_NumAttributes][20] = {
    "shade",
    "color",
    "linewidth",
    "rrectcorner",
    "fontsize",
    "fontstyle",
    "fontfamily",
    "textpos"
};

static char *CopyString();

boolean figattr__InitializeObject(ClassID, self)
struct classhdr *ClassID;
struct figattr *self;
{
    self->active = 0;

    self->shade = 0;
    self->linewidth = 0;
    self->rrectcorner = 10;
    self->color = CopyString("black");
    self->fontfamily = CopyString("andy");
    self->fontsize = 12;
    self->fontstyle = 0; /* fontdesc_Plain */
    self->textpos = figattr_PosCenter;

    return TRUE;
}

void figattr__FinalizeObject(ClassID, self)
struct classhdr *ClassID;
struct figattr *self;
{
    if (self->color)
	free(self->color);
    if (self->fontfamily)
	free(self->fontfamily);
}

struct figattr *figattr__CopySelf(self)
struct figattr *self;
{
    struct figattr *res = figattr_New();

    if (!res) return;
    res->active = self->active;

    /* ### should only copy active attributes, at least for high-cost ones like allocated string */
    res->shade = self->shade;
    res->linewidth = self->linewidth;
    res->rrectcorner = self->rrectcorner;
    res->color = CopyString(self->color);
    res->fontsize = self->fontsize;
    res->fontstyle = self->fontstyle;
    res->fontfamily = CopyString(self->fontfamily);
    res->textpos = self->textpos;

    return res;
}

void figattr__CopyData(self, src, mask)
struct figattr *self;
struct figattr *src;
unsigned long mask;
{
    if (mask & (1<<figattr_Shade)) {
	if (!figattr_IsActive(src, figattr_Shade))
	    figattr_SetActive(self, figattr_Shade, FALSE);
	else 
	    figattr_SetShade(self, figattr_GetShadeVal(src));
    }

    if (mask & (1<<figattr_LineWidth)) {
	if (!figattr_IsActive(src, figattr_LineWidth))
	    figattr_SetActive(self, figattr_LineWidth, FALSE);
	else 
	    figattr_SetLineWidth(self, figattr_GetLineWidthVal(src));
    }

    if (mask & (1<<figattr_RRectCorner)) {
	if (!figattr_IsActive(src, figattr_RRectCorner))
	    figattr_SetActive(self, figattr_RRectCorner, FALSE);
	else 
	    figattr_SetRRectCorner(self, figattr_GetRRectCornerVal(src));
    }

    if (mask & (1<<figattr_Color)) {
	if (!figattr_IsActive(src, figattr_Color))
	    figattr_SetActive(self, figattr_Color, FALSE);
	else 
	    figattr_SetColor(self, figattr_GetColorVal(src));
    }

    if (mask & (1<<figattr_FontSize)) {
	if (!figattr_IsActive(src, figattr_FontSize))
	    figattr_SetActive(self, figattr_FontSize, FALSE);
	else 
	    figattr_SetFontSize(self, figattr_GetFontSizeVal(src));
    }

    if (mask & (1<<figattr_FontStyle)) {
	if (!figattr_IsActive(src, figattr_FontStyle))
	    figattr_SetActive(self, figattr_FontStyle, FALSE);
	else 
	    figattr_SetFontStyle(self, figattr_GetFontStyleVal(src));
    }

    if (mask & (1<<figattr_FontFamily)) {
	if (!figattr_IsActive(src, figattr_FontFamily))
	    figattr_SetActive(self, figattr_FontFamily, FALSE);
	else 
	    figattr_SetFontFamily(self, figattr_GetFontFamilyVal(src));
    }

    if (mask & (1<<figattr_TextPos)) {
	if (!figattr_IsActive(src, figattr_TextPos))
	    figattr_SetActive(self, figattr_TextPos, FALSE);
	else 
	    figattr_SetTextPos(self, figattr_GetTextPosVal(src));
    }
    /* ##new */
}

static char *CopyString(str)
char *str;
{
    char *tmp;

    if (str==NULL)
	return NULL;
    tmp = malloc(strlen(str)+1);
    if (!tmp)
	return NULL;
    strcpy(tmp, str);
    return tmp;
}

/* does not use /begindata /enddata convention */
long figattr__Write(self, fp, writeid, level)
struct figattr *self;
FILE *fp;
long writeid;
int level;
{
    int ix;

    fprintf(fp, "\\figattr{\n");

    for (ix=0; ix<figattr_NumAttributes; ix++) {
	if (figattr_IsActive(self, ix)) {
	    fprintf(fp, "%s:", attribute_names[ix]);
	    switch (ix) {
		case figattr_Shade:
		    fprintf(fp, "%d", self->shade);
		    break;
		case figattr_LineWidth:
		    fprintf(fp, "%d", self->linewidth);
		    break;
		case figattr_RRectCorner:
		    fprintf(fp, "%d", self->rrectcorner);
		    break;
		case figattr_Color:
		    fprintf(fp, "%s", self->color);
		    break;
		case figattr_FontSize:
		    fprintf(fp, "%d", self->fontsize);
		    break;
		case figattr_FontStyle:
		    fprintf(fp, "%d", self->fontstyle);
		    break;
		case figattr_FontFamily:
		    fprintf(fp, "%s", self->fontfamily);
		    break;
		case figattr_TextPos:
		    fprintf(fp, "%d", self->textpos);
		    break;
		    /* ##new */
		default:
		    break;
	    }
	    fprintf(fp, "\n");
	}
    }

    fprintf(fp, "}\n");
    return 0;
}

/* does not use /begindata /enddata convention */
long figattr__Read(self, fp, id)
struct figattr *self;
FILE *fp;
long id;
{
#define LINELENGTH (250)
    static char buf[LINELENGTH+1];
    char *pt, *tmp;
    int ix;
    long ival;

    if (fgets(buf, LINELENGTH, fp) == NULL)
	return dataobject_PREMATUREEOF;
    if (strncmp(buf, "\\figattr{", 9)) return dataobject_BADFORMAT;

    if (fgets(buf, LINELENGTH, fp) == NULL)
	return dataobject_PREMATUREEOF;

    while (1) {
	
	if (!strncmp(buf, "}", 1)) return dataobject_NOREADERROR;
	pt = index(buf, ':');
	if (!pt) return dataobject_BADFORMAT;
	*pt = '\0';
	pt++;
	tmp = index(pt, '\n');
	if (tmp) 
	    *tmp = '\0';

	for (ix=0; ix<figattr_NumAttributes; ix++)
	    if (!strcmp(buf, attribute_names[ix])) break;
	if (ix==figattr_NumAttributes) return dataobject_BADFORMAT;

	switch (ix) {
	    case figattr_Shade:
		ival = atoi(pt);
		figattr_SetShade(self, ival);
		break;
	    case figattr_LineWidth:
		ival = atoi(pt);
		figattr_SetLineWidth(self, ival);
		break;
	    case figattr_RRectCorner:
		ival = atoi(pt);
		figattr_SetRRectCorner(self, ival);
		break;
	    case figattr_Color:
		figattr_SetColor(self, pt);
		break;
	    case figattr_FontFamily:
		figattr_SetFontFamily(self, pt);
		break;
	    case figattr_FontSize:
		ival = atoi(pt);
		figattr_SetFontSize(self, ival);
		break;
	    case figattr_FontStyle:
		ival = atoi(pt);
		figattr_SetFontStyle(self, ival);
		break;
	    case figattr_TextPos:
		ival = atoi(pt);
		figattr_SetTextPos(self, ival);
		break;
		/* ##new */
	    default:
		break;
	}

	if (fgets(buf, LINELENGTH, fp) == NULL)
	    return dataobject_PREMATUREEOF;
    }
}

