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
static char rcsid[] = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/common/RCS/fontdesc.c,v 2.12 1992/12/16 22:54:16 rr2b R6tape $";
#endif

#include <class.h>
#include <andrewos.h>
#include <graphic.ih>
#include <im.ih>
#include <fontdesc.eh>

#include <ctype.h>


char *fontdesc__GetFontFamily(self)
struct fontdesc *self;
{
    return self->FontName->name;
}

struct fontnamedesc *fontdesc__GetFontFamilyDesc(self)
struct fontdesc *self;
{
    return self->FontName;
}

long fontdesc__GetFontSize(self)
struct fontdesc *self;
{
    return self->FontSize;
}

long fontdesc__GetFontStyle(self)
struct fontdesc *self;
{
    return self->FontStyles;
}

struct graphic *fontdesc__CvtCharToGraphic(self, graphic, SpecialChar)
struct fontdesc *self;
struct graphic *graphic;
char SpecialChar;
{
    /* Override Me */
    return NULL;
}

struct font *fontdesc__GetRealFontDesc(self, graphic)
struct fontdesc *self;
struct graphic *graphic;
{
    /* Override Me */
    return NULL;
}

long fontdesc__TextSize(self, graphic, text, TextLength, XWidth, YWidth)
struct fontdesc *self;
struct graphic *graphic;
char *text;
long TextLength;
long *XWidth;
long *YWidth;
{
    /* Override Me */
    return 0;
}

struct FontSummary *fontdesc__FontSummary(self, graphic)
struct fontdesc *self;
struct graphic *graphic;
{
    register struct FontSummary *tsp;

    tsp = &self->summary;
    if (self->DescValid)
        return tsp;
    /* Ensure self->MachineDep... is valid */ 
    fontdesc_GetRealFontDesc(self, graphic);
    return tsp;
}

short *fontdesc__WidthTable(self, graphic)
struct fontdesc *self;
struct graphic *graphic;
{
    /* Override Me */
    return NULL;
}

short *fontdesc__HeightTable(self, graphic)
struct fontdesc *self;
struct graphic *graphic;
{
    /* Override Me */
    return NULL;

}

long fontdesc__StringSize(self, graphic, string,XWidth,YWidth)
struct fontdesc *self;
struct graphic *graphic;
register unsigned char *string;
register long *XWidth;
register long *YWidth;
{
    /* Override Me */
    return 0;
}

void fontdesc__CharSummary(self,gr,LookUpChar,retVal)
struct fontdesc *self;
struct graphic *gr;
char LookUpChar;
struct fontdesc_charInfo *retVal;
{
    /* Override Me */
}

/* Warning: The following routines are Textview critical code */

static struct fontdesc *fontdesc_CreateUsingDescriptor(FontName, FontStyle, FontSize)
struct fontnamedesc *FontName;
long FontStyle;
long FontSize;
{
    struct fontdesc *retVal;

    for (retVal = FontName->fontList; retVal != NULL; retVal = retVal->next)
        if (retVal->FontStyles == FontStyle && retVal->FontSize == FontSize)
            return retVal;
    
    retVal = im_GetFontdesc();
    
    retVal->FontName = FontName;
    retVal->FontSize = FontSize;
    retVal->FontStyles = FontStyle;
    retVal->DescValid = FALSE;
    retVal->next = FontName->fontList;
    FontName->fontList = retVal;

    return retVal;
}

struct fontdesc *fontdesc__Create(classID, fontName, fontStyle, fontSize)
struct classheader *classID;
char *fontName;
long fontStyle;
long fontSize;
{
    char tempFontName[256], *s = tempFontName;
    do {    /* Fold lowercase */
        *s++ = isupper(*fontName) ? tolower(*fontName) : *fontName;
    } while (*fontName++);
    return fontdesc_CreateUsingDescriptor
      (fontdesc_GetFontNameDesc(tempFontName), fontStyle, fontSize);
}

struct fontnamedesc *fontdesc__GetFontNameDesc(classID, fontName)
struct classheader *classID;
char *fontName;
{
    static struct fontnamedesc *allFontNames = NULL;
    register struct fontnamedesc *tp;
    
    for (tp = allFontNames; tp != NULL; tp = tp->next)
        if (*tp->name == *fontName && ! strcmp(tp->name, fontName))
	    return tp;

    tp = (struct fontnamedesc *) malloc(sizeof (struct fontnamedesc));
    tp->next = allFontNames;
    allFontNames = tp;
    tp->fontList = NULL;
    tp->name = (char *) malloc(strlen(fontName) + 1);
    strcpy(tp->name, fontName);
    return tp;
}

struct fontdesc *fontdesc__Allocate(classID)
    struct classheader *classID;
{
    return (struct fontdesc *) malloc(sizeof(struct fontdesc));
}

void fontdesc__Deallocate(classID, self)
    struct classheader *classID;
    struct fontdesc *self;
{
/* Fontdesc structures are never deallocated since they are reused. */
}

boolean fontdesc__InitializeObject(classID, self)
struct classheader *classID;
struct fontdesc *self;
{
    self->FontName = NULL;
    self->FontStyles = fontdesc_Plain;
    self->FontSize = 12;
    self->DescValid = FALSE;
    self->widthTable = NULL;
    self->heightTable = NULL;
    self->MachineDependentFontDescriptor = NULL;
    bzero(self->charValid, sizeof(self->charValid));
    return TRUE;
}

void fontdesc__FinalizeObject(classID, FontDescObject)
struct classheader *classID;
struct fontdesc *FontDescObject;
{
    if (FontDescObject->widthTable)
        free(FontDescObject->widthTable);
    if (FontDescObject->heightTable)
        free(FontDescObject->heightTable);
    if (FontDescObject->FontName)
        free(FontDescObject->FontName);
}

/* This code parses backward looking for fonts of the following format:
 *
 * <family name><optional "-" or "-s"><point size><style modifiers>
 *
 * Where the style modifiers consist of the following single modifier codes:
 *
 *     b    bold
 *     i    italic
 *     f    fixed width    (This is bogus and should disappear)
 *     s    shadow
 *
 * If there is no font size in the name (i.e. no numbers in the name), the
 * entire name is returned as the familyName and the size and style are set
 * to 0.
 *
 * This works fairly well except for fonts like "6x10" and
 * "helvetica-bold-s12". In the first case, it gets the name wrong and in the
 * second case, it loses the bold style modifier.
 *
 * Without a consistent usable X font naming convention, there is no point in
 * worrying about any fonts that don't follow Andrew conventions.
 * Furthermore, this routine should not exists. The user interface should
 * never have to know about font naming. If the window system module needs to
 * know, it ought to have a local routine to parse the fontnames.
 */

boolean fontdesc__ExplodeFontName(classID, fontName, familyName, bufSize, fontStyle, fontSize)
struct classheader *classID;
char *fontName;
char *familyName;
long bufSize;
long *fontStyle;
long *fontSize;
{
    char *endName;
    int style = 0;
    int size = 0;
    int length;

    if((fontName == NULL) || (*fontName == '\0'))
	return FALSE;
    else 
	endName = fontName + strlen(fontName) - 1;

    /* Parse style modifiers. Assumes there will be a font size between the
     * family name and the end of the whole name. */

    while (!isdigit(*endName)) {
        switch (*endName) {
            case 'b': 
                style |= fontdesc_Bold;
                break;
            case 'i': 
                style |= fontdesc_Italic;
                break;
            case 'f': 
                style |= fontdesc_Fixed;
                break;
            case 's': /* Shadow font */
                style |= fontdesc_Shadow;
                break;
            default: 
                break;
        }
	if(endName > fontName) 
	    endName--;
	else 
	    break;
    }

    if (endName == fontName) { /* No font size. Whole name is family name. */
	size = 12;
        style = 0;
        length = bufSize;
    }
    else {
        int tensPlace = 1;

        /* Snag the fontsize. */
        while (endName >= fontName && isdigit(*endName)) {
            size += (*endName-- - '0') * tensPlace;
            tensPlace *= 10;
        }

        /* Parse optional "-" or "-s". */
        if (endName >= fontName && *endName == '-')
            --endName;
        else if (endName > fontName && endName[-1] == '-' && endName[0] == 's')
            endName -= 2;

        /* If there is no family name, this call fails. */
        if (endName < fontName)
            return FALSE;

        length = endName - fontName + 1;
        if (length >= bufSize)
            length = bufSize - 1;
    }

    /* Fill in return values as appropriate. */

    if (fontStyle != NULL)
        *fontStyle = style;

    if (fontSize != NULL)
        *fontSize = size;
    
    if (familyName != NULL) {
        strncpy(familyName, fontName, length);
        familyName[length] = '\0';
    }

    return TRUE;
}

long fontdesc__StringBoundingBox(font, graphic, string, width, height)
struct fontdesc *font;
struct graphic *graphic;
char *string;
int *width;
int *height;
{
  int w, a, d, ascent, descent, junk;
  register short *fwt, *fht;
  register char *p;
  static struct fontdesc_charInfo ci;

#define MAX(a,b) (((a)>(b))?(a):(b))

  fwt = fontdesc_WidthTable (font, graphic);
  fht = fontdesc_HeightTable (font, graphic);
  if (fwt == NULL || fht == NULL)
    return 0;
  fontdesc_StringSize (font, graphic, string, (long *) &w, (long *) &junk);
  for (p = string, a = 0, d = 0;  *p != (char) 0;  p += 1)
    {
      fontdesc_CharSummary (font, graphic, *p, &ci);
      ascent = ci.yOriginOffset;
      descent = ci.height - ascent;
      a = MAX(a,ascent);
      d = MAX(d,descent);
    }
  *width = w;
  *height = a + d;
  return (w);
}  
