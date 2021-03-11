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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/wm/RCS/wfontd.c,v 1.14 1992/12/15 21:26:22 rr2b R6tape $";
#endif


 

/*
 *	wfontd.c   (nee wmfontdesc.c)
 */


#include <andyenv.h>
#include <wmclient.h>
#include <wgraphic.ih>
#include <wfontd.eh>
#include <ctype.h>


#define MDFD ((struct fcache *)(((struct fontdesc *)self)->MachineDependentFontDescriptor))

/* a pointer to an fcache value is stored in the MachineDependentFontDescriptor field of the fontdesc.
	If the hostaddr matches that of the window, use the hostfont field.
	Otherwise we do a wm_DefineFont on the fontname.  This works because
	wm caches the host:font pair itself and does a table lookup.

	XXX if wm ever has different fonts for different displays on the same machine,
	this code will be broken because it checks the host addr but does not check
	to see which display if the host has more than one
*/
struct fcache {
	unsigned long hostaddr;	/* the host for which next field value is valid */
	struct font *hostfont;	/* the value for wm_SelectFont() */
	char *fontname;	/* the Andrew name of the font */
};


/* This procedure creates the name of the andrew font file
given a particular set of characteristics */

	static struct FontSummary *
GetFontSummary(self)
    	struct fontdesc *self; 
{
    register struct FontSummary *tsp;
    register int i;
    register struct font *font;

    tsp = &self->summary;	/* for quick reference */
    /* otherwise DescValid is set to true in LoadAndrewFont */
    bzero(tsp, sizeof (struct FontSummary));

    font = MDFD->hostfont;
    for(i=0; i<fontdesc_NumIcons; i++) {
	if (i <= 127) {	/* wm has only 128 chars per font */
	    /* Get value from font table */
	    register struct icon   *c = &font -> chars[i];
	    if (c -> OffsetToGeneric) {
		register struct IconGenericPart *g =
		(struct IconGenericPart *) (((int) c) + c -> OffsetToGeneric);
		if (tsp->maxSpacing < g->Spacing.x) tsp->maxSpacing = g->Spacing.x;
		if (tsp->maxWidth < g->WtoE.x) tsp->maxWidth = g->WtoE.x;
		if (tsp->maxHeight < g->NWtoOrigin.y) tsp->maxHeight = g->NWtoOrigin.y;
		if (tsp->maxBelow < g->NtoS.y - g->NWtoOrigin.y)
		    tsp->maxBelow = g->NtoS.y - g->NWtoOrigin.y;
		if (tsp->maxLeft < g->NWtoOrigin.x) tsp->maxLeft = g->NWtoOrigin.x;
/* Wm doesn't like NUL's in random places, wm treats a form feed as a clear window operator so we make these invalid. */
                if (i != '\0' && i != '\014')
                    fontdesc_SetCharValid(self, i);
	    }
	}
    }
    tsp->maxHeight += tsp->maxBelow;
    return tsp;
}

	static struct font *
LoadAndrewFont(self, graphic)
	struct fontdesc *self;
	struct wmgraphic *graphic; 
{
	char * AndyName;
	unsigned char StyleModifiers[4]; /* bif <null> */
	unsigned char * StyleMPtr = StyleModifiers;

	/* Figure out which styles to use */
	if (self->FontStyles & fontdesc_Bold) *StyleMPtr++ = 'b';
	if (self->FontStyles & fontdesc_Italic) *StyleMPtr++ = 'i';
	if (self->FontStyles & fontdesc_Fixed) *StyleMPtr++ = 'f';
	*StyleMPtr++ = '\000'; /* And terminal it with a null */

	if (self->FontName->name) {
		AndyName = malloc(strlen(self->FontName->name)
			+ 1 /* for null */
			+ 4 /* for (gigantic) font size */
			+ 3 /* for style modifiers */
			+ 4 /* for possible .fwm extension. */
		);
		sprintf(AndyName,"%s%d%s",self->FontName->name,self->FontSize,
					StyleModifiers);
		if (*AndyName == '/')
			strcat(AndyName, ".fwm"); /* For absolute fontnames, 
					the window manager insists on full paths. */
	}
	else {
		AndyName = malloc(16); /* see above for "andysans" */
		sprintf(AndyName,"andysans%d%s",self->FontSize,StyleModifiers);
	}

	self->MachineDependentFontDescriptor = (struct font *)malloc(sizeof (struct fcache));
	MDFD->fontname = AndyName;
	MDFD->hostaddr = graphic->window->a.hostaddress;
	MDFD->hostfont = wm_DefineFont(AndyName);
	self->DescValid = TRUE;

	if (MDFD->hostfont != NULL) 
		/* recompute font summary info */
		GetFontSummary(self);
	else
		fprintf(stderr, "wmfontdesc:  wm failed to provide a font for name '%s'\n",
			AndyName);

	return MDFD->hostfont;
}


/* ************* class procedures ****************** */

struct wmfontdesc *wmfontdesc__Allocate(classID)
    struct classheader *classID;
{
    return (struct wmfontdesc *) malloc(sizeof(struct wmfontdesc));
}

void wmfontdesc__Deallocate(classID, self)
    struct classheader *classID;
    struct wmfontdesc *self;
{
/* Fontdesc structures are never deallocated since they are reused. */
}

	boolean
wmfontdesc__InitializeObject(ClassID, self)
	struct classhdr *ClassID;
	struct wmfontdesc *self;
{
	((struct fontdesc *)self)->MachineDependentFontDescriptor 
			= NULL;		/* (also done in fontdesc.c) */
	return TRUE;
}

	void
wmfontdesc__FinalizeObject(ClassID, self)
	struct classhdr *ClassID;
	struct wmfontdesc *self;
{
	if (MDFD != NULL) {
		if (MDFD->fontname != NULL)
			free (MDFD->fontname);
		free(MDFD);
	}
}


/* ************* methods ****************** */


struct graphic * wmfontdesc__CvtCharToGraphic(self, graphic, SpecialChar)
struct fontdesc * self;
struct wmgraphic *graphic;
char SpecialChar; {
    struct wmgraphic * RetValue;


    RetValue = wmgraphic_New();
    /* This is disgusting */
    RetValue->altPixMapUsed = TRUE;
    RetValue->fillChar = SpecialChar;
    RetValue->fillFont = self;

    return (struct graphic *) RetValue;
}

	struct font * 
wmfontdesc__GetRealFontDesc(self, graphic)
	struct fontdesc *self;
	struct wmgraphic *graphic; 
{
	if (self->DescValid) {
		if (graphic->window->a.hostaddress == MDFD->hostaddr)
			return MDFD->hostfont;
		return wm_DefineFont(MDFD->fontname);
	}
	return LoadAndrewFont(self, graphic);
}

long wmfontdesc__TextSize(self, graphic, text, TextLength, XWidth, YWidth)
struct fontdesc * self;
struct wmgraphic *graphic;
unsigned char * text;
long TextLength;
long * XWidth;
long * YWidth; {
    register struct font   *font = fontdesc_GetRealFontDesc(self, graphic);
    register long x = 0,
                y = 0;

    while (TextLength--) {
	register struct icon   *c = &font -> chars[*text++];
	if (c -> OffsetToGeneric) {
	    register struct IconGenericPart *g =
		(struct IconGenericPart *) (((int) c) + c -> OffsetToGeneric);
	    x += g -> Spacing.x;
	    y += g -> Spacing.y;
	}
    }
    if (XWidth) *XWidth = x;
    if (YWidth) *YWidth = y;
    return (long) x;
}

short * wmfontdesc__WidthTable(self, graphic)
struct fontdesc * self;
struct wmgraphic *graphic;     {
/* This procedure returns the font size table for all characters in
a font */
    register struct font   *font = fontdesc_GetRealFontDesc(self, graphic);
    register short * fontWidthTable;
    int i;

    if (self->widthTable) return self->widthTable;

    fontWidthTable = (short *) malloc(fontdesc_NumIcons*sizeof(short));
    self->widthTable = fontWidthTable;

    for(i=0;i<fontdesc_NumIcons;i++) {
	if(i>127)  {
	    fontWidthTable[i] = 0; /* wm only has 128 symbols */
	}
	else {
	    /* Get value from font table */
	    register struct icon   *c = &font -> chars[i];
	    if (c -> OffsetToGeneric) {
		register struct IconGenericPart *g =
		(struct IconGenericPart *) (((int) c) + c -> OffsetToGeneric);
		fontWidthTable[i] = g -> Spacing.x;
		}
	    else fontWidthTable[i] = 0;
	    }
        }
    return fontWidthTable;
}

short * wmfontdesc__HeightTable(self, graphic)
struct fontdesc * self;
struct wmgraphic *graphic; {
/* This procedure returns the font size table for all characters in
a font */
    register struct font   *font = fontdesc_GetRealFontDesc(self, graphic);
    register short * fontHeightTable;
    int i;

    if (self->heightTable) return self->heightTable;

    fontHeightTable = (short *) malloc(fontdesc_NumIcons*sizeof(short));
    self->heightTable = fontHeightTable;

    for(i=0;i<fontdesc_NumIcons;i++) {
	if(i>127) fontHeightTable[i] = 0; /* wm only has 128 symbols */
	else {
	    /* Get value from font table */
	    register struct icon   *c = &font -> chars[i];
	    if (c -> OffsetToGeneric) {
		register struct IconGenericPart *g =
		(struct IconGenericPart *) (((int) c) + c -> OffsetToGeneric);
		fontHeightTable[i] = g -> Spacing.y;
		}
	    else fontHeightTable[i] = 0;
	    }
        }
    return fontHeightTable;

}

long wmfontdesc__StringSize(self, graphic,string,XWidth,YWidth)
struct fontdesc * self;
struct wmgraphic *graphic;
register unsigned char * string;
register long * XWidth;
register long * YWidth; {
    register struct font   *font = fontdesc_GetRealFontDesc(self, graphic);
    register long   x = 0,
                y = 0;

    while (*string) {
	register struct icon   *c = &font -> chars[*string++];
	if (c -> OffsetToGeneric) {
	    register struct IconGenericPart *g =
		(struct IconGenericPart *) (((int) c) + c -> OffsetToGeneric);
	    x += g -> Spacing.x;
	    y += g -> Spacing.y;
	}
    }
    if (XWidth) *XWidth = x;
    if (YWidth) *YWidth = y;
    return (long) x;

}

void wmfontdesc__CharSummary(self,gr,LookUpChar,RetValue)
struct fontdesc * self;
struct wmgraphic * gr;
unsigned char LookUpChar;
struct fontdesc_charInfo * RetValue;
{
    register struct font   *font = fontdesc_GetRealFontDesc(self, gr);
    if (!RetValue) return;

    if (LookUpChar > 127) {
	RetValue->width = 0;
	RetValue->height = 0;
	RetValue->xOriginOffset = 0;
	RetValue->yOriginOffset = 0;
	RetValue->xSpacing = 0;
	RetValue->ySpacing = 0;
    }
    else {
	    register struct icon   *c = &font -> chars[LookUpChar];
	    if (c -> OffsetToGeneric) {
		register struct IconGenericPart *g =
		(struct IconGenericPart *) (((int) c) + c -> OffsetToGeneric);
		RetValue->width = g->WtoE.x;
		RetValue->height = g->NtoS.y;
		RetValue->xSpacing = g->Spacing.x;
		RetValue->ySpacing = g->Spacing.y;
		RetValue->xOriginOffset = g->NWtoOrigin.x;
		RetValue->yOriginOffset = g->NWtoOrigin.y;
	    }
	    else {
		RetValue->width = 0;
		RetValue->height = 0;
		RetValue->xOriginOffset = 0;
		RetValue->yOriginOffset = 0;
		RetValue->xSpacing = 0;
		RetValue->ySpacing = 0;
	    }
    }

}


