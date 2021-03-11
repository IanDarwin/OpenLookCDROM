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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/console/lib/RCS/prompts.c,v 2.9 1992/12/15 21:31:10 rr2b R6tape $";
#endif


 


#include <andrewos.h>
#include <class.h>
#include <conclass.ih>
#include <im.ih>
#include <graphic.ih>
#include <fontdesc.ih>
#include <rect.h>
#include <view.ih>
#include <console.h>
#include <sys/param.h>

int Pposx, Pposy;
char Pstring1[256] = "", Pstring2[256] = "", Pstring3[256] = "", Pstring4[MAXPATHLEN] = "";


ClearRectangle(self, clpRect, Op1, Op2)
struct consoleClass *self;
struct rectangle *clpRect;
short Op1;
struct graphic *Op2;
{
    mydbg(("entering: ClearRectangle\n"));
    consoleClass_SetTransferMode(self, Op1);
    consoleClass_FillRect(self, clpRect, Op2);
}

ClearBox(self, x, y, w, h, Op1, Op2)
struct consoleClass *self;
int x, y, w, h;
short Op1;
struct graphic *Op2;
{
    struct rectangle tempRect;

    mydbg(("entering: ClearBox\n"));
    rectangle_SetRectSize(&tempRect, x, y, w, h);
    ClearRectangle(self, &tempRect, Op1, Op2);
}

ClearWindow(self)
struct consoleClass *self;
{
    struct rectangle windowRect;
    
    mydbg(("entering: ClearWindow\n"));
    consoleClass_GetLogicalBounds(self, &windowRect);
    ClearRectangle(self, &windowRect, graphic_COPY, consoleClass_WhitePattern(self));
}

InvertWindow(self)
struct consoleClass *self;
{
    struct rectangle windowRect;

    mydbg(("entering: InvertWindow\n"));
    consoleClass_GetLogicalBounds(self, &windowRect);
    ClearRectangle(self, &windowRect, graphic_INVERT, consoleClass_BlackPattern(self));
}

InitPstrings()
{
    mydbg(("entering: InitPstrings\n"));
    Pstring1[0] = '\0';
    Pstring2[0] = '\0';
    Pstring3[0] = '\0';
    Pstring4[0] = '\0';
    Pposx = Pposy = 0;
}

PromptToWindow(self)
struct consoleClass *self;
{
    register short *fontWidth = fontdesc_WidthTable(EventFont, consoleClass_GetDrawable(self));
    int width = consoleClass_GetLogicalWidth(self), height = consoleClass_GetLogicalHeight(self);

    mydbg(("entering: PromptToWindow\n"));
    ClearWindow(self);
    consoleClass_SetFont(self, EventFont);
    consoleClass_MoveTo(self, width >> 1, height / 4);
    consoleClass_DrawString(self, Pstring1, graphic_BETWEENLEFTANDRIGHT | graphic_BETWEENTOPANDBASELINE);
    consoleClass_MoveTo(self, width >> 1, height / 2);
    consoleClass_DrawString(self, Pstring2, graphic_BETWEENLEFTANDRIGHT | graphic_BETWEENTOPANDBASELINE);
    if (!strcmp(Pstring3,"==>> ")){
	consoleClass_MoveTo(self, 10, (int) (height * 0.75));
	consoleClass_DrawString(self, Pstring3, graphic_ATLEFT | graphic_BETWEENTOPANDBASELINE);
	consoleClass_FlushGraphics(self);
	Pposy = (int) (consoleClass_GetLogicalHeight(self) *.75);
	Pposx = 10 + (fontWidth['='] * 2) + (fontWidth['>'] * 2) + fontWidth[' '];
    }
    else{
	consoleClass_MoveTo(self, width >> 1, (int) (height * 0.75));
	consoleClass_DrawString(self, Pstring3, graphic_BETWEENLEFTANDRIGHT | graphic_BETWEENTOPANDBASELINE);
    }
}    



GetStringFromWindow(self, maxSize)
struct consoleClass *self;
long maxSize;
{
    int c;
    register short   *fontWidth = fontdesc_WidthTable(EventFont, consoleClass_GetDrawable(self));
    char *tempString = Pstring4;

    mydbg(("entering: GetStringFromWindow\n"));
    consoleClass_SetFont(self, EventFont);

    while (((c = im_GetCharacter(consoleClass_GetIM(self))) != '\r') && (c != '\n') && (c != EOF) && ((tempString - Pstring4) < maxSize)) {
	switch(c){
	    case '\010': /* Backspace */
	    case '\177': /* Delete */
		if (tempString > Pstring4){
		    --tempString;
		    Pposx -= fontWidth[*tempString];
		    consoleClass_MoveTo(self, Pposx, Pposy);
		    consoleClass_SetTransferMode(self, graphic_INVERT);
		    consoleClass_DrawString(self, tempString, graphic_ATLEFT | graphic_BETWEENTOPANDBASELINE);
		    *tempString = '\0';
		}
		break;
	    case '\025': /* ^U */
		while((tempString - Pstring4) != 0){
		    --tempString;
		    Pposx -= fontWidth[*tempString];
		    consoleClass_MoveTo(self, Pposx, Pposy);
		    consoleClass_SetTransferMode(self, graphic_INVERT);
		    consoleClass_DrawString(self, tempString, graphic_ATLEFT | graphic_BETWEENTOPANDBASELINE);
		    *tempString = '\0';
		}
		Pstring4[0] = '\0';
		break;
	    case '\007': /* ^G */
		Pstring4[0] = '\0';
		return;
	    default:
		if (c > '\037'){ /* printable character */
		    *tempString = (char)c;
		    tempString[1] = '\0';
		    consoleClass_MoveTo(self, Pposx, Pposy);
		    Pposx += fontWidth[c];
		    consoleClass_DrawString(self, tempString++, graphic_ATLEFT | graphic_BETWEENTOPANDBASELINE);
		}
	}
        consoleClass_FlushGraphics(self);
        consoleClass_SetTransferMode(self, graphic_BLACK);
    }
}

RedrawPrompt(self)
struct consoleClass *self;
{
    register short *fontWidth = fontdesc_WidthTable(EventFont, consoleClass_GetDrawable(self));
    int width = consoleClass_GetLogicalWidth(self), height = consoleClass_GetLogicalHeight(self);

    mydbg(("entering: RedrawPrompt\n"));
    ClearWindow(self);
    consoleClass_SetFont(self, EventFont);
    consoleClass_MoveTo(self, width >> 1, height / 4);
    consoleClass_DrawString(self, Pstring1, graphic_BETWEENLEFTANDRIGHT | graphic_BETWEENTOPANDBASELINE);
    consoleClass_MoveTo(self, width >> 1, height / 2);
    consoleClass_DrawString(self, Pstring2, graphic_BETWEENLEFTANDRIGHT | graphic_BETWEENTOPANDBASELINE);
    if (!strcmp(Pstring3,"==>> ")){
	consoleClass_MoveTo(self, 10, (int) (height * 0.75));
	consoleClass_DrawString(self, Pstring3, graphic_ATLEFT | graphic_BETWEENTOPANDBASELINE);
	consoleClass_FlushGraphics(self);
	Pposy = (int) (consoleClass_GetLogicalHeight(self) *.75);
	Pposx = 10 + (fontWidth['='] * 2) + (fontWidth['>'] * 2) + fontWidth[' '];
	if (Pstring4[0] != '\0'){
	    int i;
	    consoleClass_MoveTo(self, Pposx, Pposy);
	    for(i = 0; Pstring4[i] != '\0'; i++){
		Pposx += fontWidth[Pstring4[i]];
	    }
	    consoleClass_DrawString(self, Pstring4, graphic_ATLEFT | graphic_BETWEENTOPANDBASELINE);
	}
    }
    else{
	consoleClass_MoveTo(self, width >> 1, (int) (height * 0.75));
	consoleClass_DrawString(self, Pstring3, graphic_BETWEENLEFTANDRIGHT | graphic_BETWEENTOPANDBASELINE);
    }
}    


