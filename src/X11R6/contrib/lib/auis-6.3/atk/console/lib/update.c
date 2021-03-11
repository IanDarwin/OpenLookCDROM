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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/console/lib/RCS/update.c,v 2.10 1992/12/15 21:31:10 rr2b R6tape $";
#endif


 

#include <class.h>
#include <conclass.ih>
#include <fontdesc.ih>
#include <rect.h>
#include <graphic.ih>
#include <view.ih>
#include <scroll.ih>
#include <console.h>
#include <andrewos.h> /* sys/time.h sys/types.h */
#include <errno.h>
#include <ctype.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <signal.h>

int DynamicXmin, DynamicXmax, DynamicYmin, DynamicYmax, XGrowthRange, YGrowthRange;


extern struct fontdesc *SetupFont();

ScaleCoordinate(self, old, IsX)
struct consoleClass *self;
int old;
boolean IsX;
{
    int Gmin, Gmax, DesiredRange, Range, GrowthRange;

    mydbg(("entering: ScaleCoordinate\n"));
    if (IsX) {
        Gmin = DynamicXmin;
        Gmax = DynamicXmax;
        DesiredRange = MaxWidth;
        Range = consoleClass_GetLogicalWidth(self);
        GrowthRange = XGrowthRange;
    } else {
        Gmin = DynamicYmin;
        Gmax = DynamicYmax;
        DesiredRange = MaxHeight;
        Range = consoleClass_GetLogicalHeight(self);
        GrowthRange = YGrowthRange;
    }
    if (Range <= DesiredRange || Gmin < 0 || Gmin == 0 && Gmax >= ScaleFactor) {
        return(old * Range / ScaleFactor);
    }
    if (old < Gmin) {
        return(old * DesiredRange / ScaleFactor);
    }
    if (old > Gmax) {
        return(Range - (((ScaleFactor - old) * DesiredRange) / ScaleFactor));
    }
    return( ((Gmin * DesiredRange) / ScaleFactor) + (((old - Gmin) * GrowthRange) / (Gmax - Gmin)));
}



RedrawDisplays(self)
struct consoleClass *self;
{
    struct display *disp;
    char    ThisLabel[256];
    int	    l;
    long    j,
	    k;

    struct rectangle    clpRect,
        clpRect2;

    mydbg(("entering: RedrawDisplays\n"));
    consoleClass_WantUpdate(self,self);

    consoleClass_GetLogicalBounds(self, &clpRect);
    consoleClass_SetClippingRect(self, &clpRect);
    consoleClass_SetTransferMode(self, graphic_COPY);
    consoleClass_FillRect(self, &clpRect, consoleClass_WhitePattern(self));
    consoleClass_FlushGraphics(self);

    XGrowthRange = consoleClass_GetLogicalWidth(self) - (((ScaleFactor - DynamicXmax + DynamicXmin) * MaxWidth) / ScaleFactor);
    YGrowthRange = consoleClass_GetLogicalHeight(self) - (((ScaleFactor - DynamicYmax + DynamicYmin) * MaxHeight) / ScaleFactor);
    for (disp = VeryFirstDisplay; disp; disp = disp->NextOfAllDisplays) {
        if (disp->WhatToDisplay->IsDisplaying) {

	    if (disp->ScrollLogView != NULL){
		scroll_UnlinkTree(disp->ScrollLogView);
	    }

            disp->Xmin = ScaleCoordinate(self, disp->RelXmin, TRUE);
            disp->Xmax = ScaleCoordinate(self, disp->RelXmax, TRUE);
            disp->Ymin = ScaleCoordinate(self, disp->RelYmin, FALSE);
            disp->Ymax = ScaleCoordinate(self, disp->RelYmax, FALSE);
            disp->Width = disp->Xmax - disp->Xmin;
            disp->FullHeight = disp->Ymax - disp->Ymin;
            disp->XCenter = disp->Xmin + disp->Width / 2;
            disp->YCenter = disp->Ymin + disp->FullHeight / 2;
            if (disp->Boxed && (!disp->DependentUponVariables || IntrnlVars[disp->WhichVariable].Value == disp->AppearIfTrue)) {
		consoleClass_SetTransferMode(self, graphic_BLACK);
		consoleClass_DrawRectSize(self, disp->Xmin, disp->Ymin, disp->Width, disp->FullHeight);
                disp->Xmin += 3;
                disp->Ymin += 3;
                disp->Xmax -= 3;
                disp->Ymax -= 3;
		disp->Width -= 6;
		disp->FullHeight -= 6;
            }
            if (disp->IsLabelling
                && (!disp->DependentUponVariables ||
                    IntrnlVars[disp->WhichVariable].Value == disp->AppearIfTrue)) {
                        long    maxheight,
                            maxwidth;

                        maketext(self, ThisLabel, disp, 2);
                        if (disp->AdjustLabelFont) {
                            maxheight = disp->Ymax - disp->Ymin;
                            maxwidth = disp->Width;
                            if (disp->IsLabelling == TOP_LABEL || disp->IsLabelling == BOTTOM_LABEL) {
                                maxheight = maxheight / disp->AdjustLabelFont;
                                if (maxheight > disp->MaxLabelFontSize) {
                                    maxheight = disp->MaxLabelFontSize;
                                }
                            }
                            else {
                                maxwidth = maxwidth / disp->AdjustLabelFont;
                            }
                            l = FontCount;
                            do {
                                --l;
                            } while (AvailFontPts[l] > maxheight && l > 0);
                            if (l == 0) {
                                ++l;
                            }
                            do {
                                --l;
				if (FontsAvail[l] == 0) {
				    FontsAvail[l] = SetupFont(AvailFontNames[l]);
                                }
                                consoleClass_SetFont(self, FontsAvail[l]);
                                fontdesc_StringSize(FontsAvail[l], consoleClass_GetDrawable(self), ThisLabel, &j, &k);
                                k = FontsAvail[l]->summary.maxHeight;
                            } while ((j > maxwidth || (k > maxheight)) && l > 0);
                            disp->Labelfont = FontsAvail[l];
                        }
                        else {
                            consoleClass_SetFont(self, disp->Labelfont);
                            fontdesc_StringSize(disp->Labelfont, consoleClass_GetDrawable(self), ThisLabel, &j, &k);
                            k = disp->Labelfont->summary.maxHeight;
                            maxheight = k;
                            maxwidth = j;
                        }
                        if (k > maxheight) {
                            maxheight = k;
                        }
                        if (j > maxwidth) {
                            maxwidth = j;
                        }
                        switch (disp->IsLabelling) {
                            case BOTTOM_LABEL:
                                consoleClass_MoveTo(self, disp->XCenter, disp->Ymax - k / 2);
                                consoleClass_DrawString(self, ThisLabel, graphic_BETWEENTOPANDBASELINE | graphic_BETWEENLEFTANDRIGHT);
                                disp->Ymax -= (maxheight + 3);
                                if (disp->Boxed) {
                                    consoleClass_MoveTo(self, disp->Xmin - 3, disp->Ymax);
                                    consoleClass_DrawLineTo(self, disp->Xmax + 3, disp->Ymax);
                                    disp->Ymax -= 3;
                                }
                                break;
                            case TOP_LABEL:
                                consoleClass_MoveTo(self, disp->XCenter, disp->Ymin + k / 2);
                                consoleClass_DrawString(self, ThisLabel, graphic_BETWEENTOPANDBASELINE | graphic_BETWEENLEFTANDRIGHT);
                                disp->Ymin += maxheight + 3;
                                if (disp->Boxed) {
                                    consoleClass_MoveTo(self, disp->Xmin - 3, disp->Ymin);
                                    consoleClass_DrawLineTo(self, disp->Xmax + 3, disp->Ymin);
                                    disp->Ymin += 3;
                                }
                                break;
                            case LEFT_LABEL:
                                consoleClass_MoveTo(self, disp->Xmin + j / 2, disp->YCenter);
                                consoleClass_DrawString(self, ThisLabel, graphic_BETWEENTOPANDBASELINE | graphic_BETWEENLEFTANDRIGHT);
                                disp->Xmin += maxwidth + 3;
                                if (disp->Boxed) {
                                    consoleClass_MoveTo(self, disp->Xmin, disp->Ymin - 3);
                                    consoleClass_DrawLineTo(self, disp->Xmin, disp->Ymax + 3);
                                    disp->Xmin += 3;
                                }
                                break;
                            case RIGHT_LABEL:
                                consoleClass_MoveTo(self, disp->Xmax - j / 2, disp->YCenter);
                                consoleClass_DrawString(self, ThisLabel, graphic_BETWEENTOPANDBASELINE | graphic_BETWEENLEFTANDRIGHT);
                                disp->Xmax -= maxwidth + 3;
                                if (disp->Boxed) {
                                    consoleClass_MoveTo(self, disp->Xmax, disp->Ymax + 3);
                                    consoleClass_DrawLineTo(self, disp->Xmax, disp->Ymin - 3);
                                    disp->Xmax -= 3;
                                }
                                break;
                        }
                        disp->Width = disp->Xmax - disp->Xmin;
                        disp->FullHeight = disp->Ymax - disp->Ymin;
                        disp->XCenter = disp->Xmin + disp->Width / 2;
                        disp->YCenter = disp->Ymin + disp->FullHeight / 2;
                    }
            if (disp->WhatToDisplay->Value >= disp->Threshhold && disp->WhatToDisplay->Value <= disp->Ceiling && (!disp->DependentUponVariables || IntrnlVars[disp->WhichVariable].Value == disp->AppearIfTrue)) {
                disp->InRange = TRUE;
                if (disp->Clipped) {
                    rectangle_SetRectSize(&clpRect2, disp->Xmin, disp->Ymin, disp->Width, disp->FullHeight + 1);
                    consoleClass_SetClippingRect(self, &clpRect2);
                }
                disp->Inverted = FALSE;
                (*(disp->DrawFunction))(self, REDRAW, disp);
                if (disp->MayFlash && disp->FlashStyle) {
                    HighlightDisplay(self, disp);
                }
                consoleClass_SetClippingRect(self, &clpRect);
            }
            else {
                disp->InRange = FALSE;
            }
	}
    }
}

