/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
*Permission to use, copy, modify, and distribute this software and its 
*documentation for any purpose is hereby granted without fee, 
*provided that the above copyright notice appear in all copies and that 
*both that copyright notice, this permission notice, and the following 
*disclaimer appear in supporting documentation, and that the names of 
*IBM, Carnegie Mellon University, and other copyright holders, not be 
*used in advertising or publicity pertaining to distribution of the software 
*without specific, written prior permission.
*
*IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
*DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
*ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
*SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
*BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
*DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
*WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
*ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
*OF THIS SOFTWARE.
* $
*/


 


/* ************************************************************ */
#define preview_MaxDviFonts 100
#define preview_MaxWMFonts 200

#define preview_DISPLAY_RESOLUTION 72

#define preview_MAXPageTable 1024

typedef char Preview_Line[256];
typedef short preview_pagenumber;
typedef long preview_coordinate;
typedef short preview_pagetableindex;

#define Boolean boolean
struct preview_fontname
{
   short    number;
   struct fontdesc *font;
};

enum preview_ScrollDirection  {
    preview_NoScroll,
    preview_ScrollForward,
    preview_ScrollBackward,
    preview_ScrollRight,
    preview_ScrollLeft,
    preview_MultipleScroll
};

class preview:view {
overrides:
    GetApplicationLayer() returns struct view *;
    DeleteApplicationLayer(struct view *applicationLayer);
    FullUpdate(enum view_UpdateType type, long left, long top, long width, long right);
    Update();
    Hit (enum view_MouseAction action, long x, long y, long numberOfClicks) returns struct preview *;
/*     DesiredSize(long width, long height, enum view_DSpass pass, long *dWidth, long *dheight) returns enum view_DSattributes; */
/*     GetOrigin(long width, long height, long *originX, long *originY);
 */    ReceiveInputFocus();
    LoseInputFocus();
    GetInterface(char *interfaceName) returns char *;
    WantUpdate(struct view *requestor);
methods:
    DviToDisplay();
    ReadyToQuit() returns int;
classprocedures:
    Create(FILE *f,char *fname,char *fbase,boolean compleated,boolean scale) returns struct preview *;
    InitializeObject(struct preview *self) returns boolean;
    FinalizeObject(struct preview *self);
data:
    enum preview_ScrollDirection scroll;
    int scrollLine;				/* line to use for scrolling. */
    int scrollDist;				/* distance to scroll */
Boolean debug;
Boolean DoScaling;
boolean DviFileComplete;
FILE * DviFileIn;
FILE * DviFileOut;
int DisplayResolution;
int DviFileLength;
preview_coordinate   InputResolution;
preview_coordinate   xoff, yoff;
preview_coordinate   PhysicalX, PhysicalY;
preview_coordinate   LogicalX, LogicalY;
preview_coordinate   xPixelsPerPage;
preview_coordinate   yPixelsPerPage;
preview_coordinate   Centre;
preview_coordinate   CentreY;
Boolean CharactersOnThisPage /* returned by DviToDisplay */;
Boolean RedrawRequested;
Boolean SizeChanged;
preview_coordinate   WindowWidth;
preview_coordinate   WindowHeight;
preview_coordinate   minWidth;
preview_coordinate   minHeight;
short PollCount;
int peekc;
int   DviFonts[preview_MaxDviFonts];
struct preview_fontname   WMFonts[preview_MaxWMFonts];
int   NWMFonts; 
int   curfont ;  
int   cursize;
int   slant;
boolean hasInputFocus;
struct keystate *keystate;
struct menulist *menulist;
struct proctable_Entry *ScaleProc;
struct proctable_Entry *FullSizeProc;
struct proctable_Entry *SetPageProc;
struct {
	  preview_pagenumber PageNumber;
	  long  FileOffset;
} PageTable[preview_MAXPageTable];
preview_pagetableindex NumberofPageTableEntries;
Boolean       FindFirstPage;
preview_pagetableindex    LowestNonBlankPageIndex ;
preview_pagetableindex    CurrentPageTableIndex ;
Preview_Line	      WindowTitle;
char          CurrentCursor;
int   lastc ;
boolean CreatedTemp;
char DviBaseName[512];
char DviFileName[L_tmpnam];
int nowreading;
Preview_Line menubuf;
int menupage;
int printpending;
int quitpending;
int CursorChanged;
struct cursor *cursor;
};
