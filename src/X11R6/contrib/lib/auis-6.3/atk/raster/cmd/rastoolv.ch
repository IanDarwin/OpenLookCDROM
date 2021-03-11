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


 

#define	ZRTOOLS_NUM	(17)
#define	ZRCMDS_NUM	(6)
#define ZRPASTEMODES_NUM (4)
#define	ZRPATTERNS_NUM	(16)
#define	ZRBRUSHES_NUM	(14)

#define	RASTOOL_PAN	(1)
#define	RASTOOL_SELECT	(2)
#define RASTOOL_TOUCHUP (3)

#define	PASTEMODE_COPY	(1)
#define	PASTEMODE_OR	(2)
#define	PASTEMODE_XOR	(3)

struct span {
    long x, y;
    struct span *next;
};

class rastoolview [rastoolv] : lpair {
    classprocedures:
      InitializeClass() returns boolean; 
      InitializeObject(struct rastoolview *self) returns boolean;
      FinalizeObject(struct rastoolview *self);
    overrides:
      ObservedChanged(struct observable *observed, long status);
      UnlinkTree();
      PostMenus(struct menulist *ml);
      PostKeyState(struct keystate *ks);
    methods:
      SetPrimaryView(struct rasterview *zrview) returns boolean;
      WantSelectionHighlighted() returns boolean;
    macromethods:
      GetPrimaryView() ((self)->primaryview)
      GetToolnum() ((self)->toolnum)
      GetPasteMode() ((self)->pastemode)
      GetToolProc() ((self)->toolproc)
      SetMoribund(val) ((self)->moribund = (val))
    data:
      short moribund;
      struct menulist *Menus;
      struct keystate *Keystate;
      struct stringtbl *patterntbl;
      struct strtblview *vpatterntbl;
      short patternacc[ZRPATTERNS_NUM];
      struct stringtbl *brushtbl;
      struct strtblview *vbrushtbl;
      short brushacc[ZRBRUSHES_NUM];
      struct stringtbl *commandtbl;
      struct strtblview *vcommandtbl;
      short commandacc[ZRCMDS_NUM];
      struct stringtbl *pastemodetbl;
      struct strtblview *vpastemodetbl;
      short pastemodeacc[ZRPASTEMODES_NUM];
      struct stringtbl *tooltbl;
      struct strtblview *vtooltbl;
      short toolacc[ZRTOOLS_NUM];
      struct lpair *lpair1, *lpair2, *lpair3;
      int patternnum;
      unsigned char *pattern;
      int brushnum;
      unsigned char *brush;
      long pastemode;
      int pastemodenum;

      int toolnum;
      void (*toolproc)();
      struct rasterview *primaryview;
      long rockx, rocky, rockw, rockh, rock;
      double lastvx, lastvy;
      long lastx, lasty;
      int sprayradius;
      double springconst;
      int color;
      int fillbit;
      struct span *fillstack;
      struct rasterimage *fillpix;
      struct raster *pasteraster, *unpasteraster;
      boolean unpaste;
      struct dataobject *primaryobj;
};
