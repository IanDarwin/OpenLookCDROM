/* figtoolv.ch - drawing editor toolset */
/*
	Copyright Carnegie Mellon University 1992 - All rights reserved
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

  $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/figure/RCS/figtoolv.ch,v 1.2 1992/12/14 20:46:05 rr2b R6tape $
*/

class figtoolview [figtoolv] : lpair {

    classprocedures:
      InitializeClass() returns boolean; 
      InitializeObject(struct figtoolview *self) returns boolean;
      FinalizeObject(struct figtoolview *self);

    overrides:
      ObservedChanged(struct observable *observed, long status);
      PostMenus(struct menulist *ml);
      PostKeyState(struct keystate *ks);
      UnlinkTree();

    methods:
      SetPrimaryView(struct figview *v) returns boolean;
      SetExpertMode(boolean val);
      AbortObjectBuilding();

    macromethods:
      GetPrimaryView() ((self)->primaryview)
      GetToolnum() ((self)->toolnum)
      GetToolProc() ((self)->toolproc)
      SetMoribund(val) ((self)->moribund = (val))
      GetSnapGrid()  ((self)->snapgrid)
      SnapToGrid(x, y)  ((((self)->snapgrid) ? ((x) += (self)->snapgrid/2, (x) -= ((x) % (self)->snapgrid)) : 0), (((self)->snapgrid) ? ((y) += (self)->snapgrid/2, (y) -= ((y) % (self)->snapgrid)) : 0))

    data:
      struct menulist *Menus;
      struct keystate *Keystate;
      struct figview *primaryview;
      boolean expertmode;
      boolean moribund;

      struct figobj **dummylist;
      struct rectangle *rectlist;
      long rect_size;
      long *tmplist;
      long tmp_size;
      long tmpnum;
      struct figobj *creating;
      boolean selectonup;
      boolean LockCreateMode;
      long submode, rock, rockx, rocky;
      long lastx, lasty, lastw, lasth, lastb;
      boolean selectdeep;
      struct view *rockview;

      struct figattr *menuatt;

      struct lpair *lp1, *lp2, *lp3, *lp4, *lp5, *lp6, *lp7;
      struct fontsel *fontsel;
      struct fontselview *vfontsel;
      struct stringtbl *tooltbl;
      struct strtblview *vtooltbl;
      boolean toolextras;
      short *toolacc;
      int toolnum;
      struct stringtbl *cmdtbl;
      struct strtblview *vcmdtbl;
      short *cmdacc;
      struct view *(*toolproc)();
      struct stringtbl *shadetbl;
      struct strtblview *vshadetbl;
      short *shadeacc;
      int shadenum;
      struct stringtbl *textpostbl;
      struct strtblview *vtextpostbl;
      short *textposacc;
      int textposnum;
      struct stringtbl *linewidthtbl;
      struct strtblview *vlinewidthtbl;
      short *linewidthacc;
      short *linewidthlist;
      int linewidths_num;
      int linewidthnum;
      struct stringtbl *rrectcornertbl;
      struct strtblview *vrrectcornertbl;
      short *rrectcorneracc;
      short *rrectcornerlist;
      int rrectcorners_num;
      int rrectcornernum;
      struct stringtbl *colortbl;
      struct strtblview *vcolortbl;
      short *coloracc;
      char **colorlist;
      int colors_num;
      int colornum;
      struct stringtbl *snapgridtbl;
      struct strtblview *vsnapgridtbl;
      short *snapgridacc;
      short *snapgridlist;
      int snapgrids_num;
      int snapgridnum;
      int snapgridunit;
      int snapgrid;
};
