/* fontselv.ch - font selection inset view */
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

  $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/figure/RCS/fontselv.ch,v 1.2 1992/12/14 20:45:16 rr2b R6tape $
*/

class fontselview [fontselv] : lpair {

    classprocedures:
      InitializeClass() returns boolean; 
      InitializeObject(struct fontselview *self) returns boolean;
      FinalizeObject(struct fontselview *self);

    overrides:
      ObservedChanged(struct fontsel *fontsel, long status);
      SetDataObject(struct dataobject *dobj);

    methods:
      ShowExtraOption();
      SetExtraOptionString(char *val);

    data:
      struct lpair *lp1, *lp2;
      struct fontsample *sample;
      boolean showdefault;
      char *defaultstring;

      struct stringtbl *familytbl;
      struct strtblview *vfamilytbl;
      short *familyacc, familyextra;
      char **familylist;
      int families_num;
      int familynum;

      struct stringtbl *styletbl;
      struct strtblview *vstyletbl;
      short *styleacc, styleextra;
      long *stylelist;
      int styles_num;
      long style_mask;

      struct stringtbl *sizetbl;
      struct strtblview *vsizetbl;
      short *sizeacc, sizeextra;
      short *sizelist;
      int sizes_num;
      int sizenum;

};
