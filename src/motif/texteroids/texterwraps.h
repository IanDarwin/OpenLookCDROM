/* texterwraps.h generated from texterwraps.psw
   by unix pswrap V1.009  Wed Apr 19 17:50:24 PDT 1989
 */

#ifndef TEXTERWRAPS_H
#define TEXTERWRAPS_H

extern void Clear( /* float w, h; int rv; */ );

extern void CreateFont( /* int size; char *name; */ );

extern void GetAllSizes( /* char *text; int n; float llx[], lly[], urx[], ury[], wid[]; */ );

extern void GetSize( /* char *text; float *llx, *lly, *urx, *ury, *wid; */ );

extern void RenderString( /* float x, y, dx, dy, angle; char *string; float red, green, blue; */ );

extern void SetXDrawable( /* int drawable, height; */ );

extern void GetInvCTM( /* float invctm[]; */ );

#endif /* TEXTERWRAPS_H */
