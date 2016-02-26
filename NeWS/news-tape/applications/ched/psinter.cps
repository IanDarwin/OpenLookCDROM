%
% This file is a product of Sun Microsystems, Inc. and is provided for
% unrestricted use provided that this legend is included on all tape
% media and as a part of the software program in whole or part.  Users
% may copy or modify this file without charge, but are not authorized to
% license or distribute it to anyone else except as part of a product
% or program developed by the user.
% 
% THIS FILE IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
% WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
% PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
% 
% This file is provided with no support and without any obligation on the
% part of Sun Microsystems, Inc. to assist in its use, correction,
% modification or enhancement.
% 
% SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
% INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS FILE
% OR ANY PART THEREOF.
% 
% In no event will Sun Microsystems, Inc. be liable for any lost revenue
% or profits or other special, indirect and consequential damages, even
% if Sun has been advised of the possibility of such damages.
% 
% Sun Microsystems, Inc.
% 2550 Garcia Avenue
% Mountain View, California  94043
%

#define DEFSTR_TAG 0
#define REDRAW_TAG 1
#define KBD_TAG 2
#define MOUSE_TAG 3
#define SCROLL_TAG 4
#define THUMB_TAG 5
C: short currentcanvas, currentfont;
C: #define ps_usecanvas(canvas) ((canvas) != currentcanvas ? ps_DO_usecanvas(currentcanvas = (canvas)) : 0)
cdef ps_DO_usecanvas(canvas) canvas UWN
C: #define ps_usefont(font) ((font) != currentfont ? ps_DO_usefont(currentfont = (font)) : 0)
cdef ps_DO_usefont(token font) font setfont
cdef ps_defstr(string name, size, index, length, bbheight, descent) => DEFSTR_TAG (bbheight, descent, length)
	name findfont size scalefont dup index DFS
cdef ps_getint(fixed x) => (x)
cdef ps_makewindow(index, string label) index label MKW
cdef ps_destroywindow(index) index load /destroy send
cdef ps_redraw(window,width,height) => REDRAW_TAG (window, height, width)
cdef ps_keyboard(window, char) => KBD_TAG (window, char)
cdef ps_mouse(window, tag, x, y) => MOUSE_TAG (window, tag, x, y)
cdef ps_scroll(window, dy) => SCROLL_TAG (window, dy)
cdef ps_thumb(window, fixed y) => THUMB_TAG (window, y)
cdef ps_setscroll(window, fixed pos) pos window STS
cdef ps_startup()
#ifdef DEBUG
	(/home/norquay/jag/ched/ched.ps) run
#else
#include "ched.ps"
#endif
cdef ps_startredraw(win) win SRD
cdef ps_endredraw() ERD
cdef ps_cwidthshow(fixed x, cstring s) x 0 32 s widthshow
cdef ps_cashow(fixed x, cstring s) x 0 s ashow
cdef ps_cawidthshow(ch, fixed sp, cstring s) sp 0 32 ch 0 s awidthshow
cdef ps_eraserect(fixed x, fixed y,fixed w,fixed h) w h x y ERS
cdef ps_frmoveto(fixed x, fixed y) x y moveto
cdef ps_frlineto(fixed x, fixed y) x y lineto
cdef ps_frdrawcaret(fixed x, fixed y) x y CRT
cdef ps_startselect() gsave 5 setrasteropcode
cdef ps_endselect() stroke grestore
%cdef ps_sync_count(n) n SYN
