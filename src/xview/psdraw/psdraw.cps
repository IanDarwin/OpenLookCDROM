%!
%
% PostScript drawing routines for libxvps demo program psdraw
%
% @(#)psdraw.cps	2.4 91/10/15 Copyright 1991 Sun Microsystems
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

cdef ps_fillcanvas(i)
	/g currentgray def
	i setgray clippath fill
	g setgray

cdef ps_fill_color(float g)
	g setgray fill

cdef ps_setlinewidth(float f)
	f setlinewidth

cdef ps_setlinejoin(i)
	i setlinejoin

cdef ps_setlinecap(i)
	i setlinecap

cdef ps_setmiterlimit(float f)
	f setmiterlimit

cdef ps_setlinedash(i)
	i
	{	% 0-3 => -
		0  { [] }
		1  { [10 15] }
		2  { [20 15] }
		3  { [10 15 20 15] }
	} case
	0 setdash

cdef ps_draw_line(x0, y0, x1, y1)
	x0 y0 moveto
	x1 y1 lineto

cdef ps_draw_rect(x0, y0, x1, y1)
	x0 y0 moveto
	x0 y1 lineto
	x1 y1 lineto
	x1 y0 lineto
	closepath

cdef ps_draw_circle(x, y, r)
	x y r 0 360 arc

cdef ps_draw_ellipse(x0, y0, x1, y1)
	matrix currentmatrix
	x0 y0 translate
	x1 x0 sub y1 y0 sub scale
	.5 .5 .5 0 360 arc
	setmatrix

cdef ps_line_stroke(style, float line_color)
	style 0 eq { line_color setgray stroke } if
	style 1 eq { gsave strokepath [] 0 setdash 0 setgray
		     0 setlinewidth stroke grestore } if
	style 2 eq { gsave 0.7 setgray stroke grestore gsave strokepath
		     [] 0 setdash line_color setgray 0 setlinewidth
		     stroke grestore } if
	
cdef ps_draw_text(x, y, font_size, string font_name, string s)
	font_name cvn findfont font_size scalefont setfont
	clippath pathbbox 0 exch translate pop pop pop 1 -1 scale
	x clippath pathbbox 4 1 roll pop pop pop y sub moveto
	s show
	clippath pathbbox 0 exch translate pop pop pop 1 -1 scale
