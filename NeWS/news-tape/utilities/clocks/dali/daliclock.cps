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

% CPS PostScript definitions to support clocks.
%
% "@(#)roundclock.cps 9.3 88/01/18
%
% Copyright (c) 1987 by Sun Microsystems, Inc.
%/

% Daliesque modifications by Stan Switzer sjs@ctt.bellcore.com 1/28/89
%   Explaination of parameters:
%     Assuming circle inscribes a square in [-50,50]X[-50,50] square
%	(rX,rY) intersection of bend line with right edge of boundin square
%	Ang	angle of intersection
%	A1	angle from (0,0) to left intersection of bend with circle
%	A2	angle from (0,0) to right intersection of bend with circle
%	Sk	skew factor above bend
%	H	height factor above bend
% Each set of parameters below gives interesting results.
% The first is the most complex, but the last (in my opinion) gives the
% most realistinc 3D effect as it can be "draped" over the edge of another
% window for comic/artictic effect.

cdef ps_createclock()
    % /rX 50 def /rY -20 def /Ang -30 def /A1 141 def /A2 338 def /Sk .7 def /H 1 def
    % /rX 50 def /rY 15 def /Ang 0 def /A2 17.5 def /A1 180 A2 sub def /Sk 1.2 def /H .5 def
    % /rX 50 def /rY 15 def /Ang 0 def /A2 17.5 def /A1 180 A2 sub def /Sk 1.5 def /H .4 def
    /rX 50 def /rY 15 def /Ang 0 def /A2 17.5 def /A1 180 A2 sub def /Sk 1.7 def /H .3 def
    /xform {
	rX rY translate
	Ang rotate
	[ 1 0 Sk H 0 0 ] concat
	Ang neg rotate
	rX neg rY neg translate
    } def
    /lowclip {
	matrix currentmatrix
	-50 -50 moveto 100 0 rlineto 0 50 rY add rlineto
	Ang rotate -200 0 rlineto closepath clip newpath
	setmatrix
    } def
    /hiclip {
	matrix currentmatrix
	-50 50 moveto 100 0 rlineto 0 -50 rY add rlineto
	Ang rotate -200 0 rlineto closepath clip newpath
	setmatrix
    } def
    /dopaint { % proc =>
	dup gsave lowclip exec grestore
	gsave xform hiclip exec grestore
    } def
    /window framebuffer /new DefaultWindow send def
    {
	/IconLabel (DaliClock) def
	/IconImage /iconedit def
	/ClientMinWidth 4 def /ClientMinHeight 4 def
	/FixFrame { (F) print } def
	/PaintClient { (P) print } def
	/ShapeFrameCanvas {
	    gsave ParentCanvas setcanvas
	    /FrameHeight FrameHeight
	        50 rY sub H mul 50 rY add add 100 div div cvi def   
	    FrameX FrameY translate
	    FrameWidth 100 div FrameHeight 100 div scale
	    50 50 translate
	    0 0 50 A1 A2 arc
	    xform
	    0 0 50 A2 A1 arc
	    flattenpath FrameCanvas setcanvasshape
	    grestore
    	} def
	/ShapeClientCanvas { } def
	/CreateClientCanvas { /ClientCanvas FrameCanvas newcanvas def } def
	/PaintFrame { } def
	/PaintFocus {
	    gsave FrameCanvas setcanvas
	    KeyFocus? {KeyFocusColor} {backgroundcolor} ifelse setcolor
	    calctransform
	    { 0 0 40 0 360 arc stroke } dopaint
	    grestore
	} def
    } window send
    /reshapefromuser window send				% Shape it.
    /map window send  % Map the window. (Damage causes PaintClient to be called)

    window /FrameCanvas get setcanvas
    /calctransform {
	initmatrix initclip
	{ FrameWidth 100 div FrameHeight 100 div } window send scale
	50 50 translate
    } def
    /RDC {
	window /FrameCanvas get setcanvas
	damagepath clipcanvas
	calctransform
	{ drawclockframe } dopaint
	clipcanvas
    } def
    /F {
	/PaintFocus window send
    } def

cdef ps_white() W
cdef ps_black() B
cdef ps_redrawclock() RDC
cdef ps_redrawfocus() F

%
% ps_hand draws a plain clock hand.
%
cdef ps_hand(rot,rad)
    gsave 
    { rot rotate 0 0 moveto 0 rad rlineto stroke } dopaint
    grestore

%
% ps_fancy_hand draws a fancy clock hand.
%
cdef ps_fancy_hand(rot,rad)
    gsave 
    { rot rotate newpath -5 0 moveto 0 0 5 180 360 arc
    0 rad rlineto -5 5 rlineto -5 -5 rlineto closepath
    fill } dopaint
    grestore

cdef ps_initializeclock()
    /drawclockframe { 
	bordercolor setcolor clippath fill
	0 0 45 0 360 arc
	backgroundcolor setcolor fill
	textcolor setcolor
	12 {0 40 moveto 0 5 rlineto stroke 30 rotate} repeat
    } def
    /B {
	textcolor setcolor
    } def
    /W {
	backgroundcolor setcolor
    } def


cdef ps_fancy_initializeclock()
    /drawclockframe {
	.75 monochromecanvas {setgray} {.7 .7 setrgbcolor} ifelse fill
	clippath fill
	0 0 40 0 360 arc
	1 monochromecanvas {setgray} {1 1 setrgbcolor} ifelse fill
	1 monochromecanvas {setgray} {1 0 setrgbcolor} ifelse fill
	0 45 5 0 360 arc fill
    } def
    /B {
	.5 monochromecanvas {setgray} {.6 1 setrgbcolor} ifelse fill
    } def
    /W {
	1 monochromecanvas {setgray} {1 1 setrgbcolor} ifelse fill
    } def
