% Copyright (C) 1988 by Martha Zimet. All rights reserved.
% This program is provided for unrestricted use, provided that this 
% copyright message is preserved. There is no warranty, and no author 
% or distributer accepts responsibility for any damage caused by this 
% program. 

% CPS header file for the function display program

#define MENUHIT_TAG 1

cdef ps_initialize()

	/displaylist {} def	% Define an empty display list
	/paintchart {	% function to paint the chart
		gsave
		win /ClientCanvas get setcanvas
		clippath pathbbox	% get window width and height
		3 div exch 13 div exch	% width/13 height/3
		scale			% new coordinate system is 5x3
		erasepage
		0 1.5 translate		% put 0,0 in the middle at the left
		0 0 moveto 13 0 lineto
		stroke			% x axis
		0 0 moveto
		displaylist		% Build the curve
		stroke			% Draw it
		grestore
	} def
	% Make window:
	/win framebuffer /new DefaultWindow send def		% Create a window
	{														% Install my stuff.
		/FrameLabel (Function Chart) def
		/PaintClient {paintchart} def
		/ClientMenu
			[(sin) (cos) (damped) (sum)]
			[{ MENUHIT_TAG tagprint /currentindex self send typedprint}]
			/new DefaultMenu send def
	} win send
	/reshapefromuser win send	% Shape it.

	% Activate window
	/map win send  % Map the window. (Damage causes PaintClient to be called)

cdef ps_begincurve()
	/displaylist {
cdef ps_endcurve()
	} def paintchart	% After the curve has been defined, paint it
cdef ps_menuhit(index) => MENUHIT_TAG (index)
cdef ps_lineto(float x, float y) x y lineto
