%
% This file is a product of Sun Microsystems, Inc. and is provided for
% unrestricted use provided that this legend is included on all tape
% media and as a part of the software program in whole or part.
% Users may copy, modify, or distribute this file at will.
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
%       @(#) message.cps 9.1 87/11/05
%
% message.cps
%
% contains
%    ps_SetupMessages: define procedures to initialize the message canvas
%


%
% ps_SetupMessages
%
% define procedures to initialize the message canvas
%

cdef ps_SetupMessages()

	% define the initial messages
	/Message () def

	/ErrorMsg () def

	% PrintMessage: string -> void
	% print the string on the message line and save it in case
	% of repaint requests
	/PrintMessage {
	    /Message exch store
	    gsave
	    MesgCanvas setcanvas
	    % fill the line with white to erase the previous message
	    0						% x
	    MessageFont fontheight			% x y
	    clippath pathbbox 4 2 roll pop pop pop	% x y w
	    MessageFont fontheight			% x y w h
	    rectpath
	    White setcolor fill
	    % print the new message
	    Black setcolor
	    MessageFont setfont
	    0 MessageFont fontheight MessageFont fontdescent add moveto
	    Message show
	    grestore
	} def

	% PrintError: string -> void
	% print the string on the error line and save it in case
	% of repaint requests
	/PrintError {
	    /ErrorMsg exch store
	    gsave
	    MesgCanvas setcanvas
	    % erase the old error message
	    0						% x
	    0						% x y
	    clippath pathbbox 4 2 roll pop pop pop	% x y w
	    MessageFont fontheight			% x y w h
	    rectpath
	    White setcolor fill
	    % print the new one
	    Black setcolor
	    MessageFont setfont
	    0 MessageFont fontdescent moveto ErrorMsg show
	    grestore
	} def

	% RepaintMessages: void -> void
	% repaint the message canvas
	/PaintMessages {
	    gsave
	    MesgCanvas setcanvas
	    White setcolor clippath fill	% clear canvas
	    Black setcolor
	    0 MessageFont fontdescent moveto ErrorMsg show
	    0 MessageFont fontheight MessageFont fontdescent add moveto
	    Message show
	    grestore
	} def
