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
%       @(#) panel.cps 9.1 87/11/05
%
% panel.cps
%
% contains
%    ps_SetupPanel: define procedures to initialize the panel canvas
%


%
% ps_SetupPanel
%
% define procedures to initialize the panel canvas
%

cdef ps_SetupPanel()

	% make some definitions
	/ButtonHorMargin 10 def		% margin around text in a button
	/ButtonVertMargin 0 def
	/PanelLeftMargin 10 def		% margin around buttons in a panel
	/PanelBottomMargin 10 def
	/PanelButtonGap 10 def		% space between buttons

	% make sure liteitem.ps has been loaded
	systemdict /Item known not { (NeWS/liteitem.ps) run } if

	% MakeButton: x y Label Proc -> Next_x Next_y Button
	% create a button with the given label at the given location,
	% install it in the canvas, and return its handle along with 
	% a location for the next button (based on the size of this one)
	/MakeButton {
	    PanelCanvas setcanvas	% x y Label Proc
	    % first determine the size of the button given the label
	    1 index stringbbox 4 2 roll pop pop	% x y Label Proc w h
	    ButtonVertMargin 2 mul add	% x y Label Proc w h+
	    exch ButtonHorMargin 2 mul add exch	% x y Label Proc w+ h+
	    % save a copy of the width
	    1 index 7 1 roll		% w+ x y Label Proc w+ h+
	    % now construct the button
	    PanelCanvas 3 1 roll	% w+ x y Label Proc Canvas w+ h+
	    /new ButtonItem send	% w+ x y Button
	    dup begin
	        /ItemLabelFont ButtonFont def
	        /ItemFrame 1 def
	        /ItemGap 2 def
	    end
	    3 copy /move exch send	% w+ x y Button
	    % compute the location of the next button and leave on stack
	    4 2 roll add PanelButtonGap add 3 1 roll % x+ y Button
	} def

	% CreateButtons: void -> void
	% create all the items in the panel canvas
	/CreateButtons {
	    /Buttons 30 dict dup begin
		% put the x and y location of the first button on the
		% stack; MakeButton will compute the location for
		% successive buttons in the same row
		PanelLeftMargin
		MessageFont fontheight 2 mul PanelBottomMargin add

		(Double)
		{ (%\n) [DoubleRequestCode] SendToGammonClient }
		MakeButton
		/DoubleButton exch def

		(Accept Double)
		{ (%\n) [AcceptDoubleCode] SendToGammonClient }
		MakeButton
		/AcceptButton exch def

		(Refuse Double)
		{ (%\n) [RefuseDoubleCode] SendToGammonClient }
		MakeButton
		/RefuseButton exch def

		(Show Last Move)
		{ (%\n) [ShowLastMoveCode] SendToGammonClient }
		MakeButton
		/ShowButton exch def

		(Redo Move)
		{ (%\n) [RedoLastMoveCode] SendToGammonClient }
		MakeButton
		/RedoButton exch def

		% start second row: pop the location computed by
		% MakeButton and push a new one
		pop pop PanelLeftMargin PanelBottomMargin

		(Redo Entire Move)
		{ (%\n) [RestartMoveCode] SendToGammonClient }
		MakeButton
		/RedoAllButton exch def

		(Forfeit)
		{ (%\n) [ForfeitCode] SendToGammonClient }
		MakeButton
		/ForfeitButton exch def

		(Quit)
		{ (%\n) [QuitCode] SendToGammonClient }
		MakeButton
		/QuitButton exch def

		(New Game)
		{ (%\n) [NewGameCode] SendToGammonClient }
		MakeButton
		/NewButton exch def

		/ColorItem (Color:) [HumanColorName ComputerColorName]
		/Right { ChangeColor } PanelCanvas 0 0 /new CycleItem send
		dup begin
		    /ItemLabelFont ButtonFont def
		    /ItemFont ButtonFont def
		    /ItemGap 2 def
		end
		4 2 roll /move 3 index send def

	    end def % Buttons dictionary definition
	} def % CreateButtons

	% RepaintPanel: void -> void
	% repaint the panel canvas
	/PaintPanel {
	    gsave
	    PanelCanvas setcanvas
	    White setcolor clippath fill
	    Buttons paintitems
	    grestore
	} def

	% CreatePanel: void -> void
	% initialize the panel canvas
	/CreatePanel {
	    CreateButtons
	    Buttons forkitems
	} def
