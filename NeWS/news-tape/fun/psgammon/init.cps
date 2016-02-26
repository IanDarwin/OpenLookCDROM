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
%       @(#) init.cps 9.1 87/11/05
%
% init.cps
%
% contains
%    ps_InitConstants: initialize constants used throughout server side
%
%    ps_CreateWindow: make the frame and start the server side off
%


%
% ps_InitConstants
%
% initialize constants used throughout server side
%

cdef ps_InitConstants()

% just like normal printf but onto the console
/errprintf {
    sprintf console exch writestring
    console flushfile
} def

	% create a new dictionary since the userdict is too small
	/GammonDict 200 dict def
	GammonDict begin

	/Debugging false def

	/ButtonFont /Times-Roman findfont 12 scalefont def
	/MessageFont /Times-Roman findfont 12 scalefont def

	/PanelLines 5 def	% vertical size of panel and message
	/MesgLines 2 def	% windows (measured in text lines in the
				% message font)

	% a palette of colors for backgammon
	/Black 0 0 0 rgbcolor def
	/White 1 1 1 rgbcolor def
	/Gold 0.8 0.5 0.2 rgbcolor def
	/Tan 0.8 0.6 0.45 rgbcolor def
	/Chocolate 0.6 0.3 0 rgbcolor def
	/DarkChocolate 0.42 0.23 0 rgbcolor def

	% set colors based on framebuffer characteritics
	gsave Black setcolor Gold contrastswithcurrent
	{
	    % living color
	    /BoardColor Tan def
	    /PieceColorA Gold def	% (PieceColorA goes with BorderColorA)
	    /BorderColorA DarkChocolate def
	    /PieceColorB DarkChocolate def
	    /BorderColorB Gold def
	    /PointColorA Black def
	    /PointColorB Chocolate def
	    /HumanColorName (Gold) def
	    /ComputerColorName (Brown) def
	}
	{
	    % boring black and white
	    /BoardColor White def
	    /PieceColorA White def	% (PieceColorA goes with BorderColorA)
	    /BorderColorA Black def
	    /PieceColorB Black def
	    /BorderColorB White def
	    /PointColorA Black def
	    /PointColorB White def
	    /HumanColorName (White) def
	    /ComputerColorName (Black) def
	} ifelse
	/HumanPieceColor PieceColorA def
	/HumanBorderColor BorderColorA def
	/ComputerPieceColor PieceColorB def
	/ComputerBorderColor BorderColorB def

	% the three major canvases
	/PanelCanvas null def
	/MesgCanvas null def
	/BoardCanvas null def

	/HumanLeftDie 0 def
	/HumanRightDie 0 def
	/ComputerLeftDie 0 def
	/ComputerRightDie 0 def
	/CubeValue 0 def
	/HumanScore 0 def
	/ComputerScore 0 def
	/HumanName (human) def
	/DoublingCubeIsRightSideUp true def

	/HumanCanMove false def	% true when it is human's turn

	% IdealBoardSizeRatio is the ideal ratio of the width
	% of the board canvas to the height of the board canvas
	/IdealBoardSizeRatio 3 2 div def

	% compute a good fixed size
	/FixedBoardHeight 400 def
	/FixedBoardWidth FixedBoardHeight IdealBoardSizeRatio mul def
	LiteWindow begin
	    BorderTop BorderBottom 2 mul BorderLeft BorderRight
	end
	FixedBoardWidth add add /FixedWidth exch def
	FixedBoardHeight add add PanelLines MesgLines add
			MessageFont fontheight mul add /FixedHeight exch def
	/UseFixedSize false def		% if true, don't let user select size

	% UnitScale: void -> void
	% set the transformation matrix so that the current canvas
	% is a unit square with lower-left corner at the origin
	/UnitScale {
	    clippath pathbbox pop pop translate
	    clippath pathbbox exch 4 -1 roll sub 3 1 roll exch sub scale
	} def

	% MakeDamageRepairInterest: array proc canvas -> interest
	% create an interest suitable for forkeventmgr which will
	% call proc with the arguments in array whenever canvas
	% receives a /Damaged event
	/MakeDamageRepairInterest {
	    /Damaged
	    {
		    % stack: event
		dup /Canvas get setcanvas
		damagepath clipcanvas
		/FillInArgs aload pop /FillInProc exec
		newpath clipcanvas
	    }
	    13 array copy
		% stack: array proc canvas /Damaged proc
	    dup 6 6 index put dup 9 5 index put
	    null
		% stack: array proc canvas /Damaged proc null
	    4 -1 roll eventmgrinterest
		% stack: array proc interest
	    3 1 roll pop pop
	} def

	% set up the cursor font
#include "gammonfont/GammonCursors.ps"
	FontDirectory /GFont known
	{/GammonFont /GFont findfont 12 scalefont def}
	{/GammonFont (/usr/NeWS/fonts/GFont.ff) findfilefont 12 scalefont def}
	ifelse

	% codes for communicating with C client program
	/MoveCode (M) def
	/DoubleRequestCode (D) def
	/AcceptDoubleCode (A) def
	/RefuseDoubleCode (R) def
	/ShowLastMoveCode (S) def
	/RedoLastMoveCode (B) def
	/RestartMoveCode (U) def
	/ForfeitCode (F) def
	/QuitCode (Q) def
	/NewGameCode (N) def
	/GotMouseClickCode (C) def

C: #define ps_MoveCode 'M'
C: #define ps_DoubleRequestCode 'D'
C: #define ps_AcceptDoubleCode 'A'
C: #define ps_RefuseDoubleCode 'R'
C: #define ps_ShowLastMoveCode 'S'
C: #define ps_RedoLastMoveCode 'B'
C: #define ps_RestartMoveCode 'U'
C: #define ps_ForfeitCode 'F'
C: #define ps_QuitCode 'Q'
C: #define ps_NewGameCode 'N'
C: #define ps_GotMouseClickCode 'C'

%
% ps_CreateWindow
%
% make the frame and start the server side off
%

cdef ps_CreateWindow()

	% CreateCanvas: name -> canvas
	% create a new canvas and store it under the given name
	/CreateCanvas {
	    FrameCanvas newcanvas dup 3 1 roll store
	    /ptr /ptr_m 2 index setstandardcursor
	    begin
		/Mapped true def
		/EventsConsumed /MatchedEvents def
	    end
	} def

	% ShapeMajorCanvases: void -> void
	% resize the three main canvases
	/ShapeMajorCanvases {
	    % only do this if the canvases have been created
	    BoardCanvas null ne {
		gsave
		FrameCanvas setcanvas

		% put the board canvas at the bottom
		BorderLeft BorderBottom translate
		0 0
		FrameWidth BorderLeft BorderRight add sub
		FrameHeight BorderBottom 2 mul BorderTop add sub
		MessageFont fontheight PanelLines MesgLines add mul sub
		rectpath BoardCanvas reshapecanvas

		% put the message canvas BorderBottom units above
		% the board canvas
		0
		FrameHeight BorderTop BorderBottom MessageFont fontheight
		PanelLines mul add add sub
		translate
		0 0
		FrameWidth BorderLeft BorderRight add sub
		MessageFont fontheight PanelLines mul
		rectpath PanelCanvas reshapecanvas

		% put the panel canvas immediately above the message
		% canvas
		0 MessageFont fontheight MesgLines mul -1 mul translate
		0 0
		FrameWidth BorderLeft BorderRight add sub
		MessageFont fontheight MesgLines mul
		rectpath MesgCanvas reshapecanvas

		grestore
		% Hack! Hack! Hack!
		% this takes care of a strange bug causing the frame
		% not to repaint properly on a resize
		gsave FrameCanvas setcanvas MyPaintFrame grestore
	    } if
	} def

	% MyPaintFrame: void -> void
	% paint the frame as in a normal frame except separate
	% the board and panel/message areas by a border as wide
	% as the bottom border
	/MyPaintFrame {
	    % clear
	    FrameFillColor fillcanvas FrameBorderColor strokecanvas

	    % box the board canvas
	    BorderLeft .5 sub BorderBottom .5 sub
	    FrameWidth BorderLeft BorderRight add sub 1 add
	    FrameHeight BorderBottom 2 mul BorderTop add sub 1 add
	    MessageFont fontheight PanelLines MesgLines add mul sub
	    rectpath stroke

	    % box the message/panel canvases
	    BorderLeft .5 sub
	    FrameHeight BorderTop MessageFont fontheight
	    PanelLines MesgLines add mul add sub 0.5 sub
	    FrameWidth BorderLeft BorderRight add sub 1 add
	    MessageFont fontheight PanelLines MesgLines add mul 1 add
	    rectpath stroke

	    % paint the controls, etc.
	    /paintframelabel self send
	    PaintFrameControls
	    PaintFocus 
	} def

	% create the window object and set up the repaint handlers
	/window framebuffer /new DefaultWindow send def
	{
	    /PaintClient {PaintPanel PaintMessages} def
	    /FrameLabel (Backgammon) def
	    /IconImage /gammon def

	    % the client canvas does not get created or shaped until
	    % the /reshapefromuser message below, so we can change
	    % the create and reshape procedures here
	    /CreateClientCanvas {
		/PanelCanvas CreateCanvas
		/MesgCanvas CreateCanvas
		/BoardCanvas CreateCanvas
		BoardCanvas begin
		    /Transparent false def
		    /Retained true def
		end
		[
		    [] {PaintBoardBackground} BoardCanvas
		    MakeDamageRepairInterest
		] forkeventmgr pop
		CreatePieceCanvases
		CreateDieCanvases
		CreateScoreCanvas
	    } def
	    /ShapeClientCanvas {
		ShapeMajorCanvases
		ShapePieceCanvases
		ShapeDieCanvases
		ShapeScoreCanvas
	    } def
	    /PaintFrame {MyPaintFrame} def
	    /DestroyClient {
		(%\n) [QuitCode] SendToGammonClient
	    } def
	} window send

	UseFixedSize
	{0 0 FixedWidth FixedHeight /reshape window send}
	{/reshapefromuser window send}
	ifelse

	% change the frame menu so the user can't quit illegally
% Use /DestroyClient instead! -deh	   
%	4 (Zap) { (%\n) [QuitCode] SendToGammonClient } /changeitem
%	window /FrameMenu get send

	/GammonMenu [
	    (Double) { (%\n) [DoubleRequestCode] SendToGammonClient }
	    (Accept Double)	{ (%\n) [AcceptDoubleCode] SendToGammonClient }
	    (Refuse Double)	{ (%\n) [RefuseDoubleCode] SendToGammonClient }
	    (Show Last Move) { (%\n) [ShowLastMoveCode] SendToGammonClient }
	    (Redo Move) { (%\n) [RedoLastMoveCode] SendToGammonClient }
	    (Redo Entire Move) { (%\n) [RestartMoveCode] SendToGammonClient }
	    (Forfeit) { (%\n) [ForfeitCode] SendToGammonClient }
	    (Quit) { (%\n) [QuitCode] SendToGammonClient }
	    (New Game) { (%\n) [NewGameCode] SendToGammonClient }
	] /new DefaultMenu send def
	[
	    RightMouseButton
	    {/showat GammonMenu send}
	    DownTransition
	    BoardCanvas
	    eventmgrinterest
	] forkeventmgr
	
	CreatePanel
	CreateBoard

	/map window send
