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
%       @(#) interface.cps 9.1 87/11/05
%
% interface.cps
%
% contains
%    ps_SetupInterface: define procedures by which server communicates
%			with C client
%
%    ps_HumanMove: finish human's move
%
%    ps_MovePiece: move a piece from a given point to another given point
%
%    ps_InvalidMove: cancel a move which was invalid
%
%    ps_Message: display a message
%
%    ps_ErrorMsg: display an error message
%
%    ps_DrawDice: draw the dice with new values
%
%    ps_InitGame: initialize for a new game
%
%    ps_SetCursor: set the board canvas cursor to a particular cursor char
%
%    ps_HumanCanMove: allow or disallow player to move pieces
%
%    ps_FixedSize: used a default-size board instead of letting user
%                  select size
%
%    ps_DrawCube: draw the doubling cube
%
%    ps_DrawScore: draw the score
%
%    ps_SetHumanName: record the login name of the person playing
%
%    ps_DebuggingOn: turn on some debugging machinery
%

%
% ps_SetupInterface
%
% define procedures by which server communicates with C client
%

cdef ps_SetupInterface()

	% SendToGammonClient: string array -> void
	% send a message to the client by creating an ASCII
	% string (sprintf style) and printing it;
	% choke if you want, but since the number and
	% size of the messages is so small the performance
	% improvement achieved by using encoded data is minimal,
	% and encoded data is much harder to look at for
	% debugging purposes
	/SendToGammonClient {
	    printf
	} def

	/AllBoardCanvases [
	    BoardCanvas
	    HumanLeftDieCanvas
	    HumanRightDieCanvas
	    ComputerLeftDieCanvas
	    ComputerRightDieCanvas
	    CubeCanvas
	] def

	% SetBoardCursor: char char -> void
	% first arg is the primary image char number from GammonFont
	% second is the mask char number
	/SetBoardCursor {
	    gsave
	    GammonFont 3 1 roll
	    AllBoardCanvases {
		setcanvas 3 copy setcanvascursor
	    } forall
	    0 1 29 {
		PieceCanvases exch get setcanvas 3 copy setcanvascursor
	    } for
	    grestore
	    pop pop pop
	} def

%
% ps_HumanMove
%
% finish human's move
%

cdef ps_HumanMove(piece, newpoint)

	piece newpoint FinishMovingPiece
	% see DragPiece in board.cps
	/HumanCanMove true store

%
% ps_MovePiece
%
% move a piece from a given point to another given point
%

cdef ps_MovePiece(from, to)

	from to MovePiece

%
% ps_InvalidMove
%
% cancel a move which was invalid
%

cdef ps_InvalidMove(piece)

	piece ReplacePiece
	% see DragPiece in board.cps
	/HumanCanMove true store

%
% ps_Message
%
% display a message
%

cdef ps_Message(string message)

	message PrintMessage

%
% ps_ErrorMsg
%
% display an error message
%

cdef ps_ErrorMsg(string error)

	error PrintError

%
% ps_DrawDice
%
% draw the dice with new values
%

cdef ps_DrawDice(human_left, human_right, computer_left, computer_right)

	/HumanLeftDie human_left def
	/HumanRightDie human_right def
	/ComputerLeftDie computer_left def
	/ComputerRightDie computer_right def
	PaintDice

%
% ps_InitGame
%
% initialize for a new game
%

cdef ps_InitGame()

	PutPiecesBack

%
% ps_SetCursor
%
% set the board canvas cursor to a particular cursor char
%

cdef ps_SetCursor(cursor_id)

	cursor_id [
	    0 {
		/ptr /ptr_m AllBoardCanvases {
		    3 copy setstandardcursor pop
		} forall
		0 1 29 {
		    PieceCanvases exch get 3 copy setstandardcursor pop
		} for
	      }
	    1 {fist fist_m SetBoardCursor}
	    2 {hand hand_m SetBoardCursor}
	    3 {hourglass hourglass_m SetBoardCursor}
	    4 {cash cash_m SetBoardCursor}
	] case

C: #define ORIGINAL_CUR	0
C: #define ROLL_CUR	1
C: #define MOVE_CUR	2
C: #define THINKING_CUR	3
C: #define DOUBLING_CUR	4

%
% ps_HumanCanMove
%
% allow or disallow player to move pieces
%

cdef ps_HumanCanMove(yes)

	/HumanCanMove yes 1 eq store

%
% ps_FixedSize
%
% used a default-size board instead of letting user select size
%

cdef ps_FixedSize()

	/UseFixedSize true store

%
% ps_DrawCube
%
% draw the doubling cube
%

cdef ps_DrawCube(value, facingup)

	/CubeValue value store
	facingup 0 ne DoublingCubeIsRightSideUp eq
	{
	    TurnDoublingCube
	    /DoublingCubeIsRightSideUp DoublingCubeIsRightSideUp not store
	} if
	PaintCube

%
% ps_DrawScore
%
% draw the score
%

cdef ps_DrawScore(human, computer)

	/HumanScore human store
	/ComputerScore computer store
	ScoreCanvas null ne {PaintScore} if

%
% ps_SetHumanName
%
% record the login name of the person playing
%

cdef ps_SetHumanName(string name)

	/HumanName name store

%
% ps_DebuggingOn
%
% turn on some debugging machinery
%

cdef ps_DebuggingOn()

	[
	    MiddleMouseButton {PrintStats} DownTransition BoardCanvas
	    eventmgrinterest
	] forkeventmgr
	/Debugging true store
