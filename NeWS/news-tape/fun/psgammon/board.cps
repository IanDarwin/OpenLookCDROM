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
%       @(#) board.cps 9.1 87/11/05
%
% board.cps
%
% contains
%    ps_SetupBoard: define procedures for manipulating the pieces and
%		    redrawing the board
%


%
% implementation notes:
%
% Canvases: The board, message area, button area, score area, each die,
% and each piece all have their own canvas.  This facilitates clipping,
% damage repair, and input distribution.
%
% The default matrix for the board is device coordinates (pixels) with
% the origin at the lower-left corner.  The piece and die canvases
% are all children of this canvas but have a default matrix
% measured in "point coordinates": each horizontal unit is the width
% of one point and each vertical unit is such that a unit box appears
% square on the display when the ratio of the width to the height of the
% board canvas equals IdealBoardSizeRatio (defined in init.cps).  (If
% the window is not scaled to this size, the pieces will not appear
% round).  The origin for point coordinates is also the lower-left
% corner of the board.
%
% The piece canvases themselves are slightly smaller than the point size so
% that they don't overlap with each other.  Thus, to display them you
% must compute the point's coordinates and then add an offset.  See
% FindPieceCoordinates.
%


%
% point numbering:
%
% 13 14 15 16 17 18  0  19 20 21 22 23 24  25
% |                 BAR                   HOME
% |					    |
% |		    BAR			  HOME
% 12 11 10  9  8  7  0   6  5  4  3  2  1  25
%


%
% ps_SetupBoard
%
% define procedures for manipulating the pieces and redrawing the board
%

cdef ps_SetupBoard()

	% ---------------------- definitions ----------------------


	% PieceCanvases associates piece numbers with canvases
	/PieceCanvases 30 array def

	% PieceInterests associates piece numbers with interests;
	% the ClientData dict in the interest contains the following
	% fields:
	%	PieceNumber	the piece's number
	%	Color		the piece's color
	%	BorderColor	the piece's border color
	%	PointNumber	the point the piece is on
	%	PointPosition	the position of the piece on
	%			the point measured as the distance
	%			from the edge of the board; the
	%			first piece is 0, then 1, 2, 3, 4,
	%			(now comes the second layer) 0.3, 1.3,
	%			2.3, 3.3, 4.3, (now the third) 0.6, ...
	/PieceInterests 30 array def

	% Board associates each point number with a small dictionary
	% containing the following fields:
	%	NumberOfPieces	number of pieces on this point
	%	PieceByPosition	an array associating point position
	%			with point number
	/Board 28 array def

	/BoardIsInitialized false def

	/HumanBar 0 def		% point number of human's bar
	/ComputerBar 25 def	% point number of computer's bar
	/HumanHome 26 def	% point number of human's home
	/ComputerHome 27 def	% point number of computer's home
	/BoardFirst 0 def	% lowest point number
	/BoardLast 27 def	% highest point number

	/BoardWidthInPoints 15 def	% width of board measured in
				% points (12 points, bar, home, score)

	/BoardHeightInPoints
	    BoardWidthInPoints IdealBoardSizeRatio div
	def

	/PointWidth 1 BoardWidthInPoints div def	% width of
				% a point as a fraction of the width
				% of the board
	/PointHeight 0.4 def	% height of point as fraction of
				% board height
	/PointHeightInPoints	BoardHeightInPoints PointHeight mul def
			% 6 def ???

	/PieceRadius 0.4 def	% radius of a piece given that a point
				% has width 1
	/PieceSize PieceRadius 2 mul def
	/PieceOffsetX 0.5 PieceRadius sub def	% horizontal distance
				% between edge of point and end of piece canvas
	/PieceOffsetY PieceOffsetX def	% ditto for vertical distance

	/CubeFont /Times-Roman findfont 0.7 scalefont def
				% font for doubling  cube
	/CubeSide 1.5 def	% length of side of cube measured in points
	/CubeCanvas null def	% canvas for doubling cube

	/ScoreCanvas null def	% canvas for painting score

	% ScaleToPointCoordinates: void -> void
	% sets the CTM so that the current canvas is measured in
	% point coordinates (see implementation notes above)
	% this should only be called when BoardCanvas is the current
	% canvas
	/ScaleToPointCoordinates {
	    UnitScale
	    PointWidth dup IdealBoardSizeRatio mul scale
	} def


	% --------------- initialization procedures ---------------


	% CreatePieceCanvases: void -> void
	% create canvases for each piece and initialize PieceCanvases
	% and PieceInterests
	/CreatePieceCanvases {
	    0 1 29 {
		% create canvas and store in PieceCanvases
		    % stack: piece_number
		dup PieceCanvases exch BoardCanvas newcanvas
		dup 4 1 roll put
		    % stack: piece_number piece_canvas
		/ptr /ptr_m 2 index setstandardcursor
		dup begin
		    /EventsConsumed /MatchedEvents def
		    /Transparent false def
		    /Mapped true def
		    /Retained true def
		end
		% create interests
			% stack: piece_number piece_canvas
		[
		    [3 index] {PaintPiece} 3 index
		    MakeDamageRepairInterest
		] forkeventmgr pop

		LeftMouseButton
		    % stack: piece_number piece_canvas LeftMouseButton
		% the proc for the interest must check if it is the
		% human's turn and then call DragPiece with the event;
		% otherwise, the event gets sent to the board canvas
		{
			% stack: event
		    () PrintError
		    HumanCanMove
			{ DragPiece }
			{ dup /Canvas BoardCanvas put redistributeevent }
		    ifelse
		}
		    % stack: piece_number piece_canvas LeftMouseButton proc
		DownTransition 4 -1 roll eventmgrinterest
		    % stack: piece_number piece_interest
		dup PieceInterests 3 index 3 -1 roll put
		% create fields in ClientData (meaningful values will
		% be filled in later for PointNumber and PointPosition)
		dup /ClientData get begin
			% stack: piece_number piece_interest
		    exch dup /PieceNumber exch def
		    dup /Color exch 15 ge
			{ComputerPieceColor} {HumanPieceColor} ifelse def
		    /BorderColor exch 15 ge
			{ComputerBorderColor} {HumanBorderColor} ifelse def
		    /PointNumber 0 def
		    /PointPosition 0 def
			% stack: piece_interest
		    % only fork event managers for the human's pieces
		    PieceNumber 15 lt {[ exch ] forkeventmgr pop} {pop} ifelse
			% stack: empty
		end
	    } for
	} def

	% ShapePieceCanvases: void -> void
	% resize the canvases; their default matrix (described
	% above under implementation notes) is such that the canvas
	% fits inside a unit square but is slightly smaller
	/ShapePieceCanvases {
	    PieceCanvases {
		gsave
		BoardCanvas setcanvas
		ScaleToPointCoordinates
		PieceRadius PieceRadius PieceRadius 0 360 arc reshapecanvas
		grestore
	    } forall
	    % move the pieces back to where they belong if this isn't the
	    % initial sizing
	    BoardIsInitialized
	    {
		0 1 29 {
		    PieceInterests exch get /ClientData get begin
			gsave
			PieceCanvases PieceNumber get setcanvas
			PointNumber PointPosition FindPieceCoordinates
			movecanvas
			grestore
		    end
		} for
	    } if
	} def

	% CreateDieCanvases: void -> void
	% make canvases for the dice
	/CreateDieCanvases {
	    /HumanLeftDie /HumanBorderColor /HumanPieceColor
		/HumanLeftDieCanvas
	    /HumanRightDie /HumanBorderColor /HumanPieceColor
		/HumanRightDieCanvas
	    /ComputerLeftDie /ComputerBorderColor /ComputerPieceColor
		/ComputerLeftDieCanvas
	    /ComputerRightDie /ComputerBorderColor /ComputerPieceColor
		/ComputerRightDieCanvas
	    4 {
		dup GammonDict exch
		    % stack: diename bordername colorname name dict name
		BoardCanvas newcanvas dup 4 1 roll put
		    % stack: diename bordername colorname name canvas
		dup /ptr /ptr_m 3 -1 roll setstandardcursor
		dup begin
		    /Transparent false def
		    /Mapped false def
		    /Retained true def
		end
		[
		    % PaintDie takes the following arguments:
		    %	number border_color color canvas -> void
		    % the array created below has the name objects
		    % corresponding to the arguments, and then at
		    % execution time these are executed to look
		    % up the values (can't look them up now because
		    % they change)

			% stack: diename bordername colorname name canvas [
		    [
			6 index cvx
			6 index cvx
			6 index cvx
			6 index cvx
		    ] {4 {exec 4 1 roll} repeat PaintDie} 3 index
		    MakeDamageRepairInterest
		] forkeventmgr
		    % stack: diename bordername colorname name canvas process
		6 {pop} repeat
	    } repeat
	    GammonDict /CubeCanvas BoardCanvas newcanvas put
	    /ptr /ptr_m CubeCanvas setstandardcursor
	    CubeCanvas begin
		/Transparent false def
		/Mapped false def
		/Retained true def
	    end
	    [
		[] {PaintCube} CubeCanvas
		MakeDamageRepairInterest
	    ] forkeventmgr pop
	} def

	% ShapeDieCanvases: void -> void
	% resize the die canvases; the default matrix is such
	% that the die is a unit square properly positioned
	% on the board
	/ShapeDieCanvases {
	    gsave
	    BoardCanvas setcanvas ScaleToPointCoordinates

	    2.75 BoardHeightInPoints 2 div 0.5 sub translate
	    0 0 1 1 rectpath HumanLeftDieCanvas reshapecanvas

	    1.5 0 translate
	    0 0 1 1 rectpath HumanRightDieCanvas reshapecanvas

	    5.5 0 translate
	    0 0 1 1 rectpath ComputerLeftDieCanvas reshapecanvas

	    1.5 0 translate
	    0 0 1 1 rectpath ComputerRightDieCanvas reshapecanvas
	    grestore

	    gsave
	    BoardCanvas setcanvas
	    ScaleToPointCoordinates
	    BoardWidthInPoints CubeSide sub 2 div
	    BoardHeightInPoints CubeSide sub 2 div
	    translate
	    CubeSide CubeSide scale
	    0 0 1 1 rectpath CubeCanvas reshapecanvas
	    DoublingCubeIsRightSideUp not {TurnDoublingCube} if
	    grestore
	} def

	% CreateScoreCanvas: void -> void
	% create a canvas for the score area so that it is easy to erase
	% the old score without having to repaint the board
	/CreateScoreCanvas {
	    /ScoreCanvas BoardCanvas newcanvas store
	    /ptr /ptr_m ScoreCanvas setstandardcursor
	    ScoreCanvas begin
		/Transparent false def
		/Mapped true def
		/Retained true def
	    end
	    [
		[] {PaintScore} ScoreCanvas
		MakeDamageRepairInterest
	    ] forkeventmgr pop
	} def

	% ShapeScoreCanvas: void -> void
	% shape the score canvas
	/ShapeScoreCanvas {
	    BoardCanvas setcanvas
	    clippath pathbbox exch PointWidth mul exch rectpath
	    ScoreCanvas reshapecanvas
	} def

	% CreateBoard: void -> void
	% do remaining initialization of data structures associated
	% with the board and move the pieces into their initial places;
	% also express interest in mouse clicks over the board
	/CreateBoard {
	    InitBoard
		24	0	0	SetInitialPosition
		24	1	1	SetInitialPosition

		13	0	2	SetInitialPosition
		13	1	3	SetInitialPosition
		13	2	4	SetInitialPosition
		13	3	5	SetInitialPosition
		13	4	6	SetInitialPosition

		8	0	7	SetInitialPosition
		8	1	8	SetInitialPosition
		8	2	9	SetInitialPosition

		6	0	10	SetInitialPosition
		6	1	11	SetInitialPosition
		6	2	12	SetInitialPosition
		6	3	13	SetInitialPosition
		6	4	14	SetInitialPosition

		1	0	15	SetInitialPosition
		1	1	16	SetInitialPosition

		12	0	17	SetInitialPosition
		12	1	18	SetInitialPosition
		12	2	19	SetInitialPosition
		12	3	20	SetInitialPosition
		12	4	21	SetInitialPosition

		17	0	22	SetInitialPosition
		17	1	23	SetInitialPosition
		17	2	24	SetInitialPosition

		19	0	25	SetInitialPosition
		19	1	26	SetInitialPosition
		19	2	27	SetInitialPosition
		19	3	28	SetInitialPosition
		19	4	29	SetInitialPosition
	    PieceInterests {
		/ClientData get begin
		    true StartPoint StartPosition PieceNumber PutPieceAt
		end
	    } forall
	    [
		LeftMouseButton
		{(%\n) [GotMouseClickCode] SendToGammonClient}
		DownTransition
		BoardCanvas
		eventmgrinterest
	    ] forkeventmgr
	} def % CreateBoard

	% InitBoard: void -> void
	% initialize the board data structure
	/InitBoard {
	    /BoardIsInitialized true store
	    0 1 27 {
		Board exch 2 dict dup 4 1 roll put
		begin
		    /NumberOfPieces 0 def
		    /PieceByPosition 15 array def
		end
	    } for
	} def

	% SetInitialPosition: point position piece -> void
	% set the initial position of piece to be the given position
	% on the given point
	/SetInitialPosition {
	    PieceInterests exch get /ClientData get begin
		/StartPosition exch def
		/StartPoint exch def
	    end
	} def


	% ------------- piece manipulation procedures -------------


	% RemovePieceFrom: piece_number -> void
	% remove a piece from its current point:
	%	1. update the Board data structure
	%	2. move the other pieces on the point to fill
	%	   up the hole (if the removed piece was not
	%	   on top)
	/RemovePieceFrom {
	    dup PieceInterests exch get /ClientData get /PointNumber get
	    Board exch get begin
		% search for the piece in the point array
		0 1 NumberOfPieces 1 sub {
			% stack: piece_number index
		    dup PieceByPosition exch get 2 index eq
		    {exit} {pop} ifelse
		} for
		    % stack: piece_number index_of_piece
		exch pop
		% move all pieces above it down one slot
		1 NumberOfPieces 2 sub {
			% stack: index
		    % get the piece number of the next piece on the point
		    dup 1 add PieceByPosition exch get
			% stack: index next_piece
		    dup PieceInterests exch get /ClientData get begin
			    % stack: index next_piece
			% determine where to move the piece
			1 index
			dup 4 gt {5 sub 1 3 div add} if
			dup 4.5 gt {5 sub 1 3 div add} if
			/PointPosition exch def
			% move that piece
			gsave
			PieceCanvases PieceNumber get dup setcanvas
			canvastotop
			PointNumber PointPosition FindPieceCoordinates
			movecanvas
			grestore
			% update the board data structure
			PieceByPosition 3 1 roll put
			    % stack: empty
		    end
		} for
		% decrement number of pieces
		/NumberOfPieces NumberOfPieces 1 sub def
	    end
	} def	% RemovePieceFrom

	% PutPieceAt: fast pointnumber pointposition piecenumber -> void
	% update the data structures with the new location of the
	% given piece and move the piece's canvas; this does NOT
	% remove the piece from the old point (RemovePieceFrom does
	% that); if fast is true, then movecanvas is used to actually
	% move the canvas; otherwise, FloatCanvasTo is used
	/PutPieceAt {
	    gsave
	    PieceInterests exch get /ClientData get begin
		/PointPosition exch store
		/PointNumber exch store
		Board PointNumber get begin
		    PieceByPosition NumberOfPieces PieceNumber put
		    /NumberOfPieces NumberOfPieces 1 add def
		end
		PieceCanvases PieceNumber get dup setcanvas
		canvastotop
		PointNumber PointPosition FindPieceCoordinates
		3 -1 roll {movecanvas} {FloatCanvasTo} ifelse
	    end
	    grestore
	} def

	% ReplacePiece: piecenumber -> void
	% put a piece back after the user has dragged it to an illegal
	% location; RemovePiece has already been executed, but the
	% piece's interest still contains the original point number
	/ReplacePiece {
	    PieceInterests 1 index get /ClientData get /PointNumber get
		% stack: piecenumber pointnumber
	    dup NextSpaceOnPoint
		% stack: piece_number new_point_number point_position
	    3 -1 roll true 4 1 roll PutPieceAt
	} def

	% DragPiece: event -> void
	% drag the piece currently under the mouse
	/DragPiece {
	    dup /Interest get /ClientData get /PieceNumber get
		% stack: event piece_number
	    % don't let the human move any more pieces until we
	    % get an answer back from gammon regarding whether the
	    % move is legal or not; I know this is ugly, but it
	    % is a stopgap to avoid a horrible race condition when
	    % the user rapidly clicks twice on a piece
	    /HumanCanMove false store
	    gsave
	    dup RemovePieceFrom
		% stack: event piece_number
	    dup PieceCanvases exch get dup canvastotop
		% stack: event piece_number piece_canvas
	    dup 4 -1 roll DragCanvas
		% stack: piece_number canvas
	    BoardCanvas setcanvas
	    ScaleToPointCoordinates
	    getcanvaslocation
	    Debugging {
		(Human dropped canvas at %, %\n) [3 index 3 index] errprintf
	    } if

	    % find the point closest to where the user dropped the mouse
	    % (FindNearestPoint returns 0 if there is no point nearby)

		% stack: piece_number new_x new_y
	    FindNearestPoint

	    % if there was no nearby point, replace the piece
	    % otherwise send a message to the C client, which
	    % will either accept the move or refuse it by
	    % sending ps_HumanMove or ps_InvalidMove
	    % (note that ps_HumanMove still has to move the piece
	    % into the right position on the point)
	    dup 0 eq
		    % stack: piece_number 0
		{pop ReplacePiece /HumanCanMove true store}
		    % stack: piece_number new_point_number
		{
		    1 index PieceInterests exch get /ClientData get
		    /PointNumber get
			% stack: piece_number new_point_number old_point
		    Debugging {(Human Requests Move piece % to % from %\n)
			[4 index 4 index 4 index] errprintf} if
		    exch (% % % %\n) [MoveCode 6 -3 roll]
		    SendToGammonClient
		}
	    ifelse
	    grestore
	} def	% DragPiece

	% DragCanvas: canvas event -> void
	% let the user drag the specified canvas with the mouse
	% the event is the button-down event
	/DragCanvas {
	    5 dict begin
		gsave
		    % stack: canvas event
		exch dup /Canvas exch def setcanvas
		    % stack: event
		dup /XLocation get /xo exch neg def
		/YLocation get /yo exch neg def
		    % stack: empty
		BoardCanvas createoverlay setcanvas
		ScaleToPointCoordinates
		clippath pathbbox /height exch def /width exch def pop pop
		0 0
		{
		    % make sure piece doesn't fall off edges
		    yo add dup 0 lt {pop 0} if exch
		    xo add dup width 1 sub gt {pop width 1 sub} if
			dup 1 lt {pop 1} if exch
		    gsave Canvas setcanvas movecanvas grestore
		    newpath
		}
		mygetanimated waitprocess pop
		grestore
	    end
	} def

	% FindNearestPoint: x y -> point_number or 0
	% return the point number nearest (x,y) or 0 if there is no
	% point nearby; "nearby" means the middle of a piece with
	% origin at (x,y) would be located somewhere in the rectangle
	% bounding the triangle on the point; x and y are in point
	% coordinates
	/FindNearestPoint {
	    Debugging {(% % FindNearestPoint ) [3 index 3 index] errprintf} if
	    % change (x,y) from piece origin to center of piece
	    PieceRadius add exch PieceRadius add truncate
		% stack: piece_y piece_x
	    % determine how many actual points the piece is from the
	    % left edge of the board; if x is 7 then the piece would
	    % be on the bar which is illegal so return 0; if x > 7
	    % then subtract one because we don't want to count the bar
	    dup 7 ge {dup 8 ge {1 sub} {pop 0} ifelse} if
		% stack: piece_y piece_x'
	    dup 0 eq
	    {pop pop 0}		% return failure (either the piece was
				% on the bar (above test failed) or in
				% the score area (trunc x == 0))
	    {
		% now look at the y coordinate
		exch dup BoardHeightInPoints exch sub PointHeightInPoints le
		{
		    % piece is in upper half of board so get rid of
		    % the saved y coordinate and add 12 to the modified
		    % x to get the point number; check that it is not in
		    % the home area (human can never move to the upper
		    % home area)
			% stack: piece_x' piece_y
		    pop 12 add
			% stack: point_number
		    dup 24 gt {pop 0} if
		}
		{
		    % piece is not in upper half; check lower half
		    % (piece could also be in the middle of the board)
		    PointHeightInPoints le
			% piece is in lower half; convert to point
			% number and check if it is in home area
		    {13 exch sub dup 0 le {pop HumanHome} if}
			% piece is in middle: fail
		    {pop 0}
		    ifelse
		}
		ifelse
		    % stack: either 0 or point_number
		    % (note that since point 0 is the bar, you can't
		    % move a piece there)
	    } ifelse
	    Debugging {(%\n) [2 index] errprintf} if
	} def

	% FinishMovingPiece: piece_number new_point_number -> void
	% finish human's move; the piece has already been removed
	% from the old point, but must now be positioned properly on
	% the new point; the data structures must be updated as well
	/FinishMovingPiece {
	    Debugging {(% % FinishMovingPiece\n) [3 index 3 index] errprintf} if
	    dup NextSpaceOnPoint
		% stack: piece_number new_point_number point_position
	    3 -1 roll true 4 1 roll PutPieceAt
	} def

	% MovePiece: old_point new_point -> void
	% move one of the computer's pieces from old_point to new_point
	/MovePiece {
	    Debugging {(% % MovePiece\n) [3 index 3 index] errprintf} if
	    exch Board exch get begin
		PieceByPosition NumberOfPieces 1 sub get
	    end
	        % stack: new_point_number piece_number
	    dup RemovePieceFrom
	    exch dup NextSpaceOnPoint
		% stack: piece_number new_point_number point_position
	    3 -1 roll false 4 1 roll PutPieceAt
	} def

	% NextSpaceOnPoint: point_number -> point_position
	% determine the next available position on the given point
	% the sequence is 0, 1, 2, 3, 4, 0.3, 1.3, 2.3, 3.3, 4.3,
	% 0.6, 1.6, 2.6, 3.6, 4.6
	/NextSpaceOnPoint {
	    Board exch get /NumberOfPieces get
	    dup 4 gt {5 sub 1 3 div add} if
	    dup 4.5 gt {5 sub 1 3 div add} if
	} def

	% FindPieceCoordinates: point_number point_position -> x y
	% given a point number and a position on that point, find
	% the coordinates of a piece canvas on that point (point
	% coordinates); the x coordinate is the left coordinate of
	% the point plus an offset to center the piece on the point;
	% the y coordinate is some multiple of a piece's radius from
	% the edge of the board
	/FindPieceCoordinates {
	    1 index [
		HumanBar {BoardHeightInPoints 1 index 1 add PieceSize mul sub  7}
		ComputerBar {dup PieceSize mul    7}
		HumanHome {dup PieceSize mul   14}
		ComputerHome {BoardHeightInPoints 1 index 1 add
						PieceSize mul sub    14}
		/Default {
		    1 index 13 ge
			% stack: point_number point_position
			{BoardHeightInPoints 1 index 1 add PieceSize mul sub
			 2 index 12 sub}
			{dup PieceSize mul 13 3 index sub}
		    ifelse
		    % compensate for the bar
		    dup 6 gt {1 add} if
		}
	    ] case
	    exch
	    4 2 roll pop pop	% get rid of the arguments which
				% are still on stack
	    % add a delta to the x coordinate to center it on the point
	    exch PieceOffsetX add exch
	} def

	% ChangeColor: void -> void
	% swap the color of the human's and computer's pieces
	/ChangeColor {
	    PieceInterests {
		/ClientData get begin
		    /Color Color PieceColorA eq
				{PieceColorB} {PieceColorA} ifelse def
		    /BorderColor BorderColor BorderColorA eq
				{BorderColorB} {BorderColorA} ifelse def
		end
	    } forall
	    PaintAllPieces
	} def

	% FloatCanvasTo: x y -> void
	% move the current canvas from its current position to x y by
	% slowly dragging it there; this way the user can see where the
	% computer moves its pieces
	/FloatCanvasTo {
% Option A: boring movecanvas
%	     movecanvas
% Option B: float piece along straight lines
%	% this code appears to demonstrate a severe memory
%	% leak in NeWS
%	    gsave
%	    10 dict begin
%		/FinalY exch def
%		/FinalX exch def
%		/CanvasToMove currentcanvas def
%		/FloatDelta 1 2 div def
%		/FloatEvent createevent dup /Name /ContinueFloat put def
%		FloatEvent expressinterest
%%		/Count 0 def
%		{
%%		    (At top of loop\n) [] errprintf
%%		    (Number of dicts[1] = %\n) [countdictstack] errprintf
%		    % compute a delta to move the canvas
%
%		    BoardCanvas setcanvas ScaleToPointCoordinates
%		    CanvasToMove getcanvaslocation
%		    /CurrentY exch def
%		    /CurrentX exch def
%%		    Count 5 eq {(Abort!\n) [] errprintf exit} if
%%		    /Count Count 1 add store
%%		    (Now at %, %; Going to % %\n) [CurrentX CurrentY FinalX FinalY] errprintf
%		    FinalX CurrentX sub abs FloatDelta le
%		    {
%			FinalX
%		    }
%		    {
%			FinalX CurrentX sub 0 lt
%			    {CurrentX FloatDelta sub}
%			    {CurrentX FloatDelta add}
%			ifelse
%		    }
%		    ifelse
%		    FinalY CurrentY sub abs FloatDelta le
%		    {
%			FinalY
%		    }
%		    {
%			FinalY CurrentY sub 0 lt
%			    {CurrentY FloatDelta sub}
%			    {CurrentY FloatDelta add}
%			ifelse
%		    }
%		    ifelse
%
%		    % move the canvas and check if we are done
%
%%		    (Moving to %, %\n) [3 index 3 index] errprintf
%		    CanvasToMove setcanvas 2 copy movecanvas
%		    FinalY eq exch FinalX eq and {exit} if
%
%		    % wait until "virtual now," i.e. all events queued
%		    % up to the current time have been processed
%		    % (this prevents the animation from causing the
%		    % input queue to fill up)
%		    FloatEvent createevent copy dup
%		    /TimeStamp currenttime put
%		    sendevent
%%		    (Number of dicts[2] = %\n) [countdictstack] errprintf
%		    {
%			awaitevent begin
%			    Name /ContinueFloat eq {end exit} if
%			end
%		    } loop
%%		    (Number of dicts[3] = %\n) [countdictstack] errprintf
%%		    (At bottom of loop\n) [] errprintf
%		} loop
%%		(Loop exited\n) [] errprintf
%	    FloatEvent revokeinterest
%	    end
%	    grestore
% Option C: float along a curve
	    gsave
	    10 dict begin
		    % stack: final_x final_y
		/FinalY exch def
		/FinalX exch def
		    % stack: empty
		/CanvasToMove currentcanvas def
		/FloatEvent createevent dup /Name /ContinueFloat put def
		FloatEvent expressinterest
		BoardCanvas setcanvas ScaleToPointCoordinates
		CanvasToMove getcanvaslocation
		    % stack: current_x current_y
		2 copy moveto
		pop BoardHeightInPoints 2 div
		    % stack: x1 y1
		FinalX BoardHeightInPoints 2 div
		    % stack: x1 y1 x2 y2
		FinalX FinalY curveto
		    % stack: empty
		flattenpath
		{CanvasToMove MoveOneSegment}
		{CanvasToMove MoveOneSegment}
		{}
		{FinalX FinalY CanvasToMove MoveOneSegment}
		pathforall
		FloatEvent revokeinterest
	    end
	    grestore
	} def

	% MoveOneSegment: x y canvas -> void
	% move canvas to x, y and sleep until "virtual now" (i.e.
	% until all events timestamped up to the current time
	% have been read, so that we don't slow other processes
	% down)
	/MoveOneSegment {
	    gsave setcanvas movecanvas grestore
	    FloatEvent createevent copy dup
	    /TimeStamp currenttime put
	    sendevent
	    {
		awaitevent begin
		    Name /ContinueFloat eq {end exit} if
		end
	    } loop
	} def

	% TurnDoublingCube: void -> void
	% rotate the doubling cube canvas 180 degrees (but don't repaint it)
	/TurnDoublingCube {
	    gsave
	    CubeCanvas setcanvas
	    1 1 translate -1 -1 scale 0 0 1 1 rectpath
	    CubeCanvas reshapecanvas
	    grestore
	} def

	% PutPiecesBack: void -> void
	% put the pieces back into their initial position for a new game
	/PutPiecesBack {
	    % clear board
	    Board {
		/NumberOfPieces 0 put
	    } forall
	    PieceInterests {
		/ClientData get begin
		    false StartPoint StartPosition PieceNumber PutPieceAt
		end
	    } forall
	} def


	% --------------- board painting procedures ---------------


	% PaintBoard: void -> void
	% repaint the entire board, including pieces
	/PaintBoard {
% XXX Hack because of NeWS clip bug
UnMapPieces
	    PaintBoardBackground
	    PaintDice
	    PaintCube
	    PaintScore
	    PaintAllPieces
ReMapPieces
	} def

	% PaintBoardBackground: void -> void
	% clear the board and repaint the points and lines
	/PaintBoardBackground {
	    gsave
	    BoardCanvas setcanvas

	    % clear
	    BoardColor setcolor clippath fill
	    UnitScale

	    % draw the lines for the bar and home area
	    Black setcolor
	    PointWidth 7 mul dup 0 moveto 1 lineto stroke
	    PointWidth 8 mul dup 0 moveto 1 lineto stroke
	    PointWidth 14 mul dup 0 moveto 1 lineto stroke

	    % draw the triangular points
	    % set up the two colors
	    /color1 PointColorA def
	    /color2 PointColorB def
	    % scale so that a unit triangle is stretched appropriately
	    PointWidth PointHeight scale
	    % draw the six lower-left points
	    1 1 6 {
		pop
		1 0 translate
		color1 triangle
		% swap color1 and color2 for the next point
		color1 /color1 color2 store /color2 exch store
	    } for
	    1 0 translate	% skip the bar
	    % draw the lower-right points
	    1 1 6 {
		pop
		1 0 translate
		color1 triangle
		% swap color1 and color2 for the next point
		color1 /color1 color2 store /color2 exch store
	    } for
	    % draw the upper-left points
	    -13 2.5 translate 1 -1 scale
	    color1 /color1 color2 store /color2 exch store
	    1 1 6 {
		pop
		1 0 translate
		color1 triangle
		% swap color1 and color2 for the next point
		color1 /color1 color2 store /color2 exch store
	    } for
	    1 0 translate	% skip the bar
	    % draw the upper-right points
	    1 1 6 {
		pop
		1 0 translate
		color1 triangle
		% swap color1 and color2 for the next point
		color1 /color1 color2 store /color2 exch store
	    } for
	    grestore
	} def

	/triangle {
	    % tri: color -> void
	    % draw a triangle in a unit box in the given color with
	    % a black outline
	    0 0 moveto 0.5 1 lineto 1 0 lineto closepath
	    setcolor gsave fill grestore
	    Black setcolor stroke
	} def

	% PaintAllPieces: void -> void
	% repaint all the piece canvases
	/PaintAllPieces {
	    BoardFirst 1 BoardLast {PaintPiecesOnPoint} for
	} def

	% PaintPiecesOnPoint: point_number -> void
	% paint all the piece canvases on a given point
	/PaintPiecesOnPoint {
	    Board exch get
		% stack: point_dict
	    dup /PieceByPosition get exch /NumberOfPieces get 1 sub
		% stack: PieceByPosition_array NumberOfPieces-1
	    0 1 3 -1 roll {
		    % stack: PieceByPosition_array next_piece_index
		1 index exch get PaintPiece
	    } for
	    pop
	} def

	% PaintPiece: piece_number -> void
	% paint a given piece canvas
	/PaintPiece {
	    PieceInterests exch get
	    /ClientData get begin
		gsave
		PieceCanvases PieceNumber get setcanvas
		Color setcolor clippath fill
		BorderColor setcolor
		0.05 PieceRadius 0 0
		PieceRadius 2 mul dup rrectframe eofill
		grestore
	    end
	} def

	% PaintDice: void -> void
	% paint the die canvases based on the variables HumanLeftDie,
	% etc.
	/PaintDice {
	    gsave
	    HumanLeftDie HumanBorderColor HumanPieceColor
				HumanLeftDieCanvas PaintDie
	    HumanRightDie HumanBorderColor HumanPieceColor
				HumanRightDieCanvas PaintDie
	    ComputerLeftDie ComputerBorderColor ComputerPieceColor
				ComputerLeftDieCanvas PaintDie
	    ComputerRightDie ComputerBorderColor ComputerPieceColor
				ComputerRightDieCanvas PaintDie
	    grestore
	} def

	% PaintDie: number border_color color canvas -> void
	% paint a die canvas; if number is 0, the canvas gets cleared
	/PaintDie {
	    3 index 0 eq
	    {/Mapped false put pop pop pop}
	    {
		    % stack: number border_color color canvas
		dup 5 1 roll setcanvas
		setcolor clippath fill
		setcolor
% XXX Hack because of NeWS clip bug; should use strokecanvas
%		0.1 0 0 1 1 rectframe eofill
0.05 clippath pathbbox rectframe eofill
		    % stack: number
		{
		    1 {0.5 0.5 Pip}
		    2 {0.2 0.2 Pip 0.8 0.8 Pip}
		    3 {0.2 0.2 Pip 0.5 0.5 Pip 0.8 0.8 Pip}
		    4 {0.2 0.2 Pip 0.2 0.8 Pip 0.8 0.2 Pip 0.8 0.8 Pip}
		    5 {0.2 0.2 Pip 0.5 0.5 Pip 0.2 0.8 Pip
		       0.8 0.2 Pip 0.8 0.8 Pip}
		    6 {0.2 0.2 Pip 0.2 0.5 Pip 0.2 0.8 Pip
		       0.8 0.2 Pip 0.8 0.5 Pip 0.8 0.8 Pip}
		} case
		/Mapped true put
	    }
	    ifelse
	} def

	% Pip: void -> void
	% draw a pip of appropriate size for a die sized to a unit box
	/Pip {
	    0.1 0 360 arc fill
	} def

	% PaintCube: void -> void
	% paint the doubling cube
	/PaintCube {
	    CubeValue 1 le
	    {
		% no cube if game is worth 1 point
		CubeCanvas /Mapped false put
	    } 
	    {
		CubeCanvas /Mapped false put
		gsave
		CubeCanvas setcanvas
		White setcolor clippath fill
		Black setcolor
% XXX Hack because of NeWS clip bug; should use strokecanvas
		0.05 clippath pathbbox rectframe eofill
		CubeFont setfont
		0.5 0.25 moveto CubeValue (      ) cvs cshow
		grestore
		CubeCanvas /Mapped true put
	    } ifelse
	} def

	% PaintScore: void -> void
	% paint the score
	/PaintScore {
	    gsave
	    ScoreCanvas setcanvas
	    BoardColor setcolor clippath fill
	    Black setcolor

	    % draw lines in score area
	    clippath pathbbox
		% stack: 0 0 width height
	    1 index 0.5 sub dup 0 moveto
		% stack: 0 0 width height width-0.5
	    1 index lineto stroke
		% stack: 0 0 width height
	    2 div dup 0 exch moveto
		% stack: 0 0 width height/2
	    lineto stroke pop pop
		% stack: empty

	    % paint the text
	    clippath pathbbox exch 2 div
	    exch 2 copy
		% stack: 0 0 x y x y
	    (% %) [HumanName HumanScore] sprintf PaintScoreString
		% stack: 0 0 x y
	    2 div
	    (computer %) [ComputerScore] sprintf PaintScoreString
		% stack: 0 0
	    pop pop
	    grestore
	} def

	% PaintScoreString: x y string -> void
	% paint a string in the score area; assumes BoardCanvas is the
	% current canvas
	/PaintScoreString {
	    MessageFont setfont
	    dup length MessageFont fontheight mul
		% stack: x y string mesg_height
	    clippath pathbbox 4 1 roll pop pop pop 2 div exch sub 2 div
		% stack: x y string top
	    exch 4 1 roll sub 2 copy moveto 3 2 roll
		% stack: x y string
	    MessageFont fontheight
		% stack: x y string fontheight
	    exch dup length 1 sub 0 1 3 -1 roll {
		    % stack: x y fontheight string index
		1 index exch get ( ) dup 3 1 roll 0 3 -1 roll put cshow
		    % stack: x y fontheight string
		4 2 roll 3 index sub 2 copy moveto 4 2 roll
	    } for
		% stack: x y fontheight string
	    pop pop pop pop
	} def
/UnMapPieces {
    PieceCanvases {/Mapped false put} forall
} def

/ReMapPieces {
    PieceCanvases {/Mapped true put} forall
} def

/PrintStats {
    BoardFirst 1 BoardLast {
	(Point %:\n) [2 index] errprintf
	Board exch get begin
	    (\tNumber of pieces: %\n) [NumberOfPieces] errprintf
	    0 1 NumberOfPieces 1 sub {
		PieceByPosition exch get
		(\tPiece number: %\n) [2 index] errprintf
		PieceInterests exch get /ClientData get begin
		    (\tPiece data:\n) [] errprintf
		    (\t\tPieceNumber: %\n) [PieceNumber] errprintf
		    (\t\tPointNumber: %\n) [PointNumber] errprintf
		    (\t\tPointPosition: %\n) [PointPosition] errprintf
		end
	    } for
	end
	pause
    } for
} def

/mygetanimated {
    10 dict begin
	/proc exch def
	/y0 exch def
	/x0 exch def
	currentcursorlocation /y exch def /x exch def
	GA_constraint null ne GA_value null eq and {
	    /GA_value currentcursorlocation GA_constraint 1 eq {exch} if
	    pop store
	} if
	null blockinputqueue
	{
	    createevent dup begin
		/Action [UpTransition DownTransition] def
	    end expressinterest
	    createevent dup /Name /MouseDragged put expressinterest
	    unblockinputqueue
	    {
		GA_constraint 0 eq {/x GA_value def} if
		GA_constraint 1 eq {/y GA_value def} if
		erasepage x0 y0 moveto x y /proc load exec stroke
		awaitevent begin
		    Action UpTransition eq { end exit } if
		    /x XLocation store /y YLocation store
		end
	    } loop
	    erasepage
	    /GA_constraint null store
	    /GA_value null store
	    [x y] 
	} fork
    end
} def


/errstack {
    console (Stack:\n) writestring
    count copy count 2 div {
	    50 string cvs console exch writestring
	    console (\n) writestring
    } repeat
    console (\n) writestring
    console flushfile
} def
