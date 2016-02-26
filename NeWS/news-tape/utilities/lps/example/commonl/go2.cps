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

%
% "@(#)go.cps 9.2 88/01/18
%
% Copyright (c) 1987 by Sun Microsystems, Inc.
%

#define DONE_TAG	1
#define BLACK_TAG	2
#define WHITE_TAG	3
#define DAMAGE_TAG	4
#define MENU_TAG	5

% Constants needed in both C & PostScript:
C: #define ERASE_CMD	0
C: #define FILL_CMD	1
C: #define BOARD_SIZE	19
#define ERASE_CMD	0
#define FILL_CMD	1
#define BOARD_SIZE	19

#define BOARD_MAX	18
#define LINE_WIDTH	.02
#define STONE_SIZE	.80

#define BLACK_EVENT	/LeftMouseButton
#define WHITE_EVENT	/MiddleMouseButton

cdef initialize()

/black_color	0 0 0 rgbcolor def		% Black
/white_color	1 1 1 rgbcolor def		% White
/board_color    .9 .69 .28 rgbcolor def         % Wood color
/line_color	black_color def
/outline_color	black_color def   

/draw_board { % - => - (draw the playing surface)
    board_color setcolor clippath fill 
    line_color setcolor
    0 1 BOARD_MAX {
	dup 0 moveto 0 BOARD_MAX rlineto
	0 exch moveto BOARD_MAX 0 rlineto
    } for
    stroke
    pause
} def

/stone { % outline_color stone_color x y => - (draw stone)
    STONE_SIZE 2 div 0 360 arc            % set stones path
    gsave setcolor fill grestore          % fill with stone_color
    setcolor stroke                       % stroke with outline_color
    pause
} def

/cross { % x y => - (draw cross)
10 dict begin
    /y exch def
    /x exch def
    
    % clear the stone:
    x .5 sub y .5 sub 1 1 rectpath
    board_color setcolor fill
    
    % draw the two cross strokes, carfully adjusting for edge locations:
    x .5 sub 0 max y moveto x .5 add BOARD_MAX min y lineto	% horiz stroke
    x y .5 sub 0 max moveto x y .5 add BOARD_MAX min lineto	% vert stroke
    line_color setcolor stroke

    pause
end
} def

/repair { % - => - (repair the board)
    DAMAGE_TAG tagprint uniquecid typedprint
    [currentcid cidinterest] forkeventmgr
    waitprocess pop
} def

/checkloc { % float => int (convert location to legal board location)
    0 max BOARD_MAX min round
} def
/placestone { % event tag => - (place stone at event's x,y)
    ClientCanvas setcanvas
    tagprint uniquecid typedprint
    begin XLocation checkloc YLocation checkloc end typedprint typedprint
    [currentcid cidinterest1only] forkeventmgr
    waitprocess pop
} def

/downeventinterest {/DownTransition ClientCanvas eventmgrinterest} def
/startinput { % - => - (Wait for input)
    /ButtonMgr [
        BLACK_EVENT {BLACK_TAG placestone} downeventinterest
        WHITE_EVENT {WHITE_TAG placestone} downeventinterest
    ] forkeventmgr store
} def

/makewin { % - => - (builds a go window)
    /GoWindow DefaultWindow             % create subclass
    dictbegin
	/ButtonMgr	null def 
	/FrameLabel	( Go ) def
    dictend 
    classbegin
	/PaintClient {repair} def
	/PaintIcon {repair} def
	/DestroyClient {
	    ButtonMgr killprocess
	    DONE_TAG tagprint
	} def
        /flipiconic { % - => -
        % Redraw current state for icon.
 	    /flipiconic super send
	    Iconic? IconCanvas /Retained get and {/paint self send} if
        } def
	/ForkFrameEventMgr {
	    /ForkFrameEventMgr super send
	    startinput
	} def
	/ClientPath { % x y w h => -
	    4 2 roll translate
	    BOARD_SIZE div exch BOARD_SIZE div exch scale
	    .5 .5 translate
	    -.5 -.5 BOARD_SIZE BOARD_SIZE rectpath
	} def
	/IconPath {ClientPath} def
	/ClientMenu  [
	    (Erase Board)	{MENU_TAG tagprint ERASE_CMD typedprint}
	    (Fill Board)	{MENU_TAG tagprint FILL_CMD typedprint}
	] /new DefaultMenu send def
    classend def

    /win framebuffer /new GoWindow send def 
    /reshapefromuser win send               % shape the window
    /map win send                           % map the window
} def
% end cdef initialize ()

cdef done() => DONE_TAG()
cdef get_damage(int id) => DAMAGE_TAG(id)
cdef get_black(int id, int x, int y) => BLACK_TAG(id, y, x)
cdef get_white(int id, int x, int y) => WHITE_TAG(id, y, x)
cdef get_menu(int cmd) => MENU_TAG(cmd)
cdef draw_board(int id)
     id {draw_board} sendcidevent
cdef black_stone(int id, int x, int y)
     id {outline_color black_color x y stone} sendcidevent
cdef white_stone(int id, int x, int y)
     id {outline_color white_color x y stone} sendcidevent
cdef cross(int id, int x, int y)
     id {x y cross} sendcidevent
cdef repaired(int id)
     id {exit} sendcidevent
cdef repaint()
     /paintclient win send
cdef execute()
     makewin

