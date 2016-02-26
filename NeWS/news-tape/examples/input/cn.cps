%!
% keyboard example program 2

#define MOUSETAG        100
#define MENUTAG         101
#define	KBTAG		102

cdef ps_mouse_click(int mouse_x, mouse_y) => MOUSETAG (mouse_x, mouse_y)
cdef ps_menu_choice(int menu_index) => MENUTAG (menu_index)
cdef ps_kbinput(string kbstring) => KBTAG (kbstring)

cdef ps_initialize()

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TEXT STORAGE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/inputtext () def
/textx 40 def
/initx 40 def
/texty 15 def
/inity 15 def
/textfont /Times-Roman findfont 24 scalefont def
/cleartext {
	can setcanvas 1 setgray
	initx inity moveto inputtext show
	/inputtext () def
	0 setgray
	/textx initx store
	/texty inity store
} def

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% KEY HANDLING PROCEEDURES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/addchar {
	cvis dup
	inputtext exch append
	/inputtext exch store
	textfont setfont
	can setcanvas
	textx texty moveto show
	currentpoint /texty exch def /textx exch def
	textx texty setcursorlocation
} def
/deletechar {
	inputtext () ne {
	inputtext dup length 1 sub get cvis
	inputtext dup length 1 sub 0 exch getinterval
	can setcanvas textfont setfont
	initx inity moveto dup stringwidth pop 0 rmoveto exch
	currentpoint /texty exch store /textx exch store
	1 setgray show 0 setgray
	/inputtext exch store
	textx texty setcursorlocation
	} if
} def
/deleteline {
	cleartext
	textx texty setcursorlocation
} def
/returnkey {
	KBTAG tagprint inputtext typedprint cleartext
	textx texty setcursorlocation
} def

/handlers 200 dict dup begin
	0 1 127 {
		dup [ exch /addchar cvx ] cvx def
        } for
	13 {returnkey} def
	/EditBackChar {deletechar} def
        /EditBackLine {deleteline} def
end def

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INITIALIZE A WINDOW
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/repair {
    /Symbol findfont 24 scalefont setfont
    initx inity moveto
    (/) dup stringwidth pop neg 0 rmoveto show
    /Times-Roman findfont 24 scalefont setfont
    inputtext show
} def

/win framebuffer /new DefaultWindow send def

/reshapefromuser win send

{
	/PaintClient {repair} def
	/FrameLabel (C example program) def
	/ClientMenu [
		(first choice) {MENUTAG tagprint 0 typedprint}
		(second choice) {MENUTAG tagprint 1 typedprint}
	] /new DefaultMenu send def
} win send

/map win send

/can win /ClientCanvas get def

/MouseClickEventMgr [
	PointButton {
		MOUSETAG tagprint
		begin
		can setcanvas
		XLocation typedprint
		YLocation typedprint
		end
	} /DownTransition can eventmgrinterest
] forkeventmgr def

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% KEYBOARD INPUT LOOP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

{
    can setcanvas
    currentcanvas addkbdinterests pop
    currentcanvas addeditkeysinterest pop
    {
        awaitevent /Name get dup
        handlers exch known {
            handlers exch get exec
        }
        {
            pop
        } ifelse
    } loop
} fork

