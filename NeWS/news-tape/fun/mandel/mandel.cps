C: #define NEWAREA 1
C: #define QUIT 99

#define NEWAREA 1
#define QUIT 99

cdef pps_init(w,h,nareas, string hostname)
    /hostname hostname def
    /width w def
    /height h def

%
% Convert hue/saturation/brightness to red/green/blue color struct.
%
/hsbtorgb {			% h s b => r g b
    10 dict begin

    /brightness exch def
    /saturation exch def
    /hue exch def

    /i null def
    /f null def

    /p null def
    /q null def
    /t null def

    saturation 0 eq
	{ brightness brightness brightness }
	{
	    /hue hue 6 mul store
	    hue 6 ge { /hue 0 store } if
	    /i hue floor store
	    /f hue i sub store
	    /p brightness 1 saturation sub mul store
	    /q brightness 1 saturation f mul sub mul store
	    /t brightness 1 saturation 1 f sub mul sub mul store
	    i
	    dup 0 eq { brightness t p } if
	    dup 1 eq { q brightness p } if
	    dup 2 eq { p brightness t } if
	    dup 3 eq { p q brightness } if
	    dup 4 eq { t p brightness } if
	    dup 5 eq { brightness p q } if
	    4 -1 roll pop
	}
	ifelse
    end
} def

/mysetcolor {			% h s b => -  (sets current color or gray)
    colorcanvas
	{ sethsbcolor }
	{ hsbtorgb 3 mul exch .6 mul add exch .1 mul add setgray }
	ifelse
}def

    /selectarea {
	4 dict begin
	    ovcan setcanvas
	    currentcursorlocation
	    /ny exch def
	    /nx exch def
	    nx ny { x y0 lineto lineto x0 y lineto closepath } getanimated
	    waitprocess aload pop
	    /ny0 exch def
	    /nx0 exch def
	    NEWAREA typedprint
	    nx ny idtransform exch typedprint typedprint
	    nx0 ny0 idtransform exch typedprint typedprint
	    can setcanvas
	    end
    } def

    /areasdone 0 def
    /totareas 0 def

    /redraw {
	gsave
	clippath pathbbox height div exch width div exch scale pop pop
	0 1 1 mysetcolor
	clippath fill
	0 height translate
	1 -1 scale
	areasdone 0 gt {
	    1 1 areasdone {
		dup pathfuncs exch 1 sub get exec
		totareas 1 sub div dup 1 le
		    { 1 1 mysetcolor } { pop 0 setgray } ifelse
		fill
		pause
	    } for
	} if
	grestore
    } def

    /storefunc {
	gsave
	pathfuncs area 1 sub /func load put
	/areasdone area store
	can setcanvas
	clippath pathbbox height div exch width div exch scale pop pop
	0 height translate
	1 -1 scale
	pathfuncs area 1 sub get exec
	area totareas 1 sub div dup 1 le
	    { 1 1 mysetcolor } { pop 0 setgray } ifelse
	fill
	grestore
	ican setcanvas icondraw
    } def


    /icondraw {
	gsave
	clippath pathbbox height div exch width div exch scale pop pop
	0 1 1 mysetcolor
	clippath fill
	0 height translate
	1 -1 scale
	areasdone 0 gt {
	    pathfuncs areasdone 1 sub get exec
	    0 setgray
	    fill
	} if
	grestore
    } def

    /win framebuffer /new DefaultWindow send def
    {
      /FrameLabel (MANDELBROT  running on host ) hostname append def
      /PaintIcon { IconCanvas setcanvas icondraw } def
      /PaintClient {ClientCanvas setcanvas redraw} def
      /ClientMenu
	[(Quit)] [{QUIT typedprint}]
	/new DefaultMenu send def
    } win send
    /reshapefromuser win send
    /map win send

    /can win /ClientCanvas get def
    /ican win /IconCanvas get def
    ican makecanvasretained


    /eventmgr [
	LeftMouseButton {selectarea}
	    DownTransition can eventmgrinterest
    ] forkeventmgr def
    /ovcan can createoverlay def
    can setcanvas
    pause
    1 typedprint
    /totareas nareas store		% How many areas
    /pathfuncs totareas array def	% Array to store the pathfuncs in

cdef pps_setcolor(i)
    /area i def

cdef pps_start(x,y)
    /func {
	newpath
	x y moveto

cdef pps_end()
	closepath
    } def
    storefunc
