%
% This file is a product of Sun Microsystems, Inc. and is provided for
% unrestricted use provided that this legend is included on all tape
% media and as a part of the software program in whole or part.
% Users may copy, modify or distribute this file at will.
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

cdef ps_createclock()
	/ULength .25 def
	/QWidth ULength 3 div def
	/ULWidth ULength 3.35 div def
	/UWidth ULength 6 div def
	/UHwidth ULWidth 2 div def
	/UDraw {
	   /Uy exch def
	   /Ux exch def
	    Ux Uy UWidth add UHwidth add moveto
	    Ux ULength add Uy UWidth UHwidth add 90 -90 arcn
	    ULength neg 0 rlineto
	    0 ULWidth rlineto
	    Ux ULength add Uy UWidth UHwidth sub -90 90 arc
	    ULength neg 0 rlineto
	    closepath
	 }def
	/TwoU
	 { /Umat 6 array currentmatrix def
	   translate
	   0 0 QWidth sub UDraw
	   ULWidth 2 idiv
	   ULength UWidth .03 add add add
	   0 translate
	   180 rotate
	   0 0 QWidth sub UDraw
	   Umat setmatrix } def
	/sunlogo {
	    6 array currentmatrix
	   .5 .5 translate
	   45 rotate
	   4
	    {
	      QWidth ULWidth sub
	      0 QWidth dup add sub
	      TwoU
	      90 rotate
	    } repeat
	    setmatrix
	} def
	
	/outer { % - => -
		0.5 0 moveto
		1 0.5 lineto
		0.5 1 lineto
		0 0.5 lineto
		closepath
	
		sunlogo
	} def
	
	    /YesKeyFocusColor {
	         monochromecanvas {.55 setgray} { 0.7 0.7 0.7 setrgbcolor} ifelse              
	    }  def
	    /NoKeyFocusColor {
	         monochromecanvas {.2 setgray} { 1 0.5 1 setrgbcolor} ifelse
	    }  def
	    /window framebuffer /new DefaultWindow send def
	    {
	        /IconLabel (Clock) def
	        /FixFrame { gsave
			FrameCanvas setcanvas clippath NoKeyFocusColor fill 
			grestore (F) print } def
	        /PaintClient { (P) print } def
	        /ShapeFrameCanvas {
	            gsave ParentCanvas setcanvas
	            FrameX FrameY translate
	            FrameWidth FrameHeight scale
		    outer FrameCanvas setcanvasshape
		    grestore
		} def
	        /ShapeClientCanvas { } def
	        /CreateClientCanvas { /ClientCanvas FrameCanvas newcanvas def } def
	        /PaintFrame { } def
	        /PaintFocus {
	            gsave FrameCanvas setcanvas
	            KeyFocus? {YesKeyFocusColor} {NoKeyFocusColor} ifelse
		    clippath fill
	            grestore
	        } def
		/totop {
		    GetCanvas canvastotop
		    HourCanvas canvastotop
		    MinuteCanvas canvastotop
		} def
		/tobottom {
		    MinuteCanvas canvastobottom
		    HourCanvas canvastobottom
		    GetCanvas canvastobottom
		} def
		/move { % x y => -
        		CheckCanvases
        		gsave
        		Iconic?
            		{IconCanvas /IconX /IconY}
            		{FrameCanvas /FrameX /FrameY} ifelse
        		3 index def 3 index def
        		setcanvas movecanvas
        		grestore
			(R) print
		} def
	    } window send
	    /reshapefromuser window send                                % Shape it.
	    /map window send  % Map the window. (Damage causes PaintClient to be called)
	
	    window /FrameCanvas get setcanvas
	    /calctransform {
	        initmatrix initclip
	        clippath pathbbox scale pop pop
	        0.50 0.50 translate                                
	    } def
	
	    /RDC {
	        window /FrameCanvas get setcanvas
	        damagepath clipcanvas
	        calctransform drawclockframe clipcanvas
	    } def

	    /HourCanvas framebuffer newcanvas def
	    /MinuteCanvas framebuffer newcanvas def
	    /SecondCanvas framebuffer newcanvas def
	    [ HourCanvas MinuteCanvas SecondCanvas ]
	    { /SaveBehind true put } forall

	/Sink 0.25 def
	/Isosceles { % base height => -
	    5 dict begin
	    /height exch def
	    /BaseLength exch def
	    newpath BaseLength -2 div height Sink mul neg moveto
	    BaseLength 0 rlineto
	    0 height 1 Sink sub mul lineto
	    closepath
	    end
	} def
	    
	/DoHand { % rot base rad color canvas => -
	    10 dict begin
	    gsave
		/can exch def
		monochromecanvas {setgray} {setcolor} ifelse
		/rad exch def
		/base exch def
		/rot exch def
		% can /Mapped false put
		rot rotate
		base rad 100 div Isosceles
		can reshapecanvas
		can /Mapped true put
		can setcanvas
		clippath fill
	    grestore
	    end
	} def

      /minute_hand_color { % - => -
	   monochromecanvas {0} {1 1 0 rgbcolor} ifelse
	   } def

     /hour_hand_color { % - => -
	   monochromecanvas {.5} {0 1 1 rgbcolor} ifelse
           } def

cdef ps_white() W
cdef ps_black() B
cdef ps_redrawclock() RDC

%
% ps_hand draws a plain clock hand.
%
cdef ps_second_hand(rot,rad)
    % rot 0.02 rad minute_hand_color SecondCanvas DoHand

cdef ps_minute_hand(rot,rad)
    rot 0.06 rad minute_hand_color MinuteCanvas DoHand

cdef ps_hour_hand(rot,rad)
    rot 0.09 rad hour_hand_color HourCanvas DoHand

cdef ps_initializeclock()
    /drawclockframe { } def
    /B {
        textcolor setcolor
    } def
    /W {
        backgroundcolor setcolor
    } def

cdef ps_fancy_hand(rot,rad)
    gsave
    rot rotate newpath -5 0 moveto 0 0 5 180 360 arc
    0 rad 100 div rlineto -5 5 rlineto -5 -5 rlineto closepath
    fill
    grestore

cdef ps_fancy_initializeclock()
    /drawclockframe {
    } def
    /B {
        .5 monochromecanvas {setgray} {.6 1 setrgbcolor} ifelse fill
    } def
    /W {
        1 monochromecanvas {setgray} {1 1 setrgbcolor} ifelse fill
    } def
