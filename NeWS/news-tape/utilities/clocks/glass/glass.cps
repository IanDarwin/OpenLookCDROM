%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Glass Clock by Michael Powers (powers.henr801m@xerox.com)
% Copyright (C) 1988 Xerox Corp.
%
% Back end disclaimer:
%
% This program is provided free for unrestricted use and
% redistribution provided that this header is included.
% No author (that be me), company (that be Xerox), or
% distributor (that be me again) accepts
% liability for any problems, lost revenue, or damages.
%
% Misc disclaimer:
%
% The postscript isn't pretty, the clock face isn't pretty,
% and the author isn't pretty.
%
% glass.cps
%   just run it through cps to generate glass.h
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cdef ps_createclock()
    /Hour 0 def
    /Min 0 def
    /Sec 0 def
    /ShowSeconds? 0 def
    /HourLength 25 def
    /HourWidth  6 def
    /MinLength 38 def
    /MinWidth   4 def
    /SecLength 38 def
    /SecWidth   2 def
    /draw_hand { % rad wid rot =>
      dup rotate 3 1 roll
      dup 2 div neg 3 moveto
      dup 0 rlineto
      exch dup 0 exch rlineto
      exch neg 0 rlineto
      neg 0 exch rlineto
      neg rotate
    } def
    /draw_face {
      50 50 translate
      ShowSeconds? 1 eq {SecLength SecWidth Sec draw_hand} if
      MinLength MinWidth Min draw_hand
      HourLength HourWidth Hour draw_hand
      12 {-2 42 moveto 4 0 rlineto 0 6 rlineto
          -4 0 rlineto 0 -6 rlineto 30 rotate} repeat
    } def
    /window framebuffer /new DefaultWindow send def
    {
	/IconLabel (Clock) def
	/PaintClient {
          gsave
            ClientCanvas setcanvas
            1 fillcanvas
            FrameWidth 100 div FrameHeight 100 div scale
            48 52 translate
            0 setgray 2 setlinewidth
            gsave
            HourLength HourWidth Hour draw_hand
            gsave 1 setgray fill grestore stroke pause
            MinLength MinWidth Min draw_hand
            gsave 1 setgray fill grestore stroke pause
            ShowSeconds? 1 eq {SecLength SecWidth Sec draw_hand
       .                       gsave 1 setgray fill grestore stroke} if
            12 {-2 42 moveto 4 0 rlineto 0 6 rlineto
                -4 0 rlineto 0 -6 rlineto 30 rotate} repeat
            gsave 1 setgray fill grestore stroke
          grestore
        } def
	/ShapeFrameCanvas {
	  gsave
           ParentCanvas setcanvas
           FrameX FrameY translate
           FrameWidth 100 div FrameHeight 100 div scale
           draw_face
           FrameCanvas setcanvasshape
	  grestore
    	} def
	/ShapeClientCanvas {
            gsave
              FrameCanvas setcanvas
              0 0 FrameWidth FrameHeight rectpath
              ClientCanvas setcanvasshape
            grestore
        } def
	/PaintFrame { } def
        /PaintFocus {
          gsave FrameCanvas setcanvas
          FrameWidth 100 div FrameHeight 100 div scale
          KeyFocus? {1} {0} ifelse setgray
          1 setlinewidth 48 48 43 0 360 arc stroke
          grestore
        } def
    } window send
    /reshapefromuser window send				% Shape it.
    /map window send  % Map the window. (Damage causes PaintClient to be called)

    window /ClientCanvas get setcanvas

cdef ps_update(hour,min,sec,show)
    gsave
      /Hour hour 6 neg mul store
      /Min min 6 neg mul store
      /Sec sec 6 neg mul store
      /ShowSeconds? show store
      /ShapeFrameCanvas window send
    grestore
