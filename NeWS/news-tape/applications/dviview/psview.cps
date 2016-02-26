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
 
#define PAGE_TAG 0 
#define EXIT_TAG 1
cdef ps_initialize(w, h, cx, cy)
	/FG 0 0 0 rgbcolor def
	/BG 1 1 1 rgbcolor def
	/errordict 10 dict def
	errordict /rangecheck { stop } put
%	systemdict /settransfer /pop load put
	/settransfer {pop} def 
	/currenttransfer { { } } def
	/currentscreen { 0 0 { } } def
	/setscreen { pop pop pop } def
	/definefont { exch dup type /keywordtype ne { (                                    ) cvs cvn } if
			exch //definefont } def
	/StandardEncoding magic:AdobeSequence def
	magic:fontdict /Encoding StandardEncoding put
    /PGC {
	cx cy moveto
	/Times-Roman findfont 20 scalefont setfont
	(Please Wait) cshow
    } def
    /DoBox true def
    /PrologDone false def
    /PSPageW w def
    /PSPageH h def
    /privudict 200 dict def
    /@Dicts 0 array def
    /ps_scale { % canvas => -  scale and translate canvas
	setcanvas initmatrix clippath pathbbox
	h div exch w div min dup scale pop pop
	pathbbox 2 div exch 2 div exch translate pop pop
	cx neg cy neg translate
	PSScale PSPageX PSPageY translate dup scale
	startmatrix concat
    } def
    /ps_paint {
	PSCanvas ps_scale
	FG setcolor
	PrologDone {
		DoBox { cx w 2 div sub cy h 2 div sub moveto
			w h rect stroke } if
		/paint PSSlider send
		/paint PSHbar send /paint PSVbar send
		/paint PScycle send
        FrameBorderColor strokecanvas 
	} if
%	startmatrix concat
	{PGC} stopped {
		cx cy moveto
		/Times-Roman findfont 20 scalefont setfont
		(Page Display Error) cshow
	} if
    } def
    [1 0 0 1 0 0] setmatrix
cdef ps_ditroff_fix()
    /p { initmatrix xi } def
cdef ps_nobox() /DoBox false def
cdef ps_rgbcolor(r, g, b) r 256 div g 256 div b 256 div rgbcolor
cdef ps_hsbcolor(h, s, b) h 256 div s 256 div b 256 div hsbcolor
cdef ps_defFG() /FG exch def
cdef ps_defBG() /BG exch def
cdef ps_redefine_colors()
	/setgray { 
		dup 0 eq
		 { pop FG setcolor } 
		 { dup 1 eq { pop BG setcolor } { setgray } ifelse } 
		ifelse
	} def
cdef ps_setupwindow(string name)
    /startmatrix 6 array currentmatrix def
% 
	systemdict /Item known not { (NeWS/liteitem.ps) run } if
	/new_page { % new page number => -
		    dup /PageCount exch store
		    PAGE_TAG tagprint typedprint
		    PSSlider /ItemValue PageCount put
		    /paint PSSlider send
		  } def 
	/NEXT	  { PageCount 1 add dup MAX gt {pop}
		    { new_page } ifelse } def
	/PREV	   { PageCount 1 sub dup MIN lt {pop}
		    { new_page } ifelse } def
	/REDIS     { PageCount new_page } def
 	/FIRST     { MIN new_page } def
 	/LAST      { MAX new_page } def
	/new_scale { /PSPageX PSHbar /ItemValue get
		     PSScale 1 sub 0 max PSPageW mul mul neg def
		     /PSPageY PSVbar /ItemValue get
		     PSScale 1 sub 0 max PSPageH mul mul neg def
		   } def
	/ENLARGE   { {PSScale 1.41 mul /PSScale exch def
		      new_scale paint} win send
		   } def
	/REDUCE	   { {PSScale 1.41 div /PSScale exch def
		      new_scale paint} win send
		   } def
	/NORMAL	   { {/PSScale 1 def /PSPageX 0 def /PSPageY 0 def
		      paint} win send
		   } def
	/LEFT	   { {PScycle /ItemValue get 0 ne
		     {gsave
			PSOverlay ps_scale currentcursorlocation 2 copy
			{y0 sub exch x0 sub exch rect
			 x0 y0 x y points2rect /PSdisplay win send} getanimated
			waitprocess aload pop points2rect
			/PSBoxH exch def /PSBoxW exch def
			/PSBoxY exch def /PSBoxX exch def
			/PSBoxP PageCount def paintov
		       grestore} {NEXT} ifelse} win send
		   } def
	/MIDDLE	   { {PScycle /ItemValue get 0 ne PSBoxP PageCount eq and
		     {gsave
			PSOverlay ps_scale currentcursorlocation 2 copy
			{y0 sub PSBoxY add exch x0 sub PSBoxX add exch
			 2 copy moveto PSBoxW PSBoxH rect
			 PSBoxW PSBoxH /PSdisplay win send} getanimated
			waitprocess aload pop
			PSBoxY add exch PSBoxX add
			4 -1 roll sub /PSBoxX exch def
			exch sub /PSBoxY exch def paintov
		      grestore} if} win send
		   } def
	/EXIT      { EXIT_TAG tagprint } def
	/clearov { PSOverlay ps_scale erasepage } def
	/paintov { clearov
		PScycle /ItemValue get 0 ne PSBoxP PageCount eq and {
			0 setlinewidth
			PSBoxX PSBoxY PSBoxW PSBoxH rectpath stroke
			PSBoxX PSBoxY PSBoxW PSBoxH PSdisplay
		} {
			() /printstring PSmessage send
		} ifelse
	} def
	/paintfr {
	  win /FrameLabel fr_label put
	  /paint win send
	} def  
	/fr_label {(%) [fr_data] sprintf } def 
	/fr_data name def 
%
	/PSWindow DefaultWindow      
        /PSCanvas	null def	% the image sub window
	/PSScale	1 def		% image scale
	/PSPageX	0 def		% image translation (in)
	/PSPageY	0 def
        /PSOverlay	null def	% box overlay
	/PSBoxX		0 def
	/PSBoxY		0 def
	/PSBoxW		0 def
	/PSBoxH		0 def
	/PSBoxP		-1 def
        /PSSlider	null def	% page slider
	/PSHbar		null def	% scroll bars
	/PSVbar		null def	% scroll bars
	/PScycle	null def	% box cycle
	/PSmessage	null def	% box message
  	/PageCount	1  def		% current page number, initially = 1 
	/MIN		1  def 	        % MIN always = 1, MAX sent by client
%
   dictbegin 	        
	/FrameLabel fr_label def
	/PaintClient { clearov ps_paint paintov } def
	/FrameFillColor BG def
	/IconHeight 15 def
	/HbarLen 0 def
	/VbarLen 0 def
%
%	gsave
%	    IconFont setfont
%	    FrameLabel stringwidth pop 4 add 64 max
%	    /IconWidth exch def
%	grestore
%
	/PaintIcon { 
	    gsave IconCanvas setcanvas
	    IconFillColor fillcanvas IconBorderColor strokecanvas
	    IconTextColor setcolor IconFont setfont
	    IconWidth 2 div 3 moveto FrameLabel cshow
	    grestore
	} def 
	/ClientMenu [							   
	    (Next Page)    	{ NEXT  } 
            (Previous Page)     { PREV  }
%	    (Redisplay)         { REDIS }
	    (First Page)	{ FIRST }
	    (Last Page)  	{ LAST  }
	    (Enlarge)           { ENLARGE }
            (Reduce)            { REDUCE }
            (Normal Size)       { NORMAL }
	    (Quit)         	{ EXIT  }    	     
	] /new DefaultMenu send def   
   dictend
%  
   classbegin
        % Two class variables for defining the subwindow geometry 
        /PSItemHeight   30 def   % Height of the slider area
        /PSInset        20 def   % Inset of the PSCanvas from ClientCanvas
	% if PageCount != ItemValue, repaint SliderItem
        /PSset { % page number => -   
	    dictbegin
	    /page exch def
	    page MAX gt page MIN lt page PageCount eq or or {pop}
	    { page new_page } ifelse
	    dictend
        } def
        /PSHscroll { % frac => -
	    PSScale 1 sub 0 max PSPageW mul mul neg
	    dup PSPageX eq
	    {pop} {/PSPageX exch def paint} ifelse
        } def
        /PSVscroll { % frac => -
	    PSScale 1 sub 0 max PSPageH mul mul neg
	    dup PSPageY eq
	    {pop} {/PSPageY exch def paint} ifelse
        } def
	/PSdisplay { % x0 y0 x y => -
	    PScycle /ItemValue get { % scale values
	      1 {4 {1.389 mul cvi 100 div 4 1 roll} repeat}	%inches
	      2 {4 {cvi 4 1 roll} repeat} 			%points
	      3 {4 {3.527 mul cvi 100 div 4 1 roll} repeat}	%centimetres
	    } case
            (X=% Y=% W=% H=%) [6 2 roll] /printf PSmessage send
	} def
	% Override two methods: the Client create & shape methods
    	/CreateClientCanvas {
    	    % create the ClientCanvas:
    	    /CreateClientCanvas super send   	    
%	    ClientCanvas /Transparent false put
%	    ClientCanvas /Mapped true put
	    % set up width factors:
		/SliderLen 150 def
		/HbarLen 200 def
		/VbarLen 200 def
		/CycleLen 60 def
		/MessageLen 225 def

    	    % Create the slider item:
	        /PSSlider (Page: ) [ MIN MAX PageCount ]
		/Right {ItemValue /PSset win send}
		ClientCanvas SliderLen 0 /new SliderItem send
		dup /ItemFrame 1 put
%		PSInset 5 /move 3 index send
		store

	    % create scrollbars
		/PSHbar [0 1 .01 .1 null] 0 {ItemValue /PSHscroll win send}
		ClientCanvas 0 HbarLen /new SimpleScrollbar send
		dup /BarVertical? false put
%		PSInset VbarLen PSInset add /move 3 index send
		store

		/PSVbar [0 1 .01 .1 null] 1 {ItemValue /PSVscroll win send}
		ClientCanvas 0 VbarLen /new SimpleScrollbar send
%		HbarLen PSInset add PSItemHeight /move 3 index send
		store

	    % create box cycle
		/PScycle (Box: ) [(off) (in) (pt) (cm)]
		/Right {/paintov win send}
		ClientCanvas CycleLen 0 /new CycleItem send
		dup /ItemFrame 1 put
%		PSInset HbarLen add CycleLen 2 div sub 5 /move 3 index send
		store

	    % create message box
		/PSmessage () () /Right {}
		ClientCanvas MessageLen 0 /new MessageItem send
%		HbarLen 2 div PSInset add MessageLen 2 div sub 5 /move 3 index send
		store

	    % Activate the controls
	    [PSSlider PSHbar PSVbar PScycle PSmessage] forkitems pop
	    % Create the PostScript subwindow:
	    /PSCanvas ClientCanvas newcanvas store
	    PSCanvas /Mapped true put
	    /PSOverlay PSCanvas createoverlay store
    	} def
    	/ShapeClientCanvas {	   
    	    % [Re] Shape the ClientCanvas:
	    /ShapeClientCanvas super send
	    ClientCanvas setcanvas 	    
	    % Move the controls
	    gsave
		ClientCanvas setcanvas clippath pathbbox points2rect
		PSInset sub PSItemHeight sub /VbarLen exch def
		PSInset 2 mul sub /HbarLen exch def
		pop pop
	    grestore
	    PSInset 5 /move PSSlider send
	    PSInset VbarLen PSItemHeight add HbarLen 16 /reshape PSHbar send
	    HbarLen PSInset add PSItemHeight 16 VbarLen /reshape PSVbar send
	    PSInset HbarLen add CycleLen sub 5 /move PScycle send
	    PSInset HbarLen add CycleLen sub MessageLen sub PSInset sub 5
		/move PSmessage send
	    % [Re] Shape the PostScript subwindow:
	    gsave
	       ClientCanvas setcanvas clippath pathbbox
	       PSInset PSItemHeight translate
	       PSItemHeight sub PSInset sub exch PSInset 2 mul sub exch
	       rectpath PSCanvas reshapecanvas  
%              PSCanvas setcanvas clippath PSOverlay reshapecanvas
               /PSOverlay PSCanvas createoverlay store
	    grestore
    	} def
    classend def    
    /win framebuffer /new PSWindow send def		
    /reshapefromuser win send				
    /map win send 
%
% Create handlers for each key
    /handlers 100 dict def
    handlers begin
	32 { NEXT } def	% ' '
	43 { NEXT } def	% '+'
	8  { PREV } def	% '\b'
	45 { PREV } def	% '-'
	48 { FIRST} def	% '0'
	60 { FIRST} def	% '<'
	44 { FIRST} def	% ','
	62 { LAST } def	% '>'
	46 { LAST } def	% ','
	36 { LAST } def	% '$'
	101 { ENLARGE } def % 'e'
	114 { REDUCE } def % 'r'
	110 { NORMAL } def % 'n'
	3  { EXIT } def	% ^C
	113 {EXIT } def	% 'q'
	/LeftMouseButton {LEFT} def
	/MiddleMouseButton {MIDDLE} def
    end

{ 		% done as a separate process so the file reading
		% process can exit
    win /ClientCanvas get setcanvas

% Start the input handler
    systemdict /Selections known {	% The new extended input system
        currentcanvas addkbdinterests pop
    } if
    createevent dup begin
        /Name 200 dict dup begin
	    0 1 127 { dup def } for
	    /LeftMouseButton dup def
	    /MiddleMouseButton dup def
	end def
	/Action /DownTransition def
	/Canvas currentcanvas def
    end expressinterest

% Input handling loop
    {   clear
	/CurrentEventX awaitevent def
	CurrentEventX /Name get dup
	handlers exch known {
	    handlers exch get cvx exec
	} if
    } loop
} fork clear
%
cdef ps_startpage()
    /@DictHeight countdictstack def
    @Dicts length 1 sub -1 0 { @Dicts exch get begin } for
    /PGC {
	/@DictHeight countdictstack def
	@Dicts length 1 sub -1 0 { @Dicts exch get begin } for
cdef ps_endpage()
	{ countdictstack @DictHeight le { exit } if
	  end } loop
    }
    { countdictstack @DictHeight le { exit } if
      end } loop
    def
cdef ps_dvips_fix()
	{PSCanvas ps_scale} win send
cdef ps_prolog_done()
	/PrologDone true def
cdef ps_startprolog()
	/@DictHeight countdictstack def
	privudict begin
cdef ps_endprolog()
    [ { countdictstack @DictHeight le { exit } if
	currentdict end } loop ]
    /@Dicts exch def
cdef ps_damageall()
    createevent begin
        /Name /Damaged def
        /Canvas win /FrameCanvas get def
        currentdict
    end null exch sendevent
cdef get_exit() => EXIT_TAG
cdef get_page_selection(selection) => PAGE_TAG(selection)

