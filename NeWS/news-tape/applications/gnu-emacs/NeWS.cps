%
% NeWS interface for GNU Emacs.
%
% Author:	Chris Maio
% Last edit:	4 Sep 1988

% PS -> C message tags
#define tRepairWindow	1
#define tGetWinInfo	2
#define tGetFontInfo	3
#define tKeyboardInput	4
#define tMetaKeyDown	5
#define tEmacsInput	6
#define tEmacsInsert	7
#define tEmacsCommand	8
#define tSetSelectionAt	9
#define tExtSelectionTo	10
#define tDebugMessage	11

cdef InitializePS()
    false setprintermatch

    /textfont /Courier findfont 12 scalefont def
    /inputprocess null def
    /Stuff? true def			% true => simulate typein

    /backfill { backgroundcolor setcolor fill textcolor setcolor } def
    % begin repair - currently unused
    /BR {
	damagepath clipcanvas
    } def
    % end repair
    /ER {
	currentcanvas setcanvas clipcanvas
    } def
    % clear screen
    /CL {
	clippath backfill
    } def
    % clear to end of line
    /CE {						    % x y
	moveto linewidth lineheight rect backfill
    } def
    % clear and show
    /CS {				% s x y n x y
	moveto lineheight rect backfill moveto show
    } def
    % clear and show inverted
    /CSI {				% s x y n x y
	moveto lineheight rect textcolor setcolor fill
	moveto backgroundcolor setcolor show textcolor setcolor
    } def
    % insert lines
    /IL {				    	    	    % y n
	/dy exch def /top exch lineheight add def
	0 dy moveto linewidth top dy sub rect 0 dy neg copyarea
	0 top moveto linewidth dy neg rect backfill
    } def
    % insert lines within scroll region
    /IL2 {				% top bottom dy
	neg 3 1 roll			% -dy top bottom
	2 index sub dup			% -dy top (bottom+dy) (bottom+dy)
	0 exch moveto			% -dy top (bottom+dy)
	1 index exch sub		% -dy top (top-(bottom+dy))
	linewidth exch rect		% -dy top
	1 index 0 exch copyarea		% -dy top
	0 exch moveto linewidth exch rect backfill
    } def
    % delete lines
    /DL {						    % y n
	/dy exch def /top exch lineheight add def
	0 0 moveto linewidth top dy sub rect 0 dy copyarea
	0 0 moveto linewidth dy rect backfill
    } def
    % delete lines within scroll region
    /DL2 {				% top bottom dy
	1 index 0 exch moveto		% top bottom dy
	3 -1 roll			% bottom dy top
	2 index sub			% bottom dy top-bottom
	1 index sub			% bottom dy top-bottom-dy
	linewidth exch rect		% bottom dy
	dup 0 exch copyarea		% bottom dy
	exch 0 exch moveto		% dy
	linewidth exch rect backfill
    } def
    % delete characters 
    /DC {    	    	    	    	    	    	    % x y dx
	dup dup dup					    % x y dx dx dx dx
	6 -1 roll add					    % y dx dx dx x2
	4 index						    % y dx dx dx x2 y
	moveto linewidth lineheight rect neg 0 copyarea	    % y dx dx
	3 1 roll					    % dx y dx
	linewidth exch sub exch moveto lineheight rect backfill
    } def
    % insert characters
    /IC {						    % x y dx
	3 1 roll 3 copy					    % dx x y dx x y
	moveto linewidth lineheight rect 0 copyarea
	moveto lineheight rect backfill
    } def
    % show text
    /S { moveto show } def				    % s x y
    % show text in the background color
    /SI {						    % s x y
	moveto backgroundcolor setcolor show textcolor setcolor
    } def
    % fill background with the foreground color
    /FI {						    % n x y
	moveto linewidth lineheight rect textcolor setcolor fill
    } def
    % toggle the cursor
    /CT {						    % x y
	moveto charwidth lineheight rect
	currentrasteropcode 5 setrasteropcode fill setrasteropcode
	textcolor setcolor
    } def
    % flash the screen
    /^G { gsave 5 setrasteropcode clippath fill clippath fill grestore } def
    % create a window
    /emacswindow {
	textfont setfont
	(m) stringwidth pop /charwidth exch round cvi def
	/baseline textfont fontdescent round cvi def
	/lineheight textfont fontheight baseline add round cvi def
	/window framebuffer /new DefaultWindow send def
	{
	    % set frame label to `emacs on hostname' if possible.
	    { currentfile getsocketpeername } stopped {
		pop (emacs)
	    } {
		(.) search { 3 1 roll pop pop } if (emacs on ) exch append
	    } ifelse
	    /FrameLabel exch def
	    /PaintClient /repairwindow load def
	    /IconImage /emacs def
	    80 24 SD
	} window send
	window /ClientCanvas get setcanvas
	clippath pathbbox 4 2 roll pop pop
	/winheight exch round cvi def /linewidth exch round cvi def
	textfont setfont
	textcolor setcolor
    } def
    % repaint a damaged canvas -- passes damaged region back to client
    /repairwindow {
	{
	    currentcanvas
	    FrameCanvas setcanvas
	    BorderLeft BorderBottom translate clipcanvaspath pathbbox
	    tRepairWindow tagprint typedprint typedprint typedprint typedprint
	    setcanvas
	} window send
    } def
    % reshape the window
    /SD {				% cols rows => -
	{
	    lineheight mul 2 add BorderTop add BorderBottom add exch
	    charwidth mul 4 add BorderLeft add BorderRight add exch
	    FrameX FrameY 4 2 roll /reshape self send
	} window send
    } def
    /FL {				% framelabel => -
	{
	    /FrameLabel exch def
	    FrameCanvas /Mapped get FrameCanvas /Retained get or {
		/paint self send
	    } if
	} window send
    } def
    % Set the font -- should be followed by a SD to reshape the window
    /SF {				% /Courier 10 => -
	mark 3 1 roll
	{ exch findfont exch scalefont /textfont exch def } stopped cleartomark
	textfont setfont
	(m) stringwidth pop /charwidth exch round cvi def
	/baseline textfont fontdescent round cvi def
	/lineheight textfont fontheight baseline add round cvi def
    } def
    % interactively move the window
    /IM {
	{
	    framebuffer setcanvas currentcursorlocation
	    exch FrameWidth sub exch /move self send
	    FrameCanvas setcanvas
	    % XXX kludge for non-yet-mapped b&w canvases
	    5 dict begin
		/dragframe? true def
	    	InteractionLock { interactivemove } monitor
	    end
	    ParentCanvas getcanvaslocation
	    neg exch neg exch /move self send
	    ClientCanvas setcanvas
	} window send
    } def
    % send a debug message
    /msg {
	tDebugMessage tagprint typedprint
    } def
    % send keyboard input to emacs
    /emacsinput {
	tEmacsInput tagprint typedprint
    } def
    % ask emacs to insert a string
    /emacsinsert {
	tEmacsInsert tagprint typedprint
    } def
    % ask emacs to call a function, e.g. `(rmail) emacscmd'
    /emacscmd {
	{
	    dup type /stringtype ne { 1024 string cvs } if
	    tEmacsCommand tagprint typedprint
	} stopped pop
    } def
    % input handler
    % int values for selection rank
    /PrimarySelection 0 def
    /SecondarySelection 1 def
    /ShelfSelection 2 def
    % safe function for mapping back to names
    /rankname {				% 0 => /PrimarySelection
	[/PrimarySelection /SecondarySelection /ShelfSelection] exch
	{ get } stopped { pop pop /ShelfSelection } if
    } def
    /IP {
	% request keyboard events
	systemdict /Selections known {
	    currentcanvas addkbdinterests pop
	    currentcanvas addselectioninterests pop
	    currentcanvas addfunctionstringsinterest pop
	} {
	    createevent dup begin
		/Name 300 dict dup begin
		    0 1 255 { dup def } for
		end def
		/Action /DownTransition def
		/Canvas currentcanvas def
	    end expressinterest
	} ifelse
	% request meta key events
	createevent dup begin
	    /Name /Meta def
	    /Action 3 dict dup begin
		/DownTransition { tMetaKeyDown tagprint 1 typedprint } def
		/UpTransition { tMetaKeyDown tagprint 0 typedprint } def
	    end def
	end expressinterest
	% finally, watch for hints that the canvas may have changed
	createevent dup begin
	    /Name /resetyourself def
	    /Canvas currentcanvas def
	end expressinterest
	{
	    clear initmatrix
	    awaitevent dup /Name get dup
	    type /integertype eq {
		tKeyboardInput tagprint typedprint
	    } {				% event name
		{
		    /resetyourself {
			currentcanvas setcanvas
		    }
		    /InsertValue {
			/Action get dup
			type /stringtype eq {
			    Stuff? { emacsinput } { emacsinsert } ifelse
			} if
		    }
		    /SetSelectionAt {
			/Action get begin
			    tSetSelectionAt tagprint
			    Rank load typedprint Size typedprint
			    X typedprint Y typedprint
			end
		    }
		    /ExtendSelectionTo {
			/Action get begin
			    tExtSelectionTo tagprint
			    Rank load typedprint Size typedprint
			    X typedprint Y typedprint
			end
		    }
		    /DeSelect {
			/pendingselection null store
		    }
		} case
	    } ifelse
	} loop
    } def
    % Send Selection
    /SS {				% rank size string => -
	32 dict dup begin
	    4 1 roll			% sel-dict rank size string
	    /ContentsAscii exch def
	    /SelectionObjSize exch def
	end rankname setselection
    } def
    % execute a random string; n != 0 means expect to find something on top
    % of the stack afterwards which should be sent back to Emacs.
    /DO {				% string n => -
	0 eq {
	    { cvx exec } stopped pop
	} {
	    { cvx exec } stopped not count 1 gt and {
		{
		    dup type /stringtype eq not { 255 string cvs } if
		    Stuff? { emacsinput } { emacsinsert } ifelse
		} stopped pop
	    } if
	} ifelse
	clear
    } def
    % create the initial window
    emacswindow
    % Emacs takes over here

% C interface

cdef GetWinInfo(width, height)
    currentcanvas setcanvas
    inputprocess null ne {
	createevent dup begin
	    /Name /resetyourself def
	    /Process inputprocess def
	end sendevent
    } if
    clippath pathbbox 4 2 roll pop pop
    /winheight exch def /linewidth exch def
    tGetWinInfo tagprint
    linewidth typedprint
    winheight typedprint
=>  tGetWinInfo(width, height)

cdef GetFontInfo(font_charwidth, font_baseline, font_lineheight)
    tGetFontInfo tagprint
    charwidth typedprint
    baseline typedprint
    lineheight typedprint
=>  tGetFontInfo(font_charwidth, font_baseline, font_lineheight)

cdef RepairWindow(x0, y0, x1, y1) => tRepairWindow(y1, x1, y0, x0)
cdef KeyboardInput(c) => tKeyboardInput(c)
cdef MetaKeyDown(c) => tMetaKeyDown(c)
cdef EmacsInsert(string s) => tEmacsInsert(s)
cdef EmacsCommand(string s) => tEmacsCommand(s)
cdef EmacsInput(string s) => tEmacsInput(s)
cdef SetSelectionAt(rank, size, x, y) => tSetSelectionAt(rank, size, x, y)
cdef ExtSelectionTo(rank, size, x, y) => tExtSelectionTo(rank, size, x, y)
cdef DebugMessage(string s) => tDebugMessage(s)

cdef BeginRepair()	    BR
cdef EndRepair()	    ER
cdef ClearScreen()	    CL
cdef Flash()		    ^G
cdef ClearToEOL(x, y)	    x y CE
cdef InvertToEOL(x, y)	    x y FI
cdef InsertLines(y, n)	    y n IL
cdef InsertLines2(y, z, n)  y z n IL2
cdef DeleteLines(y, n)	    y n DL
cdef DeleteLines2(y, z, n)  y z n DL2
cdef DeleteChars(x, y, n)   x y n DC
cdef InsertChars(x, y, n)   x y n IC
cdef ToggleCursor(x, y)	    x y CT
cdef Show(x, y, cstring s)  s x y S
cdef ShowInverted(x, y, cstring s) s x y SI
cdef ClearShow(x, y, n, y2, cstring s) s x y2 n x y CS
cdef ClearShowInverted(x, y, n, y2, cstring s) s x y2 n x y CSI
cdef SendPS (cstring s, resp) s resp DO
cdef SetStuff (bool)	    /Stuff? bool 0 ne def
cdef SetFont(cstring f, n)  f n SF
cdef SetOrigin(x, y)	    x y /move window send
cdef SetDimensions(w, h)    w h SD
cdef SetFrameLabel(cstring s) s FL
cdef SetSelection(rank, size, cstring s) rank size s SS
cdef StartInput()	    /inputprocess /IP load fork def
cdef MapWindow()	    /map window send
cdef ReshapeWindow()	    /reshapefromuser window send
cdef InteractiveMove()	    IM
cdef RetainWindow(n)	    { FrameCanvas /Retained n 0 ne put } window send
