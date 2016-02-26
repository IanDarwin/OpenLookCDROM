%! news.cps
%
% Definitions for MicroEmacs NeWS front end.
%
% P.R.Ove

% C PostScript interface for NeWS distributed microEmacs
%
% Keep these guys short.  The defs should be sent in ps_initdef
#define GETS_TAG		0
#define GETSCREENSIZE_TAG	1
#define GETWINDOWSIZE_TAG	2
#define LOCALINIT_TAG		3


% Main channel to the remote process.
cdef ps_gets(int n, string s)
	getstring				% stack: string
	GETS_TAG		tagprint	% id tag
	dup length		typedprint	% send length
				typedprint	% send string
=> GETS_TAG (n, s)

cdef ps_getscreensize(int rows, int cols)
	GETSCREENSIZE_TAG	tagprint
	win /Rows get		typedprint
	win /Cols get		typedprint
=> GETSCREENSIZE_TAG (rows, cols)

cdef ps_getwindowsize(int height, int width)
	GETWINDOWSIZE_TAG	tagprint
	win /Height get		typedprint
	win /Width get		typedprint
=> GETWINDOWSIZE_TAG (height, width)

cdef ps_move(int row, int col)
	{row col remotemove} win send

cdef ps_eeol()
	{erase_eol} win send

cdef ps_eeop()
	{erase_eop} win send

cdef ps_cls()
	{clrscreen} win send

cdef ps_setrowcolors(int row, int fg, int bg)
	{row fg bg setcolors} win send

cdef ps_terminate()	% why must I do this?
	{Terminate} win send

cdef ps_putc(int c)
	{c putc} win send

cdef ps_putline(int row, string s, int fgcol, int bgcol)
	{row s fgcol bgcol putline} win send

cdef ps_fastputline(int row, string s)	% first use cls
	{row s fastputline} win send

cdef ps_scroller(int src, int dest, int n)
	{src dest n scroller} win send

cdef ps_dump(string s)		% debugging aid
{	(ps_dump: %\n) [s] dbgprintf
	0 1 3 { (row % = %\n) [3 -1 roll dup Image exch get] dbgprintf } for
	(\n) [] dbgprintf
} win send

cdef ps_insertoff()
	{overtypemode} win send

cdef ps_inserton()
	{insertmode} win send

cdef ps_cmodeon()
	{cmodeon} win send

cdef ps_cmodeoff()
	{cmodeoff} win send

cdef ps_immediateon()
	{immediateon} win send

cdef ps_immediateoff()
	{immediateoff} win send

cdef ps_statefix()				% NeWS bug workaround
	{State setstate} win send

cdef ps_localupdate()
	{imagetoscreen} win send



cdef ps_initialize(int arg_rows)

% basic non-graphics utilities
	/ctrl {		% (A) => ctrl A
		0 get dup 96 gt {32 sub} if
		64 sub
	} def
	/ascii { 0 get } def	% (A) => 65
	/ESC	27 def
	/CTRLX	(X) ctrl def
	/SPEC	128 def
	/BLANK	32 def
	/bounds {		% r c => r c  (restrain)
		exch LastRow min exch
		LastCol min
		exch 0 max exch
		0 max
	} def
	/rc_xy {		% r c => x y
		bounds
		CharWidth mul exch
		1 add Rows exch sub CharHeight mul
	} def
	/xy_rc {		% x y => r c
		CharHeight idiv LastRow exch sub
		exch CharWidth idiv
		bounds
	} def
	/clrstring {		% string => -	% slow, used to init "blanks"
		0 1 2 index length 1 sub
			{1 index exch 32 put } for pop
	} def
	/blankline {	% Image row => -	% faster, uses "blanks"
		blanks Cols string copy put
	} def
	/chartostring {		% 65 => (A)
		( ) dup 0 4 -1 roll put
	} def


% Color definitions
	/setcoloring {		% flag => -	(true for color)
	     {	/WHITE		0	0	1 hsbcolor def
		/CYAN		.5	.3	1 hsbcolor def
		/MAGENTA	.82	.3	1 hsbcolor def
		/YELLOW		.165	.3	1 hsbcolor def
		/BLUE		.6	.3	1 hsbcolor def
		/RED		1	.3	1 hsbcolor def
		/GREEN		.3	.3	1 hsbcolor def
		/BLACK		0	0	0 hsbcolor def
		/shade	{colortable exch get setcolor} def
		}
	     {	/BLACK		0 def
		/CYAN		.87 def
		/MAGENTA	.8 def
		/YELLOW		.95 def
		/BLUE		.6 def
		/RED		.7 def
		/GREEN		.5 def
		/WHITE		1 def
		/shade	{colortable exch get setgray} def
		} ifelse
		/colortable [WHITE RED GREEN YELLOW
			     BLUE MAGENTA CYAN BLACK] def
	} def


% Low level drawing utilities (don't effect the image buffer).
	/rc_moveto {rc_xy moveto} def	% r c => -
	/showchar {		% r c int => -
		gsave
		Fgcolor 3 index get shade
		3 -1 roll 3 -1 roll
		rc_moveto
		chartostring show
		grestore
	} def
	/whitebox {		% r c count -   (for erasing characters)
		gsave
		Bgcolor 3 index get shade
		3 -1 roll 3 -1 roll rc_xy translate
		BoxWidth mul dup		% stack: width width
		newpath
		0 BoxBottom moveto
		BoxBottom lineto
		BoxTop lineto
		0 BoxTop lineto
		closepath fill
		grestore
	} def
	/blackbox {		% r c -   (for text cursor)
		gsave
		Fgcolor 2 index get shade rc_xy translate
		newpath
		0 BoxBottom moveto
		BoxWidth BoxBottom lineto
		BoxWidth BoxTop lineto
		0 BoxTop lineto
		closepath fill
		grestore
	} def


% Text manipulation routines (effect cursor and image buffer).
% These can call the lower routines, but calling each other is
% risky (accidental recursion).
	% text cursor
	/tcursormove {		% r c => -
		bounds
		Tccol Cols lt {		% screen width may have changed
			Tcrow Tccol 1 whitebox
			Tcrow Tccol Image Tcrow get Tccol get showchar
		} if
		/Tccol exch store /Tcrow exch store
		Tcrow Tccol blackbox
		Tcrow Tccol rc_moveto
		gsave
		ColorDisplay?		% can't make grey chars on b&w
			{Bgcolor Tcrow get shade}
			{0 shade}
		ifelse
		Tcrow Tccol rc_moveto
		Image Tcrow get Tccol get chartostring show
		grestore
	} def
	% mouse cursor
	/mcursorwait {		% when waiting for remote to position
		/hourg /hourg_m ClientCanvas setstandardcursor
		/InputLock? true store
	} def
	/mcursornormal {
		/ptr /ptr_m ClientCanvas setstandardcursor
		/Confused? false store
		/InputLock? false store
	} def
	/remotemove {		% row col => -
		/prefix_flag false store
		1 index MessageLine eq {no_prefixes} {normal_prefixes} ifelse
		tcursormove
		mcursornormal
	} def
	/onmessageline? {
		{Tcrow MessageLine eq} win send
	} def
	/putc {			% like overtypechar, but handles backspace
		dup 8 eq {
			pop
			Tcrow Tccol 1 whitebox
			Image Tcrow get Tccol 32 put
			Tcrow Tccol 1 sub tcursormove }
		{overtypechar}
		ifelse
	} def
	/overtypechar {		% int => -	(at current)
		Tcrow Tccol 1 whitebox
		TempFont setfont
		Tcrow Tccol 2 index showchar
		Image Tcrow get Tccol 2 index put
		Tcrow Tccol 1 add tcursormove
		NormalFont setfont
		pop
	} def
	/insertchar {		% int => -	(at current location)
		Image Tcrow get Tccol			% char line index
		2 copy Cols Tccol sub 1 sub getinterval	% char line index tail
		exch 1 add exch				% shift index
		putinterval				% write tail
		gsave
		Tcrow Tccol Cols Tccol sub whitebox	% clear line
		Tcrow Tccol rc_moveto
		Fgcolor Tcrow get shade
		Image Tcrow get Tccol Cols Tccol sub getinterval show
		overtypechar				% put char
		grestore
	} def
	/writechar {		% int => -
		InsertMode {insertchar} {overtypechar} ifelse
	} def
	/fastputline {		% row string => -	(assume all cleared)
		gsave
		Fgcolor 2 index get shade
		exch dup 0 rc_moveto
		Image exch get copy show		% transfer text
		grestore
	} def
	/putline {		% row string fgcol bgcol => -
		gsave
		Bgcolor 4 index 3 -1 roll put
		Fgcolor 3 index 3 -1 roll put
		Fgcolor 2 index get shade
		Image 2 index blankline			% clear image line
		Image 2 index get copy pop		% transfer text
		dup 0 Cols whitebox			% clear screen line
		dup 0 rc_moveto
		Image exch get show
		grestore
	} def
	/setcolors {		% row fg bg => -
		Fgcolor 3 index 3 index put
		Bgcolor 3 index 2 index put
		pop pop pop
	} def
	/scroller {		% src dest count => -
		Image 3 index 2 index getinterval
		Image 3 index 3 -1 roll putinterval
		1 index add 1 sub 1 exch {
			dup 0 Cols whitebox
			dup Image exch get fastputline
		} for
		pop	% src popped
	} def
	/imagetoscreen {	% display text buffer
		gsave
		clearpage
		0 1 LastRow {
			dup Fgcolor exch get shade
			dup 0 rc_moveto
			Image exch get show
		} for
		Tcrow Tccol tcursormove
		grestore
	} def
	/clearpage {		% clear the display (not Image buffer)
		0 1 LastRow {0 Cols whitebox} for
	} def
	/cls {			% clear text buffer and screen
		0 1 LastRow {Image exch blankline} for
		clearpage
	} def
	/clrscreen {		% all but message line
		Image MessageLine get dup length string copy
		cls
		MessageLine exch fastputline
	} def
	/erase_eol {		% clear to end of line, from cursor
		Image Tcrow get				% line
		Tccol					% index
		blanks 0 Cols Tccol sub getinterval	% blanks
		putinterval				% insert them
		Tcrow Tccol Cols Tccol sub whitebox
		Tcrow Tccol tcursormove
	} def
	/erase_eop {		% clear to end of page, from cursor
		erase_eol
		Tcrow 1 add 1 MessageLine {
			Image 1 index blankline
			0 Cols whitebox
		} for
	} def




	% Window definition, menus, etc

	/filemenu [
		(find)		{ [CTRLX (F) ctrl] sendarray}
		(read)		{ [CTRLX (R) ctrl] sendarray}
		(insert)	{ [CTRLX (I) ctrl] sendarray}
		(view)		{ [CTRLX (V) ctrl] sendarray}
		(save)		{ [CTRLX (S) ctrl] sendarray}
		(write)		{ [CTRLX (W) ctrl] sendarray}
	] /new DefaultMenu send def
	/buffermenu [
		(next)		{ [CTRLX (X) ascii] sendarray }
		(list)		{ [CTRLX (B) ctrl] sendarray }
		(select)	{ [CTRLX (B) ascii] sendarray }
		(kill)		{ [CTRLX (K) ascii] sendarray }
		(overtype mode)	{ (\201SLover\n) sendarray }
		(C mode)	{ (\201SLcmode\n) sendarray }
	] /new DefaultMenu send def
	/lcolormenu [
		(white)		{ (\201SLwhite\n) sendarray }
		(blue)		{ (\201SLblue\n) sendarray }
		(yellow)	{ (\201SLyellow\n) sendarray }
		(cyan)		{ (\201SLcyan\n) sendarray }
		(red)		{ (\201SLred\n) sendarray }
		(magenta)	{ (\201SLmagenta\n) sendarray }
		(green)		{ (\201SLgreen\n) sendarray }
	] /new DefaultMenu send def
	/windowmenu [
		(split)		{ [CTRLX (2) ascii] sendarray }
		(delete)	{ [CTRLX (0) ascii] sendarray }
		(delete others)	{ [CTRLX (1) ascii] sendarray }
		(redraw)	{ [ESC (L) ctrl] sendarray }
		(set colors =>) lcolormenu
	] /new DefaultMenu send def
	/searchmenu [
		(search)		{ [(S) ctrl] sendarray }
		(reverse search)	{ [(R) ctrl] sendarray }
		(incremental search)	{ [CTRLX (S) ascii] sendarray }
		(reverse incremental search) { [CTRLX (R) ascii] sendarray }
	] /new DefaultMenu send def
	/replacemenu [
		(replace)	{ [ESC (R) ascii] sendarray }
		(query replace)	{ [ESC (R) ctrl] sendarray }
	] /new DefaultMenu send def
	/argmenu
		[(prompt) (2) (3) (4) (5) (10) (100) (1000)]
		[{[(U) ctrl] sendarray}
		 {[(U) ctrl] sendarray currentkey (    ) cvs sendarray}	]
	/new DefaultMenu send def
	/pagemenu [
		(beginning)	{ [ESC (<) ascii] sendarray }
		(end)		{ [ESC (>) ascii] sendarray }
		(next)		{ [(V) ctrl] sendarray }
		(previous)	{ [(Z) ctrl] sendarray }
	] /new DefaultMenu send def
	/miscmenu [
		(bind function key) { [ESC (K) ascii] sendarray }
		(color)		{ {true setcoloring PaintClient} win send }
		(B & W)		{ {false setcoloring PaintClient} win send }
	] /new DefaultMenu send def
	/gcolormenu [
		(white)		{ (\201SGwhite\n) sendarray }
		(blue)		{ (\201SGblue\n) sendarray }
		(yellow)	{ (\201SGyellow\n) sendarray }
		(cyan)		{ (\201SGcyan\n) sendarray }
		(red)		{ (\201SGred\n) sendarray }
		(magenta)	{ (\201SGmagenta\n) sendarray }
		(green)		{ (\201SGgreen\n) sendarray }
	] /new DefaultMenu send def
	/globalmodemenu [
		(overtype)	{ (\201SGover\n) sendarray }
		(insert)	{ (\201RGover\n) sendarray }
		(C mode)	{ (\201SGcmode\n) sendarray }
		(C mode off)	{ (\201RGcmode\n) sendarray }
		(set colors =>)	gcolormenu
	] /new DefaultMenu send def


	% Mode menu support
	/emacsmode {		% (OVER) flag => -	(false to reset)
		[CTRLX (M) 4 -1 roll {ascii} {ctrl} ifelse] sendarray
		sendarray (\n) sendarray
	} def
	/overtypemode {
		/InsertMode false store
		(overtype mode) /searchkey buffermenu send {
			(insert mode) { (\201RLover\n) sendarray }
			/changeitem buffermenu send
		} if
	} def
	/insertmode {
		/InsertMode true store
		(insert mode) /searchkey buffermenu send {
			(overtype mode) { (\201SLover\n) sendarray }
			/changeitem buffermenu send
		} if
	} def
	/cmodeon {
		(\)}]#) {times exch default_short put} forall
		(C mode) /searchkey buffermenu send {
			(C mode off) { (\201RLcmode\n) sendarray }
			/changeitem buffermenu send
		} if
	} def
	/cmodeoff {
		(\)}]#) {times exch default_long put} forall
		(C mode off) /searchkey buffermenu send {
			(C mode) { (\201SLcmode\n) sendarray }
			/changeitem buffermenu send
		} if
	} def

	/request_refresh	{ [(L) ctrl] sendarray } def




	/win framebuffer /new DefaultWindow send def	% Create a window
	/wincanvas {win /ClientCanvas get} def

	% define window context
	{
		/State		0 def
		/Confused?	false store
		/InputLock?	false store
		/NormalFont	0 def
		/TempFont	0 def
		/Rows		arg_rows 2 add def	% space for messages
		/Cols		80 def
		/LastRow	Rows 1 sub def
		/LastCol	Cols 1 sub def
		/MessageLine	Rows 2 sub def
		/Fgcolor	[Rows {7} repeat] def
		/Bgcolor	[Rows {0} repeat] def
		/Tcrow		0 def		% text cursor location
		/Tccol		0 def
		/Image		Rows array def	
		/Height		0 def
		/Width		0 def
		/CharHeight	0 def
		/CharWidth	0 def
		/BoxTop		0 def		% cursor and wipeout box
		/BoxBottom	0 def
		/BoxWidth	0 def
		/OldFrame	[0 0] def	% to detect resize repaints
		/InsertMode	true def	% local insert/overtype
		/FrameLabel	(NeWS Distributed uEMACS) def
		/framechanged? {	
			OldFrame 0 get Width ne
			OldFrame 1 get Height ne
			or
		} def
		/sizeOK? {
			dup 50 gt
		} def
		/trimwindow {		% so that it is multiple of char size
			ClientCanvas setcanvas
			clippath pathbbox 3 -1 roll pop 3 -1 roll pop
			sizeOK?
			{
				/Height exch store
				Height Rows Height Rows div round mul sub
				dup 0 ne
				{	FrameHeight exch sub
					/FrameHeight exch store
					FrameX FrameY FrameWidth FrameHeight reshape }
				{ 	pop }
				ifelse
			} if
		} def
		/Terminate {		% let remote guy kill us
			stopeventtraps
			/destroy self send
		} def
		/DestroyClient {	% request termination from remote guy
			unmap		% hide it in case we miss
			[CTRLX (C) ctrl (Y) ascii] sendarray
		} def
		/PaintClient {
			stopeventtraps
			trimwindow
			initwindow
			framechanged?
			{	0 1 LastRow {Image exch Cols string put} for
				request_refresh
			}
			{	imagetoscreen }
			ifelse
			starteventtraps
		} def
		/ClientMenu [
			(Paging =>)	pagemenu
			(Files =>)	filemenu
			(Buffers =>)	buffermenu
			(Windows =>)	windowmenu
			(Global modes =>)	globalmodemenu
			(Search =>)	searchmenu
			(Replace =>)	replacemenu
			(Argument =>)	argmenu
			(Miscellaneous =>)	miscmenu
			(quit)				{ [CTRLX (C) ctrl] sendarray }
			(save & exit)		{ [ESC (Z) ascii] sendarray }
		] /new DefaultMenu send def
	} win send

	/initwindow {
		/OldFrame [Width Height] store
		ClientCanvas setcanvas
		clippath pathbbox 3 -1 roll pop 3 -1 roll pop
		/Height exch store /Width exch store
		/CharHeight Height Rows div store
		/NormalFont (Courier-Bold) findfont CharHeight scalefont store
		/TempFont (Courier) findfont CharHeight scalefont store
		NormalFont setfont
		/CharWidth (A) stringwidth pop store
		/Cols Width CharWidth idiv store
		/LastCol Cols 1 sub store
		/BoxWidth CharWidth store
		gsave newpath 0 0 moveto ([|/) true charpath pathbbox 
		/BoxTop exch 1 sub store pop pop pop	
		gsave newpath 0 0 moveto (_py) true charpath pathbbox pop pop
		/BoxBottom exch store pop
		/BoxTop BoxTop CharHeight BoxBottom add max store
		/blanks Cols string dup clrstring def
		NormalFont setfont
		/State currentstate store
	} def



	/starteventtraps {
	% Input dispatcher.  Catches all kbd input and mouse activity.
	/dispatcher_pid {
		wincanvas addkbdinterests pop
		wincanvas addfunctionstringsinterest pop
		createevent dup begin
			/Name dictbegin
				0 1 127 { dup def } for
				/MiddleMouseButton dup def
				/InsertValue dup def
			dictend def
			/Action /DownTransition def
			/Canvas wincanvas def
		end expressinterest
		createevent dup begin
			/Name dictbegin
				/LeftMouseButton dup def
				/MouseDragged dup def
			dictend def
			/Action null def
			/Canvas wincanvas def
		end expressinterest
		systemdict /IP currentprocess put

		% input loop
		{	clear
			awaitevent begin
				% only ascii allowed on message line
				onmessageline?
				{keyeventdict Name known}
				{eventdict Name known}
				ifelse
				{	% Invoke the right handler
					XLocation YLocation Action
					eventdict Name get exec
				} if
			end
		} loop
	} fork def

	% Mouse still handler (derived from mouse drag event).
	/stillmouse_pid {
		createevent dup begin
			/Name /MouseStill def
			/Canvas wincanvas def
		end expressinterest

		% input loop
		{	clear
			awaitevent begin
				128 addtobuf	% SPEC code
				Xl Yl /xy_rc win send
				exch addtobuf addtobuf
				default_short outtimerrefresh
			end
		} loop
	} fork def
	} def

	/stopeventtraps {
		currentdict /dispatcher_pid known
			{dispatcher_pid killprocess} if
		currentdict /stillmouse_pid known
			{stillmouse_pid killprocess} if
	} def


	% distribution mechanism: character timing and prefixes

	/prefix_flag false def
	/prefixes 0 string def
	/normal_prefixes {
		/prefixes 2 string store
		prefixes 0 ESC put
		prefixes 1 CTRLX put
	} def
	/no_prefixes {
		/prefixes 0 string store
	} def
	normal_prefixes
	/prefix? {	% int => flag		(is char in the set?)
		chartostring prefixes exch search
		{pop pop pop true}
		{pop false}
		ifelse
	} def


	/times 128 array def		% delay times
	/default_short .4 def		% command time delay
	/default_long 2. def		% text time delay
	/normal_times {
		0 1 31 {times exch default_short put} for
		32 1 126 {times exch default_long put} for
		times 127 default_short put
	} def
	normal_times

	/immediateon {
		{0 1 127 {times exch default_short put} for} win send
		no_prefixes
	} def
	/immediateoff {
		normal_times
		normal_prefixes
	} def
	/local? {	% int => flag	% local display if long
		times exch get default_long eq
	} def


	/outtimerrefresh {		% seconds => -	refresh timer
		/outtimer_seconds exch def
		currentdict /flushevent known {
			flushevent recallevent
		} if
		/flushevent createevent def
		flushevent dup begin
			/Name /Flush def
			/TimeStamp currenttime 
				outtimer_seconds 60 div add def
			/Canvas wincanvas def
		end sendevent
	} def


	% Event handlers.  Routines are invoked with XLocation, YLocation,
	% and Action on the stack.

	/eventdict dictbegin
		/EnterEvent {		% refork event handlers
			pop pop pop
			stopeventtraps
			starteventtraps
		} def
		/LeftMouseButton {	% cutting text
			/UpTransition eq
			{	% wipeout
				/xy_rc win send
				SPEC addtobuf
				[exch 3 -1 roll exch (W) ctrl] sendarray
			}
			{	% set mark
				/xy_rc win send
				SPEC addtobuf
				[exch 3 -1 roll exch ESC BLANK] sendarray
				10. outtimerrefresh	% "infinite" wait
			} ifelse
		} def
		/MiddleMouseButton {	% yanking text
			pop
			/xy_rc win send
			SPEC addtobuf
			[exch 3 -1 roll exch (Y) ctrl] sendarray
		} def
		/MouseDragged { pop		% reset "still" timer
			/yl exch def
			/xl exch def
			/mcursorwait win send
			currentdict /stillevent known {
				stillevent recallevent
			} if
			/stillevent createevent def
			stillevent dup begin
				/Name /MouseStill def
				/TimeStamp currenttime .5 60 div add def
				/Canvas wincanvas def
				/Xl xl def
				/Yl yl def
			end sendevent
		} def
		/InsertValue {
			dup length 3 sub 2 exch getinterval cvi
			[exch] sendarray
			pop pop
		} def
		0 1 127 { dup [exch /asciihandler cvx] cvx def } for
	dictend def



	% A restricted set handler names, safe on the command line.
	/keyeventdict dictbegin
		/InsertValue dup def
		0 1 127 { dup [exch /asciihandler cvx] cvx def } for
	dictend def

	% ordinary character handler (called with Xl, Yl, Action, and code).
	/asciihandler {
		InputLock?
		{ pop }
		{ ascii_nolock }
		ifelse
		pop pop pop
	} def

	% character handler, input not locked (called with ascii code on stack).
	/ascii_nolock {
		prefix_flag
		{	/prefix_flag false store
			default_short outtimerrefresh }
		{
			dup ascii_noprefix }
		ifelse
		addtobuf
	} def

	% character handler, no prefix in effect (called with ascii code on stack).
	/ascii_noprefix {
		/ascii_code exch def
		ascii_code prefix?
		{	/prefix_flag true store
			default_long outtimerrefresh }
		{	ascii_code local?
			{	Confused?
				{default_short outtimerrefresh}
				{times ascii_code get
				 outtimerrefresh
				 ascii_code /writechar win send}
				ifelse
			}
			{	/Confused? true store
				times ascii_code get
				outtimerrefresh
			}
			ifelse
		} ifelse
	} def




	% Send an array of characters to the remote process.
	/sendarray {		% [(A) ascii (B) ascii] => -
		{addtobuf} forall
		default_short outtimerrefresh
	ResetKeyboard
	} def


	% Output buffer (accumulate stuff to send to the remote machine).
	/Outbufdict dictbegin
		/outbuf 1000 string def
		/bufindex 0 def
	dictend def

	/addtobuf {		% int => -	% Add a character
		Outbufdict begin
			outbuf bufindex 3 -1 roll put
			/bufindex bufindex 1 add store
		end
	} def
	/clearbuf {
		Outbufdict begin
			/bufindex 0 store
		end
	} def
	/getbuf {
		Outbufdict begin
			outbuf 0 bufindex getinterval
		end
	} def

	% Get a string from the user
	/getstring {		% - => array
		createevent dup begin
			/Name /Flush def
			/Canvas wincanvas def
		end expressinterest
	
		{	awaitevent pop
			getbuf
			dup length 0 gt {
				clearbuf
				exit
			} if
			pop
		} loop
	} def

	/UserProfile 100 dict def
	/ResetKeyboard {
		createevent dup begin
			/Name
				UserProfile /ViewStop known {
						UserProfile /ViewStop get
							} {
						16#6f01     %  default to L1
						} ifelse def
						/Action /DownTransition def
		end dup sendevent
		createevent copy dup /Action /UpTransition put
		sendevent
	} def

	% Start her up.
	{	reshapefromuser
		trimwindow
		initwindow
		ColorDisplay? setcoloring
		0 1 LastRow {Image exch Cols string put} for
		starteventtraps
		cls map
		ResetKeyboard
	} win send

