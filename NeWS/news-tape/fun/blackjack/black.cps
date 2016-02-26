#define	TAG_HIT		901
#define	TAG_STAND	902
#define	TAG_NEWGAME	903
#define	TAG_QUIT	904
#define	TAG_WAGER	905
#define	TAG_PAINT	906
#define	TAG_SPLIT	907

cdef ps_hit()	=> TAG_HIT ()
cdef ps_stand() => TAG_STAND ()
cdef ps_newgame() => TAG_NEWGAME ()
cdef ps_quit()	=> TAG_QUIT ()
cdef ps_wager(string v) => TAG_WAGER (v)
cdef ps_paint() => TAG_PAINT ()
cdef ps_split() => TAG_SPLIT ()

cdef ps_message(string s)
	(%) [s] /printf messages send

cdef ps_set_wager(v)
	wager begin
		/ItemValue v 20 string cvs def
	end
	[wager] paintitems

cdef ps_set_chips(v)
	(Chips:%) [v] /printf chips send

cdef ps_split_ok()
	/notify-split { TAG_SPLIT tagprint } def

cdef ps_split_no()
	/notify-split {} def

cdef ps_wager_ok()
	/notify-wager { TAG_WAGER tagprint ItemValue typedprint } def

cdef ps_wager_no()
	/notify-wager {} def

cdef ps_hit_ok()
	/notify-hit { TAG_HIT tagprint } def

cdef ps_hit_no()
	/notify-hit {} def

cdef ps_stand_ok()
	/notify-stand { TAG_STAND tagprint } def

cdef ps_stand_no()
	/notify-stand {} def

cdef ps_newgame_ok()
	/notify-newgame { TAG_NEWGAME tagprint } def

cdef ps_newgame_no()
	/notify-newgame {} def

cdef ps_split_first()
	gsave
		20 40 moveto
		0 10 rlineto 10 0 rlineto 0 5 rlineto 10 -10 rlineto
		-10 -10 rlineto 0 5 rlineto -10 0 rlineto
		0 0 0 rgbcolor setcolor fill
	grestore

cdef ps_split_second()
	gsave
		20 40 moveto
		0 10 rlineto 10 0 rlineto 0 5 rlineto 10 -10 rlineto
		-10 -10 rlineto 0 5 rlineto -10 0 rlineto
		FillColor setcolor fill
		20 100 moveto
		0 10 rlineto 10 0 rlineto 0 5 rlineto 10 -10 rlineto
		-10 -10 rlineto 0 5 rlineto -10 0 rlineto
		0 0 0 rgbcolor setcolor fill
	grestore

cdef ps_clear_cards()
	gsave
		FillColor setcolor
		0 0 moveto 400 150 rect fill
		0 300 moveto 400 150 rect fill
	grestore

cdef ps_flip_card(val, suit)
		gsave
			1 1 1 rgbcolor setcolor
			50 320 moveto 50 110 rect fill
			55 410 moveto
			0 0 0 rgbcolor setcolor
			TF card-val val 1 sub get show
			SF card-suit suit 1 sub get show
			50 320 moveto 50 110 rect stroke
		grestore

cdef ps_show_card(who, up, cnt, suit, val, split)
	can setcanvas
	/this-startx cnt 1 add 50 mul def
	who 2 lt
		 { split 0 eq
			{/this-starty  20 def /hgt 110 def}
			{ split 1 eq
				{ /this-starty 20 def /hgt 50 def }
				{ /this-starty 80 def /hgt 50 def } ifelse
			} ifelse
		 }
		 { /this-starty 320 def /hgt 110 def}
		 ifelse
	gsave
		0 0 0 rgbcolor setcolor
		up 0 eq
		{
			gsave
				can /Color get
					{ 1 0 0 rgbcolor }
					{ .9 .9 .9 rgbcolor } ifelse
				setcolor
				5 this-startx this-starty 60 hgt rrectpath fill
				0 0 0 rgbcolor setcolor
				this-startx 30 add this-starty hgt 2 div add 5 add moveto
				TF
				(Keith's) cshow
				this-startx 30 add this-starty hgt 2 div add 5 sub moveto
				(Casino) cshow
			grestore
		}
		{
			gsave
				1 1 1 rgbcolor setcolor
				5 this-startx this-starty 60 hgt rrectpath fill
				this-startx 5 add this-starty hgt 20 sub add moveto
				0 0 0 rgbcolor setcolor
				TF card-val val 1 sub get show
				SF card-suit suit 1 sub get show
			grestore
		} ifelse
		5 this-startx this-starty 60 hgt rrectpath stroke
	grestore

cdef ps_init()
	systemdict /Item known not { (NeWS/liteitem.ps) run } if
	1 setlinequality
	3 setlinewidth
	/this-startx 0 def
	/this-starty 0 def
	/hgt 110 def
	/TF { /Times-Bold findfont 12 scalefont setfont } def
	/SF { /Symbol findfont 12 scalefont setfont } def
	/card-val [(A) (2) (3) (4) (5) (6) (7) (8) (9) (10) (J) (Q) (K)] def
	/card-suit [<A7> <A8> <A9> <AA>] def
	/notify-hit { } def
	/notify-stand { } def
	/notify-newgame { } def
	/notify-wager { } def
	/notify-split { } def
	/createitems
	{
		/items 10 dict dup begin
			/split (SPLIT) /notify-split can 40 40 /new ButtonItem send
				450 200 /move 3 index send def
			/hit (HIT) /notify-hit can 40 40 /new ButtonItem send
				450 150 /move 3 index send def
			/stand (STAND) /notify-stand can 40 40 /new ButtonItem send
				450 100 /move 3 index send def
			/newgame (NEW GAME) /notify-newgame can 40 40 /new ButtonItem send
				450 50 /move 3 index send def
			/quit (QUIT) {TAG_QUIT tagprint} can 40 40 /new ButtonItem send
				450 0 /move 3 index send def
			/wager (Wager:) (1         ) /Right /notify-wager can 50 0
				/new TextItem send 450 250 /move 3 index send def
			/chips /panel_text (Chips:5000   ) /Right {} can 80 0
				/new MessageItem send dup begin
					/ItemFrame 1 def
					/ItemBorder 4 def
				end 450 300 /move 3 index send def
			/messages /panel_text () /Right {} can 500 0
				/new MessageItem send dup begin
					/ItemFrame 1 def
					/ItemBorder 4 def
				end 10 500 /move 3 index send def
		end def
		/wager items /wager get def
		/chips items /chips get def
		/messages items /messages get def
	} def
	/slideitem
	{
		gsave
			dup 4 1 roll
			/moveinteractive exch send
			/bbox exch send
		grestore
	} def
	/main
	{
		/win framebuffer /new DefaultWindow send def
		{
			/PaintClient {FillColor fillcanvas items paintitems TAG_PAINT tagprint} def
			/FrameLabel (Black Jack) def
		} win send
		200 200 600 600 /reshape win send
		/can win /ClientCanvas get def
		/FillColor can /Color get
			{0 1 0 rgbcolor}
			{.5 .5 .5 rgbcolor} ifelse
			def

		createitems

		/slidemgr [
			items {
				exch pop dup /ItemCanvas get
				MiddleMouseButton [items FillColor 6 -1 roll /slideitem cvx] cvx
				DownTransition 4 -1 roll eventmgrinterest
			      } forall
		] forkeventmgr def

		/map win send

		/itemmgr items forkitems def
	} def
	main
