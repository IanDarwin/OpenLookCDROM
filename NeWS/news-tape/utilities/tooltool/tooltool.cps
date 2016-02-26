%
%  Tooltool cps file
%
%		Josh Axelrod 2/88

#define DONE_TAG 1
#define ITEM_DUMP_TAG 2
#define ITEM_CREATE_TAG 3
#define ITEM_CREATE_INFO_TAG 4
#define ITEM_DELETE_TAG 6
#define ITEM_XY_TAG 7
#define BACKGROUND 1
#define MAXITEMS 100
#define TOSTDOUT 1
#define TOFILE 2
#define INDEMO 3

%% Init Stuff %%

cdef init()

systemdict /Item known not {
  (NeWS/liteitem.ps) run
} if

%% Define keys in Userdict

/win null def			% main window
/popupwin null def		% popup window
/alertwin null def		% alert window
/helpwin null def		% help window
/can null def			% user canvas
/popupcan null def		% popup canvas
/alertcan null def		% alert canvas
/helpcan null def		% help canvas
/items MAXITEMS dict def	% user items
/popupitems null def		% popup items
/alertitems null def		% alert items
/helpitems null def		% help items
/itemhandler null def		% user item handler
/itemmover null def		% process that moves items
/popuphandler null def		% popup item handler
/alerthandler null def		% alert item handler
/helphandler null def		% help handler
/PopWin null def		% subclass for popups
/creating? false def		% flag 

/blankstring 40 string def	% load up with blanks
	0 1 39 {blankstring exch 32 put} for 
/longblank 95 string def
	0 1 94 {longblank exch 32 put} for

/unpackstring null def
/tmpstring null def

/rmoverdict 5 dict dup begin	% temps for rmover
	/currentitem null def		
	/currentitemname null def
end def

/builddict 10 dict dup begin	% temps for builditem
	/buildlocation null def
	/buildobject null def		
	/buildlabel null def
	/buildname null def
	/buildtype null def
	/unpackstring null def
end def

/delete_procdict 5 dict dup begin
	/tempkey null def
end def

framebuffer setcanvas clippath pathbbox
/MaxY exch def
/MaxX exch def
pop pop

%% Handler Setup %%

/rhandler { 			% restarts user item handler
	itemhandler null eq not 
		{
		itemhandler killprocess
		} if
	items length 0 ne 
		{		% make sure some items exist
		/itemhandler items forkitems store
		} if
	} def

/rmover { 			% restarts user item mover
	rmoverdict begin

	itemmover null eq not 
		{
		itemmover killprocess
		} if
	items length 0 ne 
	{			% make sure some items exist
	/itemmover		% there might be a better method for 
		[ items 	% building proc for eventmgrinterest
			{
		 	/currentitem exch store
			/currentitemname exch store

			/MiddleMouseButton 
			[ /items cvx BACKGROUND /moveinteractive 
			   /items cvx currentitemname /get cvx 
			   /send cvx] cvx
			/DownTransition
			currentitem /ItemCanvas get
			eventmgrinterest

			} forall
		]
		forkeventmgr store
	} if

	end			% pop rmoverdict

	} def

%% End of Handler Setup %%

%% Menus %%

/dumpmenu			% where to dump info
	[	
	(To stdout) {ITEM_DUMP_TAG tagprint TOSTDOUT typedprint}
	(To a file)  {ITEM_DUMP_TAG tagprint TOFILE typedprint}
	(Into a demo)  {ITEM_DUMP_TAG tagprint INDEMO typedprint}
	]
	/new DefaultMenu send def
{/MenuFont /Times-Bold findfont 18 scalefont def} dumpmenu send

/ditemmenu 			% delete item pullright
				% item names get added/deleted
	[[(Choose Item)	/Times-Italic findfont 18 scalefont] {nullproc}]
	/new DefaultMenu send def
{/MenuFont /Times-Bold findfont 18 scalefont def} ditemmenu send


/itemmenu 			% item creation pullright
	[	
	(ButtonItem)
	(CycleItem)
	(SliderItem)
	(TextItem)
	(MessageItem)
	(ArrayItem)
	]
	[{create_proc}] 
	/new DefaultMenu send def
{/MenuFont /Times-Bold findfont 18 scalefont def} itemmenu send

/basemenu 			% base menu
	[
	(Create an Item =>) itemmenu
	(Delete an Item =>) ditemmenu
	(Dump Current Items =>) dumpmenu
	]
	/new DefaultMenu send def
{/MenuFont /Times-Bold findfont 18 scalefont def} basemenu send

%% End of Menus %%

%% Menu Procs %%

/delete_proc {			% notify client of delete
				% fix menu and items dict 

		delete_procdict begin

		/tempkey /currentkey ditemmenu send store

		ITEM_DELETE_TAG tagprint
		tempkey typedprint

		items tempkey cvn known 
		{
			items tempkey cvn undef 
			rhandler 
			rmover
			/paint win send
		} if

		tempkey /searchkey ditemmenu send 
		{
			/deleteitem ditemmenu send
		} if

		end		% pop delete_procdict
	} def

/create_proc {			% notify client of new item
				% update popup for item type

		creating? not {

			ITEM_CREATE_TAG tagprint
			/currentkey itemmenu send typedprint

			/currentkey itemmenu send
			/printstring popupitems /themessage get send

			/currentkey itemmenu send (ButtonItem) eq
			{
			/unmap popupitems /theitemobject get send
			/unmap popupitems /theitemlocation get send
			}{
			/map popupitems /theitemobject get send
			/map popupitems /theitemlocation get send
			} ifelse

			getpopupplace /move popupwin send
			/paint popupwin send
			/map popupwin send
			/totop popupwin send
			/creating? true store

		}{
			(Finish Current Item) alertup
		} ifelse

	} def

%% End of Menu Procs %%

%% General Procs %%

/sendxy { 			% send current x,y for all items
				% called after choosing dump items
				% also window size on last send

	/tmpstring 10 string store 
	items begin
		items
			{
			pop dup 			% key key
			ITEM_XY_TAG tagprint
			tmpstring cvs typedprint 	% key
			/location exch cvx exec send	% x y
			exch typedprint typedprint
			} forall
	end
	ITEM_XY_TAG tagprint (done) typedprint 
	win /FrameWidth get typedprint
	win /FrameHeight get typedprint
	} def


/dotoken? { % string => bool
	false exch
	{
	dup dup 91 eq exch 47 eq or
		{ pop pop true exit } if
	32 eq not
		{ exit } if
	} forall
	} def

/unpack { % string => some object

	/unpackstring exch store

	unpackstring null eq unpackstring length 0 eq or
	{ 
		null
	}{
		unpackstring dotoken?
		{

			[ 
			unpackstring
				{
					token not 
					{
						exit
					}{ 
						exch
					} ifelse
				} loop

			]

			{exec} forall
		}{
			unpackstring
		} ifelse
	} ifelse
	} def

/builditem {			% type name label object loc => -
				% builds a new item

	builddict begin

	/buildlocation exch cvn store
	/buildobject exch unpack store
	/buildlabel exch unpack store
	/buildname exch cvn store
	/buildtype exch cvn store
	/tmpstring 10 string store % build a new string for each item

	{
	buildtype /ButtonItem eq
		{
		items buildname buildlabel 
			/nullproc can /new buildtype cvx exec send put
		20 20 0 0 /reshape items buildname get send
		} {
		items buildname buildlabel buildobject buildlocation
			/nullproc can /new buildtype cvx exec send put
		20 20 0 0 /reshape items buildname get send
		} ifelse

	} errored

	{
		(Item Creation Problem) alertup

		ITEM_DELETE_TAG tagprint
		buildname (        ) cvs typedprint

		items buildname cvn known 
		{
			items buildname cvn undef 
		} if
	}{
				% update delete pullright
		MAXITEMS buildname tmpstring cvs {delete_proc}
			/insertitem ditemmenu send
	} ifelse

	builddict { pop builddict exch null put } forall

	end			% pop builddict

	rhandler 
	rmover
	/paint win send
	/creating? false store

	} def

%% End of General Procs %%

%% Build Base Window %%

/win framebuffer /new DefaultWindow send store
	{
	/PaintClient {BACKGROUND fillcanvas items paintitems} def
	/FrameLabel (Tooltool) def
	/IconImage /no_ties def
	/DestroyClient {DONE_TAG tagprint /unmap win send} def
	/ClientMenu basemenu def
 	} win send

	/reshapefromuser win send
  	/can /GetCanvas win send store
	/map win send

%% End of Base Window %%
%% End of init %%

%% Popup Stuff %%

cdef init_popup()

%% Define SubClass, no controls, different menu %%

/PopWin LiteWindow []		% Should put more functionality in class
classbegin
/CreateFrameControls nullproc def
/CreateFrameInterests {
	FrameInterests begin
	    /FrameTopEvent
	        PointButton /totop
	        DownTransition FrameCanvas eventmgrinterest def
%	    /FrameCloseEvent
%	        PointButton /flipiconic
%	        DownTransition CloseControl eventmgrinterest def
%	    /FrameStretchEvent
%	        PointButton /stretchcorner
%	        DownTransition StretchControl eventmgrinterest def
	    /FrameMoveEvent
	        AdjustButton /slide
	        DownTransition FrameCanvas eventmgrinterest def
	    /FrameMenuEvent
		MenuButton {/showat FrameMenu send}
	        DownTransition FrameCanvas eventmgrinterest def
	    /FrameDamageEvent
		/Damaged /FixFrame
		null FrameCanvas eventmgrinterest def
	    /FrameEnterEvent
		/EnterEvent /EnterFrame
		[0 2] FrameCanvas eventmgrinterest def
	    /FrameExitEvent
		/ExitEvent /ExitFrame
		[0 2] FrameCanvas eventmgrinterest def
	    /FrameDoItEvent
		/DoItEvent {gsave /ClientData get cvx exec grestore}
		/Window null eventmgrinterest def
%	    /FrameIconicFcnKeyEvent
%		/WindowFunction /flipiconic
%		/FlipIconic FrameCanvas eventmgrinterest def
	    /FrameFrontFcnKeyEvent
		/WindowFunction /totop
		/FlipFront FrameCanvas eventmgrinterest def
        end
    } def
/CreateFrameMenu {
	/FrameMenu [
	    (Move)	{/slide ThisWindow send}
	    (Move Constrained)
		{getfbclick pop pop /slideconstrained ThisWindow send}
	    (Top)	{/totop ThisWindow send}
	    (Bottom)	{/tobottom ThisWindow send}
%	    (Zap)	{/destroy ThisWindow send}
%	    (Resize)	{/reshapefromuser ThisWindow send}
%	    (Stretch Corner)
%	    	{getfbclick pop pop /stretchcorner ThisWindow send}
%	    (Stretch Edge)
%	    	{getfbclick pop pop /stretchwindowedge ThisWindow send}
%	    (Close)	{/flipiconic ThisWindow send}
	    (Redisplay)	{/paint ThisWindow send}
	] /new DefaultMenu send def
%	FrameMenu /ThisWindow self put % GC LEAK
    } def

/MoveFrameControls nullproc def
/PaintFrameControls nullproc def

classend store

%% Popup Button Procs %%

/getpopupplace {		% - => x y  some good place for popup

	win /FrameX get	win /FrameWidth get add
	popupwin /FrameWidth get add
		MaxX gt
			{
				MaxX popupwin /FrameWidth get sub 
			}{
				win /FrameX get win /FrameWidth get add
			} ifelse

	win /FrameY get	win /FrameHeight get add
	popupwin /FrameHeight get add
		MaxY gt
			{
				MaxY popupwin /FrameHeight get sub 
			}{
				win /FrameY get win /FrameHeight get add
			} ifelse
	} def 
		
/popupdoneproc {		% send item info back

	ITEM_CREATE_INFO_TAG tagprint
	popupitems begin
		/getvalue theitemlabel send typedprint
		theitemlocation /ItemObject get
		theitemlocation /ItemValue get 1 get get
		theitemlocation /ItemValue get 0 get get typedprint
		/getvalue theitemobject send typedprint

				% need to reset text items too!
		[3 0] /setvalue theitemlocation send
%		() /inserttext theitemlabel send
%		() /inserttext theitemobject send
	end

	/unmap popupwin send

	} def

/cancelproc {			% cancel item creation

	popupitems begin
		[3 0] /setvalue theitemlocation send
%		() /inserttext theitemlabel send
%		() /inserttext theitemobject send
	end

	/unmap popupwin send
	/creating? false store

	} def

%% End of Popup Button Procs %%

/makepopupitems {		% items for popup
	/popupitems 
 	 dictbegin
		/donebutton (Done) /popupdoneproc popupcan
			/new ButtonItem send def
		230 20 0 0 /reshape donebutton send

		/cancelbutton (Cancel) /cancelproc popupcan
			/new ButtonItem send def
		285 20 0 0 /reshape cancelbutton send

		/helpbutton (Help) /helpup popupcan
			/new ButtonItem send def
		350 20 0 0 /reshape helpbutton send

		/theitemlabel (Label:) blankstring
			/Right /nullproc popupcan /new TextItem send def
		20 20 100 0 /reshape theitemlabel send

		/theitemobject (Object:) longblank
			/Right /nullproc popupcan /new TextItem send def
		20 60  100 0 /reshape theitemobject send

		/theitemlocation (Object Location:) 
			[[(Top) (Bottom) (Left) (Right)]] /Right
			/nullproc popupcan /new ArrayItem send def
		20 100 0 0 /reshape theitemlocation send
		[3 0] /setvalue theitemlocation send

		/themessage (Currently building a) 
			(                     ) /Right 
			/nullproc popupcan /new MessageItem send def
		20 140 0 0 /reshape themessage send
	 dictend store
	} def

%% Build Popup Window %%

/popupwin framebuffer /new PopWin send store
	{
	/PaintClient {BACKGROUND fillcanvas popupitems paintitems} def
	/FrameLabel (Item Info Sheet) def
	/DestroyClient {
		FrameEventMgr null ne {FrameEventMgr killprocess} if
		/unmap self send
		} def
 	} popupwin send

	
	0 0 430 200 /reshape popupwin send
	getpopupplace /move popupwin send
  	/popupcan /GetCanvas popupwin send store
	makepopupitems
	/popuphandler popupitems forkitems store

%% End of Popup Window %%

%% End Popup Stuff %%

%% Alert Stuff %%

/alertup {			% string => -
	/printstring alertitems /themessage get send
	getalertplace /move alertwin send
	/map alertwin send
	/totop alertwin send
	} def

/alertdown {
	/unmap alertwin send
	} def

/getalertplace {		% - => x y  some good place for popup

	win /FrameX get	win /FrameWidth get add
	alertwin /FrameWidth get add
		MaxX gt
			{
				MaxX alertwin /FrameWidth get sub 
			}{
				win /FrameX get win /FrameWidth get add
			} ifelse

	win /FrameY get	win /FrameHeight get add
	alertwin /FrameHeight get add
		MaxY gt
			{
				MaxY alertwin /FrameHeight get sub 
			}{
				win /FrameY get win /FrameHeight get add
			} ifelse
	} def 

/makealertitems {
	/alertitems
	dictbegin

		/cancelbutton (OK) /alertdown alertcan
			/new ButtonItem send def
		260 27 0 0 /reshape cancelbutton send

		/themessage ()
	(                                                    ) /Right
			/nullproc alertcan /new MessageItem send def
		{/ItemFont /Times-Bold findfont 16 scalefont def}
			themessage send
		25 35 0 0 /reshape themessage send

	dictend store
	} def

/alertwin framebuffer /new PopWin send store
	{
	/PaintClient {BACKGROUND fillcanvas alertitems paintitems} def
	/FrameLabel (Alert!) def
	/FrameFont /Times-Bold findfont 16 scalefont def
	/DestroyClient {
		FrameEventMgr null ne {FrameEventMgr killprocess} if
		/unmap self send
		} def
 	} alertwin send

	
	0 0 350 100 /reshape alertwin send
	getalertplace /move alertwin send
  	/alertcan /GetCanvas alertwin send store
	makealertitems
	/alerthandler alertitems forkitems store


%% End of Alert Stuff %%

%% Help Stuff %%

/helpup {			% string => -
	gethelpplace /move helpwin send
	/map helpwin send
	/totop helpwin send
	/paint helpwin send
	} def

/helpdown {
	/unmap helpwin send
	} def

/LeftMargin 10 def
/TopMargin 150 def
/tmptype null def
/ff /Times-Bold findfont 14 scalefont def

/newline {
	currentpoint 16 sub LeftMargin exch moveto pop
	} def

/writehelptext {

 	/tmptype popupitems /themessage get /ItemObject get store
	LeftMargin TopMargin moveto
	0 setgray

	(For a ) show tmptype show
	newline newline

	(ItemLabel is a string or an icon name) show
	newline
	gsave
		ff setfont
		(     aneatlabel or /clown) show
	grestore

	tmptype cvn /ButtonItem ne
	{
		tmptype {

		/CycleItem {
		newline newline
		(ItemObject is an array of strings or icons) show
		newline
		gsave
			ff setfont
		(     [(one) (two) (three)] or [/clown /baseball]) show
		grestore
		}
		/SliderItem {
		newline newline
		(ItemObject is an array of ints [min max step]) show
		newline
		gsave
			ff setfont
			(     [1 100 1]) show
		grestore
		}
		/TextItem {
		newline newline
		(ItemObject is a string) show
		newline
		gsave
			ff setfont
			(     file not found) show
		grestore
		}
		/MessageItem {
		newline newline
		(ItemObject is a string or an icon name) show
		newline
		gsave
			ff setfont
			(     aneatlabel or /clown) show
		grestore
		}
		/ArrayItem {
		newline newline
		(ItemObject is an array of equal size arrays) show
		newline
		(consisting of strings or icon names) show
		newline
		gsave
			ff setfont
		(      [[(one) (two)][(three) (four)]]) show
			newline
		(      [[/baseball /chess][(wow) (neat)]]) show
		grestore		
		}

		} case
	} if

	} def

/gethelpplace {		% - => x y  some good place for popup

	popupwin /FrameX get
	helpwin /FrameWidth get add
		MaxX gt
			{
				MaxX helpwin /FrameWidth get sub 
			}{
				popupwin /FrameX get 
			} ifelse

	popupwin /FrameY get
	helpwin /FrameHeight get add
		MaxY gt
			{
				MaxY helpwin /FrameHeight get sub 
			}{
				popupwin /FrameY get 
			} ifelse
	} def 

/makehelpitems {
	/helpitems
	dictbegin

		/cancelbutton (Done) /helpdown helpcan
			/new ButtonItem send def
		260 27 0 0 /reshape cancelbutton send

	dictend store
	} def

/helpwin framebuffer /new PopWin send store
	{
	/PaintClient {BACKGROUND fillcanvas helpitems paintitems
			writehelptext
		} def
	/FrameLabel (Help) def
	/FrameFont /Times-Bold findfont 16 scalefont def
	/DestroyClient {
		FrameEventMgr null ne {FrameEventMgr killprocess} if
		/unmap self send
		} def
 	} helpwin send

	
	0 0 350 200 /reshape helpwin send
	gethelpplace /move helpwin send
  	/helpcan /GetCanvas helpwin send store
	makehelpitems
	/helphandler helpitems forkitems store


%% End of Help Stuff %%
%% Client Side View %%

cdef get_done() => DONE_TAG()

cdef get_delete(string itemname) => ITEM_DELETE_TAG(itemname)

cdef get_item_info(string label, string object, string location) =>
		ITEM_CREATE_INFO_TAG(label, object, location)

cdef get_itemxy(string name, int x, int y) => ITEM_XY_TAG(name,x,y)

cdef dump_items(int mode) => ITEM_DUMP_TAG(mode)

cdef create_item(string itemtype) => ITEM_CREATE_TAG(itemtype)

cdef alert_up(string alertmessage)
	alertmessage alertup

cdef send_itemxy()
	sendxy

cdef build_item(string itype,string name, string label, 
	string object, string location)

	itype name label object location builditem

%% End Client Side View %%
