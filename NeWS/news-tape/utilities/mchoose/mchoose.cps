#define KEY_TAG 0
#define STRING_TAG 1
#define ZAP_TAG 2

cdef choose_init(string label, string menuclassname)
  100 dict begin
  /popup-font /Times-Bold findfont 46 scalefont def
  /popup-margin 20 def
  /popup-black-thickness 14 def
  /popup-gray-thickness 6 def
  /popup-white-thickness 3 def
  /popup-radius 25 def
  /kbd-interests null def
  /menu-class
    menuclassname cvn cvlit
    systemdict 1 index known {
      systemdict exch get
    } {
      pop DefaultMenu
    } ifelse
  def
  /popup { % string => -
    gsave
      /msg exch def
      framebuffer setcanvas
      initmatrix
      currentcursorlocation /cury exch def /curx exch def
      popup-font setfont
      /popup-can framebuffer newcanvas def
      msg stringbbox
      /h exch popup-margin dup add add def
      /w exch popup-margin dup add add def
      pop pop
      /x  curx  w 2 div sub  def
      /y  cury  h 2 div sub  def
      systemdict /HereIAm known {
        currentdict HereIAm } if
      x y translate
      newpath
      popup-radius 0 0 w h rrectpath 
      popup-can reshapecanvas
      popup-can canvastotop
      popup-can /Mapped true put
      /this-menu null def
      /popup-menu null def
      /popup-interests [
        MenuButton /do-popup-menu DownTransition popup-can
          eventmgrinterest
        AdjustButton /slide DownTransition popup-can
          eventmgrinterest
        /Damaged /draw-popup null popup-can 
	  eventmgrinterest
        PointButton /zap-popup UpTransition popup-can
          eventmgrinterest
      ] def
    grestore
  } def

  /AcceptFocus {} def
  /RestoreFocus {} def
  /DeSelect {} def
  /Ignore {} def

  /InsertValue {
    ev /Action get do-string
  } def

  /do-key {
    KEY_TAG tagprint
    currentkey typedprint
    pause flush
  } def

  /do-string { % string => -
    STRING_TAG tagprint
    typedprint
    pause flush
  } def

  /zap-popup {
    popup-can /Mapped false put
%     kbd-interests null ne {
%       kbd-interests popup-can revokekbdinterests
%     } if
    ZAP_TAG tagprint
    pause flush
    currentprocess killprocessgroup
  } def

  /add-key { % key => -
    pause
    this-menu null eq {
      /this-menu
        [ 3 -1 roll ]
	[ {do-key} ]
	/new menu-class send
      def
    } {
        this-menu /MenuKeys get length exch {do-key} 
	/insertitem this-menu send
    } ifelse
  } def

  /add-submenu { % key id => -
    this-menu null eq {
      /this-menu			%  key id /this-menu
        [ 4 -1 roll ]			%  id /this-menu [key]
	[ 4 -1 roll cvn load ]		%  /this-menu [key] [id]
	/new menu-class send		%  /this-menu menu
      def				%
    } {
        this-menu /MenuKeys get length	%  key id len
	3 1 roll cvn load
	/insertitem this-menu send
    } ifelse
  } def

  /name-menu { % name => -
    cvn this-menu def
    /this-menu null def
  } def

  /start-popup { % name => -
      cvn load /popup-menu exch def
      /popup-mgr popup-interests forkeventmgr def
      popup-can /Mapped true put
      /kbd-interests popup-can addkbdinterests def
      {
        clear ( ) dup 0
        awaitevent /ev exch def
	ev /Name get
	dup type /integertype eq {
	  put do-string
	} {
          { cvx exec } stopped
	} ifelse
      } loop
  } def

  /do-popup-menu {
    popup-menu null ne {
      { /showat where } popup-menu send {
        pop
        CurrentEvent /showat popup-menu send
      } {
        /show popup-menu send 
      } ifelse
    } if
  } def

  /slide { % - => -  (Interactively move window)
    {
      popup-can setcanvas
      InteractionLock { interactivemove } monitor
      popup-can /Parent get getcanvaslocation
      neg /y exch def neg /x exch def
    } fork pop
  } def

  /draw-popup {
    gsave
      popup-can setcanvas
      popup-font setfont
      newpath
      clippath 1 setgray fill
      newpath
      popup-black-thickness popup-radius 0 0 w h rrectframe
      .5 setgray eofill
      newpath
      popup-gray-thickness popup-radius 0 0 w h rrectframe
      0 setgray eofill
%      newpath
%      popup-white-thickness popup-radius 0 0 w h insetrrect rrectpath
%      closepath
%      1 setlinewidth 1 setgray stroke
      0 setgray
      popup-margin dup moveto
      msg show
    grestore
  } def

  label popup

cdef add_key(string key)
  key add-key

cdef add_submenu(string key, string id)
  key id add-submenu

cdef start_popup(string name)
  name start-popup

cdef name_menu(string name)
  name name-menu

cdef get_key(string key) => KEY_TAG (key)

cdef get_string(string key) => STRING_TAG (key)

cdef get_zap() => ZAP_TAG ()
