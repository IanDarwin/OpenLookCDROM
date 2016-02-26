#define LMB_TAG 1
#define DAMAGE_TAG 2
#define DONE_TAG 3
#define MMB_TAG 4
#define RMB_TAG 5
#define DELB_TAG 6

cdef cps_initwindow(int sx,int sy,int xrot,int yrot)

systemdict /Item known not { (NeWS/liteitem.ps) run } if

/LMB /LeftMouseButton def
/MMB /MiddleMouseButton def
/RMB /RightMouseButton def

yrot xrot
userdict begin /xrot exch def /yrot exch def end

/downeventinterest {/DownTransition ClientCanvas eventmgrinterest} def

/startinput {
   /ButtonMgr[
      LMB {LMB_TAG getdirection} downeventinterest
      MMB {MMB_TAG getgeomx} downeventinterest
      RMB {RMB_TAG getgeomx} downeventinterest
    ]forkeventmgr store
} def

/repair {
   DAMAGE_TAG tagprint
   uniquecid dup typedprint
   [exch cidinterest] forkeventmgr
   waitprocess pop
} def

/makescale {
  /y exch def /x exch def
  4 2 roll pop pop
  y 0.5 add div exch
  x 1.5 add div exch scale
  0.01 setlinewidth
  0 y 0.5 add translate
  1 -1 scale
  0.75 0.25 translate
} def

/TreeWindow DefaultWindow
   dictbegin
       /ScaleX sx def
       /ScaleY sy def
       /DelButtonHeight 45 def
       /DelButtonWidth 60 def
       /FrameLabel (Tree) def
       /ButtonMgr null def
   dictend
   classbegin
       /CreateClientCanvas {
          /CreateClientCanvas super send
          /items 1 dict dup begin
             /DelButton (Delete) {DELB_TAG screenbutton} FrameCanvas
                DelButtonWidth 0 /new ButtonItem send
                dup /ItemBorderColor 0 0 0 rgbcolor put
                def
          end def
          /itemmgr items forkitems def
       } def
       /RescaleCanvas {
          /ScaleY exch def
          /ScaleX exch def
          setcanvas
%           [1 0 0 1 0 0] setmatrix % X11/NeWS: Comes out upside down!
          initmatrix % Fixed for X11/NewS. -Don
          0 exch translate
          initclip clippath pathbbox
          ScaleX ScaleY makescale
          currentcanvas reshapecanvas
       } def
       /ShapeClientCanvas {
          gsave
          /ShapeClientCanvas super send
          currentcanvas
          FrameCanvas setcanvas
          FrameWidth 2 div
          DelButtonWidth 2 div sub
          BorderBottom 5 add /move
          window /items get /DelButton get send
          setcanvas
          grestore
       } def
       /PaintClient { 1 fillcanvas
            repair
            /paint items /DelButton get send
            } def
       /PaintIcon { 0.75 0.498 0.196 rgbcolor fillcanvas
                    repair } def
       /flipiconic {
            /flipiconic super send
            Iconic? IconCanvas /Retained get and {/paint self send} if
       } def
       /move {
            Iconic? {IconHeight sub} if
            /move super send
       } def
       /DestroyClient {
           itemmgr killprocess
           ButtonMgr killprocess
           DONE_TAG tagprint
       } def
       /ForkFrameEventMgr{
           /ForkFrameEventMgr super send
           startinput
       } def
       /ClientPath {
           4 copy
           rectpath
           DelButtonHeight sub
           0 DelButtonHeight translate
           ScaleX ScaleY
           makescale
       } def
       /IconPath {
           4 copy
           rectpath
           ScaleX ScaleY
           makescale
       } def
   classend def

/window framebuffer /new TreeWindow send def
/reshapefromuser window send
/map window send

/getdirection {
   ClientCanvas setcanvas
   tagprint uniquecid dup typedprint
   exch
   begin
      YLocation userdict /yrot get sub dup mul
      XLocation userdict /xrot get sub 3 div dup mul add 0.09 lt
      {3}
      {YLocation userdict /yrot get gt
         {XLocation userdict /xrot get lt {1} {2} ifelse}
         {0} ifelse} ifelse
   end
   typedprint
   [exch cidinterest] forkeventmgr
   waitprocess pop
} def

/getgeomx {
   ClientCanvas setcanvas
   tagprint uniquecid dup typedprint
   exch
   begin
      XLocation
   end
   typedprint
   [exch cidinterest] forkeventmgr
   waitprocess pop
} def

/screenbutton {
   window begin
   ClientCanvas setcanvas
   tagprint uniquecid dup typedprint
   [exch cidinterest] forkeventmgr
   waitprocess pop
   end
} def

cdef cps_get_comm(int id, int cnum) => LMB_TAG(id,cnum)
cdef cps_midbutton(int id, float geomx) => MMB_TAG(id,geomx)
cdef cps_rightbutton(int id, float geomx) => RMB_TAG(id,geomx)
cdef cps_delbutton(int id) => DELB_TAG(id)
cdef cps_damage(int id) => DAMAGE_TAG(id)
cdef cps_done() => DONE_TAG()

cdef cps_exit(int id)
       id {exit} sendcidevent

cdef cps_clearandrescale(int id, int sx, int sy)
  id {
    DelButtonHeight ClientCanvas sx sy /RescaleCanvas window send
    0 IconCanvas sx sy /RescaleCanvas window send
    ClientCanvas setcanvas
    gsave
%       [1 0 0 1 0 0] setmatrix % X11/NeWS: Comes out upside down!
       initmatrix % Fixed for X11/NewS. -Don
       initclip clippath pathbbox
       0 DelButtonHeight translate
       DelButtonHeight sub
       rectpath 1 setgray fill
    grestore
    repair
   pause} sendcidevent

cdef cps_drawedge(int id, int x1, int y1, int x2, int y2)
  id {
   x1 y1 moveto x2 y2 lineto 0 setgray stroke
  pause} sendcidevent

cdef cps_drawnode(int id, int x,int y)
  id {
   gsave x y translate 3 1 scale
   0 0 0.25 0 360 arc 0 setgray fill grestore
  pause} sendcidevent

cdef cps_Wcirc(int id,int x,int y)
  id {
   gsave x y translate 3 1 scale
   0 0 0.1 0 360 arc 1 setgray fill grestore
   userdict begin
      /xrot x def
      /yrot y def
   end
 pause} sendcidevent

cdef cps_Bcirc(int id,int x,int y)
  id {
   gsave x y translate 3 1 scale
   0 0 0.1 0 360 arc 0 setgray fill grestore
  pause} sendcidevent
