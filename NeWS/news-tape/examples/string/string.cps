% string.cps - a simple NeWS client

% Copyright (C) 1988 by Martha Zimet. All rights reserved.
% This program is provided for unrestricted use, provided that this 
% copyright message is preserved. There is no warranty, and no author 
% or distributer accepts responsibility for any damage caused by this 
% program. 

#define SET_GRAY_TAG		1
#define PAINT_CLIENT_TAG	2
#define DONE_TAG		3
#define GET_STR_TAG		4

cdef initialize()
    % a common cliche
    systemdict /Item known not { (NeWS/liteitem.ps) run } if
    /hold_string (<Current String comes here>) def
    /notify? true def
    /notify {
        notify? {(Notify: Value=%) [ItemValue] /printf messages send
		 userdict /hold_string ItemValue put } if
    } def 

    /createitems {
       /items 50 dict dup begin
          /nameitem (String:) () /Right /notify can 220 0
          /new TextItem send 20 260 /move 3 index send def

          /messages /panel_text hold_string /Right {} can 500 0
          /new MessageItem send dup begin
             /ItemFrame 1 def
             /ItemBorder 4 def
             end 20 290 /move 3 index send def
          end def
          /messages items /messages get def
       } def

    /win framebuffer /new DefaultWindow send def
    {	/FrameLabel (GetStr) def
	/IconLabel (GetStr) def
	/PaintClient {PAINT_CLIENT_TAG tagprint} def
	/ClientMenu [							
	    (Send String) {  hold_string GET_STR_TAG tagprint typedprint}
	    (White)    {  1 SET_GRAY_TAG tagprint typedprint}
	    (Lite)     {.75 SET_GRAY_TAG tagprint typedprint}
	    (Gray)     {.50 SET_GRAY_TAG tagprint typedprint}
	    (Dark)     {.25 SET_GRAY_TAG tagprint typedprint}
	    (Black)    {  0 SET_GRAY_TAG tagprint typedprint}
	] /new DefaultMenu send def
    } win send
    200 200 700 350 /reshape win send
    /can win /ClientCanvas get def

    % Create Items
    createitems
    /reshapefromuser win send
    /map win send
    /itemmgr items forkitems def

cdef get_gray(float fillgray) => SET_GRAY_TAG (fillgray)
cdef get_str(string h_string) => GET_STR_TAG (h_string)
cdef get_paint_client() => PAINT_CLIENT_TAG()
cdef get_done() => DONE_TAG()

cdef call_paint_client()
    /paintclient win send
cdef paint_client(float fillgray)
    {ClientCanvas setcanvas fillgray fillcanvas items paintitems} win send

