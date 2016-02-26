%  @(#)tnt_cps.cps	1.24 91/09/14

%
% Copyright (c) 1991 by Sun Microsystems, Inc.
%
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

#define JET_MAJOR_VERSION 5 /* incremented from 4 by Liam */
#define JET_MINOR_VERSION 1

#define	PACKAGE_STATUS	0

cdef enter_package(int return_value) => PACKAGE_STATUS (return_value)
    /is_v3? openwinversion 0 get 51 eq def

    is_v3? {
	/NeWS 3 0 knownpackage not {
	    /NeWS 3 0 findpackage beginpackage
	} if

	systemdict /_DefaultFileSearchPath known {
	    {
		[3 0] sleep
		systemdict  /_DefaultFileSearchPath known not { 
		    [3 0] sleep
		    exit 
		} if
	    } loop
	} if

	/TNTCore 3 0 knownpackage not {
	    /TNTCore 3 0 findpackage beginpackage
	    /TNT     3 0 findpackage beginpackage
	} if
	
	/X11     3 0 knownpackage not {
	    /X11 3 0 findpackage beginpackage
	} if

        shareddict /NeWSJet-monitor-critter known not {
	    shareddict /NeWSJet-monitor-critter createmonitor put
        } if

	shareddict
    } {
        {
            systemdict /BannerCanvas known not { exit } if
            3 60 div sleep
        } loop

        systemdict /NeWSJet-monitor-critter known not {
	    systemdict /NeWSJet-monitor-critter createmonitor put
        } if

	systemdict
    } ifelse

    /NeWSJet-monitor-critter get {
	{
	    is_v3? {
		/NeWSTerm-jet JET_MAJOR_VERSION JET_MINOR_VERSION findpackage beginpackage
	    } {
		NeWSTerm-jet begin userdict begin
	    } ifelse
	} stopped { 0 } { 1 } ifelse 
        PACKAGE_STATUS tagprint typedprint flush
    } monitor
		    { % Attempting to fool emacs into indenting correctly
		    end
		    } pop

cdef define_package()
    mark

    is_v3? {shareddict}{systemdict} ifelse
    /NeWSJet-monitor-critter get {
	{
	    is_v3? {
		/NeWSTerm-jet JET_MAJOR_VERSION JET_MINOR_VERSION findpackage
	    } {
		NeWSTerm-jet
	    } ifelse
	} stopped {
#include "package.ps"
	} if
    } monitor
    cleartomark

cdef ps_init_ps(string font_name, int font_size, int retained,  int scrollbar_side,  int autos, int cols,  int c_thing, int r_thing)
    {
	/cols c_thing def
	/rows r_thing def
	/tsf font_name def
	
	tsf cvn findfont font_size scalefont 	      	
	/ISOLatin1Encoding encodefont setfont
	
	NeWSTerm-startup-part1
	
	is_v3? not {
	    0 1 font_list length 1 sub { 
		dup font_list exch get font_name eq {
		    28 lt {
			userdict /v2_fixed true put
		    } {
			userdict /v2_fixed false put
		    } ifelse
		    exit
		} {
		    pop
		} ifelse
	    } for
	} if
	
	tsf cvn /ChangeFont can send
	
	font_size /set_tsize can send
	
	autos 0 eq {
	    false /set_autoscale can send
	} {
	    true  /set_autoscale can send
	} ifelse
	
	cols /set_cols can send
	
	/win_retained retained 0 eq not def
	
	/sbar_side scrollbar_side def

	
	NeWSTerm-startup-part2
    } exec

cdef ps_set_tag(string name, int tag_num)
    name cvn tag_num def

cdef ps_reset_canvas()
    /reset_can can send

cdef ps_clear_region(float x1, float y1, float x2, float y2)
    x1 y1 x2 y2 cr

cdef ps_ul_show_at(float x,float y,string str)
    x y moveto str show
    gsave
	currentfont begin
	    FontMatrix 0 get dup	% scale scale (e.g. 0.012 for 12 pt)
	    UnderlineThickness mul	% scale thick
	    setlinewidth
	    UnderlinePosition mul
	end
	dup				% dy dy
	0 exch rmoveto			% dy
	x				% dy x
	exch y add			% x y
	lineto stroke
    grestore

cdef ps_show_at(float x,float y,string str)
    x y moveto str show

cdef ps_text_color(float r, float g, float b)
    r g b rgbcolor setcolor
    % FG setcolor

cdef ps_back_color()
    BG setcolor

cdef ps_fill_region(float x1,float y1,float x2,float y2)
    x1 y1 x2 y2 rectpath fill

cdef ps_clear_screen()
    cs

cdef ps_invert_box(float tx1, float ty1, float tx2, float ty2)
      tx1 ty1 tx2 ty2 ib

cdef ps_copyarea(float x1, float y1, float x2, float y2, float x3, float y3)
    x1 y1 x2 y2 rectpath x3 y3 copyarea
%    x3 y3 x1 y1 x2 y2 cpa

cdef ps_bell()
    beep

cdef ps_set_selection(string str)
    str /set_selection can send

cdef ps_set_size(float xsize, float ysize)
    framebuffer setcanvas
    xsize ysize rs
    pause pause pause
    can setcanvas
    newpath clipcanvas

cdef ps_scroll_bottom()
    /goto_bottom slider send

cdef ps_set_view(int x, int y)
    y x sub x y /setparameters slider send

cdef ps_pause()
    pause


cdef ps_set_defaults(string font_name, int font_size, int retained, int scrollmode, int scrollbar_side, string term_id, int autos, int cols, int saved_lines)
    {
	autos 0 eq {
	    /Inactive /setvisualstate columns_label send
	    /Inactive /setvisualstate columns_text send
	    /Active /setvisualstate size_set send
	    [0] /setvalue fixed_set send
	} {
	    /Active /setvisualstate columns_label send
	    /Active /setvisualstate columns_text send
	    /Inactive /setvisualstate size_set send
	    [1] /setvalue fixed_set send
	} ifelse
	cols /setvalue columns_text send

	[
	    font_size 8 sub 2 div cvi
	    dup 28 eq {pop 11 } if
	] /setvalue size_set send
	[scrollbar_side] /setvalue sbar-pos send
	[scrollmode] /setvalue scrolling_set send
	retained 0 eq {[]} {[0]} ifelse /setvalue retained_check send
	term_id /setvalue termident_text send
	[] /setvalue list send
	0 1 font_list length 1 sub { 
	    dup font_list exch get font_name eq {
		1 array astore /setvalue list send
	    } {
		pop
	    } ifelse
	} for
	/value list send length 0 eq {
	    /Prop_Font font_name store
	} {
	    /locatechoice list send
	} ifelse
	saved_lines /setvalue saved_lines_text send
    } exec

cdef ps_apply()
    apply_values

cdef ps_cancel_selection()
    /PrimarySelection clearselection

cdef ps_set_pins(int pin_x, int pin_y, int last_x, int last_y)
    mark {
	/PrimarySelection getselection
	[pin_x pin_y] [last_x last_y] before {
	    pin_x  pin_y   /set_point1      3 index   send
	    last_x last_y  /set_point2      4 -1 roll send
	} {
	    pin_x  pin_y   /set_point2      3 index   send
	    last_x last_y  /set_point1      4 -1 roll send
	} ifelse
    } stopped cleartomark

cdef ps_set_label(string str)
    str /setlabel win send

cdef ps_set_icon_label(string str)
    str /seticonlabel win send


cdef ps_draw_cursor(int x_critter, int y_critter)
    x_critter y_critter dc

cdef ps_erase_cursor()
    ec

cdef ps_put_char(int c_critter)
    c_critter d

cdef ps_new_clip()
    self setcanvas

cdef ps_set_clip()
    clip_path setpath clip newpath

cdef ps_namedictbegin(string name)
    name 30 dict begin

cdef ps_dictenddef()
    currentdict end def

cdef ps_namebooleandef(string name, int value)
    name cvn value 0 ne def

cdef ps_nameintdef(string name, int value)
    name cvn value def

cdef ps_namerealdef(string name, float value)
    name cvn value def

cdef ps_namestringdef(string name, string value)
    name cvn value def

cdef ps_namenamedef(string name, string value)
    name cvn value cvn def

cdef ps_ready()
    ready_tag tagprint flush

cdef tnt_reshape_win_ps(int x1,int y1,int x2,int y2)
    gsave
        framebuffer setcanvas
        x1 y1 x2 y2 /reshape win send
    grestore

cdef ps_disable_cursor()
    cursor_enabled {
	/cursor_enabled false promote
	ec
    } if

cdef ps_enable_cursor()
    cursor_enabled not {
	/cursor_enabled true promote
	ec
    } if

cdef ps_close_connection()
    currentfile closefile

cdef ps_damage_start(int tag, float fx1, float fy1, float fx2, float fy2) => tag (fy2, fx2, fy1, fx1)
  ds

cdef ps_damage_end()
  de
