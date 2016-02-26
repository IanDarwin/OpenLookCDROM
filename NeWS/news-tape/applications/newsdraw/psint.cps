%!
%
% $Header: psint.cps,v 1.6 88/12/02 10:43:17 bvs Exp $
%

cdef ps_init(string buf)
	/DEFSTR_TAG	0 def
	/TOOL_TAG	100 def
	/SELECT_TAG	200 def
	/ADJUST_TAG	300 def
	/POINT_TAG	400 def
	/ANGLE_TAG	401 def
	/KEY_TAG	500 def
	/DAMAGE_TAG	600 def
	/PROP_TAG	700 def

	/STRETCH_MODE 	0 def
	/ROTATE_MODE 	1 def
	/BRUSH_MODE		2 def
	/LINE_MODE		3 def
	/RECT_MODE		4 def
	/CIRC_MODE	 	5 def
	/OVAL_MODE		6 def
	/TEXT_MODE		7 def
	/POLY_MODE		8 def

	/FILL_KEY	1 def
	/STROKE_KEY	2 def
	/WIDTH_KEY	3 def
	/PRINT_KEY	4 def
	/TOTOP_KEY	5 def
	/TOBOT_KEY	6 def
	/TEXT_KEY	7 def
	/REDRAW_KEY	8 def
	/WRITE_KEY	9 def
	/READ_KEY	10 def

	buf (/menubar.ps) append LoadFile
	buf (/draw.ps) append LoadFile
	clear

#define DEFSTR_TAG	0
#define TOOL_TAG	100
#define SELECT_TAG	200
#define ADJUST_TAG	300
#define POINT_TAG	400
#define ANGLE_TAG	401
#define KEY_TAG		500
#define DAMAGE_TAG	600
#define PROP_TAG	700


cdef ps_fontsetup(string name, size, index, length, bbheight, descent) => \
	DEFSTR_TAG(bbheight, descent, length)
	name findfont size scalefont dup index fontsetup

cdef ps_setcanvas()
	win begin ClientCanvas setcanvas end
cdef ps_setgray(float gray)
	gray setgray

cdef ps_rotate(float angle) angle rotate
cdef ps_translate(x, y) x y translate

cdef ps_dorotate(int x, int y, float angle)
	matrix currentmatrix
	x y translate angle rotate
	x neg y neg translate
cdef ps_unrotate()
	setmatrix

cdef ps_moveinteractive(xinit, yinit, float angle, x0, y0, x1, y1)
	xinit yinit angle x0 y0 x1 y1 /rectMoveInteractive win send

cdef ps_pencilCreateinteractive(x, y)
	x y /pencilCreateInteractive win send
cdef ps_linecreateinteractive(x, y)
	x y /lineCreateInteractive win send
cdef ps_rectcreateinteractive(x, y)
	x y /rectCreateInteractive win send
cdef ps_ovalcreateinteractive(x, y)
	x y /ovalCreateInteractive win send
cdef ps_circcreateinteractive(x, y)
	x y /circCreateInteractive win send
cdef ps_rotateInteractive(float angle, xinit, yinit, xcenter, ycenter, x0, y0, x1, y1)
	angle xinit yinit xcenter ycenter x0 y0 x1 y1 /rotateInteractive win send

cdef ps_stdcursor() /ptr /ptr_m win begin ClientCanvas end setstandardcursor
cdef ps_xcursor() /xhair /xhair_m win begin ClientCanvas end setstandardcursor

cdef ps_stroke() stroke
cdef ps_fill() fill
cdef ps_setlinewidth(width) width setlinewidth
cdef ps_pushclip(x0, y0, x1, y1)
	gsave x0 y0 moveto x0 y1 lineto x1 y1 lineto x1 y0 lineto closepath clip
cdef ps_popclip() grestore
cdef ps_savearea(x, y, w, h)
	x y w h /savearea win send
cdef ps_restorearea()
	/restorearea win send

cdef ps_drawline(x0, y0, x, y)
	x0 y0 moveto x y lineto
cdef ps_drawrect(x0, y0, x, y)
	x0 y0 moveto x0 y lineto x y lineto x y0 lineto closepath
cdef ps_drawcirc(x0, y0, r)
	x0 y0 r 0 360 arc
cdef ps_drawoval(x0, y0, x, y)
	matrix currentmatrix
	x0 y0 translate
	x x0 sub y y0 sub scale
	.5 .5 .5 0 360 arc
	setmatrix

%% ps_drawoval(x, y, r)
%%		x y r 0 360 arc

cdef ps_getcreatepoint(x, y) => POINT_TAG(x, y)
cdef ps_getcreateangle(float f) => ANGLE_TAG(f)

cdef ps_getmode(int mode) => TOOL_TAG(mode)
cdef ps_getselect(int x, int y) => SELECT_TAG(x, y)
cdef ps_getadjust(int x, int y) => ADJUST_TAG(x, y)
cdef ps_getkey(int key) => KEY_TAG(key)
cdef ps_getprop(int key) => PROP_TAG(key)
cdef ps_getdamage(int x0, int y0, int x1, int y1) => DAMAGE_TAG(x0, y0, x1, y1)
cdef ps_getint(int i) => (i)
cdef ps_getfloat(float f) => (f)


cdef ps_polyline(int count)
	moveto count { lineto } repeat stroke

cdef ps_sendpoint(int x, int y) x y

cdef ps_doclick()
	currentcanvas createoverlay setcanvas
	getclick CLICK_TAG tagprint exch typedprint typedprint
	pstack
