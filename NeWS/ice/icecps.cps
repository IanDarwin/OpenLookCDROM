% icecps.cps

#include "ice_tags.h"
C: #include "ice_tags.h"


cdef news_syncserver() => SYNC_TAG()

	SYNC_TAG tagprint

cdef news_setcanvas(int PixmapID, int dpi) => SETC_TAG()
% the caller doesn't need the tagprint, but without it
% CPS seems to garble the PixmapID arg if the caller
% starts switching from one pixmap to another because
% the page size is changed -- I haven't got a fucking
% clue why this happens!!

	SETC_TAG tagprint
	PixmapID /XLookupID XResource send setcanvas
	dpi clippath pathbbox 0 exch
	translate pop pop pop
	72 div dup neg scale

cdef news_echoargs(int arg1, int arg2, int res1, int res2) => ECHO_TAG (res2, res1)
% used for test purposes only

	ECHO_TAG tagprint
	arg2 typedprint
	arg1 typedprint

cdef news_traperrors()
% attempt to insulate calling program from
% PostScript errors contained within an external file
% that the caller is attempting to render --
% filetokens used:
% 0 => save context
% 1 => true until caller reaches end of rendering code
% 2 => true until a rendering error causes stop invocation

	/ICEnoperands count def
	/ICEndicts countdictstack def
	save 0 setfileinputtoken
	true 1 setfileinputtoken
	true 2 setfileinputtoken
	{
        	{ currentfile cvx exec } stopped
        	{

			% restore op and dict stacks to original state
			count ICEnoperands gt {
				/ICEpopoperands count ICEnoperands sub def
				1 1 ICEpopoperands { pop pop } for
			} if
			countdictstack ICEndicts gt {
				/ICEpopdicts countdictstack ICEndicts sub def
				1 1 ICEpopdicts { pop end } for
			} if

			% clean up VM
			0 getfileinputtoken restore

			% still in the rendering code?
			1 getfileinputtoken

			% in middle of rendering code, so the stop
			% was triggered by a rendering error
			{

				% any previous rendering error?
				2 getfileinputtoken

				% no previous rendering error
				{
					false 2 setfileinputtoken
					RENDERERR_TAG tagprint
				} if
			}

			% at end of rendering code, so the stop was
			% invoked by the caller after rendering completion
			{
				exit
			} ifelse
		}
        	{

			% restore op and dict stacks to original state
			count ICEnoperands gt {
				/ICEpopoperands count ICEnoperands sub def
				1 1 ICEpopoperands { pop pop } for
			} if
			countdictstack ICEndicts gt {
				/ICEpopdicts countdictstack ICEndicts sub def
				1 1 ICEpopdicts { pop end } for
			} if

			% clean up VM
			0 getfileinputtoken restore

			exit
		} ifelse
	} loop

cdef news_fillcanvas(float fill, int ObjectID)

	ObjectID news_setcanvas
	fill fillcanvas

cdef news_translate(float x, float y)

	x y translate

cdef news_scale(float h, float v)

	h v scale

cdef news_rotate(float r)

	r rotate

cdef news_font(string name, float size)

	name cvn findfont size scalefont setfont

cdef news_setrgbcolor(float r, float g, float b)

	r g b setrgbcolor

cdef news_fillrectangle(float x, float y, float w, float h)

	newpath
	x y moveto
	w 0 rlineto
	0 h rlineto
	w neg 0 rlineto
	closepath fill

cdef news_strwidth(string str, float w) => STRWIDTH_TAG(w)

	str stringwidth pop
	STRWIDTH_TAG tagprint
	typedprint

cdef news_show(float x, float y, string str)

	x y moveto
	str show

cdef news_widthshow(float x, float y, float blankadj, string str)

	x y moveto
	blankadj 0 8#040 str widthshow

cdef news_setlinewidth(float w)

	w setlinewidth

cdef news_setlinecap(int i)

	i setlinecap

cdef news_drawline(float xstart, float ystart, float xstop, float ystop)

	newpath
	xstart ystart moveto
	xstop ystop lineto
	stroke

cdef news_drawsquare(float radius,
		     int drawbnd, float bndwidth, float rb, float gb, float bb,
		     int drawfill, float rf, float gf, float bf)

	newpath
	radius neg radius neg moveto
	radius 2 mul 0 rlineto
	0 radius 2 mul rlineto
	radius 2 mul neg 0 rlineto
	closepath

	drawfill 1 eq {
		rf gf bf setrgbcolor
		gsave
		fill
		grestore
	} if

	drawbnd 1 eq {
		rb gb bb setrgbcolor
		bndwidth setlinewidth
		stroke
	} if

cdef news_drawtriangle(float radius,
		       int drawbnd, float bndwidth, float rb, float gb, float bb,
		       int drawfill, float rf, float gf, float bf)

	newpath
	radius neg radius neg moveto
	radius 2 mul 0 rlineto
	radius neg radius 2 mul rlineto
	closepath

	drawfill 1 eq {
		rf gf bf setrgbcolor
		gsave
		fill
		grestore
	} if

	drawbnd 1 eq {
		rb gb bb setrgbcolor
		bndwidth setlinewidth
		stroke
	} if

cdef news_drawcircle(float radius,
		     int drawbnd, float bndwidth, float rb, float gb, float bb,
		     int drawfill, float rf, float gf, float bf)

	newpath
	0 0 radius 0 360 arc

	drawfill 1 eq {
		rf gf bf setrgbcolor
		gsave
		fill
		grestore
	} if

	drawbnd 1 eq {
		rb gb bb setrgbcolor
		bndwidth setlinewidth
		stroke
	} if

cdef news_drawcross(float radius,
		    int drawbnd, float bndwidth, float rb, float gb, float bb)

	newpath
	radius neg 0 moveto
	radius 2 mul 0 rlineto
	0 radius neg moveto
	0 radius 2 mul rlineto

	drawbnd 1 eq {
		rb gb bb setrgbcolor
		bndwidth setlinewidth
		stroke
	} if

cdef news_pathtextinit()
% code to display text along the current path,
% taken from the PostScript Language Tutorial and Cookbook
% and enhanced to support letterspacing, arbitrary scaling
% and center-of-width character placement for better handling
% of large type

	/ICEPathTextDict 35 dict def

	ICEPathTextDict begin

	/PathTextSetchar {
		alarmclock 0 eq {
			pause
			/alarmclock 40 def
		} {
			/alarmclock alarmclock 1 sub def
		} ifelse
		/currchar str charcount 1 getinterval def
		/charcount charcount 1 add def
		/currw currchar stringwidth pop def
		/dw currw hscale mul 2 div ltrspace add def
		charcount str length lt {
			/nextchar str charcount 1 getinterval def
			/nextw nextchar stringwidth pop def
			/dw nextw hscale mul 2 div dw add def
		} {
			/nextw 0 def
		} ifelse
		gsave
		cpx cpy itransform translate
		dy dx atan rotate
		hscale vscale scale
		currw 2 div neg 0 moveto
		currchar show
		ltrspace hscale div nextw 2 div add 0 rmoveto
		currentpoint transform
		/cpy exch def
		/cpx exch def
		grestore
		/setdist setdist dw add def
	} def

	/PathTextMovetoproc {
		alarmclock 0 eq {
			pause
			/alarmclock 40 def
		} {
			/alarmclock alarmclock 1 sub def
		} ifelse
		/newy exch def
		/newx exch def
		/firstx newx def
		/firsty newy def
		pathstarted 1 eq {
			/ovr 0 def
		} {
			/ovr setdist def
		} ifelse
		/pathstarted 1 def
		newx newy transform
		/cpy exch def
		/cpx exch def
	} def

	/PathTextLinetoproc {
		alarmclock 0 eq {
			pause
			/alarmclock 40 def
		} {
			/alarmclock alarmclock 1 sub def
		} ifelse
		/oldx newx def
		/oldy newy def
		/newy exch def
		/newx exch def
		/dx newx oldx sub def
		/dy newy oldy sub def
		/dist dx dup mul dy dup mul add sqrt def
		dist 0 ne {
			/dsx dx dist div ovr mul def
			/dsy dy dist div ovr mul def
			oldx dsx add oldy dsy add transform
			/cpy exch def
			/cpx exch def
			/pathdist pathdist dist add def
			{
				setdist pathdist le {
					charcount str length lt {
						PathTextSetchar
					} {
						exit
					} ifelse
				} {
					/ovr setdist pathdist sub def
					exit
				} ifelse
			} loop
		} if
	} def

	/PathTextCurvetoproc {
		(ERROR: No curveto after flattenpath.) print
	} def

	/PathTextClosepathproc {
		firstx firsty PathTextLinetoproc
		firstx firsty PathTextMovetoproc
	} def

	% string offset letterspace hscale vscale PathText => -
	/PathText {
		/vscale exch def
		/hscale exch def
		/ltrspace exch def
		/offset exch def
		/str exch def
		/charcount 0 def
		/pathstarted 0 def
		/currchar str charcount 1 getinterval def
		/dw currchar stringwidth pop hscale mul 2 div def
		/setdist offset dw add def
		/pathdist 0 def
		/alarmclock 40 def
		gsave
		flattenpath
		{ PathTextMovetoproc } { PathTextLinetoproc }
		{ PathTextCurvetoproc } { PathTextClosepathproc } pathforall
		grestore
		newpath
	} def

	end

cdef news_vectorinit()
% code to render a vector with arrow-shaped pointers

	/ICEVectorDict 25 dict def

	ICEVectorDict begin

	% angle x y Pointer => -
	/Pointer {
		gsave
		translate
		rotate
		0 setlinejoin
		[] 0 setdash
		ptrstyle 0 eq {
			newpath
			ptrolen ptrwd moveto
			0 0 lineto
			ptrolen ptrwd neg lineto
			stroke
		} {
			newpath
			0 0 moveto
			ptrolen ptrwd lineto
			ptrilen 0 lineto
			ptrolen ptrwd neg lineto
			closepath
			fill
		} ifelse
		grestore
	} def

	% xstart ystart xstop ystop linewd ptr ptrstyle ptrwd ptrolen ptrilen Vector => -
	/Vector {
		/ptrilen exch def
		/ptrolen exch def
		/ptrwd exch 2 div def
		/ptrstyle exch def
		/ptr exch def
		/linewd exch def
		/ystop exch 72 mul def
		/xstop exch 72 mul def
		/ystart exch 72 mul def
		/xstart exch 72 mul def
		/xdiff xstop xstart sub def
		/ydiff ystop ystart sub def
		/veclen xdiff xdiff mul ydiff ydiff mul add sqrt def
		/angle ydiff xdiff atan def
		gsave
		linewd setlinewidth
		xstart ystart translate
		angle rotate
		gsave
		1 ptr eq {
			newpath
			0 0 moveto
			ptrolen ptrwd lineto
			ptrolen linewd lineto
			veclen linewd add linewd lineto
			veclen linewd add linewd neg lineto
			ptrolen linewd neg lineto
			ptrolen ptrwd neg lineto
			closepath
			clip
		} if
		2 ptr eq {
			newpath
			linewd neg linewd moveto
			veclen ptrolen sub linewd lineto
			veclen ptrolen sub ptrwd lineto
			veclen 0 lineto
			veclen ptrolen sub ptrwd neg lineto
			veclen ptrolen sub linewd neg lineto
			linewd neg linewd neg lineto
			closepath
			clip
		} if
		3 ptr eq {
			newpath
			0 0 moveto
			ptrolen ptrwd lineto
			ptrolen linewd lineto
			veclen ptrolen sub linewd lineto
			veclen ptrolen sub ptrwd lineto
			veclen 0 lineto
			veclen ptrolen sub ptrwd neg lineto
			veclen ptrolen sub linewd neg lineto
			ptrolen linewd neg lineto
			ptrolen ptrwd neg lineto
			closepath
			clip
		} if
		newpath
		0 0 moveto
		veclen 0 lineto
		stroke
		grestore
		1 ptr eq {
			0 0 0 Pointer
		} if
		2 ptr eq {
			180 veclen 0 Pointer
		} if
		3 ptr eq {
			0 0 0 Pointer
			180 veclen 0 Pointer
		} if
		grestore
	} def

	end

