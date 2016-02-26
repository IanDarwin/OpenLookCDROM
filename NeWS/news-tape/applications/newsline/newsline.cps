	%
	% This file is a product of Sun Microsystems, Inc. and is provided for
	% unrestricted use provided that this legend is included on all tape
	% media and as a part of the software program in whole or part.
	% Users may copy, modify or distribute this file at will.
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
	%
#define PAGE_TAG 0 
#define EXIT_TAG 1
#define FILE_TAG 2
cdef ps_initialize(string pwd)
	/pagetag  { PAGE_TAG tagprint } def
	/exittag  { EXIT_TAG tagprint } def
	/filetag  { FILE_TAG tagprint } def
	/FG 0 0 0 rgbcolor def
	/BG 1 1 1 rgbcolor def
	/errordict 10 dict def
	errordict /rangecheck { stop } put
	systemdict /settransfer /pop load def
	/settransfer {pop} def 
	/currenttransfer { { } } def
	/currentscreen { 0 0 { } } def
	/setscreen { pop pop pop } def
	/definefont { exch dup type /keywordtype ne
		{ (                                    ) cvs cvn } if
		exch definefont } def
	/StandardEncoding magic:AdobeSequence def
	magic:fontdict /Encoding StandardEncoding put
    /PGC { } def
    /privudict 200 dict def
    /@Dicts 0 array def
    [1 0 0 1 0 0] setmatrix
	% (Load file from %\n) [ pwd ] dbgprintf
	/PicWindow where
		{ pop }
		{ systemdict begin pwd (/pw.ps) append LoadFile pop end }
	ifelse
	/MenuBar where
		{ pop }
		{ systemdict begin pwd (/menubar.ps) append LoadFile pop end }
	ifelse
	/NewsLineWin where
		{ pop }
		{ systemdict begin pwd (/newslinewin.ps) append LoadFile pop end }
	ifelse
	pwd (/newsline.ps) append LoadFile pop
cdef ps_setminmax(mn, mx)
	mn mx SetMinMax
cdef ps_ditroff_fix()
    /p { initmatrix xi } def
cdef ps_nobox()
cdef ps_rgbcolor(r, g, b) r 256 div g 256 div b 256 div rgbcolor
cdef ps_hsbcolor(h, s, b) h 256 div s 256 div b 256 div hsbcolor
cdef ps_defFG() /FG exch def
cdef ps_defBG() /BG exch def
cdef ps_redefine_colors()
	/setgray { 
		dup 0 eq
		 { pop FG setcolor } 
		 { dup 1 eq { pop BG setcolor } { setgray } ifelse } 
		ifelse
	} def

cdef ps_setupwindow(string name)

cdef PS_startpage()
	/tmpfile (/tmp/psview.out) (w) file def
	array astore 
    {
		currentfile tmpfile cat
	} forall
cdef ps_endpage()
	tmpfile closefile

C:	#define ps_startpage(bytes) { int c = bytes; int i;\
C:	for(i=0; i<(c/10000); i++) ps_int(10000); \
C:	if(c % 10000) { ps_int(c % 10000); i++; }\
C:	ps_int(i); PS_startpage(); }

cdef ps_startprolog(bytes)
	/tmpfile (/tmp/prolog.out) (w) file def
    bytes currentfile tmpfile cat
cdef ps_endprolog()
	tmpfile closefile
	/@DictHeight countdictstack def
	privudict begin
	(/tmp/prolog.out) run
    [ { countdictstack @DictHeight le { exit } if
	currentdict end } loop ]
    /@Dicts exch def

cdef ps_int(i) i
cdef ps_damageall() ps_paint

cdef get_exit() => EXIT_TAG
cdef get_page_selection(selection) => PAGE_TAG(selection)
cdef get_newfile(string fname, string name) => FILE_TAG(fname, name)

