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
% Modifications to the original Sun Microsystems, Inc. source code
% made by the Grasshopper Group are in the Public Domain.
%
% Extensions to this file by Eric Messick of the Grasshopper Group.
%
% Grasshopper Group
% 212 Clayton St
% San Francisco, CA 94117
%
%
% @(#)$Header: paper.cps,v 1.4 88/09/14 15:16:58 eric Exp $
%
 
#define PAGE_TAG 0 
#define EXIT_TAG 1

cdef PSDefs()
	systemdict /PaperDict known not {
		(paper.ps) LoadFile pop
	} if
	userdict end PaperDict begin begin		% PaperDict begin dictstackexch

cdef ps_initialize(w, h, cx, cy) w h cx cy ps_initialize
cdef ps_ditroff_fix()	/p { initmatrix xi } def
cdef ps_nobox() /DoBox false def
cdef ps_setupwindow(string name) name ps_setupwindow
cdef set_MAX(n) /MAX n def

cdef ps_startprolog()
	end	% @PaperUserDict (should be just systemdict and userdict now)
	/@DictHeight countdictstack def

cdef ps_endprolog()
	[ { countdictstack @DictHeight le { exit } if
		currentdict end } loop ]
	/@Dicts exch def
	@PaperUserDict begin

cdef ps_startpage()
	/StartPage win send
	end	% @PaperUserDict (should be just systemdict and userdict now)
	/@DictHeight countdictstack def
	@Dicts length 1 sub -1 0 { @Dicts exch get begin } for

cdef ps_endpage()
	{ countdictstack @DictHeight le { exit } if end } loop
	@PaperUserDict begin
	/EndPage win send

cdef ps_paintimage() ps_paintimage

cdef popstring(string s) currentcursorlocation s popmsg pop

cdef get_exit() => EXIT_TAG
cdef get_page_selection(selection) => PAGE_TAG(selection)

