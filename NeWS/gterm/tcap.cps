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
% 210 Clayton St
% San Francisco, CA 94117
%
%
%
% "@(#)tcap.cps 9.5 88/01/19 SMI
% "@(#)$Header: /it/grass/gterm/RCS/tcap.cps,v 2.10 1991/04/23 06:52:36 hugh Grass2 $
%
% Copyright (c) 1985 by Sun Microsystems, Inc.
%/

% note:  all instinces of GTermDict_n and gterm_???_n.psh should
%  have a value of n which matches a protocall level.  The Patchlevel
%  is NOT the same as the protacall.  This lets the c side code get
%  updated without haveing to create a whole new protocall suite.

cdef PSDefs(reload)
	% Incase of NeWS 1.1 protocall libcps.a
	69 42 count 0 gt { dup 42 eq { pop pop } if } if
	systemdict /LoadingGTerm_1 known {
		createevent dup begin
			/Name [/GTimer /GTermLoaded_1] def
		end expressinterest
		createevent dup begin
			/Name /GTimer def
			/TimeStamp currenttime 1 add def
		end sendevent awaitevent pop
	} if pause
	systemdict /GTermDict_1 known not reload 0 ne or {
		systemdict /LoadingGTerm_1 true put
		systemdict /GTermDict_1 undef
		% This lets us add custom toolkit adaptors
		(gtermstart.psh) LoadFile not {
			currentdict /toolkit known not {
				/toolkit (fail) def
			} if
		} if
	} if pause
	systemdict /GTermDict_1 known {
		(y\n) print
	} {
		(n\n) print pause toolkit print (\n) print
	} ifelse

cdef PSGettoolkit()
	toolkit (fail) eq { /toolkit (none) def } if
	toolkit print (\n) print

cdef PSInitCode(string userinit)
	GTermDict_1 /UserCodeLoaded known not {
		(.gtermrc) LoadFile pop
		GTermDict_1 /UserCodeLoaded true put
		systemdict /LoadingGTerm_1 undef
		createevent dup begin
			/Name /GTermLoaded_1 def
			/TimeStamp currenttime def
		end sendevent
	} if pause
        GTermDict_1 begin userdict begin
	GTermInit
	GTermDict_1 userinit known { userinit cvx exec } if

cdef CreateWindow(x, y, fs, col, lines, string framelabel,
	string iconlabel, string initialfont, starticonic, iconx, icony)
		x y fs col lines
		  framelabel iconlabel initialfont starticonic iconx icony
		  createwindow

cdef StartInput() startinput
cdef ReInitialize() resetscale

cdef CursorUp(x,y,cstring c) c x y CU
cdef CursorDown(x,y,cstring c) c x y CD

cdef PaintUnderRev(cstring s)	s UR
cdef PaintUnderNor(cstring s)	s UN
cdef PaintRev(cstring s)	s PR
cdef PaintNor(cstring s)	s PN
cdef MoveTo(x, y)		x y MT
cdef ClearToEndOfLine()		CE
cdef ClearScreen()		CS
cdef CopyLines(yfrom, yby, w, nl) yby w yfrom nl CL

cdef BeginRepair() BRP
cdef EndRepair() ERP
cdef EndRefresh() EOR

cdef SetFrameLabel(string str) str SL
cdef SetSelContents(r, s, l, cstring str)	str s l r setselcontents
cdef RingBell() RB
cdef VisibleBell() VB
cdef SetPageMode(onoff) onoff PM
cdef SetAutoMargins(onoff) onoff AM

cdef StartHiLighting(strokeit) strokeit [
cdef HiLightLine(length) length
cdef EndHiLighting(endcol, startcol, startrow) endcol ] startcol startrow HL
cdef ClearSelectionPath() clearselectionpath
cdef RePaintHiLight() PaintHiLight
cdef StrHiLightLine(cstring s) s
cdef StrEndHiLighting(cstring ends, cstring starts,
	startrow) ends ] starts startrow HL
cdef TakeDownOutline() takedownoutline
cdef StartSavingSelection() startselset
cdef SaveSelectionPiece(cstring s) s extsel
cdef FinishSavingSelection() finishselset
cdef HiLightRect(startcol, startrow, endcol, endrow, strokeit)
	strokeit startcol startrow endcol endrow HiLightRect

cdef ToggleScrollBar(len) len TSB
cdef SetScrollBarValue(len, pos) len pos SSBV

cdef PopMsg(string str)
	gsave framebuffer setcanvas currentcursorlocation str popmsg grestore

cdef PSSideKill() currentprocess killprocess
