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
%
% "@(#)tcap.cps 9.5 88/01/19 SMI
% "@(#)$Header: tcap.cps,v 2.5 88/11/08 19:35:45 eric Update $
%
% Copyright (c) 1985 by Sun Microsystems, Inc.
%/

% note:  all instinces of PSTermDict_n and psterm_n.ps should
%  have a value of n which matches PATCHLEVEL as defined in
%  patchlevel.h.  this mechinism is used to allow a single
%  news_server to bring up psterm windows on multiple machines
%  running different versions of psterm.

cdef PSDefs(reload)
	systemdict /LoadingPSTerm_1 known {
		createevent dup begin
			/Name [/PSTimer /PSTermLoaded_1] def
		end expressinterest
		createevent dup begin
			/Name /PSTimer def
			/TimeStamp currenttime 1 add def
		end sendevent awaitevent pop
	} if pause
	systemdict /PSTermDict_1 known not reload 0 ne or {
		systemdict /LoadingPSTerm_1 true put
		systemdict /PSTermDict_1 undef
		(psterm_1.ps) LoadFile pop
	} if pause
	systemdict /PSTermDict_1 known {
		(y\n) print
	} {
		(n\n) print
	} ifelse

cdef PSInitCode(string userinit)
	PSTermDict_1 /UserCodeLoaded known not {
		(.pstermrc) LoadFile pop
		PSTermDict_1 /UserCodeLoaded true put
		systemdict /LoadingPSTerm_1 undef
		createevent dup begin
			/Name /PSTermLoaded_1 def
			/TimeStamp currenttime def
		end sendevent
	} if pause
	userdict end PSTermDict_1 begin begin		
             % PSTermDict_1 begin dictstackexch
	PSTermInit
	PSTermDict_1 userinit known { userinit cvx exec } if

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
cdef CopyLines(yfrom, yby, w, nl) yby w yfrom nl CL

cdef BeginRepair() BRP
cdef EndRepair() ERP
cdef EndRefresh() EOR

cdef SetFrameLabel(string str) str SL
cdef SetSelContents(r, s, l, cstring str)	str s l r setselcontents
cdef RingBell() VB		% no audible bell as yet
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
