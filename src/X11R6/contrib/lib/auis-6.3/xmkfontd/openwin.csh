#!/bin/csh
## ###################################################################### ##
##         Copyright IBM Corporation 1988,1991 - All Rights Reserved      ##
##        For full copyright information see:'andrew/config/COPYRITE'     ##
## ###################################################################### ##
# $Disclaimer: 
# Permission to use, copy, modify, and distribute this software and its 
# documentation for any purpose is hereby granted without fee, 
# provided that the above copyright notice appear in all copies and that 
# both that copyright notice, this permission notice, and the following 
# disclaimer appear in supporting documentation, and that the names of 
# IBM, Carnegie Mellon University, and other copyright holders, not be 
# used in advertising or publicity pertaining to distribution of the software 
# without specific, written prior permission.
# 
# IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
# DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
# ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
# SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
# BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
# DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
# WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
# ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
# OF THIS SOFTWARE.
#  $

# Usage:  openwin.csh dirname
#
echo "converting fonts in $1 to OpenWindows format"
cd $1
if ($#argv == 2) then
	set builder = $2
else
	if ( $?OPENWINHOME ) then
		set builder = $(OPENWINHOME)/bin/bldfamily
	else
		if ( -e /usr/openwin/bin ) then
			set builder = /usr/openwin/bin/bldfamily
		else
			set builder = bldfamily
		endif
	endif
endif

if ( -e ../X11fonts/fonts.alias ) then
	awk '{printf("/%s /%s _FontDirectorySYN\n",$1,$2)}' <../X11fonts/fonts.alias >Synonyms.list
endif

${builder} -f 200
echo "Done converting to OpenWindows font format."
exit(0)
