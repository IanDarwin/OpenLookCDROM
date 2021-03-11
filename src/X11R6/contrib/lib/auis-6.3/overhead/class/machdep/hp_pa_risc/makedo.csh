#!/bin/csh -f
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


# Script to convert normal object files into a dynamically loadable module.

if (! $?ANDREWDIR) setenv ANDREWDIR /usr/andrew
if ($#argv == 0) then
    echo "usage: makedo [-o outfile] [-b bindir] [-d libdir] [-e entrypoint] [-g] files..."
    echo "  -b overrides /usr/andrew/bin for finding dofix, doindex"
    echo "  -d overrides ${ANDREWDIR}/lib for finding libcx.a"
    echo "  -e overrides the default entry point"
    echo "  -g causes .dog file to be generated for debugger use"
    exit 1
endif
set filelist
set bindir="${ANDREWDIR}/bin"
set libdir="${ANDREWDIR}/lib"
foreach file ($*)
    if ($?outcoming) then
        set outfile=$file
	unset outcoming
	continue
    endif
    if ($?bincoming) then
        set bindir=$file
	unset bincoming
	continue
    endif
    if ($?libcoming) then
	set libdir=$file
	unset libcoming
	continue
    endif
    if ($?entrypointcoming) then
              set entrypoint=$file
              unset entrypointcoming
              continue
    endif
    switch ($file)
    case -o:
	set outcoming
	breaksw
    case -b:
	set bincoming
	breaksw
    case -d:
	set libcoming
	breaksw
    case -e:
              set entrypointcoming
	breaksw
    case -g:
	set gflag
	breaksw
    default:
	if (! $?outfile) set outfile=$file
        set filelist=($filelist $file)
    endsw
end
if ($?outcoming) then
    echo "makedo: missing argument to -o switch."
    exit 1
endif
if ($?bincoming) then
    echo "makedo: missing argument to -b switch."
    exit 1
endif
if ($?libcoming) then
    echo "makedo: missing argument to -d switch."
    exit 1
endif
if ($?entrypointcoming) then
    echo "makedo: missing argument to -e switch."
    exit 1
endif
if (! $?filelist) then
    echo "makedo: No object modules given."
    exit 1
endif
if (! $?entrypoint) then
    set entrypoint=_${outfile:r}__GetClassInfo
endif
ld -b -E -o ${outfile:r}.do $filelist -e $entrypoint

set retcode=$status
exit($retcode)
