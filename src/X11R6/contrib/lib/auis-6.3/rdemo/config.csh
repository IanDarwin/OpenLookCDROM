#!/bin/csh -f

# Copyright 1992 Carnegie Mellon University.  All rights reserved.
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

# This script contains definitions for all rdemo
# build/run-time variables.  It will produce a file
# suitable for inclusion in a Makefile or Imakefile (setting
# Make variables) if given the -M flag.  It will
# produce a file suitable for inclusion in a C
# program (creating #define's) if given the -C flag.
# It can also be source'd by csh scripts.
# Beware of sourcing from csh scripts which have
# -C or -M in their argv's.
# Switches must be given separately.

######################################################################
# BEGIN USER-CONFIGURABLE SECTION
######################################################################

######################################################################
# Andrewdir is, of course, ANDREWDIR

set andrewdir = /usr/itc/released
setenv ANDREWDIR $andrewdir
######################################################################
# demodir is the root of where programs expect to find demo files

set demodir = /usr/itc/released/rdemo
######################################################################
# demodir_inst is the root of where things should be installed

set demodir_inst = /afs/andrew.cmu.edu/itc/destdir/projects/depot/rdemo/rdemo
######################################################################
# destdir is the same as demodir_inst

set destdir = $demodir_inst
######################################################################
# name of file to source to set up default paths

set pathsfile = /usr/itc/tools/lib/global.paths
######################################################################
# demouser is the username of the user account under which the demo runs

set demouser = rdemo
######################################################################
# this is demouser's home directory

set demouserhome = /afs/andrew.cmu.edu/usr11/rdemo
######################################################################
# these are the subdirectories of demodir

set auditdir =     $demodir/audit
set commentdir =   $demodir/comments
set fontdir =      $demodir/fonts
set homedir =      $demodir/home
set semaphoredir = $demodir/semaphore
set textdir =      $demodir/text
set bindir =       $demodir/bin
set dlibdir =      $demodir/dlib
set miscdir =      $demodir/misc
######################################################################
# these are the subdirectories of demodir_inst

set auditdir_inst =     $demodir_inst/audit
set commentdir_inst =   $demodir_inst/comments
set fontdir_inst =      $demodir_inst/fonts
set homedir_inst =      $demodir_inst/home
set semaphoredir_inst = $demodir_inst/semaphore
set textdir_inst =      $demodir_inst/text
set bindir_inst =       $demodir_inst/bin
set dlibdir_inst =      $demodir_inst/dlib
set miscdir_inst =      $demodir_inst/misc
######################################################################
# these are the hostnames for the demo servers

set demohosts = (atk.itc.cmu.edu akron.andrew.cmu.edu \
                 bangor.andrew.cmu.edu fallscreek.andrew.cmu.edu)
######################################################################
# this is the pathname to the X11r5 font server (fs) program

set fontserver = /usr/local.r5/bin/fs
######################################################################
# this is the hostname for the X11r5 fontserver

set fontservhost = fontserv.itc.cmu.edu
######################################################################
# this is the port number for fontserver connections

set fontservport = 7000
######################################################################
# this is the time limit (in minutes) for a demo session

set demominutes = 60
######################################################################
# this is the root of the Andrew source tree

set andrewsrc = /afs/andrew.cmu.edu/itc/src/projects/andrew
######################################################################
# this is the maximum number of users at once per host

set maxusers = 3
######################################################################
# this is the location of the setuid-root version of getstats

set getstats_suid = /etc/getstats
######################################################################
# these are some auxiliary programs that are required

set trprog = /usr/bin/tr			# tr
set unlogprog = /usr/local/bin/unlog		# AFS unlog
set xsetprog = /usr/local/bin/xset		# xset
set runappprog = $andrewdir/bin/runapp		# ATK runapp
set fsprog = /usr/local/bin/fs			# AFS fs program
set cshprog = /bin/csh				# C shell
set tarprog = /bin/tar				# tar
set makeprog = /bin/make			# make
set setsprog = /usr/contributed/bin/sets	# sets x y z -d y => x z
set mppprog = /usr/local/bin/mpp		# macro preprocessor
set genidprog = /usr/itc/released/etc/ams_genid	# unique id -> stdout
set mkpathprog = /usr/contributed/bin/mkpath	# does mkdir a a/b a/b/c
######################################################################
# these are some miscellaneous files

set welcomefile = $textdir/Welcome
set tourfile = $textdir/Tour
set ezfile = PlayArea
set commentblurbfile = $textdir/.comments
set commentfile = .comments
######################################################################

######################################################################
# END USER-CONFIGURABLE SECTION
######################################################################

######################################################################
# First process command-line arguments.

set myargv = ($argv)

while ($#myargv > 0)
	switch ("$myargv[1]")
		case -C:
			set do_c
			breaksw
		case -M:
			set do_m
			breaksw
	endsw
	shift myargv
end

######################################################################
######################################################################
# Now output C define's if -C was present

if ($?do_c) then
	cat > config.h <<_EOF_
/* DO NOT EDIT -- this file was automatically created from config.csh */

/********************************************************************/
/* Andrewdir is, of course, ANDREWDIR */

#define ANDREWDIR ("$andrewdir")
/********************************************************************/
/* demodir is the root of where programs expect to find demo files */

#define DEMODIR ("$demodir")
/********************************************************************/
/* demodir_inst is the root of where things should be installed */

#define DEMODIR_INST ("$demodir_inst")
/********************************************************************/
/* destdir is the same as demodir_inst */

#define DESTDIR ("$destdir")
/********************************************************************/
/* name of file to source to set up default paths */

#define PATHSFILE ("$pathsfile")
/********************************************************************/
/* demouser is the username of the user account under which the demo runs */

#define DEMOUSER ("$demouser")
/********************************************************************/
/* this is demouser's home directory */

#define DEMOUSERHOME ("$demouserhome")
/********************************************************************/
/* these are the subdirectories of demodir */

#define AUDITDIR ("$auditdir")
#define COMMENTDIR ("$commentdir")
#define FONTDIR ("$fontdir")
#define HOMEDIR ("$homedir")
#define SEMAPHOREDIR ("$semaphoredir")
#define TEXTDIR ("$textdir")
#define BINDIR ("$bindir")
#define DLIBDIR ("$dlibdir")
#define MISCDIR ("$miscdir")
/********************************************************************/
/* these are the subdirectories of demodir_inst */

#define AUDITDIR_INST ("$auditdir_inst")
#define COMMENTDIR_INST ("$commentdir_inst")
#define FONTDIR_INST ("$fontdir_inst")
#define HOMEDIR_INST ("$homedir_inst")
#define SEMAPHOREDIR_INST ("$semaphoredir_inst")
#define TEXTDIR_INST ("$textdir_inst")
#define BINDIR_INST ("$bindir_inst")
#define DLIBDIR_INST ("$dlibdir_inst")
#define MISCDIR_INST ("$miscdir_inst")
/********************************************************************/
/* these are the hostnames for the demo servers */

#define DEMOHOSTS ("$demohosts")
/********************************************************************/
/* this is the pathname to the X11r5 font server (fs) program */

#define FONTSERVER ("$fontserver")
/********************************************************************/
/* this is the hostname for the X11r5 fontserver */

#define FONTSERVHOST ("$fontservhost")
/********************************************************************/
/* this is the port number for fontserver connections */

#define FONTSERVPORT ($fontservport)
/********************************************************************/
/* this is the time limit (in minutes) for a demo session */

#define DEMOMINUTES ($demominutes)
/********************************************************************/
/* this is the root of the Andrew source tree */

#define ANDREWSRC ("$andrewsrc")
/********************************************************************/
/* this is the maximum number of users at once per host */

#define MAXUSERS ($maxusers)
/********************************************************************/
/* this is the location of the setuid-root version of getstats */

#define GETSTATS_SUID ("$getstats_suid")
/********************************************************************/
/* these are some auxiliary programs that are required */

#define TRPROG ("$trprog")
#define UNLOGPROG ("$unlogprog")
#define XSETPROG ("$xsetprog")
#define RUNAPPPROG ("$runappprog")
#define FSPROG ("$fsprog")
#define CSHPROG ("$cshprog")
#define TARPROG ("$tarprog")
#define MAKEPROG ("$makeprog")
#define SETSPROG ("$setsprog")
#define MPPPROG ("$mppprog")
#define GENIDPROG ("$genidprog")
#define MKPATHPROG ("$mkpathprog")
/********************************************************************/
/* these are some miscellaneous files */

#define WELCOMEFILE ("$welcomefile")
#define TOURFILE ("$tourfile")
#define EZFILE ("$ezfile")
#define COMMENTBLURBFILE ("$commentblurbfile")
#define COMMENTFILE ("$commentfile")
/********************************************************************/

_EOF_

endif

######################################################################
######################################################################
# Now output Make variables if -M was present

if ($?do_m) then
	cat > config.make <<_EOF_
# DO NOT EDIT -- this file was automatically created
# from config.csh

######################################################################
# Andrewdir is, of course, ANDREWDIR

ANDREWDIR=$andrewdir
######################################################################
# demodir is the root of where programs expect to find demo files

DEMODIR=$demodir
######################################################################
# demodir_inst is the root of where things should be installed

DEMODIR_INST=$demodir_inst
######################################################################
# destdir is the same as demodir_inst

DESTDIR=$destdir
######################################################################
# name of file to source to set up default paths

PATHSFILE=$pathsfile
######################################################################
# demouser is the username of the user account under which the demo runs

DEMOUSER=$demouser
######################################################################
# this is demouser's home directory

DEMOUSERHOME=$demouserhome
######################################################################
# these are the subdirectories of demodir

AUDITDIR=$auditdir
COMMENTDIR=$commentdir
FONTDIR=$fontdir
HOMEDIR=$homedir
SEMAPHOREDIR=$semaphoredir
TEXTDIR=$textdir
BINDIR=$bindir
DLIBDIR=$dlibdir
MISCDIR=$miscdir
######################################################################
# these are the subdirectories of demodir_inst

AUDITDIR_INST=$auditdir_inst
COMMENTDIR_INST=$commentdir_inst
FONTDIR_INST=$fontdir_inst
HOMEDIR_INST=$homedir_inst
SEMAPHOREDIR_INST=$semaphoredir_inst
TEXTDIR_INST=$textdir_inst
BINDIR_INST=$bindir_inst
DLIBDIR_INST=$dlibdir_inst
MISCDIR_INST=$miscdir_inst
######################################################################
# these are the hostnames for the demo servers

DEMOHOSTS=$demohosts
######################################################################
# this is the pathname to the X11r5 font server (fs) program

FONTSERVER=$fontserver
######################################################################
# this is the hostname for the X11r5 fontserver

FONTSERVHOST=$fontservhost
######################################################################
# this is the port number for fontserver connections

FONTSERVPORT=$fontservport
######################################################################
# this is the time limit (in minutes) for a demo session

DEMOMINUTES=$demominutes
######################################################################
# this is the root of the Andrew source tree

ANDREWSRC=$andrewsrc
######################################################################
# this is the maximum number of users at once per host

MAXUSERS=$maxusers
######################################################################
# this is the location of the setuid-root version of getstats

GETSTATS_SUID=$getstats_suid
######################################################################
# these are some auxiliary programs that are required

TRPROG=$trprog
UNLOGPROG=$unlogprog
XSETPROG=$xsetprog
RUNAPPPROG=$runappprog
FSPROG=$fsprog
CSHPROG=$cshprog
TARPROG=$tarprog
MAKEPROG=$makeprog
SETSPROG=$setsprog
MPPPROG=$mppprog
GENIDPROG=$genidprog
MKPATHPROG=$mkpathprog
######################################################################
# these are some miscellaneous files

WELCOMEFILE=$welcomefile
TOURFILE=$tourfile
EZFILE=$ezfile
COMMENTBLURBFILE=$commentblurbfile
COMMENTFILE=$commentfile
######################################################################

_EOF_
endif
