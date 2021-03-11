#!/usr3/bin/perl
################################################################################
#
# File:         xmenu.pl
# RCS:          $Header: $
# Description:  simple perl interface to the WINTERP-based menu server...
# Author:       Richard Hess, Consilium
# Created:      Sat Oct  5 23:49:59 1991
# Modified:     Sat Oct  5 23:53:10 1991 (Niels Mayer) mayer@hplnpm
# Language:     Perl
# Package:      N/A
# Status:       X11r5 contrib tape release
#
# WINTERP Copyright 1989, 1990, 1991 Hewlett-Packard Company (by Niels Mayer).
# XLISP version 2.1, Copyright (c) 1989, by David Betz.
#
# Permission to use, copy, modify, distribute, and sell this software and its
# documentation for any purpose is hereby granted without fee, provided that
# the above copyright notice appear in all copies and that both that
# copyright notice and this permission notice appear in supporting
# documentation, and that the name of Hewlett-Packard and David Betz not be
# used in advertising or publicity pertaining to distribution of the software
# without specific, written prior permission.  Hewlett-Packard and David Betz
# make no representations about the suitability of this software for any
# purpose. It is provided "as is" without express or implied warranty.
#
################################################################################

# +---------------------------------------------------------------------------
#  WHO:    Richard Hess                    CORP:   Consilium
#  TITLE:  Staff Engineer                  VOICE:  [415] 691-6342
#      [ X-SWAT Team:  Special Projects ]  USNAIL: 640 Clyde Court
#  UUCP:   ...!uunet!cimshop!rhess                 Mountain View, CA 94043
# +---------------------------------------------------------------------------

$heading = "Select Item" ;
$key     =  777 ;
$note    = "::Perl" ;
$output  = "/tmp/.xmu_output" ;
$server  = "/tmp/.xmu_server" ;
$xpos    = "t" ;
$ypos    = "nil" ;

eval "\$$1=\$2" while $ARGV[0] =~ /^(\w+)=(.*)/ && shift;

if (-e $server) { } else { print ":NoServer\n" ; exit ; }

if ($xpos eq "t") {
    $ypos = "nil" ;
}

$nargs = @ARGV ;

if ($nargs > 0) {
    foreach $item (@ARGV) {
	$item = sprintf("\"%s\"", $item) ;
    }

    if ($heading eq "nil") {
	system 'wl' , "-f" , "$server", "\(xmu_menu $key" , "nil \`\(" , @ARGV , "\) \"$note\"\)" ;
    } else {			
	system 'wl' , "-f" , "$server", "\(xmu_menu $key" , "\"$heading\" \`\(" , @ARGV , "\) \"$note\"\)" ;
    }
}

system 'wl' , "-f" , "$server", "\(xmu_popup $key $xpos $ypos \'GNU_cbk\)" ;

open(LOG, "<$output") ;
$etime = time;
($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,$mtime,$ctime,$blksize,$blocks) = stat(LOG) ;

while ($etime > $mtime) {
select(undef,undef,undef,0.5);
($dev,$ino,$mode,$nlink,$uid,$gid,$rdev,$size,$atime,$mtime,$ctime,$blksize,$blocks) = stat(LOG) ;
}

while (-z LOG) {
select(undef,undef,undef,0.5);
}

while (read(LOG,$buf,256)) {
    chop($buf) ;
    print "$buf\n" ;
}
close(LOG) ;

# ----------<eof>
