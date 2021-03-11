#!/usr/local/bin/perl -p
#
# quick script to change y offset of a player config file.  Useful
# for changing screen size and saves lots of editing.

$offset = -60;
if (/^load (\w*) (\w*) ([\d-]*) ([\d-]*) /) {
  $thing = $4 + $offset;
  print "load $1 $2 $3 $thing $'";
  $_ = "";
}

