#!/usr/local/bin/perl -p
#
# perl script to take a player config file and generate one that faces
# the other direction.  Useful for flipping bitmaps to have a left-facing
# character face right, and vice-versa.
#
# unfinished.  Doesn't work.  Sorry I ran out of time.
#
# --FNA 5/24/94
#

if (/^set pathprefix /) {
  $pathprefix = $';
}
if (/^set bmmsuffix /) {
  $suffix = $';
}
$offset = -60;
if (/^load (\w*) (\w*) ([\d-]*) ([\d-]*) ([\d- ]*,) ([\d- ]*,) ([\d- ]*,) ([\d- ]*,)/) {
  # read the mask and parse out the width.  
  # new fields is width - oldvalue.

  $thing = $4 + $offset;
  print "load $1 $2 $3 $thing $'";
  $_ = "";
}

