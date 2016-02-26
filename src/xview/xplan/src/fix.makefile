#
# This script deletes the dependencies from a Makefile that were
# created by makedepend, and are not header files that we wrote.
# In other words things from /usr/include, etc.
#
# Run with:
# 	sed -f fix.makefile < Makefile > New.Makefile
#	mv New.Makefile Makefile
#
# Mark Lacey
# December 13, 1992
#

/# DO NOT DELETE THIS LINE/ b hold
x
/# DO NOT DELETE THIS LINE/ b test
x
b
:test
x
s/\/usr[^ ]*\.h\>[ ]*//g
/^.*:[ ]*$/ d
b
:hold
h
