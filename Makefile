#!/bin/ksh -ex

# This is left here to show you exactly how we made 
# "The OPEN LOOK, XView and NeWS Archive CD-ROM".

VERSION='OpenLook-XView-1.0e'		# CHANGE THIS EVERY TIME!!
# Used as the Primary Volume Identifier. AVOID volume names with 
# imbedded slashes as some UNIX versions will be unable to auto-mount them!

# BE SURE YOU CHANGE THIS to where you want the CD image to go.
# If you get this wrong, and run as root, YOU LOSE A DISK partition.
RAW_DISK=/dev/rdsk/c0t2d0s4	# Your mileage WILL vary. CHANGE THIS.

# Set this if you want to make a copy on tape.
DISTN_TAPE=/dev/rmt/0

# How many 512-byte blocks to write per tape block:
BLK_FACT=4		# each 4b == 1 2048-byte CD-ROM block
#BLK_FACT=40		# faster, but check with reader first!

set -o noclobber			# ksh feature to preserve logfiles

exec > $HOME/proj/olcd/log.${VERSION} 2>&1

# echo "Start cleanup at `date`"
# ./scripts/fixperms

echo "Start premastering at `date`"

mkisofs \
    -P 'Darwin Open Systems, Box 278, Palgrave, ON Canada L0N 1P0.' \
    -p 'Ian F. Darwin, ian@darwinsys.com' 	\
	-R 			\
	-T 			\
	-V ${VERSION}		\
	.			\
	> ${RAW_DISK}

# Now tell UNIX that we have a CD-ROM image ready
# volcheck ${RAW_DISK}		# Doesn't work on Solaris 2.4

# And copy to tape for CD master on another machine.
echo "Starting tape copy at `date`"
dd bs=${BLK_FACT}b if=${RAW_DISK} of=${DISTN_TAPE}
mt -f ${DISTN_TAPE} offline
echo "Finished tape copy at `date`"
