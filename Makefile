#!/bin/ksh -ex

# This is left here to show you exactly how we made 
# "The OPEN LOOK, XView and NeWS Archive CD-ROM".

VERSION='OpenLook-XView-1.1a'		# CHANGE THIS EVERY TIME!!
# Used as the Primary Volume Identifier. AVOID volume names with 
# imbedded slashes as some UNIX versions will be unable to auto-mount them!

# BE SURE YOU CHANGE THIS to where you want the CD image to go.
# If you get this wrong, it's a partition, and you run as root, 
# YOU PROBABLY LOSE A WHOLE DISK PARTITION.

DISK_IMAGE=/dev/rdsk/c0t2d0s4	# Your mileage WILL vary. CHANGE THIS.
DISK_IMAGE=~ian/olcd_image

set -o noclobber			# ksh feature to preserve logfiles

exec > $HOME/proj/olcd/log.${VERSION} 2>&1

# echo "Start cleanup at `date`"
# ./scripts/fixperms

echo "Start premastering at `date`"

mkisofs \
    -P 'Darwin Open Systems, R R # 1, Palgrave, ON Canada L0N 1P0.' \
    -p 'Ian F. Darwin, ian@darwinsys.com' 	\
	-R 			\
	-T 			\
	-V ${VERSION}		\
	.			\
	> ${DISK_IMAGE}

# Now tell UNIX that we have a CD-ROM image ready
# volcheck ${DISK_IMAGE}		# Doesn't work on Solaris 2.4
# Mount using vnd...
vnconfig -c -v /dev/vnd0 ${DISK_IMAGE}
