#!/bin/ksh -e

# This is left here to show you exactly how we made 
# "The OPEN LOOK, XView and NeWS Archive CD-ROM".

VERSION='OpenLook-XView-1.1a'		# CHANGE THIS EVERY TIME!!
# Used as the Primary Volume Identifier. AVOID volume names with 
# imbedded slashes as some UNIX versions will be unable to auto-mount them!

# BE SURE YOU CHANGE THIS to where you want the CD image to go.
# If you get this wrong, it's a partition, and you run as root, 
# YOU PROBABLY LOSE A WHOLE DISK PARTITION.

DISK_IMAGE=/dev/rdsk/c0t2d0s4	# Your mileage WILL vary. CHANGE THIS.
DISK_IMAGE=~ian/olcd_image.fs

LOGFILE=$HOME/proj/olcd/log.${VERSION}
if [ -f ${LOGFILE} ]; then
	echo "$0: ${LOGFILE} exists: remove it or change version number"
	exit 1
fi
(
exec > ${LOGFILE}

echo "Start premastering at `date`"

set -x
mkisofs \
    -P 'Darwin Open Systems, R R # 1, Palgrave, ON Canada L0N 1P0.' \
	-A "The OPEN LOOK and XView CD-ROM" \
    -p 'Ian F. Darwin, ian@darwinsys.com' 	\
	-r 			\
	-V ${VERSION}		\
	-o ${DISK_IMAGE} \
	.
) &
tail -f ${LOGFILE} &

# Now tell UNIX that we have a CD-ROM image ready
# volcheck ${DISK_IMAGE}		# Doesn't work on Solaris 2.4
# Mount using vnd...
sudo vnconfig -c -v /dev/vnd0 ${DISK_IMAGE}
sudo mount -t cd9660 /dev/vnd0 /mnt
sudo ls -l /mnt
