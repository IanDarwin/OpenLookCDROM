#!/bin/sh

# this runs a demo of Workman.
# Your CD-ROM must not be "mounted" under UNIX.

# This tries to include the CD-ROM device 
# names oused on most LINUXes - if yours isn't
# listed, edit the script before running, and
# notify Darwin Open Systems so we can fix the script.

if [ -f /dev/cdrom ]; then workman -c /dev/cdrom;
elif [ -f /dev/sr0   ]; then workman -c /dev/sr0;
else echo "Can't find your CD-ROM drive"
fi
