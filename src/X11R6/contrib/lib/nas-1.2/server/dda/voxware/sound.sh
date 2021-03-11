# $NCDId: @(#)sound.sh,v 1.1 1994/04/27 23:48:56 greg Exp $
#
#
AUDIOSERVER=:0 export AUDIOSERVER
PIDFILE=/usr/local/lib/aupid
AU=/usr/local/bin/au
AUPLAY=/usr/local/bin/auplay
UPSOUND=/home/incoming/Sounds/tada.wav
DOWNSOUND=/home/incoming/Sounds/mind.wav
start ()
{
	echo "Starting sound server..."
	sleep 2
	$AU -aa &
	AUPID=$!
	echo $AUPID > $PIDFILE
	#
	#	Whip it into the Real Time class - doesn't hurt us & gives it
	#	CPU when it does need it.
	#
	if [ -x /usr/bin/priocntl ]
	then
		priocntl -c RT -s -i pid $AUPID
	fi
	sleep 2
	$AUPLAY $UPSOUND &
	sleep 1
}

stop()
{
	$AUPLAY $DOWNSOUND
	sleep 3
	kill -15 `cat $PIDFILE`
}

case $1 in
start)
	start;;
stop)
	stop;;
esac

