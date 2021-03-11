if (-e /usr/local/lib/global.login ) then
	source /usr/local/lib/global.login
endif

echo "Logging in...."

set SYST=`sys -c`
set TTY=`tty`

#now change everything anyway

setenv CLASSPATH .:/afs/andrew.cmu.edu/usr3/jz1j/dlib:/usr/andrew/dlib/atk:/usr/contributed/dlib
setenv PAGER less
setenv MANPAGER $PAGER

set path = ( \
	. \
	/afs/andrew.cmu.edu/usr3/jz1j/private/bin \
	/afs/andrew.cmu.edu/usr3/jz1j/bin \
	/afs/andrew.cmu.edu/usr3/jz1j/scripts \
	/usr/itc/tools/bin \
	/usr/itc/released/bin \
	/usr/itc/projects/bin \
	/usr/local/bin \
	/usr/andrew/bin \
	/usr/ucb \
	/bin \
	/usr/bin \
	/usr/contributed/bin \
	/usr/contributed/etc \
	/usr/ibm \
	/afs/andrew.cmu.edu/usr0/cl0x/bin \
	/usr/bin/X11 \
	/afs/andrew.cmu.edu/usr0/games/bin \
	/afs/andrew.cmu.edu/rt_r3/usr/contributed/bin )

echo "Fetching..."

#make keys work right
#stty dec

	if (-e /usr/ibm/pf && "$TTY" == /dev/console ) then
		/usr/ibm/pf -e 'set backspace=\177'
		setenv MOUSERATE 100
		setenv MOUSERESL 200
	endif

# make me available for ucbtalk
mesg y

setenv MANPATH /usr/man:/usr/local/man:/usr/local/man:/usr/contributed/man:/usr/andrew/doc:/usr/local/help:/afs/andrew.cmu.edu/usr0/cl0x/man:/afs/andrew.cmu.edu/usr0/games/man:/afs/andrew.cmu.edu/usr3/jz1j/doc 

set MACH = `niftylog in`
echo -n "Logged into: "
echo $MACH

setenv USE_WINDOW_ENVIRON x11

setenv X11_WINDOW_MANAGER 'uwm'

setenv EDITOR /usr/local/bin/ez
setenv PRINTER plum

set THING = console

if ("$TTY" ==  /dev/console) then
	set blah = `niftyprompt -anycase -tolower -7 "Startup? [U]wm/[N]iftyterm/[V]t220/[G]eneric/v[T]100 ->" unvgt`
else
	set blah = `niftyprompt -anycase -tolower -7 "Startup? [U]wm/[N]iftyterm/[V]t220/[G]eneric/v[T]100 ->" gvunt`
endif
switch($blah)
    case w:
		echo 'Starting wmc, please wait ...'
		set THING = wmc
		breaksw
	case u:
		setenv X11_WINDOW_MANAGER uwm
		set THING = x11
		breaksw
	case g:
		set THING = console
		breaksw
	case n:
		set THING = niftyterm
        breaksw
	case v:
		set THING = vt220
		setenv TERM vt220
		breaksw
	case t:
		set THING = vt100
		setenv TERM vt100
		breaksw
endsw

switch ($THING)
	case x11:
		/usr/local/bin/xinit
		unset THING
	breaksw
	case wmc:
		/usr/local/bin/wmc
		unset thing
		breaksw
	case console:
		unset thing
		breaksw
	case vt220:
		echo "Emulation set to vt220."
		unset thing
		breaksw
	case vt100:
		echo "Emulation set to vt100."
		unset thing
		breaksw
	case niftyterm:
		echo "Starting niftyterm."
		niftyterm
		unset thing
		breaksw
endsw







