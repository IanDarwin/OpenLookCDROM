#if ($SHELL == "/bin/csh") then
#	if (-e  /usr/contributed/bin/tcsh) then
#		echo "(tcsh)"
#		setenv SHELL "/usr/contributed/bin/tcsh"
#		exec $SHELL -l
#	else
#		echo "(csh)"
#	endif
#endif
clear
umask 022
unset ignoreeof

# user actions on login

# define standard search paths for programs, ATK extensions, and help
source /usr/itc/tools/lib/global.paths

#if (`sys` == rs_aix31) then
#	set path = ($path[1-4] /usr/bin/X11 $path[5-9])
#endif

#  define printer you ordinaily use
setenv PRINTER cherry
setenv CLASSPATH	.:$CLASSPATH
setenv EPATH		$HOME/maclib:/usr/local/lib/emacs/maclib
setenv MANPATH		/usr/itc/tools/man:$ANDREWDIR/man:/usr/itc/projects/man:/usr/local/man:/usr/man:/usr/contributed/man
setenv TEMPLATEPATH ~/lib/tpls:/usr/itc/tools/lib/tpls:/usr/itc/released/lib/tpls:/usr/itc/projects/lib/tpls:/usr/local/lib/tpls
setenv CONSOLELIB $HOME/lib/consoles
setenv NAME "AquaMan"
setenv NETHACKOPTIONS "\!verbose,restonspace,time,dogname:Flounder,catname:Guppy,\!tombstone,fruit:Squid"

# Try to determine if we are logged onto the console
set TTY = `tty`
if ($TTY == /dev/console		\
	|| $TTY == /dev/hft/0		\
	|| $TTY == /dev/ttyaed		\
	|| $TTY == /dev/ttyapa16	\
	|| $TTY == /dev/ttyap16		\
	|| $TTY == /dev/tty8514		\
	|| $TTY == /dev/ttymono) then
	set consolelogin
endif
unset TTY

if ($?consolelogin) then
	/bin/fs mariner off >& /dev/null
	if (`sys -c` == "rt" && -x /usr/ibm/pf) then
		/usr/ibm/pf ~/.modkeys
	else if (`sys -c` == "rs") then
		setenv TERM hft
		chhwkbd -r30
		chsound -q
	endif
# alternatives for the following are:  twm, mwm
#	if (`sys -c` == "rt") then
#		setenv X11_WINDOW_MANAGER tvtwm
#	else
		setenv X11_WINDOW_MANAGER twm
#	endif
# alternatives for the following are:  x11, wmc, none, auto, yes
	setenv USE_WINDOW_ENVIRON x11
	setenv USE_PROMPT_ENVIRON yes
# uncomment the following to stay logged in after quitting window manager
 	setenv NO_LOGOUT_ON_QUIT
	source /usr/itc/tools/lib/global.startwindows
else
	if (-x /usr/contributed/bin/consoletype) then
		set term = `/usr/contributed/bin/consoletype`
	else
		set term = unknown
	endif
	loop:
		set noglob; eval `tset -e^H -s \?$TERM`;unset noglob
		if ($TERM == unknown) goto loop
endif
tset -Q
echo ""
set foo = `/bin/ls $HOME/Mailbox | wc -l`
echo "You have -> $foo <- pieces of mail"
unset foo


