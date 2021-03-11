# set up shell variables

if ($path[1] != ~/bin) then
  if (-d /usr/itc) then
	  set itcdir = /usr/itc
  else
	  if (-d /afs/andrew.cmu.edu/itc/@sys/destdir) then
		  set itcdir = /afs/andrew.cmu.edu/itc/@sys/destdir
	  endif
  endif

  if ($?itcdir) then
	  setenv ITCDIR $itcdir
	  set andrewdir = $ITCDIR/released
  else
	  if (-d /usr/amos) then
		  set andrewdir = /usr/amos
	  else
		  if (-d /usr/andy) then
			  set andrewdir = /usr/andy
		  else
			  if (-d /usr/andrew) then
				  set andrewdir = /usr/andrew
			  endif
		  endif
	  endif
  endif

  if ($?andrewdir) then
	  setenv ANDREWDIR $andrewdir
  endif

  setenv XLIBDIR /usr/local/lib/X11
  setenv XWM twm

  set tpath = ~/bin
  if ($?itcdir) set tpath = ($tpath $ITCDIR/tools/bin)
  if ($?andrewdir) set tpath = ($tpath $ANDREWDIR/bin)
  if ($?itcdir) set tpath = ($tpath $ITCDIR/projects/bin)
  set tpath = ($tpath /usr/contributed/bin)
  set tpath = ($tpath /usr/contributed/lib/pbm)
  set tpath = ($tpath /usr/local/bin)
  set tpath = ($tpath /usr/ucb)
  set tpath = ($tpath /bin)
  set tpath = ($tpath /usr/bin)
  set tpath = ($tpath /afs/andrew.cmu.edu/usr0/cl0x/bin)
  set tpath = ($tpath /afs/andrew.cmu.edu/usr0/games/bin)

  set path = ($tpath)
  unset tpath

  setenv CLASSPATH ${HOME}/dlib/atk
  if ($?andrewdir) setenv CLASSPATH ${CLASSPATH}:${ANDREWDIR}/dlib/atk
  setenv CLASSPATH ${CLASSPATH}:${ITCDIR}/projects/dlib/atk

  setenv ELKLDARGS '-L/usr/local/lib -L/usr/local/lib/motif -L/usr/contributed/lib'
  setenv ELKTMPDIR ~/tmp
  setenv ENSCRIPT '-2Gr -p -'
  setenv HOST `hostname`
  setenv IRCSERVER irc.andrew.cmu.edu

  setenv MANPATH ~/man
  if ($?itcdir) setenv MANPATH ${MANPATH}:${ITCDIR}/tools/man
  if ($?andrewdir) setenv MANPATH ${MANPATH}:${ANDREWDIR}/man
  if ($?itcdir) setenv MANPATH ${MANPATH}:${ITCDIR}/projects/man
  setenv MANPATH ${MANPATH}:/usr/local/man
  setenv MANPATH ${MANPATH}:/usr/man
  setenv MANPATH ${MANPATH}:/usr/contributed/man
  setenv MANPATH ${MANPATH}:/afs/andrew.cmu.edu/usr0/cl0x/man
  setenv MANPATH ${MANPATH}:/afs/andrew.cmu.edu/usr0/games/man

  setenv MONTH AIO
  setenv NETHACKOPTIONS "name:Dorq,time,nopickup,dogname:Bleep,catname:Bloop"
  setenv QUIPDIR ~/common/lib/quips
  setenv RMEXTS "Z protect"

  setenv SYS `sys`

  setenv TEMPLATEPATH ${HOME}/lib/tpls
  if ($?andrewdir) setenv TEMPLATEPATH ${TEMPLATEPATH}:${ANDREWDIR}/lib/tpls
  setenv TEMPLATEPATH ${TEMPLATEPATH}:/usr/contributed/lib/templates

  setenv TFLC 'loadcpu vm vice venus mail outgoing procsother:1 time'

  alias Daemons 'monthd -i10 >& /dev/console & daemon -I30 -c"month -B7 > /afs/andrew/usr/bobg/private/this-week" &'

  alias Home 'set term = h19; stty rows 24; stty erase "^?"; tset; tflc $TFLC; month -B3'

  alias Logout 'touch ~/.tflcexit && clear && quip && quip && quip && logout'

  alias Runx 'setenv XAPPLRESDIR $XLIBDIR/app-defaults; xinit -- :0 -s 1800'

  unset itcdir
  unset andrewdir
endif

set history = 100
set ignoreeof
set noclobber
set tty = `tty`

# set up aliases

alias ckfs fs checkservers
alias ckm 'echo -n Items of new mail:\ ; ls ~/Mailbox | wc -l'
alias ckom 'echo -n Items of undelivered mail:\ ; ls ~/.Outgoing/QF* |& grep QF | wc -l'
alias compress compress -v
alias cwd 'echo $cwd'
alias emacs gnu-emacs
alias l ls -l
alias lf ls -F
alias rot13 tr A-Za-z N-ZA-Mn-za-m
alias screen	screen -e
alias xpostit xpostit -geometry 85x24+357+742

# set up fancy prompt and typescript-title-bar hacks

if ($?prompt) then
	if ($?ANDREWDIR) then
		if ("$ANDREWDIR" != /usr/itc/released) then
			set pandrewdir = "[${ANDREWDIR}] "
#			set pandrewdir = "[$pandrewdir:t] "
		else
			set pandrewdir
		endif
	else
		set pandrewdir
	endif

	if ("$HOST" != ephrata.andrew.cmu.edu) then
		set phost = `echo $HOST | awk -F. '{print $1}'`\ 
	else
		set phost
	endif

	if ("$SYS" != rt_aos4) then
		set psys = "($SYS) "
	else
		set psys
	endif

	set tscripthack
	if ($?term) then
		if ("$term" == wm) then
			set tscripthack = '; echo -n "$cwd"'
		else
			if ("$term" == screen) then
				set term = vt100
				if ("$tty" == /dev/ttyapa8) stty rows 31
			endif

			if ( ! ($term == "" || $term == dialup || \
				$term == wm || $term == emacs || \
				$term == network)) tset

		endif
	endif

	alias cd 'chdir \!*'$tscripthack'; set prompt = "${pandrewdir}${phost}${psys}`set cdpath = $cwd:h; chdir $cwd:t`! % "'
	alias pushd 'pushd \!*'$tscripthack'; set prompt = "${pandrewdir}${phost}${psys}`set cdpath = $cwd:h; chdir $cwd:t`! % "'
	alias popd 'popd \!*'$tscripthack'; set prompt = "${pandrewdir}${phost}${psys}`set cdpath = $cwd:h; chdir $cwd:t`! % "'
	cd .

	unset tscripthack
endif
