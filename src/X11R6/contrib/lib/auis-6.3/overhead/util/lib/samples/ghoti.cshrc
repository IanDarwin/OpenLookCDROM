# Prototypical .cshrc for ITC

source /usr/itc/tools/lib/global.cshrc

# uncomment to display command execution times
set time=(10 "Time: %E (U: %U, S: %S)")

# common improvements in csh defaults
unset ignoreeof
set notify
stty erase ^H
# need to unlimit to be able to run X, but limit core 0 avoids wasting time on space on occasional dumps
if (`sys -c` != vax) then
	unlimit
else
	limit stack 20m
	limit data 20m
endif
limit core 0

# following executed only for interactive shells

if ($?prompt) then
	if ($term == wm) then
		alias cwdcmd	'echo -n "$cwd"'
	else
		alias cwdcmd	'echo ""'
	endif
# prompt and typescript title bar
	alias SETPROMPT 'set prompt = "($HOST)$cwd:t> ";cwdcmd'
#	alias SETPROMPT 'set cwwd=$cwd:h;set prompt = "($HOST)$cwwd:t/$cwd:t> ";cwdcmd'
	SETPROMPT
	alias cd 	'cd    \!* ; SETPROMPT'
	alias pushd 	'pushd \!* ; SETPROMPT'
	alias popd	'popd  \!* ; SETPROMPT'
	alias ls 	'/bin/ls -F'
	set history = 100
	alias ppath \
 echo \$path \| "awk '{for(i=1; i<=NF; i++) printf" '"%3d: %s\n", i, $i}'"'"

# aliases for program development
	alias gdbatk	'gdb /afs/andrew/itc/obj/projects/andrew/atk/apps/runapp'
	alias debug	gdbatk
	alias ANDREW	'pd /afs/andrew/itc/src/projects/andrew/\!*'
	alias OBJ	'pushd /afs/andrew/itc/obj/\!*'
	alias SRC	'pushd /afs/andrew/itc/src/\!*'
	alias RCS	'pd /afs/andrew/itc/rcs/\!*'
	alias obj	'pd /afs/andrew/usr21/ghoti/obj/\!*'
	alias src	'pd /afs/andrew/usr21/ghoti/src/\!*'

	alias pd	pushd

# aliases for printing
if (`sys -c` == "rs") then
	alias c2print	'enscript -p- -2Gr \!* | lpr'
	alias lc2print	'enscript -p- -2Gr \!* | lpr -Plp0'
	alias pprint	'enscript -p- -G -fCourier8 \!* | lpr'
	alias lpprint	'enscript -p- -G -fCourier8 \!* | lpr -Plp0'
else
	alias c2print 	'enscript -2Gr -P$PRINTER \!*'
	alias pprint	'enscript -G -fCourier8 \!*'
endif
	alias mkps 	'ezprint -t $1.ez | troff -Tpostscript | psdit >$1.PS'
	alias prman 	'/usr/local/bin/troff  -man -Tpsc \!* | print -Tdvi &'

# aliases for common listfile cases
	alias lf 	'/bin/ls -F'
	alias ll 	'/bin/ls -l'
	alias lsf	'/bin/ls'
	alias w 	'look \!* /etc/passwd'
	alias h		'history 30'
	alias sc 	'source ~/\!*'
	alias leapto 	'leapto -t'
	alias lleapto	'/usr/local/bin/leapto'
	alias cl	'set echo;rm *.CKP *.BAK .*.CKP .*.BAK *~ .*~;unset echo'
	alias shit	'rm core'

	if (-e /usr/contributed/bin/eggrep) then
	  alias find 	'eggrep -n \!* *.[chH] *.ch'
	else
	  alias find 	'egrep -n \!* *.[chH] *.ch'
	endif

	alias ckk	'(cd ~/Mailbox;egrep "From:|Subject:" *);cd .'
	alias msgs	'messagesn -w'
	alias MSGS	'messagess -w'
	alias k9 	'kill -9 \!*'
	alias mank	'/usr/ucb/man -k'
	alias fscheck	'fs checkservers'
	alias xdraw 	'idraw -geometry 1000x700+0+0 \!* &'
	alias ckbk	'table ~/.private/7-12-91.table'
	alias bigterm	'xterm -fn -adobe-courier-medium-r-normal--20-140-100-100-m-110-iso8859-1 -rv -geometry 80x24+5+5 &'
	alias xtx 	'xterm -geometry 100x40+5+5 -rv &'

	alias xmj 	'/afs/andrew/usr0/games/bin/xmahjongg -r &'
	alias xmjj	'/afs/andrew/usr21/msp/bin/xmahjongg -r \!* &'
	alias spider	'/afs/andrew/usr0/games/bin/spider -geometry 1000x700+0+0 &'
	alias vega	'/afs/andrew/usr0/vega/bin/sun/vega'
	alias hack 	'(/afs/andrew/usr0/games/bin/nethack \!* -u'\$NAME' )'
	alias hackscore	'~games/bin/nethack -s'
	alias binopts	'/afs/andrew/itc/@sys/obj/itc/ams/messages/binopts \!*'
	alias bit	'bitmap -geometry =850x800+0+0 \!*'
	alias dosdir	'dosdir -v'
	alias fishies	'nice -20 xfish &'
	alias web 	'echo 6403'
	alias dwm 	'echo "681-5367"'
	alias aaron	'echo 5032'
	alias XX	'xset s 1800;xset -c;xsetroot -solid #179de5'
	alias ev	'eval `resize`'
endif




