set history = 100
if (! $?FIRSTTIME) then
	if (-e /usr/itc/tools/lib/global.paths) then
		source /usr/itc/tools/lib/global.paths
		setenv PATH ${PATH}:/afs/andrew/usr0/cl0x/bin:/afs/andrew/usr0/games/bin
	else
		set path = (~/bin /usr/itc/tools/bin /usr/itc/released/bin /usr/itc/projects/bin /usr/local/bin  /usr/ucb /bin /usr/bin /usr/contributed/bin /afs/andrew/usr0/cl0x/bin /afs/andrew/usr0/games/bin) 		
	endif
endif

alias emacs gnu-emacs
alias ls "ls -F"
alias leapto "leapto -t"
alias ci "ci -u "
alias demo "setenv DEMO YES ; wmc"
alias debug "source ~/bin/.amos"
alias weather "xloadimage /afs/cs/user/jfriedl/weather/latest"
