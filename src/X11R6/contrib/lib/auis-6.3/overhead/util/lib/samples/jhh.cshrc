# options and initializations for csh - whether login or secondary

# global initialization

source /afs/andrew/itc/@sys/destdir/tools/lib/global.cshrc
set path=(~/bin/@sys $path)

# common improvements in csh defaults
set ignoreeof
set notify
set history = 100
set time = (10 "Elapsed time %E.")	# print elapsed time if > 10 seconds
set filec				# file completion in csh
set nonomatch			# non-matching * is not an error

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

# prompt and typescript title bar
	if ($term == wm) then
		alias SETPROMPT 'set prompt="$cwd:t> ";echo -n "$cwd"'
	else
		alias SETPROMPT 'set prompt = "($HOST) $cwd:t> "'
	endif
	SETPROMPT
	alias cd 	'cd    \!* ; SETPROMPT'
	alias pushd 	'pushd \!* ; SETPROMPT'
	alias popd	'popd  \!* ; SETPROMPT'

# aliases for program development
	alias gdbatk	'gdb /afs/andrew/itc/obj/projects/andrew/atk/apps/runapp'
	alias OBJ	'pushd /afs/andrew/itc/obj/\!*'
	alias SRC	'pushd /afs/andrew/itc/src/\!*'

# aliases for printing
	alias c2print 	'enscript -2r -P$PRINTER \!*'
	alias mkps 	'ezprint -t $1.ez | troff -Tpostscript | psdit >$1.PS'
	alias prman 	'/usr/local/bin/troff  -man -Tpostscript \!\* \| print -Tdvi \&'

# aliases for common listfile cases
	alias lf 	'/bin/ls -F'
	alias ll 	'/bin/ls -l'

# print path in a single column
	alias ppath echo \$path \| "awk '{for(i=1; i<=NF; i++) printf" '"%3d: %s\n", i, $i}'"'"

# jhh aliases
	alias tele grep -i \<~/lib/names/fones
	alias netaddr grep -i \<~/.AMS_aliases
	alias printf print -F CR10
	alias printl print -F CR9 -l -nh
	alias scan grep -i
	alias listlocks rlog -L -h RCS/\*,v
	alias leapto leapto -t
	alias kermit kermit -p n
	alias wset ps vx
	alias typescript typescript -t $HOST

endif
