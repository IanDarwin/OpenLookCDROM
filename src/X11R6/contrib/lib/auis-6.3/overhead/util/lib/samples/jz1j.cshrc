# Switch to tcsh if it 'taint running already

if ($SHELL == "/bin/csh" && -e /usr/contributed/bin/tcsh && ($?TERM)) then
	echo "Switching to a REAL c-shell."
	setenv SHELL "/usr/contributed/bin/tcsh"
	exec $SHELL -l $*
endif
set SYST=`sys -c`

set filec

source /afs/andrew/usr3/jz1j/.common/aliases

set prompt = "JimZ-> "
set history = 50
limit coredumpsize 0
