# initializations for csh - login shell only

if (! $?term) then
  set term=unknown
endif

# setenv PRINT -p
setenv PRINTER ash
setenv RMEXTS "c h ch C cpp s f l y mss tex p pas ada sh csh ml d table foils doc help letter"
if (`sys -c` != 'rs') stty crt erase ^H

# since path and term are automatically exported to the environment,
# use set rather than setenv.  (The reverse does not work; eg $term.)

if ($term == 'network' || $term == 'telnet' || $term == "unknown") then
  set term=vt100
  stty crt erase 
  tset
endif
if (-x /usr/ibm/pf && $term == 'ibm6155') then
  /usr/ibm/pf .login.kbd
endif

unset mail
