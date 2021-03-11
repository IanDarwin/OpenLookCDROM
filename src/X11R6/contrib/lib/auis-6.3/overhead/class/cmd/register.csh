#!/bin/csh -f

if ($#argv < 4) then 
	echo 'usage: cregister ANDREWDIR class.do \"objs\" \"libs\"' 
	echo "Normally cregister is run automatically in the process of" 
	echo "creating a .do file." 
	exit 1 
endif 
set pwd = `pwd` 
set destdir = $1 
set dobj=$2 
set classkey = ${dobj:r} 
shift
shift
set objs = ()
while ($#argv > 1 && ("$1" != "---"))
	set objs = ($objs $1)
	shift
end
if "$1" == "---" then
	shift
endif
set libs = ( $argv ) 
set nobjs=() 
foreach name ($objs) 
 set nobjs=($nobjs $pwd/$name) 
end 
set nlibs=() 
foreach name ($libs) 
 if ("$name" =~ /* || "$name" =~ -*) then 
	set nlibs=($nlibs $name) 
else 
	set nlibs=($nlibs $pwd/$name) 
endif 
end 
if -w $destdir/build then 
	rm -f $destdir/build/$classkey.{ols,lls,ref} 
	echo $nobjs>$destdir/build/$classkey.ols 
	echo $nlibs>$destdir/build/$classkey.lls 
	nm $nobjs|sed -n -e 's/^.*[^a-zA-Z0-9_]\([a-zA-Z0-9_][a-zA-Z0-9]*_class[rd]ef_\).*$/\1/p' -e 's/^\([a-zA-Z0-9_][a-zA-Z0-9]*_class[rd]ef_\).*$/\1/p'|awk '\
$0 ~ /[_]?[^_]*_class[rd]ef_/ {\
    n=split($0, foo, "_");\
    for(i=1;i<=n;i++) {\
	if(foo[i] =="classdef" || foo[i] == "classref") {\
	    if(i>1) {\
		if(foo[i]=="classdef") {\
		    refs[foo[i-1]]=2;\
		} else {\
		    if (refs[foo[i-1]]!=2) {\
			refs[foo[i-1]]=1;\
		    }\
		}\
	    }\
	}\
    }\
}\
END {\
    for(i in refs)\
	if(i!="" && refs[i]==2) print i;\
    print "===";\
    for (i in refs)\
	if(i!="" && refs[i]==1) print i;\
}'>$destdir/build/$classkey.ref 
endif 
