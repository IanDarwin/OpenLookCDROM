:

#
# script to generate a Makefile from an Imakefile.  Sometimes useful for 
# generating Makefiles for stuff outside the AF sources.
# 

progname=$0
aftop=$1
what=$2

if [ af"$aftop" = x ]; then
    echo "usage:  $progname  afsrctop  [whattomake]"
    exit 1
fi

if [ af"$what" = x ]; then
    what="Makefile"
fi

if [ ! -d $aftop ]; then
    echo "$progname"":  no such directory $aftop"
    exit 1
fi

if [ ! -d $aftop/config ]; then
    echo "$progname"":  no AF configuration files under $aftop"
    exit 1
fi

if [ ! -f Imakefile ]; then
    echo "$progname"":  can't find `pwd`/Imakefile"
    exit 1
fi

if [ -f $what ]; then
	rm -f ${what}.bak
	mv $what ${what}.bak
fi

echo "Making $what from Imakefile"
PATH=$aftop/util/imake:$PATH \
imake -DTOPDIR=$aftop -I$aftop/config $what
