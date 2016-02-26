#
#	Makefile for "disktool"
#
# =============================================
#
# In the following section, you should uncomment the lines
# pertaining to your environment and make any path changes
# based on your installation.
#
# Configurable stuff here
#
OPENWINHOME	= /usr/openwin
#
#       SunOS 4.1.x(tested under 4.1.3) uncomment the next line
#
AUX_CFLAGS	= -DSUNOS4
#
#       Solaris 2.x(tested under 2.3) uncomment the next 2 lines
#
#AUX_CFLAGS	= -DSOLARIS2
#OTHERLIBS	= -R${OPENWINHOME}/lib -lnsl
#
#       HP-UX uncomment the next 4 lines
#
#AUX_CFLAGS	= -DHPUX
#XINCLUDES	= -I/usr/include/X11R5
#XLIBS		= /usr/lib/X11R5
#OTHERLIBS	= -lBSD
#
#	Linux uncomment the following
AUX_CFLAGS	= -DLinux
CC		= gcc -g -O
#
# End of Configurable stuff
#
# =============================================
#
OBJS	   	= disktool.o

CFILES		= disktool.c

INCLUDE 	= -I${OPENWINHOME}/include ${XINCLUDES}

CFLAGS		= ${INCLUDE} -c

# Change -O to -g for debugging purposes
LDFLAGS         = -O


XVIEW_LIBS 	= -L${OPENWINHOME}/lib \
		  -lxview -lolgx -lX11

disktool:  ${OBJS} common.h
	${CC} ${LDFLAGS} -o $@ ${OBJS} ${XLIBS} ${XVIEW_LIBS} ${OTHERLIBS}

${OBJS}:   ${CFILES} common.h
	${CC} ${CFLAGS} ${AUX_CFLAGS} $*.c

clean:
	/bin/rm -f core disktool *.o
