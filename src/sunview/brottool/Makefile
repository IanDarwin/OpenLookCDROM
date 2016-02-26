#
# Makefile for brottool.
#

PACKAGE = brottool
GLIBS = -lsuntool -lsunwindow -lpixrect
CFLAGS = -O
.PRECIOUS: f_tab_sqrt.o

${PACKAGE}: ${PACKAGE}.o f_tab_sqrt.o
	cc ${CFLAGS} -o ${PACKAGE} ${PACKAGE}.o f_tab_sqrt.o $(GLIBS)

${PACKAGE}.o: brottool.h brottool.icon

