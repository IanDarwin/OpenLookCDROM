# $Header: makefile,v 1.1 86/10/05 15:19:16 design Exp $

#------------------------------------------------------------------------
# makefile for X course programs
#------------------------------------------------------------------------

#COMPILER= cc -Bstatic
COMPILER= gcc
#LINKER= cc -Bstatic
LINKER= ${COMPILER}
DFLAGS= -g 
CFLAGS= -c ${DFLAGS}
# for Suns, use this:
IFLAGS= -I. -I/usr/bs/X11R5/include
# for sequents, use this:
#IFLAGS= -I.
LFLAGS= -L/usr/bs/X11R5/lib
WHEREFLAGS= ${IFLAGS} ${LFLAGS}
LIVEINCLUDES= -I/usr/bs/motif-1.1/include -I/usr/bs/X11R5/include -I. -D_NO_PROTO
WHERE_LIVE = -L/usr/bs/motif-1.1/lib -L.
XLIBS = -lXaw -lXmu -lXt -lX11 -lXext
LIVELIBS = -lXc -lXm
SHAPE= -DSHAPE
BAKUP = .
WORKING = .

#------------------------------------------------------------------------
# programs
#------------------------------------------------------------------------

# To compile Answer Garden

AG ag: AG.o Layout.o Grapher.o Knedit.o Util.o \
Discuss.o NodeServ.o FileServ.o GButton.o FuncObj.o StackObj.o Edit.o EditInfo.o \
EditFile.o QA.o Globals.o CommServ.o AsciiNode.o Outline.o NodeOut.o
	${LINKER} -o AG ${WHEREFLAGS} AG.o Globals.o Layout.o Grapher.o \
	Knedit.o Util.o Discuss.o NodeServ.o GButton.o FuncObj.o StackObj.o \
	Edit.o FileServ.o EditInfo.o EditFile.o QA.o CommServ.o \
	AsciiNode.o Outline.o NodeOut.o \
	${XLIBS}


#
#  To compile a version of Answer Garden with journal nodes 
#	(for me)
#
JAG jag: AG.o Layout.o Grapher.o Knedit.o Util.o \
Discuss.o NodeServ.o FileServ.o GButton.o FuncObj.o StackObj.o Edit.o EditInfo.o \
EditFile.o QA.o Globals.o journal.o CommServ.o cistk.o AsciiNode.o 
	${COMPILER} ${CFLAGS} ${IFLAGS} Globals.c -DJOURNAL
	${COMPILER} -o AG ${WHEREFLAGS} Globals.o AG.o Layout.o Grapher.o \
	Knedit.o Util.o Discuss.o NodeServ.o GButton.o FuncObj.o StackObj.o \
	Edit.o FileServ.o EditInfo.o EditFile.o QA.o journal.o CommServ.o \
	cistk.o AsciiNode.o \
	${XLIBS}
	rm Globals.o

#
#  To compile a version of LiveDoc 
#      This is a demo hack!  Notice the link....
#      The RichText widget requires Motif 1.1 and is written in C++.
#
livedoc LiveDoc: AG.o Layout.o Grapher.o Knedit.o Util.o \
Discuss.o NodeServ.o FileServ.o GButton.o FuncObj.o StackObj.o Edit.o EditInfo.o \
EditFile.o QA.o Globals.o CommServ.o AsciiNode.o Outline.o NodeOut.o \
RichText.o Doc.o
	${COMPILER} ${CFLAGS} ${IFLAGS} Globals.c -DLIVEDOC
	g++ -o LiveDoc ${WHEREFLAGS} ${WHERE_LIVE} Globals.o AG.o Layout.o \
	Grapher.o \
	Knedit.o Util.o Discuss.o NodeServ.o GButton.o FuncObj.o StackObj.o \
	Edit.o FileServ.o EditInfo.o EditFile.o QA.o CommServ.o \
	AsciiNode.o Outline.o NodeOut.o RichText.o  Doc.o \
	${LIVELIBS} ${XLIBS} 


authoring:  mail_stats everybody_here

bakup: 
	cp -r ${BAKUP}/one_back/* ${BAKUP}/two_back
	cp -r ${BAKUP}/current/* ${BAKUP}/one_back
	cp -r * ${BAKUP}/current

AG.h: AGmacros.h
	touch AG.h

NodeServ.o: NodeServ.c AG.h
	${COMPILER} ${CFLAGS} ${IFLAGS} NodeServ.c -DTEMPORARY_NODES

Doc.o: Doc.c AG.h
	${COMPILER} ${CFLAGS} ${LIVEINCLUDES} Doc.c

CommServ.o: CommServ.c AG.h
	${COMPILER} ${CFLAGS} ${IFLAGS} CommServ.c

Main.o: Main.c AG.h
	${COMPILER} ${CFLAGS} ${IFLAGS} Main.c

FileServ.o: FileServ.c AG.h
	${COMPILER} ${CFLAGS} ${IFLAGS} FileServ.c

Discuss.o: Discuss.c AG.h
	${COMPILER} ${CFLAGS} ${IFLAGS} Discuss.c

Util.o: Util.c AG.h
	${COMPILER} ${CFLAGS} ${IFLAGS} Util.c 

Grapher.o: Grapher.c Grapher.h GrapherP.h AGmacros.h
	${COMPILER} ${CFLAGS} ${IFLAGS} Grapher.c

FuncObj.o: FuncObj.c FuncObj.h FuncObjP.h AGmacros.h
	${COMPILER} ${CFLAGS} ${IFLAGS} FuncObj.c

StackObj.o: StackObj.c StackObj.h StackObjP.h AGmacros.h
	${COMPILER} ${CFLAGS} ${IFLAGS} StackObj.c

Layout.o: Layout.c Layout.h LayoutP.h 
	${COMPILER} ${CFLAGS} ${IFLAGS} Layout.c

Knedit.o: Knedit.c Knedit.h KneditP.h AGmacros.h
	${COMPILER} ${CFLAGS} ${IFLAGS} Knedit.c

AG.o: AG.c AG.h 
	${COMPILER} ${CFLAGS} ${IFLAGS} AG.c

Edit.o: Edit.c AG.h Edit.h
	${COMPILER} ${CFLAGS} ${IFLAGS} Edit.c

EditInfo.o: EditInfo.c AG.h Edit.h
	${COMPILER} ${CFLAGS} ${IFLAGS} EditInfo.c

EditFile.o: EditFile.c AG.h Edit.h
	${COMPILER} ${CFLAGS} ${IFLAGS} EditFile.c

GButton.o: GButton.c GButton.h GButtonP.h AGmacros.h
	${COMPILER} ${CFLAGS} ${IFLAGS} ${SHAPE} GButton.c

QA.o: QA.c AG.h
	${COMPILER} ${CFLAGS} ${IFLAGS} QA.c


Globals.o: Globals.c AG.h
	${COMPILER} ${CFLAGS} ${IFLAGS} Globals.c

AsciiNode.o: AsciiNode.c AG.h
	${COMPILER} ${CFLAGS} ${IFLAGS} AsciiNode.c



NodeOut.o: NodeOut.c AG.h
	${COMPILER} ${CFLAGS} ${IFLAGS} NodeOut.c

Outline.o: Outline.c AG.h
	${COMPILER} ${CFLAGS} ${IFLAGS} Outline.c

stubs.o: stubs.c AG.h
	${COMPILER} ${CFLAGS} ${IFLAGS} stubs.c

journal.o: journal.c AG.h
	${COMPILER} ${CFLAGS} ${IFLAGS} journal.c

cistk.o: cistk.c AG.h
	${COMPILER} ${CFLAGS} ${IFLAGS} cistk.c


everybody_here.o: everybody_here.c AG.h
	${COMPILER} ${CFLAGS} ${IFLAGS} everybody_here.c


everybody_here: everybody_here.o NodeServ.o Util.o Globals.o \
stubs.o FileServ.o
	${COMPILER} ${WHEREFLAGS} -o everybody_here \
	 everybody_here.o NodeServ.o Util.o Globals.o FileServ.o stubs.o \
	 ${XLIBS}

mail_stats: mail_stats.c AG.h 
	${COMPILER} ${DFLAGS} ${WHEREFLAGS} -o mail_stats mail_stats.c ${XLIBS}

foobar: foobar.c
	${COMPILER} -o foobar foobar.c  -lXmu -lXt -lX11 -lXext









