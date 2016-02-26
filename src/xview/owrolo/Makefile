

#	Makefile for Rolotool V2.0
#
#	@(#)Makefile	2.4 8/14/88

ARCH	= sun4
INCLPATH = 	$(OPENWINHOME)/include 
LIBPATH	   =    $(OPENWINHOME)/lib

PROG =		rolo
BINS =		$(PROG)

OFILES =	main.o panel.o cards.o popup.o openlook.o
HFILES =	defs.h openp.h
HELPFILES =	help.h
ICONS  =	rolo.icon
BUTTONS	=	bup.curs bdown.curs

C1 =		main.c cards.c popup.c
C2 =		panel.c openlook.c
CFILES =	$(C1) $(C2)
MANPAGE =	$(PROG).1
OTHERS =	README Makefile Manifest Notice Patchlevel rolo.info

SOURCES =	$(OTHERS) $(MANPAGE) $(CFILES) $(HFILES) $(HELPFILES) \
	$(ICONS) $(BUTTONS)

LIBS =		-lxview -lX11 -lolgx

CFLAGS =	-g -DSTANDALONE -I$(INCLPATH)
LDFLAGS =	-O 

LINT =		lint
#LNFLAGS =	-abhxuz


$(PROG):	$(OFILES) 
		$(CC) -o $(PROG) $(LDFLAGS) $(OFILES) -L$(LIBPATH) $(LIBS)

lint:
		$(LINT) -i $(LNFLAGS) $(CFILES) $(LIBS) > $(PROG).lint


clean:
		rm -f $(OFILES) core $(PROG) $(PROG).lint

print:
		pr $(HELPFILES) $(HFILES) $(CFILES) | lpr

ctags:
		ctags $(CFILES) $(HFILES)

#		Shar up everything for posting to the known universe
release:
		shar -c $(MANPAGE) $(HFILES) $(C1) $(ICONS) \
			$(POPBUTTONS) > $(PROG).shar.1of2
		shar -c $(OTHERS) $(HELPFILES) $(C2) $(BUTTONS) \
			> $(PROG).shar.2of2

#		Make one shar file of necessaries, barely squeezes into 100K
shar1:
		shar -c Makefile $(MANPAGE) $(HFILES) $(HELPFILES) $(CFILES) \
		$(ICONS) $(BUTTONS) $(POPBUTTONS) > $(PROG).shar



$(OFILES):	$(HFILES)

main.o:		$(ICONS)

panel.o:	$(BUTTONS) $(HELPFILES)

popup.o:	$(POPBUTTONS)

SOURCE-DIR = /home/erp/demo/src/rolo
install-src:
	rm -rf $(SOURCE-DIR)
	mkdir $(SOURCE-DIR)
	tar cf - $(SOURCES) | ( cd $(SOURCE-DIR) ; tar xvf - )

INSTALL-DIR = /home/erp/demo
BIN-DIR = $(INSTALL-DIR)/bin
LIB-DIR = $(INSTALL-DIR)/lib
MAN-DIR = $(INSTALL-DIR)/man
install: $(BINS)
	strip $(BINS)
	cd $(BIN-DIR); rm -f $(BINS)
	tar cf - $(BINS) | ( cd $(BIN-DIR) ; tar xvf - )
	cd $(MAN-DIR); rm -f $(MANPAGE)
	cp $(MANPAGE) $(MAN-DIR); /usr/bin/chmod 444 $(MAN-DIR)/$(MANPAGE)
