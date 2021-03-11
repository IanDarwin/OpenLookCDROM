# BSD UNIX Makefile for ELVIS - a clone of `vi`
#
# Use `make` to compile all programs
# Use `make install` to copy the programs to the BIN directory
# Use `make clean` to remove all object files
# Use `make clobber` to remove everything except source & documentation
# Use `make tags` to build new "tags" and "refs" files
# Use `make uue` to produce uuencoded compressed tar archives of the source
# Use `make sh` to produce shar archives of the source

PROGS=	celvis virec # ctags ref
BIN=	/usr/local/bin
OBJS=	blk.o cmd1.o cmd2.o curses.o cut.o ex.o input.o main.o misc.o\
	modify.o move1.o move2.o move3.o move4.o move5.o opts.o recycle.o\
	redraw.o regexp.o regsub.o system.o tio.o tmp.o vars.o vcmd.o vi.o
SRC1=	elvis.man
SRC2=	Elvis.lnk Elvis.mak Elvis.prj Makefile.bsd Makefile.s5 Makefile.st\
	Makefile.pc Makefile.tos blk.c cmd1.c cmd2.c config.h
SRC3=	atari.c curses.c curses.h cut.c elvis.ndx ex.c input.c main.c misc.c
SRC4=	modify.c move1.c move2.c move3.c move4.c move5.c nomagic.c opts.c pc.c\
	recycle.c redraw.c
SRC5=	regexp.c regexp.h regsub.c sysdos.c system.c tinytcap.c tio.c tmp.c
SRC6=	vars.c vcmd.c vi.c vi.h ctags.c ref.c virec.c wildcard.c shell.c
EXTRA=
CFLAGS=	-O
LIBS=	-ltermcap

all: $(PROGS)

celvis: $(OBJS) $(EXTRA)
	$(CC) $(CFLAGS) -o celvis $(OBJS) $(EXTRA) $(LIBS)

ctags: ctags.c
	$(CC) $(CFLAGS) -DSORT ctags.c -o ctags

ref: ref.c
	$(CC) $(CFLAGS) ref.c -o ref

virec: virec.c
	$(CC) $(CFLAGS) virec.c -o virec

# The file cmd1.c is compiled with the extra flag -DDATE="today's date".
# This date is reported when elvis is run and the :version command is given.
# If you have trouble defining the date in this way, then just comment out
# these two lines so cmd1.c is compiled normally.  The :version command
# won't report the compilation date, but that's really no big deal.
cmd1.o: cmd1.c vi.h
	eval $(CC) -c $(CFLAGS) -DDATE=\'\"`date`\"\' cmd1.c

# This just says that a change to nomagic.c is effectively a change to regexp.c
regexp.o: nomagic.c

##############################################################################
install: $(PROGS)
	cp $(PROGS) $(BIN)
	(cd $(BIN); chown bin $(PROGS))
	(cd $(BIN); chmod 755 $(PROGS))

clean:
	rm -f *.o *.uue core

clobber: clean
	rm -f tags refs $(PROGS)

which:
	@echo 'BSD Unix'

tags refs: ctags
	./ctags -r *.[ch]

uue: elvis1.uue elvis2.uue elvis3.uue elvis4.uue elvis5.uue elvis6.uue

elvis1.uue: $(SRC1)
	tar cf elvis1.tar $(SRC1)
	compress -b13 elvis1.tar
	uuencode elvis1.tar.Z <elvis1.tar.Z >elvis1.uue
	rm elvis1.tar*

elvis2.uue: $(SRC2)
	tar cf elvis2.tar $(SRC2)
	compress -b13 elvis2.tar
	uuencode elvis2.tar.Z <elvis2.tar.Z >elvis2.uue
	rm elvis2.tar*

elvis3.uue: $(SRC3)
	tar cf elvis3.tar $(SRC3)
	compress -b13 elvis3.tar
	uuencode elvis3.tar.Z <elvis3.tar.Z >elvis3.uue
	rm elvis3.tar*

elvis4.uue: $(SRC4)
	tar cf elvis4.tar $(SRC4)
	compress -b13 elvis4.tar
	uuencode elvis4.tar.Z <elvis4.tar.Z >elvis4.uue
	rm elvis4.tar*

elvis5.uue: $(SRC5)
	tar cf elvis5.tar $(SRC5)
	compress -b13 elvis5.tar
	uuencode elvis5.tar.Z <elvis5.tar.Z >elvis5.uue
	rm elvis5.tar*

elvis6.uue: $(SRC6)
	tar cf elvis6.tar $(SRC6)
	compress -b13 elvis6.tar
	uuencode elvis6.tar.Z <elvis6.tar.Z >elvis6.uue
	rm elvis6.tar*

sh: elvis1.sh elvis2.sh elvis3.sh elvis4.sh elvis5.sh elvis6.sh

elvis1.sh: $(SRC1)
	shar $(SRC1) >elvis1.sh

elvis2.sh: $(SRC2)
	shar $(SRC2) >elvis2.sh

elvis3.sh: $(SRC3)
	shar $(SRC3) >elvis3.sh

elvis4.sh: $(SRC4)
	shar $(SRC4) >elvis4.sh

elvis5.sh: $(SRC5)
	shar $(SRC5) >elvis5.sh

elvis6.sh: $(SRC6)
	shar $(SRC6) >elvis6.sh

