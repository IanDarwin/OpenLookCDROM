# Makefile for MSC - if you don't have NDmake, use this one,
# but don't expect to be happy.
# And don't expect to do anything but making the executables, either.

OBJS=	blk.obj cmd1.obj cmd2.obj curses.obj cut.obj ex.obj input.obj \
	main.obj misc.obj modify.obj move1.obj move2.obj move3.obj move4.obj \
	move5.obj opts.obj recycle.obj redraw.obj regexp.obj regsub.obj \
	system.obj tio.obj tmp.obj vars.obj vcmd.obj vi.obj \
	pc.obj sysdos.obj tinytcap.obj

CC=	cl
	
blk.obj:	blk.c
	$(CC) -AL -c blk.c

cmd1.obj:	cmd1.c
	$(CC) -AL -c cmd1.c

cmd2.obj:	cmd2.c
	$(CC) -AL -c cmd2.c

curses.obj:	curses.c
	$(CC) -AL -c curses.c

cut.obj:	cut.c
	$(CC) -AL -c cut.c

ex.obj:		ex.c
	$(CC) -AL -c ex.c

input.obj:	input.c
	$(CC) -AL -c input.c

main.obj:	main.c
	$(CC) -AL -c main.c

misc.obj:	misc.c
	$(CC) -AL -c misc.c

modify.obj:	modify.c
	$(CC) -AL -c modify.c

move1.obj:	move1.c
	$(CC) -AL -c move1.c

move2.obj:	move2.c
	$(CC) -AL -c move2.c

move3.obj:	move3.c
	$(CC) -AL -c move3.c

move4.obj:	move4.c
	$(CC) -AL -c move4.c

move5.obj:	move5.c
	$(CC) -AL -c move5.c

opts.obj:	opts.c
	$(CC) -AL -c opts.c

recycle.obj:	recycle.c
	$(CC) -AL -c recycle.c

redraw.obj:	redraw.c
	$(CC) -AL -c redraw.c

regexp.obj:	regexp.c
	$(CC) -AL -c regexp.c

regsub.obj:	regsub.c
	$(CC) -AL -c regsub.c

system.obj:	system.c
	$(CC) -AL -c system.c

tio.obj:	tio.c
	$(CC) -AL -c tio.c

tmp.obj:	tmp.c
	$(CC) -AL -c tmp.c

vars.obj:	vars.c
	$(CC) -AL -c vars.c

vcmd.obj:	vcmd.c
	$(CC) -AL -c vcmd.c

vi.obj:		vi.c
	$(CC) -AL -c vi.c

pc.obj:		pc.c
	$(CC) -AL -c pc.c

sysdos.obj:	sysdos.c
	$(CC) -AL -c sysdos.c

tinytcap.obj:	tinytcap.c
	$(CC) -AL -c tinytcap.c

elvis.exe: $(OBJS)
	link @elvis.lnk

ctags.exe: ctags.c wildcard.c
	$(CC) -AL ctags.c -o ctags.exe

ref.exe: ref.c
	$(CC) -AL ref.c -o ref.exe

virec.exe: virec.c wildcard.c
	$(CC) -AL virec.c -o virec.exe

wildcard.exe: wildcard.c
	$(CC) -AL wildcard.c -o wildcard.exe
