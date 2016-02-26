CFLAGS = -g -DCHANGE_CURSOR
OPTIONS = 
LIBS = -lsuntool -lsunwindow -lpixrect -lm
EXE = touchup
OBJ =   ffill.o cms_rainbow.o oval.o circle.o confirmer.o fat.o touchup.o \
	info.o fonts.o brush.o command.o color_palet.o \
	disk_io.o pattern.o drawing.o interface.o magnify.o cursor.o


SRC =   ffill.c cms_rainbow.c oval.c circle.c confirmer.c fat.c touchup.c \
	info.c fonts.c brush.c command.c color_palet.c \
	disk_io.c pattern.c drawing.c magnify.c cursor.c


SH_BASE_DIR = /u13/sr/rayk/touchup
SH_PATTERNS_DIR = pattern_icons
SH_COMMANDS_DIR = command_icons



################################################################
#   change these dirs if you move the files
################################################################
# next line must be hard coded if not using csh; no trailing space!
# remove the comment at the begining of the next line if you are
# not using csh and set the path to the path of the dir that touchup is in.
#PWD=/usr/yourlogin/src/touchup	
BASE_DIR = $(PWD)
PATTERNS_DIR = $(BASE_DIR)/pattern_icons
COMMANDS_DIR = $(BASE_DIR)/command_icons
INFO_PICTURE = $(BASE_DIR)/info.image
MAN_PAGE= $(BASE_DIR)/touchup.cat
###############################################################
###############################################################


$(EXE): .install $(OBJ) touchup.cat
	$(CC) $(CFLAGS) -o $(EXE) $(OBJ) $(OPTIONS) $(LIBS)
	strip touchup 			#final version


interface.o :
	$(CC) $(CFLAGS)  -c  interface.c -I$(PATTERNS_DIR) $(OPTIONS)

pattern.o:
	$(CC) $(CFLAGS)  -c  pattern.c -I$(PATTERNS_DIR) $(OPTIONS)

brush.o:
	$(CC) $(CFLAGS)  -c  brush.c -I$(PATTERNS_DIR) $(OPTIONS)

command.o:
	$(CC) $(CFLAGS)  -c  command.c -I$(COMMANDS_DIR) $(OPTIONS)

cursor.o:
	$(CC) $(CFLAGS)  -c  cursor.c -I$(COMMANDS_DIR) $(OPTIONS)

fat.o:
	$(CC) $(CFLAGS)  -c fat.c $(OPTIONS) -DNO_FASTAN

circle.o:
	$(CC) $(CFLAGS)  -c circle.c $(OPTIONS)

info.o:
	$(CC) $(CFLAGS)  -c info.c $(OPTIONS) -DINFO_IMAGE='"$(INFO_PICTURE)"' -DHELP_FILE='"$(MAN_PAGE)"'

fonts.o:
	$(CC) $(CFLAGS)  -c fonts.c $(OPTIONS) -DINSTALL_DIR='"$(BASE_DIR)"'

touchup.o:
	$(CC) $(CFLAGS)  -c touchup.c $(OPTIONS)


touchup.cat: touchup.man
	nroff -man $(BASE_DIR)/touchup.man | colcrt -  > $(BASE_DIR)/touchup.cat
	

shar: 
	uuencode info.image info.image > temp_image
	shar -n1 -e7 header.h Makefile README touchup.man temp_image \
		     touchup_fonts touchup.icon README_FONTS > touchup.shar1
	shar -n2 -e7 $(SH_COMMANDS_DIR) \
		     $(SH_COMMANDS_DIR)/*.cicon \
		     $(SH_COMMANDS_DIR)/*.cur \
		     $(SH_COMMANDS_DIR)/strip_icon48x48  > touchup.shar2
	shar -n3 -e7 $(SH_PATTERNS_DIR) \
		     $(SH_PATTERNS_DIR)/pattern[1-2]*.icon \
		     $(SH_PATTERNS_DIR)/strip_icon32x32 > touchup.shar3
	shar -n4 -e7 $(SH_PATTERNS_DIR)/pattern[3-9]*.icon \
		     $(SH_PATTERNS_DIR)/brush[1-9].icon > touchup.shar4
	shar -n5 -e7 [a-d]*.c > touchup.shar5
	shar -n6 -e7 [e-l]*.c > touchup.shar6
	shar -n7 -e7 [m-z]*.c > touchup.shar7
	rm -f temp_image

.install:
	@echo -n "The pattern directory is :"
	@echo $(PATTERNS_DIR)
	@echo -n "The command directory is :"
	@echo $(COMMANDS_DIR)
	@echo
	@echo "if these are incorrect change the defintions in the Makefile"
	@echo
	touch info.image
	chmod 777 info.image
	uudecode temp_image
	$(COMMANDS_DIR)/strip_icon48x48 $(COMMANDS_DIR)
	$(PATTERNS_DIR)/strip_icon32x32 $(PATTERNS_DIR)
	@echo
	touch .install

my_backup:
	cp *.sh *.man *.c *.h Makefile backup

lint_it: $(SRC)
	lint $(SRC) $(OPTIONS) $(LIBS) -DINFO_IMAGE='"$(INFO_PICTURE)"' -DHELP_FILE='"$(MAN_PAGE)"' -DNO_FASTAN

clean:; rm -f *.pat *.o

