# mcpong@cs.ust.hk last updated Mon May 30 14:41:20 HKT 1994

# change INSTALLDIR to where you would like
#
INSTALLDIR= .

IFLAGS	=
# CFLAGS	= -g -DDEBUG -DDICT_TRIE
CFLAGS	= -O -DBMMFMM
CC	= cc $(IFLAGS) $(CFLAGS)

OBJwordg2b	= wordg2b.o wgsegment.o
OBJaddword	= addword.o

.c.o:
	$(CC) -c $*.c

all:: wordg2b
all:: BuildDICT
all:: addword
all:: AddDICT
all:: checkdup

man: wordg2b.1
	nroff -man wordg2b.1 > wordg2b.man

install: datafiles
	cp wordg2b $(INSTALLDIR)
	@echo Installing the data files used by wordg2b:
	cp DICT.BMM $(INSTALLDIR)
	cp DICT.FMM $(INSTALLDIR)
	cp g2bword.tab $(INSTALLDIR)
	cp g2bchar.tab $(INSTALLDIR)
	cp prefix.scw.bigram.tab $(INSTALLDIR)
	cp suffix.scw.bigram.tab $(INSTALLDIR)
	@echo Installation done.

# create/prepare the datafiles used by the program "wordg2b"
#
datafiles:
	cp g2bword.tab.base g2bword.tab
	@echo
	@echo Next step will take about one minute:
	sh frq2prob.sh gbchar.frq gbword.frq
	@echo DICT.BMM and DICT.FMM generated.
	@echo

# convert GB file to Big5 file based on Word tables
#
wordg2b: $(OBJwordg2b) libDICT.a
	$(CC) -o $@ $(OBJwordg2b) libDICT.a

# build the "compiled" form of the dictionary into DICT form
#               as the file "DICT".
#
BuildDICT:         BuildDICT.o ReverseBuffer.o DICTSave.o $(OBJDICT)
	$(CC) -o $@ BuildDICT.o ReverseBuffer.o DICTSave.o $(OBJDICT)
#	$(CC) -o $@ BuildDICT.o ReverseBuffer.o DICTSave.o DICTPrint.o $(OBJDICT)

##############################################################
OBJDICT		= DICT.o gen_da.o error.o

libDICT.a:    DICTRead.o ReverseBuffer.o $(OBJDICT)
	-rm -f $@
	ar rc  $@ DICTRead.o ReverseBuffer.o $(OBJDICT)
	ranlib $@
##############################################################

##############################################################
# other utilities:
##############################################################

# loop to input GB & B5 word and output to monogram.attr.gb.+ & g2bword.tab.+
#
addword: $(OBJaddword)
	$(CC) -o $@ $(OBJaddword)

# print duplicated word in consecutive lines to output file
#
checkdup:       checkdup.o
	$(CC) -o $@ checkdup.o

# count the no. of hanzi's in the input file
#
hzcount: hzcount.o
	$(CC) -o $@ hzcount.o

# count the no. of different hanzi's in the 2 input files
#
hzdiff: hzdiff.o
	$(CC) -o $@ hzdiff.o

##############################################################
# The following are for enhancing the dictionary.
##############################################################

##############################################################
# AddDICT       adds input file of words to given "compiled" form 
#               of the dictionary in DICT form, and output the 
#               final dictionary as the file "DICT".

AddDICT:        AddDICT.o DICTSave.o libDICT.a
	$(CC) -o $@ AddDICT.o DICTSave.o libDICT.a

##############################################################
# more utilities:
##############################################################
# PrintAmbHz	input "g2bchar.tab";
#		output files "amb.gb" & "amb.b5" containing
#		ambiguous GB & B5 hanzi respectively.

PrintAmbHz: PrintAmbHz.o
	$(CC) -o $@ PrintAmbHz.o

##############################################################
clean:
	rm -f libDICT.a *.o *.in *.out amb.* \
		wordg2b BuildDICT \
		AddDICT addword checkdup \
		PrintAmbHz hzcount hzdiff

