#
# big versions of the standard NeWS cursors
# good for demos, large screen displays, etc
# not the best artistically, but they do the job
# mike@etn-wlv.eaton.com
#
#include ../MakeConfig

PSLIB=$(NEWSHOME)/lib/NeWS
F=$(NEWSHOME)/fonts
LIST=bcursor.list
CURSORS=BigBEye.cursor          BigNoUse_m.cursor       BigXCurs.cursor \
	BigBEye_m.cursor        BigPtr.cursor           BigXCurs_m.cursor \
	BigHourG.cursor         BigPtr_m.cursor         BigXHair.cursor \
	BigHourG_m.cursor       BigRtArr.cursor         BigXHair_m.cursor \
	BigNoUse.cursor         BigRtArr_m.cursor

all:

install:   $F/BCursor.ff $F/BCursor.fb $(PSLIB)/installbcursor.ps

clean:
	rm -f *.afb *.ff *.fb

cleanbins:
listsrcs:
	@echo *.cursor

$(PSLIB)/installbcursor.ps: installbcursor.ps
	cp $? $@

$F/BCursor.ff: BCursor.ff
	cp *.ff $(NEWSHOME)/fonts
$F/BCursor.fb: BCursor.fb
	cp *.fb $(NEWSHOME)/fonts

BCursor.ff BCursor.fb: $(CURSORS) $(LIST)
	./makeit bcursor BCursor 32
	