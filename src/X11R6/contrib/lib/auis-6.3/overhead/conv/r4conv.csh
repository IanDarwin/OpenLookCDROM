#!/bin/csh -f 
## ###################################################################### ##
##         Copyright IBM Corporation 1988,1991 - All Rights Reserved      ##
##        For full copyright information see:'andrew/config/COPYRITE'     ##
## ###################################################################### ##
# $Disclaimer: 
# Permission to use, copy, modify, and distribute this software and its 
# documentation for any purpose is hereby granted without fee, 
# provided that the above copyright notice appear in all copies and that 
# both that copyright notice, this permission notice, and the following 
# disclaimer appear in supporting documentation, and that the names of 
# IBM, Carnegie Mellon University, and other copyright holders, not be 
# used in advertising or publicity pertaining to distribution of the software 
# without specific, written prior permission.
# 
# IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
# DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
# ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
# SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
# BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
# DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
# WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
# ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
# OF THIS SOFTWARE.
#  $


set PROGNAME = $0
set SEDFILE = /tmp/r3tor4.sed.$$
set SEDFILE2 = /tmp/r3tor4.sed2.$$
set QUIET = 0

if ($#argv > 0 && $1 == "-q") then
    set QUIET = 1
    shift
endif

if ($QUIET == 0) then
echo "R3toR4 -- ATK Source code conversion assistance program, by N. Borenstein"
echo "  "
echo === This program attempts to convert code written for the X11R3 Andrew Toolkit release
echo "    to run on the latest version of Andrew.  The big changes that it deals with are"
echo "    the change from .H files to .ch files, and the change to use only very short, lower-case"
echo "    file names for the objects in the standard ATK release, for better portability."
echo "    "
echo "=== We recommend that you make a backup of your sources before running this"
echo "    conversion script.  We make no guarantee that this script will work completely"
echo "    or even partially, although it does SEEM to help ease the transition."
echo "    "
echo "=== You have ten seconds to hit ^C before the conversion starts..."
sleep 10
endif
set nonomatch

if ($#argv > 0) cd $1
echo === Converting the source tree rooted at `pwd`
echo === Creating temporary sed files in $SEDFILE and $SEDFILE2
cat > $SEDFILE <<!
s/application.ih/app.ih/g
s/arbiterview.ih/arbiterv.ih/g
s/bargraphV.ih/bargrphv.ih/g
s/be1be2app.ih/be1be2a.ih/g
s/butterview.ih/butterv.ih/g
s/buttonV.ih/buttonv.ih/g
s/celview.ih/celv.ih/g
s/chimpview.ih/chimpv.ih/g
s/chlistview.ih/chlistv.ih/g
s/chompview.ih/chompv.ih/g
s/clicklistV.ih/clklistv.ih/g
s/cltextview.ih/cltextv.ih/g
s/completion.ih/complete.ih/g
s/consoleapp.ih/consolea.ih/g
s/controlV.ih/controlv.ih/g
s/ctextview.ih/ctextv.ih/g
s/datacatapp.ih/datacata.ih/g
s/dataobject.ih/dataobj.ih/g
s/describer.ih/describe.ih/g
s/dictionary.ih/dict.ih/g
s/diredview.ih/diredv.ih/g
s/dynsearch.ih/dsearch.ih/g
s/enodeview.ih/enodev.ih/g
s/enterstrV.ih/entrstrv.ih/g
s/entertext.ih/entrtext.ih/g
s/environment.ih/envrment.ih/g
s/eqview.ih/eqv.ih/g
s/etextview.ih/etextv.ih/g
s/ezapp.ih/eza.ih/g
s/ezprintapp.ih/ezprinta.ih/g
s/fadview.ih/fadv.ih/g
s/fourwayV.ih/fourwayv.ih/g
s/framecmds.ih/framecmd.ih/g
s/framemessage.ih/framemsg.ih/g
s/frameview.ih/framev.ih/g
s/getRecV.ih/getrecv.ih/g
s/geview.ih/gev.ih/g
s/gobview.ih/gobv.ih/g
s/graphlook.ih/grlook.ih/g
s/graphview.ih/grv.ih/g
s/helpapp.ih/helpa.ih/g
s/helptextview.ih/helptxtv.ih/g
s/incsearch.ih/isearch.ih/g
s/labelview.ih/labelv.ih/g
s/listprocsapp.ih/lprocsa.ih/g
s/lookzview.ih/lookzv.ih/g
s/lprrulerview.ih/lprrulv.ih/g
s/lsetview.ih/lsetv.ih/g
s/ltextview.ih/ltextv.ih/g
s/menterstrV.ih/mentstrv.ih/g
s/mentertext.ih/menttext.ih/g
s/messagesapp.ih/msgsa.ih/g
s/metextview.ih/metextv.ih/g
s/monthview.ih/monthv.ih/g
s/msghandler.ih/msghndlr.ih/g
s/mtextview.ih/mtextv.ih/g
s/namespace.ih/namespc.ih/g
s/nessproc0x.ih/nsproc0x.ih/g
s/nessproc1x.ih/nsproc1x.ih/g
s/nessproc2x.ih/nsproc2x.ih/g
s/nessproc3x.ih/nsproc3x.ih/g
s/nessproc4x.ih/nsproc4x.ih/g
s/nessview.ih/nessv.ih/g
s/nestedmark.ih/nstdmark.ih/g
s/observable.ih/observe.ih/g
s/oldRF.ih/oldrf.ih/g
s/onoffV.ih/onoffv.ih/g
s/pianoV.ih/pianov.ih/g
s/pixelimage.ih/pixelimg.ih/g
s/plusspace.ih/plusspc.ih/g
s/previewapp.ih/previewa.ih/g
s/proctable.ih/proctbl.ih/g
s/pscriptapp.ih/pscripta.ih/g
s/rasterimage.ih/rastimg.ih/g
s/rasterview.ih/rasterv.ih/g
s/rofftextapp.ih/rofftxta.ih/g
s/sendmessage.ih/sendmsg.ih/g
s/shrubbery.ih/shrubery.ih/g
s/sliderV.ih/sliderv.ih/g
s/sliderstrV.ih/slidstrv.ih/g
s/simpletext.ih/smpltext.ih/g
s/stringV.ih/stringv.ih/g
s/stringtbl.ih/strtbl.ih/g
s/strtblview.ih/strtblv.ih/g
s/stylesheet.ih/stylesht.ih/g
s/termexport.ih/termexp.ih/g
s/textview.ih/textv.ih/g
s/t822view.ih/text822v.ih/g
s/thumbstrV.ih/thmbstrv.ih/g
s/thumbV.ih/thumbv.ih/g
s/termulator.ih/tm.ih/g
s/tmapp.ih/tma.ih/g
s/tmview.ih/tmv.ih/g
s/tree23int.ih/tree23.ih/g
s/typescript.ih/tscript.ih/g
s/tscriptapp.ih/tscripta.ih/g
s/texttroff.ih/txttroff.ih/g
s/updatelist.ih/updlist.ih/g
s/valueview.ih/valuev.ih/g
s/vopconapp.ih/vopcona.ih/g
s/wmcursor.ih/wcursor.ih/g
s/wmfontdesc.ih/wfontd.ih/g
s/wmgraphic.ih/wgraphic.ih/g
s/wmim.ih/wim.ih/g
s/wincelview.ih/wincelv.ih/g
s/windowsystem.ih/winsys.ih/g
s/wmws.ih/wws.ih/g
s/wrapperview.ih/wrapperv.ih/g
s/xfontdesc.ih/xfontd.ih/g
s/zipoarrow.ih/zipoarrw.ih/g
s/zipobject.ih/zipobj.ih/g
s/zipofcapt.ih/zipofcap.ih/g
s/zipoimbed.ih/zipoimbd.ih/g
s/zipopolygon.ih/zipopoly.ih/g
s/ziposymbol.ih/ziposym.ih/g
s/zipstatus.ih/zipstat.ih/g
s/zipview.ih/zipv.ih/g
!
cat > $SEDFILE2 <<!
s/:[ 	]*application[ 	]*{/: application [app] {/g
s/:[ 	]*arbiterview[ 	]*{/: arbiterview [arbiterv] {/g
s/:[ 	]*bargraphV[ 	]*{/: bargraphV [bargrphv] {/g
s/:[ 	]*be1be2app[ 	]*{/: be1be2app [be1be2a] {/g
s/:[ 	]*butterview[ 	]*{/: butterview [butterv] {/g
s/:[ 	]*buttonV[ 	]*{/: buttonV [buttonv] {/g
s/:[ 	]*celview[ 	]*{/: celview [celv] {/g
s/:[ 	]*chimpview[ 	]*{/: chimpview [chimpv] {/g
s/:[ 	]*chlistview[ 	]*{/: chlistview [chlistv] {/g
s/:[ 	]*chompview[ 	]*{/: chompview [chompv] {/g
s/:[ 	]*clicklistV[ 	]*{/: clicklistV [clklistv] {/g
s/:[ 	]*cltextview[ 	]*{/: cltextview [cltextv] {/g
s/:[ 	]*completion[ 	]*{/: completion [complete] {/g
s/:[ 	]*consoleapp[ 	]*{/: consoleapp [consolea] {/g
s/:[ 	]*controlV[ 	]*{/: controlV [controlv] {/g
s/:[ 	]*ctextview[ 	]*{/: ctextview [ctextv] {/g
s/:[ 	]*datacatapp[ 	]*{/: datacatapp [datacata] {/g
s/:[ 	]*dataobject[ 	]*{/: dataobject [dataobj] {/g
s/:[ 	]*describer[ 	]*{/: describer [describe] {/g
s/:[ 	]*dictionary[ 	]*{/: dictionary [dict] {/g
s/:[ 	]*diredview[ 	]*{/: diredview [diredv] {/g
s/:[ 	]*dynsearch[ 	]*{/: dynsearch [dsearch] {/g
s/:[ 	]*enodeview[ 	]*{/: enodeview [enodev] {/g
s/:[ 	]*enterstrV[ 	]*{/: enterstrV [entrstrv] {/g
s/:[ 	]*entertext[ 	]*{/: entertext [entrtext] {/g
s/:[ 	]*environment[ 	]*{/: environment [envrment] {/g
s/:[ 	]*eqview[ 	]*{/: eqview [eqv] {/g
s/:[ 	]*etextview[ 	]*{/: etextview [etextv] {/g
s/:[ 	]*ezapp[ 	]*{/: ezapp [eza] {/g
s/:[ 	]*ezprintapp[ 	]*{/: ezprintapp [ezprinta] {/g
s/:[ 	]*fadview[ 	]*{/: fadview [fadv] {/g
s/:[ 	]*fourwayV[ 	]*{/: fourwayV [fourwayv] {/g
s/:[ 	]*framecmds[ 	]*{/: framecmds [framecmd] {/g
s/:[ 	]*framemessage[ 	]*{/: framemessage [framemsg] {/g
s/:[ 	]*frameview[ 	]*{/: frameview [framev] {/g
s/:[ 	]*getRecV[ 	]*{/: getRecV [getrecv] {/g
s/:[ 	]*geview[ 	]*{/: geview [gev] {/g
s/:[ 	]*gobview[ 	]*{/: gobview [gobv] {/g
s/:[ 	]*graphlook[ 	]*{/: graphlook [grlook] {/g
s/:[ 	]*graphview[ 	]*{/: graphview [grv] {/g
s/:[ 	]*helpapp[ 	]*{/: helpapp [helpa] {/g
s/:[ 	]*helptextview[ 	]*{/: helptextview [helptxtv] {/g
s/:[ 	]*incsearch[ 	]*{/: incsearch [isearch] {/g
s/:[ 	]*labelview[ 	]*{/: labelview [labelv] {/g
s/:[ 	]*listprocsapp[ 	]*{/: listprocsapp [lprocsa] {/g
s/:[ 	]*lookzview[ 	]*{/: lookzview [lookzv] {/g
s/:[ 	]*lprrulerview[ 	]*{/: lprrulerview [lprrulv] {/g
s/:[ 	]*lsetview[ 	]*{/: lsetview [lsetv] {/g
s/:[ 	]*ltextview[ 	]*{/: ltextview [ltextv] {/g
s/:[ 	]*menterstrV[ 	]*{/: menterstrV [mentstrv] {/g
s/:[ 	]*mentertext[ 	]*{/: mentertext [menttext] {/g
s/:[ 	]*messagesapp[ 	]*{/: messagesapp [msgsa] {/g
s/:[ 	]*metextview[ 	]*{/: metextview [metextv] {/g
s/:[ 	]*monthview[ 	]*{/: monthview [monthv] {/g
s/:[ 	]*msghandler[ 	]*{/: msghandler [msghndlr] {/g
s/:[ 	]*mtextview[ 	]*{/: mtextview [mtextv] {/g
s/:[ 	]*namespace[ 	]*{/: namespace [namespc] {/g
s/:[ 	]*nessproc0x[ 	]*{/: nessproc0x [nsproc0x] {/g
s/:[ 	]*nessproc1x[ 	]*{/: nessproc1x [nsproc1x] {/g
s/:[ 	]*nessproc2x[ 	]*{/: nessproc2x [nsproc2x] {/g
s/:[ 	]*nessproc3x[ 	]*{/: nessproc3x [nsproc3x] {/g
s/:[ 	]*nessproc4x[ 	]*{/: nessproc4x [nsproc4x] {/g
s/:[ 	]*nessview[ 	]*{/: nessview [nessv] {/g
s/:[ 	]*nestedmark[ 	]*{/: nestedmark [nstdmark] {/g
s/:[ 	]*observable[ 	]*{/: observable [observe] {/g
s/:[ 	]*oldRF[ 	]*{/: oldRF [oldrf] {/g
s/:[ 	]*onoffV[ 	]*{/: onoffV [onoffv] {/g
s/:[ 	]*pianoV[ 	]*{/: pianoV [pianov] {/g
s/:[ 	]*pixelimage[ 	]*{/: pixelimage [pixelimg] {/g
s/:[ 	]*plusspace[ 	]*{/: plusspace [plusspc] {/g
s/:[ 	]*previewapp[ 	]*{/: previewapp [previewa] {/g
s/:[ 	]*proctable[ 	]*{/: proctable [proctbl] {/g
s/:[ 	]*pscriptapp[ 	]*{/: pscriptapp [pscripta] {/g
s/:[ 	]*rasterimage[ 	]*{/: rasterimage [rastimg] {/g
s/:[ 	]*rasterview[ 	]*{/: rasterview [rasterv] {/g
s/:[ 	]*rofftextapp[ 	]*{/: rofftextapp [rofftxta] {/g
s/:[ 	]*sendmessage[ 	]*{/: sendmessage [sendmsg] {/g
s/:[ 	]*shrubbery[ 	]*{/: shrubbery [shrubery] {/g
s/:[ 	]*sliderV[ 	]*{/: sliderV [sliderv] {/g
s/:[ 	]*sliderstrV[ 	]*{/: sliderstrV [slidstrv] {/g
s/:[ 	]*simpletext[ 	]*{/: simpletext [smpltext] {/g
s/:[ 	]*stringV[ 	]*{/: stringV [stringv] {/g
s/:[ 	]*stringtbl[ 	]*{/: stringtbl [strtbl] {/g
s/:[ 	]*strtblview[ 	]*{/: strtblview [strtblv] {/g
s/:[ 	]*stylesheet[ 	]*{/: stylesheet [stylesht] {/g
s/:[ 	]*termexport[ 	]*{/: termexport [termexp] {/g
s/:[ 	]*textview[ 	]*{/: textview [textv] {/g
s/:[ 	]*t822view[ 	]*{/: t822view [text822v] {/g
s/:[ 	]*thumbstrV[ 	]*{/: thumbstrV [thmbstrv] {/g
s/:[ 	]*thumbV[ 	]*{/: thumbV [thumbv] {/g
s/:[ 	]*termulator[ 	]*{/: termulator [tm] {/g
s/:[ 	]*tmapp[ 	]*{/: tmapp [tma] {/g
s/:[ 	]*tmview[ 	]*{/: tmview [tmv] {/g
s/:[ 	]*tree23int[ 	]*{/: tree23int [tree23] {/g
s/:[ 	]*typescript[ 	]*{/: typescript [tscript] {/g
s/:[ 	]*tscriptapp[ 	]*{/: tscriptapp [tscripta] {/g
s/:[ 	]*texttroff[ 	]*{/: texttroff [txttroff] {/g
s/:[ 	]*updatelist[ 	]*{/: updatelist [updlist] {/g
s/:[ 	]*valueview[ 	]*{/: valueview [valuev] {/g
s/:[ 	]*vopconapp[ 	]*{/: vopconapp [vopcona] {/g
s/:[ 	]*wmcursor[ 	]*{/: wmcursor [wcursor] {/g
s/:[ 	]*wmfontdesc[ 	]*{/: wmfontdesc [wfontd] {/g
s/:[ 	]*wmgraphic[ 	]*{/: wmgraphic [wgraphic] {/g
s/:[ 	]*wmim[ 	]*{/: wmim [wim] {/g
s/:[ 	]*wincelview[ 	]*{/: wincelview [wincelv] {/g
s/:[ 	]*windowsystem[ 	]*{/: windowsystem [winsys] {/g
s/:[ 	]*wmws[ 	]*{/: wmws [wws] {/g
s/:[ 	]*wrapperview[ 	]*{/: wrapperview [wrapperv] {/g
s/:[ 	]*xfontdesc[ 	]*{/: xfontdesc [xfontd] {/g
s/:[ 	]*zipoarrow[ 	]*{/: zipoarrow [zipoarrw] {/g
s/:[ 	]*zipobject[ 	]*{/: zipobject [zipobj] {/g
s/:[ 	]*zipofcapt[ 	]*{/: zipofcapt [zipofcap] {/g
s/:[ 	]*zipoimbed[ 	]*{/: zipoimbed [zipoimbd] {/g
s/:[ 	]*zipopolygon[ 	]*{/: zipopolygon [zipopoly] {/g
s/:[ 	]*ziposymbol[ 	]*{/: ziposymbol [ziposym] {/g
s/:[ 	]*zipstatus[ 	]*{/: zipstatus [zipstat] {/g
s/:[ 	]*zipview[ 	]*{/: zipview [zipv] {/g
!

foreach i (*)
  if (-d $i) then
    echo === Converting subdirectory $i
    $PROGNAME -q $i
  endif
end

echo === Executing 'make clean' in directory `pwd`
make clean

if (-e Makefile) then
    echo === Editing Makefile
    sed -e s/\\.H/.ch/g < Makefile > Makefile.sed
    mv Makefile Makefile.old
    mv Makefile.sed Makefile
endif

if (-e Imakefile) then
    echo === Editing Imakefile
    sed -e s/\\.H/.ch/g < Imakefile > Imakefile.sed
    mv Imakefile Imakefile.old
    mv Imakefile.sed Imakefile
endif

foreach i (*.H)
   if (-e $i) then
       echo === Renaming $i to $i:r.ch
       mv $i $i:r.ch
       echo "    ... Changing superclass names in" $i:r.ch
       sed -f $SEDFILE2 < $i:r.ch > $i:r.ch.sed
       mv $i:r.ch $i:r.ch.old
       mv $i:r.ch.sed $i:r.ch
   endif
end

foreach i (*.c Imakefile Makefile)
  if (-e $i) then
    echo === Changing .ih references in $i
    sed -f $SEDFILE < $i >$i.sed
    mv $i $i.old
    mv $i.sed $i
  endif
end

rm $SEDFILE $SEDFILE2
if ($QUIET == 0) then
echo === All done.  Your source directories are now probably TEEMING with .old files.  
echo "    If everything seems to work, you probably should delete all these .old files."
echo "  "
echo "*** Good luck, and thank you for using the Andrew Toolkit."
endif
