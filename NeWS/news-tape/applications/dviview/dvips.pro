% 
%  Dvips.pro - included prolog for DviLaser-generated PostScript files.
%
%  This version hacked for NeWS previewing - AJCD 22/3/89
% 
%  Copyright (c) 1986-88, ArborText, Inc.  All Rights Reserved.
%
%  This PostScript prolog code is part of the proprietary DVILASER/PS
%  program package and may not be copied or re-distributed without
%  the permission of ArborText, Inc. 
%

/DeFiNeFoNt /definefont load def

/definefont {
  exch {findfont} stopped
  {exch DeFiNeFoNt}
  {exch pop}
  ifelse
} bind def

systemdict /setpacking known  % use array packing mode if its available
  {/savepackingmode currentpacking def 
   true setpacking}
  if
 
/$DviLaser where
  {pop}
  {/$DviLaser 200 dict def}
  ifelse 
 
% Begin document
/BeginDviLaserDoc {
  vmstatus pop pop 0 eq 
    { $DviLaser begin 
      InitializeState }
    { /DviLaserJob save def
      $DviLaser begin
      InitializeState
      /DviLaserFonts save def }
    ifelse
} bind def
 
% End document
/EndDviLaserDoc {
  vmstatus pop pop 0 eq 
    { end }
    { DviLaserFonts restore
      end
      DviLaserJob restore }
    ifelse
} bind def
 
$DviLaser begin

/tempstr 64 string def
/tempint 0 def
/tempmatrix matrix def
 
%
%  Debugging routines
%
/DebugMode false def

/PrintInt {
  tempstr cvs print
} bind def

/PrintLn {
  (\n) print flush
} bind def
 
/PrintVMStats {
  (VM status - ) print
  vmstatus
  3 copy
  PrintInt (\(total\), ) print
  PrintInt (\(used\), ) print
  pop
  exch sub 
  PrintInt (\(remaining\), ) print
  PrintInt (\(level\)) print
  PrintLn
} bind def
 
/VMS /PrintVMStats load def 
 
/VMSDebug {
  DebugMode
    {PrintVMStats}
    {pop}
    ifelse
} bind def
 
(beginning of common prolog) VMSDebug 

% Make it easy to bind definitions.
/bdef /def load def %{ bind def } bind def
/xdef { exch def } bdef

% Begin page
/BP {
  /Magnification xdef
  /DviLaserPage save def
  (beginning of page) VMSDebug 
} bdef
 
% End page
/EP {
  DviLaserPage restore
} bdef
 
% Exit page (temporarily) to add fonts/characters.
/XP {
  % Save current point information so it can be reset later. 
  /Xpos where {pop Xpos} {0} ifelse
  /Ypos where {pop Ypos} {0} ifelse
  {currentpoint} stopped {0 0 moveto currentpoint} if 
  /DviLaserPage where {pop DviLaserPage restore} if
  moveto
  /Ypos xdef
  /Xpos xdef
} bdef
 
% Resume page
/RP {
  /DviLaserPage save def
} bdef
 
% Purge all fonts to reclaim memory space. 
/PF {
  GlobalMode
  LocalMode
} bdef
 
% Switch to base save/restore level, saving state information. 
/GlobalMode {
  /UserSave where {pop UserSave} if  % invoke "UserSave" if available
  PortraitMode 
  PaperWidth 
  PaperHeight 
  PxlResolution 
  Resolution 
  Magnification
  Ymax
  RasterScaleFactor
  % Save current point information so it can be reset later. 
  /currentpoint cvx stopped {0 0 moveto currentpoint} if 
  /DviLaserPage where {pop DviLaserPage restore} if
  DviLaserFonts restore
  RecoverState
} bdef
 
% Preserve state at the base level.
/RecoverState {
  10 copy
  /Ypos xdef
  /Xpos xdef
  /RasterScaleFactor xdef
  /Ymax xdef
  /Magnification xdef
  /Resolution xdef
  /PxlResolution xdef
  /PaperHeight xdef
  /PaperWidth xdef
  /PortraitMode xdef
  DoInitialScaling
  PortraitMode not {PaperWidth 0 SetupLandscape} if
  Xpos Ypos moveto
} bdef

% Initialize state variables to default values.
/InitializeState {
  /Resolution 3600.0 def
  /PxlResolution 300.0 def
  /RasterScaleFactor PxlResolution Resolution div def
  /PortraitMode true def
  GetPageDimensions
  72.0 div Resolution mul /PaperHeight xdef
  72.0 div Resolution mul /PaperWidth xdef
  /Ymax PaperHeight def
  /Magnification 1000.0 def
  /Xpos 0.0 def
  /Ypos 0.0 def
  /InitialMatrix {{PSCanvas ps_scale matrix currentmatrix} win send} def
} bdef
 
%
%  Procedure to figure out the current page dimensions.  There unfortunately
%  is no direct way to obtain this information.  This approach works for 
%  letter, note, and legal page sizes.
%
%      GetPageDimensions <height> <width>
%
/GetPageDimensions {
  /PSPageW win send 72.0 mul /PSPageH win send 72.0 mul
} bdef

% Switch from base save/restore level, restoring state information. 
/LocalMode {
  /Ypos xdef
  /Xpos xdef
  /RasterScaleFactor xdef
  /Ymax xdef
  /Magnification xdef
  /Resolution xdef
  /PxlResolution xdef
  /PaperHeight xdef
  /PaperWidth xdef
  /PortraitMode xdef
  DoInitialScaling
  PortraitMode not {PaperWidth 0 SetupLandscape} if
  Xpos Ypos moveto
  /UserRestore where {pop UserRestore} if  % invoke "UserRestore" if available
  /DviLaserFonts save def
  /DviLaserPage save def
} bdef
 
% Abbreviations 
/S /show load def
/SV /save load def
/RST /restore load def
 
/Yadjust {Ymax exch sub} bdef
 
% (x,y) position absolute, just set Xpos & Ypos, don't move.
/SXY {
  Yadjust 
  /Ypos xdef /Xpos xdef
} bdef
 
% (x,y) position absolute
/XY {
  Yadjust 
  2 copy /Ypos xdef /Xpos xdef
  moveto
} bdef
 
% (x,0) position absolute
/X {
  currentpoint exch pop 
  2 copy /Ypos xdef /Xpos xdef
  moveto
} bdef
 
% (0,y) position absolute
/Y {
  currentpoint pop exch Yadjust 
  2 copy /Ypos xdef /Xpos xdef
  moveto
} bdef
 
% (x,y) position relative
/xy {
  neg rmoveto
  currentpoint /Ypos xdef /Xpos xdef
} bdef
 
% (x,0) position relative
/x {
  0.0 rmoveto
  currentpoint /Ypos xdef /Xpos xdef
} bdef
 
% (0,y) position relative
/y {
  0.0 exch neg rmoveto
  currentpoint /Ypos xdef /Xpos xdef
  } bdef
 
% Print a rule
/R {
  /ht xdef
  /wd xdef 
  gsave
    0 setgray
    currentpoint
    newpath
      moveto
      0.0 ht rlineto
      wd 0.0 rlineto
      0.0 ht neg rlineto
      wd neg 0.0 rlineto
    closepath fill
  grestore
  wd 0.0 rmoveto
  currentpoint /Ypos xdef /Xpos xdef
} bdef

%
%  <PXL-file resolution(pix/inch)> <resolution(pix/inch)> RES
%
/RES {
  /Resolution xdef
  /PxlResolution xdef
  /RasterScaleFactor PxlResolution Resolution div def
  DoInitialScaling
} bdef

%
% Do initial scaling.
%
/DoInitialScaling {
  InitialMatrix setmatrix
  72.0 Resolution div dup scale   
} bdef
 
%
%  <paper-height(pix)> <paper-width(pix)> PM 
%
/PM { 
  XP
  /PaperWidth xdef 
  /PaperHeight xdef
  /Ymax PaperHeight def
  /PortraitMode true def
  DoInitialScaling
  RP
} bdef  
 
%
%  <paper-height(pix)> <paper-width(pix)> LM 
%
/LM {
  XP
  /PaperWidth xdef 
  /PaperHeight xdef
  /Ymax PaperWidth def
  /PortraitMode false def
  DoInitialScaling
  PaperWidth 0 SetupLandscape
  RP
} bdef  
  
% Change magnification setting
/MAG {
  XP
  /Magnification xdef
  RP
} bdef
 
%
%  Switch to landscape mode
%
/SetupLandscape {
  translate
  90.0 rotate
} bdef
 
%
%  <mode> SPB - begin "\special" mode
%
%  This is the PostScript procedure used to transfer from the internal
%  environment used for the DVI translation code emitted by DVIPS to
%  a standard PostScript environment.
%
%  Parameters: 0 - Local
%              1 - Global
%              2 - Inline
%
/SPB {
  /spc_mode xdef
  spc_mode 0 eq spc_mode 2 eq or
    {XP}
    {spc_mode 1 eq {GlobalMode} if} 
    ifelse
  Resolution 72.0 div dup scale        % Restore default scaling...
  Magnification 1000.0 div dup scale   % Adjust for any magnification...
  /Xpos Xpos 72.0 Resolution div mul 1000.0 Magnification div mul def
  /Ypos Ypos 72.0 Resolution div mul 1000.0 Magnification div mul def
} bdef
 
%
%  <mode> SPE - end "\special" mode
%
%  This is the PostScript procedure used to reenter the internal
%  environment used for the DVI translation code emitted by DVIPS from 
%  the standard PostScript environment provided for processing user-supplied
%  PostScript code.
%
%  Parameters: 0 - Local
%              1 - Global
%              2 - Inline
%
/SPE {
  /spc_mode xdef
  1000.0 Magnification div dup scale   % Un-adjust for any magnification...
  72.0 Resolution div dup scale        % Restore default internal scaling...
  spc_mode 0 eq spc_mode 2 eq or
    {RP}
    {spc_mode 1 eq {LocalMode} if} 
    ifelse
} bdef
 
%
%  <num-copies> PP
%
/PP {
  /#copies xdef
  showpage
  /#copies 1 def
} bdef
 
%
%  CLRP
%
/CLRP {
%  erasepage
} bdef
 
%
%  /font-name <point-size(pix)> DMF
%
/DMF {
  /psz xdef
  /nam xdef
  nam findfont psz scalefont setfont
} bdef
 
%
%  /abcd (xxx) str-concat  ==> /abcdxxx
%
/str-concatstr 64 string def

/str-concat {
  /xxx xdef
  /nam xdef
  /namstr nam str-concatstr cvs def
  /newnam namstr length xxx length add string def
  newnam 0 namstr putinterval
  newnam namstr length xxx putinterval
  newnam cvn 
} bdef
 
%
%  /abcdef 2 str-strip ==> /cdef
%
/str-strip {
  /num xdef
  /nam xdef
  /namstr nam tempstr cvs def
  /newlen namstr length num sub def
  namstr num newlen getinterval
  cvn
} bdef
 
%
%  <old-dict> copydict ==> new-dict on stack
%
/copydict {
  dup length 1 add dict /newdict xdef
    {1 index /FID ne
      {newdict 3 1 roll put}
      {pop pop}
     ifelse
    } forall 
  newdict
} bdef
 
%
%  <font-type> DefineCMEncoding
%
/DefineCMEncoding {
  /EncodeType xdef
 
  /CMEncoding 256 array def
  /Times-Roman findfont /Encoding get aload pop CMEncoding astore pop
 
  EncodeType 11 eq {Do-CM-rm-encoding} if
  EncodeType 12 eq {Do-CM-it-encoding} if
  EncodeType 13 eq {Do-CM-tt-encoding} if
} bdef
 
%
%  Do special mappings for the various CM-font types.  Characters that
%  get "covered up" are repositioned in the range (128,128+32).
%
/Do-standard-CM-encodings {
  CMEncoding
  dup 0 /.notdef put
  dup 1 /.notdef put
  dup 2 /.notdef put
  dup 3 /.notdef put
  dup 4 /.notdef put
  dup 5 /.notdef put
  dup 6 /.notdef put
  dup 7 /.notdef put
 
  dup 8 /.notdef put
  dup 9 /.notdef put
  dup 10 /.notdef put
  dup 11 /.notdef put
  dup 12 /fi put
  dup 13 /fl put
  dup 14 /.notdef put
  dup 15 /.notdef put
 
  dup 16 /dotlessi put
  dup 17 /.notdef put
  dup 18 /grave put
  dup 19 /acute put
  dup 20 /caron put
  dup 21 /breve put
  dup 22 /macron put
  dup 23 /ring put
 
  dup 24 /cedilla put
  dup 25 /germandbls put
  dup 26 /ae put
  dup 27 /oe put
  dup 28 /oslash put
  dup 29 /AE put
  dup 30 /OE put
  dup 31 /Oslash put
  dup 127 /dieresis put

  dup 128 /space put
  dup 129 /quotedbl put
  dup 130 /sterling put
  dup 131 /dollar put
  dup 132 /less put
  dup 133 /greater put
  dup 134 /backslash put
  dup 135 /asciicircum put
  dup 136 /underscore put
  dup 137 /braceleft put
  dup 138 /bar put
  dup 139 /braceright put
  dup 140 /asciitilde put
  pop
} bdef
 
/Do-CM-rm-encoding {
  Do-standard-CM-encodings
  CMEncoding
  dup 32 /.notdef put
  dup 34 /quotedblright put
  dup 60 /exclamdown put
  dup 62 /questiondown put
  dup 92 /quotedblleft put
  dup 94 /circumflex put
  dup 95 /dotaccent put
  dup 123 /endash put
  dup 124 /emdash put
  dup 125 /hungarumlaut put
  dup 126 /tilde put
  pop
} bdef
 
/Do-CM-it-encoding {
  Do-standard-CM-encodings
  CMEncoding
  dup 32 /.notdef put
  dup 34 /quotedblright put
  dup 36 /sterling put
  dup 60 /exclamdown put
  dup 62 /questiondown put
  dup 92 /quotedblleft put
  dup 94 /circumflex put
  dup 95 /dotaccent put
  dup 123 /endash put
  dup 124 /emdash put
  dup 125 /hungarumlaut put
  dup 126 /tilde put
  pop
} bdef
 
/Do-CM-tt-encoding {
  Do-standard-CM-encodings
  CMEncoding
  dup 12 /.notdef put
  dup 13 /quotesingle put
  dup 14 /exclamdown put
  dup 15 /questiondown put
  pop
} bdef

%
%  <int-font-name> <ext-font-name> <pt-sz(pix)> <type> <loaded-fg> DefineCMFont
%
%    type 10: "as-is" PostScript font
%    type 11: CM-mapped PostScript font - roman
%    type 12: CM-mapped PostScript font - text italic 
%    type 13: CM-mapped PostScript font - typewriter type 
%
/int-dict-name {int (-dict) str-concat} bdef
/int-dict {int (-dict) str-concat cvx load} bdef

/DF {
  true  % signal that the font is already loaded
  DefineCMFont
} bdef
 
/DNF {
  false  % signal that the font is not already loaded
  DefineCMFont
} bdef

/DefineCMFont {
  /loaded xdef
  /typ xdef
  /psz xdef
  /ext xdef
  /int xdef
 
  typ 10 ne 
    { % font_type = 11, 12, 13
    loaded not
      { /fnam ext 3 str-strip def
        fnam findfont copydict /newdict xdef 
        typ DefineCMEncoding
        newdict /Encoding CMEncoding put
        ext newdict definefont pop
      } if
%    int-dict-name ext findfont psz scalefont def
%    currentdict int [int-dict /setfont cvx] cvx put
    currentdict int [ext findfont psz scalefont /setfont cvx] cvx put
    }
    { % font_type = 10
%    /fnam ext def
%    int-dict-name fnam findfont psz scalefont def
%    currentdict int [int-dict /setfont cvx] cvx put
    currentdict int [ext findfont psz scalefont /setfont cvx] cvx put
    }
  ifelse
} bdef 
 
%
%  <int-font-name> <ext-font-name> <pt-sz(pix)> <PXL mag> <num-chars>
%      [llx lly urx ury] <newfont-fg> DefinePXLFont
%

/PXLF {
  true  % signal that the font is already loaded
  DefinePXLFont
} bdef
 
/PXLNF {
  false  % signal that the font is not already loaded
  DefinePXLFont
} bdef

/PXLBuildCharDict 17 dict def
 
/CMEncodingArray 256 array def
0 1 255 {CMEncodingArray exch dup tempstr cvs cvn put} for

/RasterConvert {RasterScaleFactor div} bdef
 
/TransformBBox {
  aload pop
 
  /BB-ury xdef
  /BB-urx xdef
  /BB-lly xdef
  /BB-llx xdef
 
  [BB-llx RasterConvert BB-lly RasterConvert 
   BB-urx RasterConvert BB-ury RasterConvert]
} bdef

/DefinePXLFont {
  /newfont xdef
  /bb xdef
  /num xdef
  /psz xdef
  /dsz xdef
  /pxlmag xdef
  /ext xdef
  /int xdef
 
  /fnam ext (-) str-concat pxlmag tempstr cvs str-concat def
 
  newfont not {
    int-dict-name 13 dict def
   
    int-dict begin
      /FontType 3 def
      /FontMatrix [1 dsz div 0 0 1 dsz div 0 0] def
      /FontBBox bb TransformBBox def
      /Encoding CMEncodingArray def
      /BuildChar
        { 
          PXLBuildCharDict begin
            /char xdef
            /fontdict xdef

            fontdict /CharDict get /Char-Info get char get aload pop

            /rasters xdef
            /cols xdef
            /rows xdef
            /wx xdef
            /ury xdef
            /urx xdef
            /lly xdef
            /llx xdef

            rows 0 lt
              { /rows rows neg def
                /runlength 1 def }
              { /runlength 0 def }
             ifelse
 
            wx 0 
            llx RasterConvert lly RasterConvert 
            urx RasterConvert ury RasterConvert setcachedevice
            rows 0 ne
              {
	      gsave
		llx .5 add lly .5 add translate
                cols rows scale % work around NeWS bugs
		true rasters imagemaskcanvas
              grestore
              } if
            end
        } def
        /CharDict 1 dict def
	  CharDict /Char-Info num array put
      end
      fnam int-dict definefont pop
    } if 

%  int-dict-name fnam findfont psz scalefont def
%  currentdict int [int-dict /setfont cvx] cvx put
  currentdict int [fnam findfont psz scalefont /setfont cvx] cvx put
} bdef 
 
%
%  <int-font-name> <code> <wx> <llx> <lly> <urx> <ury> <rows> <cols> <runlength> <rasters> PXLC
%
/PXLC {
 
  /rasters xdef
  /runlength xdef
  /cols xdef
  /rows xdef
  /ury xdef
  /urx xdef
  /lly xdef
  /llx xdef
  /wx xdef
  /code xdef
  /int xdef
 
  % See if the long or short format is required
%  true cols CKSZ rows CKSZ ury CKSZ urx CKSZ lly CKSZ llx CKSZ 
    TackRunLengthToRows
    int-dict /CharDict get /Char-Info get code 
    [llx lly urx ury wx rows cols % rasters => build canvas for NeWS
	cols rows 1 [RasterScaleFactor 0 0 RasterScaleFactor neg 0 0]
        GenerateRasters buildimage
    ] put
} bdef
 
/CKSZ {abs 127 le and} bdef
/TackRunLengthToRows {runlength 0 ne {/rows rows neg def} if} bdef
 
%
%  <wx> <dsz> <psz> <llx> <lly> <urx> <ury> <rows> <cols> <runlength> <rasters> PLOTC
%
/PLOTC {
  /rasters xdef
  /runlength xdef
  /cols xdef
  /rows xdef
  /ury xdef
  /urx xdef
  /lly xdef
  /llx xdef
  /psz xdef
  /dsz xdef
  /wx xdef
 
  % "Plot" a character's raster pattern.
  rows 0 ne
    {
    gsave
      currentpoint translate
      psz dsz div dup scale
%                cols rows scale % work around NeWS bugs
%                llx ury moveto  % NeWS ignores imagemask matrix
      cols rows true 
      RasterScaleFactor 0 0 RasterScaleFactor neg llx .5 add neg ury .5 add 
        tempmatrix astore
      GenerateRasters imagemask
    grestore
    } if
  wx x
} bdef
 
% Routine to generate rasters for "imagemask".
/GenerateRasters {
  rasters
  runlength 1 eq {RunLengthToRasters} if
} bdef
 
% Routine to convert from runlength encoding back to rasters.
/RunLengthToRasters {
  % ...not done yet...
} bdef
 
%
%  These procedures handle bitmap processing.
%
%  <bitmap columns> <bitmap rows> <bitmap pix/inch> <magnification> BMbeg
%
/BMbeg {
  /BMmagnification xdef
  /BMresolution xdef
  /BMrows xdef
  /BMcols xdef

  /BMcurrentrow 0 def
  gsave
    0.0 setgray
    Resolution BMresolution div dup scale
    currentpoint translate
    BMmagnification 1000.0 div dup scale
    0.0 BMrows moveto
    BMrows dup scale
    currentpoint translate
    /BMCheckpoint save def
  } bdef

/BMend {
  BMCheckpoint restore
  grestore
  } bdef

%
%  <hex raster bitmap> <rows> BMswath 
%
/BMswath {
  /rows xdef
  /rasters xdef

  BMcols rows true
  [BMrows 0 0 BMrows neg 0 BMcurrentrow neg]
  rasters
  imagemask

  /BMcurrentrow BMcurrentrow rows add def
  BMcurrentrow % save this on the stack around a restore...
  BMCheckpoint restore
  /BMcurrentrow xdef
  /BMCheckpoint save def
  } bdef

(end of common prolog) VMSDebug 

end
 
systemdict /setpacking known 
  {savepackingmode setpacking}
  if

% 
% End of included prolog section.
%

