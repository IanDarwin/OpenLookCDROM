#!/usr/local/bin/wish -f
# editwin.tk - simple Tk-based text editor window
# from jstools, adjusted by creilly@maths.tcd.ie to be more modular
#
#THIS IS ALPHA.
#
# Thu Jul 15 20:24:58 BST 1993
#Takes as an argument a procedure to call upon quitting and (optionally)
#a piece of text to edit.
#Also optionally toplevel to tell it to go to a toplevel window and 
#an optional title for the toplevel
#It return the window name which is static for the moment.
#
#TODO
#
#Add an optional script to run so we can configure the window
#to fit the look of other apps.
#


proc edittkmain {rv {text ""} {type "frame"} {title "Editor"} {script ""}} {
    if {$type == "toplevel"}  {
	toplevel .edit 
	wm title .edit $title
    } {
	frame .edit
    }

    global env argc argv

    catch {tk colormodel . color}		;# colour even on a 2-bit display

    global _edtkVERSION
    global _edtk_rv
    set _edtk_rv $rv
    set _edtkVERSION {3.2/1.0}

# TO DO
#   abbrev fixes:
#     abbrev panel
#     maybe some heuristics for things like plurals
#     maybe a syntax for suffixes (e.g., commit;t -> commitment)
#     deal with capitalisation
#   site-wide default bindings file
#   documentation for keybindings
#   routine to display output of a command
#   problem with filename getting set when you cancel Save 
#     for the first time on a new unnamed file
#   improve find panel
#     have find wrap around (if last time didn't match)
#     regex search/replace
#     change all
#     break out search and replace routines for re-use
#   gesture commands

######################################################################

global _edtkNAME			;# user's login name
global _edtkHOME			;# user's home directory

global _edtkPREFS			;# user preferences

if [info exists env(USER)] {
    set _edtkNAME $env(USER)
} elseif [info exists env(LOGNAME)] {
    set _edtkNAME $env(LOGNAME)
} else {
    error "Can't find environment variable USER or LOGNAME"
}
set _edtkHOME $env(HOME)

# check for $HOME/.tk/jlibrary.tcl and read it in if it exists.
# this contains library procedures.  it would normally be in $tk_library,
# but we check for its presence here for people who can't put things in
# $tk_library.
#
    if {[file isfile "$_edtkHOME/.tk/jlibrary.tcl"]} then {
	source "$_edtkHOME/.tk/jlibrary.tcl"
    }

    if {! [info exists env(PRINTER)]} {set env(PRINTER) lp}
    set _edtkPREFS(autoposition) 0	;# centre dialogue boxes and alerts
    set _edtkPREFS(confirm) 1		;# annoying confirmation dialogues
    set _edtkPREFS(textwidth) 80	;# width of text widget
    set _edtkPREFS(textheight) 24	;# height of text widget
    set _edtkPREFS(textwrap) char	;# none, char, word
    set _edtkPREFS(sabbrev) 0		;# <Space> expands static abbrevs?
    set _edtkPREFS(dabbrev) 0		;# <Tab> expands dynamic abbrevs?
    set _edtkPREFS(autobreak) 0		;# break long lines on <Space>?
    set _edtkPREFS(autoindent) 0	;# preserve indentation on <Return>?

    global _edtkFILENAME		;# left unset, so we can ask for one if needed

    global _edtkCUTBUFFER
    set _edtkCUTBUFFER {}

    global _edtkABBREV			;# contains last abbrev expanded
    set _edtkABBREV {}
    global _edtkABBREV_POS		;# start of last abbrev
    set _edtkABBREV_POS {}
    global MATCH			;# contains last match found
    set MATCH {}
    global MATCH_POS	;# position last match found
    set MATCH_POS {}
    global _edtkABBREV_LIST		;# list of abbrevs read from file
    set _edtkABBREV_LIST {}
    global _edtkABBREVS			;# text-indexed array of expansions

######################################################################
    foreach key {! $ & * ) - _ = + ] \} ; : ' \" , . / ? } {
         bind Text $key {
    tke_sabbrev_hook %W
    %W insert insert %A
    %W yview -pickplace insert
}
    }
######################################################################

# bind arrow keys before reading in user's configuration, so
#   the user can override bindings in ~/.textbindings.tcl.
     bind Text <Up> {
%W mark set insert {insert - 1 line}
  %W yview -pickplace insert
}
bind Text <Down> {
  %W mark set insert {insert + 1 line}
  %W yview -pickplace insert
}
bind Text <Left> {
  %W mark set insert {insert - 1 char}
  %W yview -pickplace insert
}
bind Text <Right> {
  %W mark set insert {insert + 1 char}
  %W yview -pickplace insert
}


######################################################################
# some default values that can be redefined in .edit.edittkrc.tcl:
#
global _edtkTEXTBG; set _edtkTEXTBG white		;# Tk default background is {#4eee94}
global _edtkTEXTFG; set _edtkTEXTFG black
global _edtkTEXTSB; set _edtkTEXTSB black		;# selected bg; Tk default is lt blue
global _edtkTEXTSF; set _edtkTEXTSF white		;# selected fg
global _edtkTEXTBW; set _edtkTEXTBW 0		;# selection border width
global _edtkPREFS(textfont); set _edtkPREFS(textfont) default	;
		# if default, font won't be set

tkW3MenuMakeMenus .edit.menu {
   {{file "File" 0} {
       {{Load ...} {cmd_load}}
       {{Save} {cmd_save}}
       {{Save As ...} {cmd_saveas}}
       {{Print ...} {cmd_print}}
       {}
       {{Insert File ...} {cmd_insfile}}
       {}
       {{Checkpoint} {cmd_save_checkpoint}}
       {{Revert to Checkpoint} {cmd_restore_checkpoint}}
       {}
       {{Preferences ...} {cmd_prefs}}
       {{Issue Tcl Command ...} {tkW3FileRunTclCommand}}
       {}
       {{Quit ...} {cmd_quit}}
   }}
   {{edit "Edit" 0} {
       {{Cut}  {cmd_cut}}
       {{Copy}  {cmd_copy}}
       {{Paste} {cmd_paste}}
       {{Insert X Selection} {cmd_xpaste}}
       {{Select All} {cmd_select_all}}
       {}
       {{Find ...} {cmd_find}}
       {{Find Again}  {cmd_find_again}}
       {}
       {{Go to Line ...} {cmd_go_to_line}}
       {{Show Current Position ...} {cmd_current_line}}
   }}
   {{abbrev "Abbrev" 0} {}}
   {{pipe "Pipe" 0} {
       {{Format Lines with `fmt'} {tke_pipenl {fmt}}}
       {{Capitalise} {tke_pipe {tr \[a-z\] \[A-Z\]}}}
       {{Lowercase} {tke_pipe {tr \[A-Z\] \[a-z\]}}}
       {{Toggle Case} {tke_pipe {tr \[A-Z\]\[a-z\] \[a-z\]\[A-Z\]}}}
       {}
       {{Indent} {tke_pipenl {sed "s/^/  /"}}}
       {{Unindent/Unquote} {tke_pipenl {sed "s/^\[ >:|\}\] //
s/^\t/      /"}}}
       {{Quote Email} {tke_pipenl {sed "s/^/> /"}}}
       {}
       {{Sort by ASCII Sequence} {tke_pipenl {sort}}}
       {{Sort Numerically} {tke_pipenl {sort -n}}}
       {{Sort Alphabetically} {tke_pipenl {sort -if}}}
       {}
       {{Pipe Through ...} {cmd_run_pipe}}
       {{Insert Output of ...} {cmd_run_command}}
   }}
   {{help "Help" 0 right} {
       {{About the Editor ...} {tkW3HelpGetTopic edit.about.editor}}
   }}
}

.edit.menu.abbrev.m add checkbutton -label {Static Abbreviation} \
  -accelerator {[Space]} \
  -variable ED_PREFS(sabbrev) -onvalue 1 -offvalue 0
.edit.menu.abbrev.m add checkbutton -label {Dynamic Abbreviation} \
  -accelerator {[Tab]} \
  -variable ED_PREFS(dabbrev) -onvalue 1 -offvalue 0
.edit.menu.abbrev.m add separator
.edit.menu.abbrev.m add command -label {Expand Static Abbreviation} -command {
  cmd_sabbrev
}
.edit.menu.abbrev.m add command -label {Expand Dynamic Abbreviation} -command {
  cmd_dabbrev
}
.edit.menu.abbrev.m add separator
.edit.menu.abbrev.m add command -label {Edit Static Abbreviations} -command {
  cmd_edit_abbrevs
}
.edit.menu.abbrev.m add command -label {Reread Static Abbreviations} -command {
  cmd_read_abbrevs
}


######################################################################

frame .edit.main -relief flat -borderwidth 5

pack append .edit.main \
    [scrollbar .edit.main.s -command {.edit.main.t yview}] {right filly} \
    [text .edit.main.t -yscroll {.edit.main.s set} -setgrid true \
    -width $_edtkPREFS(textwidth) -height $_edtkPREFS(textheight)] \
    {right expand fill}

# the following can't be redefined in the above; if you want to change
# them, do so in $HOME/.edittkrc.tcl
#
bind .edit.main.t <Meta-space>	{set _edtkPREFS(sabbrev) [expr {! $_edtkPREFS(sabbrev)}]}
bind .edit.main.t <Meta-Tab>		{set _edtkPREFS(dabbrev) [expr {! $_edtkPREFS(dabbrev)}]}
bind .edit.main.t <Meta-bar>		{cmd_run_pipe}
bind .edit.main.t <Meta-exclam>	{cmd_run_command}
bind .edit.main.t <Meta-a>		{cmd_select_all}
bind .edit.main.t <Meta-c>		{cmd_copy}
bind .edit.main.t <Meta-C>		{cmd_current_line}
bind .edit.main.t <Meta-f>		{cmd_find}
bind .edit.main.t <Meta-g>		{cmd_find_again}
bind .edit.main.t <Meta-h>		{cmd_help}
bind .edit.main.t <Meta-i>		{cmd_insfile}
bind .edit.main.t <Meta-l>		{cmd_load}
bind .edit.main.t <Meta-L>		{cmd_go_to_line}
bind .edit.main.t <Meta-n>		{exec edit.tk &}
bind .edit.main.t <Meta-p>		{cmd_print}
bind .edit.main.t <Meta-s>		{cmd_save}
bind .edit.main.t <Meta-S>		{cmd_saveas}
bind .edit.main.t <Meta-T>		{tkW3FileRunTclCommand}
bind .edit.main.t <Meta-v>		{cmd_paste}
bind .edit.main.t <Meta-V>		{cmd_xpaste}
bind .edit.main.t <Meta-x>		{cmd_cut}
bind .edit.main.t <Meta-q>		{cmd_quit}

bind .edit.main.t <Tab>		{tke_tabkey}
bind .edit.main.t <Shift-Tab>	{%W insert insert "\t"}
bind .edit.main.t <space>		{tke_spacebar}
bind .edit.main.t <Shift-space>	{%W insert insert " "}
bind .edit.main.t <Return>		{tke_returnkey}
bind .edit.main.t <Shift-Return>	{%W insert insert "\n"}


######################################################################

pack append .edit .edit.menu {top fillx}
pack append .edit .edit.main {top expand fill}

######################################################################
######################################################################
##########  START OF PROCEDURE DEFINITIONS  ##########################
######################################################################
######################################################################

######################################################################
# abbrev - set an abbreviation (used by .edit.tk/abbrevs.tcl
######################################################################

proc abbrev {{abbrev} {expansion}} {
  global _edtkABBREVS
  set _edtkABBREVS($abbrev) $expansion
}

######################################################################
# tke_pipenl command - pipe selection through command (and replace)
#   adds a newline
######################################################################

proc tke_pipenl { command } {
    catch { eval exec $command << {[j:selection_if_any]} } result
    append result "\n"

    if [j:no_selection] {return 0}
    # save current position of selection:
    set selfirst [.edit.main.t index sel.first]
    # insert result:
    .edit.main.t insert sel.first $result
    # delete old selection:
    .edit.main.t delete sel.first sel.last
    # find end of result:
    set sellast [.edit.main.t index "$selfirst +[string length $result]chars"]
    # select result:
    .edit.main.t tag add sel $selfirst $sellast
}

######################################################################
# tke_pipe command - pipe selection through command (and replace)
#   does not add a newline
######################################################################

proc tke_pipe { command } {
  catch { eval exec $command << {[j:selection_if_any]} } result

  if [j:no_selection] {return 0}
  # save current position of selection:
  set selfirst [.edit.main.t index sel.first]
  # insert result:
  .edit.main.t insert sel.first $result
  # delete old selection:
  .edit.main.t delete sel.first sel.last
  # find end of result:
  set sellast [.edit.main.t index "$selfirst +[string length $result]chars"]
  # select result:
  .edit.main.t tag add sel $selfirst $sellast
}

######################################################################
# tke_read filename ?t? - "Load..." with supplied filename
######################################################################

proc tke_read { filename {t .edit.main.t}} {
  if {! [file exists $filename]} then {
      $t delete 1.0 end
      $t mark set insert 1.0
  } else {
    # should do error checking
    set file [open $filename {r}]
    $t delete 1.0 end
    $t insert end  [read $file]
    $t mark set insert 1.0
    close $file
  }
  focus .edit.main.t
}

######################################################################
# tke_write filename ?t? - write out a file
######################################################################

proc tke_write {filename {t .edit.main.t}} {
  # should do error checking
  set file [open $filename {w}]
  puts $file [$t get 1.0 end] nonewline
  close $file
}

######################################################################
# tke_tabkey - do whatever tab does (abbrev, indent, whatever)
######################################################################

proc tke_tabkey {{t .edit.main.t}} {
  global PREFS

  if $PREFS(dabbrev) {
    if [cmd_dabbrev] {$t insert insert " "}
  } else {
    $t insert insert "\t"
  }
}

######################################################################
# tke_spacebar - do whatever space does (abbrev, line breaking, whatever)
######################################################################

proc tke_spacebar {{t .edit.main.t}} {
  tke_sabbrev_hook $t
  $t insert insert " "
  tke_autobreak_hook
}

######################################################################
# tke_sabbrev_hook - cmd_sabbrev if it's on
######################################################################

proc tke_sabbrev_hook {{t .edit.main.t}} {
  global _edtkPREFS

  if $_edtkPREFS(sabbrev) {
    cmd_sabbrev
  }
}

######################################################################
# tke_autobreak_hook - insert a cr if line is long enough
######################################################################

proc tke_autobreak_hook {{t .edit.main.t}} {
  global _edtkPREFS

  if $_edtkPREFS(autobreak) {
    set length [string length [$t get {insert linestart} insert]]
    if {$length > ($_edtkPREFS(textwidth) - 15)} {
      tke_returnkey
    }
  }
}

######################################################################
# tke_returnkey - do whatever return does (indentation, etc.)
######################################################################

proc tke_returnkey {{t .edit.main.t}} {
  tke_sabbrev_hook $t
  $t insert insert "\n"
  $t yview -pickplace insert
  tke_autoindent_hook
}

######################################################################
# tke_autoindent_hook - insert same indentation as previous line
######################################################################

proc tke_autoindent_hook {{t .edit.main.t}} {
  global _edtkPREFS

  if $_edtkPREFS(autoindent) {
    set prevline [$t get {insert -1lines linestart} {insert -1lines lineend}]
    if [regexp "^\[ \t\]\[ \t\]*" $prevline indentation] {
      $t insert insert $indentation
    }
  }
}

######################################################################
# cmd_prefs - preferences panel
######################################################################

proc cmd_prefs {} {
  global _edtkPREFS env tk_strictMotif

  toplevel .edit.prefs

  frame .edit.prefs.autoposition
  checkbutton .edit.prefs.autoposition.cb -relief flat -anchor w \
    -text {Auto-position dialogue boxes} -variable PREFS(autoposition)
  frame .edit.prefs.confirm
  checkbutton .edit.prefs.confirm.cb -relief flat -anchor w \
    -text {Confirm actions} -variable PREFS(confirm)
  frame .edit.prefs.motif
  checkbutton .edit.prefs.motif.cb -relief flat -anchor w \
    -text {Strict Motif emulation} -variable tk_strictMotif
  frame .edit.prefs.r1 -height 2 -width 200 -borderwidth 1 -relief sunken
  frame .edit.prefs.autobreak
  checkbutton .edit.prefs.autobreak.cb -relief flat -anchor w \
    -text {Break long lines with <Space>} -variable PREFS(autobreak)
  frame .edit.prefs.autoindent
  checkbutton .edit.prefs.autoindent.cb -relief flat -anchor w \
    -text {Preserve indentation with <Return>} -variable PREFS(autoindent)
  frame .edit.prefs.r2 -height 2 -width 200 -borderwidth 1 -relief sunken
  frame .edit.prefs.sabbrev
  checkbutton .edit.prefs.sabbrev.cb -relief flat -anchor w \
    -text {Expand static abbreviations with <Space>} -variable PREFS(sabbrev)
  frame .edit.prefs.dabbrev
  checkbutton .edit.prefs.dabbrev.cb -relief flat -anchor w \
    -text {Expand dynamic abbreviations with <Tab>} -variable PREFS(dabbrev)
  frame .edit.prefs.r3 -height 2 -width 200 -borderwidth 1 -relief sunken
  frame .edit.prefs.wrap
  radiobutton .edit.prefs.wrap.none -relief flat -anchor w \
    -text {Don't wrap lines} -variable PREFS(textwrap) -value none
  radiobutton .edit.prefs.wrap.char -relief flat -anchor w \
    -text {Wrap lines on character boundaries} \
    -variable _edtkPREFS(textwrap) -value char
  radiobutton .edit.prefs.wrap.word -relief flat -anchor w \
    -text {Wrap lines at word boundaries} -variable _edtkPREFS(textwrap) -value word
  frame .edit.prefs.r4 -height 2 -width 200 -borderwidth 1 -relief sunken
  frame .edit.prefs.font
  frame .edit.prefs.font.top
  label .edit.prefs.font.top.l -text {Font:}
  button .edit.prefs.font.top.default -width 8 -text {Default} -command {
    set _edtkPREFS(textfont) {default}
  }
  button .edit.prefs.font.top.choose -text {Choose .edit. .edit. .edit.} -command {
    set _edtkPREFS(textfont) [j:prompt_font]
  }
  frame .edit.prefs.font.bot
  entry .edit.prefs.font.bot.e -relief sunken -width 50 \
    -textvariable _edtkPREFS(textfont)
  frame .edit.prefs.r5 -height 2 -width 200 -borderwidth 1 -relief sunken
  frame .edit.prefs.size
  label .edit.prefs.size.wl -text {Width:}
  entry .edit.prefs.size.we -relief sunken -width 3 -textvariable _edtkPREFS(textwidth)
  label .edit.prefs.size.hl -text {Height:}
  entry .edit.prefs.size.he -relief sunken -width 3 -textvariable _edtkPREFS(textheight)
  frame .edit.prefs.printer
  label .edit.prefs.printer.l -text {Printer:}
  entry .edit.prefs.printer.e -relief sunken -width 25 \
    -textvariable env(PRINTER)

  frame .edit.prefs.r -height 2 -width 200 -borderwidth 1 -relief sunken

  frame .edit.prefs.b
  button .edit.prefs.b.ok -text OK -bd 4 -width 8 -command {
    if {$_edtkPREFS(textwidth) < 20} {set _edtkPREFS(textwidth) 20}
    if {$_edtkPREFS(textheight) < 4} {set _edtkPREFS(textheight) 4}
    .edit.main.t configure \
       -width $_edtkPREFS(textwidth) \
       -height $_edtkPREFS(textheight) \
       -wrap $_edtkPREFS(textwrap)
    j:configure_font .edit.main.t $_edtkPREFS(textfont)	;# knows about `default'
    focus .edit.main.t
    destroy .edit.prefs
  }
  button .edit.prefs.b.save -text Save -width 8 -command {
    j:write_prefs edittkprefs.tcl
    .edit.prefs.b.ok invoke
  }
  
  pack append .edit.prefs.b \
    .edit.prefs.b.ok {right padx 10 pady 10} \
    .edit.prefs.b.save {right pady 10}
  pack append .edit.prefs.autoposition .edit.prefs.autoposition.cb {left expand fillx}
  pack append .edit.prefs.confirm .edit.prefs.confirm.cb {left expand fillx}
  pack append .edit.prefs.motif .edit.prefs.motif.cb {left expand fillx}
  pack append .edit.prefs.autobreak .edit.prefs.autobreak.cb {left expand fillx}
  pack append .edit.prefs.autoindent .edit.prefs.autoindent.cb {left expand fillx}
  pack append .edit.prefs.sabbrev .edit.prefs.sabbrev.cb {left expand fillx}
  pack append .edit.prefs.dabbrev .edit.prefs.dabbrev.cb {left expand fillx}
  pack append .edit.prefs.wrap \
    .edit.prefs.wrap.none {top expand fillx} \
    .edit.prefs.wrap.char {top expand fillx} \
    .edit.prefs.wrap.word {top expand fillx}
  pack append .edit.prefs.font.top \
    .edit.prefs.font.top.l {left} \
    .edit.prefs.font.top.choose {right padx 10 pady 5} \
    .edit.prefs.font.top.default {right pady 5}
  pack append .edit.prefs.font.bot \
    .edit.prefs.font.bot.e {left padx 10 pady 5}
  pack append .edit.prefs.font \
    .edit.prefs.font.top {top expand fillx} \
    .edit.prefs.font.bot {top expand fillx}
  pack append .edit.prefs.size \
    .edit.prefs.size.wl {left fillx} \
    .edit.prefs.size.we {left} \
    .edit.prefs.size.hl {left fillx} \
    .edit.prefs.size.he {left}
  pack append .edit.prefs.printer \
    .edit.prefs.printer.l {left} \
    .edit.prefs.printer.e {left padx 10 pady 10}

  pack append .edit.prefs \
    .edit.prefs.autoposition {top expand fill} \
    .edit.prefs.confirm {top expand fill} \
    .edit.prefs.motif {top expand fill} \
    .edit.prefs.r1 {top fillx} \
    .edit.prefs.autobreak {top fillx} \
    .edit.prefs.autoindent {top fillx} \
    .edit.prefs.r2 {top fillx} \
    .edit.prefs.sabbrev {top expand fill} \
    .edit.prefs.dabbrev {top expand fill} \
    .edit.prefs.r3 {top fillx} \
    .edit.prefs.wrap {top expand fill} \
    .edit.prefs.r4 {top fillx} \
    .edit.prefs.font {top fillx} \
    .edit.prefs.r5 {top fillx} \
    .edit.prefs.size {top expand fill} \
    .edit.prefs.printer {top expand fill} \
    .edit.prefs.r {top fillx} \
    .edit.prefs.b {top expand fillx}

  j:dialogue .edit.prefs		;# position in centre of screen

  focus .edit.prefs
  bind .edit.prefs.font.bot.e <Key-Return> {.prefs.b.ok invoke}
  bind .edit.prefs.size.we <Key-Return> {.prefs.b.ok invoke}
  bind .edit.prefs.size.he <Key-Return> {.prefs.b.ok invoke}
  bind .edit.prefs.printer.e <Key-Return> {.prefs.b.ok invoke}
  bind .edit.prefs.font.bot.e <Key-Tab> {focus .edit.prefs.size.we}
  bind .edit.prefs.size.we <Key-Tab> {focus .edit.prefs.size.he}
  bind .edit.prefs.size.he <Key-Tab> {focus .edit.prefs.printer.e}
  bind .edit.prefs.printer.e <Key-Tab> {focus .edit.prefs.size.we}
  bind .edit.prefs <Key-Return> {.prefs.b.ok invoke}
  bind .edit.prefs <Key-Tab> {focus .edit.prefs.font.bot.e}
  grab .edit.prefs
  tkwait window .edit.prefs
}

######################################################################
# cmd_quit - quit the editor
######################################################################

proc cmd_quit {} {
    global _edtk_rv
  if {[DLG:msg .edit .quit_dialog \
      "Are you sure you want to quit?" question "Yes" "No"] == 1} {
    set alpha [.edit.main.t get 1.0 end]
    destroy .edit
    $_edtk_rv $alpha 
  }
}

######################################################################
# cmd_load - read in a file
######################################################################

proc cmd_load {} {
  global _edtkFILENAME		;# so it can be default
  append _edtkFILENAME {}		;# make sure it's defined

  set prompt_result [j:fs {} "Load:"]
  if {$prompt_result != {././/CANCEL//./.}} then {
    set _edtkFILENAME $prompt_result
    tke_read $_edtkFILENAME
  }
  focus .edit.main.t
}
  
######################################################################
# cmd_save - write out a file, using $_edtkFILENAME if defined
######################################################################

proc cmd_save {{t .edit.main.t}} {
  global _edtkFILENAME		;# so it can be default
  append _edtkFILENAME {}		;# make sure it's defined

  if {[string length $_edtkFILENAME] > 0} then {
    tke_write $_edtkFILENAME $t
  } else {
     set prompt_result [j:fs {} "Save as:"]
    if {$prompt_result != {././/CANCEL//./.}} then {
      set _edtkFILENAME $prompt_result
      tke_write $_edtkFILENAME $t
    }
  }
  focus $t
}

######################################################################
# cmd_saveas - write out a file, prompting for a filename
######################################################################

proc cmd_saveas {{t .edit.main.t}} {
  global _edtkFILENAME		;# so it can be default
  append _edtkFILENAME {}		;# make sure it's defined

  set prompt_result [j:fs {} "Save as:"]
  if {$prompt_result != {././/CANCEL//./.} && \
     ( ! [file exists $prompt_result] || \
      [DLG:msg .edit .prompt_result "File \"$prompt_result\" exists; replace it?" question "Yes" "No"] == 1 )} then {
    set _edtkFILENAME $prompt_result
    tke_write $_edtkFILENAME $t
  }
  focus $t
}

######################################################################
# cmd_print - print the file using lpr
######################################################################

proc cmd_print {} {
  if {[DLG:msg .edit .print "Print using `lpr' to the default printer?" question "Yes" "No"]==1} {
    exec lpr << [.edit.main.t get 1.0 end]
  }
  focus .edit.main.t
}

######################################################################
# cmd_insfile - read in a file and insert it at the insert mark
######################################################################

proc cmd_insfile {} {
  set prompt_result [j:fs {} "Insert:"]
  if {$prompt_result != {././/CANCEL//./.}} then {
    .edit.main.t insert insert [exec cat $prompt_result]
    .edit.main.t insert insert "\n"
  }
  focus .edit.main.t
}

######################################################################
# cmd_save_checkpoint - save current state
######################################################################

proc cmd_save_checkpoint {} {
  global CKPT__edtkTEXT
  global CKPT_INSERT

  set CKPT__edtkTEXT [.edit.main.t get 1.0 end]
  set CKPT_INSERT [.edit.main.t index insert]
}

######################################################################
# cmd_restore_checkpoint - restore from saved state
######################################################################

proc cmd_restore_checkpoint {} {
  global CKPT__edtkTEXT
  global CKPT_INSERT

  if {[DLG:msg .edit .abandon \
       "Abandon changes since last checkpoint?" question "Yes" "No"] ==1} {
    .edit.main.t delete 1.0 end
    .edit.main.t insert end $CKPT__edtkTEXT
    .edit.main.t mark set insert $CKPT_INSERT
    .edit.main.t yview -pickplace insert
  }
}

######################################################################
# cmd_cut - delete the selection and copy it to _edtkCUTBUFFER
######################################################################

proc cmd_cut {} {
  global _edtkCUTBUFFER

  set _edtkCUTBUFFER [.edit.main.t get sel.first sel.last]
  .edit.main.t delete sel.first sel.last
}

######################################################################
# cmd_copy - copy the selection into _edtkCUTBUFFER
######################################################################

proc cmd_copy {} {
  global _edtkCUTBUFFER

  set _edtkCUTBUFFER [.edit.main.t get sel.first sel.last]
}

######################################################################
# cmd_paste - insert _edtkCUTBUFFER
######################################################################

proc cmd_paste {} {
  global _edtkCUTBUFFER

  .edit.main.t insert insert $_edtkCUTBUFFER
}

######################################################################
# cmd_select_all - mark the entire text as selected
######################################################################

proc cmd_select_all {} {
  .edit.main.t tag add sel 1.0 end
}

######################################################################
# cmd_run_pipe - prompt for a Unix command to run on the selection
######################################################################

proc cmd_run_pipe {} {
  global UNIX_PIPE; append UNIX_PIPE {}

  set prompt_result [j:prompt "Unix Filter:" $UNIX_PIPE]
  if {$prompt_result != {././/CANCEL//./.}} then {
    set UNIX_PIPE $prompt_result
    tke_pipenl $UNIX_PIPE
  }
}

######################################################################
# cmd_run_command - prompt for a Unix command to insert
######################################################################
# just calls cmd_run_pipe on a zero-length selection

proc cmd_run_command {} {
  global UNIX_COMMAND; append UNIX_COMMAND {}

  set prompt_result [j:prompt "Unix Command:" $UNIX_COMMAND]
  if {$prompt_result != {././/CANCEL//./.}} then {
    set UNIX_COMMAND $prompt_result
    catch { eval exec $UNIX_COMMAND } result
    if {$result != {}} {
      append result "\n"
      .edit.main.t insert insert $result
    }
  }
}

######################################################################
# cmd_dabbrev - expand abbreviation before insert
######################################################################

proc cmd_dabbrev {{t .edit.main.t}} {
  # PROBLEM: this depends on the Text widget's notion of words.
  # it would be nice to be able to expand, say, $tk_l to $tk_library.

  global _edtkABBREV _edtkABBREV_POS MATCH MATCH_POS

  $t mark set abbrevstart insert
  while {[$t compare abbrevstart != 1.0] &&
         [string match {[a-zA-Z0-9']} [$t get {abbrevstart - 1 char}]]} {
    $t mark set abbrevstart {abbrevstart -1char}
  }

  set _edtkABBREV_POS [$t index abbrevstart]	;# for dabbrev_agaiin

  set _edtkABBREV [$t get abbrevstart insert]

  set context [$t get 0.0 abbrevstart]

  while {1} {
    set matchpos [string last $_edtkABBREV $context]
  
    if {$matchpos == -1} {return 0}	;# not found

    $t mark set matchstart [$t index "0.0 +$matchpos chars"]
    if {[$t compare matchstart == {matchstart wordstart}]} {
      $t mark set matchend [$t index {matchstart wordend}]
      break				;# sort of an `until'
    }
    set context [$t get 0.0 matchstart]
  }

  set MATCH [$t get matchstart matchend]

  set MATCH_POS [$t index matchstart]

  $t delete abbrevstart insert
  $t insert insert $MATCH
  return 1
}

######################################################################
# dabbrev_again - search earlier in the text for abbrevs
######################################################################

proc dabbrev_again {{t .edit.main.t}} {
  # PROBLEM: this depends on the Text widget's notion of words.
  # it would be nice to be able to expand, say, $tk_l to $tk_library.

  global _edtkABBREV _edtkABBREV_POS _edtkMATCH _edtkMATCH_POS

  set context [$t get 0.0 $_edtkMATCH_POS]

  while {1} {
    set matchpos [string last $_edtkABBREV $context]
  
    if {$matchpos == -1} {
      return [sabbrev]			;# try the static table
    }
    $t mark set matchstart [$t index "0.0 +$matchpos chars"]
    if {[$t compare matchstart == {matchstart wordstart}]} {
      $t mark set matchend [$t index {matchstart wordend}]
      break				;# sort of an `until'
    }
    set context [$t get 0.0 matchstart]
  }

  set _edtkMATCH [$t get matchstart matchend]

  set _edtkMATCH_POS [$t index matchstart]

  $t delete $_edtkABBREV_POS insert
  $t insert insert $_edtkMATCH
  $t insert insert " "
}

######################################################################
# cmd_sabbrev - look up an abbrev in a static table
######################################################################

proc cmd_sabbrev {{t .edit.main.t}} {
  # PROBLEM: this depends on the Text widget's notion of words.
  # it would be nice to be able to expand, say, $tk_l to $tk_library.

  # following don't really need to be global (shared with dabbrev):
  global _edtkABBREV _edtkABBREV_POS _edtkABBREVS

  $t mark set abbrevstart insert
  while {[$t compare abbrevstart != 1.0] &&
         [string match {[a-zA-Z0-9']} [$t get {abbrevstart - 1 char}]]} {
    $t mark set abbrevstart {abbrevstart -1char}
  }

  set _edtkABBREV_POS [$t index abbrevstart]	;# for dabbrev_agaiin

  set _edtkABBREV [$t get abbrevstart insert]
  if {[info exists _edtkABBREVS($_edtkABBREV)]} {
    $t delete $_edtkABBREV_POS insert
    $t insert insert $_edtkABBREVS($_edtkABBREV)
    return 1
  }
  return 0
}

######################################################################
# cmd_edit_abbrevs - edit your abbrevs file
######################################################################

proc cmd_edit_abbrevs {} {
  global _edtkHOME
  if {! [file isdirectory "$_edtkHOME/.tk"]} then {
    exec mkdir "$_edtkHOME/.tk"
    # above should have error-checking
  }
  if {! [file isfile "$_edtkHOME/.tk/abbrevs.tcl"]} then {
    # create it, with examples
  }
  exec edit.tk "$_edtkHOME/.tk/abbrevs.tcl" &
}

######################################################################
# cmd_find - non-modal search-and-replace panel
######################################################################

proc cmd_find {} {
  global _edtkSEARCHFOR			;# so it can be default
  append _edtkSEARCHFOR {}			;# make sure it's defined
  global _edtkREPLACEWITH			;# so it can be default
  append _edtkREPLACEWITH {}			;# make sure it's defined
  global _edtkFINDISACTIVE			;# only one active at a time
  global _edtkFINDBACKWARDS			;# search backwards from insert point
  global _edtkFINDCASE			;# find is case-sensitive

  if {! [info exists _edtkFINDBACKWARDS]} {
    set _edtkFINDBACKWARDS 0
  }
  if {! [info exists _edtkFINDCASE]} {
    set _edtkFINDCASE 0
  }

  if [info exists _edtkFINDISACTIVE] {
    wm withdraw .edit.find
    wm deiconify .edit.find			;# just try to make it visible
    focus .edit.find.t.search.e		;# and focus on the search field
    return 0
  }
  set _edtkFINDISACTIVE 1

  toplevel .edit.find
  wm title .edit.find "Find Panel"
  frame .edit.find.t
  frame .edit.find.t.search
  label .edit.find.t.search.l -text "Search for:" -width 16 -anchor e
  entry .edit.find.t.search.e -relief sunken -width 40
  frame .edit.find.t.replace
  label .edit.find.t.replace.l -text "Replace with:" -width 16 -anchor e
  entry .edit.find.t.replace.e -relief sunken -width 40
  frame .edit.find.t.options
  label .edit.find.t.options.filler -text {} -width 16 -anchor e
  checkbutton .edit.find.t.options.backwards -relief flat -anchor w \
    -text {Backwards} -variable _edtkFINDBACKWARDS
  checkbutton .edit.find.t.options.case -relief flat -anchor w \
    -text "Case\255sensitive" -variable _edtkFINDCASE
  frame .edit.find.b
  frame .edit.find.b.r -height 2 -width 200 -borderwidth 1 -relief sunken
  button .edit.find.b.search -text Search -bd 4 -width 8 -command {
    set _edtkREPLACEWITH [.edit.find.t.replace.e get]
    tke_find [set _edtkSEARCHFOR [.edit.find.t.search.e get]]
  }
  button .edit.find.b.replace -text Replace -width 8 -command {
    tke_replace [set _edtkREPLACEWITH [.edit.find.t.replace.e get]]
    tke_find [set _edtkSEARCHFOR [.edit.find.t.search.e get]]
  }
  button .edit.find.b.cancel -text Done -width 8 -command {
    set _edtkREPLACEWITH [.edit.find.t.replace.e get]
    set _edtkSEARCHFOR [.edit.find.t.search.e get]
    focus .edit.main.t
    unset _edtkFINDISACTIVE
    destroy .edit.find
  }
  pack append .edit.find.t.search \
    .edit.find.t.search.l {left fill} \
    .edit.find.t.search.e {left fill}
  pack append .edit.find.t.replace \
    .edit.find.t.replace.l {left fill} \
    .edit.find.t.replace.e {left fill}
  pack append .edit.find.t.options \
    .edit.find.t.options.filler {left fill} \
    .edit.find.t.options.backwards {left fill} \
    .edit.find.t.options.case {left filly padx 20}
  pack append .edit.find.t \
    .edit.find.t.search {top expand pady 10} \
    .edit.find.t.replace {top expand} \
    .edit.find.t.options {top fillx pady 10}
  pack append .edit.find.b \
    .edit.find.b.r {top fillx} \
    .edit.find.b.search {right pady 10 padx 10} \
    .edit.find.b.replace {right pady 10} \
    .edit.find.b.cancel {right pady 10 padx 10}
  pack append .edit.find \
    .edit.find.t {top fill padx 10 pady 5} \
    .edit.find.b {bottom fillx}

  .edit.find.t.search.e delete 0 end
  .edit.find.t.search.e insert end $_edtkSEARCHFOR
  .edit.find.t.replace.e delete 0 end
  .edit.find.t.replace.e insert end $_edtkREPLACEWITH

  # return or Meta-g in either field searches:
  bind .edit.find.t.search.e <Key-Return> \
    {.edit.find.b.search invoke}
  bind .edit.find.t.replace.e <Key-Return> \
    {.edit.find.b.search invoke}
  bind .edit.find.t.search.e <Meta-g> \
    {.edit.find.b.search invoke}
  bind .edit.find.t.replace.e <Meta-g> \
    {.edit.find.b.search invoke}

  # tab switches fields:
  bind .edit.find.t.search.e <Key-Tab> \
    {focus .edit.find.t.replace.e}
  bind .edit.find.t.replace.e <Key-Tab> \
    {focus .edit.find.t.search.e}

  # Control-C, Meta-C, and Meta-period cancel:
  bind .edit.find.t.search.e <Control-c> {.edit.find.b.cancel invoke}
  bind .edit.find.t.search.e <Meta-c> {.edit.find.b.cancel invoke}
  bind .edit.find.t.search.e <Meta-period> {.edit.find.b.cancel invoke}
  bind .edit.find.t.replace.e <Control-c> {.edit.find.b.cancel invoke}
  bind .edit.find.t.replace.e <Meta-c> {.edit.find.b.cancel invoke}
  bind .edit.find.t.replace.e <Meta-period> {.edit.find.b.cancel invoke}

  focus .edit.find.t.search.e
}

######################################################################
# cmd_find_again - search-and-replace again
######################################################################

proc cmd_find_again {} {
  global _edtkSEARCHFOR			;# so it can be default
  append _edtkSEARCHFOR {}			;# make sure it's defined

  if {$_edtkSEARCHFOR == {}} {
    cmd_find
  } else {
    tke_find $_edtkSEARCHFOR
  }
}

######################################################################
# tke_find string - find and select the string
######################################################################
# WARNING: since this takes a copy of the file, it could use a LOT
# of memory!
# should be rewritten to use a different mark than insert.

proc tke_find { string } {
  global _edtkFINDBACKWARDS			;# search backwards from insert point
  global _edtkFINDCASE			;# find is case-sensitive

  # don't bother looking for the null string:
  if {$string == {}} {
    return 0
  }

  if {! [info exists _edtkFINDBACKWARDS]} {
    set _edtkFINDBACKWARDS 0
  }
  if {! [info exists _edtkFINDCASE]} {
    set _edtkFINDCASE 0
  }

  if $_edtkFINDBACKWARDS {
    set lastfirst last
    set textpart [.edit.main.t get 0.0 {insert -1char}]
    set countfrom 0.0
  } else {
    set lastfirst first
    set textpart [.edit.main.t get insert end]
    set countfrom insert
  }

  if {!$_edtkFINDCASE} {
    set string [string tolower $string]
    set textpart [string tolower $textpart]
  }

  set foundpos [string $lastfirst $string $textpart]

  if {$foundpos == -1} then {
    j:alert "Not found."
    return 0
  }
  # deselect any already-selected text:
  catch {.main.t tag remove sel sel.first sel.last}
  # select the range that matched:
  set lastpos [expr {$foundpos + [string length $string]}]
  .edit.main.t tag add sel \
    "$countfrom + $foundpos chars" "$countfrom + $lastpos chars"
  # move insert just after the match (so we can continue from there)
  .edit.main.t mark set insert "$countfrom + $lastpos chars"
  .edit.main.t yview -pickplace insert
}

######################################################################
# tke_replace string - replace the selection with the string
######################################################################
# SHOULD CONFIRM THAT A SELECTION EXISTS!

proc tke_replace { string } {
  .edit.main.t insert sel.first $string
  .edit.main.t mark set insert sel.first
  .edit.main.t delete sel.first sel.last
}

######################################################################
# cmd_go_to_line - go to a particular line
######## NEED TO CHECK THAT AN INDEX WAS TYPED!
######################################################################

proc cmd_go_to_line {} {
  set prompt_result [j:prompt "Go to line number:"]
  if {$prompt_result != {././/CANCEL//./.}} then {
    tke_go_to_line $prompt_result
  }
}

######################################################################
# tke_go_to_line lineno - go to a particular line
######################################################################

proc tke_go_to_line {{lineno 0}} {
  .edit.main.t mark set insert $lineno.0
  .edit.main.t yview -pickplace insert
}

######################################################################
# cmd_current_line lineno - display which line the cursor is on
######################################################################

proc cmd_current_line {} {
  set insertindex [split [.edit.main.t index insert] {.}]
  set line [lindex $insertindex 0]
  set column [lindex $insertindex 1]
  j:alert "The insertion point is at line $line, column $column."
}

######################################################################
# cmd_xpaste - insert _edtkCUTBUFFER
######################################################################

proc cmd_xpaste {} {
  .edit.main.t insert insert [j:selection_if_any]
}

######################################################################
# FINAL SETUP
######################################################################

# read in user's .edit.tk/edittkrc.tcl and .edit.tk/edittkprefs.tcl file
#

# implement user preferences (_edtkPREFS(textheight) and _edtkPREFS(textwidth) on panel):
catch {.main.t configure \
  -background $_edtkTEXTBG \
  -foreground $_edtkTEXTFG \
  -insertbackground $_edtkTEXTFG \
  -selectbackground $_edtkTEXTSB \
  -selectforeground $_edtkTEXTSF \
  -selectborderwidth $_edtkTEXTBW \
  -width $_edtkPREFS(textwidth) \
  -height $_edtkPREFS(textheight) \
  -wrap $_edtkPREFS(textwrap)}

# process arguments, if any
#
    .edit.main.t insert end $text

if {$script != ""} {eval $script}


######################################################################
wm withdraw .edit
return .edit
}
