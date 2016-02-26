## init.tcl: configuration file for tkWWW  user interface
## ==============
## Copyright (C) 1992-1993
## Globewide Network Academy
## Macvicar Institute for Educational Software Development
##
## See the file COPYRIGHT for conditions

## *************************
## The home page and the start page
## *************************

## Get it from the environment variable WWW_HOME if possible

if {[catch {set env(WWW_HOME)} home_page] != 0} {
    # Get starting pages from the environment variables set in the shell
    #    script tkWWW
    set tkW3ConfigHomePage $env(TK_WWW_HOME_PAGE)
    set tkW3ConfigStartPage $env(TK_WWW_START_PAGE)
} {
    set tkW3ConfigHomePage $home_page
    set tkW3ConfigStartPage $home_page
}

catch "set tkW3ConfigFile $env(HOME)/.tkWWW-config"
set tkW3EditChar(translate) {}
set tkW3HtPage(base) ""
set tkW3HtPage(modified) 0
set tkW3HtPage(is_index) 0

set tkW3ConfigHelpRoot http://tk-www.mit.edu:8001/tk-www/help

set tkW3ConfigMailList(request) vn@vungtau.cerf.net
set tkW3ConfigMailList(list) tkwww@vungtau.cerf.net
set tkW3ConfigMailList(bugs) tk-www-bugs@mit.edu

# set the logo
# This should be defined in the tkWWW server
set tkW3ConfigLogo HtLogoBitmap

## The programs that tkWWW uses
set tkW3ConfigViewer(editor) 		emacs
set tkW3ConfigViewer(terminal)		xterm
set tkW3ConfigViewer(printer)		lpr
set tkW3ConfigViewer(dvi)		xdvi
set tkW3ConfigViewer(image)		xli
set tkW3ConfigViewer(video)		mpeg_play
set tkW3ConfigViewer(audio)		showaudio
set tkW3ConfigViewer(postscript)	ghostview

set tkW3ConfigImageDirectView 1

## Procedure to exec a command in a terminal

proc tkW3ConfigExecTerm {command} {
    global tkW3ConfigViewer
    eval [concat "exec" $tkW3ConfigViewer(terminal) "-e" $command] 
}

## The mail command
proc tkW3ConfigSendMail {to subject body} {
    global env
    if [file executable /usr/ucb/mail] {
	set file [open "| /usr/ucb/mail -s \"$subject\" $to" w+]
    } elseif {$env(TK_WWW_MAIL) == "mhmail"} {
	set file [open "| mhmail $to -subject \"$subject\"" w+]
    } 
    puts $file $body
    close $file
}

## Configure the menus
set tkW3ConfigMenus {
    {{file "File" 0} {
	{"New" 	{tkW3EditCreateNewPage} 0}
	{}
	{"Open File..." tkW3FileLoadSource 0 }
	{"Open Link..." {DLG:show . .goto_dialog}}
	{"Change directory..." tkW3FileChangeDirectory}
	{}
        {"Save" tkW3FileSave}
	{"Save as..." tkW3FileSaveAs 0}
	{"Generate Source" tkW3EditGenerate}
	{"Save Text..." tkW3FileSaveText}
	{"Reload page" tkW3NavigateReload}
	{}
	{"Find Keyword..." {DLG:show . .find_dialog} 0 "<Meta-f>"}
	{"Clone Current Window" tkW3NavigateClone 1}
	{} 
	{"Print" {DLG:show . .print_dialog} 0 }
	{"Mail" 
	    {DLG:set_entry_value .mail_dialog 2 [tkW3NavigateGetTitle];
	     DLG:show . .mail_dialog} }
	{}
	{"Run TCL Command" {tkW3FileRunTclCommand} 0 "<Meta-x>"}
	{}
	{"Close" tkW3FileQueryCloseWindow 0}
	{"Exit" tkW3FileQueryExit 0}
    }}
    {{navigate "Navigate" 0} {
	{"Backtrack" "tkW3NavigateBacktrack" 0}
	{"Recall..." tkW3HistoryDialog 0}
	{}
	{"Bookmarks..." tkW3BookmarksDialog}
	{"Add Bookmark" tkW3BookmarksAdd 0}
	{}
	{"Open Introduction" 
	    {tkW3NavigateRecordAndGoto "http://tk-www.mit.edu:8001/tk-www/help/sources.html"}}

    }}

    {{edit "Edit" 0} {
	{"Cut" {tkW3EditCut .f.msg} 2 "<Shift-Delete>"}
        {"Copy" {tkW3EditCopy .f.msg} 0 "<Control-Insert>"} 
	{"Paste" {tkW3EditPaste .f.msg} 0 "<Shift-Insert>"}
	{}
	{"Delete" {tkW3EditDelete .f.msg} 0}
	{}
	{"Select All" {tkW3EditSelectAll .f.msg} {} }
	{"Deselect All" {tkW3EditDeselectAll .f.msg} {} }
	{}
	{"Edit page parameters ..." {tkW3EditQueryChangeTitle .f.msg} {} "<Meta-t>"}
	{"Create anchor ..." {tkW3EditBeginAnchor .f.msg} {} "<Meta-a>"}
        {}
	{"Change Fonts ..." {DLG:show . .font_dialog}}
	{}
	{"Insert List Item" {tkW3EditBeginListItem .f.msg} {} "<Meta-l>"}
	{"Insert Glossary Item" {tkW3EditBeginGlossaryItem .f.msg} {} "<Meta-g>"}
	{"Insert Image" {tkW3EditAddImage .f.msg} {} "<Meta-i>"}
	{"Insert Paragraph Break" {tkW3EditInsertBullet .f.msg P} {} "<Meta-p>"}
	{"Insert Line Break" {tkW3EditInsertBullet .f.msg BR} {}  "<Meta-b>"}
	{"Insert Horizontal Rule" {tkW3EditInsertBullet .f.msg HR} {} "<Meta-h>"}
    }}
    {{paragraph "Paragraph" 0} {
	{"Default" "tkW3EditSetStyle para {}"}
	{"Heading" "cascade" 
	    {{"Heading1" "tkW3EditSetStyle para H1" "" "<Meta-Key-1>"}
	     {"Heading2" "tkW3EditSetStyle para H2" "" "<Meta-Key-2>"}
	     {"Heading3" "tkW3EditSetStyle para H3" "" "<Meta-Key-3>"}
	     {"Heading4" "tkW3EditSetStyle para H4" "" "<Meta-Key-4>"}
	     {"Heading5" "tkW3EditSetStyle para H5" "" "<Meta-Key-5>"}
	     {"Heading6" "tkW3EditSetStyle para H6" "" "<Meta-Key-6>"}
	    }}
	{"Address" "tkW3EditSetStyle para ADDRESS"}
	{"Preformatted" "tkW3EditSetStyle para PRE"}
    }}
    {{character "Characters" 0} {
	{"default"          "tkW3EditSetStyle char {}"}
	{"Italics"          "tkW3EditSetStyle char I"}
	{"Bold"             "tkW3EditSetStyle char B"}
	{"Underline"        "tkW3EditSetStyle char U"}
	{"Typewriter"       "tkW3EditSetStyle char TT"}
	{"Emphasis"         "tkW3EditSetStyle char EM"}
	{"Strong emphasis"  "tkW3EditSetStyle char STRONG"}
	{"Code"             "tkW3EditSetStyle char CODE"}
	{"Sample"           "tkW3EditSetStyle char SAMP"}
	{"Keyboard"         "tkW3EditSetStyle char KBD"}
	{"Variable"         "tkW3EditSetStyle char VAR"}
	{"Definition"       "tkW3EditSetStyle char DFN"}
	{"Citation"         "tkW3EditSetStyle char CITE"}
    }}	
    {{annotate "Annotate" 0} {
	{"Add Annotation..." tkW3AnnotateAdd}
	{"Edit Annotation..." tkW3AnnotateEdit}
	{}
	{"Goto Annotation" cascade_external annotate}
    }}
    {{help "Help" 0 right}  {
       {"Overview" 	"tkW3HelpGetTopic overview" 0}
       {"Index" 	"tkW3HelpGetTopic index" 0}
       {"Keyboard" 	"tkW3HelpGetTopic keyboard" 0}
       {"Tutorial" 	"tkW3HelpGetTopic tutorial" 0}
       {}
       {"About Globewide Network Academy" "tkW3HelpGetTopic uu-gna" 0}
       {"Starting information sources" "tkW3HelpGetTopic sources" 0}
       {}
       {"Send mail to developer..." "DLG:show . .send_mail_dialog" 0}
       {"Subscribe to mailing list tkwww@vungtau.cerf.net" "tkW3HelpAddToMailingList" }
       {"Unsubscribe to mailing list tkwww@vungtau.cerf.net" "tkW3HelpRemoveFromMailingList" }

       {}
       {"Using Help" 	 "tkW3HelpGetTopic using_help" 6}
       {"Product Information" "tkW3HelpAbout" 0}
   }}}

set tkW3ConfigEntries {
    {para "Paragraph: "}
    {char "Character: "}
}

set tkW3ConfigButtons {
    {home "Home" {tkW3NavigateRecordAndGoto $tkW3ConfigHomePage}}
    {back "Back" {tkW3NavigateBacktrack}}
    {find "Find" {DLG:show . .find_dialog}}
    {goto "Goto..." {DLG:show . .goto_dialog}}
    {reload "Reload" tkW3NavigateReload}
    {save "Save" tkW3FileSave}
    {save_as "Save As..." tkW3FileSaveAs}
    {generate "Generate Source" {tkW3EditGenerate}}
    {clone "Clone" {tkW3NavigateClone [tkW3NavigateGetAddress]}}
    {close_window "Close Window" "tkW3FileQueryCloseWindow"}
}

set tkW3ConfigToggles {
    {is_index "Is Index"}
    {has_annotation "Has Annotations"}
    {is_modified "Modified"}
}

proc tkW3ConfigDisplay {} {
    global tkW3HistoryList
    global tkW3Bookmarks
    global tkW3HtPage

    set history_length [llength $tkW3HistoryList]
    tkW3OutputButtonSetSensitive .buttons.back $history_length
    tkW3OutputMenuSetSensitive .menu.navigate.m "Backtrack*" $history_length

    tkW3OutputButtonSetSensitive .buttons.find $tkW3HtPage(is_index)
    tkW3OutputMenuSetSensitive .menu.file.m "Find*" $tkW3HtPage(is_index)
    tkW3OutputToggleSet .toggles.is_index $tkW3HtPage(is_index)

    set annotate_display [tkW3AnnotateDisplay]
    tkW3OutputToggleSet .toggles.has_annotation $annotate_display
    tkW3OutputMenuSetSensitive .menu.annotate.m "Goto*" $annotate_display

    tkW3OutputToggleSet .toggles.is_modified $tkW3HtPage(modified)
}

set tkW3ConfigFileTypes   {
    { ".mime" "www/mime"  "8bit" 1.0 }	
    { ".PS" 	"application/postscript"  "8bit" 0.8 }
    { ".eps" 	"application/postscript"  "8bit" 0.8 }
    { ".ai" 	"application/postscript"  "8bit" 0.5 }
    { ".ps" 	"application/postscript"  "8bit" 0.8 }
    { ".html" "text/html"  "8bit" 1.0 }		
    { ".c" 	"text/plain"  "7bit" 0.5 }
    { ".h" 	"text/plain"  "7bit" 0.5 }		
    { ".m" 	"text/plain"  "7bit" 0.5 } 
    { ".txt"  "text/plain"  "7bit" 0.5 }
    { ".rtf" 	"application/x-rtf" "8bit" 1.0 }
    { ".snd"  "audio/basic"  "binary" 1.0 }
    { ".au"   "audio/basic"  "binary" 1.0 }
    { ".bin" 	"application/octet-stream"  "binary" 1.0 }
    { ".dmp"  "image/x11-dump" "binary" 1.0}
    { ".pbm" "image/portable-bitmap" "binary" 1.0}
    { ".ppm" "image/portable-pixmap" "binary" 1.0}
    { ".xbm" "image/x-bitmap" "binary" 1.0} 
    { ".gif"  "image/gif"  "binary" 1.0 }
    { ".tiff" "image/x-tiff"  "binary" 1.0 }
    { ".jpg"  "image/jpeg"  "binary" 1.0 }
    { ".JPG"  "image/jpeg"  "binary" 1.0 }
    { ".JPEG" "image/jpeg"  "binary" 1.0 }
    { ".jpeg" "image/jpeg"  "binary" 1.0 }
    { ".ras"  "image/cmu-raster" "binary" 1.0}
    { ".MPEG" "video/mpeg"  "binary" 1.0 }
    { ".mpg" "video/mpeg"  "binary" 1.0 }
    { ".MPG" "video/mpeg"  "binary" 1.0 }
    { ".mpeg" "video/mpeg"  "binary" 1.0 }
    { ".tex" "application/tex" "binary" 1.0}
    { ".latex" "application/latex" "binary" 1.0}
    { ".texi" "application/texinfo" "binary" 1.0}
    { ".dvi" "application/dvi" "binary" 1.0}
    { ".tar" "archive/tar" "binary" 1.0}
    { ".shar" "archive/shar" "binary" 1.0}
    { ".x11" "image/x11-dump" "binary" 1.0}
}

set tkW3ConfigFileEncodings {
    { ".Z" "x-compressed" 1.0} 
    { ".gz" "x-gzipped" 1.0}
}

proc tkW3ConfigDisplayFile {type file_name address {encoding ""}} {
    global tkW3ConfigViewer	
    switch -glob $type {
	"image/*" {
	    tkW3ImageDisplayFile $file_name
	}
	"video/*" {
	tkW3NavigatePreface {Video file} "video" \
	    "exec $tkW3ConfigViewer(video) $file_name &"
	}
	"audio/*" {
	tkW3NavigatePreface {Audio file} "audio" \
	    "exec $tkW3ConfigViewer(audio) $file_name &"
	}
	{application/postscript} {
	tkW3NavigatePreface {Postscript file} {postscript} \
	    "exec $tkW3ConfigViewer(postscript) $file_name &"
	}
	{application/dvi} {
	tkW3NavigatePreface {TeX Device Independent file} "TeX DVI file" \
	    "exec $tkW3ConfigViewer(dvi) $file_name &"
	}
	{application/tex} -
	{application/latex} -
	{application/octet-stream} -
	{text/plain} {
	    switch $encoding {
		{x-compressed} {
		    tkW3OutputSetBodyToFile "| zcat $file_name "
		}
		{x-gzipped} {
		    tkW3OutputSetBodyToFile "| gunzip -c $file_name"
		}
		{default} {
		    tkW3OutputSetBodyToFile $file_name
		}
	    }
	}
	{default} {
	    case [DLG:msg . .graphics_dialog "Unknown file type" question \
		  "Display as text" "Display as image" "Save file as..."] {

		      {1} {
			  case $encoding {
			      {x-compressed} {
				  tkW3OutputSetBodyToFile "| zcat $file_name "
			      }
			      {x-gzipped} {
				  tkW3OutputSetBodyToFile \
				      "| gunzip -c $file_name"
			      }
			      {default} {
				  tkW3OutputSetBodyToFile $file_name
			      }
			  }
		      }
		      {2} {tkW3ImageDisplayFile $file_name}
		      {3} {tkW3OutputSaveFileAs $file_name $address}
		  }
	}
    }
    tkW3HtSetName $address ""
}

## *************************
## Font lists
## *************************
set tkW3ConfigFont(times) { \
    {"" "-adobe-times-medium-r-*-*-12-*-*-*-*-*-*-*"}
    {"H1" "-adobe-times-bold-r-normal-*-24-*-*-*-*-*-*-*"} 
    {"H2" "-adobe-times-bold-r-normal-*-18-*-*-*-*-*-*-*"} 
    {"H3" "-adobe-times-bold-r-normal-*-18-*-*-*-*-*-*-*"} 
    {"H4" "-adobe-times-bold-r-normal-*-14-*-*-*-*-*-*-*"} 
    {"H5" "-adobe-times-bold-r-normal-*-12-*-*-*-*-*-*-*"} 
    {"H6" "-adobe-times-bold-r-normal-*-12-*-*-*-*-*-*-*"} 
    {"ADDRESS" "-adobe-times-medium-i-normal-*-14-*-*-*-*-*-*-*"} 
    {"XMP" "-adobe-courier-medium-r-normal-*-12-*-*-*-*-*-*-*"}
    {"PRE" "-adobe-courier-medium-r-normal-*-12-*-*-*-*-*-*-*"}

    {"TT"  "-adobe-courier-medium-r-normal-*-12-*-*-*-*-*-*-*"}
    {"CODE" "-adobe-courier-medium-r-normal-*-12-*-*-*-*-*-*-*"}
    {"SAMP" "-adobe-courier-medium-r-normal-*-12-*-*-*-*-*-*-*"}
    {"B" "-adobe-times-bold-r-*-*-12-*-*-*-*-*-*-*"}
    {"EM" "-adobe-times-bold-r-*-*-12-*-*-*-*-*-*-*"}
    {"I" "-adobe-times-medium-i-*-*-12-*-*-*-*-*-*-*"}
    
}

set tkW3ConfigFont(helvetica) { \
    {"" "-adobe-helvetica-medium-r-*-*-12-*-*-*-*-*-*-*"}
    {"H1" "-adobe-helvetica-bold-r-normal-*-24-*-*-*-*-*-*-*"} 
    {"H2" "-adobe-helvetica-bold-r-normal-*-18-*-*-*-*-*-*-*"} 
    {"H3" "-adobe-helvetica-bold-r-normal-*-18-*-*-*-*-*-*-*"} 
    {"H4" "-adobe-helvetica-bold-r-normal-*-14-*-*-*-*-*-*-*"} 
    {"H5" "-adobe-helvetica-bold-r-normal-*-12-*-*-*-*-*-*-*"} 
    {"H6" "-adobe-helvetica-bold-r-normal-*-12-*-*-*-*-*-*-*"} 
    {"ADDRESS" "-adobe-helvetica-medium-o-normal-*-14-*-*-*-*-*-*-*"} 
    {"XMP" "-adobe-courier-medium-r-normal-*-12-*-*-*-*-*-*-*"}
    {"PRE" "-adobe-courier-medium-r-normal-*-12-*-*-*-*-*-*-*"}

    {"TT"  "-adobe-courier-medium-r-normal-*-12-*-*-*-*-*-*-*"}
    {"CODE" "-adobe-courier-medium-r-normal-*-12-*-*-*-*-*-*-*"}
    {"SAMP" "-adobe-courier-medium-r-normal-*-12-*-*-*-*-*-*-*"}
    {"B" "-adobe-helvetica-bold-r-normal-*-12-*-*-*-*-*-*-*"} 
    {"EM" "-adobe-helvetica-bold-r-normal-*-12-*-*-*-*-*-*-*"} 
    {"I" "-adobe-helvetica-medium-o-normal-*-14-*-*-*-*-*-*-*"} 
}

set tkW3ConfigFont(new_century) { \
    {"" "-adobe-new century schoolbook-medium-r-*-*-12-*-*-*-*-*-*-*"}
    {"H1" "-adobe-new century schoolbook-bold-r-normal-*-24-*-*-*-*-*-*-*"} 
    {"H2" "-adobe-new century schoolbook-bold-r-normal-*-18-*-*-*-*-*-*-*"} 
    {"H3" "-adobe-new century schoolbook-bold-r-normal-*-18-*-*-*-*-*-*-*"} 
    {"H4" "-adobe-new century schoolbook-bold-r-normal-*-14-*-*-*-*-*-*-*"} 
    {"H5" "-adobe-new century schoolbook-bold-r-normal-*-12-*-*-*-*-*-*-*"} 
    {"H6" "-adobe-new century schoolbook-bold-r-normal-*-12-*-*-*-*-*-*-*"} 
    {"ADDRESS" "-adobe-new century schoolbook-medium-i-normal-*-14-*-*-*-*-*-*-*"} 
    {"XMP" "-adobe-courier-medium-r-normal-*-12-*-*-*-*-*-*-*"}
    {"PRE" "-adobe-courier-medium-r-normal-*-12-*-*-*-*-*-*-*"}

    {"TT"  "-adobe-courier-medium-r-normal-*-12-*-*-*-*-*-*-*"}
    {"CODE" "-adobe-courier-medium-r-normal-*-12-*-*-*-*-*-*-*"}
    {"SAMP" "-adobe-courier-medium-r-normal-*-12-*-*-*-*-*-*-*"}
    {"B" "-adobe-new century schoolbook-bold-r-normal-*-12-*-*-*-*-*-*-*"} 
    {"EM" "-adobe-new century schoolbook-bold-r-normal-*-12-*-*-*-*-*-*-*"}
    {"I" "-adobe-new century schoolbook-medium-i-normal-*-14-*-*-*-*-*-*-*"}  
}

set tkW3ConfigFont(lucida) { \
    {"" "-b&h-lucida-medium-r-*-*-12-*-*-*-*-*-*-*"}
    {"H1" "-b&h-lucida-bold-r-normal-*-24-*-*-*-*-*-*-*"} 
    {"H2" "-b&h-lucida-bold-r-normal-*-18-*-*-*-*-*-*-*"} 
    {"H3" "-b&h-lucida-bold-r-normal-*-18-*-*-*-*-*-*-*"} 
    {"H4" "-b&h-lucida-bold-r-normal-*-14-*-*-*-*-*-*-*"} 
    {"H5" "-b&h-lucida-bold-r-normal-*-12-*-*-*-*-*-*-*"} 
    {"H6" "-b&h-lucida-bold-r-normal-*-12-*-*-*-*-*-*-*"} 
    {"ADDRESS" "-b&h-lucida-medium-i-normal-*-14-*-*-*-*-*-*-*"} 
    {"XMP" "-adobe-courier-medium-r-normal-*-12-*-*-*-*-*-*-*"}
    {"PRE" "-adobe-courier-medium-r-normal-*-12-*-*-*-*-*-*-*"}

    {"TT"  "-adobe-courier-medium-r-normal-*-12-*-*-*-*-*-*-*"}
    {"CODE" "-adobe-courier-medium-r-normal-*-12-*-*-*-*-*-*-*"}
    {"SAMP" "-adobe-courier-medium-r-normal-*-12-*-*-*-*-*-*-*"}
}

set tkW3ConfigFontList {
    {times Times} 
    {helvetica Helvetica}
    {lucida Lucida}
    {new_century "New Century"}
}

set tkW3ConfigFontDefault times

## *************************
## Set options
## *************************

option add *background gray userDefault
option add *foreground black userDefault
option add *buttons*relief raised userDefault
option add *buttons*borderWidth 2 userDefault
option add *toggles*relief flat userDefault
option add *toggles*borderWidth 2 userDefault
option add *Scrollbar.foreground gray userDefault
option add *Hypertext.Button.relief raised userDefault
option add *Frame.relief raised userDefault
option add *Frame.borderWidth 2 userDefault
option add *Hypertext*background bisque userDefault
option add *Text*background bisque userDefault
option add *Text*wrap word userDefault
option add *Text.relief sunken userDefault
option add *Text.borderWidth 2 userDefault
option add *Scrollbar*relief sunken userDefault

## *************************
## Set key bindings
## *************************

bind Text <Left> {# xf ignore me 9
  %W mark set insert insert-1c; %W yview -pickplace insert}
bind Text <Right> {# xf ignore me 9
  %W mark set insert insert+1c; %W yview -pickplace insert}
bind Text <Up> {# xf ignore me 9
  %W mark set insert insert-1l; %W yview -pickplace insert}
bind Text <Down> {# xf ignore me 9
  %W mark set insert insert+1l; %W yview -pickplace insert}
bind Text <Delete> {# xf ignore me 9
  tk_textBackspace %W; %W yview -pickplace insert}
bind Text <Control-a> {# xf ignore me 9
  %W mark set insert "insert linestart"; %W yview -pickplace insert}
bind Text <Control-b> {# xf ignore me 9
  %W mark set insert insert-1c; %W yview -pickplace insert}
bind Text <Control-d> {# xf ignore me 9
  %W delete insert insert+1c; %W yview -pickplace insert}
bind Text <Control-e> {# xf ignore me 9
  %W mark set insert "insert lineend"; %W yview -pickplace insert}
bind Text <Control-f> {# xf ignore me 9
  %W mark set insert insert+1c; %W yview -pickplace insert}
bind Text <Meta-h> {# xf ignore me 9
  %W delete "insert linestart" insert; %W yview -pickplace insert}
bind Text <Control-i> {# xf ignore me 9
  %W inser insert \t; %W yview -pickplace insert}
bind Text <Control-j> {# xf ignore me 9
  %W inser insert \n; %W yview -pickplace insert}
bind Text <Control-k> {# xf ignore me 9
  if {[%W compare insert == "insert lineend"]} {
    %W delete insert "insert lineend +1c"
    %W yview -pickplace insert
  } {
    %W delete insert "insert lineend"
    %W yview -pickplace insert}}
bind Text <Control-l> {# xf ignore me 9
  %W yview -pickplace insert}
bind Text <Control-m> {# xf ignore me 9
  %W inser insert \n; %W yview -pickplace insert}
bind Text <Control-n> {# xf ignore me 9
  %W mark set insert insert+1l; %W yview -pickplace insert}
bind Text <Control-o> {# xf ignore me 9
  %W inser insert \n; %W yview -pickplace insert}
bind Text <Control-p> {# xf ignore me 9
  %W mark set insert insert-1l; %W yview -pickplace insert}
bind Text <Meta-v> {# xf ignore me 9
  set xfCounter [lindex [%W config -height] 4]
  while {$xfCounter > 0} {
    %W mark set insert insert-1l
    incr xfCounter -1
  }
  %W yview -pickplace insert}
bind Text <Control-v> {# xf ignore me 9
  set xfCounter [lindex [%W config -height] 4]
  while {$xfCounter > 0} {
    %W mark set insert insert+1l
    incr xfCounter -1
  }
  %W yview -pickplace insert}
bind Text <Control-w> {# xf ignore me 9
  catch "%W delete sel.first sel.last"}
bind Text <Control-y> {# xf ignore me 9
  %W insert insert "[tkW3EditSelectionGet]"; %W yview -pickplace insert}
bind Text <Home> {# xf ignore me 9 
  %W mark set insert 1.0; %W yview -pickplace insert}
bind Text <End> {# xf ignore me 9
  %W mark set insert end; %W yview -pickplace insert}
bind Text <Prior> {# xf ignore me 9
  set xfCounter [lindex [%W config -height] 4]
  while {$xfCounter > 0} {
    %W mark set insert insert-1l
    incr xfCounter -1
  }
  %W yview -pickplace insert}
bind Text <Next> {# xf ignore me 9
  set xfCounter [lindex [%W config -height] 4]
  while {$xfCounter > 0} {
    %W mark set insert insert+1l
    incr xfCounter -1
  }
  %W yview -pickplace insert}


bind Entry <Right> {
    %W icursor [expr [%W index insert]+1]; tk_entrySeeCaret %W }
bind Entry <Left> {# xf ignore me 9
  %W icursor [expr [%W index insert]-1]; tk_entrySeeCaret %W }
bind Entry <Control-a> {# xf ignore me 9
  %W icursor 0; %W view 0}
bind Entry <Control-b> {# xf ignore me 9
  %W icursor [expr [%W index insert]-1]; tk_entrySeeCaret %W}
bind Entry <Control-d> {# xf ignore me 9
  %W delete [%W index insert]; tk_entrySeeCaret %W }
bind Entry <Control-e> {# xf ignore me 9
  %W icursor end; tk_entrySeeCaret %W}
bind Entry <Control-f> {# xf ignore me 9
  %W icursor [expr [%W index insert]+1]; tk_entrySeeCaret %W}
bind Entry <Control-h> {# xf ignore me 9
  tk_entryBackspace %W; tk_entrySeeCaret %W}
bind Entry <Meta-h> {# xf ignore me 9
  %W delete 0 insert; %W view insert}
bind Entry <Control-i> {# xf ignore me 9
  %W insert insert \t; tk_entrySeeCaret %W}
bind Entry <Control-k> {# xf ignore me 9
  %W delete insert end; tk_entrySeeCaret %W}
bind Entry <Control-l> {# xf ignore me 9
  tk_entrySeeCaret %W}
bind Entry <Control-v> {# xf ignore me 9
  %W insert insert "[tkW3EditSelectionGet]"; tk_entrySeeCaret %W}
bind Entry <Control-w> {# xf ignore me 9
  catch "%W delete sel.first sel.last"; tk_entrySeeCaret %W}
bind Entry <Control-y> {# xf ignore me 9
  %W insert insert "[tkW3EditSelectionGet]"; tk_entrySeeCaret %W}
bind Entry <2> {# xf ignore me 9
  %W insert insert "[tkW3EditSelectionGet]"; tk_entrySeeCaret %W}
