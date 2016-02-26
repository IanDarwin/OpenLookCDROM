## main.tcl main file for tkWWW  user interface
## ==============
## Copyright (C) 1992-1993
## Globewide Network Academy
## Macvicar Institute for Educational Software Development
##
## See the file COPYRIGHT for conditions 

# If the binary variable is not set, then set it to assume that we are
# not in the binary

if ![info exists tkW3BinaryInBinary] {
    global tkW3BinaryInBinary
    set tkW3BinaryInBinary 0
}

## *************************
## This is the directory containing the source code
## It is retrieved from an environment variable set 
## in the shell script tkWWW
## *************************

# skip this section if we are in a binary

if !$tkW3BinaryInBinary {
    set tkW3SourcePath $env(TK_WWW_SRC_DIR)
    set tkW3Version "0.11"

    source $tkW3SourcePath/init.tcl

    # Add the source path to the auto load table
    set auto_path "$tkW3SourcePath $auto_path"
}

# Forbid rename for security sake
rename rename ""

## *************************
## Parse the command line
## *************************

tkW3ParseArgs $argv

## ************************
## Check for incompatibilties between frontend and backend
## ************************

tkW3VersionVerify $tkW3Version

## *************************
## source user configuration file
## *************************

catch {source $tkW3ConfigFile}

# # *************************
# # Window management and Interface code 
# # *************************
wm title . "tkWWW"
wm minsize . 0 0
wm iconbitmap . $tkW3ConfigLogo

pack append . \
     [ tkW3MenuMakeMenus .menu $tkW3ConfigMenus ] {top fillx} \
     [ tkW3OutputMakeTitleBox .titles ] {top fillx} \
     [ tkW3OutputMakeText .f ] {top expand fill } \
     [ tkW3OutputMakeButtons .buttons $tkW3ConfigButtons ] {top fillx} \
     [ tkW3OutputMakeEntries .style $tkW3ConfigEntries ] {top fillx } \
     [ tkW3OutputMakeToggles .toggles $tkW3ConfigToggles ] {top fillx} 

tkW3OutputMakeScrollbars


## *************************
## Do initializations
## *************************

tkW3NavigateInitialize
tkW3FontInitialize
tkW3FileInitialize
tkW3EditInitialize
tkW3HistoryInitialize
tkW3PanInitialize
tkW3HelpInitialize
tkW3BookmarksInitialize
FSBoxInitialize

## *************************
## GUI is created
## Set the welcome message and
## Jump to the first page
## *************************

tkW3NavigateGoto $tkW3ConfigStartPage

if {$tkW3ParseArgsShowInfo != 0} {
	tkW3HelpAbout
}

## enable image links
if {$tkW3ParseArgsImages(anchor) != ""} {
    tkW3ImageEnableAnchor \
 	$tkW3ParseArgsImages(anchor) $tkW3ParseArgsImages(ismap)

}
