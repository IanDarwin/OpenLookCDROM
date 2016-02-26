## edit.modified.tcl Some auxilary procedures for edit
## ==============
## Copyright (C) 1992-1993
## Globewide Network Academy
## Macvicar Institute for Educational Software Development
##
## See the file COPYRIGHT for conditions

## ********************
## Set/Reset and check Modified flag
## ********************

proc tkW3EditModifiedSet {{on 1}} {
    ## Parameter on : 0 = off ; else on
    global tkW3HtPage
    set tkW3HtPage(modified) $on
    tkW3ConfigDisplay
 }

proc tkW3EditModifiedCheck {} {
     global tkW3HtPage
     if {$tkW3HtPage(modified) == 0} {return 1}

     if {[DLG:msg . .confirm_action_dialog \
        "You didn't save this file! Do you want to discard your changes?"\
        question "Discard" "Cancel"] == 1} {
            return 1
        }
     return 0
 }
