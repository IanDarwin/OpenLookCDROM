/*	"@(#)help.h	2.3 8/14/88"	*/

/*
 *	The text of the help message.
 */



static char	*help_msg [] = {

" \n\
      ROLO - Manage notes like a Rolodex\n\
\n\
   Each note in your Rolo file is meant to\n\
simulate a 3x5 card upon which you may scribble\n\
anything you like.  You can have any number of\n\
cards in your rolodex, Rolo will keep track of\n\
them for you.  You may browse around through\n\
them, create new ones, throw old ones away,\n\
search for strings in them, etc.\n\
\n\
   The buttons in the top part of the Rolo\n\
window provide the main controls with which\n\
you manipulate your cards.  Every button on\n\
the control panel has a menu attached which\n\
can be pulled up by pressing the right mouse\n\
button over that item.  The menus provide\n\
textual explanations of what the buttons do,\n\
as well showing the shift accelerators which\n\
may be used with the buttons.  Selecting from\n\
the button menus is equivalent to clicking the\n\
button directly.  Herewith is a quick overview\n\
of what the buttons do.\n\
\n",

"\
ARROW BUTTONS\n\
   These buttons move you through the cards.\n\
The down arrow moves you to the next card in\n\
the file, the up arrow moves you backwards to\n\
the previous card.  Holding down the Shift key\n\
when the down or up button is clicked, or\n\
choosing the second menu item, will jump you\n\
to the last or first card respectively.\n\
\n\
NEW CARD BUTTON\n\
   This button creates a new blank card.  By\n\
default, the new card will be inserted after\n\
the currently displayed card.  Using the Shift\n\
key will cause it to be inserted before the\n\
current card.\n\
\n\
TRASHCAN BUTTON\n\
   This button deletes and undeletes cards\n\
from your rolodex.  A normal unshifted click\n\
on this button deletes the card which is\n\
currently displayed.  Deleted cards are not\n\
lost, they are saved on a stack of deleted\n\
cards and may be retrieved any time before\n\
Rolo exits.  The alternate selections on the\n\
menu for the Trash button may be used to\n\
undelete any card which was previously deleted.\n\
These undelete menu items have pullright\n\
menus which will list the first line of all\n\
the cards on the deleted stack.  You may select\n\
any one of these deleted cards directly from\n\
the pullright menu and it will re-inserted\n\
back into the active list of cards before or\n\
after the current card.  If you select one\n\
of the undelete menu items directly, without\n\
pulling right to pick from the list of deleted\n\
cards, the card on the top of the deleted\n\
stack (the last one deleted) will be undeleted.\n\
Using Shift and Control key accelerators when\n\
clicking the button will give the same effect.\n\
If there are no deleted cards on the stack,\n\
the undelete items are marked as inactive.\n\
You can rearrange the order of the cards in\n\
your rolodex by deleting a card, moving to\n\
a new position, then undeleting it. Note that\n\
blanks cards containing only white space are\n\
not saved on the undeleted stack, they are\n\
thrown away.\n\
\n",

"\
LIST BUTTON\n\
   This button produces an index list of all\n\
the cards in your rolodex.  The first line\n\
of each card is displayed in the window so\n\
that you can get a quick overview of all\n\
your cards.  The card numbers to the left of\n\
each line can be used to jump directly to that\n\
card with the slider in the control panel.\n\
Alternately, the user can click on the \n\
desired entry followed by a <SHIFT>-click \n\
on the list button (or use the list button \n\
menu) to bring up the selected card.\n\
The \"List Cards Matching Regex\" menu\n\
entry produces an index list of cards\n\
matching the regular expression in the\n\
Expression text panel using the selected\n\
match type.\n\
\n\
FILE BUTTON\n\
   This button loads and saves your rolodex\n\
cards from and to disk.  The default action\n\
is to write the current list of cards out to\n\
the file they were loaded from (the file name\n\
listed in the title bar).  You can also reload\n\
the cards from that file as-of the last time\n\
they were saved.  There are also items on\n\
this menu which will sort your cards into\n\
alphabetical order according to their first\n\
lines.  By default, sorting is done in a case\n\
insensitive manner. The sort mode may be set\n\
by selecting the case [In]Sensitive pullright\n\
menu or by setting the \n\
       \"xrolo.CaseInsensitiveSort\"\n\
resource to 0 or 1. Xrolo implements a very \n\
rudimentary field sort. Each line in the card\n\
is considered to be a field. Pointing and\n\
clicking on a selected field before invoking \n\
the sort will cause xrolo to sort the cards\n\
by the selected field. The last item on\n\
the menu, which can only be selected from\n\
the menu, will load from or save to a named\n\
file.  You can select a string in some\n\
window to provide the file name, similar\n\
to the way the TextEdit tool\n\
works.  The path you give must be relative to\n\
your home directory (Rolo does a cd to $HOME\n\
when it starts up).  Saving to or loading from\n\
a file this way causes that new file to become\n\
the default for the regular load and save\n\
operations.  You will be warned about unsaved\n\
modifications to your cards, overwriting\n\
existing files, etc.\n\
\n\
HELP BUTTON\n\
   Displays the information you're reading now\n\
and allows you to send mail to me.\n\
\n\
PHONE BUTTON\n\
   On SparcStations this button can be used to\n\
dial a number entered in the expression field\n\
or highlighted with the mouse.  When selected\n\
the \"Set Phone Attributes\" menu item creates\n\
a panel that allows speaker volume adjustment,\n\
the entering of a dial prefix and the enabling\n\
or disabling of the dial prefix.  Phone\n\
numbers and dial prefixes may consist of any\n\
alpha numeric character, *, # and \",\". \n\
A comma in the dial string causes the dial\n\
routine to pause for 2 seconds before\n\
dialing the next character in the sequence.\n\
Xrolo supports several X resources used to\n\
to control default phone attributes.\n\
\"xrolo.DefaultVolume\" sets the default\n\
speaker volume. The volume must be entered\n\
as an integer value ranging in value from\n\
0 to 100 where 100 is the maximum gain.\n\
\"xrolo.UseDialPrefix\" accepts a boolean value\n\
which enables or disables the use of the dial\n\
prefix while dialing.\n\
\"xrolo.DialPrefix\" sets the default dial prefix\n\
string.\n\
\n\
FINISHED BUTTON\n\
   This button is used when you are finished\n\
using Rolo.  A normal click saves any changes\n\
you've made and causes Rolo to close to an\n\
icon.  Alternate selections will cause Rolo to\n\
exit with or without saving any pending\n\
changes you've made.  Rolo automatically\n\
saves any pending changes if SunTools\n\
forces it to exit.\n\
\n",

"\
PRINT BUTTON\n\
   This button is used to print. A normal click\n\
causes Rolo to print the current record. \n\
Alternate selections will cause Rolo to print\n\
all the entries in the current sorted order\n\
or set the print command. The default print\n\
command is \"lpr\".  A user can customize\n\
the default printer command by creating an\n\
X resource entry called\n\
     \"xrolo.PrintCommand\".\n\
The \"Print Records Matching Regex\" menu\n\
entry spools to the print command all cards\n\
matching the regular expression in the\n\
Expression text panel using the selected\n\
match type.\n\
\n\
FIND BUTTON\n\
   This button searches for a regular expression\n\
in your cards.  This can be used to find any\n\
text pattern which appears anywhere in any of\n\
your cards.  The pattern to search for may be\n\
entered in the text item to the right of the\n\
Find button.  If, at the time the Find\n\
button is clicked, there is an active selection\n\
in any window, it will be used as the search\n\
pattern instead.  Pressing the return key in\n\
the search pattern item is equivalent to\n\
clicking the Find button.  Due to the way the\n\
library search routines work, all the text of a\n\
given card is treated as one logical line.  The\n\
main effect of this is that the '^' and '$'\n\
operations won't match individual lines within\n\
a card, only the text of the card as a whole.\n\
Note: \"Regular expressions\" are the kind used\n\
by egrep(1), which are not the same as the\n\
shell meta-characters. By default xrolo will\n\
do a case insensitive (or \"sloppy\") regular\n\
regular expression search.  If soundex search\n\
mode is selected, xrolo will try to match an\n\
entry which sounds like the expression\n\
(e.g. \"cuper\" will match \"cooper\").\n\
The default search mode may be changed by\n\
selecting \"Egrep Regex Match\" or \"Soundex\"\n\
from the find menu or by setting the X\n\
resource \n\
    \"xrolo.SearchType\"\n\
to 0, 1 or 3 for EGREP, SLOPPY or SOUNDEX\n\
searches respectively.\n\
\n\
SLIDER\n\
   The slider item on the control panel may\n\
be used to select any card directly.  Simply\n\
drag the bar back and forth until the number\n\
of the card you wish to see is displayed to\n\
the left of the slider, then release the\n\
mouse button. Alternately, the record number\n\
may be entered directly by typing in the\n\
current record field\n\
\n\
\n",

"\
RESOURCES\n\
   Xrolo supports a number of resources which\n\
include:\n\
    xrolo.TextWidth - set width of text window\n\
          in pixels.\n\
    xrolo.CaseInsensitive - 1 causes xrolo to\n\
         ignore case when sorting. 0 turns on\n\
         case sensitive sorts.\n\
    xrolo.PrintCommand - text string used to\n\
         set the default print command.\n\
    xrolo.SearchType - default search mode for\n\
         find. 0, 1 or 2 for EGREP, SLOPPY or\n\
         SOUNDEX search modes respectively.\n\
    xrolo.UseDialPrefix - 1 causes xrolo to\n\
         use the dial prefix when dialing.\n\
    xrolo.DefaultVolume - volume gain for\n\
        SS1 internal speaker.  The gain must\n\
        be entered as an integer value between\n\
        0 and 100 where 100 is the maximum gain.\n\
    xrolo.DialPrefix - text string used to set\n\
        the default dial prefix.\n\
\n",

"\
AUTHOR\n\
   Ron Hitchens\n\
    ronbo@vixen.uucp, hitchens@cs.utexas.edu\n\
   And\n\
   Luis Soltero\n\
    luis@rice.edu\n\
\n\
   March  1987, version 1.0\n\
   August 1988, version 2.0\n\
\n\
   Ported to OpenWindows by  Luis Soltero\n\
   (luis@rice.edu) November 1990\n\
\n\
   Thanks to S. Page (spage@sun.com) and\n\
Mike Ekberg (mae@sun.com) at Sun for changes\n\
and improvements to the original Rolo.\n\
\n\
\n\
   Rolo is public domain, anyone and everyone\n\
is welcome to it.  Feel free to give it to your\n\
friends (or even your enemies).  I only ask\n\
that you keep it all together, source included,\n\
as posted.  Please send bug reports, fixes,\n\
gripes, suggestions, etc, to me at the email\n\
address above.\n\
\n\
[RH 8/8/88]\n\
" };
