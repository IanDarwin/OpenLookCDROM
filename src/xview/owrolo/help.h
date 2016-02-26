/*	"@(#)help.h	2.3 8/14/88"	*/

/*
 *	The text of the help message.
 */



static char	*help_msg [] = {

"\
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
button over that item.\n\
Clicking the button directly is equivalent to\n\
selecting the default item from the button menus.\n\
Herewith is a quick overview of what the\n\
buttons do.\n\
ARROW BUTTONS\n\
        These buttons move you through the cards.\n\
The down arrow moves you to the next card in\n\
the file, the up arrow moves you backwards to\n\
the previous card .\n\
EDIT MENU\n\
New Card:\n\
        This creates a new blank card.  By\n\
default, the new card will be inserted after\n\
the currently displayed card.  Using the Shift\n\
key will cause it to be inserted before the\n\
current card.\n\
\n\
Delete:\n\
        This button deletes cards from your\n\
rolodex.  A click on this menu button by default\n\
deletes the card which is currently displayed.\n\
You can however select from the active list a\n\
collection of cards to delete. Deleted cards are\n\
not lost, they are saved on a stack of deleted\n\
cards and may be retrieved any time before Rolo\n\
exits.\n\
\n\
Undelete Last:\n\
        This undelete the last card which was\n\
previously deleted. It can be inserted either\n\
before or after the current card.\n\
\n\
Undelete...:\n\
        This pops up a new frame below the\n\
origial window, which will list the first line\n\
of all the cards on the deleted stack.  You may\n\
select any of these deleted cards directly from\n\
the list and they will re-inserted back into the\n\
active list of cards before or after the current\n\
card. \n\
\n\
        You can rearrange the order of the cards\n\
in your rolodex by deleting a card, moving to\n\
a new position, then undeleting it. Note that\n\
blanks cards containing only white space are\n\
not saved on the undeleted stack, they are\n\
thrown away.\n\
\n\
\n",

"\
VIEW BUTTON\n\
\n\
List:\n\
        This is the default action.  It displays \n\
an index list of all the cards in your rolodex.\n\
The first line of each card is displayed in the\n\
window so that you can get a quick overview of\n\
all your cards. Selecting a card and click\n\
\"show card\" will take you directly to that\n\
card.\n\
\n\
Help:\n\
        Displays the information you're reading now.\n\
\n\
Sort:\n\
        Sorts all the cards in ascending or\n\
descending alphabetical order according to\n\
their first lines.\n\
\n\
\n\
FILE MENU\n\
\n\
Load: From current file\n\
	Loads your rolodex cards from that file\n\
as-of the last time they were saved.(the file name\n\
listed in the title bar). You can also reload\n\
the cards from that file as-of the last time\n\
they were saved.\n\
\n\
Load: From named file\n\
            Pops a window, and prompts for a file\n\
    name. The default file is shown in the text field.\n\
    It then loads your rolodex cards from that file.\n\
\n\
Save: To current file\n\
            Saves your rolodex cards to that file\n\
    that the cards were loaded from.(default: ~/.rolo)\n\
\n\
Save: To  named file\n\
            Pops a window, and prompts for a file\n\
    name. The default file is shown in the text field.\n\
    It then saves your rolodex cards to that file.\n\
\n\
        Saving to or loading from a file \n\
causes that new file to become the default\n\
for the regular load and save operations.  You will\n\
be warned about unsaved modifications to your cards,\n\
overwriting existing files, etc.\n\
\n\
\n\
\n",

"\
FIND BUTTON\n\
   This button searches for a regular expression\n\
in your cards.  This can be used to find any\n\
text pattern which appears anywhere in any of\n\
your cards.  The pattern to search for may be\n\
entered in the text item to the right of the\n\
Find button.  If, at the time the Find\n\
button is clicked, with no text in to text item,\n\
the active selection will be used instead.\n\
      Pressing the return key in the search\n\
pattern item is equivalent to clicking the Find\n\
button but ignoring the selection.\n\
      Due to the way the library search\n\
routines work, all the text of a given card\n\
is treated as one logical line.  The\n\
main effect of this is that the '^' and '$'\n\
operations won't match individual lines within\n\
a card, only the text of the card as a whole.\n\
Note: \"Regular expressions\" are the kind used\n\
by egrep(1), which are not the same as the\n\
shell meta-characters.\n\
\n\
SLIDER\n\
       The slider item on the control panel may\n\
be used to select any card directly.  Simply\n\
drag the bar back and forth until the number\n\
of the card you wish to see is displayed to\n\
the left of the slider, then release the\n\
mouse button.\n\
Clicking the mouse in  buttons on the ends of\n\
the slider gets you to the first and the last\n\
card.\n\
\n\
DRAG AND DROP\n\
To load a file:\n\
        Drag a rolofile from the file manager and\n\
   drop it into the control panel or inside the\n\
   load-file pop window.\n\
To load a card:\n\
        Drag a mailfile or a file and drop it inside the\n\
    rolocard (textsw) region.\n\
\n\
Warning: There is no error checking on the file format\n\
or content (drag in garbage at your own risk).\n\
\n\
\n",

"\
AUTHOR\n\
   Ron Hitchens\n\
    ronbo@vixen.uucp, hitchens@cs.utexas.edu\n\
Modified by Phoebe Couch for OpenLook.\n\
	May 1989. Updated Oct,1989 \n\
\n\
   March  1987, version 1.0\n\
   August 1988, version 2.0\n\
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
\n\
  This version of Rolo has been ported to Xview\n\
and the user interface has been modified to\n\
conform to Openlook.\n\
 The code added is a product of Sun Microsystems, Inc. and is provided for\n\
 unrestricted use provided that this legend is included on all tape\n\
 media and as a part of the software program in whole or part.  Users\n\
 may copy or modify this code without charge.\n\
 \n\
 THIS CODE IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE\n\
 WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR\n\
 PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.\n\
 \n\
 This code is provided with no support and without any obligation on the\n\
 part of Sun Microsystems, Inc. to assist in its use, correction,\n\
 modification or enhancement.\n\
 \n\
 SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE\n\
 INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS FILE\n\
 OR ANY PART THEREOF.\n\
 \n\
 In no event will Sun Microsystems, Inc. be liable for any lost revenue\n\
 or profits or other special, indirect and consequential damages, even\n\
 if Sun has been advised of the possibility of such damages.\n\
 \n\
 Sun Microsystems, Inc.\n\
 2550 Garcia Avenue\n\
 Mountain View, California  94043\n\
\n\
If there are any port specific questions,\n\
please email to me at phoebe@sun.com.\n\
\n\
[RH 8/8/88]\n\
" };
