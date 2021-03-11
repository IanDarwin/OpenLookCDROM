\begindata{text,17531264}
\textdsversion{12}
\template{help}
\define{global
}
\chapter{Link Inset:  A Static Hyperlink

}
\section{What Link is

}\leftindent{
The \bold{link} inset is a button that resides in the text of a document. It 
``links'' one document to another: a new window containing a target document 
is created when the link inset is clicked on with the left mouse button.  Link 
is a basic hypermedia building block.


Use Delete Window (\bold{Ctrl-X Ctrl-D}) to remove the new window.  See also 
\bold{Warnings}.

}
\section{Starting Link

}\leftindent{
You can ``follow'' a link by left-clicking on the link inset.  By default, a 
link inset looks like a shaded button, but its appearance may be changed with 
the linkstyle preference.  See the  \italic{\helptopic{preferences}}  help 
document for more information on setting preferences.


You insert a link in a document in the same manner as any other inset.  Type 
ESC-Tab, and answer ``link'' to the prompt ``Data object to insert here''. 
 See the  \italic{\helptopic{inset} } help document for more information on 
inserting insets into documents.

}
\section{Warnings

}\leftindent{
Following a link results in the creation of a new window, with the target of 
the link in that new window.  Note that with some window managers (e.g.:  twm 
on X-Windows), the new window may appear directly on top of the window in 
which you invoked the link--just move the new window to see the old one.


Also, note that \bold{\italic{to remove a window}} created by following a 
link, \bold{\italic{do \underline{not}}} use Quit (either from the menu or a 
keystroke sequence, most likely bound to Ctrl-X Ctrl-C), or ``Zap'' the 
window, as this will quit the application (EZ, Help, Messages, etc.) from 
which you invoked the link.  You want to use \bold{Delete Window} (usually 
bound to the keystroke sequence \bold{Ctrl-X Ctrl-D}).

}
\section{Pop-up menu meanings

}
\leftindent{Link provides only one menu card, entitled Link, for configuring 
links.  The menu card becomes visible when the link is selected.  You can 
select a link without bringing up the target by right-mouse-clicking on the 
link inset.  These are the options:


\bold{Autolink Target Here}:  Select this menu option from the target window, 
after selecting ``\bold{Autolink''} on the source link.


\bold{Autolink}:  Select this menu option on the source link, and then specify 
the target window with the ``\bold{Autolink Target Here''} menu option.  If 
you can't find ``\bold{Autolink Target Here''}, you need to modify your 
.ezinit file.


\bold{Set Link}:  Specify the name of the target file for this link manually 
(assisted by the completion package).  The filename you specify can have 
\italic{environment variables} (such as $HOME and $ANDREWDIR) within them.


Because link is a subclass of the pushbutton inset, you may use the Pushbutton 
menu to program link as well.

}
\section{How Link works, Using Link, Link Concepts, The default Link, 
Preferences, Programming Link

}\leftindent{
For more information on these topics, 
\begindata{link,17531648}
Datastream version: 3
$DESTDIR/Link.d
0
0
\begindata{link,17531648}
Datastream version: 2
Press Me
2
andysans8b
black
white
\enddata{link,17531648}
\enddata{link,17531648}
\view{linkview,17531648,20,0,0}.

}
\section{Program Author}

\leftindent{
Michael McInerny, ITC (address mail to mcinerny+@andrew.cmu.edu).}


\section{Related tools}  


Select (highlight) one of the italicized names and choose "Show Help on 
Selected Word" from the pop-up menu to see the help file for:


\leftindent{\italic{\helptopic{pushbutton

insets

ez}}}


\begindata{bp,17519360}
\enddata{bp,17519360}
\view{bpv,17519360,21,0,0}
Copyright 1992 Carnegie Mellon University and IBM.  All rights reserved.

\smaller{\smaller{$Disclaimer: 

Permission to use, copy, modify, and distribute this software and its 

documentation for any purpose is hereby granted without fee, 

provided that the above copyright notice appear in all copies and that 

both that copyright notice, this permission notice, and the following 

disclaimer appear in supporting documentation, and that the names of 

IBM, Carnegie Mellon University, and other copyright holders, not be 

used in advertising or publicity pertaining to distribution of the software 

without specific, written prior permission.



IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 

DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 

ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 

SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 

BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 

DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 

WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 

ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 

OF THIS SOFTWARE.

 $

}}\enddata{text,17531264}
