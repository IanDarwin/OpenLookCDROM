\begindata{text,538322772}
\textdsversion{12}
\template{default}
\define{global
}
\majorheading{Link Developer Documentation}

\center{How to Make Linked Documents}


\quotation{\center{Abstract}

The Link inset provides a simple, fast way of building a set of 
cross-referenced hypermedia documents.  Using a Link inset requires no 
programming and can be done by anyone.

}
\heading{Why do I want to use Links?}

By using Linked documents, you, the document developer, can tie together large 
amounts of information which can be conveniently navigated by users of your 
document (for a description of the user interface, see 
\begindata{link,539353224}
Datastream version: 3
$DESTDIR/Help.d
0
0
\begindata{link,539353224}
Datastream version: 2
Link Help
2
andysans8b
black
white
\enddata{link,539353224}
\enddata{link,539353224}
\view{linkv,539353224,775,0,0}).  Links also give you new freedom in 
structuring your documents, because with links, you can break large pieces of 
information into smaller, more manageable chunks which cross-reference each 
other.


The target of a link is a filename which may contain \italic{environment 
variables} such as $HOME and $ANDREWDIR.  The value of the variable is used 
when the link is traversed, in order to construct the complete target 
filename.  Thus, for example, a link pointing to "$HOME/preferences" will 
always take the user to his or her \italic{own preferences file} because the 
value of $HOME is always the current user's home directory.


Links are a file-reference tool.  They do not support offsets into documents, 
and so it is recommended that links be used to help users find whole 
documents, rather than pieces of it. (For instance, Links wouldn't be useful 
for producing a table of contents warp for a large document.  However, if that 
document is broken up into chapters and sections, then a table of contents 
link document could be used to reference each of the pieces of the document.)


\heading{How do I insert a Link?}

\quotation{Note:  The following procedure assumes that you have customized 
your ".ezinit" file to allow for Autolinking.  If you haven't, please see 
\begindata{link,539283208}
Datastream version: 3
$DESTDIR/Procs.d
0
0
\begindata{link,539283208}
Datastream version: 2
Link Procs
2
andysans8b
black
white
\enddata{link,539283208}
\enddata{link,539283208}
\view{linkv,539283208,776,0,0} before continuing.}

Links are inserted just like any other inset.  For instance, most ATK objects 
capable of holding insets (like text) use "Esc-Tab" to specify the inset.  The 
name of the Link dataobject is "link", so, to insert a link in a text 
document, you would:

\indent{\description{- select an insertion point (where you want the Link 
inset to appear)

- press the "Esc" key and then the "Tab" key

- type "link" and press the "Enter" key}}

At this point, you should see an empty Link inset (which has no target, and no 
label, displaying itself as 
"\
\begindata{link,539283336}
Datastream version: 3

0
0
\begindata{link,539283336}
Datastream version: 2
Link (v30)
2
andy12b
black
white
\enddata{link,539283336}
\enddata{link,539283336}
\view{linkview,539283336,777,0,0}").  The easiest way to specify the target 
(and a label for the link) is to use the "Autolink" procedure.  To do this, 
with the Link inset selected:

\indent{\description{- press the menu button on your mouse (the middle button 
on a three-button mouse or the left and right buttons together on a two button 
mouse) and choose "Autolink" from the menu.

- move the mouse to the window of the file you want to link to, and bring up 
the menus again.  Choose "Autolink target here".}}

At this point, the Link inset will change its name to the filename of the 
target document, and it will be ready for use as a Link.  To test it, just 
click your left mouse button over the link inset, and you should be warped to 
the target document.  You can insert as many links as you want into a 
document, and you can link to any other document (even system files)--you can 
even link to the same document the link is in (although it's of dubious worth, 
since there are no offests).


\heading{How do I change a Link?}

You can autolink again, but it won't change the label.  The reason for this is 
that you may want to set your label manually before autolinking, and the 
autolink procedure won't change an already defined label.  You can change the 
label manually using the Set Label menu item (on the Pushbutton card), which 
will prompt you for a new text string.  You can also change the font, using 
the Set Font menu item.  Finally, you can set the link manually, using the Set 
Link menu item, which uses the "completion" package to help you specify a 
filename.  The Set Link procedure will also set the label of the link if there 
isn't already one defined (like autolink).  You may include 
\italic{environment variables} as part of the filename (see above), but if you 
use variables, file completion will not work.  Environment variables may be 
specified as $VARIABLE, $(VARIABLE), or $\{VARIABLE\}.


\heading{How do I remove a Link?}

You can remove a Link inset like any other inset:  just select it and choose 
the cut option from the menu, or backspace over it (and confirm the dialog 
prompt).


\heading{Caveats}

\description{You shouldn't put Links (or any other inset or style) into system 
documents (like programs and init files).  This will cause the document to 
become unreadable by the system.

Don't change the name of a link to the empty string.  It will become invisible 
and very hard to select again.  You will probably need to backspace over it.}


\heading{How can I find out more?}

You can use the overview document 
\begindata{link,539283080}
Datastream version: 3
$DESTDIR/Link.d
0
0
\begindata{link,539283080}
Datastream version: 2
Link
2
andy12b
black
white
\enddata{link,539283080}
\enddata{link,539283080}
\view{linkv,539283080,778,0,0} to find out more about Links.


\begindata{bp,537558784}
\enddata{bp,537558784}
\view{bpv,537558784,780,0,0}
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

}}\enddata{text,538322772}
