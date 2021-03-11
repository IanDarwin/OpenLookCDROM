\begindata{text,538489940}
\textdsversion{12}
\template{default}
\define{global
}
\majorheading{Link Preferences

}\center{Changing the Link Appearance

}
\quotation{\center{Abstract}

You can select one of two "look-and-feels" for a Link inset.

}
\heading{The Link Style Preference

}Link currently only supports one preference, \typewriter{linkstyle}, which 
should be an integer, either 0 or 2.  Set \typewriter{linkstyle} to 0 if you 
want links to look like regular, in-line text.  A \typewriter{linkstyle} of 2 
is the default, and causes Links to look like three-dimensional buttons.


You can change link styles on a global or per-application basis only.  You 
cannot, for instance, change the way links appear on a document or per-link 
basis.


To make a global change, put the following line in your preferences file:

\example{*.linkstyle: \italic{desired-style}

}where \italic{desired-style} is either 0 or 2.


To make a per-application change, put a line of the form:

\example{\italic{application}.linkstyle: \italic{desired-style}

}where \italic{application} is the name of the application (ez, table, 
\italic{etc}.) and \italic{desired-style} is again either 0 or 2.



\heading{Where can I find out more?}

The overview document for Link is 
\begindata{link,539282952}
Datastream version: 3
$DESTDIR/Link.d
0
0
\begindata{link,539282952}
Datastream version: 2
Link
2
andy12b
black
white
\enddata{link,539282952}
\enddata{link,539282952}
\view{linkv,539282952,759,0,0}.


\begindata{bp,537558784}
\enddata{bp,537558784}
\view{bpv,537558784,761,0,0}
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

}}\enddata{text,538489940}
