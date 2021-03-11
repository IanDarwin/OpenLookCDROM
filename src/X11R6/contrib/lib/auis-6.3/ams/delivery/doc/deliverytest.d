\begindata{text,538387092}
\textdsversion{12}
\template{default}
\define{italic
menu:[Font,Italic]
attr:[FontFace Italic Int Set]}
\define{bold
menu:[Font,Bold]
attr:[FontFace Bold Int Set]}
\define{chapter
menu:[Title,Chapter]
attr:[Justification LeftJustified Point 0]
attr:[FontFace Bold Int Set]
attr:[FontSize PreviousFontSize Point 4]}
\define{section
menu:[Title,Section]
attr:[Justification LeftJustified Point 0]
attr:[FontFace Bold Int Set]
attr:[FontSize PreviousFontSize Point 2]}
\define{subsection
menu:[Title,Subsection]
attr:[Justification LeftJustified Point 0]
attr:[FontFace Bold Int Set]}
\define{paragraph
menu:[Title,Paragraph]
attr:[Justification LeftJustified Point 0]
attr:[FontFace Italic Int Set]}
\define{bigger
menu:[Font,Bigger]
attr:[FontSize PreviousFontSize Point 2]}
\define{indent
menu:[Region,Indent]
attr:[LeftMargin LeftMargin Inch 32768]
attr:[RightMargin RightMargin Inch 32768]}
\define{typewriter
menu:[Font,Typewriter]
attr:[FontFace FixedFace Int Set]
attr:[FontFamily AndyType Int 0]}
\define{display
menu:[Region,Display]
attr:[LeftMargin LeftMargin Inch 32768]
attr:[RightMargin RightMargin Inch 32768]
attr:[Justification LeftJustified Point 0]}
\define{example
menu:[Region,Example]
attr:[LeftMargin LeftMargin Inch 32768]
attr:[Justification LeftJustified Point 0]
attr:[FontFace FixedFace Int Set]
attr:[FontFamily AndyType Int 0]}
\define{description
menu:[Region,Description]
attr:[LeftMargin LeftMargin Inch 32768]
attr:[Indent LeftEdge Inch -32768]}
\define{quotation
menu:[Region,Quotation]
attr:[LeftMargin LeftMargin Inch 32768]
attr:[RightMargin RightMargin Inch 32768]
attr:[FontFace Italic Int Set]}
\define{subscript
menu:[Font~1,Subscript~31]
attr:[Script PreviousScriptMovement Point 2]
attr:[FontSize PreviousFontSize Point -2]}
\define{superscript
menu:[Font~1,Superscript~30]
attr:[Script PreviousScriptMovement Point -6]
attr:[FontSize PreviousFontSize Point -2]}
\define{smaller
menu:[Font,Smaller]
attr:[FontSize PreviousFontSize Point -2]}
\define{heading
menu:[Title,Heading]
attr:[LeftMargin LeftMargin Inch -13107]
attr:[Justification LeftJustified Point 0]
attr:[FontFace Bold Int Set]}
\define{majorheading
menu:[Title,MajorHeading]
attr:[Justification Centered Point 0]
attr:[FontSize PreviousFontSize Point 4]}
\define{formatnote
menu:[Region,FormatNote]
attr:[Flags PassThru Int Set]}
\define{subheading
menu:[Title,Subheading]
attr:[Justification LeftJustified Point 0]
attr:[FontFace Bold Int Set]}
\define{center
menu:[Justify,Center]
attr:[Justification Centered Point 0]}
\define{flushleft
menu:[Justify,FlushLeft]
attr:[Justification LeftJustified Point 0]}
\define{flushright
menu:[Justify,FlushRight]
attr:[Justification RightJustified Point 0]}
\define{leftindent
menu:[Region,LeftIndent]
attr:[LeftMargin LeftMargin Inch 32768]}
\majorheading{Testing Script for Mail Delivery System}

\center{24 November 1986

C. F. Everhart

\italic{\smaller{file: in RCS directory andrew/doc/itc/ams/progl, file 
deliverytest.d}}}


\smaller{This script tests the basic functioning of the programs queuemail, 
trymail, switchmail, and vicemail, all of which are currently executed out 
of /usr/andrew/etc/.


All of these test procedures assume that your Andrew userid is 
\italic{foo}.  Whenever the word foo is used in a script, you should 
replace that word with your own Andrew userid (e.g., cfe, or pp9r, or 
zt54).}


\heading{Queuemail}

\smaller{The program /usr/andrew/etc/queuemail is used whenever mail is 
entered in the mail delivery system to be sent.  Test it by seeing if it 
delivers or enqueues mail properly.  (Generally, to test whether mail was 
delivered, you will have to be able to look in the destination mailbox. 
 Thus, for the most part, you'll have to test whether mail was delivered to 
yourself.)


\italic{q.1.}  Type into a command window the command

\example{/usr/andrew/etc/queuemail -f /.hostname foo; echo $status}

You should see the digit 0 printed as the result status of this command, 
and you should receive a very short piece of mail consisting of a 
Return-Path: header line, a Received: header line (which may continue to 
more than one physical line in the mail, as usual), and the name of the 
workstation.


\italic{q.2.}  Type the following command into a command window:

\example{\smaller{echo "Test."|/usr/andrew/etc/queuemail -i -r "<foo>" foo; 
echo $status}}

You should again see the digit 0 printed as the result status of this 
command.  This time, the short piece of mail should consist of a 
Return-Path: header line, a Received: header line, and the text ``Test.''}


\heading{Trymail and Switchmail}

\smaller{The programs /usr/andrew/etc/trymail and 
/usr/andrew/etc/switchmail are versions of the same underlying program. 
 Trymail is the program that executes on the workstation and attempts to 
deliver mail to local Andrew addressees.  Switchmail performs a similar 
function on post office machines, but it also knows how to queue network 
mail and return error messages to mail senders.


For the most part, it is simplest to test trymail not by delivering lots of 
mail, but by getting it to tell you where the mail would have gone if it 
had completed its delivery.


Trymail and switchmail need you to give a list of addressees to them via 
their standard input.  What this means is that you should start them with a 
command in a command window, then type the given set of addressees and 
follow them with a control-D.  Thus, for the first example given below, you 
should type the line that begins ``/usr/andrew/etc/trymail'' to a shell 
prompt in a command window, then you should type the line with the 
hash-mark, and then you should type a control-D (hold down the CTRL key and 
strike the D key) on a third line.


\italic{t.1.}  Type the following command into a command window:

\example{/usr/andrew/etc/trymail -F -T /.hostname "<foo>"

foo#}

\leftindent{(control-D)}

You should see the messages

\example{170 foo+@andrew.cmu.edu (Requested not to send)

999 End.}

appear.


\italic{t.2.}  Type the following command into a command window:

\example{/usr/andrew/etc/trymail -F /.hostname "<foo>"

foo#}

\leftindent{(control-D)}

You should see the messages

\example{101 foo+@andrew.cmu.edu (Your Andrew Name)

999 End.}

appear.  Furthermore, you should now have a new piece of mail in your 
Andrew mailbox, consisting, again, of a Return-path: header line, a 
Received: header line, and the name of your workstation.


\italic{t.3.}  Type the following command into a command window:

\example{/usr/andrew/etc/trymail -F -T /.hostname "<foo>"

owjeroijweori}

\leftindent{(control-D)}

You should see the messages

\example{229 owjeroijweori@andrew.cmu.edu (User name not recognized)

999 End.}

appear.


\italic{t.4.}  Type the following command into a command window:

\example{/usr/andrew/etc/switchmail -F -T /.hostname "<foo>"

owjeroijweori}

\leftindent{(control-D)}

After a slight delay, you should see the messages

\example{190 owjeroijweori@andrew.cmu.edu (No such addressee; error message 
sent OK)

999 End.}

appear.  Furthermore, in your mailbox should be a note from the 
Mailer-daemon explaining that there is no such addressee as 
``owjeroijweori@andrew.cmu.edu'', and enclosing the entire text of your 
mail, which consists solely of the name of your workstation.


\italic{t.5.}  Type the following command into a command window:

\example{/usr/andrew/etc/trymail -F -T /.hostname "<foo>"

c.f.everhart}

\leftindent{(control-D)}

You should see the messages

\example{170 cfe+@andrew.cmu.edu (Requested not to send)

999 End.}

appear.


\italic{t.6.}  Type the following command into a command window:

\example{/usr/andrew/etc/trymail -F -T /.hostname "<foo>"

c.f.everhart#}

\leftindent{(control-D)}

You should see the messages

\example{229 c.f.everhart+@andrew.cmu.edu (User name not recognized)

999 End.}

appear.


\italic{t.7.}  Type the following command into a command window:

\example{/usr/andrew/etc/trymail -F -T /.hostname "<foo>"

cfe+foo, c.f.everhart, cfe+bar, bark@dog, meow@cat, bark@dog}

\leftindent{(control-D)}

You should see the messages

\example{170 cfe+foo@andrew.cmu.edu (Requested not to send)

210 bark@dog (non-local host)

210 meow@cat (non-local host)

999 End.}

appear.  (The ordering of the messages before ``999 End.'' is not 
significant.)}


\heading{Vicemail}

\smaller{Vicemail is a simple program that in some circumstances performs 
final delivery of mail into the Vice file system.


\italic{v.1.}  Type into a command window the command

\example{echo "Test" | /usr/andrew/etc/vicemail foo; echo $status}

You should see the digit 0 printed as the result status of this command, 
and you should receive a very short piece of mail consisting of a 
Return-Path: header line, a Received: header line (which may continue to 
more than one physical line in the mail, as usual), and the word ``Test''.


\italic{v.2.}  Type into a command window the command

\example{echo "Test" | /usr/andrew/etc/vicemail vxzutrp9; echo $status}

You should see the number 67 (``no such user'') printed as the result 
status of this command.  You may also generate a piece of warning mail for 
the postmaster.

}
\begindata{bp,538210560}
\enddata{bp,538210560}
\view{bpv,538210560,0,0,0}
Copyright 1988, IBM and Carnegie Mellon University.  All rights reserved.

$Disclaimer: 
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

\enddata{text,538387092}
