\begindata{text,537620288}
\textdsversion{12}
\template{default}
\majorheading{VUI Internals

}
\center{Author - Mark Chance

Information Technology Center

Pittsburgh, PA

10 February 1988

}
\heading{Abstract


}This document describes the internal data structures and algorithms of the 
VUI program.  For information about facilities heavily used by this program 
the reader is referred to the following:


ams.help on Andrew (issue help ams)


\italic{LibraryCalls.d} and \italic{ServerCalls.d} by N. S. Borenstein\italic{

}
\italic{PanelsNMenus.d} by M. Chance, et al.

\italic{
HowToPC.d} by M. Chance


andrew.ms.tech bulletin board


\heading{Background

}\
\begindata{zip,537665536}
%ViewWidth 570
%ViewHeight 427
*D;-700,900
>-1100,900;-1100,100
*A;-600,900
TMS\nS\nt\nu\nb
MCM
*C;-700,1100
>-700,100
*D;400,-600
>0,-600;0,-1200
*D;700,-400
>400,-400;400,-1200;400,-1200
*A;0,1300
Fandy14b
TTwo Process Version
MCM
*A;0,-100
Fandy14b
TOne Process Version
MCM
*A;900,-600
TA\nF\nS
MCM
*C;700,-200
>700,-1200
*G;-900,-200
>1000,-1200
*A;1700,700
TA\nF\nS
MCM
*C;1500,1100
>1500,100
*A;-500,-700
TClient
MCM
*A;200,-900
TCUI
MCM
*A;500,-700
TMS
MCM
*A;1000,700
TMessage\nServer\n
MCM
*A;500,800
Ts\nn\na\np\n
MCM
*A;-300,800
Ts\nn\na\np\n
MCM
*A;-900,600
TCUI\nLibrary\n
MCM
*A;-1500,700
TClient\nCode\n
MCM
*D;400,900
>600,900;600,300;400,300
*D;-200,900
>-400,900;-400,300;-200,300
*C;400,600
>300,700
*C;300,500
>400,600
*C;-200,600
>-100,500
*C;-100,700
>-200,600
*C;-200,600
>400,600
*A;900,1200
Fandysans12
THost\n
MCM
*A;-1200,1200
Fandysans12
TPC\n
MCM
*G;400,1100
>1800,100
*G;-1800,1100
>-200,100
*D;-1000,1400
N8.5X11
>-1000,1400

\enddata{zip,537665536}
\view{zipview,537665536,0,572,429}
AFS - Andrew File System

MS - Message Server

CUI - Common User Interface (a program as well as a library)

SNAP - Simple Network Access Protocol


\heading{Overview of the files

}
vui.h - \leftindent{This file contains all the structures and constants used 
by the program.}


vui.c - \leftindent{This file has all the high level loops and menu processing 
code.  It depends heavily on vuipnl and vuibase.  It includes vuimenus for 
command identification.}


vuimenus.h - \leftindent{This file has all the declaration of data structures 
for the menu hierarchies.  If you want to change the menus all you have to do 
is rearrange the options defined in this file.  The calling program sees none 
of the hierarchy. You should be careful to preserve the numbering since the 
help files that are displayed are keyed to the index number.}


vuiscrns.c - \leftindent{This file has all the declarations of panels.  There 
is (almost) no executable code here.}


vuipnl.c - \leftindent{This file has most all the routines that deal with 
maintaining the display.  The file relies heavily on the paneling and menu 
code.  You will find a great deal of symmetry in the sections of code that 
deal with the display and selection of folders, captions, and message bodies.}


vuibase.c - \leftindent{This file has the majority of the code that deals with 
the message system.}


*.f - \leftindent{These files contain function prototypes.  The worst problem 
in porting to the PC is that pointers and ints are 32 bytes on the RT and 16 
bytes on the PC.  This creates problems when used in function arguments.  By 
defining function prototypes the compiler knows when to convert sizes.}


\heading{Folder structures and processing

}
At the time this code was written folders were still being called directories, 
so all the routines and data that start with DIR refer to folder things.


There are flags kept around to indicate various items.  \italic{Mailorbb} 
tells whether we are looking at a list of bboards or mail folders.  The reason 
is that we don't want to present delete options, etc. on the list of bboards. 
 The user can select mail or bboards.  If bboards they could want subscribed 
bboards or all.  If they want all, they could want all that match a certain 
pattern.  To maintain all that status we have the \italic{mailorbb}, 
\italic{suborall}, and \italic{template} variables.  Also if the user is only 
looking at subscribed bboards with new messages, they would like to have an 
assist in sequentially going through all new messages.  For this reason we set 
the \italic{autoorman} flag appropriately.


Rather than storing the entire list of directories in memory the program 
relies on files built by the message server and stored on its disk.  These are 
called "map files".  The server generates these files based on a path entry. 
 At one time it was the case that you specified in preferences a list of paths 
of messages you wanted to read.  This was to provide for private bulletin 
boards, your mail, etc.  So each of these had an entry number.  The 
\italic{dir_page_list} structure stores the entry number and an offset into 
the message server file for each screenful of directories as well as forward 
and backward pointers through the chain.  There are a set of routines that map 
the entry number to message server file name.  These include 
ClearMapFileNames, GenMapFileName, CacheMapFile, etc.  Each of these map files 
has the short name, the long name, and subscription status of each directory. 
 The code goes through some gyrations to keep some kind of real-time status on 
subscriptions.  This means that when the user un-subscribes to a particular 
directory, they would like to see that reflected on the screen.  So we go 
mucking through the map file to re-write the status code, to avoid the server 
having to rebuild the file.


The process for creating the on-screen list of folders consists of reading a 
map file.  Given an entry number, the program comes up with a map file name 
and starts reading it at a certain offset.  Each line is read and checked to 
see if it matches the current criteria for display (LoadOneDirPage).  When a 
screenful is reached, the \italic{dir_page_list} is updated with the starting 
offset for the next page.


\heading{Caption structures and processing

}
All routines and data items that start with "msg" refer to captions.  For 
early versions, the \italic{msg_page_list} structure was used to keep track of 
the caption screens, but now that messages are available by number, there is 
no list required.


You will find some "date" variables in this section of code.  The reason is 
that in versions 1.x the user could ask for messages since a particular date. 
 This has been eliminated in versions 2.x.  Now everything is by message 
number.  The message number is from one to N.  A different number is the 
\italic{cuid}.  This is an identifier stored by the server and relates more to 
when the caption is fetched.  If you ask for messages starting at the 28th, it 
will create \italic{cuids} 1 through 20 (if you ask for 20 of them).  Since 
the \italic{cuid} is part of the snapshot (a data structure for each caption 
created by the server), there are functions to get the \italic{cuid} from the 
message number.  The main reason for this is that many of the library routines 
do things based on the \italic{cuid}.


For messages in folders which you have write-access, the system maintains a 
set of flags.  Currently the only ones VUI worries about are unread and 
deleted.  These are denoted on the captions screen by "N" and "D" 
respectively.  When a caption is read or deleted, the line of the screen is 
updated with the appropriate flag information.


When reading bboards we like to not have to read messages twice, so there is 
the "update message number".  This is equivalent to the line that Messages 
puts between captions.  Here we use an asterisk to denote which message is the 
most recent one read.  When the user is done reading bboards, we tell the 
message server to update the user's profile with the date of the update 
message number.


\heading{Message body structures and processing

}
This section of code is more complex than might be imagined for two reasons: 
 1. the entire message conforms to RFC 822 format specifying headers and body, 
and 2. Andrew Toolkit extended data stream (i.e. BE2 format).


The first complexity means that there any number of arbitrary headers of the 
form Header: value.  These may be continued from line to line by starting the 
line with a tab character.  The headers are separated from the body of the 
message by a blank line.  This means two new line characters in a row.  By 
itself this is no big deal, but of course nobody wants to see all these 
headers so we have a mechanism known as filtering.  The simplistic mechanism I 
came up with was to store a list of headers of interest and a flag that says 
whether these are to be deleted or included.  As you will find, the default is 
to keep Subject, From, Date, To and CC.  Some message system gurus prefer to 
delete a list of headers since that lets them see new headers that may be 
coming in across the network.  Much of the code in LoadOneBodyPage is devoted 
to this filtering process.


The good news is that the second of these complexities is taken care of in one 
fell swoop, MS_WriteUnscribedBodyFile.  This routine has the server filter out 
all the badness that we currently cannot handle on non-graphic displays.


The program uses the \italic{msg_page_list} structure to keep track of the 
pages in displaying message bodies.  Here, the offset refers to the actual 
offset in the file where each new page starts.  We keep track of where in the 
file we saw the end of the headers so that depending on where we are in the 
file we know whether or not to invoke header filtering.


The ScrambleBody function performs an undocumented feature.  Some messages 
that come across the network contain material that may be offensive to some. 
 Therefore authors of these notes encode them using an algorithm called 
"ROT13".  To view this encoded text, pressing Alt-F2 or Esc-@ invokes 
ScrambleBody which decodes the current page.  To see all of the text, you may 
have to press Page Down and Alt-F2 repeatedly.


After a message has been displayed and the user is going on to other things, 
we update the data base (i.e. set the "read" flag if a mail folder).  We also 
call CUI_ProcessMessageAttributes.  This is a library routine that looks for 
special headers such as folder creation and voting.  Based on these headers 
the program may ask the user questions and do some special processing.


\heading{Message entry structures and processing

}
This part of the code is perhaps the most tricky and has undergone the most 
changes since the program was written.  The current scheme is to keep separate 
data structures for each part of the message.  The program also maintains 
flags that indicate if the current page needs to be saved to disk, and whether 
the user has edited the current message so we know to warn them of loss when 
quitting.  At one time this section of code was also used to allow the user to 
enter/edit an arbitrary file.  This has since been eliminated.


\subheading{Addresses

}The To and CC fields have their own list of addressees.  The objective here 
is that each entry in the list will contain what was in one line of user 
input.  After validation, each entry will contain one validated address, which 
will then receive its own line for display.  When the user is editing these 
fields, they may page up and down through the list.  If they are at the end of 
the list, they must enter addresses on the displayed lines before being 
allowed to page down and enter more.


\subheading{Body

}The complexities in this part of the code are for two reasons:  the paneling 
code does not interpret new line characters, so it maintains padding blanks in 
the empty parts of the field.  Also since the PC is limited in memory the 
program relies on storing the message data in a file at the message server. 
 At initialization time we calculate the size of the field for the body.  We 
store all those bytes in the file, so that when they hit page up or down we 
just add or subtract that amount to the offset where we read the file.  This 
also adds the feature that no matter how much editing the user did, we still 
maintain that offset calculation.  Unfortunately there are some un-handy 
ramifications of this for the user trying to edit a message.


Given this brute force scheme, the program is obligated to do compression and 
decompression of the message text.  The compression algorithm includes 
eliminating blank lines that occurred at the bottom of screen panels, since 
the user could not otherwise eliminate them.


\heading{File display structures and processing

}
This section of code is quite similar to message body display without the 
header filtering.  At one time the user was allowed to display any arbitrary 
file, however now this section of code is limited to showing help files.


\heading{The help scheme

}
Basically there are two important parts that enable context sensitive help to 
always be available.  The first is that the menu code knows what option number 
is currently highlighted.  This number is used to build the name of a file 
that is displayed to the user.  The second aspect is that each high-level 
function is responsible for setting a global variable (\italic{VUI_RepaintFn}) 
that tells what function may be called to restore the screen display.  When 
the user is finished viewing the help file, this function is called.


\heading{Option setting


}This section provides for display of the few user specified options that 
program uses.  To update any of these the user specifies the appropriate 
command and they are then asked questions at the prompt line.  No "on-screen" 
editing of options is provided.  When finished making changes the user has the 
option of making the changes permanent.  If so, the program calls routines to 
update the preferences file in their home directory.


\heading{Future Extensions

}
At present the program does not provide for including extra headers as part of 
messages it sends out.  As the message system facilities for message threads 
improve, there will be demand for including the "In-reply-to" and "References" 
headers.  These are provided by NameReplyFile and just need to be included in 
the resultant file that is given to the message server for sending.


Many enhancements are being (and have been) made to the message system data 
base.  Facilities will be requested to accommodate these.  Currently these 
include: creating voting messages, flagging messages as urgent, etc., and 
creating messages with enclosures.


Clearly the editing facility could be upgraded.  The padding with blanks 
scheme is definitely brute force and could be made much more elegant.  As 
always there are more editing features that could be included such as word 
wrap, word skipping, etc.  I have previously avoided this on the grounds of 
program size and the fact that the user can load and run an external editor 
such as emacs.


\begindata{bp,538071304}
\enddata{bp,538071304}
\view{bpv,538071304,1,0,0}
Copyright 1988 Carnegie Mellon University and IBM.  All rights reserved.

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

 $}}

\enddata{text,537620288}
