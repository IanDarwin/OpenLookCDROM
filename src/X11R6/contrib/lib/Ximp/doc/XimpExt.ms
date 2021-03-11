.EH '''' 
.OH '''' 
.EF ''''
.OF ''''
.fp 6 I 
.fp 7 C 
.fp 8 CB 
.ps 11 
.nr PS 11 
\& 
.sp 3 
.TL 
Extension Protocol Specification for the Distributed Input System 
.sp 
on the X Window System, Version 11
.sp 3
.AU
Masahiko Narita
Takashi Fujiwara
Keisuke Fukui
.AI
Fujitsu Limited
.AU
Masaki Takeuchi
.AI
Sony Corporation
.AU
Makoto Matsuyama
Kazuki Nishihara
.AI
Fuji Xerox Co., Ltd.
.AU
Yasuhiro Kawai
.AI
Oki Technosystems Laboratory, Inc.
.AU
Katsuhisa Yano
.AI
Toshiba Corporation
.AU
Hideki Hiura
Masayuki Seino
.AI
Nihon Sun Microsystems K.K.
.sp 5
Revision 0.2                       11 October, 1991
.sp 6p
Revision 1.0                            31 May, 1994
.ce 0
.br
\&
.LP
.ps 11
.nr PS 11
.bp
\&
.ps 9
.nr PS 9
.sp 8
.LP
.DS C
Copyright 1991 - 1994 Fujitsu Limited
Copyright 1991 - 1994 Sony Corporation
Copyright 1991 Fuji Xerox Co., Ltd.
Copyright 1991 Oki Technosystems Laboratory, Inc.
Copyright 1991 Toshiba Corporation
Copyright 1991 Nihon Sun Microsystems K.K.
.DE
.sp 3
.LP
Permission to use, copy, modify, distribute, and sell this documentation
for any purpose without fee is hereby granted, provided that the above
copyright notice appear in all copies and that both that copyright notice
and this permission notice are retained, and that the name of FUJITSU
LIMITED, Sony Corporation, Fuji Xerox Co., Ltd., Oki Technosystems
Laboratory, Inc., Toshiba Corporation, Nihon Sun Microsystems K.K., 
not be used in advertising or publicity pertaining to this document 
without specific, written prior permission.
FUJITSU LIMITED, Sony Corporation, Fuji Xerox Co., Ltd., Oki Technosystems
Laboratory, Inc., Toshiba Corporation, Nihon Sun Microsystems K.K., 
make no representations about the suitability of this document or 
the protocol defined in this document for any purpose.  
It is provided "as is" without express or implied warranty.
.sp 5
.LP
HISTORY
.IP "Revision 1.0"
Adding Section 6, 7 and 8 to set the input mode depending on 
the native language, and fixing typo.
.RS
Revision 1.0 edited by 
.RS
Takashi Fujiwara, Makiko Shimamura (FUJITSU LIMITED)
.br
Makoto Wakamatsu (Sony Corporation)
.RE
.RE
.ps 11
.nr PS 11
.bp 1
.EH '\fBXIMP Extension\fP'''
.OH '\fBXIMP Extension\fP'''
.EF ''\fB % \fP''
.OF ''\fB % \fP''
.NH 1
Overview
.XS
\*(SN Overview
.XE
.LP
This specification provides the extension for "Protocol Specification 
for the Distributed Input System on the X Window System, Version 11". 
.LP
Some kind of applications or language-engin sometimes require 
the IM Server to support the functions beyond the general functionality 
provided by XIM.
.LP
.RS
Examples are:
.IP \(bu
Setting the status window.
.IP \(bu
Selecting the front-end method or back-end methood.
.IP \(bu
Selecting the mode of listing candidates.
.IP \(bu
Switching the language on IC.
.IP \(bu
Setting the input mode depending on the native language.
.br
(e.g. latin-KANA conversion mode, alphanumeric mode).
.RE
.LP
This documentation describes a set of these functions depended
on the IM Server as shown above but commonly used or convenient. 
.LP
However, every application have to perform the minimum processing
even if the IM Server doesn't support any extension to keep 
inter-operability between the application and IM servers.
.LP
These should be implemented with the protocols based on senction 
6 of "Protocol Specification for the Distributed Input System on 
the X Window System, Version 11".
.LP
.bp
.NH 1
Status Window ID Extension
.XS
\*(SN Status Window ID Extension
.XE
.LP
The status window can be handled as the separate window
rather than be specified relative to the client window.
.LP
.RS
Atom Name of Extension Function  _XIMP_EXT_XIMP_STATUSWINDOW
.RE
.LP
\fBClientMessage Format\fP
.LP
.ce
Data Field in the XClientMessage Structure
.LP
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:\fBXIMP_EXTENSION\fP
4:ICID
8:Atom ID of _XIMP_EXT_XIMP_STATUSWINDOW
.TE 
.sp
.LP
When the IM Server is notified of the above ClientMessage,
it selects the status window with _XIMP_STATUS property and
displays the status on the window, where the coordination of
Area is relative to the window.
.LP
Note that this ClientMessage must be specified before the 
issuance of XIMP_BEGIN. Otherwise, it has no valid effect.
.bp
.NH 1
Selecting the front-end method or back-end method
.XS
\*(SN Selecting the front-end method or back-end method
.XE
.LP
Either method can be used in the present interface.
The following specifies which method is to be used.
.LP
.RS
Atom Name of Extension Function  _XIMP_EXT_XIMP_BACK_FRONT
.LP
.IP NOTE:
Supporting XIMP 3.5, _XIMP_EXT_XIMP_BACK_FRONT is used.
.br
Supporting XIMP 4.0, _XIMP_TYPE is used.
.RE
.LP
The IM Server should specify the following property at the startup time.
.LP
.RS
.TS
tab(|);
l c l.
Property Name|:|_XIMP_EXT_XIMP_BACK_FRONT
Window|:|IMS Window
Property Type|:|_XIMP_EXT_XIMP_BACK_FRONT
Format|:|32
data|:|the mode recommended by the IM Server
nelements|:|1
.TE
.RE
.LP
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:1 or 0
.TE 
.LP
.RS
.RS
.TS
tab(|);
l c l.
1|:|the back-end method mode
0|:|the front-end method mode
.TE
.RE
.RE
.LP
.B
Data Format
.LP
The Library notifys the IM Server of the following ClientMessage.
.LP
.ce	
 Data Field of the XClientMessage Structure
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:\fBXIMP_EXTENSION\fP
4:ICID
8:Atom ID of _XIMP_EXT_XIMP_BACK_FRONT
12:1 or 0
.TE
.LP
.RS
.RS
.TS
tab(|);
l c l.
1|:|the back-end method mode
0|:|the front-end method mode
.TE
.RE
.RE
.LP
The IM Server is capable of operating either in the back-end 
method or in front-end method registers the _XIMP_EXT_XIMP_BACK_FRONT 
atom into the _XIMP_EXTENSIONS property of the IMS window, and set 
the recommended mode to the _XIMP_EXT_XIMP_BACK_FRONT property.
The client can specify which method by issuing the above 
ClientMessage if and only if this atom has been set to the 
_XIMP_EXTENSIONS property.
.LP
When the IM Server is notified of the above ClientMessage, it 
switches input methods between the back-end mode and front-end mode.
Otherwise, it operates in the the front-end mode.
.LP
Note that this ClientMessage must be specified before the 
issuance of XIMP_BEGIN. After the issuance, it has no valid effect.
.bp
.NH 1
Candidate List Displaying Extension
.XS
\*(SN Candidate List Displaying Extension
.XE
.LP
The client can display the candidates in a list form by using
the LookupChoices extension call-back.
.sp
.LP
Atom Name of Extension Function : _XIMP_EXT_XIMP_LOOKUPCHOICES
.RE
.sp
.LP
\fBClientMessage Format\fP
.LP
.ce 
Data Field of the XClientMessage Structure
.TS
center, allbox, tab(:);
n | lw(8c).
.vs 15
0:\fBXIMP_EXTENSION\fP
4:ICID
8:Atom ID of _XIMP_EXT_XIMP_LOOKUPCHOICES
12:<Request/Reply #> (refer the below)
16:<data>
.TE
.sp
.LP
\fBRequest from the Client to the IM Server\fP
.LP
.TS
center, tab(:);
cw(6.5c) | cw(4c) | c | l
l | l | c | c.
_
.sp 6p
.B
Request:Content:Value:Sect.#
.sp 6p
_
.sp 6p
.R
\fBLOOKUP_CHOICES_BEGIN\fP:Negotiation:0:4.1
.sp 6p
_
.TE
.sp
.LP
\fBRequest from the IM Server to the Client\fP
.LP
.TS  
center, tab(:);
cw(6.5c) | cw(4c) | c | l
l | l | c | c.
_
.sp 6p
.B
Reply:Content:Value:Sect.#
.sp 6p
_
.sp 6p
.R
\fBLOOKUP_CHOICES_START_REQ\fP:LookupStartCallbacks:1:4.2
\fBLOOKUP_CHOICES_DRAW_REQ\fP:LookupDrawCallbacks:3:4.3
\fBLOOKUP_CHOICES_PROCESS_REQ\fP:LookupProcessCallbacks:5::4.4
\fBLOOKUP_CHOICES_DONE_REQ\fP:LookupDoneCallbacks:6:4.5
.sp 6p
_
.TE
.sp
.LP
\fBReply from the Client to the IM Server\fP
.LP
.TS  
center, tab(:);
cw(6.5c) | cw(4c) | c | l
l | l | c | c.
_
.sp 6p
.B
Reply:Content:Value:Sect.#
.sp 6p
_
.sp 6p
.R
\fBLOOKUP_CHOICES_START_REP\fP:LookupStartCallbacks:2:4.2
\fBLOOKUP_CHOICES_PROCESS_REP\fP:LookupProcessCallbacks:4:4.4
.sp 6p
_
.TE
.LP
.bp
.NH 2
Negotiation
.XS
\*(SN Negotiation
.XE
.LP 
This request informs the IM Server of that listing candidates will 
be done on the client side with the LookupChoices extension call-backs,
.LP
\fBFrom the Client to the IM Server\fP
.LP
.ce 
 Data Field of the IM Server
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:\fBXIMP_EXTENSION\fP
4:ICID
8:Atom ID of _XIMP_EXT_XIMP_LOOKUPCHOICES
12:LOOKUP_CHOICES_BEGIN
.TE        
.sp
.LP
The IM Server should register the above atom name into _XIMP_EXTENSIONS 
property of the IMS window, if the Server wants to make the client 
display. The candidate list with the LookupChoices extension callbacks.
.LP
The client using the LookupChoices extension call-backs should 
notify the IM Server of using this interface after confirming that 
the above atom name has been registered into the _XIMP_EXTENSIONS 
property.
.LP 
This ClientMessage must be specified before the issuance of 
XIMP_BEGIN. The following protocols will be used if and only if this 
ClientMessage is issued.
.bp
.NH 2
LookupStartCallbacks
.XS
\*(SN LookupStartCallbacks
.XE
.LP
\fBFrom the IM Server to the Client\fP
.LP
.ce
Data Field of the XClientMessage Structure
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:\fBXIMP_EXTENSION\fP
4:ICID
8:Atom ID _XIMP_EXT_XIMP_LOOKUPCHOICES
12:LOOKUP_CHOICES_START_REQ
16:Atom ID of stored property (*)
.TE
.sp
.LP
.RS
(*) The Atom ID of stored property
.TS
tab(|);
l c l.
window|:|IMS window
type|:|_XIMP_EXT_XIMP_CHOICE_START_REQ
format|:|32
data|:|XIMLookupStartCallbackStruct
nelements|:|8 (elements of data)
.TE
.LP
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:event\(->xkey.keycode
4:event\(->xkey.state
8:WhoIsMaster
12:XIMPreference\(->choice_per_window
16:XIMPreference\(->nrows
20:XIMPreference\(->ncolumns
24:XIMPreference\(->DrawUpDirection
28:XIMPreference\(->WhoOwnsLabel
.TE
.RE
.bp
.LP
\fBFrom the Client to the IM Server\fP
.LP
.ce
Data Field of the XClientMessage Structure
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:\fBXIMP_EXTENSION\fP
4:ICID
8:Atom ID of _XIMP_EXT_XIMP_LOOKUPCHOICES
12:LOOKUP_CHOICES_START_REP
16:Atom ID of stored property (*)
.TE
.sp
.LP
.RS
(*) The Atom ID of stored property
.LP
.TS
tab(|);
l c l.
window|:|Client Window
type|:|_XIMP_EXT_XIMP_CHOICE_START_REP
format|:|32
data|:|XIMLookupStartCallbackStruct
nelements|:|6 (elements of data)
.TE
.RE
.LP
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:WhoIsMaster
4:CBPreference\(->choice_per_window
8:CBPreference\(->nrows
12:CBPreference\(->ncolumns
16:CBPreference\(->DrawUpDirection
20:CBPreference\(->WhoOwnsLabel
.TE
.LP
.bp
.NH 2
LookupDrawCallbacks
.XS
\*(SN LookupDrawCallbacks
.XE
.LP
\fBFrom the IM Server to the Client\fP
.LP
.ce
Data Field of the XClientMessage Structure
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:\fBXIMP_EXTENSION\fP
4:ICID
8:Atom ID of _XIMP_EXT_XIMP_LOOKUPCHOICES
12:LOOKUP_CHOICES_DRAW_REQ
16:Atom ID of stored property (*)
.TE
.sp
.LP
.RS
(*) The Atom ID of stored property
.TS
tab(|);
l c l.
window|:|IMS window
type|:|_XIMP_EXT_XIMP_CHOICE_DRAW_REQ
format|:|32
data|:|XIMLookupDrawCallbackStruct
nelements|:|5 (elements of data)
.TE
.RE
.LP
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:max_len
4:index_of_first_candidate
8:index_of_last_candidate
12:Atom ID of choices(Text) (*1)
16:Atom ID of choices(Feedbacks) / NULL (*2)
.TE
.sp
.LP
.RS
(*1) Property specified with choices(Text)
.TS
tab(|);
l c l.
window|:|IMS window
type|:|XA_COMPOUND_TEXT
format|:|8
data|:|choices[0].label, choices[0].value, ...
nelements|:|string length of data
.TE
.RE
.LP
.TS
center, allbox;
lw(9c).
.vs 15
data
.TE
.sp
.LP
.RS
(*2) Property specified with choices(Feedbacks)
.TS
tab(|);
l c l.
window|:|IMS window
type|:|_XIMP_FEEDBACKS
format|:|8
data|:|Choice[0].label\(->feedbacks, choice[0].value\(->feedbacks, ...
nelements|:|The number of choices[] * 2
.TE
.RE
.LP
.TS
center, box, tab(:);
n  | lw(8c)
n  | l
n  | c.
.vs 15
0:choices[0].label\(->feedbacks
_
1:choices[0].value\(->feedbacks
_
:\(bu
:\(bu
:\(bu
.TE
.LP
.bp
.NH 2
LookupProcessCallbacks
.XS
\*(SN LookupProcessCallbacks
.XE
.LP
\fBFrom the IM Server to the Client\fP
.LP
.ce
Data Field of the XClientMessage Structure
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:\fBXIMP_EXTENSION\fP
4:ICID
8:Atom ID of _XIMP_EXT_XIMP_LOOKUPCHOICES
12:LOOKUP_CHOICES_PROCESS_REQ
16:Atom ID of stored property (*)
.TE
.sp
.LP
.RS
(*) The Atom ID of stored property
.TS
tab(|);
l c l.
window|:|IMS window
type|:|_XIMP_EXT_XIMP_CHOICE_PROC_REQ
format|:|32
data|:|data XIMLookupProcessCallbackStruct
nelements|:|elements of data
.TE
.RE
.LP
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:event\(->xkey.keycode
4:event\(->xkey.state
.TE
.sp
.LP
\fBFrom the Client to the IM Server\fP
.LP
.ce
Data Field of the XClientMessage Structure
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:\fBXIMP_EXTENSION\fP
4:ICID
8:Atom ID of _XIMP_EXT_XIMP_LOOKUPCHOICES
12:LOOKUP_CHOICES_PROCESS_REP
16:index_of_choice_selected
.TE
.bp
.NH 2
LookupDoneCallbacks
.XS
\*(SN LookupDoneCallbacks
.XE
.LP
\fBFrom the IM Server to the Client\fP
.LP
.ce
Data Field of the XClientMessage Structure
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:\fBXIMP_EXTENSION\fP
4:ICID
8:Atom ID of _XIMP_EXT_XIMP_LOOKUPCHOICES
12:LOOKUP_CHOICES_DONE_REQ
.TE
.LP
.bp
.NH 1
Handling/Querying IM Server's conversion mode
.XS
\*(SN Handling/Querying IM Server's conversion mode
.XE
.LP
This protocol handles or queries IM Server's conversion mode.
If a client wishes to change IM Server's conversion mode, send this
client message from the client to the IM Server with "operation" field 
is "Set", and "conversion" field is "on" or "off". Client can query 
IM Server's conversion mode by sending this client message from the 
client to the IM Server with "operation" field is "Get". In both of cases, 
the IM Server will reply by sending back the client message as shown above.
.sp
.LP
\fBFrom the Client to the IM Server (To query or to set)\fP
.LP
.ce
Data Field of the XClientMessage Structure
.TS
center, allbox, tab(;); 
n lw(8c).
.vs 15
0;\fBXIMP_EXTENSION\fP
4;ICID
8;Atom ID of _XIMP_EXT_XIMP_CONVERSION
12;Operation (*1)
16;Conversion  (*2)
.TE
.sp
.LP
\fBFrom the IM Server to the Client (reply of querying)\fP
.LP
.ce
Data Field of the XClientMessage Structure
.TS
center, allbox, tab(;); 
n lw(8c).
.vs 15
0;\fBXIMP_EXTENSION\fP
4;ICID
8;Atom ID of _XIMP_EXT_XIMP_CONVERSION
12;Conversion (*2)
.TE
.LP
.RS
.IP (*1)
Operation is defined as the followings:
.RS
.TS
tab(|);
l c l.
1|:|Get
0|:|Set
.TE
.RE
.LP
.IP (*2)
Conversion is defined as the followings:
.RS
.TS
tab(|);
l c l.
1|:|Off
0|:|On
.TE
.RE
.RE
.LP
.bp
.NH 1
Control the input mode depending on the native language
.XS
\*(SN Control the input mode depending on the native language
.XE
.LP
A client can set, lock or change the IM Server's input mode.
.sp
.TS
center, tab(:);
cw(5.5c) | cw(4c) | c
l | l | l.
_
.sp 6p
.B
Request:Content
.sp 6p
_
.sp 6p
.R
INPUTMODE:Setting Method:6.1
MODELOCK:Locking Method:6.2
MODECHANGE:Reporting Method:6.3
.sp 6p
_
.TE
.sp
.LP
The values to set or change the IM Server's input mode are as follows:
.TS
center, tab(:);
lw(5c) l.
HENKAN:0x00001000
MUHENKAN:0x00002000
ZENKAKU:0x00000100
HANKAKU:0x00000200
KANA:0x00000010
ROMAN:0x00000020
HIRAGANA:0x00000001
KATAKANA:0x00001002
UPPER:0x00001004
LOWER:0x00001008
.TE
.LP
The values to lock the IM Server's input mode are as follows:
.TS
center, tab(:);
lw(5c) l.
HANKAKU_HIRAGANA:0x00000100
HANKAKU_KATAKANA:0x00000200
HANKAKU_UPPER:0x00000400
HANKAKU_LOWER:0x00000800
ZENKAKU_HIRAGANA:0x00000001
ZENKAKU_KATAKANA:0x00000002
ZENKAKU_UPPER:0x00000004
ZENKAKU_LOWER:0x00000008
.TE
.LP
.NH 2
Setting methods
.XS
\*(SN Setting methods
.XE
.LP
A client sends the following client message to set the IM Server's 
input mode. The reply to this message must be sent synchronously.
As the IM Server returns the new input mode, the client can know 
whether the request succeeds or not. 
.LP
\fBFrom the Client to the IM Server (To set)\fP
.LP
.ce 
Data Field of the XClientMessage Structure
.LP
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:\fBXIMP_EXTENSION\fP
4:ICID
8:Atom ID of _XIMP_EXT_FUJITSU_INPUTMODE
12:input mode to set
.TE
.bp
.LP
\fBFrom the IM Server to the Client lib (reply)\fP
.LP
.ce 
Data Field of the XClientMessage Structure
.LP
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:\fBXIMP_EXTENSION\fP
4:ICID
8:Atom ID of _XIMP_EXT_FUJITSU_INPUTMODE
12:new input mode (*)
.TE
.LP
.RS
.IP (*) 
If any errors occur during data processing, all bit of the new input mode 
are 1.  
.RE
.bp
.NH 2
Locking method
.XS
\*(SN Locking method
.XE
.LP
A client sends the following client message to lock the IM Server's 
input mode. The reply to this message must be sent synchronously.
As IM Server returns the new input mode, the client can know 
whether the request succeeds or not. 
.LP
\fBFrom the Client to the IM Server (To lock)\fP
.LP
.ce 
Data Field of the XClientMessage Structure
.LP
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:\fBXIMP_EXTENSION\fP
4:ICID
8:Atom ID of _XIMP_EXT_FUJITSU_MODELOCK
12:lock mode to set
.TE
.LP
.sp
\fBFrom the IM Server to the Client (reply)\fP
.LP
.ce 
Data Field of the XClientMessage Structure
.LP
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:\fBXIMP_EXTENSION\fP
4:ICID
8:Atom ID of _XIMP_EXT_FUJITSU_MODELOCK
12:set lock mode (*)
.TE
.LP
.RS
.IP (*) 
If any errors occur during data processing, all bit of the new input mode 
are 1.  
.RE
.bp
.NH 2
Reporting method
.XS
\*(SN Reporting method
.XE 
.LP
This protocol is sent to notify that IM Server's input mode has been 
changed.  
.LP
A client notifys the IM Server with the following client message that the
client needs to know IM Server's input mode. 
After receiving this protocol, the IM Server sends the starting mode.
And the IM Server must send the changed mode to the client, 
if the input mode is changed, 
.LP
\fBFrom the Client to the IM Server (To require)\fP
.LP
.ce 
Data Field of the XClientMessage Structure
.LP
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:\fBXIMP_EXTENSION\fP
4:ICID
8:Atom ID of _XIMP_EXT_FUJITSU_MODECHANGE
.TE
.LP
.sp
\fBFrom the IM Server to the Client (To notify)\fP
.LP
.ce 
Data Field of the XClientMessage Structure
.LP
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:\fBXIMP_EXTENSION\fP
4:ICID
8:Atom ID of _XIMP_EXT_FUJITSU_MODECHANGE
12:new input mode
16:command dependent on the input mode (*)
.TE
.LP
.RS
.IP (*) 
Specify the operations that the client must handle, as follows:
.TS
tab(:);
l | l.
_
.sp 6p
.B
bit:command
.sp 6p
_
.sp 6p
.R
command & (1L << 0):need not to send XIMP_MOVE message
command & (1L << 1):reserved
command & (1L << 2):reserved
.sp 6p
_
.TE
.RE
.LP
.bp
.NH 1
Reporting the KeyBinding change
.XS
\*(SN Reporting the KeyBinding change
.XE
.LP
This protocol is sent to notify that IM Server's KeyBinding has been changed.
A client notifys the IM Server with the following client message that the
client needs to know IM Server's KeyBinding is _XIMP_SPROC_STARTED_KEYS or
_XIMP_SPROC_STORED_KEYS. (Please refer to section 3.3.7 and 3.3.8 of
"Protocol Specification for the Distributed Input System
on the X Window System, Version 11".)
The IM Server must send the changed KeyBinding to the client,
if the KeyBinding is changed,
.LP
\fBFrom the Client to the IM Server (To require)\fP
.LP
.ce 
Data Field of the XClientMessage Structure
.LP
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:\fBXIMP_EXTENSION\fP
4:ICID
8:Atom ID of _XIMP_EXT_FUJITSU_KEYBINDCHANGE
.TE
.LP
.sp
\fBFrom the IM Server to the Client (To notify)\fP
.LP
.ce 
Data Field of the XClientMessage Structure
.LP
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:\fBXIMP_EXTENSION\fP
4:ICID
8:Atom ID of _XIMP_EXT_FUJITSU_KEYBINDCHANGE
.TE
.LP
.bp
.NH 1
Flushing Preedit buffer
.XS
\*(SN Flushing Preedit buffer
.XE
.LP
A client sends the following client message to flush the preedit
buffer. The reply to this message is sent asynchronously.
The IM Server returns the committed string by converted string.
(Please refer to section 4.7 of "Protocol Specification for the 
Distributed Input System on the X Window System, Version 11".)
.LP
\fBFrom the to the IM Server (To require)\fP
.LP
.ce 
Data Field of the XClientMessage Structure
.LP
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:\fBXIMP_EXTENSION\fP
4:ICID
8:Atom ID of _XIMP_EXT_FUJITSU_FLUSH
.TE
.LP
.bp
.TC
