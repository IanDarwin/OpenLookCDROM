.\" To print this file, type tbl macros.t ThisFile | troff -ms
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
\s+3\fBProtocol Specification for the Distributed Input System\fP\s-3
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
.sp 10
Revision 3.5            20 September, 1991
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
Copyright 1991 Fujitsu Limitedi
Copyright 1991 Sony Corporation
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
and this permission notice are retained, and that the name of FUJITSU LIMITED,
Sony Corporation, Fuji Xerox Co., Ltd., Oki Technosystems Laboratory, Inc.,
Toshiba Corporation, Nihon Sun Microsystems K.K. not be used in advertising
or publicity pertaining to this document without specific, written prior
permission.
FUJITSU LIMITED, Sony Corporation, Fuji Xerox Co., Ltd.,
Oki Technosystems Laboratory, Inc., Toshiba Corporation,
Nihon Sun Microsystems K.K. make no representations about the suitability
of this document or the protocol defined in this document for any purpose.
It is provided "as is" without express or implied warranty.
.ps 11
.nr PS 11
.bp 1
.EH '\fBXIMP 3.5\fP'''
.OH '\fBXIMP 3.5\fP'''
.EF ''\fB % \fP''
.OF ''\fB % \fP''
.NH 1
Overview
.XS
\*(SN Overview
.XE
.NH 2
Scope
.XS
\*(SN Scope
.XE
.LP
MIT X Consortium made a specification for input method, called "X Window
System, Version 11 Input Method Specifications" (XIM specification).
This specification provides a common interface to a input library of the string
which depends on the native language. By the XIM specification,
an application programmer can write software supporting a native language by
unique source code without a knowledge of the implementation of this library.
.LP
As regards server-client IM system, however, there is no guarantee
of the interoperability between heterogeneous environments such as
the one provided by X Window System itself.
.LP
This documentation, XIM input server protocol specification,
makes it possible that one input server connect multi-clients on
a display. This documentation describes a implementation method
based on the XIM specification. It is possible to refer the XIM
specification about the API.
.LP
.NH 2
Characteristics
.XS
\*(SN Characteristics
.XE
.LP
The basic idea of this protocol is the followings.
.IP \(bu
This protocol preserves the network interoperability between
heterogeneous environments.
.IP \(bu
This protocol is able to separate the input system and clients
and supports a distributed system.
.IP \(bu
This protocol doesn't require any special communication techniques
except for the X protocol.
.IP \(bu
This protocol doesn't depend on a specific native language.
.IP \(bu
This protocol can provide multi-language services on one display.
.IP \(bu
This protocol can provide a callback method.
.IP \(bu
This protocol supports both the front-end method and the back-end method.
.LP
.NH 2
Basic Concept
.XS
\*(SN Basic Concept
.XE
.LP
.IP "IM Server"
Server process which provide a native language input function. This protocol
supports both the front-end method and back-end method.
.IP "IM Client"
Client process which asks the input service to the IM server.
.IP "IMS Window"
The window which IM server uses to receive the communications
from the client. In the case of the multi-lingual, there are
corresponding windows to language.
.IP "Front-End Method"
The method which key press passed to the IM server directly,
not via the client.
.IP "Back-End Method"
The method which the key press event passed to the IM server through the client.
.IP "Callback Method"
The method which the application's callback routines display the strings
for preedit.
.bp
.in +4c
.\" ---------- figure start ----------
... -0.25 -2.5 2 0.75
... 0.000i 3.250i 2.250i 0.000i
.nr 00 \n(.u
.nf
.PS 3.250i 2.250i 
.br
\h'1.250i'\v'0.750i'\h'-0.0m'\v'0.2m'\h'-\w'IM Client'u/2u'IM Client\h'-\w'IM Client'u/2u'
.sp -1
\h'0.250i'\v'1.500i'\v'-.13m'\L'-1.500i\(br'\v'.13m'
.sp -1
\h'0.250i'\l'2.000i'
.sp -1
\h'2.250i'\v'-.13m'\L'1.500i\(br'\v'.13m'
.sp -1
\h'2.250i'\v'1.500i'\l'-2.000i'
.sp -1
\h'1.500i'\v'1.250i'\h'-0.0m'\v'0.2m'\h'-\w'XIM Library'u/2u'XIM Library\h'-\w'XIM Library'u/2u'
.sp -1
\h'0.750i'\v'1.500i'\D'l0.050i 0.000i'
.sp -1
\h'0.854i'\v'1.500i'\D'l0.050i 0.000i'
.sp -1
\h'0.957i'\v'1.500i'\D'l0.050i 0.000i'
.sp -1
\h'1.061i'\v'1.500i'\D'l0.050i 0.000i'
.sp -1
\h'1.164i'\v'1.500i'\D'l0.050i 0.000i'
.sp -1
\h'1.268i'\v'1.500i'\D'l0.050i 0.000i'
.sp -1
\h'1.371i'\v'1.500i'\D'l0.050i 0.000i'
.sp -1
\h'1.475i'\v'1.500i'\D'l0.050i 0.000i'
.sp -1
\h'1.579i'\v'1.500i'\D'l0.050i 0.000i'
.sp -1
\h'1.682i'\v'1.500i'\D'l0.050i 0.000i'
.sp -1
\h'1.786i'\v'1.500i'\D'l0.050i 0.000i'
.sp -1
\h'1.889i'\v'1.500i'\D'l0.050i 0.000i'
.sp -1
\h'1.993i'\v'1.500i'\D'l0.050i 0.000i'
.sp -1
\h'2.096i'\v'1.500i'\D'l0.050i 0.000i'
.sp -1
\h'2.200i'\v'1.500i'\D'l0.050i 0.000i'
.sp -1
\h'2.250i'\v'1.500i'\D'l0.000i -0.050i'
.sp -1
\h'2.250i'\v'1.388i'\D'l0.000i -0.050i'
.sp -1
\h'2.250i'\v'1.275i'\D'l0.000i -0.050i'
.sp -1
\h'2.250i'\v'1.163i'\D'l0.000i -0.050i'
.sp -1
\h'2.250i'\v'1.050i'\D'l0.000i -0.050i'
.sp -1
\h'2.250i'\v'1.000i'\D'l-0.050i 0.000i'
.sp -1
\h'2.146i'\v'1.000i'\D'l-0.050i 0.000i'
.sp -1
\h'2.043i'\v'1.000i'\D'l-0.050i 0.000i'
.sp -1
\h'1.939i'\v'1.000i'\D'l-0.050i 0.000i'
.sp -1
\h'1.836i'\v'1.000i'\D'l-0.050i 0.000i'
.sp -1
\h'1.732i'\v'1.000i'\D'l-0.050i 0.000i'
.sp -1
\h'1.629i'\v'1.000i'\D'l-0.050i 0.000i'
.sp -1
\h'1.525i'\v'1.000i'\D'l-0.050i 0.000i'
.sp -1
\h'1.421i'\v'1.000i'\D'l-0.050i 0.000i'
.sp -1
\h'1.318i'\v'1.000i'\D'l-0.050i 0.000i'
.sp -1
\h'1.214i'\v'1.000i'\D'l-0.050i 0.000i'
.sp -1
\h'1.111i'\v'1.000i'\D'l-0.050i 0.000i'
.sp -1
\h'1.007i'\v'1.000i'\D'l-0.050i 0.000i'
.sp -1
\h'0.904i'\v'1.000i'\D'l-0.050i 0.000i'
.sp -1
\h'0.800i'\v'1.000i'\D'l-0.050i 0.000i'
.sp -1
\h'0.750i'\v'1.000i'\D'l0.000i 0.050i'
.sp -1
\h'0.750i'\v'1.112i'\D'l0.000i 0.050i'
.sp -1
\h'0.750i'\v'1.225i'\D'l0.000i 0.050i'
.sp -1
\h'0.750i'\v'1.337i'\D'l0.000i 0.050i'
.sp -1
\h'0.750i'\v'1.450i'\D'l0.000i 0.050i'
.sp -1
\h'1.500i'\v'2.000i'\h'-0.0m'\v'0.2m'\h'-\w'IM Server'u/2u'IM Server\h'-\w'IM Server'u/2u'
.sp -1
\h'0.750i'\v'2.250i'\v'-.13m'\L'-0.500i\(br'\v'.13m'
.sp -1
\h'0.750i'\v'1.750i'\l'1.500i'
.sp -1
\h'2.250i'\v'1.750i'\v'-.13m'\L'0.500i\(br'\v'.13m'
.sp -1
\h'2.250i'\v'2.250i'\l'-1.500i'
.sp -1
\h'1.250i'\v'3.000i'\h'-0.0m'\v'0.2m'\h'-\w'X Server'u/2u'X Server\h'-\w'X Server'u/2u'
.sp -1
\h'0.250i'\v'3.250i'\v'-.13m'\L'-0.500i\(br'\v'.13m'
.sp -1
\h'0.250i'\v'2.750i'\l'2.000i'
.sp -1
\h'2.250i'\v'2.750i'\v'-.13m'\L'0.500i\(br'\v'.13m'
.sp -1
\h'2.250i'\v'3.250i'\l'-2.000i'
.sp -1
\h'0.500i'\v'2.750i'\D'l0.000i -0.050i'
.sp -1
\h'0.500i'\v'2.650i'\D'l0.000i -0.050i'
.sp -1
\h'0.500i'\v'2.550i'\D'l0.000i -0.050i'
.sp -1
\h'0.500i'\v'2.450i'\D'l0.000i -0.050i'
.sp -1
\h'0.500i'\v'2.350i'\D'l0.000i -0.050i'
.sp -1
\h'0.500i'\v'2.250i'\D'l0.000i -0.050i'
.sp -1
\h'0.500i'\v'2.150i'\D'l0.000i -0.050i'
.sp -1
\h'0.500i'\v'2.050i'\D'l0.000i -0.050i'
.sp -1
\h'0.500i'\v'1.950i'\D'l0.000i -0.050i'
.sp -1
\h'0.500i'\v'1.850i'\D'l0.000i -0.050i'
.sp -1
\h'0.500i'\v'1.750i'\D'l0.000i -0.050i'
.sp -1
\h'0.500i'\v'1.650i'\D'l0.000i -0.050i'
.sp -1
\h'0.500i'\v'1.550i'\D'l0.000i -0.050i'
.sp -1
\h'0.500i'\v'1.450i'\D'l0.000i -0.050i'
.sp -1
\h'0.500i'\v'1.350i'\D'l0.000i -0.050i'
.sp -1
\h'0.500i'\v'1.250i'\D'l0.000i -0.050i'
.sp -1
\h'0.500i'\v'1.150i'\D'l0.000i -0.050i'
.sp -1
\h'0.500i'\v'1.050i'\D'l0.000i -0.050i'
.sp -1
\h'0.500i'\v'0.950i'\D'l0.000i -0.050i'
.sp -1
\h'0.500i'\v'0.900i'\D'l0.050i 0.000i'
.sp -1
\h'0.613i'\v'0.900i'\D'l0.050i 0.000i'
.sp -1
\h'0.725i'\v'0.900i'\D'l0.050i 0.000i'
.sp -1
\h'0.837i'\v'0.900i'\D'l0.050i 0.000i'
.sp -1
\h'0.950i'\v'0.900i'\D'l0.050i 0.000i'
.sp -1
\h'1.000i'\v'0.900i'\D'l0.000i 0.050i'
.sp -1
\h'1.000i'\v'1.000i'\D'l0.000i 0.050i'
.sp -1
\h'1.000i'\v'1.100i'\D'l0.000i 0.050i'
.sp -1
\h'1.000i'\v'1.200i'\D'l0.000i 0.050i'
.sp -1
\h'1.000i'\v'1.300i'\D'l0.000i 0.050i'
.sp -1
\h'1.000i'\v'1.400i'\D'l0.000i 0.050i'
.sp -1
\h'1.000i'\v'1.500i'\D'l0.000i 0.050i'
.sp -1
\h'1.000i'\v'1.600i'\D'l0.000i 0.050i'
.sp -1
\h'1.000i'\v'1.700i'\D'l0.000i 0.050i'
.sp -1
\h'0.975i'\v'1.650i'\D'l0.025i 0.100i'
.sp -1
\h'1.025i'\v'1.650i'\D'l-0.025i 0.100i'
.sp -1
\v'2.500i'\h'-0.0m'\v'0.2m'\v'-1u*\n(.vu/2u'\h'-\w'Back End'u/2u'Back End\h'-\w'Back End'u/2u'
.sp -1
\v'2.500i'\h'-0.0m'\v'0.2m'\v'1u*\n(.vu/2u'\h'-\w'Method'u/2u'Method\h'-\w'Method'u/2u'
.sp -1
\h'1.250i'\v'2.750i'\D'l0.000i -0.050i'
.sp -1
\h'1.250i'\v'2.638i'\D'l0.000i -0.050i'
.sp -1
\h'1.250i'\v'2.525i'\D'l0.000i -0.050i'
.sp -1
\h'1.250i'\v'2.412i'\D'l0.000i -0.050i'
.sp -1
\h'1.250i'\v'2.300i'\D'l0.000i -0.050i'
.sp -1
\h'1.275i'\v'2.350i'\D'l-0.025i -0.100i'
.sp -1
\h'1.225i'\v'2.350i'\D'l0.025i -0.100i'
.sp -1
\h'1.750i'\v'2.500i'\h'-0.0m'\v'0.2m'\v'-1u*\n(.vu/2u'\h'-\w'Front End'u/2u'Front End\h'-\w'Front End'u/2u'
.sp -1
\h'1.750i'\v'2.500i'\h'-0.0m'\v'0.2m'\v'1u*\n(.vu/2u'\h'-\w'Method'u/2u'Method\h'-\w'Method'u/2u'
.sp -1
.sp 1+3.250i
.PE
.if \n(00 .fi
.\" ---------- figure end ----------
.in -4c
.sp
.ce
Fig.1 The Flow of The Key Press Event
.LP
.bp
.NH 1
Data Structure
.XS
\*(SN Data Structure
.XE
.LP
This protocol uses the ClientMessage and the Property which has been
provided by the X Window System.
.IP \(bu
This protocol uses a ClientMessage event in the case of small data.
.IP \(bu
This protocol uses a Property in the case of big data.
.LP
The following sector describes about the common format of the data.
.LP
.NH 2
ClientMessage
.XS
\*(SN ClientMessage
.XE
.NH 3
The Format from the Client to the IM Server
.XS
\*(SN The Format from the Client to the IM Server
.XE
.LP
The library function generates a input service request and sends it to
the IM server by the ClientMessage event. The following is a list of the
requests:
.RS
.IP \(bu
Forward KeyEvent
.IP \(bu
Open Connection Request
.IP \(bu
Close Connection Request
.IP \(bu
Begin Process Request
.IP \(bu
End Process Request
.IP \(bu
Focus Window Request
.IP \(bu
Unfocus Window Request
.IP \(bu
Control IM Server Request
.IP \(bu
Move Preedit Window Request
.IP \(bu
Reset IM Server Request
.RE
.LP
The ClientMessage event's format is the following.
.RS
.TS
tab(:);
c s | c
lw(2c) lw(2.5c) | l.
_
.sp 6p
.B
Structure Member:Contents
.sp 6p
_
.sp 6p
.R
int:type:ClientMessage
u_long:serial:Set by the X Window System
Bool:send_event:Set by the X Window System
Display:*display:The value specified with XOpenIM
Window:window:IMS Window ID
Atom:message_type:_XIMP_PROTOCOL
int:format:32
.sp 6p
_
.sp 6p
long:data[5]:(to be described in each request)
.sp 6p
_
.TE
.RE
.LP
.NH 3
The Format from the IM Server to the Client
.XS
\*(SN The Format from the IM Server to the Client
.XE
.LP
The IM server uses the ClientMessage event to reply for the request
from the client, to notify of a completion of processing, and to
notify of the error.
.RS
.IP \(bu
Send KeyEvent
.IP \(bu
Return ICID
.IP \(bu
Begin Process Notify
.IP \(bu
End Process Notify
.IP \(bu
Return Converted String
.IP \(bu
Return Attributes
.IP \(bu
Error Notify
.IP \(bu
Callback Procedure Call
.RE
.LP
The format is shown belows.
.RS
.TS
tab(:);
c s | c
lw(2c) lw(2.5c) | l.
_
.sp 6p
.B
Structure Member:Contents
.sp 6p
_
.sp 6p
.R
int:type:ClientMessage
u_long:serial:Set by the X Window System
Bool:send_event:Set by the X Window System
Display:*display:IM Server's Display
Window:window:Focus Window ID specified in IC
Atom:message_type:_XIMP_PROTOCOL
int:format:32 or 8 (*)
.sp 6p
_
.sp 6p
long:data[5]:to be described in each request)
.sp 6p
_
.TE
.LP
.IP (*) 
The format depends on the notifying contents. In regards to format 8,
it specified as below.
.RS
.TS
tab(:);
l n.
Send KeyEvent:32
Return ICID:32
Begin Process Notify:32
End Process Notify:32
Return Converted String:8
Return Attributes:32
Error Notify:32
Callback Procedure Call:32
.TE
.RE
.RE
.LP
.bp
.NH 2
Property
.XS
\*(SN Property
.XE
.NH 3
The Property Changing by the Client
.XS
\*(SN The Property Changing by the Client
.XE
.LP
The client sends informations to the IM server using the "property"
mechanism. This is performed by the XChangeProperty function with the
corresponding data for each request.
.RS
.IP \(bu
Connection Attributes Data
.IP \(bu
Callback Request Data
.RE
.LP
The library changes the Property using the XChangeProperty function
with the following arguments.
.RS
.TS
tab(:);
c s | c
lw(2c) lw(2c) | l.
_
.sp 6p
.B
Argumen:Contents
.sp 6p
_
.sp 6p
.R
Display:*displa:Specified in XOpenIM
Window:window:Client window ID specified in XIC (*)
Atom:property:to be described in each request
Atom:type:to be described in each request
int:format:to be described in each request
int:mode:PropModeAppend
u_char:*data:to be described in each request
int:nelements:to be described in each request
.sp 6p
_
.TE
.LP
.IP (*) 
XIC is a resource ID of Input Context, which specified in XIM specification.
.RE
.LP
The clients changes the property with the mode of PropModeAppend and
IM server reads it with the delete mode (delete == True).
.LP
.NH 3
The Property Changing by the IM Server
.XS
\*(SN The Property Changing by the IM Server
.XE
.LP
The IM server uses the property for notifying or sending data
to the client.
.LP
.RS
.IP \(bu
IM Server's Characteristics Data
.IP \(bu
Converted String Data
.IP \(bu
IM Server's Attributes Reply Data
.IP \(bu
Callback Procedures Call Data
.RE
.LP
The IM server changes the Property using the XChangeProperty function
with the following arguments.
.RS
.TS
tab(:);
c s | c
lw(2c) lw(2.5c) | l.
_
.sp 6p
.B
Argument:Contents
.sp 6p
_
.R
.sp 6p
Display:*display:IM Server's Display
Window:window:IMS Window ID
Atom:property:to be described in each request
Atom:type:to be described in each request
int:format:to be described in each request
int:mode:PropModeAppend
u_char:*data:to be described in each request
int:nelements:to be described in each request
.sp 6p
_
.TE
.RE
.LP
.bp
.NH 3
Initial Processing in IM Server
.XS
\*(SN Initial Processing in IM Server
.XE
.LP
IM server sets the following in initialization.
.RS
.IP \(bu
Creation of the Atom declaring the language
.IP \(bu
Declaration the IMS window
.IP \(bu
Registration of the attributes of the IM server
.RE
.LP
And after the connection with the client, IM server processes
the following.
.RS
.IP \(bu
Replying corresponding to each requests
.IP \(bu
Calling a callback procedure
.LP
.RE
These are described below.
.LP
.NH 2
Creation of the Atom declaring the input language
.XS
\*(SN Creation of the Atom declaring the input language
.XE
.LP
IM server registers the name of input language with the following format.
.DS
Basic Declaration of Input	_XIMP_<localename>
Detailed Declaration of Input	_XIMP_<localename>@<servername>.<screen>
.sp
	<localename>	locale name
	<servername>	IM server name
	<screen>		screen number
.DE
.LP
.RS
.TS
tab(;);
l s s
lw(1c) l l.
examples:
.sp 6p
;_XIMP_ja_JP;Japanese
;_XIMP_ja_JP@xxx.0
;_XIMP_ko_KR;Korean
;_XIMP_ko_KR@xxx.0
;_XIMP_zh_CN;Chinese in Mainland China
;_XIMP_zh_CN@xxx.0
;_XIMP_zh_TW;Chinese in R.O.C.
;_XIMP_zh_TW@xxx.0
.TE
.RE
.LP
IM server must declare basic declaration format at least.
.LP
This localename format is compliant with XPG3, using the following format.
.DS
<language>_<territory>
.DE
.LP
And this protocol uses ISO 639 (language code) in "language"
and ISO 3166 (country code) in "territory".
.LP
.bp
.NH 2
Declaration of the IMS Window
.XS
\*(SN Declaration of the IMS Window
.XE
.LP
IM server creates a window (IMS Window) depending on a input language
for communication between clients and the IM server. An IMS window is
declared as a selection owner of the Atom of declaring the input language.
.LP
.bp
.NH 2
Registration of the Attributes of the IM Server
.XS
\*(SN Registration of the Attributes of the IM Server
.XE
.LP
IM server registers the following information to IMS window as a
Property. Detail information are described below.
.RS
.TS
.tab(:);
lw(6c) l.
_XIMP_VERSION:Protocol version
_XIMP_STYLE:List of supported styles
_XIMP_KEYS:List of keys of start/end service
_XIMP_SERVERNAME:Name of IM server
_XIMP_SERVERVERSION:Version of IM server
_XIMP_VENDORNAM:Name of vendor
_XIMP_EXTENSIONS:List of vendor extensions
.TE
.RE
.LP
.NH 3
_XIMP_VERSION
.XS
\*(SN _XIMP_VERSION
.XE
.LP
.RS
.TS
tab(|);
l c l.
Property Name|:|_XIMP_VERSION
Property Type|:|XA_STRING
Format|:|8
data|:|String described protocol version
nelements|:|String Length of data
.TE
.RE
.LP
.TS
allbox, center;
lw(9c).
.vs 15
XIMP.3.5
.TE
.LP
.NH 3
_XIMP_STYLE
.XS
\*(SN _XIMP_STYLE
.XE
.LP
.RS
.TS
tab(|);
l c l.
Property Name|:|_XIMP_STYLE
Property Type|:|_XIMP_STYLE
Format|:|32
data|:|List of XIMStyle specified in XIM specification
nelements|:|The number of styles, IM server supported
.TE
.RE
.LP
.TS
allbox, center, tab(:);
n lw(8c).
.vs 15
0:\&
4:\&
.TE
.LP
.NH 3
_XIMP_KEYS
.XS
\*(SN _XIMP_KEYS
.XE
.LP
.RS
.TS
tab(|);
l c l.
Property Name|:|_XIMP_KEYS
Property Type|:|_XIMP_KEYS
Format|:|32
data|:|Modifier, modifier mask and keysym for judging the key event of start/end service
nelements|:|The number of keys * 3
.TE
.RE
.LP
.TS
allbox, center, tab(:);
n lw(8c).
.vs 15
0:modifier
4:modifier_mask
8:keysym
\&:\&
.TE
.LP
.NH 3
_XIMP_SERVERNAME
.XS
\*(SN _XIMP_SERVERNAME
.XE
.LP
.RS
.TS
tab(|);
l c l.
Property Name|:|_XIMP_SERVERNAME
Property Type|:|XA_STRING
Format|:|8
data|:|String described server name
nelements|:|String length of data
.TE
.RE
.LP
.TS
allbox, center;
cw(9c).
.vs 15
\&
.TE
.LP
.NH 3
_XIMP_SERVERVERSION
.XS
\*(SN _XIMP_SERVERVERSION
.XE
.LP
.RS
.TS
tab(|);
l c l.
Property Name|:|_XIMP_SERVERVERSION
Property Type|:|XA_STRING
Format|:|8
data|:|String described server version
nelements|:|String length of data
.TE
.RE
.LP
.TS
allbox center;
cw(9c).
.vs 15
\&
.TE
.LP
.NH 3
_XIMP_VENDORNAME
.XS
\*(SN _XIMP_VENDORNAME
.XE
.LP
.RS
.TS
tab(|);
l c l.
Property Name|:|_XIMP_VENDORNAME
Property Type|:|XA_STRING
Format|:|8
data|:|String described vendor name
nelements|:|String length of data
.TE
.RE
.LP
.TS
allbox, center;
cw(9c).
.vs 15
\&
.TE
.LP
.bp
.NH 1
Basic Request
.XS
\*(SN Basic Request
.XE
.LP
There are following requests and reply between IM server and clients.
.IP 1)
Request from the client to the IM server.
.TS
tab(:);
cw(7.5c) | cw(5c) | c
l | l | l.
_
.sp 6p
.B
Request:Contents
.sp 6p
_
.sp 6p
.R
\fBXIMP_KEYPRESS\fP:Forward KeyEvent:4.1.
\fBXIMP_CREATE\fP:Open Connection Request:4.2.1.
\fBXIMP_DESTROY\fP:Close Connection Request:4.3.
\fBXIMP_BEGIN\fP:Begin Process Request:4.4.1.
\fBXIMP_END\fP:End Process Request:4.5.1.
\fBXIMP_SETFOCUS\fP:Focus Window Request:4.6.
\fBXIMP_UNSETFOCUS\fP:Unfocus Window Request:4.7.
\fBXIMP_SETVALUE\fP:Set Value Request:4.9.
\fBXIMP_MOVE\fP:Move Preedit Window Request:4.10.
\fBXIMP_GETVALUE\fP:Get Value Request:4.11.1.
\fBXIMP_RESET\fP:Reset IM Server Request:4.12.
.sp 6p
_
.TE
.LP
.IP 2)
Reply from the IM server to the client
.TS
tab(:);
cw(7.5c) | cw(5c) | c
l | l | l.
_
.sp 6p
Reply:Contents
.sp 6p
_
.sp 6p
.R
\fBXIMP_KEYPRESS\fP:Forward KeyEvent:4.1.
\fBXIMP_CREATE_RETURN\fP:Return ICID:4.2.3.
\fBXIMP_PROCESS_BEGIN\fP:Begin Process Notify:4.4.2.
\fBXIMP_PROCESS_END\fP:End Process Notify:4.5.2.
\fBXIMP_READPROP\fP:Return Converted String:4.8.1.
\fBXIMP_GETVALUE_RETURN\fP:Return Attributes:4.11.2.
\fBXIMP_RESET_RETURN\fP:Reset IM Server Return:4.12.
\fBXIMP_ERROR\fP:Error Notify:4.13.1.
.sp 6p
_
.TE
.LP
Request and Reply are the integer.
.LP
.bp
.NH 2
Forward KeyEvent
.XS
\*(SN Forward KeyEvent
.XE
.LP
This protocol will be used in the case of a KeyPress Event. And this
protocol is used in the both case, which the IM server working
in the front-end method forwards a intercepted key event that isn't used
in the IM to the client, and which clients using the IM server by the
back-end method send the key event to the IM server.
.LP
It's easy to send the key event using the XSendEvent function, but all
of clients can send the key event, therefore there is a security problem.
Better application can ignore the key event sending by the XSendEvent.
.LP
To consider the security problem, clients and the IM server use a
established channel based on this protocol to send the key event.
By this method, it seems to be better than the simple XSendEvent method.
.LP
.RS
Data Format
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:\fBXIMP_KEYPRESS\fP
4:ICID
8:keycode
12:state
.TE
.LP
.RS
2 members, keycode and state, of the XKeyEvent are sent in this client message.
.RE
.RE
.LP
.bp
.NH 2
Initial processing for establishing the connection
.XS
\*(SN Initial processing for establishing the connection
.XE
.LP
.IP "1) Initial processing in the client"
The client performs the following procedure before sending
input service requests.
.RS
.IP \(bu
Getting the IMS window id
.IP \(bu
Getting the keys of start/end service
.IP \(bu
Setting the attributes using in initial connection
.IP \(bu
Sending the request for connection
.IP \(bu
Receiving the reply of ICID from IM server
.RE
.LP
.IP "2) Processing in IM server"
.RS
.IP \(bu
Registration of the client
.IP \(bu
Reply of ICID corresponding to the client
.RE
.LP
.NH 3
Setting the IC Informations
.XS
\*(SN
Setting the IC Informations
.XE
.LP
The clients set the following Property before the connection request,
and the IM server reads it at the connection initializing processing.
It is possible to set or change this information even if using
.PN XIMP_SETVALUE.
.DS
_XIMP_VERSION	Protocol version using the client
_XIMP_FOCUS		Current focus window
_XIMP_PREEDIT	Detail information of preedit window
_XIMP_STATUS		Detail information of status window
_XIMP_PREEDITFONT	Font information of preedit window
_XIMP_STATUSFONT	Font information of status window
.DE
.LP
.bp
.IP "\(bu _XIMP_VERSION Property"
.TS
tab(|);
l c l.
Property Name|:|_XIMP_VERSION
Property Type|:|XA_STRING
Format|:|8
data|:|String specifies Protocol Version
nelements|:|String length of data
.TE
.LP
.TS
center, allbox;
lw(9c).
.vs 15
XIMP.3.5 
.TE
.LP
.IP "\(bu _XIMP_FOCUS Property"
.TS
tab(|);
l c l.
Property Name|:|_XIMP_FOCUS
Property Type|:|XA_WINDOW
Format|:|32
data|:|Focus Window ID
nelements|:|1
.TE
.LP
.TS
center, allbox;
lw(9c).
.vs 15
WIndow ID
.TE
.LP
.bp
.IP "\(bu _XIMP_PREEDIT Property"
The client registers the following Property to set the pre-edit
window's attributes.
.TS
tab(|);
l c l.
Property Name|:|_XIMP_PREEDIT
Property Type|:|_XIMP_PREEDIT
Format|:|32
data|:|Values corresponding the XIM specification with the following format.
nelements|:|14
.TE
.LP
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:Area.x
4:Area.y
8:Area.width
12:Area.height
16:Foreground
20:Background
24:Colormap
28:Bg Pixmap
32:LineSpacing
36:Cursor
40:AreaNeeded.width
44:AreaNeeded.height
48:SpotLocation.x
52:SpotLocation.y
.TE
.LP
.bp
.IP "\(bu _XIMP_STATUS Property"
The client registers the following Property to set the status
window's attributes.
.TS
tab(|);
l c l.
Property Name|:|_XIMP_STATUS
Property Type|:|_XIMP_STATUS
Format|:|32
data|:|Values corresponding the XIM specification with the following format
nelements|:|1
.TE
.LP
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:Area.x
4:Area.y
8:Area.width
12:Area.height
16:Foreground
20:Background
24:Colormap
28:Bg Pixmap
32:LineSpacing
36:Cursor
40:AreaNeeded.width
44:AreaNeeded.height
48:WID of status window (*)
.TE
.LP
.RS
.IP (*)
Status window id, if the client want to set it. If this ID is not zero,
the IM server uses it to status display. (XIMP extension)
.RE
.LP
.IP "\(bu _XIMP_PREEDITFONT Property"
.TS
tab(|);
l c l.
Property Name|:|_XIMP_PREEDITFONT
Property Type|:|XA_STRING
Format|:|8
data|:|String described base font name list
nelements|:|string length of data
.TE
.LP
.TS
center, allbox;
lw(9c).
.vs 15
data
.TE
.LP
.bp
.IP "\(bu _XIMP_STATUSFONT Property"
.TS
tab(|);
l c l.
Property Name|:|_XIMP_STATUSFONT
Property Type|:|XA_STRING
Format|:|8
data|:|String described base font name list
nelements|:|string length of data
.TE
.LP
.TS
center, allbox;
lw(9c).
.vs 15
data
.TE
.LP
.NH 3
Request for connection establishing
.XS
\*(SN Request for connection establishing
.XE
.LP
The client sends the following ClientMessage to the IMS window.
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:\fBXIMP_CREATE\fP
4:Client Window ID
8:InputStyle
12:mask (*)
.TE
.LP
.IP (*) 
The mask is the following.
.TS
tab(:);
c | c | c
l | l | l.
_
.sp 6p
.B
Name:Value:Contents
.sp  6p
_
.sp 6p
.R
\fBXIMP_FOCUS_WIN_MASK\fP:(1L<< 0):_XIMP_FOCUS
\fBXIMP_PRE_AREA_MASK\fP:(1L<< 1):_XIMP_PREEDIT Area
\fBXIMP_PRE_FG_MASK\fP:(1L<< 2):_XIMP_PREEDIT Foreground
\fBXIMP_PRE_BG_MASK\fP:(1L<< 3):_XIMP_PREEDIT Background
\fBXIMP_PRE_COLORMAP_MASK\fP:(1L<< 4):_XIMP_PREEDIT Colormap
\fBXIMP_PRE_BGPIXMAP_MASK\fP:(1L<< 5):_XIMP_PREEDIT Bg Pixmap
\fBXIMP_PRE_LINESP_MASK\fP:(1L<< 6):_XIMP_PREEDIT LineSpacing
\fBXIMP_PRE_CURSOR_MASK\fP:(1L<< 7):_XIMP_PREEDIT Cursor
\fBXIMP_PRE_AREANEED_MASK\fP:(1L<< 8):_XIMP_PREEDIT AreaNeeded
\fBXIMP_PRE_SPOTL_MASK\fP:(1L<< 9):_XIMP_PREEDIT SpotLocation
\fBXIMP_STS_AREA_MASK\fP:(1L<<10):_XIMP_STATUS Area
\fBXIMP_STS_FG_MASK\fP:(1L<<11):_XIMP_STATUS Foreground
\fBXIMP_STS_BG_MASK\fP:(1L<<12):_XIMP_STATUS Background
\fBXIMP_STS_COLORMAP_MASK\fP:(1L<<13):_XIMP_STATUS Colormap
\fBXIMP_STS_BGPIXMAP_MASK\fP:(1L<<14):_XIMP_STATUS Bg Pixmap
\fBXIMP_STS_LINESP_MASK\fP:(1L<<15):_XIMP_STATUS LineSpacing
\fBXIMP_STS_CURSOR_MASK\fP:(1L<<16):_XIMP_STATUS Cursor
\fBXIMP_STS_AREANEED_MASK\fP:(1L<<17):_XIMP_STATUS AreaNeeded
\fBXIMP_STS_WINDOW_MASK\fP:(1L<<18):_XIMP_STATUS WID of status
\fBXIMP_PRE_FONT_MASK\fP:(1L<<19):_XIMP_PREEDITFONT
\fBXIMP_STS_FONT_MASK\fP:(1L<<20):_XIMP_STATUSFONT
.sp 6p
_
.TE
.LP
.bp
.NH 3
Receiving the ICID
.XS
\*(SN Receiving the ICID
.XE
.LP
The clients receive the ICID with the following format.
If there are any errors, the clients receive the error notify
described in below.
.LP
The ClientMessage from the IM server.
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:\fBXIMP_CREATE_RETURN\fP
4:ICID
.TE
.LP
.bp
.NH 2
The End of the Input Service Request
.XS
\*(SN The End of the Input Service Request
.XE
.LP
The client sends the end request to the IM server with 
.PN XIMP_DESTROY
to notify the end of the input service request.
.LP
.B
Processing Contents
.IP "1) Processing in the Client"
.RS
.IP \(bu
The client sends the ClientMessage to the IM server for
request of the ending the input service.
.IP \(bu
The client performs to free the resources which created at the opening.
.RE
.LP
.IP "2) Processing in the IM server"
.RS
.IP \(bu
The IM server performs to free the resources.
.IP \(bu
The IM server deletes the registration of the client.
.LP
Except the ending request from the client, the IM server performs
this closing processing in the case of getting the destroy event
of focused window.
.RE
.LP
.B
Data Format of This ClientMessage
.LP
The client sends the following ClientMessage.
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:\fBXIMP_DESTROY\fP
4:ICID
.TE
.LP
.NH 2
Start of the Native Language Input Service
.XS
\*(SN Start of the Native Language Input Service
.XE
.LP
The client sends the request of the starting for the native language
input service to the IM server with 
.PN XIMP_BEGIN, 
or the IM server sends the starting notify to the client with 
.PN XIMP_PROCESS_BEGIN.
.LP
.NH 3
Start Request of the Input Service from the Client
.XS
\*(SN Start Request of the Input Service from the Client
.XE
.LP
The client sends a request of the start of input service with 
.PN XIMP_BEGIN
to the IM server.
.LP
.B
Processing Contents
.IP "1) Processing in the Client"
.RS
.IP \(bu
The client sends the 
.PN XIMP_BEGIN 
request of specified window.
.IP \(bu
If the front end method, the client deletes the key press event
mask of that window.
.RE
.LP
.IP "2) Processing in the IM server"
.RS
.IP \(bu
If the front end method, the IM server sets the key press event
mask of specified window.
.IP \(bu
The IM server begins the service.
.RE
.LP
.B
Data Format of This ClientMessage
.LP
The client sends the following ClientMessage.
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, tab(:), allbox;
n lw(8c).
.vs 15
0:\fBXIMP_BEGIN\fP
4:ICID
.TE
.LP
.bp
.NH 3
Notify a Starting of the Input Service from the IM Server
.XS
\*(SN Notify a Starting of the Input Service from the IM Server
.XE
.LP
The IM server sends a notify of the start of input service with 
.PN XIMP_PROCESS_BEGIN 
to the client.
.LP
.B
Processing Contents
.IP "1) Processing in the IM server"
.RS
.IP \(bu
The IM server sends a 
.PN XIMP_PROCESS_BEGIN
to the client.
.IP \(bu
If the front end method, the IM server sets the key press event mask of
specified window.
.IP \(bu
The IM server begins the service.
.RE
.LP
.IP "2) Processing in the Client"
.RS
.IP \(u
The client receives the 
.PN XIMP_PROCESS_BEGIN
notify from the IM server.
.IP \(u
If the front end method, the client deletes the key press event mask
of that window.
.RE
.LP
.B
Data Format of This ClientMessage
.LP
The client sends the following ClientMessage.
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, tab(:), allbox;
n lw(8c).
.vs 15
0:\fBXIMP_PROCESS_BEGIN\fP
4:ICID
.TE
.LP
.bp
.NH 2
End of the Native Language Input Service
.XS
\*(SN End of the Native Language Input Service
.XE
.LP
The client sends the request of the ending for the native language
input service to the IM server with 
.PN XIMP_END,
or the IM server sends the ending notify to the client with 
.PN XIMP_PROCESS_END.
.LP
.NH 3
End Request from the Client
.XS
\*(SN End Request from the Client
.XE
.LP
.B
Processing Contents
.IP "1) Processing in the Client"
.RS
.IP \(bu
The client sends the 
.PN XIMP_END request 
to the IM server.
.IP \(bu
If the front end method, the client sets the key press event mask.
.RE
.LP
.IP "2) Processing in the IM server"
.RS
.IP \(bu
When the IM server receives 
.PN XIMP_END
request, the IM server performs the ending processing of the service.
.IP \(bu
The IM server deletes the key press event mask of focused window,
if the front end method.
.RE
.LP
.B
Data Format of This ClientMessage
.LP
The client sends the following ClientMessage to the IM server.
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, tab(:), allbox;
n lw(8c).
.vs 15
0:\fBXIMP_END\fP
4:ICID
.TE
.LP
.NH 3
Process End Notify from the IM Server
.XS
\*(SN Process End Notify from the IM Server
.XE
.LP
.B
Processing Contents
.IP "1) Processing in the IM server"
.RS
.IP \(bu
The IM server sends a 
.PN XIMP_PROCESS_END 
to the client.
.IP \(bu
If the front end method, the IM server deletes the key press event
mask of specified window.
.IP \(bu
The IM server ends the service.
.RE
.LP
.IP "2) Processing in the Client"
.RS
.IP \(bu
The client receives the 
.PN XIMP_PROCESS_END
notify from the IM server.
.IP \(bu
If the front end method, the client sets the key press event
mask of that window.
.RE
.LP
.B
Data Format of This ClientMessage
.LP
The IM server sends the following ClientMessage to the client.
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, tab(:), allbox;
n lw(8c).
.vs 15
0:\fBXIMP_PROCESS_END\fP
4:ICID
.TE
.LP
.bp
.NH 2
Focus of the Input Service Windows
.XS
\*(SN Focus of the Input Service Windows
.XE
.LP
The client sends the request of focus of input service windows with 
.PN XIMP_SETFOCUS. 
This means corresponding IC is set focus.  The IM server makes input 
service windows focus. Actually,
the behavior of input service windows depends on the IM server.
.LP
.B
Processing Contents
.IP "1) Processing in the Client"
.RS
.IP \(bu
The client sends the focus request with the ClientMessage.
.RE
.LP "2) Processing in the IM server"
.RS
.IP \(bu
The IM server makes input service windows focus.
.LP
Except this request, IM server redisplays the input service windows
when creating these windows.
.RE
.LP
.B
Data Format of This ClientMessage
.LP
The client sends the following ClientMessage.
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, tab(:), allbox;
n lw(8c).
.vs 15
0:\fBXIMP_SETFOCUS\fP
4:ICID
.TE
.LP
.bp
.NH 2
Unfocus of the Input Service Windows
.XS
\*(SN Unfocus of the Input Service Windows
.XE
.LP
The client sends the request of unfocus of input service windows with 
.PN XIMP_UNSETFOCUS. 
This means corresponding IC is unset focus.
The IM server makes input service windows unfocus. Actually,
the behavior of input service windows depends on the IM server.
.LP
.B
Processing Contents
.IP "1) Processing in the Client"
.RS
.IP \(bu
The client sends the unfocus request with the ClientMessage.
.RE
.LP
.IP "2) Processing in the IM server"
.RS
.IP \(bu
The IM server makes input service windows unfocus.
.RE
.LP
.B
Data Format of This ClientMessage
.LP
The client sends the following ClientMessage.
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, tab(:), allbox;
n lw(8c).
.vs 15
0:\fBXIMP_UNSETFOCUS\fP
4:ICID
.TE
.LP
.bp
.NH 2
Getting the Converted String
.XS
\*(SN Getting the Converted String
.XE
.LP
IM server sends the converted string to the client with compound text.
.LP
.B
Processing Contents
.LP
.IP "1) Processing in the Client"
.RS
.IP \(bu
The client receives the string, and converts that compound text format to
the adopted format depending on the locale specified
in XOpenIM (which is in XIM specification).
.RE
.LP
.IP "2) Processing in the IM server"
.RS
.IP \(bu
IM server sends the resulted string by the ClientMessage or the Property.
This string is compound text.
.RE
.LP
.NH 3
Getting the Converted String by the ClientMessage
.XS
\*(SN Getting the Converted String by the ClientMessage
.XE
.LP
.B
Data Format of This ClientMessage
.LP
The IM server sends the following ClientMessage.
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, tab(:);
| l | c | lw(5c) r |
| c | c | c n |
n | c | c n |. 
.vs 15
0:4:5:19
_
ICID:
_
:Size:Compound Text
.TE
.LP
This ClientMessage's format is 8. The ICID must be processed in
network byte order.
.LP
If a compound text is longer to be sent in one client message,
compound text may be divide into pieces and sent by several client message.
The following is the format in this case.
.TS
center, allbox, tab(:);
c c lw(6c).
.vs 15
ICID:Size:Compound Text
.TE
.TS
center, allbox, tab(:);
c c lw(6c).
.vs 15
ICID:Size:Compound Text
.TE
.LP
.ce
\(bu \(bu \(bu
.TS
center, allbox, tab(:);
c c lw(6c).
.vs 15
ICID:Size:Compound Text
.TE
.LP
"Size" indicates the number of rest bytes of a compound text
which includes the piece of this compound text in that client message.
.LP
.NH 3
Getting the Converted String by the Property
.LP
.IP 1) 
IM server makes a default property used in this communication.
.RS
.TS
tab(|);
l c l.
Property Name|:|depends on the implementation of IM server
Property Type|:|COMPOUND_TEXT
Format|:|8
data|:|resulted string
nelements|:|elements of data
.TE
.RE
.LP
.TS
box, center;
lw(9c).
.vs 15
Compound Text
.TE
.LP
.IP 2)
IM server stores the resulted compound text in this property.
IM server stores it to the client window by XChangeProperty
function with the mode PropModeAppend.
.LP
.IP 3)
After that, IM server sends the client message to the client.
.sp 6p
Data Format
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, tab(:), box;
n | lw(5c) r.
.vs 15
0:\fBXIMP_READPROP\fP
_
4:ICID
_
8:Atom ID of stored property:* stored property
.TE
.LP
.IP 4)
The client receives the 
.PN XIMP_READPROP
client message, gets the result by XGetWindowProperty function with
True in delete. This performs in filter.
.LP
.IP Supplement
The name of this property is not specified in this protocol,
therefore, IM server can prepare each property per each client,
or each property per each ICID, or only one property.
.LP
.bp
.NH 2
Controlling the IM server
.XS
\*(SN Controlling the IM server
.XE
.LP
The client sends the 
.PN XIMP_SETVALUE
request for changing attributes of the input service window which is 
specified in the previous 
.PN XIMP_CREATE
request or 
.PN XIMP_SETVALUE
request.
.LP
.B
Processing Contents
.IP "1) Processing in the Client"
.RS
.IP \(bu
The client sets the attributes of input service windows to Property.
.IP \(bu
The client sends the 
.PN XIMP_SETVALUE
request to the IM server by the ClientMessage.
.RE
.LP
.IP "2) Processing in the IM server"
.RS
.IP \(bu
The IM server gets the attributes from the Property and
changes the attributes of corresponding input service window.
.RE
.LP
.B
Data Format of This ClientMessage
.LP
The client sends the following ClientMessage.
.LP
.ce
Data Field in the XClientMessage Structure
.TS
allbox, tab(:), center;
n lw(8c).
.vs 15
0:\fBXIMP_SETVALUE\fP
4:ICID
8:mask (*)
.TE
.LP
.IP (*) 
This mask is same as 
.PN XIMP_CREATE.
.LP
.NH 2
Moving the Preedit Window
.XS
\*(SN Moving the Preedit Window
.XE
.LP
The client sends the request for moving the location of preedit window with 
.PN XIMP_MOVE. 
This is effective only if the client specified XIMPreeditPosition.
.LP
This makes the same result in the case of that 
.PN XIMP_SETVALUE 
does only moving of the preedit window, but the communication protocol is
more simplified, so it performs faster.
.LP
.B
Processing Contents
.IP "1) Processing in the Client"
.RS
.IP \(bu
The client sends the moving request by the ClientMessage.
.RE
.LP
.IP "2) Processing in the IM server"
.RS
.IP \(bu
The IM server move the pre-edit window.
.RE
.LP
.B
Data Format of This ClientMessage
.LP
The client sends the following ClientMessage.
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:\fBXIMP_MOVE\fP
4:ICID
8:X coordinate
12:Y coordinate
.TE
.LP
.bp
.NH 2
Querying Attributes of IM Server
.XS
\*(SN Querying Attributes of IM Server
.XE
.LP
The client can query the IC informations from the IM server with
.PN XIMP_GETVALUE.
Also the client can query the default attributes which are not 
specified by the client.
.LP
.B
Processing Contents
.IP "1) Processing in the Client"
.RS
.IP \(bu
The client sends the request of 
.PN XIMP_GETVALUE
.IP \(bu
The client receives the reply of 
.PN XIMP_GETVALUE_RETURN,
and gets the information from the specified property.
.RE
.LP
.IP "2) Processing in the IM server"
.RS
.IP \(bu
The IM server sets the corresponding attributes to specified mask
in property, and sends the 
.PN XIMP_GETVALUE_RETURN
reply to the client.
.RE
.LP
.NH 3
Querying Attributes Request
.XS
\*(SN Querying Attributes Request
.XE
.LP
.PN XIMP_GETVALUE 
request uses the following ClientMessage.
.LP
.B
Data Format of This ClientMessage
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, tab(:), allbox;
n lw(8c).
.vs 15
0:\fBXIMP_GETVALUE\fP
4:ICID
8:mask (*)
.TE
.LP
.IP (*) 
This mask is same as 
.PN XIMP_CREATE.
.LP
.NH 3
Querying Attributes Reply
.XS
\*(SN Querying Attributes Reply
.XE
.LP
The IM server returns the following ClientMessage after setting
the corresponding attributes.
.LP
.B
Data Format of This ClientMessage
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, tab(:), allbox;
n lw(8c).
.vs 15
0:\fBXIMP_GETVALUE_RETURN\fP
4:ICID
.TE
.LP
The client gets the information using XGetWindowProperty with
True as the delete argument.
.LP
IM server must use the same properties used in 
.PN XIMP_CREATE.
.LP
.bp
.NH 2
Reset of the IC
.XS
\*(SN Reset of the IC
.XE
.NH 3
Reset IC Request
.XS
\*(SN Reset IC Request
.XE
.LP
.LP
The client sends the 
.PN XIMP_RESET 
request in the X*ResetIC functions.
This performs the processing which is described in the
XIM specification.
.LP
.B
Data Format of This ClientMessage
.LP
The client sends the following ClientMessage.
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, tab(:), allbox;
n lw(8c).
.vs 15
0:\fBXIMP_RESET\fP
4:ICID
.TE
.LP
.NH 3
Reset IC Reply
.XS
\*(SN Reset IC Reply
.XE
.LP
The IM server returns a following reply corresponding with the
.PN XIMP_RESET
request.
.LP
.B
Data Format of This ClientMessage
.LP
The client sends the following ClientMessage.
.LP
.ce
Data Field in the XClientMessage Structure
.TS
allbox, tab(:), center;
n lw(8c).
.vs 15
0:\fBXIMP_RESET_RETURN\fP
4:ICID
8:Atom ID of reset data (*)
.TE
.LP
.RS
.IP (*) 
The Atom ID of reset data.
.LP
.TS
tab(|);
l c l.
Property Name|:|depends on the implementation of IM server
Property Type|:|COMPOUND_TEXT
Format|:|8
data|:|resulted string
nelements|:|elements of data
.TE
.RE
.LP
.TS
allbox, center;
lw(9c).
.vs 15
Compound Text
.TE
.LP
.bp
.NH 2
Error Notify
.XS
\*(SN Error Notify
.XE
.NH 3
Error Notify from IM Server
.XS
\*(SN Error Notify from IM Server
.XE
.LP
In the case of the occurrence of the error at the IM server 
processing, IM server notifies the client of an error using the 
ClientMessage. The format of an error notify message is described
as follows:
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:\fBXIMP_ERROR\fP
4:ICID
8:Error Request Number (*1)
12:Detail Error Number (*2)
16:(Vender Defined Detail Error Number) (*3)
.TE
.LP
.RS
.IP (*1)
Request number which caused this error.
.IP (*2)
The number for error information which describes the reason of the
error in detail. The following is a list of Detail Error Number:
.TS
tab(:);
l n l.
XIMP_NoError:0:/* No error */
XIMP_BadAlloc:1:/* memory allocation fail */
XIMP_BadStyle:2:/* unsupported input style */
XIMP_BadClientWindow:3:/* invalid client window */
XIMP_BadFocusWindow:4:/* invalid focus window */
XIMP_BadArea:5:/* invalid area */
XIMP_BadSpotLocation:6:/* spot location out of range of client/focus window */
XIMP_BadColormap:7:/* invalid colormap id */
XIMP_BadAtom:8:/* invalid atom id */
XIMP_BadPixel:9:/* invalid pixel value in input method colormap */
XIMP_BadPixmap:10:/* invalid pixmap value */
XIMP_BadName:11:/* invalid font name */
XIMP_BadCursor:12:/* invalid cursor id */
.sp
XIMP_BadProtocol:13:/* invalid protocol */
XIMP_BadProperty:14:/* invalid property */
XIMP_BadPropertyType:15:/* invalid property type */
.TE
.LP
.IP (*3)
The vender may add its own detail error information if necessary.
However, the client will ignore if it does not know of the vendor
defined detail error.
.RE
.LP
.NH 3
Abnormal Termination of the IM Server
.XS
\*(SN Abnormal Termination of the IM Server
.XE
.LP
In the case of destroying the IMS window, this library thinks
it abnormal termination of the IM server. And this library performs
initialization and notifys the error to the client program.
.LP
.bp
.NH 1
Callback Protocol
.XS
\*(SN Callback Protocol
.XE
.LP
The client sets XIMPreeditCallback, XIMStatusCallback,
XIMPreeditArea or XIMStatusArea as the XIMStyle, IM server
calls the corresponding callback.
.LP
This is performed by the sending the ClientMessage from the IM
server to the client. The arguments of the callback functions is set in
the data field of the ClientMessage basically, however, if it is
impossible, it is set in the each described Property.
Each formats of the callbacks are the following.
Callbacks requests was following.
.IP "1) ClientMessage requests from IM server to the client"
.TS
tab(:);
cw(7.5c) | cw(5c) | l
l | l | l.
_
.sp 6p
.B
Requests:Contents
.sp 6p
_
.sp 6p
.R
\fBXIMP_GEOMETRY\fP:GeometoryCallBack:5.1.
\fBXIMP_PREEDITSTART\fP:PreeditStartCallback:5.2.
\fBXIMP_PREEDITDONE\fP:PreeditDontCallback:5.3.
\fBXIMP_PREEDITDRAW\fP:PreeditDrawCallback:5.4.
\fBXIMP_PREEDITDRAW_CM\fP:PreeditDrawCallback:5.4.
\fBXIMP_PREEDITDRAW_TINY\fP:PreeditDrawCallback:5.4.
\fBXIMP_PREEDITCARET\fP:PreeditCaretCallback:5.5.
\fBXIMP_STATUSSTART\fP:StatusStartCallback:5.6.
\fBXIMP_STATUSDONE\fP:StatusDoneCallback:5.7.
\fBXIMP_STATUSDRAW\fP:StatusDrawCallback:5.8.
\fBXIMP_STATUSDRAW_CM\fP:StatusDrawCallback:5.8.
.sp 6p
_
.TE
.LP
.IP "2) Reply from the client to the IM server"
.TS
tab(:);
cw(7.5c) | cw(5c) | l
l | l | l.
_
.sp 6p
.B
Reply:Contents
.sp 6p
_
.sp 6p
.R
\fBXIMP_PREEDITSTART_RETURN\fP:PreeditStartCAllback:5.2.
\fBXIMP_PREEDITCARET_RETURN\fP:PreeditCaretCallback:5.5.
.sp 6p
_
.TE
.LP
.bp
.NH 2
Geometry Callback
.XS
\*(SN Geometry Callback
.XE
.LP
.ce
Data Field in the XClientMessage Structure
.TS
allbox, tab(:), center;
n lw(8c).
.vs 15
0:\fBXIMP_GEOMETRY\fP
4:ICID
.TE  
.LP
.bp
.NH 2
Preedit Start Callback
.XS
\*(SN Preedit Start Callback
.XE
.LP
.ce
Data Field in the XClientMessage Structure
.TS
allbox, tab(:), center;
n lw(8c).
.vs 15
0:\fBXIMP_PREEDITSTART\fP
4:ICID
.TE
.LP
And the return value of the callback function is sent
to the IM server by the following ClientMessage.
.LP
.ce
Data Field in the XClientMessage Structure
.TS
allbox, tab(:), center;
n lw(8c).
.vs 15
0:\fBXIMP_PREEDITSTART_RETURN\fP
4:ICID
8:Return Value
.TE
.LP
.bp
.NH 2
Preedit Done Callback
.XS
\*(SN Preedit Done Callback
.XE
.LP
.ce
Data Field in the XClientMessage Structure
.TS
allbox, tab(:), center;
n lw(8c).
.vs 15
0:\fBXIMP_PREEDITDONE\fP
4:ICID
.TE  
.LP
.bp
.NH 2
Preedit Draw Callback
.XS
\*(SN Preedit Draw Callback
.XE
.LP
Protocols using ClientMessages and ones using propertys.
These protocols are used together or independently.
.IP "1) Protocol format for using Property"
.ce
Data Field in the XClientMessage Structure
.TS
allbox, tab(:), center;
n lw(8c).
.vs 15
0:\fBXIMP_PREEDITDRAW\fP
4:ICID
8:Atom 1
12:Atom 2
16:Atom 3
.TE
.LP
.RS
Each Atom 1, 2 and 3 specifies the following each properties.
.IP "Atom 1"
.TS
tab(|);
l c l.
Property Name|:|_XIMP_CALLBACKS_#
||(# is any number, This name is recommendation)
Window|:|IMS window
Property Type|:|_XIMP_PREEDIT_DRAW_DATA
Format|:|32
data|:|Values corresponding the XIM specification with the following format.
nelements|:|3
.TE
.LP
.TS
allbox, tab(:), center;
n lw(8c).
.vs 15
0:caret
4:chg_first
8:chg_length
.TE
.LP
.IP "Atom 2"
.TS
tab(|);
l c l.
Property Name|:|_XIMP_CALLBACKS_#
||(# is any number, This name is recommendation)
Window|:|IMS window
Property Type|:|COMPOUND_TEXT
Format|:|8
data|:|Contents of pre-edit
nelements|:|elements of data
.TE
.LP
.TS
center, allbox;
lw(9c).
.vs 15
Compound Text
.TE
.LP
.IP "Atom 3"
.TS
tab(|);
l c l.
Property Name|:|_XIMP_CALLBACKS_#
||(# is any number, This name is a recommendation)
Window|:|IMS window
Property Type|:|_XIMP_FEEDBACKS
Format|:|32
data|:|attributes of each characters in the contents of pre-edit
nelements|:|elements of data
.TE
.LP
.TS
box, center;
lw(9c).
.vs 15
feedback
.TE
.RE
.LP
.IP "2) Protocol format for using ClientMessages (part 1.)"
This protocol was realized by two ClientMessages.
The IM server does not intrude into this two ClientMessages.
.LP
Format for First ClientMessage
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, box, tab(:);
n | l s
n | l s
n | lw(3.5c) | cw(3.5c)
n | lw(3.5c) | cw(3.5c)
n | l s
n | l s.
.vs 15
0:\fBXIMP_PREEDITDRAW_CM\fP
_
4:ICID
_
8:PreeditDrawCBStatus:caret
_
12:chg_first:chg_length
_
16:feedback
.TE
.LP
.RS
.IP "Note 1."
format == 32, and chg_first, chg_length are into 12-15 byte
field, they are using 16 bits and they are Network Order.
.IP "Note 2."
The PreeditDrawCBStatus is a BIT array of follows.
.RS
.IP "0x0001 no_text:"
if set, string == NULL and no following client message,
else string exists.
.IP "0x0002 no_feedback:"
if set, feedback == NULL, else feedback exists.
.IP "0x0004 feedbacks_via_property:"
if set, feedback field has atom# that indicates the
property that has XIMFeedback values of string field,
else feedback field has one XIMFeedback value
for all of the characters in string field. 
.RE
.LP
.IP "Note 3."
The feedbacks_via_property bit == 0  means the XIMFeedback value of
all characters are same.
If the string has more than two XIMFeedback values,
it must be divided. Therefore, one protocol includes
one XIMFeedback value.
.RE
.LP
Format for Second ClientMessage
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, tab(:);
| l | l s s s s | cw(4c) r |
| l | l | l | l | l | l | cw(4c) | r |
l | l | c s s s s s |
l s s s s s s s.
.vs 15
0:4   5   6   7   8::19
_
ICID
_
::Compound Text
         Size
.TE
.LP
.RS
format == 8, ICID is set in the 0-3 byte field using Network Order.
.RE
.LP
.bp
.IP "3) Protocol format for using ClientMessages (part 2.)"
This protocol is realized by only one ClientMessage.
This is used when a tiny change is made in a preedit draw data.
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, box, tab(:);
n | l s s
n | l s s
n | lw(2.2c) | cw(2.2c) | cw(2.2c)
n | l s s.
.vs 15
0:\fBXIMP_PREEDITDRAW_TINY\fP
_
4:ICID
_
8:chg_first:chg_length:length
_
12:string (COMPOUND TEXT)
_
16:string (continued.)
.TE
.LP
.RS
.IP "Note 1."
format == 32, and chg_first, chg_length, length are stored into 
8-11 byte field, and chg_first is represented by 16bits,
chg_length, length are represented by 8 bits, 
and they are Network Order.
.IP "Note 2."
New caret position is the end of changed area of preedit string.
And caret value is omitted because it can be calculated by
information in this protocol.
.IP "Note 3."
This protocol implies the feedback field == NULL.
.IP "Note 4."
String is stored into 12-15 and 16-19 byte field, and using 8 bits 
format, it is Network Order.
.RE
.LP
.bp
.NH 2
Preedit Caret Callback
.XS
\*(SN Preedit Caret Callback
.XE
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, tab(:), allbox;
n lw(8c).
.vs 15
0:\fBXIMP_PREEDITCARET\fP
4:ICID
8:position
12:direction
16:style
.TE
.LP
.RS
.TS
tab(|);
l s s
lw(1c) l n.
direction:
.sp 6p
|XIMForwardChar|0
|XIMBackwardChar|1
|XIMForwardWord|2
|XIMBackwardWord|3
|XIMCaretUp|4
|XIMCaretDown|5
|XIMNextLine|6
|XIMPreviousLine|7
|XIMLineStart|8
|XIMLineEnd|9
|XIMAbsolutePosition|10
|XIMDontChange|11

style:
.sp 6p
|XIMInvisible|0
|XIMPrimary\1
|XIMSecondary|2
.TE
.RE
.LP
The return value of callback function is sent by the
client message to the IM server with the following format.
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, tab(:), allbox;
n lw(8c).
.vs 15
0:\fBXIMP_PREEDITCARET_RETURN\fP
4:ICID
8:return value (new position)
.TE
.bp
.NH 2
Status Start Callback
.XS
\*(SN Status Start Callback
.XE
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, tab(:), allbox;
n lw(8c).
.vs 15
0:\fBXIMP_STATUSSTART\fP
4:ICID
.TE
.LP
.bp
.NH 2
Status Done Callback
.XS
\*(SN Status Done Callback
.XE
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, tab(:), allbox;
n lw(8c).
.vs 15
0:\fBXIMP_STATUSDONE\fP
4:ICID
.TE  
.LP
.bp
.NH 2
Status Draw Callback
.XS
\*(SN Status Draw Callback
.XE
.LP
.LP
Protocol using ClientMessage and ones using propertys.
These protocols are use together or independently.
.IP "1) Protocol Format for Using a Property"
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, tab(:), allbox;
n lw(8c).
.vs 15
0:\fBXIMP_STATUSDRAW\fP
4:ICID
8:type
12:bitmap or Atom 1
16:NULL or Atom 2.
.TE 
.LP
.RS
.IP "Type is defined as the following."
.TS
tab(|);
l c lw(10c).
0|:|Text Type
1|:|Bitmap Type
.TE
.LP
.IP "bitmap or Atom 1."
.TS
tab(|);
l c lw(10c).
type == 0|:|T{
The Atom id specifies the property including the string using to display 
in status area.
T}
type == 1|:|The ID of bitmap to display in status area.
.TE
.LP
.IP "NULL or Atom 2."
.TS
tab(|);
l c lw(10c).
type == 0|:|T{
The Atom id specified the property including the attributes of each characters.
T}
type == 1|:|Not used.
.TE
.LP
.IP "Atom 1."
.TS
tab(|);
l c l.
Property Name|:|_XIMP_CALLBACKS_#
||(# is any number, This name is recommendation)
Window|:|IMS window
Property Type|:|COMPOUND_TEXT
Format|:|8
data|:|string to display in status area
nelements|:|elements of data
.TE
.LP
.TS
center, allbox;
lw(9c).
.vs 15
compound Text
.TE
.LP
.IP "Atom 2."
.TS
tab(|);
l c l.
Property Name|:|_XIMP_CALLBACKS_#
||(# is any number, This name is recommendation)
Window|:|IMS window
Property Type|:|_XIMP_FEEDBACKS
Format|:|32
data|:|attributes of each characters
nelements|:|elements of data
.TE
.LP
.TS
center, allbox;
lw(9c).
.vs 15
feedback
.TE
.RE
.LP
.IP "2) Protocol format for using ClientMessages"
This protocol was provided for use two ClientMessages
The IM server does not intrude into this two ClientMessages.
.LP
.RS
Format for first ClientMessage.
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, tab(:), allbox;
n lw(8c).
.vs 15
0:\fBXIMP_STATUSDRAW_CM\fP
4:ICID
8:type
12:bitmap or feedback
.TE
.LP
.IP "Note 1."
format == 32
.IP "Note 2."
The feedback field has one XIMFeedback value for all of
the characters which are contained in the following ClientMessage.
This means the XIMFeedback value of all characters are same.
If the string has more than two XIMFeedback values,
it must be divided. Therefore, one protocol includes
one XIMFeedback value.
.LP
Format for second ClientMessage
.LP
.ce 
Data Field in the XClientMessage Structure
.TS
center, tab(:);
| l | l s s s s | cw(4c) r |
| l | l | l | l | l | l | cw(4c) | r |
l | l | c s s s s s |
l s s s s s s s.
.vs 15
0:4   5   6   7   8::19
_
ICID
_
::Compound Text
         Size
.TE
.LP
format == 8, ICID was setting in the 0-3 byte field using Network Order.
.RE
.LP
.bp
.NH 1
Vendor Specific Extensions Protocol
.XS
\*(SN Vendor Specific Extensions Protocol
.XE
.LP
Vendor dependent extensions mean the IM server functions which
is impossible to include in the common interface depending the
situation of the application or the each native language.
Therefore, it depends on the implementation of the IM server.
The client must perform the minimum processing even if the
IM server doesn't have a extension.
.LP
Specific usage is the following examples.
.IP \(bu
Setting the status window.
.IP \(bu
Selecting the front-end method or back-end method.
.IP \(bu
Selecting the mode of listing candidates.
.IP \(bu
Switching the language on IC.
.IP \(bu
etting the input mode depending on the native language.
.br
(e.g. latin-KANA conversion mode, alphanumeric mode).
.LP
.NH 2
Format
.XS
\*(SN Format
.XE
.LP
The Atom name of the vendor dependent function is defined as
the following rules.
.RS
.TS
tab(|);
r c l.
<Property Name>|::|_XIMP_EXT_<Vendor Name>_<Function Name>

<Vendor Name>|::|Specifies the vendor name as a string
<Function Name>|::|Specifies the function name as a string
.TE
.RE
.LP
The IM server registers the vendor extensions to the IMS window
with the following format.
.RS
.TS
tab(:);
l l.
Property Name:_XIMP_EXTENSIONS
Type:_XIMP_EXTENSIONS
Format:32
data:List of the Atom of the vendor extensions
.TE
.RE
.LP
If the above Property is declared, client messages in the following 
format can be used in order to invoke the vendor specific functions.
IM server can responds to it with the same format ClientMessage.
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, tab(:), allbox;
n lw(8c).
.vs 15
0:\fBXIMP_EXTENSION\fP
4:ICID
8:Atom ID of Vendor Extension
12:data
16:data
.TE 
.LP
The format of data fields is not restricted.
.LP
.IP Note.
Any IM server with vendor specific must not send the ClientMessage
to the library that does not expect the vendor specific.
.LP
.bp
.NH 1
Acknowledgements
.XS
\*(SN Acknowledgements
.XE
.LP
The following people are participated to draft and review this document.
.DS
C.Ko (III)
S.Hung (CCL/ITRI)
D.Lee (KAIST)
J.Yoon (KAIST)
S.Whang (KAIST)
K.Kudo (USLP)
.DE
.LP
Special thanks for meeting cooperation,
.DS
G.Watson (UI)
Y.Hiratsuka (UIAP)
.DE
.LE
.LP
.TC
