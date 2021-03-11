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
Makiko Shimamura
.AI
FUJITSU LIMITED
.AU
Masaki Takeuchi
Makoto Wakamatsu
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
Hiromu Inukai
.AI
Nihon Sun Microsystems K.K.
.AU
Katsumi Kudo
.AI
UNIX System Laboratories Pacific, Ltd.
.sp 4
Revision 4.0		 26 March, 1992
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
Copyright 1991, 1992 FUJITSU LIMITED
Copyright 1991, 1992 Sony Corporation
Copyright 1991, 1992 Fuji Xerox Co., Ltd.
Copyright 1991, 1992 Oki Technosystems Laboratory, Inc.
Copyright 1991, 1992 Toshiba Corporation
Copyright 1991, 1992 Nihon Sun Microsystems K.K.
Copyright 1991, 1992 UNIX System Laboratories Pacific, Ltd.
.DE
.sp 3
.LP
Permission to use, copy, modify, distribute, and sell this documentation
for any purpose without fee is hereby granted, provided that the above
copyright notice appear in all copies and that both that copyright notice
and this permission notice are retained, and that the name of FUJITSU 
LIMITED, Sony Corporation, Fuji Xerox Co., Ltd., Oki Technosystems 
Laboratory, Inc., Toshiba Corporation, Nihon Sun Microsystems K.K., UNIX 
System Laboratories Pacific, Ltd., not be used in advertising or publicity 
pertaining to this document without specific, written prior permission.
FUJITSU LIMITED, Sony Corporation, Fuji Xerox Co., Ltd., Oki Technosystems 
Laboratory, Inc., Toshiba Corporation, Nihon Sun Microsystems K.K., UNIX 
System Laboratories Pacific, Ltd., make no representations about the 
suitability of this document or the protocol defined in this document for 
any purpose.  It is provided "as is" without express or implied warranty.
.ps 11
.nr PS 11
.bp 1
.EH '\fBXIMP 4.0\fP'''
.OH '\fBXIMP 4.0\fP'''
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
The MIT X Consortium has developed a specification for an input method, 
called the "X Window System, Version 11 Input Method Specifications" 
(XIM specification). This specification provides a common interface to 
an input library for a string which depends on a native language. 
With the XIM specification, an application programmer can write software 
supporting a native language with unique source code without a knowledge 
of the library implementation.
.LP
With the client-server IM system, however, there is no guarantee of the 
interoperability between heterogeneous environments such as the one 
provided by XIM itself.
.LP
The XIM input server protocol specification document makes it possible 
for one input server to connect to multiple clients on a display. 
This document describes a protocol specification between the input server 
and the clients that is able to do the implementation based on the XIM 
specification.
.LP
.NH 2
Characteristics
.XS
\*(SN Characteristics
.XE
.LP
The basic ideas that are addressed by this protocol is listed below:
.IP \(bu 
This protocol preserves network interoperability between heterogeneous 
environments.
.IP \(bu
This protocol is able to separate the input system from clients and 
supports a distributed system.
.IP \(bu
This protocol doesn't require any special communication technique except 
for the X protocol.
.IP \(bu
This protocol doesn't depend on a specific native language.
.IP \(bu
This protocol can provide multi-language services on one display.
.IP \(bu
This protocol can provide a callback method.
.IP \(bu
This protocol supports both the Front-End method and the Back-End method.
.LP
.NH 2
Basic Concept
.XS
\*(SN Basic Concept
.XE
.LP
.IP "IM Server"
Server process which provide a native language input function.
This protocol supports both the Front-End method and Back-End method.
.IP "IM Client"
Client process which asks the IM Server for input service.
.IP "IMS Window"
The Input Method Service Window.  The window which the IM Server uses to 
receive the communication from the client.  In the multi-lingual case, 
there are windows that correspond to each language.
.IP "Front-End Method"
The method where the key press event is passed to the IM Server directly, 
not via the client.
.IP "Back-End Method"
The method where the key press event is passed to the IM Server through 
the client.
.IP "Callback Method"
The method which used by the application's callback routines to display 
the strings for preedit, status and other input related information.
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
This protocol uses the ClientMessage and the Window Property (property)
capabilities which are provided by the X Window System.
.IP \(bu
This protocol uses a ClientMessage event in the case of small data.
.IP \(bu
This protocol uses a property in the case of big data.
.LP
The following sector describes the common format of the data.
.LP
.NH 2
Format from the Client to the IM Server 
.XS
\*(SN Format from the Client to the IM Server 
.XE
.LP
The library function generates a input service request and sends it to 
the IM Server with the ClientMessage event and the property. 
.LP
.NH 3
ClientMessage
.XS
\*(SN ClientMessage
.XE
.LP
The ClientMessage event's format is shown below:
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
Property
.XS
\*(SN Property
.XE
.LP
This is performed by the XChangeProperty function with the data for each 
request.
.LP
The library changes the property using the XChangeProperty function with 
the following arguments.
.RS
.TS H
tab(:);
c s | c
lw(2c) lw(2.5c) | l.
_
.sp 6p
.B
Argument:Contents
.sp 6p
_
.sp 6p
.TH
.R
Display:*display:Specified in XOpenIM
Window:window:Client window ID specified in IC (*)
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
IC is a resource ID of Input Context, which is specified in the XIM 
specification.
.RE
.LP
The client changes the property with the mode of PropModeAppend and the 
IM Server reads it with the delete mode (delete == True).
.LP
.bp
.NH 2
Format from the IM Server to the Client
.XS
\*(SN Format from the IM Server to the Client
.XE
.LP
The IM Server uses the ClientMessage event and the property to reply to 
the request from the client, for notification of the completion of 
processing, and for notification of an error. The IM Server uses the 
property for the IM Server's Characteristics data (3.3). 
.LP
.NH 3
ClientMessage
.XS
\*(SN ClientMessage
.XE
.LP
The ClientMessage event's format is shown below.
.RS
.TS H
tab(:);
c s | c
lw(2c) lw(2.5c) | l.
_
.sp 6p
.B
Structure Member:Contents
.sp 6p
_
.TH
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
long:data[5]:(to be described in each request)
.sp 6p
_
.TE
.LP
.IP (*)
The format depends on the contents of the notification.
.RE
.LP
.NH 3
Property
.XS
\*(SN Introduction
.XE
.NH 4
Property for Reply and Notification 
.XS
\*(SN Property for Reply and Notification 
.XE
.LP
The IM Server changes the property using the XChangeProperty function 
with the following arguments.
.RS
.TS H
tab(:);
c s | c
lw(2c) lw(2.5c) | l.
_
.sp 6p
.B
Argument:Contents
.sp 6p
_
.sp 6p
.TH
.R
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
The IM Server changes the property with the mode of PropModeAppend and 
the client reads it with the delete mode (delete == True).
.LP
.bp
.NH 4
Property for IM Server's Characteristics Data 
.XS
\*(SN Property for IM Server's Characteristics Data 
.XE
.LP
The IM Server changes the property using the XChangeProperty function 
with the following arguments.
.RS
.TS H
tab(:);
c s  | c
lw(2c) lw(2.5c)  | l.
_
.sp 6p
.TH
.B
Argument:Contents
.sp 6p
_
.sp 6p
.sp 6p
.R
Display:*display:IM Server's Display
Window:window:IMS Window ID
Atom:property:to be described in each request
Atom:type:to be described in each request
int:format:to be described in each request
int:mode:PropModeReplace
u_char:*data:to be described in each request
int:nelements:to be described in each request
.sp 6p
_
.TE
.RE
.LP
The IM Server changes the property with the mode of PropModeAppend and 
the client reads it with the delete mode (delete == False).
.LP
.bp
.NH 1
Initial Processing in IM Server
.XS
\*(SN Initial Processing in IM Server
.XE
.LP
The IM Server sets the following during initialization.
.IP \(bu
Creation of the Atom declaring the language
.IP \(bu
Declaration the IMS Window
.IP \(bu
Registration of the attributes of the IM Server
.LP
.NH 2
Creation of the Atom declaring the input language
.XS
\*(SN Creation of the Atom declaring the input language
.XE
.LP
The IM Server registers the name of input language with the following 
format.
.DS
Basic Declaration of Input	_XIMP_<localename>
Detailed Declaration of Input	_XIMP_<localename>@<servername>.<screen>
.sp
	<localename>	locale name
	<servername>	IM Server name
	<screen>		screen number
.DE
.LP
The IM Server must at least declare basic declaration format.
.LP
This localename format is compliant with POSIX and XPG3, using the 
following format.
.DS
<language>[_<territory>]
.DE
.LP
This protocol uses ISO 639 (language code) in "language" and ISO 3166 
(country code) in "territory".
.LP
In case of multiple Atoms for a locale, choose the longest name.
.RS
.TS
tab(;);
l s s
lw(1c) l l.
For example:
.sp 6p
;_XIMP_ja;Japanese
;_XIMP_ja_JP
;_XIMP_ja_JP@xxx.0
;_XIMP_ko;Korean
;_XIMP_ko_KR
;_XIMP_ko_KR@xxx.0
;_XIMP_zh_CN;Chinese in Mainland China
;_XIMP_zh_CN@xxx.0
;_XIMP_zh_TW;Chinese in R.O.C.
;_XIMP_zh_TW@xxx.0
.TE
.RE
.LP
.NH 2
Declaration of the IMS Window
.XS
\*(SN Declaration of the IMS Window
.XE
.LP
The IM Server creates a window (IMS Window) for communication between 
clients and the IM Server depending on the input language. An IMS Window 
is declared as a selection owner of the Atom which declares the input 
language. 
.LP
.bp
.NH 2
Registration of the Attributes of the IM Server
.XS
\*(SN Registration of the Attributes of the IM Server
.XE
.LP 
IM Server registers the following information to an IMS Window as a 
property. The detailed information shown below.
.RS
.TS
tab(:);
lw(6c) l.
_XIMP_VERSION:Protocol version
_XIMP_STYLE:List of supported styles
_XIMP_TYPE:Type of protocol
.TE
.RE
.LP
The above attributes must be set by the IM Server. The following 
attributes are optional.
.RS
.TS
tab(:);
lw(6c) l.
_XIMP_SERVERNAME:Name of IM Server
_XIMP_SERVERVERSION:Version of IM Server
_XIMP_VENDORNAME:Name of vendor
_XIMP_SPROC_STARTED_KEYS:Start of keys
_XIMP_SPROC_STOPPED_KEYS:Last of keys
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
center, allbox;
lw(9c).
.vs 15
XIMP.4.0
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
data|:|List of XIMStyle that are described in the XIM specification
nelements|:|The number of styles which are supported by the IM Server
.TE
.RE
.LP
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:\&
4:\&
\&
.TE
.LP
.bp
.NH 3
_XIMP_TYPE
.XS
\*(SN _XIMP_TYPE
.XE
.LP
.RS
.TS
tab(|);
l c l.
Property Name|:|_XIMP_TYPE
Property Type|:|_XIMP_TYPE
Format|:|32
data|:|List of bitmasks, each of which specifies the IM Server supported 
style nelements|:|The number of types that the IM Server supports
.TE
.RE
.LP
.TS
center, box, tab(:);
n | l | l.
.vs 15
0:each element is described below:priority high
_
4
_

::priority low
.TE
.LP
Current possible values are a combination of the following:
.TS
center, tab(:);
| lw(3c) | n | n | n | n | n | n |
l n n n n n n.
_
\&
_
:5:4:3:2:1:0
.TE
.LP
.RS
.IP "0 : Front-End"
.IP "1 : Back-End"
.IP "2 : Type1"
library recognizes registered keys and notifies a server to start to 
processing key events.
.IP "3 : Type2"
library does not recognize any registered keys.  IM Server will always 
be the first to process key events.
.IP "4 : Type3"
This type is only occurred in the case of Front-End. Key events are 
always passed to both of IM Server and the library. Both of them 
recognize registered keys.
.IP "5 : Synchronous-Transfer"
KeyPress is transferred synchronously. (See 4.1.) 
(This bit has meaning only when the Back-End bit is ON.)
.RE
.LP
Protocol details are determined by the type. Type dependent Protocols 
are described below.
.TS
center, tab(:);
l | c | c | c | c | c.
_
.sp 6p
.B
Protocol Name:FEP/Type1:FEP/2:FEP/3:BEP/1:BEP/2
.sp 6p
_
.sp 6p
.R
\fBXIMP_REG_KEY_PRESSED\fP:mandatory:none:optional:mandatory:none
\fBXIMP_SPROC_STARTED\fP:optional:none:optional:none:none
\fBXIMP_SPROC_STOPPED\fP:mandatory:none:optional:mandatory:none
.sp 6p
_
.TE
.LP
.TS
center, tab(:);
l c c c.
_
.sp 6p
.B
:Optional:Mandatory:None
.R
.sp 6p
_
.sp 6p
\fBReceiver\fP:need not receive message:must receive message:don't receive message
.sp 6p
_
.sp 6p
\fBSender\fP:must send message:must send message:don't send message
.sp 6p
_
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
center, allbox;
lw(9c).
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
center, allbox;
lw(9c).
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
center, allbox;
lw(9c).
.vs 15
\&
.TE
.LP
.NH 3
_XIMP_SPROC_STARTED_KEYS
.XS
\*(SN _XIMP_SPROC_STARTED_KEYS
.XE
.LP
.RS
.TS
tab(|);
l c l.
Property Name|:|_XIMP_SPROC_STARTED_KEYS
Property Type|:|_XIMP_SPROC_STARTED_KEYS
Format|:|32
data|:|Modifier, modifier mask and keysym for judging the key event of start
nelements|:|The number of keys * 3
.TE
.RE
.LP
.TS
center, box, tab(:);
n | lw(8c). 
.vs 15
0:modifier
_
4:modifier_mask
_
8:keysym
\&
\&
_
.TE
.LP
.bp
.NH 3
_XIMP_SPROC_STOPPED_KEYS
.XS
\*(SN _XIMP_SPROC_STOPPED_KEYS
.XE
.LP
.RS
.TS
tab(|);
l c l.
Property Name|:|_XIMP_SPROC_STOPPED_KEYS
Property Type|:|_XIMP_SPROC_STOPPED_KEYS
Format|:|32
data|:|Modifier, modifier mask and keysym for judging the key event of stop
nelements|:|The number of keys * 3
.TE
.RE
.LP
.TS
center, box, tab(:);
n | lw(8c).
.vs 15
0:modifier
_
4:modifier_mask
_
8:keysym
_
\&
\&
\&
_
.TE
.LP
.bp
.NH 1
Basic Request
.XS
\*(SN Basic Request
.XE
.LP
There are the following requests and replies between IM Server and clients.
.IP 1)
Request from the client to the IM Server.
.TS H
tab(:);
cw(7.5c) | cw(5c) | c
l | l | l.
_
.sp 6p
.B
Request:Contents:Sect. #
.sp 6p
_
.sp 6p
.TH
.R
\fBXIMP_KEYPRESS\fP:Forward KeyPressEvent:4.1 
\fBXIMP_KEYRELEASE\fP:Forward KeyReleaseEvent:4.1
\fBXIMP_CREATE\fP:Open Connection Request:4.2.1 
\fBXIMP_DESTROY\fP:Close Connection Request:4.3 
\fBXIMP_REG_KEY_PRESSED\fP:Start Conversion Request:4.4 
\fBXIMP_SETFOCUS\fP:Focus Window Request:4.5 
\fBXIMP_UNSETFOCUS\fP:Unfocus Window Request:4.6 
\fBXIMP_CLIENTWINDOW\fP:Changing the Client Window:4.8.1 
\fBXIMP_FOCUSWINDOW\fP:Changing the Focus Window:4.8.2 
\fBXIMP_MOVE\fP:Move Preedit Window Request:4.8.3 
\fBXIMP_SETVALUE\fP:Set Value Request:4.8.4 
\fBXIMP_GETVALUE\fP:Get Value Request:4.9.1
\fBXIMP_RESET\fP:Reset IM Server Request:4.10.1
\fBXIMP_EVENTMASK_NOTIFY\fP:Event Mask Notification:4.11
.sp 6p
_
.TE
.LP
.IP 2)
Reply from the IM Server to the client
.TS H
tab(:);
cw(7.5c) | cw(5c) | c
l | l | l.
_
.sp 6p
.B
Reply:Contents:Sect. #
.sp 6p
_
.sp 6p
.TH
.R
\fBXIMP_KEYPRESS\fP:Forward KeyPressEvent:4.1
\fBXIMP_KEYPRESS_RETURN\fP:Forward KeyPressEvent:4.1
\fBXIMP_KEYRELEASE\fP:Forward KeyReleaseEvent:4.1
\fBXIMP_KEYRELEASE_RETURN\fP:Forward KeyReleaseEvent:4.1
\fBXIMP_CREATE_RETURN\fP:Return ICID:4.2.3
\fBXIMP_SPROC_STARTED\fP:Process Start Notification:4.4
\fBXIMP_SPROC_STOPPED\fP:Process Stop Notification:4.4
\fBXIMP_READPROP\fP:Return Converted String:4.7.1
\fBXIMP_CLIENTWINDOW_RETURN\fP:Return Client Window ID:4.8.1
\fBXIMP_FOCUSWINDOW_RETURN\fP:Return Focus Window ID:4.8.2
\fBXIMP_GETVALUE_RETURN\fP:Return Attributes:4.9.2
\fBXIMP_RESET_RETURN\fP:Reset IM Server Return:4.10
\fBXIMP_EVENTMASK_NOTIFY_RETURN\fP:Event Mask Notification:4.11.2
\fBXIMP_ERROR\fP:Error Notification:4.12.1
.sp 6p
_
.TE
.LP
.bp
.NH 2
Forward KeyEvent
.XS
\*(SN Forward KeyEvent
.XE
.LP
These protocols will be used in the case of a KeyPress Event or a 
KeyRelease Event. These protocols are used with both Front-End and 
Back-End methods.  The Front-End method forwards a intercepted key event 
to the client which isn't used in the IM. The Back-End method sends the 
key event to the IM Server.
.LP
It's easy to send the key event using the XSendEvent function, but there 
is a security problem because all clients can send the key event. 
More secure applications can ignore the key event by sending by the 
XSendEvent.
.LP
Considering the security problem, clients and the IM Server use an 
established channel based on this protocol to send the key event. Using 
this method seems to be better than using the simple XSendEvent method.
.LP
.RS
Data Format (for KeyPress)
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
16:time stamp
.TE
.LP
.RS
3 members of the XKeyEvent, keycode, state and time stamp, are sent in 
this ClientMessage.
.RE
.LP
Data Format (for KeyRelease)
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:X\fBIMP_KEYRELEASE\fP
4:ICID
8:keycode
12:state
16:time stamp
.TE
.LP
.RS
3 members of the XKeyEvent, keycode, state and time stamp, are sent in 
this ClientMessage.
.RE
.RE
.LP
.IP "Synchronous Transfer"
When the type of a protocol is "Synchronous-Transfer", KeyPress Event 
and KeyRelease Event are treated synchronously. Specifically, IM Server 
returns the following reply to a 
.PN XIMP_KEYPRESS
request and 
.PN XIMP_KEYRELEASE
request from a client. The request from IM Server to the client which 
was generated on the trigger of the 
.PN XIMP_KEYPRESS
request or 
.PN XIMP_KEYRELEASE 
request (for example, a callback request) should be sent before this 
reply. The client shall postpone processing the normal XEvent till 
receiving this reply.
.LP
.bp
.RS
Data Format (for KeyPress)
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:\fBXIMP_KEYPRESS_RETURN\fP
4:ICID
.TE
.LP
Data Format (for KeyRelease)
.LP
.ce
Data Field in the XClientMessage Structure
.vs 15
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:\fBXIMP_KEYRELEASE_RETURN\fP
4:ICID
.TE
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
The client performs the following procedure before sending input service 
requests.
.RS
.IP \(bu
Get the IMS Window id
.IP \(bu
Set the attributes used in initial connection
.IP \(bu
Send the request for connection
.IP \(bu
Receive the reply which includes the ICID from IM Server
.RE
.LP
.IP "2) Processing in IM Server"
.RS
.IP \(bu
Registration of the client
.IP \(bu
Reply which includes the ICID corresponding to the client
.RE
.LP
.NH 3
Setting the IC Information
.XS
\*(SN Setting the IC Information
.XE
.LP
The clients set the following property before the connection request, 
and the IM Server reads it at during the initialization of the connection. 
It is possible to set or change this information even while using 
.PN XIMP_SETVALUE.
.DS
_XIMP_VERSION		protocol version using the client
_XIMP_FOCUS			Current focus window
_XIMP_PREEDIT		Detailed information about preedit window
_XIMP_PREEDITFONT	Font information about preedit window
_XIMP_STATUS			Detailed information about status window
_XIMP_STATUSFONT		Font information about status window
_XIMP_TYPE			Type of protocol
.DE
.LP
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
XIMP.4.0
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
Window ID
.TE
.bp
.LP
.IP "\(bu _XIMP_PREEDIT Property"
.sp 6p
The client registers the following property to set the preedit window's 
attributes.
.TS
tab(|);
l c l.
Property Name|:|_XIMP_PREEDIT
Property Type|:|_XIMP_PREEDIT
Format|:|32
data|:|Values corresponding the XIM specification with the following format.
nelements|:|15
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
16:AreaNeeded.width
20:AreaNeeded.height
24:SpotLocation.x
28:SpotLocation.y
32:Colormap
36:Std_Colormap
40:Foreground
44:Background
48:Bg Pixmap
52:LineSpacing
56:Cursor
.TE
.LP
.bp
.IP "\(bu _XIMP_STATUS Property"
.sp 6p
The client registers the following property to set the status window's 
attributes.
.TS
tab(|);
l c l.
Property Name|:|_XIMP_STATUS
Property Type|:|_XIMP_STATUS
Format|:|32
data|:|Values corresponding the XIM specification with the following format
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
16:AreaNeeded.width
20:AreaNeeded.height
24:Colormap
28:Std_Colormap
32:Foreground
36:Background
40:Bg Pixmap
44:LineSpacing
48:Cursor
52:WID of status window (*)
.TE
.LP
.RS
.IP (*)
Status window id if the client want to set it. If this ID is not zero, 
the IM Server uses it to display status. (XIMP extension)
.RE
.LP
.IP "\(bu _XIMP_PREEDITFONT Property"
.TS
tab(|);
l c l.
Property Name|:|_XIMP_PREEDITFONT
Property Type|:|XA_STRING
Format|:|8
data|:|String describing the base font name list
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
data|:|String describing the base font name list
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
.IP "\(bu _XIMP_TYPE Property"
.sp 6p
This property is optional. If this property isn't set by The client, then 
the IM Server choses the first option provided by the IM Server.
.TS
tab(|);
l c l.
Property Name|:|_XIMP_TYPE
Property Type|:|_XIMP_TYPE
Format|:|32
data|:|bitmask which specifies the chosen style
nelements|:|1
.TE
.LP
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:\&
.TE
.LP
.bp
.NH 4
Request for Establishing the Connection
.XS
\*(SN Request for Establishing the Connection
.XE
.LP
The client sends the following ClientMessage to the IMS Window.
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
12:mask (*1)
16:FocusWindow Select-event-mask (*3)
.TE
.LP
.IP (*1)
The mask is as follows.
.TS H
tab(:);
c | c | c
l | c | l.
_
.sp 6p
.B
Name:Value:Contents
.sp 6p
_
.sp 6p
.TH
.R
\fBXIMP_FOCUS_WIN_MASK\fP:(1L<< 0):_XIMP_FOCUS (*2)
\fBXIMP_PRE_AREA_MASK\fP:(1L<< 1):_XIMP_PREEDIT Area
\fBXIMP_PRE_AREANEED_MASK\fP:(1L<< 2):_XIMP_PREEDIT AreaNeeded
\fBXIMP_PRE_COLORMAP_MASK\fP:(1L<< 3):_XIMP_PREEDIT Colormap
\fBXIMP_PRE_STD_COLORMAP_MASK\fP:(1L<< 4):_XIMP_PREEDIT Std Colormap
\fBXIMP_PRE_FG_MASK\fP:(1L<< 5):_XIMP_PREEDIT Foreground
\fBXIMP_PRE_BG_MASK\fP:(1L<< 6):_XIMP_PREEDIT Background
\fBXIMP_PRE_BGPIXMAP_MASK\fP:(1L<< 7):_XIMP_PREEDIT Bg Pixmap
\fBXIMP_PRE_LINESP_MASK\fP:(1L<< 8):_XIMP_PREEDIT LineSpacing
\fBXIMP_PRE_CURSOR_MASK\fP:(1L<< 9):_XIMP_PREEDIT Cursor
\fBXIMP_PRE_SPOTL_MASK\fP:(1L<<10):_XIMP_PREEDIT SpotLocation
\fBXIMP_STS_AREA_MASK\fP:(1L<<11):_XIMP_STATUS Area
\fBXIMP_STS_AREANEED_MASK\fP:(1L<<12):_XIMP_STATUS AreaNeeded
\fBXIMP_STS_COLORMAP_MASK\fP:(1L<<13):_XIMP_STATUS Colormap
\fBXIMP_STS_STD_COLORMAP_MASK\fP:(1L<<14):_XIMP_STATUS Std Colormap
\fBXIMP_STS_FG_MASK\fP:(1L<<15):_XIMP_STATUS Foreground
\fBXIMP_STS_BG_MASK\fP:(1L<<16):_XIMP_STATUS Background
\fBXIMP_STS_BGPIXMAP_MASK\fP:(1L<<17):_XIMP_STATUS Bg Pixmap
\fBXIMP_STS_LINESP_MASK\fP:(1L<<18):_XIMP_STATUS LineSpacing
\fBXIMP_STS_CURSOR_MASK\fP:(1L<<19):_XIMP_STATUS Cursor
\fBXIMP_STS_WINDOW_MASK\fP:(1L<<20):_XIMP_STATUS WID of status
\fBXIMP_PRE_FONT_MASK\fP:(1L<<21):_XIMP_PREEDITFONT
\fBXIMP_STS_FONT_MASK\fP:(1L<<22):_XIMP_STATUSFONT
\fBXIMP_SERVERTYPE_MASK\fP:(1L<<23):_XIMP_TYPE (*2)
.sp 6p
_
.TE
.LP
.IP (*2)
.PN XIMP_FOCUS_WIN_MASK
and 
.PN XIMP_SERVERTYPE_MASK
is validated only with 
.PN XIMP_CREATE
request.
.IP (*3)
FocusWindow Select-event-mask is the value of SelectInputMask for the 
Focus Window. With Front-End method, it is used as a hint for 
XSelectInputMask of events.
.LP
.bp
.NH 3
Receiving the ICID
.XS
\*(SN Receiving the ICID
.XE
.LP
The clients receive the ICID with the following format. If there are 
any errors, the clients receive the error notification described in the 
.PN XIMP_ERROR
protocol (in section 4.12).
.LP
The ClientMessage from the IM Server.
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
The client sends the end request to the IM Server using 
.PN XIMP_DESTROY
to notify of the end of the input service request.
.LP
.B
Processing Contents
.IP "1) Processing in the Client"
.RS
.IP \(bu
The client sends the ClientMessage to the IM Server to request the end 
of input service.
.IP \(bu
The client frees the resources which were created at open.
.RE
.LP
.IP "2) Processing in the IM Server"
.RS
.IP \(bu
The IM Server frees the resources.
.IP \(bu
The IM Server deletes the registration of the client.
.LP
Except for the end request from the client, the IM Server performs this 
closing processing for the the destroy event for the focused window.
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
.RE
.LP
.bp
.NH 2
Start/end of the Native Language Input Service
.XS
\*(SN Start/end of the Native Language Input Service
.XE
.LP
The following messages are used to start and end the native language 
input service. For a definition of Type1, Type2 and Type3, refer to 
section 3.3.3. 
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:\fBXIMP_REG_KEY_PRESSED\fP
4:ICID\fP
.TE
.LP
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:\fBXIMP_SPROC_STARTED\fP
4:ICID
.TE
.LP
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:\fBXIMP_SPROC_STOPPED\fP
4:ICID
.TE
.LP
.NH 3
Front-End Type1
.XS
\*(SN Front-End Type1
.XE
.LP
The IM Server must register the property "_XIMP__SPROC_STARTED_KEYS, 
_XIMP_SPROC_STOPPED_KEYS" specified in section 3.3.7 and 3.3.8.  This 
will be used to trigger the start of the native language input service.
.LP
When the client receives the key event corresponding to the registered 
keys, the client needs to send the 
.PN XIMP_REG_KEY_PRESSED
message to the IM Server.  The IM Server will then notify the client of 
the start of service by sending the 
.PN XIMP_SPROC_STARTED
message and by turning on the key press event mask of the client window.  
After sending the 
.PN XIMP_SPROC_STARTED
message, the IM Server is responsible for the flow of key events until 
the IM Server sends the 
.PN XIMP_SPROC_STOPPED
message to notify the client of the end of native language input service.
.LP
The client can turn off the key press event mask at three different times. 
1) before sending 
.PN XIMP_REG_KEY_PRESSED
message 2) after sending 
.PN XIMP_REG_KEY_PRESSED
message 3) after receiving 
.PN XIMP_SPROC_STARTED
message.  The choice is implementation dependent.
.LP
.NH 3
Front-End Type2
.XS
\*(SN Front-End Type2
.XE
.LP
The IM Server must turn on the key press event mask after receiving the 
.PN XIMP_CREATE
message from the client.
.LP
The client must turn off the key press event mask after receiving the 
.PN XIMP_CREATE_RETURN
message from the IM Server.
.LP
When implementing a Front-End Type2, the three XIMP protocol messages 
described above are not needed.
.LP
.NH 3
Front-End Type3
.XS
\*(SN Front-End Type3
.XE
.LP
The IM Server must turn on the key press event mask after receiving the 
.PN XIMP_CREATE 
message from the client. The library does not turn off the key press 
event mask. So the key press event to the client will be passed to both 
of the IM Server and the client. Both of them must watch the key press 
event, either it is a registered key or not. The library must cut off key 
press event by the filter mechanism when it will be processed by the IM 
Server. The IM Server must not process the key press event when it should 
be processed in the client.
.LP
.NH 3
Back-End Type1
.XS
\*(SN Back-End Type1
.XE
.LP
The IM Server must register the property "_XIMP_SPROC_STARTED_KEYS, 
_XIMP_SPROC_STOPPED_KEYS" specified in section 3.3.7 and 3.3.8.  This will 
be used to trigger the start of the native language input service.
.LP
When the client receives the key event corresponding to the registered 
keys, the client needs to start forwarding all key press events to the 
IM Server by using the 
.PN XIMP_KEYPRESS
messages.  The IM Server will then start processing the forwarded key 
press events.
.LP
To stop forwarding key press events to the IM Server, the IM Server needs 
to send the 
.PN XIMP_SPROC_STOPPED
message to the client to notify the end of the native language input 
service.
.LP
.NH 3
Back-End Type2
.XS
\*(SN Back-End Type2
.XE
.LP
The client must forward all key press events to the IM Server after 
receiving the 
.PN XIMP_CREATE_RETURN
message from the IM Server.
.LP
When implementing a Back-End Type2, the three XIMP protocol messages 
described above are not needed.
.LP
.bp
.NH 2
Focus of the IMS Windows
.XS
\*(SN Focus of the IMS Windows
.XE
.LP
The client sends the request of setting the focus with 
.PN XIMP_SETFOCUS. 
This means the corresponding IC's focus window receives the focus.  
The IM Server makes this IC's focus window the input service window.  
The complete feedback specification is a matter of input Server policy.
.LP
.B
Processing Contents
.IP "1) Processing in the Client"
.RS
.IP \(bu
The client sends the focus request with ClientMessage.
.RE
.LP
.IP "2) Processing in the IM server"
.RS
.IP \(bu
The IM Server makes this IC's focus window be the input service window.
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
0:\fBXIMP_SETFOCUS\fP
4:ICID
.TE
.LP
.bp
.NH 2
Unfocus of the IMS Windows
.XS
\*(SN Unfocus of the IMS Windows
.XE
.LP
The client sends the request of unsetting the focus
with 
.PN XIMP_UNSETFOCUS.
This means the corresponding IC's focus window loses the focus. On the IM 
Server side, this IC's focus window is no longer the input service window.
The complete feedback specification is a matter of input server policy.
.LP
.B
Processing Contents
.IP "1) Processing in the Client"
.RS
.IP \(bu
The client sends the unfocus request with the ClientMessage.
.RE
.LP
.IP "2) Processing in the IM Server"
.RS
.IP \(bu
On the IM Server side, this IC's focus window is no longer the input 
service window.
.LP
.RE
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
0:\fBXIMP_UNSETFOCUS\fP
4:ICID
.TE
.RE
.LP
.bp
.NH 2
Getting the Converted String
.XS
\*(SN Getting the Converted String
.XE
.LP
IM Server sends the converted string to the client with compound text.
.LP
.B
Processing Contents
.IP "1) Processing in the Client"
.RS
.IP \(bu
The client receives the string, and converts that compound text format 
to the adopted format depending on the locale specified in XOpenIM 
(which is in XIM specification).
.RE
.LP
.IP "2) Processing in the IM Server"
.RS
.IP \(bu
IM Server sends the resulted string by the ClientMessage or the property. 
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
The IM Server sends the following ClientMessage.
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, box, tab(:);
n | lw(8c).
.vs 15
0:ICID (31-24bit)
_
1:ICID (23-16bit)
_
2:ICID (15-8bit)
_
3:ICID (7-0bit)
_
4:Size
_
5:\&
\&
:Compound Text
\&
19\&
_
.TE
.LP
This ClientMessage's format is 8. The ICID must be processed in network 
byte order.
.LP
If a compound text to be sent is longer than one ClientMessage, the 
compound text may be divided into pieces and sent in several 
ClientMessage's. In that case, the following format is used. 
.TS
center, allbox, tab(:);
c c lw(6c).
.vs 15
ICID:Size:Compound Text
.TE
.LP
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
"Size" indicates the number of bytes of compound text which remain to 
be sent.  This includes the number of bytes of compound text in the 
current ClientMessage. 
.RE
.LP
.bp
.NH 3
Getting the Converted String by the Property
.XS
\*(SN Getting the Converted String by the Property
.XE
.LP
.IP 1)
IM Server makes a default property that is used in this communication.
.TS
tab(|);
l c l.
Property Name|:|depends on the implementation of IM Server
Property Type|:|COMPOUND_TEXT
Format|:|8
data|:|resulted string
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
.IP 2)
IM Server stores the resulting compound text in this property.
IM Server provides it to the client window using the XChangeProperty
function with the mode PropModeAppend.
.LP
.IP 3)
After that, IM Server sends the ClientMessage to the client.
.LP
.RS
Data Format
.RE
.ce
Data Field in the XClientMessage Structure
.TS
center, box, tab(:);
| n | lw(5c) r.
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
ClientMessage and gets the result using the XGetWindowProperty function 
with delete set to True. This function is performed by a filter.
.LP
.IP "Supplement"
The name of this property is not specified in this protocol, therefore 
the IM Server can prepare each property for each client, or each property 
for each ICID.  The IM Server can also prepare only one property.
.LP
.bp
.NH 2
Controlling the IM Server
.XS
\*(SN Controlling the IM Server
.XE
.LP
The client sends the following ClientMessages for the controlling the 
IM Server.
.DS
\fBXIMP_CLIENTWINDOW\fP	: Changing the Client Window
\fBXIMP_FOCUSWINDOW\fP	: Changing the Focus Window
\fBXIMP_MOVE\fP			: Moving the Preedit Window
\fBXIMP_SETVALUE\fP		: Changing the attributes of the input service
.DE
.LP
.NH 3
Changing the Client Window
.XS
\*(SN Changing the Client Window
.XE
.LP
If the application don't specify the Client Window with XCreateIC,
the library specifies the window for communication as Client Window.
After the application specifies the Client Window, the Client Window
sends the ClientMessage for once.
.LP
.NH 4
Request for changing the Client Window
.XS
\*(SN Request for changing the Client Window
.XE
.LP
The client sends the following ClientMessage to the IMS Window.
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:\fBXIMP_CLIENTWINDOW\fP
4:ICID
8:Client Window ID
12:FocusWindow Select-event-mask (*)
.TE
.LP
.RS
.IP "(*)"
FocusWindow Select-event-mask is the value of SelectInputMask for the 
Focus Window. With Front-End method, it is used as a hint for 
XSelectInputMask of events.
.RE
.LP
.NH 4
Receiving the request for changing the Client Window
.XS
\*(SN Receiving the request for changing the Client Window
.XE
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:\fBXIMP_CLIENTWINDOW_RETURN\fP
4:ICID
.TE
.LP
.NH 3
Changing the Focus Window
.XS
\*(SN Changing the Focus Window
.XE
.LP
The Client sends the following ClientMessages for changing the Focus Window.
.LP
.NH 4
Request for changing the Focus Window
.XS
\*(SN Request for changing the Focus Window
.XE
.LP
The client sends the following ClientMessage to the IMS Window.
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:\fBXIMP_FOCUSWINDOW\fP
4:ICID
8:Focus Window ID
12:FocusWindow Select-event-mask (*)
.TE
.LP
.RS
.IP "(*)"
FocusWindow Select-event-mask is the value of SelectInputMask for the 
Focus Window. With Front-End method, it is used as a hint for 
XSelectInputMask of events.
.RE
.bp
.LP
.NH 4
Receiving the request for changing the Focus Window
.XS
\*(SN Receiving the request for changing the Focus Window
.XE
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:\fBXIMP_FOCUSWINDOW_RETURN\fP
4:ICID
.TE
.LP
.NH 3
Moving the Preedit Window
.XS
\*(SN Moving the Preedit Window
.XE
.LP
The client sends the request for moving the location of the preedit window 
with 
.PN XIMP_MOVE.
This is effective only if the client specified XIMPreeditPosition.
.LP
This provides the same result as the case of that 
.PN XIMP_SETVALUE
only moves the preedit window, but because the communication protocol is 
simpler, the performance is better.
.LP
.B
Processing Contents
.IP "1) Processing in the Client"
.RS
.IP \(bu
 The client sends the move request by ClientMessage.
.RE
.LP
.IP "2) Processing in the IM Server"
.RS
.IP \(bu
The IM Server moves the preedit window.
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
.RE
.LP
.NH 3
Changing the attributes of the input service
.XS
\*(SN The End of the Input Service Request
.XE
.LP
The client sends the 
.PN XIMP_SETVALUE
request for changing the attributes of the input service window which was 
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
The client sets the attributes of input context to the appropriate property. 
The input context is specified by ICID.
.IP \(bu
The client sends the 
.PN XIMP_SETVALUE
request to the IM Server using ClientMessage.
.RE
.LP
.IP "2) Processing in the IM Server"
.RS
.IP \(bu
The IM Server gets the attributes from the property and changes the 
attributes of the corresponding input service window.
.RE
.bp
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
0:\fBXIMP_SETVALUE\fP
4:ICID
8:mask (*)
.TE	    
.LP
.RS
.IP "(*)"
This mask is same as 
.PN XIMP_CREATE.
.PN XIMP_FOCUS_WIN_MASK
and 
.PN XIMP_SERVERTYPE_MASK
is not validated with 
.PN XIMP_SETVALUE
request.
.RE
.RE
.LP
.bp
.NH 2
Querying Attributes of IM Server
.XS
\*(SN Querying Attributes of IM Server
.XE
.LP
The client can query the IC information from the IM Server using the 
.PN XIMP_GETVALUE
function.  The client can also query the default attributes which are not 
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
.IP "2) Processing in the IM Server"
.RS
.IP \(bu
The IM Server sets the corresponding attributes to the specified mask
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
The 
.PN XIMP_GETVALUE 
request uses the following ClientMessage.  If there are any errors, the 
client receives the error notification described in 
.PN XIMP_ERROR
protocol (section 4.12).
.LP
.B
Data Format of This ClientMessage
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:\fBXIMP_GETVALUE\fP
4:ICID
8:mask (*)
.TE
.LP
.RS
.IP (*)
This mask is same as 
.PN XIMP_CREATE.
.RE
.LP
.NH 3
Querying Attributes Reply
.XS
\*(SN The End of the Input Service Request
.XE
.LP
The IM Server returns the following ClientMessage after setting the 
corresponding attributes.
.LP
.B
Data Format of This ClientMessage
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:\fBXIMP_GETVALUE_RETURN\fP
4:ICID
.TE
.LP
The client gets the information using XGetWindowProperty with the delete 
argument set as True.
.LP
IM Server must use the same properties used in 
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
The client sends the 
.PN XIMP_RESET
request in the X*ResetIC functions.  This performs the processing 
described in the XIM specification.
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
The IM Server returns a reply corresponding with the 
.PN XIMP_RESET
request. 
If there are any errors, the clients receive the error notification 
described in the 
.PN XIMP_ERROR 
protocol (in section 4.12).
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
0:\fBXIMP_RESET_RETURN\fP
4:ICID
8:Specify a way of sending reset data (*1)
12:Atom ID of reset data (*2)
.TE
.LP
.RS
.IP (*1)
This value specifies a way of sending the reset data as follow.
.TS
tab(|);
n c lw(9c).
0|:|no reset data
1|:|T{
reset data is sent by the ClientMessage with the format described in 
section 4.10.1
T}
2|:|T{
reset data is sent by the specified property (*2) with the format 
described in section 4.10.2
T}
.TE
.RE
.LP
.bp
.NH 2
Event Mask Notification
.XS
\*(SN Event Mask Notification
.XE
.NH 3
Sending Event Mask Notification
.XS
\*(SN Sending Event Mask Notification
.XE
.LP
With Front-End, the IM library notifies the IM Server of the event mask
for the client. The IM Server uses it as a hint to decide if the IM 
Server handles the keyrelease event or not. The format of an event mask 
notification message is described as follows:
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:\fBXIMP_EVENTMASK_NOTIFY\fP
8:ICID
12:FocusWindow Select-event-mask
.TE
.LP
.NH 3
Receiving Event Mask Notification
.XS
\*(SN Receiving Event Mask Notification
.XE
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:\fBXIMP_EVENTMASK_NOTIFY_RETURN\fP
4:ICID
.TE
.LP
.bp
.NH 2
Error Notification
.XS
\*(SN Error Notification
.XE
.NH 3
Error Notification from IM Server
.XS
\*(SN Error Notification from IM Server
.XE
.LP
If an error occurs during IM Server processing, the IM Server notifies 
the client of an error using the ClientMessage. The format of an error 
notification message is described as follows:
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
The error number which describes the reason of the error in detail. 
The following is a list of Detailed Error Number:|
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
The vendor may add its own detail error information if necessary.
However, the client will ignore it if the client does not know of the
vendor defined error.
.RE
.LP
.NH 3
Abnormal Termination of the IM Server
.XS
\*(SN Abnormal Termination of the IM Server
.XE
.LP
When IM Server abnormally terminates, the behavior of client is
implementation dependent.
.LP
.bp
.NH 1
Callback Protocol
.XS
\*(SN Callback Protocol
.XE
.LP
The client sets XIMGeometryCallback, XIMPreeditCallback, XIMStatusCallback, 
XIMPreeditArea or XIMStatusArea.  The IM Server sends the corresponding 
protocol messages to the client. 
.LP
This is performed by the sending the ClientMessage from the IM Server
to the client. The arguments of the callback functions are set in 
the data field of the ClientMessage.  However, if it is impossible
to use the ClientMesaage, it is set in the each described property.
The formats of the callbacks are the shown below. 
.LP
The current XIM specification's preedit callback implementation/usage 
is ambiguous. The preedit callbacks must be implemented in such a way 
that the multiple sequences of 
.PN XIMP_PREEDITSTART, 
.PN XIMP_PREEDITDRAW, 
and 
.PN XIMP_PREEDITDONE 
are used during one session of native input service (conversion on/off).
.LP
For example, when composing a preedit string, 
.PN XIMP_PREEDITSTART 
should be used before multiple calls of 
.PN XIMP_PREEDITDRAW.  
When preedit string is committed, 
.PN XIMP_PREEDITDONE 
should be used. This sequence should repeat until the end of native input 
service.
.IP "1) ClientMessage requests from IM Server to the client"
.TS H
tab(:);
lw(7c) lw(5c) l.
_
.sp 6p
.B
Requests:Contents:Sect. #
.sp 6p
_
.sp 6p
.TH
.R
\fBXIMP_GEOMETRY\fP:GeometryCallback:5.1
\fBXIMP_PREEDITSTART\fP:PreeditStartCallback:5.2
\fBXIMP_PREEDITDONE\fP:PreeditDoneCallback:5.3
\fBXIMP_PREEDITDRAW\fP:PreeditDrawCallback:5.4
\fBXIMP_PREEDITDRAW_CM\fP:PreeditDrawCallback:5.4
\fBXIMP_PREEDITCARET\fP:PreeditCaretCallback:5.5
\fBXIMP_STATUSSTART\fP:StatusStartCallback:5.6
\fBXIMP_STATUSDONE\fP:StatusDoneCallback:5.7
\fBXIMP_STATUSDRAW\fP:StatusDrawCallback:5.8
\fBXIMP_STATUSDRAW_CM\fP:StatusDrawCallback:5.8
.sp 6p
_
.TE
.LP
.IP "2) Reply from the client to the IM Server"
.TS H
tab(:);
lw(7c) lw(5c) l.
_
.sp 6p
.B
Reply:Contents:Sect. #
.sp 6p
_
.sp 6p
.TH
.R
\fBXIMP_PREEDITSTART_RETURN\fP:PreeditStartCallback:5.2
\fBXIMP_PREEDITCARET_RETURN\fP:PreeditCaretCallback:5.5
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
This Callback function is used when XIMStyle includes XIMPreeditArea
or XIMStatusArea. 
.PN XIMP_GEOMETRY 
is issued when the IM Server judges Geometry negotiation should be 
performed. The client calls GeometryCallback function when it receives 
.PN XIMP_GEOMETRY.
.LP
.ce
Data Field in the XClientMessage Structrure
.TS
center, allbox, tab(:);
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
\*(SN Geometry Callback
.XE
.LP
This Callback function is used when XIMStyle includes XIMPreeditCallback.
.PN XIMP_PREEDITSTART
is issued when the IM Server start to pre-edit.
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:\fBXIMP_PREEDITSTART\fP
4:ICID
.TE
.LP
And the return value of the callback function is sent to the IM Server 
by the following ClientMessage. 
.LP
The client calls PreeditStartCallback function when it receives 
.PN XIMP_PREEDITSTART, 
and reply to Server by sending 
.PN XIMP_PREEDITSTART_RETURN. 
Return value of the function is set to "Return Value" field of the 
following ClientMessage.
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, allbox, tab(:);
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
This Callback function is used when XIMStyle includes XIMPreeditCallback.
.PN XIMP_PREEDITDONE
is issued when the IM Server stop to pre-edit.  The client calls 
PreeditStartCallback function when it receives 
.PN XIMP_PREEDITDONE.
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, allbox, tab(:);
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
Preedit Draw Callback can be done in two ways, by using ClientMessages 
and by using properties.  These protocols can be used together or 
independently.  
.LP
These Callback functions are used when XIMStyle includes XIMPreeditCallback.
One of them is issued when the IM Server update the preedit string.  
The client calls PreeditDrawCallback function with call_data when it receives 
one of them.  Call_data is built up from protocol data.
.LP
In following data formats,
.RS
caret means the value of call_data->caret.
.br
chg_first means the value of call_data->chg_first.
.br
chg_length means the value of call_data->chg_length.
.RE
And,
.RS
preedit-string means contents of preedit, and decoded to multi_byte or
.br
wide character string, and into call_data->text->string.
.RE
And,
.RS
feedbacks means call_data->text->feedback.
.br
feedback-value means call_data->text->feedback[*], in this case,
.br
all feedback values pointed by call_data->text->feedback are same.
.RE
.LP
.IP "1) Protocol format for using Property"
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, allbox, tab(:);
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
Each Atom 1, 2 and 3 specifies the following properties.
.IP "Atom 1"
.TS
tab(|);
l c l.
Property Name|:|_XIMP_CALLBACKS_# 
||(# is any number.  This property name is recommended.)
Window|:|IMS Window
Property Type|:|_XIMP_PREEDIT_DRAW_DATA
Format|:|32
data|:|Values corresponding the XIM specification with the following format.
nelements|:|3
.TE
.LP
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:caret
4:chg_first
8:chg_length
.TE
.LP
.bp
.IP "Atom 2"
.TS
tab(|);
l c l.
Property Name|:|_XIMP_CALLBACKS_#
||(# is any number. This property name is recommended)
Window|:|IMS Window
Property Type|:|COMPOUND_TEXT
Format|:|8
data|:|preedit-string
nelements|:|elements of data
.TE
.LP
.TS
center, allbox, tab(:);
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
||(# is any number. This property name is recommended)
Window|:|IMS Window
Property Type|:|_XIMP_FEEDBACKS
Format|:|32
data|:|feedbacks
nelements|:|elements of data
.TE
.LP
.TS
center, allbox, tab(:);
lw(9c).
.vs 15
feedback
.TE
.LP
.IP "Note:"
If the property specified by Atom 2 does not exist, the client should set 
the string field of XIMTest within XIMPreeditDrawCallbackStruct to NULL.  
If the property specified by Atom 3 does not exist, the client should set 
the feedback field of XIMTest within XIMPreeditDrawCallbackStruct to NULL.  
If the properties specified by Atom 2 and Atom 3 do not exist, the client 
should set the text field of XIMTest within XIMPreeditDrawCallbackStruct 
to NULL.  
.RE
.LP
.IP "2) Protocol format for using ClientMessages"
This method requires two ClientMessages.  The IM Server must send these 
messages consecutively.
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
16:feedback-value
.TE
.LP
.RS
.IP "Note 1."
format == 32, and chg_first, chg_length are into 12-15 byte field, 
they are using 16 bits and they are Network Order.
.bp
.IP "Note 2."
The PreeditDrawCBStatus is a BIT array as follows.
.RS
.IP "0x0001  no_string:"
If this bit is set, the client should set the string field of XIMText 
within XIMPreeditDrawCallbackStruct to NULL.
.IP "0x0002  no_feedback:"
If this bit is set, the client should set the feedback field of XIMText 
within XIMPreeditDrawCallbackStruct to NULL.
.IP "0x0004  feedback_via_property:" 
If this bit is set, the feedback field of this client message contains 
an ATOM number.  The Atom that it is pointing to has the same format as 
Atom 3 described in item 1 of section 5.4.
.sp 6p
If this bit is not set, the feedback field has one XIMFeedback value for 
all of the characters in the string field. If the string has more than two 
XIMFeedback values, it must be divided. Therefore, one protocol includes
one XIMFeedback value.
.RE
.IP "Note 3."
The no_feedback bit takes precedence over the feedbacks_via_property bit.
If the no_string and no_feedback bits are set, the text field of 
XIMPreeditDrawCallbackStruct will be NULL.
.RE
.LP
Format for Second ClientMessage
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, tab(:);
| l | n | n | n | n | n | lw(4c) | n |
| c | l | l s s s s | l |
l | c | c s s s s s |.
.vs 15
0:4:5:6:7:8::19
_
ICID::preedit-string:
_
:Size:CompundText
.TE
.LP
.RS
format == 8, ICID is set in the 0-3 byte field using Network Order.
.RE
.RE
.LP
.bp
.NH 2
Preedit Caret Callback
.XS
\*(SN Preedit Caret Callback
.XE
.LP
This Callback function is used when XIMStyle includes XIMPreeditCallback.
.PN XIMP_PREEDITCARET
is issued when the IM Server wish to change the caret position. The client 
calls PreeditCaretCallback function with call_data when it receives 
.PN XIMP_PREEDITCARET. 
Call_data is built up from protocol data.  In following data formats,
.RS
position means the value of call_data->position.
.br
direction means the value of call_data->direction.
.br
style means the value of call_data->style.
.RE
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, allbox, tab(:);
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
|XIMPrimary|1
|XIMSecondary|2
.TE
.RE
.LP
In some cases, PreeditCaretCallback calculates the new position for caret.
So, client replies the following ClientMessage.
.LP
.bp
.NH 2
Status Start Callback
.XS
\*(SN Status Start Callback
.XE
.LP
This Callback function is used when XIMStyle includes XIMStatusCallback.
.PN XIMP_STATUSSTART 
is issued when the IM Server start to draw the status.  The client calls 
PreeditStartCallback function when it receives
.PN XIMP_STATUSSTART.
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, allbox, tab(:);
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
This Callback function is used when XIMStyle includes XIMStatusCallback.
.PN XIMP_STATUSDONE 
is issued when the IM Server stop to draw the status.  The client calls 
PreeditStartCallback function when it receives
.PN XIMP_STATUSDONE.
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, allbox, tab(:);
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
These Callback functions are used when XIMStyle includes XIMStatusCallback.
One of them is issued when the IM Server update the status.  The client calls 
StatusDrawCallback function with call_data when it receives one of them.
Call_data is built up from protocol data.
In following data formats,
.RS
type determines the value of call_data->type.
.br
bitmap means the value of call_data->data.bitmap.
.RE
And,
.RS
status-string means contents of preedit, and decoded to multi_byte or
.br
wide character string, and into call_data->data.text->string.
.RE
And,
.RS
feedbacks means call_data->data.text->feedback.
.br
feedback-value means call_data->data.text->feedback[*], in this case,
.br
all feedback values pointed by call_data->data.text->feedback are same.
.RE
.LP
Status Draw Callback can be done in two ways, by using ClientMessages and
by using properties.  These protocols can be used together or independently.
.IP "1) Protocol Format for Using a Properties"
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:\fBXIMP_STATUSDRAW\fP
4:ICID
8:type
12:bitmap or Atom 1
16:NULL or Atom 2
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
l n lw(10c).
type == 0|:|T{
The Atom id specified the property including the attributes of each 
characters.
T}
type == 1|:|Not used.
.TE
.LP
.bp
.IP "Atom 1"
.TS
tab(|);
l c l.
Property Name|:| _XIMP_CALLBACKS_#
||(# is any number, This name is recommendation)
Window|:|IMS Window
Property Type|:|COMPOUND_TEXT
Format|:|8
data|:|status-string
nelements|:|elements of data
.TE
.LP
.TS
center, allbox, tab(:);
lw(9c).
.vs 15
compound Text
.TE
.LP
.IP "Atom 2"
.TS
tab(|);
l c l.
Property Name|:|_XIMP_CALLBACKS_# 
||(# is any number, This name is recommendation)
Window|:|IMS Window
Property Type|:|_XIMP_FEEDBACKS
Format|:|32
data|:|feedbacks
nelements|:|elements of data
.TE
.LP
.TS
center, allbox, tab(:);
lw(9c).
.vs 15
feedback
.TE
.RE
.LP
.IP "2) Protocol format for using ClientMessages."
This method requires two ClientMessages.  The IM Server should insure 
that these two ClientMessages are consecutive.
.LP
.RS
Format for first ClientMessage.
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, allbox, tab(:);
n lw(8c).
.vs 15
0:\fBXIMP_STATUSDRAW_CM\fP
4:ICID
8:type
12:bitmap or feedback-value
.TE
.LP
.IP "Note 1."
format == 32
.IP "Note 2."
The feedback field has one XIMFeedback value for all of the characters 
which are contained in the following ClientMessage.  This means the 
XIMFeedback value of all characters are same.  If the string has more 
than two XIMFeedback values, it must be divided. Therefore, one protocol 
includes one XIMFeedback value.
.ce
Data Field in the XClientMessage Structure
.TS
center, tab(:);
| l | n | n | n | n | n | lw(4c) | n |
| c | l | l s s s s | l |
l | c | c s s s s s |.
.vs 15
0:4:5:6:7:8::19
_
ICID::status-string:
_
:Size:CompundText
.TE
.LP
format == 8, ICID was setting in the 0-3 byte field using Network Order.
.RE
.LP
.bp
.NH 1
Vendor Extensions to the Protocol
.XS
\*(SN Vendor Extensions to the Protocol
.XE
.LP
Vendor extensions are IM Server functions that are not included in the 
common interface because of application or native language dependencies.  
Therefore, it depends on the implementation of the IM Server.  The client 
must perform the minimum processing even if the IM Server doesn't have 
a extension.
.LP
Specific usage is shown in the following examples.
.IP \(bu 
Setting the status window.
.IP \(bu 
Selecting the Front-End method or Back-End method.
.IP \(bu 
Selecting the mode of listing candidates.
.IP \(bu 
Switching the language on IC.
.IP \(bu 
Setting the input mode depending on the native language.
.br
(e.g. latin-KANA conversion mode, alphanumeric mode).
.LP
.NH 2
Format
.XS
\*(SN Format 
.XE
.LP
The Atom name of the vendor dependent function is defined with the 
following rules.
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
The IM Server registers the vendor extensions to the IMS Window with the 
following format.
.DS
Property Name	_XIMP_EXTENSIONS
Type		_XIMP_EXTENSIONS
Format		32
data		List of the Atom of the vendor extensions
.DE
.LP
If the above property is declared, ClientMessage's can be used in order 
to invoke the vendor specific functions.  The format of the client messages 
is shown below.  IM Server responds with the same ClientMessage format. 
.LP
.ce
Data Field in the XClientMessage Structure
.TS
center, allbox, tab(:);
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
.bp
.XS
Appendix.A. Protocol Number 
.XE
.ce 10
.sp 5
\s+2\fBAppendix A  Protocol Number\fP\s-2
.ce 0
.sp
.LP
.TS
center, tab(:);
lw(10c) l.
_
.sp 6p
\fBXIMP_KEYRELEASE\fP:100
\fBXIMP_KEYPRESS\fP:101
\fBXIMP_CREATE\fP:102
\fBXIMP_DESTROY\fP:103
\fBXIMP_REG_KEY_PRESSED\fP:104
\fBXIMP_SETFOCUS\fP:105
\fBXIMP_UNSETFOCUS\fP:106
\fBXIMP_CLIENTWINDOW\fP:107
\fBXIMP_FOCUSWINDOW\fP:108
\fBXIMP_MOVE\fP:109
\fBXIMP_RESET\fP:110
\fBXIMP_SETVALUE\fP:111
\fBXIMP_GETVALUE\fP:112

\fBXIMP_PREEDITSTART_RETURN\fP:120
\fBXIMP_PREEDITCARET_RETURN\fP:121

\fBXIMP_SPROC_STARTED\fP:130
\fBXIMP_SPROC_STOPPED\fP:131
\fBXIMP_READPROP\fP:132
\fBXIMP_CLIENTWINDOW_RETURN\fP:133
\fBXIMP_FOCUSWINDOW_RETURN\fP:134
\fBXIMP_GETVALUE_RETURN\fP:135
\fBXIMP_RESET_RETURN\fP:136
\fBXIMP_CREATE_RETURN\fP:137
\fBXIMP_KEYPRESS_RETURN\fP:138
\fBXIMP_KEYRELEASE_RETURN\fP:139

\fBXIMP_GEOMETRY\fP:140

\fBXIMP_PREEDITSTART\fP:150
\fBXIMP_PREEDITDONE\fP:151
\fBXIMP_PREEDITDRAW\fP:152
\fBXIMP_PREEDITDRAW_CM\fP:153
\fBXIMP_PREEDITCARET\fP:156

\fBXIMP_STATUSSTART\fP:160
\fBXIMP_STATUSDONE\fP:161
\fBXIMP_STATUSDRAW\fP:162
\fBXIMP_STATUSDRAW_CM\fP:163

\fBXIMP_EVENTMASK_NOTIFY\fP:200
\fBXIMP_EVENTMASK_NOTIFY_RETURN\fP:201

\fBXIMP_EXTENSION\fP:500

\fBXIMP_ERROR\fP:999
.sp 6p
_
.TE
.\" print Table of Contents
.YZ 3
