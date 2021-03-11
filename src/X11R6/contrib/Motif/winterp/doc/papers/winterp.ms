.RP no
.\"-*-nroff-*-
.\"##############################################################################
.\"
.\" File:         winterp.ms
.\" RCS:          $Header: /users/npm/src/winterp/doc/papers/RCS/winterp.ms,v 2.4 1994/06/06 15:24:35 npm Exp $
.\" Description:  WINTERP paper...
.\"		  Translated from LaTeX to braindamaged 'troff -ms' format
.\"               via "typesetting by example" based on the formatting of
.\"               /nfs/X11r4/mit/doc/config/usenixws/paper.ms and
.\"		  /nfs/X11r4/mit/doc/tutorials/HelloWorld/xhw.ms
.\" Author:       Niels Mayer, HPLabs
.\" Created:
.\" Modified:
.\" Language:     rtroff -ms
.\"		  nroff -ms -Tlp winterp.ms | lp -oduplex
.\" Package:      N/A
.\" Status:       X11r5 contrib tape

.\" WINTERP Copyright 1989, 1990, 1991 Hewlett-Packard Company (by Niels Mayer).
.\" XLISP version 2.1, Copyright (c) 1989, by David Betz.
.\"
.\" Permission to use, copy, modify, distribute, and sell this software and
.\" documentation for any purpose is hereby granted without fee, provided that
.\" the above copyright notice appear in all copies and that both that copyright
.\" notice and this permission notice appear in supporting documentation, and that
.\" the name of Hewlett-Packard, Niels Mayer, and David Betz not be used in
.\" advertising or publicity pertaining to distribution of the software and
.\" documentation without specific, written prior permission. Hewlett-Packard,
.\" Niels Mayer, and David Betz make no representations about the suitability of
.\" this software and documentation for any purpose. It is provided "as is"
.\" without express or implied warranty.
.\"
.\" HEWLETT-PACKARD AND DAVID BETZ DISCLAIM ALL WARRANTIES WITH REGARD TO THIS
.\" SOFTWARE AND DOCUMENTATION, INCLUDING ALL IMPLIED WARRANTIES OF
.\" MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL HEWLETT-PACKARD, NIELS
.\" MAYER, NOR DAVID BETZ BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL
.\" DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
.\" PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS
.\" ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF
.\" THIS SOFTWARE AND DOCUMENTATION.
.\"
.\"##############################################################################
.\" macros ripped off from Rosenthal and Lemke's paper
.\.EQ
.\delim $$
.\.EN
.ds CH
.de Ip
.IP \(bu 3
..
.de Qp
.nr PS -2
.nr VS -2
.QP
..
.de Qe
.nr PS +2
.nr VS +2
..
.de RQ
.br
.di
.nr NF 0
.if \\n(dn-\\n(.t .nr NF 1
.if \\n(TC .nr NF 1
.if !\\n(NF .if \\n(TB .nr TB 0
.nf
.rs
.nr TC 5
.in 0
.ls 1
.if !\\n(TB \{\
.	ev
.	br
.	ev 2
.	KK
.\}
.ls
.ce 0
.if !\\n(TB .rm KK
.if \\n(TB .da KJ
.if \\n(TB \!.KD \\n(dn
.if \\n(TB .KK
.if \\n(TB .di
.nr TC \\n(TB
.if \\n(KN .fi
.in
.ev
..
.\"	These macros should select a typewriter font if you have one.
.de LS
.KS
.LD
.ft CW
.ta .6i 1.2i 1.8i 2.4i 3i 3.6i 4.2i
..
.de LE
.ft P
.DE
.KE
..
.de Ls
.nr PS -4
.nr VS -6
.LS
..
.de Le
.LE
.nr PS +4
.nr VS +6
.LP
..
.nr PO 1.25i
.OF "' '% '"
.EF "' '% '"
.ND
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.\"TITLE
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.TL
The WINTERP Widget INTERPreter \*- An application prototyping and
extension environment for OSF/Motif.
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.\"AUTHOR
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.AU
Niels P. Mayer (mayer@hplabs.hp.com)
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.\"AUTHOR's INSTITUTION
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.AI
Hewlett-Packard Laboratories
Human-Computer Interaction Department
1501 Page Mill Road
Palo Alto, CA. 94304-1126
USA
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.\"ABSTRACT
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.AB
.LP
\fIWinterp\fP is an interactive, language-based user-interface and
application-construction environment enabling rapid prototyping of
applications with graphical user interfaces based on the OSF/Motif UI
Toolkit. \fIWinterp\fP also serves as a customization environment for
delivered applications by providing a real programming language as an
extension language. Many existing user-interface languages only have the
expressive power to describe static layout of user interface forms; by
using a high-level language for extensions and prototyping, \fIWinterp\fP
also handles the dynamic aspects of UI presentation, e.g. the use of direct
manipulation, browsers, and dialog.  \fIWinterp\fP makes rapid prototyping
possible because its language is based on an interpreter, thereby enabling
interactive construction of application functionality and giving immediate
feedback on incremental changes.
.LP
\fIWinterp\fP's language is based on David Betz's public domain \fIXlisp\fP
interpreter which features a subset of Common Lisp's functionality. The
language is extensible, permitting new Lisp primitives to be added in the C
language and allowing hybrid implementations constructed from interpreted
Lisp and compiled C. Hybrid implementation gives \fIWinterp\fP-based
applications the successful extension and rapid-prototyping capabilities of
Lisp-based environments, while delivering the multiprocessing performance
of C applications running on personal \fIUnix\fP workstations.
.AE
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.\"section{}
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.NH 1
Introduction
.LP
\fIWinterp\fP\**
.FS
\fIWinterp\fP is pronounced WIN-TERP, not
WINTER-P.
.FE
is a \fBW\fPidget\**
.FS
A Widget is a graphical object that can be manipulated by mouse or keyboard
input: examples of OSF/Motif widgets include scrollbars, pushbuttons,
menus, text editors, etc.
.FE
\fBInterp\fPreter, an application development environment enabling rapid
prototyping of graphical user-interfaces (UI) through the interactive
programmatic manipulation of user interface objects and their attached
actions. The interpreter, based on David Betz's \fIXlisp\fP [Betz89],
provides an interface to the X11 toolkit Intrinsics (Xt), the OSF/Motif\**
.FS
Motif is a trademark of the Open Software Foundation.
.FE
widget set [OSF90] [Young90], primitives for collecting data from
\fIUnix\fP\**
.FS
\fIUnix\fP is a trademark of American Telephone and Telegraph, Bell
Laboratories.
.FE
processes, and facilities for interacting with other \fIUnix\fP processes.
These features enable \fIWinterp\fP to support rapid prototyping of
applications using multi-window graphical user-interfaces by allowing the
user to interactively change both the appearance and functionality.
.LP
In addition to prototyping applications and experimenting with UI layout,
\fIWinterp\fP may be embedded in applications requiring an extension
language for customization or systems integration. Traditional X
applications based on the Xtoolkit allow users to alter X resources to
tailor application UI parameters such as fonts, colors, window sizes, etc.
Motif's User Interface Language (\fIUIL\fP) [OSF90] [Bourne90] extends that
level of customization by allowing the layout of the application's UI
widgets to be tailored. As a language, \fIUIL\fP has the expressive power
to describe the layout of static UI forms, but has none of the control flow
and data handling constructs associated with real programming languages.  A
programming language is needed to support the full range of requirements
needed by User Interface Management Systems (UIMS) [Myers89]; to describe
dynamic, data-driven UI forms, and to model user/application dialog.
\fIWinterp\fP provides such an embedded programming language allowing
tailoring of the UI's static and dynamic layout, UI-to-application dialog,
and application functionality.
.LP
\fIWinterp\fP is thus an interactive \*Qlanguage based\*U user-interface
development environment (UIDE). \fIWinterp\fP is not a UIMS \*- it
provides UI primitives and a high-level language to support a wide variety
of UI-to-application partitionings\**
.FS
Examples of such UI-to-application partitioning that can be implemented in
\fIWinterp\fP include Smalltalk's Model-View-Controller paradigm, state
transition machines, event grammars, etc.
.FE
that are characteristic of the UIMS approach. \fIWinterp\fP is designed to
allow the programmer to \fIevolve\fP a suitable UIMS model that is
appropriate for extending and customizing a particular application.
\fIWinterp\fP is also designed to support direct manipulation UI building.
The current version contains a useful primitive for \*Qdirect manipulation
programming\*U with widget-objects.
.LP
An environment similar to \fIWinterp\fP's already exists in the \fIGNU
Emacs\fP [Stallman87] text editor \*- in fact, \fIWinterp\fP is strongly
influenced by \fIGNU Emacs\fP' successful design. In \fIGNU Emacs\fP, a
mini-Lisp interpreter is used to extend the editor to provide text-browser
style interfaces to a number of \fIUnix\fP applications (e.g.  e-mail user
agents, directory browsers, debuggers, etc). Whereas \fIEmacs-Lisp\fP
enables programmers to create new applications by tying together
C-implemented primitives that operate on first-class types providing
textual interfaces (buffers, windows), \fIWinterp-Lisp\fP ties together
operations on graphical user-interface objects implemented by the Motif
widgets. Both application construction environments achieve the
flexibility, expressiveness, and rapid-prototyping capabilities common for
systems implemented in Lisp, while still attaining the speed of execution
and (relatively) small size associated with C-implemented applications.
.LP
\fIWinterp\fP was initially made public on the MIT X Consortium's X11r4
\*Qcontrib\*U distribution; up-to-date versions are available via anonymous
ftp from a number of Internet sites including export.lcs.mit.edu.
\fIWinterp\fP is quite robust and bug-free, it is in active use by a number
of research projects at HP Labs, and is also being used by companies and
universities worldwide. \fIWinterp\fP was designed to be portable \*- it
runs on \*Qstandards-oriented\*U \fIUnix\fP platforms without porting. A
number of improvements have already been contributed by \fIWinterp\fP's
user group since \fIWinterp\fP's initial public release; submitted
improvements will be included in publicly available updates of
\fIWinterp\fP.
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.\"section{}
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.NH 1
Background
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.\"subsection{}
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.NH 2
Design Constraints
.LP
\fIWinterp\fP was created as the platform on top of which the
\*QCollaborative Interaction Tools\*U project at Hewlett-Packard Laboratories
is developing \fIStrudel\fP [Shepherd90], an extensible
electronic conversation toolkit enabling computer supported cooperative
work. The design of \fIStrudel\fP has resulted in strong constraints on
\fIWinterp\fP: for development, we required an environment supporting rapid
prototyping and exploratory programming; for delivery to end-users,
\fIStrudel\fP had to allow extensive customization in order to fit in to a
particular group's computer-based work environment; finally, to allow us to
validate our groupware design, the platform must be acceptable to \*Qearly
adopters\*U wanting to use \fIStrudel\fP in real work situations.  Unlike
traditional software, groupware cannot be evaluated by individual
beta-testers \*- the power of groupware comes from having a number of
people using it to work together.  In order to gain this wide experimental
user base, we found it necessary to place additional constraints on the
platform: it needs to be freely distributable, easy to install, and it must
have good performance when running alongside other applications on a
standard \fIUnix\fP workstation. In this paper, we discuss the design of
\fIWinterp\fP with such constraints in mind, pointing out the advantages of
\fIWinterp\fP as a general platform for application prototyping and
delivery.
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.\"subsection{}
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.NH 2
Strudel \*- A Customizable Application Based on Winterp
.LP
\fIStrudel\fP is a generic framework for interlinking group e-mail
conversations and group or individual tasks.  Strudel consists of
\fIWinterp\fP's interpreter and user interface primitives, combined with
primitives implementing a distributed hypertext-like system using
replicated e-mail messages as nodes, and high-level operators that will
allow the nodes to be viewed-by and linked-to a variety of browsers (e.g.
conversation presenters, task and to-do lists, calendars).  \fIStrudel\fP's
flexible architecture makes it akin to a \*Q\fIGNU Emacs\fP for groupware\*U
\*- the specialized primitives permit the system to be customized to
support special modes of communication for particular workgroup
environments much in the same way that \fIGNU Emacs\fP Lisp is used to
customize the editor to support special editing modes for particular
programming environments.
.LP
Users will be able to choose from a library of e-mail forms that are
designed to track specific types of conversations \*- scheduling meetings
and resources, software defect tracking, group design deliberation, etc.
Workgroups can extend the library of forms to help capture and manage
recurrent conversations that are not covered by \fIStrudel\fP's standard
forms library. The kinds of interface customizations can range from adding
new menu entries for often-used functions to designing new e-mail forms and
associated browsers for their data.  We are working closely with a few HP
entities to provide \fIWinterp\fP-Lisp \*Qscripts\*U\**
.FS
In analogy to \*Qshell scripts\*U running under \fIUnix\fP shell programs.
.FE
implementing specific scenarios involving conversations arising in the
domain of team software and hardware production: group design deliberation,
software maintenance, defect tracking, etc. See Figure 1 for a snapshot of
the \fIStrudel\fP prototype; for further information on \fIStrudel\fP, see
[Shepherd90].
.LP
Research related to \fIStrudel\fP includes speech act and dialog theory
[Searle76] [Reichman85], commitment-based software frameworks [Fikes82]
[Winograd86], \fIThe Coordinator\fP\**
.FS
\fIThe Coordinator\fP is a registered trademark of Action Technologies,
Inc.
.FE
and the language/action perspective [Fikes82] [Winograd87], e-mail message
filtering [Rose86] [Borenstein88] semi-structured message systems
[Malone86], the Object-Lens [Lai88], conversation management [Comer86]
[Dollimore89] [Sulonen90] [Kaplan90], office procedure and coordination
models [Holt81], collaborative hypertext systems [Trigg86], and Issue-Based
Information Systems [Conk87].
.\"%%%%%%%%%%%%%
.\"begin{figure}
.\"label{strudel_snapshot}
.\"%%%%%%%%%%%%%
.KF
.sp 8.25i
.sp
.QP
Figure 1. The \fIStrudel\fP prototype, showing the e-mail browser and some
active e-mail forms used in conversations arising during team software
production.
.\"%%%%%%%%%%%%%
.\"end{figure}
.\"%%%%%%%%%%%%%
.KE
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.\"section{}
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.NH 1
Interpreting User Interfaces?
.LP
\fIWinterp\fP differs from many other UI languages in that it is
interpretive rather than compiled. We believe that interpretation of UI
specifications offers a number of practical advantages over the compilation
approach taken by \fIUIL\fP, traditional C programming with the Xtoolkit,
or compiled UIMSs.
.LP
Neither traditional C Xtoolkit applications, nor \fIUIL\fP applications
take advantage of the interpretive nature of the Xtoolkit. When programming
in C, one is forced to go through tedious edit/compile/test cycles even for
trivial program changes. The Xtoolkit provides a resource manager (Xrm)
that allows one to shorten the edit/compile/test cycle for simple
application tailoring such as selecting fonts, colors, label names, or
choosing from an enumerated set of application-defined customization
choices. This is implemented by having the application load
(non-interactive interpretation) the appropriate resource settings from the
resource data base at application initialization time.  While this
mechanism eliminates a compilation stage for a number of simple
customizations, the result is still a cycle consisting of repeated edits of
the resource database followed by running the application to test the
results.  \fIUIL\fP expands on the type of customizations possible via Xrm
by reading a compiled, structured description of the widget hierarchy,
along with associated resources and callback names.  Rather than
\*Qinterpreting\*U the data from a resource database, as is done with Xrm,
\fIUIL\fP uses an additional compilation stage. While \fIUIL\fP compilation
is quicker than C compilation, it still makes rapid prototyping impractical
because of the edit/compile/test cycle.
.LP
In contrast to the batch approach provided by \fIUIL\fP or C, \fIWinterp\fP
allows interactive programmatic manipulation of the UI via a
message-passing mechanism that takes full advantage of the interpretive,
object-oriented nature of the Xtoolkit. The Motif widgets are
\*Qinterpretive\*U in that one can give programmatic commands to the Motif
library to create new widgets, and the Xt intrinsics will create the new
widget on-the-fly.  One can also send messages to created widget objects
via the \*Qmethods\*U implemented by widget-class specific functions in Motif
or the Xt Intrinsics (such as \f(CWXtSetValues()\fP) \*- the effects of
these messages are interpreted by the toolkit and result in an eventual
updating of the graphics and actions associated with the widget.
.LP
\fIWinterp\fP provides access to the \*Qinterpretive\*U nature of the Motif
widgets through its built-in \fIXlisp\fP interpreter. The interpretive
approach enables rapid prototyping because one receives immediate feedback
on changes \*- one can incrementally build up a user interface,
piece-by-piece, and one can play \*Qwhat if\*U games by modifying both the
layout and functionality of the application. \fIWinterp\fP even includes a
\*Qdirect manipulation\*U primitive that allows changing widget resources,
callbacks and event-handlers by designating a widget with the mouse. One
need not rerun or recompile the application in order to see the results of
a change to a \fIUIL\fP or X resource \*- with \fIWinterp\fP, incremental
changes to an application can be tested interactively.
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.\"section{}
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.NH 1
The Role of a Programming Language in a UIMS.
.LP
The UIMS approach requires the power of an embedded programming language to
allow abstractions to be derived from complex changes in application state
and data. Such abstractions provide the separation between application
semantics and the user-interface that UIMSs strive for. This separation is
really nothing other than an extension of the notion of encapsulation to
the architecture of applications based on graphical user-interfaces. Such
encapsulation makes it easier to experiment with the \*Qlook and feel\*U of
an application without side-effects, enabling designers to more easily
refine their applications via rapid prototyping. A well architected system
will also provide end-users with a means of customizing the UI without
adverse effects on the application's functionality. By embedding full
programming language capabilities in a user-interface language,
\fIWinterp\fP allows designers to develop the appropriate language-based UI
abstractions for the particular kind of application and extension
architecture.
.LP
In contrast to \fIWinterp\fP, many of the problems in developing and
extending applications based on Motif's User Interface Language (\fIUIL\fP)
stem from the lack of full programming language support \*- \fIUIL\fP only
provides a language supporting a module system, a static widget description
language, and simple expression arithmetic.  In non-trivial applications,
one must write, compile, and link C code which calls hooks into \fIUIL\fP
in order to control dynamic dialog components that make up the UI of a real
application. Such an architecture imposes strong constraints on the amount
of customization possible without modifying the C source code. Ultimately,
the lack of a programming language in \fIUIL\fP limits the usefulness of
separating the UI description from the application's functionality:
.\"-----------------
.\"begin{quotation}
.\"-----------------
.QP
For a complex interface, fetching widgets from UID\**
.FS
A UID file is a User Interface Description file that is produced by
compiling a UIL file.
.FE
files won't satisfy all of a programmer's needs. Thus, XUI comes with a
complete C language library of calls through which the DECwindows\**
.FS
DECwindows is a trademark of Digital Equipment Corporation.
.FE
widgets can be defined and controlled. These calls are useful, but by using
the C language libraries, the correspondence between program structure and
program function given by UIL may be lost. [Bourne90 \*- p. 40]\**
.FS
This quote from [Bourne90 \*- p. 40] comes from an article on programming
with DECwindows, focusing on the use of \fIUIL\fP.  Although the \fIUIL\fP
in DECwindows is not the same as Motif's \fIUIL\fP, both versions provide
essentially the same functionality.
.FE
.\"---------------
.\"end{quotation}
.\"---------------
.LP
With \fIWinterp\fP, programmers can use language constructs to represent
and manipulate the state of the application and the UI.  \fIWinterp\fP
makes an effective prototyping environment because one can use \fIXlisp\fP
to build the user interface, prototype the \*Qdialog\*U aspects of the
working application, and use \fIXlisp\fP's object system to evolve
language-based abstractions. Depending on the kind of UI style and
application architecture, one may use \fIWinterp\fP's features to
encapsulate and separate application functionality from the UI representing
that functionality; alternately one may find it advantageous to build
higher level UI constructs which use class-inheritance to specialize
generic UI objects provided by Motif into application-specific objects.
Because \fIWinterp\fP's language-base is interpretive, it can be used to
describe dynamic, data-driven user-interfaces such as those found in
\fIStrudel\fP.
.LP
Traditional UIMSs are based on an abstract model for separating application
functionality from its associated user interface \*- e.g Smalltalk's Model
View Controller, the Seeheim UIMS model, transition networks, event
grammars, etc. [Myers89]. \fIWinterp\fP is not a UIMS; it provides little
policy for separating the application semantics from the UI.  However,
\fIWinterp\fP's language base enables the design and development of a
variety of UI-to-application modularization policies.
.LP
The lack of a particular UIMS policy in \fIWinterp\fP is in accordance with
the current view that traditional UIMS models are problematic; that it is
difficult to separate UI from application semantics in a general way:
.\"-----------------
.\"begin{quotation}
.\"-----------------
.QP
 ... in recent years, as UIMSs are built to handle more sophisticated user
interfaces with direct manipulation ... there has been concern that the
separation between user interface and application raises more problems than
it solves. Perhaps the prototypical problem arising from this separation is
that of whether to handle the semantics of an interaction in the UIMS or
the application program. For example, feedback is a task typically handled
by the UIMS, but the semantically rich feedback required by direct
manipulation user interfaces (e.g. highlighting while moving the mouse) is
difficult to do without involving the underlying application. If the
application and the UIMS thus need to frequently interact, their separation
becomes a hindrance rather than a help.... [Rosenberg88]
.\"---------------
.\"end{quotation}
.\"---------------
.LP
In developing applications with \fIWinterp\fP we have found that for
certain classes of applications, such as instrumentation controllers, a
simple recursive, event-driven state machine\**
.FS
Lisp's list and symbol manipulation features make it especially easy to
implement such state machines.
.FE
is an appropriate abstraction separating application semantics from the UI.
In dynamic, data-driven UI's (browsers, graph editors, CAD drawing tools),
separating the UI from the application objects has proven to create messy
architectures; a better solution is to use Motif widgets subclassed\**
.FS
Note that \fIWinterp\fP-Lisp allows subclassing of widgets without
resorting to the complexities and tedium of widget subclassing in C with
the X toolkit.
.FE
into application-specific UI objects handling their own state and actions.
We believe that it is best to give programmers the full capabilities of the
Motif toolkit, augmented by \fIWinterp\fP's interpretive, language-based
interface.  Application programmers can use these features to evolve the
appropriate UIMS to do the job.
.LP
\fIWinterp\fP's policy-free UIMS architecture makes \fIWinterp\fP-based
applications far more customizable than applications based on UIL or the
Widget Creation Library (WCL). Both UIL and WCL enforce an architecture
which strongly separates the dynamics of the application from the static
presentation of the panels comprising the application's UI. Such an
architecture will only allow trivial customization of applications
employing state-based dynamics.  A simple example of such a problem: one
wants to add a button to an application that does the same thing as a
menu-entry (because one selects this entry often).  The application
designer had the menu entry \*Qgrey out\*U to indicate that it is not a
suitable choice given a particular application state. With UIL or WCL,
special code must be written at the C-language level to handle such
state-based changes; this code is tied to assumptions about the static
interface described by the user interface language. In the case of the
customization just described, the appropriate state-based inactivation of
the button may not occur \*- this can result in the application entering a
disallowed state if the user selects it without knowing the choice was
invalid.  Even if the error can be trapped in the button's callback, the
user is not being given the kind of state-based feedback that is the
hallmark of good UI design.  The problem here is that application state
cannot be described in the UI language. The lack of programming language
features make it impossible to propagate even the simplest state changes to
state-dependent UI components and this creates an inelegant architecture
for both application designers and application customizers.
.LP
UIL and WCL force application architectures which hard-code application
dialog in C for certain expected UI presentations described in the
user-interface language. The assumption, that dialog and presentation are
separate, limits the kinds of UI customizations that are possible to
trivial layout modifications. Applications use dialog to prompt the user
for information needed to complete a previously executed command.  Dialog
is also used to limit the amount of screen real estate used by an
application \*- selection choices and input fields that are not of primary
concern are hidden until needed. Personal preference, screen real estate
usage and application usage patterns should dictate whether one elects to
select a choice directly, or via dialog. Thus, the customization of
dialog versus direct-presentation may be a useful feature for certain
applications. For example, some electronic mail (e-mail) interfaces support
multiple folders for filing/copying information that is received in one's
\*Qin-box\*U. Some e-mail UI's will use dialog for folder selection when
a \*Qcopy\*U or \*Qmove\*U operation is invoked. People doing a lot of
filing may prefer to have direct control over the selected folder\**,
.FS
An example of a e-mail UI that does not utilize dialog for folder selection
is the MIT X Consortium client \fIxmh\fP. A common complaint about
\fIxmh\fP is that the folder selection area takes up too much real estate.
This is yet another example where dialog versus presentation
customization would solve an existing problem.
.FE
rather than having the system prompt for a folder each time. With UIL or
WCL, customization of the tradeoffs between presentation and dialog is not
possible within the user interface language due to lack of programming
language constructs. With \fIWinterp\fP, such customizations are possible.
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.\"section{}
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.NH 1
Advantages of Lisp as a UIMS Language
.LP
Programming language features are present in UIMSs such as \fIOpen
Dialogue\fP [Schulert88], and \fISerpent\fP [SEI89].  \fIWinterp\fP differs
from such UIMSs because it does not attempt to define a new user interface
language into which programming language constructs need to be introduced.
Rather, \fIWinterp\fP uses a subset of a standard language \*- Common Lisp
\*- and extends this language to describe UI layout and dialog. The choice
of Lisp as the widget layout and prototyping language in \fIWinterp\fP
provides important advantages:
.\"---------------
.\"begin{itemize}
.\"---------------
.RS .5in
.\"-----
.\"\item
.\"-----
.Ip
Highly expressive:
.LP
Lisp allows new functionality to be expressed quickly using less code than
an equivalent C program because it is a high-level language featuring
symbol and list manipulation [Creech87b] [Creech87a], first class
procedures [Creech87b] [Creech87a], object-oriented programming [Betz89],
and automatic memory management via garbage collection [Creech87b]
[Creech87a]. Lisp's features for manipulating lists and trees of arbitrary
objects are used to represent UI layout structures and \fIWinterp\fP
introduces new objects (widget-objects) to represent UI components.
.\"-----
.\"\item
.\"-----
.Ip
Designed to be interactive, interpretive:
.LP
Lisp makes a good environment to drive an interactive interface to the
Motif widgets because the language was designed to be interpreted.
Interpretation can also be accomplished in traditionally compiled languages
(such as C), but in Lisp, small one-off changes can be made with reduced
effort due to dynamic typing. Such changes are typical in rapid
prototyping.
.LP
Lisp's interactive error handling and debugging allows for programming
errors to be caught, debugged, and fixed interactively. Debugging occurs
within the environment that caused the error to arise, and the full power
of the language interpreter is available to allow programmers to inspect,
alter, or fix the environment. After fixing a bug, one may be able to
resume execution of ones code from the point that caused the error without
having to restart the program. These debugging features are essential for
rapid prototyping.
.\"-----
.\"\item
.\"-----
.Ip
Code-Data equivalence:
.LP
Lisp data is represented in the same form as Lisp programs [Creech87b]
[Creech87a], which means that programs can perform computations to
create/alter data structures representing programs.  Such meta-programming
allows the creation of high-level data-driven abstractions for user
interfaces created programmatically via Motif and Xt Intrinsics calls.
.LP
\fIWinterp\fP-Lisp can thus be used to create dynamic widget layouts
through computations that create and mutate data structures representing
user-interfaces. For example, in our \fIStrudel\fP groupware toolkit,
\fIWinterp\fP's interpreter and Motif UI primitives are used as a
description language for creating and processing active/graphical forms.
Such forms can be sent (as textual programs) through standard e-mail
channels.  The receiving \fIStrudel\fP system interprets the message and
displays a form containing a user interface built from arbitrary
combinations of widgets and bitmaps. These \*Qforms\*U can even be whole
user-interfaces and associated programs that can be passed around from
user-to-user as Lisp \*Qcontinuations.\*U\**
.FS
One of our research issues is how to provide security for systems that
\*Qopen\*U to programmatic manipulation from outside \*- they can easily be
infected with viruses.
.FE
See Figure 1 for an example of a \fIStrudel\fP form.
.\"-------------
.\"end{itemize}
.\"-------------
.RE
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.\"section{}
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.NH 1
Implementation Issues in Embedding a Lisp Interpreter
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.\"subsection{}
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.NH 2
Problems with Traditional Lisp Systems
.LP
Despite its advantages, Lisp is traditionally associated with large, slow
and expensive systems\**
.FS
One solution to the \*Qlarge\*U and \*Qslow\*U problems has been to create
special operating systems and hardware for Lisp \*- Lisp machines.  Such
specialized computers are being priced out of the market by general purpose
\fIUnix\fP workstations using the X Window System and running software that
is portable across a number of vendor platforms.
.FE
\*- Lisp's flexibility has its costs.  Attempts at building Lisp-based
applications that are good citizens on \fIUnix\fP workstations have been
problematic because systems such as Common Lisp (CL) create huge,
resource-hungry processes that swap out all other applications and cause
memory thrashing [Creech87b] [Creech87a].  This results in unacceptable
overall system performance if the CL process is but one of many processes
competing for resources on a \fIUnix\fP box.
.LP
While CL continues to be an excellent prototyping platform, few acceptable
solutions to the \*Qdelivery problem\*U have been found for applications
embedded in a CL environment. One unacceptably drastic solution is to
recode the completed CL-based prototype into C \*- an approach often used
to create deliverable versions of expert systems and other complex
applications. This approach is time consuming, error prone, and changes the
feel and the flexibility of the delivered application: applications
prototyped with CL assume and make use of the underlying features of the
Lisp system; these assumptions must be removed from the design or be
recoded in the delivery language.
.LP
In the past, we have also experienced problems in interfacing large Common
Lisp systems to other C-implemented libraries and low-level device drivers
\*- the problem stems from the difficulties in importing, exporting and
translating arbitrary Lisp data structures to/from the C level; problems
also arise from the explicit control requirements of the CL interpreter's
evaluator which make it difficult to interface to event-driven programs
such as the X toolkit.
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.\"subsection{}
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.NH 2
Solution: a Hybrid Implementation
.LP
Fortunately, another class of Lisp application has been successful in a
general purpose computational environment \*- a hybrid architecture of
Lisp and C giving the flexibility of a Lisp system while allowing delivery
of a relatively small and efficient process. Under \fIUnix\fP, Richard
Stallman has created a highly-customizable editor-based programming
environment called \fIGNU Emacs\fP [Stallman87] \*- this is a system that
delivers to the \fIUnix\fP user a text-editor oriented UI that is the
foundation of the Lisp Machine programming environment. Under
\fIMS-DOS\fP\**,
.FS
\fIMS-DOS\fP is a trademark of Microsoft Corporation.
.FE
successful programs like \fIAutoCAD\fP\**
.FS
\fIAutoCAD\fP is a trademark of Autodesk Corporation.
.FE
contain a Lisp customization language embedded in a CAD program.
.LP
The approach taken by such hybrid applications is that a small mini-Lisp
interpreter serves to \*Qglue\*U together efficient C-implemented primitives
that make up an application.  User-customization and prototyping under such
a hybrid system amounts to using the Lisp interpreter to reconfigure
C-implemented building blocks in order to change, modify, or improve the
functionality of the system. Such an application architecture follows the
\*Q80/20 heuristics\*U for program execution \*- low level routines that
take up most of the computational resources are coded in C, and are
therefore fast and efficient in memory use (no garbage collections caused
by low-level code).  The Lisp interpreter is relatively slow in comparison
to a compiled C program, but it only serves to flexibly glue together
components of the \*Qouter loop\*U of a program. For an illustration of this
hybrid architecture, see Figure 2.
.LP
\fIWinterp\fP solves the problems traditionally associated with Lisp
delivery by using this hybrid approach \*- a small, fast, lightweight Lisp
interpreter based on David Betz's \fIXlisp\fP serves as an interactive,
configurable mechanism tying together high-level C-implemented
application-specific primitives.  Because \fIXlisp\fP is implemented
entirely in C, one can simply use \fIXlisp\fP's C library to directly
recode any Lisp code into a C-implemented primitive.
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.\"subsection{}
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.NH 2
Evolving from Prototype to Deliverable with Hybrid Programming
.LP
\fIWinterp\fP supports an evolutionary program lifecycle: \fIWinterp\fP
application writers rapidly prototype new functionality by using the
interpreter to interactively refine the layout, looks, and functionality of
the application.  Once functionality has stabilized, a programmer can
improve the application's efficiency by reimplementing the functionality in
C while maintaining the same programmatic interface to the rest of the
system. The new primitives will then serve as the building blocks for the
next layer of prototyping and customization. The end result, if designed
carefully, is a relatively small and fast application that provides the
right set of building blocks and hooks to permit end-users to customize the
look and feel of the application.
.LP
\fIWinterp\fP is also useful for rapid prototyping applications that do not
need to be delivered with an embedded customization language. Systems with
such delivery goals may still use the aforementioned application lifecycle.
As the application matures and Lisp prototype code stabilizes, the program
can gradually be recoded entirely into C. Eventually, this process will
allow a standard C-implemented Motif program to be delivered.
.LP
Contrast this stepwise refinement from prototype to deliverable with the
approach of throwing out the entire CL-based prototype and starting from
scratch in a language like C or C++. The advantages of creating
deliverables incrementally via hybrid programming are: (1) Existing
regression tests may be applied to the deliverable under construction, such
that the effects of each reimplementation can be tested; (2) Developers and
alpha-testers can use the application being recoded for further development
and further work, thereby checking the impact of the changes on the
usability, customizability, and functionality of the application.
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.\"section{}
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.NH 1
An Architecture for Extensible UIs and Applications
.LP
The architecture of an application built on top of \fIWinterp\fP allows for
applications to be delivered with a variety of UI styles. Such
customizability is important because it is difficult to please everyone
with a single UI style, and because system designers cannot foresee all
possible needs of all users. With \fIWinterp\fP, UI styles can be specified
via \fIWinterp\fP-Lisp \*Qscripts\*U which are loaded into the application at
run-time. Inexperienced users can customize the UI or application's
functionality by using \*Qprogramming by example\*U based on existing
scripts. Application \*Qgurus\*U may come up with new styles, merge features
of existing styles, add shortcuts and accelerators, or come up with new
functionality. Such customizations are often distributed to others within
the organization. Extensible applications like AutoCAD and Hypercard\**
.FS
Hypercard is a trademark of Apple Computer.
.FE
have even created an \*Qaftermarket\*U of scripts that create new
applications within the environment provided by the extensible application.
.LP
Figure 2 shows the architecture of an extensible application in which
\fIWinterp\fP-Lisp serves as a customization script and \*Qglue language\*U
between C-implemented application and user-interface primitives.
.\"%%%%%%%%%%%%%
.\"begin{figure}
.\"label{winterp_cust_architecture}
.\"%%%%%%%%%%%%%
.KF
.sp 2.5i
.sp
.CD
Figure 2. Architecture of an extensible application based on \fIWinterp\fP.
.DE
.\"%%%%%%%%%%%%%
.\"end{figure}
.\"%%%%%%%%%%%%%
.KE
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.\"section{}
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.NH 1
Winterp Features
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.\"subsection{}
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.NH 2
Xlisp
.LP
\fIWinterp\fP uses \fIXlisp\fP [Betz89] because it is reliable, small,
fast, and free.  \fIXlisp\fP has been around since 1985, and has evolved
considerably since it first appeared publicly. Because it has been in
widespread use for some time, most bugs have been shaken out of the system.
Also, the newsgroup comp.lang.lisp.x has been an effective public channel
for exchanging information and patches.  \fIXlisp\fP was designed to be run
on PCs; because it was designed with a limited environment in mind, it has
turned out to be quite fast and memory-efficient while remaining portable
across a variety of architectures ranging from 16 bit PC's to workstations.
\fIXlisp\fP's simple object system has enabled an elegant interface to the
object oriented structure of the Xtoolkit and the Motif widgets.  Finally,
\fIXlisp\fP is free, thanks to the generosity of David Betz.
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.\"subsection{}
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.NH 2
Motif Widgets are First-Class Objects
.LP
\fIWinterp\fP uses \fIXlisp\fP's Smalltalk-like object system [Betz89] as
its interface to the class hierarchy of widgets provided by Motif.
Specifically, each Motif widget class is represented by one or more object
classes in \fIWinterp\fP.  A wide range of UI objects are provided. Simple
widgets include text or pixmap labels, pushbuttons, toggle buttons as well
as scrollbars and scale valuators. More complex widgets include a text
editor, a file browser, and a list browser. Motif includes manager widgets
that manage the geometries of other widgets via constraints, row/column
placement, or menu layout.  Shell and dialog widgets provide top-level
windows that talk to the window manager. Other widgets that are or will be
available in the public domain or through various software suppliers
include table layout managers, directed-graph layout managers, graphical
gauges, plotting and line graphics widgets, etc.
.LP
\fIXlisp\fP classes describe the type of a particular object by declaring a
set of variables held in each object. These \fIinstance variables\fP may
only be accessed by \fImethods\fP that respond to \fImessages\fP sent to
the object.  Methods are defined for particular classes, and functionality
of other classes may be incorporated into new classes via
\fIinheritance\fP.  From \fIXlisp\fP, Motif widget classes and instances
look just like normal \fIXlisp\fP classes and instances, meaning that one
can add new methods or override old ones, possibly using polymorphism to
exploit similarities between types despite different implementations. Type
inheritance and subclassing are available to specialize existing widget
classes.  The result is that \fIWinterp\fP provides a very clean way to
interactively rapid-prototype an application, while also providing
mechanisms for code structuring and reuse.  The latter is necessary in
evolving from prototype to a structured, maintainable, and customizable
deliverable.
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.\"subsection{}
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.NH 2
Open Application Architecture
.LP
\fIWinterp\fP promotes an open, extensible architecture for applications
because designers cannot foresee all the possible needs of the end-user. In
addition to being open to the application customizer, \fIWinterp\fP is also
open to systems integration in which applications must work together with
other applications running on the network. \fIWinterp\fP enables such
integration because its language interpreter is implemented as a server.
.LP
Thus, all \fIWinterp\fP-based applications have a built-in, extensible
remote procedure call (RPC) mechanism which allows other applications,
possibly running non-locally, to send commands to execute application
functionality.  Such an architecture allows applications to talk to each
other, share data, etc. The server configuration can be specified via
Xresources, allowing the user of WINTERP or a WINTERP-based application to
enable/disable Unix-Domain or Inet-Domain server sockets.
.LP
Figure 3 shows a diagram of \fIWinterp\fP's server architecture. Both local
and remote applications can invoke remote procedure calls in
\fIWinterp\fP-based applications. Programmatic changes can be sent to
\fIWinterp\fP from the \fIUnix\fP command shell and shell scripts via a
simple client program, \f(CWwl\fP, which is included with the \fIWinterp\fP
distribution.  For example, a user-defined function
\f(CWstart-application\fP may be called within \fIWinterp\fP by executing
the following \fIUnix\fP command:
.\"%%%%%%%%%%%%%%%
.\"begin{verbatim}
.\"%%%%%%%%%%%%%%%
.LS
        wl '(start-application)'
.\"%%%%%%%%%%%%%%%
.\"end{verbatim}
.\"%%%%%%%%%%%%%%%
.LE
.\"%%%%%%%%%%%%%
.\"begin{figure}
.\"label{winterp_architecture}
.\"%%%%%%%%%%%%%
.KF
.sp 12.5c
.sp
.CD
Figure 3. Inter-application communications via \fIWinterp\fP's \fIXlisp\fP server.
.DE
.\"%%%%%%%%%%%%%
.\"end{figure}
.\"%%%%%%%%%%%%%
.KE
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.\"subsection{}
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.NH 2
\*QWhat you program is what you see\*U
.LP
The \fIGNU Emacs\fP editor provides an elegant environment for writing and
formatting\**
.FS
Emacs' Lisp-mode will automatically indent one's code and help catch
unmatched parentheses.
.FE
\fIWinterp\fP programs. The \fIWinterp\fP distribution contains an
extension to Emacs' Lisp-mode whereby an Emacs command will send the
current Lisp form being edited to \fIWinterp\fP for evaluation.  This
allows truly interactive programming because one need not exit the editor
to see the results of evaluating a code fragment; with \fIWinterp\fP, one
can see the graphical results of interactive changes to a program
immediately. Note that \fIWinterp\fP is architecturally separate from the
\fIGNU Emacs\fP editor (see Figure 3) \*- interfaces to other editors are
possible, but are not provided in the current \fIWinterp\fP distribution.
.LP
Non-Emacs users may find an example program in the \fIWinterp\fP
distribution useful for interactively editing and evaluating code with the
Motif text editor widget. By loading \f(CWwinterp/examples/w_ctrlpnl.lsp\fP
into \fIWinterp\fP, a window will pop up providing a rudimentary editor,
file browser, and control panel for the system. The control panel contains
buttons that control \fIXlisp\fP's debugger and error backtrace, as well as
controls to load files, edit files, and evaluate the Lisp form being
edited.
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.\"subsection{}
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.NH 2
Automatic Resource Conversions
.LP
In \fIWinterp\fP, any Motif resource that can be represented in the X
resource manager (i.e. one can set the resource via .Xdefaults) can be
specified as a Lisp string, and it will automatically be converted to the
appropriate type. This is especially useful for automatically converting
strings to XmStrings. Other useful conversions include converting color
names to type \f(CWPixel\fP, and converting bitmap file names to type
\f(CWPixmap\fP.
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.\"subsection{}
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.NH 2
Interpreted Callbacks
.LP
Xtoolkit callbacks, event-handlers, timeouts, translations, and
accelerators are seamlessly integrated with \fIWinterp\fP \*- Xevents can
cause arbitrary code (both Lisp and C) to be executed. (See the example
code in the next section for details.)
.LP
For Xtoolkit translations and accelerators, \fIWinterp\fP includes a
special action procedure \f(CWLisp()\fP whose arguments are evaluated as a
Lisp function call. For example, the following Xtoolkit translation- or
accelerator-table entry\**
.FS
A translation/accelerator entry may be specified in an X resource file,
e.g. .Xdefaults, or may be set directly within \fIWinterp\fP.
.FE
will cause the application-defined function \f(CWctrl-A-hit\fP to be called
when the key sequence CTRL-A is entered on the widget:
.\"%%%%%%%%%%%%%%%
.\"begin{verbatim}
.\"%%%%%%%%%%%%%%%
.LS
        "Ctrl<Key>A: Lisp(ctrl-A-hit ACTION_WIDGET ACTION_XEVENT)"
.\"%%%%%%%%%%%%%%%
.\"end{verbatim}
.\"%%%%%%%%%%%%%%%
.LE
.LP
The symbol \f(CWACTION_WIDGET\fP above is bound to the widget-object that
caused the action procedure to fire, and \f(CWACTION_XEVENT\fP is bound to
the XEvent-object that matched the translation or accelerator entry.  These
values are then passed as arguments to function \f(CWctrl-A-hit\fP.
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.\"subsection{}
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.NH 2
Automatic Memory Management
.LP
\fIXlisp\fP's garbage collector has been extended to reclaim unused X,
Xtoolkit, and Motif storage \*- this allows programmers to concentrate on
the UI and application functionality, rather than memory management.
Memory management is one of the pitfalls that complicates X programming in
C \*- novices have difficulty in determining the lifetime of objects in X
and the Xtoolkit, resulting in hard-to debug program crashes if objects are
freed too early, or subtle memory leaks if objects are not freed at all. In
\fIWinterp\fP, memory need not be managed explicitly, since Lisp's garbage
collection automatically frees up any memory that is no longer referenced
by the system.
.LP
In particular, \fIXlisp\fP's garbage collector has been extended to reclaim
storage associated with destroyed widget objects, along with any
toolkit-internal storage associated with these widget objects. Such garbage
collection occurs for callbacks, event-handlers, timeouts, XmStrings, and
XmStringTables.
.LP
Garbage collection of pixmaps in \fIWinterp\fP is especially useful because
we can expect pixmaps to take up a reasonable amount of client and server
space in typical graphic/iconic Motif applications. \fIWinterp\fP extends
Motif's reference counting and pixmap caching scheme to work with garbage
collection. Server and client-side storage associated with a pixmap will
get reclaimed when no references to them exist in any widget-object or
other \fIWinterp\fP variable.
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.\"subsection{}
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.NH 2
Programming by Direct Manipulation
.LP
The current release of \fIWinterp\fP includes a primitive which allows for
\*Qprogramming by direct manipulation.\*U When one is interactively,
programmatically modifying a user interface, one often wants to send a
message to a widget-object without knowing it's \*Qname\*U or the symbol to
which the widget object is bound. The primitive \f(CWget_moused_widget\fP
allows messages to be passed to any widget-object one can point the mouse
at. Developers may use this for rapid prototyping \*- they can immediately
see the changes they are making to the UI. Users may customize delivered
\fIWinterp\fP-based applications by simply \*Qpointing\*U at the widgets
they wish to change \*- colors, fonts, sizes, callbacks, and other such
widget parameters may be changed on the fly.
.LP
\f(CWget_moused_widget\fP may also be used to implement an interactive,
direct manipulation builder and resource editor application on top of
\fIWinterp\fP. Other direct manipulation primitives are being considered to
allow widgets to be placed or moved interactively.
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.\"subsection{}
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.NH 2
Extensibility
.LP
\fIWinterp\fP is not a closed system. Hybrid programming with \fIXlisp\fP
makes it straightforward to add new C-implemented widgets to the system so
that they can be accessed via the interpreter. It is also easy to add
special Xlib-level primitives to \fIWinterp\fP in order to implement
functionality not available via the widgets. The same goes for interfaces
to special \fIUnix\fP device drivers, etc.
.LP
Simple widgets (such as a pushbutton) can be interfaced with about 10 lines of C
code, which needs to be linked in to the rest of system. More complex
widgets (such as a text or graph editor) are added with the same techniques
as used by simple widgets.  However, if such widgets provide a number of
\*Qconvenience functions\*U (methods), then each convenience function will
need to be interfaced to Lisp. Each convenience function interface usually
takes about 5-10 lines of C code. Such C-level interfacing of convenience
functions and widgets is simple because most of it amounts to programming
via example (\*Qcut and paste\*U) from existing code.
.LP
Widgets requiring new resource representations can be added as well. To
achieve this, one needs to code Lisp-to-resource converters, or simply use
the String-to-Resource converters required to make such widgets work with
the X resource database.
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.\"section{}
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.NH 1
Example Use
.LP
In the following code, we will first step through some simple \fIWinterp\fP
code that will allow us to create windows, put widgets inside those windows
and attach callbacks to execute code when a particular widget is activated
via X input. Afterwards, we will take some of concepts from the example
code and apply it to a real application \*- a bitmap browser.
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.\"subsection{}
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.NH 2
Introduction to Xlisp Syntax
.LP
First, we shall introduce a few rudimentary aspects of \fIWinterp\fP
programming and \fIXlisp\fP syntax for those not familiar with the
language. The examples below only use a small fraction of the functionality
of Lisp \*- assignment, function calls, object creation, and message
passing.
.\"---------------
.\"begin{itemize}
.\"---------------
.RS .5in
.\"-----
.\"\item
.\"-----
.Ip
Symbols:
.LP
Those not familiar with Lisp will find symbols to be similar to variables
and function names in compiled languages such as Pascal or C.  When a
symbol is evaluated, it returns it's value. A value can be assigned to a
symbol with the function \f(CWsetq\fP.
.\"-----
.\"\item
.\"-----
.Ip
Assignment:
.LP
The function \f(CWsetq\fP will bind \f(CW<value>\fP to \f(CW<symbol>\fP:
.\"%%%%%%%%%%%%%%%
.\"begin{verbatim}
.\"%%%%%%%%%%%%%%%
.LS
        (setq <symbol> <value>)
.\"%%%%%%%%%%%%%%%
.\"end{verbatim}
.\"%%%%%%%%%%%%%%%
.LE
Upon evaluation of \f(CW<symbol>\fP, \f(CW<value>\fP will be returned.
.\"-----
.\"\item
.\"-----
.Ip
Function calls:
.LP
A function named by the symbol \f(CW<function-name>\fP can be called with a
sequence of arguments \f(CW<argument-1> <argument-2>...<argument-n>\fP.
The syntax for a function call is:
.\"%%%%%%%%%%%%%%%
.\"begin{verbatim}
.\"%%%%%%%%%%%%%%%
.LS
        (<function-name> <argument-1> <argument-2>...<argument-n>)
.\"%%%%%%%%%%%%%%%
.\"end{verbatim}
.\"%%%%%%%%%%%%%%%
.LE
Where each \f(CW<argument-i>\fP is a symbol, a function call, or a value.
Each argument is evaluated before being passed in to the function
associated with \f(CW<function-name>\fP.
.\"-----
.\"\item
.\"-----
.Ip
Sending a message to an object:
.LP
The \fIXlisp\fP function \f(CWsend\fP allows one to send the \fImessage\fP
\f(CW:<message>\fP to the object \f(CW<object-instance>\fP. If
\f(CW<object-instance>\fP's \fIclass\fP contains a \fImethod\fP for the
particular \fImessage\fP, that method will be called just like a function
call with arguments \f(CW[...arguments...]\fP.
.\"%%%%%%%%%%%%%%%
.\"begin{verbatim}
.\"%%%%%%%%%%%%%%%
.LS
        (send <object-instance> :<message> [...arguments...])
.\"%%%%%%%%%%%%%%%
.\"end{verbatim}
.\"%%%%%%%%%%%%%%%
.LE
.\"-----
.\"\item
.\"-----
.Ip
Object creation:
.LP
To create a new instance of \fIclass\fP \f(CW<object-class>\fP, send the
message \f(CW:NEW\fP to the class. The arguments \f(CW[...arguments...]\fP
are dependent on what kinds of initialization need to be done by the
instance initializer method \f(CW:ISNEW\fP.
.\"%%%%%%%%%%%%%%%
.\"begin{verbatim}
.\"%%%%%%%%%%%%%%%
.LS
        (send <object-class> :NEW [...arguments...])
.\"%%%%%%%%%%%%%%%
.\"end{verbatim}
.\"%%%%%%%%%%%%%%%
.LE
.\"-------------
.\"\end{itemize}
.\"-------------
.RE
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.\"subsection{}
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.NH 2
Create a Top-Level Window:
.LP
Below, we create a top level window\**
.FS
A top level window is conceptually a child of the root window in the X
server. These top level windows are managed by the window manager, allowing
users to move, resize, and iconize application windows.
.FE
by creating an instance of the Xt Intrinsics
\f(CWtopLevelShellWidgetClass\fP. The symbol \f(CWtoplevel_w\fP is bound to
the widget-object representing the new top level widget instance.  The
keyword arguments \f(CW:XMN_HEIGHT\fP and \f(CW:XMN_WIDTH\fP are set so
that the window is of size 500x500\**
.FS
Hard-coding the window size in an X application is not a good idea. The
code we are presenting here is just the result of our interactive session
with \fIWinterp\fP, and not representative of how a real application should
be coded.
.FE
, and we also set the titlebar and icon names displayed by the window
manager.
.LP
In order to actually create the toplevel window, the Xt Intrinsics require
that function \f(CWXtRealizeWidget()\fP be called \*- that operation is
done by sending the message \f(CW:REALIZE\fP to the widget instance.
.LP
Sending the two forms below to \fIWinterp\fP's server will cause a new
toplevel window to pop up immediately:
.\"%%%%%%%%%%%%%%%
.\"begin{verbatim}
.\"%%%%%%%%%%%%%%%
.LS
(setq toplevel_w
      (send TOP_LEVEL_SHELL_WIDGET_CLASS :new
            :XMN_HEIGHT    500
            :XMN_WIDTH     500
            :XMN_TITLE     "Winterp: example-1"
            :XMN_ICON_NAME "example-1"
            ))
(send toplevel_w :realize)
.\"%%%%%%%%%%%%%%%
.\"end{verbatim}
.\"%%%%%%%%%%%%%%%
.LE
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.\"subsection{}
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.NH 2
Create a Widget Layout Manager:
.LP
Above, we have created an instance of a
\f(CWTOP_LEVEL_SHELL_WIDGET_CLASS\fP which is bound to symbol
\f(CWtoplevel_w\fP. We now attach a layout manager widget as a child of the
top level widget.  Layout manager widgets generally do not add to the
visual appearance of an application in and of themselves, rather, they
manage the layout and geometries of the set of child widgets that appear
within them.  One of the important manager widgets in Motif is the
Row/Column manager \f(CWxmRowColumnWidgetClass\fP. This widget will lay out
its child widgets to appear in a configurable number of rows and columns.
.LP
In the code below, we create an instance of
\f(CWXM_ROW_COLUMN_WIDGET_CLASS\fP bound to symbol \f(CWrowcol_w\fP. The
widget is a child of an instance of \f(CWXM_SCROLLED_WINDOW_WIDGET_CLASS\fP
which is bound to \f(CWscrl_w\fP. The scrolled window widget will put up
scrollbars if the row/column widget within in grows larger than the 500x500
toplevel window associated with \f(CWtoplevel_w\fP. The arguments to the
resources \f(CW:XMN_ORIENTATION\fP and \f(CW:XMN_PACKING\fP cause the row
column widget's children to be laid out vertically, with no space padding
between widgets.
.LP
Sending the following code to \fIWinterp\fP's server will cause the widgets
to be created:
.\"%%%%%%%%%%%%%%%
.\"begin{verbatim}
.\"%%%%%%%%%%%%%%%
.LS
(setq scrl_w
      (send XM_SCROLLED_WINDOW_WIDGET_CLASS :new :managed 
            toplevel_w                      ;parent is toplevel
            :XMN_SCROLLING_POLICY :automatic
            ))
(setq rowcol_w
      (send XM_ROW_COLUMN_WIDGET_CLASS :new :managed
            scrl_w                          ;parent is scroller
            :XMN_ORIENTATION      :vertical
            :XMN_PACKING          :pack_tight
            :XMN_ENTRY_ALIGNMENT  :alignment_center
            :XMN_FOREGROUND       "Black"
            :XMN_BACKGROUND       "LightGray"
            ))
.\"%%%%%%%%%%%%%%%
.\"end{verbatim}
.\"%%%%%%%%%%%%%%%
.LE
.LP
Note that the \f(CW:MANAGED\fP argument for widget creation does the
equivalent of \f(CWXtManageWidget()\fP. In simplistic terms, managing a
child widget makes it visible within its parent.
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.\"subsection{}
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.NH 2
Adding Widgets to the Layout Manager:
.LP
Now we can interactively add a variety of widgets to the Row/Column manager
by sending each of the forms below to \fIWinterp\fP's server.  For every
form that gets evaluated below, we will see the corresponding widget appear
within the window titled \*QWinterp: example-1\*U that we created earlier.
See Figure 4 to see how these widgets look.
.\"%%%%%%%%%%%%%%%
.\"begin{verbatim}
.\"%%%%%%%%%%%%%%%
.LS
(setq arrow_up_w
      (send XM_ARROW_BUTTON_WIDGET_CLASS :new :managed rowcol_w
            :XMN_ARROW_DIRECTION :arrow_up
            ))
(setq big_arrow_left_w
      (send XM_ARROW_BUTTON_WIDGET_CLASS :new :managed rowcol_w
            :XMN_ARROW_DIRECTION :arrow_left
            :XMN_HEIGHT          50
            :XMN_WIDTH           50
            ))
(setq label_w
      (send XM_LABEL_WIDGET_CLASS :new :managed rowcol_w
            :XMN_LABEL_STRING "hi, I'm a label widget"
            :XMN_FOREGROUND      "White"
            :XMN_BACKGROUND      "Black"
            ))
(setq string_push_button_w
      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed rowcol_w
            :XMN_LABEL_STRING    "hi, I'm a pushbutton widget"
            :XMN_FOREGROUND      "Black"
            :XMN_BACKGROUND      "LightGrey"
            ))
(setq pixmap_push_button_w
      (send XM_PUSH_BUTTON_WIDGET_CLASS :new :managed rowcol_w
              :XMN_LABEL_TYPE    :pixmap
              :XMN_LABEL_PIXMAP  (xm_get_pixmap 
                                  "/users/mayer/src/bitmaps/bob0.xbm"
                                  "black" "white")
              ))
(setq text_editor_w 
      (send XM_TEXT_WIDGET_CLASS :new :managed :scrolled rowcol_w
            :XMN_EDIT_MODE       :MULTI_LINE_EDIT
            :XMN_FONT_LIST       "hp8.10x20b"
            :XMN_HEIGHT          100
            :XMN_FOREGROUND      "Black"
            :XMN_BACKGROUND      "LightGrey"
            ))
(setq string_toggle_button_w
      (send XM_TOGGLE_BUTTON_WIDGET_CLASS :new :managed rowcol_w
            :XMN_LABEL_STRING    "hi, I'm a togglebutton widget"
            ))
(setq pixmap_toggle_button_w
      (send XM_TOGGLE_BUTTON_WIDGET_CLASS :new :managed rowcol_w
            :XMN_LABEL_TYPE      :pixmap
            :XMN_LABEL_PIXMAP    "/users/mayer/src/bitmaps/bob1.xbm"
            ))
.\"%%%%%%%%%%%%%%%
.\"end{verbatim}
.\"%%%%%%%%%%%%%%%
.LE
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.\"subsection{}
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.NH 2
Setting or Querying the State of a Widget
.LP
Messages may be sent to widgets in order to set or get widget-specific
state information. Widget resources are accessible via the messages
\f(CW:SET_VALUES\fP and \f(CW:GET_VALUES\fP, the methods associated with
those messages correspond to the Xtoolkit functions \f(CWXtSetValues()\fP
and \f(CWXtGetValues()\fP, respectively.
.LP
Some widget classes define certain class-specific methods \*- for example,
the Motif \f(CWXM_TEXT_WIDGET_CLASS\fP responds to message
\f(CW:SET_STRING\fP by invoking the method \f(CWXmTextSetString()\fP which
sets the string within the editor. To set the string in the editor widget
instance created above, we do:
.\"%%%%%%%%%%%%%%%
.\"begin{verbatim}
.\"%%%%%%%%%%%%%%%
.LS
(send text_editor_w :set_string "Hi, I'm a text editor widget")
.\"%%%%%%%%%%%%%%%
.\"end{verbatim}
.\"%%%%%%%%%%%%%%%
.LE
.LP
Note that \fIWinterp\fP's use of a real object system brings added safety
and error checking to programming with Motif. In C, a programmer may make
the naive assumption that calling \f(CWXmTextSetString()\fP on a label
widget would set the text string in the label; in reality, the program
would core dump because the function was called on an instance of the wrong
class. In \fIWinterp\fP, such a misunderstanding would result in a
continuable error which the programmer could fix and continue-from
interactively.
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.\"subsection{}
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.NH 2
Adding Callbacks
.LP
Above, we have shown how to create a \*Qrandom\*U layout of widgets with
\fIWinterp\fP. In order to make the interface do anything, we need to add
callbacks, event-handlers, or actions to the widgets \*- these will call
arbitrary code in response to user input.
.LP
For example, suppose we want to make the push button widget with the label
\f(CW"hi I'm a pushbutton widget"\fP call some code that will print
something on standard output when it is activated via mouse. To do this, we
use the \fIWinterp\fP equivalent of \f(CWXtAddCallback()\fP, the method
\f(CW:add_callback\fP.
.LP
Each widget class's \f(CW:ADD_CALLBACK\fP method takes as argument the name
of the callback list, a widget-class specific list of symbols to bind to
data from the callback's \f(CWcall_data\fP callback structure, and a list
containing arbitrary code to be evaluated upon execution of the callback.
Note that the symbols bound to the values from \f(CWcall_data\fP are only
bound within the lexical and dynamic scope of the callback code.
.LP
In the example below, we are adding a callback to the push button widget's
\f(CWXmNactivateCallback\fP list, which will fire the callback's code
whenever the pushbutton is pressed and released. The code simply calls the
\f(CWformat\fP function, which is akin to the \fIUnix\fP
\f(CWfprintf(3S)\fP subroutine: it will print out the callback reason and
the widget-object that caused the callback on the \fIUnix\fP standard
output.
.\"%%%%%%%%%%%%%%%
.\"begin{verbatim}
.\"%%%%%%%%%%%%%%%
.LS
(send string_push_button_w :add_callback
      :XMN_ACTIVATE_CALLBACK                  ;name of callback list
      '(CALLBACK_REASON CALLBACK_WIDGET)      ;list of callback data
      '(                                      ;code to execute
        (format T "reason = ~A; widget = ~A\\\\n" 
                CALLBACK_REASON CALLBACK_WIDGET)
        ))
.\"%%%%%%%%%%%%%%%
.\"end{verbatim}
.\"%%%%%%%%%%%%%%%
.LE
.LP
Obviously, the callback above doesn't do anything very interesting. We will
have to wait till the bitmap browser example to see a more useful callback.
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.\"subsection{}
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.NH 2
Direct Manipulation Programming
.LP
In \fIWinterp\fP, any widget's resources may be changed interactively via
method \f(CW:SET_VALUES\fP, which is equivalent to the Xtoolkit's
\f(CWXtSetValues()\fP. For example, we can interactively change the
direction of the arrow in the arrow button widget by sending \fIWinterp\fP
the following code.  Immediately after \fIWinterp\fP evaluates this code,
one will see the button display a downward facing arrow:
.\"%%%%%%%%%%%%%%%
.\"begin{verbatim}
.\"%%%%%%%%%%%%%%%
.LS
(send arrow_up_w :set_values
      :XMN_ARROW_DIRECTION :arrow_down
      )
.\"%%%%%%%%%%%%%%%
.\"end{verbatim}
.\"%%%%%%%%%%%%%%%
.LE
.LP
In the example above, we needed to get a hold of the widget-object value
bound to symbol \f(CWarrow_up_w\fP before we could operate on the widget.
In some cases, this is impractical, as one would rather just point at the
desired widget. The \fIWinterp\fP primitive function
\f(CWget_moused_widget\fP allows just that.
.LP
If we send the form below to \fIWinterp\fP's server, we will see the cursor
change to a \*Qcrosshair\*U indicating we are to click on a widget.  Upon
clicking on the widget, we will see it change colors:
.\"%%%%%%%%%%%%%%%
.\"begin{verbatim}
.\"%%%%%%%%%%%%%%%
.LS
(send (get_moused_widget) :set_values 
      :XMN_FOREGROUND "Green"
      :XMN_BACKGROUND "LightGrey"
      )
.\"%%%%%%%%%%%%%%%
.\"end{verbatim}
.\"%%%%%%%%%%%%%%%
.LE
.LP
The primitive \f(CWget_moused_widget\fP can be used in any place that
requires a widget-object as a parameter. One can use this function to
interactively change callbacks, font sizes, colors, etc. In delivered
\fIWinterp\fP-based applications, one can use this feature to experiment
with resource settings without needing documentation on the internals of
the application, nor the widget hierarchy.
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.\"subsection{}
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.NH 2
A Simple Application \*- a Bitmap Browser
.LP
We now create a simple application \*- a bitmap browser \*- using some of
the ideas from the code above. For each bitmap file in a directory of
bitmaps, this browser will put up a label widget containing the name of the
bitmap file, followed by a push button widget displaying the bitmap itself,
followed by a separator widget. All these widgets are held within a
scrolled row/column widget, which we've already seen used in the example
above. When a push button containing a pixmap is pressed, that pixmap
becomes the root window's tile pattern by calling the X11 program
\f(CWxsetroot(1)\fP with the appropriate arguments.  In Figure 4, the
window titled \*QBitmap Browser Example\*U shows what this bitmap browser
looks like.
.LP
The simplicity of implementing the bitmap browser application demonstrates
the power of \fIWinterp\fP and the expressiveness of its language syntax.
Furthermore, this example illustrates one of the interesting features of
\fIWinterp\fP \*- the ability to subclass a Motif widget by using
\fIXlisp\fP's object system.
.LP
We have seen the following code in the example above \*- here, we create a
top level widget, which contains a scrolled window widget, which in turn
contains a Row/Column widget:
.\"%%%%%%%%%%%%%%%
.\"begin{verbatim}
.\"%%%%%%%%%%%%%%%
.LS
(setq toplevel_w
      (send TOP_LEVEL_SHELL_WIDGET_CLASS :new 
            :XMN_TITLE "Bitmap Browser Example"
            :XMN_ICON_NAME "Bitmap-Br"
            ))
(setq scrl_w
      (send XM_SCROLLED_WINDOW_WIDGET_CLASS :new :managed
            toplevel_w
            :XMN_SCROLLING_POLICY :automatic
            ))
(setq rowcol_w
      (send XM_ROW_COLUMN_WIDGET_CLASS :new :managed
            scrl_w
            :XMN_ORIENTATION     :vertical
            :XMN_PACKING         :pack_tight
            :XMN_ENTRY_ALIGNMENT :alignment_center
            :XMN_FOREGROUND      "Black"
            :XMN_BACKGROUND      "LightGray"))
.\"%%%%%%%%%%%%%%%
.\"end{verbatim}
.\"%%%%%%%%%%%%%%%
.LE
.LP
Rather than placing an identical callback structure on each push button
widget in the browser (which is memory-inefficient), we exploit a feature
of the Motif Row/Column widget which allows us to place a single callback,
\f(CWXmNentryCallback\fP, on the Row/Column manager widget. Any activate
callback occurring on a widget that is a child of this Row/Column widget
will end up firing the callback below.  When the callback fires, symbol
\f(CWCALLBACK_ENTRY_WIDGET\fP will be bound to the entry in the Row/Column
widget that was activated. The code for the callback then sends the message
\f(CW:XSETROOT\fP to that widget, which will cause the root window's
background tile to be set to the bitmap contained in the button that was
activated.
.\"%%%%%%%%%%%%%%%
.\"begin{verbatim}
.\"%%%%%%%%%%%%%%%
.LS
(send rowcol_w :set_callback :XMN_ENTRY_CALLBACK
       '(CALLBACK_ENTRY_WIDGET)
       '(
         (send CALLBACK_ENTRY_WIDGET :xsetroot)
         ))
.\"%%%%%%%%%%%%%%%
.\"end{verbatim}
.\"%%%%%%%%%%%%%%%
.LE
.LP
Now, we create a specialization of the Motif
\f(CWXM_PUSH_BUTTON_GADGET_CLASS\fP\**
.FS
A Motif gadget, is for all intents and purposes the same as a Motif widget.
They are more efficient, and take up less client and server memory.
.FE
by making a trivial subclass called \f(CWNiels_Pixmap_Push_Button_Class\fP.
.LP
From the outside, instances of this class work just like a pushbutton. The
difference is that at initialization time, an instance of
\f(CWNiels_Pixmap_Push_Button_Class\fP requires that one give it a filename
containing the full path to a X bitmap file. The pushbutton will then
display this bitmap.
.LP
Internally, this subclass contains an additional instance variable
\f(CWpixmap_file\fP. Each instance of this push button will thus contain
the name of the bitmap file it is displaying. This instance variable is
later used by the method \f(CW:XSETROOT\fP.
.LP
The following code implements the subclassed widget.  The second form below
overrides the instance initializer associated with the superclass (the
\fIWinterp\fP code associated with \f(CWXM_PUSH_BUTTON_GADGET_CLASS\fP that
actually creates the widget) in order to initialize the instance variable
and pass special arguments on to the superclass's initializer method
\f(CW:ISNEW\fP
.\"%%%%%%%%%%%%%%%
.\"begin{verbatim}
.\"%%%%%%%%%%%%%%%
.LS        
;;;
;;; make a trivial subclass of XM_PUSH_BUTTON_GADGET_CLASS
;;;
(setq Niels_Pixmap_Push_Button_Class                    
      (send Class :new
            '(pixmap_file)  ;a new inst-var for this subclass
            '()             ;no class vars for subclass
            XM_PUSH_BUTTON_GADGET_CLASS)) 
;;;
;;; override XM_TOGGLE_BUTTON_GADGET_CLASS's instance initializer
;;;
(send Niels_Pixmap_Push_Button_Class :answer :isnew
      '(filename &rest args)
      '(
        (setq pixmap_file filename)
        (apply 'send-super `(:isnew ,@args
                                    :XMN_LABEL_TYPE :pixmap
                                    :XMN_LABEL_PIXMAP ,filename))
        ))
.\"%%%%%%%%%%%%%%%
.\"end{verbatim}
.\"%%%%%%%%%%%%%%%
.LE
.LP
Now we define the method \f(CW:XSETROOT\fP for the new class. When one
sends the message \f(CW:XSETROOT\fP to an instance of class
\f(CWNiels_Pixmap_Push_Button_Class\fP, this method answers by calling the
X program \f(CWxsetroot(1)\fP specifying the bitmap file that was contained
in the instance variable \f(CWpixmap_file\fP. \fIWinterp\fP calls the
\f(CWxsetroot(1)\fP program through the primitive \f(CWsystem\fP, which
corresponds to the \fIUnix\fP subroutine \f(CWsystem(3S)\fP:
.\"%%%%%%%%%%%%%%%
.\"begin{verbatim}
.\"%%%%%%%%%%%%%%%
.LS        
;;;
;;; a method that calls the xsetroot(1) program to set background
;;;
(send Niels_Pixmap_Push_Button_Class :answer :xsetroot '()
      '(
        (system (format nil "xsetroot -bitmap ~A -fg Black -bg DimGrey" 
                        pixmap_file))
        ))
.\"%%%%%%%%%%%%%%%
.\"end{verbatim}
.\"%%%%%%%%%%%%%%%
.LE
.LP
The code below returns a stream of bitmap file names in a particular
directory on my system.
.LP
We use the \fIUnix\fP \f(CWpopen(3s)\fP routine to read the results of the
\fIUnix\fP command \f(CWls(1)\fP, which returns to \f(CWstdout\fP a list of
matching filenames in the shell created by \f(CWpopen(3s)\fP.\**
.FS
People familiar with \f(CWpopen(3S)\fP will realize that the command
\f(CW/bin/ls\fP could in fact be replaced by \f(CW/bin/echo\fP.
.FE
\f(CWpopen(3s)\fP returns a \f(CWFILE*\fP that can be read by the
\fIXlisp\fP primitive \f(CWread-line\fP.
.\"%%%%%%%%%%%%%%%
.\"begin{verbatim}
.\"%%%%%%%%%%%%%%%
.LS
(setq ls_reader_pipe
      (popen "/bin/ls /usr/local/mayer/src/bitmaps/*.xbm" 
             :direction :input))
.\"%%%%%%%%%%%%%%%
.\"end{verbatim}
.\"%%%%%%%%%%%%%%%
.LE
.LP
The following code does the brunt of the work in the bitmap browser
application. The \f(CWdo*\fP form is essentially a do-loop. It will
repeatedly use primitive \f(CWread-line\fP to retrieve a single file name
from \f(CWls_reader_pipe\fP that we created above. For each filename
retrieved, we create an instance of \f(CWXM_LABEL_GADGET_CLASS\fP
displaying the name of the bitmap file, followed by an instance of
\f(CWNiels_Pixmap_Push_Button_Class\fP displaying the pixmap itself, and
finally, we create an instance of \f(CWXM_SEPARATOR_GADGET_CLASS\fP to
separate the buttons from one another inside the Row/Column widget that is
managing them.
.LP
When \f(CWread-line\fP returns NIL, it means that we have hit the end of
file in \f(CWls_reader_pipe\fP. At this point we have created a browser of
all the bitmaps in the directory specified above. To clean up, we call
\f(CWpclose(3S)\fP to close the pipe opened by \f(CWpopen(3S)\fP and then
call \f(CWXtRealizeWidget()\fP to create the windows associated with the
browser, which is displayed in Figure 4.
.\"%%%%%%%%%%%%%%%
.\"begin{verbatim}
.\"%%%%%%%%%%%%%%%
.LS        
(do* 
 (;; local do-loop variables with initialize and increment expressions.
  (file-name (read-line ls_reader_pipe) (read-line ls_reader_pipe))
  )
 ((null file-name) ;loop done when (read-line) returns NIL ==> EOF
  )
 (send XM_LABEL_GADGET_CLASS :new :managed
       rowcol_w
       :XMN_LABEL_TYPE :STRING
       :XMN_LABEL_STRING file-name)
 (send Niels_Pixmap_Push_Button_Class :new file-name :managed
       rowcol_w)
 (send XM_SEPARATOR_GADGET_CLASS :new :managed
       rowcol_w
       :XMN_SEPARATOR_TYPE :DOUBLE_LINE)
 )
;;;
;;; Cleanup
;;;
(pclose ls_reader_pipe)        ;close the pipe
(send toplevel_w :realize)     ;create the widgets windows
.\"%%%%%%%%%%%%%%%
.\"end{verbatim}
.\"%%%%%%%%%%%%%%%
.LE
.\"%%%%%%%%%%%%%
.\"begin{figure}
.\"label{winterp_snapshot}
.\"%%%%%%%%%%%%%
.KF
.sp 8.25i
.sp
.CD
Figure 4. The graphics produced by the \fIWinterp\fP example code.
.DE
.\"%%%%%%%%%%%%%
.\"end{figure}
.\"%%%%%%%%%%%%%
.KE
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.\"section{}
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.\" .1C
.NH 1
A Real Application \*- A Search Browser Based on the Unix Grep(1) Program.
.LP
In this section we create a Motif user interface to the \fIUnix\fP
\f(CWgrep(1)\fP program. This example continues to show off the power and
expressivity of object oriented programming in Winterp-Lisp \*- we make
extensive use of the \fIXlisp\fP object system here. This simple but useful
application searches a wildcarded set of files for a regular expression,
displaying each matching line in a browser. When an item is browsed (via
mouse double click) the file associated with the item is displayed, with
the matching line displayed at the top of the file. See Figure 5 to see
what the \f(CWgrep(1)\fP browser application looks like.
.LP
As an introductory example of using the object system, we create a new
class \*- \f(CWGrep-Item-Class\fP. Instances of the class correspond to a
datum produced by the \fIUnix\fP \f(CWgrep(1)\fP file searching program.
\f(CWgrep(1)\fP produces output consisting of file-names, line-numbers, and
the instance of a regular expression match. Below, we create a new class
containing those items as instance variables. The new class instance is
created by sending the instance creation message \f(CW:NEW\fP to the class
\f(CWClass\fP:
.LS
;; (SEND Grep-Item-Class :NEW) returns <grep-item> instance.
(SETQ Grep-Item-Class
      (SEND Class :NEW
            '(file-name line-num match-line)
            ))
.LE
.LP
Below, we define a method corresponding to the message
\f(CW:display-string\fP \*- this method will be used by the search browser
as a way to present the objects in the browser.  We also define the
accessor methods \f(CW:file-name\fP and \f(CW:line-num\fP.
.LS
;; (SEND <grep-item> :display-string) returns "<filename>: <match-line>"
(SEND Grep-Item-Class :ANSWER :display-string '()
      '(
        (FORMAT NIL "~A: ~A"    
                file-name match-line)
        ))
;; (SEND <grep-item> :file-name) returns file-name (string) of <grep-item>
(SEND Grep-Item-Class :ANSWER :file-name '()
      '(
        file-name
        ))
;; (SEND <grep-item> :file-name) returns line-number (fixnum) of <grep-item>
(SEND Grep-Item-Class :ANSWER :line-num '()
      '(
        line-num
        ))
.LE
.LP
Now we add a new method on the class defined above, which reads a line of
output from the \fIUnix\fP command \f(CWgrep(1)\fP from a pipe, and puts
the data into the instance variables of a \f(CWGrep-Item-Class\fP object.
.LS
;; (SEND <grep-item> :read-grep-info <pipe>) returns <grep-item> or NIL
;; if EOF. Ivars are initialized to data from a line of "grep -n" output,
;; which is of the format <filename>:<linenum>:<matching line>\\\\n
(SEND Grep-Item-Class :ANSWER :read-grep-info '(pipe)
      '(
        (if (AND
             (SETQ file-name  (FSCANF-STRING pipe "%[^:]:"))
             (SETQ line-num   (FSCANF-FIXNUM pipe "%d:"))
             (SETQ match-line (FSCANF-STRING pipe "%[^\\\\n]\\\\n"))
             )
          SELF                          ;return SELF if successful
          NIL                           ;return NIL if hit EOF
          )
        ))
.LE
.LP
The Lisp function \f(CWgrep\fP, below, is an example of using
\f(CWGrep-Item-Class\fP. This function invokes \f(CW"grep -n <regexp>
<wildcarded files>"\fP in a sub-shell, sending the output of the command to
a pipe. An instance of \f(CWGrep-Item-Class\fP is created for each regular
expression match found, with method \f(CW:read-grep-info\fP initializing the
object to contain the data corresponding to one match. The function returns
a list of objects corresponding to the matching items found by
\f(CWgrep(1)\fP. This function plays a key role in the upcoming search
browser example application.
.LS
;; (grep <grep-arg-string>) returns list of <grep-item>.
;; <grep-arg-string> == "[flags] <regexp> <wildcarded files>"
(DEFUN grep (grep-arg-string) 
  (DO* 
   (;; loop variables, initializers, and increments.
    (fp (POPEN (STRCAT "grep -n " grep-arg-string " /dev/null")
               :DIRECTION :INPUT))
    (line (SEND (SEND Grep-Item-Class :NEW) :read-grep-info fp)
          (SEND (SEND Grep-Item-Class :NEW) :read-grep-info fp))
    (result '())                        ;init to an empty list
    )
   ;; loop test and return
   ((NULL line)                         ;:read-grep-info returns NIL on EOF
    (PCLOSE fp)                         ;close the pipe opened above
    (REVERSE result)                    ;return list of grep objects.
    )
   ;; loop body
   (SETQ result (CONS line result))     ;prepend grep-obj to list
   ))
.LE
.LP
As an example of using \fIXlisp\fP's object-oriented features with Motif
widgets, we specialize Motif's text editor widget as a file-viewer via
(trivial) subclassing. Instances of the new class,
\f(CWText-Viewer-Widget-Class\fP, display the file of a browsed item with
the text scrolled to the appropriate line. We subclass the Motif XmText
widget so that we can add a method \f(CW:find-file\fP which reads the
specified file, displays it in the text widget, and scrolls the text to the
appropriate line number. The class has an extra instance variable,
\f(CWfile-path\fP which stores the name of the file. This allows method
\f(CW:find-file\fP to be more intelligent \*- if the same file is browsed
multiple times, it will not be reread each time.
.LP
First, we define the new class by sending message \f(CW:NEW\fP to class
\f(CWClass\fP, with arguments specifying the instance variables of the
class, as well as the superclass. Then, we define the instance initializer
method \f(CW:ISNEW\fP, which will automatically be called whenever new
instances of the widget are created (by sending \f(CW:NEW\fP to the class).
The initializer method sets the instance variable, then calls the
superclass' initializer method to create the Motif text editor widget.
Since \f(CWText-Viewer-Widget-Class\fP is a specialized editor, the
instance initializer hard-codes optional features of the editor such that
all instances end up having scroll bars, display multiple lines of text,
etc.
.LS
(SETQ Text-Viewer-Widget-Class
      (SEND Class :NEW
            '(file-path)                ;new instance vars
            '()                         ;no class vars
            XM_TEXT_WIDGET_CLASS))      ;superclass

;; (SEND Text-Viewer-Widget-Class :NEW <:MANAGED/:UNMANAGED> <parent> [resources...])
(SEND Text-Viewer-Widget-Class :ANSWER :ISNEW '(managed-kwd &rest args)
      '(
        (SETQ file-path "")    ;initialize instance var
        (APPLY 'SEND-SUPER     ;call superclass's init to create widget
               `(:ISNEW ,managed-kwd ;either :MANAGED or :UNMANAGED
                        :SCROLLED    ;force the editor to have scrollbars
                        ,@args       ;parent widget + optional arguments
                        :XMN_EDIT_MODE :MULTI_LINE_EDIT ;multi line for files
                        :XMN_EDITABLE NIL ;do not allow user to edit text.
                        ))
        ))
.LE
.LP
The method \f(CW:find-file\fP is defined next. Note that in \fIXlisp\fP
methods, the symbol \f(CWSELF\fP is bound to the object receiving the
message. The messages \f(CW:SET_INSERTION_POSITION\fP,
\f(CW:GET_INSERTION_POSITION\fP, \f(CW:SCROLL\fP, \f(CW:REPLACE\fP,
\f(CW:SET_STRING\fP, \f(CW:ENABLE_REDISPLAY\fP and
\f(CW:DISABLE_REDISPLAY\fP correspond to methods defined on the superclass
\f(CWXM_TEXT_WIDGET_CLASS\fP. These are implemented in C by the Motif
toolkit.
.LS
;; (SEND <textviewer> :find-file <filename> <linenum>)
(SEND Text-Viewer-Widget-Class :ANSWER :find-file '(filename linenum)
      '(
        (COND
         ((STRING= filename file-path)  ;if file already read into widget
          (SEND SELF :SET_INSERTION_POSITION 0) ;then make <linenum> top
          (SEND SELF :SCROLL (1- linenum))      ;by scrolling to it.
          )
         (T                                     ;else read file into widget.
          (LET* ((fp (OPEN filename :DIRECTION :INPUT))
                 insert-pos
                 text-line)
            (IF (NULL fp)
                (ERROR "Can't open file." filename))

            (SEND SELF :SET_STRING "")          ;clear out old text
            (SEND SELF :DISABLE_REDISPLAY NIL)  ;show no changes till done
            (LOOP
             (IF (NULL (SETQ text-line (READ-LINE fp)))
                 (RETURN))
             (SETQ insert-pos (SEND SELF :GET_INSERTION_POSITION))
             (SEND SELF :REPLACE insert-pos insert-pos (STRCAT text-line "\\\\n"))
             )
            (SEND SELF :SCROLL linenum)   ;make <linenum> top of screen
            (SEND SELF :ENABLE_REDISPLAY) ;now show changes...
            (CLOSE fp)
            (SETQ file-path filename)
            )))
        ))
.LE
.LP
The browser user-interface to \f(CWgrep(1)\fP is made from the Motif list
widget, which simply displays a list of (compound) strings as a series of
selectable lines of text.  Callbacks can be defined on the list widget such
that when a line is selected one can retrieve the compound string that was
browsed, or get back the position (index) of the item that was browsed.
Both callback mechanisms require the programmer to maintain an external
association between the browser objects and their textual representation in
the list widget.  By subclassing the list widget, we can create an
interface that hides the mechanism for associating objects with the strings
representing them. As an additional feature, the abstraction enables the
browser to display an arbitrary collection of objects, as long as the
objects respond to a simple \*Qprotocol\*U \*- objects receiving the
message \f(CW:display-string\fP must return a textual representation of the
object.
.LP
The subclassed list widget adds a new instance variable \f(CWitems\fP which
holds an array of objects presented by the browser. The browser will
display the list of objects given as argument to the method
\f(CW:set-browser-items\fP (the method also initializes \f(CWitems\fP).
When an item is selected via a callback, the position of the selected item
is passed to method \f(CW:get-br-item-at-pos\fP, which returns
the browsed object using an efficient lookup implemented by array indexing.
.LS
(SETQ List-Browser-Widget-Class
      (SEND Class :NEW
            '(items)                    ;new instance vars
            '()                         ;no class vars
            XM_LIST_WIDGET_CLASS))      ;superclass

(SEND List-Browser-Widget-Class :ANSWER :set-browser-items '(items-list)
      '(
        (LET* (
               (items-end-idx (LENGTH items-list))
               (display-items (MAKE-ARRAY items-end-idx)))

          ;; initialize the 'items' instance variable so that it
          ;; holds all the BROWSER_OBJECTs passed in <items-list>
          (SETQ items (MAKE-ARRAY items-end-idx)) ;create the array
          (DO (                         ;copy elts from list to array
               (i    0          (1+ i))
               (elts items-list (CDR elts)))
              ;; loop till no more elts
              ((NULL elts))
              ;; loop body
              (SETF (AREF items i) (CAR elts)) ;set items[i]
              (SETF (AREF display-items i)     ;set display-items[i]
                    (SEND (CAR elts) :display-string))
              )
          ;; initialize the widget, passing in the browser items.
          (SEND SELF :SET_VALUES
                :XMN_ITEMS      display-items
                :XMN_ITEM_COUNT items-end-idx
                )
          )
        ))
(SEND List-Browser-Widget-Class :ANSWER :get-br-item-at-pos '(position)
      '(
        (AREF items (1- position))
        ))
.LE
.LP
The user interface for the search browser application is created by
defining a hierarchy of widget-objects. Parent widgets manage the
geometries of their children, so the resulting interface is a nesting of
windows corresponding to the parent-child relationships created.  Refer to
Figure 5 to see the graphics produced by the code below.
.LP
In the UI code below, we first create an instance of
\f(CWTOP_LEVEL_SHELL_WIDGET_CLASS\fP which represents the outermost window
of the application \*- its geometry is managed by the X window manager,
allowing the user to move, resize, and iconize the window.  Inside that, we
create an instance of \f(CWXM_PANED_WINDOW_WIDGET_CLASS\fP: this widget
divides up the space of the toplevel window into a series of vertical
panes; the sizes of the panes can be adjusted by moving their resize
handles with the mouse. The first pane is an instance of
\f(CWXM_ROW_COLUMN_WIDGET_CLASS\fP, a manager widget which lays out the
controls for the search browser in the following horizontal sequence: (1) a
pushbutton, \f(CWdoit-button-w\fP; (2) a text label "Search for string:";
(3) a single-line text editor, \f(CWsearch-editor-w\fP, into which the user
types the regular expression for the search; (4) a label "From Files:"; and
(5) a single-line text editor, \f(CWfiles-editor-w\fP, into which the user
types the wildcarded filenames to search. The pane below the controlpanel
is \f(CWbrowser-w\fP, an instance of the \f(CWList-Browser-Widget-Class\fP.
The pane below the browser is \f(CWfilename-label-w\fP, which displays the
name of the file being browsed. And finally, the last pane is
\f(CWviewtext-w\fP, an instance of \f(CWText-Viewer-Widget-Class\fP which
is used to display the browsed files. The keyword \f(CW:SCROLLED\fP used in
creating the text viewer and browser widgets mean that these widgets are
created with scrollbars.
.LE
.\"%%%%%%%%%%%%%
.\"begin{figure}
.\"label{winterp_snapshot}
.\"%%%%%%%%%%%%%
.KF
.sp 8.25i
.sp
.CD
Figure 5. The \fIWinterp\fP \f(CWgrep(1)\fP browser example application.
.DE
.\"%%%%%%%%%%%%%
.\"end{figure}
.\"%%%%%%%%%%%%%
.KE
.LS
(LET (top-w paned-w controlpanel-w)
  (SETQ top-w
        (SEND TOP_LEVEL_SHELL_WIDGET_CLASS :NEW 
              :XMN_TITLE "Grep Browser"
              :XMN_ICON_NAME "Grep Browser"
              ))
  (SETQ paned-w
        (SEND XM_PANED_WINDOW_WIDGET_CLASS :NEW :MANAGED
              top-w                     ;the only child of the toplevel window
              ))
  (SETQ controlpanel-w
        (SEND XM_ROW_COLUMN_WIDGET_CLASS :NEW :MANAGED
              paned-w                   ;the first child of the paned window
              :XMN_ORIENTATION :HORIZONTAL
              :XMN_PACKING :PACK_TIGHT
              ))
  (SETQ doit-button-w
        (SEND XM_PUSH_BUTTON_WIDGET_CLASS :NEW :MANAGED
              controlpanel-w            ;the first child of the controlpanel
              :XMN_LABEL_STRING "Do Search"
              ))
  (SEND XM_LABEL_WIDGET_CLASS :NEW :MANAGED
        controlpanel-w                  ;the second child of the controlpanel
        :XMN_LABEL_STRING "Search for string:"
        )
  (SETQ search-editor-w
        (SEND XM_TEXT_WIDGET_CLASS :NEW :MANAGED
              controlpanel-w            ;the third child of the controlpanel
              :XMN_EDIT_MODE :SINGLE_LINE_EDIT
              ))
  (SEND XM_LABEL_WIDGET_CLASS :NEW :MANAGED
        controlpanel-w                  ;the fourth child of the controlpanel
        :XMN_LABEL_STRING "From Files:"
        )
  (SETQ files-editor-w
        (SEND XM_TEXT_WIDGET_CLASS :NEW :MANAGED
              controlpanel-w            ;the fifth child of the controlpanel
              :XMN_EDIT_MODE :SINGLE_LINE_EDIT
              ))
  (SETQ browser-w
        (SEND List-Browser-Widget-Class :NEW :MANAGED :SCROLLED
              paned-w                   ;the second child of the paned window
              :XMN_VISIBLE_ITEM_COUNT 20
              ))
  (SETQ filename-label-w
        (SEND XM_LABEL_WIDGET_CLASS :NEW :MANAGED
              paned-w                   ;the third child of the paned window
              :XMN_LABEL_STRING "None"
              ))
  (SETQ viewtext-w
        (SEND Text-Viewer-Widget-Class :NEW :MANAGED
              paned-w                   ;the fourth & final child of paned window
              :XMN_HEIGHT 400
              ))
  (SEND top-w :REALIZE)                 ;create the toplevel window & children
  ;;
  ;; now that the widgets have been created, we know their sizes; set constraint 
  ;; resources on the controlpanel and the filename label widget so that the paned
  ;; window widget managing their geometries won't let them be resized (this 
  ;; removes the "resize handles" for these widgets).
  (LET (h)
       (SEND controlpanel-w :GET_VALUES :XMN_HEIGHT 'h)
       (SEND controlpanel-w :SET_VALUES 
             :XMN_PANE_MAXIMUM h
             :XMN_PANE_MINIMUM h))
  (LET (h)
       (SEND filename-label-w :GET_VALUES :XMN_HEIGHT 'h)
       (SEND filename-label-w :SET_VALUES 
             :XMN_PANE_MAXIMUM h
             :XMN_PANE_MINIMUM h))
  )
.LE
.LP
Calling function \f(CWdo-grep-search\fP runs \f(CWgrep(1)\fP, searching for
the regular expression in \f(CWsearch-editor-w\fP from the list of
wildcarded files specified in \f(CWfiles-editor-w\fP.  The list of objects
returned by \f(CWgrep\fP is then displayed in the browser widget:
.LS
(DEFUN do-grep-search ()
  (SEND browser-w :set-browser-items
        (grep (STRCAT
               "'"                                ;protect regexps from shell
               (SEND search-editor-w :GET_STRING) ;get regexp for search
               "' "
               (SEND files-editor-w :GET_STRING)) ;wildcarded files to search
              )))
.LE
.LP
We complete the user interface by attaching programmatic actions \*-
callbacks \*- to the widget-objects created above. First, we want the
pushbutton associated with symbol \f(CWdoit-button-w\fP to initiate
searches when it is pressed by calling \f(CWdo-grep-search\fP.
.LS
(SEND doit-button-w :ADD_CALLBACK :XMN_ARM_CALLBACK '()
      '(
        (do-grep-search)
        ))
.LE
.LP
We now attach a callback to the browser widget \*- when a browser-item is
double clicked, symbol \f(CWCALLBACK_ITEM_POSITION\fP gets bound to the
position of the object in the browser. The position is passed to
\f(CW:get-br-item-at-pos\fP which returns the browsed item. The
file-name and line-number data from the grep-item is retrieved and used to
display the file at the correct line number in the text viewer widget.
.LS
(SEND browser-w :ADD_CALLBACK :XMN_DEFAULT_ACTION_CALLBACK
      '(CALLBACK_ITEM_POSITION) ;bound to the position of the item selected
      `(
        (LET* ((browsed-object
                (SEND ,browser-w :get-br-item-at-pos CALLBACK_ITEM_POSITION))
               (filename
                (SEND browsed-object :file-name))
               (linenum
                (SEND browsed-object :line-num))
               )
          (SEND ,filename-label-w :SET_VALUES :XMN_LABEL_STRING filename)
          (SEND ,viewtext-w :find-file filename linenum)
          ))
      )
.LE
.LP
Finally, to make the user-interface easier to use, we note that after
entering text into one of the single-line text editor widgets, it is more
natural to hit the return-key to initiate a new search. Otherwise one has
to move from the keyboard to the mouse just to click the "Do Search"
button. The following Xtoolkit translation entries provide the binding
between hitting the return-key and calling the \f(CWdo-grep-search\fP
function that initiates a new search.
.LS
(SEND search-editor-w :OVERRIDE_TRANSLATIONS 
      "<Key>Return: Lisp(do-grep-search)")
(SEND files-editor-w  :OVERRIDE_TRANSLATIONS 
      "<Key>Return: Lisp(do-grep-search)")
.LE
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.\"subsection{}
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.NH 2
A Note on Programming Style.
.LP
The examples presented in this section do not necessarily represent good
Motif or \fIWinterp\fP programming style \*- the style has been relaxed
and simplified for the purpose of exposition.
.LP
First, we have used global variables throughout. For these examples, such
usage makes sense because that is the way one might use Lisp in
interactively prototyping an application \*- try bits of functionality out
piece by piece until one has come up with functionality that deserves to be
encapsulated. Once the prototype works, one may want to turn the code into
a function, or encapsulate it within an \fIXlisp\fP object. In that way,
the reliance on global variables will be removed.
.LP
Second, the examples above hard-code a number of X resources that should be
specifiable in the X resource database \*- either in .Xdefaults or in the
application defaults files. For the purposes of these examples, it would
have been confusing to specify these resources separately from the code.
Again, since the above code is prototype code, it is perfectly justifiable
to hard-code resources until one has settled on which parameters of
customization should and should not be accessible via the X resource
database.
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.\"section{}
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.NH 1
Future Directions
.LP
\fIWinterp\fP's features make it a good platform for a number of
interesting experiments in UIMS and \*Qdirect manipulation builder\*U
technologies. Because \fIWinterp\fP is currently a language-based builder,
it would make an ideal platform for the development of a \*Qtwo view\*U
builder that would allow application prototyping via programmatic or visual
manipulation of UIs.
.LP
\fIWinterp\fP was designed to support the addition of direct manipulation
interface building capabilities. We envision that a widget \*Qpalette\*U
could be built to allow the interactive selection and placement of widgets
within an interface. Another useful feature would be a resource browser
which would be used to display and edit the resources associated with any
widget in the system. Extending \fIWinterp\fP's widget-object based
interfaces so that they are \*Qself describing\*U would be a very elegant way
of dumping out a programmatic user-interface specification after it has
been built interactively and would provide the basis for a \*Qtwo view\*U
approach to interface building.
.LP
One of the areas where current direct-manipulation and \*QWhat You See Is
What You Get\*U (WYSIWYG) interface builders fail is in allowing end-user
customization without having to include the builder in the deliverable. The
challenge here is to allow the traditional X resources mechanism to customize
sizes, fonts, spacing, line-widths, etc., while still maintaining a
facsimile of the static UI layout intended by the application designers.
Traditional Xtoolkit-based applications do not suffer from such problems
because their interfaces are laid-out via the constraints provided by
geometry management widgets. With builders, such constraint management is
hard to specify graphically, so widget positioning and sizes must be
hard-coded.  What is needed is the ability to provide an explicit,
user-manipulable interface to the constraint-based language that is
implicit in the Xtoolkit's manager widgets. This is an open and active
research area.
.LP
Robert Leichner of HP Labs has already provided a novel UIMS structure for
\fIWinterp\fP by building a general-purpose event-driven recursive state
machine as a high level means of describing how application state interacts
with the UI. This UIMS architecture is being used in an ongoing project to
support collaboration among distributed workgroups via the use of
multimedia (audio and video). \fIWinterp\fP and the state-machine based
UIMS are being used as the basis for a media management toolkit which
controls experimental multimedia hardware residing on the workstation.  The
\fIStrudel\fP project is also experimenting with a variety of UIMS
approaches in order to come up with a high-level description of e-mail
based forms.
.LP
In order to ease application development and simplify \*Qhybrid
programming\*U, we are considering the addition of a dynamic loader to allow
new C-implemented primitives to be loaded into a running application.  It
may also be useful to dynamically load widget code on demand, instead of
always having it compiled in to the application.
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.\"section{}
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.NH 1
Conclusion
.LP
We believe that \fIWinterp\fP provides an excellent, practical development
and delivery environment for extensible Motif-based applications. If this
paper has piqued your interest in \fIWinterp\fP, you may obtain the current
source, documentation, and examples via anonymous ftp from host
\f(CWexport.lcs.mit.edu\fP: in directory \f(CWcontrib/winterp\fP you will
find the \f(CWcompress(1)'d\fP \f(CWtar(1)\fP file
\f(CWwinterp-<latestversion>.tar.Z\fP\**.
.FS
As of this writing, <latestversion> \(>= 1.12 .
.FE
If you do not have Internet access you may request the source code to be
mailed to you by sending a message to
\f(CWwinterp-source@hplnpm.hpl.hp.com\fP or
\f(CWhplabs!hplnpm!winterp-source\fP.
.LP
There is also a mailing list for \fIWinterp\fP-related announcements and
discussions. To get added to the list, send mail to
\f(CWwinterp-request@hplnpm.hpl.hp.com\fP or
\f(CWhplabs!hplnpm!winterp-request\fP.
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.\"section{}
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.NH 1
Acknowledgements
.LP
I would like to thank my team-mates Allan Kuchinsky, Allan Shepherd, and
Bob Leichner for being highly supportive early users of \fIWinterp\fP and
giving feedback on problems and limitations of the design.  I would also
like to thank Nancy Kendzierski, manager of HP Labs' Human-Computer
Interaction Department for providing the support for the development of
\fIWinterp\fP and \fIStrudel\fP. Also, Nancy Kendzierski, Jim Miller, and
Allan Kuchinsky provided helpful comments on this paper.
.LP
Doug Young deserves special thanks for answering lots of \*Qstupid
questions\*U I had in working with the Motif and HP widgets and for
providing early copies of his excellent book on programming with Xt and
Motif [Young90]. And finally, many thanks are due to David Betz for making
\fIXlisp\fP publicly available.
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.\"section{}
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.NH 1
Addendum \*- Instructions for Obtaining WINTERP 1.12 via anonymous ftp.
.LP
The following instructions give the details of how to obtain \fIWinterp\fP
sources, examples, and documentation via anonymous ftp on a \fIUnix\fP
workstation.
.LS
Note: In the output below, your input is denoted by '^^^^^^^^^^')

hplnpm-17-/tmp> ftp export.lcs.mit.edu  (HP-ites, do ftp hplnpm.hpl.hp.com)
                ^^^^^^^^^^^^^^^^^^^^^^               ^^^^^^^^^^^^^^^^^^^^^
        [...]
Name (hplnpm.hpl.hp.com:mayer): anonymous
                                ^^^^^^^^^
Password (hplnpm.hpl.hp.com:anonymous): <anypassword you want here>
                                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^
331 Guest login ok, send ident as password.
230 Guest login ok, access restrictions apply.
ftp> cd contrib/winterp   (HPites should do cd pub)
     ^^^^^^^^^^^^^^^^^^                     ^^^^^^
200 CWD command okay.

ftp> type image
     ^^^^^^^^^^
200 Type set to I.

ftp> get winterp-1.12.tar.Z
     ^^^^^^^^^^^^^^^^^^^^^^
200 PORT command okay.
150 Opening data connection for winterp-1.12.tar.Z (15.255.176.225,3988) (1414493 bytes).
226 Transfer complete.
1414493 bytes received in 690.63 seconds (1.96 Kbytes/sec)

ftp> quit
     ^^^^
221 Goodbye.

hplnpm-18-/tmp> zcat winterp-1.12.tar.Z | tar xvf -
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        [... lengthy list of files from tar ...]

hplnpm-19-/tmp> rm winterp-1.12.tar.Z
                ^^^^^^^^^^^^^^^^^^^^^

   * For compilation tips, take a look at winterp-1.12/doc/COMPILING

   * For running hints, take a look at winterp-1.12/doc/RUNNING

   * For known (Motif 1.1 only) bugs, see winterp-1.12/doc/BUGS

   * For information on the examples, look at winterp-1.12/examples/README

   * For general information about WINTERP, see winterp-1.12/doc/winterp.doc
.LE
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.\"begin{thebibliography} {99}
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.NH 1
References
.XP
[Betz89] David Michael Betz.  \fIXLISP: An Object-oriented Lisp (version
2.1)\fP.  Unpublished documentation accompanying the public domain
\fIXlisp\fP software release.  David Michael Betz, P.O. Box 144,
Peterborough, NH 03458, April, 1989. Note: this documentation is included
in the \fIWinterp\fP source distribution.
.XP
[Borenstein88] Nathaniel S. Borenstein and Chris A. Thyberg.  Cooperative
Work in the Andrew Message System.  In proceedings \fIConference on
Computer-Supported Cooperative Work\fP, 1988.
.XP
[Bourne90] Philip E. Bourne and Lawrence S. Shapiro.  Developing with
DECwindows.  \fIDEC Professional\fP, vol. 9, no. 2, pp. 36-44, February
1990.
.XP
[Comer86] D. Comer and L. Peterson.  Conversation-Based Mail.  \fIACM
Transactions on Computer Systems\fP, vol. 4, no. 4, pp 299-319, November
1986.
.XP
[Conk87] Jeff Conklin and Michael Begeman.  gIBIS: A Hypertext Tool for
Team Design Deliberation.  In proceedings \fIHypertext '87 Papers\fP,
November 1987.
.XP
[Creech87a] Michael Creech.  Lisp-C Evaluation Final Report.  Internal
Technical Report STL-TM-87-18, Hewlett-Packard Laboratories, Software
Technology Lab, July 9, 1987.
.XP
[Creech87b] Michael Creech, Scott Marovich and Niels Mayer.  Lisp-C
Evaluation Meeting Notes.  Internal Technical Report STL-TM-87-17,
Hewlett-Packard Laboratories, Software Technology Lab, July 22, 1987.
.XP
[Dollimore89] Jean Dollimore and Sylvia Wilbur.  Experiences in building a
configurable {CSCW} system.  In proceedings \fI1st European Conference on
CSCW\fP, pp 215-225, September 1989.
.XP
[Fikes82] R.E. Fikes.  A Commitment-Based Framework for Describing Informal
Cooperative Work.  \fICognitive Science\fP, vol. 6, no. 4, pp. 331-347,
1982.
.XP
[Heimburger] Olaf Heimburger. Elche Im Winter \*- Interaktive
X-Applicationbuilder unter Lisp \*- Elk und WINTERP. \fIiX\fP, July 1991, pp
64-68.
.XP
[Holt81] A.W. Holt and P.M. Cashman.  Designing Systems to Support
Cooperative Activity: An Example.  In \fIProceedings of Compsac 81. IEEE
Computer Society's Fifth International Computer Software And Applications
Conference\fP, pp 18-91, November 1981.
.XP
[Kaplan90] Simon Kaplan.  COED: A Conversation-oriented tool for
coordinated design work.  In \fIProceedings of IFIP International Workshop
on Human Factors in Information Systems\fP, in print, June 1990.
.XP
[Lai88] Kum-Yew Lai and Tom Malone.  Object-Lens: A Spreadsheet for
Cooperative Work.  In proceedings \fIConference on Computer-Supported
Cooperative Work\fP, September 1988.
.XP
[Malone86] Tom Malone, K. Grant, K. Lai, R. Rao, and D. Rosenblitt.
Semi-structured Messages are Surprisingly Useful for Computer-Supported
Coordination.  In proceedings \fIConference on Computer-Supported
Cooperative Work\fP, pp 102-114, December 1986.
.XP
[Mayer90a] Niels P. Mayer, Allan W. Shepherd and Allan J. Kuchinsky.
Winterp: An object-oriented, rapid prototyping, development and delivery
environment for building extensible applications with the OSF/Motif UI
Toolkit.  In Proceedings \fIXhibition '90, X Window System and Open Systems
Technical Conference\fP, San Jose, CA, May 1990, pp 49-64.
.XP
[Mayer90b] Niels P. Mayer, Allan W. Shepherd and Allan J. Kuchinsky.  The
WINTERP Widget INTERPreter \*- An Application Prototyping and Extension
Environment for OSF/Motif.  In Proceedings \fIX Into The Future, The
European X Users Group Autumn Conference 1990\fP, Surrey, UK, September
1990, pp. 33-55.
.XP
[Mayer91] Niels P. Mayer.  The WINTERP Widget INTERPreter \*- A Lisp
Prototyping and Extension Environment for OSF/Motif-based Applications and
User-Interfaces.  \fILisp Pointers\fP, \fIACM SIGPLAN\fP, Volume IV, Number
1, pp 45-60.
.XP
[Myers89] Brad A. Myers.  Tools for Creating User Interfaces: An
Introduction and Survey.  \fIIEEE Software\fP, vol. 6, no. 1, pp. 15-23,
January 1989.
.XP
[OSF90] Open Software Foundation.  \fIOSF/Motif Series\fP (5 Volumes):
\fIMotif Style Guide; Programmer's Guide; Programmer's Reference; User's
Guide; Application Environment Specification; User Environment Volume\fP.
Prentice-Hall, 1990.
.XP
[Reichman85] Rachel Reichman. \fIGetting Computers to Talk Like You and
Me\fP.  The MIT Press, 1985.
.XP
[Rosenberg88] Jarrett Rosenberg (moderator), Ralph Hill, Jim Miller, Andrew
Schulert, and David Shewmake (panelists).  UIMSs: Threat or Menace?  In
\fIHuman Factors in Computing Systems\fP, SIGCHI '88, Washington, D.C., May
1988, pp. 197-200.
.XP
[Rose86] M.T. Rose and J.L. Romine.  \fIThe Rand MH message handling system:
User's manual, UCI Version 6.5\fP.  University of California, December
1986.
.XP
[Schulert88] Andrew Schulert and Kate Erf.  Open Dialogue: Using an
extensible retained object workspace to support a UIMS. In proceedings \fI
USENIX C++ Workshop\fP, Denver, Colorado, 1988.
.XP
[SEI89] Software Engineering Institute.  \fISerpent Overview\fP.
Technical Report CMU/SEI-89-UG-2, Carnegie Mellon University, Software
Engineering Institute, August 1989.
.XP
[Searle76] John R. Searle.  A Taxonomy of Illocutionary Acts.  In K.
Gunderson, editor, \fILanguage, Mind and Knowledge\fP University of
Minnesota Press, 1976.
.XP
[Shepherd90] Allan Shepherd, Niels Mayer, and Allan Kuchinsky.  Strudel: An
Extensible Electronic Conversation Toolkit.  In proceedings \fIConference
on Computer-Supported Cooperative Work\fP, Los Angeles, October 1990, pp. 93-104.
.XP
[Stallman87] Richard M. Stallman.  \fIGNU Emacs Manual\fP.  Free Software
Foundation, 675 Massachusetts Ave., Cambridge, MA 02139, 1987.
.XP
[Sulonen90] Reijo Sulonen and Panu Pietikainen.  Forget-Me-Not \*-
Controlling intercompany operations by intelligent mail.  In \fIProceedings
23rd Annual Hawaii International Conference on Systems Sciences\fP, pp.
428-435, 1990.
.XP
[Trigg86] Randall Trigg, Lucy A. Suchman, and Frank G. Halasz.  Supporting
Collaboration in NoteCards.  In \fIConference on Computer-Supported
Cooperative Work\fP, December 1986.
.XP
[Winograd86] Terry Winograd and Fernando Flores.  \fIUnderstanding
computers and cognition, A new foundation for design\fP.  Ablex, Norwood
New Jersey, 1986.
.XP
[Winograd87] Terry Winograd.  A Language/Action Perspective On The Design
Of Cooperative Work.  Technical Report CSLI-87-98, Stanford University,
1987.
.XP
[Young90] Douglas A. Young.  \fIThe X Window System: Programming and
Applications With Xt, OSF/Motif Edition\fP.  Prentice Hall, 1990.
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
.\"end{thebibliography}
.\"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
