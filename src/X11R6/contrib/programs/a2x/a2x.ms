.\" Use -ms
.\" $XConsortium: a2x.ms,v 1.47 94/04/17 20:45:39 rws Exp $
.de Ds
.RS
.nf
.sp .5
..
.de De
.fi
.sp .5
.RE
..
.EH ''Using a2x''
.OH ''Using a2x''
.EF ''\fB % \fP''
.OF ''\fB % \fP''
.ps 10
.nr PS 10
\&
.sp 10
.ce 1
\s+2\fBUsing a2x\fP\s-2
.sp 10
.ce 2
Bob Scheifler
MIT X Consortium
.ps 9
.nr PS 9
.sp 10
.LP
.nf
DragonDictate is a trademark of Dragon Systems, Inc.
X Window System is a trademark of the Massachusetts Institute of Technology
DESQview/X is a trademark of Quarterdeck Office Systems
.fi
.ps 10
.nr PS 10
.bp 1
.NH 1
Getting Started
.LP
\fBa2x\fP is designed to interface DragonDictate to the X Window System.
\fBa2x\fP converts the ASCII output of DragonDictate into X device events.
\fBa2x\fP is capable of manipulating both the keyboard and the pointer;
depending on the applications, it is possible to use X hands-free through
\fBa2x\fP.
.LP
\fBa2x\fP requires an X protocol extension to function.  The XTEST, DEC-XTRAP,
and XTestExtension1 extensions can be used.  The XTEST extension was developed
by the X Consortium for use in the X Test Suite, and an R5 patch to support
the XTEST extension is available via anonymous ftp to ftp.x.org, as
the file /pub/XTEST/R5fix-xtest-1.  The DEC-XTRAP extension was developed by
Digital Equipment Corp., and public versions are available for both R4 and R5.
The XTestExtension1 extension was originally developed for an old test suite
done by an informal group known as the X Testing Consortium; the client-side
library and some device-independent code exists in both the R4 and R5 core
distributions, but only a couple sample servers (HP and X386) in the X 
Consortium distribution supports the extension (some product servers might 
though).
.LP
\fBa2x\fP was originally written for the XTEST extension, but both DEC-XTRAP
and XTestExtension1 should also work.  The main difference as far as \fBa2x\fP
is concerned is that XTEST permits time delays inside the X server, while with
DEC-XTRAP and XTestExtension1 the delays are handled on the client side and
hence can be somewhat less accurate, but time delays are rarely needed.  In
addition, XTestExtension1 does not provide a way to synthesize relative
pointer motion, so the WarpPointer protocol request is used instead.
.LP
The Imakefile for \fBa2x\fP comes configured to use only XTEST, but you can
configure it to compile in support for any or all of the extensions; \fBa2x\fP
will automatically choose whichever extension is supported by the display.  If
you dynamically switch among different displays supporting different
extensions, you will probably want to compile more than one extension into
\fBa2x\fP.  At the top of the Imakefile are #define's for BuildWithXTest,
BuildWithXTrap, and BuildWithXTestExt1; simply set the ones you want to YES
and the ones you don't want to NO.  To see which extensions are supported by
your X server, use \fBxdpyinfo\fP.  Your server must have at least one of
"XTEST", "DEC-XTRAP", or "XTestExtension1" in order to use \fBa2x\fP.
.LP
The Imakefile also comes configured by default to assume that DragonDictate
version 1.01 is being used, although the actual version can be overridden
on the \fBa2x\fP command line.  If you normally use DragonDictate 1.01a
or 2.0, then change the #define for OldDragonDictate to NO before building.
.LP
The documentation provided here is for using DragonDictate and \fBa2x\fP solely
for work under X; the assumption is that you do not use DragonDictate with
programs on the PC.  If you use DragonDictate with programs on the PC, you may
need to create dcom files for switching between PC and X use.
.LP
A mailing list of \fBa2x\fP users exists, to keep people abreast of
any new developments.  The mailing list is a2x-users@x.org; send requests to
join to a2x-users-request@x.org.
.LP
There are two ways to run DragonDictate: directly under DOS, and under
DESQview/X.  Running directly under DOS is described here; running under
DESQview/X is described later.
.LP
The normal mode of operation is to use a telnet program on the PC, and telnet
across to your workstation and log in.  You may wish to try using rsh instead
of telnet, but the performance may be substantially worse.  If you do not have
a network connection, it should be possible to use a terminal program and log
in to the workstation over a serial line, but performance over a serial line
will likely not be as good as over a network.  In the telnet session, run the
script \fBDragonDictate\fP provided with the \fBa2x\fP distribution.  You
should be able to speak this, by saying "DragonDictate", "[enter\ key]", "OK"
(the "OK" being necessary to pop down the menu to permit execution to
continue).  This script simply clears the screen and starts up \fBa2x\fP.
Clearing the screen is a convenience so that the focus of attention on the PC
screen will be the DragonDictate menus.  You can, of course, write your own
script or simply run \fBa2x\fP directly.
.LP
You will probably find it convenient to force the DragonDictate menus to
appear in a fixed position on the PC screen.  For example, in the "Parameters"
menu from "Voice Console", set
.Ds
.TA 1.5i
.ta 1.5i
pop-up-row	0
pop-up-col	24
.De
.LP
You should also set
.Ds
.TA 1.5i
.ta 1.5i
pop-up-delay	0
compatibility-delay	0
.De
for quickest operation.
.LP
\fBa2x\fP takes each ASCII character produced by DragonDictate and determines
a key on the X keyboard that can be pressed and released to generate that
character.  If Shift and Control modifiers are necessary to generate the
character, \fBa2x\fP will automatically press and release keys for those
modifiers.  For example, for the character 'A', \fBa2x\fP will typically have
to press a Shift modifier key, press an 'a' key, release the 'a' key, and then
release the Shift modifier key.
.LP
It is not necessary to perform all of your "typing" through \fBa2x\fP; you can
mix \fBa2x\fP output with direct keyboard typing and direct pointer
manipulation.  But, you should normally only switch between direct input and
\fBa2x\fP input when all keys and buttons are in the released state, to avoid
confusing \fBa2x\fP.  (One simple exception to this rule is that you can use
\fBa2x\fP to press/release buttons and keys while directly moving the pointer
with your hand.)
.LP
This document uses the notation {c\-\fI<char>\fP} to represent a control
character, the character you get by holding down the Ctrl (Control) key and
typing the character \fI<char>\fP.  For example, {c\-t} represents what you get
by holding Ctrl and typing 't'.  This syntax is the same as what you will see
in DragonDictate 1.01 menus if you enter control characters into macros.
In DragonDictate 2.0, the syntax has changed to {Ctrl\+\fI<char>\fP}, so
keep this difference in mind so that you don't get confused.
.LP
\fBa2x\fP interprets the character {c\-t} specially; it introduces a control
sequence for special \fBa2x\fP operations.  The character {c\-t} was chosen
because it occurs infrequently in normal text; escape was not chosen because
you might use it frequently in voice macros to send escape sequences to
applications.  There are two types of \fBa2x\fP control sequences: {c\-t}
followed by a single control character, and {c\-t} followed by a string of
characters terminated by another {c\-t}.  Control sequences are described in
detail below.
.LP
\fBa2x\fP also interprets the backspace character ({c\-h} or {Backspace})
specially.  When you make corrections through DragonDictate, e.g., you use
"choose \fI<n>\fP", DragonDictate will output backspaces to "undo" the
previous choice before outputting the characters for the new choice (if any).
For many X users, it is more convenient to receive Delete (Rubout) characters
instead of backspaces.  The default action of \fBa2x\fP is to convert
Backspace to Delete; if you do not want this behavior, specify "-b" on the
\fBa2x\fP command line.
.LP
\fBa2x\fP also interprets backspace characters specially in that it assumes
they are produced by DragonDictate to make corrections and are not produced as
normal text (i.e., that voice macros do not contain backspace characters).
\fBa2x\fP looks at backspace characters in order to undo edits (described in
more detail below).  As such, it is recommended that you change some
predefined DragonDictate voice macros to replace backspace characters with
delete characters or with control sequences to generate backspace.  For
example, you should modify the keystrokes of the built-in commands
"[backspace]", "[back\ 1]", "[back\ 2]", "[back\ 3]", "[back\ 4]", and
"[back\ 5]", replacing each "{Backspace}" with "{Del}" to generate delete
instead of backspace.  If you really want to generate backspace, then replace
each "{Backspace}" with the sequence "{c\-t}{c\-c}h" instead.
.LP
If you are using a terminal program over a serial line, make sure your program
is not automatically converting backspace to delete, as this will cause
\fBa2x\fP's undo mechanism to not work.
.NH 1
Keyboard Control
.LP
To "type" a non-ASCII key on the keyboard, use the following control sequence:
.Ds
{c-t}\fI<keysym>\fP{c-t}
.De
where \fI<keysym>\fP is the string name of a keysym.  \fBa2x\fP will find a
key on the keyboard labeled with this keysym, and press and release that key.
For example:
.Ds
add-word /t "[doit]" "{c-t}Execute{c-t}" invisible
.De
defines a voice macro to press and release a key labeled with the "Execute"
keysym.  If for some reason your Xlib does not know the string name of the
keysym you want, you can specify the hexadecimal value of the keysym instead.
For example:
.Ds
add-word /t "[doit]" "{c-t}FF62{c-t}" invisible
.De
would also specify the Execute keysym.
.LP
When the specified key is a modifier key, the key is pressed but is not
released until after the next nonmodifier key (or button) is pressed.  This is
called "sticky" modifiers.  For example:
.Ds
add-word /t "[meta key]" "{c-t}Meta_L{c-t}" left right
.De
would set the Meta modifier if a key labeled with Meta_L is attached to a
modifier.  So, you could say "[meta\ key]" "x" to generate meta-x.  However,
since there are both Meta_L and Meta_R keysyms, and since Meta is commonly
used, there is a special control sequence for this:
.Ds
{c-t}{c-m}
.De
So, more commonly you would specify:
.Ds
add-word /t "[meta key]" "{c-t}{c-m}" left right
add-word /t "[meta x]" "{c-t}{c-m}x" left right
.De
.LP
Similarly, there is a special control sequence for the Shift modifier:
.Ds
{c-t}{c-s}
.De
and a special control sequence for the Control modifier:
.Ds
{c-t}{c-c}
.De
.LP
The Shift control sequence is not normally needed in conjunction with
character keys, but is useful with function keys.  For example:
.Ds
add-word /t "[dismiss window]" "{c-t}{c-c}{c-t}{c-s}{c-t}{c-m}{c-t}F3{c-t}" invisible
.De
generates control-shift-meta-F3, which might, e.g., be bound in a .twmrc
as:
.Ds
"F3" = c|s|m : window : f.delete
.De
.LP
The Control sequence would not normally be needed in conjunction with
character keys, since DragonDictate can output control characters, but the
edit mechanism DragonDictate uses has problems with control characters.
Specifically, when DragonDictate generates backspaces as a result of
"choose\ \fI<n>\fP" or "[scratch\ that]", it will not generate backspaces for
control characters.  This is a problem, because it prevents \fBa2x\fP from
properly undoing control characters.  For example, if you defined:

.Ds
add-word /t "[move right]" "{c-f}" invisible
.De
in order to use emacs-style positioning control, then \fBa2x\fP would be
unable to provide the undo help of generating {c\-b} to compensate when
DragonDictate mistakenly matched this voice command, because DragonDictate
would not output any backspaces.  Instead, you should define:
.Ds
add-word /t "[move right]" "{c-t}{c-c}f" invisible
.De
.LP
This way, DragonDictate will output a backspace for correction, and \fBa2x\fP
will recognize that a control sequence is attached to the 'f' character and
treat the sequence as a single entity for undo purposes.  Of course, if no
undo action is desired for control characters, you can represent them
directly.  For example:
.Ds
add-word /t "[move end]" "{c-e}" invisible
.De
is reasonable for emacs-style positioning control because there is normally no
way to return to the original position.  You also don't need to expand control
characters if there are noncontrol characters in the sequence.  For example:
.Ds
add-word /t "[split window]" "{c-x}2" invisible
.De
.LP
DragonDictate will output a backspace for the '2', so the {c\-x} does not need
to be expanded to "{c\-t}{c\-c}x" (although it doesn't hurt).
.NH 1
Autorepeat
.LP
It is convenient at times to be able to autorepeat a keystroke, e.g., when
using incremental search in \fBemacs\fP or when paging through a document.  To
autorepeat the last keystroke generated by \fBa2x\fP, use the control
sequence:
.Ds
{c-t}{c-a}\fI<kdelay>\fP 0 0{c-t}
.De
where \fI<kdelay>\fP is a floating point number specifying the rate in
seconds.  For example:
.Ds
add-word /t "[autorepeat it]" "{c-t}{c-a}1.5 0 0{c-t}" invisible
.De
will autorepeat every 1.5 seconds.  So, you might have defined:
.Ds
add-word /t "[search for]" "{c-s}" left right
.De
for incremental search in \fBemacs\fP.  You can say "[search\ for]" to start a
search, say what you are searching for, say "[search\ for]" again, and then
say "[autorepeat\ it]" to autorepeat the search.
.LP
When autorepeat is in progress, it continues until you explicitly stop it or
until the next keystroke is generated through \fBa2x\fP.  You can explicitly
stop autorepeat with the control sequence:
.Ds
{c-t}{c-q}
.De
.LP
For example:
.Ds
add-word /t "[stop moving]" "{c-t}{c-q}" invisible
.De
.LP
The autorepeat control sequence also controls pointer motion; the two zeros
given in the keyboard autorepeat sequence can be replaced with other values,
as described further below.
.NH 1
Button Control
.LP
To toggle the state of a button, use the control sequence:
.Ds
{c-t}{c-b}\fI<button>\fP{c-t}
.De
where \fI<button>\fP is the decimal button number.  For example:
.Ds
.TA 1.5i
.ta 1.5i
add-word /t "[click it]" "{c-t}{c-b}1{c-t}{c-t}{c-b}1{c-t}" invisible
add-word /t "[double click]" "{c-t}{c-b}1{c-t}{c-t}{c-b}1{c-t}{c-t}{c-b}1{c-t}{c-t}{c-b}1{c-t}"
	invisible
.De
will single-click and double-click button 1.
.LP
To release all buttons that are pressed, use the control sequence:
.Ds
{c-t}{c-b}0{c-t}
.De
.LP
For example:
.Ds
add-word /t "[press 1]" "{c-t}{c-b}1{c-t}" invisible
add-word /t "[button up]" "{c-t}{c-b}0{c-t}" invisible
.De
so that you can say "[press\ 1]" to press button 1, move the pointer to a
final position, and then say "[button\ up]" to release the button.
.LP
Modifiers can be set for buttons, for example:
.Ds
add-word /t "[move window]" "{c-t}{c-m}{c-t}{c-b}3{c-t}" invisible
.De
generates meta-button-3.
.NH 1
Pointer Motion
.LP
To warp the pointer to a specific location on a screen, use the control
sequence:
.Ds
{c-t}{c-w}\fI<dest>\fP \fI<x>\fP \fI<y>\fP{c-t}
.De
where \fI<dest>\fP is one of:
.LP
.nf
.TA .5i
.ta .5i
<int>	decimal screen number (starting from 0)
S	current screen
W	top-level window containing the pointer
w	innermost window containing the pointer
.fi
.LP
The position (\fI<x>\fP, \fI<y>\fP) is taken relative to \fI<dest>\fP.
A positive value is relative to the left/top edge, a negative value is
taken relative to the right/bottom edge.  For example:
.Ds
add-word /t "[go to icons]" "{c-t}{c-w}0 478 235{c-t}" invisible
add-word /t "[go upper left]" "{c-t}{c-w}W 0 0{c-t}" invisible
add-word /t "[go lower right]" "{c-t}{c-w}W -0 -0{c-t}" invisible
.De
.LP
Moving to an absolute position is not really very useful.  More useful is to
be able to "remember" some position on the screen, usually relative to some
window, and then go back to it.  The current pointer location can be saved
using the control sequence:
.Ds
{c-t}{c-l}s\fI<digit>\fP{c-t}
.De
where \fI<digit>\fP is a decimal digit (0-9).  This control sequence finds the
client window (a window with a WM_STATE property) the pointer is in, or if the
pointer is not in a client window, the outermost window the pointer is in
(which might be the root window).  It records the pointer position relative to
the origin of the selected window in one of 10 variables selected by
\fI<digit>\fP.  You can then later warp the pointer back to this location with
the control sequence:
.Ds
{c-t}{c-l}w\fI<digit>\fP{c-t}
.De
If the selected window has moved on the screen, the pointer will still warp to
the correct position relative to its new origin.
.LP
Sometimes it is preferable to record the pointer position relative to the
closest edges of the selected window, rather than relative to its origin.
For example, if you want to record a location in the "lower right" corner
of the window, you want to record relative to the closest edges, so that
if you resize the window you will still go to the lower right.  You can
record relative to the closest edges with the control sequence:
.Ds
{c-t}{c-l}S\fI<digit>\fP{c-t}
.De
Example voice macros using these control sequences:
.Ds
add-word /t "[set location]" "{c-t}{c-l}S0{c-t}" invisible
add-word /t "[go back]" "{c-t}{c-l}w0{c-t}" invisible
.De
.LP
Other \fBa2x\fP mechanisms for warping the pointer to specific windows are
described further below.
.LP
To warp the pointer relative to its current position, use the control
sequence:
.Ds
{c-t}{c-d}\fI<dx>\fP \fI<dy>\fP{c-t}
.De
where \fI<dx>\fP and \fI<dy>\fP are decimal numbers (with an optional minus sign) for
the change in x and y.  For example:
.Ds
add-word /t "[nudge down]" "{c-t}{c-d}0 2{c-t}" invisible
add-word /t "[nudge left]" "{c-t}{c-d}-2 0{c-t}" invisible
add-word /t "[nudge right]" "{c-t}{c-d}2 0{c-t}" invisible
add-word /t "[nudge up]" "{c-t}{c-d}0 -2{c-t}" invisible
.De
.LP
More useful is to use relative motion in conjunction with autorepeat to move
the pointer at a continuous rate.  The autorepeat control sequence described
earlier can also be used for pointer motion:
.Ds
{c-t}{c-a}\fI<kdelay>\fP \fI<delta>\fP \fI<pdelay>\fP{c-t}
.De
where \fI<kdelay>\fP is the keyboard autorepeat rate in seconds (as before),
\fI<delta>\fP is a nonnegative integer specifying a new distance, and
\fI<pdelay>\fP is the pointer autorepeat rate in seconds.  Both \fI<kdelay>\fP
and \fI<pdelay>\fP are floating point numbers.  The \fI<kdelay>\fP value is
only used if the event being autorepeated is a keystroke; the \fI<delta>\fP
and \fI<pdelay>\fP values are only used if the event being autorepeated is
pointer motion.  When any value is 0, it means "don't change the value from
what it already is".  If a nonzero \fI<delta>\fP value is specified, it
becomes the new magnitude of motion in both x and y dimensions.  For example,
if the current motion is 5 pixels in the x dimension, and -5 pixels in the y
dimension, and a \fI<delta>\fP of 7 is given, the new motion will be 7 pixels
in the x dimension and -7 pixels in the y dimension.  Note that, although you
can initially specify motion along an angle that is not a multiple of 45
degrees, if you specify \fI<delta>\fP in the autorepeat you will end up with
an angle that is a multiple of 45 degrees.
.LP
Examples of using autorepeat:
.Ds
add-word /t "[go down right]" "{c-t}{c-d}5 5{c-t}{c-t}{c-a}0 0 .1{c-t}" invisible
add-word /t "[go up slow]" "{c-t}{c-d}0 -1{c-t}{c-t}{c-a}0 0 .1{c-t}" invisible
add-word /t "[go left]" "{c-t}{c-d}-5 0{c-t}{c-t}{c-a}0 0 .1{c-t}" invisible
add-word /t "[autorepeat]" "{c-t}{c-a}1 5 .1{c-t}" invisible
add-word /t "[slow down]" "{c-t}{c-a}2 1 0{c-t}" invisible
.De
.LP
The autorepeat sequence is designed to cover both keystrokes and motion so
that you can use a single voice command to govern speed.  For example, you
might say "[search\ for]", "[autorepeat]", then "[slow\ down]" if things are
going by too fast, and finally "[stop\ moving]".  Similarly, you might say
"[go\ left]", then "[slow\ down]" when you get close, and finally
"[stop\ moving]".  Notice that "[slow\ down]" in the example treats keyboard
repeat differently from pointer motion; the repeat rate for the keyboard is
slowed down, but the repeat rate for pointer motion remains the same and the
delta motion is made smaller instead.  This permits finer-grained positioning
of the pointer.
.NH 1
Undo
.LP
The normal mode for using DragonDictate with X is to have DragonDictate's
"keys-immediate" parameter set to 1, so that actions happen immediately
without confirmation.  But, when DragonDictate matches on the wrong utterance,
the wrong action is executed.  Although DragonDictate will generate backspaces
when you use "choose \fI<n>\fP" to make corrections, these backspaces seldom
correctly undo the action that was executed.  There is no perfect solution to
this problem, but \fBa2x\fP has an undo facility that can help in many cases.
.LP
When \fBa2x\fP starts up, it reads an undo file.  By default, it reads the
file ".a2x" from your home directory, but you can supply an alternative file
on the command line using the "-u" option.  The syntax of this file is simple;
each line is of the form:
.Ds
\fI<dd-sequence>\fP:\fI<undo-sequence>\fP
.De
.LP
The \fI<dd-sequence>\fP is the sequence that is being undone; the
\fI<undo-sequence>\fP is the sequence that \fBa2x\fP should generate to "undo"
that sequence.  Each sequence is specified as a string of characters.  Although
control characters can appear directly in the file, it is usually easier to
specify them with '^' followed by a single character; the usual convention
is followed of using the character you get by adding 0x100 to the control
character.  For example, "^T" for {c-t}, "^@" for null, "^I" for tab, "^J" for
linefeed, "^M" for carriage return, "^[" for escape.  The Delete character
can be represented as "^?".  Comments can be inserted in the file as lines
beginning with '!'.
.LP
When \fBa2x\fP receives backspaces, it searches the undo file for the longest
\fI<dd-sequence>\fP that generates that many backspaces, and then processes
the corresponding \fI<undo-sequence>\fP just as if it had come from
DragonDictate.
.LP
As a simple example, the undo entries for emacs-style forward/backward
character motion ({c\-f} and {c\-b}) are:
.Ds
^T^Cb:^T^Cf
^T^Cf:^T^Cb
.De
.LP
Note that these are specified for undoing "{c\-t}{c\-c}b" and "{c\-t}{c\-c}f",
not for {c\-b} and {c\-f}.  This is because {c\-b} and {c\-f} will not have
backspaces generated for them by DragonDictate (refer to the backspace
discussion earlier).
.LP
While you are building up your voice macros for \fBa2x\fP, you will probably
be editing your undo file from time to time.  To tell \fBa2x\fP to reload the
file, use the control sequence:
.Ds
{c-t}{c-u}
.De
.LP
For example:
.Ds
add-word /t "[reload undo]" "{c-t}{c-u}" invisible
.De
.LP
Different versions of DragonDictate generate different numbers of backspaces
you make corrections, and \fBa2x\fP needs to know this to have undo
processing work correctly.  If you always use \fBa2x\fP with just one
version of DragonDictate, make sure you compile it with the correct setting
to indicate that version.  If you use more than one version, then use the
"-v" command line option to specify the version.
.NH 1
Moving Between Windows
.LP
To warp the pointer to a new window, use the control sequence:
.Ds
{c-t}{c-j}\fI<options>\fP{c-t}
.De
or
.Ds
{c-t}{c-j}\fI<options>\fP \fI<mult>\fP{c-t}
.De
where \fI<mult>\fP is a positive floating point number, and \fI<options>\fP
consists of some subset of the following options:
.Ds
Z  (this is optional)
one of {C, D, L, R, U} or {c, d, l, r, u}  (one is required)
O  (this is optional, only useful with {C, D, L, R, U})
one or both of {k, b}  (these are optional)
one of {n, p, N, P}  (this is optional, and must be last)
.De
The options are defined as follows:
.LP
.TA .5i
.ta .5i
Z	no-op
.IP
This letter is a "no-op"; it is provided so that you can place it at the
beginning of the control sequence to "soak up" an uppercase conversion
performed by DragonDictate as a result of a previous utterance.  For example,
you might just have finished dictating a sentence, and then issue a command to
change windows; the first option letter will be converted to uppercase.  This
option is not normally needed if you set the punctuation of your macro to
"invisible".
.LP
.nf
.TA .5i
.ta .5i
C	closest top-level window
D	top-level window going down
U	top-level window going up
L	top-level window going left
R	top-level window going right
O	skip overlapping top-level windows
.fi
.IP
These options permit you to jump up ('U'), down ('D'), right ('R'), left
('L'), or in any direction ('C') to a new top-level window closest (in the
direction of travel) to the current pointer position.  'O' can be used in
conjunction with one of the other options to ignore all windows that overlap
the top-level window the pointer is in.
.IP
For example:
.Ds
add-word /t "[skip down]" "{c-t}{c-j}D{c-t}" invisible
add-word /t "[skip left]" "{c-t}{c-j}L{c-t}" invisible
add-word /t "[way right]" "{c-t}{c-j}RO{c-t}" invisible
add-word /t "[way up]" "{c-t}{c-j}UO{c-t}" invisible
.De
.LP
.nf
.TA .5i
.ta .5i
c	closest widget
d	widget going down
l	widget going left
r	widget going right
u	widget going up
.fi
.IP
These options permit you to jump to a new widget within the current top-level
window that the pointer is in.  The direction of motion is the same as for the
top-level jump options.  For example:
.Ds
add-word /t "[jump close]" "{c-t}{c-j}c{c-t}" invisible
add-word /t "[jump down]" "{c-t}{c-j}d{c-t}" invisible
.De
.IP
The distance between a window and the pointer position is computed by using
the closest visible point of the window.  When computing the distance metric,
\fBa2x\fP employs a scale factor in the "off axis" to skew the computation,
which favors windows farther away but in the desired direction over windows
closer but in the wrong direction.  For example, if you are moving in the "up"
direction, the distance between two points is computed as:
.Ds
square_root((x2 - x1) * (x2 - x1) * multiplier + (y2 - y1) * (y2 - y1))
.De
.IP
The default multiplier is 10.  You can override this default by explicitly
providing a \fI<mult>\fP floating point value in the jump control sequence.
.LP
.nf
.TA .5i
.ta .5i
k	require keyboard interest
b	require button interest
.fi
.IP
It is often useful to restrict the set of windows to those that select for
keyboard and/or button events.  Specifying 'k' requires that the window select
for KeyPress or KeyRelease events.  Specifying 'b' requires that the window
select for ButtonPress or ButtonRelease events.  Specifying both 'k' and 'b'
requires that the window select for KeyPress or KeyRelease or ButtonPress or
ButtonRelease (i.e., "kb" means 'k' or 'b', not 'k' and 'b').  For example,
when jumping between widgets it is usually more useful to require some form of
input:
.Ds
add-word /t "[jump left]" "{c-t}{c-j}lkb{c-t}" invisible
add-word /t "[jump right]" "{c-t}{c-j}rkb{c-t}" invisible
.De
.LP
.nf
.TA 1.5i
.ta 1.5i
n\fI<name>\fP	require WM_NAME name
p\fI<name>\fP	require WM_NAME name prefix
N\fI<name>\fP	require name
N.\fI<class>\fP	require class
N\fI<name>\fP.\fI<class>\fP	require name and class
P\fI<name>\fP	require name prefix
P.\fI<class>\fP	require class prefix
P\fI<name>\fP.\fI<class>\fP	require name prefix and class prefix
.fi
.IP
These options further restrict the set of windows considered to those with a
particular name and/or class.  The 'n' and 'p' options are only useful when
jumping to top-level windows; the 'N' and 'P' options are useful for all
jumps.  If one of these options is used, it must be the last option.
.IP
The 'n' and 'p' options look at the WM_NAME property on the top-level window.
The \fI<name>\fP must match exactly (for 'n') or be a prefix of (for 'p') the
WM_NAME string value for the window to be considered.  The 'N' and 'P' options
look at both the WM_CLASS property of the window and at the _MIT_OBJ_CLASS
property of the window.  Both of these properties contain a name/class pair.
The \fI<name>\fP, if specified, must match exactly (for 'N') or be a prefix of
(for 'P') the name of the window, and the \fI<class>\fP, if specified, must
match exactly (for 'N') or be a prefix of (for 'P') the class of the window,
for a window to be considered.
.IP
In case the \fI<name>\fP you want to specify has a '.' in it, \fBa2x\fP uses
the last '.' as the separator between \fI<name>\fP and \fI<class>\fP, so if
you have no \fI<class>\fP to provide, simply terminate the \fI<name>\fP with
another '.'.  For example:
.Ds
nxterm-x.org.
.De
.IP
The _MIT_OBJ_CLASS property is set by a nonstandard addition to the Xt
Intrinsics.  If the application resource "xtIdentifyWindows" is set to true,
then every widget created will have this property placed on it automatically
by the Intrinsics.  (You need R5 with public patches at least up through 16
for Xt to generate this property; earlier versions of Xt either do not have
this support or generate a WM_CLASS instead, which clobbers the real WM_CLASS
on shell windows.)
.IP
For example:
.Ds
add-word /t "[start jump]" "{c-t}{c-j}ZcP" left right
add-word /t "[start skip]" "{c-t}{c-j}ZCp" left right
add-word /t "[go there]" "{c-t}" invisible
.De
.IP
With these macros, you can say sequences like "[start\ skip]", "emacs",
"[go\ there]", to jump to the closest \fBemacs\fP application window, or
"[start\ jump]", "inbox", "[go\ there]" within an \fBxmh\fP application to
jump to the folder button named "inbox".  Note that it is OK to split a
control sequence across utterances, \fBa2x\fP cannot tell the difference.  If
you make a mistake saying the name, you can use "choose \fI<n>\fP" to make
corrections.
.NH 1
Triggers
.LP
Consider the following:
.Ds
add-word /t "[bad reply to]" "{c-t}F17{c-t}{c-t}{c-j}Cpdrafts:{c-t}" capitalize left right
.De
.LP
Assume that F17 is sent to an \fBxmh\fP application to start a reply to a
message.  It will take \fBxmh\fP some time to create the window, and it will
take additional time for the window manager to decorate the window and make it
visible.  If the jump control sequence is executed too soon, it will fail to
jump to the desired window.  It would be inconvenient to have to guess how
long this will take.  Instead, the \fBa2x\fP trigger mechanism can be used to
delay until the desired window is mapped.
.LP
To establish a trigger for a window mapping, use the control sequence:
.Ds
{c-t}{c-y}M\fI<option>\fP{c-t}
.De
.LP
To establish a trigger for a window unmapping, use the control sequence:
.Ds
{c-t}{c-y}U\fI<option>\fP{c-t}
.De
.LP
In both cases, the available options are:
.LP
.nf
.TA 1.5i
.ta 1.5i
n\fI<name>\fP	require WM_NAME name
p\fI<name>\fP	require WM_NAME name prefix
N\fI<name>\fP	require name
N.\fI<class>\fP	require class
N\fI<name>\fP.\fI<class>\fP	require name and class
P\fI<name>\fP	require name prefix
P.\fI<class>\fP	require class prefix
P\fI<name>\fP.\fI<class>\fP	require name prefix and class prefix
.fi
.LP
These options have the same interpretation as in the jump control sequence.
If no option is supplied, any window that is mapped/unmapped will satisfy the
trigger.
.LP
To establish a trigger for a selection ownership, use the control sequence:
.Ds
{c-t}{c-y}S\fI<selection>\fP{c-t}
.De
where \fI<selection>\fP is the name of the selection (e.g., PRIMARY).
\fBa2x\fP will take ownership of the specified selection; the trigger will be
satisfied when \fBa2x\fP loses ownership (usually when some other client takes
ownership).
.LP
Once a trigger has been set, you can delay until the trigger is satisfied (or
10 seconds elapse) with the control sequence:
.Ds
{c-t}{c-y}W{c-t}
.De
.LP
You can override the default timeout value by instead using the control
sequence:
.Ds
{c-t}{c-y}W \fI<delay>\fP{c-t}
.De
where \fI<delay>\fP is a floating point number in seconds.
.LP
Only one trigger can be outstanding at one time.  You should set the trigger
before you execute an action that will cause the trigger to be satisfied.  For
example:
.Ds
.TA 1.5i
.ta 1.5i
add-word /t "[reply to]" "{c-t}{c-y}Mpdrafts:{c-t}{c-t}F17{c-t}{c-t}{c-y}W{c-t}
	{c-t}{c-j}Cpdrafts:{c-t}" capitalize left right
.De
.LP
As a more complex example, consider searching for the letter '@' in an Athena
Text widget and then selecting text surrounding it.  The search command will
pop up a search dialogue box, and you must wait for the box to appear before
entering the '@'.  After entering the Return key at the box, you must wait for
the box to pop down before generating further commands to the text widget (or
else they would be executed by the search box).  For example:
.Ds
.TA 1.5i
.ta 1.5i
add-word /t "[select address]" "{c-t}{c-y}Mnsearch{c-t}{c-s}{c-t}{c-y}W{c-t}@
	{c-t}{c-y}Unsearch{c-t}{Enter}{c-t}{c-y}W{c-t}{c-t}{c-m}@" invisible
.De
.LP
This sets a map trigger before typing {c\-s}, waits for the map before entering
the '@', sets an unmap trigger before typing Return, then waits for the unmap
before selecting text (assume that {c\-t}{c\-m}@ does this).
.LP
Another example:
.Ds
.TA 1.5i
.ta 1.5i
add-word /t "[bad paste single]" "{c-@}{c-e}{c-t}{c-m}{c-w}{c-t}{c-j}Cpdrafts:{c-t}
	{c-t}{c-m}{c-y}" invisible
.De
.LP
This macro selects text in the current window (assume that {c\-@} sets a mark
at the current text position, {c\-e} moves to the end of the text line, and
{c\-t}{c\-m}{c\-w} selects the text between the mark and the current text
position), then jumps to a top-level window with "drafts:" as a name prefix,
and then pastes the selection into that window (assume that {c\-t}{c\-m}{c\-y}
causes a paste).  Unfortunately, if you try this, you probably discover that
it doesn't work correctly.  The problem is that it takes an application a
while to actually assert a selection in X, and \fBa2x\fP can usually jump to
the new window and cause the next application to ask for the selection faster
than the first application can assert the selection.  The result is that you
will often get the previous selection contents, rather than the expected
contents.  So you need to wait for the application to take ownership of the
selection.  For example:
.Ds
.TA 1.5i
.ta 1.5i
add-word /t "[paste single]" "{c-@}{c-e}{c-t}{c-y}SPRIMARY{c-t}{c-t}{c-m}{c-w}
	{c-t}{c-j}Cpdrafts:{c-t}{c-t}{c-y}W{c-t}{c-t}{c-m}{c-y}" invisible
.De
.NH 1
Delays
.LP
Sometimes the trigger mechanism may not be sufficient for your needs.  You can
also simply delay for a fixed period of time, using the control sequence:
.Ds
{c-t}{c-z}\fI<delay>\fP{c-t}
.De
where \fI<delay>\fP is a floating point number specifying the amount of time
in seconds \fBa2x\fP should wait before generating more events.  For example:
.Ds
add-word /t "[timed click]" "{c-t}{c-b}1{c-t}{c-t}{c-z}.3{c-t}{c-t}{c-b}1{c-t}" invisible
.De
.NH 1
Macros
.LP
There are occasions when you want to repeat some sequence of actions multiple
times, but it isn't worth creating a permanent DragonDictate voice macro
because you are unlikely to need the sequence again.  If all of the actions
occur within one window, and that application supports a macro record/playback
mechanism, you can use that.  But if the actions span multiple windows or
applications, you can use \fBa2x\fP to record and playback sequences.
.LP
To start recording a macro, use the control sequence:
.Ds
{c-t}{c-f}r{c-t}
.De
.LP
For example:
.Ds
add-word /t "[record macro]" "{c-t}{c-f}r{c-t}" invisible
.De
.LP
Then, speak all of the actions you wish to record.  When you have finished,
stop recording and save the macro with the control sequence:
.Ds
{c-t}{c-f}s\fI<digit>\fP{c-t}
.De
where \fI<digit>\fP is a decimal digit (0-9), permitting you to save up to 10
different recorded macros.  For example:
.Ds
add-word /t "[save macro]" "{c-t}{c-f}s0{c-t}" invisible
.De
.LP
To execute a recorded macro, use the control sequence:
.Ds
{c-t}{c-f}e\fI<digit>\fP{c-t}
.De
where \fI<digit>\fP is the number of the macro you want to execute.  For
example:
.Ds
add-word /t "[execute macro]" "{c-t}{c-f}e0{c-t}" invisible
.De
.LP
Once you are through with a macro, you can delete it (to avoid accidental
execution if DragonDictate matches wrong) with the control sequence:
.Ds
{c-t}{c-f}d\fI<digit>\fP{c-t}
.De
.LP
For example:
.Ds
add-word /t "[delete macro]" "{c-t}{c-f}d0{c-t}" invisible
.De
.NH 1
Server Grabs
.LP
If an application grabs exclusive control of the server (using the Xlib
function XGrabServer), \fBa2x\fP might not be able to function; protocol
requests from \fBa2x\fP might be suspended just as other X clients will have
their requests suspended.  Newer versions of the DEC-XTRAP (version 3.2a)
and XTEST (version 2.2) extensions provide a mechanism to make \fBa2x\fP
impervious to server grabs, so this will not be a problem when using a new
enough version of one of these extensions.
.LP
If the extension you are using does not permit \fBa2x\fP to be impervious
to server grabs, this may be a problem, depending on whether the
applications you use grab the server during the time you need to generate
input.  For example, many window managers will grab the server when they
resize or move windows, or perhaps even when displaying menus.  In that case,
you will be unable to use voice control for those operations.  Some
applications may provide options to disable server grabs; for example,
\fBtwm\fP provides the NoGrabServer option to not use server grabs for menus
or opaque window moves, and \fBolwm\fP provides a "ServerGrabs" boolean
resource for menus and notices.
.NH 1
Changing Displays
.LP
If you use more than one display in your work, you can dynamically point
\fBa2x\fP at whatever display you want to interact with, using the control
sequence:
.Ds
{c-t}{c-r}D\fI<display>\fP{c-t}
.De
where \fI<display>\fP is standard display name string or just a host name.  If
the string does not contain a colon (:), ":0" is appended to the string.  The
string can be empty, in which case it is equivalent to ":0".  For example:
.Ds
add-word /t "[start display]" "{c-t}{c-r}D" left right
add-word /t "[go there]" "{c-t}" invisible
.De
permits you to say "[start\ display]", "expo", "[go\ there]" to go to the
display "expo:0".  Note that in this example, the terminating {c\-t} of the
control sequence is in a separate voice macro, and is part of a separate
utterance.  This is fine; \fBa2x\fP does not know where utterance boundaries
are, it simply interprets a stream of characters.  If you make a mistake
saying the display name, you can use "choose \fI<n>\fP" to make corrections.
.NH 1
Miscellaneous
.LP
You can terminate \fBa2x\fP using the control sequence:
.Ds
{c-t}{c-e}
.De
.LP
For example:
.Ds
add-word /t "[dragon quit]" "{c-t}{c-e}{Dcom}dkey {Enter}{Dcom}" invisible
.De
(The "dkey" dcom simply forces the DragonDictate menu to pop down.)
.LP
If \fBa2x\fP seems to be doing strange things, you can dump recent history of
characters received using the control sequence:
.Ds
{c-t}{c-p}
.De
This will print out on your PC screen.
.LP
You can send characters to your PC screen using the control sequence:
.Ds
{c-t}{c-v}\fI<string>\fP{c-t}
.De
For example, to clear the PC screen:
.Ds
add-word /t "[dragon clear]" "{c-t}{c-v}{Esc}[H{Esc}[2J{c-t}{Dcom}dkey {Enter}{Dcom}" invisible
.De
As a special case, if the string is empty a single {c\-t} is output.
.LP
You can generate a {c\-t} with the control sequence:
.Ds
{c-t}{c-t}
.De
or with:
.Ds
{c-t}{c-c}t
.De
.NH 1
Emacs
.LP
You may find the following GNU emacs 18 functions useful for manipulating
selections through the keyboard:
.LP
.Ds
(defun dragon-cut-text (beg end)
  "Copy text between mark and position into window system cut buffer.
Save in Emacs kill ring also."
  (interactive "r")
  (x-store-cut-buffer (buffer-substring beg end))
  (copy-region-as-kill beg end))

(defun dragon-paste-text ()
  "Insert window system cut buffer contents at cursor."
  (interactive)
  (insert (x-get-cut-buffer)))
.De
.LP
For example:
.Ds
(global-set-key "\\e\\C-w" 'dragon-cut-text)
(global-set-key "\\e\\C-y" 'dragon-paste-text)
.De
.NH 1
Athena Text Widget
.LP
If you frequently use programs that use the Athena Text widget, you will
probably find it useful to manipulate selections through the keyboard.
Unfortunately, the Text widget up through R5 does not provide this support.  A
patch for the R5 Text widget is available with the \fBa2x\fP distribution.
With this patch, you can use translations such as:
.LP
.Ds
.TA .5i
.ta .5i
*Text.translations: #override\\n\\
	!:Ctrl<Key>@: select-start()\\n\\
	!:Ctrl<Key>space: select-start()\\n\\
	!:Meta<Key>@: select-word(PRIMARY, CUT_BUFFER0)\\n\\
	!:Ctrl<Key>_: insert-selection(CUT_BUFFER1)\\n\\
	!:Ctrl<Key>w: extend-end(PRIMARY, CUT_BUFFER0)kill-selection() \\n\\
	!:Meta Ctrl<Key>w: extend-end(PRIMARY, CUT_BUFFER0)\\n\\
	!:Meta Ctrl<Key>y: insert-selection(PRIMARY, CUT_BUFFER0)
.De
.NH 1
DESQview/X
.LP
If you have DESQview/X from Quarterdeck Office Systems, it is possible to
export the DragonDictate menus onto your X display.  This means you don't have
to look at another screen to make corrections, but at the cost of dedicating
some screen real estate to a window for menus.  You may also find the menu
display slower going across the network.
.LP
[If you are trying to use DESQview/X version 1.0 with FTP Software's version
2.1, you may have trouble getting it configured.  You need to have IPCUST.SYS
in your CONFIG.SYS for compatibility:
.Ds
DEVICE=C:\\PCTCP\\IPCUST.SYS
.De
and you need to do a one-time "install" step:
.Ds
CD \\PCTCP
IPCONFIG IPCUST.SYS HOSTNAME \fI<pc-hostname>\fP
IPCONFIG IPCUST.SYS USERID \fI<your-userid>\fP
.De
.LP
Reboot, and run DESQview/X's Setup again to configure the network.
Note that this is not necessary with DESQview/X version 1.1.]
.LP
To accomplish this, you start a DOS window on your X display, then start up
DragonDictate in that DOS window.  If you have a version of \fBa2x\fP compiled
to run on your PC, you can then run that \fBa2x\fP in the DOS window;
otherwise, you telnet to your workstation (from within the DOS window) and
then run the \fBDragonDictate\fP script.
.LP
To be able to run DragonDictate in a DOS window, you must permit lots of
Expanded Memory to be used.  Use DVPMAN on \\DVX\\DVPS\\DOS.DVP, and under the
Advanced Options, make the field for "Maximum Expanded Memory Size" blank.
.LP
To open a remote DOS window, you can type:
.Ds
run dos -display \fI<display-spec>\fP
.De
in a local DOS window, or else type:
.Ds
rsh \fI<PC-hostname>\fP dos
.De
from a shell on your X display.  Alternatively, you can create a fancier
DVP to run.  For example, in the \\DVP\\DVPS\\ directory, copy DOS.DVP to
A2X.DVP.  Then use DVPMAN to edit this file.  Change the "Program Name" to
"a2x", and change the "DOS Command" to:
.Ds
a2xini -display \fI<display-spec>\fP
.De
.LP
In the Advanced Options, make the field for "Maximum Expanded Memory Size"
blank, and clear the toggle for "Wait for Window to Open".  Now create a
file A2XINI.BAT, containing something like:
.Ds
CD \\DICTATE\\USERS        \fI(or wherever your voice files are)\fP
CALL DT /s \fI<your-name>\fP
TELNET \fI<workstation-hostname>\fP
.De
if you use telnet, or:
.Ds
CD \\DICTATE\\USERS        \fI(or wherever your voice files are)\fP
CALL DT /s \fI<your-name>\fP
A2X -display \fI<display-spec>\fP
.De
if you are running \fBa2x\fP directly on the PC.  Place this program in the
DESQview/X "Open Window" menu, calling it "a2x", and then you can start up
DragonDictate and connect to your workstation simply by clicking on the menu
entry when DESQview/X starts up.
.LP
Running DragonDictate under DESQview/X is a somewhat fragile setup.  Most
important, once you have started DragonDictate in one DOS window, do not try
to start up DragonDictate again, or your PC will hang.  You will have to
reboot before starting up DragonDictate again.
.LP
DESQview/X looks for the font "pc8x16" to use in the DOS window.  The BDF form
of this font comes with DESQview/X, in \\DVX\\BDF\\.  You may want to use
pc8x14 or pc8x8 instead, to take up less space on your screen.  In that case,
you will want to create an X font alias to map the name pc8x16 to the font you
prefer.
.LP
There is no good way through DESQview/X to control the initial size and
position of the DOS window on your X display.  (Actually, you can control the
initial size by changing the Maximum Width and Maximum Height in the DVP file,
but if you do this DragonDictate's menus will come out scrambled.)  For this
reason, \fBa2x\fP has command line options to permit you to reconfigure the
DOS window:
.Ds
-w \fI<DOS-window-name>\fP -g \fI<geometry-spec>\fP
.De
.LP
The \fI<DOS-window-name>\fP must be the WM_NAME of the DOS window (usually
displayed by the window manager in the title bar).  For example, if you opened
a standard DOS window, the name would be "DOS Window".  If you created an
A2X.DVP and assigned it the Program Name "a2x", then the name will be "a2x".
You want this name to be unique across all windows on your display.  The
\fI<geometry-spec>\fP should be a complete geometry specification, with x, y,
width and height all specified; the width and height are numbers of
characters.  To contain full DragonDictate dialogue boxes, you will need a
width of at least 56 and a height of at least 19.
.LP
The DOS window will not display a text cursor unless it is explicitly given
the input focus (just moving the pointer into the window is not good enough).
This makes it a bit difficult to tell when DragonDictate has recognized
"[begin\ spell\ mode]", or to see where you are when editing keystrokes of
voice macros.  To work around this \fBa2x\fP will, by default, send a
synthetic FocusIn event to the DOS window (if you have specified one with the
-w option), causing the DOS window to think it has the input focus.  If you do
not want this behavior, specify -f on the command line.
.LP
You will need a key on your X keyboard to act as the DragonDictate HOTKEY.  By
default, DragonDictate uses the keypad '+' key, which is the X keysym KP_Add.
Make sure you have a key with the appropriate keysym.  You may find it
inconvenient that the HOTKEY only works when the input focus is in the DOS
window; specifically, it is difficult to quickly turn off the microphone.  For
convenience, you can tell \fBa2x\fP to grab the HOTKEY away from all other
applications, so that it always functions as the HOTKEY regardless where the
input focus is.  To do this, specify the command line options:
.Ds
-w \fI<DOS-window-name>\fP -h \fI<HOTKEY-keysym>\fP
.De
.LP
If you are using the "DragonDictate" script, you can pass command line options
to \fBa2x\fP by providing them as options to the script, or you can preset the
A2XOPTS environment variable to the options you want.  For example:
.Ds
setenv A2XOPTS "-w a2x -h KP_Add -g 58x18+1+1"
.De
.LP
With \fBa2x\fP running in a DOS window on your X screen, you need to be
careful about speaking and typing when the input focus is in the DOS window
and no DragonDictate menu or dialogue box is up.  Any character you type will
be sent by DragonDictate to \fBa2x\fP, \fBa2x\fP will generate an event that
is received by the DOS window, DragonDictate will get the character again, and
send it on to \fBa2x\fP again, and the character will continue to infinite
loop until you move the input focus out of the DOS window.  Fortunately,
nothing really bad happens, you just drive the load average up on your
machine.  If you speak when menus are down and the input focus is in the DOS
window, the characters generated by \fBa2x\fP will be directed back to the DOS
window, and DragonDictate will enter them as corrections to the word choice.
.NH 1
Command line options
.LP
The command line options for \fBa2x\fP are:
.LP
.B \-a
.IP
Causes \fBa2x\fP to use "Alt" instead of "Meta".  That is, the most
significant bit in a character from the PC is interpreted as Alt
instead of Meta, and {c\-t}{c\-m} turns on Alt instead of Meta.
.LP
.B \-b
.IP
Prevents \fBa2x\fP from translating Backspace characters into Delete
characters on output to the X server.
.LP
.B \-B
.IP
If your DOS telnet program sends Delete when you type {Backspace} and
Backspace when you type {Del}, then use this option so that \fBa2x\fP knows.
.LP
.B \-bs
.IP
Causes \fBa2x\fP to translate Backspace characters on output into
a key labeled with keysym "BackSpace", rather than to {c\-h}.
.LP
.B \-c
.IP
Clears the screen at startup time (using the standard ANSI escape
sequence).
.LP
.B \-C \fI<char>\fP
.IP
Changes the control sequence character from {c\-t} to {c\-\fI<char>\fP}.
This option really only exists for people using \fBa2x\fP to attach an
alternative keyboard to X (not using DragonDictate) who find that
they frequently use {c\-t} as a normal control character.  This option
lets you specify a less frequently used control character as the control
sequence character.
.LP
.B \-d \fIdisplay\fP
.IP
Specifies the X server to connect to.
.LP
.B \-e
.IP
Turns on echoing of all characters typed to standard output; this is
really only useful for debugging.
.LP
.B \-E
.IP
Turns on fake echoing.  At least one DOS telnet program (PC-NFS Advanced
Telnet) transmits characters very slowly unless \fBa2x\fP echoes something
back each time it receives characters.  This option causes \fBa2x\fP to send
back a rubout character each time it reads characters.
.LP
.B \-f
.IP
Specifies that the DOS window should not receive synthetic focus.
.LP
.B \-g \fI<geometry-spec>\fP
.IP
Specifies the geometry of the DOS window when using DESQview/X.
.LP
.B \-h \fI<HOTKEY-keysym>\fP
.IP
Specifies the hotkey for use with DESQview/X.
.LP
.B \-p
.IP
Some versions of the XTestExtension1 do not handle relative pointer motion
correctly; if you physically move the mouse and then try to move the mouse
with \fBa2x\fP, the pointer will move to the wrong position.  Use this option
to work around this problem.
.LP
.B \-u \fI<undo-file>\fP
.IP
Specifies the filename of the undo file.
.LP
.B \-U
.IP
Turns off undo interpretation of Backspace by \fBa2x\fP; the character
will be passed through as a normal character.  This option really only
exists for people using \fBa2x\fP to attach an alternative keyboard to X
(not using DragonDictate).  You will probably also want to enable the
\fB\-b\fP option.
.LP
.B \-v \fI<version>\fP
.IP
Specifies the version of DragonDictate being used, for example,
"1.01", "1.01a", or "2.0".  Different versions of DragonDictate generate
backspaces differently when you make corrections, and \fBa2x\fP needs
to know this to have undo processing work correctly.
.LP
.B \-w \fI<DOS-window-name>\fP
.IP
Specifies the WM_NAME of the DOS window when using DESQview/X.
