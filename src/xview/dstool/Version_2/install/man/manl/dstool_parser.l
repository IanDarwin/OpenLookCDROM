.TH DSTOOL_PARSER l "9 September 1992"

.SH NAME
dstool_parser
\- user panel for textual entry of dynamical systems.
.SH SYNOPSIS
.SS Models Interface:
extern int parserwin_open();
.SS Models DS\_DataS Entry
/* Category 3 - Parser */

{ 3, "Parsed dynamical system...", parserwin_open}
.SS Library Name:
parserwinlib.a
.SH DESCRIPTION
The 
.B dstool_parser
is a contributed user panel optionally installed in 
.B dstool. 
For more information on this panel, please see 
the latex documentation in $DSTOOL/contrib/parser/doc.

This panel provides a text interface to dstool for inputting dynamical
systems. The text is parsed allowing the creation of new dynamical
systems without recompilation, but with a cost in performance.
This is especially useful for experimentation and classroom
exercises.

The panel can produce C code from the textual dynamical
system which may be compiled into dstool if greater
efficiency in computation is needed for more
substantial explorations.

.SH USAGE
The
.B dstool_parser
is opened by selecting the Parsed dynamical system in the Parser category
of the Models menu on the Command Window.  The panel appears containing
text describing the Lorenz system.
.SS Panel Items

.B Name read-write text field
\- This field is for giving a name
to the dynamical system which is used in the C code procedure
names.  This should be a single word of no more than 30 characters.

.B Type exclusive setting
\- This field defines whether the dynamical
system should be interpreted as a vector field or a mapping.

.B Build command button
\- Selecting this button will convert the
text from the scrolling text window into a dynamical
system and then load this as a new model into dstool.
Any errors encountered while parsing will be sent to the
parent window and the previous dynamical system will remain active.

.B Write C code window button
\- Selecting this button will open
the Dynamical System Definition Editor window.  The text from 
the scrolling text window will be converted into C code in
dstool's model format.

.B Scrolling text field
\- This portion of the window contains the text that
the user types to define a dynamical system.  This
is a standard XView text window widget and uses the 
standard keyboard and mouse commands.  Use the Menu (usually, Right) 
button on the mouse for a selection of various standard editing features.

.B Dynamical System Definition Editor
\- This window is created by selecting
the Write C code button on the Dynamical System Parser window.  It
contains C code which attempts to define in dstool format the dynamical system
entered by the user in the Dynamical System Parser scrolling text field.
This is a standard XView text window widget so the code may be modified
by the user and then saved as a file suitable for compilation into dstool.
.SS Basic Functionality
Components of vector fields or mappings are entered following a format: 
.TP 4
   x' = sigma ( y - x)
.RE

Variable and parameter names are alphanumeric strings 
beginning with an alphabetic character.

.I Auxiliary functions 
for dstool
may be declared by defining them 
.I after
the dynamical system equations. They should have the 
same format as the dynamical system, but do not use 
the prime after the name of the auxiliary function.

In order to improve evaluation efficiency, it is often useful
to define 
.I temporary variables 
to be used in the definition of
vector fields or auxiliary functions.  This can be done by
defining a function 
.I before
defining the dynamical system.

Any string not defined as a temporary variable, 
auxiliary function, or with a prime is understood 
to be a 
.I parameter 
in the system. 

Comments are allowed at the end of any line and must 
begin with the pound character, #. 

Multiplication is 
understood when no arithmetic operator is present, but 
it may be explicitly indicated with an asterisk, *.
The parser understands the following fundamental constants, 
operators, and special functions:
.RS

  constants: PI, pi, E

  operators: +, -, *, /, % (mod), ^

  special functions: sin, cos, tan, asin, acos, atan, sinh, 
cosh, tanh, log, ln, exp, abs, sqrt
.RE

Initial conditions for the parameters and variables and their default
ranges may optionally be declared using the 
.I INITIAL 
and 
.I RANGE 
commands.  
These specify specific initial values for the parameters and variables 
and the default ranges for display purposes.  Phase space variables 
may be declared periodic on a fixed interval by using the 
.I PERIODIC 
command. For example the lines:

.RS

  INITIAL x 3 y 4 u 5

  PERIODIC x 0 10

  RANGE y -10 10
.RE

would declare x to be a periodic variable with fundamental domain
[0,10] and initial value 3. We have also declared u to have an
initial value of 5 and y to have an initial value of 4 with
default plotting range [-10,10].

Default values are chosen for initial values, 
periods, and plotting ranges not specified by the user.



.SH AUTHOR
P. A. Worfolk

Center for Applied Mathematics, Cornell University

Ithaca, New York, 14853

.I paw@macomb.tn.cornell.edu
.SH FILES
$DSTOOL/contrib/parserwin
.SH SEE ALSO
.B dstool(1)

$DSTOOL/contrib/parserwin/doc
.SH BUGS
The conversion to C code is weak.  Essentially, the input
must be written in C code format, with or without the 
multiplication operator.  The exponentiation operator, ^, 
will be translated directly, but this is not a valid C 
command.  Parentheses need to be included with
the special operators for a correct translation to C.

Ranges for auxiliary functions may not be set.

The independent variable name, initial value, and 
default range cannot be set.  Nonautonomous vector fields 
are not allowed; they must be embedded in autonomous ones by 
augmenting the phase space.

The same temporary functions are used for the 
definition of the dynamical
system and the auxiliary functions.

Please send mail on other bugs to 
.I dstool_bugs@macomb.tn.cornell.edu.

