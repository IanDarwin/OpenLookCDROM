\begindata{text, 268823536}
\textdsversion{12}
\template{default}
There are two lists of marks from each simpletext.  The first is markList and 
is used by Ness for marks in variables.  The second is from fence and is for 
all marks on the stack.  The first element of this list is the fence mark and 
is never changed or modified by Ness.


NOTA BENE:  The above note describes a truly awful design.  XXX



Constants


Elimination of the IsAConst bit:  Simplestrings are deleted when the last mark 
to them is removed.   This is detected by checking their markList and 
fence->next fields.  The check is made in 


Constants referenced from the program are in the SysMark array, the same place 
where gotos are stored.


Since constants are always referenced from the program, they go away only when 
the program is revised.  We defer worrying about deleting them until we revise 
programs.




cannot compile while executing because

	return addresses in stack are absolute values

	the gap must not be allowed to get into the text stream



The store operation ASSUMES that the destination is a valid marker and must be 
unlinked.  Therefore the compiler must ensure that every marker variable is 
initialized to some valid value.




Stack Frame Structure


The stack grows from high end of memory downward so that the header word of a 
mark is at the "top" end of the stack.  In discussion, "up" is still 
considered to be toward the active push/pop end of the stack. 


The frame is adjusted by the operations ENTER, EXIT, EXITRET which bound a 
function.


A stack frame has four areas: temporaries, frame record, local variables, and 
arguments. 

	Temporaries are pushed and popped during execution of the function.

	The Frame record has a pointer to the prior frame record 	

		and the function return address

	The locals are variables used during the function.  They are initialized to

		zero during ENTER.

	The arguments are stored originally by the caller.  

	 

The return value is copied to the place of the first argument by the EXIT 
operation.


ENTER has as its argument the number of bytes of local values required.  It 
clears them all to zero.

EXIT has as its argument the number of bytes of arguments plus locals. It pops 
them all and pushes the final temporary value as the result.


Arguments and locals are addressed via positive offsets from FramePtr, which 
points at the current frame record.  (Points to its lower word.  The highest 
local is at address  0;  the size of the frame record is taken care of in the 
interpreter.)


Functions always return values.





Eventually we need to keep references to undefined procs and vars in the 
compiled code and resolve them at runtime.  The functions might be references 
to proctable functions which will not be loaded until later.  I suppose the 
code needs to refer to symbol table entries (?)  Then all arguments should be 
4 bytes?  Or keep the undefined item as a reference into a table of undefined 
values ????  Use value 0 to indicate undefined and scan the fixup table to see 
which undefined symbol is required at this point?


For now we assume fixups will be resolved prior to execution.  And will 
require recompilation to fixup.




Types:

types are checked at runtime.  the grammar has room however for the 
declaration of types of good code is desirable.


The type for a function name is Tfunc.  A node can be associated with the 
symbol to give the type of the returned value.





Changes to Make


allow full document stuff in strings in ness scripts

error indication in source code

all stack elts be two words

use the builtin alloc/dealloc of marks

add integer, boolean, pointer, real types

symbolElt be a subclass of node

use classes more

revise grammar to allow parenthesized expressions

match parens \{\} [] () 

extend lex to have a C mode

endif, etc

event handling/initiation

symbols should be nodes

plain nodes should have fewer fields 


view__DefaultObject so ESC-TAB can ask for view

	can edit to change object

many more proctable items

able to send mouse hits and keystrokes

argument types for proctable entries

call methods: check types

file of buttons,etc to copy and insert

default arbiter at outer level

document as a directory   filename.d/\{filename.dsk, pic1.raster, ...\}

security

	systemexecute(string)

	methods of typescript

	writing files





\bold{Environment for Ness}


A Ness script executes in an environment with an arbiter to provide a name 
space and a textview to provide a default text for manipulation.  The client 
of the Ness has two calls tocontrol its behavior:


	ness_SetDefaultText(nessDataObject, someTextview)

		Sets the default text of the ness to the given value.


	ness_SetAnArbiterView(nessDataObject, someViewUnderAnArbiter)

		Sets the arbiter for the ness to be the one "above" the

		second argument.


An extensive scheme of providing default values for these exists:


1) The Escape-X executor uses the parent textview passed in the call from the 
Escape-X handler as both the default text and the arbiter view.


2) The first time a nessview tries to execute on a particular ness object, it 
will check to see if the default text and arbiter have been set.  If not, the 
default for the text is whatever parent of the nessview happens to be a 
textview, if any.  If there is no arbiter, its default value is the default 
text, which may have been set by the preceeding sentence.


3) If the default text is still NULL when the ness itself begins to execute 
the text, it looks for an arbiter child with the name "defaulttext", if any.


nessview_SetDataObject has been checked to ensure that it can be called at any 
time to switch the nessview among any of a collection of nesses.



Timing


Timing 30 June

Compile mc.mc with mc compiled using em

	23.78

	23.44

	23.26

	23.35


compile using nesscomp

	2.05

	2.26

	1.72

	1.68

	1.71


execute via interpreter	(output surpressed)

	17.97

	17.69

	17.35

	17.57


11 July timings

after adding APPEND operation to speed up ~ and ~:=

also aded writefile, so output is put in file

and using nodeclasses instead of nodes for compiletime data

nesscomp compile

	1.302

	1.314

	1.342

	1.363

execute (with program and constants written to two files)

	17.209

	16.158

	17.056

	16.450

11 July timings with output surpressed

	comp	exec

	1.258	15.196

	1.254	15.369

	1.275	15.693

	(I don't see why these compile times should be shorter, although

	the two writefile output statements are not compiled, so there is a little 
less to do)




'node's via nodeclass


The nodeclass system provides creation and destruction of arbitrary nodes with 
a syntax identical to the class system, but with minimal overhead.  The latter 
is possible because there are no methods associated with nodes.


Declaring a nodeclass:


In a header file named filename.hn (or .Hn) put one or more nodeclass 
definitions.  Each nodeclass definition is in the form of a structure 
declaration with the initial word 'struct' replaced by 'nodeclass'.  For 
example, a list of integers might be


	nodeclass intlist \{

		long val;

		struct intlist *next;

	\};


Note the final semicolon;  it is required.  Note that a pointer to nodeclass 
"classname" is declared to be (struct classname *).  


After declaring a nodeclass, you can declare a value to be of the class with a 
struct declaration:

	struct intlist *mylist;

The fields of the nodeclass are referenced with standard field access: 
 mylist->val, mylist->next.  (It may someday be desirable to generate Get/Set 
routines instead.  It will be easy.)


Three class-procedures are defined for each nodeclass:  classname_New, 
classname_Destroy, and classname_Create.   The first two of these are the same 
as the routines of the same name as defined by the class system: 
 intlist_New() returns a pointer to a brand new instance of an intlist node; 
 intlist_Destroy(nodeptr) deallocates the storage devoted to the intlist 
instance pointed at by nodeptr.  


classname_Create(arg1, arg2, ...) is a delightfully easy way to create new 
nodeclass values when the number of fields is small.  The arguments consist of 
a list of the values to be stored in the fields.  For example, 
 intlist_Create(3, intlist_Create(2, NULL)) returns a pointer to a list of two 
integers: 3 and 2.  Warning: This feature is probably anathema to purists;  a 
user must remember the fields in the struct and their order.  At least if you 
get the wrong number of fields the macro preprocessor complains.  If you have 
a type mismatch, the hc compiler will complain about one of the assignments 
generated as part of the _Create macro expansion.


For use in source code, the .hn (or .Hn) file must be converted to a .h (or 
.H) file.  This is done by running the nodeclass converter:


	nessrun nodeclass.n filename.hn


The named file will be processed to create a file with the same name, but 
missing the trailing 'n'.  Approriate generic Makefile rules might be


.SUFFIXES: .h .hn .H .Hn

.hn.h:

	nessrun nodeclass.n $*.hn

.Hn.H:

	nessrun nodeclass.n $*.Hn


A file that uses a .hn file should include the .h file that results from 
processing.  It should also include nodeclass.h, which contains a declaration 
of a secret global variable (QTnode) used for _Create functions.


At present there is no notion of inheritance.  It could be added if desired. 
 There is also no notion of garbage collection;  this is harder to add, but 
easier than if the nodeclass system is not used.





yylval

Somewhere in a linked (or makedo'd) module there must be a declaration of 
yylval which does not declare it 'extern'.  

It is so declared in interptest.c because this program uses yyerror.c which 
calls lex_tokloc, which causes a load of lex.

It is so declared in nesscomp.\{gra,y,c\} because it is needed for lex.

It is also so declared in ../libcomp/lextest.c, which calls lex.






Data Structures for Scripts


A ness has pointers to 

	the script

	the list of attributes

	a list of constants

	the symbol table partition


A script has an outer list of four kinds of items:


	global variable declarations, 

	global function declarations,

	global event names,

	extensions to individual objects


An extension to an individual object may also be one of four kinds of items; 
 any of the first three above, and also an event specification.  


Each of these items is represented as a symbol in the private class 
\bold{nesssym} which is a subclass of sym.  The nesssym has in addition the 
following fields:

	next - the next element in the list

	kind - which kind of item and whether global or local

	parent - if global, the ness;  if local to extension, the extension list sym; 





			if local to a func, the func

	loc, len - the location of the declaration in the source

	type - the type of a variable or the returned type of a function

	info - info specific to a kind.  one of

		struct funcnode *

		long sysinx 

		long stack offset



nodeclass funcnode \{

	struct sym *locals;

	struct objmapnode *objmap;		/* (1st elt is for entire func) */

\}

/* the sym's pointed to by funcnode->locals have as their info a pointer to a 
localnode: */


info values are not specified for

	event name

	event spec

	extension list

the sym for an event name is the event name

the sym for an extension list is the name of the object

the sym for an event spec is unspecified


The lexical analyzer is specific to the application.  It returns whatever type 
of symbol node is required.  For the ness compiler, only the loc and len 
fields of the nesssym are filled in, and only the first time the symbol is 
encountered.  (The compiler may change these values to include the entire 
declaration.)

	



info field and kind field


kinds of sym values


special char, thong, emptystring, eof

	toksym  no loc/len  no info


intcon

	nesssym  loc/len of 1st appearance  info.intval


realcon

	nesssym  loc/len of 1st appearance  info.realval->double


strcon

	nesssym   loc/len of 1st appearance    no info


reserved word

	toksym  no loc/len or info


builtin

	




Error Correction

The rules for expr, stmt, and attribute have error handling builtin.  In each 
case, there is an error rule for each symbol which can follow the given 
non-terminal.  The rule clears the error state in the parser (yyerrok), sets 
the parser to read another token (yyclearin), and tells the lexer to reread 
the token following the non-terminal (lex_Repeat(0)).




symbols - names


The name field in nesssym's has a unique first character to distinguish the 
symbol:


&name	- an extended object name

\\ event ddd  -  an event specification

'x	- character constant

"string	- short string constant (\\ reduced)

/ddd	- long string constant

0xdddd	- numeric constant (8 digits for real, 4 for integer)

lxxxx	- function or variable name (l is a letter)

X:ddd	- replacement for previously defined declared symbol

\enddata{text,268823536}
