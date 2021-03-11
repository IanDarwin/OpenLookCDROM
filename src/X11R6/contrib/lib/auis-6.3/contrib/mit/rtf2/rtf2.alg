\begindata{text,268856672}
\textdsversion{12}
\template{scribe}
\define{global

attr:[LeftMargin LeftEdge Int 16]
attr:[Indent LeftMargin Int -16]
attr:[Justification LeftJustified Point 0]
attr:[Flags ContinueIndent Int Set]
attr:[FontFace FixedFace Int Set]
attr:[FontFamily AndyType Int 0]
attr:[FontSize ConstantFontSize Point 10]}
\center{PROGRAMMER'S NOTES


rtf2

Jeremy Paul Kirby, MIT Project Athena

}

* Usage:  rtf2 [rtffile [ezfile]] [-t transtable] [-e stderrfile]


* Description of and technical notes about the translation table file:


A properly constructed translation table file (hereinafter referred to as a 
"transfile") is composed of ASCII text (in the purist sense of ASCII, without 
the 8th bit set).  A well formed transfile consists of lines with two strings 
per line, the first string known as the rtf command (sometimes referred to as 
the ".rtf" entry) and the second string known as the ez command (also the 
".ez" entry).  The parser for this file is implemented with scanf() is 
implemented in a loop, and will blindly read pairs of strings, interpreting 
the first as the .rtf entry and the second as the .ez entry.


To provide rtf2 with the ability to translate an rtf command, the command, 
stripped of the command character, '\\', should be placed first on the line 
and either replacement text or an rtf2 command second on the line.   Example, 
to cause rtf2 to convert:


 	\{\\b boldtext\}


to


	\\bold\{boldtext\}


the proper transtable line would be:


	.rtf		.ez

	================================	

	b		bold


To cause rtf2 to replace occurences of "\\s" to 's' using the rtf2 #copy 
command, the following line should be implemented:


	.rtf		.ez

	================================

	s		#copy


Rtf2 commands are hard-coded into the program and follow a simple format.  The 
first chaeracter (the "mode flag") must be one of the following:


	'@', '#', or '$'


The '@' mode flag indicates that the command in the .rtf field is accompanied 
by parameters (by and large the most popular of all rtf functions), and that 
rtf2 should search for parameters and handle them accordingly.


The '#' mode flag indicates that the command in the .rtf field is a "naked" 
command, not accompanied by parameters (for example, the rtf \\pard command), 
and should not have empty ez delimiters appended.


The '$' mode flag indicates that the command in the .rtf field is a naked 
command and should have empty ez delimiters appended (a very unusual 
situation, but one provided for nonetheless).


Reference strings are entries in the transfile prefixed by '~'; they are never 
added to the internal translation table.  The method of identification for 
these strings is hard-coded into the program and thus is not changeable. 
 Currently the following reference strings are required for rtf2 to function:


	Internal Variable	Transfile ID

	================================================

	RTFchars		~RTFchars

	TextDSVersion		~TextDSVersion

	RTFverceiling		~RTFverceiling



ALGORITHM:

----------


RTF2:_MAIN

\{

I.   Look at the command line and set valid options as filled.  Complain if 
there are _any_ bad options and stop.


II.  Obtain unfilled necessary paramaters (necessary parameters are those 
excluding the stderrfile spec--stderr will thus not be rerouted unless 
specifically commanded to do so in the command line).


III. Read translation table into linear linked list and create reference 
strings.  Complain and exit there are any reference strings missing or if 
there are any bad attempts at reference strings.

IV.  Ignore all characters until a left brace ('\\\{') is found.  If one is 
not, declare the input file to not be RTF and exit.

V.  Ignore all characters until \\rtf\italic{n} is found.  Compare \italic{n}, 
the version number of RTF used in the file, to the highest version acceptable 
(RTFverceiling).  If unacceptable, declare the input file to not be of a 
suitable form of RTF and exit.


VI.  Parse text, reporting status every now and then:

     A.  Read a character from rtffile.

     B.  Is it '\\'?

	 1.  Yes:  Is next character an rtf special character?

	     a.  Yes:  (_EXECUTESPECIAL)

	     b.  No.

		 i.   Is it an rtf duplication character?

		      A)  Yes:  Write '\\' and character.

		      B)  No:  

			  1) (_FINDINSTRUCTION).

			  2) Is instruction in table?

			     a.  Yes:  Process and execute (_EXECUTE).

			     b.  No:  complain and insert directly.

	 2.  No: Write character.

     C.  Go to step 'A' until EOF is reached.


VII.   Close files and report final status.

\}



Note, additionally, that there are some commands that must be issued at 
certain places in an RTF file.  Namely, defining the character set must occur 
before any document text or tables occur.  Font tables, the master style 
definitions, screen colors, summary information, and document formatting 
commands must also precede any document text. Font tables must precede the 
style definitions.  To facilitate this, the TextFound and StylesFound internal 
variables are defined as toggles.


\enddata{text,268856672}
