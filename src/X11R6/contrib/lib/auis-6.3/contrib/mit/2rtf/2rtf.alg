\begindata{text,977308}
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


2rtf

Jeremy Paul Kirby, MIT Project Athena

}

* Usage:  2rtf [ezfile [rtffile]] [-t transtable] [-e stderrfile]


* Description of and technical notes about the translation table file:


A properly constructed translation table file (hereinafter referred to as a 
"transfile") is composed of ASCII text (in the purist sense of ASCII, without 
the 8th bit set).  A well formed transfile consists of lines with two strings 
per line, the first string known as the ez command (sometimes referred to as 
the ".ez" entry) and the second string known as the rtf command (also the 
".rtf" entry).  The parser for this file is implemented with scanf, which will 
blindly read pairs of strings, interpreting the first as the .ez entry and the 
second as the .rtf entry.


To provide 2rtf with the ability to translate an ez style keyword, the 
command, stripped of the command delimiter character, '\\', should be placed 
first on the line and either replacement text or an 2rtf command second on the 
line.   Example, to cause 2rtf to convert:


 	\\bold\{boldtext\}


to


	\\b\{boldtext\}


the proper transtable line would be:


	.ez		.rtf

	================================	

	bold		b


To cause 2rtf to replace occurences of "\\s" to 's' using the 2rtf #copy 
command, the following line should be implemented:


	.ez		.rtf

	================================

	s		#copy


2rtf commands are hard-coded into the program and follow a simple format.  The 
first chaeracter (the "mode flag") must be one of the following:


	'@' or '#'


The '@' mode flag indicates that the command in the .ez field is accompanied 
by parameters (by and large the most popular of all ez functions), and that 
2rtf should search for parameters and handle them accordingly.


The '#' mode flag indicates that the command in the .ez field is a "naked" 
command, not accompanied by parameters, and should not have empty rtf 
delimiters appended.


Reference strings are entries in the transfile prefixed by '~'; they are never 
added to the internal translation table.  The method of identification for 
these strings is hard-coded into the program and thus is not changeable. 
 Currently the following reference string is required for 2rtf to function:


	Internal Variable	Transfile ID

	================================================

	TextDSVersion		~TextDSVersion





ALGORITHM:

----------


2RTF:_MAIN

\{

I.   Look at the command line and set valid options as filled.  Complain if 
there are _any_ bad options and stop.


II.  Obtain unfilled necessary paramaters (necessary parameters are those 
excluding the stderrfile spec--stderr will thus not be rerouted unless 
specifically commanded to do so in the command line).


III. Read translation table into linear linked list and create reference 
strings.  Complain and exit there are any reference strings missing or if 
there are any bad attempts at reference strings.


IV.  Absord all white space.  


V.  Is next text "\\begindata\{"?

    A.  Yes: set ezflag to "yes."

    B.  No: set ezflag to "no."    


VI.  Parse text, reporting status every now and then:

     A.  Read a character from ezfile.

     B.  Is it '\\'?

	 1.  Yes:  Is next character an ez quoted backslash char?

	     a.  Yes:  Write backslash and character.

	     b.  No.

		 i.   (_FINDINSTRUCTION).

		 ii.  Is instruction in table?

		      A) Yes:  Process and execute (_EXECUTE).

		      B) No:  complain and insert directly.

	 2.  No:  Write character.

     C.  Is it a linefeed?

	 1.  If ezflag is no, insert "\\par".

	 2.  If ezflag is yes, absorb and count all linefeeds, inserting n-1 
occurences of "\\par"'s.

     D.  Go to step 'A' until EOF is reached.


VII.   Close files and report final status.

\}


\enddata{text,977308}
