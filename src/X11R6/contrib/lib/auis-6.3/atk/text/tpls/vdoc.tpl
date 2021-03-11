\begindata{text, 268544256}
\textdsversion{12}
\template{default}
\define{italic
menu:[Font,Italic]
attr:[FontFace Italic Int Set]}
\define{bold
menu:[Font,Bold]
attr:[FontFace Bold Int Set]}
\define{chapter
menu:[Title,Chapter]
attr:[Flags KeepPriorNL Int Set]
attr:[Flags KeepNextNL Int Set]
attr:[Justification Centered Point 0]
attr:[FontFace Bold Int Set]
attr:[FontSize PreviousFontSize Point 4]}
\define{section
menu:[Title,Section]
attr:[Flags KeepPriorNL Int Set]
attr:[Flags KeepNextNL Int Set]
attr:[Justification Centered Point 0]
attr:[FontFace Bold Int Set]
attr:[FontSize PreviousFontSize Point 4]}
\define{subsection
menu:[Title,Subsection]
attr:[Flags KeepPriorNL Int Set]
attr:[Flags KeepNextNL Int Set]
attr:[Justification LeftJustified Point 0]
attr:[FontSize PreviousFontSize Point 4]}
\define{paragraph
menu:[Title,Paragraph]
attr:[Flags KeepPriorNL Int Set]
attr:[Justification LeftJustified Point 0]
attr:[FontFace Italic Int Set]
attr:[FontSize PreviousFontSize Point 4]}
\define{bigger
menu:[Font,Bigger]
attr:[FontSize PreviousFontSize Point 2]}
\define{indent
menu:[Region,Indent]
attr:[Flags KeepPriorNL Int Set]
attr:[Flags KeepNextNL Int Set]
attr:[LeftMargin LeftMargin Inch 32768]
attr:[RightMargin RightMargin Inch 32768]}
\define{typewriter
menu:[Font,Typewriter]
attr:[FontFace FixedFace Int Set]
attr:[FontFamily AndyType Int 0]}
\define{display
menu:[Region,Display]
attr:[Flags KeepPriorNL Int Set]
attr:[Flags KeepNextNL Int Set]
attr:[LeftMargin LeftMargin Inch 32768]
attr:[RightMargin RightMargin Inch 32768]
attr:[Justification LeftJustified Point 0]}
\define{example
menu:[Region,Example]
attr:[Flags NoFill Int Set]
attr:[Flags KeepPriorNL Int Set]
attr:[Flags KeepNextNL Int Set]
attr:[LeftMargin LeftMargin Inch 32768]
attr:[Justification LeftJustified Point 0]
attr:[FontFace FixedFace Int Set]
attr:[FontFamily AndyType Int 0]}
\define{description
menu:[Region,Description]
attr:[Flags KeepPriorNL Int Set]
attr:[Flags KeepNextNL Int Set]
attr:[LeftMargin LeftMargin Inch 32768]
attr:[Indent LeftMargin Inch -32768]}
\define{quotation
menu:[Region,Quotation]
attr:[Flags KeepPriorNL Int Set]
attr:[Flags KeepNextNL Int Set]
attr:[LeftMargin LeftMargin Inch 32768]
attr:[RightMargin RightMargin Inch 32768]
attr:[FontFace Italic Int Set]}
\define{subscript
menu:[Font,Subscript]
attr:[Script PreviousScriptMovement Point 2]
attr:[FontSize PreviousFontSize Point -2]}
\define{superscript
menu:[Font,Superscript]
attr:[Script PreviousScriptMovement Point -6]
attr:[FontSize PreviousFontSize Point -2]}
\define{smaller
menu:[Font,Smaller]
attr:[FontSize PreviousFontSize Point -2]}
\define{heading
menu:[Title,Heading]
attr:[Flags KeepPriorNL Int Set]
attr:[Flags KeepNextNL Int Set]
attr:[LeftMargin LeftMargin Inch -13107]
attr:[Justification LeftJustified Point 0]
attr:[FontFace Bold Int Set]}
\define{majorheading
menu:[Title,MajorHeading]
attr:[Flags KeepPriorNL Int Set]
attr:[Flags KeepNextNL Int Set]
attr:[Justification Centered Point 0]
attr:[FontSize PreviousFontSize Point 4]}
\define{formatnote
menu:[Region,FormatNote]
attr:[Flags NoFill Int Set]
attr:[Flags KeepPriorNL Int Set]
attr:[Flags KeepNextNL Int Set]
attr:[Flags PassThru Int Set]}
\define{subheading
menu:[Title,Subheading]
attr:[Flags KeepPriorNL Int Set]
attr:[Flags KeepNextNL Int Set]
attr:[Justification LeftJustified Point 0]
attr:[FontFace Bold Int Set]}
\define{center
menu:[Justify,Center]
attr:[Flags KeepPriorNL Int Set]
attr:[Flags KeepNextNL Int Set]
attr:[Justification Centered Point 0]}
\define{flushleft
menu:[Justify,FlushLeft]
attr:[Flags KeepPriorNL Int Set]
attr:[Flags KeepNextNL Int Set]
attr:[Justification LeftJustified Point 0]}
\define{flushright
menu:[Justify,FlushRight]
attr:[Flags KeepPriorNL Int Set]
attr:[Flags KeepNextNL Int Set]
attr:[Justification RightJustified Point 0]}
\define{leftindent
menu:[Region,LeftIndent]
attr:[Flags KeepPriorNL Int Set]
attr:[Flags KeepNextNL Int Set]
attr:[LeftMargin LeftMargin Inch 32768]}
\define{article
menu:[Title,Article]
attr:[Flags KeepPriorNL Int Set]
attr:[Flags KeepNextNL Int Set]
attr:[Justification Centered Point 0]
attr:[FontFace Bold Int Set]
attr:[FontSize ConstantFontSize Point 36]}
\define{subpara
menu:[Title,Subpara]
attr:[Flags KeepPriorNL Int Set]
attr:[Justification LeftJustified Point 0]
attr:[FontFace Bold Int Set]}
\define{subsubpara
menu:[Title,Subsubpara]
attr:[Flags KeepPriorNL Int Set]
attr:[Justification LeftJustified Point 0]
attr:[FontFace Italic Int Set]}

\section{Structures}

This is the template for subparts of the "Structures" section of the Vice article.  If you are only doing a subsection of the "Structures" section, you can remove the Section heading above.  The same basic formatting conventions hold true for other sections of the article, in case you are working on one of those.  You can see a version of the Venus guide in this format in ~langston/public/venus.vdoc to use as a model.  It begins at the "Subsection" level because it's a subpart of the "Structures" section of the article.  You can see an outline of the overall article in ~langston/public/vicearticle.outline.


\subsection{Subpart, i.e., Venus}


\paragraph{Overview or Introduction}


\paragraph{Description}


\subpara{Subpara;
i.e., File protection and Access Control Lists}

\subpara{i.e., Open Modes}


\paragraph{Interface specification;
i.e., The PIOCTL/IOCTL Interface}

\subpara{i.e, IOCTLs}

\subsubpara{i.e., VICECLOSEWAIT}


\paragraph{Interface specification}

\paragraph{Module hierarchy}
\enddata{text,268544256}
