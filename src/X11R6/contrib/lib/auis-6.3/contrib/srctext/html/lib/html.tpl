\begindata{text,268742804}
\textdsversion{12}

\define{global
attr:[FontSize ConstantFontSize Point 14]
}

\define{head
attr:['color' 'red']
}

\define{img
attr:['color' 'red']
#attr:['callback:src' 'htmlview-set-img-src']
}

\define{anchor
menu:[Styles~5,Anchor~1]
attr:[Flags Underline Int Set]
attr:['color' 'blue4']
attr:['html' 'a']
#attr:['callback:href' 'htmlview-change-href']
}

\define{emphasised
menu:[Styles~5,Emphasised]
attr:['html' 'em']
attr:[FontFace Italic Int Set]
#attr:['callback:b 'htmlview-set-em-bold']
#attr:['callback:i 'htmlview-set-em-italic]
}

\define{header1
menu:[Headings~5,level 1 (Major Heading)~1]
attr:['html' 'h1']
attr:[FontFace Bold Int Set]
attr:[FontSize ConstantFontSize Point 16]
}

\define{header2
menu:[Headings~5,level 2~2]
attr:['html' 'h2']
attr:[FontFace Bold Int Set]
attr:[FontSize ConstantFontSize Point 14]
}

\define{header3
menu:[Headings~5,level 3~3]
attr:['html' 'h3']
attr:[FontFace Bold Int Set]
attr:[FontSize ConstantFontSize Point 12]
}

\define{header4
menu:[Headings~5,level 4~4]
attr:['html' 'h4']
attr:[FontFace Bold Int Set]
}

\define{header5
menu:[Headings~5,level 5~5]
attr:['html' 'h5']
attr:[FontFace Bold Int Set]
}

\define{header6
menu:[Headings~5,level 6~6]
attr:['html' 'h6']
attr:[FontFace Bold Int Set]
}

\define{address
menu:[Styles~5,Address~1]
attr:[FontFace Italic Int Set]
attr:[Justification RightJustified Point 0]
}

\define{preformatted
menu:[Styles~5,Preformatted~1]
attr:['html' 'pre']
attr:[Justification LeftJustified Point 0]
attr:[LeftMargin LeftEdge Int 16]
attr:[Indent LeftMargin Int -16]
attr:[Flags TabsCharacters Int Set]
attr:[FontFace FixedFace Int Set]
attr:[FontFamily AndyType Int 0]
attr:[FontSize ConstantFontSize Point 12]
attr:[Flags ContinueIndent Int Set]
}

\define{blockquote
menu:[Styles~5,Blockquote~10]
attr:[Flags KeepPriorNL Int Set]
attr:[Flags KeepNextNL Int Set]
attr:[LeftMargin LeftMargin Inch 16384]
attr:[RightMargin RightMargin Inch 16384]
attr:[FontFace Italic Int Set]
}

\define{list-item
}

\define{bulleted-list
attr:[Flags KeepPriorNL Int Set]
attr:[Flags KeepNextNL Int Set]
attr:[LeftMargin LeftMargin Inch 32768]
attr:[Indent LeftMargin Inch -12288]
#attr:['callback:compact' 'htmlview-modify-list']
}

\define{enumerated-list
attr:[Flags KeepPriorNL Int Set]
attr:[Flags KeepNextNL Int Set]
attr:[LeftMargin LeftMargin Inch 32768]
attr:[Indent LeftMargin Inch -16384]
}

\define{menu
attr:[Flags KeepPriorNL Int Set]
attr:[Flags KeepNextNL Int Set]
attr:[LeftMargin LeftMargin Inch 32768]
attr:[Indent LeftMargin Inch -16384]
attr:[FontFamily AndySans Int Set]
}

\define{directory
attr:[Flags KeepPriorNL Int Set]
attr:[Flags KeepNextNL Int Set]
attr:[LeftMargin LeftMargin Inch 32768]
attr:[Indent LeftMargin Inch -16384]
attr:[FontFamily AndyType Int Set]
attr:[FontFace FixedFace Int Set]
attr:[FontFamily AndyType Int 0]
attr:[FontSize ConstantFontSize Point 10]
}

\define{dl
attr:[Flags KeepPriorNL Int Set]
attr:[Flags KeepNextNL Int Set]
attr:[LeftMargin LeftMargin Inch 32768]
attr:[Indent LeftMargin Inch -16384]
}

\define{data-description
attr:[LeftMargin LeftMargin Inch 16384]
attr:[Indent LeftMargin Inch 0]
}

\define{data-tag
menu:[Styles,Glossary Key~30]
attr:[FontFace Bold Int Set]
attr:[Flags KeepPriorNL Int Set]
attr:[Flags KeepNextNL Int Set]
}

\define{samp
menu:[Styles,Sample~60]
attr:[FontFace FixedFace Int Set]
attr:[FontFamily AndyType Int 0]
attr:[FontSize ConstantFontSize Point 12]
}

\define{kbd
menu:[Styles,Keyboard~60]
attr:[FontFace FixedFace Int Set]
attr:[FontFamily AndyType Int 0]
}

\define{var
menu:[Styles,Variable~60]
attr:[FontFace Italic Int Set]
}

\define{dfn
menu:[Styles,Definition~60]
attr:[FontFace Italic Int Set]
attr:[FontFace Bold Int Set]
}

\define{cite
menu:[Styles,Citation~60]
attr:[FontFace Italic Int Set]
}

\define{code
menu:[Styles,Code~60]
attr:[FontFace FixedFace Int Set]
attr:[FontFamily AndyType Int 0]
attr:[FontSize ConstantFontSize Point 12]
}

\define{strong
menu:[Styles,Strong~60]
attr:[FontFace Bold Int Set]
}

\define{underline
menu:[Font,Underline~70]
attr:[Flags Underline Int Set]
}

\define{italic
menu:[Font,Italic~70]
attr:[FontFace Italic Int Set]
}

\define{bold
menu:[Font,Bold~70]
attr:[FontFace Bold Int Set]
}

\define{typewriter
menu:[Font,Typewriter~70]
attr:[FontFace FixedFace Int Set]
attr:[FontFamily AndyType Int 0]
}

\define{form
attr:[FontSize  PreviousFontSize Point -2]
attr:[FontFace FixedFace Int Set]
attr:[FontFamily AndyType Int 0]
}

\define{hr
}

\define{select
attr:[FontFace Italic Int Set]
}

\define{option
attr:['color' 'blue']
}
\define{input
attr:[Flags Underline Int Set]
attr:['color' 'blue']
}

\enddata{text,268742804}
