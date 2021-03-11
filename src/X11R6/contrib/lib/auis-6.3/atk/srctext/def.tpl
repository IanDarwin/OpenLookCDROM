\begindata{text,40}
\textdsversion{12}
\define{comment
attr:[FontFace Italic Int Set]}
\define{keyword
attr:[FontFace Bold Int Set]}
\define{predefined
attr:[FontFace Bold Int Set]
attr:[FontFace Italic Int Set]}
\define{function
attr:[FontFace Bold Int Set]
attr:[FontSize PreviousFontSize Point 2]}
\define{userdef
attr:[FontFace Bold Int Set]}
\define{global
attr:[LeftMargin LeftEdge Int 16]
attr:[Indent LeftMargin Int -16]
attr:[Justification LeftJustified Point 0]
attr:[Flags ContinueIndent Int Set]
attr:[FontFace FixedFace Int Set]
attr:[FontFamily AndyType Int 0]
attr:[Flags TabsCharacters Int Set]}
\define{string
}
\define{preproc

attr:[FontFace Italic Int Set]}
(* File <@filename@> created by <@programmer@> at <@time@> on <@date@>. *)


(*

   COPYRIGHT <copyright holder> <@year@>

   ALL RIGHTS RESERVED

*)


(* <A general description of the module as a whole>. *)


DEFINITION MODULE <@name@>;


CONST

    RCSHeader = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/srctext/RCS/def.tpl,v 1.5 1994/02/22 20:14:18 rr2b Exp $";

    

TYPE

    Handle;

    

PROCEDURE Nil(): Handle;


(* Always returns an empty Handle. *)


PROCEDURE Equal(h1, h2: Handle): BOOLEAN;


(* Returns TRUE if h1 and h2 are the same Handle and FALSE otherwise. *)


END <@name@>.


(* CHANGE LOG

<@log@>

*)

\enddata{text,40}
