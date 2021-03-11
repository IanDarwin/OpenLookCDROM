\begindata{text,40}
\textdsversion{12}
\define{comment
attr:[FontFace Italic Int Set]}
\define{pragma
attr:[FontFace Italic Int Set]
attr:[FontSize PreviousFontSize Point 2]}
\define{keyword
attr:[FontFace Bold Int Set]}
\define{identifier
attr:[FontFace Bold Int Set]
attr:[FontFace Italic Int Set]}
\define{function
attr:[FontFace Bold Int Set]
attr:[FontSize PreviousFontSize Point 2]}
\define{userdef
attr:[FontFace Bold Int Set]}
\define{string
}
\define{global
attr:[LeftMargin LeftEdge Int 16]
attr:[Indent LeftMargin Int -16]
attr:[Justification LeftJustified Point 0]
attr:[Flags ContinueIndent Int Set]
attr:[FontFace FixedFace Int Set]
attr:[FontFamily AndyType Int 0]
attr:[Flags TabsCharacters Int Set]}
INTERFACE <@name@>;


(* File <@filename@>, created by <@programmer@> at <@time@> on <@date@>.

   COPYRIGHT <copyright holder> <@year@>

   ALL RIGHTS RESERVED



Description:  A general description of the interface as a whole.

Environment:  Platform(s) where this interface is available.*)


IMPORT Text;


<*UNUSED*> CONST

  <@name@>Copyright = "<copyright holder>";  

  <@name@>RCSHeader = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/srctext/RCS/i3.tpl,v 1.5 1994/02/22 20:14:18 rr2b Exp $";


TYPE

    T = INTEGER;

    

(*For each type declaration, provide:

Description:  short paragraph describing the type.

Instance variables:  For objects, describe each instance variable*)


EXCEPTION


(*For each declaration, provide:

Description:  short paragraph describing the exception.*)


VAR


PROCEDURE Proc();


(*For each procedure, provide:

Description:  short paragraph describing the procedure.

Parameters:  description of each parameter of the procedure.

Exceptions:  description of the conditions under which each exception gets 
raised.

Return value:  description of any values returned by the procedure.

Side effects:  description of all side effects of the procedure.*)


REVEAL


END <@name@>.


(* Change Log

<@log@>

*)

\enddata{text,40}
