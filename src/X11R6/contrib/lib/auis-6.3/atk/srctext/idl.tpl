\begindata{text,40}
\textdsversion{12}
\define{comment
attr:[FontFace Italic Int Set]}
\define{function
attr:[FontFace Bold Int Set]}
\define{declaration
attr:[FontFace Bold Int Set]}
\define{type_declaration
attr:[FontFace Bold Int Set]}
\define{attribute
attr:[FontFace Bold Int Set]}
\define{operation_attribute
attr:[FontFace Bold Int Set]}
\define{type_attribute
attr:[FontFace Bold Int Set]
attr:[FontFace Italic Int Set]}
\define{port_attribute
attr:[FontFace Bold Int Set]
attr:[FontFace Italic Int Set]}
\define{type_specifier
attr:[FontFace Bold Int Set]
attr:[FontFace Italic Int Set]}
\define{parameter
attr:[FontFace Bold Int Set]
attr:[FontFace Italic Int Set]}
\define{clause
attr:[FontFace Bold Int Set]}
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
%c   /* 'c'  means the following code is similar to C code;  use 'p' for 
pascal similar code and then use the pascal version of NIDL.  (See Chapters 4 
and 5 of NCS Reference Manual). */


[ uuid(35c2c6a25000.0d.00.00.c3.66.00.00.00), version(0) ]

interface <identifier>  /*  Specifiy the name of the interface  */



/*                                  B O D Y

  The following section must contain at least one constant, type, or operation 
declaration.  All must be terminated by a semicolon.  Also, this section must 
be enclosed by braces.  */


\{

/*  Declarator List - Specify any variable names with particular types.  */

    char <variable_name>[length];

    float <variable_name>;

    long int <*pointer_name>;  /*  Specify a pointer with an asterisk  */



/*  Constant Section - Place any constant declarations here.  Examples:

             const int array_size = 100;

             const char jsb = "Jonathan Sebastian Bach";  */

    const   int <variable_name> = <type>;

    const   int <variable_name> = <type>;



/*  Type section - Place any type declarations below.  (See Chapter 4 of NCS 
Reference for more information).  */

    typedef    long <variable>;

    typedef    char <variable[length]>;



/*  Functions - Each function must contain a data type that will be returned 
by the operation and a name.  The data type returned can be any scalar or 
previously named data type, but CANNOT be a pointer.   A 'void' is used if 
 the operation does not return.   If the 'out' direction of the parameter is 
chosen, the variable_name must be a pointer (denoted by an asterisk).

Example: 	void bank$kill_acct(

			handle_t    [in] *h,

  			bank$acct_t [in] acct,

  			status_$t  [out] *st

  			);			*/


    void <function_name>(

 	 <type>   [<in/out directive>]  *<variable_name>, 

 	 <type>   [<in/out directive>]   <variable_name>  );



    int <function_name>(

 	 <type>   [<in/out directive>]  *<variable_name>, 

 	 <type>   [<in/out directive>]   <variable_name>  );


\} \
\enddata{text,40}
