%{/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
	Copyright Carnegie Mellon University 1992 - All Rights Reserved
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
\* ********************************************************************** */

#ifndef NORCSID
	static char *rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/syntax/tlex/RCS/ness.y,v 1.5 1993/12/06 23:12:31 gk5g Exp $";
#endif

%}
%start script

%left		OR
%left		AND
%left		NOT
%left 		'='  "/="
%nonassoc 	'<'  '>'  ">="  "<="
%left 		'+'  '-'
%left 		'*'  '/'  '%'
%left 		'~'
%left 		UNARYOP   

%token		setID
%token		setSTRINGCON  setINTCON  setREALCON  


%token	  	MARKER  BOOLEAN  INTEGER  REAL  OBJECT  VOID
%token	  	FUNCTION  END  ON  EXTEND FORWARD
%token	  	MOUSE  MENU  KEYS  EVENT


%token	  	RETURN  WHILE  DO  IF  THEN  ELSE  ELIF  EXIT  GOTOELSE


%token		tokTRUE  tokFALSE  tokNULL  AND  OR  NOT

/*
%type   	parmList  nonEmptyParmList 
%type   	stmtList  stmt  elsePart  
%type 		declHead declIdList  funcend  eventstart  endtag

%type   	type  relop  functype

%type 		var  constant  prim  expr
%type 		midexpr  sumexpr  mulexpr  lowexpr  args  argList  funcCall

%type   	'('  ')'
*/
%%  

script
	:	attributes
	;

attributes
	:	
	|	attributes		
		attrDecl
	;

type
	:	MARKER
	|	BOOLEAN
	|	INTEGER
	|	REAL
	|	OBJECT
	;

functype
	:	
	|	VOID
	|	type
	;

eventstart
	:	MENU
	|	KEYS
	|	MOUSE
	|	EVENT
	;

endtag
	:	FUNCTION
	|	EXTEND
	|	eventstart
	|	ON
	|	IF
	|	WHILE
	;

attrDecl
	:	error
		FUNCTION
	|	error
		EXTEND
	|	error
		type
	|	error
		VOID
	|	error
		ON
		eventstart
	|	';'	
	|	declHead	
		declIdList
	|	declHead	
		":="
		expr
	|	functype
		FUNCTION
		setID		
		'('
		parmList
		')'
		funcend
	|	EXTEND
		setSTRINGCON
		attributes
		END
		endtag	
	|	ON
		eventstart
		setSTRINGCON 
		stmtList
		END
		endtag	
	;

parmList
	:	
	|	nonEmptyParmList
	;

nonEmptyParmList
	:	functype
		setID	
	|	nonEmptyParmList
		','
		functype
		setID
	;

funcend
	:	stmtList
		END
		endtag	
	;

stmtList
	:	
	|	stmtList
		stmt
	;

stmt
	:	';'	
	|	var		
		":="
		expr
	|	var		
		"~:="
		expr
	|	declHead	
		declIdList
	|	declHead	
		":="
		expr
	|	funcCall
	|	RETURN	
		expr
	|	RETURN	
		VOID
	|	EXIT	
		FUNCTION
	|	RETURN	
		';'
	|	WHILE
		expr
		DO
		stmtList
		END
		endtag	
	|	EXIT
		WHILE
	|	IF
		expr
		THEN
		stmtList
		elsePart
		END
		endtag	
	;

declHead
	:	type
		setID
	;

declIdList
	:	
	|	declIdList
		','
		setID
	;

elsePart 
	:	
	|	ELSE
		stmtList
	|	ELIF
		expr
		THEN
		stmtList
		elsePart
	;

relop
	:	'='
	|	'<'
	|	'>'
	|	"/="
	|	"<="
	|	">="
	;

expr
	:	midexpr
	|	NOT
		expr
	|	expr
		OR
		expr
	|	expr
		AND
		expr
	;

midexpr
	:	sumexpr
	|	sumexpr
		relop
		sumexpr
	;

sumexpr
	:	mulexpr
	|	sumexpr '+'  mulexpr
	|	sumexpr  '-'  mulexpr
	;

mulexpr
	:	lowexpr
	|	mulexpr  '*'  lowexpr
	|	mulexpr  '/'  lowexpr
	|	mulexpr  '%'  lowexpr
	;

lowexpr
	:	prim
	|  	'-'
		lowexpr
	|	'+'
		lowexpr
	|	funcCall
	|	tokTRUE
	|	tokFALSE
	|	tokNULL
	|	lowexpr
		'~'
		lowexpr
	|	'('
		expr
		')'
	;

prim
	:	var
	|	constant
	;

funcCall
	:	var	
		'('
		args
		')'
	;

args
	:	argList
	|	
	;

argList
	:	
		expr
	|	argList
		','
		expr
	;

var
	:	setID
	;	

constant
	:	setSTRINGCON
	|	setINTCON			   	
	|	setREALCON		   	
	;



