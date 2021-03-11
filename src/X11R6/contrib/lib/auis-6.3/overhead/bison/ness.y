/* ness.y  -  typical grammar for bison -n */
/*
	Copyright Carnegie Mellon University 1991, 1992 - All rights reserved
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

*/

%start script

/* precedences  (most of these precedences are dictated by the grammar rather than 
		these rules.  The rules are only used for  OR AND NOT ~)
*/

%left		OR
%left		AND
%left		NOT
%left <s>	'='  "/="
%nonassoc <s>	'<'  '>'  ">="  "<="
%left <s>	'+'  '-'
%left <s>	'*'  '/'  '%'
%left <s>	'~'
%left <s>	UNARYOP   /* used for precedence */


%token	<s>  setID
%token	<s>  setSTRINGCON  setINTCON  setREALCON  /* constants */

/* reserved words in declarations */
%token	<s>  MARKER  BOOLEAN  INTEGER  REAL  OBJECT  VOID
%token	<s>  FUNCTION  END  ON  EXTEND FORWARD
%token	<s>  MOUSE  MENU  KEYS  EVENT

/* reserved words for statements */
%token	<s>  RETURN  WHILE  DO  IF  THEN  ELSE  ELIF  EXIT  GOTOELSE

/* reserved words for expressions*/
%token	<s>  tokTRUE  tokFALSE  tokNULL  AND  OR  NOT


%type <s>  parmList  nonEmptyParmList 
%type <s>  stmtList  stmt  elsePart  declHead declIdList  funcend  eventstart  endtag

%type <i>  type  relop  functype

%type <varnode>	 var  constant  prim 
%type <exprnode>	 expr  midexpr  sumexpr  mulexpr  lowexpr  args  argList  funcCall

%type <s>  '('  ')'



%%  /* rules section */

			/* $$ is nothing, symbols are linked onto 
				*curNess->AttrDest in genLinkGlobal */
script
	:
			{ /* this code creates the function for the initialization */
			   boolean junk;
			   curNess->InitFunc  =  nesssym_NLocate("init*", 
					lexdef_GetPrototype(curLex->def),
					lex_GetScope(curLex), &junk);
			   curNess->InitFunc->flags =  flag_function | flag_ness;
			   curNess->InitFunc->type = Tfunc;
			   curNess->InitFunc->parent.ness = curNess;
			   startfunc(curNess->InitFunc); 
			   nesssym_NGetINode(curNess->InitFunc, funcnode)->functype 
			   		= Tvoid; 
			   genSaveFuncState();
			}
		attributes
			{ /* this code terminates the function for the initialization */
			   genRestoreFuncState(curNess->InitFunc);
			   finishfunc(curNess->InitFunc, NULL); 
			}
	;

/* An object is extended with a sequence of attribute declarations */
			/* $$ is nothing, symbols are linked onto *curNess->AttrDest in genLinkGlobal */
attributes
	:	/* empty */	/* script may be empty */

	|	attributes		
		attrDecl
	;

		/* $$ is type as Txxx value */
type
	:	MARKER
			{ $$ = Tstr; genSaveStmtStart(0); }
	|	BOOLEAN
			{ $$ = Tbool; genSaveStmtStart(0); }
	|	INTEGER
			{ $$ = Tlong; genSaveStmtStart(0); }
	|	REAL
			{ $$ = Tdbl; genSaveStmtStart(0); }
	|	OBJECT
			{ $$ = Tptr; genSaveStmtStart(0); }
	;

			/* $$ is a type value as Txxx */
functype
	:	/* EMPTY */
			{ $$ = Tstr; }	/* default type is marker */
	|	VOID
			{ $$ = Tvoid; }
	|	type
	;

			/* $$ is $1, the token symbol */
eventstart
	:	MENU
	|	KEYS
	|	MOUSE
	|	EVENT
	;

			/* $$ is $1, the token symbol */
endtag
	:	FUNCTION
	|	EXTEND
	|	eventstart
	|	ON
	|	IF
	|	WHILE
	;

/* an attribute declaration is a variable declaration or a function definition */
			/* $$ is nothing, symbols are linked onto *curNess->AttrDest */
attrDecl
	:	error
		FUNCTION
			{ if (isFuncStart($2, TRUE, 2)) {
				abortfunc(); 
				errsynch(0, &yychar, &yyerrflag);
			    } else YYERROR; }
	|	error
		EXTEND
			{ if (isFuncStart($2, TRUE, 2)) {
				/* XXX ought to close existing EXTEND, if any */
				abortfunc(); 
				errsynch(0, &yychar, &yyerrflag);
			    } else YYERROR; }
	|	error
		type
			{ if (isFuncStart($2, FALSE, 3)) {
				abortfunc(); 
				errsynch(0, &yychar, &yyerrflag);
			    } else YYERROR; }
	|	error
		VOID
			{ struct sym *tok, *curtok;
			   /* if this is part ot /return void/ we remain in error state;
				otherwise, it may be a global declaration
				which would signal the end of error state */
			   lex_Repeat(-1);  lex_NextToken(&tok); lex_NextToken(&curtok);
			   if (strcmp(sym_GetName(tok), "return") == 0) YYERROR;
			   if (isFuncStart($2, FALSE, 3)) {
				abortfunc(); 
				errsynch(0, &yychar, &yyerrflag);
			    } else YYERROR; }
	|	error
		ON
		eventstart
			{ /* XXX should we open an EXTEND, if none ??? */
			   /* we accept the ON only if it is the first item on its line */
			    if (lex_RecentIndent(-1) < 999) {
				abortfunc(); 
				errsynch(-1, &yychar, &yyerrflag);
			    }   else YYERROR; }

	|	';'	/* allow extra semi-colons */

	|	declHead	/* variable declaration */
		declIdList
			{ struct nesssym *id, *nid;
			   $1->next = $2;
			   ProcessIdList($1->type, $1, flag_var | flag_globalvar); 
			   for (id = $1; id != NULL; id = nid) {
				nid = id->next;	/* genLinkGlobal changes the link */
				genLinkGlobal(id);
			   	nesssym_NSetInfo(id, TSysMarkRef, makeGlobal()); 
					/* XXX makeGlobal() may not be right 
					for all types */
			   }}

	|	declHead	/* variable declaration with initialization */
		":="
			{ $1->flags = flag_var | flag_globalvar;
			   genLinkGlobal($1);
			   nesssym_NSetInfo($1, TSysMarkRef, makeGlobal()); 
					/* XXX makeGlobal() may not be right 
					for all types */
			   $<varnode>$ = genvarnode($1);
			   genSaveStmtStart(-2);
			   genRestoreFuncState(curNess->InitFunc);
			   predpush(FALSE, $<varnode>$->loc+$<varnode>$->len, 'X'); }
		expr
			{ predvalue(&$4->type);  predpop(); 
			   demandnodetype($4, (($<varnode>3)->sym)->type);
			   exprnode_Destroy($4);
			   genvarstore($<varnode>3); 
			   genSaveFuncState(); }

	|	functype
		FUNCTION
		setID		/* function definition */
			{ long len;
			   if ($3->flags == (flag_function | flag_forward)) {
				/* we are defining a function that was forward referenced
					discard the pointer to the callnode
					(the object code still refers to it)  */
				$3->flags = flag_function | flag_ness;
				nesssym_NSetINode($3, funcnode, NULL);
				$<s>$ = $3;
			   }
			   else $<s>$ = uniqueinscope($3, flag_function | flag_ness, 0);
			   $<s>$->type = Tfunc;
			   if (curNess->CurrentObject == NULL) 
				$<s>$->parent.ness = curNess;
			   else {
				$<s>$->parent.nesssym = curNess->CurrentObject;
				$<s>$->flags |= flag_xfunc;
			   }
			   genSaveStmtStart(-1);
			   predpush(predcond, lex_RecentPosition(-2, &len), 'L');
			   startfunc($<s>$); 
			   nesssym_NGetINode($<s>$, funcnode)->functype 
						= $1; }			/* $4 */
		'('
			{ $<varnode>$ = varnode_Create(lex_RecentPosition(0,0),
				0, $<s>4, (struct toksym *)$5); }	/* $6 */
		parmList
		')'
			{ parencheck($<varnode>6, $8);
			   $<s>$ = $<varnode>6->sym;
			   varnode_Destroy($<varnode>6);
			   nesssym_NGetINode($<s>$, funcnode)->parmlist
						= $7; }		/* $9 */
		funcend
			{ predpop(); finishfunc($<s>9, $10); }
	
	|	EXTEND
		setSTRINGCON
			{ long len;
			   genSaveStmtStart(-1);
			   predpush(predcond, lex_RecentPosition(-1, &len), 'E');
			   $<s>$ = neventStartExtend(curNess->CurrentObject, $2);
			   curNess->saveAttrDest = curNess->AttrDest;
			   curNess->AttrDest = &(nesssym_NGetINode($<s>$, objnode)->attrs);
			   curNess->CurrentObject = $<s>$;
			   curNess->CurrentObject->parent.ness = curNess;  }
		attributes
		END
		endtag	/* EXTEND */
			{ genCheckEndtag($6, EXTEND);
			   neventFinishExtend($<s>3);  predpop();
			   curNess->AttrDest = curNess->saveAttrDest;
			   curNess->CurrentObject = NULL; }

	|	ON
		eventstart
		setSTRINGCON 
			{ long len;
			   genSaveStmtStart(-2);
			   predpush(predcond, lex_RecentPosition(-1, &len), 'V');
			   $<s>$ = neventStartEvent(curNess->CurrentObject, $2, $3); }	/* $4 */
		stmtList
		END
		endtag	/* eventstart or ON */
			{ genCheckEndtag($7, ON);
			   $<s>4 = neventFinishEvent($<s>4, $5, $7);   predpop();
			   $<s>4->parent.nesssym = curNess->CurrentObject; }
	;

			/* $$ is a reversed list of the parameter symbols */
parmList
	:	/* empty */	/* no parameters */
			{ $$ = NULL; }

	|	nonEmptyParmList
	;

			/* $$ is a reversed list of the parameter symbols */
nonEmptyParmList
	:	functype
		setID	
			{ $$ = uniqueinscope($2,  flag_var | flag_parmvar, 0);  
			   $$->type = $1; }		

	|	nonEmptyParmList
		','
		functype
		setID
			{ $$ = uniqueinscope($4,  flag_var | flag_parmvar, 0);
			   $$->type = $3;
			   $$->next = $1; }
	;

funcend
	:	stmtList
		END
		endtag	/* FUNCTION */
			{ genCheckEndtag($3, FUNCTION);  $$ = $1; }
	;


/* A <stmtList> is a list of statements.
	They are executed in succession.  The list may include declarations. 
*/
			/* $$ is the list of defined local vars. */
stmtList
	:	/* empty */
			{ $$ = NULL; }

	|	stmtList
			{ genSaveStmtStart(0);
			   $<s>$ = $1; }
		stmt
			{ $$ = appendlists($<s>2, $3); }
	;

			/* $$ is the list of defined local vars as nesssym's*/
stmt
	:	';'	/* extra semicolon */
			{ $$ = NULL; }

	|	var		/* assignment */
		":="
			{  $<varnode>$ = varIsStorable($1);
			   predpush(FALSE, $<varnode>$->loc+$<varnode>$->len, 'X'); }
		expr
			{ predvalue(&$4->type);  predpop(); 
			   demandnodetype($4, $<varnode>3->sym->type);
			   exprnode_Destroy($4);
			   genvarstore($<varnode>3);  $$ = NULL; }

	|	var		/* append */
		"~:="
			{ $<varnode>$ = varIsStorable($1); 
			   demandsymboltype($1->sym, Tstr);
			   predpush(predcond, $1->loc, 'X');	/* save loc for runtime error */
			   genvarref($1->sym);
			   predpop(); }
		expr
			{ demandnodetype($4, Tstr);
			   if (curNess->accesslevel >= ness_codeYellow)
			 	genop('A');	/* APPEND */
			   else {
				long loc, len;
				loc = $<varnode>3->loc + $<varnode>3->len;
				len = $4->loc - loc;
				SaveError(":Text append may modify a file", loc, len);
				genop('y');	/* pop, leave unchanged original */
			   }
			   exprnode_Destroy($4);
			   genvarstore($<varnode>3);  
			   $$ = NULL; }

	|	declHead	/* declaration */
		declIdList
			{ $$ = $1; $$->next = $2; 
			ProcessIdList($$->type, $$, flag_var | flag_localvar);  }

	|	declHead	/* initialized declaration */
		":="
			{ $<varnode>$ = genvarnode($1);
			   ProcessIdList($1->type, $<varnode>$->sym, 
					flag_var | flag_localvar); 
			   varIsStorable($<varnode>$);
			   predpush(FALSE, $<varnode>$->loc+$<varnode>$->len, 'X'); }
		expr
			{ predvalue(&$4->type);  predpop(); 
			   demandnodetype($4, $<varnode>3->sym->type);
			   exprnode_Destroy($4);
			   $$ = $<varnode>3->sym;  /* access before destroy varnode */
			   genvarstore($<varnode>3); }

	|	funcCall
			{ if ($1->type != Tvoid)
				genop('y');  /* POP - discardvalue  */
			   exprnode_Destroy($1);
			   $$ = NULL; }

	|	RETURN	/* return from function */
			{ long loc, len;  loc = lex_RecentPosition(0, &len);
			   predpush(FALSE, loc + len, 'X'); }
		expr
			{ predvalue(&($3->type));  predpop(); 
			   genreturn(TRUE); 
			   exprnode_Destroy($3);
			   $$ = NULL; }

	|	RETURN	/* return with no value */
		VOID
			{ genreturn(FALSE); $$ = NULL; }

	|	EXIT	/* return with no value */
		FUNCTION
			{ genreturn(FALSE); $$ = NULL; }


	|	RETURN	/* return with no value */
		';'
			{ genreturn(FALSE); $$ = NULL; }

	|	WHILE
			{ long loc, len;  loc = lex_RecentPosition(0, &len);
			   $<predstatenode>$ = predpush(FALSE, loc + len, 'W'); }
		expr
			{ predbranch($3);  predfixdropthru(); 
			   exprnode_Destroy($3);
			   $<predstatenode>$ = $<predstatenode>2; }
		DO
		stmtList
		END
		endtag	/* WHILE */
			{ genCheckEndtag($8, WHILE);
			   genbranch('g', $<predstatenode>4->objloc);
			   predfixtarget();  predpop();
			   $$ = $6; }

	|	EXIT
		WHILE
			{ predexit('W');  $$ = NULL; }

	|	IF
			{ long loc, len;  loc = lex_RecentPosition(0, &len);
			   predpush(FALSE, loc + len, 'I'); }
		expr
		THEN
			{ predbranch($3);  predfixdropthru(); 
			   exprnode_Destroy($3); }
		stmtList
		elsePart
		END
		endtag	/* IF */
			{ genCheckEndtag($9, IF);
			   predpop();  $$ = appendlists($6, $7); }
	;

			/* $$ is a nesssym for the defined variable */
declHead	:
		type
		setID
			{ $$ = uniqueinscope($2, flag_var, 0); 
			   $$->type = $1; }
	;

			/* $$ is the list of defined vars as nesssym's */
declIdList
	:	/* empty */
			{ $$ =NULL; }
	|	declIdList
		','
		setID
			{ $$ = appendlists($1, uniqueinscope($3, flag_var, 0)); }
	;


			/* $$ is the list of defined local vars as nesssym's */
elsePart 
	:	/* empty */
			{ predfixtarget();   $$ = NULL; }

	|	ELSE
			{ $<i>$ = genbranch('g', 0);  predfixtarget(); }
		stmtList
			{ fixbranch($<i>2);  $$ = $3; }

	|	ELIF
			{ long loc, len;  int fixloc = genbranch('g', 0);
			   predfixtarget();
			   loc = lex_RecentPosition(0, &len);
			   $<predstatenode>$ = predpush(FALSE, loc + len, 'I'); 
			   $<predstatenode>$->fixuploc = fixloc; }
		expr
		THEN
			{ predbranch($3);  predfixdropthru(); 
			   exprnode_Destroy($3); }
		stmtList
		elsePart
			{ fixbranch($<predstatenode>2->fixuploc);
			   predpop();  $$ = appendlists($6, $7); }
	;


			/* $$ is an integer index into TopTbl/FopTbl */
relop
	:	'='
			{ $$ = predEQ; }
	|	'<'
			{ $$ = predLT; }
	|	'>'
			{ $$ = predGT; }
	|	"/="
			{ $$ = predNE; }
	|	"<="
			{ $$ = predLE; }
	|	">="
			{ $$ = predGE; }
	;

		/* this is a syntactic class just for the error rules */
/* afterError
 *	:	','  |  ')'  |  END  |  ELIF  |  ELSE  ;
 */

/* * * * * * * * */
/* Expressions */
/* * * * * * * * */
			/* $$ is an exprnode whose type field has type as Txxx */
expr
	:	midexpr

	|	NOT
			{ $<i>$ = lex_RecentPosition(0, 0);
			   predcond = ! predcond; }
		expr
			{ predbranch($3);  predcond = ! predcond;  
			   $$ = $3;  $$->type = Tbra;
			   $$->len += $$->loc - $<i>2;
			   $$->loc = $<i>2; }

	|	expr
		OR
			{ long loc, len;
			   predbranch($1); preddropthru(predOR);
			   if (predcond == FALSE) predfixtarget(); 
			   else predfixdropthru();
			   $<exprnode>$ = $1;  
			   loc = lex_RecentPosition (0, &len);
			   predpush(predcond, loc + len, 'X'); }
		expr
			{ predbranch($4);  
			   predfixdropthru();  predpop();
			   $$ = exprnode_Combine($<exprnode>3, $4, Tbra); }

	|	expr
		AND
			{ predbranch($1); preddropthru(predAND);
			   if (predcond == TRUE) predfixtarget(); 
			   $<exprnode>$ = $1; }
		expr
			{ predbranch($4);
			   $$ = exprnode_Combine($<exprnode>3, $4, Tbra); }
	;


			/* $$ is an exprnode whose type field has type as Txxx */
midexpr	:
		sumexpr

	|	sumexpr
		relop
		sumexpr
			{ gencomp($1, $3);
			   predtarget ($2);
			   $$ = exprnode_Combine($1, $3, Tbra); }
	;

			/* $$ is an exprnode whose type field has type as Txxx */
sumexpr	:
		mulexpr

	|	sumexpr '+'  mulexpr
				{ $$ = genarith('+', $1, $3); }

	|	sumexpr  '-'  mulexpr
				{ $$ = genarith('-', $1, $3); }
	;

			/* $$ is an exprnode whose type field has type as Txxx */
mulexpr	:
		lowexpr

	|	mulexpr  '*'  lowexpr
				{ $$ = genarith('*', $1, $3); }

	|	mulexpr  '/'  lowexpr
				{ $$ = genarith('/', $1, $3); }

	|	mulexpr  '%'  lowexpr
				{ $$ = genarith('%', $1, $3); }
	;

			/* $$ is an exprnode whose type field has type as Txxx */
lowexpr
	:	prim
			{ $$ = exprnode_Create($1->sym->type, NULL, FALSE, 
						$1->loc, $1->len);
			   predpush(predcond, $1->loc, 'X');	/* save loc for runtime error */
			   if ($1->sym->flags & flag_const)
				genconstref($1->sym);
			   else
				genvarref($1->sym);
			   predpop();
			   varnode_Destroy($1); }

	|  	'-'
			{ $<i>$ = lex_RecentPosition(0, 0); }
		lowexpr
			{ if ($3->type == Tdbl) {genop('H'); genop('_');}
			   else {demandnodetype($3, Tlong);  genop('_');}
			   $$ = $3;   $$->len += $$->loc - $<i>2;
			   $$->loc = $<i>2; }

	|	'+'
			{ $<i>$ = lex_RecentPosition(0, 0); }
		lowexpr
			{ $$ = $3;  $$->len += $$->loc - $<i>2;
			   $$->loc = $<i>2; }

	|	funcCall
			{ if ($1->type == Tvoid) {
				genop('0');
				$1->type = Tlong;
			   }
			   $$ = $1; }

	|	tokTRUE
			{ long loc, len;
			   loc = lex_RecentPosition(0, &len);
			   $$ = exprnode_Create(Tbool, NULL, FALSE, loc, len);
			   genop('1'); }

	|	tokFALSE
			{ long loc, len;
			   loc = lex_RecentPosition(0, &len);
			   $$ = exprnode_Create(Tbool, NULL, FALSE, loc, len);
			   genop('9'); }

	|	tokNULL
			{ long loc, len;
			   loc = lex_RecentPosition(0, &len);
			   $$ = exprnode_Create(Tptr, NULL, FALSE, loc, len);
			   genop('^'); }

	|	lowexpr
		'~'
			{ demandnodetype($1, Tstr);
			   if ( ! $1->IsCat) {
				genop('q');	/* NEWBASE */
				genop('z');	/* SWAP */
				genop('r');	/* REPLACE */
			   }
			   $<exprnode>$ = $1; }
		lowexpr
			{ demandnodetype($4, Tstr);
			   genop('A');	/* append */
			   $$ = exprnode_Combine($<exprnode>3, $4, Tstr);
			   $$->IsCat = TRUE; }

	|	'('
			{ long loc, len;
			   loc = lex_RecentPosition(0, &len);
			   $<varnode>$ = varnode_Create(loc, 0, NULL, (struct toksym *)$1);
			   predpush(predcond, loc + len, 'X'); }
		expr
		')'
			{ long loc, len;
			   parencheck($<varnode>2, $4);
			   loc = lex_RecentPosition(0, &len);
			   predfixdropthru();  predpop();
			   $3->len = loc+len - $<varnode>2->loc;
			   $3->loc = $<varnode>2->loc;  $$ = $3;
			   varnode_Destroy($<varnode>2); }
	;

/* a primitive denotes a value */
			/* $$ is a varnode for the value */
prim
	:	var
			{ $$ = $1; }

	|	constant
			{ $$ = $1; }
	;

			/* $$ is an exprnode with type having type of returned value */
funcCall
	:	var	/* must be a function.   */
		'('
			{ $<varnode>$ = varIsFunction($1);
			   $<varnode>$->paren = (struct toksym *)$2;
			   predpush(predcond, $<varnode>$->loc, 'A'); }
		args
		')'
			{ parencheck($<varnode>3 , $5);
			   $$ = callFunc($<varnode>3, $4);
			   predpop(); }
	;

			/* $$ is a reversed list of the types of args in exprnodes */
args
	:	argList
			{ $$ = $1; }

	|	/* EMPTY */	/* no arguments */
			{ $$ = NULL; }
	;

/* <argList> gives the argument expressions in a function call */
			/* $$ is a reversed list of exprnodes */
argList
	:	
			{ long loc, len;  loc = lex_RecentPosition(0, &len);
			   predpush(FALSE, loc + len, 'A'); }
		expr
			{ predvalue(&$2->type);  predpop();
			   $$ = $2; }

	|	argList
		','
			{ long loc, len;  loc = lex_RecentPosition(0, &len);
			   predpush(FALSE, loc + len, 'A'); }
		expr
			{ predvalue(&$4->type);  predpop();
			   $$ = $4;  $4->next = $1; }
	;


		/* $$ is a varnode pointing at the symbol */
var
	:	setID		/* name.  may be a function or a var
				and will be so marked at point of declaration */
			{ $$ = genvarnode($1); }
	;	

		/* $$ is a varnode */
constant
	:	setSTRINGCON
			{ /* there are 4 cases:  EmptyStringToken, short string,
				styled short string, and long string.  
				The EmptyStringToken has name '"'.
				A short string has a name beginning with " and
				 continuing with the \ reduced value of the string.  
				A styled short string  has the name ~ddd and
				a long string has a name /ddd, where
				ddd is an integer.  */
			   long loc, len;
			   loc = lex_RecentPosition(0, &len);
			   if ( ! ($1->flags & flag_const)) 
				/* need to allocate new const */
				nesssym_NSetInfo($1, TSysMarkRef, 
					makeStyledConst(curNess, /*the text*/
						$1->header.toksym.loc, 
						$1->header.toksym.len,
						(*nesssym_NGetName($1) == '"'
						|| *nesssym_NGetName($1) == '~')
					));
			   $1->type = Tstr;  $1->flags = flag_const;
			   $$ = varnode_Create(loc, len, $1, NULL); }

	|	setINTCON			   	
			{ long loc, len;
			   if ( ! ($1->flags & flag_const)) {
				/* need to allocate new const */
				TSysMarkRef g = makeGlobal();
				struct longstkelt *l;
				l = (struct longstkelt *)(&SysMarkLowEnd[g]);
				l->hdr = longHdr;
				l->v = nesssym_NGetInfo($1, long);
				nesssym_NSetInfo($1, TSysMarkRef, g);
				$1->type = Tlong;  $1->flags = flag_const;
			   }
			   loc = lex_RecentPosition(0, &len);
			   $$ = varnode_Create(loc, len, $1, NULL); }
	|	setREALCON		   	
			{ long loc, len;
			   if ( ! ($1->flags & flag_const)) {
				/* need to allocate new const */
				double *dp;
				TSysMarkRef g = makeGlobal();
				struct dblstkelt *d;
				d = (struct dblstkelt *)(&SysMarkLowEnd[g]);
				d->hdr = dblHdr;
				dp = nesssym_NGetInfo($1, double *);
				d->v = *dp;
				nesssym_NSetInfo($1, TSysMarkRef, g);
				$1->type = Tdbl;  $1->flags = flag_const;
			   }
			   loc = lex_RecentPosition(0, &len);
			   $$ = varnode_Create(loc, len, $1, NULL); }
	;


