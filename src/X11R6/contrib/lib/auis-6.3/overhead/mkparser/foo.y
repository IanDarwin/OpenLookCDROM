%{
	typedef int YYSTYPE;
%}
%start script
%%

  script : /* EMPTY */
			{$$ = 0;}
	| script '?' slash
			{$$ = $1 + $3;}
	| script 'a' 'b'
			{$$ = 1000;}
	;
slash : '/'
			{$$ = 1;}
	;
%%
