
number		-?[0-9]+(\.[0-9]+)?
string		\"[^\"]*\"
lstring		\'\'.*\'\'$
comment         #.*

whitespace	[ \n\r\t]

%%
{whitespace}	{/* do nothing for now */}

category        {return CATEGORY;}
deadline        {return DEADLINE;}
parent          {return PARENT;}
{number}	{number_value = atoi(yytext); return NUMBER;}
{comment}       {/* do nothing with comments */}
{lstring}	{return LSTRING;}
{string}	{return STRING;}
\/              {return SLASH;}
\:		{return COLON;}
\|              {return BAR;}
D               {return BIGD;}
N               {return BIGN;}
W               {return BIGW;}
M               {return BIGM;}
T               {return BIGT;}
O               {return BIGO;}
S               {return BIGS;}
d               {return LITTLED;}
w               {return LITTLEW;}
b               {return LITTLEB;}
m               {return LITTLEM;}
y               {return LITTLEY;}

