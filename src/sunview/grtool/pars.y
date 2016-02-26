/*
	evaluate an expression

	$Header: pars.y,v 1.7 89/08/27 11:56:36 pturner Locked $
*/

%{
#include <math.h>
double result; /* return value if expression */
static int interr;
double rnorm(),fx(),normp(),invnorm(),invt();
static double tmp;
%}
 
%union {
	double val;
	double *ptr;
	int func;
}
 
%token	<val> NUMBER
%token	<ptr> VAR 
%token	<func> CEIL FLOOR MOD TAN  PI   ABS  SQR  LGAMMA LOG LN
%token	<func> ERF ERFC EXP  SIN   COS ACOS ASIN ATAN2 ATAN SQRT RAND
%token	<func> DEG  DX DY RAD   MAX MIN  INDEX INT INVN INVT IRAND NORM NORMP RNORM
%type	<val> expr
%type	<ptr> asgn
%right	'='
%left		OR
%left		AND
%nonassoc GT LT LE GE EQ NE
%left	'+' '-'
%left	'*' '/' '%'
%right '^'
%right	UMINUS NOT
%%
list:
	| list '\n'
	| list asgn '\n'
	| list expr '\n' { result=$2; }
	| error '\n' {yyerror("error:"); yyerrok; }
	;
asgn:	VAR '=' expr { *($$)=$3; }
	;
expr: NUMBER
	| VAR	{ $$ = *($1); }
	| expr '+' expr 		{ $$ = $1 + $3; }
	| expr '-' expr 		{ $$ = $1 - $3; }
	| expr '*' expr 		{ $$ = $1 * $3; }
	| expr '/' expr 		{ if ($3 !=0.0) {
						$$ = $1 / $3; 
					  }
					  else {
    						yyerror("Divide by zero");
					  }
					}
	| expr '%' expr 		{ $$ = fmod($1,$3); }
	| expr '^' expr 		{ $$ = pow($1,$3); }
	| ABS '(' expr ')'		{ $$ = fabs($3); }
	| ACOS '(' expr ')'		{ $$ = acos($3); }
	| ASIN '(' expr ')'		{ $$ = asin($3); }
	| ATAN '(' expr ')'		{ $$ = atan($3); }
	| ATAN2 '(' expr ',' expr ')'	{ $$ = atan2($3, $5); }
	| CEIL '(' expr ')'		{ $$ = ceil($3); }
	| COS '(' expr ')'		{ $$ = cos($3); }
	| DEG 				{ $$ = 180.0/M_PI; }
	| DX 				{ $$ = *xx; }
	| DY 				{ $$ = *yy; }
	| ERF '(' expr ')'		{ $$ = erf($3); }
	| ERFC '(' expr ')'		{ $$ = erfc($3); }
	| EXP '(' expr ')'		{ $$ = exp($3); }
	| FLOOR '(' expr ')'		{ $$ = floor($3); }
	| INDEX				{ $$ = setindex; }
	| INT '(' expr ')'		{ $$ = (long) $3; }
	| INVN '(' expr ')'		{ $$ = invnorm($3); }
	| INVT '(' expr ',' expr ')'	{ $$ = invt($3, (int) $5); }
	| IRAND	'(' expr ')'		{ $$ = random() % (long) ($3); }
	| LGAMMA '(' expr ')'		{ $$ = lgamma($3); }
	| LN '(' expr ')'		{ $$ = log($3); }
	| LOG '(' expr ')'		{ $$ = log10($3); }
	| MAX '(' expr ',' expr ')'	{ $$ = $3 >= $5 ? $3 : $5; }
	| MIN '(' expr ',' expr ')'	{ $$ = $3 <= $5 ? $3 : $5; }
	| MOD '(' expr ',' expr ')'	{ $$ = fmod($3,$5); }
	| NORM '(' expr ')'	{ $$ = fx($3); }
	| NORMP '(' expr ')'	{ $$ = normp($3,&tmp); }
	| PI 				{ $$ = M_PI; }
	| RAD 				{ $$ = M_PI/180.0; }
	| RAND				{ $$ = (double)random() / (pow(2.0,31.0)-1.0); }
	| RNORM '(' expr ',' expr ')'	{ $$ = rnorm($3, $5); }
	| SIN '(' expr ')'		{ $$ = sin($3); }
	| SQR '(' expr ')'		{ $$ = pow($3,2.0); }
	| SQRT '(' expr ')'		{ $$ = sqrt($3); }
	| TAN '(' expr ')'		{ $$ = tan($3); }
	| expr GT expr		{ $$ = $1 > $3; }
	| expr LT expr		{ $$ = $1 < $3; }
	| expr LE expr		{ $$ = $1 <= $3; }
	| expr GE expr		{ $$ = $1 >= $3; }
	| expr EQ expr		{ $$ = $1 == $3; }
	| expr NE expr		{ $$ = $1 != $3; }
	| expr AND expr		{ $$ = $1 && $3; }
	| expr OR expr		{ $$ = $1 || $3; }
	| NOT expr		{ $$ = !($2); }
	| '(' expr ')' { $$ = $2; }
	| '-' expr  %prec UMINUS { $$ = -$2; }
	;
%%

#include <stdio.h>
#include <ctype.h>
#include <signal.h>
#include <setjmp.h>

jmp_buf begin;

static char *progname;
static int lineno = 1;
char f_string[80];
static int pos = 0;

static double *aa, *bb, *cc, *dd, *xx, *yy;
static int setindex;

/*
          struct exception {
               int type;
               char *name;
               double arg1, arg2, retval;
          };
*/
int matherr(exc)
    struct exception *exc;
{
    char buf[255];

    strcpy(buf, exc->name);
    switch (exc->type) {
    case DOMAIN:
	strcat(buf, ": Domain");
	break;
    case SING:
	strcat(buf, ": Singularity");
	break;
    case OVERFLOW:
	strcat(buf, ": Overflow");
	break;
    case UNDERFLOW:
	strcat(buf, ": Underflow");
	break;
    }
    yyerror(buf);
}

fixupstr(val)
char val[];
{
    lowtoupper(val);
    val[strlen(val) + 1] = 0;
    val[strlen(val)] = '\n';
}

scanner(s, x, y, a, b, c, d, i, errpos)
    char s[];
double *x, *y, *a, *b, *c, *d;
int i, *errpos;

{
    interr = 0;
    if (setjmp(begin)) {
	*errpos = interr;
	return;
    }
    pos = 0;
    aa = a;
    bb = b;
    cc = c;
    dd = d;
    xx = x;
    yy = y;
    setindex = i + 1;
    strcpy(f_string, s);
    yyparse();
    *errpos = interr;
}

#define MAXFUN 41

struct funcs {
    char *s;
    int type;
} key[] = {

    "A", VAR,
    "ABS", ABS,
    "ACOS", ACOS,
    "ASIN", ASIN,
    "ATAN", ATAN,
    "ATAN2", ATAN2,
    "B", VAR,
    "C", VAR,
    "CEIL", CEIL,
    "COS", COS,
    "D", VAR,
    "DEG", DEG,
    "DX", DX,
    "DY", DY,
    "ERF", ERF,
    "ERFC", ERFC,
    "EXP", EXP,
    "FLOOR", FLOOR,
    "INDEX", INDEX,
    "INT", INT,
    "INVN", INVN,
    "INVT", INVT,
    "IRAND", IRAND,
    "LGAMMA", LGAMMA,
    "LN", LN,
    "LOG", LOG,
    "MAX", MAX,
    "MIN", MIN,
    "MOD", MOD,
    "NORM", NORM,
    "NORMP", NORMP,
    "PI", PI,
    "RAD", RAD,
    "RAND", RAND,
    "RNORM", RNORM,
    "SIN", SIN,
    "SQR", SQR,
    "SQRT", SQRT,
    "TAN", TAN,
    "X", VAR,
    "Y", VAR
};

int findf(key,s,tlen)
    struct funcs key[];
    char *s;
    int tlen;
{

    int low, high, mid;

    low = 0;
    high = tlen - 1;
    while (low <= high) {
	mid = (low + high) / 2;
	if (strcmp(s, key[mid].s) < 0) {
	    high = mid - 1;
	} else {
	    if (strcmp(s, key[mid].s) > 0) {
		low = mid + 1;
	    } else {
		return (mid);
	    }
	}
    }
    return (-1);
}

int getcharstr()
{
    if (pos >= strlen(f_string))
	return EOF;
    return (f_string[pos++]);
}

ungetchstr()
{
    if (pos > 0)
	pos--;
}

yylex()
{
    int c;
    int found;

    while ((c = getcharstr()) == ' ' || c == '\t');
    if (c == EOF)
	return (0);
    if (c == '.' || isdigit(c)) {
	char stmp[80];
	double d;
	int i;

	i = 0;
	while (c == '.' || isdigit(c)) {
	    stmp[i++] = c;
	    c = getcharstr();
	}
	if (c == 'E' || c == 'e') {
	    stmp[i++] = c;
	    c = getcharstr();
	    if (c == '+' || c == '-') {
		stmp[i++] = c;
		c = getcharstr();
	    }
	    while (c == '.' || isdigit(c)) {
		stmp[i++] = c;
		c = getcharstr();
	    }
	}
	stmp[i] = '\0';
	ungetchstr();
	sscanf(stmp, "%lf", &d);
	yylval.val = d;
	return NUMBER;
    }
    if (isalpha(c)) {
	char sbuf[100], *p = sbuf;

	do {
	    *p++ = c;
	}
	while ((c = getcharstr()) != EOF && isalnum(c));
	ungetchstr();
	*p = '\0';
	if ((found = findf(key,sbuf,MAXFUN)) >= 0) {
	    if (key[found].type == VAR) {
		switch (sbuf[0]) {
		case 'A':
		    yylval.ptr = aa;
		    return VAR;
		case 'B':
		    yylval.ptr = bb;
		    return VAR;
		case 'C':
		    yylval.ptr = cc;
		    return VAR;
		case 'D':
		    yylval.ptr = dd;
		    return VAR;
		case 'X':
		    yylval.ptr = xx;
		    return VAR;
		case 'Y':
		    yylval.ptr = yy;
		    return VAR;
		}
	    }
	    yylval.func = key[found].type;
	    return key[found].type;
	}
	else {
		strcat(sbuf,": No such function or variable");
		yyerror(sbuf);
	}
    }
    switch (c) {
    case '>':
	return follow('=', GE, GT);
    case '<':
	return follow('=', LE, LT);
    case '=':
	return follow('=', EQ, '=');
    case '!':
	return follow('=', NE, NOT);
    case '|':
	return follow('|', OR, '|');
    case '&':
	return follow('&', AND, '&');
    case '\n':
	return '\n';
    default:
	return c;
    }
}

follow(expect, ifyes, ifno)
{
    int c = getcharstr();

    if (c == expect)
	return ifyes;
    ungetchstr();
    return ifno;
}

yyerror(s)
    char *s;
{
    interr = 1;
    errwin(s);
    longjmp(begin, 1);
}

#define C1 0.1978977093962766
#define C2 0.1352915131768107

double rnorm(mean, sdev)
    double mean, sdev;
{
    double u = (double) random() / (pow(2.0, 31.0) - 1.0);

    return mean + sdev * (pow(u, C2) - pow(1.0 - u, C2)) / C1;
}

double fx(x)
double x;
{
	return 1.0/sqrt(2.0*M_PI)*exp(-x*x/2.0);
}

double normp(b,s)
double b,*s;
{
	double sum,dx,a = -8.0,fx();
	int i,n=48;
	a=-8.0;
	sum=fx(a)+fx(b);
	dx=(b-a)/n;
	for ( i=1 ; i<=((n-1) / 2);i++ )
		sum=sum+4.0*fx(a+(2.0*i-1.0)*dx)+2.0*fx(a+2.0*i*dx);
	sum=sum+4.0*fx(b-dx);
	*s=fx(b);
	return sum*dx/3.0;
}

double invnorm(p)
double p;
{
	double s,x,z,temp,normp();
	if ( p>0.5 ) x=1.0-p;
	else x=p;
	s=sqrt(-2.0*log(x));
	x=((-7.49101*s-448.047)*s-1266.846);
	x=x/(((s+109.8371)*s+748.189)*s+498.003)+s;
	if ( p<0.5 ) x=-x;
	z=p-normp(x,&s);
	z=z/s;
	s=x*x;
  	return (((((((((720.0*s+2556.0)*s+1740.0)*s+127.0)*z/7.0+
            ((120.0*s+326.0)*s+127.0)*x)*z/6.0+(24*s+46.0)*s+7.0)*z/40.0+
               (0.75*s+0.875)*x)*z+s+0.5)*z/3.0+x*0.5)*z+1.0)*z+x+0.832e-24*x;
}

double invt(p,n)
double p;
int n;
{
	double sign,temp,a,b,c,d,x,y;
	sign=1.0;
	if ( p<0.5 ) {
		p=1.0-p;
		sign=-1.0;
	}
	p=(1-p)*2;
	if ( n==2 ) {
		temp=sqrt(2.0/(p*(2.0-p))-2.0);
		temp=sign*temp;
		return temp;
	}
	else
		if ( n==1 ) {
			p=p*M_PI/2.0;
			return sign*cos(p)/sin(p);
		}
		else {
			a=1.0/(n-0.5);
			b=48.0/(a*a);
			c=((20700*a/b-98.0)*a-16.0)*a+96.36;
			d=((94.5/(b+c)-3.0)/b+1.0)*sqrt(a*M_PI/2.0)*n;
			x=d*p;
			y=exp((2.0/n)*log(x));
			if ( y>(0.05+a) ) {
				x=invnorm(p*0.5);
				y=x*x;
				if ( n<5 ) c=c+0.3*(n-4.5)*(x+0.6);
				c=(((0.05*d*x-5.0)*x-7.0)*x-2.0)*x+b+c;
				y=(((((0.4*y+6.3)*y+36.0)*y+94.5)/c-y-3.0)/b+1.0)*x;
				y=a*y*y;
				if ( y>0.002 ) y=exp(y)-1.0 ; else y=0.5*y*y+y;
			}
			else y=((1.0/(((n+0.6)/(n*y)-0.089*d-0.822)*(n+2.0)*3.0)+0.5/(n+4.0))*y-1.0)*(n+1.0)/(n+2.0)+1.0/y;
			return sign*sqrt(n*y);
		}
}
