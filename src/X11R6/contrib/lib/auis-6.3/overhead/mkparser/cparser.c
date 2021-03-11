/* parser.c	parser object */

/*
	Copyright Carnegie Mellon University 1992, 1994 - All rights reserved
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

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/mkparser/RCS/cparser.c,v 1.7 1994/04/08 17:42:03 wjh Exp $";
#endif


#include <andrewos.h>
#include <ctype.h>
#include <stdio.h>

#include <cparser.h>

static struct parser *CurrentParser = NULL;
static int DebugFlag = 0;
#define ERRORTOK 1
#define NOTOK -1


#if defined(USEMEMCPY)
#define bcopy(src, dst, num)  memcpy((dst), (src), (num))
#endif



static void debugstate(/* struct parser_tables  *desc, int  state , int  pendtok , int  errorstate */);
static void debugshift(/* struct parser_tables  *desc, int  tact */);
static void debugreduce(/* struct parser_tables  *desc, int  rule , int  revealedstate , int  newstate */);
static void debugflush(/* struct parser_tables  *desc, int  state */);
static void debugnewline();

	int
parser_SetDebug(value)
	int value;
{
	int oldval = DebugFlag;
	DebugFlag = value;
	return oldval;
}

	struct parser *
parser_GetCurrentparser() {
	return CurrentParser;
}

	void
parser_ErrorGuts(self, severity, severityname, msg)
	struct parser *self;
	int severity;
	char *severityname;
	char *msg;
{
	if(self->errorfunc) 
		self->errorfunc(self, severity, severityname, msg);
	else {
		fprintf(stderr, "%s  %s\n", severityname, msg);
		if (severity & parser_FREEMSG)
			free(msg);
	}
}

	void 
parser_Init(self)
	struct parser *self;
{
	self->tables = NULL;
	self->rock = NULL;
	self->killval = NULL;
	self->errorstate = 0;
	self->nerrors = 0;
	self->maxSeverity = parser_OK;
	self->errorfunc=NULL;
}

	struct parser *
parser_New() {
	struct parser *self = (struct parser *)malloc(sizeof(struct parser));
	if(self == NULL) 
		return NULL;
	parser_Init(self);
	return self;
}

	void 
parser_Destroy(self)
	struct parser *self;
{
	free(self);
}

	void 
parser_Error(self, severity, msg)
	struct parser *self;
	int severity;
	char *msg;
{
	int tsev = severity & (~parser_FREEMSG);
	char *name;
	if (tsev > self->maxSeverity)
		self->maxSeverity = tsev;
	self->nerrors++;

	switch(severity & (~parser_FREEMSG)) {
		case parser_OK:		name = ""; 			break;
		case parser_WARNING:	name = "*** Warning:"; 		break;
		case parser_SERIOUS:	name = "*** Error:"; 		break;
		case parser_SYNTAX:	name = "*** Syntax error:"; 	break;
		case parser_FATAL:	name = "*** Fatal error:"; 	break;
		default: 		name = "*** unknown error:"; 	break;
	}
	parser_ErrorGuts(self, severity, name, msg);
}

/* EnumerateReservedWords(self, handler, rock)
	 the handler is called for each reserved word:
		handler(rock, char *word, int tokennumber) 
*/
	void 
parser_EnumerateReservedWords(self, handler, rock)
	struct parser *self;
	parser_enumresfptr handler;
	void *rock;
{
	int i, nnames;
	char **names;
	char buf[100], *bx;

	nnames = self->tables->num_tokens;
	names = self->tables->names;
	for (i = 0; i < nnames; i++) {
		char *name = *names++;
		switch (*name) {
		case '$':
		case '\'':
		case '\"':
			continue;
		case 'e':
			if (strcmp(name, "error") == 0)
				continue;
			break;
		case 's':
			if (strncmp(name, "set", 3) == 0) 
				continue;
			break;
		case 't':
			if (strncmp(name, "tok", 3) == 0) 
				name += 3;
			break;
		}
		strncpy(buf, name, sizeof(buf)-2);
		for (bx = buf; *bx; bx++)
			if (isupper(*bx)) *bx = tolower(*bx);
			else if (islower(*bx)) *bx = toupper(*bx);
		handler(rock, buf, i);
	}
}

/* TokenNumberFromName(self, name)
	Returns the token number corresponding to the string;
	Typical strings:  "function", "setID", "tokNULL", "'a'", "\":=\""  
*/
	int 
parser_TokenNumberFromName(self, name)
	struct parser *self;
	char *name;
{
	int i, nnames, nmlen = strlen(name);
	char **names;
	nnames = self->tables->num_tokens;
	names = self->tables->names;
	for (i = 0; i < nnames; i++) {
		char *tnm = *names++;
		if (strcmp(name, tnm) == 0) return i;
		if (*tnm == '\'' || *tnm == '\"') {
			/* maybe client omitted quotes */
			if (strncmp(name, tnm+1, nmlen) == 0 &&
				nmlen == strlen(tnm) - 2)
				return i;
		}
	}
	return 0;
}


/* parser_TransEscape(buf, plen)
	translate the characters in buf as though they follow backslash
		in a C string
	store the number of characters used in *plen
	return the character value computed (type is int)

	  The translations are a superset of C:
		  escape seq	  :  translation
		  --------------- :  ------------
		  \\ \' \" \b \t  :  as in C
		  \n \v \f \r	 :  as in C
		  \ddd		  :  octal digits, as in C
		  \?		  :  DEL  or  \177
		  \e		  :  ESC  or  ctl-[ or \033
		  \^@		  :  NUL  or  \000
		  \^a ... \^z	 :  ctl-a ... ctl-z  or  \001 ... \032
		  \^[  \^\	\^]   :  \033  \034  \035
		  \^^  \^_		  :  \036  \037
		  \o		  :  other characters, unchanged
	if no character follows the \, return \ and length of zero
*/
	int 
parser_TransEscape(buf, plen)
	char  *buf;
	int  *plen;
{
	static char esctab[]
	  = "r\rn\nf\ft\tb\bv\v\"\"\'\'\\\\?\177e\033E\033R\rN\nF\fT\tB\bV\v";
	char *cx;
	int val, len;

	if (*buf == '\0') {
		len = 0;
		val = '\\';
	}
	else if (isdigit(*buf)) {
		/* parse digit string */
		len = 0;
		val = 0;
		cx = buf;
		while (isdigit(*cx) && len < 3)  {
			val = 8 * val + (*cx++ - '0');
			len++;
		}
	}
	else if (*buf == '^') {
		/* extended syntax for control-x */
		if (*(buf+1) == '\0') {
			val = '^';
			len = 1;
		}
		val = '\037' & *(buf+1);
		len = 2;
	}
	else {
		len = 1;
		for (cx = esctab ; *cx && *cx != *buf; cx +=2) {}
		val = (*cx) ? *(cx+1) : *buf;
	}
	if (plen) *plen = len;
	return val;
}


/* parser_ParseNumber(buf, plen, intval, realval)
	parses a number from buf and sets *plen to length found
	first character in buf must be legal to start an integer
	sets *intval to integer value and
		if syntax is for a real, sets *dblval to real value
		returns 1 if syntactically an integer, 2 for a double,
			and 0 for a syntax error

	An integer is a string of digits
		or 0x followed by a string of hexadecimal digits
		or a character within apostrophes, possibly \-escaped
	A real has one of the following formats
		.ddd
		ddd.
		ddd.ddd
		.dddEpddd
		ddd.Epddd
		ddd.dddEpddd
		dddEpddd
	where
		ddd is a digit sequence  (one or more digits)
		p is empty or + or -
		E may be 'e' or 'E'  and means the exponent
*/

/* translate all characters isolating those of interest for numbers:
	0..9 a..f  +  ,  -  .	*/
static char xlate[] = {   16, 20, 17, 18, 20,	  /*  +  ,  -  .  /  */
		0, 1, 2, 3, 4, 5, 6, 7, 8, 9,	  /* 0 ... 9 */
		20, 20, 20, 20, 20, 20, 	  /* :	;  <  =  >  ? */
		20, 10, 11, 12, 13, 14, 15, 20,   /* @	A ... G */
		20, 20, 20, 20, 20, 20, 20, 20,   /* H ... O */
		20, 20, 20, 20, 20, 20, 20, 20,   /* P ... W */
		19, 20, 20, 20, 20, 20, 20, 20,   /* X ... */
		20, 10, 11, 12, 13, 14, 15, 20,   /* `	a ... g */
		20, 20, 20, 20, 20, 20, 20, 20,   /* h ... o */
		20, 20, 20, 20, 20, 20, 20, 20, 19,  /* p ... x */
};

/* for each of the 21 kinds of character that may appear in or after numbers,
	and for each current state, specify the new state
	initial state is 8.  final state is 9 or 10.
*/
static char newstate [9][21] = {
/*	 char:  0, 1, 2, 3, 4, 5, 6, 7, 8, 9, a, b, c, d, e, f, +, -, .,x,oth*/

/*0 leading 0*/{0, 2, 2, 2, 2, 2, 2, 2, 7, 7,10,10,10,10,10,10,10,10, 4, 3,10},
/*1 decimal */ {1, 1, 1, 1, 1, 1, 1, 1, 1, 1,10,10,10,10, 5,10,10,10, 4,10,10},
/*2 octal */   {2, 2, 2, 2, 2, 2, 2, 2, 7, 7,10,10,10,10, 7,10,10,10, 7, 9,10},
/*3 hex */     {3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,10,10, 7, 9,10},
/*4 after . */ {4, 4, 4, 4, 4, 4, 4, 4, 4, 4,10,10,10,10, 5,10,10,10, 9, 9,10},
/*5 after e */ {6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 9, 9, 9, 9, 9, 9, 6, 6, 9, 9, 9},
/*6 exponent */{6, 6, 6, 6, 6, 6, 6, 6, 6, 6,10,10,10,10, 9,10,10,10, 9, 9,10},
/*7 has error*/{7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 9, 9, 9, 9, 9, 9, 9, 9, 7, 9, 9},
/*8 initial */ {0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 9, 9, 9, 9, 9, 9, 9, 9, 4, 9, 9}
/*9 error halt */
/*10 accept */
};
	int 
parser_ParseNumber(buf, plen, intval, dblval)
	char  *buf;
	long  *plen;
	long  *intval;
	double  *dblval;
{
	long val;
	int len;
	char oldstate, currstate;
	register int x;
	register char *bx;
	int success;

	if (*buf == '\'') {
		/* process quoted character */
		val = buf[1];
		if (val == '\\') {
			val = parser_TransEscape(buf+2, &len);
			len += 3;  /* two apostrophes and the backslash */
		}
		else len = 3;
		if (plen) *plen = len;
		if (intval) *intval = val;
		return (buf[len-1] == '\'') ? 1 : 0;
	}

	len = 0;
	val = 0;
	currstate = 8;
	bx = buf;
	x = *bx++;
			/* run the state machine */
	while (1) {
		x = (x < '+' || x > 'x')  ?  20  :  xlate[x - '+'];
		oldstate = currstate;
		currstate = newstate[oldstate][x];
		if (currstate > 8) break;

				/* accumulate value */
		switch(currstate) {
		case 1: /* accumulate decimal value */
			val = val * 10 + x;
			break;
		case 2: /* accumulate octal value */
			val = val * 8 + x;
			break;
		case 3: /* accumulate hexadecimal value */
			if (x != 19)
				val = val * 16 + x;
			break;
		}
		x = *bx++;
	}
	len = bx - buf - 1 - ((currstate == 9) ? 1 : 0);
	if (plen)
		*plen = len;		/* send back len */

	/* convert */
	if (currstate == 9)
		/* error */
		success = 0;
	else if (oldstate >= 4 && oldstate <= 6) {
		/* is real value */
		double dval;

		success = 2;
		/* convert to both integer and double */
		dval = atof(buf);
		if (dblval)
			*dblval = dval;
		if (intval)
			*intval = (long)(dval + 0.5);  /* round to integer */
	}
	else {
		/* is integer */
		success = 1;
		if (intval)
			*intval = val;
		if (dblval)
			*dblval = val;
	}
	return success;
}


	static void 
debugstate(desc, state, pendtok, errorstate)
	struct parser_tables *desc;
	int state;
	int pendtok;
	int errorstate;
{
	if (pendtok == NOTOK)
		printf("(%d,--)", state);
	else if (pendtok >= 0 && pendtok < desc->num_tokens + desc->num_nt)
		printf("(%d,%s)", state, desc->names[pendtok]);
	else
		printf("(%d,#%d)", state, pendtok);

	if (errorstate)
		printf(" {err:%d}", errorstate);
	fflush(stdout);
}

	static void 
debugshift(desc, tact)
	struct parser_tables  *desc;
	int  tact;
{
	printf(":   shift to state %d\n", tact);
	fflush(stdout);
}

	static void 
debugreduce(desc, rule, revealedstate, newstate)
	struct parser_tables *desc;
	int rule;
	int revealedstate;
	int newstate;
{
	int i;
	printf(":   reduce   %d->%d\n", revealedstate, newstate);
	if (desc->rhs == NULL) {
		printf("\t\trule %d produces non-terminal:  %s", rule,
			   desc->names[desc->lhs[rule]]);
	}
	else {
		printf("\t\tuse rule %d,  %s  :", rule,
			desc->names[desc->lhs[rule]]);
		for (i = 0; i < desc->rhssz[rule]; i++)
			printf("  %s",
				desc->names[desc->rhs[desc->rhsx[rule]+i]]);
	}
	printf("\n");
	fflush(stdout);
}

	static void 
debugflush(desc, state)
	struct parser_tables  *desc;
	int  state;
{
	printf("\t\tpop state %d\n", state);
	fflush(stdout);
}

	static void 
debugnewline() {
	printf("\n");
	fflush(stdout);
}

	int 
parser_Parse(self, lexer, lexrock)
	struct parser *self;
	parser_lexerfptr lexer;
	void *lexrock;
{
	register struct parser_tables *desc = self->tables;
	register int x, tact;	/* temps */
	void *pendval;		/* lookahead symbol value */
	register short pendtok;	/* the look ahead token.  NOTOK if none */
	register int tstate;	/* temporary for state value */
	void *tval;		/* temporary for values */
	int *StateStack;	/* states */
	void *ValueStack;	/* values */
	register int *ssp;	/* statestack pointer,
					pts to highest occupied location  */
	register void *vsp;	/* value stack pointer,
					pts to highest occupied location  */
	int nstelts;		/* number of elements in the 2 stacks */
	int *ssend;		/* pts to highest loc in StateStack */
	int vunit;		/* amt to incr vsp for each value */
	int lhs;		/* lhs of reduced rule */
	struct parser *SaveCurrentparser = CurrentParser;

	CurrentParser = self;
	nstelts = 500;
	StateStack = (int *)malloc(sizeof(int) * nstelts);
	ssp = StateStack - 1;
	ssend = StateStack + nstelts - 1;
	vunit = desc->eltsz;
	ValueStack = (void *)malloc(vunit * nstelts);
	vsp = (void *)((long)ValueStack - vunit);
	pendval = (void *)malloc(desc->eltsz);
	tval = (void *)malloc(desc->eltsz);
	pendtok = NOTOK;
	tstate = 0;

	while (tstate != desc->final_state) {
		/* push state and value left from previous step */
		if (ssp >= ssend) {
			int sofar = ssp-StateStack;
			nstelts += 500;
			StateStack = (int *)realloc(StateStack, 
					sizeof(int)*nstelts);
			ssp = StateStack + sofar;
			sofar = (((long)vsp) - ((long)ValueStack));
			ValueStack = (void *)realloc(ValueStack, 
					vunit * nstelts);
			vsp = (void *)(((long)ValueStack) + sofar);
		}

		ssp++;
		*ssp = tstate;			/* copy state to StateStack*/
		vsp = (void *)(((long)vsp)+vunit);
		bcopy(tval, vsp, desc->eltsz);	/* copy value to ValueStack */

			/* decide whether to shift, reduce, or flag error */

		x = desc->actx[tstate];	/* get pointer into action table */
		if (x == desc->defflag) {
			/* action is a reduction, for any incoming token */
			tact = desc->defred[tstate];

			if (DebugFlag)
				debugstate(desc, tstate, pendtok, 
						   self->errorstate);
		}
		else {
			/* need to look ahead;  get token if needed */
			if (pendtok == NOTOK)
				pendtok = lexer(lexrock, pendval);

			if (DebugFlag)
				debugstate(desc, tstate, pendtok, 
						   self->errorstate);

			x += pendtok;	/* index ptr by pending token */
			if (x < 0 || x > desc->table_max
					|| desc->valid[x] != pendtok) 	
				/* not in table, use default reduction */
				tact = desc->defred[tstate];
			else {
				tact = desc->table[x];
				if (tact > 0) {
					/* shift token and go to state tact */
					if (self->errorstate > 0) 
						self->errorstate--;
					tstate = tact;
					bcopy(pendval, tval, desc->eltsz);
					if (DebugFlag)
						debugshift(desc, tact);
					pendtok = NOTOK; /* absorb the token */
					continue;
				}
				/* tact is 0, -(rule#), or defflag for error*/
				if (tact == 0)
					/* get action from default table */
					tact = desc->defred[tstate];
				else if (tact != desc->defflag)
					tact = -tact;
			}
		}

		if (tact > 0) {
			int rlen = desc->rhssz[tact];  /* length of rhs */
					/* reduce by rule tact */
			bcopy((void *)(((long)vsp) - (rlen - 1)*vunit), tval,
					desc->eltsz);  /* default value */
			x = desc->actions(tact, tval, vsp, self);

				/* check for special return values */
			switch (x) {
			case 1:		/* parser_ACCEPT */
				goto popandexit;
			case 2: 	/* parser_ABORT */
				goto popandexit;
			case 3:		/*parser_ERROR */
				goto ooops;
			case 4:		/* parser_CLEARIN */
				pendtok = NOTOK;  break;
			case 5:		/* parser_ERROROK */
				self->errorstate = 0; break;
			case 6: 	/*parser_CLINEROK */
				pendtok = NOTOK; 
				self->errorstate = 0; 
				break;
			case 7:		/* parser_CLINERR */
				pendtok = NOTOK;
				goto ooops;
			}
			vsp = (void *)(((long)vsp) - rlen * vunit);
			ssp -= rlen;
			if (ssp < StateStack) {
				parser_Error(self, parser_FATAL, 
						  "Stack underflow");
				self->maxSeverity = parser_FATAL;
				goto exit;   /* (nothing to pop) */
			}

			/* new tstate is from nextx/defnext 
				based on top state after popping */
			lhs = desc->lhs[tact] - desc->num_tokens;
			x = desc->nextx[lhs];	/* index from nextx */
			if (x == desc->defflag)
				tstate = desc->defnext[lhs];
			else {
				x += *ssp;	/* index by new top state */
				if (x >= 0 && x <= desc->table_max
						&& desc->valid[x] == *ssp)
					tstate = desc->table[x];
				else tstate = desc->defnext[lhs];
			}
					/* tval was set in call to action() */
			if (DebugFlag)
				debugreduce(desc, tact, *ssp, tstate);
			continue;
		}

			/* syntax error */
		if (self->errorstate == 3) {
			/* we are in recovery;  discard input character */
			if (pendtok == 0)	/* EOF */
				goto popandexit;
			pendtok = NOTOK;
			/* pop top of stack so it can be pushed */
			tstate = *ssp--;
			bcopy(vsp, tval, desc->eltsz); 
			vsp = (void *)(((long)vsp)- vunit);
			if (DebugFlag) debugnewline();
			continue;
		}

		if (self->errorstate == 0)
			parser_Error(self, parser_SYNTAX, "Syntax error");
	    ooops:
		if (self->maxSeverity < parser_SERIOUS)
			self->maxSeverity = parser_SERIOUS;

		/* begin or reinitiate error recovery */
		/* pop stack to elt having state s such that
				'error' token is valid: ie,  
				valid[actx[s]+ERRORTOK] == ERRORTOK */
		while (ssp > StateStack) {
			x = desc->actx[*ssp];
			if (x == desc->defflag)
				{}  /* always reduce.  can't shift */
			else {
				x += ERRORTOK;
				if (x >= 0 && x <= desc->table_max
						&& desc->valid[x] == ERRORTOK)
					break;
			}
			if (DebugFlag)
				debugflush(desc, *ssp);
			ssp--;
			if (self->killval)
				(self->killval)(self, vsp);
			vsp = (void *)(((long)vsp)- vunit);
		}
		if (ssp <= StateStack)   /* no error recovery found */
			goto popandexit;

		/* set tstate to state as though 'error' has been shifted */
		tstate = desc->table[x];
		self->errorstate = 3;
	}   /* end while */

    exit:
	free(StateStack);
	free(ValueStack);
	free(pendval);
	free(tval);

	CurrentParser = SaveCurrentparser;
	return self->maxSeverity;

    popandexit:
	if (self->killval) while (vsp >= ValueStack) {
		(self->killval)(self, vsp);
		vsp = (void *)(((long)vsp)- vunit);
	}
	goto exit;
}
