/* parse.c	parse object */

/*
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/syntax/parse/RCS/parse.c,v 1.16 1993/12/15 19:15:24 wjh Exp $";
#endif

/*
	Copyright Carnegie Mellon University 1992 - All rights reserved
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

#include <andrewos.h>
#include <ctype.h>
#include <stdio.h>
#include <lexan.ih>
#include <parse.eh>

static struct parse *CurrentParse = NULL;
static boolean DebugFlag = FALSE;
#define ERRORTOK 1
#define NOTOK -1


	static int
null_action(i, pyyval, yyvsp, rock)
	int i;
	struct YYSTYPE *pyyval;
	struct YYSTYPE *yyvsp;
	void *rock;
{
	return parse_OK;
}

	static void
print_error(self, severity, msg)
	struct parse *self;
	int severity;
	char *msg;
{
	char *sevword;
	switch(severity & (~parse_FREEMSG)) {
	case parse_WARNING:	sevword = "Warning"; break;
	case parse_SERIOUS:	sevword = "Error"; break;
	case parse_SYNTAX:	sevword = "Syntax error"; break;
	case parse_FATAL:	sevword = "Fatal error"; break;
	}
	fprintf(stderr, "*** %s: %s\n", sevword, msg);
	if (severity & parse_FREEMSG)
		free(msg);
}

	static boolean
parse__InitializeClass(ClassID)
	struct classheader *ClassID;
{
	return TRUE;
}



	static boolean
parse__SetDebug(ClassID, value)
	struct classheader *ClassID;
	boolean value;
{
	boolean oldval = DebugFlag;
	DebugFlag = value;
	return oldval;
}

	static struct parse *
parse__GetCurrentParse()
{
	return CurrentParse;
}

	static boolean
parse__InitializeObject(ClassID, self)
	struct classheader *ClassID;
	register struct parse  *self;
{
	self->tables = NULL;
	self->lex = NULL;
	self->rock = NULL;
	self->killval = NULL;
	self->action = null_action;
	self->errorHandler = print_error;
	self->errorstate = 0;
	self->nerrors = 0;
	self-> maxSeverity = parse_OK;

	return TRUE;
}

	static void 
parse__FinalizeObject(ClassID, self)
	struct classheader *ClassID;
	register struct parse  *self;
{
}

	static struct parse *
parse__Create(ClassID, desc, lex, action, rock, errhand)
	struct classheader *ClassID;
	struct parse_tables *desc;
	struct lexan *lex;
	int (*action)();
	void *rock;
	void (*errhand)();
{
	struct parse *p = parse_New();
	int i;

	if (desc == NULL)
		return NULL;
	p->tables = desc;
	p->lex = lex;
	p->rock = rock;
	if (action)
		p->action = action;
	if (errhand)
		p->errorHandler = errhand;	

	return p;
}

	static void
parse__Error(self, severity, msg)
	struct parse *self;
	int severity;
	char *msg;
{
	int tsev = severity & (~parse_FREEMSG);
	if (tsev > self->maxSeverity)
		self->maxSeverity = tsev;
	self->nerrors ++;
	self->errorHandler(self, severity, msg);
}

	static void *     /* int (*)() */
parse__SetErrorHandler(self, handler)
	struct parse *self;
	void (*handler)();
{
	void (*oldhand)() = self->errorHandler;
	if (handler == NULL)
		self->errorHandler = print_error;
	else
		self->errorHandler = handler;
	return (void *)oldhand;
}

/* EnumerateReservedWords(self, handler, rock)
	 the handler is called for each reserved word:
		handler(rock, char *word, int tokennumber) 
*/
	static void
parse__EnumerateReservedWords(self, handler, rock)
	struct parse *self;
	void (*handler)();
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
		case 's':
			if (strncmp(name, "set", 3) == 0) 
				continue;
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
  returns the token number corresponding to the string;
	typical strings:  "function", "setID", "tokNULL", "'a'", "\":=\""  
	The routine is forgiving enough to accept the name parameter
		without quotes:  "a" will match "'a'";  ":=" will match "\":=\""
*/
	static int
parse__TokenNumberFromName(self, name)
	struct parse *self;
	char *name;
{
	int i, nnames, nmlen;
	char **names;
	nnames = self->tables->num_tokens;
	names = self->tables->names;
	nmlen = strlen(name);
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

debugstate(desc, state, pendtok, errorstate)
struct parse_tables *desc;
int state, pendtok, errorstate;
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

debugshift(desc, tact)
	struct parse_tables *desc;
	int tact;
{
	printf(":   shift to state %d\n", tact);
	fflush(stdout);
}

debugreduce(desc, rule, revealedstate, newstate)
	struct parse_tables *desc;
	int rule, revealedstate, newstate;
{
	int i;
	printf(":   reduce   %d->%d\n", revealedstate, newstate);
	printf("\t\tuse rule %d,  %s  :", rule,
			desc->names[desc->lhs[rule]]);
	for (i = 0; i < desc->rhssz[rule]; i++)
		printf("  %s",
			desc->names[desc->rhs[desc->rhsx[rule]+i]]);
	printf("\n");
	fflush(stdout);
}

debugflush(desc, state)
	struct parse_tables *desc;
	int state;
{
	printf("\t\tpop state %d\n", state);
	fflush(stdout);
}

debugnewline()
{
	printf("\n");
	fflush(stdout);
}

	static int
parse__Run(self)
	struct parse *self;
{
	register struct parse_tables *desc;
	register int x, tact;	/* temps */
	void *pendval;    /* lookahead symbol value */
	register short pendtok;    /* the look ahead token.  NOTOK if none */
	register int tstate;	/* temporary for state value */
	void *tval;	/* temporary for values */
	int *StateStack;	/* states */
	void **ValueStack;	/* values */
	register int *ssp;	/* statestack pointer,
					pts to highest occupied location  */
	register void **vsp;	/* value stack pointer,
					pts to highest occupied location  */
	int ssend;	    /* last available location in state stack */
	int vsend;	    /* last available location in value stack */
	int lhs;		/* lhs of reduced rule */
	struct parse *SaveCurrentParse = CurrentParse;
	
	CurrentParse = self;
	if (self->lex == NULL) {
		parse_Error(self, parse_FATAL, "No lexical analyzer");
		goto exit;
	}
	lexan_SetTranslator(self->lex, self);
	desc = self->tables;
	ssend = 2000;
	StateStack = (int *)malloc(sizeof(int) * ssend);
	ssp = StateStack-1;
	vsend = 2000;
	ValueStack = (void **)malloc(sizeof(void *) * vsend);
	vsp = ValueStack-1;
	pendtok = NOTOK;
	tstate = 0;

	while (tstate != desc->final_state) {
		/* push state and value left from previous step */
		if (ssp >= StateStack+(ssend-2)) {
		    int sofar = ssp-StateStack;
		    ssend += 2000;
		    StateStack = (int *)realloc(StateStack, sizeof(int) * ssend);
		    ssp = StateStack + sofar;
		}
		ssp++;
		*ssp = tstate;
		if (vsp >= ValueStack+(vsend-2)) {
		    int sofar = vsp-ValueStack;
		    vsend += 2000;
		    ValueStack = (void **)realloc(ValueStack, sizeof(void *) * vsend);
		    vsp = ValueStack + sofar;
		}
		vsp++;
		*vsp = tval;

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
				pendtok = lexan_NextToken(self->lex, &pendval);

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
					tval = pendval;
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
			tval = *(vsp - rlen + 1);	/* default value */
			x = self->action(tact, &tval, vsp, self->rock);

			/* check for special return values */
			switch (x) {
			case parse_ACCEPT:
				goto popandexit;
			case parse_ABORT:
				if (self->maxSeverity == parse_OK)
					self->maxSeverity = parse_ABORT;
				goto popandexit;
			case parse_ERROR:
				goto ooops;
			case parse_CLEARIN:
				pendtok = NOTOK;  break;
			case parse_ERROROK:
				self->errorstate = 0; break;
			case parse_CLEARIN | parse_ERROROK:
				pendtok = NOTOK; 
				self->errorstate = 0; 
				break;
			case parse_CLEARIN | parse_ERROR:
				pendtok = NOTOK;
				goto ooops;
			case parse_ERROROK | parse_ERROR:
				self->errorstate = 0;
				goto ooops;
			case parse_CLEARIN | parse_ERROROK 
						| parse_ERROR:
				pendtok = NOTOK;
				self->errorstate = 0;
				goto ooops;
			}
			vsp -= rlen;
			ssp -= rlen;
			if (ssp < StateStack) {
				parse_Error(self, parse_FATAL, 
					"Stack underflow");
				self->maxSeverity = parse_FATAL;
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
			if (pendtok == 0) 	/* EOF */
				goto popandexit;
			pendtok = NOTOK;
			/* pop top of stack so it can be pushed */
			tstate = *ssp--;
			tval = *vsp--;
			if (DebugFlag) debugnewline();
			continue;
		}

		if (self->errorstate == 0)
			parse_Error(self, parse_SYNTAX, 
				"Syntax error");
ooops:
		if (self->maxSeverity < parse_SERIOUS)
			self->maxSeverity = parse_SERIOUS;

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
				(self->killval)(self, *vsp);
			vsp--;
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
	
	CurrentParse = SaveCurrentParse;
	return self->maxSeverity;

popandexit:
	if (self->killval) while (vsp >= ValueStack) 
		(self->killval) (self, *vsp--);
		
	goto exit;
}
