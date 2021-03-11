#if !defined(lint) && !defined(__clipper__)
     static char *rcsid = "iops.c,v 1.2 1994/05/27 06:21:16 me Exp";
#endif

/*
 * This file is part of the Emu system.
 *
 * Copyright 1990 by PCS Computer Systeme, GmbH. Munich, West Germany.
 * 
 * Copyright 1994 by Jordan K. Hubbard and Michael W. Elbel
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL PCS, THE AUTHORS, OR THEIR HOUSEPETS BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. SO DON'T SUE US.
 * THANK YOU.
 */

/*
 * Iop routines for the ReTRIEval parser.
 *
 * Author: Terry Jones and Jordan K. Hubbard
 * Date: March 14th, 1990.
 * Description: This file contains the various and sundry routines for
 *		handling IOP actions.
 *
 * Revision History:
 *
 * iops.c,v
 * Revision 1.2  1994/05/27  06:21:16  me
 * New copyright message
 *
 * Revision 1.1.1.1  1994/05/22  11:22:45  me
 * Initial import into CVS
 *
 * Revision 1.18  92/02/26  11:42:06  me
 * Steve Crooks' clix port and general code cleanup
 * 
 * Revision 1.13  90/11/13  15:00:55  jkh
 * Double descriptor version.
 */

#include "TermP.h"

Local void push();
Local Generic pop();
Local Generic check_pop();

/*
 * push(x)
 *
 * Push x onto a stack.
 */
Local Inline void
push(stack, x, type)
Stack *stack;
Generic x;
int type;
{
     if (stack->sp == STACK_SZ)
	  fatal("stack overflow");
     stack->items[stack->sp].value = x;
     stack->items[stack->sp].type = type;
     ++stack->sp;
     return;
}

/*
 * pop()
 *
 * Return the top value of a stack
 */
Local Inline Generic
pop(stack, typep)
Stack *stack;
int *typep;
{
     if (!stack->sp)
	  fatal("stack underflow");
     if (typep)
	  *typep = stack->items[--(stack->sp)].type;
     return stack->items[stack->sp].value;
}

Local Inline Generic
check_pop(stack, check)
Stack *stack;
int check;
{
     Generic val;
     int type;

     val = pop(stack, &type);
     if (check && type != check)
	  warn("Popped stack type %s, expected %s!",
	       reg_type_name(type), reg_type_name(check));
     return val;
}
     
/* Convenient shorthand */
#define INT_POP()	(int)check_pop(values, CB_INT_TYPE)
#define STR_POP()	(char *)check_pop(values, CB_STR_TYPE)
#define CHR_POP()	(char)check_pop(values, CB_CHAR_TYPE)

/*
 * handle_iops(w, iopp, cb, niops)
 *
 * Execute the iop actions found in s.
 *
 */
Export void
handle_iops(w, iopp, cb, niops)
TermWidget w;
Iop **iopp;
ComBlockPtr cb;
int niops;
{
     register int val, i;
     register Iop *s;
#ifdef TRIE_TESTING
     int fd = 1;
#else
     int fd = w->term.master;
#endif
     Stack istack, value, *values;
     Import void dispatch();
     Import int atoi();

     /* Initialize value stack */
     value.sp = 0;
     values = &value;	/* saves us having to & values each time */

     /* Initialize if stack */
     istack.sp = 0;

     for (i = 0; i < niops; i++) {
	  s = iopp[i];
	  while (s) {
	       int typ;
	       char *gen;
	       char *tmp;
#ifdef IOP_DEBUG
	       int stepcnt = -1;
	       
	       if (s->break || stepcnt == 0)
		    stepcnt = iop_debugger(w, iopp[i], s);
	       else
		    --stepcnt;
#endif
	       /* goto from hell */
AGAIN:
	       switch(s->type) {
	       case META_CHAR_PUSH:
		    push(values, (Generic)s->value, CB_CHAR_TYPE);
		    break;
		    
	       case META_INT_PUSH:
		    push(values, (Generic)s->value, CB_INT_TYPE);
		    break;
		    
	       case META_STR_PUSH:
		    push(values, (Generic)s->value, CB_STR_TYPE);
		    break;
		    
	       case META_ADD:
		    gen = pop(values, &typ);
		    if (typ == CB_STR_TYPE) {
			 char join[2048];
			 Generic val2 = STR_POP();
			 
			 strcpy(join, gen);
			 strcat(join, val2);
			 push(values, val2 = (Generic)XtNewString(join),
			      CB_STR_TYPE);
#ifdef TRACK_MALLOC
			 debug("Malloc addr add %0x", val2);
#endif
		    }
		    else /* assume int */
			 push(values, (Generic)((int)gen + INT_POP()),
			      CB_INT_TYPE);
		    break;
		    
	       case META_AND:
		    push(values, (Generic)(INT_POP() & INT_POP()), CB_INT_TYPE);
		    break;
		    
	       case META_BUFFER:
		    tmp = STR_POP();
		    val = strlen(tmp);
		    if (s->value == 's') {
			 strncpy((char *)cb_buffer(cb), tmp, val);
			 cb_nbytes(cb) = val;
		    }
		    else if (s->value == 'a') {
			 if (val + cb_nbytes(cb) >= BUF_SZ)
			      warn("Buffer overflow for %ba");
			 else {
			      strncpy((char *)(cb_buffer(cb) + cb_nbytes(cb)),
				      tmp, val);
			      cb_nbytes(cb) += val;
			 }
		    }
		    else
			 fatal("Illegal buffer access type in IOP");
		    break;
		    
	       case META_CALL:
		    cb_opcode(cb) = INT_POP();
#ifdef TRIE_TESTING
		    fprintf(stderr, "iop: META_CALL Opcode is %d\n",
			    cb_opcode(cb));
#else
		    w->term.immediate = TRUE;
		    dispatch(w, cb);
#endif
		    break;
		    
	       case META_CAST:
		    val = INT_POP();		/* new type */
		    gen = pop(values, &typ); /* value */
		    switch (val) {
		    case CB_CHAR_TYPE:
		    case CB_INT_TYPE:
			 switch(typ) {
			 case CB_CHAR_TYPE:
			 case CB_INT_TYPE:
			 case CB_ANY_TYPE:
			      push(values, gen, CB_INT_TYPE);
			      break;
			      
			 case CB_STR_TYPE:
			      push(values, (Generic)(atoi(gen)), CB_INT_TYPE);
			      break;
			 }
			 break;
			 
		    case CB_STR_TYPE:
			 switch (typ) {
			      char new[30];
			      Generic val2;

			 case CB_CHAR_TYPE:
			      /* ugly two-stage cast to prevent warning */
			      new[0] = (char)((int)gen);
			      new[1] = '\0';
			      push(values, val2 = (Generic)XtNewString(new),
				   CB_STR_TYPE);
#ifdef TRACK_MALLOC
			      debug("Malloc addr ccast %0x", val2);
#endif
			      break;
			      
			 case CB_INT_TYPE:
			      sprintf(new, "%d", gen);
			      push(values, XtNewString(new), CB_STR_TYPE);
#ifdef TRACK_MALLOC
			      debug("Malloc addr icast %0x", val2);
#endif
			      break;
			      
			 case CB_ANY_TYPE:
			 case CB_STR_TYPE:
			      push(values, gen, CB_STR_TYPE);
			      break;
			 }
			 break;
			 
		    case CB_ANY_TYPE:
			 push(values, gen, CB_ANY_TYPE);
			 break;
		    }
		    break;
		    
	       case META_CHAR:
		    {
			 char ch;

			 /*
			  * This one case can be done with the aid of the
			  * stack or the internal value
			  */
			 if (s->value)
			      ch = (char)s->value;
			 else
			      ch = (char)((int)check_pop(values, 0));
			 write(fd, &ch, 1);
		    }
		    break;
		    
	       case META_COMP:
		    push(values, (Generic)(~(INT_POP())), CB_INT_TYPE);
		    break;
		    
	       case META_DIV:
		    val = INT_POP();
		    push(values, (Generic)(INT_POP() / val), CB_INT_TYPE);
		    break;
		    
	       case META_DROP:
		    pop(values, &typ);
		    break;
		    
	       case META_DUP:
		    gen = pop(values, &typ);
		    push(values, gen, typ);
		    push(values, gen, typ);
		    break;
		    
	       case META_EQ:
		    gen = pop(values, &typ);
		    if (typ == CB_STR_TYPE) {
			 Generic val2 = STR_POP();
			 
			 push(values, (Generic)(!strcmp(gen, val2)),
			      CB_INT_TYPE);
		    }
		    else
			 push(values, (Generic)((int)gen == INT_POP()),
			      CB_INT_TYPE);
		    break;
		    
	       case META_FREE:
		    gen = pop(values, &typ);
		    if (typ == CB_STR_TYPE || typ == CB_ANY_TYPE) {
			 XtFree(gen);
#ifdef TRACK_MALLOC
			 debug("META_FREE on address %x", gen);
#endif
		    }
		    else
			 warn("Tried to Free (%%f) type %d", typ);
		    break;
		    
	       case META_GET:
		    push(values, cb_reg_data(cb, s->value),
			 cb_reg_type(cb, s->value));
		    break;
		    
	       case META_GOTO:
		    s = (Iop *)s->value;
		    goto AGAIN;
		    
	       case META_GOTOU:
		    fatal("Unsatisfied goto to Label '%c' encountered",
			  s->value);
		    break;
		    
	       case META_GT:
		    val = INT_POP();
		    push(values, (Generic)(INT_POP() > val), CB_INT_TYPE);
		    break;
		    
	       case META_IF:
		    /* If it's false, take the branch */
		    gen = pop(values, &typ);
		    if (typ == CB_STR_TYPE && !strlen(gen)) {
			 s = (Iop *)s->value;
			 goto AGAIN;
		    }
		    else if (!gen) {
			 s = (Iop *)s->value;
			 goto AGAIN;
		    }
		    break;
		    
	       case META_IGET:
		    val = INT_POP();
		    push(values, cb_reg_data(cb, val), cb_reg_type(cb, val));
		    break;
		    
	       case META_INT:
		    {
			 char tmp[80];

			 sprintf(tmp, "%d", INT_POP());
			 write(fd, tmp, strlen(tmp));
		    }
		    break;
		    
	       case META_IPOP_TO_REG:
		    val = INT_POP();		/* register number */
		    gen = pop(values, &typ);	/* data */
		    cb_reg_data(cb, val) = gen;
		    cb_reg_type(cb, val) = typ;
		    break;
		    
	       case META_LABEL:
		    break;
		    
	       case META_LAND:
		    push(values, (Generic)(INT_POP() && INT_POP()),
			 CB_INT_TYPE);
		    break;
		    
	       case META_LNOT:
		    push(values, (Generic)(!(INT_POP())), CB_INT_TYPE);
		    break;
		    
	       case META_LOR:
		    push(values, (Generic)(INT_POP() || INT_POP()),
			 CB_INT_TYPE);
		    break;
		    
	       case META_LT:
		    val = INT_POP();
		    push(values, (Generic)(INT_POP() < val), CB_INT_TYPE);
		    break;
		    
	       case META_MENU:
		    gen = STR_POP();
		    val = INT_POP();
#if defined(TRIE_TESTING) || defined(NO_MENUS)
		    printf("iop: META_MENU op %d for '%s'\n", val, gen);
#else
		    XpEmuMenuAction(XtParent(w), gen, val);
#endif
		    break;
		    
	       case META_MOD:
		    val = INT_POP();
		    push(values, (Generic)(INT_POP() % val), CB_INT_TYPE);
		    break;
		    
	       case META_MUL:
		    push(values, (Generic)(INT_POP() * INT_POP()),
			 CB_INT_TYPE);
		    break;
		    
	       case META_NOP:
		    /* Do nothing */
		    break;
		    
	       case META_NOT:
		    val = INT_POP();
		    push(values, (Generic)(INT_POP() ^ val), CB_INT_TYPE);
		    break;
		    
	       case META_OR:
		    push(values, (Generic)(INT_POP() | INT_POP()),
			 CB_INT_TYPE);
		    break;

	       case META_POP_TO_REG:
		    gen = pop(values, &typ);
		    cb_reg_data(cb, s->value) = gen;
		    cb_reg_type(cb, s->value) = typ;
		    break;
		    
	       case META_ROP:
		    /* Call ourselves recursively on new rop */
		    cb_opcode(cb) = INT_POP();
		    rparse(w, cb);
		    break;
		    
	       case META_SIGNAL:
#ifdef TRIE_TESTING
		    printf("iop: META_SIGNAL %d to process\n", INT_POP());
#else
		    process_signal(w, INT_POP());
#endif
		    break;
		    
	       case META_STR:
		    {
			 char *out = STR_POP();

			 write(fd, out, strlen(out));
		    }
		    break;
		    
	       case META_STRLEN:
		    gen = STR_POP();
		    push(values, (Generic)strlen(gen), CB_INT_TYPE);
		    break;
		    
	       case META_SUB:
		    val = INT_POP();
		    push(values, (Generic)(INT_POP() - val), CB_INT_TYPE);
		    break;
		    
	       case META_SWAP:
		    {
			 Generic val1, val2;
			 int typ1, typ2;
			 
			 val1 = pop(values, &typ1);
			 val2 = pop(values, &typ2);
			 push(values, val1, typ1);
			 push(values, val2, typ2);
		    }
		    break;
		    
	       case META_TITLE:
		    {
			 Arg args[1];
			 
			 gen = STR_POP();
			 XtSetArg(args[0], XtNtitle, gen);
			 XtSetValues(XtParent(w), args, 1);
		    }
		    break;

	       case META_ICON:
		    {
			 Arg args[1];
			 
			 gen = STR_POP();
			 XtSetArg(args[0], XtNiconName, gen);
			 XtSetValues(XtParent(w), args, 1);
		    }
		    break;

	       case META_TOKEN:
		    val = (int)pop(values, &typ);
		    if (typ != CB_INT_TYPE && typ != CB_CHAR_TYPE)
			 fatal("Illegal sep '%s' for META_TOKEN", val);
		    gen = STR_POP();
		    /*
		     * We stick the newly split "chunk" of string in a static
		     * space since this is the easiest way to prevent memory
		     * leaks (and we generally convert the chunks immediately
		     * into something else anyway).
		     */
		    if ((tmp = index(gen, val)) != NULL) {
			 static char chunk[MAX_TOKEN];
			 
			 if (tmp - gen >= MAX_TOKEN) {
			      warn("token %d chars long (%d max) for %%%c",
				   tmp - gen, MAX_TOKEN, META_TOKEN);
			      chunk[0] = '\0';
			 }
			 else {
			      strncpy(chunk, gen, tmp - gen);
			      chunk[tmp - gen] = '\0';
			 }
			 push(values, tmp + 1, CB_STR_TYPE); /* what remains */
			 push(values, chunk, CB_STR_TYPE);   /* new chunk */
		    }
		    else {
			 /* nothing remains */
			 push(values, "", CB_STR_TYPE);
			 /* since no token, push whole string as chunk */
			 push(values, gen, CB_STR_TYPE);
		    }
		    break;
		    
	       case META_TTY:
#ifdef TRIE_TESTING
               printf("iop: META_TTY op %d\n", s->value);
#else
		    switch ((int)s->value) {
		    case TTY_GET:
			 push(values, tty_get_values(w), CB_ANY_TYPE);
			 break;
			 
		    case TTY_SET:
			 gen = check_pop(values, CB_ANY_TYPE);
			 tty_set_values(w, gen);
			 break;
			 
		    case TTY_IGET:
			 push(values, w->term.init_tty, CB_ANY_TYPE);
			 break;
			 
		    case TTY_SGET:
			 push(values, tty_get_sane(w), CB_ANY_TYPE);
			 break;
			 
		    default:
			 warn("Unknown sub-selector for META_TTY");
			 break;
		    }
#endif /* TRIE_TESTING */
		    break;

	       case META_UP:
		    /* Send a request up to the parent */
		    gen = STR_POP();
		    if (!w->term.iop_req)
			 warn("handle_iops: no parent iop callback for %%%c!",
			      META_UP);
		    else
			 XtCallCallbackList((Widget)w, w->term.iop_req, gen);
		    break;

	       default:
		    warn("handle_iops: Unknown IOP type %d (%%%c)!\n",
			 s->type, s->type);
		    break;
	       }
	       s = s->next;
	  }
     }
     return;
}

/*
 * compare_iop(iop1, iop2)
 *
 * Compare two iop chains. Return 0 if they are equal, 1 if not.
 */
Export int
compare_iop(a, b)
register IopChain *a, *b;
{
     if (!a->head && !b->head)
	  return 0;
     else if (!a->head || !b->head)
	  return 1;
     else {
	  Iop *x = a->head, *y = b->head;

	  while (x && y) {
	       if (!(x->type == y->type && x->value == y->value))
		    return 1;
	       x = x->next, y = y->next;
	  }
	  if (!x && !y)
	       return 0;
     }
     return 1;
}

/*
 * add_iop(list, type, value)
 *
 * Add a iop action transition to the list 'list'. When we encounter
 * this state while actually parsing, the iop action will be
 * taken in preference to any ordinary outgoing transition. The
 * iop actions are given in the specification of the escape
 * sequences - things like %* and %pX etc etc.
 *
 * We must add the new iop action at the END of the list of iop
 * actions. This is because we want to execute them in
 * the correct order when we are parsing.
 *
 */
Export void
add_iop(chain, type, value)
IopChain *chain;
int type;
int value;
{
     Iop *tmp;

     /* Is it the first? */
     if (!chain->head)
	  tmp = chain->head = chain->tail = new_iop(type, value);
     else {
	  /* Append to chain */
	  tmp = chain->tail->next = new_iop(type, value);
	  chain->tail = tmp;
     }
}
