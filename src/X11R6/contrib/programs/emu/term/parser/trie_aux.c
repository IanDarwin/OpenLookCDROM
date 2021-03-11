#ifndef lint
static char *rcsid = "trie_aux.c,v 1.2 1994/05/27 06:21:30 me Exp";
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
 * Auxilliary testing routines for the trie section.
 *
 * Author: Terry Jones and Jordan K. Hubbard
 * Date: June 12th, 1990.
 * Description: These routines are used exclusively for stand-alone testing
 *		of the trie section. Mostly lots of ugly routines for
 *		dumping internal structures, doing coherency checks, etc.
 *
 * Revision History:
 *
 * trie_aux.c,v
 * Revision 1.2  1994/05/27  06:21:30  me
 * New copyright message
 *
 * Revision 1.1.1.1  1994/05/22  11:22:45  me
 * Initial import into CVS
 */

#include "TermP.h"

#ifdef TRIE_TESTING
/*
 * follow_transition(state, label)
 *
 * Follow the transition marked 'label' leading out from this state.
 * Return the new state or else 0 if there is no such outgoing 
 * transition.
 */
Local State *follow_transition(state, label)
State *state;
int label;
{
     Transition *t;
     
     if (!state)
	  return NULL;
     
     t = state->transition_list;
     
     while (t) {
	  if (t->label == label)
	       return t->state;
	  t = t->next;
     }
     return NULL;
}

/*
 * parse_file(w, fp)
 *
 * Open and read the contents of 'file', adding or deleting each new
 * line to/from the trie.
 */
void parse_file(w, file)
TermWidget w;
char *file;
{
     FILE *fopen(), *fp;
     int fclose();
     char line[BUF_SZ + 1];
     
     if (!file)
	  fatal("parse file called with null pointer.");
     
     if (!(fp = fopen(file, "r")))
	  fatal("Could not open file '%s'.", file);
     
     while (fgets(line, BUF_SZ, fp))
	  parse_add(w, line);
     return;
}


/*
 * exactly_in_trie(w, s)
 *
 * Search the trie and see if 's' is there. We return the
 * request ID associated with 's'. This will be non-zero if
 * 's' was given (as oppsed to 's' being the prefix of something else).
 * A bit flaky, but it's only a testing function.
 */
Request *exactly_in_trie(w, s)
TermWidget w;
caddr_t s;
{
     State *state = w->term.zero_state;
     
#ifdef TRIE_ARG_CHECKING
     if (!s)
	  fatal("In trie called with null pointer.");
#endif /* TRIE_ARG_CHECKING */
     
     while (*s) {
	  if (!(state = follow_transition(state, (int)*s)))
	       return 0;
	  s++;
     }
     return state->request_list;
}

/*
 * trie_memory()
 *
 * Print out statistics on memory usage.
 */
void trie_memory()
{
     Import int trans_count;
     Import int state_count;
     Import int iop_count;
     Import int req_count;
     Import int rparse_count;

     int state_size = sizeof(State);
     int trans_size = sizeof(Transition);
     int spec_size = sizeof(Iop);
     int req_size = sizeof(Iop);
     int rp_size = sizeof(Rparse);

     int state_total = state_count * state_size;
     int trans_total = trans_count * trans_size;
     int spec_total = iop_count * spec_size;
     int req_total = req_count * req_size;
     int rp_total = rp_size * rparse_count;

     printf("%d states alloc'd @ %d bytes/state = %d bytes.\n",
	    state_count, state_size, state_total);
     printf("%d transitions alloc'd @ %d bytes/trans: %d bytes.\n",
	    trans_count, trans_size, trans_total);
     printf("%d iop transitions alloc'd @ %d bytes/trans: %d bytes.\n",
	    iop_count, spec_size, spec_total);
     printf("%d requests alloc'd @ %d bytes/request = %d bytes.\n",
	    req_count, req_size, req_total);
     printf("%d rparse entries @ %d bytes/entry = %d bytes.\n",
	    rparse_count, rp_size, rp_total);

     printf("total allocation is %d bytes\n",
	    state_total + trans_total + spec_total + req_total);
     return;
}

/*
 * dispatch(w, cb)
 *
 * Print out the fact that we have recognised the escape 
 * sequence with the request ID given.
 */
void dispatch(w, cb)
TermWidget w;
ComBlockPtr cb;
{
     register int i; 
     Import void print_regs();
     Import void print_stack();
     
     printf("recognised ID %2d = ", cb_opcode(w->term.cb));
     
     switch (cb_opcode(cb)) {
     case OP_INSERT:
	  printf("self_insert %d char%s: '", cb_nbytes(cb),
		 cb_nbytes(cb) != 1 ? "s" : "");
	  for (i = 0; i < cb_nbytes(cb); i++)
	       fputc(cb_buffer(cb)[i], stdout);

	  printf("'\n");
	  break;

     default:
	  printf("OPCODE %d ", cb_opcode(cb));
	  print_regs(w, "abc", cb);
	  break;
     }
     return;
}

/* Stuff for the printing of the trie. */

Local int indent_level;
Local char *ind = "  ";
#define indent { register int i; \
		 for (i = 0; i < indent_level; i++) \
		      fputs(ind, fp); } fprintf
     
/*
 * print_trie()
 *
 * Print out the contents of the trie.
 *
 */
void print_trie(w, fp)
TermWidget w;
FILE *fp;
{
     if (!w->term.zero_state) {
	  fprintf(fp, "empty trie.\n");
	  return;
     }
     indent_level = -1;
     print_state(fp, w->term.zero_state);
     return;
}

/*
 * print_state(fp, s)
 *
 * Print out the contents of the state.
 * We call ourselves indirectly by calling print_transition for
 * each transition we have.
 *
 */
void print_state(fp, s)
FILE *fp;
State *s;
{
     Import void exit();

     if (!s) {
	  fprintf(stderr, "print_state called with NULL state!\n");
	  exit(1);
     }
     
     if (s->transition_list && s->final && s->transition_list->label !=
	 EPSILON_TRAN) {
	  indent(fp, "state %d has transition list and is a final state!\n",
		 s->id);
	  exit(1); 
     }
     indent_level++;
     
     indent(fp, "STATE %d\n", s->id);
     
     if (s->transition_list) {
	  int valency, count = 0;
	  Transition *t = s->transition_list;
	  
	  /* Print the number and label of each transition. */
	  valency = transition_count(s);
	  indent(fp, "%d transition%s:\n", valency, valency > 1 ? "s" : "");
	  while (t) {
	       count++;
	       switch (t->label) {
	       case READ_INT_TRAN:
		    indent(fp, "READ_INT : transition %d\n", t->id);
		    break;

	       case READ_STR_TRAN:
		    indent(fp, "READ_STR : transition %d\n", t->id);
		    break;

	       case READ_CHAR_TRAN:
		    indent(fp, "READ_CHAR : transition %d\n", t->id);
		    break;

	       case EPSILON_TRAN:
		    indent(fp, "EPSILON TRANSITION : %d\n", t->id);
		    break;

	       default:
		    indent(fp, "read '%c' : follow transition %d\n",
			   t->label, t->id);
		    break;
	       }
	       t = t->next;
	  }
	  fputc('\n', fp);
	  if (count != valency) {
	       indent(fp, "state %d has valency %d but %d transitions!\n",
			s->id, valency, count);
	       exit(1);
	  }
	  t = s->transition_list;
	  while (t) {
	       print_transition(fp, t);
	       t = t->next;
	  }
     }
     else if (s->final) {
	  indent(fp, "a final state\n");
	  indent(fp, "request IDs are : ");
	  print_requests(fp, s->request_list);
	  fprintf(fp, "\n");
     }
     else {
	  indent(fp, "not a final state, but has no transitions.\n");
     }
     indent_level--;
     return;
}

/*
 * print_transition(fp, t)
 *
 * Print out the contents of the transition.
 * We call ourselves indirectly by calling print_transition for
 * each transition we have.
 *
 */
void print_transition(fp, t)
FILE *fp;
Transition *t;
{
     Import void exit();

     if (!t) {
	  fprintf(stderr, "print_transition called with NULL!\n");
	  exit(1); 
     }
     
     indent_level++;
     
     indent(fp, "TRANSITION %d\n", t->id);
     
     switch (t->label) {
     case READ_INT_TRAN:
	  indent(fp, "label is READ_INT\n");
	  break;

     case READ_STR_TRAN:
	  indent(fp, "label is READ_STR\n");
	  break;

     case READ_CHAR_TRAN:
	  indent(fp, "label is READ_CHAR\n");
	  break;

     default:
	  indent(fp, "label is '%c'\n", t->label);
	  break;
     }
     indent(fp, "This transition leads to state %d\n", t->state->id);
     
     if (t->iop.head) {
	  indent(fp, "Iop actions for this transition:\n");
	  print_iop(fp, t->iop.head);
     }
     else {
	  indent(fp, "No iop info associated with this transition.\n");
     }
     indent(fp, "\n");
     if (t->label != EPSILON_TRAN)
	  print_state(fp, t->state);
     indent_level--;
     return;
}

#else /* Just the IOP debugger */
#define indent fprintf
#endif /* TRIE_TESTING */

/*
 * print_regs(w, regs)
 *
 * Print the contents of some global com block registers.
 */
Export void print_regs(w, regs, cb)
TermWidget w;
char *regs;
ComBlockPtr cb;
{
     register int i, j, len;
     Import void exit();

     if (!regs)
	  return;
     len = strlen(regs);

     for (j = 0; j < len; j++) {
	  i = regs[j];
	  switch (cb_reg_type(cb, i)) {
	  case CB_INT_TYPE:
	       printf("#%d=%d ", i, (int)(cb_reg_data(cb, i)));
	       break;

	  case CB_CHAR_TYPE:
	       printf("#%d=%c ", i, (char)(cb_reg_data(cb, i)));
	       break;

	  case CB_STR_TYPE:
	       printf("#%d=%s ", i, (char *)(cb_reg_data(cb, i)));
	       break;

	  case 0:
	       fprintf(stderr, "%c not used ", i);
	       break;

	  default:
	       fprintf(stderr, "strange type %d in register %c\n",
		       cb_reg_type(cb, i), i);
	  }
     }
     printf("\n");
     return;
}

/*
 * print_iop(fp, c)
 *
 * Print out the iop info.
 *
 */
void print_iop(fp, s)
FILE *fp;
Iop *s;
{
     if (!fp)
	  fp = stdout;
     while (s) {
	  switch (s->type) {
	  case META_CHAR_PUSH:
	       indent(fp, "push char %d\n", s->value);
	       break;

	  case META_INT_PUSH:
	       indent(fp, "push integer %d\n", s->value);
	       break;

	  case META_STR_PUSH:
	       indent(fp, "push string '%s'\n", s->value);
	       break;

	  case META_ADD:
	       indent(fp, "Add top two stack elements\n");
	       break;

	  case META_AND:
	       indent(fp, "AND top two stack elements\n");
	       break;

	  case META_BUFFER:
	       indent(fp, "Buffer %s\n", s->value == 'a' ? "Append" : "Set");
	       break;

	  case META_CALL:
	       indent(fp, "Call dispatcher\n");
	       break;

	  case META_CAST:
	       indent(fp, "Cast stack elements\n");
	       break;

	  case META_CHAR:
	       if (s->value) {
		    indent(fp, "output char '%c'\n", s->value);
	       }
	       else {
		    indent(fp, "output char from stack\n");
	       }
	       break;

	  case META_COMP:
	       indent(fp, "Complement top stack element\n");
	       break;

	  case META_DIV:
	       indent(fp, "Divide top two stack elements\n");
	       break;

	  case META_DROP:
	       indent(fp, "drop top stack item\n");
	       break;

	  case META_DUP:
	       indent(fp, "dup top stack item\n");
	       break;

	  case META_EQ:
	       indent(fp, "compare for equality\n");
	       break;

	  case META_FREE:
	       indent(fp, "pass top of stack to free()\n");
	       break;

	  case META_INT:
	       indent(fp, "output INT from stack\n");
	       break;

	  case META_STR:
	       indent(fp, "output string from stack\n");
	       break;

	  case META_SUB:
	       indent(fp, "Subtract top two stack elements\n");
	       break;

	  case META_MUL:
	       indent(fp, "Multiply top two stack elements\n");
	       break;

	  case META_MOD:
	       indent(fp, "Modulus top two stack elements\n");
	       break;

	  case META_OR:
	       indent(fp, "OR top two stack elements\n");
	       break;

	  case META_NOT:
	       indent(fp, "NOT top two stack elements\n");
	       break;

	  case META_LAND:
	       indent(fp, "Logical and top two stack elements\n");
	       break;

	  case META_LOR:
	       indent(fp, "Logical or top two stack elements\n");
	       break;

	  case META_LNOT:
	       indent(fp, "Logical not top two stack elements\n");
	       break;

	  case META_LT:
	       indent(fp, "compare for less-than\n");
	       break;

	  case META_GT:
	       indent(fp, "compare for greater-than\n");
	       break;

	  case META_IF:
	       indent(fp, "IF\n");
	       ++indent_level;
	       break;

	  case META_ELSE:
	       --indent_level;
	       indent(fp, "ELSE\n");
	       ++indent_level;
	       break;

	  case META_ENDIF:
	       --indent_level;
	       indent(fp, "ENDIF\n");
	       break;

	  case META_IGET:
	       indent(fp, "Indirect get from top stack elements\n");
	       break;

	  case META_IPOP_TO_REG:
	       indent(fp, "Indirect pop from top stack elements\n");
	       break;

	  case META_POP_TO_REG:
	       indent(fp, "Pop into register #%d\n", s->value);
	       break;

	  case META_GET:
	       indent(fp, "Get from register #%d\n", s->value);
	       break;

	  case META_SWAP:
	       indent(fp, "swap top two stack items\n");
	       break;
	  }

	  s = s->next;
     }
     return;
}


/*
 * print_requests(fp, r)
 *
 * Print out the requests info.
 *
 */
void print_requests(fp, r)
FILE *fp;
Request *r;
{
     while (r) {
	  fprintf(fp, "%d ", r->request_id);
	  r = r->next;
     }
     fprintf(fp, "\n");
     return;
}

/*
 * print_reverse()
 *
 * Print out the contents of the reverse parse list.
 *
 */
void print_reverse(w)
TermWidget w;
{
     Rparse *r = w->term.rparse_list;
     
     if (!r) {
	  printf("Reverse list is empty.\n");
	  return;
     }

     while (r) {
	  printf("For ID %d:\n\t", r->id);
	  print_iop(stdout, r->ops.head);
	  r = r->next;
     }
     return;
}


/*
 * transition_count(s)
 *
 * Return the total number of transition arcs that leave state s.
 */
int transition_count(s)
State *s;
{
     register int count = 0;
     register Transition *t;
     
     if (!s)
	  return 0;
     
     t = s->transition_list;
     
     while (t) {
	  count++;
	  t = t->next;
     }
     return count;
}

#ifdef MALLOC_TEST
char *Malloc(n)
int n;
{
#undef malloc
     Import char *malloc();
     char *s = malloc(n);

     if (!s)
	  fatal(stderr, "could not malloc %d in Malloc.", n);
     
     fprintf(stderr, "malloc %2d bytes returns %#x\n", n, s);
     fflush(stderr);
     return s;
#define malloc Malloc
}

void Free(s)
char *s;
{
#undef free
     Import void free();
     fprintf(stderr, "free %#x\n", s);
     fflush(stderr);
     free(s);
#define free Free
}
#endif
