#ifndef _TRIE_H_INCLUDE
#define _TRIE_H_INCLUDE

#include "bitstream.h"
     
/* trie.h,v 1.2 1994/06/03 15:47:19 me Exp */
     
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
 * Useful defines for the trie parser.
 *
 * Author: Terry Jones and Jordan K. Hubbard
 * Date: March 14th, 1990.
 * Description: All the macro and type declarations for the parser section
 *		of the emu widget.
 *
 * Revision History:
 *
 * trie.h,v
 * Revision 1.2  1994/06/03  15:47:19  me
 * New copyright
 *
 * Revision 1.1.1.1  1994/05/22  11:22:45  me
 * Initial import into CVS
 *
 * Revision 2.11  92/02/26  11:42:26  me
 * Steve Crooks' clix port and general code cleanup
 * 
 */

/* Maximum number of characters in a string sequence */
#define MAX_STR_SEQ	(BUF_SZ / 2)

/* Maximum token size for META_TOKEN */
#define MAX_TOKEN	120

/* Maximum push depth for IOP lists */
#define MAX_IOPL	32

#define STACK_SZ   32

/* A stack */
typedef struct _stack {
     int sp;
     struct {
	  int type;
	  Generic value;
     } items[STACK_SZ];
} Stack;

#define TRIE_T		1
#define RPARSE_T	2

/* A request ID node. Just holds an opcode. */
typedef struct _request {
     int request_id;
     struct _request* next;
} Request;

/* An iop node. Holds the operation type and an argument. */
typedef struct _iop {
     short type;
     int value;
#ifdef IOP_DEBUG
     int break;
#endif
     struct _iop *next;
} Iop;

/* A chain of IOPs */
typedef struct _iopchain {
     Iop *head, *tail;
} IopChain;

/* A state in the trie. */
typedef struct _state {
     int final;				/* is this a final state? */
     struct _transition {
	  int label;			/* the transition character */
	  struct _state *state;		/* pointer back to parent */
	  IopChain iop;			/* any associated actions */
	  struct _transition *next;	/* next transaction in the chain */
#if defined(TRIE_TESTING) || defined(TRACK_MALLOC)
	  int id;
#endif
     } *transition_list;
     Request *request_list;		/* requests to generate on final */
#if defined(TRIE_TESTING) || defined(TRACK_MALLOC)
     int id;
#endif
} State;

typedef struct _transition Transition;

/* A reverse parser list element. */
typedef struct _rparse {
     int id;
     IopChain ops;
     struct _rparse *next;
} Rparse;

/* A possible state for the current state list. */
typedef struct _possible_state {
     State *state;
     Transition *trans;
     Generic value;
     int aux;
     int handle_state;
     Iop *iopl[MAX_IOPL];
     bit_decl(mask, MAX_IOPL);
     int niopl;
     struct _possible_state *next;
} Possible_state;

/*
 * Iop 'characters' for the %d, %s and %c transitions.
 * These values must be outside the range of normal chars.
 */
#define READ_INT_TRAN		257
#define READ_STR_TRAN		258
#define READ_CHAR_TRAN		259
#define EPSILON_TRAN            260

/* codes for the iop transitions. */
#define META_READ_INT		0
#define META_READ_STR		1
#define META_CHAR_PUSH		2
#define META_INT_PUSH		3
#define META_STR_PUSH		4

#define META_ADD		'+'
#define META_AND		'&'
#define META_BUFFER		'b'	/* parm */
#define META_CALL		'C'
#define META_CAST		'@'
#define META_CHAR		'c'
#define META_COMP		'~'
#define META_DIV		'/'
#define META_DROP		'X'
#define META_DUP		'D'
#define META_ELSE		':'
#define META_ENDIF		';'
#define META_EQ			'='
#define META_FREE		'f'
#define META_GET		'g'	/* parm */
#define META_GOTO		'j'	/* parm */
#define META_GOTOU		'\012'  /* unsatisfied goto */
#define META_GT			'>'
#define META_IF			'?'
#define META_IGET		'G'
#define META_INT		'd'
#define META_IPOP_TO_REG	'P'
#define META_LABEL		'L'	/* parm */
#define META_LAND		'A'
#define META_LNOT		'!'
#define META_LOR		'O'
#define META_LT			'<'
#define META_MENU		'M'
#define META_MOD		'm'
#define META_MUL		'*'
#define META_NOP		';'
#define META_NOT		'^'
#define META_OR			'|'
#define META_POP_TO_REG		'p'	/* parm */
#define META_ROP		'R'
#define META_SIGNAL		'k'
#define META_STR		's'
#define META_STRLEN		'l'
#define META_SUB		'-'
#define META_SWAP		'S'
#define META_TITLE		't'
#define META_ICON		'i'
#define META_TOKEN		'$'
#define META_TTY		'T'	/* parm */
#define META_UP			'U'

/* Sub-selector (parm) types for various ops */
#define TTY_GET			'g'
#define TTY_SET			's'
#define TTY_IGET		'i'
#define TTY_SGET		'd'

/* Return values and states for handle() and friends. */
#define RECOGNISED		0
#define IN_PROGRESS		1
#define FLUSH			2
#define DELETE_STATE		5
#define HANDLE_NORMAL		6
#define HANDLE_READ_INT		7
#define HANDLE_READ_STR		8

Import State *follow_transition();
Import int next_char(), compare_iop();
Import void emit(), handle_iops(), parse_adjust();
Import void parse_add(), rparse_add();
Import void destroy_current_states();
Import void add_iop(), handle_iops();
Import Rparse *rparse_find();

/* Nuking routines */
Import void destroy_iops();
Import void destroy_requests();
Import void destroy_state();
Import void destroy_transition();
Import void destroy_possible();
Import void destroy_rparse_list();

/* Allocation routines */
Import Transition *new_transition();
Import State *new_state();
Import Iop *new_iop();
Import Request *new_request();
Import Possible_state *new_possible_state();
Import Rparse *new_rparse();

#if defined(TRIE_TESTING) || defined(TRACK_MALLOC)
Import int req_count;
Import int iop_count;
Import int state_count;
Import int trans_count;
Import int rparse_count;
Import int possible_state_count;
#endif /* TRIE_TESTING || TRACK_MALLOC */

#ifdef TRIE_TESTING
#ifdef MALLOC_TEST
#define malloc Malloc
#define free Free
Import char *Malloc();
Import void Free();
#endif /* MALLOC_TEST */
#include <stdio.h>
Import int transition_count();
Import Request *exactly_in_trie();
Import void parse_file();
Import void print_requests(), print_reverse();
Import void print_iop(), print_state(), print_transition();
Import void print_trie(), trie_memory();
#endif /* TRIE_TESTING */

#endif /* _TRIE_H_INCLUDE */
