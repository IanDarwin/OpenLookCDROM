#ifndef _XP_COMMON_H
#define _XP_COMMON_H
/* common.h,v 1.2 1994/05/26 21:01:24 me Exp */

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
 * Common definitions for emu.
 *
 * Author: Jordan K. Hubbard, Michael Elbel and Terry Jones
 * Date: March 20th, 1990.
 * Description: This file is included by all widgets in the emu system.
 *		OS or site specific changes should not be made here.
 *		Those go in os.h and config.h
 *
 * Revision History:
 *
 * common.h,v
 * Revision 1.2  1994/05/26  21:01:24  me
 * New copyright
 *
 * Revision 1.1.1.1  1994/05/22  11:22:38  me
 * Initial import into CVS
 *
 * Revision 1.7  92/02/26  13:09:28  me
 * Steve Crooks' clix port and general code cleanup
 * 
 */

/**************************************************************
 * Various helper macros/typedefs to make the code more clear *
 **************************************************************/

#define Local	static
#define Import	extern
#define Export

#include "config.h"
#include "os.h"
#include "xt_ops.h"

/* Used for various internal buffers */
/* #define BUF_SZ                1024 */
#define BUF_SZ                10240


/*********************************************************
 * Types and macros for various internal data structures *
 *********************************************************/

/* The number of registers in a common block. (Should be at least 26) */
#define CB_NREGS		128

typedef void (*VoidFuncPtr)();		/* Function returning void */
typedef String (*StrFuncPtr)();		/* Function returning String */
typedef int (*IntFuncPtr)();		/* Function returning int */
typedef XtGeometryResult (*GFuncPtr)();	/* Function returning geometry res */

/* General linked list data structure */
typedef struct _link {
     struct _link *next;
     Generic data;
} *Link;

/* A register */
typedef struct _reg {
     unsigned char type;	/* data type */
     Generic data;		/* data ptr */
} Register;

#define reg_size		(sizeof(struct _reg))
#define reg_type(reg)		((reg).type)
#define reg_data(reg)		((reg).data)

/* A communications block */
typedef struct _comblock {
     int opcode;			/* operation code */
     unsigned char buffer[BUF_SZ];	/* data buffer */
     int nbytes;			/* data buffer size */
     Register regs[CB_NREGS];		/* register array */
} ComBlock, *ComBlockPtr;

/*
 * A save block. It is used to store the necessary data
 * for simple canvas calls (i.e. only ONE Argument, buffer or register
 */
typedef struct _saveblock {
     int opcode;
     Generic arg;
     int nbytes;
} SaveBlock;

/* Macros to access a comblock */
#define cb_size			(sizeof(struct _comblock))
#define cb_opcode(x)		(((ComBlockPtr)(x))->opcode)
#define cb_buffer(x)		(((ComBlockPtr)(x))->buffer)
#define cb_nbytes(x)		(((ComBlockPtr)(x))->nbytes)
#define cb_regs(x)		(((ComBlockPtr)(x))->regs)
#define cb_reg(x, r)		(cb_regs(x)[(r)])

/* shorthand */
#define cb_reg_type(x, r)	(reg_type(cb_reg((x), (r))))
#define cb_reg_data(x, r)	(reg_data(cb_reg((x), (r))))

/* Global resources */
typedef struct _res {
     String command;		/* command to execute */
     String termtype;		/* terminal type we're emulating */
} Resources;

/* A list of parsers */
typedef struct _parsertab {
     String name;		/* terminal name */
     VoidFuncPtr in_parser;	/* hard-coded parser routine */
     VoidFuncPtr out_parser;	/* optional output sink */
} ParserTable;

/**********************************************
 * External function and variable definitions *
 **********************************************/

Import int Debug;
Import Resources GlobalRes;

/* Misc routines */
Import void fatal(_VARARGS(String));
Import void warn(_VARARGS(String));
Import void pmessage(_VARARGS(String));
Import void debug(_VARARGS(String));

Import String basename(String);
Import String backslash_convert(String);
Import int strcomp(String, String);
Import Generic malloc_and_clear(int);
Import String get_sub_resource_string(Widget, String, String, String);
Import String reg_type_name(int);

#endif /* _XP_COMMON_H */
