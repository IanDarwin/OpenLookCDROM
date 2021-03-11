/* xt_ops.h,v 1.3 1994/06/02 20:06:15 me Exp */

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
 * termCanvas operation codes.
 *
 * Author: Jordan K. Hubbard
 * Date: March 20th, 1990.
 * Description: Here are all the operation codes supported by the parser.
 *		This file is also meant to be included by users configuring
 * 		emu in their application defaults files.
 *
 * Revision History:
 *
 * xt_ops.h,v
 * Revision 1.3  1994/06/02  20:06:15  me
 * Updated and finallly changed the comments to reflect the correct registers
 *
 * Revision 1.2  1994/05/26  21:01:30  me
 * New copyright
 *
 * Revision 1.1.1.1  1994/05/22  11:22:38  me
 * Initial import into CVS
 *
 * Revision 1.15  92/10/16  16:12:34  me
 * More fixes by Steve Crooks
 * 
 * Revision 1.13  92/02/26  13:09:37  me
 * Steve Crooks' clix port and general code cleanup
 */

/*
 * Definitions for character attributes
 */

#define ATT_NONE	0x0
#define ATT_BOLD	1	/* 1 >> 0 */
#define ATT_UNDERL	2	/* 1 >> 1 */
#define ATT_BLINK	4	/* 1 >> 2 */
#define ATT_REVERSE	8	/* 1 >> 3 */
#define ATT_ITALIC	16	/* 1 >> 4 - this isn't supported yet */

#define ATT_SELECTED	64	/* 1 >> 6 */
#define ATT_CONTINUE	128	/* 1 >> 7 */

#define ATT_FONTS	(ATT_BOLD | ATT_ITALIC)

#define ATT_BOL		256	/* 1 >> 8 */
#define ATT_EOL		512	/* 1 >> 9 */
#define ATT_BOS		1024	/* 1 >> 10*/
#define ATT_EOS		2048	/* 1 >> 11*/

#define LINE_D_NORMAL	0
#define LINE_D_WIDE	1	/* 1 >> 0 */
#define LINE_D_UPPER	2	/* 1 >> 1 */
#define LINE_D_LOWER	4	/* 1 >> 2 */

#define MOTION_STOP	-1
#define MOTION_CONT	-2

/* Types for register contents. */
#define CB_INT_TYPE		1
#define CB_STR_TYPE		2
#define CB_CHAR_TYPE		3
#define CB_ANY_TYPE		4

/* Menu action codes */
#define MENU_ITEM_ACTIVATE	1
#define MENU_ITEM_DEACTIVATE	2
#define MENU_ITEM_MARK		3
#define MENU_ITEM_UNMARK	4

/*
 * Operation codes
 */
#define OP_START		0

/* basic character insertion */
#define OP_INSERT		0
/* movement */
#define OP_MOVE_ABS			1	/* Col/Line in x/y */
#define OP_MOVE_REL			2	/* Col/Line in x/y */
#define OP_MOVE_ABS_COLUMN		3	/* Col in x */
#define OP_MOVE_ABS_ROW			4	/* Line in y */
#define OP_MOVE_REL_COLUMN		5	/* Delta Col in x */
#define OP_MOVE_REL_ROW			6	/* Delta Line in y */
#define OP_MOVE_REL_ROW_SCROLLED 	7	/* e.g. for LF, delta in y */
#define OP_IS_MOVE(op)			((op) >= 1 && (op) <= 6)
/* write modes */
#define OP_INSERT_MODE			8	/* no args */
#define OP_OVERWRITE_MODE		9	/* no args */
/* deleting */
#define OP_DELETE_CHARS			10	/* a is count */
#define OP_DELETE_TO_EOL		11	/* no args */
#define OP_DELETE_LINES			12	/* a is count */
#define OP_DELETE_TO_EOSCR		13	/* no args */
/* erasing */
#define OP_ERASE_CHARS			14	/* a is count */
#define OP_ERASE_LINE_LEFT		15	/* no args */
#define OP_ERASE_LINES			16	/* a is count */
#define OP_ERASE_FROM_TOSCR		17	/* no args */
#define OP_CLEAR_SCREEN			18	/* no args */

/* not used now */
/*					19			*/
/* inserting */

#define OP_INSERT_LINES			20	/* a is count */
/* define the scrolling region */
#define OP_SET_SCROLL_REGION		21	/* Start Line in a, End in b */
#define OP_RING_BELL			22	/* toot the horn */
/* Tabs */
#define OP_HOR_TAB			23	/* no args */
#define OP_SET_TAB_CUR_COL		24	/* no args */
#define OP_SET_TAB_COL			25	/* column in x */
#define OP_SET_TABS_EQ_WIDTH		26	/* tab width in a */
#define OP_CLEAR_TAB_CUR_COL		27	/* no args */
#define OP_CLEAR_TAB_COL		28	/* column in x */
#define OP_CLEAR_ALL_TABS		29	/* nor args */
/* Text Attributes */
#define OP_SET_ATTRIBUTE		30	/* ATT in a */
#define OP_CLEAR_ATTRIBUTE		31	/* ATT in b */
/*
 * Changing the Keyboard Translations e.g. for different Codes on the
 * Cursor Keys and Number Keys depending on the Keyboard mode.
 */
#define OP_OVERRIDE_TRANSLATIONS	32	/* name in a */
/*
 * Change the FLUT e.g. for different character sets.
 */
#define OP_CHANGE_FLUT			33	/* name in a, offset in b */

/*
 * Reports from the canvas to the parser for further use in
 * the parser.
 * They all have no arguments, the canvas however fills the comblock
 * with data.
 */
#define OP_CANVAS_SIZE    		34	/* rep. Cols in x Lines in y */
#define OP_CANVAS_CURSOR_POS		35	/* rep. Col in x Line in y */
#define OP_CANVAS_ATTRIBS		36	/* rep. Attributes in a */
#define OP_CANVAS_SCROLL_REGION		37	/* rep. Start in a, End in b */
#define OP_CANVAS_WRAP_MODE		38	/* rep. wrap mode in a */
#define OP_CANVAS_REVERSE_MODE		39	/* rep. rev. mode in a */
#define OP_CANVAS_CURSOR_ON		40	/* rep. cursor state in a */
#define OP_CANVAS_CURSOR_BLINKING	41	/* rep. if cursor blinks in a*/
#define OP_CANVAS_CURSOR_SIZE		42	/* rep. curs. size in x and y*/

/*
 * clear redraw the screen
 */
#define OP_REDRAW_SCREEN		43	/* no args */

/*
 * change the current fonts
 */
#define OP_CHANGE_FONTS			44	/* normal in a, bold in b */

/*
 * scrolling the screen and the saved area
 */
#define OP_SCROLL_SCREEN_ABSOLUTE	45	/* percentage in a */
#define OP_SCROLL_SCREEN_RELATIVE	46	/* delta percentage in a */

/*
 * general modes
 */
#define OP_CURSOR_OFF			47	/* no args */
#define OP_CURSOR_ON			48	/* no args */
#define OP_SET_SCREEN_SIZE		49	/* Cols in x, Lines in y */
#define OP_WRAP_AROUND			50	/* no args */
#define OP_DONT_WRAP			51	/* no args */
#define OP_CURSOR_POS_REL_TO_SCR_REG	52	/* no args */
#define OP_CURSOR_POS_ABSOLUTE		53	/* no args */
#define OP_REVERSE_VIDEO		54	/* no args */
#define OP_NORMAL_VIDEO			55	/* no args */
#define OP_SAVE_FLUT			56	/* no args */
#define OP_RESTORE_FLUT			57	/* no args */
#define OP_SET_CURSOR_BLINK		58	/* Flag in a */
#define OP_SET_CURSOR_SIZE		59	/* Width in x, Height in y */

/*
 * Color stuff
 */
#define OP_CHANGE_FG_COLOR 		60
#define OP_CHANGE_BG_COLOR		61
#define OP_SET_CIT_CELL			62
#define OP_CANVAS_DISPLAY_CELLS		63

/*
 * Control jump scrolling
 */
#define OP_SET_JUMP_SCROLL		64
#define OP_GET_JUMP_SCROLL		65

/*
 * Line Attributes
 */
#define OP_SET_LINE_ATTRIBUTES		66


/* last OP */
#define OP_END				66

/* special OP NO-OP, MUST be bigger than OP_END */
#define OP_NOP				9999

/* number of OPs */
#define OP_NUM (OP_END - OP_START + 1)


/*
 * Reverse Opcodes for the reverse parser
 */

#define ROP_START			0
/* basic character insertion */
#define ROP_INSERT			0
/* not so basic character insertion */
#define ROP_INSERT_CHAR			1	/* decimal char in a */
/* give back Cursor Position */
#define ROP_SCREEN_SIZE			2	/* Cols in x, Lines in y */
#define ROP_CURSOR_POS			3	/* Col in x, Line in y */
#define ROP_CUR_ATTRIBS			4	/* Attributes in a */
#define ROP_SCROLL_REGION		5	/* Start Line in a, End in b */
#define ROP_PRIMARY_DA			6
#define ROP_SECONDARY_DA		7
#define ROP_ANSWERBACK			8
#define ROP_TERM_STATUS			9
#define ROP_PRINTER_STATUS		10
#define ROP_UDK_STATUS			11
#define ROP_KEYBOARD_STATUS		12

		
/* initialization Codes start at 100 */
/*
 * Initialization for the canvas , does everything necessary to set up the
 * canvas for the current emulation.
 */
#define ROP_INIT_CANVAS			100	/* no args */
#define ROP_INIT_TTY			101	/* no args */
#define ROP_INIT_MENUS			102	/* no args */
