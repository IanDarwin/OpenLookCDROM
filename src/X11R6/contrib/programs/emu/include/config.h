/* config.h,v 1.2 1994/05/26 21:01:26 me Exp */

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
 * This file contains all user-configurable options for emu.
 *
 * Author: Jordan K. Hubbard
 * Date: March 20th, 1990.
 * Description: Here lie all user settable options and compile-time
 *		flags for emu. You may also want to look at os.h
 *		for OS-specific options.
 *
 * Revision History:
 *
 * config.h,v
 * Revision 1.2  1994/05/26  21:01:26  me
 * New copyright
 *
 * Revision 1.1.1.1  1994/05/22  11:22:38  me
 * Initial import into CVS
 *
 * Revision 1.4  90/10/12  14:08:46  jkh
 * Name changes; threw out NO_TRANSLATIONS.
 * 
 */

/* maximum line length */
#define MAX_CHARS_IN_LINE	1000

/* maximal number of lines */
#define MAX_LINES		1000

/* Double sized fonts or not? */
#define DOUBLE_FONTS
