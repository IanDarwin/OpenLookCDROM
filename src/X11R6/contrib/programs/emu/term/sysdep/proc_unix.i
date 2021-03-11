/* -*-Mode: C; -*- */

#ifndef _PROCESS_UNIX_INCLUDE
#define _PROCESS_UNIX_INCLUDE

/* proc_unix.i,v 1.2 1994/05/27 06:22:03 me Exp */

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
 * Various process manipulation routines that will work on any flavor of
 * unix.
 *
 * Author: Jordan K. Hubbard
 * Date: August 22nd, 1991
 * Description: Here are some routines that appear to be applicable to any
 *		generic and reasonably sane unix.
 *
 *
 * proc_unix.i,v
 * Revision 1.2  1994/05/27  06:22:03  me
 * New copyright message
 *
 * Revision 1.1.1.1  1994/05/22  11:22:46  me
 * Initial import into CVS
 *
 * Revision 1.2  92/05/16  06:15:13  jkh
 * Reorganized basic driver structure somewhat
 */

/* Mandatory */
#ifndef _PROCESS_INCLUDED
#define _PROCESS_INCLUDED
#endif

#ifdef WAIT_STATUS_TYPE
/* Mourn for the recently deceased */
Local int
unix_process_wait(fc)
WAIT_STATUS_TYPE *fc;
{
     int pid;

     if ((pid = wait(fc)) == -1)
          perror("wait");
     return pid;
}
#endif	/* WAIT_STATUS_TYPE */

#endif	/* _PROCESS_UNIX_INCLUDE */
