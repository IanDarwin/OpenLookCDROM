#ifndef lint
static char *rcsid = "test_canvas.c,v 1.2 1994/06/02 20:59:12 me Exp";
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
 * Description: Test Wrapper for the TermCanvas Widget
 *
 * Author: Michael Elbel
 * Date: March 20th, 1990.
 *
 * Revision History:
 *
 * test_canvas.c,v
 * Revision 1.2  1994/06/02  20:59:12  me
 * Declare exit()
 *
 * Revision 1.1  1994/06/02  10:53:13  me
 * Moved from main.c
 *
 * Revision 1.2  1994/05/24  19:55:43  me
 * New Copyright
 *
 * Revision 1.1.1.1  1994/05/22  11:22:40  me
 * Initial import into CVS
 */

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/keysym.h>
#include <X11/Shell.h>
#include "canvas.h"
#include <signal.h>
#include <stdio.h>

Widget toplevel, term;

void
main (argc, argv, envp)
int argc;
char *argv [];
char *envp [];
{
     void exit(int);
     
     Local void rparse();
     Import void key_handler();
     
     Arg args[5];
     int n = 0;
     Dimension cell_width, cell_height;

     toplevel = XtInitialize(argv[0],"test_canvas", NULL, 0, &argc, argv);
     n = 0;
     XtSetArg(args[n], XtNwidth, 80);		n++;
     XtSetArg(args[n], XtNheight, 24);		n++;
     term = XtCreateManagedWidget("screen", termCanvasWidgetClass,
					toplevel, args, n);
     
     n = 0;
     XtSetArg(args[n], XpNcellHeight, &cell_height);	n++;
     XtSetArg(args[n], XpNcellWidth, &cell_width);	n++;
     XtGetValues(term, args, n);

     printf("CellWidth = %d, CellHeight = %d\n", cell_width, cell_height);
     
     n = 0;
     XtSetArg(args[n], XtNheightInc, cell_height);	n++;
     XtSetArg(args[n], XtNwidthInc, cell_width);	n++;
     XtSetValues(toplevel, args, n);

     n = 0;
     XtSetArg(args[n], XpNoutput, rparse);	n++;
     XtSetValues(term, args, n);
     
     n = 0;
     XtSetArg(args[n], XpNtermType, "emu");	n++;
     XtSetValues(term, args, n);
     
     XtRealizeWidget(toplevel);
/* XSynchronize(XtDisplay(term), True); */
     XtSetKeyboardFocus(toplevel, term);

     signal(SIGINT, exit);
     
     XtMainLoop();
}

Local void
rparse(widget, block)
Widget widget;
ComBlockPtr block;
{
     char chr;
     
     
     switch (cb_opcode(block)) {
	case ROP_INSERT:
	  XpTermCanvasDispatch(term, block, FALSE);
	  break;
	case ROP_INSERT_CHAR:
	  cb_opcode(block) = -ROP_INSERT;
	  chr = (char)((int)cb_reg_data(block, 0));
	  cb_nbytes(block) = 1;
	  *cb_buffer(block) = chr;
	  XpTermCanvasDispatch(term, block, FALSE);
	  break;
	case 99:
	  XtDestroyWidget(toplevel);
	  break;
	       
	default:
	  warn("Unknown reverse parser opcode %d",cb_opcode(block));
     }
}

