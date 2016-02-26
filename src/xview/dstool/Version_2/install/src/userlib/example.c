/*  -------------------------------------------------------------------

This program is the property of:

                             Cornell University 
                        Center for Applied Mathematics 
                              Ithaca, NY 14853

and may be used, modified and distributed freely, subject to the 
following restrictions:

       Any product which incorporates source code from the dstool
       program or utilities, in whole or in part, is distributed
       with a copy of that source code, including this notice. You
       must give the recipients all the rights that you have with
       respect to the use of this software. Modifications of the
       software must carry prominent notices stating who changed
       the files and the date of any change.

DsTool is distributed in the hope that it will be useful, but 
WITHOUT ANY WARRANTY; without even the implied warranty of FITNESS 
FOR A PARTICULAR PURPOSE.  The software is provided as is without 
any obligation on the part of Cornell faculty, staff or students to 
assist in its use, correction, modification or enhancement.

  -----------------------------------------------------------------  */
#include <xview/xview.h>
#include <stdio.h>

/*
 * example_handler()
 *
 * Menu handler for `panelmenu (Example)'.
 */
Menu_item
  example_handler(item, op)
Menu_item item;
Menu_generate op;
{
  if (op == MENU_NOTIFY)
    {
      example_go();
    }
  return item;
}

/*
 * example_init()
 *
 * example routines initialization
 * called whenever a new dynamical system is loaded
 */
int
  example_init()
{
  /* This example takes no action when a dynamical system is changed. */
}

/*
 * example_go()
 *
 * example analysis begin
 * called when the "Example" menu item is selected
 */
int
  example_go()
{
  fprintf(stdout, "\nThe Example menu item has been selected.\n");
  fprintf(stdout, "You may add custom computational code to this button,\n");
  fprintf(stdout, "but currently no action is performed.\n\n");
/* Uncomment the following to inspect the contents of the postmaster here. */
  pm_dump();
}
