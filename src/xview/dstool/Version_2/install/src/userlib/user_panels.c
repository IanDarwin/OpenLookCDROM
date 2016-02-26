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
/*
 * user_panels.c
 *
 * Edit this file to add additional items to the dstool panelmenu.
 *
 * PLEASE EDIT WITH CARE!
 *
 * paw  8/20/91
 */
#include <stdio.h>
#include <sys/param.h>
#include <sys/types.h>
#include <xview/xview.h>
#include <xview/panel.h>

#include <constants.h>
#include <user_panels.h>

/* ******************************************* *
 *                                             *
 * DECLARE your user panel functions here.     *
 *                                             *
 * ******************************************* */

extern Menu_item browser_handler();
extern void browser_field_manager();

extern Menu_item oned_handler();
extern void oned_field_manager();

extern Menu_item lbmain_handler();
extern void lbmain_field_manager();

extern Menu_item cont_handler();
extern void cont_field_manager();

extern Menu_item porbit_handler();
extern void porbit_field_manager();

extern Menu_item autoif_handler();
extern void autoif_field_manager();

extern Menu_item geomview_handler();
extern void geomview_field_manager();

extern Menu_item tclwin_handler();
extern void tclwin_field_manager();



extern Menu_item example_handler();
/*
extern Menu_item trial_handler();
extern void int trial_field_manager();
*/

/* ******************************************* *
 *                                             *
 * ADD an entry for your user panel here.      *
 *                                             *
 ********************************************* */

struct Panel_Def USER_PANELS[] = {
  {"Browser...",browser_handler, browser_field_manager},
  {"One-D Mappings...", oned_handler, oned_field_manager},
#ifdef USING_FORTRAN
  {"LOCBIF Interface...", lbmain_handler, lbmain_field_manager},
#endif
  {"Continuation...", cont_handler, cont_field_manager},
   {"AUTO Read...", autoif_handler, autoif_field_manager},
 {"Periodic Orbits...", porbit_handler, porbit_field_manager},
  {"Geomview...", geomview_handler, geomview_field_manager},
#ifdef USING_TCL
  {"Tcl...", tclwin_handler, tclwin_field_manager},
#endif
  {"Example",example_handler, NULL},
/*  {"Trial...",trial_handler, trial_field_manager}, */

  {"", NULL, NULL}
};

/* ******************************************* *
 *                                             *
 * DO NOT MODIFY THE FOLLOWING LINE.           *
 *                                             *
 ********************************************* */

int NUM_USER_PANELS = sizeof(USER_PANELS) / sizeof(struct Panel_Def);


