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
 * sys_panels.c
 *
 * Edit this file to add additional items to the dstool panelmenu.
 *
 */
#include <stdio.h>
#include <sys/param.h>
#include <sys/types.h>
#include <xview/xview.h>
#include <xview/panel.h>

#include <constants.h>
#include <sys_panels.h>

/* ******************************************* *
 *                                             *
 * DECLARE your system panel functions here.   *
 *                                             *
 * ******************************************* */

extern Menu_item load_handler();
extern void load_field_manager();

extern Menu_item save_handler();

extern Menu_item twoD_handler();
extern void twoD_field_manager();

extern Menu_item orbit_handler();
extern void orbit_field_manager();

extern Menu_item mult_handler();
extern void mult_field_manager();

extern Menu_item periodic_handler();
extern void periodic_field_manager();

extern Menu_item selected_handler();
extern void sel_field_manager();

extern Menu_item function_handler();
extern void funct_field_manager();

extern Menu_item defaults_handler();
extern void def_field_manager();

extern Menu_item prop_batch_handler();
extern void prop_field_manager();

extern Menu_item cmd_batch_handler();
extern void cmd_field_manager();

/* ******************************************* *
 *                                             *
 * ADD entries for the system panels here.     *
 *                                             *
 ********************************************* */

struct Panel_Def FILE_PANELS[] = {
  {"Load...", load_handler, load_field_manager},
  {"Save...", save_handler, NULL},
  {"", NULL, NULL}
};

struct Panel_Def VIEW_PANELS[] = {
  {"2D View...", twoD_handler, twoD_field_manager},
  {"", NULL, NULL}
};

struct Panel_Def PANELS[] = {
  {"Orbits...", orbit_handler, orbit_field_manager},
  {"Multiple...", mult_handler, mult_field_manager},
  {"Fixed Points...", periodic_handler, periodic_field_manager},
  {"", NULL, NULL}
};

struct Panel_Def SET_PANELS[] = {
  {"Selected...", selected_handler, sel_field_manager},
  {"Function...", function_handler, funct_field_manager},
  {"Defaults...", defaults_handler, def_field_manager},
  {"", NULL, NULL}
};

struct Panel_Def SUBSID_PANELS[] = {
/*  {"Cmd", NULL, cmd_field_manager},
  {"Prop", NULL, prop_field_manager},*/
  {"Cmd", cmd_batch_handler, cmd_field_manager},
  {"Prop", prop_batch_handler, prop_field_manager},
  {"", NULL, NULL}
};

/* ******************************************* *
 *                                             *
 * DO NOT MODIFY THE FOLLOWING LINES.          *
 *                                             *
 ********************************************* */

int NUM_FILE_PANELS = sizeof(FILE_PANELS) / sizeof(struct Panel_Def);
int NUM_VIEW_PANELS = sizeof(VIEW_PANELS) / sizeof(struct Panel_Def);
int NUM_PANELS = sizeof(PANELS) / sizeof(struct Panel_Def);
int NUM_SET_PANELS = sizeof(SET_PANELS) / sizeof(struct Panel_Def);
int NUM_SUBSID_PANELS = sizeof(SUBSID_PANELS) / sizeof(struct Panel_Def);

