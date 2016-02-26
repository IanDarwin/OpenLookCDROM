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
#ifndef SYS_PANELS_HEADER
#define SYS_PANELS_HEADER

/*
 * data structure for hooking in new panels
 *
 */

struct Panel_Def {
  char* name;                /* the name to appear on the menu */
  Menu_item (*handler)();    /* the notifier for the menu item */
  void (*field_manager)();    /* the panel's field manager */
} Panel_Def;

extern struct Panel_Def FILE_PANELS[];
extern int NUM_FILE_PANELS;

extern struct Panel_Def VIEW_PANELS[];
extern int NUM_VIEW_PANELS;

extern struct Panel_Def PANELS[];
extern int NUM_PANELS;

extern struct Panel_Def SET_PANELS[];
extern int NUM_SET_PANELS;

extern struct Panel_Def SUBSID_PANELS[];
extern int NUM_SUBSID_PANELS;

#endif
