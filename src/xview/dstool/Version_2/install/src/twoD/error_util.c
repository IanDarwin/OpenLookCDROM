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
 * error_util.c - macros and utility functions for error handling
 * CONTENTS:
 * error_notice()  alerts user to simple errors
 * error_notice_option()  alerts user to simple errors and allows two options.
 * 	The function returns TRUE if the default option is chosen
 *	and FALSE if the user chooses the alternate option.
 */
#include <stdio.h>
#include <sys/param.h>
#include <sys/types.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/textsw.h>
#include <xview/xv_xrect.h>
#include <xview/notice.h>

#include <gdd.h>
#include <constants.h>



/* 
 * error_notice()  alerts user to simple errors
 * Last Modified: 28 January 1991  fjw
 */

error_notice(panel,msg)
Panel	panel;
char	*msg;
{
notice_prompt(panel, NULL,
  NOTICE_MESSAGE_STRINGS, msg, NULL,
  NOTICE_TRIGGER, MS_LEFT,
  NULL);
return(TRUE);
}


/* 
 * error_notice_option()  alerts user to simple errors and allows two options.
 * 	The function returns TRUE if the default option is chosen
 *	and FALSE if the user chooses the alternate option.
 * Last Modified: 29 January 1991  fjw
 */

int 
error_notice_option(panel, msg, default_opt, other_opt)
Panel	panel;
char	*msg;		/* error message to be displayed */
char	*default_opt;   /* default option button (eg, "Cancel") */
char	*other_opt;     /* alternative option button (eg, "Overwrite") */
{
int	result;

result = notice_prompt(panel, NULL,
            NOTICE_MESSAGE_STRINGS, msg, NULL,
            NOTICE_BUTTON_YES,      default_opt,
            NOTICE_BUTTON_NO,       other_opt,
            NULL);
if (result == NOTICE_YES)
    return(TRUE);
else
    return(FALSE);
}
