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
 *
 * windows.c
 *
 * Procedures:
 *   rebuild_windows()
 *
 */
#include <stdio.h>

#include <portability.h>

#ifdef HAS_STRINGS_H
#include <strings.h>
#endif

#ifdef HAS_STRING_H
#include <string.h>
#endif


#include <constants.h>
#include <pm.h>
#include "ui_init.h"
#include "sys_panels.h"
#include "user_panels.h"

static char *WINDOWS_OBJ_NAME = "Win";
static char *WINDOW_NAMES_OBJ_NAME = "Win_Names";

static char *WINDOWS[] = {
    "Win.Current",
    "Win.Open_Current",
    "Win.Close_Current"
};

typedef enum {
    CURRENT=0,OPENCURRENT,CLOSECURRENT
} WINDOWS_t;


void
    windows_install_init()
{
    int 
	i;

    int
	close_current_window(),
	open_current_window();

    void
	twoD_pm_install();

    pm(CREATE_OBJ, WINDOWS_OBJ_NAME,
       PUT_SAVABLE,WINDOWS_OBJ_NAME,SAVE_CONFIG,
       CREATE_ELEM,WINDOWS[CURRENT],STRNG,
       INIT,WINDOWS[CURRENT],MAX_LABEL_LEN,
       CREATE_ELEM,WINDOWS[OPENCURRENT],FNCT,
       PUT,WINDOWS[OPENCURRENT],open_current_window,
       CREATE_ELEM,WINDOWS[CLOSECURRENT],FNCT,
       PUT,WINDOWS[CLOSECURRENT],close_current_window,
       CREATE_OBJ, WINDOW_NAMES_OBJ_NAME,
       NULL);


    for(i=0; i<NUM_FILE_PANELS; i++) 
	register_window(FILE_PANELS[i].name,
			FILE_PANELS[i].handler,
			FILE_PANELS[i].field_manager);


    for(i=0; i<NUM_PANELS; i++)
	register_window(PANELS[i].name,
			PANELS[i].handler,
			PANELS[i].field_manager);


    for(i=0; i<NUM_SET_PANELS; i++)
	register_window(SET_PANELS[i].name,
			SET_PANELS[i].handler,
			SET_PANELS[i].field_manager);


    for(i=0; i<NUM_USER_PANELS; i++)
	register_window(USER_PANELS[i].name,
			USER_PANELS[i].handler,
			USER_PANELS[i].field_manager);


    for (i=0; i<NUM_SUBSID_PANELS; i++)
	register_window(SUBSID_PANELS[i].name,
			SUBSID_PANELS[i].handler,
			SUBSID_PANELS[i].field_manager);

    twoD_pm_install();

}

void
  rebuild_windows()
{
  int i;

  if (!cmd_ip) return;

  for(i=0; i<NUM_FILE_PANELS; i++)
    if (FILE_PANELS[i].field_manager != NULL)
      FILE_PANELS[i].field_manager();

  for(i=0; i<NUM_VIEW_PANELS; i++)
    if (VIEW_PANELS[i].field_manager != NULL)
      VIEW_PANELS[i].field_manager();

  for(i=0; i<NUM_PANELS; i++)
    if (PANELS[i].field_manager != NULL)
      PANELS[i].field_manager();

  for(i=0; i<NUM_SET_PANELS; i++)
    if (SET_PANELS[i].field_manager != NULL)
      SET_PANELS[i].field_manager();

  for(i=0; i<NUM_USER_PANELS; i++)
    if (USER_PANELS[i].field_manager != NULL)
      USER_PANELS[i].field_manager();

  for (i=0; i<NUM_SUBSID_PANELS; i++)
    if (SUBSID_PANELS[i].field_manager != NULL)
      SUBSID_PANELS[i].field_manager();

  for(i=get_min_twoD(); i<=get_max_twoD(); i++) 
    if ( valid_twoD_id(i) ) 
      {
	twoD_field_manager(i);
	opt_field_manager(i);
      }

  refresh_all_win();
  twoD_pm_reset();
}

/*
   Record the window and type (POPUP_WINDOW or BASE_WINDOW)
   of window_name.
*/
int	
  register_win_and_type(window_name,win,type)

char
   *window_name;
Xv_opaque
   win;
int
   type;
{
    char
	label[MAX_LABEL_LEN];
    
    sprintf(label,"Win.Handle.%s",window_name);
    pm(CREATE_ELEM,label,ADDRS,
       PUT,label,win, 
       NULL);
    sprintf(label,"Win.Type.%s",window_name);
    pm(CREATE_ELEM,label, INT,
       PUT_SAVABLE,label, SAVE_NONE,
       PUT,label,type,
       NULL);
}


/*
  Register a window with given open status, window handler, and field manager.
  The latter two, if NULL, are not updated. The postmaster entries affected
  are Win.Open_Status.<window_name>.
  Win.Handler.<window_name>, and
  Win.Field_Manager.<window_name>.

*/
int
    register_window(window_name,win_handler,win_field_manager) /*,
		    win_close_proc)*/
char
    *window_name;
Menu_item 
    (*win_handler)();    /* the notifier for the menu item */
void 
    (*win_field_manager)();    /* the panel's field manager */
/*
int
    (*win_close_proc)();*/	/* procedure for closing the window */


{
    extern int
      pm_exists_entry();
    char
	*leading_token(),
	window_short_name[MAX_LABEL_LEN],
	*short_label,
	label[MAX_LABEL_LEN];
    int
	i,
	status;

    if (strcmp(window_name,"")) {
	leading_token(window_short_name,window_name,MAX_LONG_STR-1);
   
	sprintf(label,"%s.%s",WINDOW_NAMES_OBJ_NAME,window_short_name);

	/* add window name entry if not already present */
	if (! pm_entry_exists(label)) {

	    /* Adjust short_label to <window_short_name> */
	    short_label = (char *) label; 
	    for(i=0; i < (int) strlen(WINDOW_NAMES_OBJ_NAME) +1 ;i++)
		++short_label;
	
	    if ((int) strlen(short_label) > 0)
		status = *((int *) pm(CREATE_ELEM,label,STRNG,
				      INIT,label,MAX_LABEL_LEN,
				      PUT,label,short_label,
				      PUT_SAVABLE,label,SAVE_CONFIG,
				      NULL));

	    /* mark window as having no recorded location */
	    sprintf(label,"%s.%s.%s",WINDOWS_OBJ_NAME,"Locn",window_short_name);
	    status = *((int *) pm(CREATE_ELEM,label,INT_LIST,
				  INIT,label,4,
				  PUT,label,0,NO_LOCATION,
				  NULL));

	    sprintf(label,"%s.%s.%s",WINDOWS_OBJ_NAME,"Open_Status",window_short_name);
	    status = *((int *) pm(CREATE_ELEM,label,INT,
				  PUT,label,FALSE,
				  NULL));
	}
    }

    if (win_handler != (Menu_item (*)()) NULL) {
	sprintf(label,"%s.%s.%s",WINDOWS_OBJ_NAME,"Handler",window_short_name);
	status = *((int *) pm(CREATE_ELEM,label,FNCT,
			      PUT,label,win_handler,
			      NULL));
    }

    if (win_field_manager != (void (*)()) NULL) {
	sprintf(label,"%s.%s.%s",WINDOWS_OBJ_NAME,"Field_Manager",window_short_name);
	status = *((int *) pm(CREATE_ELEM,label,FNCT,
			      PUT,label,win_field_manager,
			      NULL));
    }
/*
    if (win_close_proc != (int (*)()) NULL) {
	sprintf(label,"%s.%s.%s.%d",WINDOWS_OBJ_NAME,"Close_Proc",window_short_name,instance_number);
	status = *((int *) pm(CREATE_ELEM,label,FNCT,
			      PUT,label,win_close_proc,
			      NULL));
    }
*/
    return status;
}

/*
  Set the postmaster entry Win.Open_Status.<window_name> to TRUE.
*/
int
    mark_window_open(window_short_name)
char
    *window_short_name;



{
    int
	status;
    char
	label[MAX_LABEL_LEN];

    sprintf(label,"%s.%s.%s",WINDOWS_OBJ_NAME,"Open_Status",window_short_name);
    status = *((int *) pm(PUT,label,TRUE,
			  NULL));
    
    return status;
}


/*
  Set the postmaster entry Win.Open_Status.<window_name> to FALSE.
*/
int
    mark_window_closed(window_short_name)
char
    *window_short_name;

{
    int
	status;
    char
	label[MAX_LABEL_LEN];

    sprintf(label,"%s.%s.%s",WINDOWS_OBJ_NAME,"Open_Status",window_short_name);
    status = *((int *) pm(PUT,label,FALSE,
			  NULL));
    
    return status;
}

/*
   open the window named by Win.Current
*/
int
    open_current_window()

{
    char
	*pm_result,
	compound_label[MAX_LABEL_LEN],
	win_name[MAX_LABEL_LEN];

    Menu_item	
	item = (Menu_item) NULL;
    Menu_generate	
	op = MENU_NOTIFY;

    Menu_item 
	(*win_handler)();

    int
	status;

    /* get current window */
    pm_result = (char *) pm(GET,"Win.Current",win_name,NULL);
    if (! pm_result)
	return MINOR_ERROR;

    /* call window handler with the right arg */

    sprintf(compound_label,"%s.%s.%s",WINDOWS_OBJ_NAME,"Handler",win_name);
    if (! pm_entry_exists(compound_label) ) 
      return MINOR_ERROR;
    else
      win_handler = (void *) pm(GET,compound_label,NULL);

    if(!win_handler)
	return MINOR_ERROR;
    else
	win_handler(item,op);
    
    return NO_ERROR;
}

/*
   close the window named by Win.Current
*/
int
    close_current_window()

{
    void 
	*pm_result;
    int
	win_type;
    char
	compound_label[MAX_LABEL_LEN],
	win_name[MAX_LABEL_LEN];

    Xv_opaque		ip_win;

    /* get current window */
    pm_result = (void *) pm(GET,"Win.Current",win_name,NULL);
    if (! pm_result)
	return MINOR_ERROR;
/*
    sprintf(compound_label,"%s.%s.%s",WINDOWS_OBJ_NAME,"Close_Proc",win_name);
    pm_result = (void *) pm(EXEC,compound_label,NULL);
*/

    sprintf(compound_label,"%s.%s.%s","Win","Handle",win_name);
    ip_win = (Xv_opaque) pm(GET,compound_label,NULL);

    if (!ip_win)
	return MINOR_ERROR;
    else {
	sprintf(compound_label,"%s.%s.%s",WINDOWS_OBJ_NAME,"Open_Status",win_name);
	pm_result = (void *) pm(PUT,compound_label,FALSE,
			  NULL);
	/* test should be replaced by check whether the frame is a base frame of
	   frame_cmd */
	sprintf(compound_label,"%s.%s.%s",WINDOWS_OBJ_NAME,"Type",win_name);
	win_type = *((int *) pm(GET,compound_label,NULL));
	if (win_type == POPUP_WINDOW)
	    xv_set(ip_win, FRAME_CMD_PUSHPIN_IN, FALSE, NULL);
	xv_set(ip_win, XV_SHOW, FALSE, NULL);
    }

    /* avoid accidental re-use */
    sprintf(compound_label,"%s.%s.%s",WINDOWS_OBJ_NAME,"Locn",win_name);
    pm(PUT,compound_label,0,NO_LOCATION,NULL);


}
