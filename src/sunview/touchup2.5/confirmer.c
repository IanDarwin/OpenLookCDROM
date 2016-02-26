
/**************************************************************************
   Touchup a bitmap graphics editor for the Sun Workstation running SunView
   Copyright (c) 1988 by Raymond Kreisel
   1/22/88 @ Suny Stony Brook

   This program may be redistributed without fee as long as this copyright
   notice is intact.

==> PLEASE send comments and bug reports to one of the following addresses:

	   Ray Kreisel
	   CS Dept., SUNY at Stony Brook, Stony Brook NY 11794

	   UUCP: {allegra, philabs, pyramid, research}!sbcs!rayk   
	   ARPA-Internet: rayk@sbcs.sunysb.edu			
	   CSnet: rayk@suny-sb

 "If I get home before daylight, I just might get some sleep tonight...."

**************************************************************************/
/**************************************************************************
	file: confirmer.c
	purpose: This file contains a simple confirmer copied from the
	 Sunview manual that has been souped up a little

	modifications:
		date:	Tue Mar 22 22:04:58 EST 1988
		author:	rayk
		changes:add comments

		date:	Sun May 15 21:21:01 EDT 1988
		author:	rayk
		changes:rewrote the confirmer so that it is NOT created
			dynamiclly to try to advoid the error in the
			window creation (no more file descriptors)


**************************************************************************/

#include "header.h"

/***************************************************************
        confirm
        purpose: To display a window on the base_frame Sunwindow
                force the user to answer the question by selecting
		either YES or NO
        parameter:
                message: The question to asked.
        returns:
		1 : if the user answered YES
		0 : if the user answered NO
 ***************************************************************/
extern Panel_item con_msg_string;

int confirm(message)
char     *message; 
{
   window_bell(panel);
   confirm_msg(message);
   return((int)window_loop(confirmer));
}


confirm_msg(string)
char *string;
{
char temp_space[55];
char *temp_pt;
int i;
  
  if (strlen(string) < 50)
  {
    for(i=0;i<50;i++)
      temp_space[i]= ' ';
    temp_pt = temp_space + (50-strlen(string))/2;
    strcpy(temp_pt,string);
    panel_set(con_msg_string,PANEL_LABEL_STRING,temp_space,0);
  }
  else
    panel_set(con_msg_string,PANEL_LABEL_STRING,string,0);
}



/*
 * yes/no notify proc 
 */
void yes_no(item)
Panel_item   item;
{
        window_return(panel_get(item, PANEL_CLIENT_DATA));
}
