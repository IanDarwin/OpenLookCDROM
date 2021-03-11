/*
 *
 * 	wm_iconify.c
 * 	iconification/des-iconification
 *
 * 	Modification :  11/11/93
 *
 *	Copyright (c) 1993 Bruno RIVAS
 *	All Rights Reserved
 *
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Bruno RIVAS not be used in advertising
 * or publicity pertaining to distribution of the software without specific,
 * written prior permission.  Bruno RIVAS makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * Bruno RIVAS disclaims all warranties with regard to this software,
 * including all implied warranties of merchantability and fitness,
 * in no event shall Bruno RIVAS be liable for any special,
 * indirect or consequential damages or any damages
 * whatsoever resulting from loss of use, data or profits,
 * whether in an action of contract, negligence or other tortious
 * action, arising out of or in connection with the use 
 * or performance of this software.
 *
 *
 *	Bruno RIVAS 
 *      IMAN Window Manager version 1.2
 *
 *	Internet:       rivas@email.teaser.com
 *	Surface Mail:   Bruno RIVAS,
 *			30 avenue Marie
 *			93250 Villemomble -- FRANCE
 *	Voice phone:    (33) (1) 49.35.97.27
 *	Fax: 		(33) (1) 48.94.27.91
 *
 */



#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <malloc.h>
#include <memory.h>

#include "X11/Xlib.h"
#include "X11/Xutil.h"
#include "X11/Xos.h"


#include <X11/iman/widgets.h>
#include <X11/iman/windows.h>
#include <X11/iman/messages.h>

#include "iman.h"






int WIN_Iconify(number)
unsigned int number;
{
  int *members;
  int nummembers, maxmembers;
  int i, j;

ICONIFY_START:

  if(windows[number].state.isFrozen==True)
    return -1;

  if((windows[number].attributes&GroupLeader)!=GroupLeader&&(windows[number].attributes&GroupMember)==GroupMember&&windows[number].group.leader>0&&windows[number].group.number>=0)
  {														
    number=windows[number].group.number;
    if(windows[number].state.isFrozen==True||windows[number].class==DIALOG_BOX)
      return -1;
  }

  XUnmapWindow(tk_display->display,windows[number].mainwindow);
  XUnmapWindow(tk_display->display,windows[number].clientwindow);
  WIN_DrawUnactive(number);

#ifdef DEBUG
  fprintf(stderr,"Passe number:%d\n",number);
#endif

  if((windows[number].attributes&GroupLeader)==GroupLeader)
  {

#ifdef DEBUG
    fprintf(stderr,"Icon GroupLeader\n");
    /*WIN_UnmapAll(number);*/
#endif
    members=(unsigned int *)malloc(sizeof(unsigned int)*10);
    if(members==NULL) return -2;
    members[0]=number;
    nummembers=1;
    maxmembers=10;
#ifdef DEBUG
    fprintf(stderr,"Avant submembers nummembers:%d\n",nummembers);
#endif
    WIN_GetSubmembers(number,&members,&nummembers,&maxmembers,-1);
#ifdef DEBUG
    fprintf(stderr,"Submembers passe  nummembers:%d\n",nummembers);
#endif
    
    for(i=0;i<nummembers;i++)
    {
#ifdef DEBUG
	fprintf(stderr,"i=%d  number:%d\n",i,members[i]);
#endif
	if(windows[members[i]].state.isIconic==True&&windows[members[i]].class==TOP_LEVEL_WINDOW&&members[i]!=number)
	{
#ifdef DEBUG
	  fprintf(stderr,"Iconic window\n");
#endif
	  XUnmapWindow(tk_display->display,windows[members[i]].icon.window);
  	  XUnmapWindow(tk_display->display,windows[members[i]].icon.draw_area);
	  if(windows[members[i]].icon.hasIconWindow==True)
  	    XUnmapWindow(tk_display->display,windows[members[i]].icon.clientwindow);
	}
	else
	{
#ifdef DEBUG
	  fprintf(stderr,"normal window\n");
#endif
  	  XUnmapWindow(tk_display->display,windows[members[i]].mainwindow);
  	  /*XUnmapWindow(tk_display->display,windows[members[i]].clientwindow);	  */
	  WIN_DrawUnactive(members[i]);
	}
	windows[members[i]].state.isOnTop=False;
    }
#ifdef DEBUG
    fprintf(stderr,"Avant free\n");    
#endif
    free(members);
  }
  windows[number].state.isMapped=False;
  windows[number].state.isOnTop=False;
  windows[number].state.isIconic=True;

  if(wm_info.set_icons==True)
  {
#ifdef DEBUG
    fprintf(stderr,"Set_icons\n");
#endif
    XMapWindow(tk_display->display,windows[number].icon.window);
    XMapWindow(tk_display->display,windows[number].icon.draw_area);
    if(wm_info.set_icontitle==True) XMapWindow(tk_display->display,windows[number].icon.title);
    else XUnmapWindow(tk_display->display,windows[number].icon.title);
    if(windows[number].icon.hasIconWindow==True)  XMapRaised(tk_display->display,windows[number].icon.clientwindow);
    /*WIN_SetClientState(number,IconicState);*/
  }
  /*else WIN_SetClientState(number,IconicState);*/
#ifdef DEBUG
  fprintf(stderr,"ICN_Map\n");
#endif
  ICN_MapRaised(number);

  return 0;

}





int WIN_Uniconify(number)
unsigned int number;
{
  int *members;
  int nummembers, maxmembers;
  int i, j;


  XUnmapWindow(tk_display->display,windows[number].icon.window);
  XUnmapWindow(tk_display->display,windows[number].icon.draw_area);
  if(windows[number].icon.hasIconWindow==True)
    XUnmapWindow(tk_display->display,windows[number].icon.clientwindow);

  if((windows[number].attributes&GroupLeader)==GroupLeader&&windows[number].state.isWithdrawn==True)
  {
    windows[number].state.isIconic=False;
    windows[number].state.isWithdrawn=True;
  }
  else
  {  
    windows[number].state.isMapped=True;
    windows[number].state.isWithdrawn=False;
    windows[number].state.isIconic=False;
  }

  if((windows[number].attributes&GroupLeader)==GroupLeader)
  {
    members=(unsigned int *)malloc(sizeof(unsigned int)*10);
    if(members==NULL) return -2;
    members[0]=number;
    nummembers=1;
    maxmembers=10;
    WIN_GetSubmembers(number,&members,&nummembers,&maxmembers,-1);

    WIN_MapRaised(number);    


    if(nummembers>1) for(i=1;i<nummembers;i++)
    {
	if(windows[i].state.isIconic==True&&WIN_VerifyTree(number)==0)
	  WIN_Iconify(i);
	else if(windows[i].state.isIconic!=IconicState&&windows[i].state.isMapped==True&&WIN_VerifyTree(i)==0)
	{
          if(windows[i].state.isZoomed==False) WIN_SetClientState(i,NormalState);
	  else WIN_SetClientState(i,ZoomState);
	}
    }
    free(members);
  }
  else WIN_MapRaised(number);    

  return 0;
}



