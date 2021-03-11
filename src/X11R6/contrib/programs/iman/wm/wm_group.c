/*
 *
 * 	wm_group.c
 * 	gestion des groupes
 *
 * 	Modification :  11/11/93
 *
 *	Copyright (c) 1993 Bruno RIVAS
 *	All Rights Reserved
 *
 *
 * Permission to use, copy, modify, distribute and sell this software and its
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


/*
#define DEBUG	1
*/

#include "iman.h"





int WIN_AddToGroup(number)
int number;
{

 XWMHints *xwmhints;
 XEvent return_event;
 int leader;
 
 int i, j;
 int mask;


 if(number<0 || number>=maxwindows)
  return -2;

 if(windows[number].transient.isTransient==True && windows[number].transient.number>=0)
   return -1;


 i=WM_GetWindowNumber(windows[number].group.leader);
#ifdef DEBUG
 fprintf(stderr,"Group number:%d\n",i);
#endif
 if(i>=0 && i<maxwindows && windows[i].isUsed==True)
 {

   windows[number].group.number=i;
   switch(windows[i].class)
   { 

	case TOP_LEVEL_WINDOW:
	case DIALOG_BOX :
		
		if(windows[i].group.nummembers>=windows[i].group.maxmembers)
		{
#ifdef DEBUG
		  fprintf(stderr,"Ajout de groupes\n");
#endif
		  if(windows[i].group.maxmembers==0)
		  { 
			windows[i].group.members=(unsigned int *)malloc(sizeof(int)*10);
			memset(windows[i].group.members,0,sizeof(int)*10);
		        windows[i].group.maxmembers=10;
		        windows[i].group.nummembers=0;
		  }
		  else
		  { 
			windows[i].group.members=(unsigned int *)realloc(windows[i].group.members,sizeof(int)*(10+windows[i].group.maxmembers));
			windows[i].group.maxmembers=windows[i].group.maxmembers+10;
		  }
		  if(windows[i].group.members==NULL) return -1;
#ifdef DEBUG
 		  fprintf(stderr,"Fin d'ajout de membres\n");
#endif
		}


		windows[i].group.members[windows[i].group.nummembers]=number;
		windows[i].group.nummembers++;
		
		if((windows[i].attributes&GroupLeader)!=GroupLeader) 
		  windows[i].attributes+=GroupLeader;
		j=windows[number].state.isMapped;

#ifdef DEBUG
		fprintf(stderr,"Avant les verifs...\n");
#endif
		if(WIN_VerifyTree(i)!=0)
		{
#ifdef DEBUG
		  fprintf(stderr,"On unmap all\n");
#endif
		  WIN_UnmapAll(number);
		  windows[number].state.isMapped=j;
		}
		else
		{
#ifdef DEBUG
		  fprintf(stderr,"Verify Tree = OK\n");
#endif
		  if(windows[number].state.isMapped==True)
		    WIN_Map(number);
		}

#ifdef DEBUG
		fprintf(stderr,"OK ................\n");
#endif
		break;


	default: fprintf(stderr,"Group class inconnue: %d \n",windows[i].class);
		 goto GROUP_ERROR;
   		 break;
   }
 }

 else if(wm_action.type!=ReparentingAction)
 {
GROUP_ERROR:

#ifdef DEBUG
   fprintf(stderr,"Group error \n");
#endif

   windows[number].group.leader=0;
   if((windows[number].attributes&GroupMember)==GroupMember)
     windows[number].attributes-=GroupMember;
   windows[number].group.number=-1;
   return -1;
 }

#ifdef DEBUG
 fprintf(stderr,"fin d'adding group\n");
#endif

 return 0;
}






int WIN_UnlinkGroup(number,map)
unsigned int number;
Bool map;
{
 int i, j, k;

 if(number<=0 || number>=maxwindows)
  return -2; 

 if(windows[number].group.nummembers>0)
 for(i=0;i<windows[number].group.nummembers;i++)
 {																										
   if(windows[windows[number].group.members[i]].isUsed==True&&windows[windows[number].group.members[i]].group.leader==windows[number].clientwindow&&(windows[windows[number].group.members[i]].attributes&GroupMember)==GroupMember)
   {
     windows[windows[number].group.members[i]].group.leader=0;
     windows[windows[number].group.members[i]].group.number=-1;
     windows[windows[number].group.members[i]].attributes=windows[windows[number].group.members[i]].attributes-GroupMember;
 
     if(map==True && windows[windows[number].group.members[i]].state.isMapped==True)
     {
	XMapWindow(tk_display->display,windows[windows[number].group.members[i]].clientwindow);
	XMapWindow(tk_display->display,windows[windows[number].group.members[i]].mainwindow);
	windows[windows[number].group.members[i]].state.isMapped==True;
	WIN_Map(windows[number].group.members[i]);
     }

   }
 }

 if(windows[number].group.maxmembers>0&&windows[number].group.members!=NULL)
   free(windows[number].group.members);
 windows[number].group.nummembers=0;
 windows[number].group.maxmembers=0;
 if((windows[number].attributes&GroupLeader)==GroupLeader)
   windows[number].attributes=windows[number].attributes-GroupLeader;
 return 0;
}






int WIN_RemoveFromGroup(number,map)
unsigned int number;
Bool map;
{
  int i, j, k;


  if(number>=maxwindows)
    return -2; 


  i=windows[number].group.number;
  if(i>=0&&i<maxwindows)
  {
#ifdef DEBUG
    fprintf(stderr,"Remove %d  from group: %d  nummembers:%d  maxmembers:%d\n",number,i,windows[i].group.nummembers,windows[i].group.maxmembers);
#endif
    j=0;
    while(j<windows[i].group.nummembers && windows[i].group.members[j]!=number)
      j++;
    if(j<windows[i].group.nummembers)
    {
      for(k=j+1;k<windows[i].group.nummembers;k++)
 	windows[i].group.members[k-1]=windows[i].group.members[k];
      windows[i].group.nummembers--;
      windows[number].group.leader=0;
      windows[number].group.number=-1;
      if((windows[number].attributes&GroupMember)==GroupMember)
        windows[number].attributes-=GroupMember;

/****** A REVOIR ******/

      if(map==True&&windows[number].isUsed==True&&windows[number].state.isMapped==True)
      {
	XMapWindow(tk_display->display,windows[number].clientwindow);
	XMapWindow(tk_display->display,windows[number].mainwindow);
	windows[number].state.isMapped==True;
	WIN_Map(number);
      }
      return 0;
    }
    windows[number].group.leader=0;
    windows[number].group.number=-1;
    if((windows[number].attributes&GroupMember)==GroupMember)
      windows[number].attributes-=GroupMember;
    return -1;
  }
  else
  { 
#ifdef DEBUG
	fprintf(stderr,"Remove ((())) %d  from group: %d  nummembers:%d  maxmembers:%d\n",number,i,windows[i].group.nummembers,windows[i].group.maxmembers);
#endif
	windows[number].group.leader=0;
	windows[number].group.number=-1;
        if((windows[number].attributes&GroupMember)==GroupMember)
   	  windows[number].attributes-=GroupMember;
	return -1;
  }
}


