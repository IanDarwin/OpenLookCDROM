/*
 *
 * 	cl_events.c
 * 	evenements des fenetres de couleur
 *
 * 	Modification :  27/01/94
 *
 *	Copyright (c) 1993,1994 Bruno RIVAS
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

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include <X11/keysym.h>

#include "bm/warning.bm"
#include <X11/iman/widgets.h>
#include <X11/iman/windows.h>
#include <X11/iman/messages.h>


#include "iman.h"
extern char *rgb_names[];









/*
 *
 * Boucle principale
 *
 *
 */

int CL_Events()
{
 XEvent send_event;
 XSetWindowAttributes attrib;
 XGCValues xgcvalues;
 XWMHints wmhints;
 XColor colorcell_def, rgb_def;

 unsigned long ptr;
 int mask;
 int ret;
 int i, j, k, w, index;
 unsigned long win_type, *widcolorsptr, *widindexptr;


 switch(tk_event.ev_type)
 {

   case JUSTFOCUS :

	 if(tk_event.ev_widget==WI_BUTTON&&(tk_event.button==bn_colors_ok||tk_event.button==bn_colors_cancel||tk_event.button==bn_colors_map||tk_event.button==bn_colors_help)&&windows[wm_colors_index].state.isMapped==True&&windows[wm_desktop_index].state.isMapped==True&&windows[wm_colors_index].state.isOnTop==False)
	   WIN_MapRaised(wm_colors_index);
	 else if(tk_event.ev_widget==WI_LIST&&tk_event.list==ls_colors_items&&windows[wm_colors_index].state.isMapped==True&&windows[wm_desktop_index].state.isMapped==True&&windows[wm_colors_index].state.isOnTop==False)
	   WIN_MapRaised(wm_colors_index);
	 else for(i=0;i<10;i++)
	 {
           if(tk_event.ev_widget==WI_BUTTON&&tk_event.button==bn_colors_widgets[i]&&windows[wm_colors_index].state.isMapped==True&&windows[wm_desktop_index].state.isMapped==True&&windows[wm_colors_index].state.isOnTop==False)
	     WIN_MapRaised(wm_colors_index);
	 }
	 break;


   case BN_PUSHED :

	 if((tk_event.button==bn_colors_ok||tk_event.button==bn_colors_cancel||tk_event.button==bn_colors_map||tk_event.button==bn_colors_help)&&windows[wm_colors_index].state.isMapped==True&&windows[wm_desktop_index].state.isMapped==True&&windows[wm_colors_index].state.isOnTop==False)
	   WIN_MapRaised(wm_colors_index);
	 else for(i=0;i<10;i++)
           if(tk_event.button==bn_colors_widgets[i]&&windows[wm_colors_index].state.isMapped==True&&windows[wm_desktop_index].state.isMapped==True)
	   {
	     for(j=0;j<9;j++)
		if(j!=i) wid_SetState(tk_display,bn_colors_widgets[j],Unpushed);
	     wid_SetState(tk_display,bn_colors_widgets[i],Blocked+Pushed);
	     if(i==0) CL_SetupDesk();
	     else if(i==1) CL_SetupWin();
	     else if(i==2) CL_SetupDialog();
	     else if(i==3) CL_SetupIcon();
	     else if(i==4) CL_SetupButton();
	     else if(i==5) CL_SetupScroll();
	     else if(i==6) CL_SetupEdit();
	     else if(i==7) CL_SetupList();
	     else if(i==8) CL_SetupMenu();
	     if(i==8) i=9;
	     cl_manager.current_widget=i;
	     cl_manager.current_item=-1;
	     wid_SetPosition(tk_display,ls_colors_items,0); if(windows[wm_colors_index].state.isMapped==True&&windows[wm_desktop_index].state.isMapped==True&&windows[wm_colors_index].state.isOnTop==False)
	       WIN_MapRaised(wm_colors_index);
	     return 0;
	   }
	 break;


   case BN_RELEASED :

	 if(tk_event.button==bn_colors_cancel || tk_event.button==bn_colors_ok)
	 {  
	   if(tk_event.button==bn_colors_ok)
	     CL_SavePreferences();
	   /*else CL_GetPreferences();*/  
	   CL_Close();
	   return 0;
	 }
	 else if(tk_event.button==bn_colors_map)
	 {  
		  WIN_ResizeClient(wm_colors_index,525,370);
		  wid_SetState(tk_display,bn_colors_map,Grayed);
		  wid_GiveFocus(tk_display,ls_colors_items);
		  return 0;
	 }
	 for(i=0;i<10;i++)
         if(tk_event.ev_widget==WI_BUTTON&&tk_event.button==bn_colors_widgets[i]&&windows[wm_colors_index].state.isMapped==True&&windows[wm_desktop_index].state.isMapped==True)
	 {
	     if(windows[wm_colors_index].state.isOnTop==False) WIN_MapRaised(wm_colors_index);
	     return 0;
	 }
	break;	


   case LS_KEYUNKNOWN :
   case BN_KEYUNKNOWN :

	if(tk_event.event.xkey.keycode==XKeysymToKeycode(tk_display->display,XK_Escape))
	{
	  CL_GetPreferences();  
	  CL_Close();
	  return 0;	
	}
	break;


   case LS_CLICKED:
   case LS_DOUBLECLICKED:
   case LS_VALIDATION:
   case LS_RELEASED:
	if(windows[wm_colors_index].state.isMapped==True&&windows[wm_desktop_index].state.isMapped==True&&windows[wm_colors_index].state.isOnTop==False)
	   WIN_MapRaised(wm_colors_index);
        
	if(tk_event.list==ls_colors_items)
	{

	  k=wid_GetPosition(tk_display,ls_colors_items);
	  /*fprintf(stderr,"position=%d\n",k);*/
	  cl_manager.current_item=k;

          if(cl_manager.current_widget==0)
	  {
	  	if(k==0) index=cl_manager.desk_fg;  
	  	else if(k==1) index=cl_manager.desk_bg;  
	  	cl_manager.current_color=index;  
		if(k==0) index=cl_index.desk_fg;  
	  	else if(k==1) index=cl_index.desk_bg;  
		wid_SetPosition(tk_display,ls_colors_rgb_names,index);
	  }
          else if(cl_manager.current_widget==1)
	  {
	 	widcolorsptr=(unsigned long *)&cl_manager.win_colors;
		widindexptr=(unsigned long *)&cl_index.win_colors;
 	 	cl_manager.current_color=widcolorsptr[k];
		wid_SetPosition(tk_display,ls_colors_rgb_names,widindexptr[k]);
	  }
          else if(cl_manager.current_widget==2)
	  {
	 	widcolorsptr=(unsigned long *)&cl_manager.dlg_colors;
		widindexptr=(unsigned long *)&cl_index.dlg_colors;
 	 	cl_manager.current_color=widcolorsptr[k];
		wid_SetPosition(tk_display,ls_colors_rgb_names,widindexptr[k]);
	  }
	  else if(cl_manager.current_widget==3)
	  {	 	
		widcolorsptr=(unsigned long *)&cl_manager.icn_colors;
		widindexptr=(unsigned long *)&cl_index.icn_colors;
 	 	cl_manager.current_color=widcolorsptr[k];
		wid_SetPosition(tk_display,ls_colors_rgb_names,widindexptr[k]);
	  }
	  else if(cl_manager.current_widget==4)
	  {
	 	widcolorsptr=(unsigned long *)&cl_manager.bn_colors;
		widindexptr=(unsigned long *)&cl_index.bn_colors;
 	 	cl_manager.current_color=widcolorsptr[k];
		wid_SetPosition(tk_display,ls_colors_rgb_names,widindexptr[k]);
	  }
	  else if(cl_manager.current_widget==5)
	  {
	 	widcolorsptr=(unsigned long *)&cl_manager.sb_colors;
		widindexptr=(unsigned long *)&cl_index.sb_colors;
 	 	cl_manager.current_color=widcolorsptr[k];
		wid_SetPosition(tk_display,ls_colors_rgb_names,widindexptr[k]);  
	  }
	  else if(cl_manager.current_widget==6)
	  {
	 	widcolorsptr=(unsigned long *)&cl_manager.ed_colors;
		widindexptr=(unsigned long *)&cl_index.ed_colors;
 	 	cl_manager.current_color=widcolorsptr[k];
		wid_SetPosition(tk_display,ls_colors_rgb_names,widindexptr[k]);  
	  }
	  else if(cl_manager.current_widget==7)
	  {
	 	widcolorsptr=(unsigned long *)&cl_manager.ls_colors;
		widindexptr=(unsigned long *)&cl_index.ls_colors;
 	 	cl_manager.current_color=widcolorsptr[k];
		wid_SetPosition(tk_display,ls_colors_rgb_names,widindexptr[k]);  
	  } 
	  else if(cl_manager.current_widget==9)
	  {
	 	widcolorsptr=(unsigned long *)&cl_manager.mn_colors;
		widindexptr=(unsigned long *)&cl_index.mn_colors;
 	 	cl_manager.current_color=widcolorsptr[k];
		wid_SetPosition(tk_display,ls_colors_rgb_names,widindexptr[k]);  
	  }
	  XSetWindowBackground(tk_display->display,color_rgb,cl_manager.current_color);
	  XClearWindow(tk_display->display,color_rgb);
	}
	else if(tk_event.list==ls_colors_rgb_names)
	{
	  if(CL_IsUsed(cl_manager.current_color)==False)
	    XFreeColors(tk_display->display,default_colormap,(unsigned long *)&cl_manager.current_color,1,0);
	  k=wid_GetPosition(tk_display,ls_colors_rgb_names);
	  j=wid_GetPosition(tk_display,ls_colors_items);
	  ret=XAllocNamedColor(tk_display->display,default_colormap,rgb_names[k],&colorcell_def,&rgb_def);
	  if(ret==1)
	  {
	     XSetWindowBackground(tk_display->display,color_rgb,colorcell_def.pixel);
	     XClearWindow(tk_display->display,color_rgb);
	     cl_manager.current_color=colorcell_def.pixel;

	     if(cl_manager.current_widget==0)
	     {
	 	if(j==0)
		  cl_manager.desk_fg=cl_manager.current_color;
		if(j==0)
		  cl_index.desk_fg=k;
		if(j==1)
		  cl_manager.desk_bg=cl_manager.current_color;
		if(j==1)
		  cl_index.desk_bg=k;
		CL_DrawDesktop(j);
	     }	  
	     else if(cl_manager.current_widget==1)
	     {
	 	widcolorsptr=(unsigned long *)&cl_manager.win_colors;
		widindexptr=(unsigned long *)&cl_index.win_colors;
 	 	widcolorsptr[j]=cl_manager.current_color;
		widindexptr[j]=k;
		CL_DrawWindow(j);
	     }	  
	     else if(cl_manager.current_widget==2)
	     {
	 	widcolorsptr=(unsigned long *)&cl_manager.dlg_colors;
		widindexptr=(unsigned long *)&cl_index.dlg_colors;
 	 	widcolorsptr[j]=cl_manager.current_color;
		widindexptr[j]=k;
		CL_DrawDialog(j);
	     }	  
	     else if(cl_manager.current_widget==3)
	     {
	 	widcolorsptr=(unsigned long *)&cl_manager.icn_colors;
		widindexptr=(unsigned long *)&cl_index.icn_colors;
 	 	widcolorsptr[j]=cl_manager.current_color;
		widindexptr[j]=k;
		CL_DrawIcon(j);
	     }	  
	     else if(cl_manager.current_widget==4)
	     {
	 	widcolorsptr=(unsigned long *)&cl_manager.bn_colors;
		widindexptr=(unsigned long *)&cl_index.bn_colors;
 	 	widcolorsptr[j]=cl_manager.current_color;
		widindexptr[j]=k;
		CL_DrawButton(j);
	     }	  
    	     else if(cl_manager.current_widget==5)
	     {
	 	widcolorsptr=(unsigned long *)&cl_manager.sb_colors;
		widindexptr=(unsigned long *)&cl_index.sb_colors;
		widcolorsptr[j]=cl_manager.current_color;
		widindexptr[j]=k;
		CL_DrawScroll(j);
	 	
	     }
	     else if(cl_manager.current_widget==6)
	     {
	 	widcolorsptr=(unsigned long *)&cl_manager.ed_colors;
		widindexptr=(unsigned long *)&cl_index.ed_colors;
		widcolorsptr[j]=cl_manager.current_color;
		widindexptr[j]=k;
		CL_DrawEdit(j);
	     }
	     else if(cl_manager.current_widget==7)
	     {
	 	widcolorsptr=(unsigned long *)&cl_manager.ls_colors;
		widindexptr=(unsigned long *)&cl_index.ls_colors;
		widcolorsptr[j]=cl_manager.current_color;
		widindexptr[j]=k;
		CL_DrawList(j);
	     }
	     else if(cl_manager.current_widget==9)
	     {
	 	widcolorsptr=(unsigned long *)&cl_manager.mn_colors;
		widindexptr=(unsigned long *)&cl_index.mn_colors;
		widcolorsptr[j]=cl_manager.current_color;
		widindexptr[j]=k;
		CL_DrawMenu(j);
	 	
	     }

	  } 
 	  else
	  {
	     colorcell_def.pixel=BlackPixel(tk_display->display,tk_display->screen);
	     XSetWindowBackground(tk_display->display,color_rgb,colorcell_def.pixel);
	     XClearWindow(tk_display->display,color_rgb);
	     cl_manager.current_color=colorcell_def.pixel;
	  } 

	}
	break;




  }
  return 0;
}









void CL_Close()
{
  int i;

  XUnmapWindow(tk_display->display,windows[wm_colors_index].mainwindow);
  XUnmapWindow(tk_display->display,wm_colors_window);
  wid_SetPosition(tk_display,ls_colors_rgb_names,0); 

  i=wm_colors_index;
  windows[i].state.isInitialized=True; 
  windows[i].state.isMapped=False;
  windows[i].state.isOnTop=False;
  /*WIN_ResizeClient(i,267,370);*/

  i=wm_desktop_index;
  windows[i].state.isInitialized=True; 
  windows[i].state.isMapped=True;
  windows[i].state.isOnTop=True;
  windows[i].state.isFrozen=False;
  windows[i].freezing.leader=0;
  windows[i].freezing.number=-1;

  cl_manager.current_color=-1;
  cl_manager.current_item=-1;
  cl_manager.current_widget=0;
  WID_Unfreeze(tk_display,wm_desktop_window);
  WIN_MapRaised(wm_desktop_index);
  WIN_GiveFocus(wm_desktop_index);
  XSync(tk_display->display,False);

}



