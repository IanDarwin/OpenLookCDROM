/*
 *
 * 	wm_map.c
 * 	gestion de l'empillage des fenetres
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
#include <memory.h>
#include <malloc.h>


#include "X11/Xlib.h"
#include "X11/Xutil.h"
#include "X11/Xos.h"
#include "X11/Xatom.h"


#include <X11/iman/widgets.h>
#include <X11/iman/windows.h>
#include <X11/iman/messages.h>

#include "iman.h"



/*
#define DEBUG	
*/






int WIN_MapRaised(number)
int number;
{
  int i, j, k, l, m;
  int ret, pos, win_type;
  int oldtop;
  Window qt_root, qt_parent, *qt_children;
  XEvent send_event;

  unsigned int qt_numchildren=0;
  Bool give_focus=True;

  int nummembers, maxmembers;
  int *members;

  int current;

  int *parents;
  int numparents, maxparents;

  int *children;
  int numchildren, maxchildren;

  Window *restack;


WIN_MAP_START :

  if(number<0 || number>=maxwindows || windows[number].isUsed==False)
    return -1;


#ifdef DEBUG1
  fprintf(stderr,"Group:%ld  attributes:%d  numgroupmembers:%d  maxgroupmembers:%d\n",windows[number].group,windows[number].attributes,windows[number].group.nummembers,windows[number].group.maxmembers);
#endif

  if(windows[number].state.isFrozen==True&&windows[number].freezing.leader>0&&windows[number].freezing.number>=0) 
    goto WIN_MAP_SUITE;

  if(IsAnyOnTop()==True)
  {
     oldtop=-1;
     oldtop=WIN_GetOnTop();
#ifdef DEBUG
     fprintf(stderr,"IsOnTop==True  oldtop=%d\n",oldtop);
#endif
     if(oldtop>=0 && oldtop<maxwindows)
     {
       	if(oldtop!=number)
   	{
	    windows[oldtop].state.isOnTop=False;
	    WIN_DrawUnactive(oldtop);
 
	    if((windows[number].attributes&GroupLeader)==GroupLeader && windows[number].state.isWithdrawn==True)	    
	      goto WIN_MAP_SUITE;
	    windows[number].state.isOnTop=True;
	    WIN_DrawActive(number);
	    give_focus=True;     
	}
	else if( oldtop==number)
      	{
	    windows[number].state.isOnTop=True;
	    WIN_DrawActive(number);
	    give_focus=False;
   	}
	goto WIN_MAP_SUITE;
     }
     else
     {
	if((windows[number].attributes&GroupLeader)==GroupLeader && windows[number].state.isWithdrawn==True)	    
	  goto WIN_MAP_SUITE;
	windows[number].state.isOnTop=True;
	WIN_DrawActive(number);
	give_focus=True;
	goto WIN_MAP_SUITE;
     }
  }
  else
  {
	if((windows[number].attributes&GroupLeader)==GroupLeader && windows[number].state.isWithdrawn==True)	    
	  goto WIN_MAP_SUITE;
        windows[number].state.isOnTop=True;
        WIN_DrawActive(number);
        give_focus=True;
	goto WIN_MAP_SUITE;
  }



WIN_MAP_SUITE:
#ifdef DEBUG 
  fprintf(stderr,"WIN map suite\n");
#endif

/*
 *
 * CAS 1 
 * groupleader=False
 * groupmember=False
 * frozen=False
 * transient=False
 *
 */

  if((windows[number].attributes&GroupLeader)!=GroupLeader && (windows[number].attributes&GroupMember)!=GroupMember&&windows[number].state.isFrozen==False&&windows[number].transient.isTransient==False) 
  {
   CASE1_START:

#ifdef DEBUG   
   fprintf(stderr,"CAS 1: number:%d  oldtop:%d  numchildren:%d maxwindows:%d\n",number,oldtop,qt_numchildren,maxwindows);
#endif

   windows[number].state.isMapped=True;
   windows[number].state.isOnTop=True;
   ret=XMapRaised(tk_display->display,windows[number].clientwindow);
   if(ret==BadWindow) return -1;
   XMapRaised(tk_display->display,windows[number].mainwindow);

   ret=0;
   if(give_focus==True) ret=WIN_GiveFocus(number);
   XInstallColormap(tk_display->display,windows[number].identity.colormap);
   XSync(tk_display->display,False);

   CASE1_END:
     return ret;
  }



/*
 *
 * CAS 2 
 * groupmember=True et/ou groupleader=True et/ou transient=True
 * frozen=False
 *
 *
 */

  else if(((windows[number].attributes&GroupLeader)==GroupLeader&&windows[number].state.isFrozen==False)||((windows[number].attributes&GroupMember)==GroupMember&&windows[number].state.isFrozen==False)||(windows[number].transient.isTransient==True&&windows[number].transient.leader>0&&windows[number].state.isFrozen==False)) 
  {
CASE2_START:

#ifdef DEBUG1   
    fprintf(stderr,"CAS 2: number:%d oldtop:%d  numchildren:%d maxwindows:%d\n",number,oldtop,qt_numchildren,maxwindows);
#endif


    


    if((windows[number].attributes&GroupLeader)==GroupLeader && windows[number].state.isWithdrawn==True)
    {
      goto CASE2_VERIFY;
    }
    else
    {
      windows[number].state.isMapped=True;
      windows[number].state.isWithdrawn=False;
      windows[number].state.isOnTop=True;

    }
CASE2_VERIFY:
    if(WIN_VerifyTree(number)!=0)
      return -2;

    if((windows[number].attributes&GroupLeader)==GroupLeader && windows[number].state.isWithdrawn==True)
    {
      members=(int *)malloc(sizeof(int));
      if(members==NULL)
      {
	fprintf(stderr,"members==NULL\n");
	return -2;
      }
      nummembers=0;
      maxmembers=1;
      pos=-1;
    }
    else
    {
      members=(int *)malloc(sizeof(int));
      if(members==NULL)
      {
	fprintf(stderr,"members==NULL\n");
	return -2;
      }
      nummembers=1;
      maxmembers=1;
      pos=0;
      members[0]=number;
    }




    if((windows[number].attributes&GroupLeader)==GroupLeader&& windows[number].state.isWithdrawn==False)
    {
#ifdef DEBUG
	fprintf(stderr,"GROUPLEADER *****\n");
#endif
	XQueryTree(tk_display->display,RootWindow(tk_display->display,tk_display->screen),&qt_root,&qt_parent,&qt_children,&qt_numchildren);
#ifdef DEBUG
	fprintf(stderr,"Query tree passe numchildren:%d\n",qt_numchildren);
#endif

	if(windows[number].group.nummembers>0&&qt_numchildren>0&&qt_children!=NULL)
	for(j=qt_numchildren-1;j>=0;j--)
   	{
	     k=0;
	     while(windows[windows[number].group.members[k]].mainwindow!=qt_children[j] && k<windows[number].group.nummembers)
	       k++;

	     if(k>=0&&k<windows[number].group.nummembers)
	     {						
	       if(windows[windows[number].group.members[k]].isUsed==True&&windows[windows[number].group.members[k]].state.isMapped==True&& (windows[windows[number].group.members[k]].attributes&AlwaysOnTop)!=AlwaysOnTop&&(windows[number].attributes&Overlapped)!=Overlapped)
	       {
#ifdef DEBUG
		 fprintf(stderr,"cas 0\n");
#endif
	         children=(int *)malloc(sizeof(int)); 
    		 if(children==NULL)
    		 {
			fprintf(stderr,"children==NULL\n");
			if(members!=NULL) free(members);
			if(qt_numchildren>0&&qt_children!=NULL) XFree((char *)qt_children);
			return -2;
		 }
	         numchildren=1;
	         maxchildren=1;
	       	 children[0]=windows[number].group.members[k];
	
	         WIN_GetSubmembers(children[0],&children,&numchildren,&maxchildren,-1);
	         OrderSubmembers(&children,&numchildren,&maxchildren);
	
	         members=(int *)realloc(members,sizeof(int)*(maxmembers+numchildren));
	         
    		 if(members==NULL)
    		 {
			fprintf(stderr,"members==NULL\n");
			if(children!=NULL) free(children);
			if(qt_numchildren>0&&qt_children!=NULL) XFree((char *)qt_children);
			return -2;
		 }
	         maxmembers=maxmembers+numchildren;
	
	         for(m=0;m<numchildren;m++)
	         {
	       	   	members[nummembers]=children[m];
	       		nummembers++;
	         }
	
	         free(children);
	
	       }																									
	       else if(windows[windows[number].group.members[k]].isUsed==True&&windows[windows[number].group.members[k]].state.isMapped==True&&(windows[windows[number].group.members[k]].attributes&AlwaysOnTop)==AlwaysOnTop)
	       {
#ifdef DEBUG
		 fprintf(stderr,"cas 1\n");
#endif
	         children=(int *)malloc(sizeof(int)); 
	         if(children==NULL)
    		 {
			fprintf(stderr,"children==NULL\n");
			if(members!=NULL) free(members);
			if(qt_numchildren>0&&qt_children!=NULL) XFree((char *)qt_children);
			return -2;
		 }
	         numchildren=1;
	         maxchildren=1;
	         children[0]=windows[number].group.members[k];
	       
	         WIN_GetSubmembers(children[0],&children,&numchildren,&maxchildren,-1);
	         OrderSubmembers(&children,&numchildren,&maxchildren);
	
	         members=(int *)realloc(members,sizeof(int)*(maxmembers+numchildren));
	         if(members==NULL)
    		 {
			fprintf(stderr,"children==NULL\n");
			if(children!=NULL) free(children);
			if(qt_numchildren>0&&qt_children!=NULL) XFree((char *)qt_children);
			return -2;
		 }
	         maxmembers=maxmembers+numchildren;
	
	         for(l=nummembers;l>pos;l--)
	           members[l+numchildren-1]=members[l-1];
	         for(l=0;l<numchildren;l++)
	           members[l+pos]=children[l];
	         pos+=numchildren;
	         nummembers+=numchildren;
	
	         free(children);
	       }																			
	       else if(windows[windows[number].group.members[k]].isUsed==True&&windows[windows[number].group.members[k]].state.isMapped==True &&(windows[number].attributes&Overlapped)==Overlapped)
	       {
#ifdef DEBUG
		 fprintf(stderr,"cas 2\n");
#endif
	         children=(int *)malloc(sizeof(int)); 
    		 if(children==NULL)
    		 {
			fprintf(stderr,"children==NULL\n");
			if(children!=NULL) free(children);
			if(qt_numchildren>0&&qt_children!=NULL) XFree((char *)qt_children);
			return -2;
		 }
	         numchildren=1;
	         maxchildren=1;
	         children[0]=windows[number].group.members[k];
	       
	         WIN_GetSubmembers(children[0],&children,&numchildren,&maxchildren,-1);
	         OrderSubmembers(&children,&numchildren,&maxchildren);
	
	         members=(int *)realloc(members,sizeof(int)*(maxmembers+numchildren));
    		 if(members==NULL)
    		 {
			fprintf(stderr,"children==NULL\n");
			if(children!=NULL) free(children);
			if(qt_numchildren>0&&qt_children!=NULL) XFree((char *)qt_children);
			return -2;
		 }
	         maxmembers=maxmembers+numchildren;
	
	         for(l=nummembers;l>pos;l--)
		   members[l+numchildren-1]=members[l-1];

	         for(l=0;l<numchildren;l++)
		   members[l+pos]=children[l];
		 
	         pos+=numchildren;
	         nummembers+=numchildren;
	
	         free(children);
	       }
	     
	  }
	}

#ifdef DEBUG
	fprintf(stderr,"Fin du grouleader\n");
#endif
        if(qt_numchildren>0&&qt_children!=NULL) XFree((char *)qt_children);
    }
    else if((windows[number].attributes&GroupLeader)==GroupLeader&& windows[number].state.isWithdrawn==True)
	WIN_GetSubmembers(number,&members,&nummembers,&maxmembers,-1);
    



		/**** Recherche des ant‚c‚dents ****/


#ifdef DEBUG
    fprintf(stderr,"Je vais rechercher le group leader\n");
#endif


PRE_CURRENT2:
    current=number;

CURRENT2:

    if((windows[current].attributes&GroupMember)==GroupMember&&windows[current].group.leader>0&&windows[current].group.number>=0)
    {
    	parents=(int *)malloc(sizeof(int));
	if(parents==NULL)
	{
		fprintf(stderr,"parent== NULL\n");
		if(members!=NULL) free(members);
		return -2;
	}

        if(windows[windows[current].group.number].state.isMapped==False)
        {
 	  numparents=0;
	  maxparents=1;
	}	
	else
	{
	  parents[0]=windows[current].group.number;
 	  numparents=1;
	  maxparents=1;
	}

	WIN_GetSubmembers(windows[current].group.number,&parents,&numparents,&maxparents,current);
	OrderSubmembers(&parents,&numparents,&maxparents);

	members=(int *)realloc(members,sizeof(int)*(maxmembers+numparents));
	maxmembers=maxmembers+numparents;
	if(members==NULL)
	{
		fprintf(stderr,"members_type realloc == NULL\n");
		if(parents!=NULL) free(parents);
		return -2;
	}

	for(j=0;j<numparents;j++)
	  members[nummembers+j]=parents[j];

	nummembers=nummembers+numparents;
	free(parents);
	current=windows[current].group.number;
	goto CURRENT2;
    }
																
    else if(windows[current].transient.isTransient==True&&windows[current].transient.number>=0&&windows[current].transient.leader>0)
    {
	parents=(int *)malloc(sizeof(int));
	if(parents==NULL)
	{
		fprintf(stderr,"parents == NULL\n");
		if(members!=NULL) free(members);
		return -2;
	}

        if(windows[windows[current].transient.number].state.isMapped==False)
        {
 	  numparents=0;
	  maxparents=1;
	}	
	else
	{
	  parents[0]=windows[current].transient.number;
 	  numparents=1;
	  maxparents=1;
	}


	WIN_GetSubmembers(windows[current].transient.number,&parents,&numparents,&maxparents,current);
	OrderSubmembers(&parents,&numparents,&maxparents);

	members=(int *)realloc(members,sizeof(int)*(maxmembers+numparents));
	maxmembers=maxmembers+numparents;
	if(members==NULL)
	{
		fprintf(stderr,"members_type realloc == NULL\n");
		if(parents!=NULL) free(parents);
		return -2;
	}

	for(j=0;j<numparents;j++)
	  members[nummembers+j]=parents[j];
	nummembers=nummembers+numparents;
	free(parents);
	current=windows[current].transient.number;
	goto CURRENT2;
    }





#ifdef DEBUG
    fprintf(stderr,"Je vais restacker  nummembers:%d  maxmembers:%d\n",nummembers,maxmembers);
    for(i=0;i<nummembers;i++)
   	fprintf(stderr,"Index:%d\n",members[i]);
#endif


RESTACK2:

    restack=(Window *)malloc(sizeof(Window)*nummembers);
    if(restack==NULL)
    {
	fprintf(stderr,"Restack == NULL\n");
	free(members);
	return -2;
    }
    for(i=0;i<nummembers;i++)
    {
	XMapRaised(tk_display->display,windows[members[i]].clientwindow);
	restack[i]=windows[members[i]].mainwindow;
    }

#ifdef DEBUG
    fprintf(stderr,"Restack en cours ... nummembers:%d\n",nummembers);
#endif

    XMapRaised(tk_display->display,restack[0]);
    XRestackWindows(tk_display->display,restack,nummembers);
    for(i=1;i<nummembers;i++)
      XMapWindow(tk_display->display,windows[members[i]].mainwindow);

    if(windows[number].transient.isTransient==True&&windows[number].transient.leader>0&&windows[windows[number].transient.number].state.isFrozen==False)
    {
     	send_event.type=ClientMessage;
    	send_event.xclient.type==ClientMessage;
    	send_event.xclient.message_type=tk_display->atoms._IMAN_WM_MESSAGES;
    	send_event.xclient.window=windows[number].transient.leader;
    	send_event.xclient.format=32;
    	send_event.xclient.data.l[0]=WmFreezeWidgets;
    	XSendEvent(tk_display->display,windows[number].transient.leader,False,0,&send_event);
    	windows[windows[number].transient.number].state.isFrozen=True;
    	windows[windows[number].transient.number].freezing.number=number;
    	windows[windows[number].transient.number].freezing.leader=windows[number].clientwindow;    
    }

    XInstallColormap(tk_display->display,windows[number].identity.colormap);
    XSync(tk_display->display,False);
#ifdef DEBUG
    fprintf(stderr,"CAS 2 : libere la memoire\n");
#endif

CASE2_END:
    if((windows[number].attributes&GroupLeader)==GroupLeader && windows[number].state.isWithdrawn==True)
    {
	i=WM_FindTopestWindow();
        if(i>=0 && i<maxwindows && windows[i].isUsed==True)
	{
	  windows[i].state.isMapped=True;
	  windows[i].state.isOnTop=True;
	  WIN_DrawActive(i);
	}
    }
    else
      WIN_GiveFocus(number);
    
    free(restack);
    free(members);
    XSync(tk_display->display,False);
#ifdef DEBUG
    fprintf(stderr,"CAS 2 : FIN\n");
#endif
    return 0;
   
  }




/*
 *
 * CAS 3 
 * frozen=True
 *
 *
 */

  else if(windows[number].state.isFrozen==True) 
  {

CASE3_START:
#ifdef DEBUG   
    fprintf(stderr,"CAS 3: number:%d oldtop:%d  maxwindows:%d\n",number,oldtop,maxwindows);
#endif


    windows[number].state.isMapped=True;
  
    
    if(WIN_VerifyTree(number)!=0)
      return -2;

    current=number;

CASE3_LOOP:

    if(windows[current].state.isFrozen==True&&windows[current].freezing.leader>0&&windows[current].freezing.number>=0)
    {
	current=windows[current].freezing.number;
	goto CASE3_LOOP;
    }
    else
    {
	windows[current].state.isFrozen=False;
	windows[current].freezing.leader=0;
	windows[current].freezing.number=-1;
    }
    number=current;
    goto WIN_MAP_START;
  }



/*
 *
 * CAS INCONNU
 *
 *
 */

  else
  { 
	fprintf(stderr,"WIN cas inconnu\n");
	windows[number].state.isMapped=True;
	XMapWindow(tk_display->display,windows[number].clientwindow);
	XMapRaised(tk_display->display,windows[number].mainwindow);
	WIN_GiveFocus(number);
	XSync(tk_display->display,False);
	return 0;
  }

}






int WIN_Map(number)
int number;
{
  int i, j, k, l, m;
  int ret, pos;
  int oldtop;
  Window qt_root, qt_parent, *qt_children;
  XEvent send_event;
  unsigned int qt_numchildren=0;

  int nummembers, maxmembers;
  int *members;

  int current;

  int *parents;
  int numparents, maxparents;

  int *children;
  int numchildren, maxchildren;

  Window *restack;


WIN_MAP_START :

  if(number<0 || number>=maxwindows || windows[number].isUsed==False)
    return -1;


#ifdef DEBUG   
  fprintf(stderr,"Group:%ld  attributes:%d  numgroupmembers:%d  maxgroupmembers:%d\n",windows[number].group,windows[number].attributes,windows[number].group.nummembers,windows[number].group.maxmembers);
#endif




WIN_MAP_SUITE:
#ifdef DEBUG 
  fprintf(stderr,"WIN map suite\n");
#endif

/*
 *
 * CAS 1 
 * groupleader=False
 * groupmember=False
 * transient=False
 * frozen=False
 *
 */

  if((windows[number].attributes&GroupLeader)!=GroupLeader&&(windows[number].attributes&GroupMember)!=GroupMember&&windows[number].transient.isTransient==False&&windows[number].state.isFrozen==False) 
  {
   CASE1_START:

#ifdef DEBUG   
   fprintf(stderr,"CAS 1: number:%d  oldtop:%d  numchildren:%d maxwindows:%d\n",number,oldtop,qt_numchildren,maxwindows);
#endif

   windows[number].state.isMapped=True;
   ret=XMapWindow(tk_display->display,windows[number].clientwindow);
   if(ret==BadWindow) return -1;
   XMapWindow(tk_display->display,windows[number].mainwindow);

   ret=0;
   XInstallColormap(tk_display->display,windows[number].identity.colormap);
   XSync(tk_display->display,False);

   CASE1_END:
     return ret;
  }



/*
 *
 * CAS 2 
 * groupmember=True et/ou groupleader=True et/ou transient=True
 * frozen=False
 *
 *
 */
								
  else if(((windows[number].attributes&GroupLeader)==GroupLeader||(windows[number].attributes&GroupMember)==GroupMember||(windows[number].transient.isTransient==True&&windows[number].transient.leader>0))&&windows[number].state.isFrozen==False) 
  {
CASE2_START:
#ifdef DEBUG   
    fprintf(stderr,"CAS 2: number:%d oldtop:%d  numchildren:%d maxwindows:%d\n",number,oldtop,qt_numchildren,maxwindows);
#endif
    if((windows[number].attributes&GroupLeader)==GroupLeader && windows[number].state.isWithdrawn==True)
    {
      goto VERIFY_2;
    }
    else
    {
      windows[number].state.isMapped=True;
      windows[number].state.isWithdrawn=False;
    }
VERIFY_2:
    if(WIN_VerifyTree(number)!=0)
      return -2;

    if((windows[number].attributes&GroupLeader)==GroupLeader && windows[number].state.isWithdrawn==True)
    {
      members=(int *)malloc(sizeof(int));
      if(members==NULL)
      {
	fprintf(stderr,"members==NULL\n");
	return -2;
      }
      nummembers=0;
      maxmembers=1;
      pos=-1;
    }
    else
    {
      members=(int *)malloc(sizeof(int));
      if(members==NULL)
      {
	fprintf(stderr,"members==NULL\n");
	return -2;
      }
      nummembers=1;
      maxmembers=1;
      pos=0;
      members[0]=number;
    }

    if((windows[number].attributes&GroupLeader)==GroupLeader)
    {
         WIN_GetSubmembers(number,&members,&nummembers,&maxmembers,-1);
         OrderSubmembers(&members,&nummembers,&maxmembers);

    }





		/**** Recherche des ant‚c‚dents ****/


#ifdef DEBUG
    fprintf(stderr,"Je vais rechercher le group leader\n");
#endif


PRE_CURRENT2:
    current=number;

CURRENT2:

    if((windows[current].attributes&GroupMember)==GroupMember&&windows[current].group.leader>0&&windows[current].group.number>=0)
    {
    	parents=(int *)malloc(sizeof(int));
	if(parents==NULL)
	{
		fprintf(stderr,"parents_type == NULL\n");
		if(members!=NULL) free(members);
		return -2;
	}

        if(windows[windows[current].group.number].state.isMapped==False)
        {
 	  numparents=0;
	  maxparents=1;
	}	
	else
	{
	  parents[0]=windows[current].group.number;
 	  numparents=1;
	  maxparents=1;
	}
	
	WIN_GetSubmembers(windows[current].group.number,&parents,&numparents,&maxparents,current);
	OrderSubmembers(&parents,&numparents,&maxparents);

	members=(int *)realloc(members,sizeof(int)*(maxmembers+numparents));
	maxmembers=maxmembers+numparents;
	if(members==NULL)
	{
		fprintf(stderr,"members_type realloc == NULL\n");
		if(parents!=NULL) free(parents);
		return -2;
	}

	for(j=0;j<numparents;j++)
	  members[nummembers+j]=parents[j];

	nummembers=nummembers+numparents;
	free(parents);
	current=windows[current].group.number;
	goto CURRENT2;
    }

    else if(windows[current].transient.isTransient==True&&windows[current].transient.number>=0&&windows[current].transient.leader>0)
    {
	parents=(int *)malloc(sizeof(int));
	if(parents==NULL)
	{
		fprintf(stderr,"parents== NULL\n");
		if(members!=NULL) free(members);
		return -2;
	}
        if(windows[windows[current].transient.number].state.isMapped==False)
        {
 	  numparents=0;
	  maxparents=1;
	}	
	else
	{
	  parents[0]=windows[current].transient.number;
 	  numparents=1;
	  maxparents=1;
	}

	WIN_GetSubmembers(windows[current].transient.number,&parents,&numparents,&maxparents,current);
	OrderSubmembers(&parents,&numparents,&maxparents);

	members=(int *)realloc(members,sizeof(int)*(maxmembers+numparents));
	maxmembers=maxmembers+numparents;
	if(members==NULL)
	{
		fprintf(stderr,"members realloc == NULL\n");
		if(parents!=NULL) free(parents);
		return -2;
	}

	for(j=0;j<numparents;j++)
	  members[nummembers+j]=parents[j];
	nummembers=nummembers+numparents;
	free(parents);
	current=windows[current].transient.number;
	goto CURRENT2;
    }





#ifdef DEBUG
    fprintf(stderr,"Je vais restacker  nummembers:%d  maxmembers:%d\n",nummembers,maxmembers);
    for(i=0;i<nummembers;i++)
   	fprintf(stderr,"Index:%d\n",members[i]);
#endif


RESTACK2:

    restack=(Window *)malloc(sizeof(Window)*nummembers);
    if(restack==NULL)
    {
	fprintf(stderr,"Restack == NULL\n");
	free(members);
	return -2;
    }
    for(i=0;i<nummembers;i++)
    {
	XMapRaised(tk_display->display,windows[members[i]].clientwindow);
	restack[i]=windows[members[i]].mainwindow;
    }

#ifdef DEBUG
    fprintf(stderr,"Restack en cours ... nummembers:%d\n",nummembers);
#endif

    XMapWindow(tk_display->display,restack[0]);
    XRestackWindows(tk_display->display,restack,nummembers);
    for(i=1;i<nummembers;i++)
      XMapWindow(tk_display->display,windows[members[i]].mainwindow);

    if(windows[number].transient.isTransient==True&&windows[number].transient.leader>0&&windows[windows[number].transient.number].state.isFrozen==False)
    {
     	send_event.type=ClientMessage;
    	send_event.xclient.type==ClientMessage;
    	send_event.xclient.message_type=tk_display->atoms._IMAN_WM_MESSAGES;
    	send_event.xclient.window=windows[number].transient.leader;
    	send_event.xclient.format=32;
    	send_event.xclient.data.l[0]=WmFreezeWidgets;
    	XSendEvent(tk_display->display,windows[number].transient.leader,False,0,&send_event);
    	windows[windows[number].transient.number].state.isFrozen=True;
    	windows[windows[number].transient.number].freezing.number=number;
    	windows[windows[number].transient.number].freezing.leader=windows[number].clientwindow;    
    }

    XInstallColormap(tk_display->display,windows[number].identity.colormap);
    XSync(tk_display->display,False);
#ifdef DEBUG
    fprintf(stderr,"CAS 2 : libere la memoire\n");
#endif

CASE2_END:
    free(restack);
    free(members);
    XSync(tk_display->display,False);
#ifdef DEBUG
    fprintf(stderr,"CAS 2 : FIN\n");
#endif
    return 0;
   
  }




/*
 *
 * CAS 3 
 * frozen=True
 *
 *
 */

  else if(windows[number].state.isFrozen==True) 
  {

CASE3_START:
#ifdef DEBUG   
    fprintf(stderr,"CAS 3: number:%d oldtop:%d  maxwindows:%d\n",number,oldtop,maxwindows);
#endif


    windows[number].state.isMapped=True;
  
    
    if(WIN_VerifyTree(number)!=0)
      return -2;

    current=number;

CASE3_LOOP:

    if(windows[current].state.isFrozen==True&&windows[current].freezing.leader>0&&windows[current].freezing.number>=0)
    {
	current=windows[current].freezing.number;
	goto CASE3_LOOP;
    }
    else
    {
	windows[current].state.isFrozen=False;
	windows[current].freezing.leader=0;
	windows[current].freezing.number=-1;
    }
    number=current;
    goto WIN_MAP_START;
  }



/*
 *
 * CAS INCONNU
 *
 *
 */

  else
  { 
	fprintf(stderr,"WIN cas inconnu\n");
	windows[number].state.isMapped=True;
	XMapWindow(tk_display->display,windows[number].clientwindow);
	XMapWindow(tk_display->display,windows[number].mainwindow);
	XSync(tk_display->display,False);
	return 0;
  }

}





int WIN_Unmap(number)
int number;
{
  XEvent send_event;


  if(windows[number].transient.isTransient==True&&windows[number].transient.leader>0&&windows[windows[number].transient.number].state.isFrozen==True&&windows[windows[number].transient.number].freezing.number==number)
  {
    send_event.type=ClientMessage;
    send_event.xclient.type==ClientMessage;
    send_event.xclient.message_type=tk_display->atoms._IMAN_WM_MESSAGES;
    send_event.xclient.window=windows[number].transient.leader;
    send_event.xclient.format=32;
    send_event.xclient.data.l[0]=WmUnfreezeWidgets;
    XSendEvent(tk_display->display,windows[number].transient.leader,False,0,&send_event);
    windows[windows[number].transient.number].state.isFrozen=False;
    windows[windows[number].transient.number].freezing.number=-1;
    windows[windows[number].transient.number].freezing.leader=0;    
  }

  windows[number].state.isOnTop=False;
  windows[number].state.isMapped=False;
  WIN_DrawUnactive(number);
  XUnmapWindow(tk_display->display,windows[number].mainwindow); 
  XUnmapWindow(tk_display->display,windows[number].clientwindow); 
  return 0;
}





int WIN_UnmapAll(number)
int number;
{
 XEvent send_event;
 int i, j, k, l, m, pos, ret;

 int *members;
 int nummembers, maxmembers;



#ifdef DEBUG
   fprintf(stderr,"**** WIN UNMAP: max:%d\n",windows[number].group.maxmembers);
#endif   

   members=(int *)malloc(sizeof(int)*(windows[number].group.maxmembers+2));
   members[0]=number;
   nummembers=1;
   maxmembers=windows[number].group.maxmembers+2;
   if(members==NULL)
   {
	fprintf(stderr,"Erreur: Pas assez de memoire\n");
	exit(-1);
   }

   if(wm_info.set_groups==True&&wm_action.type!=IconifyAction&&wm_action.type!=KillProcessAction&&wm_action.type!=EndOfAllAction)
     goto UNMAP_LABEL;



       
   WIN_GetSubmembers(members[0],&members,&nummembers,&maxmembers,-1);
   OrderSubmembers(&members,&nummembers,&maxmembers);

#ifdef DEBUG
   fprintf(stderr,"Je vais construire la pile\n");   
#endif
   for(l=0;l<nummembers;l++)
   { 
     XUnmapWindow(tk_display->display,windows[members[l]].mainwindow); 
     XUnmapWindow(tk_display->display,windows[members[l]].clientwindow); 

     if(windows[members[l]].state.isOnTop==True)
     {
         windows[members[l]].state.isOnTop=False;
	 WIN_DrawUnactive(members[l]);
     }
   }


   UNMAP_LABEL:

   if(windows[number].transient.isTransient==True&&windows[number].transient.leader>0&&windows[windows[number].transient.number].state.isFrozen==True&&windows[windows[number].transient.number].freezing.number==number)
   {
     send_event.type=ClientMessage;
     send_event.xclient.type==ClientMessage;
     send_event.xclient.message_type=tk_display->atoms._IMAN_WM_MESSAGES;
     send_event.xclient.window=windows[number].transient.leader;
     send_event.xclient.format=32;
     send_event.xclient.data.l[0]=WmUnfreezeWidgets;
     XSendEvent(tk_display->display,windows[number].transient.leader,False,0,&send_event);
     windows[windows[number].transient.number].state.isFrozen=False;
     windows[windows[number].transient.number].freezing.number=-1;
     windows[windows[number].transient.number].freezing.leader=0;    
   }

   windows[number].state.isOnTop=False;
   windows[number].state.isMapped=False;
   WIN_DrawUnactive(number);
   XUnmapWindow(tk_display->display,windows[number].mainwindow); 



   CASE_END:

    free(members);
    return 0;



   PARENT_END:
	fprintf(stderr,"Erreur: Pas assez de memoire pour arranger les fenetres\n");
	free(members);
	return -2;

}






int WM_FindTopestWindow()
{
  int i, j;

  Window qt_root, qt_parent, *qt_children;
  unsigned int qt_numchildren=0;

#ifdef DEBUG
  fprintf(stderr,"WMFinToplevelFocus\n");
#endif
  if(IsAnyOnTop()==False)
  {
    XQueryTree(tk_display->display,RootWindow(tk_display->display,tk_display->screen),&qt_root,&qt_parent,&qt_children,&qt_numchildren);
  
    for(j=qt_numchildren-1;j>=0;j--)
    {
#ifdef DEBUG
	fprintf(stderr,"j=%d\n",j);
#endif
	i=0;
	while((windows[i].isUsed==False||windows[i].mainwindow!=qt_children[j]||windows[i].state.isMapped==False||windows[i].state.isIconic==True||WIN_VerifyTree(i)!=0)&&i<maxwindows)
	  i++;
	if(i>=0&&i<maxwindows)
	{
#ifdef DEBUG
	  fprintf(stderr,"Nouveau: %d\n",i);
#endif    
    	  if(qt_numchildren<=0) XFree((char *)qt_children);	  
	  return i;
	}						
    }
FIND_SUITE:
    if(qt_numchildren<=0) XFree((char *)qt_children);
    return -1;
  }
  else return -1; 

}






int WM_FindToplevelFocus()
{
  int i, j;

  Window qt_root, qt_parent, *qt_children;
  unsigned int qt_numchildren=0;

#ifdef DEBUG
  fprintf(stderr,"WMFinToplevelFocus\n");
#endif
  if(IsAnyOnTop()==False)
  {
    XQueryTree(tk_display->display,RootWindow(tk_display->display,tk_display->screen),&qt_root,&qt_parent,&qt_children,&qt_numchildren);
  
    for(j=qt_numchildren-1;j>=0;j--)
    {
#ifdef DEBUG
	fprintf(stderr,"j=%d\n",j);
#endif
	i=0;
	while((windows[i].isUsed==False||windows[i].mainwindow!=qt_children[j]||windows[i].state.isMapped==False||windows[i].state.isIconic==True||WIN_VerifyTree(i)!=0)&&i<maxwindows)
	  i++;
	if(i>=0&&i<maxwindows)
	{
#ifdef DEBUG
	  fprintf(stderr,"Nouveau: %d\n",i);
#endif
	  WIN_MapRaised(i);
	  goto FIND_SUITE;
	}						
    }
FIND_SUITE:
    if(qt_numchildren<=0) XFree((char *)qt_children);
    return 0;
  } 
/*  else
  {
    WIN_Map(WM_GetOnTop());
    return 0;
  }*/

}






int WIN_GetSubmembers(number, members, nummembers,maxmembers,excludefrom_number) 
unsigned int number;
int **members;
unsigned int *nummembers, *maxmembers;
int excludefrom_number;
{
 int i, j, k, ret;


#ifdef DEBUG
 fprintf(stderr,"WIN_Sub  exclude:%d  \n",excludefrom_number);
#endif
 if(windows[number].state.isFrozen==True&&windows[number].freezing.leader>0&&windows[number].freezing.number>=0)
 {
   /*fprintf(stderr,"Boite de dialogue detectee\n");*/

   if(excludefrom_number==windows[number].freezing.number)
     goto GET_SUITE;


   *members=(int *)realloc(*members,sizeof(int)*(*maxmembers+1));
   *maxmembers=*maxmembers+1;

   if(*members==NULL)
   {
     fprintf(stderr,"Erreur de memoire\n");
     return -1;
   }
   (*members)[*nummembers]=i=windows[number].freezing.number;
   *nummembers=*nummembers+1;
   
   if((windows[i].attributes&GroupLeader)==GroupLeader||windows[i].state.isFrozen==True)
     ret=WIN_GetSubmembers(i,members,nummembers,maxmembers,excludefrom_number);
   if(ret==-1) return -1;
 } 

 GET_SUITE:


 if((windows[number].attributes&GroupLeader)==GroupLeader&&windows[number].group.nummembers>0)
 {
   *members=(int *)realloc(*members,sizeof(int)*(*maxmembers+windows[number].group.nummembers+1));
   *maxmembers=*maxmembers+windows[number].group.nummembers+1;

   if(*members==NULL)
   {
     fprintf(stderr,"Erreur de memoire\n");
     return -1;
   }
#ifdef DEBUG
   fprintf(stderr,"Memoire allouee *num:%d  *max:%d  members[0]:%d\n",*nummembers,*maxmembers,(*members)[0]);
#endif
   for(i=0;i<windows[number].group.nummembers;i++)
   if(windows[windows[number].group.members[i]].state.isMapped==True&&windows[windows[number].group.members[i]].state.isIconic==False&&windows[windows[number].group.members[i]].isUsed==True&&windows[number].group.members[i]!=excludefrom_number)
   {
 	(*members)[*nummembers]=windows[number].group.members[i];
	*nummembers=*nummembers+1;
#ifdef DEBUG
	fprintf(stderr,"WIN member: %d\n",(*members)[*nummembers-1]);
#endif
	if(((windows[windows[number].group.members[i]].attributes&GroupLeader)==GroupLeader&&windows[windows[number].group.members[i]].group.nummembers>0)||windows[windows[number].group.members[i]].state.isFrozen==True)
	{
  	  ret=WIN_GetSubmembers(windows[number].group.members[i],members,nummembers,maxmembers,excludefrom_number);
	  if(ret==-1) return -1;
	}
   }
 }

#ifdef DEBUG
 fprintf(stderr,"Fin de WIN_GetSubgroups\n");
#endif
 return 0;
}






int OrderSubmembers(members,nummembers,maxmembers)
int **members;
unsigned int *nummembers, *maxmembers;
{
 int i, j;
 int *submembers;
 Window qt_root, qt_parent, *qt_children;
 unsigned int qt_numchildren;
 int cptr=0;




#ifdef DEBUG
  fprintf(stderr,"Order subgroups num:%d  max:%d\n",*nummembers,*maxmembers);
#endif
  submembers=(int *)malloc(sizeof(int)*(*maxmembers));

  if(submembers==NULL)
  {
    fprintf(stderr,"Erreur de memoire\n");
    return -1;
  }
#ifdef DEBUG
  fprintf(stderr,"J'envoie XQueryTree %ld\n",tk_display->display);
#endif
  XQueryTree(tk_display->display,RootWindow(tk_display->display,tk_display->screen),&qt_root,&qt_parent,&qt_children,&qt_numchildren);
#ifdef DEBUG
  fprintf(stderr,"ENTREE: nummembers=%d \n",*nummembers);
#endif

  memcpy(submembers,*members,*maxmembers*sizeof(int));

  cptr=0;
  if(qt_numchildren>0) for(i=qt_numchildren-1;i>=0;i--)
  {
    j=0;
    for(j=0;j<*nummembers;j++)
    {
      if(windows[submembers[j]].mainwindow==qt_children[i])
      {
	(*members)[cptr]=submembers[j];
	cptr++;
	goto BOUCLE_END;
      }	
    }
    BOUCLE_END:
      cptr=cptr;
  }
#ifdef DEBUG
  fprintf(stderr,"Avant d'effacer cptr=%d\n",cptr);
#endif

  free(submembers);
  if(qt_numchildren>0) XFree((char *)qt_children);
  *nummembers=cptr;
#ifdef DEBUG
  fprintf(stderr,"SORTIE: cptr=%d\n",cptr);
#endif

  return 0;
}






int WIN_VerifyTree(number)
int number;
{

 int ret;
 int current;



   current=number;

#ifdef DEBUG
   fprintf(stderr,"WIN verify\n");
   fprintf(stderr,"%d  GroupMember=%d  group.number=%d  group.leader=%ld\n",current,windows[current].attributes&GroupMember,windows[current].group.number,windows[current].group.leader);

#endif




 TREE_START:
   

   if(windows[current].transient.isTransient==True&&windows[current].transient.number>=0 && windows[current].transient.number<maxwindows && windows[current].transient.leader>0)
   {																
#ifdef DEBUG
	fprintf(stderr,"%d isTransient  transient.number=%d  transient.leader=%ld\n",current,windows[current].transient.number,windows[current].transient.leader);	
#endif
	if(windows[windows[current].transient.number].state.isIconic==True||windows[windows[current].transient.number].isUsed==False||(windows[windows[current].transient.number].state.isFrozen==True&&windows[windows[current].transient.number].freezing.number!=current))
	{
#ifdef DEBUG
	   fprintf(stderr,"TRANSIENT WIN VERIFY TREE: -2\n");
#endif
	   return -2;
	}
	current=windows[current].transient.number;
	goto TREE_START;
   }
   else if((windows[current].attributes&GroupMember)==GroupMember && windows[current].group.number>=0 && windows[current].group.number<maxwindows && windows[current].group.leader>0 && windows[current].group.leader!=windows[current].clientwindow && windows[current].group.number!=current)
   {
#ifdef DEBUG
     	fprintf(stderr,"%d GroupMember=%d  group.number=%d  group.leader=%ld\n",current,windows[current].attributes&GroupMember,windows[current].group.number,windows[current].group.leader);
#endif																	
	if(windows[windows[current].group.number].state.isIconic==True||windows[windows[current].group.number].isUsed==False)
	{
#ifdef DEBUG
	   fprintf(stderr,"GROUP WIN VERIFY TREE: -2\n");
#endif
	   return -2;
	}
	current=windows[current].group.number;
	goto TREE_START;
   }


   if((windows[current].attributes&GroupMember)==GroupMember && (windows[current].group.leader==windows[current].clientwindow || windows[current].group.number==current))
   {
#ifdef DEBUG
	fprintf(stderr,"%d Suppression du groupe recursif\n",current);
#endif
	WIN_RemoveFromGroup(current,True);
   }

   if(windows[current].transient.isTransient && (windows[current].transient.leader==windows[current].clientwindow || windows[current].transient.number==current))
   {
#ifdef DEBUG
	fprintf(stderr,"%d Suppression du TRANSIENT recursif\n",current);
#endif
	windows[current].transient.isTransient=False;
	windows[current].transient.number=-1;
	windows[current].transient.leader=0;
   }


   TREE_END:
#ifdef DEBUG
   fprintf(stderr,"WIN VERIFY TREE: 0\n");
#endif
   return 0;

}







int ICN_MapRaised(number)
int number;
{
 int i, j, k;
 int ret;

 Window qt_root, qt_parent, *qt_children;
 unsigned int qt_numchildren;

 Window *restack;
 int nummembers;


  if(number>=0 && number<maxwindows && windows[number].isUsed==True)
  {
    
    XQueryTree(tk_display->display,RootWindow(tk_display->display,tk_display->screen),&qt_root,&qt_parent,&qt_children,&qt_numchildren);
    restack=(Window *)malloc(sizeof(Window)*numwindows+1);

    nummembers=1;
    restack[0]=windows[number].icon.window;

    for(i=qt_numchildren-1;i>=0;i--)
    {

      for(j=0;j<maxwindows;j++)
      if(j!=number&&windows[j].isUsed==True&&(windows[j].attributes&IconifyBox)==IconifyBox&&(windows[j].attributes&GroupMember)!=GroupMember&&windows[j].state.isIconic==True&&windows[j].icon.window==qt_children[i])
      {
	restack[nummembers]=windows[j].icon.window;
	nummembers++;
      }

    }
    XRestackWindows(tk_display->display,restack,nummembers);
    free(restack);    
    if(qt_numchildren>0) XFree((char *)qt_children);
  }
  else return -1;
}





void WM_UnmapSystemWindows()
{
  if((windows[wm_process_index].state.isOnTop==True||windows[wm_process_index].state.isMapped==True))
    WIN_Unmap(wm_process_index); 
  if((windows[wm_about_index].state.isOnTop==True||windows[wm_about_index].state.isMapped==True))
    WIN_Unmap(wm_about_index); 
  if(windows[wm_desktop_index].state.isOnTop==True||windows[wm_desktop_index].state.isMapped==True)
  {
    if(windows[wm_colors_index].state.isMapped==True)
    {
      tk_event.ev_type=BN_RELEASED;
      tk_event.button=bn_colors_cancel;
      CL_Events();
    }
    WIN_UnmapAll(wm_desktop_index);
  }
  if(windows[wm_setup_index].state.isOnTop==True||windows[wm_setup_index].state.isMapped==True)
  {
      tk_event.ev_type=BN_RELEASED;
      tk_event.button=bn_setup_cancel;
      SET_Events();
  }
}







