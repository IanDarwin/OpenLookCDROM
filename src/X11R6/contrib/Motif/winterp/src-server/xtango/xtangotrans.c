/* -*-C-*-
*******************************************************************************
*
* File:         xtangotrans.c
* RCS:          $Header: /disk3/npm/src/winterp/src-server/xtango/RCS/xtangotrans.c,v 2.7 1994/06/09 01:30:33 npm Exp $
* Description:  XTANGO ANIMATION PACKAGE (trans)
* Author:       John T. Stasko, Doug Hayes, Niels Mayer
* Created:      1990
* Modified:     Sun Jun  5 05:23:48 1994 (Niels Mayer) npm@indeed
* Language:     C
* Package:      N/A
* Status:	X11r6 contrib release
*
* Xtango 1.52 Copyright 1990-1994 Georgia Institute of Technology
* 			     (by John T. Stasko and Doug Hayes).
* WINTERP 2.0 Copyright (C) 1994, Enterprise Integration Technologies Corp. and Niels Mayer.
* WINTERP 1.15-1.99, Copyright (c) 1993, Niels P. Mayer.
* WINTERP 1.0-1.14, Copyright (c) 1989-1992 Hewlett-Packard Co. and Niels Mayer.
* 
* This version of Xtango 1.52 (varargs version) represents a subset of
* the Xtango distribution that has been modified specifically for use with
* WINTERP. Non-WINTERP uses of Xtango should use the complete, standard
* version of Xtango, which is available under separate copyright via
* anonymous ftp from par.cc.gatech.edu:pub/xtangovarargs.tar.Z and
* par.cc.gatech.edu:pub/xtango.tar.Z.
* 
* Permission to use, copy, modify, distribute, and sell this software and its
* documentation for any purpose is hereby granted without fee, provided that
* the above copyright notice appear in all copies and that both that
* copyright notice and this permission notice appear in supporting
* documentation, and that the name of Georgia Institute of Technology, 
* John T. Stasko, Doug Hayes, Enterprise Integration Technologies, 
* Hewlett-Packard Company, or Niels Mayer not be used in advertising or
* publicity pertaining to distribution of the software without specific,
* written prior permission. Georgia Institute of Technology, John T. Stasko,
* Doug Hayes, Enterprise Integration Technologies, Hewlett-Packard Company,
* and Niels Mayer makes no representations about the suitability of this 
* software for any purpose.  It is provided "as is" without express or
* implied warranty.
* 
* GEORGIA INSTITUTE OF TECHNOLOGY, JOHN T. STASKO, DOUG HAYES, ENTERPRISE
* INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY AND NIELS MAYER
* DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL GEORGIA
* INSTITUTE OF TECHNOLOGY, JOHN T. STASKO, DOUG HAYES, ENTERPRISE INTEGRATION
* TECHNOLOGIES, HEWLETT-PACKARD COMPANY OR NIELS MAYER BE LIABLE
* FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
* RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
* CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
* CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*
*******************************************************************************
*/
static char rcs_identity[] = "@(#)$Header: /disk3/npm/src/winterp/src-server/xtango/RCS/xtangotrans.c,v 2.7 1994/06/09 01:30:33 npm Exp $";

/* Modification Date  Description			      */
/* -----------------  --------------------------------------- */
/* 90/10/26 JDH	      Baselined source.                       */
/*							      */
/**************************************************************/

/**************************************************************/
/*****************	 include files       ******************/ 
/**************************************************************/

#include "xtangolocal.h"

#include <sys/types.h>	/* For micro_sleep() */
#include <sys/time.h>	/* For micro_sleep() */

/**************************************************************/
/*****************	LOCAL data types     ******************/
/**************************************************************/

/**************************************************************/
/*****************	GLOBAL variables     ******************/
/**************************************************************/

/**************************************************************/
/*****************	LOCAL variables      ******************/
/**************************************************************/


/**************************************************************/
/*****************      LOCAL functions      ******************/
/**************************************************************/

TANGO_TRANS get_transition();
ACTION_PTR  get_action();
FRAME_PTR   get_frame();
void	    copy_frames_shift();
ACTION_PTR  copy_action_shift();
void	    transition_free();
void	    bring_front_config();
void	    push_tail_config();
void	    micro_sleep();

/**************************************************************/
/*****************       entry points        ******************/
/**************************************************************/
TANGO_TRANS TANGOtrans_create();
TANGO_TRANS TANGOtrans_iterate();
void	    TANGOtrans_perform();

#ifndef _NO_PROTO		/* NPM: <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
TANGO_TRANS TANGOtrans_concatenate(int num, ...);
TANGO_TRANS TANGOtrans_compose(int num, ...);
void        TANGOtrans_free(int num, ...);
#else  /* defined(_NO_PROTO) */
TANGO_TRANS TANGOtrans_concatenate();
TANGO_TRANS TANGOtrans_compose();
void	    TANGOtrans_free();
#endif /* !defined(_NO_PROTO) ==> ANSI */


/**************************************************************/
/* TANGOtrans_create -- Return a transition of the given      */
/*			type, that modifies the given image   */
/*			along the given path.		      */
/* 							      */
/*	move -- The image is moved along the path.	      */
/* 							      */
/*	visibility -- If the image is visible, it is made     */
/*		      invisible.  If it is invisible, it is   */
/*		      made visible.  For each offset in the   */
/*		      path, the visibility is toggled.	      */
/* 							      */
/*	fill -- The image will alternate between unfilled     */
/*		(just an outline) and filled.  For each       */
/*		offset in the path, the visibility is	      */
/*		toggled.				      */
/* 							      */
/*	resize -- The image is resized along the given path.  */
/*		  Each type of image will have a "method" for */
/*		  resizing.				      */
/* 							      */
/*      zoom -- Zoom in on the given image given a path of    */
/*              zoomfactors.                                  */
/*                                                            */
/*	raise -- The image is brought to the viewing plane    */
/*		 closest to the user.			      */
/* 							      */
/*	lower -- The image is lowered to the viewing farthest */
/*		 from the user (bottom of all images.)	      */
/* 							      */
/*	delay -- Do nothing in this frame.		      */
/* 							      */
/* RETURNS:  Appropriate transition.			      */
/**************************************************************/
TANGO_TRANS
TANGOtrans_create(type,image,path)
   TANGO_TRANS_TYPE    type;
   TANGO_IMAGE	       image;
   TANGO_PATH	       path;
{
   TANGO_TRANS	trans;
   FRAME_PTR	frame,fhead,ftail;
   int		count;
   OFFSET_PTR	op;
   ACTION_PTR	action;

   if (!TANGO__data) TANGOinit();

   DEBUG("TANGOtrans_create(0x%lx, 0x%lx, 0x%lx)\n", (unsigned long) type, (unsigned long) image, (unsigned long) path);

   if (image && !(image->alive))
     { fprintf(stderr,
	       "Warning: Call to TANGOtrans_create with a deleted image=%d\n",
#ifdef WINTERP	       
	       (int) image	/* NPM: prevent gcc compiler warning */
#else /* !defined(WINTERP) */
	       image
#endif /* WINTERP */
	       );
       return(NULL);
     }

   trans = get_transition();

   count = 0;
   fhead = ftail = NULL;
   for (op=path->head; op; op=op->nexto)
      { count++;
	frame = get_frame(type);
	frame->frame_num = count;
	frame->dx = op->dx;
	frame->dy = op->dy;

	if (!fhead)
	   fhead = frame;
	else
	   ftail->nextf = frame;
	frame->prevf = ftail;
	frame->nextf = NULL;
	ftail = frame;
      }
   action = get_action(type);
   action->fhead = fhead;
   action->ftail = ftail;
   action->image = image;
   action->nexta = NULL;

   trans->num_frames = count;
   trans->actions = action;

   return(trans);
}



/**************************************************************/
/* TANGOtrans_iterate -- Return a transition which is the     */
/*			 given transition repeated num times. */
/* 							      */
/* RETURNS:  Specified transition.			      */
/**************************************************************/
TANGO_TRANS
TANGOtrans_iterate(trans,num)
   TANGO_TRANS trans;
   int num;
{
   TANGO_TRANS	ittrans;
   ACTION_PTR	act,last_action,new_action;
   int		it;

   if (!TANGO__data) TANGOinit();

   DEBUG("TANGOtrans_iterate(0x%lx, %d)\n", (unsigned long) trans, num);

   ittrans = get_transition();

   last_action = NULL;
   for (it=0; it<num; ++it)
      for (act=trans->actions; act; act=act->nexta)
	 { new_action = copy_action_shift(act,it*trans->num_frames);
	   if (last_action)
	      last_action->nexta = new_action;
	   else
	      ittrans->actions = new_action;
	   last_action = new_action;
	 }
   ittrans->num_frames = num * trans->num_frames;

   return(ittrans);
}



/**************************************************************/
/* TANGOtrans_concatenate -- Return a transition that is the  */
/*	concatenation of given transitions one after the other.*/
/* 							      */
/* RETURNS:  Specified transition.			      */
/**************************************************************/
TANGO_TRANS
TANGOtrans_concatenate
#ifndef _NO_PROTO		/* NPM: <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
(int num, ...)
#else  /* defined(_NO_PROTO) */
(va_alist) va_dcl
#endif /* !defined(_NO_PROTO) ==> ANSI */
{
   TANGO_TRANS	contrans,transitions[50];
   ACTION_PTR	act,last_action,new_action;
   int		i,frame_count,tnum;
   va_list      ap;
#ifndef _NO_PROTO		/* NPM: <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
   va_start(ap, num);
#else  /* defined(_NO_PROTO) */
   int num;
   va_start(ap);
   num = va_arg(ap, int);
#endif /* !defined(_NO_PROTO) ==> ANSI */

   if (!TANGO__data) TANGOinit();

   for (i=0; i<num; i++)
      transitions[i] = va_arg(ap, TANGO_TRANS);

   DEBUG("TANGOtrans_concatenate(%d, 0x%lx, ...)\n", num, (unsigned long) transitions[0]);

   contrans = get_transition();

   last_action = NULL;
   frame_count = 0;
   for (tnum=0; tnum<num; ++tnum)
      { for (act=transitions[tnum]->actions; act; act=act->nexta)
	   { new_action = copy_action_shift(act,frame_count);
	     if (last_action)
		last_action->nexta = new_action;
	     else
		contrans->actions = new_action;
	     last_action = new_action;
	   }
	frame_count += transitions[tnum]->num_frames;
      }

   contrans->num_frames = frame_count;

   va_end(ap);
   return(contrans);
}



/**************************************************************/
/* TANGOtrans_compose -- Return a transition which is the     */
/*			 composition of given transitions.    */
/*			 By composition, we mean the          */
/*			 concurrent execution of all the      */
/*			 transitions.			      */
/* 							      */
/* RETURNS:  Specified transition.			      */
/**************************************************************/
TANGO_TRANS
TANGOtrans_compose
#ifndef _NO_PROTO		/* NPM: <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
(int num, ...)
#else  /* defined(_NO_PROTO) */
(va_alist) va_dcl
#endif /* !defined(_NO_PROTO) ==> ANSI */
{
   int		  i,max,tnum;
   TANGO_TRANS	  comtrans,transitions[50];
   ACTION_PTR	  act,last_action,new_action;
   va_list        ap;
#ifndef _NO_PROTO		/* NPM: <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
   va_start(ap, num);
#else  /* defined(_NO_PROTO) */
   int num;
   va_start(ap);
   num = va_arg(ap, int);
#endif /* !defined(_NO_PROTO) ==> ANSI */

   if (!TANGO__data) TANGOinit();

   for (i=0; i<num; i++) 
      transitions[i] = va_arg(ap, TANGO_TRANS);

   DEBUG("TANGOtrans_compose(%d, 0x%lx, ...)\n",
	 num, (unsigned long) transitions[0]);

   max = 0;
   for (tnum=0; tnum<num; ++tnum)
      if (transitions[tnum]->num_frames > max)
	 max = transitions[tnum]->num_frames;

   comtrans = get_transition();

   last_action = NULL;
   for (tnum=0; tnum<num; ++tnum)
      { for (act=transitions[tnum]->actions; act; act=act->nexta)
	   { new_action = copy_action_shift(act,0);
	     if (new_action->fhead)  /* if no frames added, bag this action */
		{ if (last_action)
		     last_action->nexta = new_action;
		  else
		     comtrans->actions = new_action;
		  last_action = new_action;
		}
	     else
		free(new_action);
	   }
      }

   comtrans->num_frames = max;

   va_end(ap);
   return(comtrans);
}



/**************************************************************/
/* TANGOtrans_perform -- Execute the given transition.        */
/* 							      */
/* RETURNS:  None.					      */
/**************************************************************/
void
TANGOtrans_perform(trans)
   TANGO_TRANS trans;
{
#ifdef WINTERP
  extern void Tcls_Destroy_TANGOIMAGEOBJ( /* TANGO_IMAGE image */); /* from ../t_classes.c */
#endif /* WINTERP */
   register IMAGE_PTR	    imlist;
   register ACTION_PTR	    act;
	    int 	    fnum;

   if (!TANGO__data) TANGOinit();

   DEBUG("TANGOtrans_perform(0x%lx)\n", (unsigned long) trans);

   if (!trans) return;

   for (act=trans->actions; act; act=act->nexta)
      act->doing = act->fhead;	/* reset pointers to execute frames */

   for (fnum=1; fnum<=trans->num_frames; ++fnum)
      { TANGO_check_X_events();
        for (act=trans->actions; act; act=act->nexta)
	   if ((act->doing) && (act->doing->frame_num == fnum))
	      { switch (act->type) {
                case TANGO_TRANS_TYPE_MOVE:
		     TANGO_image_damage(act->image);
                     act->image->loc[0] += act->doing->dx;
		     act->image->loc[1] += act->doing->dy;
		     TANGO_image_damage(act->image);
                     break;
                case TANGO_TRANS_TYPE_VISIBLE:
		     TANGO_image_damage(act->image);
                     act->image->visible = !(act->image->visible);
		     break;
		case TANGO_TRANS_TYPE_DELAY:
		     /* do nothing */
                     break;
		case TANGO_TRANS_TYPE_RAISE: 
                     TANGO_image_damage(act->image);
		     bring_front_config(act->image);
                     break;
		case TANGO_TRANS_TYPE_LOWER:
		     TANGO_image_damage(act->image);
                     push_tail_config(act->image);
                     break;
	        case TANGO_TRANS_TYPE_ZOOM:
                     TANGO_zoom(act->image->loc[0],act->image->loc[1],
                                  act->doing->dx,act->doing->dy);
                     break;
        	case TANGO_TRANS_TYPE_DELETE: 
                     TANGO_image_damage(act->image);
#ifdef WINTERP			/* WINTERP needs to be told that a TANGOIMAGEOBJ got deleted... */
		     if (!act->image->alive)
		       COMPLAIN("Warning: Attempt to delete already deleted image=0x%lx\n", (unsigned long) act->image);
		     else {
		       Tcls_Destroy_TANGOIMAGEOBJ(act->image, trans); /* NPM: for all referenceable transitions (on context->trans_reflist
									 as well as the current transition 'trans') remove any actions referring
									 to the deleted image, leaving all non-related transitions/actions as
									 they were. This routine will set to NULL (and free data) associated
									 with ACTION_PTR elements ->doing, ->fhead, ->ftail. Note that by
									 setting act->doing==NULL, followed by 'continue' below, we avoid doing
									 any more processing of actions relating to the deleted image. All
									 actions assoc'd w/ a deleted image have ->doing==NULL, which means
									 the for() loop won't touch the deleted frames. */
		       /* act->doing = (FRAME_PTR) NULL; -- this is already done by Tcls_Destroy_TANGOIMAGEOBJ() above */
		       continue; /* skip setting 'act->doing = act->doing->nextf', above, we set it to NULL s.t. for() loop continues on... */
		     }
#else  /* !defined(WINTERP) -- note that TANGO_image_delete() doesn't actually do anything other than mark the image w/ act->image->alive=NULL ... */
		     TANGO_image_delete(act->image);
#endif /* WINTERP */
                     break;
		case TANGO_TRANS_TYPE_REFRESH:
		       TANGO__data->damlx = TANGO__data->lx;
                       TANGO__data->damrx = TANGO__data->rx;
                       TANGO__data->damty = TANGO__data->ty;
                       TANGO__data->damby = TANGO__data->by;
		       break;
		default: /* non-standard transition, call specific image handler */
		     TANGO_image_trans(act->image, act->type, 
			               act->doing->dx,act->doing->dy);
                     break;
		   }
		act->doing = act->doing->nextf;
	      }


        if ((TANGO__data->damrx >= TANGO__data->damlx) || /* a change */
	    (TANGO__data->damty >= TANGO__data->damby))
	  { TANGO_damage_clear_screen();

           /* now draw all the images and go to the next frame */
            for (imlist=TANGO__data->configtail; imlist; imlist=imlist->previ)
                if (TANGO_image_intercepts(imlist->image))
#ifdef WINTERP /* NPM: removed superfluous parameter to all *_draw() procedures */
                   TANGO_image_draw(imlist->image, 0.0, 0.0);
#else /* !defined(WINTERP) */
                   TANGO_image_draw(imlist->image, imlist->image, 0.0, 0.0);
#endif /* WINTERP */
           }

	TANGO_anim_next_damage_frame();

	if (TANGO__data->delay > 0) {
	     TANGO_flush();
	     micro_sleep(TANGO__data->delay);
	   }
        TANGO__data->damlx = TANGO__data->rx;
        TANGO__data->damrx = TANGO__data->lx;
        TANGO__data->damty = TANGO__data->by;
        TANGO__data->damby = TANGO__data->ty;
      }
   TANGO_flush();
}



/**************************************************************/
/* TANGOtrans_free -- Free up the allocated structures        */
/*		      associated with a transition.	      */
/* 							      */
/* RETURNS:  None.					      */
/**************************************************************/
void
TANGOtrans_free
#ifndef _NO_PROTO		/* NPM: <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
(int num, ...)
#else  /* defined(_NO_PROTO) */
(va_alist) va_dcl
#endif /* !defined(_NO_PROTO) ==> ANSI */
{
   int i;
   va_list        ap;
#ifndef _NO_PROTO		/* NPM: <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
   va_start(ap, num);
#else  /* defined(_NO_PROTO) */
   int num;
   va_start(ap);
   num = va_arg(ap, int);
#endif /* !defined(_NO_PROTO) ==> ANSI */

   if (!TANGO__data) TANGOinit();

   for (i=0; i<num; i++) 
      transition_free( va_arg(ap, TANGO_TRANS) );

   DEBUG("TANGOtrans_free(%d, ...)\n", num);

   va_end(ap);
}



/**************************************************************/
/* TANGO_zoom -- Zoom in on given image by zoomfactors given  */
/*               in a path                                    */
/*               Zooms straight out at the moment             */
/*                                                            */
/* RETURNS:  None.                                            */
/**************************************************************/
void
TANGO_zoom(x,y,xzoomfactor,yzoomfactor)
     WIN_COORD   x,y;
     double xzoomfactor,yzoomfactor;
{
     TANGO__data->rx -= (TANGO__data->rx - x) * xzoomfactor;
     TANGO__data->lx += (x - TANGO__data->lx) * xzoomfactor;
     TANGO__data->by -= (TANGO__data->by - y) * yzoomfactor;
     TANGO__data->ty += (y - TANGO__data->ty) * yzoomfactor;

     TANGO__data->x_WIN_COORD_to_int = ((WIN_COORD) TANGO__data->width) /
       (TANGO__data->rx - TANGO__data->lx);
     TANGO__data->y_WIN_COORD_to_int = ((WIN_COORD) TANGO__data->height) /
       (TANGO__data->by - TANGO__data->ty);

     TANGO__data->damlx = TANGO__data->lx;
     TANGO__data->damrx = TANGO__data->rx;
     TANGO__data->damty = TANGO__data->ty;
     TANGO__data->damby = TANGO__data->by;
}




/**************************************************************/
/* get_transition -- Allocate and return a transition	      */
/*		     structure.  All the fields are set to    */
/*		     their default initial values. 	      */
/* 							      */
/* RETURNS:  Specified transition.			      */
/**************************************************************/
TANGO_TRANS
get_transition()
{
   TANGO_TRANS trans;

   trans = (TANGO_TRANS) malloc( sizeof( struct _TRANS));
   trans->num_frames = 1;
   trans->actions = NULL;
   trans->next = NULL;
   return(trans);
}



/**************************************************************/
/* get_action -- Allocate and return an action structure.     */
/*		 All the fields are set to their default      */
/*		 initial values.  An action contains frames   */
/*		 which affect an object in a transition.      */
/*		 (Because of composition, one transition may  */
/*		 affect many objects.)			      */
/* 							      */
/* RETURNS:  Specified action.				      */
/**************************************************************/
ACTION_PTR
get_action(type)
   TANGO_TRANS_TYPE type;
{
   ACTION_PTR act;

   act = (ACTION_PTR) malloc( sizeof( struct ACTION));
   act->type = type;
   act->image = NULL;
   act->doing = NULL;
   act->fhead = act->ftail = NULL;
   act->nexta = NULL;
   return(act);
}



/**************************************************************/
/* get_frame -- Allocate and return a frame structure.        */
/*		All the fields are set to their default       */
/*		initial values.  A frame is one logical time  */
/*		unit of action in a transition.  The	      */
/*		frame_num field is very important for the     */
/*		execution of transitions.		      */
/* 							      */
/* RETURNS:  Specified frame.				      */
/**************************************************************/
FRAME_PTR
get_frame(type)
   TANGO_TRANS_TYPE type;
{
   FRAME_PTR frame;

   frame = (FRAME_PTR) malloc( sizeof( struct FRAME));
   frame->type = type;
   frame->frame_num = 1;
   frame->dx = ZERO;
   frame->dy = ZERO;
   frame->nextf = frame->prevf = NULL;
   return(frame);
}



/**************************************************************/
/* copy_frames_shift -- Copy all the frames from the given    */
/*			list, and return the appropriate head */
/*			and tail ptrs.  The frame_numbers are */
/*			supplemented by the additional        */
/*			start_num.			      */
/* 							      */
/* RETURNS:  None.					      */
/**************************************************************/
void
copy_frames_shift(frames,start_num,head,tail)
   FRAME_PTR  frames;
   int	      start_num;
   FRAME_PTR *head;
   FRAME_PTR *tail;
{
   FRAME_PTR fra,new_frame;
   FRAME_PTR h,t;

   h = t = NULL;
   for (fra=frames; fra; fra=fra->nextf)
      { new_frame = (FRAME_PTR) malloc( sizeof( struct FRAME));
	new_frame->type = fra->type;
	new_frame->dx = fra->dx;
	new_frame->dy = fra ->dy;
	new_frame->frame_num = fra->frame_num + start_num;
	if (h)
	   t->nextf = new_frame;
	else
	   h = new_frame;
	new_frame->prevf = t;
	new_frame->nextf = NULL;
	t = new_frame;
      }
   *head = h;
   *tail = t;
}



/**************************************************************/
/* copy_action_shift -- Copy the given action (and its sub-   */
/*			frames) into a new action which has   */
/*			the frame_numbers supplemented by     */
/*			start_frame.  Return the ACTION_PTR.  */
/* 							      */
/* RETURNS:  Specified action.				      */
/**************************************************************/
ACTION_PTR
copy_action_shift(action,start_frame)
   ACTION_PTR action;
   int	      start_frame;
{
   ACTION_PTR new_action;
   FRAME_PTR  head,tail;

   new_action = (ACTION_PTR) malloc( sizeof( struct ACTION));
   new_action->type = action->type;
   new_action->image = action->image;
   new_action->doing = NULL;
   new_action->nexta = NULL;

   copy_frames_shift(action->fhead,start_frame,&head,&tail);
   new_action->fhead = head;
   new_action->ftail = tail;
   return(new_action);
}



/**************************************************************/
/* transition_free -- Free up the allocated structures        */
/*		      associated with a transition.	      */
/* 							      */
/* RETURNS:  None.					      */
/**************************************************************/
void
transition_free(trans)
   TANGO_TRANS trans;
{
   FRAME_PTR  f,old_f;
   ACTION_PTR act,old_act;

   if (!trans) return;
   act = trans->actions;
   while (act)
      { f = act->fhead;
	while (f)
	   { old_f = f;
	     f = f->nextf;
	     free(old_f);
	   }
	old_act = act;
	act = act->nexta;
	free(old_act);
      }
   free(trans);
}



/**************************************************************/
/* bring_front_config -- Bring the given image to the front   */
/*			 of the configuration list.  (it will */
/*			 be drawn last)			      */
/* 							      */
/* RETURNS:  None.					      */
/**************************************************************/
void
bring_front_config(image)
   TANGO_IMAGE image;
{
   IMAGE_PTR im;

   im = image->inconfig;

   if (im == TANGO__data->confighead) return;

   if (im == TANGO__data->configtail)
      TANGO__data->configtail = im->previ;
   else
      im->nexti->previ = im->previ;

   im->previ->nexti = im->nexti;
   im->nexti = TANGO__data->confighead;
   TANGO__data->confighead->previ = im;
   TANGO__data->confighead = im;
   im->previ = NULL;
}



/**************************************************************/
/* push_tail_config -- Put the given image at the tail        */
/*		       of the configuration list.  (it will   */
/*		       be drawn first)			      */
/* 							      */
/* RETURNS:  None.					      */
/**************************************************************/
void
push_tail_config(image)
   TANGO_IMAGE image;
{
   IMAGE_PTR im;

   im = image->inconfig;

   if (im == TANGO__data->configtail) return;

   if (im == TANGO__data->confighead)
      TANGO__data->confighead = im->nexti;
   else
      im->previ->nexti = im->nexti;

   im->nexti->previ = im->previ;
   im->previ = TANGO__data->configtail;
   TANGO__data->configtail->nexti = im;
   TANGO__data->configtail = im;
   im->nexti = NULL;
}



/**************************************************************/
/* TANGO_refresh -- Redraw all the active images to clean up  */
/*		    the screen.				      */
/* 							      */
/* RETURNS:  None.					      */
/**************************************************************/
void
TANGO_refresh()
{
   register IMAGE_PTR	    imlist;

   TANGO_clear_screen();   /* This resets the GC clip region to the size */
           /* of the offscreen pixmap and sets variables                 */
           /* (TANGO__data->damage_x,damage_y,damage_width,damage_height)*/
           /* to the size of the clip region                             */

   /* Reset damage area to froce redraw of entire screen */
   TANGO__data->damlx = TANGO__data->lx;
   TANGO__data->damrx = TANGO__data->rx;
   TANGO__data->damty = TANGO__data->ty;
   TANGO__data->damby = TANGO__data->by;

   /* now draw only the images that fall within the visible region
      and go to the next frame */
   for (imlist=TANGO__data->configtail; imlist; imlist=imlist->previ)
       if (TANGO_image_intercepts(imlist->image))
#ifdef WINTERP /* NPM: removed superfluous parameter to all *_draw() procedures */
          TANGO_image_draw(imlist->image, 0.0, 0.0);
#else /* !defined(WINTERP) */
          TANGO_image_draw(imlist->image, imlist->image, 0.0, 0.0);
#endif /* WINTERP */

   TANGO_anim_next_damage_frame();

   /* reset the damage region for subsequent drawing and animation */
   TANGO__data->damlx = TANGO__data->rx;
   TANGO__data->damrx = TANGO__data->lx;
   TANGO__data->damty = TANGO__data->by;
   TANGO__data->damby = TANGO__data->ty;

   TANGO_flush();
}


/**************************************************************/
/* micro_sleep -- Pause for the given number of microseconds. */
/*							      */
/* Written by Stephen P. Reiss, Brown University (ashbio.c).  */
/* 		      Used with permission.		      */
/* 							      */
/* RETURNS:  None.					      */
/**************************************************************/
void micro_sleep(usec)
   int usec;
{
   struct timeval t;
#ifndef WINTERP /* NPM: 'i' isn't used anywhere -- remove it */
   int i;
#endif /* WINTERP */

   t.tv_sec = usec >> 20;
   t.tv_usec = usec & 0xfffff;
   if (t.tv_usec >= 1000000) {
      ++t.tv_sec;
      t.tv_usec -= 1000000;
      }

   select(0, 0, 0, 0, &t);	/* Block until time expires */

   /* Note:  If your machine does not have select, then comment out the
      above code, and uncomment the code below, which is a kludge to do
      delays with the speed control bar.  */

/*   for (i=0; i<usec; i++)
      continue;     */

}

/**************************************************************/
/*****************   end of xtangotrans.c    ******************/
/**************************************************************/
