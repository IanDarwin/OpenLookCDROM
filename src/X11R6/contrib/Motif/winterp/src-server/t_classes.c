/* -*-C-*-
********************************************************************************
*
* File:         t_classes.c
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/t_classes.c,v 2.6 1994/06/06 15:41:14 npm Exp $
* Description:  XLISP <--> Xtango class interface (type TANGOIMAGEOBJ, base
*		class TANGO:IMAGE_CLASS). Code stolen and mutated from
*		w_classes.c ...
* Author:       Niels Mayer
* Created:      Wed Jun 14 16:28:45 1989
* Modified:     Sun Jun  5 14:20:40 1994 (Niels Mayer) npm@indeed
* Language:     C
* Package:      N/A
* Status:       X11r6 contrib release
*
* Copyright (C) 1994, Enterprise Integration Technologies Corp. and Niels Mayer.
* WINTERP 1.15-1.99, Copyright (c) 1993, Niels P. Mayer.
* WINTERP 1.0-1.14, Copyright (c) 1989-1992 Hewlett-Packard Co. and Niels Mayer.
* 
* Permission to use, copy, modify, distribute, and sell this software and its
* documentation for any purpose is hereby granted without fee, provided that
* the above copyright notice appear in all copies and that both that
* copyright notice and this permission notice appear in supporting
* documentation, and that the name of Enterprise Integration Technologies,
* Hewlett-Packard Company, or Niels Mayer not be used in advertising or
* publicity pertaining to distribution of the software without specific,
* written prior permission. Enterprise Integration Technologies, Hewlett-Packard
* Company, and Niels Mayer makes no representations about the suitability of
* this software for any purpose.  It is provided "as is" without express or
* implied warranty.
* 
* ENTERPRISE INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY AND NIELS MAYER
* DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL ENTERPRISE
* INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY OR NIELS MAYER BE LIABLE
* FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
* RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
* CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
* CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
*
********************************************************************************
*/
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/RCS/t_classes.c,v 2.6 1994/06/06 15:41:14 npm Exp $";

/*
*------------------------------------------------------------------------------
* See ./winterp/COPYRIGHT for information on contacting the authors.
* Please e-mail comments, modifications, questions, improvements and
* bugfixes to the WINTERP mailing list winterp@netcom.com. Please send 
* mailing list subscribe/unsubscribe notices to winterp-request@netcom.com .
* Post XLISP-specific questions/information to the USENET newsgroup
* comp.lang.lisp.x.
*------------------------------------------------------------------------------
*/

#include <stdio.h>
#include <Xm/Xm.h>
#include "winterp.h"
#include "tango.h"
  
   
extern LVAL o_TANGOIMAGE_CLASS;	/* tic_IMAGE.c */


/******************************************************************************
 * Fetches and removes a TANGOIMAGEOBJ from the argument stack, returning
 * the tangoImageID. If the TANGOIMAGEOBJ has been destroyed or is not initialized,
 * then this will signal an error. <tiobj_return> is a pointer to an LVAL,
 * it returns the TANGOIMAGEOBJ retrieved from the argument stack.
 ******************************************************************************/
TANGO_IMAGE Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(tiobj_return)
     LVAL *tiobj_return;
{
  TANGO_IMAGE tangoImage_id;

  if (tangoImage_id = get_tangoimageobj_timageID(*tiobj_return = xlga_tangoimageobj()))
    return (tangoImage_id);
  else {
    /* Xtango_Restore_Context(); -- don't need this, since this fn always called before Xtango_Save_Set_Context...() */
    xlerror("TANGOIMAGEOBJ has been :TX_DELETE'd or hasn't been initialized by :ISNEW.", *tiobj_return);
  }
}


/******************************************************************************
 * This proc assumes that Xtango_Save_Set_Context() has been called...
 ******************************************************************************/
void Tcls_Free_TANGO_IMAGE(image)
     TANGO_IMAGE image;
{
  extern Colormap colormap;	/* winterp.c */
  TANGO_IMAGE subimage, next_subimage;
  TANGO_BITMAP_PTR tango_bitmap;
  TANGO_PIXMAP_PTR tango_pixmap;
  int n;

  image->visible = 0;		/* just incase something in xtango/winterp refs this freed pointer before memory gets clobbered... */
  image->alive = 0;		/* just incase something in xtango/winterp refs this freed pointer before memory gets clobbered... */

  switch (image->type) {	/* special case for images containing special image->object fields... */
  case TANGO_IMAGE_TYPE_BITMAP:
    tango_bitmap = (TANGO_BITMAP_PTR) image->object;
    for (n = 0 ; (n < tango_bitmap->used) ; ++n) 
      XFreePixmap(TANGO__data->display, tango_bitmap->bmap[n]);
    break;
  case TANGO_IMAGE_TYPE_PIXMAP:
    tango_pixmap = (TANGO_PIXMAP_PTR) image->object;
    XFreePixmap(TANGO__data->display, tango_pixmap->pixmap);
    if (tango_pixmap->num_alloc_cols > 0)
      XFreeColors(TANGO__data->display, colormap, tango_pixmap->alloc_cols, tango_pixmap->num_alloc_cols, 0L);
    XtFree((char*) tango_pixmap->alloc_cols);
    break;
  case TANGO_IMAGE_TYPE_COMPOSITE:
    subimage = ((TANGO_COMPOSITE_PTR) image->object)->image_list;
    while (subimage) {
      subimage->visible = 0;
      subimage->alive = 0;
      next_subimage = subimage->nexti;
      Tcls_Free_TANGO_IMAGE(subimage); /* recursively free TANGO_IMAGE subimage of composite */
      subimage = next_subimage;
    }
    break;
  }

  free((char*) image->object);	/* free the image-shape-specific part, e.g. TANGO_COMPOSITE_PTR, TANGO_TEXT_PTR, etc. */
  free((char*) image);		/* free TANGO_IMAGE */
}


/******************************************************************************
 * This is called from xtangotrans.c:TANGOtrans_perform() when performing a
 * TANGO_TRANS_TYPE_DELETE transition. Calling this routine fixes the
 * problem in Xtango 1.50 in which images that are :TX_DELETE'd never really
 * get deallocated. That was ostensibly done because using Xtango from C makes
 * it impossible to figure out whether a transition contains actions on deleted
 * images -- by not deallocating the image (and just setting ->visible and
 * ->alive fields) segmentation violations would be prevented when a transition
 * containing a deleted image is performed.
 * 
 * In WINTERP, we keep a list 'context->trans_reflist' which is kept
 * up-to-date w/r/t the transitions that are still "live" -- this procedure
 * first removes all actions on deleted images for all "live" transitions.
 * Then it removes all actions on the deleted image residing on the current
 * transition 'cur_trans' -- the transition currently being executed by
 * TANGOtrans_perform(). Then the WINTERP TANGOIMAGEOBJ is marked as deallocated
 * removed from the list of saved objects. Finally, the TANGO_IMAGE is removed
 * from xtango's configuration list and it's internals are deallocated...
 ******************************************************************************/
void Tcls_Destroy_TANGOIMAGEOBJ(image, cur_trans)
     TANGO_IMAGE image;		/* the image ID being deleted -- delete all ACTION_PTR elements relating to the deleted image within each transition in context->trans_reflist */
     TANGO_TRANS cur_trans;	/* delete all ACTION_PTR elements on the current transition which refer to the deleted image */
{
  LVAL o_image;
  TANGO_TRANS trans;
  ACTION_PTR act;
  WINTERP_TANGO_CONTEXT context;
  WINTERP_TANGO_CONTEXT_REF ref;
  FRAME_PTR f, old_f;
  IMAGE_PTR im;

  o_image = (LVAL) image->tangoimageobj;
  if (o_image && tangoimageobj_p(o_image)) { /* TRUE for any images created by WINTERP */

    /*
     * For all referenceable transitions within WINTERP (as stored on 'context->trans_reflist')
     * delete all actions relating to the deleted image.
     */
    context = get_tangoimageobj_context(o_image);
    for (ref = context->trans_reflist; ref; ref = ref->next) {
      if ((trans = get_tangotrans(ref->lval)) != (TANGO_TRANS) NULL) {
	for (act = trans->actions; act; act = act->nexta) {
	  if ((act->image == image)) { /* if we've found a transition on the deleted image */
	    f = act->fhead;	/* then remove all the FRAME_PTR elts assoc'd with the image */
	    while (f) {
	      old_f = f;
	      f = f->nextf;
	      free(old_f);
	    }
	    act->doing = act->fhead = act->ftail = (FRAME_PTR) NULL; /* turn the current action into a NO-OP 0-length sequence of frames */
	  }
	}
      }
    }

    /*
     * Remove all actions on the deleted image residing on the current transition
     * 'cur_trans' -- the transition currently being executed by TANGOtrans_perform().
     */
    for (act = cur_trans->actions; act; act = act->nexta) {
      if ((act->image == image)) { /* if we've found a transition on the deleted image */
	f = act->fhead;		/* then remove all the FRAME_PTR elts assoc'd with the image */
	while (f) {
	  old_f = f;
	  f = f->nextf;
	  free(old_f);
	}
	act->doing = act->fhead = act->ftail = (FRAME_PTR) NULL; /* turn the current action into a NO-OP 0-length sequence of frames */
      }
    }

    /*
     * When a TANGOIMAGEOBJ gets destroyed, we set the TANGOIMAGEOBJ's TANGO_IMAGE field to
     * NULL to mark that the TANGOIMAGEOBJ's associated TANGO_IMAGE got destroyed. This
     * ensures that operations on any TANGOIMAGEOBJ's that are sitting around
     * (i.e. referenced in a user's variable) give an appropriate error.
     */
    set_tangoimageobj_timageID(o_image, (TANGO_IMAGE) NULL); /* mark TANGOIMAGEOBJ as invalid */

    /*
     * Destroy the TANGOIMAGEOBJ at the XLISP level by removing from v_savedobjs
     * the TANGOIMAGEOBJ o_image ... this will allow them to be garbage collected
     * next time gc() gets called.
     * BUG: don't need to go through entire hashbucket -- can stop at first TANGOIMAGEOBJ removed...
     */
    {
      int  i = Wso_Hash((get_tangoimageobj_context(o_image))->widget_OBJ); /* note that we hash on the widgetobj with which the image is assoc'd */
      LVAL l_hbucket = getelement(v_savedobjs, i); /* a list of saved objects belonging to this hashbucket */
      LVAL l_prev = NIL;

      while (l_hbucket != NIL)	/* while there are elements in the hashbucket */
	if (car(l_hbucket) == o_image) { /* car() points to cur elt, is it the TANGOIMAGEOBJ being destroyed? */
	  l_hbucket = cdr(l_hbucket); /* l_hbucket now points to next elt or NIL */
	  if (l_prev == NIL)
	    setelement(v_savedobjs, i, l_hbucket); /* remove first, head is now next elt */
	  else
	    rplacd(l_prev, l_hbucket); /* remove cur, point previous to next */
	}
	else {			/* go to next elt in hashbucket */
	  l_prev = l_hbucket;
	  l_hbucket = cdr(l_hbucket);
	}
    }

    /*
     * remove the deleted image from the configuration list
     * (a bidirectional linked list)
     */
    im = image->inconfig;
    if ((im->nexti == (IMAGE_PTR) NULL) && (im->previ == (IMAGE_PTR) NULL)) { /* special case for deleting only image */
      context->adata->confighead = (IMAGE_PTR) NULL;
      context->adata->configtail = (IMAGE_PTR) NULL;
      free(im);
    }
    else if (im->nexti == (IMAGE_PTR) NULL) { /* special case for deleting element at adata->configtail */
      context->adata->configtail = im->previ;
      im->previ->nexti = (IMAGE_PTR) NULL;
      free(im);
    }
    else if (im->previ == (IMAGE_PTR) NULL) { /* special case for deleting element at adata->confighead */
      context->adata->confighead = im->nexti;
      im->nexti->previ = (IMAGE_PTR) NULL;
      free(im);
    }
    else {			/* otherwise we're somewhere between confighead and configtail, so remove elt  */
      im->previ->nexti = im->nexti;
      im->nexti->previ = im->previ;
      free(im);
    }

    /*
     * free up the TANGO_IMAGE data...
     */
    Tcls_Free_TANGO_IMAGE(image); 
  }
}


/******************************************************************************
 * This procedure is meant to be called in an :ISNEW instance initializer
 * method for any tangoImage instances created as subclasses of TANGOIMAGE_OBJECT.
 * xlobj.c:clnew() returns an OBJECT, which is passed to the :ISNEW method
 * as the first argument. Here, we retrieve that argument (actually any 
 * OBJECT arg) and set it to XLTYPE_TANGOIMAGEOBJ so that we can tell that this
 * is a special kind of object.
 ******************************************************************************/
LVAL Tcls_Get_OBJECT_Arg_Returning_TANGOIMAGEOBJ()
{
  LVAL self = xlgaobject();	/* get an OBJECT from arg stack */
  self->n_type = XLTYPE_TANGOIMAGEOBJ; /* OBJECT 'self' is now really a TANGOIMAGEOBJ */
  return (self);
}


/******************************************************************************
 * This procedure initializes a TANGOIMAGEOBJ by initializing the TangoImageID slot.
 * Then it saves the TANGOIMAGEOBJ in v_savedobjs, so that it gets mark()'d.
 * This way, it won't be garbage collected while the tangoImage is "visible".
 *
 * Tcls_Destroy_TANGOIMAGEOBJ() above will remove the TANGOIMAGEOBJ from
 * v_savedobjs when the tangoImage is destroyed via transition
 * TANGO_TRANS_TYPE_DELETE in TANGOtrans_perform(). This allows the
 * TANGOIMAGEOBJ and any lisp objects it references to be garbage collected.
 * (Note that Tcls_Destroy_TANGOIMAGEOBJ() also removes/invalidates any "live"
 * tango transitions containing the destroyed image.)
 *
 * The TANGOIMAGEOBJ will also get removed from v_savedobj and destroyed by
 * Tcls_Free_TANGO_IMAGE() when the tango-WIDGETOBJ assoc'd w/ the
 * TANGOIMAGEOBJ gets destroyed. (See
 * wc_Xtango.c:Xtango_Widget_Destroy_Callback() for details).
 *
 * This procedure is meant to be called in an :ISNEW instance initializer
 * method for any tangoImage instances created as subclasses of TANGOIMAGE_OBJECT.
 ******************************************************************************/
void Tcls_Initialize_TANGOIMAGEOBJ(o_tangoImage, tangoImage_id, context)
     LVAL			o_tangoImage; /* XLTYPE_TANGOIMAGEOBJ */
     TANGO_IMAGE		tangoImage_id;
     WINTERP_TANGO_CONTEXT	context;
{
  set_tangoimageobj_timageID(o_tangoImage, tangoImage_id);
  set_tangoimageobj_context(o_tangoImage, context);

  tangoImage_id->tangoimageobj = (XtPointer) o_tangoImage; /* set backpointer to TANGOIMAGEOBJ used by Tcls_TangoImageID_To_TANGOIMAGEOBJ() */
    
  {				/* save the TANGOIMAGEOBJ in v_savedobjs, so that it gets mark()'d against garbage collection */
    int  i = Wso_Hash(context->widget_OBJ); /* note that we hash on the widgetobj with which the image is assoc'd */
    LVAL l_hbucket;
    
    xlsave1(l_hbucket);
    l_hbucket = cons(o_tangoImage, getelement(v_savedobjs, i));
    setelement(v_savedobjs, i, l_hbucket);
    xlpop();
  }
}


/******************************************************************************
 *
 ******************************************************************************/
LVAL Tcls_TangoImageID_To_TANGOIMAGEOBJ(tangoImage_id)
     TANGO_IMAGE tangoImage_id;	/* assume that tangoImage_id is a valid TangoImage, not NULL */
{
  LVAL o_tangoImage;

  if (!tangoImage_id)
    return (NIL);

  if ((o_tangoImage = (LVAL) tangoImage_id->tangoimageobj) != (LVAL) NULL) { /* NOTE: uses xtangolocal.h to access internals of TANGO_IMAGE */
    if (ntype(o_tangoImage) == XLTYPE_TANGOIMAGEOBJ) /* quick sanity check */
      return (o_tangoImage);
    else {
      Xtango_Restore_Context();	/* restore before returning -- assumes prior call to Xtango_Save_Set_Context_From_WidgetID() or Xtango_Save_Set_Context() */
      xlfail("Internal error in Xtango_WidgetID_To_WidgetObj() -- someone's been messing with this TANGO_IMAGE's TANGOIMAGEOBJ backpointer!");
    }
  }
  else {
    /* TODO LATER:
     * retrieve the tangoImage_id->type field, and create "dummy"
     * TANGOIMAGEOBJs based on the type of the returned tangoimage. This would
     * be needed for any tangoimages created internally by xtango and not created
     * through the WINTERP interface.
     * see Wcls_WidgetID_To_WIDGETOBJ() for analogous proc...
     */
    Xtango_Restore_Context();	/* restore before returning -- assumes prior call to Xtango_Save_Set_Context_From_WidgetID() or Xtango_Save_Set_Context() */
    xlfail("Internal error in Tcls_TangoImageID_To_TANGOIMAGEOBJ() -- TANGO_IMAGE with uninitialized TANGOIMAGOBJ backpointer...");
  }
}


/******************************************************************************/
/******************************************************************************/
/******************************************************************************/
/******************************************************************************/

/* instance variable numbers for the class 'Class' -- from xlobj.c */
#include "xlisp/xlobj.h"

/* class variable numbers for classes derived from 'WIDGET_CLASS' */
#define TANGO_IMAGE_TYPE_ID 0
#define DERIVED_TANGOIMAGE_CLASS_CVALS_SIZE 1


/*****************************************************************************
 * This procedure creates a new 'Class' instance which inherits from the
 * base class 'TANGO:IMAGE_CLASS'. This derived class contains no new instance
 * variables. This procedure is equivalent to doing:
 * (set (implode <class_name>) 
 *      (send Class :new
 *                  '()                    ;; no IVARS
 *                  '(TANGO_IMAGE_TYPE_ID) ;; no CVARS
 *                  TANGO:IMAGE_CLASS      ;; SUPERCLASS
 ****************************************************************************/
LVAL Tcls_Create_Subclass_Of_TANGOIMAGE_CLASS(class_name, image_type_id)
     char* class_name;
     TANGO_IMAGE_TYPE image_type_id;
{
  extern LVAL s_TANGO_IMAGE_TYPE_ID; /* from tic_IMAGE.c */
  LVAL self;
  LVAL v_cvals;

  /* create 'Class' instance;
     set slot IVARTOTAL = TANGOIMAGEOBJ_SIZE (superclass has all slots)
     set slot IVARS = NIL              ;; no IVAR slots in this class
     set slot MESSAGES = NIL           ;; no messages (yet) for this class */
  self = xlclass(class_name, TANGOIMAGEOBJ_SIZE); /* note: self won't get gc'd because it is a symbol's value */
  
  /* set slot IVARCNT = 0, since there's no I-Vars in this TANGO:IMAGE_CLASS
     subclass. Note that xlclass() above sets IVARTOTAL==TANGOIMAGEOBJ_SIZE==2
     since TANGO:IMAGE_CLASS contains two slots that hold
     (1) TANGO_IMAGE		get_tangoimageobj_timageID(x),
     (2) WINTERP_TANGO_CONTEXT	get_tangoimageobj_context(x). */
  setivar(self, IVARCNT, cvfixnum((FIXTYPE)0));

  /* set SUPERCLASS slot to o_TANGOIMAGE_CLASS ... */
  setivar(self, SUPERCLASS, o_TANGOIMAGE_CLASS);

  /* set CVARS */
  setivar(self, CVARS, cons(s_TANGO_IMAGE_TYPE_ID, NIL));

  /* set CVALS */
  v_cvals = newvector((unsigned) DERIVED_TANGOIMAGE_CLASS_CVALS_SIZE);
  setivar(self, CVALS, v_cvals); /* note: also prevents gc'ing of v_cvals since <self> is protected */
  setelement(v_cvals, TANGO_IMAGE_TYPE_ID, cvfixnum((FIXTYPE) image_type_id));

  return (self);
}


/*****************************************************************************
 * routine to print an object for PRINx -- stolen from xlisp/xlobj.c:xputobj().
 * this is called by wc_TANGOIMAGE.c:Tango_Image_Class_Method_PRIN1()
 ****************************************************************************/
#ifdef ANSI
VOID NEAR Tcls_Print_TANGOIMAGEOBJ(LVAL fptr, LVAL o_tangoImage)
#else /* !defined(ANSI) */
VOID Tcls_Print_TANGOIMAGEOBJ(fptr, o_tangoImage)
     LVAL fptr;			/* STREAM || USTREAM */
     LVAL o_tangoImage;		/* XLTYPE_TANGOIMAGEOBJ */
#endif /* ANSI */
{
  /* extern char buf[]; -- from xlglob.c, extern'd in xlisp.h */
  LVAL o_class = getclass(o_tangoImage);
  LVAL lval_pname;

  if (((lval_pname = getivar(o_class, PNAME)) != NIL)
      && (ntype(lval_pname) == STRING)) {
    sprintf(buf,"#<tangoImage %s: #", getstring(lval_pname));
    xlputstr(fptr, buf);
  }
  else {			/* if class of o_tangoImage isn't named w/ PNAME, use Motif tangoImage class name -- for back-compat w/ classes not defd w/ defmethod */
    LVAL o_prevclass = NIL;

    /* loop through superclasses, stopping at TANGO:IMAGE_CLASS or NIL(superclass of object) */
    while ((o_class != o_TANGOIMAGE_CLASS) && (o_class != NIL)) {
      o_prevclass = o_class;
      o_class = getivar(o_class, SUPERCLASS);
    }

    /*
     * if the loop terminated with o_CLASS==o_TANGOIMAGE_CLASS and o_prevclass!=NIL
     * then o_prevclass is the subclass of TANGO:IMAGE_CLASS created by
     * Tcls_Create_Subclass_Of_TANGOIMAGE_CLASS(). The latter calls xlclass() w/ the
     * tangoImage-class name set in class-ivar PNAME.
     */
    if ((o_class == o_TANGOIMAGE_CLASS)
	&& (o_prevclass != NIL)
	&& ((lval_pname = getivar(o_prevclass, PNAME)) != NIL)
	&& (ntype(lval_pname) == STRING)) {
      sprintf(buf,"#<tangoImage %s: #", getstring(lval_pname));
      xlputstr(fptr, buf);
    }
    else
      xlputstr(fptr, "#<tangoImage TANGO:IMAGE_CLASS: #");
  }
  
  sprintf(buf, AFMT, (long) o_tangoImage);
  xlputstr(fptr, buf);
  xlputc(fptr, '>');
}


/*****************************************************************************
 * Returns the symbol associated with the class of <o_tangoImage>
 ****************************************************************************/
LVAL Tcls_Get_TANGOIMAGECLASS_Symbol_From_TANGOIMAGEOBJ(o_tangoImage)
     LVAL o_tangoImage;		/* OBJECT(CLASS) */
{
  /* extern char buf[]; -- from xlglob.c, extern'd in xlisp.h */
  LVAL o_class = getclass(o_tangoImage);
  LVAL lval_pname;

  if (((lval_pname = getivar(o_class, PNAME)) != NIL)
      && (ntype(lval_pname) == STRING)) {
    return (xlenter(getstring(lval_pname)));
  }
  else {			/* if class of o_tangoImage isn't named w/ PNAME, use Motif tangoImage class name -- for back-compat w/ classes not defd w/ defmethod */
    LVAL o_prevclass = NIL;

    /* loop through superclasses, stopping at TANGO:IMAGE_CLASS or NIL(superclass of object) */
    while ((o_class != o_TANGOIMAGE_CLASS) && (o_class != NIL)) {
      o_prevclass = o_class;
      o_class = getivar(o_class, SUPERCLASS);
    }

    /*
     * if the loop terminated with o_CLASS==o_TANGOIMAGE_CLASS and o_prevclass!=NIL
     * then o_prevclass is the subclass of TANGO:IMAGE_CLASS created by
     * Tcls_Create_Subclass_Of_TANGOIMAGE_CLASS(). The latter calls xlclass() w/ the
     * tangoImage-class name set in class-ivar PNAME.
     */
    if ((o_class == o_TANGOIMAGE_CLASS)
	&& (o_prevclass != NIL)
	&& ((lval_pname = getivar(o_prevclass, PNAME)) != NIL)
	&& (ntype(lval_pname) == STRING)) {
      return (xlenter(getstring(lval_pname)));
    }
    else
      return (xlenter("TANGO:IMAGE_CLASS"));
  }
}


/*****************************************************************************
 * This routine accesses the TANGO_IMAGE_TYPE_ID class variable that was
 * initialized in the above Tcls_Create_Subclass_Of_TANGOIMAGE_CLASS(). This
 * routine expects parameter o_class to be a class-object, that is
 * xlobj.c:xlclass_p(o_class) must be true. If your code
 * accidentally gives this routine an object instance rather than a class
 * instance, then you're hosed as the while loop will go trapseing off
 * through memory in a random fashion....
 *
 * This routine may be called with a class object as created by
 * Tcls_Create_Subclass_Of_TANGOIMAGE_CLASS(), or it may be called with
 * a class object that is a subclass of a class generated by
 * Tcls_Create_Subclass_Of_TANGOIMAGE_CLASS(). In other words, it
 * will work for TANGO:IMAGE_CLASS subclasses created in lisp too.
 ****************************************************************************/
TANGO_IMAGE_TYPE Tcls_TANGOIMAGECLASSOBJ_To_TANGO_IMAGE_TYPE(o_class)
     LVAL o_class;		/* OBJECT satisfying xlclass_p() */
{
  LVAL v_cvals;
  LVAL o_prevclass = NIL;
  
  /* loop through superclasses, stopping at WIDGET_CLASS or NIL(superclass of object) */
  while ((o_class != o_TANGOIMAGE_CLASS) && (o_class != NIL)) {
    o_prevclass = o_class;
    o_class = getivar(o_class, SUPERCLASS);
  }

  /*
   * if the loop terminated with o_CLASS==o_TANGOIMAGE_CLASS and o_prevclass!=NIL
   * then o_prevclass is the subclass of TANGO:IMAGE_CLASS created by
   * Tcls_Create_Subclass_Of_TANGOIMAGE_CLASS() in which the class variable
   * TANGO_IMAGE_TYPE_ID was set. So return that value as TANGO_IMAGE_TYPE, else '-1'.
   */
  if ((o_class == o_TANGOIMAGE_CLASS)
      && (o_prevclass != NIL)
      && ((v_cvals = getivar(o_prevclass, CVALS)) != NIL))
    return ((TANGO_IMAGE_TYPE) getfixnum(getelement(v_cvals, TANGO_IMAGE_TYPE_ID)));
  else
    return ((TANGO_IMAGE_TYPE) -1); /* -1 indicates error */
}


/******************************************************************************
 * (TANGOIMAGEOBJP <expr>)
 * returns T if argument is a TANGOIMAGEOBJ, else NIL
 ******************************************************************************/
LVAL Tcls_Prim_TANGOIMAGEOBJP()
{
  extern LVAL true;
  LVAL arg = xlgetarg();
  xllastarg();
  return (tangoimageobj_p(arg) ? true : NIL);
}
