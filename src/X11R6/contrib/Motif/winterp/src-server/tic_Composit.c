/* -*-C-*-
********************************************************************************
*
* File:         tic_Composit.c
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/tic_Composit.c,v 2.1 1994/06/06 15:41:09 npm Exp $
* Description:  TANGO:COMPOSITE_IMAGE_CLASS (subclass of TANGO:IMAGE_CLASS).
* Author:       Niels P. Mayer
* Created:      Sat May 15 21:36:37 1993
* Modified:     Sun Jun  5 14:25:22 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/RCS/tic_Composit.c,v 2.1 1994/06/06 15:41:09 npm Exp $";

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
#include "w_funtab.h"
#include "tango.h"


#if 0 /* commented out */
/******************************************************************************/
  TANGOimage_create(TANGO_IMAGE_TYPE_COMPOSITE...):
     1. pull varargs named 'type', lx, ly, vis args from arg stack
     2. pull vararg named 'ims' from arg stack
     3. new_image = image_create(TANGO_IMAGE_TYPE_COMPOSITE,lx,ly,vis,ims);
     	3A. pull varargs named 'type', lx, ly, vis from arg stack,
	3B. new_image = (TANGO_IMAGE) malloc( sizeof( struct _IMAGE));
            new_image->type = type;
	3C. pull vararg 'ims' from arg stack
	3D. composite_create(new_image,lx,ly,vis,ims);
		3D1. composite = (TANGO_COMPOSITE_PTR) malloc(sizeof(struct _TANGO_COMPOSITE));
		3D2. composite->image_list = read_composite(subimages);
			3D2A.    TANGO_IMAGE_TYPE  type;
			3D2A.    char 	    *args,*p;
			3D2A.    int		     num,i;
			3D2A.    TANGO_IMAGE	     image,head,tail;
			3D2A.    WIN_COORD	     lx,ly,sx,sy,rad,vx[7],vy[7];
			3D2A.    int		     color,arrow,orient,vertices;
			3D2A.    double	     wid,sty,fill;
			3D2A.    char 	     colorstr[STRINGLENGTH];
			3D2A.    char 	     fontname[STRINGLENGTH];
			3D2A.    char 	    *fname;
			3D2A.    char 	     text[STRINGLENGTH];
			3D2A. 
			3D2A.    head = tail = NULL;
			3D2A.    num = 0;
			3D2A.    do
			3D2A.       { type = subimages[num].type;
			3D2A. 	args = subimages[num].args;
			3D2A. 	if (type == TANGO_IMAGE_TYPE_COMPOSITE) break;
			3D2A. 
			3D2A. 	switch (type)
			3D2A. 	{
			3D2A. 	   case TANGO_IMAGE_TYPE_LINE:
			3D2A. 	      sscanf(args,"%lf %lf %s %lf %lf %lf %lf %d",
			3D2A. 			  &lx,&ly,colorstr,&sx,&sy,&wid,&sty,&arrow);
			3D2A. 	      color = get_color(colorstr);
			3D2A. 	      image = image_create(TANGO_IMAGE_TYPE_LINE,lx,ly,1,color,
			3D2A. 				   sx,sy,wid,sty,arrow);
			3D2A. 	      break;
			3D2A. 	   case TANGO_IMAGE_TYPE_RECTANGLE:
			3D2A. 	      sscanf(args,"%lf %lf %s %lf %lf %lf",
			3D2A. 			&lx,&ly,colorstr,&sx,&sy,&fill);
			3D2A. 	      color = get_color(colorstr);
			3D2A. 	      image = image_create(TANGO_IMAGE_TYPE_RECTANGLE,lx,ly,1,
			3D2A. 					color,sx,sy,fill);
			3D2A. 	      break;
			3D2A. 	   case TANGO_IMAGE_TYPE_CIRCLE:
			3D2A. 	      sscanf(args,"%lf %lf %s %lf %lf",&lx,&ly,colorstr,&rad,&fill);
			3D2A. 	      color = get_color(colorstr);
			3D2A. 	      image = image_create(TANGO_IMAGE_TYPE_CIRCLE,lx,ly,1,color,
			3D2A. 					rad,fill);
			3D2A. 	      break;
			3D2A. 	   case TANGO_IMAGE_TYPE_ELLIPSE:
			3D2A. 	      sscanf(args,"%lf %lf %s %lf %lf %lf",
			3D2A. 			&lx,&ly,colorstr,&sx,&sy,&fill);
			3D2A. 	      color = get_color(colorstr);
			3D2A. 	      image = image_create(TANGO_IMAGE_TYPE_ELLIPSE,lx,ly,1,color,
			3D2A. 				sx,sy,fill);
			3D2A. 	      break;
			3D2A. 	   case TANGO_IMAGE_TYPE_POLYLINE:
			3D2A. 	      sscanf(args,"%lf %lf %s %lf %lf %d %d",
			3D2A. 			&lx,&ly,colorstr,&wid,&sty,&arrow,&vertices);
			3D2A. 	      color = get_color(colorstr);
			3D2A. 	      p = args;
			3D2A. 	      while (*p == ' ') p++;
			3D2A. 	      for (i=1; i<=7; ++i)	   /* get past prelim args */
			3D2A. 		 { while (*p != ' ') p++;
			3D2A. 		   while (*p == ' ') p++;
			3D2A. 		 }
			3D2A. 	      for (i=0; i<vertices-1; ++i)
			3D2A. 		 { sscanf(p,"%lf",&(vx[i]));
			3D2A. 		   while (*p != ' ') p++;
			3D2A. 		   while (*p == ' ') p++;
			3D2A. 		   sscanf(p,"%lf",&(vy[i]));
			3D2A. 		   while (*p != ' ')
			3D2A. 		      { if (*p == '\0') break;
			3D2A. 			p++;
			3D2A. 		      }
			3D2A. 		   if (*p != '\0')
			3D2A. 		      while (*p == ' ') p++;
			3D2A. 		 }
			3D2A. 	      image = image_create(TANGO_IMAGE_TYPE_POLYLINE,lx,ly,1,color,
			3D2A. 				   vertices,vx,vy,wid,sty,arrow);
			3D2A. 	      break;
			3D2A. 	   case TANGO_IMAGE_TYPE_POLYGON:
			3D2A. 	      sscanf(args,"%lf %lf %s %lf %d",
			3D2A. 			&lx,&ly,colorstr,&fill,&vertices);
			3D2A. 	      color = get_color(colorstr);
			3D2A. 	      p = args;
			3D2A. 	      while (*p == ' ') p++;
			3D2A. 	      for (i=1; i<=5; ++i)	   /* get past prelim args */
			3D2A. 		 { while (*p != ' ') p++;
			3D2A. 		   while (*p == ' ') p++;
			3D2A. 		 }
			3D2A. 	      for (i=0; i<vertices-1; ++i)
			3D2A. 		 { sscanf(p,"%lf",&(vx[i]));
			3D2A. 		   while (*p != ' ') p++;
			3D2A. 		   while (*p == ' ') p++;
			3D2A. 		   sscanf(p,"%lf",&(vy[i]));
			3D2A. 		   while (*p != ' ')
			3D2A. 		      { if (*p == '\0') break;
			3D2A. 			p++;
			3D2A. 		      }
			3D2A. 		   if (*p != '\0')
			3D2A. 		      while (*p == ' ') p++;
			3D2A. 		 }
			3D2A. 	      image = image_create(TANGO_IMAGE_TYPE_POLYGON,lx,ly,1,color,
			3D2A. 					vertices, vx,vy,fill);
			3D2A. 	      break;
			3D2A. 	   case TANGO_IMAGE_TYPE_SPLINE:
			3D2A. 	      sscanf(args,"%lf %lf %s %lf %lf %d",
			3D2A. 			&lx,&ly,colorstr,&wid,&sty,&vertices);
			3D2A. 	      color = get_color(colorstr);
			3D2A. 	      p = args;
			3D2A. 	      while (*p == ' ') p++;
			3D2A. 	      for (i=1; i<=6; ++i)	   /* get past prelim args */
			3D2A. 		 { while (*p != ' ') p++;
			3D2A. 		   while (*p == ' ') p++;
			3D2A. 		 }
			3D2A. 	      for (i=0; i<vertices-1; ++i)
			3D2A. 		 { sscanf(p,"%lf",&(vx[i]));
			3D2A. 		   while (*p != ' ') p++;
			3D2A. 		   while (*p == ' ') p++;
			3D2A. 		   sscanf(p,"%lf",&(vy[i]));
			3D2A. 		   while (*p != ' ')
			3D2A. 		      { if (*p == '\0') break;
			3D2A. 			p++;
			3D2A. 		      }
			3D2A. 		   if (*p != '\0')
			3D2A. 		      while (*p == ' ') p++;
			3D2A. 		 }
			3D2A. 	      image = image_create(TANGO_IMAGE_TYPE_SPLINE,lx,ly,1,color,
			3D2A. 					vertices,vx,vy,wid,sty);
			3D2A. 	      break;
			3D2A. 	    case TANGO_IMAGE_TYPE_TEXT:
			3D2A. 	      sscanf(args,"%lf %lf %s %s %s %d",
			3D2A. 			&lx,&ly,colorstr,fontname,text,&orient);
			3D2A. 	      color = get_color(colorstr);
			3D2A. 	      if ((strcmp(fontname,"NULL") == 0) ||
			3D2A. 		  (strcmp(fontname,"0") == 0))
			3D2A. 		 fname = NULL;
			3D2A. 	      else
			3D2A. 		 fname = fontname;
			3D2A. 	      image = image_create(TANGO_IMAGE_TYPE_TEXT,lx,ly,1,color,
			3D2A. 					fname,text,orient);
			3D2A. 	      break;
			3D2A. 	    case TANGO_IMAGE_TYPE_BITMAP:
			3D2A. 	      COMPLAIN("Warning: TANGO_IMAGE_TYPE_BITMAP not %s\n",
			3D2A. 		       "supported in COMPOSITE images.");
			3D2A. 	      continue;
			3D2A. 	    default :
			3D2A. 	      COMPLAIN("Warning: Unknown image type in COMPOSITE image: %d\n",
			3D2A. 		       type);
			3D2A. 	      continue;
			3D2A. 	}
			3D2A. 	++num;
			3D2A. 
			3D2A. 	image->nexti = NULL;
			3D2A. 	if (!head)
			3D2A. 	   head = image;
			3D2A. 	else
			3D2A. 	   tail->nexti = image;
			3D2A. 	tail = image;
			3D2A.       } while(1==1);
			3D2A. 
			3D2A.    return(head);
                                       /***^above obj set to composite->image_list in caller */
		3D3. new_image->object = composite;
		3D4. new_image->visible = vis;
		4D5. new_image->loc[0] = lx;
		3D6. new_image->loc[1] = ly;
        3E. new_image->nexti = NULL;
            return( new_image );
     4. * add this image onto the configuration draw list */
        | 
        | im = (IMAGE_PTR) malloc( sizeof( struct IMAGE) );
        | im->image = new_image;
        | im->previ = NULL;
        | im->nexti = TANGO__data->confighead;
        | if (!TANGO__data->confighead)
        |    TANGO__data->configtail = im;
        | else
        |    TANGO__data->confighead->previ = im;
        | TANGO__data->confighead = im;
        | new_image->alive = 1;
        | new_image->inconfig = im;
        | TANGO_image_damage(new_image);
        |
        | return(new_image);
/******************************************************************************/
#endif /* commented out */


/*******************************************************************************
 *
 *******************************************************************************/
static TANGO_IMAGE Xtango_Get_Args_Create_Image(err_string, err_lval)
     char* *err_string;
     LVAL  *err_lval;
{
  extern LVAL s_unbound;
  extern Boolean Xtango_Get_Line_Image_Args(); /* tic_Line.c */
  extern Boolean Xtango_Get_Rectangle_Image_Args(); /* tic_Rect.c */
  extern Boolean Xtango_Get_Circle_Image_Args(); /* tic_Circle.c */
  extern Boolean Xtango_Get_Ellipse_Image_Args(); /* tic_Ellipse.c */
  extern Boolean Xtango_Get_Polyline_Image_Args(); /* tic_Polyline.c */
  extern Boolean Xtango_Get_Polygon_Image_Args(); /* tic_Polygon.c */
  extern Boolean Xtango_Get_Spline_Image_Args(); /* tic_Spline.c */
  extern Boolean Xtango_Get_Text_Image_Args(); /* tic_Text.c */
  extern Boolean Xtango_Get_Bitmap_Image_Args(); /* tic_Bitmap.c */
  extern Boolean Xtango_Get_GIF_Image_Args(); /* tic_GIF.c */
  WIN_COORD		lx, ly, sx, sy, rad, vx[7], vy[7];
  int			color, arrow, orient, vertices, n, bwid, bhei;
  double		wid, sty, fill;
  int			*bmaps;
  char			*fname;
  char			*text;
  XImage*               ximage;
  int                   num_alloc_cols;
  unsigned long*        alloc_cols;
  LVAL			lval_arg, lval_type, lval_color;
  TANGO_IMAGE_TYPE	image_type;

  /*
   * Fetch the 1-st argument in image description sequence, a subclass of
   * TANGO:IMAGE_CLASS. Set <image_type> from this value.
   */
  if (moreargs()) {
    lval_arg = lval_type = nextarg();
    if (objectp(lval_arg)
	&& xlclass_p(lval_arg)
	&& ((image_type = Tcls_TANGOIMAGECLASSOBJ_To_TANGO_IMAGE_TYPE(lval_arg)) /* Tcls_TANGOIMAGECLASSOBJ_To_TANGO_IMAGE_TYPE() returns -1 on error */
	    != -1)) {
      /* image_type is valid */
    }
    else {
      *err_string = string_err_msg_bad_image_class;
      *err_lval   = lval_arg;
      return ((TANGO_IMAGE) NULL); /* RETURN NULL to signal error to caller. */
    }
  }
  else {
    *err_string = string_err_msg_bad_image_class;
    *err_lval   = s_unbound;
    return ((TANGO_IMAGE) NULL); /* RETURN NULL to signal error to caller. */
  }

  /*
   * Subsequent arguments are dependent on 'image_type', so call argument retrieval
   * and image-creation routines based on that...
   */
  switch (image_type) {

  case TANGO_IMAGE_TYPE_LINE:

    if (Xtango_Get_Line_Image_Args(err_string, err_lval,
				   &lx, &ly, &lval_color,
				   &sx, &sy, &wid, &sty, &arrow)) {
      color = Xtango_Cvt_LVAL_To_TANGO_COLOR_Else_Default(lval_color);
      return (image_create(TANGO_IMAGE_TYPE_LINE, lx, ly, 1, color,
			   sx, sy, wid, sty, arrow));
    }
    else
      /* Note that err_string and err_lval set when above routine returns FALSE */
      return ((TANGO_IMAGE) NULL); /* RETURN NULL to signal error to caller. */
    break;

  case TANGO_IMAGE_TYPE_RECTANGLE:

    if (Xtango_Get_Rectangle_Image_Args(err_string, err_lval,
					&lx, &ly, &lval_color,
					&sx, &sy, &fill)) {
      color = Xtango_Cvt_LVAL_To_TANGO_COLOR_Else_Default(lval_color);
      return (image_create(TANGO_IMAGE_TYPE_RECTANGLE, lx, ly, 1, color,
			   sx, sy, fill));
    }
    else
      /* Note that err_string and err_lval set when above routine returns FALSE */
      return ((TANGO_IMAGE) NULL); /* RETURN NULL to signal error to caller. */
    break;

  case TANGO_IMAGE_TYPE_CIRCLE:

   if (Xtango_Get_Circle_Image_Args(err_string, err_lval,
				     &lx, &ly, &lval_color,
				     &rad, &fill)) {
      color = Xtango_Cvt_LVAL_To_TANGO_COLOR_Else_Default(lval_color);
      return (image_create(TANGO_IMAGE_TYPE_CIRCLE, lx, ly, 1, color,
			   rad, fill));
    }
    else
      /* Note that err_string and err_lval set when above routine returns FALSE */
      return ((TANGO_IMAGE) NULL); /* RETURN NULL to signal error to caller. */
    break;

  case TANGO_IMAGE_TYPE_ELLIPSE:

    if (Xtango_Get_Ellipse_Image_Args(err_string, err_lval,
				      &lx, &ly, &lval_color,
				      &sx, &sy, &fill)) {
      color = Xtango_Cvt_LVAL_To_TANGO_COLOR_Else_Default(lval_color);
      return (image_create(TANGO_IMAGE_TYPE_ELLIPSE, lx, ly, 1, color,
			   sx, sy, fill));
    }
    else
      /* Note that err_string and err_lval set when above routine returns FALSE */
      return ((TANGO_IMAGE) NULL); /* RETURN NULL to signal error to caller. */
    break;

  case TANGO_IMAGE_TYPE_POLYLINE:

    if (Xtango_Get_Polyline_Image_Args(err_string, err_lval,
				       &lx, &ly, &lval_color,
				       &vertices, vx, vy,
				       &wid, &sty, &arrow)) {
      color = Xtango_Cvt_LVAL_To_TANGO_COLOR_Else_Default(lval_color);
      return (image_create(TANGO_IMAGE_TYPE_POLYLINE, lx, ly, 1, color,
			   vertices, vx, vy, wid, sty, arrow));
    }
    else
      /* Note that err_string and err_lval set when above routine returns FALSE */
      return ((TANGO_IMAGE) NULL); /* RETURN NULL to signal error to caller. */
    break;

  case TANGO_IMAGE_TYPE_POLYGON:

    if (Xtango_Get_Polygon_Image_Args(err_string, err_lval,
				      &lx, &ly, &lval_color,
				      &vertices, vx, vy,
				      &fill)) {
      color = Xtango_Cvt_LVAL_To_TANGO_COLOR_Else_Default(lval_color);
      return (image_create(TANGO_IMAGE_TYPE_POLYGON, lx, ly, 1, color,
			   vertices, vx, vy, fill));
    }
    else
      /* Note that err_string and err_lval set when above routine returns FALSE */
      return ((TANGO_IMAGE) NULL); /* RETURN NULL to signal error to caller. */
    break;

  case TANGO_IMAGE_TYPE_SPLINE:

    if (Xtango_Get_Spline_Image_Args(err_string, err_lval,
				     &lx, &ly, &lval_color,
				     &vertices, vx, vy,
				     &wid, &sty)) {
      color = Xtango_Cvt_LVAL_To_TANGO_COLOR_Else_Default(lval_color);
      return (image_create(TANGO_IMAGE_TYPE_SPLINE, lx, ly, 1, color,
			   vertices, vx, vy, wid, sty));
    }
    else
      /* Note that err_string and err_lval set when above routine returns FALSE */
      return ((TANGO_IMAGE) NULL); /* RETURN NULL to signal error to caller. */
    break;

  case TANGO_IMAGE_TYPE_TEXT:

    if (Xtango_Get_Text_Image_Args(err_string, err_lval,
				   &lx, &ly, &lval_color,
				   &fname, &text, &orient)) {
      color = Xtango_Cvt_LVAL_To_TANGO_COLOR_Else_Default(lval_color);
      return (image_create(TANGO_IMAGE_TYPE_TEXT, lx, ly, 1, color,
			   fname, text, orient));
    }
    else
      /* Note that err_string and err_lval set when above routine returns FALSE */
      return ((TANGO_IMAGE) NULL); /* RETURN NULL to signal error to caller. */
    break;

  case TANGO_IMAGE_TYPE_BITMAP:

    if (Xtango_Get_Bitmap_Image_Args(err_string, err_lval,
				     &lx, &ly,
				     &bmaps, &n, &bwid, &bhei)) {
      return (image_create(TANGO_IMAGE_TYPE_BITMAP, lx, ly, 1,
			   bmaps, n, bwid, bhei));
    }
    else
      /* Note that err_string and err_lval set when above routine returns FALSE */
      return ((TANGO_IMAGE) NULL); /* RETURN NULL to signal error to caller. */
    break;

  case TANGO_IMAGE_TYPE_PIXMAP:

    if (Xtango_Get_GIF_Image_Args(err_string, err_lval,
				  &lx, &ly,
				  &ximage, &num_alloc_cols, &alloc_cols)) {
      return (image_create(TANGO_IMAGE_TYPE_PIXMAP, lx, ly, 1,
			   ximage, num_alloc_cols, alloc_cols));
    }
    else
      /* Note that err_string and err_lval set when above routine returns FALSE */
      return ((TANGO_IMAGE) NULL); /* RETURN NULL to signal error to caller. */
    break;

  default:			/* TANGO_IMAGE_TYPE_COMPOSITE not supported */

    *err_string = string_err_msg_bad_image_class;
    *err_lval   = lval_type;
    return ((TANGO_IMAGE) NULL); /* RETURN NULL to signal error to caller. */
    break;
  }
}


/*******************************************************************************
 *
 *******************************************************************************/
static TANGO_IMAGE Xtango_Get_Args_Create_Image_List_Else_Error()
{ 
  TANGO_IMAGE	image, head, tail;
  char*		err_string;
  LVAL		err_lval;

  head = tail = NULL;

  for (; (moreargs());) {
    if ((image = Xtango_Get_Args_Create_Image(&err_string, &err_lval))) { /* above routine returns NULL on error */
      image->nexti = NULL;
      if (!head)
	head = image;
      else
	tail->nexti = image;
      tail = image;
    }
    else {
      while (head) {		/* free up any TANGO_IMAGEs created prior to error condition */
	image = head->nexti;
	Tcls_Free_TANGO_IMAGE(head);
	head = image;
      }
      Xtango_Restore_Context();
      xlerror(err_string, err_lval);
    }
  }
  return(head);
}


/*******************************************************************************
 *
 *******************************************************************************/
static TANGO_IMAGE Xtango_Get_Args_Create_Composite_Image(lx, ly, vis)
     double lx;
     double ly;
     int vis;
{
  TANGO_IMAGE new_image, im_list;
  TANGO_COMPOSITE_PTR composite;
  IMAGE_PTR im;

  /* call Xtango_Get_Args_Create_Image_List_Else_Error() first because if there are
     errors in parsing the arguments, we don't want to worry about preceding
     any xlisp error-exits with code that has to remove malloc'd data from
     TANGO__data->confighead and TANGO__data->configtail (also requires freeing
     the malloc'd struct _IMAGE and struct _TANGO_COMPOSITE). Also note that
     any errors returned by Xtango_Get_Args_Create_Image_List_Else_Error() will
     also do a Xtango_Restore_Context() since I assume this proc will only be used
     inside a WINTERP prim that already set up the tango context via a call to
     Xtango_Save_Set_Context(). */
  im_list = Xtango_Get_Args_Create_Image_List_Else_Error(); /* see xtangoimage.c:read_composite() */

  /* BEGIN xtangoimage.c:TANGOimage_create(TANGO_IMAGE_TYPE_COMPOSITE...) */
  /* BEGIN xtangoimage.c:image_create(TANGO_IMAGE_TYPE_COMPOSITE...) */
  new_image = (TANGO_IMAGE) XtMalloc(sizeof( struct _IMAGE));
  new_image->type = TANGO_IMAGE_TYPE_COMPOSITE;

  /* BEGIN xtangoimage.c:composite_create() */
  composite = (TANGO_COMPOSITE_PTR) XtMalloc(sizeof(struct _TANGO_COMPOSITE));
  composite->image_list = im_list; /* in xtangoimage.c:composite_create(), was originally xtangoimage.c:read_composite() */
  new_image->object = composite;
  new_image->visible = vis;
  new_image->loc[0] = lx;
  new_image->loc[1] = ly;

  /* CONTINUE xtangoimage.c:image_create(TANGO_IMAGE_TYPE_COMPOSITE...) */
  new_image->nexti = NULL;
  /* END xtangoimage.c:image_create(TANGO_IMAGE_TYPE_COMPOSITE...) -- return(new_image) */

  /* CONTINUE xtangoimage.c:TANGOimage_create(TANGO_IMAGE_TYPE_COMPOSITE...) */
  im = (IMAGE_PTR) XtMalloc(sizeof(struct IMAGE));
  im->image = new_image;
  im->previ = NULL;
  im->nexti = TANGO__data->confighead;
  if (!TANGO__data->confighead)
    TANGO__data->configtail = im;
  else
    TANGO__data->confighead->previ = im;
  TANGO__data->confighead = im;
  new_image->alive = 1;
  new_image->inconfig = im;
  TANGO_image_damage(new_image);
  /* END xtangoimage.c:TANGOimage_create(TANGO_IMAGE_TYPE_COMPOSITE...) -- return (new_image) */

  return (new_image);
}


/*****************************************************************************
 * (send TANGO:COMPOSITE_IMAGE_CLASS :new
 *       [:show] [<visible_kwd>] <tango_widget>
 *       <location_coord>
 *       <class-specific-image-instance-creation-arg>
 *       [<class-specific-image-instance-creation-arg>
 * 	...
 *       [<class-specific-image-instance-creation-arg>]]
 *       )
 * 	==> RETURNS an <tango_image> object.
 * 
 * [:show] -- OPTIONAL :show keyword. If present, displays image
 * immediately, else the image will be displayed along with the next
 * animation frame. See :TAP_SHOW :TX_DELAY.
 * 
 * [<visible_kwd>] -- OPTIONAL :VISIBLE or :INVISIBLE keyword. If
 * omitted, :VISIBLE is assumed. See also :TX_VISIBLE, :TAP_VIS_TOGGLE.
 * 
 * <tango_widget> -- an instance of TANGO:WIDGET_CLASS
 * 
 * <location_coord> -- the location for placing the composite image. A
 * COMPLEX number, of form #C(<loc_x> <loc_y>), where <loc_x> is a FLONUM,
 * typically [0.0 - 1.0] representing the X-axis location; <loc_y> is a
 * FLONUM, typically [0.0 - 1.0] representing the Y-axis location.
 * 
 * [class-specific-image-instance-creation-arg> is a sequence of
 * arguments representing the image-class-specific data needed to create
 * an instance of the specified class, e.g.
 * 
 *	 TANGO:TEXT_IMAGE_CLASS #C(0.25 0.25) :ctr "text image class"
 *				TANGO_COLOR_MAROON "6x13"
 * 
 * Each sequence [class-specific-image-instance-creation-args...] becomes
 * an element displayed in the composite image. The order, types and
 * numbers of parameters correspond to the arguments used in creating a
 * tango-image (see :NEW method descriptions for all the other subclasses
 * of TANGO:IMAGE_CLASS).  For example, if some images were created with
 * the following code
 * 
 * 	(send TANGO:CIRCLE_IMAGE_CLASS :new :show tango_w
 * 	      #C(0.25 0.25) 0.2 t TANGO_COLOR_MAROON 1.0)
 * 	(send TANGO:POLYGON_IMAGE_CLASS :new :show tango_w
 * 	      #C(0.50 0.50) #C(0.10 0.10) #C(0.10 0.10)
 * 	      "red" 1.0)
 * 
 * then you might use the following to create a composite image containing
 * the above images with the following call:
 * 
 * 	(send TANGO:COMPOSITE_IMAGE_CLASS :new
 * 	      :show tango_w #C(0.25 0.25)
 *               TANGO:CIRCLE_IMAGE_CLASS
 * 		#C(0.25 0.25) 0.2 TANGO_COLOR_MAROON 1.0
 * 	      TANGO:POLYGON_IMAGE_CLASS
 * 		#C(0.50 0.50) #C(0.10 0.10)#C(0.10 0.10) "red" 1.0
 * 	)
 * ==========================================================================
 * TANGO_IMAGE
 * TANGOimage_create(TANGO_IMAGE_TYPE_COMPOSITE,lx,ly,vis,im)
 *	double lx,ly;
 *	int vis;
 *      TANGO_IMAGE_COMPONENT im;
 ****************************************************************************/
LVAL Tango_Composite_Image_Class_Method_ISNEW()
{
  extern LVAL s_unbound;
  TANGO_IMAGE image;
  WINTERP_TANGO_CONTEXT context;
  LVAL o_widget;
  LVAL lval_arg;
  double loc_x_float, loc_y_float;

  /* get <self> from argument stack */
  LVAL self
    = Tcls_Get_OBJECT_Arg_Returning_TANGOIMAGEOBJ();

  /* get OPTIONAL [:show] from argument stack */
  int show_p
    = (moreargs() && (*xlargv == k_SHOW))
      ? (nextarg(), TRUE)
	: FALSE;

  /* get OPTIONAL [<visible_kwd>] from argument stack */
  int visible_p
    = (moreargs() && ((*xlargv == k_VISIBLE) || (*xlargv == k_INVISIBLE)))
      ? ((nextarg() == k_VISIBLE) ? TRUE : FALSE)
	: TRUE;

  /* get <tango_widget> from argument stack */
  Widget widget_id
    = Xtango_Get_TANGO_WIDGETOBJ_Returning_Validated_WidgetID(&o_widget);

  /* get <location_coord> from argument stack */
  if (moreargs()) {
    lval_arg = nextarg();
    if (complexp(lval_arg) && floatp(getelement(lval_arg, 0)) && floatp(getelement(lval_arg, 1))) {
      loc_x_float = (double) getflonum(getelement(lval_arg, 0));
      loc_y_float = (double) getflonum(getelement(lval_arg, 1));
    }
    else {
      if (!complexp(lval_arg))
	xlerror(string_err_msg_bad_loc_coord, lval_arg);
      else if (!floatp(getelement(lval_arg, 0)))
	xlerror(string_err_msg_bad_loc_coord, getelement(lval_arg, 0));
      else if (!floatp(getelement(lval_arg, 1)))
	xlerror(string_err_msg_bad_loc_coord, getelement(lval_arg, 1));
      else
	xlerror(string_err_msg_bad_loc_coord, lval_arg);
    }
  }
  else
    xlerror(string_err_msg_bad_loc_coord, s_unbound);

  /*
   * NOTE that we set up context, in this case, before finishing parsing
   * arguments. Therefore, we must do Xtango_Restore_Context() prior to
   * any error-exits caused by invalid argument problems... In other words
   * routines in here can't use the traditional xlgetarg() and xlga*()
   * routines for retrieving and type-validating XLISP arguments.
   */
  context = Xtango_Find_Context_From_WidgetID(widget_id);
  Xtango_Save_Set_Context(context);
  Xtango_Check_Initialized();

   /*** NORMALLY, we'd do the following call
  image = TANGOimage_create(TANGO_IMAGE_TYPE_COMPOSITE,loc_x_float,loc_y_float,visible_p,im);
   *** HOWEVER, Tango's way of entering subimages is pretty painful/ugly for
   *** Lisp usage, so instead, we create our own version of the above */

  /*
   * Get remaining arguments, parse each image-creation sequence and create TANGO_IMAGE;
   * The total set of image-creation sequences are returned as new composite 'image'.
   */
  image = Xtango_Get_Args_Create_Composite_Image(loc_x_float, loc_y_float, visible_p);

  xllastarg();			/* Xtango_Get_Args_Create_Composite_Image() always eats all remaining args, else error... so this not necessary implementation-wise, just notation-wise... */

  Tcls_Initialize_TANGOIMAGEOBJ(self, image, context);

  Xtango_Restore_Context();

  if (show_p)
    TAPshow(image);

  return (self);
}

static LVAL s_TANGO_IMAGE_TYPE_LINE, s_TANGO_IMAGE_TYPE_RECTANGLE, s_TANGO_IMAGE_TYPE_CIRCLE,
  s_TANGO_IMAGE_TYPE_POLYLINE, s_TANGO_IMAGE_TYPE_POLYGON, s_TANGO_IMAGE_TYPE_ELLIPSE,
  s_TANGO_IMAGE_TYPE_SPLINE, s_TANGO_IMAGE_TYPE_TEXT, s_TANGO_IMAGE_TYPE_BITMAP,
  s_TANGO_IMAGE_TYPE_PIXMAP;

/*****************************************************************************
 *
 ****************************************************************************/
static void Tango_Composite_Image_Get_Values_And_Append(image, result)
     TANGO_IMAGE image;
     LVAL result;
{
  extern LVAL Tango_Line_Image_Get_Values(); /* tic_Line.c */
  extern LVAL Tango_Rectangle_Image_Get_Values(); /* tic_Rect.c */
  extern LVAL Tango_Circle_Image_Get_Values(); /* tic_Circle.c */
  extern LVAL Tango_Polyline_Image_Get_Values(); /* tic_Polyline.c */
  extern LVAL Tango_Polygon_Image_Get_Values(); /* tic_Polygon.c */
  extern LVAL Tango_Ellipse_Image_Get_Values();	/* tic_Ellipse.c */
  extern LVAL Tango_Spline_Image_Get_Values(); /* tic_Spline.c */
  extern LVAL Tango_Text_Image_Get_Values(); /* tic_Text.c */
  extern LVAL Tango_Bitmap_Image_Get_Values(); /* tic_Bitmap.c */
  extern LVAL Tango_GIF_Image_Get_Values(); /* tic_GIF.c */
  LVAL lval_result, prev, cur;
  TANGO_IMAGE cur_im;

  /* protect some pointers */
  xlsave1(lval_result);

  for (cur_im = ((TANGO_COMPOSITE_PTR) image->object)->image_list;
       (cur_im); cur_im = cur_im->nexti) {
    switch (cur_im->type) {
    case TANGO_IMAGE_TYPE_LINE:
      lval_result = cons(s_TANGO_IMAGE_TYPE_LINE, Tango_Line_Image_Get_Values(cur_im));
      break;
    case TANGO_IMAGE_TYPE_RECTANGLE:
      lval_result = cons(s_TANGO_IMAGE_TYPE_RECTANGLE, Tango_Rectangle_Image_Get_Values(cur_im));
      break;
    case TANGO_IMAGE_TYPE_CIRCLE:
      lval_result = cons(s_TANGO_IMAGE_TYPE_CIRCLE, Tango_Circle_Image_Get_Values(cur_im));
      break;
    case TANGO_IMAGE_TYPE_POLYLINE:
      lval_result = cons(s_TANGO_IMAGE_TYPE_POLYLINE, Tango_Polyline_Image_Get_Values(cur_im));
      break;
    case TANGO_IMAGE_TYPE_POLYGON:
      lval_result = cons(s_TANGO_IMAGE_TYPE_POLYGON, Tango_Polygon_Image_Get_Values(cur_im));
      break;
    case TANGO_IMAGE_TYPE_ELLIPSE:
      lval_result = cons(s_TANGO_IMAGE_TYPE_ELLIPSE, Tango_Ellipse_Image_Get_Values(cur_im));
      break;
    case TANGO_IMAGE_TYPE_SPLINE:
      lval_result = cons(s_TANGO_IMAGE_TYPE_SPLINE, Tango_Spline_Image_Get_Values(cur_im));
      break;
    case TANGO_IMAGE_TYPE_TEXT:
      lval_result = cons(s_TANGO_IMAGE_TYPE_TEXT, Tango_Text_Image_Get_Values(cur_im));
      break;
    case TANGO_IMAGE_TYPE_BITMAP:
      lval_result = cons(s_TANGO_IMAGE_TYPE_BITMAP, Tango_Bitmap_Image_Get_Values(cur_im));
      break;
    case TANGO_IMAGE_TYPE_PIXMAP:
      lval_result = cons(s_TANGO_IMAGE_TYPE_PIXMAP, Tango_GIF_Image_Get_Values(cur_im));
      break;
    default:
      COMPLAIN("Warning: Unknown TANGO_IMAGE_TYPE value %d in Tango_Composite_Image_Get_Values()",
	       cur_im->type);
      break;
    }
    /* After loop executes, "prev" points to tail of "result" */
    for (prev = NIL, cur = result; (cur != NIL); prev = cur, cur = cdr(cur)) {}
    /* append lval_result... */
    rplacd(prev, lval_result);
    /* set up next iteration of for loop above (which cdr's to end of list... */
    result = lval_result;
  }

  /* restore the stack */
  xlpop();
}


/*****************************************************************************
 * (send <composite_image> :storeon)
 *	==> returns list (send TANGO:COMPOSITE_IMAGE_CLASS :new <visibility_kwd>
 *				*TANGO_WIDGET*
 *				#C(<location-x> <location-y>)
 *			        [class-specific-image-instance-creation-args...]
 *				[class-specific-image-instance-creation-args...]
 *				      ...)
 ****************************************************************************/
LVAL Tango_Composite_Image_Class_Method_STOREON()
{
  extern LVAL s_SEND, k_NEW, s_TANGO_W; /* wc_Xtango.c */
  LVAL o_image, lval_result;
  TANGO_IMAGE image;

  xlsave1(lval_result);		/* protect from gc */

  image = Tcls_Get_TANGOIMAGEOBJ_Arg_Returning_Validated_TangoImageID(&o_image);
  xllastarg();

  lval_result = cons(newdcomplex((double) image->loc[0], (double) image->loc[1]),
		     lval_result);
  lval_result = cons(s_TANGO_W,
		     lval_result);
  lval_result = cons((image->visible) ? k_VISIBLE : k_INVISIBLE,
		     lval_result);
  lval_result = cons(k_NEW,
		     lval_result);
  lval_result = cons(Tcls_Get_TANGOIMAGECLASS_Symbol_From_TANGOIMAGEOBJ(o_image),
		     lval_result);
  lval_result = cons(s_SEND,
		     lval_result);
    
  Tango_Composite_Image_Get_Values_And_Append(image, lval_result);

  xlpop();
  return (lval_result);
}


/*****************************************************************************
 *
 ****************************************************************************/
Tic_Composite_Init()
{
  LVAL o_TANGO_COMPOSITE_IMAGE_CLASS;

  s_TANGO_IMAGE_TYPE_LINE      = xlenter("TANGO:LINE_IMAGE_CLASS");
  s_TANGO_IMAGE_TYPE_RECTANGLE = xlenter("TANGO:RECTANGLE_IMAGE_CLASS");
  s_TANGO_IMAGE_TYPE_CIRCLE    = xlenter("TANGO:CIRCLE_IMAGE_CLASS");
  s_TANGO_IMAGE_TYPE_POLYLINE  = xlenter("TANGO:POLYLINE_IMAGE_CLASS");
  s_TANGO_IMAGE_TYPE_POLYGON   = xlenter("TANGO:POLYGON_IMAGE_CLASS");
  s_TANGO_IMAGE_TYPE_ELLIPSE   = xlenter("TANGO:ELLIPSE_IMAGE_CLASS");
  s_TANGO_IMAGE_TYPE_SPLINE    = xlenter("TANGO:SPLINE_IMAGE_CLASS");
  s_TANGO_IMAGE_TYPE_TEXT      = xlenter("TANGO:TEXT_IMAGE_CLASS");
  s_TANGO_IMAGE_TYPE_BITMAP    = xlenter("TANGO:BITMAP_IMAGE_CLASS");
  s_TANGO_IMAGE_TYPE_PIXMAP    = xlenter("TANGO:PIXMAP_IMAGE_CLASS");

  /*--------------- create 'Class' instance 'TANGO:COMPOSITE_IMAGE_CLASS' ------------*/
  o_TANGO_COMPOSITE_IMAGE_CLASS =
    Tcls_Create_Subclass_Of_TANGOIMAGE_CLASS("TANGO:COMPOSITE_IMAGE_CLASS",
					     TANGO_IMAGE_TYPE_COMPOSITE);
  xladdmsg(o_TANGO_COMPOSITE_IMAGE_CLASS, ":ISNEW",
	   FTAB_Tango_Composite_Image_Class_Method_ISNEW);
  xladdmsg(o_TANGO_COMPOSITE_IMAGE_CLASS, ":STOREON",
	   FTAB_Tango_Composite_Image_Class_Method_STOREON);

  /* :IMAGE_LOC methods differ among subclasses of TANGO:IMAGE_CLASS; most
   * use Tango_Non_Poly_Image_Class_Method_IMAGE_LOC(), but
   * TANGO:POLYLINE_IMAGE_CLASS, TANGO:POLYGON_IMAGE_CLASS, and
   * TANGO:SPLINE_IMAGE_CLASS use their own methods.
   * Thus can't add it to TANGO:IMAGE_CLASS metaclass... */
  xladdmsg(o_TANGO_COMPOSITE_IMAGE_CLASS, ":IMAGE_LOC",
	   FTAB_Tango_Non_Poly_Image_Class_Method_IMAGE_LOC);

  /* Message :TX_COLOR applies to almost all subclasses of TANGO:IMAGE_CLASS, with
   * the exception of TANGO:BITMAP_IMAGE_CLASS, thus can't add it to
   * TANGO:IMAGE_CLASS metaclass... */
  xladdmsg(o_TANGO_COMPOSITE_IMAGE_CLASS, ":TX_COLOR",
	   FTAB_Tango_Image_Class_Method_TX_COLOR);

  /* Message :TX_FILL applies to almost all subclasses of TANGO:IMAGE_CLASS, with
   * the exception of TANGO:TEXT_IMAGE_CLASS and TANGO:BITMAP_IMAGE_CLASS,
   * thus can't add it to TANGO:IMAGE_CLASS metaclass... */
  xladdmsg(o_TANGO_COMPOSITE_IMAGE_CLASS, ":TX_FILL",
	   FTAB_Tango_Image_Class_Method_TX_FILL);

  /* Message :TX_RESIZE doesn't apply to all subclasses of TANGO:IMAGE_CLASS, 
   * therefore can't add it to TANGO:IMAGE_CLASS metaclass. In particular,
   * :TX_RESIZE message doesn't exist on TANGO:POLYLINE_IMAGE_CLASS,
   * TANGO:POLYGON_IMAGE_CLASS, TANGO:SPLINE_IMAGE_CLASS, 
   * TANGO:BITMAP_IMAGE_CLASS, TANGO:TEXT_IMAGE_CLASS. */
  xladdmsg(o_TANGO_COMPOSITE_IMAGE_CLASS, ":TX_RESIZE",
	   FTAB_Tango_Non_Poly_Image_Class_Method_TX_RESIZE);

  /*
   * The following :RESIZE<i> and :GRAB<i> methods actually only apply to
   * TANGO:POLYLINE_IMAGE_CLASS, TANGO:POLYGON_IMAGE_CLASS,
   * TANGO:SPLINE_IMAGE_CLASS -- we add the methods here because composite-
   * images may have polyline, polygon, or spline subimages...
   */
  xladdmsg(o_TANGO_COMPOSITE_IMAGE_CLASS, ":TX_RESIZE1",
	   FTAB_Tango_Poly_Image_Class_Method_TX_RESIZE1);
  xladdmsg(o_TANGO_COMPOSITE_IMAGE_CLASS, ":TX_RESIZE2",
	   FTAB_Tango_Poly_Image_Class_Method_TX_RESIZE2);
  xladdmsg(o_TANGO_COMPOSITE_IMAGE_CLASS, ":TX_RESIZE3",
	   FTAB_Tango_Poly_Image_Class_Method_TX_RESIZE3);
  xladdmsg(o_TANGO_COMPOSITE_IMAGE_CLASS, ":TX_RESIZE4",
	   FTAB_Tango_Poly_Image_Class_Method_TX_RESIZE4);
  xladdmsg(o_TANGO_COMPOSITE_IMAGE_CLASS, ":TX_RESIZE5",
	   FTAB_Tango_Poly_Image_Class_Method_TX_RESIZE5);
  xladdmsg(o_TANGO_COMPOSITE_IMAGE_CLASS, ":TX_RESIZE6",
	   FTAB_Tango_Poly_Image_Class_Method_TX_RESIZE6);
  xladdmsg(o_TANGO_COMPOSITE_IMAGE_CLASS, ":TX_RESIZE7",
	   FTAB_Tango_Poly_Image_Class_Method_TX_RESIZE7);

  xladdmsg(o_TANGO_COMPOSITE_IMAGE_CLASS, ":TX_GRAB1",
	   FTAB_Tango_Poly_Image_Class_Method_TX_GRAB1);
  xladdmsg(o_TANGO_COMPOSITE_IMAGE_CLASS, ":TX_GRAB2",
	   FTAB_Tango_Poly_Image_Class_Method_TX_GRAB2);
  xladdmsg(o_TANGO_COMPOSITE_IMAGE_CLASS, ":TX_GRAB3",
	   FTAB_Tango_Poly_Image_Class_Method_TX_GRAB3);
  xladdmsg(o_TANGO_COMPOSITE_IMAGE_CLASS, ":TX_GRAB4",
	   FTAB_Tango_Poly_Image_Class_Method_TX_GRAB4);
  xladdmsg(o_TANGO_COMPOSITE_IMAGE_CLASS, ":TX_GRAB5",
	   FTAB_Tango_Poly_Image_Class_Method_TX_GRAB5);
  xladdmsg(o_TANGO_COMPOSITE_IMAGE_CLASS, ":TX_GRAB6",
	   FTAB_Tango_Poly_Image_Class_Method_TX_GRAB6);
  xladdmsg(o_TANGO_COMPOSITE_IMAGE_CLASS, ":TX_GRAB7",
	   FTAB_Tango_Poly_Image_Class_Method_TX_GRAB7);

  /*
   * The :TX_SHUFFLE method only applies to TANGO:BITMAP_IMAGE_CLASS;
   * typically, composite-images need to accept all messages destined
   * for their subimages. In this case, however, we don't at :TX_SHUFFLE
   * since tango composite images cannot include bitmaps as subimages...
  xladdmsg(o_TANGO_BITMAP_IMAGE_CLASS, ":TX_SHUFFLE",
	   FTAB_Tango_Bitmap_Image_Class_Method_TX_SHUFFLE);
   */
}
