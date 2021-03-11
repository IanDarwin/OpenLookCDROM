/* -*-C-*-
*******************************************************************************
*
* File:         xtango.h
* RCS:          $Header: /disk3/npm/src/winterp/src-server/xtango/RCS/xtango.h,v 2.8 1994/06/09 01:24:20 npm Exp $
* Description:  XTANGO ANIMATION PACKAGE (xtango.h)	      
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

#ifndef WINTERP_xtango_h	/* NPM: for WINTERP, need to prevent multiple inclusions... */
#define WINTERP_xtango_h	/* NPM: for WINTERP, need to prevent multiple inclusions... */

#ifdef WINTERP			/* NPM: needed to determin varargs (non-ANSI) vs. stdargs (ANSI) depending on whether <Xm/Xm.h> defines _NO_PROTO */
#include <Xm/Xm.h>		/* NPM: must include to define _NO_PROTO */
#endif /* WINTERP */

/**************************************************************/
/*****************	types & constants    ******************/
/**************************************************************/

typedef enum {
   TANGO_IMAGE_TYPE_LINE,
   TANGO_IMAGE_TYPE_RECTANGLE,
   TANGO_IMAGE_TYPE_CIRCLE,
   TANGO_IMAGE_TYPE_POLYLINE,
   TANGO_IMAGE_TYPE_POLYGON,
   TANGO_IMAGE_TYPE_ELLIPSE,
   TANGO_IMAGE_TYPE_SPLINE,
   TANGO_IMAGE_TYPE_TEXT,
   TANGO_IMAGE_TYPE_BITMAP,
#ifdef WINTERP
   TANGO_IMAGE_TYPE_PIXMAP,
#endif /* WINTERP */
   TANGO_IMAGE_TYPE_COMPOSITE
   } TANGO_IMAGE_TYPE;

typedef enum {
   TANGO_TRANS_TYPE_VISIBLE,
   TANGO_TRANS_TYPE_FILL,
   TANGO_TRANS_TYPE_MOVE,
   TANGO_TRANS_TYPE_RESIZE,
   TANGO_TRANS_TYPE_RESIZE1,
   TANGO_TRANS_TYPE_RESIZE2,
   TANGO_TRANS_TYPE_RESIZE3,
   TANGO_TRANS_TYPE_RESIZE4,
   TANGO_TRANS_TYPE_RESIZE5,
   TANGO_TRANS_TYPE_RESIZE6,
   TANGO_TRANS_TYPE_RESIZE7,
   TANGO_TRANS_TYPE_GRAB1,
   TANGO_TRANS_TYPE_GRAB2,
   TANGO_TRANS_TYPE_GRAB3,
   TANGO_TRANS_TYPE_GRAB4,
   TANGO_TRANS_TYPE_GRAB5,
   TANGO_TRANS_TYPE_GRAB6,
   TANGO_TRANS_TYPE_GRAB7,
   TANGO_TRANS_TYPE_COLOR,
   TANGO_TRANS_TYPE_RAISE,
   TANGO_TRANS_TYPE_LOWER,
   TANGO_TRANS_TYPE_DELAY,
   TANGO_TRANS_TYPE_REFRESH,
   TANGO_TRANS_TYPE_DELETE,
   TANGO_TRANS_TYPE_SHUFFLE,
   TANGO_TRANS_TYPE_ZOOM
   }  TANGO_TRANS_TYPE;

#define  TANGO_COLOR_WHITE			0
#define  TANGO_COLOR_YELLOW			1
#define  TANGO_COLOR_GREEN			2
#define  TANGO_COLOR_BLUE			3
#define  TANGO_COLOR_ORANGE			4
#define  TANGO_COLOR_RED			5
#define  TANGO_COLOR_MAROON			6
#define  TANGO_COLOR_BLACK			7

#define  TANGO_PATH_TYPE_STRAIGHT		0
#define  TANGO_PATH_TYPE_CLOCKWISE		1
#define  TANGO_PATH_TYPE_COUNTERCLOCKWISE	2

#define  TANGO_PART_TYPE_C			0
#define  TANGO_PART_TYPE_NW			1
#define  TANGO_PART_TYPE_N			2
#define  TANGO_PART_TYPE_NE			3
#define  TANGO_PART_TYPE_E			4
#define  TANGO_PART_TYPE_SE			5
#define  TANGO_PART_TYPE_S			6
#define  TANGO_PART_TYPE_SW			7
#define  TANGO_PART_TYPE_W			8

typedef  int		TANGO_COLOR;
typedef  int		TANGO_LINE_STYLE;
typedef  int		TANGO_FILL_STYLE;
typedef  int		TANGO_PATH_TYPE;
typedef  int		TANGO_PATH_MOTION;
typedef  int		TANGO_PART_TYPE;

typedef  struct _LOC	*TANGO_LOC;

typedef  struct _IMAGE	*TANGO_IMAGE;

typedef  struct _PATH	*TANGO_PATH;

typedef  struct _TRANS	*TANGO_TRANS;

typedef struct _IMAGE_COMP {
   TANGO_IMAGE_TYPE type;
   char 	    *args;
   } TANGO_IMAGE_COMPONENT;


typedef void *(*FPTR)();
typedef struct {	/* List of function name/function    */
   char functname[20];	/*    pointers  for TANGOalgoOp()    */
   int  numfunctions;	/* Number of functions in list	     */
   struct {		/* List of functions		     */
      int  functype;	/*    Function return type	     */
      FPTR function;	/*    Function to call		     */
      } funct[5];	/* (max funct name/funct pairs == 5) */
   } NAME_FUNCT;

/**************************************************************/
/*****************	  entry points       ******************/
/**************************************************************/

extern	void		ASSOCinit();
extern	void		ASSOCmake();

#ifndef _NO_PROTO		/* NPM: <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
extern  void		ASSOCstore(char *name, ...);
extern  int             ASSOCmember(char *name, ...);
extern  int             ASSOCdelete(char *name, ...);
extern	int		ASSOCretrieve(char *name, ...);
#else  /* defined(_NO_PROTO) */
extern	void		ASSOCstore();
extern  int             ASSOCmember();
extern  int		ASSOCdelete();
extern	int		ASSOCretrieve();
#endif /* !defined(_NO_PROTO) ==> ANSI */

extern  char *		TANGOalgoOp();

extern	void		TANGOinit();

#ifndef WINTERP
extern  void		TANGOend();
#endif /* !defined(WINTERP) */

extern	int		TANGOinput_coord();
extern	int		TANGOinput_image();

#ifdef WINTERP
extern  int		TANGOget_event_image();
extern  int		TANGOget_event_coord();
#endif /* WINTERP */

extern  int             TANGOset_coord();
extern  void            TANGOinq_coord();
extern  void            TANGOset_bgcolor();

#ifdef WINTERP
extern int Xtango_Check_Invalid_Color(); /* NPM: needed to add this for t_utils.c:Xtango_Cvt_LVAL_To_Valid_TANGO_COLOR_Else_Error() */
extern TANGO_COLOR Xtango_Load_Color_Else_Error(); /* NPM: needed to add this for Xtango_Widget_Class_Method_LOAD_COLOR(), Xtango_Cvt_LVAL_To_Valid_TANGO_COLOR_Else_Error() */
extern TANGO_COLOR Xtango_Pixel_To_TANGO_COLOR(); /* NPM: needed to add this for Xtango_Pixmap_To_Lisp_2D_Array() */
extern char* Xtango_TANGO_COLOR_To_Color_String(); /* NPM: needed for Xtango_Widget_Class_Method_COLORS_STOREON() */
#endif /* WINTERP */

extern  TANGO_COLOR	TANGOload_color();

extern	TANGO_LOC	TANGOloc_create();
extern	double		TANGOloc_X();
extern	double		TANGOloc_Y();
extern	void		TANGOloc_inquire();
extern	TANGO_LOC	TANGOloc_modify();
extern	int		TANGOloc_equal();

#ifndef _NO_PROTO		/* NPM: <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
extern	TANGO_IMAGE	TANGOimage_create(TANGO_IMAGE_TYPE type, ...);
#else  /* defined(_NO_PROTO) */
extern	TANGO_IMAGE	TANGOimage_create();
#endif /* !defined(_NO_PROTO) ==> ANSI */
extern  TANGO_IMAGE     TANGOimage_copy();
extern	TANGO_LOC	TANGOimage_loc();

extern	TANGO_PATH	TANGOpath_create();
extern	TANGO_PATH	TANGOpath_load();
extern	TANGO_PATH	TANGOpath_store();
extern	int		TANGOpath_length();
extern	double		TANGOpath_dx();
extern	double		TANGOpath_dy();
extern	TANGO_PATH	TANGOpath_rotate();
extern	TANGO_PATH	TANGOpath_interpolate();
extern  TANGO_PATH      TANGOpath_reverse();
extern	TANGO_PATH	TANGOpath_scale();
extern	TANGO_PATH	TANGOpath_extend();
extern	TANGO_PATH	TANGOpath_null();
extern	TANGO_PATH	TANGOpath_type();
extern	TANGO_PATH	TANGOpath_color();
extern	TANGO_PATH	TANGOpath_add_head();
extern	TANGO_PATH	TANGOpath_add_tail();
extern	TANGO_PATH	TANGOpath_delete_head();
extern	TANGO_PATH	TANGOpath_delete_tail();
extern	TANGO_PATH	TANGOpath_smooth();
extern	TANGO_PATH	TANGOpath_copy();
extern	TANGO_PATH	TANGOpath_example();
extern	TANGO_PATH	TANGOpath_motion();
extern	TANGO_PATH	TANGOpath_distance();
#ifndef _NO_PROTO		/* NPM: <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
extern	TANGO_PATH	TANGOpath_concatenate(int num, ...);
extern	TANGO_PATH	TANGOpath_compose(int num, ...);
extern	void		TANGOpath_free(int num, ...);
#else  /* defined(_NO_PROTO) */
extern	TANGO_PATH	TANGOpath_concatenate();
extern	TANGO_PATH	TANGOpath_compose();
extern	void		TANGOpath_free();
#endif /* !defined(_NO_PROTO) ==> ANSI */
extern	TANGO_PATH	TANGOpath_iterate();
extern	TANGO_TRANS	TANGOtrans_create();

#ifndef _NO_PROTO		/* NPM: <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
extern	TANGO_TRANS	TANGOtrans_concatenate(int num, ...);
extern	TANGO_TRANS	TANGOtrans_compose(int num, ...);
extern	void		TANGOtrans_free(int num, ...);
#else  /* defined(_NO_PROTO) */
extern	TANGO_TRANS	TANGOtrans_concatenate();
extern	TANGO_TRANS	TANGOtrans_compose();
extern	void		TANGOtrans_free();
#endif /* !defined(_NO_PROTO) ==> ANSI */
extern	TANGO_TRANS	TANGOtrans_iterate();
extern	void		TANGOtrans_perform();

extern	void		TWISTcreate_loc_array();
extern	void		TWISTcreate_2d_loc_array();
extern	void		TWISTcreate_image_array();
extern	void		TWISTcreate_graph();
extern	void		TWISTcreate_bintree();

extern  void            TAPshow();
extern  TANGO_TRANS     TAPfill();
extern  TANGO_TRANS     TAPcolor();
extern  TANGO_TRANS     TAPvis_toggle();
extern  TANGO_TRANS     TAPjump();
extern  TANGO_TRANS     TAPmove();
extern  TANGO_TRANS     TAPtraverse();
extern  TANGO_TRANS     TAPswitch();
extern  TANGO_TRANS     TAPexchange();
extern  TANGO_TRANS     TAPflash();


/**************************************************************/
/*****************	end of xtango.h      ******************/
/**************************************************************/

#endif /* WINTERP_xtango_h *//* NPM: for WINTERP, need to prevent multiple inclusions... */
