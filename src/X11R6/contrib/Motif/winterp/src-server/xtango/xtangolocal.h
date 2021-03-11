/* -*-C-*-
*******************************************************************************
*
* File:         xtangolocal.h
* RCS:          $Header: /disk3/npm/src/winterp/src-server/xtango/RCS/xtangolocal.h,v 2.8 1994/06/09 01:27:49 npm Exp $
* Description:  XTANGO ANIMATION PACKAGE (local.h)
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

/* Modification Date  Description			      */
/* -----------------  --------------------------------------- */
/* 90/10/26 JDH	      Baselined source.                       */
/*							      */
/**************************************************************/

#ifndef WINTERP_xtangolocal_h	/* NPM: for WINTERP, need to prevent multiple inclusions... */
#define WINTERP_xtangolocal_h	/* NPM: for WINTERP, need to prevent multiple inclusions... */

/**************************************************************/
/*****************	 include files       ******************/ 
/**************************************************************/

#include  <stdio.h>
#include  <string.h>
#include  <math.h>

#ifdef WINTERP			/* NPM: use varargs (non-ANSI) vs. stdargs (ANSI) depending on what Xm/Xm.h says about the System/OS/Compiler */

/* NPM: Xm.h includes Xlib.h and Intrinsic.h; Xm.h needed to make _NO_PROTO
        declaration available consistently across all of WINTERP */
#include <Xm/Xm.h>
#ifndef _NO_PROTO		/* NPM: <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
#include  <stdarg.h>
#else /* defined(_NO_PROTO) ==> !ANSI */
#include  <varargs.h>
#endif /* _NO_PROTO */

/* Xtango uses malloc() without checking for failure. Replace malloc with
   XtMalloc s.t. at least the system will puke appropriately when malloc
   fails... This can only be done by including X11/Intrinsic.h, which is
   implicitly included by <Xm/Xm.h> above */
#define   malloc(x)		XtMalloc((unsigned) (x))
#define   free(x)               XtFree((char*) (x))
#define   realloc(x, size)      XtRealloc((char*) (x), (unsigned) (size))

#else  /* !defined(WINTERP) */

#include  <varargs.h>
#include  <X11/Xlib.h>
#include  <X11/Intrinsic.h>

#endif /* WINTERP */

#include  "xtango.h"

/**************************************************************/
/*****************	LOCAL data types     ******************/
/**************************************************************/

#define  STRINGLENGTH	256
#define  MAX_BITMAPS     20
#define  ZERO		0.0

typedef  double      WIN_COORD;

struct _LOC {
   WIN_COORD  x;
   WIN_COORD  y;
   };

typedef struct	_TANGO_LINE {
   int	  color;
   double size[2];
   double width;
   double style;
   int	  arrow;   /* 0-none, 1-forward, 2-backward, 3-both */
   double arrowloc[4][2];
   } *TANGO_LINE_PTR;

typedef struct	_TANGO_RECTANGLE {
   int	  color;
   double size[2];
   double fill;
   } *TANGO_RECTANGLE_PTR;

typedef struct	_TANGO_CIRCLE {
   int	  color;
   double radius;
   double fill;
   } *TANGO_CIRCLE_PTR;

typedef struct	_TANGO_ELLIPSE {
   int	  color;
   double size[2];
   double fill;
   } *TANGO_ELLIPSE_PTR;

typedef struct	_TANGO_POLYLINE {  /* at most 8 vertices */
   int	  vertices;
   double vertex[7][2];
   int	  color;
   double width;
   double style;
   int	  arrow;   /* 0-none, 1-forward, 2-backward, 3-both */
   double arrowloc[4][2];
   } *TANGO_POLYLINE_PTR;

typedef struct	_TANGO_POLYGON {  /* at most 8 vertices */
   int	  sides;
   double vertex[7][2];
   int	  color;
   double fill;
   } *TANGO_POLYGON_PTR;

typedef struct	_TANGO_SPLINE {  /* at most 8 vertices */
   int	  vertices;
   double vertex[7][2];
   int	  color;
   double width;
   double style;
   } *TANGO_SPLINE_PTR;

typedef struct	_TANGO_TEXT {
   int	   color;
   char    font_name[STRINGLENGTH];
   int	   fontid;
   char    text[STRINGLENGTH];
   int	   orient;
   int	   xext,xoff;
   int	   yext,yoff;
   } *TANGO_TEXT_PTR;

typedef struct _TANGO_BMAP {
   Pixmap bmap[MAX_BITMAPS];	/* 20 Bitmaps max	   */
   int	  used;			/* # used out of 20	   */
   int    showing;		/* Currently displayed	   */
   int    width;		/* Bitmap width in pixels  */
   int    height;		/* Bitmap height in pixels */
   } *TANGO_BITMAP_PTR;

#ifdef WINTERP
typedef struct _TANGO_PMAP {
   Pixmap         pixmap;
   int            width;	  /* Pixmap width in pixels */
   int            height;	  /* Pixmap height in pixels */
   int            num_alloc_cols; /* the number of colors allocated by XAllocColor() for displaying the Pixmap    */
   unsigned long* alloc_cols;	  /* the array of pixel values alloc'd by XAllocColor() for displaying the Pixmap */
   } *TANGO_PIXMAP_PTR;
#endif /* WINTERP */

typedef struct	_TANGO_COMPOSITE {
   TANGO_IMAGE image_list;
   } *TANGO_COMPOSITE_PTR;

typedef struct IMAGE {
   TANGO_IMAGE	 image;
   struct IMAGE  *nexti;
   struct IMAGE  *previ;
   } *IMAGE_PTR;

struct _IMAGE {
   TANGO_IMAGE_TYPE type;
   double	    loc[2];
   int		    visible;
   int              alive;      /* used by TANGOimage_delete */
#ifdef WINTERP			
   /* NPM: this is a backpointer to the LVAL(XLTYPE_TANGOIMAGEOBJ), WINTERP's
      wrapper for TANGO_IMAGEs. Note that the type is actually an LVAL,
      but I don't want to have to include xlisp.h here. */
   XtPointer	    tangoimageobj;
#endif /* WINTERP */
   void		    *object;    /* LINE, CIRCLE, RECTANGLE, etc */
   IMAGE_PTR	    inconfig;	/* used to mark place in configuration */
   struct _IMAGE    *nexti;
   };

typedef struct OFFSET {
   WIN_COORD	  dx;
   WIN_COORD	  dy;
   int		  absx;
   int		  absy;
   struct OFFSET  *prevo;
   struct OFFSET  *nexto;
   } *OFFSET_PTR;

struct _PATH {
   OFFSET_PTR	head;
   OFFSET_PTR	tail;
   int		count;
   int		startx;   /* used for displaying a path */
   int		starty;
   struct _PATH *nextp;
   struct _PATH *prevp;
   };

typedef struct FRAME {
   TANGO_TRANS_TYPE  type;   /* maybe bag this, check if needed */
   int		     frame_num;
   WIN_COORD	     dx;
   WIN_COORD	     dy;
   struct FRAME      *prevf;
   struct FRAME      *nextf;
   } *FRAME_PTR;

typedef struct ACTION {
   TANGO_TRANS_TYPE   type;
   TANGO_IMAGE	      image;
   FRAME_PTR	      fhead;
   FRAME_PTR	      ftail;
   FRAME_PTR	      doing;
   struct ACTION      *nexta;
   } *ACTION_PTR;

struct _TRANS {
   int		    num_frames;
   ACTION_PTR	    actions;
   struct _TRANS    *next;
   };

typedef struct ANIMATION {
   Display     *display;          /* X display info                          */
   int          color_screen;     /* 1 if color, 0 if b/w                    */
   Widget       easel;            /* drawing area widget                     */
   int          debug;            /* debugging level                         */
   Window	display_window;   /* onscreen blt-target window 	     */
   Pixmap	pixmap; 	  /* offscreen pixmap for drawing into	     */
   TANGO_COLOR  bgcolor;          /* window background                       */ 
   int		width;		  /* window's width			     */
   int		height;		  /*          height			     */
   WIN_COORD	lx,by,rx,ty;	  /* working vals			     */
   WIN_COORD	damlx,damby,damrx,damty;  /* for updating		     */
   WIN_COORD	x_WIN_COORD_to_int; /* set by TANGO_refresh(),               */
                                    /* TANGO_anim_setup()                    */
   WIN_COORD	y_WIN_COORD_to_int; /* set by TANGO_refresh(),               */
                                    /* TANGO_anim_setup()                    */
   int		damage_x, damage_y;
   unsigned int damage_width, damage_height;
#ifdef WINTERP
   int		x_window_offset, y_window_offset; /* NPM: for xmDrawnButtonWidgetClass, need to offset drawing so as not to overwrite border... see xtangoanim.c:TANGO_anim_setup() and TANGO_anim_next_damage_frame() */
#else /* !defined(WINTERP): "getMouse", "mousex", "mousey" not used w/ WINTERP */
   int          getMouse;	  /* Waiting for mouse input (T/F)	     */
   int	        mousex,mousey;    /*    Coords of mouse input		     */
#endif /* WINTERP) */
   int          mono_fillstyle;    /* Patterns represent color(0)/FillStyle(1)*/
#ifdef WINTERP
   EventMask	anim_event_mask;  /* see TANGO_check_X_events() */
#else /* !defined(WINTERP) */
   int          paused;           /* indicates if currently paused	     */
#endif /* WINTERP */
   int		delay;		  /* how many usecs to sleep		     */
   int		motionblur;	  /* disable clear for printing window dumps */
   IMAGE_PTR	confighead;	  /* current configuration of TANGO images   */
   IMAGE_PTR	configtail;
   } *ANIMATION_PTR;

/**************************************************************/
/*****************	GLOBAL variables     ******************/
/**************************************************************/

extern ANIMATION_PTR		 TANGO__data;

/**************************************************************/
/*****************	LOCAL variables      ******************/
/**************************************************************/

#ifdef WINTERP			/* NPM: send xtango error messages to *error-output* */
extern void
Xtango_COMPLAIN			/* NPM: from ../t_utils.c */
(
#ifndef _NO_PROTO		/* NPM: <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 char *fmt,
 ...
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );
#define   COMPLAIN			Xtango_COMPLAIN
#else /* !defined(WINTERP) */
#define   COMPLAIN			printf
#endif /* WINTERP */

#define   XOR				10
#define   OFF				0	/* used for fill styles */
#define   ON				1

#ifdef WINTERP			/* NPM: (1) handle cases (see t_path.c/xtangopath.c) where TANGO__data hasn't been created;
				        (2) Send output to *trace-output* */
extern void
Xtango_DEBUG_TRACE_PRINT	/* NPM: from ../t_utils.c */
(
#ifndef _NO_PROTO		/* NPM: <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 char *fmt,
 ...
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );
#define   DEBUG if (TANGO__data && TANGO__data->debug) Xtango_DEBUG_TRACE_PRINT
#else  /* !defined(WINTERP) */
#define   DEBUG if (TANGO__data->debug)	printf
#endif /* WINTERP */

#define   TANGO_DEBUG_EXTERNAL		1
#define   TANGO_DEBUG_INTERNAL		2

#define   TANGO_MODE_SLOW		0
#define   TANGO_MODE_DEFAULT		1
#define   TANGO_MODE_FAST		2

#define   TANGO_PATH_POINTS		20
#define   TANGO_PATH_NUMTYPES		3

#define   TANGO_STYLE_SOLID		0
#define   TANGO_STYLE_DASHED		1
#define   TANGO_STYLE_DOTTED		2
#define   TANGO_STYLE_THICK		3
#define   TANGO_STYLE_THICK_DASHED	4
#define   TANGO_STYLE_THICK_DOTTED	5
#define   TANGO_STYLE_THICKER		6
#define   TANGO_STYLE_THICKER_DASHED	7
#define   TANGO_STYLE_THICKER_DOTTED	8

#define   TANGO_BASE_FILL		0
#define   TANGO_NUM_FILL		41
#define   TANGO_FILL_OUTLINE		TANGO_BASE_FILL
#define   TANGO_FILL_SOLID		TANGO_BASE_FILL + TANGO_NUM_FILL - 1

#define   BIG_NUMBER			1000000

#define   PICK_EPS			4
#define   CLOSE(A,B)			((A) <= (B) + PICK_EPS)
#define   SAME(A,B)			(CLOSE(A,B) && CLOSE(B,A))
#define   SAMEPT(P,Q)			(SAME(P[0],Q[0]) && SAME(P[1],Q[1]))

#define   EQUAL(A,B)			(((A)-0.002 < (B))&&((B) < (A)+0.002))

#ifndef   ABS
#define   ABS(A)			( (A) < 0.0 ? (-A) : (A) )
#endif

#ifndef   MIN
#define   MIN(A,B)			( (A) < (B) ? (A) : (B) )
#endif

#define   ROUND(X)			floor((X) + 0.5)

/* when window coords are decimals, this finds the corresponding pixel */
#define   XPIXEL(A,B)  ( (int) ROUND( ((B)-TANGO__data->lx) * (A) ) )
/* here, B is the real-valued position, and A is width/(rx-lx)	*/
#define   YPIXEL(A,B)  ( (int) ROUND( ((B)-TANGO__data->ty) * (A) ) )
/* here, B is the real-valued position, and A is height/(by-ty)  */

#ifndef WINTERP			/* NPM: moved this to xtangopath.c since it's only used there
				   (no need to replicate "static" decl in each file including xtangolocal.h */
static double TANGO__path_type[TANGO_PATH_NUMTYPES][TANGO_PATH_POINTS][2] =
   { { {   0.01,0.0    }, {   0.01,0.0    }, {   0.01,0.0    },
       {   0.01,0.0    }, {   0.01,0.0    }, {   0.01,0.0    },
       {   0.01,0.0    }, {   0.01,0.0    }, {   0.01,0.0    },
       {   0.01,0.0    }, {   0.01,0.0    }, {   0.01,0.0    },
       {   0.01,0.0    }, {   0.01,0.0    }, {   0.01,0.0    },
       {   0.01,0.0    }, {   0.01,0.0    }, {   0.01,0.0    },
       {   0.01,0.0    }, {   0.01,0.0    } },
     { {  0.002,-0.016 }, {  0.002,-0.016 }, {  0.006,-0.014 },
       {  0.010,-0.012 }, {  0.010,-0.012 }, {  0.012,-0.010 },
       {  0.012,-0.010 }, {  0.014,-0.006 }, {  0.016,-0.002 },
       {  0.016,-0.002 }, {  0.016,0.002  }, {  0.016,0.002  },
       {  0.014,0.006  }, {  0.012,0.010  }, {  0.012,0.010  },
       {  0.010,0.012  }, {  0.010,0.012  }, {  0.006,0.014  },
       {  0.002,0.016  }, {  0.002,0.016  } },
     { { -0.002,-0.016 }, { -0.002,-0.016 }, { -0.006,-0.014 },
       { -0.010,-0.012 }, { -0.010,-0.012 }, { -0.012,-0.010 },
       { -0.012,-0.010 }, { -0.014,-0.006 }, { -0.016,-0.002 },
       { -0.016,-0.002 }, { -0.016,0.002  }, { -0.016,0.002  },
       { -0.014,0.006  }, { -0.012,0.010  }, { -0.012,0.010  },
       { -0.010,0.012  }, { -0.010,0.012  }, { -0.006,0.014  },
       { -0.002,0.016  }, { -0.002,0.016  } } };
#endif /* !defined(WINTERP) */

/**************************************************************/
/*****************      LOCAL functions      ******************/
/**************************************************************/

void     TANGO_anim_setup();		/* in xtangoanim.c */
void     TANGO_anim_next_damage_frame();

void	 ASSOC_clear();			/* in xtangoassoc.c  */

void	 TANGO_Line();			/* in xtangodraw.c */
void	 TANGO_Rectangle();
void	 TANGO_Fill_Rectangle();
void	 TANGO_Ellipse();
void	 TANGO_Fill_Ellipse();
void	 TANGO_Polyline();
void	 TANGO_Polygon();
void	 TANGO_Spline();
void	 TANGO_Text();

void	 TANGO_initGC();		/* in xtangogc.c */
int	 TANGO_color();
int      TANGO_check_color();
int	 TANGO_line_style();
int	 TANGO_fill_style();
int	 TANGO_load_font();
int	 TANGO_font();
void	 TANGO_text_info();
GC	 TANGO_gc();
int	 TANGO_inq_font();
int	 TANGO_inq_color();

void	 TANGO_layout_window();		/* in xtangohp.c */
void	 TANGO_check_X_events();

void	 TANGO_image_init();		/* in xtangoimage.c  */
void	 TANGO_image_delete();
void	 TANGO_image_trans();
void	 TANGO_image_draw();
void     TANGO_image_damage();
int      TANGO_image_intercepts();
void	 TANGO_bounding_box();
void	 TANGO_clear_screen();
void	 TANGO_damage_clear_screen();

void	 TANGO_Spline();		/* in xtangospline.c */

void	 TANGO_refresh();		/* in xtangotrans.c  */
void     TANGO_zoom();

void	 TANGO_setup_windows();		/* in xtangowindow.c */
void	 TANGO_flush();
#ifndef WINTERP			/* NPM: Not used in xtango 1.52 */
void	 TANGO_batch_mode();
#endif /* !defined(WINTERP) */

#ifndef WINTERP			/* NPM: removed these because names are not distinct-enough */
void	 pan_callback();		/* in xtangoxt.c */
void	 zoom_callback();
void	 quit_callback();
void	 refresh_callback();
void	 resize_callback();
void	 select_point();
#endif /* !defined(WINTERP) */

/**************************************************************/
/*****************   end of xtangolocal.h    ******************/
/**************************************************************/

#endif /* WINTERP_xtangolocal_h *//* NPM: for WINTERP, need to prevent multiple inclusions... */
