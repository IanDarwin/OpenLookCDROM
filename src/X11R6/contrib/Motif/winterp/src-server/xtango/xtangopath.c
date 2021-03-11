/* -*-C-*-
*******************************************************************************
*
* File:         xtangopath.c
* RCS:          $Header: /disk3/npm/src/winterp/src-server/xtango/RCS/xtangopath.c,v 2.7 1994/06/09 01:28:37 npm Exp $
* Description:  XTANGO ANIMATION PACKAGE (path)
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
static char rcs_identity[] = "@(#)$Header: /disk3/npm/src/winterp/src-server/xtango/RCS/xtangopath.c,v 2.7 1994/06/09 01:28:37 npm Exp $";

/* Modification Date  Description			      */
/* -----------------  --------------------------------------- */
/* 90/10/26 JDH	      Baselined source.                       */
/*							      */
/**************************************************************/

/**************************************************************/
/*****************	 include files       ******************/ 
/**************************************************************/

#include "xtangolocal.h"

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

TANGO_PATH save_path();
void	   path_free();

/**************************************************************/
/*****************	  entry points       ******************/
/**************************************************************/

TANGO_PATH TANGOpath_load();
TANGO_PATH TANGOpath_store();
TANGO_PATH TANGOpath_create();
int	   TANGOpath_length();
double	   TANGOpath_dx();
double	   TANGOpath_dy();
TANGO_PATH TANGOpath_null();
TANGO_PATH TANGOpath_type();
TANGO_PATH TANGOpath_color();
TANGO_PATH TANGOpath_add_head();
TANGO_PATH TANGOpath_add_tail();
TANGO_PATH TANGOpath_delete_head();
TANGO_PATH TANGOpath_delete_tail();
TANGO_PATH TANGOpath_copy();
TANGO_PATH TANGOpath_reverse();
TANGO_PATH TANGOpath_smooth();
TANGO_PATH TANGOpath_rotate();
TANGO_PATH TANGOpath_scale();
TANGO_PATH TANGOpath_extend();
TANGO_PATH TANGOpath_interpolate();
TANGO_PATH TANGOpath_example();
TANGO_PATH TANGOpath_motion();
TANGO_PATH TANGOpath_distance();
TANGO_PATH TANGOpath_iterate();

#ifndef _NO_PROTO		/* NPM: <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
TANGO_PATH TANGOpath_concatenate(int num, ...);
TANGO_PATH TANGOpath_compose(int num, ...);
void TANGOpath_free(int num, ...);
#else  /* defined(_NO_PROTO) */
TANGO_PATH TANGOpath_concatenate();
TANGO_PATH TANGOpath_compose();
void TANGOpath_free();
#endif /* !defined(_NO_PROTO) ==> ANSI */


/**************************************************************/
/* TANGOpath_load -- Load a path from the given file name.    */
/* 							      */
/* RETURNS:  New path.					      */
/**************************************************************/
TANGO_PATH
TANGOpath_load(file_name)
   char *file_name;
{
   FILE *fp;
   int count;
   WIN_COORD dx,dy;
   OFFSET_PTR off,head,tail;

   if (!TANGO__data) TANGOinit();

   DEBUG("TANGOpath_load(\"%s\")\n", file_name);

   if ((fp = fopen(file_name,"r")) == NULL)
      { fprintf(stderr,"Unable to open file %s for path read\n",file_name);
	return(NULL);
      }

   head = tail = NULL;
   count = 0;

   while (fscanf(fp,"%lf %lf\n",&dx,&dy) != EOF)
      { off = (OFFSET_PTR) malloc( sizeof (struct OFFSET));
	off->dx = dx;
	off->dy = dy;
	if (!head)
	   head = off;
	else
	   tail->nexto = off;
	off->nexto = NULL;
	off->prevo = tail;
	tail = off;
	++count;
      }
   fclose(fp);
   return( save_path(head,tail,count) );
}



/**************************************************************/
/* TANGOpath_store -- Allow the user to store a path in some  */
/*		      file.  Returns NULL if the save failed. */
/* 							      */
/* RETURNS:  Given path or NULL if unable to save.	      */
/**************************************************************/
TANGO_PATH
TANGOpath_store(path,file_name)
   TANGO_PATH path;
   char *file_name;
{
  FILE *fp;
  OFFSET_PTR off;

   if (!TANGO__data) TANGOinit();

   DEBUG("TANGOpath_store(0x%lx, \"%s\")\n", (unsigned long) path, file_name);

   if ((fp = fopen(file_name,"w")) == NULL)
      { fprintf(stderr,"Unable to open file %s for path write\n",file_name);
	return(NULL);
      }

   for (off=path->head; off; off=off->nexto)
      fprintf(fp,"%f %f\n",off->dx,off->dy);
   fclose(fp);

   return(path);
}



/**************************************************************/
/* TANGOpath_create -- Create a path given a sequence of      */
/*		       offsets.				      */
/* 							      */
/* RETURNS:  Specified path.				      */
/**************************************************************/
TANGO_PATH
TANGOpath_create(num,x,y)
   int num;
   WIN_COORD x[],y[];
{
   OFFSET_PTR offset,head=NULL,tail=NULL;
   int i;

   if (!TANGO__data) TANGOinit();

   DEBUG("TANGOpath_create(%d, %f..., %f...)\n", num, *x, *y);

   for (i=0; i<num; ++i)
      { offset = (OFFSET_PTR) malloc( sizeof( struct OFFSET));
	offset->dx = x[i];
	offset->dy = y[i];
	offset->prevo = tail;
	offset->nexto = NULL;
	if (head)
	   tail->nexto = offset;
	else
	   head = offset;
	tail = offset;
      }
   return( save_path(head,tail,num) );
}



/**************************************************************/
/* TANGOpath_length -- Return the number of offsets in the    */
/*		       given path.			      */
/* 							      */
/* RETURNS:  Number of offsets.				      */
/**************************************************************/
int
TANGOpath_length(path)
   TANGO_PATH path;
{
   if (!TANGO__data) TANGOinit();

   DEBUG("TANGOpath_lenth(0x%lx)\n", (unsigned long) path);

   return( path->count );
}



/**************************************************************/
/* TANGOpath_dx -- Return the distance the path covers in     */
/*		   x animation coordiante system units.	      */
/* 							      */
/* RETURNS:  Distance in x animation coord system units.      */
/**************************************************************/
double
TANGOpath_dx(path)
   TANGO_PATH path;
{
   double dx=0.0;
   OFFSET_PTR off;

   DEBUG("TANGOpath_dx(0x%lx)\n", (unsigned long) path);

   for (off=path->head; off; off=off->nexto)
      dx += off->dx;

   return(dx);
}



/**************************************************************/
/* TANGOpath_dy -- Return the distance the path covers in     */
/*		   y animation coordiante system units.	      */
/* 							      */
/* RETURNS:  Distance in y animation coord system units.      */
/**************************************************************/
double
TANGOpath_dy(path)
   TANGO_PATH path;
{
   double dy=0.0;
   OFFSET_PTR off;

   DEBUG("TANGOpath_dy(0x%lx)\n", (unsigned long) path);

   for (off=path->head; off; off=off->nexto)
      dy += off->dy;

   return(dy);
}



/**************************************************************/
/* TANGOpath_null -- Return a null path (all <0,0> offsets)   */
/*		     which is the given number of offsets     */
/*		     long.				      */
/* 							      */
/* RETURNS:  Specified path.				      */
/**************************************************************/
TANGO_PATH
TANGOpath_null(count)
   int count;
{
   OFFSET_PTR offset,head=NULL,tail=NULL;
   int i;

   if (!TANGO__data) TANGOinit();

   DEBUG("TANGOpath_null(%d)\n", count);

   for (i=1; i<=count; ++i)
      { offset = (OFFSET_PTR) malloc( sizeof( struct OFFSET));
	offset->dx = offset->dy = ZERO;
	offset->prevo = tail;
	offset->nexto = NULL;
	if (head)
	   tail->nexto = offset;
	else
	   head = offset;
	tail = offset;
      }
   return( save_path(head,tail,count) );
}

#ifdef WINTERP			/* NPM: moved this def from xtangolocal.h since it's only used here. */
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
#endif /* WINTERP */

/**************************************************************/
/* TANGOpath_type -- create a path which corresponds to one   */
/*		     of the basic path types.		      */
/* 							      */
/* RETURNS:  Specified path.				      */
/**************************************************************/
TANGO_PATH
TANGOpath_type(path_type)
   TANGO_PATH_TYPE path_type;
{
   OFFSET_PTR offset,head=NULL,tail=NULL;
   int i;

   if (!TANGO__data) TANGOinit();

   DEBUG("TANGOpath_type(%d)\n", path_type);

   for (i=0; i<=TANGO_PATH_POINTS-1; ++i)
      { offset = (OFFSET_PTR) malloc( sizeof( struct OFFSET));
	offset->dx = TANGO__path_type[path_type][i][0];
	offset->dy = TANGO__path_type[path_type][i][1];
	offset->prevo = tail;
	offset->nexto = NULL;
	if (head)
	   tail->nexto = offset;
	else
	   head = offset;
	tail = offset;
      }
   return( save_path(head,tail,TANGO_PATH_POINTS) );
}



/**************************************************************/
/* TANGOpath_color -- Create a path that corresponds to       */
/*		      changing an image to the given color.   */
/* 							      */
/* RETURNS:  Specified path.				      */
/**************************************************************/
TANGO_PATH
TANGOpath_color(color)
   int color;
{
   OFFSET_PTR offset;

   if (!TANGO__data) TANGOinit();

   DEBUG("TANGOpath_color(%d)\n", color);

   /* Check for non default colors - not supported for path */
   if (color < TANGO_COLOR_WHITE || color > TANGO_COLOR_BLACK) {
#ifdef WINTERP
     extern char temptext[];	/* from ../winterp.c */
     extern void Xtango_Restore_Context(); /* from ../t_utils.c */
     Xtango_Restore_Context();
     sprintf(temptext, "User defined path color (%d) not suppoted.",
	     (int) color);
     xlfail(temptext);
#else /* !defined(WINTERP) */
      COMPLAIN("WARNING: TANGOpath_color: User-defined path color %s (%d)\n",
	       "not supported", color);
      color = TANGO_COLOR_BLACK;
#endif /* WINTERP */
      }

   offset = (OFFSET_PTR) malloc( sizeof( struct OFFSET));
   offset->prevo = NULL;
   offset->nexto = NULL;

   switch (color)
      { case TANGO_COLOR_WHITE :
	   offset->dx = 0.05;
	   offset->dy = -0.01;
	   break;
	case TANGO_COLOR_BLACK :
	   offset->dx = 0.01;
	   offset->dy = -0.05;
	   break;
	case TANGO_COLOR_RED :
	   offset->dx = -0.01;
	   offset->dy = -0.05;
	   break;
	case TANGO_COLOR_ORANGE :
	   offset->dx = -0.05;
	   offset->dy = -0.01;
	   break;
	case TANGO_COLOR_YELLOW :
	   offset->dx = -0.05;
	   offset->dy = 0.01;
	   break;
	case TANGO_COLOR_GREEN :
	   offset->dx = -0.01;
	   offset->dy = 0.05;
	   break;
	case TANGO_COLOR_BLUE :
	   offset->dx = 0.01;
	   offset->dy = 0.05;
	   break;
	case TANGO_COLOR_MAROON :
	   offset->dx = 0.05;
	   offset->dy = 0.01;
	   break;
	default :
	   offset->dx = offset->dy = 0.0;
      }

   return( save_path(offset,offset,1) );
}



/**************************************************************/
/* TANGOpath_add_head -- Return a path which has the given    */
/*		         number of null offsets added to the  */
/*			 head of the given path.	      */
/* 							      */
/* RETURNS:  Specified path.				      */
/**************************************************************/
TANGO_PATH
TANGOpath_add_head(path,num)
   TANGO_PATH path;
   int num;
{
   OFFSET_PTR offset,walk,head=NULL,tail=NULL;
   int i;

   if (!TANGO__data) TANGOinit();

   DEBUG("TANGOpath_add_head(0x%lx, %d)\n", (unsigned long) path, num);

   if (num < 0) num=0;

   for (i=1; i<=num; ++i)
      { offset = (OFFSET_PTR) malloc( sizeof( struct OFFSET));
	offset->dx = offset->dy = ZERO;
	offset->prevo = tail;
	offset->nexto = NULL;
	if (head)
	   tail->nexto = offset;
	else
	   head = offset;
	tail = offset;
      }

   for (walk=path->head; walk; walk=walk->nexto)
      { offset = (OFFSET_PTR) malloc( sizeof( struct OFFSET));
	offset->dx = walk->dx;
	offset->dy = walk->dy;
	offset->prevo = tail;
	offset->nexto = NULL;
	if (head)
	   tail->nexto = offset;
	else
	   head = offset;
	tail = offset;
      }

   return( save_path(head,tail,path->count+num) );
}



/**************************************************************/
/* TANGOpath_add_tail -- Return a path which has the given    */
/*			 number of null offsets added to the  */
/*			 tail of the given path.	      */
/* 							      */
/* RETURNS:  Specified path.				      */
/**************************************************************/
TANGO_PATH
TANGOpath_add_tail(path,num)
   TANGO_PATH path;
   int num;
{
   OFFSET_PTR offset,walk,head=NULL,tail=NULL;
   int i;

   if (!TANGO__data) TANGOinit();

   DEBUG("TANGOpath_add_tail(0x%lx, %d)\n", (unsigned long) path, num);

   if (num < 0) num=0;

   for (walk=path->head; walk; walk=walk->nexto)
      { offset = (OFFSET_PTR) malloc( sizeof( struct OFFSET));
	offset->dx = walk->dx;
	offset->dy = walk->dy;
	offset->prevo = tail;
	offset->nexto = NULL;
	if (head)
	   tail->nexto = offset;
	else
	   head = offset;
	tail = offset;
      }

   for (i=1; i<=num; ++i)
      { offset = (OFFSET_PTR) malloc( sizeof( struct OFFSET));
	offset->dx = offset->dy = ZERO;
	offset->prevo = tail;
	offset->nexto = NULL;
	if (head)
	   tail->nexto = offset;
	else
	   head = offset;
	tail = offset;
      }

   return( save_path(head,tail,path->count+num) );
}



/**************************************************************/
/* TANGOpath_delete_head -- Return a path which corresponds   */
/*			    to the given path with the given  */
/*			    number of offsets removed from    */
/*			    its head.			      */
/* 							      */
/* RETURNS:  Specified path.				      */
/**************************************************************/
TANGO_PATH
TANGOpath_delete_head(path,num)
   TANGO_PATH path;
   int num;
{
   OFFSET_PTR offset,walk,head=NULL,tail=NULL;
   int i;

   if (!TANGO__data) TANGOinit();

   DEBUG("TANGOpath_delete_head(0x%lx, %d)\n", (unsigned long) path, num);

   if (num < 0) num=0;

   for (i=1,walk=path->head; i<=num; ++i)
      { if (!walk) break;
	walk = walk->nexto;
      }

   for ( ; walk; walk=walk->nexto)
      { offset = (OFFSET_PTR) malloc( sizeof( struct OFFSET));
	offset->dx = walk->dx;
	offset->dy = walk->dy;
	offset->prevo = tail;
	offset->nexto = NULL;
	if (head)
	   tail->nexto = offset;
	else
	   head = offset;
	tail = offset;
      }

   return( save_path(head,tail,((path->count-num < 0) ? 0 : path->count-num)) );
}



/**************************************************************/
/* TANGOpath_delete_tail -- Return a path which corresponds   */
/*			    to the given path with the given  */
/*			    number of offsets removed from    */
/*			    its tail. 			      */
/* 							      */
/* RETURNS:  Specified path.				      */
/**************************************************************/
TANGO_PATH
TANGOpath_delete_tail(path,num)
   TANGO_PATH path;
   int num;
{
   OFFSET_PTR offset,walk,head=NULL,tail=NULL;
   int i;

   if (!TANGO__data) TANGOinit();

   DEBUG("TANGOpath_delete_tail(0x%lx, %d)\n", (unsigned long) path, num);

   if (num < 0) num=0;

   for (i=1,walk=path->head; (i<=path->count-num)&&walk; walk=walk->nexto)
      { offset = (OFFSET_PTR) malloc( sizeof( struct OFFSET));
	offset->dx = walk->dx;
	offset->dy = walk->dy;
	offset->prevo = tail;
	offset->nexto = NULL;
	if (head)
	   tail->nexto = offset;
	else
	   head = offset;
	tail = offset;
      }

   return( save_path(head,tail,((path->count-num < 0) ? 0 : path->count-num)) );
}



/**************************************************************/
/* TANGOpath_copy -- Return a new path which is a duplicate   */
/*		     of the given path.			      */
/* 							      */
/* RETURNS:  Duplicate path.				      */
/**************************************************************/
TANGO_PATH
TANGOpath_copy(path)
   TANGO_PATH path;
{
   OFFSET_PTR ptr,offset,head=NULL,tail=NULL;

   if (!TANGO__data) TANGOinit();

   DEBUG("TANGOpath_copy(0x%lx)\n", (unsigned long) path);

   for (ptr=path->head; ptr; ptr=ptr->nexto)
      { offset = (OFFSET_PTR) malloc( sizeof( struct OFFSET));
	offset->dx = ptr->dx;
	offset->dy = ptr->dy;
	offset->prevo = tail;
	offset->nexto = NULL;
	if (head)
	   tail->nexto = offset;
	else
	   head = offset;
	tail = offset;
      }
   return( save_path(head,tail,path->count) );
}



/**************************************************************/
/* TANGOpath_reverse -- Return a new path which is a reversal */
/*			of the given path.  This entails      */
/*			reversing the order of offsets and    */
/*			making each's direction be opposite.  */
/* 							      */
/* RETURNS:  Specified path.				      */
/**************************************************************/
TANGO_PATH
TANGOpath_reverse(path)
   TANGO_PATH path;
{
   OFFSET_PTR ptr,offset,head=NULL,tail=NULL;

   if (!TANGO__data) TANGOinit();

   DEBUG("TANGOpath_reverse(0x%lx)\n", (unsigned long) path);

   for (ptr=path->tail; ptr; ptr=ptr->prevo)
      { offset = (OFFSET_PTR) malloc( sizeof( struct OFFSET));
	offset->dx = -(ptr->dx);
	offset->dy = -(ptr->dy);
	offset->prevo = tail;
	offset->nexto = NULL;
	if (head)
	   tail->nexto = offset;
	else
	   head = offset;
	tail = offset;
      }
   return( save_path(head,tail,path->count) );
}



/**************************************************************/
/* TANGOpath_smooth -- Return a path that is a "smoother"     */
/*		       version of the given path (avg. in     */
/*		       nearby offsets)			      */
/* 							      */
/* RETURNS:  Specified path.				      */
/**************************************************************/
TANGO_PATH
TANGOpath_smooth(path)
   TANGO_PATH path;
{
   OFFSET_PTR ptr,offset,head,tail;
   WIN_COORD  oldx,oldy,newx,newy,xshould,yshould;

   if (!TANGO__data) TANGOinit();

   DEBUG("TANGOpath_smooth(0x%lx)\n", (unsigned long) path);

   if (TANGOpath_length(path) == 1) return( TANGOpath_copy(path) );

   ptr = path->head;
   oldx = ptr->dx;
   oldy = ptr->dy;
   newx = (3.0 * ptr->dx + ptr->nexto->dx) / 4.0;
   newy = (3.0 * ptr->dy + ptr->nexto->dy) / 4.0;

   offset = (OFFSET_PTR) malloc( sizeof( struct OFFSET));
   offset->dx = newx;
   offset->dy = newy;
   head = tail = offset;
   offset->prevo = offset->nexto = NULL;

   for (ptr=ptr->nexto; ptr->nexto; ptr=ptr->nexto)
      { oldx += ptr->dx;
	oldy += ptr->dy;
	xshould = (4.0 * oldx - ptr->prevo->dx + ptr->nexto->dx) / 4.0;
	yshould = (4.0 * oldy - ptr->prevo->dy + ptr->nexto->dy) / 4.0;

	offset = (OFFSET_PTR) malloc( sizeof( struct OFFSET));
	offset->dx = ptr->dx + xshould - oldx;
	offset->dy = ptr->dy + yshould - oldy;
	offset->prevo = tail;
	offset->nexto = NULL;
	tail->nexto = offset;
	tail = offset;

	newx += offset->dx;
	newy += offset->dy;
      }
   offset = (OFFSET_PTR) malloc( sizeof( struct OFFSET));
   offset->dx = TANGOpath_dx(path) - newx;
   offset->dy = TANGOpath_dy(path) - newy;
   offset->prevo = tail;
   offset->nexto = NULL;
   tail->nexto = offset;
   tail = offset;

   return( save_path(head,tail,path->count) );
}



/**************************************************************/
/* TANGOpath_rotate -- Create a path which is the rotation    */
/*		       of the given path by the given number  */
/*		       of degrees (only positive degrees and  */
/*		       in a counterclockwise rotation)        */
/* 							      */
/* RETURNS:  Specified path.				      */
/**************************************************************/
TANGO_PATH
TANGOpath_rotate(path,deg)
   TANGO_PATH path;
   int deg;
{
   double rads, cosine, sine;
   OFFSET_PTR ptr, offset, head=NULL, tail=NULL;

   if (!TANGO__data) TANGOinit();

   DEBUG("TANGOpath_rotate(0x%lx, %d)\n", (unsigned long) path, deg);

   rads = 0.017453 * deg;
   cosine = cos(rads);
   sine = sin(rads);

   for (ptr=path->head; ptr; ptr=ptr->nexto)
      { offset = (OFFSET_PTR) malloc( sizeof( struct OFFSET));
	offset->dx = (ptr->dx * cosine) + (ptr->dy * sine);
	offset->dy = -(ptr->dx * sine) + (ptr->dy * cosine);
	offset->prevo = tail;
	offset->nexto = NULL;  /* sine signs reversed because our system has  */
	if (head)	       /* negatives @ top & positives @ window bottom */
	   tail->nexto = offset;
	else
	   head = offset;
	tail = offset;
      }
   return( save_path(head,tail,path->count) );
}



/**************************************************************/
/* TANGOpath_scale -- Scale the path by the given factors in  */
/*		      x and y.  (Scale each offset.)	      */
/* 							      */
/* RETURNS:  Scaled path.				      */
/**************************************************************/
TANGO_PATH
TANGOpath_scale(path,xscale,yscale)
   TANGO_PATH path;
   double xscale,yscale;
{
   OFFSET_PTR ptr, offset, head=NULL, tail=NULL;

   if (!TANGO__data) TANGOinit();

   DEBUG("TANGOpath_scale(0x%lx, %f, %f)\n", (unsigned long) path, xscale, yscale);

   for (ptr=path->head; ptr; ptr=ptr->nexto)
      { offset = (OFFSET_PTR) malloc( sizeof( struct OFFSET));
	offset->dx = ptr->dx * xscale;
	offset->dy = ptr->dy * yscale;
	offset->prevo = tail;
	offset->nexto = NULL;
	if (head)
	   tail->nexto = offset;
	else
	   head = offset;
	tail = offset;
      }
   return( save_path(head,tail,path->count) );
}



/**************************************************************/
/* TANGOpath_extend -- Add x and y factors to each offset.    */
/* 							      */
/* RETURNS:  Modified path.				      */
/**************************************************************/
TANGO_PATH
TANGOpath_extend(path,x,y)
   TANGO_PATH path;
   double x,y;
{
   OFFSET_PTR ptr, offset, head=NULL, tail=NULL;

   if (!TANGO__data) TANGOinit();

   DEBUG("TANGOpath_extend(0x%lx, %f, %f)\n", (unsigned long) path, x, y);

   for (ptr=path->head; ptr; ptr=ptr->nexto)
      { offset = (OFFSET_PTR) malloc( sizeof( struct OFFSET));
	offset->dx = ptr->dx + x;
	offset->dy = ptr->dy + y;
	offset->prevo = tail;
	offset->nexto = NULL;
	if (head)
	   tail->nexto = offset;
	else
	   head = offset;
	tail = offset;
      }
   return( save_path(head,tail,path->count) );
}



/**************************************************************/
/* TANGOpath_interpolate -- Create a path which contains      */
/*			    an interpolated number of offsets */
/*			    from the given path as indicated  */
/*			    by the parameter factor.	      */
/* 							      */
/* RETURNS:  Specified path.				      */
/**************************************************************/
TANGO_PATH
TANGOpath_interpolate(path,factor)
   TANGO_PATH path;
   double factor;
{
   int count,found;
   double x,y,runx,runy,remainder,total,percent;
   OFFSET_PTR off,offset,head=NULL,tail=NULL;

   DEBUG("TANGOpath_interpolate(0x%lx, %f)\n", (unsigned long) path, factor);

   count = 0;
   remainder = 0.0;
   runx = runy = 0.0;
   total = factor;
   found = 0;
   off = path->head;
   x = off->dx;
   y = off->dy;
   while (!found)
      { if ((remainder+factor) < 1.0)
	   { runx += x;
	     runy += y;
	     off = off->nexto;
	     if (off)
		{ x = off->dx;
		  y = off->dy;
		  total += factor;
		  remainder += factor;
		}
	     else
		found = 1;
	   }
	else
	   { while (total >= 1.0)
		{ percent = (1.0 - remainder) / factor;
		  offset = (OFFSET_PTR) malloc( sizeof( struct OFFSET));
		  count++;
		  offset->dx = runx + (percent * x);
		  offset->dy = runy + (percent * y);
		  offset->prevo = tail;
		  offset->nexto = NULL;
		  if (head)
		     tail->nexto = offset;
		  else
		     head = offset;
		  tail = offset;
		  runx = runy = 0.0;
		  remainder = 0.0;
		  total -= 1.0;
		}
	     remainder = total;
	     runx = (remainder / factor) * x;
	     runy = (remainder / factor) * y;
	     off = off->nexto;
	     if (off)
		{ x = off->dx;
		  y = off->dy;
		  total += factor;
		}
	     else
		found = 1;
	   }
      }
   offset = (OFFSET_PTR) malloc( sizeof( struct OFFSET));
   count++;
   offset->dx = runx;
   offset->dy = runy;
   offset->prevo = tail;
   offset->nexto = NULL;
   if (head)
      tail->nexto = offset;
   else
      head = offset;
   tail = offset;

   return( save_path(head,tail,count) );
}



/**************************************************************/
/* TANGOpath_example -- Create and return a path which        */
/*			looks like the given path, only it    */
/*			begins at fromloc and runs to the     */
/*			toloc.				      */
/* 							      */
/* RETURNS:  Specified path.				      */
/**************************************************************/
TANGO_PATH
TANGOpath_example(fromloc,toloc,path)
   TANGO_LOC  fromloc;
   TANGO_LOC  toloc;
   TANGO_PATH path;
{
   WIN_COORD	loc_dx,loc_dy;	    /* total x and y changes in given locs */
   WIN_COORD	path_dx,path_dy;    /* total x and y changes in given path */
   OFFSET_PTR	op,offset,head,tail;
   WIN_COORD	dx_ratio,dy_ratio;  /* loc-path differences over # points  */
   int xscale,yscale;

   if (!TANGO__data) TANGOinit();

   DEBUG("TANGOpath_example(0x%lx, 0x%lx, 0x%lx)\n", (unsigned long) fromloc, (unsigned long) toloc, (unsigned long) path);

   if (!path)
      return( TANGOpath_example(fromloc,toloc,
				TANGOpath_type(TANGO_PATH_TYPE_STRAIGHT)) );

   loc_dx = toloc->x - fromloc->x;
   loc_dy = toloc->y - fromloc->y;
   path_dx = TANGOpath_dx(path);
   path_dy = TANGOpath_dy(path);

   if ( EQUAL(path_dx,0.0) )
      { dx_ratio = loc_dx / (double)(path->count);
	xscale = 0;
      }
   else
      { dx_ratio = loc_dx / path_dx;
	xscale = 1;
      }

   if ( EQUAL(path_dy,0.0) )
      { dy_ratio = loc_dy / (double)(path->count);
	yscale = 0;
      }
   else
      { dy_ratio = loc_dy / path_dy;
	yscale = 1;
      }

   head = tail = NULL;

   for (op=path->head; op; op=op->nexto)
      { offset = (OFFSET_PTR) malloc( sizeof( struct OFFSET));

	if (xscale)
	   offset->dx = op->dx * dx_ratio;
	else
	   offset->dx = op->dx + dx_ratio;

	if (yscale)
	   offset->dy = op->dy * dy_ratio;
	else
	   offset->dy = op->dy + dy_ratio;

	if (!head)
	   head = offset;
	else
	   tail->nexto = offset;
	offset->prevo = tail;
	offset->nexto = NULL;
	tail = offset;
      }

   return( save_path(head,tail,path->count) );
}



/**************************************************************/
/* TANGOpath_motion -- Create a path of the given type that   */
/*		       moves between the two locations.	      */
/* 							      */
/* RETURNS:  Specified path.				      */
/**************************************************************/
TANGO_PATH
TANGOpath_motion(fromloc,toloc,type)
   TANGO_LOC  fromloc;
   TANGO_LOC  toloc;
   TANGO_PATH_TYPE type;
{
   TANGO_PATH path,ret_path;

   DEBUG("TANGOpath_motion(0x%lx, 0x%lx, %d)\n", (unsigned long) fromloc, (unsigned long) toloc, type);

   path = TANGOpath_type(type);
   ret_path = TANGOpath_example(fromloc,toloc,path);
   path_free(path);
   return(ret_path);
}



/**************************************************************/
/* TANGOpath_distance -- Create and return a path that moves  */
/*			 from the fromloc to the toloc, and   */
/*			 that has an offset every time the    */
/*			 given distance has been covered.     */
/* 							      */
/* RETURNS:  Specified path.				      */
/**************************************************************/
TANGO_PATH
TANGOpath_distance(fromloc,toloc,distance)
   TANGO_LOC fromloc,toloc;
   WIN_COORD distance;
{
   int	       steps,i;
   WIN_COORD   fromx,fromy,tox,toy,
	       dx,dy,totdist,ratio,
	       xstep,ystep,runx,runy;
   OFFSET_PTR  offset,head,tail;

   if (!TANGO__data) TANGOinit();

   DEBUG("TANGOpath_distance(0x%lx, 0x%lx, %f)\n", (unsigned long) fromloc, (unsigned long) toloc, distance);

   TANGOloc_inquire(fromloc,&fromx,&fromy);
   TANGOloc_inquire(toloc,&tox,&toy);
   dx = tox- fromx;
   dy = toy - fromy;

   totdist = sqrt( (dx*dx) + (dy*dy) );

   ratio = distance / totdist;

   steps = (int) (1.0 / ratio);

   xstep = dx * ratio;
   ystep = dy * ratio;
   runx = fromx;
   runy = fromy;

   head = tail = NULL;
   for (i=1; i<=steps; ++i)
      { offset = (OFFSET_PTR) malloc( sizeof( struct OFFSET));
	offset->dx = xstep;
	offset->dy = ystep;
	if (!head)
	   head = offset;
	else
	   tail->nexto = offset;
	offset->prevo = tail;
	offset->nexto = NULL;
	tail = offset;

	runx += xstep;
	runy += ystep;
      }

   if ((runx != dx) || (runy != dy) || (steps == 0))
      { offset = (OFFSET_PTR) malloc( sizeof( struct OFFSET));
	offset->dx = tox - runx;
	offset->dy = toy - runy;
	if (!head)
	   head = offset;
	else
	   tail->nexto = offset;
	offset->prevo = tail;
	offset->nexto = NULL;
	tail = offset;
	++steps;
      }

   return( save_path(head,tail,steps) );
}



/**************************************************************/
/* TANGOpath_iterate -- Return a path which is "num"          */
/*			iterations of the path "path"	      */
/*			concatenated after each other.	      */
/* 							      */
/* RETURNS:  Specified path.				      */
/**************************************************************/
TANGO_PATH
TANGOpath_iterate(path,num)
   TANGO_PATH path;
   double num;
{
   OFFSET_PTR offset,head,tail,op;
   int whole,num_offsets,parts,i;
   double decimal;

   if (!TANGO__data) TANGOinit();

   DEBUG("TANGOpath_iterate(0x%lx, %f)\n", (unsigned long) path, num);

   whole = (int) num;

   head = tail = NULL;
   num_offsets = 0;

   for (i=1; i<=whole; ++i)
      { for (op=path->head; op; op=op->nexto)
	   { offset = (OFFSET_PTR) malloc( sizeof( struct OFFSET));
	     offset->dx = op->dx;
	     offset->dy = op->dy;
	     if (!head)
		head = offset;
	     else
		tail->nexto = offset;
	     offset->prevo = tail;
	     offset->nexto = NULL;
	     tail = offset;
	   }
	num_offsets += path->count;
      }

   decimal = num - (double) whole;
   parts = (int) (decimal * (double)(path->count));

   op = path->head;
   for (i=1; i<=parts; ++i)
      { offset = (OFFSET_PTR) malloc( sizeof( struct OFFSET));
	offset->dx = op->dx;
	offset->dy = op->dy;
	if (!head)
	   head = offset;
	else
	   tail->nexto = offset;
	offset->prevo = tail;
	offset->nexto = NULL;
	tail = offset;
	op = op->nexto;
      }
   num_offsets += parts;

   return( save_path(head,tail,num_offsets) );
}



/**************************************************************/
/* TANGOpath_concatenate -- Concatenate "num" paths together  */
/*			    (they're stored in the array      */
/*			    "paths") into one new path.	      */
/* 							      */
/* RETURNS:  Concatenated path.				      */
/**************************************************************/
TANGO_PATH
TANGOpath_concatenate
#ifndef _NO_PROTO		/* NPM: <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
(int num, ...)
#else  /* defined(_NO_PROTO) */
(va_alist) va_dcl
#endif /* !defined(_NO_PROTO) ==> ANSI */
{
   OFFSET_PTR offset,head,tail,op;
   int count,i;
   TANGO_PATH paths[50];
   va_list ap;
#ifndef _NO_PROTO		/* NPM: <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
   va_start(ap, num);
#else  /* defined(_NO_PROTO) */
   int num;
   va_start(ap);
   num = va_arg(ap, int);
#endif /* !defined(_NO_PROTO) ==> ANSI */

   if (!TANGO__data) TANGOinit();

   for (i=0; i<num; i++)
      paths[i] = va_arg(ap, TANGO_PATH);

   DEBUG("TANGOpath_concatenate(%d, 0x%lx...)\n", num, (unsigned long) paths[0]);

   head = tail = NULL;
   count = 1;

   for (i=0; i<=num-1; ++i)
      { for (op=paths[i]->head; op; op=op->nexto)
	   { offset = (OFFSET_PTR) malloc( sizeof( struct OFFSET));
	     offset->dx = op->dx;
	     offset->dy = op->dy;
	     if (!head)
		head = offset;
	     else
		tail->nexto = offset;
	     offset->prevo = tail;
	     offset->nexto = NULL;
	     tail = offset;
	   }
	count += paths[i]->count;
      }
   va_end(ap);
   return( save_path(head,tail,count) );
}



/**************************************************************/
/* TANGOpath_compose -- Return a path which is the            */
/*			composition of all the paths in       */
/*			"paths".  The paths must have the     */
/*			same number of offsets.		      */
/* 							      */
/* RETURNS:  Composed path.				      */
/**************************************************************/
TANGO_PATH
TANGOpath_compose
#ifndef _NO_PROTO		/* NPM: <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
(int num, ...)
#else  /* defined(_NO_PROTO) */
(va_alist) va_dcl
#endif /* !defined(_NO_PROTO) ==> ANSI */
{
   int i,j,count;
   WIN_COORD dx,dy;
   OFFSET_PTR offset,head,tail;
   OFFSET_PTR ptr[50];
   TANGO_PATH paths[50];
   va_list ap;
#ifndef _NO_PROTO		/* NPM: <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
   va_start(ap, num);
#else  /* defined(_NO_PROTO) */
   int num;
   va_start(ap);
   num = va_arg(ap, int);
#endif /* !defined(_NO_PROTO) ==> ANSI */

   if (!TANGO__data) TANGOinit();

   for (i=0; i<num; i++)
      paths[i] = va_arg(ap, TANGO_PATH);

   DEBUG("TANGOpath_compose(%d, 0x%lx...)\n", num, (unsigned long) paths[0]);

   count = paths[0]->count;

#ifndef WINTERP			/* NPM: Xtango_Prim_TANGO_TX_COMPOSE() checks for this condition- */
   for (i=1; i<num; ++i)	/* NPM: -so we don't need to check for it here (nor worry about- */
      if (paths[i]->count != count) /* NPM: -dealing with TANGO_PATH return value of NULL)... */
	 return(NULL);
#endif /* !defined(WINTERP) */

   for (i=0; i<num; ++i)
      { ptr[i] = paths[i]->head;
      }
   head = tail = NULL;
   for (i=1; i<=count; ++i)
      { dx = dy = ZERO;
	for (j=0; j<num; ++j)
	   { dx += ptr[j]->dx;
	     dy += ptr[j]->dy;
	     ptr[j] = ptr[j]->nexto;
	   }
	offset = (OFFSET_PTR) malloc( sizeof( struct OFFSET));
	offset->dx = dx;
	offset->dy = dy;
	if (!head)
	   head = offset;
	else
	   tail->nexto = offset;
	offset->prevo = tail;
	offset->nexto = NULL;
	tail = offset;
      }
   va_end(ap);
   return( save_path(head,tail,count) );
}



/**************************************************************/
/* TANGOpath_free -- Free up the malloced space occupied by   */
/*		     "path."                                  */
/* 							      */
/* RETURNS:  None.					      */
/**************************************************************/
void
TANGOpath_free
#ifndef _NO_PROTO		/* NPM: <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
(int num, ...)
#else  /* defined(_NO_PROTO) */
(va_alist) va_dcl
#endif /* !defined(_NO_PROTO) ==> ANSI */
{
   int i;
   va_list ap;
#ifndef _NO_PROTO		/* NPM: <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
   va_start(ap, num);
#else  /* defined(_NO_PROTO) */
   int num;
   va_start(ap);
   num = va_arg(ap, int);
#endif /* !defined(_NO_PROTO) ==> ANSI */

   if (!TANGO__data) TANGOinit();

   for (i=0; i<num; i++)
      path_free( va_arg(ap, TANGO_PATH) );

   DEBUG("TANGOpath_free(%d, ...)\n", num);
   va_end(ap);
}



/**************************************************************/
/* save_path -- Receive a list of offsets and save them as a  */
/*		new path.				      */
/* 							      */
/* RETURNS:  Newly created path.			      */
/**************************************************************/
TANGO_PATH
save_path(head,tail,count)
   OFFSET_PTR head;
   OFFSET_PTR tail;
   int count;
{
   TANGO_PATH new_path;

   new_path = (TANGO_PATH) malloc( sizeof (struct _PATH));
   new_path->head = head;
   new_path->tail = tail;
   new_path->count = count;
   new_path->nextp = NULL;
   new_path->prevp = NULL;
   return(new_path);
}



/**************************************************************/
/* path_free -- Free up the malloced space occupied by path.  */
/* 							      */
/* RETURNS:  None.					      */
/**************************************************************/
void
path_free(path)
   TANGO_PATH path;
{
   OFFSET_PTR op,old;

   if (!path) return;
   op = path->head;
   while (op)
      { old = op;
	op = op->nexto;
	free(old);
      }
   free(path);
}

/**************************************************************/
/*****************    end of xtangopath.c    ******************/
/**************************************************************/
