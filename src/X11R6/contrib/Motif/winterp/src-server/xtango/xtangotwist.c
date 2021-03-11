/* -*-C-*-
*******************************************************************************
*
* File:         xtangotwist.c
* RCS:          $Header: /disk3/npm/src/winterp/src-server/xtango/RCS/xtangotwist.c,v 2.5 1994/06/09 01:30:16 npm Exp $
* Description:  TWIST PACKAGE
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
static char rcs_identity[] = "@(#)$Header: /disk3/npm/src/winterp/src-server/xtango/RCS/xtangotwist.c,v 2.5 1994/06/09 01:30:16 npm Exp $";

/* Modification Date  Description			      */
/* -----------------  --------------------------------------- */
/* 90/10/26 JDH	      Baselined source.                       */
/*							      */
/**************************************************************/

/**************************************************************/
/*****************	 include files       ******************/ 
/**************************************************************/

#include  "xtangolocal.h"

/**************************************************************/
/*****************	LOCAL data types     ******************/
/**************************************************************/

typedef struct TREENODE
{
   WIN_COORD x;
   WIN_COORD y;
   int	     level;
   int	     count;
   TANGO_LOC loc;
   struct TREENODE *next;
} *TREENODE_PTR;

/**************************************************************/
/*****************	GLOBAL variables     ******************/
/**************************************************************/

/**************************************************************/
/*****************	LOCAL variables      ******************/
/**************************************************************/

#define   BOTTOM     0
#define   LEFT	     0
#define   MIDDLE     1
#define   TOP	     2
#define   RIGHT      2

/**************************************************************/
/*****************      LOCAL functions      ******************/
/**************************************************************/

void	      create_rect_array();
void	      create_line_array();
void	      create_circle_array();
TREENODE_PTR  allocate_node();

/**************************************************************/
/*****************	  entry points       ******************/
/**************************************************************/


/**************************************************************/
/* TWISTcreate_loc_array -- Create a line of TANGO_LOCS       */
/*			    subject to the given parameters.  */
/* 							      */
/* RETURNS:  None.					      */
/**************************************************************/
void
TWISTcreate_loc_array(assoc,id,num,horiz,x,y,spacing)
   char	     *assoc;
   int	     id;
   int	     num;
   int	     horiz;
   WIN_COORD x,y;
   WIN_COORD spacing;
{
   int	     i;
   TANGO_LOC loc;

   DEBUG("TWISTcreate_loc_array(\"%s\", %d, %d, %d, %f, %f, %f)\n",
	 assoc, id, num, horiz, x, y, spacing);

   for (i=0; i<num; ++i)
      { loc = TANGOloc_create(x,y);
	if (!assoc)
	   ASSOCstore("ID",id,i,loc);
	else
	   ASSOCstore(assoc,id,i,loc);
	if (horiz)
	   x += spacing;
	else
	   y += spacing;
     }
}



/**************************************************************/
/* TWISTcreate_2d_loc_array -- Create a grid of TANGO_LOCS    */
/*			       subject to the given	      */
/*			       parameters.		      */
/* 							      */
/* RETURNS:  None.					      */
/**************************************************************/
void
TWISTcreate_2d_loc_array(assoc,id,rows,cols,x,y,xspacing,yspacing)
   char	     *assoc;
   int	     id;
   int	     rows,cols;
   WIN_COORD x,y;
   WIN_COORD xspacing,yspacing;
{
   int	     r,c;
   WIN_COORD px,py;
   TANGO_LOC loc;

   DEBUG("TWISTcreate_2d_loc_array(\"%s\", %d, %d, %d, %f, %f, %f, %f)\n"
	 , assoc, id, rows, cols, x, y, xspacing, yspacing);

   py = y;
   for (r=0; r<rows; ++r)
      { px = x;
	for (c=0; c<cols; ++c)
	   { loc = TANGOloc_create(px,py);
	     if (!assoc)
		ASSOCstore("ID3",id,r,c,loc);
	     else
		ASSOCstore(assoc,id,r,c,loc);
	     px += xspacing;
	   }
	py += yspacing;
     }
}



/**************************************************************/
/* TWISTcreate_image_array -- Create a row or column of       */
/*     			      TANGO_IMAGEs subject to the     */
/*			      given parameters.		      */
/* 							      */
/* RETURNS:  None.					      */
/**************************************************************/
void
TWISTcreate_image_array(assoc,id,num,type,horiz,just,x,y,xvals,xfactor,
		     yvals,yfactor,spacing,vis,color,fill,width,style)
   char		    *assoc;
   int		    id;
   int		    num;
   TANGO_IMAGE_TYPE type;
   int		    horiz;	  /*  1-horiz, 0-vert			  */
   int		    just;	  /*  justification: 0,1, or 2		  */
   WIN_COORD	    x,y;	  /*  starting placement loc		  */
   double	    xvals[];	  /*  width factors (if NULL, constant)   */
   WIN_COORD	    xfactor;	  /*  multiply by xvals to get widths	  */
   double	    yvals[];	  /*  height factors (if NULL, constant   */
   WIN_COORD	    yfactor;	  /*  multiply by yvals to get heights	  */
   WIN_COORD	    spacing;	  /*  spacing between images		  */
   int		    vis;
   int		    color;
   double	    fill;
   double	    width;
   double	    style;
{
   DEBUG("TWISTcreate_image_array(\"%s\",%d,%d,0x%x,%d,%d,%f,%f,0x%x,%f,0x%x,%f,%f,%d,%d,%f,%f,%f)\n", assoc,id,num,type,horiz,just,x,y,xvals,
	 xfactor,yvals,yfactor,spacing,vis,color,fill,width,style);

   switch (type)
      {  case TANGO_IMAGE_TYPE_LINE:
	    create_line_array(assoc,id,num,horiz,just,x,y,xvals,xfactor,
		     yvals,yfactor,spacing,vis,color,width,style);
	    break;
	 case TANGO_IMAGE_TYPE_RECTANGLE:
	    create_rect_array(assoc,id,num,horiz,just,x,y,xvals,xfactor,
		     yvals,yfactor,spacing,vis,color,fill);
	    break;
	 case TANGO_IMAGE_TYPE_CIRCLE:
	    create_circle_array(assoc,id,num,horiz,just,x,y,xvals,xfactor,
		     yvals,yfactor,spacing,vis,color,fill);
	    break;
	 default: ;
      }
   return;
}



/**************************************************************/
/* TWISTcreate_graph -- Create an array of TANGO_LOC 	      */
/*			locations and an adjacency matrix,    */
/*			lay out a graph subject to the other  */
/*			parameters.			      */
/* 							      */
/* RETURNS:  None.					      */
/**************************************************************/
void
TWISTcreate_graph(assoc,id,num,loc,adj,im_type,size,fill)
   char		    *assoc;
   int		    id;
   int		    num;
   TANGO_LOC	    loc[];
   int		    adj[][50];
   TANGO_IMAGE_TYPE im_type;
   WIN_COORD	    size;
   double	    fill;
{
   WIN_COORD   hs;
   int	       v,w;
   WIN_COORD   x,y,x0,y0,x1,y1;
   TANGO_IMAGE im;

   DEBUG("TWISTcreate_graph(\"%s\", %d, %d, 0x%x, 0x%x, %d, %f, %f)\n",
	 assoc,id,num,loc,adj,im_type,size,fill);

   hs = size / 2.0;
   for (v=0; v<num; ++v)
      { TANGOloc_inquire(loc[v],&x,&y);
	if (im_type == TANGO_IMAGE_TYPE_RECTANGLE)
	   im = TANGOimage_create(TANGO_IMAGE_TYPE_RECTANGLE,x-hs,y-hs,1,
				  TANGO_COLOR_BLACK,size,size,fill);
	else if (im_type == TANGO_IMAGE_TYPE_CIRCLE)
	   im = TANGOimage_create(TANGO_IMAGE_TYPE_CIRCLE,x,y,1,
				  TANGO_COLOR_BLACK,size,fill);
	else
	   return;
	ASSOCstore("IMAGE_AT",id,loc[v],im);
      }

   ASSOCmake("EDGE_BETWEEN",3);
   for (v=0; v<num-1; ++v)
      for (w=v+1; w<num; ++w)
	 if (adj[v][w])
	    { TANGOloc_inquire(loc[v],&x0,&y0);
	      TANGOloc_inquire(loc[w],&x1,&y1);
	      im = TANGOimage_create(TANGO_IMAGE_TYPE_LINE,x0,y0,1,
				     TANGO_COLOR_BLACK,x1-x0,y1-y0,0.0,1.0,0);
	      ASSOCstore("EDGE_BETWEEN",id,v,w,im);
	      ASSOCstore("EDGE_BETWEEN",id,w,v,im);
	    }
}



/**************************************************************/
/* TWISTcreate_bintree -- This routine lays out a binary tree */
/*			  of TANGO_LOCs and sets up some      */
/*			  associations so they can be         */
/*			  retrieved.  It does a breadth-first */
/*			  search basically.		      */
/* 							      */
/* RETURNS:  None.					      */
/**************************************************************/
void
TWISTcreate_bintree(treeid,lx,by,rx,ty,edgelevels)
   int	     treeid;	/* used in association */
   WIN_COORD lx,by,rx,ty;
   int	     edgelevels;
{
   int	        i,nextlevel,number;
   int	        numonlevel[16];
   WIN_COORD    width[16];
   TREENODE_PTR node,lnode,rnode,Qbeg,Qend,old;
   WIN_COORD    x,y,wid,dy;
   TANGO_LOC    loc;

   DEBUG("TWISTcreate_bintree(d, %f, %f, %f, %f, %d)\n",
	 treeid, lx, by, rx, ty, edgelevels);

   if ((rx <= lx) || (by <= ty))
      { fprintf(stderr,"Illegal box coords passed to TWISTcreate_bintree\n");
	return;
      }

   ASSOCmake("NODE",3);
   ASSOCmake("NUMBER",2);
   ASSOCmake("PARENT",2);
   ASSOCmake("LCHILD",2);
   ASSOCmake("RCHILD",2);

   for (i=0; i<16; ++i)
      numonlevel[i] = 0;
   width[0] = (rx-lx)/2.0;
   for (i=1; i<16; ++i)
      width[i] = width[i-1] / 2.0;

   number = 0;
   dy = (by-ty) / edgelevels;
   loc = TANGOloc_create(lx + width[0],ty);
   node = allocate_node(lx+width[0],ty,0,0,loc);
   ASSOCstore("PARENT",treeid,loc,NULL);
   ASSOCstore("NODE",treeid,0,0,loc);
   ASSOCstore("NUMBER",treeid,number++,loc);
   Qbeg = Qend = node;

   while (Qbeg)
      { if (Qbeg->level == edgelevels) /* at the leaf level */
	   { ASSOCstore("LCHILD",treeid,Qbeg->loc,NULL);
	     ASSOCstore("RCHILD",treeid,Qbeg->loc,NULL);
	     old = Qbeg;
	     Qbeg = Qbeg->next;
	     free(old);
	   }
	else
	   { nextlevel = Qbeg->level + 1;
	     wid = width[nextlevel];
	     x = Qbeg->x - wid;
	     y = Qbeg->y + dy;
	     loc = TANGOloc_create(x,y);
	     lnode = allocate_node(x,y,nextlevel,numonlevel[nextlevel]++,loc);
	     ASSOCstore("PARENT",treeid,loc,Qbeg->loc);
	     ASSOCstore("LCHILD",treeid,Qbeg->loc,loc);
	     ASSOCstore("NODE",treeid,lnode->level,lnode->count,loc);
	     ASSOCstore("NUMBER",treeid,number++,loc);

	     x = Qbeg->x + wid;
	     loc = TANGOloc_create(x,y);
	     rnode = allocate_node(x,y,nextlevel,numonlevel[nextlevel]++,loc);
	     ASSOCstore("PARENT",treeid,loc,Qbeg->loc);
	     ASSOCstore("RCHILD",treeid,Qbeg->loc,loc);
	     ASSOCstore("NODE",treeid,lnode->level,lnode->count,loc);
	     ASSOCstore("NUMBER",treeid,number++,loc);

	     Qend->next = lnode;   /* add the 2 new ones to the queue */
	     lnode->next = rnode;
	     Qend = rnode;
	     old = Qbeg;
	     Qbeg = Qbeg->next;
	     free(old);
	   }
      }
}



/**************************************************************/
/* create_line_array -- Create a row or column of lines       */
/*			according to the given parameters.    */
/* 							      */
/* RETURNS:  None.					      */
/**************************************************************/
void
create_line_array(assoc,id,num,horiz,just,x,y,xvals,xfactor,yvals,yfactor,
		  spacing,vis,color,width,style)
   char	     *assoc;
   int	     id;
   int	     num;
   int	     horiz;	  /*  1-horiz, 0-vert			  */
   int	     just;	  /*  justification: 0,1, or 2		  */
   WIN_COORD x,y;	  /*  starting placement loc		  */
   double    xvals[];	  /*  width factors (if NULL, constant)   */
   WIN_COORD xfactor;	  /*  multiply by xvals to get widths	  */
   double    yvals[];	  /*  height factors (if NULL, constant   */
   WIN_COORD yfactor;	  /*  multiply by yvals to get heights	  */
   WIN_COORD spacing;	  /*  spacing between images		  */
   int	     vis;
   int	     color;
   double    width;
   double    style;
{
   int	       i;
   WIN_COORD   px,py;    /* running "location" of next image */
   WIN_COORD   sx,sy;    /* sizes */
   TANGO_IMAGE im;

   px = x;
   py = y;
   for (i=0; i<num; ++i)
      { if (horiz)
	   { if (yvals)
		sy =  yvals[i] * yfactor;
	     else
		sy = yfactor;

	     if (just == BOTTOM)
		im = TANGOimage_create(TANGO_IMAGE_TYPE_LINE,px,py-sy,vis,color,0.0,sy,width,style,0);
	     else if (just == MIDDLE)
		im =  TANGOimage_create(TANGO_IMAGE_TYPE_LINE,px,py-(sy/2.0),vis,color,0.0,sy,width,style,0);
	     else if (just == TOP)
		im = TANGOimage_create(TANGO_IMAGE_TYPE_LINE,px,py,vis,color,0.0,sy,width,style,0);

	     px += spacing;
	   }
	else /* vertical */
	   { if (xvals)
		sx =  xvals[i] * xfactor;
	     else
		sx = xfactor;

	     if (just == LEFT)
		im = TANGOimage_create(TANGO_IMAGE_TYPE_LINE,px,py,vis,color,sx,0.0,width,style,0);
	     else if (just == MIDDLE)
		im =  TANGOimage_create(TANGO_IMAGE_TYPE_LINE,px-(sx/2.0),py,vis,color,sx,0.0,width,style,0);
	     else if (just == RIGHT)
		im = TANGOimage_create(TANGO_IMAGE_TYPE_LINE,px-sx,py,vis,color,sx,0.0,width,style,0);

	     py += spacing;
	   }

	if (!assoc)
	   ASSOCstore("ID",id,i,im);
	else
	   ASSOCstore(assoc,id,i,im);
      }
}



/**************************************************************/
/* create_rect_array -- Create a row or column of rectangles  */
/*			according to the given parameters.    */
/* 							      */
/* RETURNS:  None.					      */
/**************************************************************/
void
create_rect_array(assoc,id,num,horiz,just,x,y,xvals,xfactor,yvals,yfactor,
		  spacing,vis,color,fill)
   char	     *assoc;
   int	     id;
   int	     num;
   int	     horiz;		  /*  1-horiz, 0-vert			  */
   int	     just;		  /*  justification: 0,1, or 2		  */
   WIN_COORD x,y;	  /*  starting placement loc		  */
   double    xvals[];	  /*  width factors (if NULL, constant)   */
   WIN_COORD xfactor;	  /*  multiply by xvals to get widths	  */
   double    yvals[];	  /*  height factors (if NULL, constant   */
   WIN_COORD yfactor;	  /*  multiply by yvals to get heights	  */
   WIN_COORD spacing;	  /*  spacing between images		  */
   int	     vis;
   int	     color;
   double    fill;
{
   int	       i;
   WIN_COORD   px,py;    /* running "location" of next image */
   WIN_COORD   sx,sy;    /* sizes */
   TANGO_IMAGE im;

   px = x;
   py = y;
   for (i=0; i<num; ++i)
      { if (xvals)
	   sx =  xvals[i] * xfactor;
	else
	   sx = xfactor;
	if (yvals)
	   sy =  yvals[i] * yfactor;
	else
	   sy = yfactor;

	if (horiz)
	   { if (just == BOTTOM)
		im = TANGOimage_create(TANGO_IMAGE_TYPE_RECTANGLE,px,py-sy,vis,
				       color,sx,sy,fill);
	     else if (just == MIDDLE)
		im =  TANGOimage_create(TANGO_IMAGE_TYPE_RECTANGLE,
					px,py-(sy/2.0),vis,color,sx,sy,fill);
	     else if (just == TOP)
		im = TANGOimage_create(TANGO_IMAGE_TYPE_RECTANGLE,px,py,vis,
				       color,sx,sy,fill);

	     px += sx + spacing;
	   }
	else /* vertical */
	   { if (just == LEFT)
		im = TANGOimage_create(TANGO_IMAGE_TYPE_RECTANGLE,px,py,vis,
				       color,sx,sy,fill);
	     else if (just == MIDDLE)
		im =  TANGOimage_create(TANGO_IMAGE_TYPE_RECTANGLE,
					px-(sx/2.0),py,vis,color,sx,sy,fill);
	     else if (just == RIGHT)
		im = TANGOimage_create(TANGO_IMAGE_TYPE_RECTANGLE,px-color,
				       sx,py,vis,sx,sy,fill);

	     py += sy + spacing;
	   }

	if (!assoc)
	   ASSOCstore("ID",id,i,im);
	else
	   ASSOCstore(assoc,id,i,im);
      }
}



/**************************************************************/
/* create_circle_array -- Create a row or column of circles   */
/*			  according to the given parameters.  */
/* 							      */
/* RETURNS:  None.					      */
/**************************************************************/
void
create_circle_array(assoc,id,num,horiz,just,x,y,xvals,xfactor,yvals,yfactor,
		  spacing,vis,color,fill)
   char	     *assoc;
   int	     id;
   int	     num;
   int	     horiz;	  /*  1-horiz, 0-vert			  */
   int	     just;	  /*  justification: 0,1, or 2		  */
   WIN_COORD x,y;	  /*  starting placement loc		  */
   double    xvals[];	  /*  width factors (if NULL, constant)   */
   WIN_COORD xfactor;	  /*  multiply by xvals to get widths	  */
   double    yvals[];	  /*  height factors (if NULL, constant   */
   WIN_COORD yfactor;	  /*  multiply by yvals to get heights	  */
   WIN_COORD spacing;	  /*  spacing between images		  */
   int	     vis;
   int	     color;
   double    fill;
{
   int	       i;
   WIN_COORD   px,py;    /* running "location" of next image */
   WIN_COORD   rad;      /* radius of circles */
   TANGO_IMAGE im;

   px = x;
   py = y;
   for (i=0; i<num; ++i)
      { if (xvals)
	   rad =  xvals[i] * xfactor;
	else
	   rad = xfactor;

	if (horiz)
	   { if (just == BOTTOM)
		im = TANGOimage_create(TANGO_IMAGE_TYPE_CIRCLE,px,py-rad,vis,
				       color,rad,fill);
	     else if (just == MIDDLE)
		im =  TANGOimage_create(TANGO_IMAGE_TYPE_CIRCLE,px,py,vis,
					color,rad,fill);
	     else if (just == TOP)
		im = TANGOimage_create(TANGO_IMAGE_TYPE_CIRCLE,px,py+rad,vis,
				       color,rad,fill);

	     px += (2.0 * rad) + spacing;
	   }
	else /* vertical */
	   { if (just == LEFT)
		im = TANGOimage_create(TANGO_IMAGE_TYPE_CIRCLE,px+rad,py,vis,
				       color,rad,fill);
	     else if (just == MIDDLE)
		im =  TANGOimage_create(TANGO_IMAGE_TYPE_CIRCLE,px,py,vis,
					color,rad,fill);
	     else if (just == RIGHT)
		im = TANGOimage_create(TANGO_IMAGE_TYPE_CIRCLE,px-rad,py,vis,
				       color,rad,fill);

	     py += (2.0 * rad) + spacing;
	   }

	if (!assoc)
	   ASSOCstore("ID",id,i,im);
	else
	   ASSOCstore(assoc,id,i,im);
      }
}



/**************************************************************/
/* allocate_node -- Allocate a node that will go on the queue */
/*		    of tree nodes being set up.		      */
/* 							      */
/* RETURNS:  Node to go on the queue of tree nodes.	      */
/**************************************************************/
TREENODE_PTR
allocate_node(x,y,level,count,loc)
   WIN_COORD x,y;
   int	     level,count;
   TANGO_LOC loc;
{
   TREENODE_PTR node;

   node = (TREENODE_PTR) malloc( sizeof (struct TREENODE));
   node->x = x;
   node->y = y;
   node->level = level;
   node->count = count;
   node->loc = loc;
   node->next = NULL;
   return(node);
}

/**************************************************************/
/*****************   end of xtangotwist.c    ******************/
/**************************************************************/
