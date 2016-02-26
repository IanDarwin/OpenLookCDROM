
/**************************************************************************
   Touchup a bitmap graphics editor for the Sun Workstation running SunView
   Copyright (c) 1988 by Raymond Kreisel
   1/22/88 @ Suny Stony Brook

   This program may be redistributed without fee as long as this copyright
   notice is intact.

==> PLEASE send comments and bug reports to one of the following addresses:

	   Ray Kreisel
	   CS Dept., SUNY at Stony Brook, Stony Brook NY 11794

	   UUCP: {allegra, philabs, pyramid, research}!sbcs!rayk   
	   ARPA-Internet: rayk@sbcs.sunysb.edu			
	   CSnet: rayk@suny-sb
	   (If nobody is home at any of the above addresses try:
		S72QKRE@TOWSONVX.BITNET			        )

 "If I get home before daylight, I just might get some sleep tonight...."

**************************************************************************/
/**************************************************************************
	file: ffill.c
	purpose: this file has the functions that do flood fill

	modifications:
		date:	Tue Mar 22 22:04:58 EST 1988
		author:	rayk
		changes:add comments

	modifications:
		date:	Fri Jun 24 23:50:13 EDT 1988
		author:	tnosoes!tom@uunet.UU.NET & rayk & Paul Heckbert
		changes:changed Paul Heckbert's flood fill so that
			it would flood fill with a pattern, pattern
			flood code was taken from tnosoes!tom@uunet.UU.NET
**************************************************************************/

#include "header.h"

static struct pixrect	*org;

/*
 * let's go into flood fill mode because we got a button click on flood fill
 */
fill_mode()
{
int x,y;


  if (select_pt_x != -1)
      {
         print_msg("Hold down the RIGHT mouse button to cancel flood fill.");
  	 x = select_pt_x;
         y = select_pt_y;
  	 clean_point();
	 save_screen();
	org= my_mem_create(image_wid,image_hgt,image_depth);
  	pr_rop(org,0,0,image_wid,image_hgt,PIX_SRC,pw->pw_prretained,0,0);

	 if (fill(x,y,cur_color))
		ERROR("Flood fill cancelled !!");
	 else
	    hide_msg();
	 MY_pr_destroy(org);
      }
  else
      {
	ERROR("Select point first, then select flood fill");
	set_select_mode();
      }
}


#define FILL_CANCEL 1
#define FILL_OK 0

/*
 * fill.c : one page seed fill program, 1 channel frame buffer version
 *
 * doesn't read each pixel twice like the BASICFILL algorithm in
 *      Alvy Ray Smith, "Tint Fill", SIGGRAPH '79
 *
 * Paul Heckbert        13 Sept 1982, 28 Jan 1987
 */

typedef int pixel;
extern int wx1, wx2, wy1, wy2;  /* screen window */

struct seg {short y, xl, xr, dy;};      /* horizontal segment of scan line y */
#define FILL_MAX 10000               /* max depth of stack */
#define pixelread(x,y) 		pr_get(org,x,y)
 
#define PUSH(Y, XL, XR, DY) \
    if (sp<stack+FILL_MAX && Y+(DY)>=wy1 && Y+(DY)<=wy2) \
    {sp->y = Y; sp->xl = XL; sp->xr = XR; sp->dy = DY; sp++;}

#define POP(Y, XL, XR, DY) \
    {sp--; Y = sp->y+(DY = sp->dy); XL = sp->xl; XR = sp->xr;}

/*
 * fill: set the pixel at (x,y) and all of its 4-connected neighbors
 * with the same pixel value to the new pixel value nv.
 * A 4-connected neighbor is a pixel above, below, left, or right of a pixel.
 */
fill(x, y, nv)
int x, y;       /* seed point */
pixel nv;       /* new pixel value */
{
int wx1=0, wx2, wy1=0, wy2;  /* screen window */

    int l, x1, x2, dy, sx;
    pixel ov;   /* old pixel value */
    struct seg stack[FILL_MAX], *sp = stack; /* stack of filled segments */

     wx2 = image_wid;
     wy2 = image_hgt;
     
    ov = pixelread(x, y);               /* read pv at seed point */
    if (ov==nv || x<wx1 || x>wx2 || y<wy1 || y>wy2) return(FILL_OK);
    PUSH(y, x, x, 1);                   /* needed in some cases */
    PUSH(y+1, x, x, -1);                /* seed segment (popped 1st) */

    while (sp>stack) {
        /* pop segment off stack and fill a neighboring scan line */
        POP(y, x1, x2, dy);
        /*
         * segment of scan line y-dy for x1<=x<=x2 was previously filled,
         * now explore adjacent pixels in scan line y
         */
        for (x=x1; x>=wx1 && pixelread(x, y)==ov; x--);
	pr_rop(org,x+1,y,x1-x,1,PIX_SET,0,0,0);
	pw_replrop(pw,x+1,y,x1-x,1,(nv == 0 ? PIX_CLR : PIX_SRC | PIX_COLOR(nv)),pattern[(int)panel_get_value(pattern_choice)],x+1,y);
        if (x>=x1) goto skip;
        l = x+1;
        if (l<x1) PUSH(y, l, x1-1, -dy);                /* leak on left? */
        x = x1+1;
        do {
	    sx = x;
            for (; x<=wx2 && pixelread(x, y)==ov; x++);
	    if (sx != x)
	    {
	      pr_rop(org,sx,y,x-sx,1,PIX_SET,0,0,0);
	      pw_replrop(pw,sx,y,x-sx,1,(nv == 0 ? PIX_CLR : PIX_SRC | PIX_COLOR(nv)),pattern[(int)panel_get_value(pattern_choice)],sx,y);
	    }
            PUSH(y, l, x-1, dy);
            if (x>x2+1) PUSH(y, x2+1, x-1, -dy);        /* leak on right? */
skip:       for (x++; x<=x2 && pixelread(x, y)!=ov; x++);
            l = x;
	} while (x<=x2);
        if (window_get(canvas, WIN_EVENT_STATE, MS_RIGHT))
            return(FILL_CANCEL);

    }
  return(FILL_OK);
}


