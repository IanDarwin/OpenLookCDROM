/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms of the license.
 *
 *	@(#) darea1.c 1.6 92/10/23 
 */

#include <xview/frame.h>
#include <xview/cms.h>
#include <xview/svrimage.h>
#include <sspkg/canshell.h> 
#include <sspkg/rectobj.h> 
#include <sspkg/drawobj.h> 
 
static unsigned short banana_bits[] = {
#include "../icons/banana.icon"
};

static unsigned short bananamask_bits[] = {
#include "../icons/bananamask.icon"
};

void		resize();
void		draw_drawarea();

Drawarea	drawarea;
Cms		cms;


#define		WHITE	0
#define		COLOR1	1
#define		COLOR2	2
#define		COLOR3	3
#define		COLOR4	4
#define 	BLACK	5


main(argc, argv)
	int	argc;
	char	*argv[];
{
	Frame		frame;
	Canvas_shell	shell;

	xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

	cms = (Cms) xv_create(XV_NULL, CMS,
		CMS_SIZE, 6,
		CMS_NAMED_COLORS, "snow", "peru", "orange", "firebrick", "gold", "black", NULL,
		NULL);
 
	drawarea = (Drawarea) xv_create(XV_NULL, DRAWAREA,
		NULL);

	frame = (Frame) xv_create(XV_NULL, FRAME, 
		FRAME_LABEL, argv[0],
		XV_WIDTH, 800,
		XV_HEIGHT, 400,
		NULL);
 
	shell = (Canvas_shell) xv_create(frame, CANVAS_SHELL, 
		WIN_CMS, cms,
		CANVAS_RESIZE_PROC, resize,
		NULL);

	xv_set(drawarea,
		XV_OWNER, shell,
		XV_WIDTH, xv_get(shell, XV_WIDTH),
		XV_HEIGHT, xv_get(shell, XV_HEIGHT),
		NULL);

	draw_drawarea(drawarea);


	xv_main_loop(frame); 
} 
 

void
resize(shell, width, height)
	Canvas_shell	shell;
	int		width;
	int		height;
{
	/*
	 * Fill the canvas_shell with the drawarea:
	 * Drawarea is created before the canvas, so the
	 * new size can be applied at any time.  Otherwise this
	 * would be a problem because xview calls the resize proc 
	 * from the xv_create of the canvas_shell.  The alternate
	 * way of doing this is to test to see if drawarea has 
	 * been created yet.  Also, this allows the drawarea
	 * to come up initialized to the right size.
	 */
	xv_set(drawarea,
		XV_WIDTH, width,
		XV_HEIGHT, height,
		NULL);
}



/* position, sizes, and centers for pie graph. */
#define PIEX 1000
#define PIEY 2000
#define PIEW 3000
#define PIEH 6000
#define PIECENTERX (PIEX+PIEW/2)
#define PIECENTERY (PIEY+PIEH/2)

void
draw_drawarea(drawarea)
	Drawarea drawarea;
{
	Server_image banana;
	Server_image bananamask;

	banana = (Server_image) xv_create(XV_NULL, SERVER_IMAGE,
		XV_WIDTH, 32,
		XV_HEIGHT, 20,
		SERVER_IMAGE_BITS, banana_bits,
		NULL);

	bananamask = (Server_image) xv_create(XV_NULL, SERVER_IMAGE,
		XV_WIDTH, 32,
		XV_HEIGHT, 20,
		SERVER_IMAGE_BITS, bananamask_bits,
		NULL);

	/*
	 * Draw some lines, much of which will be painted over
	 * by the arcs of the pie chart...
	 */

	VSetColor( drawarea, BLACK );
	VSetLineWidth( drawarea, 3);
        VDrawLine( drawarea, PIECENTERX, PIECENTERY, 
			3750, 2500);

        VDrawLine( drawarea, PIECENTERX, PIECENTERY, 
			1725, 1775);

        VDrawLine( drawarea, PIECENTERX, PIECENTERY, 
			750, 4950);

        VDrawLine( drawarea, PIECENTERX, PIECENTERY, 
			3000, 8300);

	/*
	 * Draw some filled slices of the pie....
	 */
	VSetColor( drawarea, COLOR1 );
	VFillArc( drawarea, PIEX, PIEY, PIEW, PIEH, 
		0, 90*64);

	VSetColor( drawarea, COLOR2 );
	VFillArc( drawarea, PIEX, PIEY, PIEW, PIEH, 
		90*64, 45*64);

	VSetColor( drawarea, COLOR3 );
	VFillArc( drawarea, PIEX, PIEY, PIEW, PIEH, 
		135*64, 80*64);

	VSetColor( drawarea, BLACK );
        VSetStipple( drawarea, bananamask);
        VSetFillStyle( drawarea, FillOpaqueStippled);
	VFillArc( drawarea, PIEX, PIEY, PIEW, PIEH, 
		215*64, 145*64);

	VSetColor( drawarea, COLOR4);
        VSetStipple( drawarea, banana);
        VSetFillStyle( drawarea, FillStippled);
	VFillArc( drawarea, PIEX, PIEY, PIEW, PIEH, 
		215*64, 145*64);
        VSetFillStyle( drawarea, FillSolid);

	VSetColor( drawarea, BLACK );

	/*
	 * Draw the surrounding circle.
	 */
	VDrawArc( drawarea, PIEX, PIEY, PIEW, PIEH, 
		0*64, 360*64);

	/*
	 * Draw the labels.
	 */
        VDrawString( drawarea, 3800, 2550, "Apples", 6);
        VDrawString( drawarea, 1600, 1700, "Tangerines", 10);
        VDrawString( drawarea, 350, 4900, "Oranges", 7);
        VDrawString( drawarea, 3050, 8350, "Bananas", 7);

	/*
	 * Draw the bar chart.
	 */

	VSetColor( drawarea, COLOR1 );
	VFillRectangle( drawarea, 6500, 1550, 1860, 1650);

	VSetColor( drawarea, COLOR2 );
	VFillRectangle( drawarea, 6500, 3300, 930, 1650);

	VSetColor( drawarea, COLOR3 );
	VFillRectangle( drawarea, 6500, 5050, 1655, 1650);

	VSetColor( drawarea, BLACK );
        VSetStipple( drawarea, bananamask);
        VSetFillStyle( drawarea, FillStippled);
	VFillRectangle( drawarea, 6500, 6800, 3000, 1650);

	VSetColor( drawarea, COLOR4);
        VSetStipple( drawarea, banana);
	VFillRectangle( drawarea, 6500, 6800, 3000, 1650);
        VSetFillStyle( drawarea, FillSolid);
	VDrawRectangle( drawarea, 6500, 6800, 3000, 1650);

	/*
	 * Draw the box around the bar chart.
	 */
	VSetColor( drawarea, BLACK );
	VDrawRectangle( drawarea, 6500, 1500, 3100, 7000 );

	/*
	 * Label the bar chart.
	 */

        VDrawString( drawarea, 5500, 2325, "Apples", 6);
        VDrawString( drawarea, 5500, 4075, "Tangerines", 10);
        VDrawString( drawarea, 5500, 5825, "Oranges", 7);
        VDrawString( drawarea, 5500, 7575, "Bananas", 7);
}

