/*
 *      (c) Copyright 1991 Scott Oaks
 *      See LEGAL_NOTICE file for terms of the license.
 */

#ifdef IDENT
#ident	"@(#)virtual.h	1.3 olvwm version 07 Jan 1994"
#endif

#ifndef _OLWM_VIRTUAL_H
#define _OLWM_VIRTUAL_H

/*
 * The virtual desktop exists as a logical abstraction only; there is no
 * actual X window called the virtual desktop to which the frames are
 * reparented (unlike other virtual managers).  Frames are still reparented
 * to the root.
 *
 * Moving the virtual desktop then means that we have to move each window;
 * windows which no longer appear on the screen have coordinates outside
 * of [(0,0);(1152,900)] (or whatever the size of your screen is).  This
 * means that windows on the screen still map to (0,0);(1152,900).
 *
 * Moving each window doesn't really entail much overhead, since even if
 * we had a virtual root and shifted it, we'd still have to send synthetic
 * configure events to each window.
 *
 *
 * The Virtual Desktop is the logical abstraction and is several screens
 * large.  The VDM is the X window which displays this abstraction and
 * is a small X window on the display.  The VDM will have a frame and look
 * like any other base window, but it may never be unstuck.

                    <- absoluteWidth ->
     |-----------------------------------------------------|
   ^ |                                                     |
   | |                                                     |
   a |                                                     |
   b |            ->offsetX, offsetY (negative from up/left|
   s |            |                                        |
   o |            *******************                      |
   l |            *                 *                      |
   u |            *                 *                      |
   t |            *                 *                      |
   e |            *                 *                      |
   H |            *                 *                      |
   e |            *******************                      |
   i |                                                     |
   g |                                                     |
   h |                                                     |
   t |-----------------------------------------------------|

 */

typedef struct _virtualresources {
    char		*size;
    char		*geometry;
    char		*iconGeometry;
    int			scale;
    VirtualGridType	grid;
    char		*background;
    PixInfo		pixInfo;
} VirtualResources;

typedef struct _virtualdesktop {
    struct _client	*client;	/* Handle to client structure; this
					 * leads to a recursive pointer trail */

    int		offsetX;		/* Position of the upper/left corner  */
    int		offsetY;		/* of the current screen on the VDM
					 * These will always be <= 0, since it
					 * should be added to a windows coord
					 * to determine where on the virtual
					 * desktop the window is              */

    int		absoluteWidth;		/* Width of virtual desktop in pixels */
    int		absoluteHeight;		/* Height of virtual desk in pixels   */

    int		columns;		/* Number of logical screens across   */
    int		rows;			/* Number of logical screens down     */
    int		height;			/* height of the VDM in pixels        */
    int		width;			/* width of the VDM in pixels         */
    int		screenWidth;		/* Width of logical screen within VDM */
    int		screenHeight;		/* Height of logical screen within VDM*/
    int		screenX;		/* X position of current screen in VDM*/
    int		screenY;		/* Y position of current screen in VDM*/
    int		max_ascent;		/* max ascent of font in VDM          */
    int		saveX;			/* for save/restore, last position    */
    int		saveY;			/* for save/restore, last position    */
    VirtualResources	*resources;	/* Screen-specific virtual resources  */
} VirtualDesktop;

/*
 * Structure to bundle the information needed to perform a drag/move within
 * the VDM
 */
typedef struct _vdmstuff {
    VirtualDesktop	*vdm;		/* Virtual Desktop involved in move   */
    int			initX, initY;	/* Initial location of screenX        */
    int			pointerX,	/* Initial press of button in move    */
			pointerY;
    int			numMoves;	/* number of times we got a motion ev */
    GC			gc;		/* gc to draw with (use root gc       */
} VDMstuff;

/*
 * Structure to bundle amount a view into the desktop has moved
 */
struct deltas {
    double	delta_x, delta_y;
};

#endif	/* _OLWM_VIRTUAL_H */
