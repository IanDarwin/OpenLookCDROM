/* File: pixmap.c     */
/* Author: Steven Lee */
/* Created: 06/10/94  */
/* Updated: 06/10/94  */

/* Description: */
/* Simple example demonstrating the use of pixmaps.
   This requires the XPM include files and library.

   Compile using:
      $(CC) -I$OPENWINHOME/include pixmap.c -lxview -lolgx -lX11 -lXpm
*/

#ident "%Z% %M% %I% %E% %U%"

#include <stdio.h>
#include <xview/xview.h>
#include <xview/frame.h>
#include <xview/panel.h>
#include <xpm.h>

/* Change to name of your pixmap file. */
#define PIXMAP_FILE "pixmap.xpm"

int main(int argc, char *argv[])
{
	Frame frame;
	Panel panel;
	Display *dpy;
	Pixmap image, image_mask;
	Server_image simage;
	int status;

	xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);
	frame=(Frame)xv_create (XV_NULL, FRAME,
	  FRAME_LABEL, "Pixmap Demo",
	  NULL);
	panel=(Panel)xv_create(frame, PANEL, NULL);

	dpy=(Display *)xv_get(frame, XV_DISPLAY), 

	status=XpmReadFileToPixmap(dpy, DefaultRootWindow(dpy), PIXMAP_FILE,
	  &image, &image_mask, NULL);
	if (status == XpmSuccess) {
		simage=(Server_image)xv_create(XV_NULL, SERVER_IMAGE,
		  SERVER_IMAGE_PIXMAP, image,
		  NULL);
	}
	else {
		fprintf(stderr, "Error %d reading %s\n", status, PIXMAP_FILE);
		exit(1);
	}

    xv_create(panel, PANEL_BUTTON,
	  PANEL_LABEL_IMAGE, simage,
      NULL);

	xv_main_loop(frame);
	exit(0);
}
