/*
     Copyright (c) 1994    Frank Adelstein

     The X Consortium, and any party obtaining a copy of these files from
     the X Consortium, directly or indirectly, is granted, free of charge, a
     full and unrestricted irrevocable, world-wide, paid up, royalty-free,
     nonexclusive right and license to deal in this software and
     documentation files (the "Software"), including without limitation the
     rights to use, copy, modify, merge, publish, distribute, sublicense,
     and/or sell copies of the Software, and to permit persons who receive
     copies from any such party to do so.  This license includes without
     limitation a license to do the foregoing actions under any patents of
     the party supplying this software to the X Consortium.
 */

#include "types.h"
#include <X11/extensions/shape.h>
#include <X11/IntrinsicP.h>

StartShape(glob, display, player, shape)
Glob  *glob;
Display *display;
Player *player;
Widget *shape;
{
  int shape_event_base, shape_error_base;
  Widget parent;
  int move, width, height, x, y, i;
  Dimension startx, starty, offsetx, offsety;
  int cnt;
  Arg args[15];
  Pixmap pm, mask;
  GC gc;
  Widget toplevel;

  /* make sure the server supports the shape extension */
  if (!XShapeQueryExtension(display, &shape_event_base, &shape_error_base)) 
    return;

  /* find the image we're going to use */
  if ((move = Findmove("zap", player)) == -1) 
    return;

  if (display == XtDisplay(glob->Toplevel1)) {
    pm       =  player->Images1[player->moves[move].frameindex[0]].pm; 
    mask     =  player->Images1[player->moves[move].frameindex[0]].mask; 
    gc       =  glob->blackgc1;
    toplevel = glob->Toplevel1;
  } else {
    pm       =  player->Images2[player->moves[move].frameindex[0]].pm; 
    mask     =  player->Images2[player->moves[move].frameindex[0]].mask;
    gc       =  glob->blackgc2;
    toplevel = glob->Toplevel2;
  }
  
  cnt = 0;
  XtSetArg (args[cnt], XtNx,  &startx);                     cnt++;
  XtSetArg (args[cnt], XtNy,  &starty);                     cnt++;
  XtGetValues(toplevel, args, cnt);

  cnt = 0;
  XtSetArg (args[cnt], XtNx,  &offsetx);                     cnt++;
  XtSetArg (args[cnt], XtNy,  &offsety);                     cnt++;
  XtGetValues(XtNameToWidget(toplevel, "*display"), args, cnt);

  startx += player->x + offsetx;
  starty += player->y + offsety;

  /* create a popup window */
  cnt = 0;
  width  =    player->Images1[player->moves[move].frameindex[0]].width;  
  height =    player->Images1[player->moves[move].frameindex[0]].height;

  XtSetArg (args[cnt], XtNoverrideRedirect, True);          cnt++;
  XtSetArg (args[cnt], XtNx,  startx);                      cnt++;
  XtSetArg (args[cnt], XtNy,  starty);                      cnt++;
  XtSetArg (args[cnt], XtNwidth,  width);                   cnt++;
  XtSetArg (args[cnt], XtNheight,  height);                 cnt++;

  *shape = XtAppCreateShell ("shape", CLASS, transientShellWidgetClass,
			     display, args, cnt);

  XtRealizeWidget(*shape);
  Update(glob);

  x = y = 0;
  for (parent = *shape; XtParent(parent); parent = XtParent(parent)) {
    x += parent->core.x + parent->core.border_width;
    y += parent->core.y + parent->core.border_width;
  }

  XShapeCombineMask(display, XtWindow(parent), ShapeBounding, x, y, 
		    mask, ShapeSet);

  /* draw image */
  XSetClipMask (display, gc, mask);
  XSetClipOrigin(display, gc, 0, 0);
  XCopyPlane (display, pm, XtWindow(*shape), gc, 0, 0, width, height, 0, 0, 1);

}

MoveShape(glob, display, player, shape)
Glob  *glob;
Display *display;
Player *player;
Widget *shape;
{
  int i;
  Dimension x, y;
  int cnt;
  Arg args[15];

  if (*shape == NULL) return;

  cnt = 0;
  XtSetArg (args[cnt], XtNx,  &x);                         cnt++;
  XtSetArg (args[cnt], XtNy,  &y);                         cnt++;
  XtGetValues(*shape, args, cnt);

  if (player->facing == RIGHT) {
    /* faces right, flies left */
    if (x > 0) {
      XtMoveWidget(*shape, x - 100, y);
    } else {
      XtDestroyWidget(*shape);
      *shape = NULL;
    }
  } else {
    /* faces left, flies right */
    if (x < 1000) {
      XtMoveWidget(*shape, x + 100, y);
    } else {
      XtDestroyWidget(*shape);
      *shape = NULL;
    }
  }
}
