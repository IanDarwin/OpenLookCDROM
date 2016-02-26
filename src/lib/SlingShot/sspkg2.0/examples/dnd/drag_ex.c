/*
 * drag_ex.c
 *
 * SlingShot 2.0 drag-n-drop example to be used with drop_ex.c.
 * 
 * janne (janne@torpa.se)
 *
 *      @(#) drag_ex.c 1.1 92/10/21 
 */
#include <xview/frame.h>
#include <xview/font.h>
#include <xview/cms.h>
#include <xview/dragdrop.h>

#include <sspkg/canshell.h>
#include <sspkg/rectobj.h>
#include <sspkg/drawobj.h>
#include <sspkg/array.h>
#include <sspkg/box.h>

static Frame    frame;
static Dnd      dnd;
static Selection_item sel_item;

main(argc, argv)
  int             argc;
  char           *argv[];
{
  Canvas_shell    shell;
  Box             box;
  Array_tile      atile;
  Drawtext        drawtext1;
  Drawtext        drawtext2;
  Drawtext        drawtext3;

  void            start_drag();
  int             selection_convert();
  void            selection_lose();

  xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

  frame = (Frame) xv_create(NULL, FRAME,
    FRAME_LABEL, argv[0],
    FRAME_SHOW_FOOTER, TRUE,
    NULL);

  shell = (Canvas_shell) xv_create(frame, CANVAS_SHELL,
    CANVAS_SHELL_AUTO_DROP_SITE, TRUE,
    NULL);

  box = (Box) xv_create(shell, BOX,
    BOX_LAYOUT, BOX_LAYOUT_VERTICAL,
    RECTOBJ_BORDER, 10,
    NULL);

  atile = (Array_tile) xv_create(box, ARRAY_TILE,
    ARRAY_TILE_N_COLUMNS, 3,
    ARRAY_TILE_COLUMN_GAP, 40,
    ARRAY_TILE_ALIGN, ARRAY_TILE_ALIGN_WEST,
    ARRAY_TILE_VLINES, TRUE,
    ARRAY_TILE_HLINES, TRUE,
    NULL);

  drawtext1 = (Drawtext) xv_create(atile, DRAWTEXT,
    DRAWTEXT_STRING, "Drag Text 1",
    RECTOBJ_DRAGGABLE, TRUE,
    RECTOBJ_START_DRAG_PROC, start_drag,
    NULL);

  drawtext2 = (Drawtext) xv_create(atile, DRAWTEXT,
    DRAWTEXT_STRING, "Drag Text 2",
    XV_Y, 30,
    RECTOBJ_DRAGGABLE, TRUE,
    RECTOBJ_START_DRAG_PROC, start_drag,
    NULL);

  drawtext3 = (Drawtext) xv_create(atile, DRAWTEXT,
    DRAWTEXT_STRING, "Drag Text 3",
    XV_Y, 60,
    RECTOBJ_DRAGGABLE, TRUE,
    RECTOBJ_START_DRAG_PROC, start_drag,
    NULL);

  dnd = xv_create(canvas_paint_window(shell), DRAGDROP,
    DND_TYPE, DND_COPY,
    SEL_CONVERT_PROC, selection_convert,
    0);

  sel_item = xv_create(dnd, SELECTION_ITEM,
    0);

  /* size the canvas to the size of enclosed objects */
  xv_set(shell,
    XV_WIDTH, xv_get(box, XV_WIDTH),
    XV_HEIGHT, xv_get(box, XV_HEIGHT),
    NULL);

  window_fit(frame);

  xv_main_loop(frame);
}

void
set_footer(str)
     char           *str;
{
  xv_set(frame,
    FRAME_LEFT_FOOTER, str,
    NULL);
}

/*
 * Start drag operation. The selecion data is picked up from the label of the
 * drawtext object being dragged.
 */
void
start_drag(paint_window, event, canvas_shell, rectobj, x, y, adjust)
     Xv_window       paint_window;
     Event          *event;
     Canvas_shell    canvas_shell;
     Rectobj         rectobj;
     int             x, y, adjust;
{
  set_footer("Drag and Drop: Started");
  xv_set(sel_item,
    SEL_TYPE, (Atom) XA_STRING,
    SEL_DATA, (Xv_opaque) xv_get(rectobj, DRAWTEXT_STRING),
    0);
  switch (dnd_send_drop(dnd)) {
  case XV_OK:
    break;			       /* ok, no (more) message */
  case DND_ABORTED:
    set_footer("Drag and Drop: Aborted");
    break;
  case DND_TIMEOUT:
    set_footer("Drag and Drop: Timed Out");
    break;
  case DND_ROOT:
  case DND_ILLEGAL_TARGET:
    set_footer("Drag and Drop: Illegal Target");
    break;
  case DND_SELECTION:
    set_footer("Drag and Drop: Bad Selection");
    break;
  case XV_ERROR:
    set_footer("Drag and Drop: Failed");
    break;
  }
}

/*
 * In this example no real conversion is done. We just want to find out
 * when the drag is completed.
 */
int
selection_convert(seln, type, data, length, format)
     Selection_owner seln;
     Atom           *type;
     Xv_opaque      *data;
     unsigned long  *length;
     int            *format;
{
  Xv_Server       server = XV_SERVER_FROM_WINDOW(xv_get(seln, XV_OWNER));
  
  if (*type == (Atom) xv_get(server, SERVER_ATOM, "_SUN_DRAGDROP_DONE"))
    /* Destination has told us it has completed the drag and drop transaction. */
    set_footer("Drag and Drop: Completed");
  
  return (sel_convert_proc(seln, type, data, length, format));
}


