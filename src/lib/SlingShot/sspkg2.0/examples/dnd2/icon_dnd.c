/*
 * icon_dnd.c
 *
 * SlingShot 2.0 drag-n-drop example.
 *
 * This example shows how file manager objects (files) can be dragged and
 * dropped, between different deskset applikations. Files are represented
 * using file manager alike icons, defined using the Classing Engine (CE)
 * interface provided in cei.h/cei.c.
 *
 * Janne (janne@torpa.se)
 *
 *	@(#) icon_dnd.c 1.3 92/10/27 
 */
#include <stdlib.h>
#include <sys/param.h>		       /* MAXPATHLEN */
#include <sys/stat.h>
#include <fcntl.h>

#ifdef SVR4
#include <netdb.h>
#endif

#include <xview/xview.h>
#include <xview/scrollbar.h>
#include <xview/sel_pkg.h>
#include <xview/dragdrop.h>
#include <xview/icon_load.h>
#include <xview/cursor.h>

#include <sspkg/canshell.h>
#include <sspkg/rectobj.h>
#include <sspkg/drawobj.h>
#include <sspkg/array.h>
#include <sspkg/box.h>

#include "cei.h"

/*
 * To get debug information "#define DEBUG".
 */
#ifdef DEBUG
#define DPRINTF(x) printf x
#else
#define DPRINTF(x)		       /* nothing */
#endif

/*
 * Window objects.
 */
static Xv_Server server;
static Frame    frame;
static Canvas_shell shell;
static Array_tile atile;

/*
 * KEY_DATA - used to store path to file
 */
static int      path_key_data;
#define PATH_KEY_DATA XV_KEY_DATA, path_key_data

/*
 * Drag-n-drop stuff.
 */
static Dnd      dnd;
Selection_requestor sel_req;
static char    *dragged_file;

/*
 * Function forward deklarations.
 */
static void     create_icon();
static void     set_footer();
static void     drop_proc();
static int      drag_selection_convert();
static Server_image path2Image();

main(argc, argv)
  int             argc;
  char           *argv[];
{
  Scrollbar       vscroll;
  Scrollbar       hscroll;
  char            msg[1024];

  server = xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

  frame = (Frame) xv_create(NULL, FRAME,
    FRAME_LABEL, argv[0],
    FRAME_SHOW_FOOTER, TRUE,
    NULL);

  shell = (Canvas_shell) xv_create(frame, CANVAS_SHELL,
    CANVAS_SHELL_AUTO_DROP_SITE, TRUE,
    XV_WIDTH, 300,
    XV_HEIGHT, 200,
    RECTOBJ_DROP_PROC, drop_proc,
    RECTOBJ_ACCEPTS_DROP, TRUE,
    NULL);

  vscroll = (Scrollbar) xv_create(shell, SCROLLBAR,
    SCROLLBAR_DIRECTION, SCROLLBAR_VERTICAL,
    SCROLLBAR_SPLITTABLE, TRUE,
    SCROLLBAR_PIXELS_PER_UNIT, 10,
    NULL);

  hscroll = (Scrollbar) xv_create(shell, SCROLLBAR,
    SCROLLBAR_DIRECTION, SCROLLBAR_HORIZONTAL,
    SCROLLBAR_SPLITTABLE, TRUE,
    SCROLLBAR_PIXELS_PER_UNIT, 10,
    NULL);

  atile = (Array_tile) xv_create(shell, ARRAY_TILE,
    ARRAY_TILE_N_COLUMNS, 4,
    ARRAY_TILE_COLUMN_GAP, 4,
    ARRAY_TILE_ROW_GAP, 4,
    RECTOBJ_DROP_PROC, drop_proc,
    RECTOBJ_ACCEPTS_DROP, TRUE,
    NULL);

  /*
   * create unique XV_KEY_DATA key
   */
  path_key_data = xv_unique_key();

  /*
   * open the Classing Engine database
   */
  if (cei_open(msg) != 0) {
    fprintf(stderr, msg);
    exit(4);
  }

  /*
   * Create DND object to be used for drag operations.
   */
  dnd = xv_create(canvas_paint_window(shell), DRAGDROP,
    DND_TYPE, DND_COPY,
    SEL_CONVERT_PROC, drag_selection_convert,
    NULL);

  /*
   * Create selection requestor to get selectons (drop operations).
   */
  sel_req = xv_create(shell, SELECTION_REQUESTOR, NULL);

  window_fit(frame);
  set_footer("Drag a file from the file manager!");
  xv_main_loop(frame);
  cei_close();
}

/*
 * Set footer message.
 */
static void
set_footer(str)
  char           *str;
{
  xv_set(frame,
    FRAME_LEFT_FOOTER, str,
    NULL);
}

/*
 * Create file icon.
 */
static void
create_icon(path)
  char           *path;
{
  Cei_Attributes  attr;
  char            cei_msg[1024];
  char           *name;
  static void     start_drag();

  /*
   * get name (skip path)
   */
  name = strrchr(path, '/');
  if (name)
    name++;
  else
    name = path;

  /*
   * get CE attributes and create icon object. Note that the full path name is
   * saved as client key data.
   */
  if (cei_get_attr(path, &attr, cei_msg) == 0) {
    xv_create(atile, DRAWICON,
      DRAWTEXT_STRING, strdup(name),
      DRAWIMAGE_IMAGE1, path2Image(attr.icon),
      DRAWIMAGE_IMAGE1_MASK, path2Image(attr.icon_mask),
      DRAWIMAGE_IMAGE2, path2Image(attr.icon),
      DRAWIMAGE_IMAGE2_MASK, path2Image(attr.icon_mask),
      PATH_KEY_DATA, strdup(path),
      RECTOBJ_DRAGGABLE, TRUE,
      RECTOBJ_START_DRAG_PROC, start_drag,
      NULL);

    /*
     * In case the array grew bigger than the paint window, set the paint
     * window size.  This way, the scrollbars will be properly set.
     */
    xv_set(shell,
      CANVAS_MIN_PAINT_WIDTH, xv_get(atile, XV_WIDTH),
      CANVAS_MIN_PAINT_HEIGHT, xv_get(atile, XV_HEIGHT),
      NULL);
  }
  else

    /*
     * CE operation failed!
     */
    set_footer(cei_msg);
}

/*
 * Function called when something is dropped on the Canvas.
 */
static void
drop_proc(paint_window, event, canvas_shell, drawtext)
  Xv_window       paint_window;
  Event          *event;
  Canvas_shell    canvas_shell;
  Drawtext        drawtext;
{
  static void     get_drop();

  switch (event_action(event)) {
  case ACTION_DRAG_PREVIEW:
    break;
  case ACTION_DRAG_COPY:
  case ACTION_DRAG_MOVE:
  case ACTION_DRAG_LOAD:
    if (dnd_is_local(event)) {
      DPRINTF(("Icon droppped on myself! Ignored.\n"));
      dnd_done(sel_req);
      return;
    }
    get_drop(event);
    break;
  }
}

/*
 * Get the drop message. We use the atom FILE_NAME to get the name
 * of the dropped file icon.
 */
static void
get_drop(event)
  Event          *event;
{
  Atom           *list = NULL;
  char           *name;
  int             length, format;

  DPRINTF(("get_drop: decoding drop.\n"));
  if (dnd_decode_drop(sel_req, event) != XV_ERROR) {

    /*
     * Use the FILE_NAME atom to get file name.
     */
    xv_set(sel_req,
      SEL_TYPE, (Atom) xv_get(server, SERVER_ATOM,
	"FILE_NAME"),
      NULL);
    name = (char *) xv_get(sel_req, SEL_DATA, &length, &format);
    if (length != SEL_ERROR) {
      DPRINTF(("get_drop: got FILE_NAME: %s.\n", name));
      set_footer("Drop completed");
      create_icon(name);
      free(name);
    }
    else
      set_footer("Drop completed");
    dnd_done(sel_req);
  }
  else
    set_footer("Drop completed");
}

/*
 * Start drag operation.
 */
static void
start_drag(paint_window, event, canvas_shell, rectobj, x, y, adjust)
  Xv_window       paint_window;
  Event          *event;
  Canvas_shell    canvas_shell;
  Rectobj         rectobj;
  int             x, y, adjust;
{
  Xv_Cursor       drag_cursor;

  set_footer("Drag and Drop: Started");

  /* workaround for alpha bug */
  rectobj_set_event_grab(canvas_shell, 0, 0, 0);

  dragged_file = (char *) xv_get(rectobj, PATH_KEY_DATA);
  DPRINTF(("dragging: %s \n", dragged_file));

  drag_cursor = xv_create(XV_NULL, CURSOR,
    CURSOR_IMAGE, xv_get(rectobj, DRAWIMAGE_IMAGE1),
    NULL);
  xv_set(dnd,
    DND_CURSOR, drag_cursor,
    NULL);
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
  xv_destroy_safe(drag_cursor);

  /*
   * de-select icon.
   */
  xv_set(rectobj,
    RECTOBJ_SELECTED, FALSE,
    RECTOBJ_NORMAL,		       /* workaround for alpha bug */
    NULL);
}

/*
 * Huge Selection convert routine. Most of the messages used by the deskset is
 * implemented, but not all of them. For are "real" application I suggest
 * that you take a look in the drag and drop support routines provided with
 * Devguide 3.0. 
 */
static int
drag_selection_convert(seln, type, data, length, format)
  Selection_owner seln;
  Atom           *type;
  Xv_opaque      *data;
  unsigned long  *length;
  int            *format;
{
  static Atom     target_list[5];
  int             length_buf;
  static char    *incr_tmp = (char *) NULL;
  static int      incr_first = 1;
  static int      incr_bytes = 0;
  static int      incr_fd;

#ifdef DEBUG
  char           *atom_name;
  if (type != NULL)
    atom_name = XGetAtomName((Display *) xv_get(server, XV_DISPLAY), *type);
  else
    atom_name = "[None]";
  printf("drag_selection_convert: being asked to convert %s\n", atom_name);
#endif

  if (*type == (Atom) xv_get(server, SERVER_ATOM, "_SUN_SELECTION_END")||
      *type == (Atom) xv_get(server, SERVER_ATOM, "_SUN_DRAGDROP_DONE"))

    /*
     * Destination has told us it has completed the drag and drop transaction.
     * display message and let sel_convert_proc() return the correct reply.
     */
    set_footer("Drag and Drop: Completed");

  else if (*type == (Atom) xv_get(server, SERVER_ATOM,
      "TARGETS")) {

    /*
     * This request should return all of the targets that can be converted.
     * Required by ICCCM.
     */
    DPRINTF(("drag_selection_convert: returning targets\n"));
    *format = 32;
    *length = 0;
    *type = XA_ATOM;
    target_list[(*length)++] = XA_STRING;
    target_list[(*length)++] = (Atom) xv_get(server, SERVER_ATOM,
      "TARGETS");
    target_list[(*length)++] = (Atom) xv_get(server, SERVER_ATOM,
      "_SUN_FILE_HOST_NAME");
    target_list[(*length)++] = (Atom) xv_get(server, SERVER_ATOM,
      "_SUN_ALTERNATE_TRANSPORT_METHODS");
    *data = (Xv_opaque) target_list;
    return (TRUE);
  }

  else if (*type == (Atom) xv_get(server, SERVER_ATOM,
      "_SUN_ALTERNATE_TRANSPORT_METHODS")) {

    /*
     * Return alternate transport methods.
     */
    DPRINTF(("drag_selection_convert: returning atm\n"));
    *format = 32;
    *length = 1;
    target_list[0] = (Atom) xv_get(server, SERVER_ATOM,
      "_SUN_ATM_FILE_NAME");
    *data = (Xv_opaque) target_list;
    return (True);
  }

  else if (*type == (Atom) xv_get(server, SERVER_ATOM,
      "_SUN_ENUMERATION_COUNT")) {

    /*
     * We always return 1 (number of drop operations)
     */
    DPRINTF(("drag_selection_convert: returning count (1)\n"));
    length_buf = 1;
    *format = 32;
    *length = 1;
    *type = XA_INTEGER;
    *data = (Xv_opaque) & length_buf;
    return (True);
  }

  else if (*type == (Atom) xv_get(server, SERVER_ATOM,
      "_SUN_FILE_HOST_NAME")) {

    /*
     * Return the hostname that the application is running on
     */
    char            hostname[MAXHOSTNAMELEN];
#ifdef SVR4
    strcpy(hostname, gethostent()->h_name);
#else
    gethostname(hostname, MAXHOSTNAMELEN);
#endif
    DPRINTF(("drag_selection_convert: returning host name (%s)\n",
	hostname));
    *format = 8;
    *length = strlen(hostname);
    *data = (Xv_opaque) hostname;
    return (TRUE);
  }

  else if (*type == (Atom) xv_get(server, SERVER_ATOM,
      "FILE_NAME")) {

    /*
     * Return file name of file being dragged.
     */
    DPRINTF(("drag_selection_convert: returning file name (%s)\n", dragged_file));
    *format = 8;
    *length = strlen(dragged_file);
    *data = (Xv_opaque) dragged_file;
    *type = XA_STRING;
    return (TRUE);
  }
  else if (*type == (Atom) xv_get(server, SERVER_ATOM,
      "_SUN_DATA_LABEL")) {

    /*
     * Return label. Label in our case is the name of the file.
     */
    char           *name;

    /*
     * get file name (skip path)
     */
    name = strrchr(dragged_file, '/');
    if (name)
      name++;
    else
      name = dragged_file;

    DPRINTF(("drag_selection_convert: returning label (%s)\n", name));
    *format = 8;
    *length = strlen(name);
    *data = (Xv_opaque) name;
    *type = XA_STRING;
    return (TRUE);
  }

  else if (*type == XA_STRING) {
    /*
     * String data (used by some deskset toold to get file contents)
     *
     * if first call, initiate incremental sending of data.
     */
    if (incr_first) {
      static long file_size;
      struct stat buf;
      incr_fd = open(dragged_file, O_RDONLY);
      if (incr_fd < 0) {
	set_footer("Drag and drop operation failed.");
	return(False);
      }
      fstat(incr_fd, &buf);
      file_size = buf.st_size;
      DPRINTF(("drag_selection_convert: increment reply\n"));
      DPRINTF(("drag_selection_convert: sending file (%s) size (%d)\n",
	       dragged_file,file_size));
      *type = (Atom)xv_get(server, SERVER_ATOM, "INCR");
      *data = (Xv_opaque) &file_size;
      *format = 32;
      *length = 1;
      incr_first = 0;
      return(True);
    }

    if (incr_tmp != NULL)
      free(incr_tmp);
    incr_tmp = (char*) malloc(BUFSIZ);

    if ((incr_bytes = read(incr_fd, incr_tmp, BUFSIZ)) == -1) {
      set_footer("Drag and drop operation failed.");
      return(False);
    }

    if (incr_bytes == 0) {
      free(incr_tmp);
      incr_tmp = NULL;
      close(incr_fd);
      incr_first = 1;
    }

    DPRINTF(("drag_selection_convert: returning XA_STRING. N bytes: %d\n",
	     incr_bytes));
    *format = 8;
    *length = incr_bytes;
    *type = XA_STRING;
    *data = (Xv_opaque) incr_tmp;
    return(True);
  }
  
  return (sel_convert_proc(seln, type, data, length, format));
}

/*
 * return server image, specified by path.
 */
static          Server_image
path2Image(path)
  char           *path;
{
  Server_image    image;
  char            load_msg[IL_ERRORMSG_SIZE];
  char            load_path[MAXPATHLEN];

  /*
   * expand path (it contains env. variables) Note: uses undocumented XView
   * function 'expand_path()'
   */
  expand_path(path, load_path);
  
  /*
   * To create the server image we use an other useful function. This one
   * "documented" in the header file: xview/icon_load.h.
   */
  image = icon_load_svrim(load_path, load_msg);
  if (!image) {
    set_footer("Image path not found");
    fprintf(stderr, "Can not load image from file: %s\n%s\n",
      load_path, load_msg);
    return (Server_image) NULL;
  }


}
