#include <math.h>
#include <sys/types.h>
#include <sys/timeb.h>
#include <sys/time.h>
#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/frame.h>
#include <xview/canvas.h>
#include <xview/notice.h>
#include <xview/cms.h>
#include <xview/font.h>

#include "xvmines.h"
#include "version.h"

#include "xvmines.icons"

#define CREATE_ICON(bits,width,height) \
				XCreateBitmapFromData(display,RootWindow(display,screen),\
					bits,width,height)
/* #define DEBUG */

#ifndef lint
static char     sccsid[] = "@(#)xvmines,v0.2 by Manolis Lourakis.";
static char     Copyright[] = "(c) 1992 Manolis Lourakis.\n";
static char     Mail[] = "lourakis@csd.uch.gr";
#endif

#define T_FONT_NAME "-adobe-*-*-*-normal-*-16-*-*-*-*-*-*-*"


extern char    *optarg;
extern int      optind;

#ifdef __STDC__
void            canvas_event_proc(Xv_Window, Event *);
void            canvas_repaint_proc(void);
void            finish(char *);
void            print_string(char *, int, int, int);
#else
void            canvas_event_proc();
void            canvas_repaint_proc();
void            finish();
void            print_string();
#endif


Display        *display;
Visual         *visual;
int             screen;
GC              def_gc;

Frame           frame;
Panel           panel1;
Panel_item      opt;
Canvas          canvas, msg_canvas;

Window          dr_win;
Pixmap          pmap, icons[NUM_ICONS];
cell           *Cells;
char            buff[256];

Notify_func     active_func = NOTIFY_FUNC_NULL;

int             quit_proc(), init(), restart(), hide_cmd_frame();

#ifdef __STDC__
int             adj_mine_num(Panel_item, Event *), adj_size(Panel_item, Event *),
                show_cmd_frame(Menu, Menu_item);
int             place_tiles(int, int, int),
                mark_tile(int, int),
                open_tile(int, int),
                auto_open(int, int);
Frame           create_cmd_frame(Frame, char *, int (*notifier) ());
#else
int             adj_mine_num(), adj_size(), show_cmd_frame();
int             place_tiles(), mark_tile(), open_tile(), auto_open();
Frame           create_cmd_frame();
#endif

int             mines_2_go, closed_tiles, playing, first, checks, secs;
int             first_tile;
int             width = HORIZ_CELLS, height = VERT_CELLS, mines = NUM_MINES,
                quantum = QUANTUM;

struct itimerval timer;

#ifdef __STDC__
int
main(int argc, char *argv[])
#else
int
main(argc, argv)
	int             argc;
	char           *argv[];
#endif
{
	int             ret, tmp;
	register int    i, j;
	int             errflag = 0, badparam = 0, gc;
	XWMHints        wm_hints;
	XClassHint      class_hints;
	XTextProperty   windowName, iconName;
	char           *window_name = version_string;
	char           *icon_name = version_string;
	Menu            menu;
	Menu_item       entry1, entry2, entry3;
	Frame           aux_fr;
	Cms             cms;

	xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);
	while ((gc = getopt(argc, argv, "w:h:m:q:")) != EOF)
		switch (gc) {
		case 'w':
			tmp = atoi(optarg);
			if (tmp > 0 && tmp <= MAX_WIDTH)
				width = tmp;
			else
				badparam++;
			break;
		case 'h':
			tmp = atoi(optarg);
			if (tmp > 0 && tmp <= MAX_HEIGHT)
				height = tmp;
			else
				badparam++;
			break;
		case 'm':
			tmp = atoi(optarg);
			if (tmp > 0)
				mines = tmp;
			else
				badparam++;
			break;
		case 'q':
			tmp = atoi(optarg);
			if (tmp > 0)
				quantum = tmp;
			else
				badparam++;
			break;
		case '?':
			errflag++;
			break;
		}
	if (badparam) {
		fprintf(stderr,
			"xvmines: Bad argument(s).\n");
		exit(1);
	}
	if (errflag) {
		fprintf(stderr,
			"Usage: xvmines [-w width] [-h height] [-m mines] [-q quantum]\n");
		exit(1);
	}
	if (mines > width * height) {
		fprintf(stderr, "xvmines: Too many mines, using defaults\n");
		width = HORIZ_CELLS;
		height = VERT_CELLS;
		mines = NUM_MINES;
	}
	Cells = (cell *) malloc(width * height * sizeof(cell));
	if (Cells == NULL) {
		fprintf(stderr, "xvmines: malloc failed\n");
		exit(1);
	}
	cms = xv_create(NULL, CMS,
			CMS_SIZE, CMS_CONTROL_COLORS + 1,
			CMS_CONTROL_CMS, True,
			CMS_NAMED_COLORS, "red", NULL,
			NULL);

	frame = (Frame) xv_create(NULL, FRAME,
				  FRAME_LABEL, version_string,
				  XV_WIDTH, width * IC_WIDTH,
				  WIN_CMS, cms,
				  FRAME_SHOW_FOOTER, TRUE,
				  FRAME_LEFT_FOOTER, "",
				  FRAME_RIGHT_FOOTER, "",
				  NULL);
	display = (Display *) xv_get(frame, XV_DISPLAY);
	screen = DefaultScreen(display);
	visual = DefaultVisual(display, screen);
	def_gc = DefaultGC(display, screen);

	panel1 = xv_create(frame, PANEL,
			   PANEL_LAYOUT, PANEL_HORIZONTAL,
			   NULL);

	xv_create(panel1, PANEL_BUTTON,
		  PANEL_LABEL_STRING, "Quit",
		  PANEL_NOTIFY_PROC, quit_proc,
		  NULL);
	xv_create(panel1, PANEL_BUTTON,
		  PANEL_LABEL_STRING, "New",
		  PANEL_NOTIFY_PROC, restart,
		  NULL);
	aux_fr = create_cmd_frame(frame, "New number of mines: ", adj_mine_num);
	entry1 = (Menu_item) xv_create((unsigned long) NULL, MENUITEM,
				       MENU_STRING, "Mines",
				       XV_KEY_DATA, KEY, aux_fr,
				       MENU_NOTIFY_PROC, show_cmd_frame,
				       NULL);
	aux_fr = create_cmd_frame(frame, "New dimensions (wxh): ", adj_size);
	entry2 = (Menu_item) xv_create((unsigned long) NULL, MENUITEM,
				       MENU_STRING, "Dimensions",
				       XV_KEY_DATA, KEY, aux_fr,
				       MENU_NOTIFY_PROC, show_cmd_frame,
				       NULL);
	menu = (Menu) xv_create((unsigned long) NULL, MENU,
				MENU_APPEND_ITEM, entry1,
				MENU_APPEND_ITEM, entry2,
				NULL);

	opt = xv_create(panel1, PANEL_BUTTON,
			PANEL_LABEL_STRING, "Options",
			PANEL_ITEM_MENU, menu,
			NULL);

	window_fit_height(panel1);
	msg_canvas = xv_create(frame, CANVAS,
			       XV_HEIGHT, MSG_CANVAS_HEIGHT,
			       NULL);

	sprintf(buff, "Mines: %d\n", mines);
	xv_set(frame, FRAME_LEFT_FOOTER, buff, NULL);
	sprintf(buff, "Checks: 0");
	xv_set(frame, FRAME_RIGHT_FOOTER, buff, NULL);
	/* window_fit_height(panel2); */

	canvas = xv_create(frame, CANVAS,
			   CANVAS_X_PAINT_WINDOW, TRUE,
			   XV_WIDTH, IC_WIDTH * width,
			   XV_HEIGHT, IC_HEIGHT * height,
			   WIN_EVENT_PROC, canvas_event_proc,
			   CANVAS_REPAINT_PROC, canvas_repaint_proc,
			   NULL);
	xv_set(canvas_paint_window(canvas),
	       WIN_CONSUME_X_EVENT_MASK,
	       ButtonPressMask | ButtonReleaseMask |
	       EnterWindowMask | LeaveWindowMask,
	       WIN_EVENT_PROC, canvas_event_proc,
	       NULL);

	dr_win = xv_get(canvas_paint_window(canvas), XV_XID);

	pmap = XCreatePixmap(display, dr_win, IC_WIDTH * width,
			 IC_HEIGHT * height, DefaultDepth(display, screen));

	icons[EMPTY] = CREATE_ICON(empty_bits, empty_width, empty_height);
	icons[ONE] = CREATE_ICON(one_bits, one_width, one_height);
	icons[TWO] = CREATE_ICON(two_bits, two_width, two_height);
	icons[THREE] = CREATE_ICON(three_bits, three_width, three_height);
	icons[FOUR] = CREATE_ICON(four_bits, four_width, four_height);
	icons[FIVE] = CREATE_ICON(five_bits, five_width, five_height);
	icons[SIX] = CREATE_ICON(six_bits, six_width, six_height);
	icons[SEVEN] = CREATE_ICON(seven_bits, seven_width, seven_height);
	icons[EIGHT] = CREATE_ICON(eight_bits, eight_width, eight_height);
	icons[CLOSED] = CREATE_ICON(closed_bits, closed_width, closed_height);
	icons[MARKED] = CREATE_ICON(marked_bits, marked_width, marked_height);
	icons[MINE] = CREATE_ICON(mine_bits, mine_width, mine_height);
	icons[QUEST] = CREATE_ICON(quest_bits, quest_width, quest_height);

	if (XStringListToTextProperty(&window_name, 1, &windowName) == 0) {
		fprintf(stderr, "structure allocation for windowName failed\n");
		exit(1);
	}
	if (XStringListToTextProperty(&icon_name, 1, &iconName) == 0) {
		fprintf(stderr, "structure allocation for windowName failed\n");
		exit(1);
	}
	wm_hints.initial_state = NormalState;
	wm_hints.input = True;
	wm_hints.icon_pixmap = icons[MINE];
	wm_hints.flags = StateHint | IconPixmapHint | InputHint;

	class_hints.res_name = "XVmines";
	class_hints.res_class = "Games";

	XSetWMProperties(display, xv_get(frame, XV_XID), &windowName, &iconName, argv,
			 argc, NULL, &wm_hints, &class_hints);



	timer.it_interval.tv_sec = quantum;
	timer.it_interval.tv_usec = 0;

	timer.it_value.tv_sec = quantum;
	timer.it_value.tv_usec = 0;

	init();
	window_fit(frame);
	xv_main_loop(frame);
}


void
canvas_repaint_proc()
{
	XCopyArea(display, pmap, dr_win, def_gc, 0, 0,
		  width * IC_WIDTH, height * IC_HEIGHT, 0, 0);
}

#ifdef __STDC__
void
canvas_event_proc(Xv_Window window, Event * event)
#else
void
canvas_event_proc(window, event)
	Xv_Window       window;
	Event          *event;
#endif
{
	int             x, y, which;
	cell           *aux;

#ifdef DEBUG
	printf("Mines to go %d,checks %d closed %d\n", mines_2_go, checks, closed_tiles);
#endif

	if (!playing)
		return;
	switch (event_xevent(event)->type) {
	case ButtonRelease:
		get_cell(event_xevent(event)->xbutton.x, event_xevent(event)->xbutton.y,
			 &x, &y);
		if (first) {
			place_tiles(width * height, mines, x * width + y);
			xv_set(opt, PANEL_INACTIVE, TRUE, NULL);
			print_string("Time->> 0:0", TIME_X, TIME_Y, CLEAR_YES);
			first = FALSE;
		}
		switch (event_xevent(event)->xbutton.button) {
		case Button1:
			if (open_tile(x, y)) {
				XCopyArea(display, pmap, dr_win, def_gc, 0, 0,
				width * IC_WIDTH, height * IC_HEIGHT, 0, 0);
			}
			if (mines_2_go == 0 && closed_tiles == mines)
				finish("  Successful");
			break;
		case Button2:
			aux = Cells + x * width + y;
			if (aux->out_state != QUEST && aux->out_state != CLOSED)
				break;
			which = (aux->out_state == CLOSED) ? QUEST : CLOSED;
			PLACE_ICON(icons[which], x * IC_WIDTH,
				   y * IC_HEIGHT);
			aux->out_state = which;
			XCopyArea(display, pmap, dr_win, def_gc, 0, 0,
				width * IC_WIDTH, height * IC_HEIGHT, 0, 0);
			break;
		case Button3:
			if (mark_tile(x, y)) {
				XCopyArea(display, pmap, dr_win, def_gc, 0, 0,
				width * IC_WIDTH, height * IC_HEIGHT, 0, 0);
			}
			if (mines_2_go == 0 && closed_tiles == mines)
				finish("  Successful");

			break;
		}
		break;
	case EnterNotify:
		notify_set_itimer_func(canvas, active_func, ITIMER_REAL, &timer, NULL);
		break;
	case LeaveNotify:
		notify_set_itimer_func(canvas, NOTIFY_FUNC_NULL, ITIMER_REAL, &timer, NULL);
		break;
	default:
		break;
	}
}

quit_proc()
{
	int             result;

	result = notice_prompt(panel1, NULL,
			       NOTICE_MESSAGE_STRINGS,
			       "Really Quit?",
			       "Press YES to confirm",
			       "Press NO to cancel",
			       NULL,
			       NOTICE_BUTTON_YES, "YES",
			       NOTICE_BUTTON_NO, "NO",
			       NULL);

	switch (result) {
	case NOTICE_YES:
		exit(0);
		break;
	case NOTICE_NO:
		break;
	case NOTICE_FAILED:
	default:
		break;
	}
}

#ifdef __STDC__
int
adj_mine_num(Panel_item item, Event * event)
#else
int
adj_mine_num(item, event)
	Panel_item      item;
	Event          *event;
#endif
{
	mines = atoi((char *) xv_get(item, PANEL_VALUE));
	xv_set((Frame) xv_get(item, PANEL_CLIENT_DATA), XV_SHOW, FALSE, NULL);

	if (mines > width * height || mines <= 0) {
		fprintf(stderr, "xvmines: Invalid number of mines specified, using defaults\n");
		if (NUM_MINES < width * height)
			mines = NUM_MINES;
		else
			mines = width;
	}
	sprintf(buff, "Mines: %d\n", mines);
	xv_set(frame, FRAME_LEFT_FOOTER, buff, NULL);
	init();

}

#ifdef __STDC__
int
adj_size(Panel_item item, Event * event)
#else
int
adj_size(item, event)
	Panel_item      item;
	Event          *event;
#endif
{
	int             frame_height, n;

	n = sscanf((char *) xv_get(item, PANEL_VALUE), "%dx%d", &width, &height);
	xv_set((Frame) xv_get(item, PANEL_CLIENT_DATA), XV_SHOW, FALSE, NULL);
	if (n != 2 || width <= 0 || width > MAX_WIDTH || height <= 0 || height > MAX_HEIGHT) {
		fprintf(stderr, "xvmines: Invalid dimensions specified,using defaults\n");
		width = HORIZ_CELLS;
		height = VERT_CELLS;
	}

	XFreePixmap(display, pmap);
	pmap = XCreatePixmap(display, dr_win, IC_WIDTH * width,
			 IC_HEIGHT * height, DefaultDepth(display, screen));
	frame_height = height * IC_HEIGHT + xv_get(panel1, XV_HEIGHT) +
		xv_get(msg_canvas, XV_HEIGHT);


	xv_set(frame, XV_WIDTH, width * IC_WIDTH,
	       XV_HEIGHT, frame_height,
	       NULL);
	xv_set(canvas, XV_WIDTH, width * IC_WIDTH,
	       XV_HEIGHT, height * IC_HEIGHT,
	       NULL);
	free(Cells);
	Cells = (cell *) malloc(width * height * sizeof(cell));
	if (Cells == NULL) {
		fprintf(stderr, "xvmines: malloc failed\n");
		exit(1);
	}
	init();
}

int
restart()
{
	init();
	xv_set(opt, PANEL_INACTIVE, FALSE, NULL);
}

#ifdef __STDC__
int
hide_cmd_frame(Panel_item item, Event * event)
#else
int
hide_cmd_frame(item, event)
	Panel_item      item;
	Event          *event;
#endif
{
	xv_set((Frame) xv_get(item, PANEL_CLIENT_DATA), XV_SHOW, FALSE, NULL);
}

#ifdef __STDC__
int
show_cmd_frame(Menu menu, Menu_item item)
#else
int
show_cmd_frame(menu, item)
	Menu            menu;
	Menu_item       item;
#endif
{
	xv_set((Frame) xv_get(item, XV_KEY_DATA, KEY), XV_SHOW, TRUE, NULL);
}

#ifdef __STDC__
Frame
create_cmd_frame(Frame parent, char *msg, int (*notifier) ())
#else
Frame
create_cmd_frame(parent, msg, notifier)
	Frame           parent;
	char           *msg;
	int             (*notifier) ();
#endif
{
	Frame           popup;
	Panel           aux_pan;

	popup = xv_create(parent, FRAME_CMD,
			  FRAME_LABEL, "prompt",
			  XV_WIDTH, 250,
			  XV_HEIGHT, 60,
			  XV_X, (int) xv_get(parent, XV_X),
			  XV_Y, (int) xv_get(parent, XV_Y),
			  NULL);

	aux_pan = (Panel) xv_get(popup, FRAME_CMD_PANEL);
	xv_set(aux_pan, PANEL_LAYOUT, PANEL_VERTICAL, NULL);
	xv_create(aux_pan, PANEL_TEXT,
		  PANEL_LABEL_STRING, msg,
		  PANEL_VALUE_DISPLAY_LENGTH, 5,
		  PANEL_VALUE_STORED_LENGTH, 5,
		  PANEL_CLIENT_DATA, popup,
		  PANEL_NOTIFY_PROC, notifier,
		  NULL);
	xv_create(aux_pan, PANEL_BUTTON,
		  PANEL_LABEL_STRING, "Cancel",
		  PANEL_CLIENT_DATA, popup,
		  PANEL_NOTIFY_PROC, hide_cmd_frame,
		  NULL);
	return popup;
}

#ifdef __STDC__
void
print_string(char *to_print, int x, int y, int clr)
#else
void
print_string(to_print, x, y, clr)
	char           *to_print;
	int             x, y, clr;
#endif
{
	static XFontStruct *t_font = NULL;
	static int      letter_width;
	static int      letter_height;
	static Window   win;

	XTextItem       text;

	if (t_font == NULL) {
		t_font = XQueryFont(display, XLoadFont(display, T_FONT_NAME));
		if (t_font == NULL) {
			fprintf(stderr,
			  "xvmines: Failed to load font %s\n", T_FONT_NAME);
			exit(1);
		}
		letter_width = t_font->max_bounds.rbearing - t_font->min_bounds.lbearing;
		letter_height = t_font->max_bounds.ascent + t_font->max_bounds.descent;
		win = (Window) xv_get(canvas_paint_window(msg_canvas), XV_XID);
	}
	text.chars = to_print;
	text.nchars = strlen(to_print);
	text.delta = 0;
	text.font = t_font->fid;
	if (clr == CLEAR_YES)
		XClearWindow(display, win);
	XDrawText(display, win, def_gc, x, y, &text, 1);
}
