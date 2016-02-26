#include "ftptool.h"

#pragma ident   "@(#)dnd.c 1.5     93/05/26"

#ifdef XVIEW3

static int remote_drag;
static int local_drag;

#ifdef USE_PROTOTYPES
void remote_drop(Xv_Window window, Event *event, Selection_requestor sel)
#else
void remote_drop(window, event, sel)
Xv_Window	window;
Event 		*event;
Selection_requestor	sel;
#endif
{
	Xv_drop_site	ds;
	int		length, format;
	static int i;
	int		nitems;
	int		*string_length;
	char	*filename;
	char	*hostname;
	Xv_Server	server = XV_SERVER_FROM_WINDOW(event_window(event));

	if ((ds = dnd_decode_drop(sel, event)) == XV_ERROR) {
		footer_message("Decode drop failed.");
		return;
	}

	if (remote_drag) {
		footer_message("Ignoring drag & drop on same window.");
	} else if (!connected) {
		footer_message("Not connected.");
	} else if (dnd_is_local(event)){
		dowhat = DOPUT;
		notify_stop();
	} else {
		footer_message("Can only handle local drops.");
	}

#ifdef notdef
	xv_set(sel,
		SEL_TYPE_NAME, "_SUN_ENUMERATION_COUNT",
		NULL);

	string_length = (int *)xv_get(sel, SEL_DATA, &length, &format);
	if (length != SEL_ERROR) {
		nitems = *string_length;
		free ((char *)string_length);
	}

	for (i = 0; i < nitems; i++) {

		xv_set(sel,
			SEL_TYPE,
			    xv_get(server, SERVER_ATOM,
				"_SUN_ENUMERATION_ITEM"),
			SEL_TYPE_INDEX, 0,
			SEL_PROP_TYPE, XA_INTEGER,
			SEL_PROP_DATA, &i,
			SEL_PROP_FORMAT, 32,
			SEL_PROP_LENGTH, 1,
			NULL);

		(void) xv_get(sel, SEL_DATA, &length, &format);

		xv_set(sel,
			SEL_TYPE, xv_get(server, SERVER_ATOM, "FILE_NAME"),
			NULL);

		filename = (char *)xv_get(sel, SEL_DATA, &length, &format);
		if (length != SEL_ERROR) {
			free (filename);
		}

		/* XXX */
		/* Care about remote host drops? Probably. */
		xv_set(sel,
			SEL_TYPE, xv_get(server, SERVER_ATOM, "HOST_NAME"),
			NULL);

		hostname = (char *)xv_get(sel, SEL_DATA, &length, &format);
		if (length != SEL_ERROR) {
			free (hostname);
		}

	}
	xv_set(sel,
		SEL_TYPE_NAME, "_SUN_SELECTION_END",
		NULL);

	(void) xv_get(sel, SEL_DATA, &length, &format);
#endif

	dnd_done(sel);

}

static void (*remote_list_event_func)();

#ifdef USE_PROTOTYPES
void get_remote_list_event_proc(void)
#else
void get_remote_list_event_proc()
#endif
{
	remote_list_event_func = (void (*)())xv_get(base_window.list,
		PANEL_EVENT_PROC);
	if (remote_list_event_func == (void (*)())NULL) {
		fprintf(stderr,
		    "ftptool: could not get default list eventproc!\n");
		exit(1);
	}
}

#ifdef USE_PROTOTYPES
void remote_list_event(Panel_item item, Event *event)
#else
void remote_list_event(item, event)
Panel_item	item;
Event 		*event;
#endif
{
	short	mask;
	static int drag_pixels;

	switch (event_action(event)) {
	case ACTION_SELECT:
		if (!event_is_down(event)) {
			drag_pixels = 0;
		}
		break;
	case ACTION_ADJUST:
		/*
		 * XXX: translate ACTION_ADJUST to ACTION_SELECT,
		 * so they can still use ADJUST to drag-select items
		 * on the list.
		 */
		event_set_action(event, ACTION_SELECT);
		break;
	case LOC_DRAG:
		/*
		 * XXX: translate ACTION_ADJUST to ACTION_SELECT,
		 * so they can still use ADJUST to drag-select
		 * items on the list.
		 */
		if (action_adjust_is_down(event)) {
			mask = event_shiftmask(event);
			mask &= ~MS_MIDDLE_MASK;
			mask |= MS_LEFT_MASK;
			event_set_shiftmask(event, mask);
			drag_pixels = 0;
		} else if (action_select_is_down(event) &&
		    (event_x(event) != 0) && (event_y(event) != 0)) {
			/*
			 * clicks on the scrollbar, which should _not source a
			 * drag, have x and y as 0.
			 */
			if (connected &&
			    (remote_list_nfiles + remote_list_ndirs +
			    remote_list_nothers) &&
			    (drag_pixels++ >= drag_threshold)) {
				/* source the drag */
				remote_drag = 1;
				switch (dnd_send_drop(base_window.dnd)) {
				case XV_OK:
					break;
				case DND_TIMEOUT:
					footer_message(
					    "Drag and Drop: Timed out.");
					break;
				case DND_ILLEGAL_TARGET:
					footer_message(
					    "Drag and Drop: Illegal target.");
					break;
				case DND_SELECTION:
					footer_message(
					    "Drag and Drop: Bad selection.");
					break;
				case DND_ROOT:
					dowhat = DOREMOTEVIEW;
					notify_stop();
					break;
				case XV_ERROR:
					footer_message(
					    "Drag and Drop: Failed.");
					break;
				}
				remote_drag = 0;
				drag_pixels = 0;
				return;
			}
		} else {
			drag_pixels = 0;
		}
		break;
	default:
		break;
	}
	(*remote_list_event_func)(item, event);
}

#ifdef USE_PROTOTYPES
void local_drop(Xv_Window window, Event *event, Selection_requestor sel)
#else
void local_drop(window, event, sel)
Xv_Window	window;
Event 		*event;
Selection_requestor	sel;
#endif
{
	Xv_drop_site	ds;
	int		length, format;
	static int i;
	int		nitems;
	int		*string_length;
	char	*filename;
	char	*hostname;
	Xv_Server	server = XV_SERVER_FROM_WINDOW(event_window(event));

	if ((ds = dnd_decode_drop(sel, event)) == XV_ERROR) {
		footer_message("Decode drop failed.");
		return;
	}

	if (local_drag) {
		local_footer_message("Ignoring drag & drop on same window");
	} else if (dnd_is_local(event)) {
		dowhat = DOGET;
		notify_stop();
	} else {
		local_footer_message("Can only handle local drops.");
	}


#ifdef notdef
	xv_set(sel,
		SEL_TYPE_NAME, "_SUN_ENUMERATION_COUNT",
		NULL);

	string_length = (int *)xv_get(sel, SEL_DATA, &length, &format);
	if (length != SEL_ERROR) {
		nitems = *string_length;
		free ((char *)string_length);
	}

	for (i = 0; i < nitems; i++) {

		xv_set(sel,
			SEL_TYPE,
			    xv_get(server, SERVER_ATOM,
				"_SUN_ENUMERATION_ITEM"),
			SEL_TYPE_INDEX, 0,
			SEL_PROP_TYPE, XA_INTEGER,
			SEL_PROP_DATA, &i,
			SEL_PROP_FORMAT, 32,
			SEL_PROP_LENGTH, 1,
			NULL);

		(void) xv_get(sel, SEL_DATA, &length, &format);

		xv_set(sel,
			SEL_TYPE, xv_get(server, SERVER_ATOM, "FILE_NAME"),
			NULL);

		filename = (char *)xv_get(sel, SEL_DATA, &length, &format);
		if (length != SEL_ERROR) {
			free (filename);
		}

		/* XXX */
		/* Care about remote host drops? Probably. */
		xv_set(sel,
			SEL_TYPE, xv_get(server, SERVER_ATOM, "HOST_NAME"),
			NULL);

		hostname = (char *)xv_get(sel, SEL_DATA, &length, &format);
		if (length != SEL_ERROR) {
			free (hostname);
		}

	}
	xv_set(sel,
		SEL_TYPE_NAME, "_SUN_SELECTION_END",
		NULL);

	(void) xv_get(sel, SEL_DATA, &length, &format);
#endif

	dnd_done(sel);

}

static void (*local_list_event_func)();

#ifdef USE_PROTOTYPES
void get_local_list_event_proc(void)
#else
void get_local_list_event_proc()
#endif
{
	local_list_event_func = (void (*)())xv_get(base_window.list,
		PANEL_EVENT_PROC);
	if (local_list_event_func == (void (*)())NULL) {
		fprintf(stderr,
		    "ftptool: could not get default list eventproc!\n");
		exit(1);
	}
}

#ifdef USE_PROTOTYPES
void local_list_event(Panel_item item, Event *event)
#else
void local_list_event(item, event)
Panel_item	item;
Event 		*event;
#endif
{
	short	mask;
	static int		drag_pixels;

	switch (event_action(event)) {
	case ACTION_SELECT:
		break;
	case ACTION_ADJUST:
		/*
		 * XXX: translate ACTION_ADJUST to ACTION_SELECT,
		 * so they can still use ADJUST to drag-select
		 * items on the list.
		 */
		event_set_action(event, ACTION_SELECT);
		break;
	case LOC_DRAG:
		/*
		 * XXX: translate ACTION_ADJUST to ACTION_SELECT,
		 * so they can still use ADJUST to drag-select items
		 * on the list.
		 */
		if (action_adjust_is_down(event)) {
			mask = event_shiftmask(event);
			mask &= ~MS_MIDDLE_MASK;
			mask |= MS_LEFT_MASK;
			event_set_shiftmask(event, mask);
			drag_pixels = 0;
		} else if (action_select_is_down(event) &&
		    (event_x(event) != 0) && (event_y(event) != 0)) {
			/*
			 * clicks on the scrollbar, which should _not source a
			 * drag, have x and y as 0.
			 */
			if ((local_list_nfiles + local_list_ndirs +
			    local_list_nothers) &&
			    (drag_pixels++ >= drag_threshold)) {
				/* source the drag */
				local_drag = 1;
				switch (dnd_send_drop(local_window.dnd)) {
				case XV_OK:
					break;
				case DND_TIMEOUT:
					local_footer_message(
					    "Drag and Drop: Timed out.");
					break;
				case DND_ILLEGAL_TARGET:
					local_footer_message(
					    "Drag and Drop: Illegal target.");
					break;
				case DND_SELECTION:
					local_footer_message(
					    "Drag and Drop: Bad selection.");
					break;
				case DND_ROOT:
					dowhat = DOLOCALVIEW;
					notify_stop();
					break;
				case XV_ERROR:
					footer_message(
					    "Drag and Drop: Failed.");
					break;
				}
				local_drag = 0;
				drag_pixels = 0;
				return;
			}
		} else {
			drag_pixels = 0;
		}
		break;
	default:
		break;
	}
	(*local_list_event_func)(item, event);
}

#endif
