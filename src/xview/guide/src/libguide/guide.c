/*
 * This file is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify this file without charge, but are not authorized to
 * license or distribute it to anyone else except as part of a product
 * or program developed by the user.
 * 
 * THIS FILE IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 * 
 * This file is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 * 
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS FILE
 * OR ANY PART THEREOF.
 * 
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even
 * if Sun has been advised of the possibility of such damages.
 * 
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */

#ifndef lint
static char     sccsid[] = "@(#)guide.c	2.34 91/10/15 Copyright 1989 Sun Microsystems";
#endif

#include "guide.h"

/*
 * Table of attribute name strings indexed by attribute.  The entries in this
 * table must match G_ATTRS.
 */
static char    *attr_string[] =
{
	":actions",
	":anchor-object",
	":anchor-point",
	":background-color",
	":busy-drop-glyph",
	":button-type",
	":choices",
	":choice-defaults",
	":choice-label-types",
	":choice-colors",
	":columns",
	":column-alignment",
	":connections",
	":constant-width",
	":default-drop-site",
	":dnd-accept-cursor",
	":dnd-accept-cursor-xhot",
	":dnd-accept-cursor-yhot",
	":dnd-cursor",
	":dnd-cursor-xhot",
	":dnd-cursor-yhot",
	":draggable",
	":done-handler",
	":droppable",
	":drop-target_width",
	":drawing-model",
	":events",
	":event-handler",
	":foreground-color",
	":group-type",
	":height",
	":help",
	":horizontal-offset",
	":horizontal-scrollbar",
	":horizontal-spacing",
	":icon-file",
	":icon-label",
	":icon-mask-file",
	":initial-list-glyphs",
	":initial-list-values",
	":initial-selections",
	":initial-state",
	":initial-value",
	":international-db-begin",
	":international-db-end",
	":label",
	":label-type",
	":label-bold",
	":layout-type",
	":mapped",
	":max-tick-string",
	":max-value",
	":max-value-string",
	":members",
	":menu-handler",
	":menu-item-colors",
	":menu-item-defaults",
	":menu-item-handlers",
	":menu-item-labels",
	":menu-item-label-types",
	":menu-item-menus",
	":menu-item-states",
	":menu",
	":menu-title",
	":menu-type",
	":min-tick-string",
	":min-value",
	":min-value-string",
	":multiple-selections",
	":name",
	":normal-drop-glyph",
	":notify-handler",
	":orientation",
	":owner",
	":pinnable",
	":pinned",
	":read-only",
	":reference-point",
	":repaint-proc",
	":resizable",
	":rows",
	":row-alignment",
	":scrollable-height",
	":scrollable-width",
	":selection-required",
	":setting-type",
	":show-border",
	":show-endboxes",
	":show-footer",
	":show-range",
	":show-value",
	":slider-width",
	":stored-length",
	":text-type",
	":ticks",
	":title",
	":type",
	":user-data",
	":value-length",
	":value-underlined",
	":value-x",
	":value-y",
	":vertical-offset",
	":vertical-scrollbar",
	":vertical-spacing",
	":width",
	":x",
	":y",
	0,
};


/*
 * The actions strings.
 * The order of these strings must match that in G_ACTION_ATTRS.
 */
static char	*action_attr_string[] =
{
	":from",
	":when",
	":to",
	":function_type",
	":arg_type",
	":action",
	0,
};


/*
 * Table of attribute name strings indexed by attribute.  The entries
 * in this table must match G_PROJ_ATTRIS.
 */
static char	*proj_attr_string[] =
{
	":interfaces",
	":actions",
	":root_window",
	0,
};


/*
 * Table of attribute name strings indexed by attribute.  The entries
 * in this table must match G_RESFILE_ATTRS.
 */
static char	*resfile_attr_string[] =
{
	":type",
	":events",
	":actions",
	":receivers",
	0,
};


/*
 * Table of argument type strings indexed by argument type.  The
 * entries in this table must match G_ARG_TYPES.
 */
static char	*arg_type_string[] =
{
	":void",
	":integer",
	":float",
	":string",
	0,
};

/*
 * Table of function type strings indexed by argument type.  The
 * entries in this table must match G_FUNC_TYPES.
 */
static char	*func_type_string[] =
{
	":user-defined",
	":function",
	":code",
	0,
};

/*
 * Table of Initial State strings indexed by argument type.  The
 * entries in this table must match G_INITIAL_STATES.
 */
static char	*initial_state_string[] =
{
	":active",
	":iconic",
	":inactive",
	":invisible",
	":notselected",
	":open",
	":selected",
	":visible",
	0,
};

/*
 * Table of button name strings indexed by button type.  The entries in this
 * table must match G_BUTTON_TYPES.
 */
static char    *button_type_string[] =
{
	":normal",
	":abbreviated",
	0,
};

/*
 * Table of drawing model strings indexed by drawing model.  The entries in
 * this table must match G_DRAWING_MODELS.
 */
static char    *drawing_model_string[] =
{
	":xview",
	":xwindows",
	":postscript",
	0,
};

/*
 * Table of event name strings indexed by event type.  The entries in this
 * table must match G_EVENT_TYPES.
 */
static char    *event_type_string[] =
{
	":keyboard",
	":keyboard-left",
	":keyboard-right",
	":keyboard-top",
	":mouse",
	":mouse-drag",
	":mouse-enter",
	":mouse-exit",
	":mouse-move",
	0,
};

/*
 * Table of label name strings indexed by layout type.  The entries in this
 * table must match G_LABEL_TYPES.
 */
static char    *label_type_string[] =
{
	":string",
	":glyph",
	0,
};

/*
 * Table of menu name strings indexed by menu type.  The entries in this
 * table must match G_MENU_TYPES.
 */
static char    *menu_type_string[] =
{
	":command",
	":exclusive",
	":nonexclusive",
	0,
};

/*
 * Table of layout name strings indexed by attribute.  The entries in this
 * table must match G_LAYOUT_TYPE.
 */
static char    *layout_type_string[] =
{
	":horizontal",
	":vertical",
	0,
};

/*
 * Table of group type name strings indexed by attribute.  The entries in this
 * table must match G_GROUP_TYPE.
 */
static char    *group_type_string[] =
{
	"nil",
	":row",
	":column",
	":row-column",
	0,
};

/*
 * Table of compass points.  The entries in this table must match
 * G_COMPASS_POINTS.
 */
static char    *compass_point_string[] =
{
	":north-west",
	":north",
	":north-east",
	":west",
	":center",
	":east",
	":south-west",
	":south",
	":south-east",
	0,
};

/*
 * Table of column alignment types.  The entries in this table must match
 * G_COL_ALIGNMENTS.
 */
static char    *col_alignment_string[] =
{
	":left-edges",
	":labels",
	":vertical-centers",
	":right-edges",
	0,
};

/*
 * Table of row alignment types.  The entries in this table must match
 * G_ROW_ALIGNMENTS.
 */
static char    *row_alignment_string[] =
{
	":top-edges",
	":horizontal-centers",
	":bottom-edges",
	0,
};

/*
 * Setting types.
 */
static char    *setting_type_string[] =
{
	":exclusive",
	":nonexclusive",
	":check",
	":stack",	/* MOOSE - can this be changed to ":setting-stack"? */
	0,
};

/*
 * Text types.
 */
static char    *text_type_string[] =
{
	":alphanumeric",
	":multiline",
	":numeric",
	0,
};

/*
 * Table of object type name strings indexed by object type.  The entries in
 * this table must match G_TYPES.
 */
static char    *type_string[] =
{
	":base-window",
	":button",
	":canvas-pane",
	":control-area",
	":drop-target",
	":gauge",
	":group",
	":menu",
	":menu-items",
	":message",
	":popup-window",
	":scrolling-list",
	":setting",
	":setting-items",
	":slider",
	":stack",
	":term-pane",
	":text-field",
	":text-pane",
	0,
};

/*
 * Internal procedure declarations.
 */
#ifdef linux
static int      lookup();
#else
int             lookup();
#endif

/*
 * Return a list of object attributes for the specified object type.
 */
G_ATTRS        *
#ifdef __STDC__
G_object_attrs(G_TYPES type)
#else
G_object_attrs(type)
	G_TYPES         type;
#endif
{
	switch (type)
	{
	case G_BASE_WINDOW:
	{
		static G_ATTRS  attrs[] =
		{
			G_TYPE,
			G_NAME,
			G_OWNER,
			G_INTERNATIONAL_DB_BEGIN,
			G_WIDTH,
			G_HEIGHT,
			G_INTERNATIONAL_DB_END,
			G_BACKGROUND_COLOR,
			G_FOREGROUND_COLOR,
			G_LABEL,
			G_LABEL_TYPE,
			G_MAPPED,
			G_INITIAL_STATE,
			G_SHOW_FOOTER,
			G_RESIZABLE,
			G_ICON,
			G_ICON_LABEL,
			G_ICON_MASK,
			G_EVENT_HANDLER,
			G_EVENTS,
			G_USER_DATA,
			G_ACTIONS,
			-1,
		};
		
		return attrs;
	}
		
	case G_BUTTON:
	{
		static G_ATTRS  attrs[] =
		{
			G_TYPE,
			G_NAME,
			G_OWNER,
			G_HELP,
			G_INTERNATIONAL_DB_BEGIN,
			G_X,
			G_Y,
			G_WIDTH,
			G_HEIGHT,
			G_CONSTANT_WIDTH,
			G_INTERNATIONAL_DB_END,
			G_BUTTON_TYPE,
			G_FOREGROUND_COLOR,
			G_LABEL,
			G_LABEL_TYPE,
			G_INITIAL_STATE,
			G_MENU_NAME,
			G_NOTIFY_HANDLER,
			G_EVENT_HANDLER,
			G_EVENTS,
			G_USER_DATA,
			G_ACTIONS,
			-1,
		};
		
		return attrs;
	}
		
	case G_CANVAS_PANE:
	{
		static G_ATTRS  attrs[] =
		{
			G_TYPE,
			G_NAME,
			G_OWNER,
			G_HELP,
			G_INTERNATIONAL_DB_BEGIN,
			G_X,
			G_Y,
			G_WIDTH,
			G_HEIGHT,
			G_INTERNATIONAL_DB_END,
			G_BACKGROUND_COLOR,
			G_FOREGROUND_COLOR,
			G_INITIAL_STATE,
			G_DRAGGABLE,
			G_DROPPABLE,
			G_DEFAULT_DROP_SITE,
			G_MENU_NAME,
			G_HSCROLL,
			G_SCROLLABLE_WIDTH,
			G_VSCROLL,
			G_SCROLLABLE_HEIGHT,
			G_REPAINT_PROC,
			G_EVENT_HANDLER,
			G_EVENTS,
			G_DRAWING_MODEL,
			G_USER_DATA,
			G_ACTIONS,
			-1,
		};
		
		return attrs;
	}

	case G_CONTROL_AREA:
	{
		static G_ATTRS  attrs[] =
		{
			G_TYPE,
			G_NAME,
			G_OWNER,
			G_HELP,
			G_INTERNATIONAL_DB_BEGIN,
			G_X,
			G_Y,
			G_WIDTH,
			G_HEIGHT,
			G_INTERNATIONAL_DB_END,
			G_BACKGROUND_COLOR,
			G_FOREGROUND_COLOR,
			G_INITIAL_STATE,
			G_SHOW_BORDER,
			G_MENU_NAME,
			G_EVENT_HANDLER,
			G_EVENTS,
			G_USER_DATA,
			G_ACTIONS,
			-1,
		};
		return attrs;
	}

	case G_DROP_TARGET:
	{
		static G_ATTRS  attrs[] =
		{
			G_TYPE,
			G_NAME,
			G_OWNER,
			G_HELP,
			G_INTERNATIONAL_DB_BEGIN,
			G_X,
			G_Y,
			G_WIDTH,
			G_HEIGHT,
			G_DROP_TARGET_WIDTH,
			G_INTERNATIONAL_DB_END,
			G_DEFAULT_DROP_SITE,
			G_DRAGGABLE,
			G_DROPPABLE,
			G_LABEL,
			G_LABEL_TYPE,
			G_NORMAL_DROP_GLYPH,
			G_BUSY_DROP_GLYPH,
			G_DND_CURSOR,
			G_DND_CURSOR_XHOT,
			G_DND_CURSOR_YHOT,
			G_DND_ACCEPT_CURSOR,
			G_DND_ACCEPT_CURSOR_XHOT,
			G_DND_ACCEPT_CURSOR_YHOT,
			G_FOREGROUND_COLOR,
			G_INITIAL_STATE,
			G_NOTIFY_HANDLER,
			G_EVENT_HANDLER,
			G_EVENTS,
			G_USER_DATA,
			G_ACTIONS,
			-1,
		};
		
		return attrs;
	}

	case G_GAUGE:
	{
		static G_ATTRS  attrs[] =
		{
			G_TYPE,
			G_NAME,
			G_OWNER,
			G_HELP,
			G_INTERNATIONAL_DB_BEGIN,
			G_X,
			G_Y,
			G_WIDTH,
			G_HEIGHT,
			G_VALUE_X,
			G_VALUE_Y,
			G_SLIDER_WIDTH,
			G_TICKS,
			G_INTERNATIONAL_DB_END,
			G_FOREGROUND_COLOR,
			G_LABEL,
			G_LABEL_TYPE,
			G_LAYOUT_TYPE,
			G_ORIENTATION,
			G_SHOW_RANGE,
			G_MIN_VALUE,
			G_MAX_VALUE,
			G_MIN_TICK_STRING,
			G_MAX_TICK_STRING,
			G_INITIAL_VALUE,
			G_INITIAL_STATE,
			G_EVENT_HANDLER,
			G_EVENTS,
			G_USER_DATA,
			G_ACTIONS,
			-1,
		};
		
		return attrs;
	}

	case G_GROUP:
	{
		static G_ATTRS  attrs[] =
		{
			G_TYPE,
			G_NAME,
			G_X,
			G_Y,
			G_GROUP_TYPE,
			G_MEMBERS,
			G_ROWS,
			G_ROW_ALIGNMENT,
			G_COLUMNS,
			G_COL_ALIGNMENT,
			G_HSPACING,
			G_VSPACING,
			G_ANCHOR_OBJECT,
			G_ANCHOR_POINT,
			G_REFERENCE_POINT,
/* MOOSE, JI18N - should this be part of groups???
			G_INTERNATIONAL_DB_BEGIN,
			G_INTERNATIONAL_DB_END,
*/
			G_HOFFSET,
			G_VOFFSET,
			G_USER_DATA,
			G_ACTIONS,
			-1,
		};
		
		return attrs;
	}

	case G_MENU:
	{
		static G_ATTRS	attrs[] =
		{
			G_TYPE,
			G_NAME,
			G_HELP,
			G_COLUMNS,
			G_MENU_TYPE,
			G_MENU_HANDLER,
			G_MENU_TITLE,
			G_MENU_ITEM_LABELS,
			G_MENU_ITEM_LABEL_TYPES,
			G_MENU_ITEM_STATES,
			G_MENU_ITEM_DEFAULTS,
			G_INITIAL_SELECTIONS,
			G_MENU_ITEM_HANDLERS,
			G_MENU_ITEM_MENUS,
			G_MENU_ITEM_COLORS,
			G_PINNABLE,		/* must follow G_MENU_ITEM_* */
			G_USER_DATA,
			G_ACTIONS,
			-1,
		};
		return attrs;
	}

	case G_MESSAGE:
	{
		static G_ATTRS  attrs[] =
		{
			G_TYPE,
			G_NAME,
			G_OWNER,
			G_HELP,
			G_INTERNATIONAL_DB_BEGIN,
			G_X,
			G_Y,
			G_WIDTH,
			G_HEIGHT,
			G_INTERNATIONAL_DB_END,
			G_FOREGROUND_COLOR,
			G_LABEL,
			G_LABEL_TYPE,
			G_LABEL_BOLD,
			G_INITIAL_STATE,
			G_EVENT_HANDLER,
			G_EVENTS,
			G_USER_DATA,
			G_ACTIONS,
			-1,
		};
		
		return attrs;
	}

	case G_POPUP_WINDOW:
	{
		static G_ATTRS  attrs[] =
		{
			G_TYPE,
			G_NAME,
			G_OWNER,
			G_INTERNATIONAL_DB_BEGIN,
			G_WIDTH,
			G_HEIGHT,
			G_INTERNATIONAL_DB_END,
			G_BACKGROUND_COLOR,
			G_FOREGROUND_COLOR,
			G_LABEL,
			G_LABEL_TYPE,
			G_MAPPED,
			G_INITIAL_STATE,
			G_SHOW_FOOTER,
			G_RESIZABLE,
			G_PINNED,
			G_DONE_HANDLER,
			G_EVENT_HANDLER,
			G_EVENTS,
			G_USER_DATA,
			G_ACTIONS,
			-1,
		};
		
		return attrs;
	}

	case G_SCROLLING_LIST:
	{
		static G_ATTRS  attrs[] =
		{
			G_TYPE,
			G_NAME,
			G_OWNER,
			G_HELP,
			G_INTERNATIONAL_DB_BEGIN,
			G_X,
			G_Y,
			G_WIDTH,
			G_HEIGHT,
			G_VALUE_X,
			G_VALUE_Y,
			G_ROWS,
			G_INTERNATIONAL_DB_END,
			G_FOREGROUND_COLOR,
			G_LABEL,
			G_TITLE,
			G_LABEL_TYPE,
			G_LAYOUT_TYPE,
			G_READ_ONLY,
			G_MULTIPLE_SELECTIONS,
			G_SELECTION_REQUIRED,
			G_INITIAL_STATE,
			G_DROPPABLE,
			G_DEFAULT_DROP_SITE,
			G_MENU_NAME,
			G_NOTIFY_HANDLER,
			G_EVENT_HANDLER,
			G_EVENTS,
			G_INITIAL_LIST_VALUES,
			G_INITIAL_LIST_GLYPHS,
			G_INITIAL_SELECTIONS,
			G_USER_DATA,
			G_ACTIONS,
			-1,
		};
		
		return attrs;
	}

	case G_SETTING:
	{
		static G_ATTRS  attrs[] =
		{
			G_TYPE,
			G_NAME,
			G_OWNER,
			G_HELP,
			G_INTERNATIONAL_DB_BEGIN,
			G_X,
			G_Y,
			G_WIDTH,
			G_HEIGHT,
			G_VALUE_X,
			G_VALUE_Y,
			G_ROWS,
			G_COLUMNS,
			G_INTERNATIONAL_DB_END,
			G_LAYOUT_TYPE,
			G_FOREGROUND_COLOR,
			G_SETTING_TYPE,
			G_SELECTION_REQUIRED,
			G_LABEL,
			G_LABEL_TYPE,
			G_NOTIFY_HANDLER,
			G_EVENT_HANDLER,
			G_EVENTS,
			G_CHOICES,
			G_CHOICE_LABEL_TYPES,
			G_CHOICE_COLORS,
			G_CHOICE_DEFAULTS,
			G_INITIAL_SELECTIONS,
			G_INITIAL_STATE,
			G_USER_DATA,
			G_ACTIONS,
			-1,
		};
		
		return attrs;
	}
		
	case G_SLIDER:
	{
		static G_ATTRS  attrs[] =
		{
			G_TYPE,
			G_NAME,
			G_OWNER,
			G_HELP,
			G_INTERNATIONAL_DB_BEGIN,
			G_X,
			G_Y,
			G_WIDTH,
			G_HEIGHT,
			G_VALUE_X,
			G_VALUE_Y,
			G_SLIDER_WIDTH,
			G_TICKS,
			G_INTERNATIONAL_DB_END,
			G_FOREGROUND_COLOR,
			G_LABEL,
			G_LABEL_TYPE,
			G_LAYOUT_TYPE,
			G_ORIENTATION,
			G_SHOW_ENDBOXES,
			G_SHOW_RANGE,
			G_SHOW_VALUE,
			G_MIN_VALUE,
			G_MAX_VALUE,
			G_MIN_VALUE_STRING,
			G_MAX_VALUE_STRING,
			G_MIN_TICK_STRING,
			G_MAX_TICK_STRING,
			G_INITIAL_VALUE,
			G_INITIAL_STATE,
			G_NOTIFY_HANDLER,
			G_EVENT_HANDLER,
			G_EVENTS,
			G_USER_DATA,
			G_ACTIONS,
			-1,
		};
		
		return attrs;
	}

	case G_STACK:
	{
		static G_ATTRS  attrs[] =
		{
			G_TYPE,
			G_NAME,
			G_OWNER,
			G_MEMBERS,
			G_USER_DATA,
			-1,
		};
		
		return attrs;
	}

	case G_TERM_PANE:
	{
		static G_ATTRS  attrs[] =
		{
			G_TYPE,
			G_NAME,
			G_OWNER,
			G_HELP,
			G_INTERNATIONAL_DB_BEGIN,
			G_X,
			G_Y,
			G_WIDTH,
			G_HEIGHT,
			G_INTERNATIONAL_DB_END,
			G_BACKGROUND_COLOR,
			G_FOREGROUND_COLOR,
			G_INITIAL_STATE,
			G_SHOW_BORDER,
			G_EVENT_HANDLER,
			G_EVENTS,
			G_USER_DATA,
			G_ACTIONS,
			-1,
		};
		return attrs;
	}

	case G_TEXT_FIELD:
	{
		static G_ATTRS  attrs[] =
		{
			G_TYPE,
			G_NAME,
			G_OWNER,
			G_HELP,
			G_INTERNATIONAL_DB_BEGIN,
			G_X,
			G_Y,
			G_WIDTH,
			G_HEIGHT,
			G_VALUE_X,
			G_VALUE_Y,
			G_VALUE_LENGTH,
			G_STORED_LENGTH,
			G_ROWS,
			G_INTERNATIONAL_DB_END,
			G_FOREGROUND_COLOR,
			G_TEXT_TYPE,
			G_LABEL,
			G_LABEL_TYPE,
			G_LAYOUT_TYPE,
			G_VALUE_UNDERLINED,
			G_MAX_VALUE,
			G_MIN_VALUE,
			G_INITIAL_VALUE,
			G_INITIAL_STATE,
			G_READ_ONLY,
			G_NOTIFY_HANDLER,
			G_EVENT_HANDLER,
			G_EVENTS,
			G_USER_DATA,
			G_ACTIONS,
			-1,
		};
		
		return attrs;
	}
		
	case G_TEXT_PANE:
	{
		static G_ATTRS  attrs[] =
		{
			G_TYPE,
			G_NAME,
			G_OWNER,
			G_HELP,
			G_INTERNATIONAL_DB_BEGIN,
			G_X,
			G_Y,
			G_WIDTH,
			G_HEIGHT,
			G_INTERNATIONAL_DB_END,
			G_BACKGROUND_COLOR,
			G_FOREGROUND_COLOR,
			G_INITIAL_STATE,
			G_SHOW_BORDER,
			G_READ_ONLY,
			G_EVENT_HANDLER,
			G_EVENTS,
			G_USER_DATA,
			G_ACTIONS,
			-1,
		};
		return attrs;
	}

	default:
	{
		static G_ATTRS  attrs[] =
		{
			G_TYPE,
			G_NAME,
			G_OWNER,
			G_HELP,
			G_EVENT_HANDLER,
			G_EVENTS,
			G_USER_DATA,
			G_ACTIONS,
			-1,
		};
		
		return attrs;
	}
	}
}


/*
 * Return a list of action attributes.
 */
G_ACTION_ATTRS        *
G_action_attrs()
{
	static G_ACTION_ATTRS  attrs[] =
	{
		G_ACTION_FROM,
		G_ACTION_WHEN,
		G_ACTION_TO,
		G_ACTION_FUNC_TYPE,
		G_ACTION_ARG_TYPE,
		G_ACTION_OPERATION,
		-1,
	};
		
	return attrs;
}


/*
 * The list of attributes specifying the order of things written in the
 * project file.
 */
G_PROJ_ATTRS	*
G_project_attrs()
{
	static G_PROJ_ATTRS	proj_attrs[] =
	{
		G_INTERFACES,
		G_PROJ_ACTIONS,
		G_ROOT_WINDOW,
		-1,
	};

	return proj_attrs;
}


/*
 * Change an attribute into a string.  Returns a string.
 * 
 * REMIND: error checking.
 */
char           *
#ifdef __STDC__
G_attr_to_string(G_ATTRS attr)
#else
G_attr_to_string(attr)
	G_ATTRS         attr;
#endif
{
	return attr_string[attr];
}

/*
 * Change a string into an attribute.  Returns the attribute number if
 * successful, otherwise -1.
 */
G_ATTRS
#ifdef __STDC__
G_string_to_attr(char *s)
#else
G_string_to_attr(s)
	char           *s;
#endif
{
	return lookup(s, attr_string);
}


/*
 * Return the action string given a symbolic attribute.
 */
char	*
#ifdef __STDC__
G_action_attr_to_string(G_ACTION_ATTRS attr)
#else
G_action_attr_to_string(attr)
	G_ACTION_ATTRS	attr;
#endif
{
	return action_attr_string[attr];
}


/*
 * Return the action attribute given a string.
 */
G_ACTION_ATTRS
#ifdef __STDC__
G_string_to_action_attr(char *s)
#else
G_string_to_action_attr(s)
	char	*s;
#endif
{
	return lookup(s, action_attr_string);
}


/*
 * Returns the attribute string given the attribute number.
 */
char	*
#ifdef __STDC__
G_proj_attr_to_string(G_PROJ_ATTRS attr)
#else
G_proj_attr_to_string(attr)
	G_PROJ_ATTRS	attr;
#endif
{
	return proj_attr_string[attr];
} 


/*
 * Change a string into an attribute.  Returns the attribute number if
 * successful, otherwise -1.
 */
G_PROJ_ATTRS
#ifdef __STDC__
G_string_to_proj_attr(char *s)
#else
G_string_to_proj_attr(s)
	char	*s;
#endif
{
	return lookup(s, proj_attr_string);
}


/*
 * Change a string into an attribute.  Returns the attribute number if
 * successful, otherwise -1.
 */
G_RESFILE_ATTRS
#ifdef __STDC__
G_string_to_resfile_attr(char *s)
#else
G_string_to_resfile_attr(s)
	char	*s;
#endif
{
	return lookup(s, resfile_attr_string);
}

/*
 * Change a string into an argument type.
 */
G_ARG_TYPES
#ifdef __STDC__
G_string_to_arg_type(char *s)
#else
G_string_to_arg_type(s)
	char	*s;
#endif
{
	return lookup(s, arg_type_string);
}

/*
 * Change a string into an function type.
 */
G_FUNC_TYPES
#ifdef __STDC__
G_string_to_func_type(char *s)
#else
G_string_to_func_type(s)
	char	*s;
#endif
{
	return lookup(s, func_type_string);
}

/*
 * Change a button type into a string.  Returns a string.
 * 
 * REMIND: error checking.
 */
char           *
#ifdef __STDC__
G_button_type_to_string(G_BUTTON_TYPES val)
#else
G_button_type_to_string(val)
	G_BUTTON_TYPES   val;
#endif
{
	return button_type_string[val];
}

/*
 * Change a string into a button type.
 */
G_BUTTON_TYPES
#ifdef __STDC__
G_string_to_button_type(char *s)
#else
G_string_to_button_type(s)
	char           *s;
#endif
{
	return lookup(s, button_type_string);
}

/*
 * Change a drawing model into a string.  Returns a string.
 * 
 * REMIND: error checking.
 */
char           *
#ifdef __STDC__
G_drawing_model_to_string(G_DRAWING_MODELS drawing_model)
#else
G_drawing_model_to_string(drawing_model)
	G_DRAWING_MODELS	drawing_model;
#endif
{
	return drawing_model_string[drawing_model];
}

/*
 * Change a string into an attribute.  Returns the attribute number if
 * successful, otherwise -1.
 */
G_DRAWING_MODELS
#ifdef __STDC__
G_string_to_drawing_model(char *s)
#else
G_string_to_drawing_model(s)
	char           *s;
#endif
{
	return lookup(s, drawing_model_string);
}

/*
 * Change an event type into a string.  Returns a string.
 * 
 * REMIND: error checking.
 */
char           *
#ifdef __STDC__
G_event_type_to_string(G_EVENT_TYPES event)
#else
G_event_type_to_string(event)
	G_EVENT_TYPES	event;
#endif
{
	return event_type_string[event];
}

/*
 * Change a string into an attribute.  Returns the attribute number if
 * successful, otherwise -1.
 */
G_EVENT_TYPES
#ifdef __STDC__
G_string_to_event_type(char *s)
#else
G_string_to_event_type(s)
	char           *s;
#endif
{
	return lookup(s, event_type_string);
}

/*
 * Change a label type into a string.  Returns a string.
 * 
 * REMIND: error checking.
 */
char           *
#ifdef __STDC__
G_label_type_to_string(G_LABEL_TYPES val)
#else
G_label_type_to_string(val)
	G_LABEL_TYPES   val;
#endif
{
	return label_type_string[val];
}

/*
 * Change a string into a label type.
 */
G_LABEL_TYPES
#ifdef __STDC__
G_string_to_label_type(char *s)
#else
G_string_to_label_type(s)
	char           *s;
#endif
{
	return lookup(s, label_type_string);
}

/*
 * Change a layout type into a string.  Returns a string.
 * 
 * REMIND: error checking.
 */
char           *
#ifdef __STDC__
G_layout_type_to_string(G_LAYOUT_TYPES val)
#else
G_layout_type_to_string(val)
	G_LAYOUT_TYPES  val;
#endif
{
	return layout_type_string[val];
}

/*
 * Change a string into a layout type.
 */
G_LAYOUT_TYPES
#ifdef __STDC__
G_string_to_layout_type(char *s)
#else
G_string_to_layout_type(s)
	char           *s;
#endif
{
	return lookup(s, layout_type_string);
}

/*
 * Change a group type into a string.  Returns a string.
 * 
 * REMIND: error checking.
 */
char           *
#ifdef __STDC__
G_group_type_to_string(G_GROUP_TYPES val)
#else
G_group_type_to_string(val)
	G_GROUP_TYPES  val;
#endif
{
	return group_type_string[val];
}

/*
 * Change a string into a group type.
 */
G_GROUP_TYPES
#ifdef __STDC__
G_string_to_group_type(char *s)
#else
G_string_to_group_type(s)
	char           *s;
#endif
{
	return lookup(s, group_type_string);
}

/*
 * Change a reference point into a string.  Returns a string.
 * 
 * REMIND: error checking.
 */
char           *
#ifdef __STDC__
G_compass_point_to_string(G_COMPASS_POINTS val)
#else
G_compass_point_to_string(val)
	G_COMPASS_POINTS  val;
#endif
{
	return compass_point_string[val];
}

/*
 * Change a string into a group type.
 */
G_COMPASS_POINTS
#ifdef __STDC__
G_string_to_compass_point(char *s)
#else
G_string_to_compass_point(s)
	char           *s;
#endif
{
	return lookup(s, compass_point_string);
}

/*
 * Change a column alignment type into a string.  Returns a string.
 * 
 * REMIND: error checking.
 */
char           *
#ifdef __STDC__
G_col_alignment_to_string(G_COL_ALIGNMENTS val)
#else
G_col_alignment_to_string(val)
	G_COL_ALIGNMENTS  val;
#endif
{
	return col_alignment_string[val];
}

/*
 * Change a string into a column alignment type.
 */
G_COL_ALIGNMENTS
#ifdef __STDC__
G_string_to_col_alignment(char *s)
#else
G_string_to_col_alignment(s)
	char           *s;
#endif
{
	return lookup(s, col_alignment_string);
}

/*
 * Change a row alignment type into a string.  Returns a string.
 * 
 * REMIND: error checking.
 */
char           *
#ifdef __STDC__
G_row_alignment_to_string(G_ROW_ALIGNMENTS val)
#else
G_row_alignment_to_string(val)
	G_ROW_ALIGNMENTS  val;
#endif
{
	return row_alignment_string[val];
}

/*
 * Change a string into a row alignment type.
 */
G_ROW_ALIGNMENTS
#ifdef __STDC__
G_string_to_row_alignment(char *s)
#else
G_string_to_row_alignment(s)
	char           *s;
#endif
{
	return lookup(s, row_alignment_string);
}

/*
 * Change a menu type into a string.  Returns a string.
 * 
 * REMIND: error checking.
 */
char           *
#ifdef __STDC__
G_menu_type_to_string(G_MENU_TYPES val)
#else
G_menu_type_to_string(val)
	G_MENU_TYPES   val;
#endif
{
	return menu_type_string[val];
}

/*
 * Change a string into a menu type.
 */
G_MENU_TYPES
#ifdef __STDC__
G_string_to_menu_type(char *s)
#else
G_string_to_menu_type(s)
	char           *s;
#endif
{
	return lookup(s, menu_type_string);
}

/*
 * Change a setting type into a string.  Returns a string.
 * 
 * REMIND: error checking.
 */
char           *
#ifdef __STDC__
G_setting_type_to_string(G_SETTING_TYPES val)
#else
G_setting_type_to_string(val)
	G_SETTING_TYPES val;
#endif
{
	return setting_type_string[val];
}

/*
 * Change a string into a setting type.
 */
G_SETTING_TYPES
#ifdef __STDC__
G_string_to_setting_type(char *s)
#else
G_string_to_setting_type(s)
	char           *s;
#endif
{
	return lookup(s, setting_type_string);
}

/*
 * Change a setting type into a string.  Returns a string.
 * 
 * REMIND: error checking.
 */
char           *
#ifdef __STDC__
G_text_type_to_string(G_TEXT_TYPES val)
#else
G_text_type_to_string(val)
	G_TEXT_TYPES val;
#endif
{
	return text_type_string[val];
}


/*
 * Change a Initial State into a string.  Returns a string.
 * 
 */
char           *
#ifdef __STDC__
G_initial_state_to_string(G_INITIAL_STATES initial_state)
#else
G_initial_state_to_string(initial_state)
	G_INITIAL_STATES	initial_state;	
#endif
{
	return initial_state_string[initial_state];
}

/*
 * Change a string into an Initial State.  Returns the  Initial State
 * number if successful, otherwise -1.
 */
G_INITIAL_STATES
#ifdef __STDC__
G_string_to_initial_state(char *s)
#else
G_string_to_initial_state(s)
	char           *s;
#endif
{
	return lookup(s, initial_state_string);
}

/*
 * Change a string into a text type.
 */
G_TEXT_TYPES
#ifdef __STDC__
G_string_to_text_type(char *s)
#else
G_string_to_text_type(s)
	char           *s;
#endif
{
	return lookup(s, text_type_string);
}

/*
 * Change a type into a string.  Returns a string.
 * 
 * REMIND: error checking.
 */
char           *
#ifdef __STDC__
G_type_to_string(G_TYPES type)
#else
G_type_to_string(type)
	G_TYPES         type;
#endif
{
	return type_string[type];
}

/*
 * Change a string into a type.  Returns the type number if successful,
 * otherwise -1.
 */
G_TYPES
#ifdef __STDC__
G_string_to_type(char *s)
#else
G_string_to_type(s)
	char           *s;
#endif
{
	return lookup(s, type_string);
}

/*
 * Search for a string in the specified table.  Returns the table index if
 * successful, otherwise -1.
 */
static int
#ifdef __STDC__
lookup(char *s, char **tab)
#else
lookup(s, tab)
	char           *s;
	char          **tab;
#endif
{
	int             i;

	for (i = 0; tab[i]; i++)
		if (strcmp(s, tab[i]) == 0)
			return i;
	return -1;
}
