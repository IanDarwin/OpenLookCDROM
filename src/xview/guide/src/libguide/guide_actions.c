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
static char     sccsid[] = "@(#)guide_actions.c	2.22 91/10/15 Copyright 1989 Sun Microsystems";
#endif

/*
 * This file contains tables of default messages to be loaded
 * into guide during start up time.
 */

#include "guide.h"
#include "guide_actions.h"


/*
 * The complete list of default events.
 */
G_MSG_STRUCT	*
G_all_default_events()
{
	static G_MSG_STRUCT events[] =
	{
 	{G_USER_TYPE,   NOTIFY,		G_VOID_TYPE},
	{G_USER_TYPE,   MOUSE_SELECT,   G_VOID_TYPE},
	{G_USER_TYPE,   MOUSE_ADJUST,   G_VOID_TYPE},
	{G_USER_TYPE,   MOUSE_MENU,     G_VOID_TYPE},
	{G_USER_TYPE,   ANYEVENT,       G_VOID_TYPE},
	{G_USER_TYPE,   DOUBLECLICK,    G_VOID_TYPE},
	{G_USER_TYPE,   ENTER,          G_VOID_TYPE},
	{G_USER_TYPE,   EXIT,           G_VOID_TYPE},
	{G_USER_TYPE,   RESIZE,         G_VOID_TYPE},
	{G_USER_TYPE,   KEYPRESS,       G_VOID_TYPE},
	{G_USER_TYPE,	REPAINT,	G_VOID_TYPE},
	{G_USER_TYPE,	DROPPED_UPON,	G_VOID_TYPE},
	{G_USER_TYPE,	DRAGGED_FROM,	G_VOID_TYPE},
		0,
	};
	return events;
}

/*
 * The complete list of default messages.
 */
G_MSG_STRUCT    *
G_all_default_messages()
{
	static G_MSG_STRUCT actions[] =
	{
	{G_USER_TYPE,		SHOW,		G_VOID_TYPE},
	{G_USER_TYPE,		HIDE,		G_VOID_TYPE},
	{G_USER_TYPE,		ENABLE,		G_VOID_TYPE},
	{G_USER_TYPE,		DISABLE,	G_VOID_TYPE},
	{G_USER_TYPE,		SETLABEL,	G_STRING_TYPE},
	{G_USER_TYPE,		GETLABEL,	G_VOID_TYPE},
	{G_USER_TYPE,		SETLEFTFOOTER,	G_STRING_TYPE},
	{G_USER_TYPE,		GETLEFTFOOTER,	G_VOID_TYPE},
	{G_USER_TYPE,		SETRIGHTFOOTER,	G_STRING_TYPE},
	{G_USER_TYPE,		GETRIGHTFOOTER, G_VOID_TYPE},
	{G_USER_TYPE,		SETVALUENUMBER, G_STRING_TYPE},
	{G_USER_TYPE,		GETVALUENUMBER, G_VOID_TYPE},
	{G_USER_TYPE,		LOADTEXTFILE,	G_STRING_TYPE},
	{G_FUNCTION_TYPE,	CALLFUNCTION,	G_VOID_TYPE},
	{G_CODE_TYPE,		EXECUTECODE,	G_STRING_TYPE},
		0,
	};
	return actions;
}	

/*
 * The complete list of default receivers.
 */
G_TYPES *
G_all_default_receivers()
{
	static G_TYPES types[] =
	{
		G_BASE_WINDOW,
		G_BUTTON,
		G_CANVAS_PANE,
		G_CONTROL_AREA,
		G_DROP_TARGET,
		G_GAUGE,
		G_GROUP,
		G_MENU,
		G_MENU_ITEMS,
		G_MESSAGE,
		G_POPUP_WINDOW,
		G_SCROLLING_LIST,
		G_SETTING,
		G_SETTING_ITEMS,
		G_SLIDER,
		G_TERM_PANE,
		G_TEXT_FIELD,
		G_TEXT_PANE,
		-1,
	};
	return types;
}

/*
 * Return the valid events for a given type of object.
 */
G_MSG_STRUCT	*
#ifdef __STDC__
G_default_events(G_TYPES type)
#else
G_default_events(type)
	G_TYPES		type;
#endif
{
	switch (type)
	{
	case G_BASE_WINDOW:
	{
		static G_MSG_STRUCT events[] =
		{
		{G_USER_TYPE,	MOUSE_SELECT, 	G_VOID_TYPE},
		{G_USER_TYPE,	MOUSE_ADJUST,	G_VOID_TYPE},
		{G_USER_TYPE,	MOUSE_MENU, 	G_VOID_TYPE},
		{G_USER_TYPE,	ANYEVENT,	G_VOID_TYPE},
		{G_USER_TYPE,	DOUBLECLICK,	G_VOID_TYPE},
		{G_USER_TYPE,	ENTER,		G_VOID_TYPE},
		{G_USER_TYPE, 	EXIT, 		G_VOID_TYPE},
		{G_USER_TYPE,	RESIZE,		G_VOID_TYPE},
		{G_USER_TYPE,	KEYPRESS,	G_VOID_TYPE},
			0,
		};
		return events;
	}
	case G_BUTTON:
	{
		static G_MSG_STRUCT events[] =
		{
		{G_USER_TYPE,	NOTIFY, 	G_VOID_TYPE},
		{G_USER_TYPE,	ANYEVENT,	G_VOID_TYPE},
		{G_USER_TYPE, 	MOUSE_MENU, 	G_VOID_TYPE},
			0,
		};
		return events;
	}
	case G_CANVAS_PANE:
	{
		static G_MSG_STRUCT events[] =
		{
		{G_USER_TYPE,	MOUSE_SELECT, 	G_VOID_TYPE},
		{G_USER_TYPE,	MOUSE_ADJUST, 	G_VOID_TYPE},
		{G_USER_TYPE,	MOUSE_MENU, 	G_VOID_TYPE},
		{G_USER_TYPE,	ANYEVENT,	G_VOID_TYPE},
		{G_USER_TYPE,	DOUBLECLICK,	G_VOID_TYPE},
		{G_USER_TYPE,	ENTER,		G_VOID_TYPE},
		{G_USER_TYPE,	EXIT,		G_VOID_TYPE},
		{G_USER_TYPE,	REPAINT,	G_VOID_TYPE},
		{G_USER_TYPE,	RESIZE,		G_VOID_TYPE},
		{G_USER_TYPE,	KEYPRESS,	G_VOID_TYPE},
		{G_USER_TYPE,	DROPPED_UPON,	G_VOID_TYPE},
			0,
		};
		return events;
	}
	case G_CONTROL_AREA:
	{
		static G_MSG_STRUCT events[] =
		{
		{G_USER_TYPE,	MOUSE_SELECT, 	G_VOID_TYPE},
		{G_USER_TYPE,	MOUSE_ADJUST, 	G_VOID_TYPE},
		{G_USER_TYPE,	MOUSE_MENU, 	G_VOID_TYPE},
		{G_USER_TYPE,	ANYEVENT,	G_VOID_TYPE},
		{G_USER_TYPE,	DOUBLECLICK,	G_VOID_TYPE},
		{G_USER_TYPE,	ENTER,		G_VOID_TYPE},
		{G_USER_TYPE,	EXIT,		G_VOID_TYPE},
		{G_USER_TYPE,	KEYPRESS,	G_VOID_TYPE},
			0,
		};
		return events;
	}
	case G_DROP_TARGET:
	{
		static G_MSG_STRUCT events[] =
		{
		{G_USER_TYPE,	DROPPED_UPON,	G_VOID_TYPE},
		{G_USER_TYPE,	DRAGGED_FROM,	G_VOID_TYPE},
			0,
		};
		return events;
	}
	case G_GAUGE:
	{
		static G_MSG_STRUCT events[] =
		{
		{G_USER_TYPE,	MOUSE_SELECT, 	G_VOID_TYPE},
		{G_USER_TYPE,	MOUSE_ADJUST, 	G_VOID_TYPE},
		{G_USER_TYPE,	MOUSE_MENU, 	G_VOID_TYPE},
		{G_USER_TYPE,	ANYEVENT,	G_VOID_TYPE},
			0,
		};
		return events;
	}
	case G_GROUP:
	{
#ifdef linux
	        static G_MSG_STRUCT events[] = { 0 };
#else
		static G_MSG_STRUCT events[] = 0;
#endif
		return events;
	}
	case G_MENU:
	{
		static G_MSG_STRUCT events[] =
		{
		{G_USER_TYPE,	NOTIFY, 	G_VOID_TYPE},
			0,
		};
		return events;
	}
	case G_MENU_ITEMS:
	{
		static G_MSG_STRUCT events[] =
		{
		{G_USER_TYPE,	NOTIFY, 	G_VOID_TYPE},
			0,
		};
		return events;
	}
	case G_MESSAGE:
	{
		static G_MSG_STRUCT events[] =
		{
		{G_USER_TYPE,	MOUSE_SELECT, 	G_VOID_TYPE},
		{G_USER_TYPE,	MOUSE_ADJUST, 	G_VOID_TYPE},
		{G_USER_TYPE,	MOUSE_MENU, 	G_VOID_TYPE},
		{G_USER_TYPE,	ANYEVENT,	G_VOID_TYPE},
			0,
		};
		return events;
	}
	case G_POPUP_WINDOW:
	{
		static G_MSG_STRUCT events[] =
		{
		{G_USER_TYPE,	MOUSE_SELECT, 	G_VOID_TYPE},
		{G_USER_TYPE,	MOUSE_ADJUST, 	G_VOID_TYPE},
		{G_USER_TYPE,	MOUSE_MENU,	G_VOID_TYPE},
		{G_USER_TYPE,	ANYEVENT,	G_VOID_TYPE},
		{G_USER_TYPE,	DOUBLECLICK,	G_VOID_TYPE},
		{G_USER_TYPE,	DONE,		G_VOID_TYPE},
		{G_USER_TYPE,	ENTER,		G_VOID_TYPE},
		{G_USER_TYPE,	EXIT,		G_VOID_TYPE},
		{G_USER_TYPE,	RESIZE,		G_VOID_TYPE},
		{G_USER_TYPE,	KEYPRESS,	G_VOID_TYPE},
			0,
		};
		return events;
	}
	case G_SCROLLING_LIST:
	{
		static G_MSG_STRUCT events[] =
		{
		{G_USER_TYPE,	NOTIFY, 	G_VOID_TYPE},
		{G_USER_TYPE,	ANYEVENT,	G_VOID_TYPE},
		{G_USER_TYPE,	MOUSE_MENU,	G_VOID_TYPE},
		{G_USER_TYPE,	DROPPED_UPON, 	G_VOID_TYPE},
			0,
		};
		return events;
	}
	case G_SETTING:
	{
		static G_MSG_STRUCT events[] =
		{
		{G_USER_TYPE,	NOTIFY, 	G_VOID_TYPE},
		{G_USER_TYPE,	ANYEVENT,	G_VOID_TYPE},
			0,
		};
		return events;
	}
	case G_SETTING_ITEMS:
	{
		static G_MSG_STRUCT events[] =
		{
		{G_USER_TYPE,	NOTIFY, 	G_VOID_TYPE},
			0,
		};
		return events;
	}
	case G_SLIDER:
	{
		static G_MSG_STRUCT events[] =
		{
		{G_USER_TYPE,	NOTIFY, 	G_VOID_TYPE},
		{G_USER_TYPE,	ANYEVENT,	G_VOID_TYPE},
			0,
		};
		return events;
	}
	case G_TERM_PANE:
	{		
		static G_MSG_STRUCT events[] =
		{
		{G_USER_TYPE,	MOUSE_SELECT, 	G_VOID_TYPE},
		{G_USER_TYPE,	MOUSE_ADJUST, 	G_VOID_TYPE},
		{G_USER_TYPE,	MOUSE_MENU, 	G_VOID_TYPE},
		{G_USER_TYPE,	ANYEVENT,	G_VOID_TYPE},
		{G_USER_TYPE,	DOUBLECLICK,	G_VOID_TYPE},
		{G_USER_TYPE,	ENTER,		G_VOID_TYPE},
		{G_USER_TYPE,	EXIT,		G_VOID_TYPE},
		{G_USER_TYPE,	KEYPRESS,	G_VOID_TYPE},
			0,
		};
		return events;
	}
	case G_TEXT_FIELD:
	{
		static G_MSG_STRUCT events[] =
		{
		{G_USER_TYPE,	NOTIFY, 	G_VOID_TYPE},
		{G_USER_TYPE,	ANYEVENT,	G_VOID_TYPE},
			0,
		};
		return events;
	}
	case G_TEXT_PANE:
	{
		static G_MSG_STRUCT events[] =
		{
		{G_USER_TYPE,	MOUSE_SELECT, 	G_VOID_TYPE},
		{G_USER_TYPE,	MOUSE_ADJUST, 	G_VOID_TYPE},
		{G_USER_TYPE,	MOUSE_MENU, 	G_VOID_TYPE},
		{G_USER_TYPE,	ANYEVENT,	G_VOID_TYPE},
		{G_USER_TYPE,	DOUBLECLICK,	G_VOID_TYPE},
		{G_USER_TYPE,	ENTER,		G_VOID_TYPE},
		{G_USER_TYPE,	EXIT,		G_VOID_TYPE},
		{G_USER_TYPE,	KEYPRESS,	G_VOID_TYPE},
			0,
		};
		return events;
	}
	default:
	{
#ifdef linux
                static G_MSG_STRUCT events[] = { 0 };
#else
	        static G_MSG_STRUCT events[] = 0;
#endif
		return events;
	}
	}
}



/*
 * Return a list of valid messages given an object type.
 */ 
G_MSG_STRUCT	*
#ifdef __STDC__
G_default_messages(G_TYPES type)
#else
G_default_messages(type)
	G_TYPES		type;
#endif
{
	switch (type)
	{
	case G_BASE_WINDOW:
	{
		static G_MSG_STRUCT actions[] =
		{
		{G_USER_TYPE,		SHOW,		G_VOID_TYPE},
		{G_USER_TYPE,		HIDE,		G_VOID_TYPE},
		{G_USER_TYPE,		SETLABEL,	G_STRING_TYPE},
		{G_USER_TYPE,		GETLABEL,	G_VOID_TYPE},
		{G_USER_TYPE,		SETLEFTFOOTER,	G_STRING_TYPE},
		{G_USER_TYPE,		GETLEFTFOOTER,	G_VOID_TYPE},
		{G_USER_TYPE,		SETRIGHTFOOTER,	G_STRING_TYPE},
		{G_USER_TYPE,		GETRIGHTFOOTER, G_VOID_TYPE},
		{G_FUNCTION_TYPE,	CALLFUNCTION,	G_VOID_TYPE},
		{G_CODE_TYPE,		EXECUTECODE,	G_STRING_TYPE},
			0,
		};
		return actions;
	}
	case G_BUTTON:
	{
		static G_MSG_STRUCT actions[] =
		{
		{G_USER_TYPE,		SHOW,		G_VOID_TYPE},
		{G_USER_TYPE,		HIDE,		G_VOID_TYPE},
		{G_USER_TYPE,		ENABLE,		G_VOID_TYPE},
		{G_USER_TYPE,		DISABLE,	G_VOID_TYPE},
		{G_USER_TYPE,		SETLABEL,	G_STRING_TYPE},
		{G_USER_TYPE,		GETLABEL,	G_VOID_TYPE},
		{G_FUNCTION_TYPE,	CALLFUNCTION,	G_VOID_TYPE},
		{G_CODE_TYPE,		EXECUTECODE,	G_STRING_TYPE},
			0,
		};
		return actions;
	}
	case G_CANVAS_PANE:
	{
		static G_MSG_STRUCT actions[] =
		{
		{G_USER_TYPE,		SHOW,		G_VOID_TYPE},
		{G_USER_TYPE,		HIDE,		G_VOID_TYPE},
		{G_FUNCTION_TYPE,	CALLFUNCTION,	G_VOID_TYPE},
		{G_CODE_TYPE,		EXECUTECODE,	G_STRING_TYPE},
			0,
		};
		return actions;
	}
	case G_CONTROL_AREA:
	{
		static G_MSG_STRUCT actions[] =
		{
		{G_USER_TYPE,		SHOW,		G_VOID_TYPE},
		{G_USER_TYPE,		HIDE,		G_VOID_TYPE},
		{G_FUNCTION_TYPE,	CALLFUNCTION,	G_VOID_TYPE},
		{G_CODE_TYPE,		EXECUTECODE,	G_STRING_TYPE},
			0,
		};
		return actions;
	}
	case G_DROP_TARGET:
	{
		static G_MSG_STRUCT actions[] =
		{
		{G_FUNCTION_TYPE,	CALLFUNCTION,	G_VOID_TYPE},
			0,
		};
		return actions;
	}
	case G_GAUGE:
	{
		static G_MSG_STRUCT actions[] =
		{
		{G_USER_TYPE,		SHOW,		G_VOID_TYPE},
		{G_USER_TYPE,		HIDE,		G_VOID_TYPE},
		{G_USER_TYPE,		ENABLE,		G_VOID_TYPE},
		{G_USER_TYPE,		DISABLE,	G_VOID_TYPE},
		{G_USER_TYPE,		SETVALUENUMBER, G_STRING_TYPE},
		{G_USER_TYPE,		GETVALUENUMBER, G_VOID_TYPE},
		{G_USER_TYPE,		SETLABEL,	G_STRING_TYPE},
		{G_USER_TYPE,		GETLABEL,	G_VOID_TYPE},
		{G_FUNCTION_TYPE,	CALLFUNCTION,	G_VOID_TYPE},
		{G_CODE_TYPE,		EXECUTECODE,	G_STRING_TYPE},
			0,
		};
		return actions;
	}
	case G_GROUP:
	{
		static G_MSG_STRUCT actions[] =
		{
		{G_USER_TYPE,		SHOW,		G_VOID_TYPE},
		{G_USER_TYPE,		HIDE,		G_VOID_TYPE},
		{G_USER_TYPE,		ENABLE,		G_VOID_TYPE},
		{G_USER_TYPE,		DISABLE,	G_VOID_TYPE},
			0,
		};
		return actions;
	}
	case G_MENU:
	{
		static G_MSG_STRUCT actions[] =
		{
		{G_USER_TYPE,		SHOW,		G_VOID_TYPE},
		{G_FUNCTION_TYPE,	CALLFUNCTION,	G_VOID_TYPE},
		{G_CODE_TYPE,		EXECUTECODE,	G_STRING_TYPE},
			0,
		};
		return actions;
	}
	case G_MENU_ITEMS:
	{
		static G_MSG_STRUCT actions[] =
		{
		{G_FUNCTION_TYPE,	CALLFUNCTION,	G_VOID_TYPE},
		{G_CODE_TYPE,		EXECUTECODE,	G_STRING_TYPE},
			0,
		};
		return actions;
	}
	case G_MESSAGE:
	{
		static G_MSG_STRUCT actions[] =
		{
		{G_USER_TYPE,		SHOW,		G_VOID_TYPE},
		{G_USER_TYPE,		HIDE,		G_VOID_TYPE},
		{G_USER_TYPE,		ENABLE,		G_VOID_TYPE},
		{G_USER_TYPE,		DISABLE,	G_VOID_TYPE},
		{G_USER_TYPE,		SETLABEL, 	G_STRING_TYPE},
		{G_USER_TYPE,		GETLABEL, 	G_VOID_TYPE},
		{G_FUNCTION_TYPE,	CALLFUNCTION,	G_VOID_TYPE},
		{G_CODE_TYPE,		EXECUTECODE,	G_STRING_TYPE},
			0,
		};
		return actions;
	}
	case G_POPUP_WINDOW:
	{
		static G_MSG_STRUCT actions[] =
		{
		{G_USER_TYPE,		SHOW,		G_VOID_TYPE},
		{G_USER_TYPE,		HIDE,		G_VOID_TYPE},
		{G_USER_TYPE,		SETLABEL,	G_STRING_TYPE},
		{G_USER_TYPE,		GETLABEL,	G_VOID_TYPE},
		{G_USER_TYPE,		SETLEFTFOOTER,	G_STRING_TYPE},
		{G_USER_TYPE,		GETLEFTFOOTER,	G_VOID_TYPE},
		{G_USER_TYPE,		SETRIGHTFOOTER,	G_STRING_TYPE},
		{G_USER_TYPE,		GETRIGHTFOOTER, G_VOID_TYPE},
		{G_FUNCTION_TYPE,	CALLFUNCTION,	G_VOID_TYPE},
		{G_CODE_TYPE,		EXECUTECODE,	G_STRING_TYPE},
			0,
		};
		return actions;
	}
	case G_SCROLLING_LIST:
	{
		static G_MSG_STRUCT actions[] =
		{
		{G_USER_TYPE,		SHOW,		G_VOID_TYPE},
		{G_USER_TYPE,		HIDE,		G_VOID_TYPE},
		{G_USER_TYPE,		ENABLE,		G_VOID_TYPE},
		{G_USER_TYPE,		DISABLE,	G_VOID_TYPE},
		{G_USER_TYPE,		SETLABEL,	G_STRING_TYPE},
		{G_USER_TYPE,		GETLABEL,	G_VOID_TYPE},
		{G_FUNCTION_TYPE,	CALLFUNCTION,	G_VOID_TYPE},
		{G_CODE_TYPE,		EXECUTECODE,	G_STRING_TYPE},
			0,
		};
		return actions;
	}
	case G_SETTING:
	{
		static G_MSG_STRUCT actions[] =
		{
		{G_USER_TYPE,		SHOW,		G_VOID_TYPE},
		{G_USER_TYPE,		HIDE,		G_VOID_TYPE},
		{G_USER_TYPE,		ENABLE,		G_VOID_TYPE},
		{G_USER_TYPE,		DISABLE,	G_VOID_TYPE},
		{G_USER_TYPE,		SETLABEL,	G_STRING_TYPE},
		{G_USER_TYPE,		GETLABEL,	G_VOID_TYPE},
		{G_FUNCTION_TYPE,	CALLFUNCTION,	G_VOID_TYPE},
		{G_CODE_TYPE,		EXECUTECODE,	G_STRING_TYPE},
			0,
		};
		return actions;
	}
	case G_SETTING_ITEMS:
	{
		static G_MSG_STRUCT actions[] =
		{
		{G_FUNCTION_TYPE,	CALLFUNCTION,	G_VOID_TYPE},
		{G_CODE_TYPE,		EXECUTECODE,	G_STRING_TYPE},
			0,
		};
		return actions;
	}
	case G_SLIDER:
	{
		static G_MSG_STRUCT actions[] =
		{
		{G_USER_TYPE,		SHOW,		G_VOID_TYPE},
		{G_USER_TYPE,		HIDE,		G_VOID_TYPE},
		{G_USER_TYPE,		ENABLE,		G_VOID_TYPE},
		{G_USER_TYPE,		DISABLE,	G_VOID_TYPE},
		{G_USER_TYPE,		SETVALUENUMBER,	G_INT_TYPE},
		{G_USER_TYPE,		GETVALUENUMBER,	G_VOID_TYPE},
		{G_USER_TYPE,		SETLABEL,	G_STRING_TYPE},
		{G_USER_TYPE,		GETLABEL,	G_VOID_TYPE},
		{G_FUNCTION_TYPE,	CALLFUNCTION,	G_VOID_TYPE},
		{G_CODE_TYPE,		EXECUTECODE,	G_STRING_TYPE},
			0,
		};
		return actions;
	}
	case G_TERM_PANE:
	{
		static G_MSG_STRUCT actions[] = 
		{
		{G_USER_TYPE,		SHOW,		G_VOID_TYPE},
		{G_USER_TYPE,		HIDE,		G_VOID_TYPE},
			0,
		};
		return actions;
	}
	case G_TEXT_FIELD:
	{
		static G_MSG_STRUCT actions[] =
		{
		{G_USER_TYPE,		SHOW,		G_VOID_TYPE},
		{G_USER_TYPE,		HIDE,		G_VOID_TYPE},
		{G_USER_TYPE,		ENABLE,		G_VOID_TYPE},
		{G_USER_TYPE,		DISABLE,	G_VOID_TYPE},
		{G_USER_TYPE,		SETVALUESTRING, G_STRING_TYPE},
		{G_USER_TYPE,		GETVALUESTRING,	G_VOID_TYPE},
		{G_USER_TYPE,		SETVALUENUMBER, G_INT_TYPE},
		{G_USER_TYPE,		GETVALUENUMBER,	G_VOID_TYPE},
		{G_USER_TYPE,		SETLABEL,	G_STRING_TYPE},
		{G_USER_TYPE,		GETLABEL,	G_VOID_TYPE},
		{G_FUNCTION_TYPE,	CALLFUNCTION,	G_VOID_TYPE},
		{G_CODE_TYPE,		EXECUTECODE,	G_STRING_TYPE},
			0,
		};
		return actions;
	}
	case G_TEXT_PANE:
	{
		static G_MSG_STRUCT actions[] =
		{
		{G_USER_TYPE,		SHOW,		G_VOID_TYPE},
		{G_USER_TYPE,		HIDE,		G_VOID_TYPE},
		{G_USER_TYPE,		LOADTEXTFILE,	G_STRING_TYPE},
		{G_FUNCTION_TYPE,	CALLFUNCTION,	G_VOID_TYPE},
		{G_CODE_TYPE,		EXECUTECODE,	G_STRING_TYPE},
			0,
		};
		return actions;
	}
	default:
	{
#ifdef linux
                static G_MSG_STRUCT actions[] = { 0 };
#else
		static G_MSG_STRUCT actions[] = 0;
#endif
		return actions;
	}
	}
}

/*
 * Return a list of valid receivers given an object type.
 */
G_TYPES	*
#ifdef __STDC__
G_default_receivers(G_TYPES type)
#else
G_default_receivers(type)
	G_TYPES		type;
#endif
{
	switch (type)
	{
	case G_BASE_WINDOW:
	{
		static G_TYPES types[] =
		{
			G_BASE_WINDOW,
			-1,
		};
		return types;
	}
	case G_BUTTON:
	{
		static G_TYPES types[] =
		{
			G_BASE_WINDOW,
			G_BUTTON,
			G_CANVAS_PANE,
			G_CONTROL_AREA,
			G_GAUGE,
			G_GROUP,
			G_MENU,
			G_MENU_ITEMS,
			G_MESSAGE,
			G_POPUP_WINDOW,
			G_SCROLLING_LIST,
			G_SETTING,
			G_SETTING_ITEMS,
			G_SLIDER,
			G_TERM_PANE,
			G_TEXT_FIELD,
			G_TEXT_PANE,
			-1,
		};
		return types;
	}
	case G_CANVAS_PANE:
	{
		static G_TYPES types[] =
		{
			G_CANVAS_PANE,
			G_MENU,
			-1,
		};
		return types;
	}
	case G_CONTROL_AREA:
	{
		static G_TYPES types[] =
		{
			G_CONTROL_AREA,
			G_MENU,
			-1,
		}; 
		return types;
	}
	case G_DROP_TARGET:
	{
		static G_TYPES types[] =
		{
			G_DROP_TARGET,
			-1,
		}; 
		return types;
	}
	case G_GAUGE:
	{
		static G_TYPES types[] =
		{
			G_BASE_WINDOW,
			G_BUTTON,
			G_CANVAS_PANE,
			G_CONTROL_AREA,
			G_GAUGE,
			G_GROUP,
			G_MESSAGE,
			G_POPUP_WINDOW,
			G_SCROLLING_LIST,
			G_SLIDER,
			G_TERM_PANE,
			G_TEXT_FIELD,
			G_TEXT_PANE,
			-1,
		};
		return types;
	}
	case G_GROUP:
	{
		static G_TYPES types[] =
		{
			G_BASE_WINDOW,
                        G_BUTTON,
                        G_CANVAS_PANE,
                        G_CONTROL_AREA,
                        G_GAUGE,
                        G_GROUP,
                        G_MENU,
                        G_MENU_ITEMS,
                        G_MESSAGE,
                        G_POPUP_WINDOW,
                        G_SCROLLING_LIST,
                        G_SETTING,
                        G_SETTING_ITEMS,
                        G_SLIDER,
                        G_TERM_PANE,
                        G_TEXT_FIELD,
                        G_TEXT_PANE,
                        -1,
                };
	}	
	case G_MENU:
	{
		static G_TYPES types[] =
		{ 
			G_BASE_WINDOW,
			G_BUTTON,
			G_CANVAS_PANE,
			G_CONTROL_AREA,
			G_GAUGE,
			G_GROUP,
			G_MENU,
			G_MENU_ITEMS,
			G_MESSAGE,
			G_POPUP_WINDOW,
			G_SCROLLING_LIST,
			G_SETTING,
			G_SETTING_ITEMS,
			G_SLIDER,
			G_TERM_PANE,
			G_TEXT_FIELD,
			G_TEXT_PANE,
			-1,
		};
		return types;
	}
	case G_MENU_ITEMS:
	{
		static G_TYPES types[] =
		{ 
			G_BASE_WINDOW,
			G_BUTTON,
			G_CANVAS_PANE,
			G_CONTROL_AREA,
			G_GAUGE,
			G_GROUP,
			G_MENU,
			G_MENU_ITEMS,
			G_MESSAGE,
			G_POPUP_WINDOW,
			G_SCROLLING_LIST,
			G_SETTING,
			G_SETTING_ITEMS,
			G_SLIDER,
			G_TERM_PANE,
			G_TEXT_FIELD,
			G_TEXT_PANE,
			-1,
		};
		return types;
	}
	case G_MESSAGE:
	{
		static G_TYPES types[] =
		{
			G_MESSAGE,
			-1,
		}; 
		return types;
	}
	case G_POPUP_WINDOW: 
	{
		static G_TYPES types[] =
		{
			G_POPUP_WINDOW,
			-1,
		}; 
		return types;
	}
	case G_SCROLLING_LIST: 
	{
		static G_TYPES types[] =
		{ 
			G_BASE_WINDOW,
			G_BUTTON,
			G_CANVAS_PANE,
			G_CONTROL_AREA,
			G_GAUGE,
			G_GROUP,
			G_MENU,
			G_MENU_ITEMS,
			G_MESSAGE,
			G_POPUP_WINDOW,
			G_SCROLLING_LIST,
			G_SETTING,
			G_SETTING_ITEMS,
			G_SLIDER,
			G_TERM_PANE,
			G_TEXT_FIELD,
			G_TEXT_PANE,
			-1,
		};
		return types;
	}
	case G_SETTING: 
	{
		static G_TYPES types[] =
		{ 
			G_BASE_WINDOW,
			G_BUTTON,
			G_CANVAS_PANE,
			G_CONTROL_AREA,
			G_GAUGE,
			G_GROUP,
			G_MESSAGE,
			G_POPUP_WINDOW,
			G_SCROLLING_LIST,
			G_SETTING,
			G_SETTING_ITEMS,
			G_SLIDER,
			G_TERM_PANE,
			G_TEXT_FIELD,
			G_TEXT_PANE,
			-1,
		};
		return types;
	}
	case G_SETTING_ITEMS: 
	{
		static G_TYPES types[] =
		{ 
			G_BASE_WINDOW,
			G_BUTTON,
			G_CANVAS_PANE,
			G_CONTROL_AREA,
			G_GAUGE,
			G_GROUP,
			G_MENU_ITEMS,
			G_MESSAGE,
			G_POPUP_WINDOW,
			G_SCROLLING_LIST,
			G_SETTING,
			G_SETTING_ITEMS,
			G_SLIDER,
			G_TERM_PANE,
			G_TEXT_FIELD,
			G_TEXT_PANE,
			-1,
		};
		return types;
	}
	case G_SLIDER: 
	{
		static G_TYPES types[] =
		{ 
			G_BASE_WINDOW,
			G_BUTTON,
			G_CANVAS_PANE,
			G_CONTROL_AREA,
			G_GAUGE,
			G_GROUP,
			G_MESSAGE,
			G_POPUP_WINDOW,
			G_SCROLLING_LIST,
			G_SETTING,
			G_SETTING_ITEMS,
			G_SLIDER,
			G_TERM_PANE,
			G_TEXT_FIELD,
			G_TEXT_PANE,
			-1,
		};
		return types;
	}
	case G_TERM_PANE:
	{
		static G_TYPES types[] =
		{
			G_TERM_PANE,
			-1,
		}; 
		return types;
	}
	case G_TEXT_FIELD: 
	{
		static G_TYPES types[] =
		{ 
			G_BASE_WINDOW,
			G_BUTTON,
			G_CANVAS_PANE,
			G_CONTROL_AREA,
			G_GAUGE,
			G_GROUP,
			G_MESSAGE,
			G_POPUP_WINDOW,
			G_SCROLLING_LIST,
			G_SETTING,
			G_SETTING_ITEMS,
			G_SLIDER,
			G_TERM_PANE,
			G_TEXT_FIELD,
			G_TEXT_PANE,
			-1,
		};
		return types;
	}
	case G_TEXT_PANE: 
	{
		static G_TYPES types[] =
		{
			G_TEXT_PANE,
			-1,
		}; 
		return types;
	}
	default: 
	{
#ifdef linux
	        static G_TYPES types[] = { -1 };
#else
	        static G_TYPES types[] = -1; 
#endif
		return types;
	}
	}
}


