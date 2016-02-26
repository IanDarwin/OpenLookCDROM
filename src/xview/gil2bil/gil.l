%e 10000
%n 1000
%p 5000
%%
%{
/* Parser for GIL files. */
#include <stdlib.h>
#include "x.tab.h"
extern int tokval;
%}
\;GIL-3		return GIL_HDR;
\(		{ return LEFT; }
\)		{ return RIGHT; }
\"[^"]*\"		{ 
		if (yytext[yyleng-1] == '\\')
			yymore();
		else {
			tokval = (int)strdup(yytext);
			return QSTRING;
		}
		}

nil		{ return NIL; }
t[^a-zA-Z_]	{ return TRUE; }
[a-zA-Z_]+	{ tokval = (int)strdup(yytext); return OBJ_NAME; }
[0-9]+		{ tokval = atoi(yytext); return NUMBER; }

[ 	\n]	;

:action			return ACTION;
:actions		return ACTIONS;
:anchor-object		return ANCHOR_OBJECT;
:anchor-point		return ANCHOR_POINT;
:arg_type		return ARG_TYPE;
:background-color	return BACKGROUND_COLOR;
:button-type		return BUTTON_TYPE;
:choice-colors		return CHOICE_COLORS;
:choice-label-types	return CHOICE_LABEL_TYPES;
:choices		return CHOICES;
:column-alignment	return COLUMN_ALIGNMENT;
:columns		return COLUMNS;
:constant-width		return CONSTANT_WIDTH;
:default-drop-site	return DEFAULT_DROP_SITE;
:done-handler		return DONE_HANDLER;
:droppable		return DROPPABLE;
:event-handler		return EVENT_HANDLER;
:foreground-color	return FOREGROUND_COLOR;
:from			return FROM;
:function_type		return FUNCTION_TYPE;
:group-type		return GROUP_TYPE;
:height			return HEIGHT;
:help			return HELP;
:horizontal-offset	return HORIZONTAL_OFFSET;
:horizontal-spacing	return HORIZONTAL_SPACING;
:icon-file		return ICON_FILE;
:icon-label		return ICON_LABEL;
:icon-mask-file		return ICON_MASK_FILE;
:initial-list-glyphs	return INITIAL_LIST_GLYPHS;
:initial-list-values	return INITIAL_LIST_VALUES;
:initial-selections	return INITIAL_SELECTIONS;
:initial-state		return INITIAL_STATE;
:initial-value		return INITIAL_VALUE;
:label			return LABEL;
:label-type		return LABEL_TYPE;
:layout-type		return LAYOUT_TYPE;
:max-value		return MAX_VALUE;
:members		return MEMBERS;
:menu			return MENU;
:min-value		return MIN_VALUE;
:multiple-selections	return MULTIPLE_SELECTIONS;
:name			return NAME;
:notify-handler		return NOTIFY_HANDLER;
:owner			return OWNER;
:pinned			return PINNED;
:read-only		return READ_ONLY;
:reference-point	return REFERENCE_POINT;
:resizable		return RESIZABLE;
:row-alignment		return ROW_ALIGNMENT;
:rows			return ROWS;
:selection-required	return SELECTION_REQUIRED;
:setting-type		return SETTING_TYPE;
:show-border		return SHOW_BORDER;
:show-footer		return SHOW_FOOTER;
:stored-length		return STORED_LENGTH;
:text-type		return TEXT_TYPE;
:title			return TITLE;
:to			return TO;
:type			return TYPE;
:user-data		return USER_DATA;
:value-length		return VALUE_LENGTH;
:value-underlined	return VALUE_UNDERLINED;
:value-x		return VALUE_X;
:value-y		return VALUE_Y;
:vertical-offset	return VERTICAL_OFFSET;
:vertical-spacing	return VERTICAL_SPACING;
:when			return WHEN;
:width			return WIDTH;
:x			return X;
:y			return Y;

:active			return ACTIVE;
:alphanumeric		return ALPHANUMERIC;
:base-window		return BASE_WINDOW;
:bottom-edges		return BOTTOM_EDGES;
:button			return BUTTON;
:column			return COLUMN;
:control-area		return CONTROL_AREA;
:exclusive		return EXCLUSIVE;
:group			return GROUP;
:horizontal		return HORIZONTAL;
:horizontal-centers	return HORIZONTAL_CENTERS;
:inactive		return INACTIVE;
:invisible		return INVISIBLE;
:labels			return LABELS;
:left-edges		return LEFT_EDGES;
:normal			return NORMAL;
:north-west		return NORTH_WEST;
:numeric		return NUMERIC;
:open			return OPEN;
:popup-window		return POPUP_WINDOW;
:row			return ROW;
:scrolling-list		return SCROLLING_LIST;
:setting		return SETTING;
:string			return STRING;
:text-field		return TEXT_FIELD;
:text-pane		return TEXT_PANE;
:top-edges		return TOP_EDGES;
:user_defined		return USER_DEFINED;
:vertical-centers	return VERTICAL_CENTERS;
:visible		return VISIBLE;

