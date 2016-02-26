%{
/* Copyright */

#include <stdio.h>
#include <stdlib.h>
int tokval;
%}

%token GIL_HDR LEFT RIGHT QSTRING OBJ_NAME NUMBER

%token ACTION ACTIONS ANCHOR_OBJECT ANCHOR_POINT ARG_TYPE
%token BACKGROUND_COLOR BUTTON_TYPE CHOICE_COLORS CHOICE_LABEL_TYPES CHOICES
%token COLUMN_ALIGNMENT COLUMNS CONSTANT_WIDTH DEFAULT_DROP_SITE DONE_HANDLER
%token DROPPABLE EVENT_HANDLER FOREGROUND_COLOR FROM FUNCTION_TYPE GROUP_TYPE
%token HEIGHT HELP HORIZONTAL_OFFSET HORIZONTAL_SPACING ICON_FILE ICON_LABEL
%token ICON_MASK_FILE INITIAL_LIST_GLYPHS INITIAL_LIST_VALUES
%token INITIAL_SELECTIONS INITIAL_STATE INITIAL_VALUE LABEL LABEL_TYPE
%token LAYOUT_TYPE MAX_VALUE MEMBERS MENU MIN_VALUE MULTIPLE_SELECTIONS NAME
%token NOTIFY_HANDLER OWNER PINNED READ_ONLY REFERENCE_POINT RESIZABLE
%token ROW_ALIGNMENT ROWS SELECTION_REQUIRED SETTING_TYPE SHOW_BORDER
%token SHOW_FOOTER STORED_LENGTH TEXT_TYPE TITLE TO TYPE USER_DATA
%token VALUE_LENGTH VALUE_UNDERLINED VALUE_X VALUE_Y VERTICAL_OFFSET
%token VERTICAL_SPACING WHEN WIDTH X Y

%token ACTIVE ALPHANUMERIC BASE_WINDOW BOTTOM_EDGES BUTTON COLUMN
%token CONTROL_AREA EXCLUSIVE GROUP HORIZONTAL HORIZONTAL_CENTERS INACTIVE
%token INVISIBLE LABELS LEFT_EDGES NIL NORMAL NORTH_WEST NUMERIC OPEN 
%token POPUP_WINDOW
%token ROW SCROLLING_LIST SETTING STRING 
%token TEXT_FIELD TEXT_PANE TOP_EDGES TRUE
%token USER_DEFINED VERTICAL_CENTERS VISIBLE

%%

list		: /* empty */
		| list GIL_HDR
			{ fprintf(stderr, "GIL header\n"); }
		  LEFT statmnts RIGHT 
			{ fprintf(stderr, "Rest of GIL\n"); }
		| list error 
			{ fprintf(stderr, "Error action\n"); exit(1); }
		;

statmnts	: LEFT baseframe RIGHT
		| LEFT window RIGHT
		;

baseframe	: baseframe bf_element
		| bf_element
		;

bf_element	: TYPE                   BASE_WINDOW
			{ fprintf(stderr, "Got a base window\n"); }
		| NAME                   object
			{ fprintf(stderr, "Got baseframe %s\n", (char*)tokval); }
		| OWNER                  object
		| WIDTH                  NUMBER
		| HEIGHT                 NUMBER
		| BACKGROUND_COLOR       QSTRING
			{ fprintf(stderr, "BG_COLOR %s\n", (char*)tokval); }
		| FOREGROUND_COLOR       QSTRING
		| LABEL                  QSTRING
		| LABEL_TYPE             STRING
		| INITIAL_STATE          OPEN
		| SHOW_FOOTER            boolean
		| RESIZABLE              boolean
		| ICON_FILE              QSTRING
		| ICON_LABEL             QSTRING
		| ICON_MASK_FILE         QSTRING
		| EVENT_HANDLER          object
		| USER_DATA              LEFT RIGHT
		| ACTIONS                LEFT RIGHT
		;

window		: NIL
		;

object		: OBJ_NAME
		| NIL
		;

boolean		: NIL | TRUE ;

%%

/* Print message with context, and abort. All yyerrors are fatal, OK? */
void
yyerror(char *msg)
{
	extern int yylineno;

	fprintf(stderr, "%s: line %d: %s\n", "gil2bil", yylineno, msg);
	exit(1);
}
