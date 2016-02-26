/* (C) Universitaet Passau 1986-1991 */
/*************************************************************************
**									**
**	MODULNAME:	termgraph_window.c				**
**									**
**************************************************************************
**									**
**	ERSTELLUNG:	Rudolf Seisenberger, 	1990			**
**									**
**	AUFGABEN:	In diesem Modul wird die Benutzeroberflaeche 	**
**			erzeugt und verwaltet. Die Algorithmen 		**
**			selbst befinden sich teilweise auch in 		**
**			den anderen termgraph_... -Dateien.		**
**									**
*************************************************************************/

/*************************************************************************
**									**
**				FUNKTIONEN				**
**									**
**************************************************************************
**									**
**	Extern definierte TermGraph-Prozeduren sind:			**
**									**
**	extern term_to_graph_creation_proc ()				** 
**	extern termgraph_zoom_proc ()					**
**	extern termgraph_scroll_proc ()					**
**	extern draw_proc()						**
**	extern void call_petrinet_turn_proc()				**
**	extern void main_several_token_proc()				**
**	extern int main_proc_token()					**
**									**
**	Alle anderen Prozeduren sind in diesem Modul enthalten, sofern	**
**	es sich nicht um GraphEd- oder sunview-Funktionen handelt.	**
**									**
*************************************************************************/

/*************************************************************************
**									**
**			    GLOBALE VARIABLE		 		**
**									**
**************************************************************************
**									**
**	Xv_Window base_frame; die einzige Variable aus GraphEd		**
**									**
**	Die meisten globalen Variablen der Applikation TermGraph 	**
**	werden hier verwendet. 						**
**									**
*************************************************************************/


#include "termgraph_decl.h"

extern base_frame; 

extern term_to_graph_creation_proc ();
extern termgraph_zoom_proc ();
extern termgraph_scroll_proc ();
extern term_main();
extern void call_petrinet_turn_proc(), main_several_token_proc();
extern int main_proc_token();

extern Sgraph_proc_info termglob_info;

Frame	termgraph_frame = (Frame)NULL;
Panel	termgraph_header_panel, termgraph_panel, termgraph_param_panel;
Textsw	termgraph_text;
Tty	termgraph_tty;
Panel_item termgraph_fname_item, termgraph_size_item, termgraph_header_item,
	termgraph_param_item1, termgraph_load_button_item, termgraph_store_button_item,
	termgraph_set_basic_item, termgraph_set_misc_item, termgraph_misc_button,
	termgraph_transx_item, termgraph_transy_item, termgraph_placex_item,
	termgraph_placey_item, termgraph_edgeline_button;
char 	termglob_agent_name[MAXSTRING],
	termglob_agent_stdin[MAXSTRING],
	termglob_agent_dir[MAXSTRING];
	
extern int termgraph_basic_length;
extern int ELEMENT_LENGTH;
extern int termgraph_transx_length;
extern int termgraph_transy_length;
extern int termgraph_placex_length;
extern int termgraph_placey_length;
extern int termgraph_transition_color;
extern int termgraph_place_color;
extern int termgraph_edge_color;
extern int termgraph_transition_font;
extern int termgraph_place_font;
extern int termgraph_edge_font;
extern int termgraph_transition_nlv;
extern int termgraph_place_nlv;
extern int termgraph_edge_elv;
extern int termgraph_arrow_length;
extern float termgraph_arrow_angle;
extern int termgraph_initial_x;
extern int termgraph_initial_y;

extern char termgraph_default_agent_name[MAXNAME];
extern int termgraph_window_exists;
extern float termgraph_zoom_value;
extern int termgraph_misc_item_value;
extern int termgraph_only_defaults;
extern int termgraph_edgeline_type;
extern int termgraph_nodes_sum;


static void ls_proc(), select_file_proc(), quit_proc(), dirset_proc(), empty_proc(),
            show_proc (), chdir_proc(), string_proc(), clear_proc(), 
            termgraph_window_choice(), termgraph_header_choice(), termgraph_info_proc(),
            load_agent_proc(), quick_store_agent_proc(), store_agent_proc(), draw_proc(), 
            termgraph_call_zoom_proc(), termgraph_call_scroll_proc(),
            token_proc(), several_token_proc(), done_token_proc(), 
            termgraph_done_proc(), termgraph_set_misc_choice(), termgraph_misc_set_proc(),
            termgraph_set_basic_choice(),
            termgraph_set_transx_choice(), 
            termgraph_set_transy_choice(), 
            termgraph_set_placex_choice(),
            termgraph_set_placey_choice(),
            termgraph_what_edgeline(), turn_proc(), cls_str(),
            termgraph_load_values(), termgraph_save_values();
char	    *termgraph_get_line();

int termgraph_window_main (info)
Sgraph_proc_info	info;
{
	char cmdstring[MAXBUF];
	FILE *default_agent_file;
	
	
	if ((Frame)NULL == 
	(termgraph_frame = (Frame)xv_create(base_frame, FRAME,
			WIN_ERROR_MSG, 	"*** Couldn't create the TermGraph/Command window ***",
			XV_WIDTH,  		500,
			XV_HEIGHT,		200,
			FRAME_INHERIT_COLORS,	TRUE,		
			XV_LABEL,		"TermGraph   -   (Version 1.0)",
			FRAME_SHOW_LABEL,	TRUE,
			FRAME_DONE_PROC,	quit_proc,
			WIN_SHOW,		FALSE,
			NULL))) {
		message ("*** Couldn't create the TermGraph/Command window ***\n");
		return 0;
	}
	
	if ((Panel)NULL ==
	(termgraph_header_panel = (Panel)xv_create(termgraph_frame, PANEL,
			WIN_SHOW,	TRUE,
			WIN_ERROR_MSG, 	"*** Couldn't create the TermGraph/Command window ***",
			NULL))) {
		message ("*** Couldn't create the TermGraph/Command window ***\n");
		return 0;
	}
	
	if ((Panel)NULL ==
	(termgraph_panel = (Panel)xv_create(termgraph_frame, PANEL,
			WIN_Y,		30,
			WIN_SHOW,	TRUE,
			WIN_MOUSE_XY,	200+ (int)xv_get(termgraph_frame,WIN_X,0),
					50+ (int)xv_get(termgraph_frame,WIN_Y,0),
			WIN_ERROR_MSG, 	"*** Couldn't create the TermGraph/command window ***",
			NULL))) {
		message ("*** Couldn't create the TermGraph/command window ***\n");
		return 0;
	}
	
	if ((Panel)NULL ==
	(termgraph_param_panel = (Panel)xv_create(termgraph_frame, PANEL,
			WIN_Y,	30,
			WIN_SHOW,	FALSE,
			WIN_MOUSE_XY,	200+ (int)xv_get(termgraph_frame,WIN_X,0),
					50+ (int)xv_get(termgraph_frame,WIN_Y,0),
			WIN_ERROR_MSG, 	"*** Couldn't create the TermGraph/Parameter window ***",
			NULL))) {
		message ("*** Couldn't create the TermGraph/Parameter window ***\n");
		return 0;
	}

	create_panel_items ();
	
	if ((Tty)NULL ==
	(termgraph_tty = (Tty)xv_create(termgraph_frame, TTY,
			WIN_Y,		120,
			WIN_SHOW,	TRUE,
			WIN_ERROR_MSG, 	"*** Couldn't create the TermGraph/Tty window ***",
			NULL))) {
		message ("*** Couldn't create the TermGraph/Tty window ***");
		return 0;
	}
		
	if (termgraph_default_agent_name[0] == '\0') {
		strcpy (termgraph_default_agent_name, DEFAULT_AGENT_FILE);
	}	
	if ((default_agent_file = fopen( termgraph_default_agent_name, "r" )) == NULL) {
		default_agent_file = fopen( termgraph_default_agent_name, "w" );
	}
	fclose(default_agent_file);
	
	if ((Textsw)NULL ==
	(termgraph_text = (Textsw)xv_create(termgraph_frame, TEXTSW,
			WIN_Y,				30,
			WIN_SHOW,			FALSE,
			XV_LABEL,			"   Agent Editor",
			FRAME_SHOW_LABEL,		TRUE,
			TEXTSW_FILE,			termgraph_default_agent_name,
			WIN_ERROR_MSG, 	"*** Couldn't create the TermGraph Texteditor ***",
			NULL))) {
		message ("*** Couldn't create the TermGraph Texteditor ***");
		return 0;
	} else {
	(void)xv_create(termgraph_text, SCROLLBAR,
			SCROLLBAR_DIRECTION,		SCROLLBAR_VERTICAL,
			NULL);
	}
					
	sprintf (cmdstring, "set prompt = \"TermGraph(\\!): $cwd:t%% \"; clear\n");
	ttysw_input (termgraph_tty, cmdstring, strlen(cmdstring));
	cls_str(cmdstring);
			
	xv_set(termgraph_frame, WIN_SHOW, TRUE, NULL);
	
	return 1;		
}

create_panel_items (info)
Sgraph_proc_info	info;
{
	char cmdstring[MAXBUF], str[MAXNAME];

	termgraph_header_item = xv_create(termgraph_header_panel, PANEL_CYCLE,
		PANEL_CHOICE_STRINGS,	"Commands",
					"Agent Editor",
					"Redaction",
					"Close",
					NULL,
		PANEL_NOTIFY_PROC,	termgraph_header_choice,
		NULL);

	termgraph_load_button_item = xv_create(termgraph_header_panel, PANEL_BUTTON,
		XV_SHOW,		FALSE,
		PANEL_NOTIFY_PROC,	load_agent_proc,
		PANEL_LABEL_STRING,	"Load",
		NULL);

	termgraph_store_button_item = xv_create(termgraph_header_panel, PANEL_BUTTON,
		XV_SHOW,		FALSE,
		PANEL_LABEL_STRING,	"Store to",
		PANEL_NOTIFY_PROC,	store_agent_proc,
		NULL);

	termgraph_header_item  = xv_create(termgraph_header_panel, PANEL_TEXT, 
		PANEL_LABEL_X,			120,
		PANEL_LABEL_STRING,		" - current: (nothing selected)                                                       ",
		PANEL_VALUE,			DEFAULT_AGENT_FILE,
		PANEL_VALUE_DISPLAY_LENGTH,	MAXBUF,
		NULL);	

	window_fit_height(termgraph_header_panel);	

	/******************************************************************************/

	termgraph_fname_item  = xv_create(termgraph_panel, PANEL_TEXT, 
		PANEL_LABEL_STRING, 	"Agent (dir|file|data):",
		PANEL_VALUE_DISPLAY_LENGTH,	MAXBUF, /* auch fuer lange Agenten */
		NULL);

	(void)xv_create(termgraph_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"List",
		PANEL_NOTIFY_PROC,	ls_proc,
		NULL);

	(void)xv_create(termgraph_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"Show",
		PANEL_NOTIFY_PROC,	show_proc,
		NULL);	

	(void)xv_create(termgraph_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"Select as Path",
		PANEL_NOTIFY_PROC,	dirset_proc,
		NULL);

	(void)xv_create(termgraph_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"as File",
		PANEL_NOTIFY_PROC,	select_file_proc,
		NULL);

	(void)xv_create(termgraph_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"as Data",
		PANEL_NOTIFY_PROC,	string_proc,
		NULL);

	(void)xv_create(termgraph_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	" Clear ",
		PANEL_NOTIFY_PROC,	clear_proc,
		NULL);

	(void)xv_create(termgraph_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"Ch Dir",
		PANEL_NOTIFY_PROC,	chdir_proc,
		NULL);

	(void)xv_create(termgraph_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"Term --> Graph",
		PANEL_NOTIFY_PROC,	draw_proc,			
		NULL);

	termgraph_size_item = xv_create(termgraph_panel, PANEL_CYCLE,
		PANEL_CHOICE_STRINGS,	"small window",
					"high window",
					"big window",
					"wide window",
					NULL,
		PANEL_NOTIFY_PROC,	termgraph_window_choice,
		NULL);

	(void)xv_create(termgraph_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	" Switch ",
		PANEL_NOTIFY_PROC,	token_proc,
		NULL);

	(void)xv_create(termgraph_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	" Done ",
		PANEL_NOTIFY_PROC,	termgraph_done_proc,
		NULL);

	window_fit_height (termgraph_panel);

	/*********************************************************************************/

	termgraph_param_item1 = xv_create(termgraph_param_panel, PANEL_TEXT, 
		PANEL_LABEL_STRING,		"Only default values  ",
		PANEL_VALUE_DISPLAY_LENGTH,	4,
		NULL);

	termgraph_set_misc_item = xv_create(termgraph_param_panel, PANEL_CYCLE,
		PANEL_LABEL_X,		245+20,
		PANEL_CHOICE_STRINGS,	"transition color",
					"place color",
					"edge color",
					"transition font",
					"place font",
					"edge font",
					"trans. label visibility",
					"place label visibility",
					"edge label visibility",
					"arrow length (normal)",
					"arrow angle (normal)",
					"initial x coordinate",
					"initial y coordinate",
					"draw only with defaults",
					"reset parameters",
					NULL,
		PANEL_NOTIFY_PROC,	termgraph_set_misc_choice,
		NULL);

	termgraph_set_basic_item = xv_create(termgraph_param_panel, PANEL_CYCLE,
		PANEL_CHOICE_STRINGS,	"basic length: default",
					"basic length: 32",
					"basic length: 48",
					"basic length: 64",
					"basic length: += 8",
					"basic length: get/set",
					NULL,
		PANEL_NOTIFY_PROC,	termgraph_set_basic_choice,
		NULL);


	termgraph_misc_button = xv_create(termgraph_param_panel, PANEL_BUTTON,
		PANEL_LABEL_X,		245+20,
		PANEL_LABEL_STRING,	"set trans. color {0..15} ",
		PANEL_NOTIFY_PROC,	termgraph_misc_set_proc,
		NULL);
		
	termgraph_transx_item = xv_create(termgraph_param_panel, PANEL_CYCLE,
		PANEL_CHOICE_STRINGS,	"transition width: default",
					"transition width: 20",
					"transition width: 50",
					"transition width: 60",
					"transition width: += 2",
					"transition width: -= 2",
					"transition width: get/set",
					NULL,
		PANEL_NOTIFY_PROC,	termgraph_set_transx_choice,
		NULL);

	termgraph_transy_item = xv_create(termgraph_param_panel, PANEL_CYCLE,
		PANEL_LABEL_X,		245,
		PANEL_CHOICE_STRINGS,	"transition height: default",
					"transition height: 3 (line)",
					"transition height: 32",
					"transition height: as width",
					"transition height: += 2",
					"transition height: -= 2",
					"transition height: get/set",
					NULL,
		PANEL_NOTIFY_PROC,	termgraph_set_transy_choice,
		NULL);

	termgraph_placex_item = xv_create(termgraph_param_panel, PANEL_CYCLE,
		PANEL_CHOICE_STRINGS,	"place width: default",
					"place width: 16",
					"place width: 32",
					"place width: 48",
					"place width: += 2",
					"place width: -= 2",
					"place width: get/set",
					NULL,
		PANEL_NOTIFY_PROC,	termgraph_set_placex_choice,
		NULL);

	termgraph_placey_item = xv_create(termgraph_param_panel, PANEL_CYCLE,
		PANEL_LABEL_X,		245,
		PANEL_CHOICE_STRINGS,	"place height: width",
					"place height: += 2",
					"place height: -= 2",
					"place height: 3/4 width ",
					"place height: get/set",
					NULL,
		PANEL_NOTIFY_PROC,	termgraph_set_placey_choice,
		NULL);	

	(void)xv_create(termgraph_param_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"Info",
		PANEL_NOTIFY_PROC,	termgraph_info_proc,
		NULL);

	(void)xv_create(termgraph_param_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"1/4 Turn Left",
		PANEL_NOTIFY_PROC,	turn_proc,
		NULL);

	(void)xv_create(termgraph_param_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"  Zoom  ",
		PANEL_NOTIFY_PROC,	termgraph_call_zoom_proc,
		NULL);

	(void)xv_create(termgraph_param_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"Save Values",
		PANEL_NOTIFY_PROC,	termgraph_save_values,
		NULL);

	(void)xv_create(termgraph_param_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"Load Values",
		PANEL_NOTIFY_PROC,	termgraph_load_values,
		NULL);

	(void)xv_create(termgraph_param_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"One Switch",
		PANEL_NOTIFY_PROC,	token_proc,
		NULL);

	(void)xv_create(termgraph_param_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	"Several Switches",
		PANEL_NOTIFY_PROC,	several_token_proc,
		NULL);

	termgraph_load_values(); /* Layoutparameter von .termgraph laden */				
	switch (termgraph_edgeline_type)
	{
	case 0:		sprintf(str, "%s", "Hard Edgeline");
			break;
	default:	sprintf(str, "%s", "Fine Edgeline");
			break;
	}
	termgraph_edgeline_button = xv_create(termgraph_param_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	str,
		PANEL_NOTIFY_PROC,	termgraph_what_edgeline,
		NULL);

	(void)xv_create(termgraph_param_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING,	" Get Graph ",
		PANEL_NOTIFY_PROC,	termgraph_call_scroll_proc,
		NULL);

	window_fit_height (termgraph_param_panel);		
}

static void ls_proc ()
{
	char cmdstring[MAXBUF];
/*
	xv_set(termgraph_frame,
		WIN_MOUSE_XY,		40+ (int)xv_get(termgraph_panel,WIN_X,0),
					130+ (int)xv_get(termgraph_panel,WIN_Y,0),
		NULL);
*/	
	sprintf (cmdstring, "ls %s | more\n", xv_get(termgraph_fname_item, PANEL_VALUE));
	ttysw_input (termgraph_tty, cmdstring, strlen (cmdstring));
	cls_str(cmdstring);
}

static void select_file_proc ()
{
	char cmdstring[MAXBUF];
	char hname[MAXBUF];

	sprintf (hname, "%s", xv_get(termgraph_fname_item, PANEL_VALUE));
	if (hname[0] == '\0') {
		sprintf (cmdstring, 
			"clear; echo \"*** Select: Current name is '%s' ***\"; ", 
			termglob_agent_name);
		sprintf (cmdstring+strlen(cmdstring), 
			"echo \"*** Select: No new selection for agent filename! ***\"\n");
	} else {
		sprintf (termglob_agent_name, "%s/%s", termglob_agent_dir[0]=='\0'?".":termglob_agent_dir, hname);
		sprintf (cmdstring, 
			"clear; echo \"*** File '%s' selected as agent file. ***\" \n",termglob_agent_name);
		xv_set(termgraph_fname_item, PANEL_VALUE, termglob_agent_name, NULL);
	}
	ttysw_input (termgraph_tty, cmdstring, strlen(cmdstring));
	cls_str (cmdstring);
	sprintf (cmdstring, " - current: Selected file in %s                         ", 
		termglob_agent_dir[0] == '\0' ? "." : termglob_agent_name);
	xv_set(termgraph_header_item, PANEL_LABEL_STRING, cmdstring, NULL);
	cls_str (cmdstring);
}

static void string_proc ()
{
	char cmdstring[MAXBUF];
	char hname[MAXBUF];

	sprintf (hname, "%s", xv_get(termgraph_fname_item, PANEL_VALUE));
	if (hname[0] == '\0') {
		cls_str (termglob_agent_stdin);
		sprintf (cmdstring, 
			"clear; echo \"*** Select: Current name is '%s' ***\"; ", 
			termglob_agent_name);
		sprintf (cmdstring+strlen(cmdstring), 
			"echo \"*** Select: No new selection for termgraph agent! ***\"\n");
	} else {
		cls_str (termglob_agent_name);
		sprintf (cmdstring, 
			"clear; echo \"*** '%s' selected as input for termgraph agent! ***\" \n",hname);
		sprintf (termglob_agent_stdin, "%s",  hname);
	}
	ttysw_input (termgraph_tty, cmdstring, strlen(cmdstring));
	cls_str (cmdstring);
	xv_set(termgraph_header_item, PANEL_LABEL_STRING, " - current: Selected as an agent                    ", NULL);
}


static void dirset_proc ()
{
	char cmdstring[MAXBUF];

	cls_str (termglob_agent_dir);

 	sprintf (termglob_agent_dir, "%s", xv_get(termgraph_fname_item, PANEL_VALUE));
 	sprintf (cmdstring, 
			"clear; echo \"*** '%s' selected as agent path related to GraphEd. ***\" \n",
			termglob_agent_dir[0] == '\0' ? "." : termglob_agent_dir);
	ttysw_input (termgraph_tty, cmdstring, strlen(cmdstring));

	cls_str (cmdstring);
	sprintf (cmdstring, 
			" - current: directory %s selected                               ", 
			termglob_agent_dir[0] == '\0' ? "." : termglob_agent_dir);
	xv_set(termgraph_header_item, PANEL_LABEL_STRING, cmdstring, NULL);	
	xv_set(termgraph_fname_item, PANEL_VALUE, termglob_agent_dir, NULL);
	cls_str (cmdstring);
}


static void show_proc ()
{
	char cmdstring[MAXBUF];
/*
	xv_set(termgraph_frame,
			WIN_MOUSE_XY,	100+ (int)xv_get(termgraph_panel,WIN_X,0),
					130+ (int)xv_get(termgraph_panel,WIN_Y,0),
			NULL);
*/	
	cls_str(cmdstring);
	sprintf (cmdstring, 
		"clear; echo \"*** File '%s': ***\"; more %s\n", 
		xv_get(termgraph_fname_item, PANEL_VALUE), xv_get(termgraph_fname_item, PANEL_VALUE));
	if (cmdstring[13] == '\0')
		ttysw_input (termgraph_tty, "clear; echo \"*** Show: But which file? ***\"\n     ", 47);
	else
		ttysw_input (termgraph_tty, cmdstring, strlen (cmdstring));
	cls_str(cmdstring);
}

static void chdir_proc ()
{
	char cmdstring[MAXBUF];

	cls_str (cmdstring);	
	sprintf (cmdstring, 
		"clear; cd %s; set prompt = \"TermGraph(\\!): $cwd:t%% \"\n", 
		xv_get(termgraph_fname_item, PANEL_VALUE));
	ttysw_input (termgraph_tty, cmdstring, strlen(cmdstring));
	while (strncmp(termglob_agent_dir,"./",2) == 0)
		sprintf (termglob_agent_dir, "%s", termglob_agent_dir+2);
	if (strcmp (xv_get(termgraph_fname_item, PANEL_VALUE), "") != 0)
		sprintf (termglob_agent_dir, "%s/%s", termglob_agent_dir[0]=='\0'?".":termglob_agent_dir, xv_get(termgraph_fname_item, PANEL_VALUE));

	cls_str (cmdstring);
	sprintf (cmdstring, 
			" - current: extended path to %s                        ", termglob_agent_dir);
	xv_set(termgraph_header_item, PANEL_LABEL_STRING, cmdstring, NULL);		

	cls_str (cmdstring);
	xv_set(termgraph_fname_item, PANEL_VALUE, cmdstring, NULL);
}

static void termgraph_header_choice(item, event)
Panel_item item;
Event *event;
{
	char str[MAXNAME], filename[MAXNAME];
	FILE *file;

	switch ((int)xv_get(item, PANEL_VALUE))
	{
	case 0: 
		if (( (int)xv_get(termgraph_frame,XV_HEIGHT,0) < 100) ||
			( (int)xv_get(termgraph_frame,XV_WIDTH,0) < 300) ) {
			xv_set(termgraph_frame, XV_HEIGHT, 200, XV_WIDTH, 500, NULL);
		}
		xv_set(termgraph_load_button_item, XV_SHOW, FALSE, NULL);
		xv_set(termgraph_store_button_item, XV_SHOW, FALSE, NULL);
		xv_set(termgraph_header_item,
			PANEL_LABEL_X,		120,
			PANEL_LABEL_STRING,	" - current:  (no new selection)                                               ",
			NULL);
		xv_set(termgraph_panel,
			WIN_SHOW,		TRUE,
/*
			WIN_MOUSE_XY,		300+ (int)xv_get(termgraph_panel,WIN_X,0),
						30+ (int)xv_get(termgraph_panel,WIN_Y,0),
*/
			NULL);
		xv_set(termgraph_tty, WIN_SHOW, TRUE, NULL);
		xv_set(termgraph_text, WIN_SHOW, FALSE, NULL);
		xv_set(termgraph_param_panel, WIN_SHOW, FALSE, NULL);
		break;

	case 1: 
		if (( (int)xv_get(termgraph_frame,XV_HEIGHT,0) < 100) ||
			( (int)xv_get(termgraph_frame,XV_WIDTH,0) < 300) ) {
			xv_set(termgraph_frame, XV_HEIGHT, 200, XV_WIDTH, 500, NULL);
		}

		if (termglob_agent_name[0] != '\0') {
			sprintf (str, "%s", termglob_agent_name);
		} else {
			sprintf (str, "%s", xv_get(termgraph_header_item, PANEL_VALUE));
		}
		sprintf (filename, "%s", xv_get(termgraph_header_item, PANEL_VALUE));
		if (strcmp(filename, str) != 0) {
			sprintf (filename, "%s", str);
			if ((file = fopen(filename, "r")) == NULL) {
				message ("TermGraph%% *** Unknown file \"%s\"! ***\n", filename); /* ohne bell() */
			} else {
				fclose(file);
				xv_set(termgraph_text, TEXTSW_FILE, filename, NULL);
				message ("TermGraph%% Changed agent-file to %s\n", filename);
			}
		};
		xv_set(termgraph_header_item,
			PANEL_LABEL_X,		265,
			PANEL_LABEL_STRING,	"File:",
			PANEL_VALUE,		filename,
			NULL);
		xv_set(termgraph_load_button_item, XV_SHOW, TRUE, NULL);
		xv_set(termgraph_store_button_item, XV_SHOW, TRUE, NULL);	
		xv_set(termgraph_panel, WIN_SHOW, FALSE, NULL);
		xv_set(termgraph_tty, WIN_SHOW, FALSE, NULL);
		xv_set(termgraph_text, WIN_SHOW, TRUE, NULL);
		xv_set(termgraph_param_panel, WIN_SHOW, FALSE, NULL);
		break;

	case 2: 
		if (( (int)xv_get(termgraph_frame,XV_HEIGHT,0) < 100) ||
			( (int)xv_get(termgraph_frame,XV_WIDTH,0) < 300) ) {
			xv_set(termgraph_frame, XV_HEIGHT, 200, XV_WIDTH, 500, NULL);
		}
		xv_set(termgraph_load_button_item, XV_SHOW, FALSE, NULL);
		xv_set(termgraph_store_button_item, XV_SHOW, FALSE, NULL);
		xv_set(termgraph_header_item,
			PANEL_LABEL_X,		120,
			PANEL_LABEL_STRING, 	" - Parameter show/set - Switch - Turn -                                      ",
			NULL);

		xv_set(termgraph_panel, WIN_SHOW, FALSE, NULL);
		xv_set(termgraph_tty, WIN_SHOW, FALSE, NULL);
		xv_set(termgraph_param_panel, WIN_SHOW, TRUE, NULL);
		xv_set(termgraph_text, WIN_SHOW, FALSE, NULL);
		break;

	case 3: xv_set(termgraph_frame, XV_HEIGHT,  47, XV_WIDTH,  130,  NULL);
		xv_set(termgraph_load_button_item, XV_SHOW, FALSE, NULL);
		xv_set(termgraph_store_button_item, XV_SHOW, FALSE, NULL);
		xv_set(termgraph_header_item,
			PANEL_LABEL_X,		120,
			PANEL_LABEL_STRING, 	" - current: closed window size                                                  ",
			NULL);
		break;

	default: break;
	}
}

static void load_agent_proc()
{
	char str[MAXNAME];
	FILE *file;

	sprintf (str, "%s", xv_get(termgraph_header_item, PANEL_VALUE));
	if ( str[0] != '\0' ) {
		if ((file = fopen(str, "r")) == NULL) {
			message ("TermGraph%% *** Unknown file \"%s\"! ***\n", str);
			bell();
		} else {
			fclose(file);
			xv_set(termgraph_text, TEXTSW_FILE, str, NULL);
			message ("TermGraph%% Loaded agent-file %s\n", str);
		}
	} else {
		message ("TermGraph%% Give an agent-file name to load!\n");
		bell();
	}	
}

static void quick_store_agent_proc()
{
	char str[MAXNAME];

	sprintf (str, "%s.agent-", xv_get(termgraph_header_item, PANEL_VALUE));
	textsw_store_file ( termgraph_text, str,
		(int)xv_get(termgraph_frame,WIN_X,0) + 200,
		(int)xv_get(termgraph_frame,WIN_Y,0) + 50 );
	message ("TermGraph%% Saved Editor text to %s\n", str);	
}

static void store_agent_proc()
{
	char str[MAXNAME];

	sprintf (str, "%s", xv_get(termgraph_header_item, PANEL_VALUE));
	if ( ( str[0] != '\0' ) && ( 0 == (int)textsw_store_file ( termgraph_text, str,
		(int)xv_get(termgraph_frame,WIN_X,0) + 200,
		(int)xv_get(termgraph_frame,WIN_Y,0) + 50 )) ) {

		message ("TermGraph%% Stored agent to file %s\n", str);	
	} else {
		message ("TermGraph%% Perhaps missing agent-file name to store?\n");
		bell();
	}	
}

static void termgraph_window_choice(item, event)
Panel_item item;
Event *event;
{
	switch ((int)xv_get(item, PANEL_VALUE))
	{
	case 0: xv_set(termgraph_frame, XV_HEIGHT, 200, XV_WIDTH, 500, NULL);
		break;
	case 1: xv_set(termgraph_frame, XV_HEIGHT, 800, XV_WIDTH, 500, NULL);
		break;
	case 2: xv_set(termgraph_frame, XV_HEIGHT, 800, XV_WIDTH, 750, NULL);
		break;
	case 3: xv_set(termgraph_frame, XV_HEIGHT, 200, XV_WIDTH, 750, NULL);
		break;
	default:
		break;
	}
}

static void termgraph_set_misc_choice (item, event)
Panel_item item;
Event *event;
{	
	char str[MAXNAME], button_str[MAXNAME];
	int val;

	termgraph_misc_item_value = (int)xv_get(item, PANEL_VALUE);
	/* diese globale Variable wird von termgraph_misc_set_proc abgefragt */

	switch (termgraph_misc_item_value)
	{
	case 0: sprintf (str, "TRANSITION COLOR: [%d]", termgraph_transition_color);
		sprintf (button_str, " set trans. color {0..15} ");
		break;

	case 1: sprintf (str, "PLACE COLOR: [%d] ", termgraph_place_color);
		sprintf (button_str, " set place color {0..15}  ");
		break;

	case 2: sprintf (str, "EDGE COLOR: [%d] ", termgraph_edge_color);
		sprintf (button_str, "  set edge color {0..15}  ");
		break;

	case 3: sprintf (str, "TRANSITION FONT: [%d]", termgraph_transition_font);
		sprintf (button_str, " set trans. font {0..11}  ");
		break;

	case 4: sprintf (str, "PLACE FONT: [%d] ", termgraph_place_font);
		sprintf (button_str, "  set place font {0..11}  ");
		break;

	case 5: sprintf (str, "EDGE FONT: [%d] ", termgraph_edge_font);
		sprintf (button_str, "  set edge font {0..11}   ");
		break;

	case 6: if (termgraph_transition_nlv == 1) {
			sprintf (str, "TRANSITION LABEL VISIBLE");
			sprintf (button_str, "set trans. label invisibel");
		} else {
			sprintf (str, "TRANSITION LABEL INVISIBLE");
			sprintf (button_str, " set trans. label visibel ");
		}
		break;
		
	case 7: if (termgraph_place_nlv == 1) {
			sprintf (str, "PLACE LABEL VISIBLE");
			sprintf (button_str, " set place label invisibel");
		} else {
			sprintf (str, "PLACE LABEL INVISIBLE");
			sprintf (button_str, " set place label visibel  ");
		}
		break;
		
	case 8: if (termgraph_edge_elv == 1) {
			sprintf (str, "EDGE LABEL VISIBLE");
			sprintf (button_str, " set edge label invisibel ");
		} else {
			sprintf (str, "EDGE LABEL INVISIBLE");
			sprintf (button_str, "  set edge label visibel  ");
		}
		break;
		
	case 9: sprintf (str, "ARROW LENGTH: [%d] ", termgraph_arrow_length);
		sprintf (button_str, "set arrow length ( >= 0 ) ");
		break;
		
	case 10: sprintf (str, "ARROW ANGLE: [%f] ", termgraph_arrow_angle);
		sprintf (button_str, "set arrow angle [0 - 2.0] ");
		break;
		
	case 11: sprintf (str, "INITIAL X: [%d] ", termgraph_initial_x);
		sprintf (button_str, "  set initial x [ > 0 ]   ");
		break;
		
	case 12: sprintf (str, "INITIAL Y: [%d] ", termgraph_initial_y);
		        sprintf (button_str, "  set initial y [ > 0 ]   ");
		break;						        
		
	case 13: if (termgraph_only_defaults == 1) {                    
			sprintf (str, "TAKE ONLY DEFAULTS");            
			sprintf (button_str, "    set own parameters    ");
		} else {
			sprintf (str, "TAKE OWN PARAMETERS");
			sprintf (button_str, "    set only defaults     ");
		}
		break;

	case 14: sprintf (str, "DO YOU WANT TO RESET ALL?");            
		sprintf (button_str, "     reset parameters     ");
		break;

	default: sprintf (str, "NO VALUE");
		sprintf (button_str, "                      ");
		break;
	}

	xv_set(termgraph_misc_button, PANEL_LABEL_STRING, button_str, NULL);

	xv_set(termgraph_param_item1,
		PANEL_LABEL_STRING,	str,
		PANEL_VALUE,		"",
		NULL);

	xv_set(termgraph_header_item,
		PANEL_LABEL_X,		120,
		PANEL_LABEL_STRING, 	"- Get Value                                                                         ",
		NULL);
		
} /* termgraph_set_misc_choice */
		
static void termgraph_misc_set_proc()
{	
	char 	str[MAXNAME], button_str[MAXNAME];
	int	val, switch_value, wrong_val = 0;
	float	fval;

	sprintf ( button_str, "-");
	sprintf ( str, "%s", xv_get(termgraph_param_item1, PANEL_VALUE) );
	if ((str[0] == '\0') 
	&& (termgraph_misc_item_value !=  6) /* die boolschen Abfragen */
	&& (termgraph_misc_item_value !=  7)
	&& (termgraph_misc_item_value !=  8)
	&& (termgraph_misc_item_value != 14)
	&& (termgraph_misc_item_value != 13)) {
		switch_value = 99;
	} else {
		val = atoi (str);
		switch_value = termgraph_misc_item_value;
	}

	switch (switch_value)
	{
	case 0: if ((val >= 0) && (val <= 15)) {
			termgraph_transition_color = (int)val;
		} else {
			wrong_val = 1;
		}
		break;

	case 1: if ((val >= 0) && (val <= 15)) {
			termgraph_place_color = (int)val;
		} else {
			wrong_val = 1;
		}
		break;

	case 2: if ((val >= 0) && (val <= 15)) {
			termgraph_edge_color = (int)val;
		} else {
			wrong_val = 1;
		}
		break;
		
	case 3: if ((val >= 0) && (val <= 11)) {
			termgraph_transition_font = val;
		} else {
			wrong_val = 1;
		}
		break;
		
	case 4: if ((val >= 0) && (val <= 11)) {
			termgraph_place_font = val;
		} else {
			wrong_val = 1;
		}
		break;
		
	case 5: if ((val >= 0) && (val <= 11)) {
			termgraph_edge_font = val;
		} else {
			wrong_val = 1;
		}
		break;
		
	case 6: termgraph_transition_nlv = (termgraph_transition_nlv + 1) % 2;
			/* 0 = FALSE, 1 = TRUE */
		break;
		
	case 7: termgraph_place_nlv = (termgraph_place_nlv + 1) % 2;
			/* 0 = FALSE, 1 = TRUE */
		break;
		
	case 8: termgraph_edge_elv = (termgraph_edge_elv + 1) % 2;
			/* 0 = FALSE, 1 = TRUE */
		break;
		
	case 9: if (val >= 0) {
			termgraph_arrow_length = val;
		} else {
			wrong_val = 1;
		}
		break;

	case 10: sprintf ( str, "%s", xv_get(termgraph_param_item1, PANEL_VALUE) );
		if (str[0] != '\0') {
			sscanf ( str, "%f", &fval );
			if ((fval > 0.0) && (fval <= 2.0)) {
				termgraph_arrow_angle = fval;
			} else {
				wrong_val = 1;
			}
		} else {
			wrong_val = 1;
		}
		break;
		
	case 11: if ((val >= 0) /*&& (val <= 3950)*/) {
			termgraph_initial_x = val;
		} else {
			wrong_val = 1;
		}
		break;
		
	case 12: if ((val >= 0) /*&& (val <= 3950)*/) {
			termgraph_initial_y = val;
		} else {
			wrong_val = 1;
		}
		break;
		
	case 13: termgraph_only_defaults = (termgraph_only_defaults + 1) % 2;
			/* 0 = FALSE, 1 = TRUE */
		break;
		
	case 14: termgraph_basic_length = DEFAULT_ELEMENT_LENGTH;
		ELEMENT_LENGTH = termgraph_basic_length;
 		termgraph_transx_length = TRANSX;
 		termgraph_transy_length = TRANSY;
 		termgraph_placex_length = PLACEX;
 		termgraph_placey_length = PLACEY;
		termgraph_transition_color = DEFAULT_TRANSITION_COLOR;
		termgraph_place_color = DEFAULT_PLACE_COLOR;
		termgraph_edge_color = DEFAULT_EDGE_COLOR;
		termgraph_transition_font = T_NFI;
		termgraph_place_font = P_NFI;
 		termgraph_edge_font = P_EFI;
		termgraph_transition_nlv = T_NLV;
		termgraph_place_nlv = P_NLV;
		termgraph_edge_elv = P_ELV;
		termgraph_arrow_length = P_AL;
 		termgraph_arrow_angle = P_AA;
 		termgraph_initial_x = DEFAULT_CORX;
		termgraph_initial_y = DEFAULT_CORY;
 		strcpy (termgraph_default_agent_name, DEFAULT_AGENT_FILE);
  		termgraph_zoom_value = 1.0;
 		termgraph_only_defaults = 0;
 		termgraph_edgeline_type = 0;
 		/* hierzu gehoert: */
 		panel_set (termgraph_edgeline_button, PANEL_LABEL_STRING, "Hard Edgeline", 0);
 		break;	

	default: 
		break;
	}


	/* now give a message about the new value */

	if ((str[0] == '\0') 
	&& (termgraph_misc_item_value !=  6) /* die boolschen Abfragen */
	&& (termgraph_misc_item_value !=  7)
	&& (termgraph_misc_item_value !=  8)
	&& (termgraph_misc_item_value != 14)
	&& (termgraph_misc_item_value != 13)) {
		switch_value = 99;
	} else {
		val = atoi (str);
		switch_value = termgraph_misc_item_value;
	}

	switch (switch_value)
	{
	case 0: sprintf (str, "TRANSITION COLOR: [%d]", termgraph_transition_color);
		break;
	case 1: sprintf (str, "PLACE COLOR: [%d] ", termgraph_place_color);
		break;
	case 2: sprintf (str, "EDGE COLOR: [%d] ", termgraph_edge_color);
		break;
	case 3: sprintf (str, "TRANSITION FONT: [%d]", termgraph_transition_font);
		break;
	case 4: sprintf (str, "PLACE FONT: [%d] ", termgraph_place_font);
		break;
	case 5: sprintf (str, "EDGE FONT: [%d] ", termgraph_edge_font);
		break;

	case 6: if (termgraph_transition_nlv == 1) {
			sprintf (str, "TRANSITION LABEL VISIBLE");
			sprintf (button_str, "set trans. label invisibel");
		} else {
			sprintf (str, "TRANSITION LABEL INVISIBLE");
			sprintf (button_str, " set trans. label visibel ");
		}
		break;

	case 7: if (termgraph_place_nlv == 1) {
			sprintf (str, "PLACE LABEL VISIBLE  ");
			sprintf (button_str, "set place label invisibel ");
		} else {
			sprintf (str, "PLACE LABEL INVISIBLE");
			sprintf (button_str, " set place label visibel  ");
		}
		break;
		
	case 8: if (termgraph_edge_elv == 1) {
			sprintf (str, "EDGE LABEL VISIBLE  ");
			sprintf (button_str, " set edge label invisibel ");
		} else {
			sprintf (str, "EDGE LABEL INVISIBLE");
			sprintf (button_str, "  set edge label visibel  ");
		}
		break;
		
	case 9: sprintf (str, "ARROW LENGTH: [%d]  ", termgraph_arrow_length);
		break;
	case 10: sprintf (str, "ARROW ANGLE: [%f]  ", termgraph_arrow_angle);
		break;
	case 11: sprintf (str, "INITIAL X: [%d]    ", termgraph_initial_x);
		break;
	case 12: sprintf (str, "INITIAL Y: [%d]    ", termgraph_initial_y);
		break;
		
	case 13: if (termgraph_only_defaults == 1) {
			sprintf (str, "TAKE ONLY DEFAULTS");
			sprintf (button_str, "    set own parameters    ");
		} else {
			sprintf (str, "TAKE OWN PARAMETERS");
			sprintf (button_str, "    set only defaults     ");
		}
		break;
		
	case 14: sprintf (str, "RESET TO DEFAULTS");
		sprintf (button_str, "     reset parameters     ");
		break;	
			
	default: sprintf (str, "NO VALUE ...     ");
		break;
	}
	
	if (strcmp (button_str, "-") != 0) {
		xv_set(termgraph_misc_button, PANEL_LABEL_STRING, button_str, NULL);
	}
			
	if (wrong_val == 0) {
		xv_set(termgraph_param_item1,
			PANEL_LABEL_STRING,	str,
			PANEL_VALUE,		"",
			NULL);
		if (switch_value != 13 /* == termgraph_only_defaults */) {
			termgraph_only_defaults = 0;
		}
	} else {
		xv_set(termgraph_param_item1,
			PANEL_LABEL_STRING,	"NO VALUE CHANGED  ",
			PANEL_VALUE,		"",
			NULL);
		message ("TermGraph%% *** Value is out of specified range! ***\n");
		bell();
	}
	xv_set(termgraph_header_item,
		PANEL_LABEL_X,			120,
		PANEL_LABEL_STRING, 	"- Set Value                                                                   ",
		NULL);
		
} /* termgraph_misc_set_proc */

static void termgraph_set_basic_choice(item, event)
Panel_item item;
Event *event;
{	
	char str[MAXNAME];
	int val;
	
	switch ((int)xv_get(item, PANEL_VALUE))
	{
	case 0: termgraph_basic_length = DEFAULT_ELEMENT_LENGTH;
		break;
	case 1: termgraph_basic_length = 32;
		termgraph_only_defaults = 0;
		break;
	case 2: termgraph_basic_length = 48;
		termgraph_only_defaults = 0;
		break;
	case 3: termgraph_basic_length = 64;
		termgraph_only_defaults = 0;
		break;
	case 4: termgraph_basic_length += 8;
		termgraph_only_defaults = 0;
		break;
	case 5: sprintf ( str, "%s", xv_get(termgraph_param_item1, PANEL_VALUE) );
		val = atoi (str);
		if (val > 0) {
			termgraph_basic_length = val;
			termgraph_only_defaults = 0;
		}
		break;
			
	default: termgraph_basic_length = DEFAULT_ELEMENT_LENGTH;
		break;
	}
	sprintf (str, "BASIC LAYOUT LENGTH: [%d] ", termgraph_basic_length);
	xv_set(termgraph_param_item1,
		PANEL_LABEL_STRING,	str,
		PANEL_VALUE,		"",
		NULL);
	ELEMENT_LENGTH = termgraph_basic_length;
	xv_set(termgraph_header_item,
		PANEL_LABEL_X,		120,
		PANEL_LABEL_STRING, 	"- Set Value                                                                   ",
		NULL);
}

static void termgraph_set_placex_choice(item, event)
Panel_item item;
Event *event;
{	
	char str[MAXNAME];
	int val;

	switch ((int)xv_get(item, PANEL_VALUE))
	{
	case 0: termgraph_placex_length = DEFAULT_PLACEX_LENGTH;
		break;
	case 1: termgraph_placex_length = 16;
		termgraph_only_defaults = 0;
		break;
	case 2: termgraph_placex_length = 32;
		termgraph_only_defaults = 0;
		break;
	case 3: termgraph_placex_length = 48;
		termgraph_only_defaults = 0;
		break;
	case 4: termgraph_placex_length += 2;
		termgraph_only_defaults = 0;
		break;
	case 5: termgraph_placex_length -= 2;
		termgraph_only_defaults = 0;
		break;
	case 6: sprintf ( str, "%s", xv_get(termgraph_param_item1, PANEL_VALUE) );
		val = atoi (str);
		if (val > 0) {
			termgraph_placex_length = val;
			termgraph_only_defaults = 0;
		}
		break;
		
	default: termgraph_placex_length = DEFAULT_PLACEX_LENGTH;
		break;
	}
	sprintf (str, "PLACE WIDTH: [%d]", termgraph_placex_length);
	xv_set(termgraph_param_item1,
		PANEL_LABEL_STRING,	str,
		PANEL_VALUE,		"",
		NULL);
	xv_set(termgraph_header_item,
		PANEL_LABEL_X,		120,
		PANEL_LABEL_STRING, 	"- Set Value                                                                   ",
		NULL);

}
	
static void termgraph_set_placey_choice(item, event)
Panel_item item;
Event *event;
{	
	char str[MAXNAME];
	int val;
	
	switch ((int)xv_get(item, PANEL_VALUE))
	{
	case 0: termgraph_placey_length = termgraph_placex_length;
		break;
	case 1: termgraph_placey_length += 2;
		termgraph_only_defaults = 0;
		break;
	case 2: termgraph_placey_length -= 2;
		termgraph_only_defaults = 0;
		break;
	case 3: termgraph_placey_length = 3*termgraph_placex_length/4;
		termgraph_only_defaults = 0;
		break;
	case 4: sprintf ( str, "%s", xv_get(termgraph_param_item1, PANEL_VALUE) );
		val = atoi (str);
		if (val > 0) {
			termgraph_placey_length = val;
			termgraph_only_defaults = 0;
		}
		break;
			
	default: termgraph_placey_length = termgraph_placex_length;
		break;
	}
	sprintf (str, "PLACE HEIGHT: [%d] ", termgraph_placey_length);
	xv_set(termgraph_param_item1,
		PANEL_LABEL_STRING,	str,
		PANEL_VALUE,		"",
		NULL);
	xv_set(termgraph_header_item,
		PANEL_LABEL_X,		120,
		PANEL_LABEL_STRING, 	"- Set Value                                                                   ",
		NULL);
}

static void termgraph_set_transx_choice(item, event)
Panel_item item;
Event *event;
{	
	char str[MAXNAME];
	int val;
	
	switch ((int)xv_get(item, PANEL_VALUE))
	{
	case 0: termgraph_transx_length = DEFAULT_TRANSX_LENGTH;
		break;
	case 1: termgraph_transx_length = 20;
		termgraph_only_defaults = 0;
		break;
	case 2: termgraph_transx_length = 50;
		termgraph_only_defaults = 0;
		break;
	case 3: termgraph_transx_length = 60;
		termgraph_only_defaults = 0;
		break;
	case 4: termgraph_transx_length  += 2;
		termgraph_only_defaults = 0;
		break;
	case 5: termgraph_transx_length  -= 2;
		termgraph_only_defaults = 0;
		break;
	case 6: sprintf ( str, "%s", xv_get(termgraph_param_item1, PANEL_VALUE) );
		val = atoi (str);
		if (val > 0) {
			termgraph_transx_length = val;
			termgraph_only_defaults = 0;
		}
		break;
		
	default: termgraph_transx_length = DEFAULT_TRANSX_LENGTH;
		break;
	}
	sprintf (str, "TRANSITION WIDTH: [%d] ", termgraph_transx_length);
	xv_set(termgraph_param_item1,
		PANEL_LABEL_STRING,	str,
		PANEL_VALUE,		"",
		NULL);
	xv_set(termgraph_header_item,
		PANEL_LABEL_X,		120,
		PANEL_LABEL_STRING, 	"- Set Value                                                                   ",
		NULL);
}
	
static void termgraph_set_transy_choice(item, event)
Panel_item item;
Event *event;
{	
	char str[MAXNAME];
	int val;
	
	switch ((int)xv_get(item, PANEL_VALUE))
	{
	case 0: termgraph_transy_length = DEFAULT_TRANSY_LENGTH;
		break;
	case 1: termgraph_transy_length = 3;
		termgraph_only_defaults = 0;
		break;
	case 2: termgraph_transy_length = 32;
		termgraph_only_defaults = 0;
		break;
	case 3: termgraph_transy_length = termgraph_transx_length;
		termgraph_only_defaults = 0;
		break;
	case 4: termgraph_transy_length += 2;
		termgraph_only_defaults = 0;
		break;
	case 5: termgraph_transy_length -= 2;
		termgraph_only_defaults = 0;
		break;
	case 6: sprintf ( str, "%s", xv_get(termgraph_param_item1, PANEL_VALUE) );
		val = atoi (str);
		if (val > 0) {
			termgraph_transy_length = val;
			termgraph_only_defaults = 0;
		}
		break;
				
	default: termgraph_transy_length = DEFAULT_TRANSY_LENGTH;
		break;
	}
	sprintf (str, "TRANSITION HEIGHT: [%d] ", termgraph_transy_length);
	xv_set(termgraph_param_item1,
		PANEL_LABEL_STRING,	str,
		PANEL_VALUE,		"",
		NULL);
	xv_set(termgraph_header_item,
		PANEL_LABEL_X,		120,
		PANEL_LABEL_STRING, 	"- Set Value                                                                   ",
		NULL);
}
	
static void draw_proc (info)
Sgraph_proc_info;
{
	/*xv_set(termgraph_frame,
			WIN_MOUSE_XY,	30+ (int)xv_get(termgraph_panel,WIN_X,0),
					300+ (int)xv_get(termgraph_panel,WIN_Y,0),
			NULL);*/
	ttysw_input (termgraph_tty, "clear\n ", 7);
	/* ttysw_input (termgraph_tty, "date \"+START %T\"\n    ", 20); */
	call_sgraph_proc (term_to_graph_creation_proc);
	/* ttysw_input (termgraph_tty, "date \"+STOP  %T\"\n    ", 20); */
	xv_set(termgraph_header_item, PANEL_LABEL_STRING, " - current: Paint the Graph                             ", NULL);
	if (termgraph_nodes_sum > 200) {
		bell();	/* "Der Graph ist gezeichnet" */
	}
}

static void token_proc ()
{
/*
	xv_set(termgraph_frame,
		WIN_MOUSE_XY,	400+ (int)xv_get(termgraph_panel,WIN_X,0),
				300+ (int)xv_get(termgraph_panel,WIN_Y,0),
		NULL);
*/
	call_sgraph_proc (main_proc_token);
	xv_set(termgraph_header_item,
		PANEL_LABEL_X,		120,
		PANEL_LABEL_STRING, 	"- Got Label as Token Capacity and fired once.                                                          ",
		NULL);
}

static void several_token_proc ()
{
	message ("TermGraph%% Tokenplay in 25 switches:");
/*
	xv_set(termgraph_frame,
		WIN_MOUSE_XY,	400+ (int)xv_get(termgraph_panel,WIN_X,0),
				300+ (int)xv_get(termgraph_panel,WIN_Y,0),
		NULL);
*/
	call_sgraph_proc (main_several_token_proc);
	xv_set(termgraph_header_item,
		PANEL_LABEL_X,		120,
		PANEL_LABEL_STRING, 	"- Got Label as Token Capacity and fired.                                                          ",
		NULL);
}

static void termgraph_info_proc ()
{
	system ("/usr/demo/SOUND/play -v 10 /usr/demo/SOUND/sounds/clink.au &");  /* test audio control */
	xv_set(termgraph_header_item,
		PANEL_LABEL_X,		120,
		PANEL_LABEL_STRING, 	"- Termgraph - written by Rudolf Seisenberger - 1990 - Termgraph -            ",
		NULL);

}

static void termgraph_call_zoom_proc (info)
{
	float dval;
	char str[MAXNAME];
/*
	xv_set(termgraph_frame,
		WIN_MOUSE_XY,	400+ (int)xv_get(termgraph_param_panel,WIN_X,0),
				300+ (int)xv_get(termgraph_param_panel,WIN_Y,0),
		NULL);
*/
	sprintf ( str, "%s", xv_get(termgraph_param_item1, PANEL_VALUE) );
	if (str[0] != '\0') {
		sscanf ( str, "%f", &dval );
	} else {
		dval = 0.0;
	}
	if ((dval <= 0) || (dval > 100)) {
		sprintf (str, "ZOOM FACTOR OUT OF RANGE!");
		xv_set(termgraph_param_item1,
			PANEL_LABEL_STRING,	str,
			PANEL_VALUE,		"",
			NULL);
		xv_set(termgraph_header_item,
			PANEL_LABEL_X,		120,
			PANEL_LABEL_STRING, 	"- current: (no new selection)                                                          ",
			NULL);	
		bell();
		termgraph_zoom_value = 1;
	} else {
		termgraph_zoom_value = dval;
		if (termgraph_zoom_value != 1) {
			sprintf (str, "ZOOM FACTOR: [%1.3f] ", termgraph_zoom_value);
			panel_set(termgraph_param_item1,
				PANEL_LABEL_STRING,	str,
/*
				PANEL_VALUE,		"",
*/
				NULL);
			call_sgraph_proc (termgraph_zoom_proc);
			xv_set(termgraph_header_item,
				PANEL_LABEL_X,		120,
				PANEL_LABEL_STRING, 	"- Zoomed the Graph.                                                           ",
				NULL);
		} else {
			sprintf (str, "TRIVIAL ZOOM FACTOR: [1] ");
			xv_set(termgraph_param_item1,
				PANEL_LABEL_STRING,	str,
				PANEL_VALUE,		"",
				NULL);
			xv_set(termgraph_header_item,
				PANEL_LABEL_X,		120,
				PANEL_LABEL_STRING, 	"- current: (no new selection)                                                           ",
				NULL);
		}
	}
}

static void termgraph_load_values()
{
	FILE *file;
	char str[MAXNAME], snam[MAXNAME], sval[MAXNAME], *line, edgeline_str[MAXNAME];
	int i;
		
	if ( (file = fopen(TERMGRAPH_PARAMETER_FILE, "r")) == NULL ) {
		sprintf (str, "CANNOT LOAD %s", TERMGRAPH_PARAMETER_FILE);
		message ("TermGraph%% *** Cannot load file %s! ***\n", TERMGRAPH_PARAMETER_FILE);
	} else {
		line = termgraph_get_line(file);
		while ( line[0] != '\0') {
			sscanf (line, " %s %s ", snam, sval);
			for (i=0; i<21; i++) {
				if ((strcmp(snam, name[i]) == 0) && (sval[0] != '\0')) {
					switch (i)
					{
					case  0: termgraph_only_defaults = atoi (sval);
						break;
					case  1: termgraph_basic_length = atoi (sval);
						break;
					case  2: termgraph_transx_length = atoi (sval);
						break;
					case  3: termgraph_transy_length = atoi (sval);
						break;
					case  4: termgraph_placex_length = atoi (sval);
						break;
					case  5: termgraph_placey_length = atoi (sval);
						break;
					case  6: termgraph_transition_color = atoi (sval);
						break;
					case  7: termgraph_place_color = atoi (sval);
						break;
					case  8: termgraph_edge_color = atoi (sval);
						break;
					case  9: termgraph_transition_font = atoi (sval);
						break;
					case 10: termgraph_place_font = atoi (sval);
						break;
					case 11: termgraph_edge_font = atoi (sval);
						break;
					case 12: termgraph_transition_nlv = atoi (sval);
						break;
					case 13: termgraph_place_nlv = atoi (sval);
						break;
					case 14: termgraph_edge_elv = atoi (sval);
						break;
					case 15: termgraph_arrow_length = atoi (sval);
						break;
					case 16: termgraph_arrow_angle = atof (sval);
						break;
					case 17: termgraph_initial_x = atoi (sval);
						break;
					case 18: termgraph_initial_y = atoi (sval);
						break;
					case 19: strcpy (termgraph_default_agent_name, sval);
						break;
					case 20: termgraph_edgeline_type = atoi (sval);
						switch (termgraph_edgeline_type)
						{
						case 0:	 sprintf(edgeline_str, "%s", "Hard Edgeline");
							break;
						default: sprintf(edgeline_str, "%s", "Fine Edgeline");
							break;
						}
						xv_set(termgraph_edgeline_button, PANEL_LABEL_STRING, edgeline_str, NULL);
						break;	
						
					/*case 23:  = atoi (sval);
						break;
					case 21:  = atoi (sval);
						break;
					case 22:  = atoi (sval);
						break;	 nicht belegt */
						
					default: sprint ("UNKNOWN KEYWORD!", str);
						break;
					}
					break; /* for */
				}
			}
			line = termgraph_get_line(file);
		}
		sprintf (str, "VALUES FROM %s", TERMGRAPH_PARAMETER_FILE);
		fclose (file);
	}
	xv_set(termgraph_param_item1,
		PANEL_LABEL_STRING,	str,
		PANEL_VALUE,		"",
		NULL);
	xv_set(termgraph_header_item,
		PANEL_LABEL_X,		120,
		PANEL_LABEL_STRING, 	" - Loading layout parameters from file.                                       ",
		NULL);
} /* termgraph_load_values */

char *termgraph_get_line(file)
FILE *file;
{
	char line[MAXBUF], c='\0';
	int  i;
	
	i = 0;
	while ( ((c = getc(file)) != EOF) && (c != '\n') && (i <= MAXBUF) ) {
		line[i++] = c;
	}
	line[i] = '\0';
	
	if (i < 12 /* Mindestlaenge einer Definition */) {
		line[0] = '\0';
	}

	return line;
	
} /* termgraph_get_line */

static void termgraph_save_values()
{
	FILE *file;
	char str[MAXNAME], snam[MAXNAME], sval[MAXNAME], *line;
	int i;
	
	if ( (file = fopen(TERMGRAPH_PARAMETER_FILE, "w")) == NULL ) {
		sprintf (str, "CANNOT OPEN %s", TERMGRAPH_PARAMETER_FILE);
		message ("TermGraph%% *** Cannot open file %s! ***\n", TERMGRAPH_PARAMETER_FILE);
	} else {
		fprintf ( file, "%s \t%d\n%s \t%d\n%s \t%d\n%s \t%d\n%s \t%d\n%s \t%d\n%s \t%d\n%s \t%d\n%s \t%d\n%s \t%d\n%s \t%d\n%s \t%d\n%s \t%d\n%s \t%d\n%s \t%d\n%s \t%d\n%s \t%f\n%s \t%d\n%s \t%d\n%s \t%s\n%s \t%d\n",
			name[ 0], termgraph_only_defaults,
			name[ 1], termgraph_basic_length,
			name[ 2], termgraph_transx_length,
			name[ 3], termgraph_transy_length,
			name[ 4], termgraph_placex_length,
			name[ 5], termgraph_placey_length,
			name[ 6], termgraph_transition_color,
			name[ 7], termgraph_place_color,
			name[ 8], termgraph_edge_color,
			name[ 9], termgraph_transition_font,
			name[10], termgraph_place_font,
			name[11], termgraph_edge_font,
			name[12], termgraph_transition_nlv,
			name[13], termgraph_place_nlv,
			name[14], termgraph_edge_elv,
			name[15], termgraph_arrow_length,
			name[16], termgraph_arrow_angle,
			name[17], termgraph_initial_x,
			name[18], termgraph_initial_y,
			name[19], termgraph_default_agent_name,
			name[20], termgraph_edgeline_type );
		fclose (file);
		sprintf (str, "VALUES SAVED TO %s", TERMGRAPH_PARAMETER_FILE);
	}
	xv_set(termgraph_param_item1,
		PANEL_LABEL_STRING,	str,
		PANEL_VALUE,		"",
		NULL);
	xv_set(termgraph_header_item,
		PANEL_LABEL_X,		120,
		PANEL_LABEL_STRING, 	" - Storing layout parameters to a file.                                        ",
		NULL);
} /* termgraph_save_values */

static void termgraph_what_edgeline (item, event)
Panel_item item;
Event *event;
{
	char str[MAXNAME], str1[MAXNAME];

	termgraph_edgeline_type = (termgraph_edgeline_type + 1) % 2; /* Anzahl verschiedener Kantenlayouts */
	
	switch (termgraph_edgeline_type)
	{
	case 0:		sprintf(str, "%s", "Hard Edgeline");
			sprintf(str1, "%s", "USE HARD EDGELINE");
			break;
			
	default:	sprintf(str, "%s", "Fine Edgeline");
			sprintf(str1, "%s", "USE FINE EDGELINE");
			break;
	}
	xv_set(termgraph_edgeline_button, PANEL_LABEL_STRING, str, NULL);
	xv_set(termgraph_param_item1,
		PANEL_LABEL_STRING,	str1,
		PANEL_VALUE,		"",
		NULL);	
	xv_set(termgraph_header_item,
		PANEL_LABEL_X,		120,
		PANEL_LABEL_STRING, 	"- Change number of edge segments.                                                        ",
		NULL);
	
} /* termgraph_what_edgeline */

static void termgraph_call_scroll_proc (info)
Sgraph_proc_info info;
{
	float dval;
	char str[MAXNAME];
/*
	xv_set(termgraph_frame,
		WIN_MOUSE_XY,	400+ (int)xv_get(termgraph_param_panel,WIN_X,0),
				300+ (int)xv_get(termgraph_param_panel,WIN_Y,0),
		NULL);
*/

	call_sgraph_proc (termgraph_scroll_proc);

	xv_set(termgraph_header_item,
		PANEL_LABEL_X,		120,
		PANEL_LABEL_STRING, 	" - Scrolled Graph to Working Area.                                             ",
		NULL);
}

static void turn_proc(info)
Sgraph_proc_info info;
{
/*
	xv_set(termgraph_frame,
		WIN_MOUSE_XY,	30+ (int)xv_get(termgraph_panel,WIN_X,0),
				300+ (int)xv_get(termgraph_panel,WIN_Y,0),
		NULL);
*/
	ttysw_input (termgraph_tty, "clear\n ", 7);
	call_sgraph_proc (call_petrinet_turn_proc);
	xv_set(termgraph_header_item, PANEL_LABEL_STRING, " - current: Turned the Graph to left                                          ", NULL);
}

static void quit_proc ()
{
	xv_destroy_safe(termgraph_frame);
	termgraph_window_exists = 0; /* nein */
}

static void empty_proc ()
{
}

static void termgraph_done_proc ()
{
	xv_set(termgraph_frame, WIN_SHOW, FALSE, NULL);
	message ("TermGraph%% Window hidden. Data recoverable.\n");
}

static void clear_proc ()
{
	char cmdstring[MAXBUF];
	
	cls_str (cmdstring);
	cls_str (termglob_agent_stdin);
	xv_set(termgraph_fname_item, PANEL_VALUE, cmdstring, NULL);
	xv_set(termgraph_header_item, PANEL_LABEL_STRING, " - current: (no new selection)                                                ", NULL);
	ttysw_input (termgraph_tty, "clear\n ", 7);
}

static void cls_str (str)
char *str;
{
	int i,j;
	j=strlen(str);
	for (i=0; i<j; i++) 
		str[i] = '\0';
}

