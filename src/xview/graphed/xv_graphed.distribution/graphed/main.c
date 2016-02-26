/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
/************************************************************************/
/*									*/
/*				main.c					*/
/*									*/
/************************************************************************/
/*									*/
/*		G R A P H  E D    -   M A I N   F I L E			*/
/*									*/
/*	Dieses Modul enthaelt neben der obligatorischen main - Prozedur	*/
/*	die Verwaltung des base_frame (inklusive Erzeugung aller seiner	*/
/*	Subwindows), Initialisierungen fuer verschiedene globale	*/
/*	Variablen und den "Interpreter" fuer Argumente aus der		*/
/*	Kommandozeile.							*/
/*									*/
/************************************************************************/

#include "misc.h"
#include "graph.h"

#include "graphed_subwindows.h"

#include "repaint.h"
#include "graphed_mpr.h"
#include "user.h"
#include "load.h"


#define GRAPHED_USAGE	"Usage : graphed [-wa w h] [-f file] [-] [file]"

#define	GRAPHED_BACKGROUND_PIXRECT_FILENAME "graphed.background.pr"

/************************************************************************/
/*									*/
/*			GLOBALE PROZEDUREN				*/
/*									*/
/************************************************************************/
/*									*/
/*	void	set_base_frame_label (what, data)			*/
/*	void	bell ()							*/
/*									*/
/*	void	fill_panel_choice_attr_list_of_strings ();		*/
/*	void	fill_panel_choice_attr_list_of_images  ();		*/
/*									*/
/************************************************************************/

void	fill_panel_choice_attr_list_of_strings ();
void	fill_panel_choice_attr_list_of_images  ();



/************************************************************************/
/*									*/
/*			GLOBALE VARIABLE				*/
/*									*/
/************************************************************************/


Frame	base_frame;

int	screenwidth,	/* Bildschirmgroesse, wird dynamisch in		*/
	screenheight;	/* create_base_frame ermittelt.			*/


Graphed_state	graphed_state;	/* Globaler Programmzustand		*/


/*	temporary_subframe_shown : "Semaphor", um zu verhindern, dass	*/
/*	zwei temporaere Subframes gleichzeitig erzeugt werden (das	*/
/*	spart Filedeskriptoren, da jeder FRAME, jedes PANEL und jeder	*/
/*	CANVAS einen, jedes TEXTSW sogar drei dieser leider auch in	*/
/*	UNIX nicht unbegrenzt verfuegbaren Kanaele belegen).		*/

int	temporary_subframe_shown = FALSE;
	


/*	Listen mit Texten bzw. Bildern, mit denen Aufzahelungstypen	*/
/*	in Menues etc. dargestellt werden koennen.			*/
/*	Jede Liste ist zweimal vorhanden :				*/
/*	- die Texte (..._strings) bzw. Bilder (..._images) selbst	*/
/*	- dieselben Listen so aufbereitet (..._for_cycle), dass sie in	*/
/*	  PANEL_CYCLE's ueber ATTR_LIST uebergeben werden koennen.	*/
/*	Die Aufbereitung erfolgt in der Prozedur init_misc.		*/

char		*nei_strings [NUMBER_OF_NODE_EDGE_INTERFACES];
Server_image	nei_images  [NUMBER_OF_NODE_EDGE_INTERFACES];
char		*nlp_strings [NUMBER_OF_NODELABEL_PLACEMENTS];
Server_image	nlp_images  [NUMBER_OF_NODELABEL_PLACEMENTS];
char		*scaling_strings [NUMBER_OF_SCALINGS];
char		*gragra_type_strings [NUMBER_OF_GRAGRA_TYPES];

char	*nei_strings_for_cycle [NUMBER_OF_NODE_EDGE_INTERFACES+3];
char	*nei_images_for_cycle  [NUMBER_OF_NODE_EDGE_INTERFACES+3];
char	*nlp_strings_for_cycle [NUMBER_OF_NODELABEL_PLACEMENTS+3];
char	*nlp_images_for_cycle  [NUMBER_OF_NODELABEL_PLACEMENTS+3];
char	*scaling_strings_for_cycle [NUMBER_OF_SCALINGS+3];
char	*gragra_type_strings_for_cycle [NUMBER_OF_GRAGRA_TYPES+3];



/************************************************************************/
/*									*/
/*			LOKALE VARIABLEN				*/
/*									*/
/************************************************************************/


static	int	type_setup_width,	/* Groessen der einzelnen	*/
		font_setup_width,	/* Fenster			*/
		type_setup_height,
		font_setup_height,
		message_textsw_height,
		message_textsw_width;


/************************************************************************/
/*									*/
/*		LOKALE FUNKTIONEN / PROZEDUREN				*/
/*									*/
/************************************************************************/


static	void		create_base_frame                      ();
static	Notify_value	my_base_frame_destroyer                ();
static	void		init_base_frame_and_working_area_menu  ();

static	void		init_misc                              ();

static	void		load_initialisation_file               ();
static	void		dispatch_command_line_arguments        ();
static	void		command_line_error                     ();


/************************************************************************/
/*									*/
/*									*/
/*	+=======================================================+	*/
/*	!							!	*/
/*	!	+---------------------------------------+	!	*/
/*	!	|					|	!	*/
/*	!   *   |	H A U P T P R O G R A M M	|   *	!	*/
/*	!	|					|	!	*/
/*	!	+---------------------------------------+	!	*/
/*	!							!	*/
/*	+=======================================================+	*/
/*									*/
/*									*/
/************************************************************************/
/*									*/
/*	   Initialisiert alles moegliche usw. und startet die		*/
/*			   window_main_loop				*/
/*									*/
/************************************************************************/


my_error_proc (object, avlist)
Xv_object	object;
Attr_attribute	avlist[ATTR_STANDARD_SIZE];
{
	printf ("%s\n", xv_error_format (object, avlist));
	fflush (stdout);
}


graphed_main(argc, argv)
int	argc;
char	**argv;
{

#ifdef GRAPHED_DEBUG_MALOOC
#include <malloc.h>
malloc_debug (1);
/* remember to link with /usr/lib/debug/malloc.o */
#endif

	xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, XV_ERROR_PROC, my_error_proc, NULL);

	init_graphed_state ();	/* -> state.c				*/
	graphed_state.startup = TRUE;

	init_config   ();
	init_misc     ();	
	init_graphed_colormap ();
	
	create_base_frame(&argc, argv);/* Nimmt alle SunView -		*/
					/* Atribute aus (argc, argv).	*/
/* Removed MH conversion
	if (((Pixwin *)xv_get(base_frame, WIN_PIXWIN))->pw_pixrect->pr_depth > 1)
		graphed_state.colorscreen = TRUE;
*/
	graphed_state.colorscreen = FALSE;

	init_base_frame_and_working_area_menu ();
	/* Erzeugt alle Subwindows des base_frame (keine Subframes)	*/
	init_canvases ();
			
	create_node_subframe    ();
	create_edge_subframe    ();
	create_group_subframe   ();
	create_node_defaults_subframe ();
	create_edge_defaults_subframe ();
/*	create_xprompt_subframe ();			fisprompt*/
	
	init_fonts   ();	/* -> fonts.c				*/
	init_types   ();	/* -> types.c				*/
	init_buffers ();	/* -> ggraph.c				*/
	set_working_area_canvas (canvases[create_buffer()].canvas);
	
	init_graph_state   ();	/* -> state.c				*/
	init_graphs        ();	/* -> ggraph.c				*/
	
	set_filename ("");
	
	init_user_interface      (); /* -> user.c			*/
	load_initialisation_file (); /* Kann evtl. Progamm abbrechen !	*/

	dispatch_command_line_arguments (argc, argv);
	
	init_extra_menu ();

	init_user_menu ();
	init_user_event_functions ();
	
	menu_called_from = MENU_CALLED_FROM_CANVAS;
	if (buffer_is_empty(wac_buffer)) {
		dispatch_user_action (CREATE_MODE);
	} else {
		dispatch_user_action (SELECT_MODE);
	}
	
	graphed_state.startup = FALSE;

	load_graphed_background_pixrect (GRAPHED_BACKGROUND_PIXRECT_FILENAME);
	
	xv_main_loop(base_frame);
}
/************************************************************************/
/*									*/
/*			VERWALTUNG DES BASE_FRAME			*/
/*									*/
/************************************************************************/
/*									*/
/*	static	void	create_base_frame (argc, argv)			*/
/*									*/
/*	Erzeugt den base_frame. Aus dem Menue werden "move" und		*/
/*	"resize" entfernt. screenwidth und screenheight werden		*/
/*	entsprechend den lokalen Gegebenheiten ermittelt.		*/
/*	Mit argc und argv koennen SunView-Optionen aus der		*/
/*	Kommandozeile uebergeben werden. Alle so gefundenen Optionen	*/
/*	werden (von xv_set) aus (argc, argv) entfernt. Der Rest		*/
/*	wird von main aus ueber dispatch_command_line_arguments		*/
/*	ausgewertet.							*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	set_base_frame_label (what, data)			*/
/*									*/
/*	Setzt eine Anzeige im Label des base_frame; what gibt an,	*/
/*	welche :							*/
/*	- FRAME_LABEL_FILENAME : data ist ein string, der den		*/
/*	  Dateinamen enthaelt. Diese Option setzt auch den Label des	*/
/*	  Icons.							*/
/*	- FRAME_LABEL_EDITED : gibt an, ob der Graph seit dem		*/
/*	  letzten Laden bzw. Abspeichern veraendert wurde; data ist	*/
/*	  ein int - Wert (TRUE, FALSE).					*/
/*	- FRAME_LABEL_CONSTRAINED : markiert eine Beschraenkung		*/
/*	  der Bewegung der Maus (naeheres -> user.c); data ist ein	*/
/*	  int - Wert (TRUE, FALSE).					*/
/*	- FRAME_LABEL_MODE_STRING : gibt den Modus (Node, Edge		*/
/*	  oder Select) der Benutzerschnittstelle (-> user.c) an.	*/
/*									*/
/*	typedef enum {							*/
/*		FRAME_LABEL_FILENAME,					*/
/*		FRAME_LABEL_EDITED,					*/
/*		FRAME_LABEL_CONSTRAINED,				*/
/*		FRAME_LABEL_MODE_STRING					*/
/*	}								*/
/*		Frame_label_attribute;					*/
/*									*/
/*======================================================================*/
/*									*/
/*	static  Notify_value  my_base_frame_destroyer (client, status)	*/
/*									*/
/*	Wenn GraphEd beendet wird, so wird (ueber den SunView-Notifier)	*/
/*	diese Prozedur aufgerufen. Wenn der Graph noch nicht		*/
/*	abgespeichert wurde, so erfolgt eine Sicherheitsabfrage.	*/
/*									*/
/*======================================================================*/
/*									*/
/*	static	void	init_base_frame_and_working_area_menu ()	*/
/*									*/
/*	Legt die Groessen fuer die Subwindows des base_frame fest und	*/
/*	erzeugt sie.							*/
/*	DIESE ROUTINE KANN EVTL. DAS PROGRAMM ABBRECHEN (ist aber	*/
/*	sehr unwahrscheinlich).						*/
/*									*/
/************************************************************************/

	
static	void	create_base_frame (argc_ptr, argv)
int		*argc_ptr;
char		**argv;
{
	Menu	base_frame_menu;
	Rect	*screenrect;

	base_frame = (Frame)xv_create(NULL, FRAME,
/* Commented MH Conversion
		FRAME_ICON,		icon_create (ICON_IMAGE, &graphed_icon_pixrect, 0),
*/
		WIN_X,			0,
		WIN_Y,			0,
		XV_SHOW,		FALSE,
		FRAME_NO_CONFIRM,	TRUE,
		FRAME_ARGC_PTR_ARGV,	argc_ptr, argv,
		WIN_ERROR_MSG,		"Could not create base frame.\n Good bye!",
		FRAME_SHOW_RESIZE_CORNER,	FALSE,
		NULL);

	screenrect   = (Rect *)xv_get(base_frame, WIN_SCREEN_RECT);
	screenwidth  = rect_width (screenrect);
	screenheight = rect_height (screenrect);
	
	/* Wenn "quit" im Menue eingegeben wird, verzweige auch zu der	*/
	/* folgenden Prozedur :						*/

	notify_interpose_destroy_func (base_frame, my_base_frame_destroyer);
}



void	set_base_frame_label (va_alist)
va_dcl
{
	Frame_label_attribute	what;
	va_list			pvar;

	static	int	edited      = FALSE,	/* sichergestelle	*/
			constrained = FALSE;	/* Werte der Attribute	*/
	static	Node_or_edge	group_labelling_operation = NODE;
	static	char		filename [FILENAMESIZE];
#define MODE_STRING_MAX_LENGTH 100
	static	char		mode_string [MODE_STRING_MAX_LENGTH];
	char			buffer[FILENAMESIZE];
	
	va_start(pvar);
	what = va_arg(pvar, Frame_label_attribute);

	switch (what) {
	    case FRAME_LABEL_FILENAME :
		strncpy (filename, va_arg(pvar, char *), FILENAMESIZE);
		break;
	    case FRAME_LABEL_EDITED :
		edited = va_arg(pvar, int);
		break;
	    case FRAME_LABEL_CONSTRAINED :
		constrained = va_arg(pvar, int);
		break;
	    case FRAME_LABEL_GROUP_LABELLING_OPERATION :
		group_labelling_operation = va_arg(pvar, Node_or_edge);
		break;
	    case FRAME_LABEL_MODE_STRING :
		strncpy (mode_string, va_arg(pvar, char *), MODE_STRING_MAX_LENGTH);
		break;
	}
	
	sprintf (buffer, "GraphEd %s  Mode [%s] [%s] [%s]",
		GRAPHED_VERSION,
		mode_string,
		iif (constrained, "constrained", "unconstrained"),
		iif (group_labelling_operation == EDGE,
		     "group labels edge",
		     "group labels node")
		);
	
	xv_set(base_frame, XV_LABEL, buffer, NULL);

	va_end(pvar);
}



static	Notify_value	my_base_frame_destroyer (client, status)
Notify_client	client;
Destroy_status	status;
{
	Prompt	user_choice;
	char	buffer [FILENAMESIZE + 100];
	int	i;
	
	if (status == DESTROY_CHECKING) {
	    if (any_graph_has_changed() && (get_filename() == NULL || !strcmp(get_filename(),""))) {
		user_choice = notice_prompt (base_frame, NULL,	/*fisprompt*/
			NOTICE_FOCUS_XY,	screenwidth/3, screenheight/2,
			NOTICE_MESSAGE_STRINGS,	"No store since last change.",
						"really quit ?", NULL,
			NOTICE_BUTTON,		"yes",		PROMPT_ACCEPT,
			NOTICE_BUTTON,		"no",		PROMPT_REFUSE,
			NOTICE_BUTTON,		"cancel",	PROMPT_CANCEL,
			NULL);
		switch (user_choice) {
		    case PROMPT_ACCEPT :
			break;
		    case PROMPT_REFUSE :
		    case PROMPT_CANCEL :
			(void)notify_veto_destroy(client);
			return NOTIFY_DONE;
			break;
		}
	    } else if (any_graph_has_changed() && get_filename() != NULL && !strcmp(get_filename(),"")) {
		sprintf (buffer, "No store since last change - store to %s ?",
		         get_filename());
		user_choice = notice_prompt (base_frame, NULL,	/*fisprompt*/
			NOTICE_FOCUS_XY,	screenwidth/3, screenheight/2,
			NOTICE_MESSAGE_STRINGS,	buffer, NULL,
			NOTICE_BUTTON,		"yes",		PROMPT_ACCEPT,
			NOTICE_BUTTON,		"no",		PROMPT_REFUSE,
			NOTICE_BUTTON,		"cancel",	PROMPT_CANCEL,
			NULL);
		switch (user_choice) {
		    case PROMPT_ACCEPT :
			store_graphs (get_filename());
			break;
		    case PROMPT_REFUSE :
			break;
		    case PROMPT_CANCEL :
			(void)notify_veto_destroy(client);
			return NOTIFY_DONE;
			break;
		}
	    }
	}
	
	graphed_state.shutdown = TRUE;
	for (i=N_PASTE_BUFFERS; i<N_BUFFERS; i++)
		if (buffers[i].used) {
			unuse_buffer (i);
			xv_set(canvases[i].frame, FRAME_NO_CONFIRM, TRUE, NULL);
			destroy_frame_and_canvas (i);
		}
	
	return(notify_next_destroy_func(client,status));
}


static	void	init_base_frame_and_working_area_menu ()
{
	node_edge_panel = (Panel)xv_create(base_frame, PANEL,
		WIN_ERROR_MSG,	"Could not create node-edge panel.\nGood bye!\n",
		NULL);
	create_node_edge_panel();
	window_fit (node_edge_panel);

	message_textsw = (Textsw)xv_create(base_frame, TEXTSW,
		XV_HEIGHT,	(int)xv_get(node_edge_panel, XV_HEIGHT),
		XV_WIDTH,	(int)xv_get(node_edge_panel, XV_WIDTH),
		WIN_ERROR_MSG,	"Could not create message textsw.\nGood bye!\n",
		NULL);
	create_message_textsw();

	create_working_area_menu ();

	window_fit(base_frame);
}
/************************************************************************/
/*									*/
/*			INITIALISIERUNGEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	static	void	load_initialisation_file ()			*/
/*									*/
/*	Initialisiert Zeichensatztabelle, Knoten- und Kantentypen-	*/
/*	tabellen, Attribute von Knoten und Kanten sowie Groesse und	*/
/*	Scrollbars der working_area.					*/
/*	Die zu setzenden Werte werden einer Datei			*/
/*	GRAPHED_INITIALISATION_FILE (".graphed") entnommen, die		*/
/*	zunaechst im "."- und dann im "Home" - Directory gesucht wird.	*/
/*	Falls sie dort nicht gefunden wird, sucht die Prozedur nach	*/
/*	der Datei GRAPHED_DEFAULT_INITIALISATION_FILE. Ist auch das	*/
/*	erfolglos, so wird das Programm abgebrochen.			*/
/*	IN DIESER ROUTINE KANN DAS PROGRAMM ABGEBROCHEN WERDEN, falls	*/
/*	naemlich beim parsen der Datei ein Fehler (Syntax, aber auch	*/
/*	wenn keine Zeichensaetze, Knoten - oder Kantentypen gefunden)	*/
/*	auftritt (oder die Datei gar nicht gefunden wird).		*/
/*									*/
/*======================================================================*/
/*									*/
/*	static	void	init_misc ()					*/
/*									*/
/*	Tut, was der Name sagt : Initialisierung von			*/
/*	- nlp_strings, nlp_images					*/
/*	- nei_strings, nei_images					*/
/*	- scaling_strings						*/
/*	- nlp_strings_for_cycle, nei_images_for_cycle			*/
/*	- nei_strings_for_cycle, nlp_images_for_cycle			*/
/*	- scaling_strings_for_cycle					*/
/*	- global_repaint_rectlist, global_erase_rectlist (->repaint.c,	*/
/*	  direkt schafft das C-Compiler leider nicht)			*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	fill_panel_choice_attr_list_of_strings (		*/
/*				strings, n, attr_list)			*/
/*	void	fill_panel_choice_attr_list_of_images (			*/
/*				images,  n, attr_list)			*/
/*									*/
/*	Hilfsprozeduren fuer init_misc : erzeugen aus dem Array		*/
/*	strings (images) der Laenge n eine Liste attr_list, die an ein	*/
/*	PANEL_CYCLE als ATTR_LIST uebergeben werden kann.		*/
/*	Genug Speicherplatz fuer attr_list muss bereitgestellt werden !	*/
/*									*/
/************************************************************************/



static	void	load_initialisation_file ()
{
	FILE	*initialisation_file;
	char	initialisation_file_name [FILENAMESIZE];
	
	static	char	*try_nodetypes[] = { "#box", "#circle", "#diamond" };
	static	char	*try_edgetypes[] = { "#solid", "#dashed", "#dotted", "#dashdotted", "#dashdotdotted", "#longdashed" };
	static	char	*try_fonts[]     = { "/usr/lib/fonts/fixedwidthfonts/cour.r.10",
		                             "/usr/lib/fonts/fixedwidthfonts/cour.b.10",
		                             "/usr/lib/fonts/fixedwidthfonts/cour.r.12",
		                             "/usr/lib/fonts/fixedwidthfonts/cour.b.12",
		                             "/usr/lib/fonts/fixedwidthfonts/cour.r.14",
		                             "/usr/lib/fonts/fixedwidthfonts/cour.b.14",
		                             "/usr/lib/fonts/fixedwidthfonts/cour.r.16",
		                             "/usr/lib/fonts/fixedwidthfonts/cour.b.16",
		                             "/usr/lib/fonts/fixedwidthfonts/cour.r.18",
		                             "/usr/lib/fonts/fixedwidthfonts/cour.b.18",
		                             "/usr/lib/fonts/fixedwidthfonts/cour.r.24",
		                             "/usr/lib/fonts/fixedwidthfonts/cour.b.24" };

	strcpy (initialisation_file_name, GRAPHED_INITIALISATION_FILE);
	if ((initialisation_file = fopen (initialisation_file_name, "r")) == (FILE *)NULL) {
	    strcpy (initialisation_file_name, getenv ("HOME"));
	    strcat (initialisation_file_name, "/");
	    strcat (initialisation_file_name, GRAPHED_INITIALISATION_FILE);
	    if ((initialisation_file = fopen (initialisation_file_name, "r")) == (FILE *)NULL) {
		if ((initialisation_file = fopen (GRAPHED_DEFAULT_INITIALISATION_FILE, "r")) == (FILE *)NULL) {
		    fatal_error ("Can't find %s - startup failed\n", GRAPHED_INITIALISATION_FILE);
		}
	    }
	}
	load_buffer = wac_buffer;
	set_lex_input (initialisation_file);
	if (yyparse() == 1)
		fatal_error ("error reading %s\n", GRAPHED_INITIALISATION_FILE);
}



static	void	init_misc ()
{
	int	i;

	svi_init (); /* Initialize XView Server_image's */

	nlp_strings [(int)NODELABEL_MIDDLE]     = "middle";
	nlp_strings [(int)NODELABEL_UPPERLEFT]  = "upper left";
	nlp_strings [(int)NODELABEL_UPPERRIGHT] = "upper right";
	nlp_strings [(int)NODELABEL_LOWERLEFT]  = "lower left";
	nlp_strings [(int)NODELABEL_LOWERRIGHT] = "lower right";
	
	nlp_images [(int)NODELABEL_MIDDLE]     = nlp_middle_icon_svi;
	nlp_images [(int)NODELABEL_UPPERLEFT]  = nlp_upperleft_icon_svi;
	nlp_images [(int)NODELABEL_UPPERRIGHT] = nlp_upperright_icon_svi;
	nlp_images [(int)NODELABEL_LOWERLEFT]  = nlp_lowerleft_icon_svi;
	nlp_images [(int)NODELABEL_LOWERRIGHT] = nlp_lowerright_icon_svi;
	
	
	nei_strings [(int)NO_NODE_EDGE_INTERFACE]      = "none";
	nei_strings [(int)TO_BORDER_OF_BOUNDING_BOX]   = "to middle of border of bounding box";
	nei_strings [(int)TO_CORNER_OF_BOUNDING_BOX]   = "to corner of bounding box";
	nei_strings [(int)CLIPPED_TO_MIDDLE_OF_NODE]   = "clipped to middle of node";
	nei_strings [(int)SPECIAL_NODE_EDGE_INTERFACE] = "special";

	nei_images [(int)NO_NODE_EDGE_INTERFACE]      = nei_none_icon_svi;
	nei_images [(int)TO_BORDER_OF_BOUNDING_BOX]   = nei_middle_icon_svi;
	nei_images [(int)TO_CORNER_OF_BOUNDING_BOX]   = nei_corner_icon_svi;
	nei_images [(int)CLIPPED_TO_MIDDLE_OF_NODE]   = nei_clipped_icon_svi;
	nei_images [(int)SPECIAL_NODE_EDGE_INTERFACE] = nei_special_icon_svi;

	
	scaling_strings [(int)SCALE_16_16]    = "16  x 16    ";
	scaling_strings [(int)SCALE_32_32]    = "32  x 32    ";
	scaling_strings [(int)SCALE_64_64]    = "64  x 64    ";
	scaling_strings [(int)SCALE_96_96]    = "96  x 96    ";
	scaling_strings [(int)SCALE_128_128]  = "128 x 128   ";
	scaling_strings [(int)SCALE_192_192]  = "192 x 192   ";
	scaling_strings [(int)SCALE_256_256]  = "256 x 256   ";
	scaling_strings [(int)SCALE_384_384]  = "384 x 384   ";
	scaling_strings [(int)SCALE_512_512]  = "512 x 512   ";
	scaling_strings [(int)SCALE_IDENTITY] = "...         ";
	scaling_strings [(int)SCALE_DOWN_XY]  = "down   x & y";
	scaling_strings [(int)SCALE_DOWN_X]   = "down   x    ";
	scaling_strings [(int)SCALE_DOWN_Y]   = "down   y    ";
	scaling_strings [(int)SCALE_UP_XY]    = "up     x & y";
	scaling_strings [(int)SCALE_UP_X]     = "up     x    ";
	scaling_strings [(int)SCALE_UP_Y]     = "up     y    ";
	scaling_strings [(int)SCALE_SQUARE_X] = "square x    ";
	scaling_strings [(int)SCALE_SQUARE_Y] = "square y    ";
	
	gragra_type_strings [gragra_type_to_int(ENCE_1)] = "1-ENCE";
	gragra_type_strings [gragra_type_to_int(NCE_1)]  = "1-NCE";
	gragra_type_strings [gragra_type_to_int(NLC)]    = "NLC";
	gragra_type_strings [gragra_type_to_int(BNLC)]   = "BNLC";
	
	fill_panel_choice_attr_list_of_strings (
		nlp_strings, NUMBER_OF_NODELABEL_PLACEMENTS,
		nlp_strings_for_cycle);
	fill_panel_choice_attr_list_of_images (
		nlp_images, NUMBER_OF_NODELABEL_PLACEMENTS,
		nlp_images_for_cycle);
	fill_panel_choice_attr_list_of_strings (
		nei_strings, NUMBER_OF_NODE_EDGE_INTERFACES,
		nei_strings_for_cycle);
	fill_panel_choice_attr_list_of_images (
		nei_images, NUMBER_OF_NODE_EDGE_INTERFACES,
		nei_images_for_cycle);
	fill_panel_choice_attr_list_of_strings (
		scaling_strings, 10,
		scaling_strings_for_cycle);
	fill_panel_choice_attr_list_of_strings (
		gragra_type_strings, NUMBER_OF_GRAGRA_TYPES,
		gragra_type_strings_for_cycle);
	
	for (i=0; i<N_BUFFERS; i++) {
		global_repaint_rectlists[i] = rl_null;
		global_erase_rectlists[i]   = rl_null;
	}
}


void    fill_panel_choice_attr_list_of_strings (strings, n, attr_list)
char	*strings[];
int	n;
char	*attr_list[];
{
	int	i;
	
	attr_list [0] = (char *)PANEL_CHOICE_STRINGS;
	for (i=0; i<n; i++)
		attr_list[i+1] = strings[i];
	attr_list [n+1] = (char *)0;
	attr_list [n+2] = (char *)0;
}



void	fill_panel_choice_attr_list_of_images (images, n, attr_list)
Server_image	images[];
int	n;
char	*attr_list[];
{
	int	i;
	
	attr_list [0] = (char *)PANEL_CHOICE_IMAGES;
	for (i=0; i<n; i++)
		attr_list[i+1] = (char *)images[i];
	attr_list [n+1] = (char *)0;
	attr_list [n+2] = (char *)0;
}



int	load_graphed_background_pixrect (name)
char	*name;
{
	FILE	*file;
	char	*full_name;
	
	full_name = file_exists_somewhere (GRAPHED_BACKGROUND_PIXRECT_FILENAME, getenv ("GRAPHED_INPUTS"));
	
	if ((full_name != NULL) && ((file = fopen (full_name, "r")) != (FILE *)NULL)) {
		background_pixrect = pr_load (file, NULL);
		fclose (file);
		return TRUE;
	} else {
		return FALSE;
	}
}
/************************************************************************/
/*									*/
/*		ARGUMENTE AUS DER KOMMANDOZEILE				*/
/*									*/
/************************************************************************/
/*									*/
/*	static	void	dispatch_command_line_arguments (argc, argv)	*/
/*									*/
/*	Wertet die Argumente in der Kommandozeile aus, argc und argv	*/
/*	wie in main. Bei einem Fehler bricht das Programm ab.		*/
/*									*/
/*	Moegliche Argumente sind:					*/
/*	-wa -working_area <w> <h>	Groesse der working_area	*/
/*	-f  -file         <datei>	lade Graph von Datei		*/
/*	-   -stdin			lade Graph von stdin		*/
/*	-h  -graphed_help		Argumente ausgeben		*/
/*	                  <datei>	lade Graph von Datei		*/
/*									*/
/*	wobei <datei> = [^-].* (also alles, das nicht mit '-'		*/
/*	anfaengt); die Option "-f" wird eigentlich nur dann gebraucht,	*/
/*	wenn ein Dateiname mit '-' beginnt (!?!).			*/
/*									*/
/*======================================================================*/
/*									*/
/*	static	void	command_line_error ()				*/
/*									*/
/*	Gibt eine "Usage : ..." - Meldung auf stderr aus und BRICHT	*/
/*	DAS PROGRAMM AB.						*/
/*									*/
/************************************************************************/



static	void	dispatch_command_line_arguments (argc, argv)
int		argc;
char		*argv[];
{
	int	i;
	
	for (i=1; i<argc; i++) {
		if ( !strcmp(argv[i], "-wa") || !strcmp(argv[i], "-working_area")) {
			if (i+2 < argc) {
				graphed_state.default_working_area_canvas_width  = atoi (argv[i+1]);
				graphed_state.default_working_area_canvas_height = atoi (argv[i+2]);
				set_working_area_size (graphed_state.default_working_area_canvas_width, graphed_state.default_working_area_canvas_height);
				i += 2;
			} else
				command_line_error ();
		} else if (!strcmp(argv[i], "-f") || !strcmp(argv[i], "-file")) {
			if (i+1 < argc)  {
				load (wac_buffer, argv[i+1]);
				i += 1;
			} else
				command_line_error ();
		} else if (!strcmp(argv[i], "-h") || !strcmp(argv[i], "-graphed_help")) {
			printf ("FLAG  LONG FLAG     ARGS   DESCRIPTION\n");
			printf ("-wa  -working_area  w h    Set working area size\n");
			printf ("-f   -file          file   Load graph\n");
			printf ("-    -stdin                Load graph from stdin\n");
			printf ("-h   -graphed_help         Print this message\n");
			printf ("                    file   Load graph (same as -f)\n");
			exit (0);
		} else if (!strcmp(argv[i], "-") || !strcmp(argv[i], "-stdin")) {
			load (wac_buffer, "");
		} else if (argv[i][0] != '-') {
			load (wac_buffer, argv[i]);
			dispatch_user_action (SELECT_MODE);
		} else
			command_line_error ();
	}
}


static	void	command_line_error ()
{
	fprintf (stderr, "%s\n", GRAPHED_USAGE);
	exit (1);
}
/************************************************************************/
/*									*/
/*			LAST, BUT NOT LEAST				*/
/*									*/
/************************************************************************/
/*									*/
/*	void	bell ()							*/
/*									*/
/*	Klingelt.							*/
/*	BUG : wenn der Benutzer mit defaultsedit die Klingel		*/
/*	abgeschaltet hat, tut diese Prozedur nichts !			*/
/*									*/
/************************************************************************/


int	bell ()
{
	window_bell (base_frame);
}

