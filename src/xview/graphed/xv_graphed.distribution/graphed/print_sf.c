/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
/************************************************************************/
/*									*/
/*				print_sf.c				*/
/*									*/
/************************************************************************/
/*									*/
/*									*/
/************************************************************************/


#include "misc.h"
#include "graph.h"
#include "graphed_subwindows.h"

#include "print.h"
#include "repaint.h"
#include "fileselector/fileselect.h"


/************************************************************************/
/*									*/
/*			GLOBALE PROZEDUREN				*/
/*									*/
/*----------------------------------------------------------------------*/
/*									*/
/*	void	show_print_subframe ()					*/
/*									*/
/************************************************************************/



/************************************************************************/
/*									*/
/*			LOKALE VARIABLE					*/
/*									*/
/************************************************************************/

static	Fs_item		print_fileselector;
static	Print_device	printer_device = OUTPUT_RASTERFILE;

static	Panel_item	print_device_cycle;
static	Panel_item	print_area_cycle;
static	Panel_item	print_color_toggle;
static	Panel_item	print_postscript_format_choice;
static	Panel_item	print_text_a;
static	Panel_item	print_text_b;
static	Panel_item	print_text3;
static	Panel_item	print_text5;
static	Panel_item	print_text7;

#define	toggle_bit_on(value,bit)	(((unsigned int)value) & (1 << (bit)))
#define	toggle_bit_off(value,bit)	(!(toggle_bit_on(value,bit)))

/*	Da der print_subframe nur bei Bedarf erzeugt wird, muessen	*/
/*	seine Einstellungen waehrend der Nichtexistenz			*/
/*	zwischengespeichert werden.					*/

typedef	enum	{ FORMAT_FIT_ON_PAGE, FORMAT_ORIGINAL_SIZE } Print_format;
static	char	*format_strings [] = {
			"fit on page",	/* FORMAT_FIT_ON_PAGE   */
			"original size" /* FORMAT_ORIGINAL_SIZE */
		};
		
static	char	*device_strings [] = {
			"PostScript",
			"Rasterfile",
			"Nec P6"			
		};
		
typedef	enum	{ AREA_VISIBLE, AREA_FULL } Print_area;
static	char	*area_strings [] = {
			"visible area",
			"full area",
		};
		
typedef	enum	{ PRINT_COLOR, PRINT_NOCOLOR } Print_color;
static	char	*color_strings [] = {
			"color",
			"no color",
		};
		
/************************************************************************/
/*									*/
/*			LOKALE FUNKTIONEN				*/
/*									*/
/************************************************************************/

static		notify_print_cycles   ();

static	void	create_panel_items();
static	void	local_print();


/************************************************************************/
/*									*/
/*		FILE_SELECTION_SUBFRAME VERWALTEN			*/
/*									*/
/*	Dieser Subframe wird nur bei Bedarf erzeugt, um			*/
/*	Filedeskriptoren zu sparen.					*/
/*									*/
/************************************************************************/
/*									*/
/*	void	show_print_subframe ()					*/
/*									*/
/*----------------------------------------------------------------------*/
/*									*/
/*	static	notify_print_cycles  (item, event)			*/
/*									*/
/************************************************************************/


static	int	first = TRUE;

void	init_print_fileselector ()
{
	extern	char	*get_existing_fileselector_startup_filename();
	
	if( first ) {
		first = FALSE;
		print_fileselector = fls_create();
		fls_setup_from_file( print_fileselector, get_existing_fileselector_startup_filename(), "printer" );
		fls_set_user_panel_items_create_proc( print_fileselector, create_panel_items );
		fls_set_info( print_fileselector, " << PRINT GRAPH >>" );
	}
}

void	show_print_subframe ()
{	
	init_print_fileselector ();
	fileselect( print_fileselector, base_frame, local_print );
}

void	write_print_fileselector( file )
FILE	*file;
{
	init_print_fileselector ();
	fls_write_to_file( print_fileselector, file, "printer" );
	fls_close (print_fileselector);
}

static	void	local_print(dir, file)
char	*dir, *file;
{
	char		filename  [FILENAMESIZE];
	char		directory [FILENAMESIZE];
	Print_format	format;
	Print_device	device;
	Print_area	area;
	Print_color	color;
	Rect		print_rect, visible_rect;
	
	/* Vorverarbeitung : mode, dateiname und directory bestimmen	*/
	
	if ( strcmp( file, "NOTHING SELECTED" ) ) {

		strcpy (filename, dir);
		if( strcmp( dir, "" ) && strcmp( dir, "/" ) ) {
			strcat( filename, "/" );
		}
		strcat( filename, file );
		
		if (filename == NULL || !strcmp (filename, "")) {
			error ("No filename provided.\n");
			return;
		} else if (!check_file_is_single (filename)) {
			return;
		} else if (file_exists (filename) && NOTICE_NO == notice_prompt (base_frame, NULL,		/*fisprompt*/
				NOTICE_MESSAGE_STRINGS,	"This file does already exist.", NULL,
				NOTICE_BUTTON_YES,	"Overwrite",
				NOTICE_BUTTON_NO,	"Cancel",
				NULL)) {
			return;
		}
				
		device = (Print_device)	xv_get(print_device_cycle, PANEL_VALUE);
		area   = (Print_area)	xv_get(print_area_cycle, PANEL_VALUE);
		color  = toggle_bit_on(	xv_get(print_color_toggle, PANEL_VALUE),0) ? PRINT_COLOR : PRINT_NOCOLOR;
		
		switch (area) {
		    case AREA_VISIBLE :
			get_buffer_visible_rect (wac_buffer, &visible_rect);
			print_rect = compute_rect_around_graphs (wac_buffer);
			rect_intersection (&print_rect,
				&visible_rect,
				&print_rect);
			break;
		    case AREA_FULL :
			print_rect = compute_rect_around_graphs (wac_buffer);
			break;
		}
		
		switch (device) {
		    case OUTPUT_POSTSCRIPT :
			{
				format = (Print_format)	xv_get(print_postscript_format_choice, PANEL_VALUE);
				print_postscript (filename, &print_rect,
					iif (format == FORMAT_FIT_ON_PAGE, gesamt, autoteilen));
			}
			break;
		    case OUTPUT_RASTERFILE :
			{
				Pixrect *p;
				FILE    *f;
				
				f = fopen (filename, "w");
				if (f != (FILE *)NULL) {
					p = paint_graph_in_rect (&print_rect, wac_buffer,
						iif (color == PRINT_COLOR, TRUE, FALSE) );
					if (p != (Pixrect *)NULL) {
						pr_dump (p, f, NULL, RT_STANDARD, 0);
#ifdef XVIEW_COMMENT
     XView CONVERSION - This is raw pixrect image in 68000 byte order so must
     include rasterfile.h and/or consider whether you really want to use
     this still
#endif

					}
					fclose (f);
				} else {
				}
			}
			break;
		    case OUTPUT_NECP6 :
			{
				int		no1,no2;
				float		f1,f2;
				double		paper_width, paper_length;
				int		needle_number;
				char		decision1, decision2, decision3;
				wi_out_str      pri_sett;
				Pixrect		*p;

				no1 = sscanf((char*) xv_get(print_text_a, PANEL_VALUE), "%f" , &f1);
				no2 = sscanf((char*) xv_get(print_text_b, PANEL_VALUE), "%f" , &f2);
				paper_width = (double)f1;
				paper_length = (double)f2;
				switch (xv_get(print_text3, PANEL_VALUE)) {
				    case 0:
					needle_number = 8;
					break;
				    case 1:
				    default:
					needle_number = 24;
					break;
				}
				switch (xv_get(print_text5, PANEL_VALUE)) {
				    case 0:
					decision1 = 'y';
					break;
				    case 1:
				    default:
					decision1 = 'n';
					break;
				}
				switch (color) {
				    case PRINT_COLOR:
					decision3 = 'c';
					break;
				    case PRINT_NOCOLOR:
					decision3 = 'm';
					break;
				}
				switch (xv_get(print_text7, PANEL_VALUE)) {
				    case 0:
					decision2 = 'y';
					break;
				    case 1:
				    default:
					decision2 = 'n';
					break;
				}

				if ( no1 == 0 || paper_width <= 0.0 || paper_width > 16.0) {
					error("paper width in incorrect\n");
				} else if (no2 == 0 || paper_length <= 0.0 || paper_length > 16.0) {
					error("paper length is incorrect\n");
				}
				
				
				pri_sett.width = paper_width;
				pri_sett.length = paper_length;
				pri_sett.count = needle_number;
				pri_sett.dec1 = decision1;
				pri_sett.dec2 = decision2;
				pri_sett.dec3 = decision3;
				pri_sett.name = filename;
					
				p = paint_graph_in_rect (&print_rect, wac_buffer,
					iif (color == PRINT_COLOR, TRUE, FALSE ));
				if (p != (Pixrect *)NULL) {
					p7_c_print(p,&pri_sett);
				}

			}
			break;
		}
	}
}


static		notify_print_cycles (item, value, event)
Panel_item	item;
Event		*event;
int		value;
{
	if (item == print_device_cycle) {
		printer_device = (Print_device)	xv_get(print_device_cycle, PANEL_VALUE);
		switch ( printer_device ) {
		    case OUTPUT_POSTSCRIPT :
			xv_set(print_text_a, XV_SHOW, FALSE, NULL);
			xv_set(print_text_b, XV_SHOW, FALSE, NULL);
			xv_set(print_text3, XV_SHOW, FALSE, NULL);
			xv_set(print_text5, XV_SHOW, FALSE, NULL);
			xv_set(print_text7, XV_SHOW, FALSE, NULL);
			xv_set(print_postscript_format_choice, XV_SHOW, TRUE, NULL);
			break;
		    case OUTPUT_RASTERFILE :
			xv_set(print_postscript_format_choice, XV_SHOW, FALSE, NULL);
			xv_set(print_text_a, XV_SHOW, FALSE, NULL);
			xv_set(print_text_b, XV_SHOW, FALSE, NULL);
			xv_set(print_text3, XV_SHOW, FALSE, NULL);
			xv_set(print_text5, XV_SHOW, FALSE, NULL);
			xv_set(print_text7, XV_SHOW, FALSE, NULL);
			break;
		    case OUTPUT_NECP6 :
			xv_set(print_postscript_format_choice, XV_SHOW, FALSE, NULL);
			xv_set(print_text_a, XV_SHOW, TRUE, NULL);
			xv_set(print_text_b, XV_SHOW, TRUE, NULL);
			xv_set(print_text3, XV_SHOW, TRUE, NULL);
			xv_set(print_text5, XV_SHOW, TRUE, NULL);
			xv_set(print_text7, XV_SHOW, TRUE, NULL);
			break;
		}
	}
}


/*------------------------------------------------------*/
static	void	create_panel_items( panel )
Panel	panel;
{
	int 	row_count = 0;
	int	row_count_options_start;
		
	row_count = 0;	
	print_device_cycle  = xv_create(panel, PANEL_CYCLE,
		XV_X,			xv_col(panel,0),
		XV_Y,			xv_row(panel,row_count),
		PANEL_CHOICE_STRINGS,	device_strings [OUTPUT_POSTSCRIPT],
					device_strings [OUTPUT_RASTERFILE],
					device_strings [OUTPUT_NECP6],
					NULL,
		PANEL_VALUE,		printer_device,
		PANEL_NOTIFY_PROC,	notify_print_cycles,
		NULL);

	row_count += 1;
	print_area_cycle  = xv_create(panel, PANEL_CHOICE,
		PANEL_LAYOUT,		PANEL_HORIZONTAL,
		XV_X,			xv_col(panel,0),
		XV_Y,			xv_row(panel,row_count),
		PANEL_CHOICE_STRINGS,	area_strings [AREA_VISIBLE],
					area_strings [AREA_FULL],
					NULL,
		PANEL_VALUE,		AREA_VISIBLE,
		NULL);

	row_count += 1;
	print_color_toggle  = xv_create(panel, PANEL_TOGGLE,
		XV_X,			xv_col(panel,0),
		XV_Y,			xv_row(panel,row_count),
		PANEL_CHOICE_STRINGS,	"color", 0,
		PANEL_VALUE,		AREA_VISIBLE,
		NULL);


	/* LaserWriter / PostScript Options */
	
	row_count += 2;
	row_count_options_start = row_count;
	print_postscript_format_choice  = xv_create(panel, PANEL_CHOICE,
		XV_X,			xv_col(panel,0),
		XV_Y,			xv_row(panel,row_count),
		PANEL_CHOICE_STRINGS,	format_strings [FORMAT_FIT_ON_PAGE],
					format_strings [FORMAT_ORIGINAL_SIZE],
					NULL,
		PANEL_VALUE,		FORMAT_FIT_ON_PAGE,
		NULL);
		

	/* Nec P7+ Options */
	
	row_count = row_count_options_start;
	print_text_a = xv_create(panel, PANEL_TEXT,
		PANEL_LABEL_X,			xv_col(panel,0),
		PANEL_LABEL_Y,			xv_row(panel,row_count),
		PANEL_LABEL_BOLD,		TRUE,
		PANEL_LABEL_STRING,		"paper width  (0.01 - 16)",
		PANEL_VALUE,			"8.0",
		PANEL_VALUE_STORED_LENGTH,	5,
		PANEL_VALUE_DISPLAY_LENGTH,	5,
		NULL);

	row_count += 1;
	print_text_b = xv_create(panel, PANEL_TEXT,
		PANEL_LABEL_X,			xv_col(panel,0),
		PANEL_LABEL_Y,			xv_row(panel,row_count),
		PANEL_LABEL_BOLD,		TRUE,
		PANEL_LABEL_STRING,		"paper length (0.01 -16 )",
		PANEL_VALUE,			"12.0",
		PANEL_VALUE_STORED_LENGTH,	5,
		PANEL_VALUE_DISPLAY_LENGTH,	5,
		NULL);

	row_count += 1;
	print_text3 = xv_create(panel, PANEL_CHOICE,
		PANEL_LAYOUT,			PANEL_HORIZONTAL,
		PANEL_LABEL_X,			xv_col(panel,0),
		PANEL_LABEL_Y,			xv_row(panel,row_count),
		PANEL_VALUE,			1,
		PANEL_CHOICE_STRINGS,		"8", "24  needles"/*, "36"*/, NULL,
		NULL);
     
	row_count += 1;
	print_text5 = xv_create(panel, PANEL_CHOICE,
		PANEL_LAYOUT,			PANEL_HORIZONTAL,
		PANEL_LABEL_X,			xv_col(panel,0),
		PANEL_LABEL_Y,			xv_row(panel,row_count),
		PANEL_VALUE,			1,
		PANEL_CHOICE_STRINGS,		"quality", "speed", NULL,
		NULL);


	row_count += 1; 
	print_text7 = xv_create(panel, PANEL_CHOICE,
		PANEL_LAYOUT,			PANEL_HORIZONTAL,
		PANEL_LABEL_X,			xv_col(panel,0),
		PANEL_LABEL_Y,			xv_row(panel,row_count),
		PANEL_VALUE,			FORMAT_FIT_ON_PAGE,
		PANEL_CHOICE_STRINGS,		format_strings [FORMAT_FIT_ON_PAGE],
						format_strings [FORMAT_ORIGINAL_SIZE],
						NULL,
		NULL);

	window_fit_height(panel);
	
	
	switch ((Print_device)	xv_get(print_device_cycle, PANEL_VALUE)) {
	    case OUTPUT_POSTSCRIPT :
		xv_set(print_text_a, XV_SHOW, FALSE, NULL);
		xv_set(print_text_b, XV_SHOW, FALSE, NULL);
		xv_set(print_text3, XV_SHOW, FALSE, NULL);
		xv_set(print_text5, XV_SHOW, FALSE, NULL);
		xv_set(print_text7, XV_SHOW, FALSE, NULL);
		xv_set(print_postscript_format_choice, XV_SHOW, TRUE, NULL);
		break;
	    case OUTPUT_RASTERFILE :
		xv_set(print_postscript_format_choice, XV_SHOW, FALSE, NULL);
		xv_set(print_text_a, XV_SHOW, FALSE, NULL);
		xv_set(print_text_b, XV_SHOW, FALSE, NULL);
		xv_set(print_text3, XV_SHOW, FALSE, NULL);
		xv_set(print_text5, XV_SHOW, FALSE, NULL);
		xv_set(print_text7, XV_SHOW, FALSE, NULL);
		break;
	    case OUTPUT_NECP6 :
		xv_set(print_postscript_format_choice, XV_SHOW, FALSE, NULL);
		xv_set(print_text_a, XV_SHOW, TRUE, NULL);
		xv_set(print_text_b, XV_SHOW, TRUE, NULL);
		xv_set(print_text3, XV_SHOW, TRUE, NULL);
		xv_set(print_text5, XV_SHOW, TRUE, NULL);
		xv_set(print_text7, XV_SHOW, TRUE, NULL);
		break;
	}
}


