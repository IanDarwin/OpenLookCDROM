/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
/************************************************************************/
/*									*/
/*				buffer.c				*/
/*									*/
/************************************************************************/

#include "misc.h"
#include "graph.h"
#include "graphed_subwindows.h"


/************************************************************************/
/*									*/
/*			GLOBALE FUNKTIONEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	void	init_buffers  ()					*/
/*	int	create_buffer ()					*/
/*									*/
/*	void	delete_buffer           (buffer)			*/
/*	void	delete_graphs_in_buffer (buffer)			*/
/*									*/
/*	void	unuse_buffer           (buffer)				*/
/*									*/
/************************************************************************/

/************************************************************************/
/*									*/
/*			GLOBALE VARIABLEN				*/
/*									*/
/************************************************************************/


Buffer		buffers [N_BUFFERS];

int		paste_buffer = 0;
int		wac_buffer   = N_PASTE_BUFFERS;



/************************************************************************/
/*									*/
/*				BUFFER					*/
/*									*/
/************************************************************************/
/*									*/
/*	Ein Buffer ist eine Liste von Graphen. Es gibt zwei Arten von	*/
/*	Buffern : den Buffer der Arbeitsflaeche, wac_buffer und den	*/
/*	paste-buffer. Allgemein sind es N_CANVASES und N_PASTE_BUFFERS.	*/
/*	Dabei kommen zuerst die paste-buffers und dann die canvases.	*/
/*									*/
/*	Datenstruktur :							*/
/*									*/
/*	typedef	struct	buffer	{					*/
/*		Graph	graphs;						*/
/*		int	changed;					*/
/*		int	used;		Buffer belegt ?			*/
/*		char	*filename;					*/
/*	}								*/
/*		Buffer;							*/
/*									*/
/*	N_BUFFERS = N_CANVASES + N_PASTE_BUFFERS			*/
/*	Buffer	buffers [N_BUFFERS];					*/
/*									*/
/*	int	wac_buffer;		Index des altuellen wac-buffer	*/
/*	int	paste_buffer;		Index des aktuellen		*/
/*					paste-buffer			*/
/*									*/
/*----------------------------------------------------------------------*/
/*									*/
/*	Makros :							*/
/*									*/
/*	buffer_is_empty(b)	Buffer leer ?				*/
/*	graphs_of_buffer(b)	buffers[b].graphs			*/
/*									*/
/************************************************************************/
/*									*/
/*	void	init_buffers ()						*/
/*									*/
/************************************************************************/
/*									*/
/*	int	create_buffer ()					*/
/*									*/
/*	Sucht einen neuen (wac-) Buffer zum Belegen. Rueckgabe :	*/
/*	Nummer des buffers bzw. (-1), falls keiner mehr frei.		*/
/*	Ein freier Buffer hat used == FALSE.				*/
/*									*/
/************************************************************************/
/*									*/
/*	void	delete_graphs_in_buffer (b)				*/
/*									*/
/*	Loescht alle Graphen im Buffer b.				*/
/*									*/
/************************************************************************/
/*									*/
/*	void	delete_buffer (b)					*/
/*									*/
/*	Loescht alle Graphen im Buffer b und zusaetzlich die		*/
/*	zugehoerigen Canvases. Der Buffer wird wieder freigegeben,	*/
/*	also used auf FALSE gesetzt.					*/
/*									*/
/************************************************************************/
/*									*/
/*	Zusaetszlich werden in ggraph.c einige Prozeduren fuer buffer	*/
/*	(reset_buffer_has_changed, compute_rect_around_graphs, ...)	*/
/*									*/
/*	Parallel zu den Buffern liegt die Struktur canvases;		*/
/*	buffers[i] entspricht canvases[i].				*/
/*									*/
/************************************************************************/


void	init_buffers ()
{
	int	i;
	
	for (i=0; i< N_PASTE_BUFFERS; i++) {
		buffers[i].graphs  = empty_graph;
		buffers[i].changed = FALSE;
		buffers[i].used    = TRUE;	/* Always used !	*/
	}
	
	for (i=N_PASTE_BUFFERS; i< N_BUFFERS; i++) {
		buffers[i].graphs  = empty_graph;
		buffers[i].changed = FALSE;
		buffers[i].used    = FALSE;
	}
	paste_buffer = 0;
	wac_buffer   = -1;
}


int	create_buffer ()
{
	int	i;
	
	for (i=N_PASTE_BUFFERS; i< N_BUFFERS; i++) {
		if (buffers[i].used == FALSE)
			break;
	}
	
	if (i == N_BUFFERS)
		return (-1);
	else {
		create_canvas(i,
			graphed_state.default_working_area_canvas_width,
			graphed_state.default_working_area_canvas_height);
		set_canvas_window_size (i,
			graphed_state.default_working_area_window_width,
			graphed_state.default_working_area_window_height);

		buffers[i].graphs   = empty_graph;
		buffers[i].changed  = FALSE;
		buffers[i].used     = TRUE;
		buffers[i].filename = NULL;
		return i;
	}
}


void	delete_graphs_in_buffer (b)
int	b;
{
	if (b == wac_buffer) {
		while (buffers[b].graphs != empty_graph)
			erase_and_delete_graph (buffers[b].graphs);
	} else {
		while (buffers[b].graphs != empty_graph)
			delete_graph (buffers[b].graphs);
	}
}


void	delete_buffer (b)
int	b;
{
	delete_graphs_in_buffer  (b);
	unuse_buffer (b);
	destroy_frame_and_canvas (b);
}


void	unuse_buffer (b)
int	b;
{
	buffers[b].used     = FALSE;
	buffers[b].changed  = FALSE;
	buffers[b].graphs   = empty_graph;
	if (buffers[b].filename != NULL) {
		myfree (buffers[b].filename);
	}
	buffers[b].filename = NULL;
}


/************************************************************************/
/*									*/
/*				Goodies					*/
/*									*/
/************************************************************************/


void	buffer_set_filename (buffer, filename)
int	buffer;
char	*filename;
{
	buffers[buffer].filename = strsave (filename);
}

char	*buffer_get_filename (buffer)
int	buffer;
{
	return	buffers[buffer].filename;
}

int	buffer_is_unused (buffer)
int	buffer;
{
	return buffers[buffer].used;
}
