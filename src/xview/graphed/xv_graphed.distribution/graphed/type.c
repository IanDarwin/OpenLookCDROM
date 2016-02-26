/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt, Thomas Lamshoeft,	*/
/*                              Uwe Schnieders				*/
/************************************************************************/
/*									*/
/*				type.c					*/
/*									*/
/************************************************************************/
/*									*/
/*	Dieses Modul verwaltet Knoten- und Kantentypen.			*/
/*									*/
/************************************************************************/

#include "misc.h"
#include "graph.h"

#include <pixrect/pixrect_hs.h>
#include <xview/panel.h>
#include <xview/icon_load.h>

#include "install.h"
#include "load.h"
#include "type.h"
#include "paint.h"
#include "adjust.h"
#include "modula.h" /* fuer la_paint.h */
#include "la_paint.h"


/************************************************************************/
/*									*/
/*			GLOBALE FUNKTIONEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	int		insert_nodetype (filename, insert_position)	*/
/*	int		insert_edgetype (filename, insert_position)	*/
/*	int		add_nodetype    (filename)			*/
/*	int		add_edgetype    (filename)			*/
/*	int		delete_nodetype (delete_position)		*/
/*	int		delete_edgetype (delete_position)		*/
/*									*/
/*	Nodetype	get_current_nodetype ()				*/
/*	Edgetype	get_current_edgetype ()				*/
/*									*/
/*	Nodetype	use_nodetype (nodetype_index)			*/
/*	Edgetype	use_edgetype (edgetype_index)			*/
/*	void		unuse_nodetype (nodetype)			*/
/*	void		unuse_nodetype (nodetype)			*/
/*	Nodetypeimage	use_nodetypeimage   (type, sx,sy)		*/
/*	void		unuse_nodetypeimage (type, image)		*/
/*									*/
/*	int		get_nodetype_index (type)			*/
/*	int		get_edgetype_index (type)			*/
/*	int		find_nodetype (filename)			*/
/*	int		find_edgetype (filename)			*/
/*      Nodetype	get_nodetype  (index);				*/
/*									*/
/*	void		install_current_nodetype ()			*/
/*	void		install_current_edgetype ()			*/
/*									*/
/*	void		init_types ()					*/
/*	void		write_nodetypes (file)				*/
/*	void		write_edgetypes (file)				*/
/*									*/
/************************************************************************/



/************************************************************************/
/*									*/
/*			LOKALE VARIABLEN				*/
/*									*/
/************************************************************************/


static	struct	{
		int		number_of_nodetypes;
		Nodetype	list            [MAX_NODETYPE];
		char		*list_for_cycle [MAX_NODETYPE+3];
	}
	nodetypes;

static	struct	{
		int		number_of_edgetypes;
		Edgetype	list            [MAX_EDGETYPE];
		char		*list_for_cycle [MAX_EDGETYPE+3];
	}
	edgetypes;


static	struct	{
		char	*name;
		void	(*adjust_func)();
		void	(*pr_paint_func)();
		void	(*laser_paint_func)();
	}
#define	NUMBER_OF_SYSTEM_NODETYPES 4
	system_nodetypes [NUMBER_OF_SYSTEM_NODETYPES] =
	{
		"#box",     adjust_to_box_node,
		            pr_paint_box_node,
		            laser_paint_box,
		"#circle",  adjust_to_elliptical_node,
		            pr_paint_elliptical_node,
		            laser_paint_circle,
		"#diamond", adjust_to_diamond_node,
		            pr_paint_diamond_node,
		            laser_paint_diamond,
		"#rect",    adjust_to_elliptical_node,
		            pr_paint_box_node,
		            laser_paint_box

	};

static short Pr_tex_dotted[]        = { 3 , 4 , 0 };
static short Pr_tex_dashed[]        = { 12 , 4 , 0 };
static short Pr_tex_dashdot[]       = { 20 , 4 , 4 , 4 , 0 };
static short Pr_tex_dashdotdotted[] = { 12 , 4 , 4 , 4 , 4 , 4 , 0 };
static short Pr_tex_longdashed[]    = { 26 , 6 , 0 };

#define NUMBER_OF_SYSTEM_EDGETYPES 6

static struct	{
		char	*name;
		short	*pattern;
	}
	system_edgetypes [NUMBER_OF_SYSTEM_EDGETYPES] =
	{
		"#solid"	, (short *)NULL,
		"#dotted"	, Pr_tex_dotted,
		"#dashed"	, Pr_tex_dashed,
		"#dashdotted"	, Pr_tex_dashdot,
		"#dashdotdotted", Pr_tex_dashdotdotted,
		"#longdashed"	, Pr_tex_longdashed
	};
	
	
/************************************************************************/
/*									*/
/*			LOKALE PROZEDUREN				*/
/*									*/
/************************************************************************/


static	void	install_nodetypelist_for_cycle ();
static	void	install_edgetypelist_for_cycle ();


/************************************************************************/
/*									*/
/*		KNOTEN- UND KANTENZEICHENTYPEN : DATENSTRUKTUR 		*/
/*									*/
/************************************************************************/
/*									*/
/*	So sieht ein Knotentyp aus  :					*/
/*									*/
/*	typedef	struct	nodetype					*/
/*	{								*/
/*			Nodetypeimage	images;				*/
/*			Pixrect	*pr;				*/
/*			char		*filename;			*/
/*			int		used;				*/
/*			int		is_system;			*/
/*			void		(*adjust_func)();		*/
/*			void		(*pr_paint_func)();		*/
/*			void		(*laser_paint_func)();		*/
/*	}								*/
/*		*Nodetype;						*/
/*									*/
/*									*/
/*	images		Liste mit Bildern dieses Knotentyps.		*/
/*			Details siehe Nodetypeimage.			*/
/*	pr		Pixrect mit dem (Muster-) Knotenbild		*/
/*	filename	Name des Knotentyps. Bei systemdefinierten	*/
/*			Knotentypen steht hier der Name in		*/
/*			system_nodetypes[?].name, sonst der Dateiname	*/
/*			des Icons, aus dem der Knotentyp gewonnen wird.	*/
/*	used		Zaehler, wie oft type und type->images		*/
/*			verwendet werden. Loeschen des Knotentyps ist	*/
/*			nur gestattet, wenn used == 0.			*/
/*	is_system	Gibt an, ob es sich um einen vordefinierten	*/
/*			Knotentyp (TRUE) handelt oder um einen aus	*/
/*			Icon synthetisierten (s.u.).			*/
/*	pr_paint_func	Gibt bei vordefinierten Knotentypen die		*/
/*			Prozedur an, mit der dieser Knotentyp		*/
/*			auf ein Pixrect gezeichnet wird			*/
/*	laser_paint_func Wie vor, Ausgabe auf Postscript - File		*/
/*	adjust_func	Gibt bei vordefinierten Knotentypen die		*/
/*			Prozedur an, die Kanten an den Knoten anpasst.	*/
/*									*/
/*	Zur Unterscheidung vordefinierter - aus Icon gewonnener		*/
/*	Knotentyp :							*/
/*	Vordefinierte Knotentypen besitzen spezielle Prozeduren zum	*/
/*	Zeichnen und Anpassen von Kanten. Alle vordefinierten Knoten-	*/
/*	typen sind in einer Liste system_nodetypes abgespeichert; es	*/
/*	gibt insgesamt NUMBER_OF_SYSTEM_NODETYPES von ihnen. Das	*/
/*	"Laden"	eines vordefinierten Knotentyps wird ueber die		*/
/*	"normale" Prozedur insert_nodetype (bzw. add_nodetype)		*/
/*	vorgenommen, wobei der Dateiname mit dem Zeichen		*/
/*	SYSTEM_TYPES_IDENTIFICATION_CHARACTER ('#') zu beginnen hat.	*/
/*	Aus Icons gewonnene Knotentypen sind de facto Pixelmuster.	*/
/*	Fuer sie sind keine speziellen Prozeduren zum Zeichnen und	*/
/*	Anpassen noetig. Das Laden erfolgt ueber insert_nodetype (bzw.	*/
/*	add_nodetype). Vergroessern und Verkleinern dieser Pixelmuster	*/
/*	wird dann durch Saklierung des "Musters" (type->pr)		*/
/*	bewerkstelligt.							*/
/*									*/
/*======================================================================*/
/*									*/
/*	typedef	struct	nodetypeimage {					*/
/*		int			sx,sy;				*/
/*		Pixrect		*pr;				*/
/*		int			used;				*/
/*		struct	nodetypeimage	*pre, *suc;			*/
/*	}								*/
/*		*Nodetypeimage;						*/
/*									*/
/*	sx,sy		Groesse von pr.					*/
/*	used		Zaehler, wie oft dieses Bild verwendet wird.	*/
/*			Ist used == 0, so wird das Bild geloescht !	*/
/*	pre, suc	alle images zu einem Knoten sind in einer	*/
/*			doppelt verketteten, nicht geschlossenen Liste	*/
/*			organisiert.					*/
/*									*/
/*	Um Speicherplatz und Rechenzeit zu sparen, werden alle Bilder	*/
/*	eines Knotentyps, die gleiche Groesse haben, nur einmal		*/
/*	erzeugt und ueber node->image angesprochen. Die Liste aller	*/
/*	Bilder befindet sich in type->images.				*/
/*									*/
/*======================================================================*/
/*									*/
/*	Liste der vordefinierten Knotentypen :				*/
/*									*/
/*	struct {							*/
/*		char	*name;			type->filename		*/
/*		void	(*adjust_func)();	type->adjust_func	*/
/*		void	(*pr_paint_func)();	type->pr_paint_func	*/
/*		void	(*laser_paint_func)();	type->laser_paint_func	*/
/*	}								*/
/*		system_nodetypes;					*/
/*									*/
/*	Zur Zeit verfuegbar sind :					*/
/*	- #box, mit adjust_to_box_node und paint_box_node		*/
/*	- #circle, mit adjust_to_elliptical_node und			*/
/*	  paint_elliptical_node						*/
/*	- #diamond, mit adjust_to_elliptical_node und			*/
/*	  paint_elliptical_node						*/
/*									*/
/*======================================================================*/
/*									*/
/*	So sieht ein Kantentyp aus :					*/
/*									*/
/*	typedef	struct	edgetype					*/
/*	{								*/
/*			Pixrect	*pr;				*/
/*			char		filename[FILENAMESIZE];		*/
/*			int		used;				*/
/*	}								*/
/*		*Edgetype;						*/
/*									*/
/*									*/
/*	pr		Pixrect mit dem Kantenbild, aus dem einmal	*/
/*			Informationen ueber Strichbreite und Strichform	*/
/*			gewonnen derden sollen.				*/
/*	filename	In diesem File steht das Icon, aus dem pr	*/
/*			geladen wird.					*/
/*	used		Sooft wird der Kantentyp verwendet. Ein Typ	*/
/*			darf nur geloescht werden, wenn used == 0.	*/
/*									*/
/*	Kantentypen sind bis jetzt fuer das Zeichnen ohne Bedeutung,	*/
/*	werden aber in der Hoffnung auf eine spaetere Verwendung	*/
/*	mitgefuehrt.							*/
/*									*/
/*======================================================================*/
/*									*/
/*	Alle Knoten- und Kantentypen sind in Listen abgespeichert :	*/
/*									*/
/*	static	struct	{						*/
/*			int	    number_of_nodetypes;		*/
/*			Nodetype    list            [MAX_NODETYPE];	*/
/*			char	    *list_for_cycle [MAX_NODETYPE+3];	*/
/*		}							*/
/*		nodetypes;						*/
/*									*/	
/*	static	struct	{						*/
/*			int	    number_of_edgetypes;		*/
/*			Edgetype    list            [MAX_EDGETYPE];	*/
/*			char	    *list_for_cycle [MAX_EDGETYPE+3];	*/
/*		}							*/
/*		edgetypes;						*/
/*									*/
/*									*/
/*	MAX_EDGETYPES		Maximalzahl an moeglichen Typen		*/
/*	MAX_NODETYPES		fuer Knoten bzw. Kanten; deklariert	*/
/*				in config.h			*/
/*	number_of_nodetypes	Anzahl der vorhandenen Typen,		*/
/*	number_of_edgetypes	durch MAX_NODETYPES bzw. MAX_EDGETYPES	*/
/*				beschraenkt				*/
/*	list			Liste der Knoten- bzw. Kantentypen	*/
/*	list_for_cycle		Liste, mit denen die Knoten- bzw.	*/
/*				Kantentypen in Panel_cycle's abgebildet	*/
/*				werden koennen. Details siehe unten.	*/
/*									*/
/*									*/
/*	ACHTUNG : Diese Tabelle ist ausserhalb von type.c nicht		*/
/*	sichtbar. Der Zugriff erfolgt von aussen ueber die Indices	*/
/*									*/
/*	current_nodetype_index	Index des aktuellen Knotentyps, MAKRO !	*/
/*	current_edgetype_index	Index des aktuellen Kantentyps, MAKRO !	*/
/*									*/
/*	Die Indices fuer Knoten- bzw. Kantentypen liegen in dem		*/
/*	Intervall [0..nodetypes.number_of_types-1] bzw.			*/
/*	          [0..edgetypes.number_of_types-1].			*/
/*	Die "Variablen" liegen in Wirklichkeit in graph_state		*/
/*	(-> graph.h)							*/
/*									*/
/*	DER INDEX DES CURRENT_NODE/EDGEFONT KANN SICH NACH INSERT_FONT	*/
/*	ODER DELETE_FONT AENDERN !					*/
/*									*/
/*									*/
/*	Format von list_for_cycle :					*/
/*									*/
/*	PANEL_CHOICE_IMAGES						*/
/*		.							*/
/*		.							*/
/*		.							*/
/*		.							*/
/*		(nodetype_images/edgetype_images)			*/
/*		.							*/
/*		.							*/
/*		.							*/
/*		0   (Ende Images)					*/
/*	0   (Ende Liste)						*/
/*									*/
/*	Diese Liste kann an ein PANEL_CYCLE als ATTR_LIST uebergeben	*/
/*	werden. Dann erscheinen die Bilder der Knotentypen bzw.		*/
/*	Kantentypen (pr in Nodetype bzw. Edgetype) im cycle-Menue.	*/
/*									*/
/************************************************************************/
/************************************************************************/
/*									*/
/*	KNOTEN- UND KANTENTYPEN EINFUEGEN UND LOESCHEN			*/
/*									*/
/************************************************************************/
/*									*/
/*	int	insert_nodetype (filename, insert_position)		*/
/*	int	insert_edgetype (filename, insert_position)		*/
/*									*/
/*	Fuegt einen Knoten- bzw. Kantentyp aus filename ein. Rueckgabe	*/
/*	ist der Index, an dem tatsaechlich eingefuegt wurde :		*/
/*	Strategie :							*/
/*	- gibt es den Typ mit filename schon, so melde nur seinen Index	*/
/*	  (insert_position wird in diesem Fall ausser Acht gelassen).	*/
/*	  Sonst :							*/
/*	- ist insert_postion < ...types.number_of_..._types, so fuege	*/
/*	  vor insert_position ein					*/
/*	- ist insert_position = types.number_of_types, so haenge an	*/
/*	  die Liste an.							*/
/*									*/
/*	Kann die Datei unter filename nicht gefunden werden und		*/
/*	ist filename kein absoluter Pfad, so wird in den im		*/
/*	Environment unter GRAPHED_INPUTS angebenenen Directories	*/
/*	nachgesehen							*/
/*									*/
/*	Falls schon zu viele Typen da sind oder das File nicht		*/
/*	geoeffnet werden kann, wird eine Fehlermeldung ausgegeben und	*/
/*	anstatt eines Index -1 zurueckgegeben.				*/
/*									*/
/*----------------------------------------------------------------------*/
/*									*/
/*	int	add_nodetype (filename)					*/
/*	int	add_edgetype (filename)					*/
/*									*/
/*	= insert_...type (filename, ...types.number_of_...types, d.h.	*/
/*	Anhangen an die Liste, wenn nicht schon vorhanden.		*/
/*									*/
/*----------------------------------------------------------------------*/
/*									*/
/*	int	delete_nodetype (delete_position)			*/
/*	int	delete_edgetype (delete_position)			*/
/*									*/
/*	Loescht den Knoten- bzw. Kantentyp mit Index delete_position.	*/
/*	Strategie :							*/
/*	- delete_position = current_...type_index > 0 :			*/
/*	  neuer current_...type_index wird der Knoten- bzw. Kantentyp,	*/
/*	  der vor delete_position liegt					*/
/*	- delete_position = current_...type_index = 0 :			*/
/*	  neuer current_...type_index wird der neue erste Knoten- bzw.	*/
/*	  Kantentyp.							*/
/*									*/
/*	Ist nur noch ein Typ uebrig, so darf dieser sinnvollerweise	*/
/*	nicht auch noch geloescht werden. Dann wird eine Fehlermeldung	*/
/*	aus- und FALSE zurueckgegeben, ansonsten natuerlich TRUE.	*/
/*									*/
/*----------------------------------------------------------------------*/
/*									*/
/*	ACHTUNG * WICHTIG * ACHTUNG * WICHTIG * ACHTUNG * WICHTIG	*/
/*									*/
/*	Alle Prozeduren lassen den current_nodetype bzw.		*/
/*	current_edgetype unveraendert. Dazu ist es notwendig, bei	*/
/*	insert_nodetype und delete_nodetype bzw. insert_edgetype und	*/
/*	delete_edgetype ggf. den current_nodetype_index bzw.		*/
/*	current_edgetype_index zu aendern.				*/
/*									*/
/************************************************************************/


int		insert_nodetype (filename, insert_position)
char 		*filename;		/* einzufuegender Font		*/
int		insert_position;	/* Wo ?				*/
{
	Nodetype	new_nodetype;
	char		errormsg [IL_ERRORMSG_SIZE];
	char		*extended_filename;
	int		i;
	Pixrect		*pr;

	if ((i = find_nodetype(filename)) != -1)
		return i;
	if (nodetypes.number_of_nodetypes == MAX_NODETYPE) {
		error ("Can't insert a new type - too many types\n");
		return (-1);
	}

	new_nodetype = (Nodetype) mymalloc(sizeof(struct nodetype));
	
	if (filename[0] == SYSTEM_TYPES_IDENTIFICATION_CHARACTER) {
	
		/* Lade Knotentyp aus system_nodetypes	*/
		
		int	found_system_nodetype = FALSE;
		
		for (i=0; !found_system_nodetype && i < NUMBER_OF_SYSTEM_NODETYPES; i++)
		    if (!strcmp (filename, system_nodetypes[i].name)) {
			found_system_nodetype   = TRUE;
		    	new_nodetype->is_system = TRUE;
			new_nodetype->pr_paint_func  = system_nodetypes[i].pr_paint_func;
			new_nodetype->adjust_func = system_nodetypes[i].adjust_func;
			new_nodetype->laser_paint_func = system_nodetypes[i].laser_paint_func;
			new_nodetype->pr = mem_create (DEFAULT_ICON_WIDTH, DEFAULT_ICON_HEIGHT, DEFAULT_ICON_DEPTH);
			if (new_nodetype->pr == (Pixrect *)NULL) {
				error ("Insufficient memory for nodetype\n");
				return (-1);
			}
			paint_nodetype_on_pr (new_nodetype, new_nodetype->pr,
				new_nodetype->pr->pr_size.x,
				new_nodetype->pr->pr_size.y,
				1);
		    }
		if (!found_system_nodetype) {
			myfree (new_nodetype);
			error ("Can't find system nodetype %s\n", filename);
			return (-1);
		}
	
	} else {
	
		new_nodetype->is_system = FALSE;
		extended_filename = file_exists_somewhere (filename, getenv ("GRAPHED_INPUTS"));
		if ( extended_filename == NULL ) {
			myfree (new_nodetype);
			error ("Cannot find file %s anywhere\n", filename);
			return (-1);
		} else if ((new_nodetype->pr = icon_load_mpr (extended_filename, errormsg)) == (Pixrect *)NULL) {
			FILE *f = fopen (extended_filename, "r");
			new_nodetype->pr = pr_load (f, NULL);
			fclose (f);
			if (new_nodetype->pr == (Pixrect *)NULL) {
				myfree (new_nodetype);
				error ("Cannot load nodetype\n");
				return (-1);
			}
		}
		
	}
	
	new_nodetype->images = (Nodetypeimage)NULL;
	new_nodetype->used = 0;
	strcpy (new_nodetype->filename = mymalloc(strlen(filename)+1), filename);

	for (i = nodetypes.number_of_nodetypes; i > insert_position; i--)
		nodetypes.list[i] = nodetypes.list[i-1];
	nodetypes.list[insert_position] = new_nodetype;
	
	for (i = nodetypes.number_of_nodetypes+3; i > insert_position+1; i--)
		nodetypes.list_for_cycle[i] = nodetypes.list_for_cycle[i-1];

	pr = mem_create (DEFAULT_ICON_WIDTH, DEFAULT_ICON_HEIGHT, DEFAULT_ICON_DEPTH);
	paint_nodetype_on_pr (new_nodetype, pr,
	                      DEFAULT_ICON_WIDTH,
	                      DEFAULT_ICON_HEIGHT,
	                      DEFAULT_ICON_DEPTH);

	/* Added MH Conversion */
	nodetypes.list_for_cycle[insert_position+1] = (char *)pr_to_svi(pr);

	nodetypes.number_of_nodetypes ++;

	install_nodetypelist_for_cycle ();
	if (current_nodetype_index >= insert_position)
		set_current_nodetype (current_nodetype_index + 1);
	for (i=0; i<NUMBER_OF_NODE_STYLES; i++) {
		Node_attributes	node_attr;
		node_attr = get_node_style (i);
		if (node_attr.type_index >= insert_position) {
			node_attr.type_index++;
			set_node_style (i, node_attr);
		}
	}
	
	return(insert_position);
}


int		insert_edgetype (filename, insert_position)
char 		*filename;		/* einzufuegender Font		*/
int		insert_position;	/* Wo ?				*/
{
	Edgetype		new_edgetype;
	char			errormsg[IL_ERRORMSG_SIZE];
	char			*extended_filename;
	int			i;
	struct pr_texture	*new_texture;


	if ((i = find_edgetype(filename)) != -1)
		return i;
	if (edgetypes.number_of_edgetypes == MAX_EDGETYPE) {
		error ("Can't insert a new type - too many types\n");
		return (-1);
	}

	new_edgetype = (Edgetype) mymalloc(sizeof(struct edgetype));

	if ( filename[0] == SYSTEM_TYPES_IDENTIFICATION_CHARACTER ) {

		int	found_system_edgetype = FALSE;
		
		for (i=0; !found_system_edgetype && i < NUMBER_OF_SYSTEM_EDGETYPES; i++)
		    if (!strcmp (filename, system_edgetypes[i].name)) {
			found_system_edgetype   = TRUE;
			
			new_texture = (struct pr_texture *)NULL;
			
			strcpy (new_edgetype->filename = mymalloc(strlen(filename)+1), filename);
			new_edgetype->used = 0;
			if ( system_edgetypes[i].pattern != (short *)NULL ) {
				new_texture = (struct pr_texture *) mymalloc(sizeof(struct pr_texture));
				
				new_texture->pattern = system_edgetypes[i].pattern;
				new_texture->offset = 0;
				new_texture->options.startpoint = 1;
				new_texture->options.endpoint = 1;
				new_texture->options.balanced = 0;
				new_texture->options.givenpattern = 0;
			}
			new_edgetype->texture = new_texture;
			
			new_edgetype->pr = mem_create (DEFAULT_ICON_WIDTH, DEFAULT_ICON_HEIGHT, DEFAULT_ICON_DEPTH);
			if (new_edgetype->pr == (Pixrect *)NULL) {
				error ("Insufficient memory for edgetype\n");
				return (-1);
			}
			paint_edgetype_on_pr (new_edgetype, new_edgetype->pr,
				new_edgetype->pr->pr_size.x,
				new_edgetype->pr->pr_size.y,
				1);

		    }
		if (!found_system_edgetype) {
			myfree (new_edgetype);
			error ("Can't find system edgetype %s\n", filename);
			return (-1);
		}
	
	} else {
		/* Correction MH 12/6/89	*/
		return insert_edgetype ("#solid", insert_position);
/*
		error("%s : \nNo user defined edgetypes implemented yet\n", filename);
		return(-1);
*/
	}
	
	for (i = edgetypes.number_of_edgetypes; i > insert_position; i--)
			edgetypes.list[i] = edgetypes.list[i-1];
	edgetypes.list[insert_position] = new_edgetype;

	for (i = edgetypes.number_of_edgetypes+3; i > insert_position+1; i--)
		edgetypes.list_for_cycle[i] = edgetypes.list_for_cycle[i-1];
	edgetypes.list_for_cycle[insert_position+1] =
		(char *)pr_to_svi(edgetypes.list[insert_position]->pr);

	edgetypes.number_of_edgetypes ++;

	install_edgetypelist_for_cycle ();
	if (current_edgetype_index >= insert_position)
		set_current_edgetype (current_edgetype_index + 1);
	for (i=0; i<NUMBER_OF_EDGE_STYLES; i++) {
		Edge_attributes	edge_attr;
		edge_attr = get_edge_style (i);
		if (edge_attr.type_index >= insert_position) {
			edge_attr.type_index++;
			set_edge_style (i, edge_attr);
		}
	}

	return(insert_position);
}


int	add_nodetype (filename)
char	*filename;
{
	return	insert_nodetype (filename, nodetypes.number_of_nodetypes);
}


int	add_edgetype (filename)
char	*filename;
{
	return	insert_edgetype (filename, edgetypes.number_of_edgetypes);
}



int	delete_nodetype (delete_position)
int	delete_position;
{
	Nodetype	old_nodetype;
	int		i;

	if (nodetypes.number_of_nodetypes == 1) {
		error ("Only one type left - Can't delete it\n");
		return (FALSE);
	}
	if (nodetypes.list[delete_position]->used != 0) {
		error ("This nodetype is used - Can't delete it\n");
		return (FALSE);
	}
	
	old_nodetype = nodetypes.list[delete_position];
	
	for (i = delete_position+1; i < nodetypes.number_of_nodetypes; i++)
		nodetypes.list[i-1] = nodetypes.list[i];
	
	for (i = delete_position+2; i < nodetypes.number_of_nodetypes+3; i++)
		nodetypes.list_for_cycle[i-1] = nodetypes.list_for_cycle[i];

	nodetypes.number_of_nodetypes--;

	install_nodetypelist_for_cycle ();
	if (current_nodetype_index >= delete_position)
		set_current_nodetype (maximum (current_nodetype_index-1,0));
	for (i=0; i<NUMBER_OF_NODE_STYLES; i++) {
		Node_attributes	node_attr;
		node_attr = get_node_style (i);
		if (node_attr.type_index == delete_position) {
			node_attr.type_index = current_nodetype_index;
			set_node_style (i, node_attr);
		} else if (node_attr.type_index > delete_position) {
			node_attr.type_index = maximum (node_attr.type_index-1,0);
			set_node_style (i, node_attr);
		}
	}

	pr_destroy (old_nodetype->pr);
	myfree       (old_nodetype->filename);
	myfree       (old_nodetype);

	return (TRUE);
}


int	delete_edgetype (delete_position)
int	delete_position;
{
	Edgetype	old_edgetype;
	int		i;

	if (edgetypes.number_of_edgetypes == 1) {
		error ("Only one type left - Can't delete it\n");
		return (FALSE);
	}
	if (edgetypes.list[delete_position]->used != 0) {
		error ("This edgetype is used - Can't delete it\n");
		return (FALSE);
	}

	old_edgetype = edgetypes.list[delete_position];

	for (i = delete_position+1; i < edgetypes.number_of_edgetypes; i++)
		edgetypes.list[i-1] = edgetypes.list[i];
		
	for (i = delete_position+2; i < edgetypes.number_of_edgetypes+3; i++)
		edgetypes.list_for_cycle[i-1] = edgetypes.list_for_cycle[i];

	edgetypes.number_of_edgetypes--;

	install_edgetypelist_for_cycle ();
	if (current_edgetype_index >= delete_position)
		set_current_edgetype (maximum (current_edgetype_index-1,0));
	for (i=0; i<NUMBER_OF_EDGE_STYLES; i++) {
		Edge_attributes	edge_attr;
		edge_attr = get_edge_style (i);
		if (edge_attr.type_index == delete_position) {
			edge_attr.type_index = current_edgetype_index;
			set_edge_style (i, edge_attr);
		} else if (edge_attr.type_index > delete_position) {
			edge_attr.type_index = maximum (edge_attr.type_index-1,0);
			set_edge_style (i, edge_attr);
		}
	}

	pr_destroy (old_edgetype->pr);
	myfree       (old_edgetype->filename);
	myfree       (old_edgetype);
	
	return (TRUE);
}
/************************************************************************/
/*									*/
/*		AKTUELLER KNOTEN- BZW. KANTENTYP			*/
/*									*/
/************************************************************************/
/*									*/
/*	Nodetype	get_current_nodetype ()				*/
/*	Edgetype	get_current_edgetype ()				*/
/*									*/
/*	Direct access to the current node- resp. edgetype		*/
/*									*/
/************************************************************************/


Edgetype	get_current_edgetype ()
{
	return edgetypes.list [current_edgetype_index];
}


Nodetype	get_current_nodetype ()
{
	return nodetypes.list [current_nodetype_index];
}

/************************************************************************/
/*									*/
/*		KNOTEN- BZW. KANTENTYPEN SUCHEN UND VERWENDEN		*/
/*									*/
/************************************************************************/
/*									*/
/*	Nodetype	use_nodetype   (nodetype_index)			*/
/*	Edgetype	use_edgetype   (edgetype_index)			*/
/*	void		unuse_nodetype (nodetype)			*/
/*	void		unuse_edgetype (edgetype)			*/
/*									*/
/*	Melden die Benutzung eines Knoten- bzw. Kantentyps an oder ab.	*/
/*	Der von use_nodetype bzw. use_edgetype zurueckgegebene Typ muss	*/
/*	bei unuse_nodetype bzw. unuse_edgetype als Argument uebergeben	*/
/*	werden.								*/
/*	use_nodetype bzw. use_edgetype zaehlt				*/
/*	...types.list[type_index]->used hoch, unuse_nodetype bzw.	*/
/*	unuse_edgetype zaehlt ...types.list[type_index]->used herunter.	*/
/*	Knoten- und Kantentypen koennen nur geloescht werden, wenn	*/
/*	used == 0.							*/
/*	unuse_nodetype und unuse_edgetype sind gegen NULL als Argument	*/
/*	und used == 0 unempfindlich.					*/
/*									*/
/*	ACHTUNG * WICHTIG * ACHTUNG * WICHTIG * ACHTUNG * WICHTIG	*/
/*									*/
/*	Soll ein Knoten- oder Kantentyp in einem Knoten bzw. einer	*/
/*	Kante verwendet werden, muss er unbedingt mit use_...type an-	*/
/*	und unuse_...type abgemeldet werden, da sonst eine		*/
/*	versehentliche Loeschung und damit Zugriff ueber einen		*/
/*	"dangling pointer" mit all seinen Konsequenzen ("core dumped")	*/
/*	moeglich ist !							*/
/*									*/
/*======================================================================*/
/*									*/
/*	Nodetypeimage	use_nodetypeimage   (type, w,h)			*/
/*	void		unuse_nodetypeimage (type, image)		*/
/*									*/
/*	Auch die Benutzung von Knotenbildern muss angemeldet werden.	*/
/*	w und h sind die Bildgroesse, type ist der Knotentyp, von dem	*/
/*	das Bild gemacht werden soll. Ansonsten gilt sinngemaess das	*/
/*	oben gesagte.							*/
/*									*/
/*======================================================================*/
/*									*/
/*	int	get_nodetype_index (nodetype)				*/
/*	int	get_edgetype_index (edgetype)				*/
/*									*/
/*	Gibt den Index des angegebenen Knoten- bzw. Kantentyps wieder.	*/
/*	DER INDEX KANN NACH INSERT_NODETYPE BZW. DELETE_NODETYPE	*/
/*	UNGUELTIG WERDEN.						*/
/*	Wenn nicht gefunden, Rueckgabe -1.				*/
/*									*/
/*======================================================================*/
/*									*/
/*	int	find_nodetype (filename)				*/
/*	int	find_edgetype (filename)				*/
/*									*/
/*	Sucht Knoten- bzw. Kantentyp mit dem Filenamen filename.	*/
/*	Rueckgabe nodetype_index bzw. edgetype_index oder -1, wenn	*/
/*	nicht gefunden.							*/
/*									*/
/*======================================================================*/
/*									*/
/*	Nodetype	get_nodetype (index)				*/
/*									*/
/*	Get nodetypes.list[index]. May NOT be used for entering a	*/
/*	nodetype in a node, only for reading the contents of a		*/
/*	nodetype.							*/
/*									*/
/************************************************************************/


Nodetype	use_nodetype (nodetype_index)
int		nodetype_index;
{
	nodetypes.list[nodetype_index]->used ++;
	return nodetypes.list[nodetype_index];
}


void		unuse_nodetype (nodetype)
Nodetype	nodetype;
{
	if (nodetype != (Nodetype)NULL)
		nodetype->used --;
}



Edgetype	use_edgetype (edgetype_index)
int		edgetype_index;
{
	edgetypes.list[edgetype_index]->used ++;
	return edgetypes.list[edgetype_index];
}


void		unuse_edgetype (edgetype)
Edgetype	edgetype;
{
	if (edgetype != (Edgetype)NULL)
		edgetype->used --;
}



Nodetypeimage	use_nodetypeimage (type, w,h)
Nodetype	type;
int		w,h;
{
	register Nodetypeimage	image,
	                        last_image;
	
	image      = type->images;
	last_image = (Nodetypeimage)NULL;
	while (image != (Nodetypeimage)NULL && (image->sx != w || image->sy != h)) {
		last_image = image;
		image      = image->suc;
	}
	
	if (image == (Nodetypeimage)NULL) { /* make a new one */
		image = (Nodetypeimage)mymalloc (sizeof (struct nodetypeimage));
		image->sx = w;
		image->sy = h;
		image->pr = mem_create (w,h, 1);
		if (image->pr == (Pixrect *)NULL) {
			fatal_error ("Insufficient memory for nodetypeimage\n");
		}
		image->used = 0;
		if (last_image == (Nodetypeimage)NULL) { /* erster	*/
			image->pre   = (Nodetypeimage)NULL;
			image->suc   = type->images;
			if (image->suc != (Nodetypeimage)NULL)
				image->suc->pre = image;
			type->images = image;
		} else {
			image->pre      = last_image;
			image->suc      = last_image->suc;
			image->pre->suc = image;
			if (image->suc != (Nodetypeimage)NULL)
				image->suc->pre = image;
		}
		paint_nodetype_on_pr (type, image->pr, w,h, 1);
	}
	
	image->used ++;
	type->used  ++;	/* Sicherstellung, dass kein Typ geloescht	*/
			/* wird, wenn noch ein Bild von ihm im Umlauf	*/
			/* ist						*/
	
	return image;
}

void		unuse_nodetypeimage (type, image)
Nodetype	type;
Nodetypeimage	image;
{
	if (type != (Nodetype)NULL && image != (Nodetypeimage)NULL) {
		type->used  --;
		image->used --;
		if (image->used == 0) { /* remove */
			if (image == type->images) {
				type->images    = image->suc;
				if (image->suc != (Nodetypeimage)NULL)
					image->suc->pre = (Nodetypeimage)NULL;
			} else {
				image->pre->suc = image->suc;
				if (image->suc != (Nodetypeimage)NULL)
					image->suc->pre = image->pre;
			}
			pr_destroy (image->pr);
			myfree (image);
		}
	}
}


int		get_nodetype_index (nodetype)
Nodetype	nodetype;
{
	int	i;
	
	for (i=0; i < nodetypes.number_of_nodetypes; i++)
		if (nodetype == nodetypes.list[i])
			return i;
	return -1;
}


int		get_edgetype_index (edgetype)
Edgetype	edgetype;
{
	int	i;
	
	for (i=0; i < edgetypes.number_of_edgetypes; i++)
		if (edgetype == edgetypes.list[i])
			return i;
	return -1;
}


int	find_nodetype (filename)
char	*filename;
{
	int	i;
	
	for (i=0; i < nodetypes.number_of_nodetypes; i++)
		if (!strcmp(nodetypes.list[i]->filename, filename))
			return i;

	return -1;
}


int	find_edgetype (filename)
char	*filename;
{
	int	i;
	
	for (i=0; i < edgetypes.number_of_edgetypes; i++)
		if (!strcmp(edgetypes.list[i]->filename, filename))
	     		return i;

	return -1;
}


Nodetype	get_nodetype (index)
int		index;
{
	if (index > nodetypes.number_of_nodetypes) 
		return (Nodetype)NULL;
	else 
		return nodetypes.list[index];
}

/************************************************************************/
/*									*/
/*			LIST_FOR_CYCLE VERWALTEN			*/
/*									*/
/************************************************************************/
/*									*/
/*	static	void	install_nodetypelist_for_cycle ()		*/
/*	static	void	install_edgetypelist_for_cycle ()		*/
/*	void		install_current_nodetype       ()		*/
/*	void		install_current_edgetype       ()		*/
/*									*/
/*	Die install_... - Prozeduren setzen die				*/
/*	...types.list_for_cycle's bzw. current_nodetype_index bzw.	*/
/*	current_edgetype_index ueber weitere install_... - Prozeduren	*/
/*	aus anderen Modulen an den benoetigten Stellen der graphischen	*/
/*	Oberflaeche ein.						*/
/*									*/
/************************************************************************/


static	void	install_nodetypelist_for_cycle ()
{
	install_nodetypelist_in_node_subframe          (nodetypes.list_for_cycle);
	install_nodetypelist_in_group_subframe         (nodetypes.list_for_cycle);
	install_nodetypelist_in_nodetype_selection     (nodetypes.list_for_cycle);
	install_nodetypelist_in_node_defaults_subframe (nodetypes.list_for_cycle);
}


static	void	install_edgetypelist_for_cycle ()
{	
	install_edgetypelist_in_edge_subframe          (edgetypes.list_for_cycle);
	install_edgetypelist_in_group_subframe         (edgetypes.list_for_cycle);
	install_edgetypelist_in_edgetype_selection     (edgetypes.list_for_cycle);
	install_edgetypelist_in_edge_defaults_subframe (edgetypes.list_for_cycle);
}



void	install_current_nodetype ()
{
	install_current_nodetype_in_nodetype_selection     ();
}


void	install_current_edgetype ()
{
	install_current_edgetype_in_edgetype_selection     ();
}
/************************************************************************/
/*									*/
/*		KNOTEN- UND KANTENTYPEN INITIALISIEREN			*/
/*			KONFIGURATION ABSPEICHERN			*/
/*									*/
/************************************************************************/
/*									*/
/*	void		init_types ()					*/
/*									*/
/*	Initialisiert die Strukturen nodetypes und edgetypes.		*/
/*	Nur beim Hochfahren des Programms zu verwenden !		*/
/*									*/
/*======================================================================*/
/*									*/
/*	void		write_nodetypes (file)				*/
/*	void		write_edgetypes (file)				*/
/*									*/
/*	Schreibt die Liste der Knoten- bzw. Kantentypen auf file.	*/
/*	Die Datei muss offen sein und wird nicht geschlossen.		*/
/*	Das Format ist (natuerlich) voll kompatibel zu den		*/
/*	entsprechenden Teilen der Grammatik in parser.y.		*/
/*									*/
/************************************************************************/


void	init_types()
{
	nodetypes.number_of_nodetypes = 0;
	edgetypes.number_of_edgetypes = 0;
	nodetypes.list_for_cycle [0] = (char *)PANEL_CHOICE_IMAGES;
	nodetypes.list_for_cycle [1] = (char *)0;
	nodetypes.list_for_cycle [2] = (char *)0;
	edgetypes.list_for_cycle [0] = (char *)PANEL_CHOICE_IMAGES;
	edgetypes.list_for_cycle [1] = (char *)0;
	edgetypes.list_for_cycle [2] = (char *)0;
}


int	write_nodetypes (file)
FILE	*file;
{
	int	i;

	for (i=0; i < nodetypes.number_of_nodetypes; i++) {
		write_quoted_text (file, nodetypes.list[i]->filename);
		fprintf (file, "\n");
	}
}


int	write_edgetypes (file)
FILE	*file;
{
	int	i;

	for (i=0; i < edgetypes.number_of_edgetypes; i++) {
		write_quoted_text (file, edgetypes.list[i]->filename);
		fprintf (file, "\n");
	}
}
