/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt, Thomas Lamshoeft	*/
/************************************************************************/
/*									*/
/*				paint.c					*/
/*									*/
/************************************************************************/

#include "misc.h"
#include "graph.h"
#include "paint.h"

#include "graphed_subwindows.h"

/************************************************************************/
/*									*/
/*		GLOBALE FUNKTIONEN UND PROZEDUREN			*/
/*									*/
/************************************************************************/
/*									*/
/*	void	paint_node_internal (node, op)				*/
/*	void	paint_virtual_node  (x,y, w,h, image, op)		*/
/*									*/
/*	void	paint_nodetype_on_pr (type, pr, w,h)			*/
/*									*/
/*	void	pr_paint_box_node       (pr, x,y,   w,h)		*/
/*	void	pr_paint_elliptical_node(pr, x0,y0, a,b)		*/
/*	void	pr_paint_diamond_node   (pr, x0,y0, a,b)		*/
/*									*/
/*	void	pw_paint_box_node       (pw, x,y,   w,h)		*/
/*	void	pw_paint_elliptical_node(pw, x0,y0, a,b)		*/
/*	void	pw_paint_diamond_node   (pw, x0,y0, a,b)		*/
/*									*/
/*	void	paint_single_edgeline_internal (el, op)			*/
/*	void	paint_line_internal            (x0,y1, x1,y1, op)	*/
/*	void	paint_edgelabel_internal       (edge, op)		*/
/*									*/
/*	Rect	*set_clip_region (clip_rect)				*/
/*									*/
/*	void	paint_dot_internal     (x,y)				*/
/*	void	paint_rectangle        (pw, x1,y1, x2,y2)		*/
/*	void	paint_line             (pw, x1,y1, x2,y2)		*/
/*	void	paint_marker_square    (pw, x,y)			*/
/*	void	paint_marker_rect      (pw, x,y)			*/
/*	void	paint_marker_rectangle (pw, x,y)			*/
/*									*/
/************************************************************************/
/************************************************************************/
/*									*/
/*			LOKALE VARIABLEN				*/
/*									*/
/************************************************************************/


Pixrect	*paint_pr = NULL;


/************************************************************************/
/*									*/
/*			LOKALE VARIABLEN				*/
/*									*/
/************************************************************************/


static	int	offset_x = 0;		/* zum Zeichnen in	*/
static	int	offset_y = 0;		/* Clip-Rechteck	*/
/************************************************************************/
/*									*/
/*			ALLGEMEINES					*/
/*									*/
/************************************************************************/
/*									*/
/*			Namensgebung					*/
/*									*/
/*	Prozeduren :							*/
/*									*/
/*	..._internal	Diese Prozeduren sind die eigentlichen Zeichen-	*/
/*			prozeduren. Hier wird auf das Pixrect		*/
/*			paint_pr (s.u.) gezeichnet. force_repainting	*/
/*			(-> repaint.c) etc. arbeiten immer (bis auf	*/
/*			do_unmark_...) ueber diese Prozeduren (sie	*/
/*			werden auch an keiner anderen Stelle verwendet)	*/
/*	..._virtual_...	Hier wird ein "virtuelles" Objekt gezeichnet.	*/
/*			Virtuelle Objekte werden mit PIX_XOR direkt auf	*/
/*			working_area_pixwin_gezeichnet und innerhalb	*/
/*			des "normalen" Neuzeichnens in repaint.c	*/
/*			nicht beachtet.					*/
/*									*/
/*	Argumente :							*/
/*									*/
/*	op		ist immer eine RasterOp - Operation. Naehres	*/
/*			die Handbuecher zu dem Graphiksystem "Pixrect"	*/
/*			und zu "Pixwin" in "SunView".			*/
/*	pr		ist immer ein Pixrect, auf das gezeichnet wird.	*/
/*									*/
/*	Ist kein Pixrect als Argument vorhanden und faellt die Prozedur	*/
/*	nicht unter die beiden oben angegebenen Kategorien, so wird	*/
/*	immer auf die Zeichenflaeche (working_area_canvas_pxiwin)	*/
/*	ausgegeben (ein  Argument op ist in einem solchen Fall immer	*/
/*	vorhanden).							*/
/*									*/
/*======================================================================*/
/*									*/
/*		Die Sache mit "paint_pr"				*/
/*									*/
/*	Beim Neuzeichnen von Teilen des Graphen wird immer		*/
/*	Rechteckweise vorgegangen. Jedes dieser Rechtecke wird einzeln	*/
/*	neu gezeichnet. Dazu wird ein Bitmap der benoetigten Groesse im	*/
/*	Speicher unter der Variablen paint_pr angelegt, das fuer das	*/
/*	Neuzeichnen verwendet wird.					*/
/*	Aus technischen Gruenden muessen dann alle Koordinaten im	*/
/*	System dieses paint_pr liegen, so dass in den Prozeduren,	*/
/*	die auf paint_pr zeichnen (Name paint_..._internal), eine	*/
/*	Transformation erfolgen muss. Diese erfolgt mit den in diesem	*/
/*	Modul deklarierten lokalen Variablen offset_x und offset_y.	*/
/*									*/
/************************************************************************/
/************************************************************************/
/*									*/
/*			KNOTEN ZEICHNEN					*/
/*									*/
/************************************************************************/
/*									*/
/*	void	paint_node_internal (node, op)				*/
/*									*/
/*	Bei systemdefinierten Knotentypen wird auf type->pr_paint_func	*/
/*	zurueckgegriffen.						*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	paint_virtual_node (x,y, w,h, image, op)		*/
/*									*/
/*	Zeichnet einen "virtuellen" Knoten im Rechteck (x,y, w,h)	*/
/*	mit Operation op.						*/
/*									*/
/************************************************************************/


void	paint_node_internal (node, op)
Node	node;
int	op;
{
	int	line;
	
	if (node->type->is_system) {
		node->type->pr_paint_func (paint_pr,
			(int)(node_left(node) - offset_x),
			(int)(node_top(node) - offset_y),
			(int) node_width (node),
			(int) node_height (node),
			op);
	} else {
		pr_rop (paint_pr,
			(int)(node_left(node) - offset_x),
		        (int)(node_top(node) - offset_y),
			(int) node_width (node),
			(int) node_height (node),
			op,
			node->image->pr, 0, 0);
	}
	
	if (node->label.visible && node->label.text_to_draw != NULL) {
		for (line = 0; node->label.text_to_draw[line] != NULL; line++) {
			pr_text (paint_pr,
				node->label.x - offset_x,
				node->label.y - offset_y + line * node->label.line_height,
				op,
				node->label.font->font,
				node->label.text_to_draw[line]);
		}
	}
}



void		paint_virtual_node (x,y, w,h, image, op)
int		x,y, w,h;
Nodetypeimage	image;
int		op;
{
	pw_rop (working_area_canvas_pixwin,
	        x-w/2,y-h/2, w,h,
		op,
		image->pr, 0, 0);
}

/************************************************************************/
/*									*/
/*   selbst entwickelte Routine zum Zeichnen von gestrichelten Linien	*/
/*									*/
/************************************************************************/
/*									*/
/* void 	PR_line( pr, x0, y0, x1, y1, brush, tex, op )		*/
/*									*/
/* PR_line(...) ist "voll" kompatibel zur Routine pr_line(...) in der	*/
/* PIXRECT-Library. "voll" bezieht sich auf die Parameteruebergabe und	*/
/* das Verhalten im GRAPHED. Es gibt noch folgendes zu bemerken :	*/
/*									*/
/* - der Parameter brush wird ignoriert					*/
/* - die Parameter tex->options.givenpattern und tex->options.balanced	*/
/*   werden ignoriert. Die Routine nimmt immer an, dass beide auf	*/
/*   (unsigned) 0 gesetzt sind.						*/
/* - der Parameter tex->offset wird ignoriert und von der Routine immer	*/
/*   auf (short) 0 gesetzt.						*/
/*									*/
/* Beschreibung und Funktion der angegebenen Parameter : siehe PIXRECT	*/
/* REFERENCE MANUAL bei "struct pr_texture" und "#define pr_line(...)"	*/
/*									*/
/************************************************************************/

void PR_line( pr, x0, y0, x1, y1, brush, tex, op )
Pixrect 		*pr;
int 			x0, y0, x1, y1;
struct pr_brush 	*brush;	/* ignored here */
struct pr_texture	*tex;
int			op;
{
	register int	pr_put_value =
			iif (graphed_state.colorscreen == TRUE,
			     PIX_OPCOLOR(op),
			     iif (PIX_OPCOLOR(op) == 0, 0, 1));

/*  	#define TRUE (1==1) */

	register int	test,dx,dy,laufx,laufy,stepy,stepx,
			streckungsfaktor, zaehler,
			muster_index, muster_kopie;
	int		PEN_DOWN = TRUE;

	if (tex == (struct pr_texture *)NULL) {		/* ==> solid line */
		pr_vector( pr, x0,y0, x1,y1, op, PIX_OPCOLOR(op));
		return;
	}

	dx = x1 - x0;
	dy = y1 - y0;
	stepy = 1;	/* default: nach unten */
	if (dy < 0) {	/* Pech gehabt: es geht nach oben */
		stepy = -1;
		dy = -dy;	/* ==> dy >= 0 ! */
	}
	stepx = 1;	/* default: nach rechts */
	if (dx < 0) {	/* Pech gehabt: es geht nach links */
		stepx = -1;
		dx = -dx;	/* ==> dx >= 0 ! */
	}

	laufx = x0; laufy = y0; /* Laufvariablen auf Startpunkt setzen	*/

/*----------------------------------------------------------------------*/
/* Berechnung des Streckungsfaktors fuer das Linienmuster, um immer	*/
/* optisch gleiche Linien zu erhalten, unabhaengig von der Steigung der */
/* Linie. Der Streckungsfaktor ist eine Funktion in Abhaengigkeit von 	*/
/* der Steigung der Linie. Diese Funktion wird linear interpoliert. Der	*/
/* dabei auftretende Fehler ist < 1/1000.				*/
/*----------------------------------------------------------------------*/

	streckungsfaktor = 256;	/* 1.00 in 8-bit Fixkomma-Darstellung	*/

	/* Berechnung der "Steigung" der Linie in 8-bit Fixkomma	*/
	if (dx >= dy) {
		if (dx != 0 ) { streckungsfaktor = (dy * 256) / dx; }
	} else {
		streckungsfaktor = (dx * 256) / dy;
	}

	/* lineare Interpolation des Streckungsfaktors in Abhaengigkeit	*/
	/* von der eben berechneten Steignung der Linie (jetzt aber als	*/
	/* 16-bit Fixkomma-Wert)					*/
	if (streckungsfaktor >= 107) {		
		streckungsfaktor = 99855 - 134 * streckungsfaktor;
	} else {
		if (streckungsfaktor >= 45) {
			streckungsfaktor = 95468 - 93 * streckungsfaktor;
		} else {
			streckungsfaktor = 92678 - 31 * streckungsfaktor;
		}
	}
/*-------------Ende Berechnung Streckungsfaktor-------------------------*/

	zaehler = streckungsfaktor;
	muster_index = 0;
	muster_kopie = tex->pattern[0];
	
	if (tex->options.startpoint == 1) {
		pr_put (pr, laufx,laufy, pr_put_value);
	}
	if (dx >= dy) {	/* --> mehr in x-Richtung */
		test = ( dx >> 1 );
		while (laufx != x1) {
			laufx += stepx;
			test += dy;
			if (test >= dx) {
				laufy += stepy;
				test -= dx;
			}	
			if (PEN_DOWN) {
				pr_put (pr, laufx,laufy, pr_put_value);
			}
			zaehler -= 65536;
			if (zaehler <= 0) {
				zaehler += streckungsfaktor;
				muster_kopie--;
				if (muster_kopie <= 0){
					PEN_DOWN = !PEN_DOWN;
					muster_kopie = tex->pattern[++muster_index];
					if (muster_kopie == 0) {
						muster_index = 0;
						muster_kopie = tex->pattern[0];
						PEN_DOWN = TRUE;
					}
				}
			}
		}
	} else {	/* mehr in y-Richtung */
	
		test = ( dy >> 1 );
		while (laufy != y1) {
			laufy += stepy;
			test += dx;
			if (test >= dy) {
				laufx += stepx;
				test -= dy;
			}
			if (PEN_DOWN) {
				pr_put (pr, laufx,laufy, pr_put_value);
			}
			zaehler -= 65536;
			if (zaehler <= 0) {
				zaehler += streckungsfaktor;
				muster_kopie--;
				if (muster_kopie <= 0){
					PEN_DOWN = !PEN_DOWN;
					muster_kopie = tex->pattern[++muster_index];
					if (muster_kopie == 0) {
						muster_index = 0;
						muster_kopie = tex->pattern[0];
						PEN_DOWN = TRUE;
					}
				}
			}
		}
	}
	if (tex->options.endpoint == 1) {
		pr_put (pr, x1,y1, pr_put_value);
	}
}
/************************************************************************/
/*									*/
/*		KNOTENTYP AUF PIXRECT ZEICHNEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	void	paint_nodetype_on_pr (type, pr, w,h)			*/
/*									*/
/*	Zeichnet auf pr ein Bild des Knotentyps type der Groesse	*/
/*	(w,h), ohne type->images zu benutzen (da naemlich mit dieser	*/
/*	Prozedur gerade type->images erzeugt wird). Dazu wird		*/
/*	folgendermassen vorgegangen :					*/
/*	- ist type->is_system = FALSE, so skaliert die Routine das	*/
/*	  Pixelmuster in type->pr auf die passende Groesse		*/
/*	- sonst kann type->pr_paint_func verwendet werden.		*/
/*	Der Speicherplatz fuer pr wird selbstverstaendlich		*/
/*	bereitgestellt.							*/
/*									*/
/************************************************************************/


void		paint_nodetype_on_pr (type, pr, w,h, color)
Nodetype	type;
Pixrect	*pr;
int		w,h;	/* Groesse des zu zeichnenden Bildes		*/
int		color;
{
	if (!type->is_system) {
		
		/* Skaliere von dem Muster in type->pr auf die		*/
		/* gewuenschte Groesse.					*/
		
		int		tw,th;	/* Groesse des Musters in type	*/	
		Pixrect	*tpr;	/* Pixel des Musters in type	*/
		int		i;
		
		
		tpr = type->pr;
		tw = tpr->pr_size.x;
		th = tpr->pr_size.y;
		
		if (w > 0 && h > 0) {
		
			register int     x,y, xx,yy, xx1,yy1;
			register Pixrect *tmp_pr;
			
			/* x,y :    Koordinaten in tpr			*/
			/* xx,yy :  Koordinaten des zu setzenden	*/
			/*          Punktes in pr			*/
			/* xx1-xx : Groesse des zu setzenden Punktes	*/
			/* yy1-yy : in pr				*/
			
			tmp_pr = mem_create (
				maximum (w,tw),
				maximum (h,th),
				pr->pr_depth);

			for (x = 0; x < tw; x++) {
			
				xx  = (x*w)/tw;
				xx1 = ((x+1)*w)/tw;
				
				if (xx1 == xx) xx1++;	/* sonst wird kein	*/
							/* Punkt gesetzt !	*/
				for (i = 0; i < xx1-xx; i++) {
					pr_rop (tmp_pr,
						xx+i, 0,
						1,    th,
						PIX_SRC | PIX_DST | PIX_COLOR (color),
						tpr,
						x, 0);
				}
					
			}
			
			
			for (y = 0; y < th; y++) {
			
				yy  = (y*h)/th;
				yy1 = ((y+1)*h)/th;
				
				if (yy1 == yy) yy1++;	/* sonst wird kein	*/
							/* Punkt gesetzt !	*/
				for (i = 0; i < yy1-yy; i++) {
					pr_rop (pr,
						0, yy+i,
						w, 1,
						PIX_SRC | PIX_DST | PIX_COLOR (color),
						tmp_pr,
						0, y);
				}
					
			}
			
			pr_destroy (tmp_pr);
		}
	
	} else {
	
		type->pr_paint_func (pr, 0,0,w,h, PIX_SRC | PIX_COLOR (color));
	
	}
}
/************************************************************************/
/*									*/
/*		KANTENTYP AUF PIXRECT ZEICHNEN				*/
/*									*/
/************************************************************************/
/*									*/
/* 	void paint_edgetype_on_pr( type, pr, w, h, color)		*/
/*									*/
/* Zeichnet den Kantentyp type auf das pixrect pr als horizontale Linie	*/
/* auf Hoehe h/2 und ueber die volle Breite w (des pixrects).		*/
/* Zusaetzlich wird in der unteren linken Ecke des pixrects der Name	*/
/* des Kantentyps ausgegeben.						*/
/* Diese Routine wird bisher nur benutzt, um die pixrects der Kanten-	*/
/* typen fuer die Darstellung in der Menueauswahl zu erzeugen.		*/
/************************************************************************/

void paint_edgetype_on_pr( type, pr, w, h, color)
Edgetype 	type;
Pixrect	*pr;
int		w,h;
int		color;
{
/* Added MH conversion */
	extern	Pixfont		*pf_default();

	static struct pr_brush  brstruct = { 1 },
				*brush = &brstruct;
	Pixfont 		*pf;
	int			lauf = 0 ,last_slash = -1;

	h-=2; w-=2;
				
/* An dieser Stelle muesste pr_line(...) aufgerufen werden		*/
/* (Beschreibung im PIXRECT REFERENCE MANUAL).				*/
/* Da aber diese Routine zur Zeit (8.8.89) noch fehlerhaft zu sein 	*/
/* scheint, wird hier eine selbstgeschriebene, voll kompatible Routine	*/
/* verwendet								*/

	PR_line( 	pr, 0, h/2, w, h/2, 
			(struct pr_brush *)brush, 
			type->texture,
			PIX_SRC | PIX_COLOR (color)
		);
	while( type->filename[lauf] != 0 ) {
		if (type->filename[lauf] == '/') {
			last_slash = lauf;
		}
		lauf ++;
	}
	last_slash ++;	
	pf = pf_default();
/* Commented MH Conversion
	pr_text( pr, 0 , h-3 , PIX_SRC | PIX_COLOR (color), pf , &type->filename[last_slash] );
*/
}


/************************************************************************/
/*									*/
/*		SPEZIELLE (SYSTEM -) KNOTENTYPEN ZEICHNEN.		*/
/*									*/
/************************************************************************/
/*									*/
/*	Hier befinden sich die Prozeduren zum zeichnen von		*/
/*	vordefinierten Knotentypen (d.h. type->system == TRUE).		*/
/*	Bei den entsprechenden Knotentypen werden sie (bzw. Zeiger	*/
/*	darauf) unter type->pr_paint_func und type->pw_paint_func,	*/
/*	je nachdem auf Pixwin oder Pixrect gezeichnet wird, gefuehrt.	*/
/*	(die sich anbietende Parameterisierung ist problematisch, da	*/
/*	die Pixwin- bzw Pixrectzeichenprozeduren keine sind, sondern	*/
/*	Makros).							*/
/*									*/
/*	Die Parameterliste fuer solche Prozeduren muss folgenden	*/
/*	Aufbau haben :							*/
/*									*/
/*	Pixrect	*pr;	Pixrect, auf das gezeichnet wird bzw.	*/
/*	struct	pixwin	*pw;	Pixwin, auf das gezeichnet wird		*/
/*	int	x,y, w,h;	Linke obere Ecke, Breite, Hoehe		*/
/*	int	op;		Pixrect-Zeichenoperation		*/
/*									*/
/*	Die pr_... Prozeduren werden nicht zum Neuzeichnen, sondern	*/
/*	nur beim Anlegen des Bildes (node->image) verwendet.		*/
/*									*/
/*	Weitere Einzelheiten siehe auch use_nodetypeimage im Modul	*/
/*	type.c.								*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	pr_paint_box_node(pr, x,y, w,h, op)			*/
/*									*/
/*	Zeichenprozedur fuer Knoten "#box" (-> system_nodetypes,	*/
/*	type.c).							*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	pr_paint_elliptical_node (pr, x0,y0, a,b, op)		*/
/*									*/
/*	Zeichenprozedur fuer Knoten "#circle" (-> system_nodetypes,	*/
/*	type.c).							*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	pr_paint_diamond_node (pr, x0,y0, a,b, op)		*/
/*									*/
/*	Zeichenprozedur fuer Knoten "#diamond" (-> system_nodetypes,	*/
/*	type.c).							*/
/*									*/
/************************************************************************/



void		pr_paint_box_node(pr, x,y, w,h, op)
Pixrect	*pr;
int		x,y, w,h;
int		op;
{
	register int	x1,y1, x2,y2;
	
	x1 = x;
	y1 = y;
	x2 = x + w-1;
	y2 = y + h-1;
	
	pr_vector (pr, x1,y1, x1,y2, op, PIX_OPCOLOR(op));
	pr_vector (pr, x1,y2, x2,y2, op, PIX_OPCOLOR(op));
	pr_vector (pr, x2,y2, x2,y1, op, PIX_OPCOLOR(op));
	pr_vector (pr, x2,y1, x1,y1, op, PIX_OPCOLOR(op));
}


void		pr_paint_elliptical_node(pr, x0,y0, w,h, op)
Pixrect	*pr;
int		x0,y0, w,h;
int		op;
{
	register int	a, b, d, xm,
			xm_plus_x, xm_minus_x,
			ym_plus_y, ym_minus_y,
			a_quadrat, b_quadrat,
			zwei_a_quadrat, zwei_b_quadrat,
			delta_x, delta_y;
	register int	y;
	
	register int	pr_put_value =
			iif (graphed_state.colorscreen == TRUE,
			     PIX_OPCOLOR(op),
			     iif (PIX_OPCOLOR(op) == 0, 0, 1));

	w-- ; h--;
	a = w / 2 ;  b = h / 2 ;
	if ( b==0 ) {
		pr_vector(pr, x0, y0,   x0+w, y0,   op, PIX_OPCOLOR(op));
		pr_vector(pr, x0, y0+h, x0+w, y0+h, op, PIX_OPCOLOR(op));
	} else {
		xm_minus_x = x0 + a ;
		xm_plus_x = xm_minus_x + (w & 1) ;
		ym_plus_y = y0 + h ; ym_minus_y = y0 ;

		a_quadrat = a*a;
		b_quadrat = b*b;
		zwei_a_quadrat = 2 * a_quadrat ;
		zwei_b_quadrat = 2 * b_quadrat ;
		delta_y = a_quadrat * ( 2*b - 1 );
		delta_x = b_quadrat ;

		d = delta_y - 1;  /* - 2*a*b */

		y = b;

		do {
			pr_put (pr, xm_plus_x  , ym_plus_y  , pr_put_value);
			pr_put (pr, xm_plus_x  , ym_minus_y , pr_put_value);
			pr_put (pr, xm_minus_x , ym_plus_y  , pr_put_value);
			pr_put (pr, xm_minus_x , ym_minus_y , pr_put_value);
		
			if (d >= 0) {
				d -= delta_x;
				delta_x += zwei_b_quadrat ;
				xm_plus_x++;
				xm_minus_x--;
			}
			if (d < 0) {
				delta_y -= zwei_a_quadrat ;
				d += delta_y ;
				y--;
				ym_plus_y--;
				ym_minus_y++;
			}
		} while (y >= 0);
	}
}


void		pr_paint_diamond_node(pr, x,y, w,h, op)
Pixrect	*pr;
int		x,y;
int		w,h;
int		op;
{
	int x1,y1, x2,y2, x3,y3, x4,y4;
	
	x1 = x;       y1 = y+h/2-1;
	x2 = x+w/2-1; y2 = y;
	x3 = x+w-1;   y3 = y+h/2;
	x4 = x+w/2;   y4 = y+h-1;

	pr_vector (pr, x1,y1, x2,y2, op, PIX_OPCOLOR(op));
	pr_vector (pr, x2,y2, x3,y3, op, PIX_OPCOLOR(op));
	pr_vector (pr, x3,y3, x4,y4, op, PIX_OPCOLOR(op));
	pr_vector (pr, x4,y4, x1,y1, op, PIX_OPCOLOR(op));
}
/************************************************************************/
/*									*/
/*			KANTEN ZEICHNEN					*/
/*									*/
/************************************************************************/
/*									*/
/*	void	paint_single_edgeline_internal (el, op)			*/
/*	void	paint_line_internal            (x0,y0, x1,y1, op)	*/
/*									*/
/*	Zeichnet das Kantenstueck (el) - (el->suc) bzw. die Linie	*/
/*	(x0,y0) - (x1,y1) auf paint_pr.					*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	paint_edgelabel_internal       (edge, op)		*/
/*									*/
/*	Zeichne *(edge->label.text_to_draw) (wenn nicht NULL) in	*/
/*	paint_pr.							*/
/*									*/
/************************************************************************/


void		paint_single_edgeline_internal (el, type, op)
Edgeline	el;
Edgetype	type;
int		op;
{
	if ( type != (Edgetype) NULL ) {

/* An dieser Stelle muesste pr_line(...) aufgerufen werden		*/
/* (Beschreibung im PIXRECT REFERENCE MANUAL).				*/
/* Da aber diese Routine zur Zeit (8.8.89) noch fehlerhaft zu sein 	*/
/* scheint, wird hier eine selbstgeschriebene, voll kompatible Routine	*/
/* verwendet								*/

		PR_line(	paint_pr,
				(int)(el->x      - offset_x),
		           	(int)(el->y      - offset_y),
		           	(int)(el->suc->x - offset_x),
		           	(int)(el->suc->y - offset_y),
		           	(struct pr_brush *)NULL,
		           	(struct pr_texture *)(type->texture),
		           	op
		        );
	} else {
		error( "paint_single_edgeline_internal :\n Edgetype = NULL !!!\n");
	}
}


void	paint_line_internal (x0,y0, x1,y1, op)
int	x0,y0, x1,y1;
int	op;
{
	pr_vector (paint_pr,
	           x0 - offset_x, y0 - offset_y,
	           x1 - offset_x, y1 - offset_y,
                   op, PIX_OPCOLOR(op));
}


void	paint_edgelabel_internal (edge, op)
Edge	edge;
int	op;
{			
	if (edge != empty_edge && edge->label.text_to_draw != NULL)
		pr_text (paint_pr,
		         edge->label.x - offset_x,
		         edge->label.y - offset_y,
		         op,
		         edge->label.font->font,
		         *(edge->label.text_to_draw));
}
/************************************************************************/
/*									*/
/*			CLIP_REGION SETZEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	Rect	*set_clip_region (clip_rect)				*/
/*									*/
/*	Erzeugt paint_pr, auf das von den paint_..._internal -		*/
/*	Funktionen gezeichnet wird (s.o.). Die Funktion liefert als	*/
/*	Resultat den ab den auf dem Bildschirm sichtbaren Teil von	*/
/*	clip_rect (wenn der also leer ist, braucht man beim		*/
/*	Neuzeichnen nichts zu tun). Achtung : das Resultat ist ein	*/
/*	Zeiger auf eine Funktionslokale static - Variable !		*/
/*									*/
/************************************************************************/


Rect	*set_clip_region (clip_rect, buffer, clip_at_window_boundary, color)
Rect	*clip_rect;
int	buffer;
int	clip_at_window_boundary;
int	color;
{
	Rect		rect_to_draw_in;
	Rect		clipped_rect_to_draw_in;
	int		scroll_offset_x = 0;
	int		scroll_offset_y = 0;
	static Rect	clipped_rect;
	Rect		canvas_rect;
	
	/* Hole die Groesse des canvas auf dem Bildschirm	*/
	if (clip_at_window_boundary) {
/* Changed MH Conversion
		pw_get_region_rect (canvases[buffer].pixwin,
		                    &canvas_rect);
*/
		canvas_rect = *((Rect *)xv_get(canvases[buffer].canvas,
			CANVAS_VIEWABLE_RECT,
			canvases[buffer].pixwin));
	} else {
		rect_construct (&canvas_rect, 0,0,
			(int)xv_get(canvases[buffer].canvas, CANVAS_WIDTH),
			(int)xv_get(canvases[buffer].canvas, CANVAS_HEIGHT));
	}
	
	/* Wenn in paint_pr noch etwas uebrig war, loesche es	*/
	if (paint_pr != (Pixrect *)NULL)
		pr_destroy (paint_pr);
	
	/* Hole den Scroll-Offset	*/
	if (clip_at_window_boundary) {
		get_scroll_offset (buffer, &scroll_offset_x, &scroll_offset_y);
	} else {
		scroll_offset_x = 0;
		scroll_offset_y = 0;
	}
	
	rect_to_draw_in = *clip_rect;
/* MH Conversion
   It seems that scroll_offset_[x|y] is no longer needed
	rect_left (&rect_to_draw_in) -= scroll_offset_x;
	rect_top  (&rect_to_draw_in) -= scroll_offset_y;
*/
	rect_intersection (&canvas_rect,
	                   &rect_to_draw_in,
	                   &clipped_rect_to_draw_in);
	
	/* Erzeuge paint_pr	*/
	paint_pr = mem_create (rect_width  (&clipped_rect_to_draw_in),
	                       rect_height (&clipped_rect_to_draw_in),
/* MH changed conversion */
/* UMARBEITUNG noetig */
/*
	                       color ? canvases[buffer].pixwin->pw_pixrect->pr_depth : 1);
*/
				1);

	/* Berechne den Offset fuer die Koordinaten, da diese	*/
	/* im System des clip_rect liegen muessen		*/
/* MH Conversion
	offset_x = scroll_offset_x + rect_left (&clipped_rect_to_draw_in);
	offset_y = scroll_offset_y + rect_top  (&clipped_rect_to_draw_in);
*/
	offset_x = rect_left (&clipped_rect_to_draw_in);
	offset_y = rect_top  (&clipped_rect_to_draw_in);
	 
	clipped_rect = clipped_rect_to_draw_in;
/* MH Conversion
	rect_left (&clipped_rect) += scroll_offset_x;
	rect_top  (&clipped_rect) += scroll_offset_y;
*/
	
	return &clipped_rect;
	/* ACHTUNG : Rueckgabe Zeiger auf statische Variable !	*/
}
/************************************************************************/
/*									*/
/*		ZUSAETZLICHE ZEICHENPROZEDUREN				*/
/*									*/
/************************************************************************/
/*									*/
/*	void	paint_dot_internal (x,y)				*/
/*									*/
/*	Gibt einen Punkt (Groesse DOTSIZE, ->paint.h) an der Stelle	*/
/*	(x,y) aus. Diese Prozedur wird verwendet, um die Gitterpunkte	*/
/*	(-> show_grid in draw.c) zu zeichnen.				*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	paint_rectangle (pw, x1,y1, x2,y2, op)			*/
/*									*/
/*	Zeichnet ein Rechteck mit linker oberer Ecke (x1,y1) und	*/
/*	rechter unterer Ecke (x2,y2).					*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	paint_line (pw, x1,y1, x2,y2, op)			*/
/*									*/
/*	Zeichnet eine Linie (x1,y1) - (x2,y2).				*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	paint_marker_square    (pw, x,y)			*/
/*	void	paint_marker_rect      (pw, x,y)			*/
/*	void	paint_marker_rectangle (pw, x,y)			*/
/*									*/
/*	Zeichnen Markierungen fuer do_mark_node bzw. do_unmark_node.	*/
/*	Beide haben die Groesse MARKER_SQUARE_SIZE			*/
/*	(-> paint.h); bei "square" handelt sich es um ein ausgefuelltes	*/
/*	Rechteck.							*/
/*	Da in beiden Faellen mit XOR gezeichnet wird, koennen Zeichnen	*/
/*	und Loeschen mit der selben Prozedur erfolgen.			*/
/*									*/
/************************************************************************/



void	paint_dot_internal (x,y)
int	x,y;
{
	pr_rop (paint_pr,
		x - DOTSIZE/2 - offset_x, y - DOTSIZE/2 - offset_y,
		DOTSIZE, DOTSIZE,
		PIX_NOT(PIX_SRC),
		(Pixrect *)NULL, 0,0);
}



void	paint_rectangle (pw, x1,y1, x2,y2, op)
Pixwin	*pw;
int	x1,y1;
int	x2,y2;
int	op;
{
	pw_vector (pw, x1,y1, x1,y2, op, PIX_OPCOLOR(op));
	pw_vector (pw, x1,y2, x2,y2, op, PIX_OPCOLOR(op));
	pw_vector (pw, x2,y2, x2,y1, op, PIX_OPCOLOR(op));
	pw_vector (pw, x2,y1, x1,y1, op, PIX_OPCOLOR(op));
}


void	paint_line (pw, x1,y1, x2,y2, op)
Pixwin	*pw;
int	x1,y1;
int	x2,y2;
int	op;
{
	pw_vector (pw, x1,y1, x2,y2, op, PIX_OPCOLOR(op));
}


void	paint_marker_square (pw, x,y)
Pixwin	*pw;
int	x,y;
{
	pw_rop (pw,
	        x - MARKER_SQUARE_SIZE/2,
	        y - MARKER_SQUARE_SIZE/2,
	        MARKER_SQUARE_SIZE,
	        MARKER_SQUARE_SIZE,
	        (PIX_SRC ^ PIX_DST) | PIX_COLOR(BLACK),
	        (Pixrect *)NULL, 0, 0);
}


void	paint_marker_rect (pw, x,y)
Pixwin	*pw;
int	x,y;
{
	int	x1,y1, x2,y2, op;
	
	x1 = x - MARKER_SQUARE_SIZE/2;
	y1 = y - MARKER_SQUARE_SIZE/2;
	x2 = x1 + MARKER_SQUARE_SIZE;
	y2 = y1 + MARKER_SQUARE_SIZE;
	op = (PIX_SRC ^ PIX_DST) | PIX_COLOR(BLACK);
	
	paint_rectangle (pw, x1,y1, x2,y2, op);
}


void	paint_marker_rectangle (pw, x1,y1, x2,y2)
Pixwin	*pw;
int	x1,y1, x2,y2;
{
	int	op;
	
	x1 = x1 - MARKER_SQUARE_SIZE/2;
	y1 = y1 - MARKER_SQUARE_SIZE/2;
	x2 = x2 - MARKER_SQUARE_SIZE/2;
	y2 = y2 - MARKER_SQUARE_SIZE/2;
	
#define	paint_block(x,y,w,h) 				\
	pw_rop (pw,					\
		(x), (y), (w), (h),			\
	        (PIX_SRC ^ PIX_DST) | PIX_COLOR(BLACK),	\
	        (Pixrect *)NULL, 0, 0);

	paint_block (x1,y1, MARKER_SQUARE_SIZE,y2-y1);
	paint_block (x1+MARKER_SQUARE_SIZE,y1, x2-x1,MARKER_SQUARE_SIZE);
	paint_block (x1,y2, x2-x1,MARKER_SQUARE_SIZE);
	paint_block (x2,y1+MARKER_SQUARE_SIZE, MARKER_SQUARE_SIZE,y2-y1);
	
#undef paint_block
}



void	paint_background (rect)
Rect	*rect;
{
	extern	Pixrect	*background_pixrect;
	
	if (background_pixrect != (Pixrect *)NULL) {
		pr_replrop (paint_pr,
			rect_left (rect)- offset_x,
			rect_top (rect) - offset_y,
			rect_width (rect),
			rect_height (rect),
			PIX_SRC | PIX_COLOR(8), /* Gray */
			background_pixrect,
/*
			rect_left (rect) % background_pixrect->pr_width,
			rect_top (rect)  % background_pixrect->pr_height);
*/
			rect_left (rect),
			rect_top (rect));
	}

}
