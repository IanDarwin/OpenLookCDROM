/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Uwe Schnieders, Michael Himsolt		*/
/************************************************************************/
/*									*/
/*				la_paint.c				*/
/*									*/
/************************************************************************/
/*									*/
/*	la_paint.c druckt Graphen auf einen Laserdrucker im		*/
/*	Postscript - Format						*/
/*									*/
/************************************************************************/
/*  Alle Postscript-Ausgaben werden zur Zeit in eine Datei              */
/*  mit Namen 'graphed.ps'  geschrieben . Diese Datei muss dann vom     */
/*  Benutzer auf einen Laserdrucker ausgedruckt werden .                */
/*  Im Verzeichnis muessen die Dateien 'graphed.header.ps',		*/
/*  'graphed.tail.ps' existieren .					*/
/*  Diese werden in die Datei 'graphed.ps' hinein kopiert.		*/
/************************************************************************/

#include "misc.h"
#include "graph.h"
#include "draw.h"
#include "type.h"

#include "graphed_subwindows.h"

#include "modula.h"

#include "print.h"
#include "la_paint.h"
#include "postscri.h"


static	char	nodename[] = "NODE   ";

static  short	default_line[] = { 0 } ;

/*****************************************************************/
/*								 */
/*  FUNCTION : setze_rechteck_auf_laserseite(rect)    		 */
/*       							 */
/*   INPUT   : Ein Rechteck    					 */
/*       							 */
/*   OUTPUT  : die modulglobalen Variablen 			 */
/*             x_trans,y_trans,x_scale,y_scale  		 */
/*       	werden so gesetzt,dass das Rechteck ohne	 */
/*		Verzerrung auf die Laserseite passt		 */
/*****************************************************************/

void		setze_rechteck_auf_laserseite(rect)
Rect		*rect;
BEGIN
 	post_def("r_left",  ((rect)->r_left)  );
	post_def("r_top",   ((rect)->r_top)  );
	post_def("r_width", ((rect)->r_width) );
	post_def("r_height",((rect)->r_height) );

END

/*****************************************************************/
/*								 */
/*  FUNCTION :      						 */
/*       							 */
/*   INPUT   :    						 */
/*       							 */
/*   OUTPUT  :   						 */
/*       							 */
/*****************************************************************/

void	laser_paint_splitt_rect(rect,X,Y)
Rect		*rect;
INTEGER		*X,*Y;
BEGIN
	Rect	working_area_canvas_rect;

	pw_get_region_rect (working_area_canvas_pixwin,&working_area_canvas_rect);	
	(*X) = rect_width(rect)  DIV rect_width(&working_area_canvas_rect)  + 1 ;
	(*Y) = rect_height(rect) DIV rect_height(&working_area_canvas_rect) + 1 ;
END


/*****************************************************************/
/*								 */
/*  FUNCTION :      						 */
/*       							 */
/*   INPUT   :    						 */
/*       							 */
/*   OUTPUT  :   						 */
/*       							 */
/*****************************************************************/

int		laser_paint_rect (modus,rect,fname)
Druckmodus	modus;
Rect		*rect;
char		fname[];

BEGIN
	INTEGER Xanzahl,Yanzahl;

	if (post_help_open_output(fname) == FALSE)
		return FALSE;

	laser_paint_nodetypes();

	switch(modus) 
          BEGIN
            case gesamt : Xanzahl = 1;
			  Yanzahl = 1;
			  laser_paint_ausschnitte(Xanzahl,Yanzahl,rect);
			  break;
	    case  autoteilen :
			   laser_paint_splitt_rect(rect,&Xanzahl,&Yanzahl);
			   laser_paint_ausschnitte(Xanzahl,Yanzahl,rect);
			   break;
	  END;       

	return post_help_close_output();
END


/*****************************************************************/
/*								 */
/*  FUNCTION :      						 */
/*       							 */
/*   INPUT   :    						 */
/*       							 */
/*   OUTPUT  :   						 */
/*       							 */
/*****************************************************************/

void	laser_paint_ausschnitte(Xanzahl,Yanzahl,rect)
INTEGER		Xanzahl,Yanzahl;
Rect		*rect;
BEGIN
	INTEGER i,j;
	Rect	hilf_rect;

	FOR i=1;i<=Xanzahl;i++ DO
	  FOR j=1;j<=Yanzahl;j++ DO
	    rect_left (&hilf_rect)   = rect_left(rect)+(rect_width(rect)* 
					  	      (i-1) DIV Xanzahl );
	    rect_top  (&hilf_rect)    = rect_top(rect) +(rect_height(rect)*
						      (j-1) DIV Yanzahl );
	    rect_width  (&hilf_rect)  = rect_width(rect)  DIV Xanzahl;
	    rect_height (&hilf_rect) = rect_height(rect) DIV Yanzahl;
	    laser_paint_graph_in_rect (&hilf_rect);
	    post_showpage();
	    
	  END;
	END; 
END


/*****************************************************************/
/*								 */
/*  FUNCTION :      						 */
/*       							 */
/*   INPUT   :    						 */
/*       							 */
/*   OUTPUT  :   						 */
/*       							 */
/*****************************************************************/

void	laser_paint_graph_in_rect (rect)
Rect		*rect;
{
	register Graph		g;
	register Node		n;
	register Edge		e;
	register Edgeline	el;
	
	/* Funktionsaufrufe zu Makros ... damit es schneller geht	*/
	
#define	graph_intersects_rect(graph, rect)          \
	rect_intersectsrect (&(graph->box), rect)
#define	node_intersects_rect(node, rect)          \
	rect_intersectsrect (&(node->box), rect)
#define	edge_intersects_rect(edge, rect)          \
	rect_intersectsrect (&(edge->box), rect)
#define	single_edgeline_intersects_rect(el, rect) \
	rect_intersectsrect (&(el->box), rect)
#define	edgelabel_intersects_rect(edge, rect)     \
	rect_intersectsrect (&(edge->label.box), rect)
#define	arrow_intersects_rect(arrow, rect)        \
	rect_intersectsrect (&(arrow.box), rect)

	if (rect_isnull(rect))
		return;		/* nicht sichtbar	*/


	setze_rechteck_auf_laserseite(rect);
	
	post_call_proc("graphedA4");

	post_setlinewidth(1);
	
	for_all_graphs (wac_buffer, g) if (graph_intersects_rect(g, rect))
	    for_nodes (g, n)
		if (node_intersects_rect (n, rect))
		    laser_paint_node (n);
	    end_for_nodes (g, n);
	end_for_all_graphs (wac_buffer, g);
	
	for_all_graphs (wac_buffer, g) if (graph_intersects_rect(g, rect))
	    for_nodes (g, n)
		for_edge_sourcelist (n, e)
		    
		    if (edge_intersects_rect (e, rect)) {
			laser_set_edgetype(e);
			if ((el = e->line) != (Edgeline)NULL) do {
			    if (single_edgeline_intersects_rect (el, rect))
				laser_paint_single_edgeline(el);
				el = el->suc;
			} while (el->suc != e->line);
			if (e->source->graph->directed && e->arrow.length > 0 &&
			    arrow_intersects_rect (e->arrow, rect)) {
			    post_setdash(default_line);
			    laser_paint_line(e->arrow.x0, e->arrow.y0,
				e->arrow.x1, e->arrow.y1);
			    laser_paint_line(e->arrow.x1, e->arrow.y1,
				e->arrow.x2, e->arrow.y2);
			}
			if (e->label.visible &&
			    edgelabel_intersects_rect (e, rect))
				laser_paint_edgelabel(e);
		    }
			
		end_for_edge_sourcelist (n, e);
	    end_for_nodes (g, n);
	end_for_all_graphs (wac_buffer, g);


}


/*****************************************************************/
/*								 */
/*  FUNCTION :      						 */
/*       							 */
/*   INPUT   :    						 */
/*       							 */
/*   OUTPUT  :   						 */
/*       							 */
/*****************************************************************/

void	laser_paint_node(node)
Node	node;
BEGIN
	IF node->type->is_system THEN
		node->type->laser_paint_func (
			(int)(node_left(node) ),
			(int)(node_top(node) ),
			(int) node_width (node),
			(int) node_height (node));
	ELSE 
		laser_paint_node_image(
			(int)(node_left(node) ),
		        (int)(node_top(node) ),
			node);
			
	END;
	
	 IF node->label.visible && node->label.text_to_draw != NULL THEN
		laser_paint_text(
		         node->label.x ,
		         node->label.y ,
		         node->label.box,
		         *(node->label.text_to_draw),
			 node->label.font);
	END;
END


/*****************************************************************/
/*								 */
/*  FUNCTION :      						 */
/*       							 */
/*   INPUT   :    						 */
/*       							 */
/*   OUTPUT  :   						 */
/*       							 */
/*****************************************************************/

char	*make_nodename(nr)
INTEGER		nr;
BEGIN
        nodename[4] = 48+(nr DIV 100);
        nodename[5] = 48+((nr MOD 100) DIV 10);
        nodename[6] = 48+(nr MOD 10);
	RETURN(nodename);
END

/*****************************************************************/
/*								 */
/*  FUNCTION :      						 */
/*       							 */
/*   INPUT   :    						 */
/*       							 */
/*   OUTPUT  :   						 */
/*       							 */
/*****************************************************************/

void	laser_paint_nodetypes()
BEGIN
  INTEGER	i;
  Nodetype	help;

  FOR i=1;i<=MAX_NODETYPE;i++ DO
    help = get_nodetype(i);
    IF help != NIL THEN
      IF (help->used > 0) AND ( NOT (help->is_system) ) THEN
        post_def_string(make_nodename(i),
			post_help_compute_string_length_from_pixrect(
					help->pr->pr_width,
					help->pr->pr_height)
			);
        post_read_image(make_nodename(i),
			help->pr->pr_width,
			help->pr->pr_height,
			help->pr);
      END;
    END;
  END;
END

/*****************************************************************/
/*								 */
/*  FUNCTION :      						 */
/*       							 */
/*   INPUT   :    						 */
/*       							 */
/*   OUTPUT  :   						 */
/*       							 */
/*****************************************************************/
void		laser_set_edgetype(edge)
Edge	edge;
BEGIN
	IF edge->type->texture THEN
	  IF edge->type->texture->pattern THEN
	    post_setdash(edge->type->texture->pattern);
	   ELSE 
	    post_setdash(default_line);
	  END;
	 ELSE
	  post_setdash(default_line);
	END;
END


/*****************************************************************/
/*								 */
/*  FUNCTION :      						 */
/*       							 */
/*   INPUT   :    						 */
/*       							 */
/*   OUTPUT  :   						 */
/*       							 */
/*****************************************************************/

void		laser_paint_single_edgeline(el)
Edgeline	el;
BEGIN 


	laser_paint_vector (
	           (int)(el->x),
	           (int)(el->y),
	           (int)(el->suc->x),
	           (int)(el->suc->y));
END


/*****************************************************************/
/*								 */
/*  FUNCTION :      						 */
/*       							 */
/*   INPUT   :    						 */
/*       							 */
/*   OUTPUT  :   						 */
/*       							 */
/*****************************************************************/

void	laser_paint_line(x0,y0, x1,y1)
int	x0,y0, x1,y1;
BEGIN
	laser_paint_vector (
	           x0, y0,
	           x1, y1);
END


/*****************************************************************/
/*								 */
/*  FUNCTION :      						 */
/*       							 */
/*   INPUT   :    						 */
/*       							 */
/*   OUTPUT  :   						 */
/*       							 */
/*****************************************************************/

void	laser_paint_edgelabel(edge)
Edge	edge;
BEGIN			
	IF edge != empty_edge && edge->label.text_to_draw != NULL THEN
		laser_paint_text(
		         edge->label.x,
		         edge->label.y,
		         edge->label.box,
		         *(edge->label.text_to_draw),
			 edge->label.font);
	END;
END


/*****************************************************************/
/*								 */
/*  FUNCTION :      						 */
/*       							 */
/*   INPUT   :    						 */
/*       							 */
/*   OUTPUT  :   						 */
/*       							 */
/*****************************************************************/

void	laser_paint_dot(x,y)
int	x,y;
BEGIN 
	post_dot(x - DOTSIZE/2, y - DOTSIZE/2,DOTSIZE);
	post_stroke();
END

/*****************************************************************/
/*								 */
/*  FUNCTION :      						 */
/*       							 */
/*   INPUT   :    						 */
/*       							 */
/*   OUTPUT  :   						 */
/*       							 */
/*****************************************************************/

void	laser_paint_box(x,y,width,heigh)
int	x,y;
int     width,heigh;
BEGIN 
/*	message("#box  /: %d %d %d %d \n",x,y,width,heigh); */
        post_box(x,y,width,heigh);
END


/*****************************************************************/
/*								 */
/*  FUNCTION :      						 */
/*       							 */
/*   INPUT   :    						 */
/*       							 */
/*   OUTPUT  :   						 */
/*       							 */
/*****************************************************************/

void	laser_paint_circle(x,y,width,heigh)
int	x,y;
int     width,heigh;
BEGIN 
/*	message("#circle  /: %d %d %d %d \n",x,y,width,heigh); */
	post_ellipse(x+(width / 2),y+(heigh / 2),width,heigh );
END


/*****************************************************************/
/*								 */
/*  FUNCTION :      						 */
/*       							 */
/*   INPUT   :    						 */
/*       							 */
/*   OUTPUT  :   						 */
/*       							 */
/*****************************************************************/

void	laser_paint_diamond(x,y,width,heigh)
int	x,y;
int     width,heigh;
BEGIN 
/*	message("#diamond : %d %d %d %d \n",x,y,width,heigh); */
	post_moveto(x+(width / 2),y);
	post_lineto(x+ width     ,y+(heigh / 2));
	post_lineto(x+(width / 2),y+ heigh );
	post_lineto(x            ,y+(heigh / 2));
	post_lineto(x+(width / 2),y);
	post_closepath();
	post_stroke();
END


/*****************************************************************/
/*								 */
/*  FUNCTION :      						 */
/*       							 */
/*   INPUT   :    						 */
/*       							 */
/*   OUTPUT  :   						 */
/*       							 */
/*****************************************************************/

void	laser_paint_node_image(x,y,node)
int	x,y;
Node	node;
BEGIN 
	INTEGER	heigh,width,source_width,source_heigh;

	source_width = (int)(node->type->pr->pr_width);
	source_heigh = (int)(node->type->pr->pr_height);
	width = (int) node_width (node);
	heigh = (int) node_height(node);

/*	message("#image : %d %d %d %d \n",x,y,width,heigh); */
	post_gsave();
	post_translate((x),(y));
	post_scale(-width,heigh);
	post_rotate(90);
	post_write_image(make_nodename(get_nodetype_index(node->type)),
		   source_width,source_heigh);
	post_grestore();
END


/*****************************************************************/
/*								 */
/*  FUNCTION :      						 */
/*       							 */
/*   INPUT   :    						 */
/*       							 */
/*   OUTPUT  :   						 */
/*       							 */
/*****************************************************************/

void	laser_paint_image(x,y,source_width,source_heigh,width,heigh,pr)
int	x,y;
INTEGER	heigh,width,source_width,source_heigh;
Pixrect	*pr;
BEGIN 
	post_gsave();
	post_translate((x),(y));
	post_scale((width),(heigh));
	post_image(source_width,source_heigh,pr);
	post_grestore();
END


/*****************************************************************/
/*								 */
/*  FUNCTION :      						 */
/*       							 */
/*   INPUT   :    						 */
/*       							 */
/*   OUTPUT  :   						 */
/*       							 */
/*****************************************************************/


void	laser_paint_text(x,y,box,text,font)
int		x,y;
Rect		box;
char		*text;
Graphed_font	font;
BEGIN 
	Pixrect	*pr;
	char		*last_backslash;
	char		*fontname;
	
	/* enhanced by MH 21/8/89	*/
	last_backslash = strrchr (font->filename, '/');
	fontname = iif (last_backslash != NULL, last_backslash+1, font->filename);
	IF !strncmp (fontname, "cour", 4) THEN
/* Changed MH conversion
		post_font("Courier", (INTEGER)(((font->font)->pf_defaultsize).x));
*/
/* BEARBEITUNG notwendig */
		post_font ("Courier", 12);
		post_moveto (x,y);
		post_show (text);
	  ELSE
		pr = mem_create( rect_width(&box), rect_height(&box),1 );
		pr_text (pr,x- rect_left(&box),y-rect_top(&box),PIX_SRC, font->font,text);
		laser_paint_image(rect_left(&box),rect_top(&box),
			rect_width(&box),rect_height(&box),
			rect_width(&box),rect_height(&box),pr);
		myfree (pr);
	END
END





void	laser_paint_vector(x0,y0,x1,y1)
int	x0,y0,x1,y1;
BEGIN 
/*	message("#vector : %d %d %d %d \n",x0,y0,x1,y1); */
	post_vector(x0,y0,x1,y1);
END






