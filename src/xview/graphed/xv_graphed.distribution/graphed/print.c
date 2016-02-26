/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt, Uwe Schnieders		*/
/************************************************************************/
/*									*/
/*			    print.c					*/
/*									*/
/************************************************************************/
/*									*/
/*		Ausgaberoutinen auf Drucker etc.			*/
/*									*/
/*									*/
/*	Die Ausgabe auf einen Laserdrucker benoetigt die Dateien:	*/
/*									*/
/*	print.h , print.c 						*/
/*	postscri.h , postscri.c						*/
/*	la_paint.h , la_paint.c						*/
/*	modula.h							*/
/*									*/
/*	Alle Ausgabe werden nicht sofort an den Drucker gesendet,	*/
/*	sondern in eine Datei ,die durch 'fname' angegeben wird	,	*/
/*	umgeleitet.							*/
/*	Hierzu werden die Dateien :					*/
/*									*/
/*	graphed.header.ps						*/
/*	graphed.tail.ps							*/
/*									*/
/*	benoetigt ,die an den Anfang bzw. an das Ende der Ausgabedatei	*/
/*	kopiert werden .						*/
/*									*/
/************************************************************************/

#include "modula.h"
#include "misc.h"
#include "graph.h"
#include "print.h"
#include "la_paint.h"


void		print_postscript (fname,rect,druck_modus)
char		fname[];
Rect		*rect;
Druckmodus	druck_modus;
{
 	if (laser_paint_rect (druck_modus,rect,fname))
		message("Printed to file %s\n", fname);
}
