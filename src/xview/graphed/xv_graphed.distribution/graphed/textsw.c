/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
/************************************************************************/
/*									*/
/*				textsw.c				*/
/*									*/
/************************************************************************/
/*									*/
/*	Verwaltung des Textfensters zur Ausgabe von Meldungen.		*/
/*	Ausgaben auf dieses Fenster laufen i.a. ueber die Prozeduren	*/
/*	in error.c.							*/
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
/*	void	create_message_textsw ()				*/
/*	void	write_message         (message)				*/
/*									*/
/************************************************************************/


/************************************************************************/
/*									*/
/*			GLOBALE VARIABLEN				*/
/*									*/
/************************************************************************/

Textsw	message_textsw = (Textsw)NULL;


/************************************************************************/
/*									*/
/*			MESSAGE_TEXTSW AUFBAUEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	void	create_message_textsw ()				*/
/*									*/
/*	"Erzeugt" das message_textsw. "Load" und "Set directory" werden	*/
/*	im Menue des Textfensters verboten, ebenso wird readonly	*/
/*	gesetzt.							*/
/*									*/
/************************************************************************/

		
void	create_message_textsw()
{
	xv_set(message_textsw,
		TEXTSW_DISABLE_LOAD,		TRUE,
		TEXTSW_DISABLE_CD,		TRUE,
		TEXTSW_INSERTION_POINT,		TEXTSW_INFINITY,
		TEXTSW_READ_ONLY,		TRUE,
		TEXTSW_INSERT_MAKES_VISIBLE,	TRUE,
		TEXTSW_IGNORE_LIMIT,		TEXTSW_INFINITY,
		TEXTSW_CONTENTS,		"*****     Messages     *****\n\n",
		NULL);
}
/************************************************************************/
/*									*/
/*		MELDUNG AUF MESSAGE_TEXTSW AUSGEBEN			*/
/*									*/
/************************************************************************/
/*									*/
/*	void	write_message (message)					*/
/*									*/
/*	Ausgabe von message in message_textsw oder, wenn dieses (noch)	*/
/*	nicht, existiert, auf stderr.					*/
/*	BUG : um auf das message_textsw schreiben zu koennen, muss hier	*/
/*	kurzzeitig der Readonly-Status aufgehoben werden.		*/
/*									*/
/************************************************************************/


void	write_message (message)
char	*message;
{
	if (message_textsw != (Textsw)NULL)
		xv_set(message_textsw,
			TEXTSW_INSERTION_POINT,	TEXTSW_INFINITY,
			TEXTSW_READ_ONLY,	FALSE,
			/* sonst kann auch das Programm nicht	*/
			/* schreiben ! (?)			*/
			NULL);
	textsw_possibly_normalize (message_textsw,
		xv_get(message_textsw, TEXTSW_INSERTION_POINT));

	if (message_textsw != (Textsw)NULL)
		textsw_insert (message_textsw, message, strlen(message));
	else
		fprintf (stderr, "%s", message);
	
	if (message_textsw != (Textsw)NULL) {
		xv_set(message_textsw, TEXTSW_READ_ONLY, TRUE, NULL);
	}
}
