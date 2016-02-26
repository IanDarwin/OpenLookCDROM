/* (C) Universitaet Passau 1986-1991 */
/************************************************************************
 * File    : gfunc.h							*
 * Aufgabe : Allgemeine Graf-Funktionen					*
 *									*
 * Autor   : Torsten Bachmann						*
 * Datum   : 11.12.89							*
 ************************************************************************/

#define G_Test_Stack_Overflow(ptr) \
				if ((ptr)==NULL) \
				{	fprintf(stderr,"Speicher voll"); \
					exit(3); \
				}

int	G_knoten_anzahl ();
int	G_kanten_anzahl ();
int	G_exist_edge ();
Sedge	G_has_double_edges();
