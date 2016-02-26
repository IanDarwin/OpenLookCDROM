/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
#undef GRAPHED_DEBUG_MALLOC

/************************************************************************/
/*									*/
/*			    misc.c					*/
/*									*/
/************************************************************************/
/*									*/
/*	In diesem Modul befinden sich diverse Hilfsprozeduren aus	*/
/*	allen Bereichen.						*/
/*									*/
/************************************************************************/


#include "misc.h"
#include <ctype.h>
#include <xview/expandname.h>
#include <sys/types.h>
#include <sys/stat.h>


#define	PATH_DIRECTORY_SEPERATOR ":"

/************************************************************************/
/*									*/
/*			GLOBALE FUNKTIONEN				*/
/*									*/
/************************************************************************/
/*									*/
/*	int	find_min_distance_between_pointclusters	 (		*/
/*			p1_x,p1_y,n1,  p2_x,p2_y,n2,  m1,m2)		*/
/*									*/
/*	void	write_quoted_text (file, text)				*/
/*	char	*remove_escape_characters_from_text (text, length)	*/
/*									*/
/*	char	*remove_control_chars_from_string (s)			*/
/*									*/
/*	void	display_files        (filename)				*/
/*	int	check_file_is_single (filename)				*/
/*	int	file_exists          (filename)				*/
/*	int	file_exists_somewhere(filename)				*/
/*									*/
/*	char	*mymalloc   (size)					*/
/*									*/
/*	float	deg_to_rad (deg)					*/
/*	int	rad_to_deg (deg)					*/
/*	char	*int_to_ascii   (i)					*/
/*	char	*float_to_ascii (f)					*/
/*									*/
/*	void	constrain_8 (orgx, orgy, x, y)				*/
/*	void	scale       (scaling, x,y)				*/
/*									*/
/************************************************************************/
/************************************************************************/
/*									*/
/*				GEOMETRIE				*/
/*									*/
/************************************************************************/
/*									*/
/*	int	find_min_distance_between_pointclusters (		*/
/*			p1_x,p1_y,n1,  p2_x,p2_y,n2,  m1,m2)		*/
/*									*/
/*	(p1_x,p1_y) und (p2_x,p2_y) sind Mengen von n1 bzw. n2 Punkten.	*/
/*	Die Prozedur uebergibt in m1 bzw. m2 die Indices der Punkte aus	*/
/*	den Mengen, die einander am naechsten liegen.			*/
/*									*/
/************************************************************************/	
	
	
int	find_min_distance_between_pointclusters (
		p1_x,p1_y,n1,  p2_x,p2_y,n2,  m1,m2)
int	p1_x[],p1_y[], n1,	/* 1. Punktecluster, n1 Punkte		*/
	p2_x[],p2_y[], n2;	/* 2. Punktecluster, n2 Punkte		*/
int	*m1,  *m2;		/* Indices der Punkte mit minimalem	*/
				/* Abstand				*/
{
	register int	i = 0, j = 0,
			dist, min_dist = MAXINT;
	
	for (i=0; i<n1; i++)
		for (j=0; j<n2; j++) {
			dist = dist_2 (p1_x[i],p1_y[i], p2_x[j],p2_y[j]);
			if (min_dist > dist) {
				min_dist = dist;
				*m1 = i; *m2 = j;
			}
		}
	
	return min_dist;
}
/************************************************************************/
/*									*/
/*		STRING MIT ESCAPE_CHARACTER SCHREIBEN UND LESEN		*/
/*									*/
/************************************************************************/
/*									*/
/*	void	write_quoted_text (file, text)				*/
/*									*/
/*	Schreibt einen Text (mit beliebigen Sonderzeichen !) von '"'	*/
/*	umgeben auf file. Dabei werden im Text folgende (UNIX -		*/
/*	uebliche) Substitutionen durchgefuehrt :			*/
/*									*/
/*		"	--->	\"					*/
/*		\	--->	\\					*/
/*									*/
/*	(fuer ESCAPE_CHARACTER = '\')					*/
/*									*/
/*	Die Datei wird weder geoeffnet noch geschlossen.		*/
/*									*/
/*======================================================================*/
/*									*/
/*	char	*remove_escape_characters_from_text (text, length)	*/
/*									*/
/*	Macht die obige Substituion rueckgaengig.			*/
/*	ACHTUNG : der zurueckgegebene Text wird mit mymalloc angelegt !	*/
/*									*/
/************************************************************************/


void		write_quoted_text (file, text)
register FILE	*file;
register char	*text;
{
	register int	i;
	
	fprintf (file, "\"");
	if (text != NULL) for (i = 0; text[i] != '\0'; i++) {
		switch (text[i]) {
		    case '"' :
			fputc (ESCAPE_CHARACTER, file);
			break;
		    case ESCAPE_CHARACTER :
			fputc (ESCAPE_CHARACTER, file);
			break;
		    default :
			break;
		}
		fputc (text[i], file);
	}
	fprintf (file, "\"");
}



char	*remove_escape_characters_from_text (text, length)
register char	*text;
register int	length;
{
	register	int	i,j;
	register	int	length_without_escapes = 0;
	register	char	*text_without_escapes;
	
	
	/* Berechne length_without_escapes	*/
	for (i = 0; i < length; i++) {
		if (text[i] == ESCAPE_CHARACTER)
			i ++;
		length_without_escapes ++;
	}

	/* Platz machen	*/
	text_without_escapes = (char *)mymalloc (length_without_escapes+1);
	
	/* Forme text in text_without_escapes um :	*/
	/* i = index in text;				*/
	/* j = index in text_without_escapes		*/
	for (i=0,j=0; i<length; i++,j++) {
		if (text[i]  == ESCAPE_CHARACTER)
			i++;
		text_without_escapes[j] = text[i];
	}
	text_without_escapes [length_without_escapes] = '\0';
	
	return	text_without_escapes;
}
/************************************************************************/
/*									*/
/*		KONTROLLSEQUENZEN AUS STRING ENTFERNEN			*/
/*									*/
/************************************************************************/
/*									*/
/*	char	*remove_control_chars_from_string (s)			*/
/*									*/
/*	Alle Tabulatoren in string werden durch Spaces ersetzt, beim	*/
/*	ersten Zeilenumbruch (oder anderem nicht druckbaren Zeichen)	*/
/*	wird abgebrochen.						*/
/*	ACHTUNG : Rueckgabe wird mit mymalloc angelegt !		*/
/*									*/
/************************************************************************/


char	*remove_control_chars_from_string (string)
char	*string;
{
	register int	i, j, k, length;
	char		*smoothed_string;
	
	for (i=length=0; string[i] != '\0'; i++) {
		if (isprint (string[i]) || string[i] == ' ') {
			length += 1;
		} else if (string[i] == '\t') {
			length += (TABSIZE - length % TABSIZE);
		} else /* if (string[i] == '\n') */ {
			break;
		}
	}
	
	smoothed_string = mymalloc (length+1);
	
	for (i=j=0; string[i] != '\0'; i++) {
		if (isprint(string[i]) || string[i] == ' ') {
			smoothed_string[j] = string[i];
			j += 1;
		} else if (string[i] == '\t') {
			for (k = j; k < j + (TABSIZE - j % TABSIZE); k++)
				smoothed_string[k] = ' ';
			j = k;
		} else {
			break;
		}
	}
	smoothed_string[length] = '\0';
	
	return smoothed_string;
}


char	**split_string (string)
char	*string;
{
	int	i, line_count, line_i, length;
	char	**lines;

#define	is_printable(c) (isprint(c) || (c) == ' ' || (c) == '\t')

	if (string == NULL) {
		return (char **)NULL;
	}
	
	length = strlen (string);
	
	line_count = 1; /* There is always one line */
	for (i=0; i < length; i++) {
		if (!is_printable(string[i])) {
			line_count ++;
		}
	}
	
	lines = (char **)mycalloc((unsigned)(line_count+1), sizeof(char *));
	for (i=0; i<line_count+1; i++) {
		lines[i] = NULL;
	}
	
	i = 0;
	line_i = 0;
	while (i < length) {
		lines[line_i] = remove_control_chars_from_string (&(string[i]));
		line_i ++;
		while (i < length && is_printable(string[i])) {
			i++;
		}
		i++;
	}
	
	return lines;
}
/************************************************************************/
/*									*/
/*			DATEIVERWALTUNGSUTILITIES			*/
/*									*/
/************************************************************************/
/*									*/
/*	void	display_files (name)					*/
/*									*/
/*	Gibt die Files, die auf name passen, auf dem message_textsw	*/
/*	aus; name derf ein regulaerer Ausdruck sein.			*/
/*									*/
/*----------------------------------------------------------------------*/
/*									*/
/*	int	check_file_is_single (filename)				*/
/*									*/
/*	Prueft, ob es genau ein File mit diesem Namen gibt (bzw. geben	*/
/*	kann, falls (noch) keines existiert). filename darf auch ein	*/
/*	regulaerer Ausdruck sein.					*/
/*	Existiert kein File dieses Namens bzw. existieren gleich	*/
/*	mehrere, so wird eine Fehlermeldung aus- und FALSE zurueckge-	*/
/*	gegeben.							*/
/*	Ist filename tatsaechlich eindeutig, so wird der expandierte	*/
/*	Name auf filename kopiert; DAZU MUSS FILENAME ALS		*/
/*	FILENAME[FILENAMESIZE] DEKLARIERT WORDEN SEIN.			*/
/*									*/
/*======================================================================*/
/*									*/
/*	int	file_exists (filename)					*/
/*									*/
/*	Gibt an, ob eine Datei filename existiert (keine regulaeren	*/
/*	Ausdruecke !).							*/
/*									*/
/*======================================================================*/
/*									*/
/*	char	*file_exists_somewhere (filename, path)			*/
/*									*/
/*	Stellt fest, ob eine Datei filename existiert :			*/
/*	ist filename eine absolute Pfadangabe (beginnend mit /), so	*/
/*	wird direkt gesucht, sonst in den unter path angegebenen	*/
/*	Directories. path hat die - uebliche - Gestalt			*/
/*	"dir1:dir2:...".						*/
/*	An path werden noch GRAPHED_DEFAULT_FONTS_DIRECTORY,		*/
/*	GRAPHED_DEFAULT_TYPES_DIRECTORY und GRAPHED_LIBRARY_DIRECTORY	*/
/*	angehaengt.							*/
/*	Falls die Suche erfolgreich war, so gibt die Prozedur einen	*/
/*	Zeiger auf eine STATISCHE Variable zurueck, die den Dateinamen	*/
/*	enthaelt. path darf NULL sein.					*/
/*									*/
/************************************************************************/


void	display_files (name)
char	*name;
{
	struct	namelist	*file_list;
	int			i,
				files_found = FALSE;
	char			*index;
	struct	stat		buf;
	
	message ("\nFiles %s : \n", name);
	
	file_list = xv_expand_name(name);
	
	for (i=0; i<file_list->count; i++)
		if (stat (file_list->names[i], &buf) == 0) {
			files_found = TRUE;
			index = strrchr(file_list->names[i], '/');
			message ("%s\n", iif(index == NULL,
			                     file_list->names[i], index+1));
		}
	if (!files_found)
		message ("No files found\n");
		
	free_namelist (file_list);
}


int	check_file_is_single (filename)
char	*filename;
{
	struct	namelist	*file_list;
	int			is_single;
	
	file_list = xv_expand_name(filename);
	if (file_list->count > 1) {
		error ("Too many files match %s\n", filename);
		is_single = FALSE;
	} else if (file_list->count == 0) {
		error ("No such file\n");
		is_single = FALSE;
	} else {
		strcpy (filename, file_list->names[0]);
		is_single = TRUE;
	}
	free_namelist (file_list);
	
	return is_single;
}


int	file_exists (filename)
char	*filename;
{
	struct	stat	buf;
	
	if (stat (filename, &buf) == -1)
		return FALSE;
	else
		return TRUE;
}


 char	*file_exists_somewhere (filename, path)
char	*filename, *path;
{
	static	char	 fn    [FILENAMESIZE];
	char		 xpath [FILENAMESIZE*2];
	char		 *dir;
	struct	namelist *file_list;
	
	if (filename[0] == '/' || filename[0] == '~') {
	
		file_list = xv_expand_name(filename);
		if (file_list->count == 1 && file_exists(file_list->names[0])) {
			free_namelist (file_list);
			strcpy (fn, file_list->names[0]);
			return fn;
		} else {
			free_namelist (file_list);
			return NULL;
		}
		
	} else {
	
		/* Baue den (erweiterten) Pfad xpath auf	*/
		if (path != NULL)
			strcpy (xpath, path);
		else
			strcpy (xpath, ".");
		strcat(strcat(xpath,PATH_DIRECTORY_SEPERATOR),
		       GRAPHED_DEFAULT_FONTS_DIRECTORY);
		strcat(strcat(xpath,PATH_DIRECTORY_SEPERATOR),
		       GRAPHED_DEFAULT_TYPES_DIRECTORY);
		strcat(strcat(xpath,PATH_DIRECTORY_SEPERATOR),
		       GRAPHED_LIBRARY_DIRECTORY);
		
		dir = strtok (xpath, PATH_DIRECTORY_SEPERATOR);
		while (dir != NULL) {
			strcat(strcat(strcpy(fn, dir), "/"), filename);
			file_list = xv_expand_name(fn);
			if (file_list->count == 1 && file_exists(file_list->names[0])) {
				free_namelist (file_list);
				strcpy (fn, file_list->names[0]);
				return fn;
			} else {
				free_namelist (file_list);
			}
			dir = strtok (NULL, PATH_DIRECTORY_SEPERATOR);
		}
	}

	return NULL;
}


char	*get_possible_fileselector_startup_filename ()
{
	static	char	filename [FILENAMESIZE];
	
	sprintf (filename, "%s/%s", getenv ("HOME"), ".graphed_fileselectors");

	return filename;
}


char	*get_existing_fileselector_startup_filename ()
{
	char	*name;
	
	name = file_exists_somewhere (".graphed_fileselectors",
		getenv ("HOME"));
	
	return name;
}
/************************************************************************/
/*									*/
/*		SYSTEMDIENSTE MIT FEHLERABFRAGE AUFRUFEN		*/
/*									*/
/************************************************************************/
/*									*/
/*	char	*mymalloc (size)					*/
/*									*/
/*	Wirkt wie malloc, nur fatal_error bei nicht mehr ausreichendem	*/
/*	Speicherplatz.							*/
/*									*/
/*======================================================================*/
/*									*/
/*	char	*mycalloc (n, size)					*/
/*									*/
/*	Pendant zu calloc.						*/
/*									*/
/************************************************************************/

#ifdef GRAPHED_DEBUG_MALLOC
#define N_MALLOC_LOG 100000
typedef struct {char * data; int count; } Malloc_log;
Malloc_log	malloc_log [N_MALLOC_LOG];
int		nallocated = 0;
#endif

char		*mymalloc (size)
unsigned	size;
{
	static	int	count = 0;
	static	unsigned sum  = 0;
	char		*allocated_space;

#ifdef GRAPHED_DEBUG_MALLOC
int i, j;
count++; sum += size;
if (count % 100 == 0) { fprintf (stderr, "malloc : %d    %d\n", count, sum); }
if (nallocated == 0) {
for (i=0; i<N_MALLOC_LOG; i++) {malloc_log[i].data=NULL; malloc_log[i].count = 0; }
}
#endif

	allocated_space = malloc (size);
	if (allocated_space == NULL) {
		fatal_error ("Can't get more memory\n");
	}

#ifdef GRAPHED_DEBUG_MALLOC
{ for (i=0; i<nallocated; i++) if (malloc_log[i].data == allocated_space) {
	if (malloc_log[i].count > 0)
		fprintf (stderr, "malloc-bug : %d %d\n", (int)malloc_log[i].data, malloc_log[i].count);
	malloc_log[i].count ++;
	break;
}
if (i == nallocated) { malloc_log[i].data = allocated_space; malloc_log[i].count= 1; nallocated++; }
}
fprintf (stderr, "MALLOC : %d    %d    %d\n", (int)size, (int)sum, (int)allocated_space);
#endif

	return allocated_space;
}

void	myfree (ptr)
char	*ptr;
{
	static	int	count = 0;

#ifdef GRAPHED_DEBUG_MALLOC
int i,j;
count++;
if (count % 100 == 0) { fprintf (stderr, "free : %d    %d\n", count, malloc_verify()); }
#endif

	if (ptr == NULL) {
		fprintf (stderr, "GraphEd internal error : NULL free\n");
	} else {
		free (ptr);
	}

#ifdef GRAPHED_DEBUG_MALLOC
{ for (i=0; i<nallocated; i++) if (malloc_log[i].data == ptr ) {
	if (malloc_log[i].count == 0) fprintf (stderr, "free-bug : %d %d\n", (int)malloc_log[i].data, malloc_log[i].count);
	malloc_log[i].count --;
	break;
}
if (i == nallocated) {
fprintf (stderr, "free-bug : not allocated at all %d\n", (int)ptr); }
}
fprintf (stderr, "FREE : %d\n",  (int)ptr);
#endif
}

char		*mycalloc (n, size)
unsigned	n, size;
{
	static	int	count = 0;
	static	unsigned sum  = 0;
	char		*allocated_space;
	extern	char	*calloc ();

#ifdef GRAPHED_DEBUG_MALLOC
int i,j;
count++; sum += n*size;
if (count % 100 == 0) { fprintf (stderr, "calloc : %d    %d\n", count, sum); }
#endif

	allocated_space = calloc (n, size);
	if (allocated_space == NULL) {
		fatal_error ("Can't get more memory\n");
	}
	
#ifdef GRAPHED_DEBUG_MALLOC
{ for (i=0; i<nallocated; i++) if (malloc_log[i].data == allocated_space) {
	if (malloc_log[i].count > 0) fprintf (stderr, "ccmalloc-bug : %d %d\n", (int)malloc_log[i].data, malloc_log[i].count);
	malloc_log[i].count ++;
	break;
}
if (i == nallocated) { malloc_log[i].data = allocated_space; malloc_log[i].count= 1; nallocated++; }
}
fprintf (stderr, "CALLOC : %d    %d\n", (int)(n*size), (int)allocated_space);
#endif
	return allocated_space;
}


void	free_lines (lines)
char	**lines;
{
	int	line = 0;
	
	for (line = 0; lines[line] != NULL; line ++) {
		myfree (lines[line]);
	}
	myfree (lines);
}


/************************************************************************/
/*									*/
/*			KOORDINATENTRANSFORMATIONEN			*/
/*									*/
/************************************************************************/
/*									*/
/*	void	scale (scaling, x,y)					*/
/*									*/
/*	Skaliert (x,y) entsprechend scaling.				*/
/*									*/
/*	typedef	enum {							*/
/*		SCALE_16_16,		x = 16;  y = 16			*/
/*		SCALE_32_32,		x = 32;  y = 32			*/
/*		SCALE_64_64,		x = 64;  y = 64			*/
/*		SCALE_128_128,		x = 128; y = 128		*/
/*		SCALE_DOWN_XY,		x = x/2; y = y/2		*/
/*		SCALE_DOWN_X,		x = x/2				*/
/*		SCALE_DOWN_Y,		y = y/2				*/
/*		SCALE_UP_XY,		x = max(1,x*2); y = max(1,y*2)	*/
/*		SCALE_UP_X,		x = max(1,x*2)			*/
/*		SCALE_UP_Y,		y = max(1,y*2)			*/
/*		SCALE_SQUARE_X,		y = x				*/
/*		SCALE_SQUARE_Y,		x = y				*/
/*		NUMBER_OF_SCALINGS					*/
/*	}								*/
/*		Scaling;						*/
/*									*/
/*	Scaling	scaling;						*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	constrain_8 (orgx, orgy, x, y)				*/
/*									*/
/*	Die Linie (orgx, orgy) -- (x, y) wird am Ende (x, y) so		*/
/*	justiert, dass sie vertikal, horizontal oder schraeg unter 45	*/
/*	Grad verlaeuft.							*/
/*									*/
/*======================================================================*/
/*									*/
/*	void	constrain_to_grid (width, x, y)				*/
/*									*/
/*	Der Punkt (x,y) wird auf den naechstliegenden Punkt eines	*/
/*	Gitters mit Weite width gesetzt.				*/
/*									*/
/************************************************************************/


void	scale (scaling, x,y)
Scaling	scaling;
int	*x, *y;
{
	switch (scaling) {
	    case SCALE_16_16 :
		*x = 16;
		*y = 16;
		break;
	    case SCALE_32_32 :
		*x = 32;
		*y = 32;
		break;
	    case SCALE_64_64 :
		*x = 64;
		*y = 64;
		break;
	    case SCALE_96_96 :
		*x = 96;
		*y = 96;
		break;
	    case SCALE_128_128 :
		*x = 128;
		*y = 128;
		break;
	    case SCALE_192_192 :
		*x = 192;
		*y = 192;
		break;
	    case SCALE_256_256 :
		*x = 256;
		*y = 256;
		break;
	    case SCALE_384_384 :
		*x = 384;
		*y = 384;
		break;
	    case SCALE_512_512 :
		*x = 512;
		*y = 512;
		break;
	    case SCALE_IDENTITY :
		*x = *x;
		*y = *y;
		break;
	    case SCALE_DOWN_XY :
		*x = *x/2;
		*y = *y/2;
		break;
	    case SCALE_DOWN_X :
		*x = *x/2;
		break;
	    case SCALE_DOWN_Y :
		*y = *y/2;
		break;
	    case SCALE_UP_XY :
		*x = maximum ((*x)*2, 1);
		*y = maximum ((*y)*2, 1);
		break;
	    case SCALE_UP_X :
		*x = maximum ((*x)*2, 1);
		break;
	    case SCALE_UP_Y :
		*y = maximum ((*y)*2, 1);
		break;
	    case SCALE_SQUARE_X :
		*y = *x;
		break;
	    case SCALE_SQUARE_Y :
		*x = *y;
		break;
	    default :
		break;
	}
}


Scaling	size_to_scale (size)
int	size;
{
	switch (size) {
	     case 16 :
		return SCALE_16_16;
		break;
	     case 32 :
		return SCALE_32_32;
		break;
	     case 64 :
		return SCALE_64_64;
		break;
	     case 96 :
		return SCALE_96_96;
		break;
	     case 128 :
		return SCALE_128_128;
		break;
	     case 192 :
		return SCALE_192_192;
		break;
	     case 256 :
		return SCALE_256_256;
		break;
	     case 384 :
		return SCALE_384_384;
		break;
	     case 512 :
		return SCALE_512_512;
		break;
	     default :
		return SCALE_IDENTITY;
		break;
	} 
}



void	constrain_8 (orgx, orgy, x, y)
int	orgx, orgy;
int	*x, *y;
{
	register int	dx = *x - orgx,
			dy = *y - orgy,
			dx_abs = abs (dx),
			dy_abs = abs (dy),
			dx_new,
			dy_new;
	
	if ( (dx_abs > dy_abs/2) && (dx_abs < dy_abs*2)) {
		if (dx < 0)
			dx_new = - maximum (dx_abs, dy_abs);
		else
			dx_new =   maximum (dx_abs, dy_abs);
		if (dy < 0)
			dy_new = - maximum (dx_abs, dy_abs);
		else
			dy_new =   maximum (dx_abs, dy_abs);
	} else {
		if (dx_abs > dy_abs) {
			dy_new = 0;
			dx_new = dx;
		} else {
			dx_new = 0;
			dy_new = dy;
		}
	}
	*x = orgx + dx_new;
	*y = orgy + dy_new;
}


void	constrain_to_grid_in_one_dimension (width, x)
int	width;
int	*x;
{
	int	mod_width;
	
	mod_width = *x % width;
	*x -= mod_width;
	if (mod_width > width/2) *x += width;
}


void	constrain_to_grid (width, x,y)
int	width;
int	*x,*y;
{
	
	constrain_to_grid_in_one_dimension (width, x);
	constrain_to_grid_in_one_dimension (width, y);
}


/************************************************************************/
/*									*/
/*			TYPKONVERSION					*/
/*									*/
/************************************************************************/
/*									*/
/*	float	deg_to_rad (deg)					*/
/*	int	rad_to_deg (rad)					*/
/*									*/
/*	Umrechnung Bogenmass - Grad, grad ist int !			*/
/*									*/
/*======================================================================*/
/*									*/
/*	char	*int_to_acii    (i)					*/
/*	char	*float_to_ascii (f)					*/
/*									*/
/*	Umrechnung int - ascii-string.					*/
/*	ACHTUNG : Rueckgabe ist Zeiger auf statisches Array !		*/
/*									*/
/************************************************************************/


float	deg_to_rad (deg)
int	deg;
{
	return ((float)(deg)/(float)(180) * M_PI);
}


int	rad_to_deg (rad)
float	rad;
{
	return (int)((rad/M_PI) * (float)180);
}



char	*int_to_ascii (i)
int	i;
{
	static	char	buf[50];
	
	sprintf (buf, "%d\0", i);
	
	return buf;
}


char	*float_to_ascii (f)
float	f;
{
	static	char	buf[50];
	
	sprintf (buf, "%.1f\0", f);
	
	return buf;
}
/****************************************************************/
/*								*/
/*			    TIMESTAMPS				*/
/*								*/
/****************************************************************/
/*								*/
/*	int	ticks ()					*/
/*								*/
/*	Timestamps are implemented as a simple counter.		*/
/*	Each call of ticks () returns the increments this	*/
/*	counter.						*/
/*								*/
/****************************************************************/


int	ticks ()
{
	static	int	tick_tack = 1;	/* Always > 0	*/

	return tick_tack ++;
}
/****************************************************************/
/*								*/
/*		    Calculate Subwindow Position		*/
/*								*/
/****************************************************************/
/*								*/
/*	void	calculate_subwindow_position ()			*/
/*								*/
/****************************************************************/


void	calculate_subwindow_position (subwindow_width, subwindow_height,
	                              selection_x,     selection_y,
	                              selection_width, selection_height,
	                              x,y)
int	subwindow_width, subwindow_height;
int	selection_x, selection_y,
	selection_width, selection_height;
int	*x,*y;
{
#define	SUBWINDOW_POPUP_GAP_X 10
#define	SUBWINDOW_POPUP_GAP_Y 10

	extern	int	screenwidth;
	extern	int	screenheight;
	int		pos_left, pos_right;
	int		pos_up, pos_down, pos_vertical;
	int		horizontal_position_scrambled = FALSE;
	
	translate_wac_to_base_frame_space (&selection_x, &selection_y);
	
	pos_right    = selection_x + selection_width/2 + SUBWINDOW_POPUP_GAP_X;
	pos_left     = selection_x - selection_width/2 - SUBWINDOW_POPUP_GAP_X - subwindow_width;
	pos_up       = selection_y - selection_height/2 - subwindow_height;
	pos_down     = selection_y + selection_height/2;
	pos_vertical = selection_y - selection_height/2;
	
	if (screenwidth >= pos_right + subwindow_width) {
		*x = pos_right;
	} else if (pos_left >= 0) {
		*x = pos_left;
	} else {
		int	possible_right = maximum (screenwidth - subwindow_width + SUBWINDOW_POPUP_GAP_X, 0);
		int	possible_left  = 0;
		int	possible_left_edge = possible_left + subwindow_width + SUBWINDOW_POPUP_GAP_X;
		if (possible_right - (selection_x - selection_width/2) >= 
		    (selection_x + selection_width/2) - possible_left_edge) {
			*x = possible_right;
		} else {
			*x = possible_left;
		}
		horizontal_position_scrambled = TRUE;
	}
	
	if (!horizontal_position_scrambled) {
		if (screenheight >= pos_vertical + subwindow_height) {
			*y = pos_vertical;
		} else {
			*y = maximum (minimum (pos_vertical, screenheight - subwindow_height), 0);
		}
	} else {
		if (screenheight >= pos_down + SUBWINDOW_POPUP_GAP_Y + subwindow_height) {
			*y = pos_down + SUBWINDOW_POPUP_GAP_Y;
		} else if (pos_up - SUBWINDOW_POPUP_GAP_Y >= 0) {
			*y = pos_up - SUBWINDOW_POPUP_GAP_Y;
		} else {
			int	possible_down = maximum (screenheight - subwindow_height - SUBWINDOW_POPUP_GAP_Y, 0);
			int	possible_up   = 0;
			int	possible_up_edge = possible_up + subwindow_height + SUBWINDOW_POPUP_GAP_Y;
			if (possible_up_edge - (selection_y + selection_height/2) >= 
			    (selection_y - selection_height/2) - possible_down) {
				*y = possible_up;
			} else {
				*y = possible_down;
			}
		}
	}
}
