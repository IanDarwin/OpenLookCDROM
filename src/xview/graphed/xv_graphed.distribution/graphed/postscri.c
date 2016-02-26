/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Uwe Schnieders, Michael Himsolt		*/
/************************************************************************/
/*									*/
/*				postscri.c				*/
/*									*/
/************************************************************************/
/*									*/
/*	postscri.c enthaelt alle Functionen die zur Ausgabe im 		*/
/*	Postscript-Format notwendig sind .				*/
/*	Alle Functionen mit:						*/
/*	'post_help' schreiben nichts in die Datei			*/
/*	'post_' schreiben im Postscript-Format auf die Datei		*/
/*									*/
/*									*/
/*									*/
/************************************************************************/

#include "misc.h"
#include "graph.h"

#include "modula.h"
#include "print.h"
#include "la_paint.h"
#include "postscri.h"



/****************************************************************/

static	FILE	*out_file;
static  FILE	*in_file;

Orientierung orientierung;


/****************************************************************/

void	exchange(x,y)
	INTEGER *x,*y;
BEGIN
  INTEGER z;

  z  = *x;
  *x = *y;
  *y = z;
END


int	post_help_compute_string_length_from_pixrect(width,heigh)
INTEGER	heigh,width;
BEGIN
  RETURN((int)((heigh /8 )+iif(heigh % 8 ==0,0,1))*width)
END

void	post_help_orientierung(x)
	Orientierung x;
BEGIN
  orientierung = x;
END


void	post_help_orientiere(x,y)
	INTEGER x,y;
BEGIN
  IF orientierung = landscape THEN
    exchange(&x,&y);
  END
END


int	post_help_open_output(fname)
	char fname[];
BEGIN
  IF NOT( out_file = fopen(fname,"w") ) THEN	
	error ("error opening %s\n");
	return FALSE;
  END;
  return post_dateikopf();
END;

int	post_help_close_output()
BEGIN
  if (post_dateiende() == FALSE)
  	return FALSE;
  IF fclose(out_file) THEN
	error ("error closing %s\n");
	return FALSE;
  END;
  return TRUE;
END
  

/****************************************************************/
/* 	Nur die Functionen, die mit 'post_' beginnen,		*/
/*	schreiben in Postscriptformat auf die Ausgabedatei 	*/
/*								*/
/****************************************************************/


int	post_dateikopf()
BEGIN 
  char	ch;
  char  *extended_filename;

  extended_filename = file_exists_somewhere (graphed_postscript_header,
                                             getenv ("GRAPHED_INPUTS"));
  IF extended_filename == NULL ||
     NOT( in_file = fopen (extended_filename,"r") ) THEN	
	error ("error opening %s\n", graphed_postscript_header);
	return FALSE;
  END;
  WHILE (ch=fgetc(in_file)) != EOF DO
    fputc(ch,out_file);
  END;
  IF fclose(in_file) THEN
	error ("error closing %s\n", graphed_postscript_header);
	return FALSE;
  END;
  return TRUE;
END


int	post_dateiende()
BEGIN 
  char	ch;
  char *extended_filename;

  extended_filename = file_exists_somewhere (graphed_postscript_tail,
                                             getenv ("GRAPHED_INPUTS"));
  IF extended_filename == NULL ||
     NOT( in_file = fopen(extended_filename,"r") ) THEN	
	error ("error opening %s\n", graphed_postscript_tail);
	return FALSE;
  END;
  WHILE (ch=fgetc(in_file)) != EOF DO
    fputc(ch,out_file);
  END;
  IF fclose(in_file) THEN
	error ("error closing %s\n", graphed_postscript_tail);
	return FALSE;
  END;
  return TRUE;
END

/****************************************************************/
/*	post_dot(X-Position,Y-Position,Durchmesser)		*/
/*		setzt einen Punkt mit 'Durchmesser' an die	*/
/*		X,Y-Position					*/
/****************************************************************/

void	post_dot(x0,y0,d)
int	x0,y0,d;
BEGIN 
	fprintf(out_file,"%d %d %d dot \n"
			,x0
			,y0
			,d / 2  );
END

/****************************************************************/
/*	post_moveto(X,Y) 					*/
/*		setzt den internen Postscript Cursor auf die	*/
/*		X,Y-Position					*/
/****************************************************************/

void	post_moveto(x0,y0)
int	x0,y0;
BEGIN 
	fprintf(out_file,"%d %d m \n"
			,x0 
   			,y0);
END

/****************************************************************/
/*	post_lineto(X,Y)					*/
/*		zieht eine Linie von der alten Cursorposition	*/
/*		zur neuen X,Y-Position				*/
/****************************************************************/

void	post_lineto(x0,y0)
int	x0,y0;
BEGIN 
	fprintf(out_file,"%d %d l \n"
			,x0
			,y0);
END

/****************************************************************/
/*	post_vector(X0,Y0,X1,Y1)				*/
/*		setzt den cursor auf die Position X0,Y0		*/
/*		und zieht dann ein Linie nach X1,Y1		*/
/****************************************************************/

void	post_vector(x0,y0,x1,y1)
int	x0,y0,x1,y1;
BEGIN 
	fprintf(out_file,"%d %d %d %d v \n"
			,x1
			,y1
			,x0
			,y0);
END

/****************************************************************/
/*	post_ellipse(X0,Y0,xr,yr)				*/
/*		beschreibt eine Ellipse mit dem Mittelpunkt 	*/
/*		X0,Y0 und den Radien xr,yr			*/
/****************************************************************/

void	post_ellipse(x0,y0,xr,yr)
int	x0,y0,xr,yr;
BEGIN 
	post_gsave();
	post_newpath();
	fprintf(out_file,"%d %d %d %d %d %d ellipse \n"
			,x0
			,y0
			,(xr-1) / 2
			,(yr-1) / 2
			,0,360);
	post_stroke();
	post_grestore();
END

/****************************************************************/
/*	post_box(X0,Y0,XR,YR)					*/
/*		beschreib ein Rechteck mit linken oberen Punkt	*/
/*		X0,Y0 einer Breite von XR und einer Hoehe von YR*/
/****************************************************************/

void	post_box(x0,y0,xr,yr)
int	x0,y0,xr,yr;
BEGIN 
	post_gsave();
	post_newpath();
	fprintf(out_file,"%d %d %d %d box \n"
			,x0
			,y0
			,(xr-1)
			,(yr-1));
	post_stroke();
	post_grestore();
END

/****************************************************************/
/*	post_font(font,groesse)					*/
/*		waehlt einen Font des Laserdrucker aus und 	*/
/*		scaliert diesen auf eine 'groesse'		*/
/****************************************************************/

void	post_font(font,groesse)
char 	*font;
INTEGER	groesse;
BEGIN 
	fprintf(out_file,"/%s findfont %d graphedscalefont setfont \n",
			font,groesse );	
END

/****************************************************************/
/*	post_showt(Text)					*/
/*		schreibt einen Text				*/
/* 		(momentan um 90 Grad gedreht)			*/
/****************************************************************/

void	post_show(text)
char 	*text;
BEGIN 
	char	*c;
	
	/* Enhanced by MH 21/8/89	*/
	fprintf (out_file, "(");
	if (text != NULL) for (c = text; *c != '\0'; c++) {
		switch (*c) {
		    case '(' :
		    case ')' :
		    case '\\' :
			fprintf (out_file, "\\%c", *c);
			break;
		    case '\t' :
			fprintf (out_file, "\t");
			break;
		    default :
			fprintf (out_file, "%c", *c);
			break;
		}
	}
	fprintf (out_file,") showt\n");	
END

/****************************************************************/
/*	post_def_string(Stringname,Laenge)			*/
/*		definiert eine Stringvariable			*/
/* 								*/
/****************************************************************/

void	post_def_string(stringname,laenge)
char 	stringname[];
INTEGER	laenge;
BEGIN 
	fprintf(out_file,"/%s %d string def \n",stringname,laenge);	
END

/****************************************************************/
/*	post_read_image(stringname,width,heigh,pr)		*/
/*		liesst ein Pixrect in einen String		*/
/*								*/
/****************************************************************/

void	post_read_image(stringname,width,heigh,pr)
char	stringname[];
int     width,heigh;
Pixrect	*pr;
BEGIN 
	register	int	ausgabe;
	register	int	i,j,k;
	
	fprintf(out_file,"currentfile %s readhexstring \n",stringname);	
	FOR i=width-1;i>=0;i-- DO
	  FOR j=0;j<=heigh-1;j=j+8 DO
	    ausgabe = 0 ;
	    FOR k=0;k<=7;k++ DO
	      ausgabe = (ausgabe * 2 ) + iif(pr_get(pr,i,j+k) >0 ,0,1);
	    END;
	    fprintf(out_file,"%02x",ausgabe);
	  END;
	  fprintf(out_file,"\n");
	END;
	fprintf(out_file,"pop pop \n");	
END


/****************************************************************/
/*	post_write image(stringname,width,heigh)		*/
/*		schreibt ein Pixrect  aus stringname 		*/
/*		in Postscriptformat				*/
/*		auf die Ausgabedatei				*/
/****************************************************************/

void	post_write_image(stringname,width,heigh)
char	stringname[];
int     width,heigh;
BEGIN 	
	fprintf(out_file," %d %d 1 \n",heigh,width);	
	fprintf(out_file,"[ %d 0 0 %d 0 %d] \n",heigh,-width,width);	
	fprintf(out_file,"{ %s } image \n",stringname);	
END


/****************************************************************/
/*	post_image(width,heigh,pr)			*/
/*		schreibt ein Pixrect in Postscriptformat	*/
/*		auf die Ausgabedatei				*/
/*								*/
/****************************************************************/

void	post_image(width,heigh,pr)
int     width,heigh;
Pixrect	*pr;
BEGIN 
	register	int	ausgabe;
	register	int	i,j,k;
	
	fprintf(out_file,"/buffer %d string def \n",
		post_help_compute_string_length_from_pixrect(width,heigh));	
	fprintf(out_file," %d %d 1 \n",heigh,width);	
	fprintf(out_file,"[ %d 0 0 %d 0 %d] \n",heigh,-width,width);	
	fprintf(out_file,"{ currentfile buffer readhexstring pop} image \n");	
	FOR i=width-1;i>=0;i-- DO
	  FOR j=0;j<=heigh-1;j=j+8 DO
	    ausgabe = 0 ;
	    FOR k=0;k<=7;k++ DO
	      ausgabe = (ausgabe * 2 ) + iif(pr_get(pr,i,j+k) >0 ,0,1);
	    END;
	    fprintf(out_file,"%02x",ausgabe);
	  END;
	  fprintf(out_file,"\n");
	END;
END


/****************************************************************/
/*	post_closepath()					*/
/*		schliesst einen Pfad (dies Operation ist 	*/
/*		notwendig falls ein Flaeche gefuellt werden soll*/
/****************************************************************/

void	post_closepath()
BEGIN 
	fprintf(out_file,"closepath \n");
END


void	post_newpath()
BEGIN 
	fprintf(out_file,"newpath \n");
END


void	post_stroke()
BEGIN 
	fprintf(out_file,"stroke \n");
END


void	post_gsave()
BEGIN 
	fprintf(out_file,"gsave \n");
END


void	post_grestore()
BEGIN 
	fprintf(out_file,"grestore \n");
END


void	post_scale(x0,y0)
INTEGER	x0,y0;
BEGIN 
	fprintf(out_file,"%d %d scale \n"
			,x0
			,y0  );
END


void	post_translate(x0,y0)
INTEGER	x0,y0;
BEGIN 
	fprintf(out_file,"%d %d translate \n"
			,x0
			,y0 );
END


void	post_rotate(winkel)
INTEGER	winkel;
BEGIN 
	fprintf(out_file,"%d rotate \n"
			,winkel);
END


void	post_setlinewidth(width)
INTEGER	width;
BEGIN
	fprintf(out_file,"%d graphedsetlinewidth \n",width);
END


void	post_showpage()
BEGIN
	fprintf(out_file,"showpage \n");
END


void 	post_setdash(dash_array)
short	dash_array[];
BEGIN
	short	i;

	fprintf(out_file,"[");
	i=0;
        WHILE dash_array[i] != 0 DO
	  fprintf(out_file,"%d ",dash_array[i] );
	  i++;
	END;
	fprintf(out_file,"] 0 setdash \n");
END

void	post_def(string,zahl)
char	string[];
INTEGER 	zahl;
BEGIN
	fprintf(out_file,"/%s { %d } def \n",string,zahl);
END

void	post_call_proc(string)
char	string[];
BEGIN
	fprintf(out_file,"%s \n",string);
END

