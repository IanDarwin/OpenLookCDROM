static char *RCSid = "$Header: drawplot.c,v 1.3 88/03/01 08:09:55 dwf Locked $";

/*
 * $Log:	drawplot.c,v $
 * Revision 1.3  88/03/01  08:09:55  dwf
 * Modified Gouraud shading to compute image on C side (instead of in server)
 * 
 * Revision 1.2  87/06/01  21:51:01  dwf
 * Gouraud shading in PostScript
 * 
 */

#include "surfmodl.h" 
#include "draw.h"
float gray = 0;
initialize()
{
	if (ps_open_PostScript() == 0) {
		fprintf(stderr,"Surfmodl: Cannot connect to window server\n");
		exit(0);
	}
	ps_initialize("SURFMODL",Gxmax,Gymax);
	ps_findfont("Times-Roman");
	ps_scalefont(Gxmax/30);
	ps_setfont();
}

void gmoveto(X,Y)
int X,Y;
{
  ps_moveto(X, Y);
}
void glineto(X, Y)
int X,Y;
{
  ps_lineto(X,Y);
}
void gfill()
{
   ps_fill();
}
void gsave()
{
	ps_gsave();
}
void grestore()
{
	ps_grestore();
}
void gnewpath()
{
   ps_newpath();
}
void gstroke()
{
   ps_stroke();
}
void gshow(s)
char *s;
{
    ps_show(s);
}
void gsetgray(Shade)
float Shade;
{
   ps_setgray((float)(Shade));
   gray =  Shade;
}
void gcolor(Color,Saturation)
double Color,Saturation;
{
   /*  ps_setcolor((float)Color,(float)Saturation); */
     fprintf(PostScript, " %7.4f %7.4f currentgray sethsbcolor\n",Color,Saturation);
}

/* initialize  variables for gouraud image  operator */

void gsetimage(xmin,ymin,xmax,ymax)
int xmin,ymin,xmax,ymax;
{
	ps_setimage(xmin,ymin,xmax,ymax);
}

void gshowimage()
{
	ps_showimage();
}

void gdraw(X1,Y1,X2,Y2)
int X1,Y1,X2,Y2;
{
  ps_moveto(X1, Y1);
  ps_lineto(X2, Y2);
  ps_stroke();
}

void gplot(X,Y,Color)
int X,Y,Color;
{
  ps_setgray(Color); 
  ps_moveto(X,Y);
  ps_lineto(X+1,Y+1);
  ps_stroke();
}

void shdraw(X1,X2,Y,Color,Fmod)
int X1,X2,Y,Color;
int Fmod;
{
 int X;           /* x coord */

  if ( (Fmod > 1) ) {
    for ( X =X1  ;X <= X2; X++  )
      if ( ( X % Fmod == Y % Fmod) )
         gplot(X,Y , Color);
       else
         gplot(X,Y , 0);
  }  else if ( (Fmod < -1) ) {
    for ( X = X1 ; X <= X2; X++  )
      if ( ( X % -Fmod == Y % -Fmod) )
         gplot(X ,Y , 0);
       else
         gplot(X ,Y , Color);
  }  else
     gcolor(Color);
     gdraw(X1,Y ,X2 ,Y ); 
} /* procedure Shdraw */

void shplot(X,Y,Color,Fmod)
int X,Y,Color;
int Fmod;
{
  if ( (Fmod > 1) ) {
    if ( (X % Fmod == Y % Fmod) )
       gplot(X, Y, Color);
     else
       gplot(X, Y, 0);
  }  else if ( (Fmod < -1) ) {
    if ( ( X% -Fmod == Y % -Fmod) )
       gplot(X, Y, 0);
     else
       gplot(X, Y, Color);
  }  else
     gplot(X, Y, Color);
} /* procedure Shplot */

/* This is the 4x4 dithering matrix by Jan Allebach, as described in:
  Blake, Robert A., and Allebach, Jan P., "Digital Ultrasonic Image
  Construction Using Electronic Ordered Dither Techniques", Journal of
  Nondestructive Evaluation, Vol. 2, No. 1, 1981.
*/
 int Dither[4][4] =  {
   {11, 16,  3,  7},
   { 5,  6, 10, 12},
   {15,  2, 14,  4},
   { 1,  9,  8, 13}
   }; 

void dithplot(X,Y,Ishade,Color)
int X,Y,Ishade,Color;
{ 
  int Xmod;    /* X & Y coords modulo 4. This is the place in */
                            /* the dither matrix */
  int Ymod;    /* X & Y coords modulo 4. This is the place in */
                            /* the dither matrix */
  Xmod = X % 4 + 1;
  Ymod = Y % 4 + 1;
  if ( (Ishade >= Dither[Xmod][Ymod]) )
     gplot(X, Y, Color);
   else
     gplot(X, Y, 0);
} /* procedure DITHPLOT */

void dithdraw(X1,X2,Y,Ishade,Color)
int X1,X2,Y,Ishade,Color;
{
 int X;        /* X coord along line */
 int Xmod;    /* X & Y coords modulo 4. This is the place in */
                            /* the dither matrix */
 int Ymod;    /* X & Y coords modulo 4. This is the place in */
                            /* the dither matrix */

  Ymod = Y % 4 + 1;
  for (X  = X1 ; X < X2; X++ ) {
    Xmod = X % 4 + 1;
    if ( (Ishade >= Dither[Xmod][Ymod]) )
       gplot(X, Y, Color);
     else
       gplot(X, Y, 0);
  } /* for X */
} /* procedure DITHDRAW */

void intrplot(X,Y,Shade)
int X,Y;
float Shade;
{
    int Pcolor;        /* color to set pixel */
    int Fmod;          /* mod for fill pixel setting */
    int Ishade;        /* integer version of shade (0..64) for dithering */

#ifdef NEWS
    register unsigned char *sh;
    char *malloc();
    register int i,nx;
    int Sh;
    nx = Xmax-Xmin+1;
    sh = (unsigned char *)malloc(nx);
    Sh = (int)Shade;
    for (i = 0;i <=nx;i++)
      sh[i] = (unsigned char)(Sh);
  /*  fwrite(sh,nx,1,PostScript); */
    ps_storeshade(Y-Ymin,sh,nx); 
    free(sh);
/*
    ps_storeline(Y,(float)Shade,(float)Shade);
*/

    return;
#else
  if ( (Ncolors >= 3) && (Mono) ) {
    /* Use system's colors as shades of grey */
    colormod (Shade, System, &Pcolor, &Fmod);
    /* Now finally set the pixel to the desired shade */
    shplot (X, Y, Pcolor, Fmod);
  }  else {
    /* Use dithered shading */
    Ishade = trunc (Shade * 16.0);
    dithplot (X, Y, Ishade, Color);
  } /* if Ncolors... */
#endif
} /* Procedure Intrplot */

void intrdraw(X1,X2,Y,Shade1,Shade2)
int X1,X2,Y;
float Shade1,Shade2;
{
 
    int X;
    double Shfact;           /* factor for shade interpolation */
    boolean Firstsh;       /* flag first time through */
    double Shade;            /* shade at pixel */
#ifdef NEWS
    register int i;
    register unsigned char *sh;
    int nx;
    int Sh1, Sh2;
    char *realloc(), *malloc();

    /* Just store two values of Shade at edge of bounding box at Y value,
       rely on clipping to remove unwanted regions */
    nx = Xmax - Xmin + 1;
    sh = (unsigned char *)malloc(nx);
    Sh1 = (int) 255 * Shade1;
    Sh2 = (int) 255 * Shade2;

    if (X2 == X1){
      for (i = X1-Xmin; i <= X2-Xmin; i++)
	sh[i] = (unsigned char)(Sh1);
      ps_storeshade(Y-Ymin,sh,nx); 
    /*  fwrite(sh,nx,1,PostScript); */
  /*    ps_storeline(Y,(float)Shade1,(float)Shade1); */

    } else {
      for (i = 0; i <= nx; i++)
          sh[i] = (unsigned char)( ((i+Xmin-X1) * (Sh2-Sh1))/(X2-X1+1) + Sh1);
      ps_storeshade(Y-Ymin,sh,nx);
    /* fwrite(sh,nx,1,PostScript); */
    }
    free(sh);

    return;
#else

  Firstsh = TRUE;
  if ( ( X2 == X1) )
    Shfact = 0.0;
   else
    Shfact = (Shade2 - Shade1) / (X2 - X1);

  for ( X = X1 ; X <= X2; X++ ) {
    if ( (Shfact == 0.0) )
      if ( (Firstsh) ) {
        Shade = Shade1;
        Firstsh = FALSE;
      }  else
        Shade = Shade2;
     else
      Shade = Shade1 + (X-X1) * Shfact;

    /* Plot this pixel with shading */
    intrplot (X, Y,  Shade);
  } /* for X */
#endif
} /* Procedure Intrdraw */

