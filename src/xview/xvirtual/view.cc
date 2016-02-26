// 1993 by M. Roth

#include "view.h"
#include <string.h>
#include <math.h>
#include "x.h"

void  (*line)(int,int,int,int,int);
void  (*shadeLine)(int,int,int,int,int,int);
void  (*shadowLine)(int,int,int);
void  (*point)(int,int,int);
void  (*openScreen)();
void  (*closeScreen)();
void  (*swopScreen)();
void  (*clearScreen)();
int   maxX,maxY;
float UrsprungX,UrsprungY;
float stretchX,stretchY;
int   openFlag=0;
int   aktuelleFarbe;
struct
{
  char name[20];
  float rot;
  float gruen;
  float blau;
} color[16] =
{/*
  { "rot",200,0,0 },
  { "hellrot",255,63,63 },
  { "gruen",0,200,0 },
  { "hellgruen",63,200,63 },
  { "blau",0,0,200 },
  { "hellblau",63,63,255 },
  { "weiss",255,255,255 },
  { "grau",127,127,127 },
  { "schwarz",17,17,17 },
  { "gelb",255,200,0 },
  { "orange",255,80,0 },
  { "braun",150,100,30 },
  { "lila",255,0,127 },
*/
  { "red",200,0,0 },
  { "lightred",255,63,63 },
  { "green",0,200,0 },
  { "lightgreen",63,200,63 },
  { "blue",0,0,200 },
  { "lightblue",63,63,255 },
  { "white",255,255,255 },
  { "gray",127,127,127 },
  { "black",17,17,17 },
  { "yellow",255,200,0 },
  { "orange",255,80,0 },
  { "brown",150,100,30 },
  { "violet",255,0,127 },
};
int   colorCount=13;

// Treiber initialisieren
void ViewSet( void (*OpenScreen)(),
	      void (*CloseScreen)(),
	      void (*SwopScreen)(),
	      void (*ClearScreen)(),
	      void (*Line)(int,int,int,int,int),
	      void (*Point)(int,int,int),
	      void (*ShadeLine)(int,int,int,int,int,int),
	      void (*ShadowLine)(int,int,int),
	      int MaxX,
	      int MaxY)
{
  line=Line;
  shadeLine=ShadeLine;
  shadowLine=ShadowLine;
  point=Point;
  openScreen=OpenScreen;
  closeScreen=CloseScreen;
  swopScreen=SwopScreen;
  clearScreen=ClearScreen;
  maxX=MaxX;
  maxY=MaxY;
  UrsprungX=MaxX/2;
  UrsprungY=MaxY/2;
}

// Zeichen und Anzeigebildschirm tauschen
void ViewSwop()
{
  swopScreen();
}

// Bildschirm l”schen
void ViewClear()
{
  clearScreen();
}

// Punkt zeichnen
void ViewPoint(int x, int y, int farbe)
{
  point(x,y,farbe);
}

// Linie zeichnen
void ViewLine(int x1, int y1, int x2, int y2, int farbe)
{
  line(x1,y1,x2,y2,farbe);
}

// Shattierte Zeile zeichnen
void ViewShadeLine(int y,int laenge,int xv,int hv,int hp,int farbe)
{
  shadeLine(y,laenge,xv,hv,hp,farbe);
}

// Shattierte Zeile zeichnen
void ViewShadowLine(int y,int x,int laenge)
{
  shadowLine(y,x,laenge);
}

// Alle Farben l”schen
void ViewClearColor()
{
  colorCount=0;
}

// Farbregister laden
void ViewLoadColorReg(int nr,float r,float g,float b)
{
  int c; 
  float max=63*63;
  float minP=.02;

  r=r*max/255;
  g=g*max/256;
  b=b*max/256;
  for(c=0 ; c<16 ; c++)
  {
    X_AllocColor((nr+1)*16+c,
		 (int) sqrt(r*minP+(r-r*minP)/16*(c)),
		 (int) sqrt(g*minP+(g-g*minP)/16*(c)),
		 (int) sqrt(b*minP+(b-b*minP)/16*(c)));
  }
}

// Farbe definieren
void ViewDefColor(char *n,float r,float g,float b)
{
  if (colorCount>15) return;
  strcpy(color[colorCount].name,n);
  color[colorCount].rot=r;
  color[colorCount].gruen=g;
  color[colorCount].blau=b;
  if(openFlag) ViewLoadColorReg(colorCount,r,g,b);
}

// Aktuelle Zeichenfarbe setzen
void ViewSetColor(char *n)
{
  int c;

  for(c=0 ; c<colorCount ; c++)
  {
    if( !(strcmp(n,color[c].name) ))
    {
      aktuelleFarbe=c+1;
      break;
    }
  }
}

// Aktuelle Zeichenfarbe setzen
void ViewSetColor(int f)
{
  aktuelleFarbe=f;
}

// Koordinate in View projezieren
void ViewProjektion(float &x,float &y, float &z)
{
  float yy;
  yy=800/y;
  x=x*yy*stretchX+UrsprungX;
  z=z*yy*stretchY+UrsprungY;
  y=1/y;
}

// Maximalen X-Wert ermitteln
int ViewGetMaxX()
{
  return maxX;
}

// Maximale Y-Wert ermitteln
int ViewGetMaxY()
{
  return maxY;
}

// Liefert aktuellen Farbwert
int ViewGetColor()
{
  return aktuelleFarbe;
}

// Bildschirm oeffnen und Parameter setzen
void ViewInit()
{
  int c;

  openScreen();
  stretchX=1.0;
  stretchY=X_Geometry();
  openFlag=1;
  for(c=0 ; c<colorCount ; c++)
  {
    ViewLoadColorReg(c,color[c].rot,color[c].gruen,color[c].blau);
  }
}

// Bildschirm schliessen
void ViewEnd()
{
  if (openFlag==1) closeScreen();
  openFlag=0;
}





