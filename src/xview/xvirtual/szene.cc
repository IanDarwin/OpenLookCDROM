//  1993 by M. Roth

#include "szene.h"
#include "view.h"
#include "zbuffer.h"
#include <sys/times.h>
#include <unistd.h>

long 		startZeit=0;
long 		lastZeit=0;
int 		drawMode=WIRE;
KoerperPtr 	szeneKoerper;
KoerperPtr 	lichtKoerper;
NormVektorPtr 	licht;
Matrix 		rotation;
Vektor 		position;

// Zeit in hundertstel Secunden abfragen
long GetTime()
{
  struct tms	tmsBuff;
  clock_t       t;

  t=times(&tmsBuff);
  return (long)t;
}

// Startzeit setzen
void SzeneResetTime()
{
  startZeit=GetTime();
  startZeit--;
}

// Initialisieren der Speicherverwaltung fr Objekte
void SzeneInit()
{
  szeneKoerper=new Koerper("szene");
  lichtKoerper=new Koerper("light");
  szeneKoerper->add( lichtKoerper );
  licht=new NormVektor(Vektor(0,-1,0));
  lichtKoerper->add(licht);
  rotation=Matrix(0,0,0);
  position=Vektor(0,0,0);
  startZeit=0;
  lastZeit=0;
}

// Beenden der Speicherverwaltung fuer Objekte
void SzeneEnd()
{
  delete (Koerper *)szeneKoerper;
}

// Zeichenmodus festlegen
void SzeneDrawMode(int mode)
{
  drawMode=mode;
}

// Zeichenmodus abfragen
int SzeneGetDrawMode()
{
  return drawMode;
}

// Szene errechnen, zeichnen und anzeigen
void SzeneErstellen()
{
  long zeit;

  ViewClear();
  szeneKoerper->Initialize();
  if(drawMode & ANIMATE)
  {
    zeit=GetTime()-startZeit;
    // Nicht zu oft zeichnen
    if (zeit-lastZeit < 5)
      {
	usleep((5-zeit+lastZeit)*10000); 
	zeit=GetTime()-startZeit;
      }
    szeneKoerper->Animate(lastZeit,zeit-lastZeit);
    lastZeit=zeit;
  }
  szeneKoerper->Errechnen(rotation,position);
  if(drawMode & WIRE)
  {
    szeneKoerper->DrawWire();
  }
  else
  {
    ZBufferClear();
    szeneKoerper->DrawSolid();
    if (drawMode & SHADOW)
    {
      szeneKoerper->DrawShadow();
    }
  }
  ViewSwop();
}

// Einfallswinkel des Lichtes festlegen
void SzeneSetLight(float x,float z)
{
  lichtKoerper->Rotiere( Matrix(x,0,0) );
  lichtKoerper->Rotiere( Matrix(0,0,z) );
}

// Vektor des Lichtes zurueckgeben
Vektor SzeneGetLight()
{
  return licht->Proj();
}

// Koerper suchen
KoerperPtr SzeneGetKoerper(char *name)
{
  return szeneKoerper->SucheKoerper(name);
}

// Szene Rotieren
void SzeneRotate(Matrix &m)
{
  rotation=rotation*m;
}

// Szene Verschieben
void SzeneMove(Vektor &v)
{
  position=position+v;
}

// Rotation setzen
void SzeneSetRot(Matrix &m)
{
  rotation=m;
}

// Position setzen
void SzeneSetPos(Vektor &v)
{
  position=v;
}

// MinMax Werte ermitteln
void SzeneGetMinMax(float &minX,float &maxX,float &minY,float &maxY,float &minZ,float &maxZ)
{
  minX=999999999;
  minY=999999999;
  minZ=999999999;
  maxX=-999999999;
  maxY=-999999999;
  maxZ=-999999999;
  szeneKoerper->Errechnen(rotation,position);
  szeneKoerper->GetMinMax(minX,maxX,minY,maxY,minZ,maxZ);
  return;
}


