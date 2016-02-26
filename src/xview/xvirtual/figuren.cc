//  1994 by M. Roth
#include <math.h>

#include "szene.h"
#include "figuren.h"
#include "view.h"

#define FIGUR_ARC 1

// Quader
KoerperPtr Quader(char *name,float x,float y,float z)
{
  KoerperPtr koerper;
  KoordinatenPtr k1,k2,k3,k4,k5,k6,k7,k8;
  FlaechenPtr f;

  koerper=new Koerper(name);
  k1=new Koordinate(Vektor( 0.5*x, 0.5*y,-0.5*z ));
  k2=new Koordinate(Vektor( 0.5*x,-0.5*y,-0.5*z ));
  k3=new Koordinate(Vektor(-0.5*x,-0.5*y,-0.5*z ));
  k4=new Koordinate(Vektor(-0.5*x, 0.5*y,-0.5*z ));
  k5=new Koordinate(Vektor( 0.5*x, 0.5*y, 0.5*z ));
  k6=new Koordinate(Vektor( 0.5*x,-0.5*y, 0.5*z ));
  k7=new Koordinate(Vektor(-0.5*x,-0.5*y, 0.5*z ));
  k8=new Koordinate(Vektor(-0.5*x, 0.5*y, 0.5*z ));
  koerper->add(k1);
  koerper->add(k2);
  koerper->add(k3);
  koerper->add(k4);
  koerper->add(k5);
  koerper->add(k6);
  koerper->add(k7);
  koerper->add(k8);

  f=new Flaeche(koerper);
  f->add(new Eckpunkt(k1));
  f->add(new Eckpunkt(k2));
  f->add(new Eckpunkt(k3));
  f->add(new Eckpunkt(k4));
  koerper->add(f);
  f=new Flaeche(koerper);
  f->add(new Eckpunkt(k2));
  f->add(new Eckpunkt(k1));
  f->add(new Eckpunkt(k5));
  f->add(new Eckpunkt(k6));
  koerper->add(f);
  f=new Flaeche(koerper);
  f->add(new Eckpunkt(k8));
  f->add(new Eckpunkt(k7));
  f->add(new Eckpunkt(k6));
  f->add(new Eckpunkt(k5));
  koerper->add(f);
  f=new Flaeche(koerper);
  f->add(new Eckpunkt(k8));
  f->add(new Eckpunkt(k4));
  f->add(new Eckpunkt(k3));
  f->add(new Eckpunkt(k7));
  koerper->add(f);
  f=new Flaeche(koerper);
  f->add(new Eckpunkt(k3));
  f->add(new Eckpunkt(k2));
  f->add(new Eckpunkt(k6));
  f->add(new Eckpunkt(k7));
  koerper->add(f);
  f=new Flaeche(koerper);
  f->add(new Eckpunkt(k4));
  f->add(new Eckpunkt(k8));
  f->add(new Eckpunkt(k5));
  f->add(new Eckpunkt(k1));
  koerper->add(f);
  koerper->SearchNeighbor(0,7);
  return koerper;
}

// Pyramide
KoerperPtr Pyramide(char *name,float breite,float hoehe)
{
  KoerperPtr koerper;
  KoordinatenPtr k1,k2,k3,k4,k5;
  FlaechenPtr f;

  koerper=new Koerper(name);
  k1=new Koordinate(Vektor( 0.5*breite, 0.5*breite,0 ));
  k2=new Koordinate(Vektor( 0.5*breite,-0.5*breite,0 ));
  k3=new Koordinate(Vektor(-0.5*breite,-0.5*breite,0 ));
  k4=new Koordinate(Vektor(-0.5*breite, 0.5*breite,0 ));
  k5=new Koordinate(Vektor( 0, 0,hoehe ));
  koerper->add(k1);
  koerper->add(k2);
  koerper->add(k3);
  koerper->add(k4);
  koerper->add(k5);
  f=new Flaeche(koerper);
  f->add(new Eckpunkt(k5));
  f->add(new Eckpunkt(k2));
  f->add(new Eckpunkt(k1));
  koerper->add(f);
  f=new Flaeche(koerper);
  f->add(new Eckpunkt(k5));
  f->add(new Eckpunkt(k3));
  f->add(new Eckpunkt(k2));
  koerper->add(f);
  f=new Flaeche(koerper);
  f->add(new Eckpunkt(k5));
  f->add(new Eckpunkt(k4));
  f->add(new Eckpunkt(k3));
  koerper->add(f);
  f=new Flaeche(koerper);
  f->add(new Eckpunkt(k5));
  f->add(new Eckpunkt(k1));
  f->add(new Eckpunkt(k4));
  koerper->add(f);
  f=new Flaeche(koerper);
  f->add(new Eckpunkt(k1));
  f->add(new Eckpunkt(k2));
  f->add(new Eckpunkt(k3));
  f->add(new Eckpunkt(k4));
  koerper->add(f);
  koerper->SearchNeighbor(0,5);
  return koerper;
}

// Stern
KoerperPtr Stern(char *name,float b,float h)
{
  KoerperPtr     stern;
  KoerperPtr     spitze;

  stern=new Koerper(name);
  spitze=Pyramide("s1",b,h);spitze->Verschiebe(Vektor(0,0,b/2));
  stern->add(spitze);
  spitze=Pyramide("s2",b,h);spitze->Verschiebe(Vektor(b/2,0,0));spitze->Rotiere(Matrix(0,90,0));
  stern->add(spitze);
  spitze=Pyramide("s3",b,h);spitze->Verschiebe(Vektor(0,0,-b/2));;spitze->Rotiere(Matrix(0,180,0));
  stern->add(spitze);
  spitze=Pyramide("s4",b,h);spitze->Verschiebe(Vektor(-b/2,0,0));;spitze->Rotiere(Matrix(0,270,0));
  stern->add(spitze);
  spitze=Pyramide("s5",b,h);spitze->Verschiebe(Vektor(0,b/2,0));;spitze->Rotiere(Matrix(270,0,0));
  stern->add(spitze);
  spitze=Pyramide("s6",b,h);spitze->Verschiebe(Vektor(0,-b/2,0));;spitze->Rotiere(Matrix(90,0,0));
  stern->add(spitze);
  return stern;
}

int figPointMax=-1;
int figClosed=0;
struct
{
  Vektor point;
  int farbe;
  Vektor normVektor;
  int normVektorFlag;
} figPoint[50];

// Punkt fÅr Rotationskîrper hinzufÅgen
void FigurPointAdd(float x,float y,Vektor *normVektor)
{
  figPointMax++;
  figPoint[ figPointMax ].point = Vektor(x,0,y);
  figPoint[ figPointMax ].farbe = ViewGetColor();
  if (normVektor)
  {
    figPoint[ figPointMax ].normVektor = *normVektor;
    figPoint[ figPointMax ].normVektorFlag = 1;
  }
  else
    figPoint[ figPointMax ].normVektorFlag = 0;
}

// Pruefen, ob ein Punkt ueber oder unter einer Linie liegt
int LineTest(float xv,float yv,float xb,float yb,float x,float y,int &drunter)
{
  float a=xb-xv;
  float b=yb-yv;
  float c;
  int r,d;

  if (yv < yb)
  {
    drunter=0;
    return 1;
  }
  c = b*(x-xv)-a*(y-yv);
  if (c<0)
    d=1;
  else
    d=2;
  if ((drunter!=0) && (d!=drunter))
  {
    drunter=0;
    return 1;
  }
  else
  {
    drunter=d;
    return 0;
  }
}

// NachbarflÑchen zuordnen
void RotSearchNeighbor(Koerper *koerper,int segmente)
{
  int c;
  int start=0;
  int ende;
  int drunter=0;

  if ( figPoint[0].point.x != 0)
  {
    if (LineTest(0                   , figPoint[0].point.z ,
		 figPoint[0].point.x , figPoint[0].point.z ,
		 figPoint[1].point.x , figPoint[1].point.z ,
		 drunter))
    {
      start=1;
    }
  }
  for(c=1 ; c<figPointMax ; c++)
  {
    if ( LineTest( figPoint[c-1].point.x , figPoint[c-1].point.z ,
		   figPoint[c].point.x   , figPoint[c].point.z   ,
		   figPoint[c+1].point.x , figPoint[c+1].point.z ,
		   drunter))
    {
      ende=c*segmente-1;
      if((figPoint[0].point.x!=0) && (!(figClosed)))
	 ende++;
      if(start<ende)
	 koerper->SearchNeighbor(start,ende);
      start=ende+1;
    }
  }
  if(figPoint[figPointMax].point.x!=0)
  {
    if ( LineTest( figPoint[figPointMax-1].point.x, figPoint[figPointMax-1].point.z ,
		   figPoint[figPointMax].point.x  , figPoint[figPointMax].point.z   ,
		   0                              , figPoint[figPointMax].point.z ,
		   drunter))
    {
      ende=figPointMax*segmente-1;
      if((figPoint[0].point.x!=0) && (!(figClosed)))
	 ende++;
      if(start<ende)
	 koerper->SearchNeighbor(start,ende);
      start=ende+1;
    }
   }
   ende=figPointMax*segmente-1;
   if (!(figClosed))
   {
     if(figPoint[0].point.x          !=0) ende++;
     if(figPoint[figPointMax].point.x!=0) ende++;
   }
   if(start<ende)
     koerper->SearchNeighbor(start,ende);
}

// Rotationskîrper erstellen
KoerperPtr RotKoerper(char *name,int segmente)
{
  Matrix m=Matrix(0.0,0.0,360.0/segmente);
  Matrix mHalb=Matrix(0.0,0.0,-180.0/segmente);
  KoerperPtr koerper;
  List<Koordinate> koordLst;
  List<Eckpunkt> eckptLst;
  List<NormVektor> nVektLst;
  FlaechenPtr flaeche;
  int a,b,c,d,e;
  int farbe;
  int zeilen;

  koerper=new Koerper(name);
  // Pruefen, ob der Linienzug geschlossen ist
  if ((fabs(figPoint[0].point.x - figPoint[figPointMax].point.x) < 0.01) &&
      (fabs(figPoint[0].point.y - figPoint[figPointMax].point.y) < 0.01) &&
      (fabs(figPoint[0].point.z - figPoint[figPointMax].point.z) < 0.01))
  {
    figClosed=1;
    figPointMax--;
  }
  else
  {
    figClosed=0;
  }
  // Aktuelle Farbe retten
  farbe=ViewGetColor();
  // Alle Koordinaten errechnen und zum Kîrper hinzufÅgen
  for(a=0 ; a<=figPointMax ; a++)
  {
    for(b=0 ; b<segmente ; b++)
    {
      koerper->add( new Koordinate( figPoint[ a ].point ));
      koerper->add( new NormVektor( figPoint[ a ].normVektor) );
      figPoint[ a ].normVektor = m * figPoint[ a ].normVektor;
      figPoint[ a ].point      = m * figPoint[ a ].point;
    }
  }
  // RotationsflÑchen erstellen
  koordLst=koerper->KoordinatenLst();
  nVektLst=koerper->NormVektorLst();
  // Deckel erstellen
  if ((figPoint[0].point.x != 0) && (!(figClosed)))
  {
    ViewSetColor(figPoint[ 0 ].farbe);
    flaeche=new Flaeche(koerper);
    koerper->add(flaeche);
    for(a=0 ; a<segmente ; a++)
      flaeche->add(new Eckpunkt( koordLst[a] ));
  }
  // SeitenflÑchen erstellen
  if (figClosed)
    zeilen=figPointMax+1;
  else
    zeilen=figPointMax;
  for(a=0 ; a<zeilen ; a++)
  {
    ViewSetColor(figPoint[ a ].farbe);
    for(b=0 ; b<segmente ; b++)
    {
      flaeche=new Flaeche(koerper);
      koerper->add(flaeche);
      // Linke Seite von Unten nach Oben
      c=a*segmente+b;
      if (a==figPointMax)
	e=b;
      else
	e=c+segmente;
      d=c;
      if (figPoint[a].normVektorFlag==1) d=e;
      flaeche->add(new Eckpunkt( koordLst[c],nVektLst[c] ));
      flaeche->add(new Eckpunkt( koordLst[e],nVektLst[d] ));
      // Rechte Seite von Oben nach Unten
      c=a*segmente+b+1;
      if (a==figPointMax)
	e=b+1;
      else
	e=c+segmente;
      if((b+1) == segmente)
      {
	c-=segmente;
	e-=segmente;
      }
      d=c;
      if (figPoint[a+1].normVektorFlag==1) d=e;
      flaeche->add(new Eckpunkt( koordLst[e],nVektLst[d] ));
      flaeche->add(new Eckpunkt( koordLst[c],nVektLst[c] ));
      flaeche->Initialize();
      // Wenn nicht vorhanden, dann Kantenvektoren berechnen
      if (figPoint[a].normVektorFlag==0)
	nVektLst[a*segmente+b]->Urspr( mHalb * flaeche->NVekt()->Urspr() );
    }
  }
  // Farbe wieder zurÅcksetzen
  ViewSetColor(farbe);
  // Boden erstellen
  if ((figPoint[figPointMax].point.x != 0) && (!(figClosed)))
  {
    flaeche=new Flaeche(koerper);
    koerper->add(flaeche);
    for(a=segmente-1 ; a>=0 ; a--)
      flaeche->add(new Eckpunkt( koordLst[figPointMax*segmente+a] ));
  }
  RotSearchNeighbor(koerper,segmente);
  figPointMax=-1;
  return koerper;
}

// Rotationsbogen erstellen
void RotArc(float x,float y,float w,float s)
{
  Vektor point;
  Vektor normVektor;
  Matrix rotMat=Matrix(0,w/s,0);
  int c;

  normVektor=figPoint[ figPointMax ].point - Vektor(x,0,y);
  normVektor=normVektor/(sqrt(normVektor*normVektor));
  if (w<0)
    normVektor=normVektor*-1;
  point=figPoint[ figPointMax ].point-Vektor(x,0,y);
  figPoint[ figPointMax ].normVektor = normVektor;
  figPoint[ figPointMax ].normVektorFlag = 1;
  figPoint[ figPointMax ].normVektor = normVektor;
  for(c=1 ; c<=s ; c++)
  {
    point=rotMat*point;
    normVektor=rotMat*normVektor;
    FigurPointAdd(point.x+x,point.z+y,&normVektor);
  }
}

// Sweep-Kloerper erstellen
KoerperPtr SweepKoerper(char *name,float hoehe)
{
  KoerperPtr 		koerper;
  List<Koordinate> 	koordLst;
  List<Eckpunkt> 	eckptLst;
  List<NormVektor> 	nVektLst;
  FlaechenPtr 		flaeche;
  int 			farbe;
  int 			zeilen;
  int 			a,b,c;
  NormVektorPtr		nv1,nv2;
  int			nVektCount=0;

  koerper=new Koerper(name);
  // Aktuelle Farbe retten
  farbe=ViewGetColor();
  // Pruefen, ob der Linienzug nicht geschlossen ist
  if ((fabs(figPoint[0].point.x - figPoint[figPointMax].point.x) < 0.01) &&
      (fabs(figPoint[0].point.y - figPoint[figPointMax].point.y) < 0.01) &&
      (fabs(figPoint[0].point.z - figPoint[figPointMax].point.z) < 0.01))
  {
    figClosed=1;
    figPointMax--;
  }
  else
  {
    figClosed=0;
  }
  // Alle Koordinaten errechnen und zum Kîrper hinzufÅgen
  for(a=0 ; a<=figPointMax ; a++)
    koerper->add( new Koordinate( figPoint[ a ].point + Vektor(0,hoehe/2,0)));
  for(a=0 ; a<=figPointMax ; a++)
    koerper->add( new Koordinate( figPoint[ a ].point - Vektor(0,hoehe/2,0)));
  for(a=0 ; a<=figPointMax ; a++)
  {
    if (figPoint[a].normVektorFlag)
      koerper->add( new NormVektor( figPoint[ a ].normVektor ));
  }
  koordLst=koerper->KoordinatenLst();
  nVektLst=koerper->NormVektorLst();
  // Deckel erstellen. Farbe = aktuelle Farbe
  flaeche=new Flaeche(koerper);
  koerper->add(flaeche);
  for(a=0 ; a<=figPointMax ; a++)
    flaeche->add(new Eckpunkt( koordLst[a] ));
  // Boden erstellen. Farbe = aktuelle Farbe
  flaeche=new Flaeche(koerper);
  koerper->add(flaeche);
  for(a=0 ; a<=figPointMax ; a++)
    flaeche->add(new Eckpunkt( koordLst[(figPointMax+1)+figPointMax-a] ));
  // SeitenflÑchen erstellen
  for(a=0 ; a<=figPointMax ; a++)
  {
    ViewSetColor(figPoint[ a ].farbe);
    b=a+1;
    if(b>figPointMax) b=0;
    flaeche=new Flaeche(koerper);
    koerper->add(flaeche);
    if (((a!=figPointMax) || (figClosed!=0)) &&
	((figPoint[a].normVektorFlag) && (figPoint[a].normVektorFlag)))
    {
      nv1=nVektLst[nVektCount++];
      if (a==figPointMax)
	nv2=nVektLst[0];
      else
	nv2=nVektLst[nVektCount];
    }
    else
    {
      nv1=0;
      nv2=0;
    }
    flaeche->add(new Eckpunkt( koordLst[b] , nv2 ));
    flaeche->add(new Eckpunkt( koordLst[a] , nv1 ));
    flaeche->add(new Eckpunkt( koordLst[a+figPointMax+1] , nv1 ));
    flaeche->add(new Eckpunkt( koordLst[b+figPointMax+1] , nv2 ));
  }
  figPointMax=-1;
  // Farbe wieder zurÅcksetzen
  ViewSetColor(farbe);
  return koerper;
}

