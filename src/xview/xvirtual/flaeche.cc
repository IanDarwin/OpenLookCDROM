//  1993 by M. Roth

#include "szene.h"
#include "view.h"
#include "shape.h"
#include "clipping.h"
#include "zbuffer.h"
#include "shadow.h"

#define shadMax 100
Flaeche *shadTab[shadMax];
int shadPut=0;
int shadGet=0;

void ShadPut(Flaeche *f)
{
  shadTab[shadPut]=f;
  shadPut++;
  if(shadPut==shadMax)
    shadPut=0;
}

int ShadGet(FlaechenPtr &f)
{
  if(shadPut==shadGet)
    return 0;
  f=shadTab[shadGet];
  shadGet++;
  if(shadGet==shadMax)
    shadGet=0;
  return 1;
}

// Neue Flaeche initialisieren
Flaeche::Flaeche(Koerper *k)
{
  koerper=k;
  normVektor=new NormVektor;
  koerper->add(normVektor);
  flag.initialize=1;
  farbe=ViewGetColor();
}

// Flaeche loeschen
Flaeche::~Flaeche()
{
  koerper->detach(normVektor);
  delete (NormVektor*)normVektor;
  eckpunktLst.Flush();
}

// Eckpunkt zur Flaeche hinzufgen
void Flaeche::add(EckpunktPtr e)
{
  eckpunktLst+=e;
  flag.initialize=1;
}

// Eckpunkt entfernen
void Flaeche::detach(EckpunktPtr p)
{
  eckpunktLst-=p;
  flag.initialize=1;
}

// Errechnen des Normalen-Vektors einer Flaeche
int Flaeche::RechneFlaechenVektor()
{
  Vektor vi,vj,v;
  float a,b,c;
  EckpunktPtr ep1;
  EckpunktPtr ep2;
  float laenge;

  a=b=c=0;
  ep1=eckpunktLst.First();
  ep2=eckpunktLst.Last();
  while(ep1)
  {
    vj=ep1->Koord()->Urspr();
    vi=ep2->Koord()->Urspr();
    ep2=ep1;
    a+= ( vi.y - vj.y ) * ( vi.z + vj.z );
    b+= ( vi.z - vj.z ) * ( vi.x + vj.x );
    c+= ( vi.x - vj.x ) * ( vi.y + vj.y );
    ep1++;
  }
  v=Vektor(a,b,c);
  laenge=sqrt(v*v);
  if (laenge==0) return 1;
  normVektor->Urspr(v/laenge);
  return 0;
}

// Flaechendaten initialisieren
int Flaeche::Initialize()
{
  int rc=0;

  if (flag.initialize)
  {
    rc=RechneFlaechenVektor();
    flag.initialize=0;
  }
  return rc;
}

// Flaechendaten errechnen
void Flaeche::Errechnen()
{
  // Sichtbarkeitstest
  if ( normVektor->Proj() * eckpunktLst.First()->Koord()->Proj() > 0)
    flag.sichtbar=0;
  else
    flag.sichtbar=1;
}

// Flaeche als Drahtmodell zeichnen
void Flaeche::DrawWire()
{
  EckpunktPtr e;
  Vektor k;
  ShapeParm sParm;
  int xv,yv,xb,yb,x,y;
  int f;

  // farbe ermitteln
  f=(farbe<<4)+15;
  // Flaeche zeichnen
  if (flag.sichtbar)
  {
    e=eckpunktLst.First();
    while(e)
    {
      k=e->Koord()->Proj();
      ClippingPut( k.x , k.y , k.z ,0);
      e++;
    }
    if (ClippingGet( sParm ))
    {
      x=xv=(int)sParm.x;
      y=yv=(int)sParm.z;
      while(ClippingGet( sParm ))
      {
	xb=(int)sParm.x;
	yb=(int)sParm.z;
	if (((xv!=xb) && (yv!=yb))  ||
	    ((xv==xb) && (xv!=0) && (xv!=ViewGetMaxX())) ||
	    ((yv==yb) && (yv!=0) && (yv!=ViewGetMaxY())))
	    ViewLine(xv,yv,xb,yb,f);
	xv=xb;
	yv=yb;
      }
      if (((xv!=x) && (yv!=y))  ||
	  ((xv==x) && (xv!=0) && (xv!=ViewGetMaxX())) ||
	  ((yv==y) && (yv!=0) && (yv!=ViewGetMaxY())))
	 ViewLine(xv,yv,x,y,f);
    }
  }
}

// Flaeche zeichnen
void Flaeche::DrawSolid()
{
  EckpunktPtr e;
  Vektor k;
  float x,y,z,h;
  ZBufferParm zbParm;
  ShapeParm sParm;

  // Shadowflag loeschen
  flag.shadow=0;
  // Flaeche zeichnen
  if (flag.sichtbar)
  {
    e=eckpunktLst.First();
    while(e)
    {
//      if (e->NVekt())
//	h=e->NVekt()->Helligkeit();
//      else
	h=normVektor->Helligkeit();
      k=e->Koord()->Proj();
      ClippingPut( k.x , k.y , k.z ,h);
      e++;
    }
    while(ClippingGet( sParm ))
      ShapePutPoint( sParm ); 
    while(ShapeGetLine( zbParm ))
      while(ZBufferSolidTest( zbParm ))
	ViewShadeLine(zbParm.y,   
		      zbParm.laenge,
		      zbParm.vonX,
		      zbParm.vonH,
		      zbParm.hp,
		      farbe);             
  }
}

// Schattentest fuer Schattenk”rperseiten
void FlaecheShadowTest()
{
  ZBufferParm zbParm;
  ShapeParm sParm;

  while(ClippingGet( sParm ))
  ShapePutPoint( sParm );
  while(ShapeGetLine( zbParm ))
  {
    while(ZBufferShadowTest( zbParm ))
    {
      ShadowPut( zbParm.vonX , zbParm.y );
      ShadowPut( zbParm.vonX+zbParm.laenge , zbParm.y );
    }
  }
}

// Flaechenschatten berechnen
void FlaecheShadow(Flaeche *flaeche)
{
  EckpunktPtr e1,e2;
  Vektor vek1,vek2,vekLen;
  ZBufferParm zbParm;
  ShapeParm sParm;
  Flaeche *nachbar;

  // Schattenflag setzen
  flaeche->flag.shadow=1;
  // L„nge des Schattenk”rpers
  vekLen=SzeneGetLight()*-2000000000;
  // Oberseite des Schattenk”rpers
  e1=flaeche->eckpunktLst.First();
  while(e1)
  {
    vek1=e1->Koord()->Proj();
    ClippingPut( vek1.x , vek1.y , vek1.z ,0);
    e1++;
  }
  // Schatten von Hauptflaeche liegt immer im Schattenk”rper
 while(ClippingGet( sParm ))
    ShapePutPoint( sParm );
  while(ShapeGetLine( zbParm ))
  {
    if ( flaeche->normVektor->Helligkeit()>0 )
    {
      ShadowPut( zbParm.xv , zbParm.y );
      ShadowPut( zbParm.xb , zbParm.y );
    }
    if(flaeche->flag.sichtbar)
    {
      while(ZBufferSolidTest( zbParm ))
      {
	ShadowPut( zbParm.vonX , zbParm.y,1 );
	ShadowPut( zbParm.vonX+zbParm.laenge , zbParm.y,1 );
      }
    }
  }
  if ( flaeche->normVektor->Helligkeit()>0 )
  {
    // L„nge des Schattenk”rpers
    vekLen=SzeneGetLight()*-2000000000;
    // Unterseite des Schattenk”rpers
    e1=flaeche->eckpunktLst.First();
    while(e1)
    {
      vek1=e1->Koord()->Proj() + vekLen;
      ClippingPut( vek1.x , vek1.y , vek1.z ,1);
      e1++;
    }
    FlaecheShadowTest();
  }
  // Seitenteile des Schattenk”rpers
  e1=flaeche->eckpunktLst.Last();
  e2=flaeche->eckpunktLst.First();
  while(e2)
  {
    nachbar=e1->NachbFlae();
    if (nachbar && nachbar->flag.shadow==0)
    {
      nachbar->flag.shadow=1;
      ShadPut(nachbar);
    }
    if ( flaeche->normVektor->Helligkeit()>0 )
    {
      if (!(nachbar) || nachbar->normVektor->Helligkeit()<=0)
      {
	vek1=e1->Koord()->Proj();
	ClippingPut( vek1.x , vek1.y , vek1.z ,1);
	vek2=e2->Koord()->Proj();
	ClippingPut( vek2.x , vek2.y , vek2.z ,1);
	vek1=vek1+vekLen;
	vek2=vek2+vekLen;
	ClippingPut( vek2.x , vek2.y , vek2.z ,1);
	ClippingPut( vek1.x , vek1.y , vek1.z ,1);
	FlaecheShadowTest();
      }
    }
    e1=e2;
    e2++;
  } 
}

// Flaechenschatten berechnen
void Flaeche::DrawShadow()
{
  int y,xVon,laenge;
  FlaechenPtr flaeche;

  // Pruefen, ob bereits verarbeitet
  if(flag.shadow)
    return;
  // Schattenflag setzen
  flag.shadow=1;
  // Nur Schatten berechnen, wenn Licht auf die Fl„che F„llt
  ShadPut(this);
  while(ShadGet(flaeche))
    FlaecheShadow(flaeche);
  while(ShadowGet( y,xVon,laenge ))
    ViewShadowLine(y,xVon,laenge);
}

// Nachbarfl„chen zuordnen
void Flaeche::SearchNeighbor(int von,int bis)
{
  EckpunktPtr e1,e2,e3,e4;
  KoordinatenPtr k1,k2;
  FlaechenPtr f,fend;

  fend=koerper->FlaechenLst()[bis];
  e1=eckpunktLst.Last();
  e2=eckpunktLst.First();
  while(e2)
  {
    k1=e1->Koord();
    k2=e2->Koord();
    if (!(e1->NachbFlae()))
    {
      f=koerper->FlaechenLst()[von];
      while(f)
      {
	e3=f->eckpunktLst.Last();
	e4=f->eckpunktLst.First();
	while(e4)
	{
	  if ((k1 == e4->Koord()) &&
	      (k2 == e3->Koord()) )
	  {
	    e1->NachbFlae(f);
	    e3->NachbFlae(this);
	    break;
	  }
	  e3=e4;
	  e4++;
	}
	if (e4) break;
	if (f==fend)
	  f=0;
	else
	  f++;
      }
    }
    e1=e2;
    e2++;
  }
}

// Flaeche kopieren
Flaeche &Flaeche::operator=(Flaeche &f)
{
  EckpunktPtr		eckpPtr,eckp;
  KoordinatenPtr        koord;
  NormVektorPtr         nvekt;
  int 			n;

  flag	= f.flag;
  farbe	= f.farbe;
  n=f.koerper->NormVektorLst().Number(f.normVektor);
  normVektor=koerper->NormVektorLst()[n];
  eckpPtr = f.eckpunktLst.First();
  while(eckpPtr)
  {
    n=f.koerper->KoordinatenLst().Number(eckpPtr->Koord());
    koord=koerper->KoordinatenLst()[n];
    if( eckpPtr->NVekt() )
    {
      n=f.koerper->NormVektorLst().Number(eckpPtr->NVekt());
      nvekt=koerper->NormVektorLst()[n];
      eckp=new Eckpunkt(koord,nvekt);
    }
    else
      eckp=new Eckpunkt(koord);
    eckpunktLst+=eckp;
    eckpPtr++;
  }
  return f;
}

// Nachbarreferenzen kopieren
void Flaeche::CopyNeighbor(Flaeche *f)
{
  EckpunktPtr 		eckpPtr,eckp;
  Flaeche		*nachbFlae;
  int			n;

  eckpPtr = f->eckpunktLst.First();
  eckp    =    eckpunktLst.First();
  while(eckpPtr)
  {
    n=f->koerper->FlaechenLst().Number(eckpPtr->NachbFlae());
    nachbFlae=koerper->FlaechenLst()[n];
    eckp->NachbFlae(nachbFlae);
    eckp++;
    eckpPtr++;
  }
}
