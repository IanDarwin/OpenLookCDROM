//  1993 by M. Roth

#include "koerper.h"

// Konstruktor
Koerper::Koerper(char *n)
{
  if(n)
    strcpy(name,n);
  else
    name[0]=0;
  rotation=Matrix(0,0,0);
  position=Vektor(0,0,0);
  flag.show=1;
  flag.initialize=1;
  flag.move=0;
  flag.rotate=0;
  flag.centre=0;
}

// Destruktor
Koerper::~Koerper()
{
  koerperLst.Flush();
  flaechenLst.Flush();
  koordinatenLst.Flush();
  normVektorLst.Flush();
  animationLst.Flush();
}

// Koerper mit einem bestimmten Namen suchen
KoerperPtr Koerper::SucheKoerper(char *such)
{
  KoerperPtr 		k=0;
  KoerperPtr 		koer=0;
  KoerperPtr 		koerPtr;

  if (strcmp(such,name)==0) return this;
  koerPtr=koerperLst.First();
  while(koerPtr)
  {
    k=koerPtr->SucheKoerper(such);
    if(k) koer=k;
    koerPtr++;
  }
  return koer;
}

// Alle Daten berechnen, die zum zeichnen ben”tigt werden.
void Koerper::Errechnen(Matrix m,Vektor v)
{
  KoordinatenPtr koordPtr;
  NormVektorPtr nvektPtr;
  FlaechenPtr flaePtr;
  KoerperPtr koerPtr;
  Matrix mneu;
  Vektor vneu;

  // Nur berechnen, wenn der Koerper auch angezeigt werden soll
  if (flag.show==0) return;
  if (flag.rotate)
    mneu= m * rotation;
  else
    mneu= m;
  if (flag.move)
    vneu= m * position + v;
  else
    vneu = v;
  if (flag.centre)
    vneu= vneu + m*centre - mneu*centre;
  koordPtr=koordinatenLst.First();
  while(koordPtr)
  {
    koordPtr->Errechnen(mneu,vneu);
    ++koordPtr;
  }
  nvektPtr=normVektorLst.First();
  while(nvektPtr)
  {
    nvektPtr->Errechnen(mneu);
    ++nvektPtr;
  }
  flaePtr=flaechenLst.First();
  while(flaePtr)
  {
    flaePtr->Errechnen();
    ++flaePtr;
  }
  koerPtr=koerperLst.First();
  while(koerPtr)
  {
    koerPtr->Errechnen(mneu,vneu);
    ++koerPtr;
  }
}

// Initialdaten errechnen
void Koerper::Initialize()
{
  FlaechenPtr flaePtr;
  KoerperPtr koerPtr;

  if (flag.initialize)
  {
    flaePtr=flaechenLst.First();
    while(flaePtr)
    {
      flaePtr->Initialize();
      ++flaePtr;
    }
    koerPtr=koerperLst.First();
    while(koerPtr)
    {
      koerPtr->Initialize();
      ++koerPtr;
    }
    flag.initialize=0;
  }
}

// Koerper animieren
void Koerper::Animate(float time,float sec)
{
  AnimationPtr animPtr;
  KoerperPtr koerPtr;
  double t=time;
  double s=sec;

  AnimateList(animationLst,t,s,0);
  koerPtr=koerperLst.First();
  while(koerPtr)
  {
    koerPtr->Animate(time,sec);
    ++koerPtr;
  }
}

// Koerper skalieren
void Koerper::Scale(float x,float y,float z)
{
  KoordinatenPtr koordPtr;
  KoerperPtr koerPtr;

  koordPtr=koordinatenLst.First();
  while(koordPtr)
  {
    koordPtr->Scale(x,y,z);
    koordPtr++;
  }
  koerPtr=koerperLst.First();
  while(koerPtr)
  {
    koerPtr->Scale(x,y,z);
    koerPtr->centre.x*=x;
    koerPtr->centre.y*=y;
    koerPtr->centre.z*=z;
    koerPtr->position.x*=x;
    koerPtr->position.y*=y;
    koerPtr->position.z*=z;
    ++koerPtr;
  }
}

// Koerper zentrieren
void Koerper::Centre(Vektor &v)
{
  KoerperPtr koerPtr;
  KoordinatenPtr koordPtr;

  centre=v;
  flag.centre=1;
}

// Koerper rotieren
void Koerper::Rotiere(Matrix &m)
{
  rotation=m*rotation;
  flag.rotate=1;
}

// Koerper verschieben
void Koerper::Verschiebe(Vektor &v)
{
  position=position+v;
  flag.move=1;
}

// Koerper zeichnen
void Koerper::DrawWire()
{
  FlaechenPtr flaePtr;
  KoerperPtr koerPtr;

  // Nur Zeichnen, wenn der Koerper auch angezeigt werden soll
  if (flag.show==0) return;
  flaePtr=flaechenLst.First();
  while(flaePtr)
  {
    flaePtr->DrawWire();
    ++flaePtr;
  }
  koerPtr=koerperLst.First();
  while(koerPtr)
  {
    koerPtr->DrawWire();
    ++koerPtr;
  }
}

// Koerper zeichnen
void Koerper::DrawSolid()
{
  FlaechenPtr flaePtr;
  KoerperPtr koerPtr;

  // Nur Zeichnen, wenn der K”rper auch angezeigt werden soll
  if (flag.show==0) return;
  flaePtr=flaechenLst.First();
  while(flaePtr)
  {
    flaePtr->DrawSolid();
    ++flaePtr;
  }
  koerPtr=koerperLst.First();
  while(koerPtr)
  {
    koerPtr->DrawSolid();
    ++koerPtr;
  }
}

// Koerper zeichnen
void Koerper::DrawShadow()
{
  FlaechenPtr flaePtr;
  KoerperPtr koerPtr;

  // Nur Zeichnen, wenn der K”rper auch angezeigt werden soll
  if (flag.show==0) return;
  flaePtr=flaechenLst.First();
  while(flaePtr)
  {
    flaePtr->DrawShadow();
    ++flaePtr;
  }
  koerPtr=koerperLst.First();
  while(koerPtr)
  {
    koerPtr->DrawShadow();
    ++koerPtr;
  }
}

// Nachbarflaechen zuordnen
void Koerper::SearchNeighbor(int von,int bis)
{
  FlaechenPtr flaePtr,flaeEndPtr;

  flaeEndPtr=flaechenLst[bis];
  flaePtr=flaechenLst[von];
  while(flaePtr)
  {
    flaePtr->SearchNeighbor(von,bis);
    if (flaePtr==flaeEndPtr)
      flaePtr=0;
    else
      ++flaePtr;
  }
}

// Koerper kopieren
Koerper &Koerper::operator=(Koerper &k)
{
  KoerperPtr 		koerPtr,  koer;
  KoordinatenPtr 	koordPtr, koord;
  NormVektorPtr 	nvektPtr, nvekt;
  FlaechenPtr 		flaePtr,  flae;
  AnimationPtr 		animPtr,  anim;

  centre = k.centre;
  rotation = k.rotation;
  position = k.position;
  strcpy(name,k.name);
  flag = k.flag;
  koerPtr = k.koerperLst.First();
  while(koerPtr)
  {
     koer=new Koerper;
     *koer=*koerPtr;
     koerperLst+=koer;
     koerPtr++;
  }
  koordPtr = k.koordinatenLst.First();
  while(koordPtr)
  {
     koord=new Koordinate;
     *koord=*koordPtr;
     koordinatenLst+=koord;
     koordPtr++;
  }
  nvektPtr = k.normVektorLst.First();
  while(nvektPtr)
  {
     nvekt=new NormVektor;
     *nvekt=*nvektPtr;
     normVektorLst+=nvekt;
     nvektPtr++;
  }
  // Alle Fl„chen anlegen
  flaePtr = k.flaechenLst.First();
  while(flaePtr)
  {
     flae=new Flaeche(this);
     *flae=*flaePtr;
     flaechenLst+=flae;
     flaePtr++;
  }
  // Nachbarreferenzen kopieren
  flaePtr = k.flaechenLst.First();
  flae    = flaechenLst.First();
  while(flaePtr)
  {
    flae->CopyNeighbor(flaePtr);
    flaePtr++;
    flae++;
  }
  animPtr = k.animationLst.First();
  while(animPtr)
  {
     anim=new Animation(0,0,0,this);
     *anim=*animPtr;
     animationLst+=anim;
     animPtr++;
  }
  return (Koerper &) k;
}

// Startzeit fuer eine animation setzen
void Koerper::AnimSetStartTime(float time)
{
  KoerperPtr 		koerPtr;

  if (animationLst.First())
    animationLst.First()->StartTime(time);
  koerPtr=koerperLst.First();
  while(koerPtr)
  {
    koerPtr->AnimSetStartTime(time);
    ++koerPtr;
  }
}

// MinMax Werte ermitteln
void Koerper::GetMinMax(float &minX,float &maxX,float &minY,float &maxY,float &minZ,float &maxZ)
{
  KoordinatenPtr	koordPtr;
  KoerperPtr		koerPtr;

  koordPtr = koordinatenLst.First();
  while(koordPtr)
  {
    if (koordPtr->Proj().x < minX) minX = koordPtr->Proj().x;
    if (koordPtr->Proj().y < minY) minY = koordPtr->Proj().y;
    if (koordPtr->Proj().z < minZ) minZ = koordPtr->Proj().z;
    if (koordPtr->Proj().x > maxX) maxX = koordPtr->Proj().x;
    if (koordPtr->Proj().y > maxY) maxY = koordPtr->Proj().y;
    if (koordPtr->Proj().z > maxZ) maxZ = koordPtr->Proj().z;
    koordPtr++;
  }
  koerPtr=koerperLst.First();
  while(koerPtr)
  {
    koerPtr->GetMinMax(minX,maxX,minY,maxY,minZ,maxZ);
    ++koerPtr;
  }
  return;
}
