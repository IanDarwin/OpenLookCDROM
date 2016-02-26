//  1993 by M. Roth

#ifndef Flaeche_h
#define Flaeche_h 1

#ifndef List_h
#include "list.h"
#endif
#ifndef Vektor_h
#include "vektor.h"
#endif
#ifndef Eckpunkt_h
#include "eckpunkt.h"
#endif

class Koerper;
class Flaeche:public Node<Flaeche>
{
 private:
  List<Eckpunkt>	eckpunktLst;
  NormVektor            *normVektor;
  Koerper               *koerper;
  struct
  {
    unsigned int sichtbar:1;
    unsigned int initialize:1;
    unsigned int shadow:1;
  } flag;
  int 			farbe;
  int Flaeche::RechneFlaechenVektor();

 public:
  Flaeche(Koerper *);
  ~Flaeche();
  void add(EckpunktPtr);
  void detach(EckpunktPtr);
  int  Initialize();
  void Errechnen();
  void DrawSolid();
  void DrawWire();
  void DrawShadow();
  void SearchNeighbor(int,int);
  void CopyNeighbor(Flaeche *);
  List<Eckpunkt> EckpunktLst() {return eckpunktLst;}
  NormVektor *NVekt() {return normVektor;}
  friend void FlaecheShadow(Flaeche *);
  Flaeche &operator=(Flaeche &f);
};
typedef NodePtr<Flaeche> FlaechenPtr;

#endif
