#ifndef Eckpunkt_h
#define Eckpunkt_h 1

#ifndef __IOSTREAM_H
#include <iostream.h>
#endif
#ifndef List_h
#include "list.h"
#endif
#ifndef Vektor_h
#include "vektor.h"
#endif
#ifndef Koord_h
#include "koord.h"
#endif
#ifndef NVekt_h
#include "nvekt.h"
#endif

class NormVektor;
class Flaeche;
class Eckpunkt:public Node<Eckpunkt>
{
 private:
  KoordinatenPtr	       koordinate;
  NormVektorPtr                normVektor;
  Flaeche                     *nachbarFlaeche;

 public:
  Eckpunkt(KoordinatenPtr k,NormVektorPtr nVekt=0)
  {
    koordinate=k;
    normVektor=nVekt;
    nachbarFlaeche=0;
  }
  NormVektorPtr NVekt() {return normVektor;}
  KoordinatenPtr Koord() {return koordinate;}
  Flaeche *NachbFlae() {return nachbarFlaeche;}
  void NachbFlae(Flaeche *f) {nachbarFlaeche=f;}
};
typedef NodePtr<Eckpunkt> EckpunktPtr;

#endif
