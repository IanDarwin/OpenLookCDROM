#ifndef NVekt_h
#define NVekt_h 1

#ifndef List_h
#include "list.h"
#endif
#ifndef Vektor_h
#include "vektor.h"
#endif

class NormVektor:public Node<NormVektor>
{
 private:
  Vektor 		ursprung;
  Vektor 		projektion;
  float			helligkeit;

 public:
  NormVektor(Vektor &v=Vektor(0,0,0))
  {
    ursprung=v;
  }
  void Errechnen(Matrix &);
  void Urspr(Vektor &v) {ursprung =v;}
  Vektor Urspr() {return ursprung;}
  Vektor Proj() {return projektion;}
  float Helligkeit()
  {
    return helligkeit;
  }
  friend ostream& operator<<(ostream&,NormVektor&);
};
typedef NodePtr<NormVektor> NormVektorPtr;
#endif