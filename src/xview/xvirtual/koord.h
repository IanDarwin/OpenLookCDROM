#ifndef Koord_h
#define Koord_h 1

#ifndef List_h
#include "list.h"
#endif
#ifndef Vektor_h
#include "vektor.h"
#endif

class Koordinate:public Node<Koordinate>
{
 private:
  Vektor 		ursprung;
  Vektor 		projektion;

 public:
  Koordinate(Vektor &v=Vektor(0,0,0))
  {
    ursprung=v;
  }
  Vektor Urspr() {return ursprung;}
  Vektor Proj() {return projektion;}
  void Errechnen(Matrix &m,Vektor &v)
  {
    projektion = m * ursprung + v;
  }
  void Scale(float x,float y,float z)
  {
    ursprung.x*=x;
    ursprung.y*=y;
    ursprung.z*=z;
  }
  void Move(Vektor v)
  {
    ursprung=ursprung+v;
  }
};
typedef NodePtr<Koordinate> KoordinatenPtr;

#endif