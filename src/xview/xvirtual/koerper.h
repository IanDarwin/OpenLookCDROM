#ifndef Koerper_h
#define Koerper_h 1

#ifndef List_h
#include "list.h"
#endif
#ifndef Vektor_h
#include "vektor.h"
#endif
#ifndef Koord_h
#include "koord.h"
#endif
#ifndef Flaeche_h
#include "flaeche.h"
#endif
#ifndef Animate_h
#include "animate.h"
#endif
#include <string.h>

class Koerper:public Node<Koerper>
{
 private:
  char 			name[15];
  List<Koerper>		koerperLst;
  List<Koordinate>	koordinatenLst;
  List<NormVektor>      normVektorLst;
  List<Flaeche>		flaechenLst;
  List<Animation>       animationLst;
  Matrix 		rotation;
  Vektor 		position;
  Vektor		centre;
  struct
  {
    int show:1;
    int initialize:1;
    int move:1;
    int rotate:1;
    int centre:1;
  } flag;

 public:
  Koerper(char *n=0);
  ~Koerper();
  void add(NodePtr<Koerper> p) {koerperLst+=p;}
  void add(NodePtr<Koordinate> p) {koordinatenLst+=p;}
  void add(NodePtr<Flaeche> f) {flaechenLst+=f;}
  void add(NodePtr<NormVektor> n) {normVektorLst+=n;}
  void add(NodePtr<Animation> p) {animationLst+=p;}
  void detach(NodePtr<Koerper> p) {koerperLst-=p;}
  void detach(NodePtr<Koordinate> p) {koordinatenLst-=p;}
  void detach(NodePtr<Flaeche> p) {flaechenLst-=p;}
  void detach(NodePtr<NormVektor> p) {normVektorLst-=p;}
  void detach(NodePtr<Animation> p) {animationLst-=p;}
  void Rotiere(Matrix &m);
  void Verschiebe(Vektor &v);
  void Scale(float x,float y,float z);
  void Centre(Vektor &v);
  List<Koordinate> &KoordinatenLst() { return koordinatenLst; }
  List<Flaeche> &FlaechenLst() { return flaechenLst; }
  List<Koerper> &KoerperLst() { return koerperLst; }
  List<NormVektor> &NormVektorLst() { return normVektorLst; }
  void Initialize();
  void Errechnen(Matrix,Vektor);
  void Animate(float time,float sec);
  void DrawWire();
  void DrawSolid();
  void DrawShadow();
  void SearchNeighbor(int von,int bis);
  NodePtr<Koordinate> SucheKoordinate(long);
  NodePtr<Koerper> SucheKoerper(char *);
  Koerper &operator=(Koerper &k);
  void Rename(char *n) {strcpy(name,n);}
  void AnimSetStartTime(float t);
  void GetMinMax(float &,float &,float &,float &,float &,float &);
  void Show() {flag.show=1;}
  void Hide() {flag.show=0;}
  char *GetName() {return name;}
};
typedef NodePtr<Koerper> KoerperPtr;

#endif