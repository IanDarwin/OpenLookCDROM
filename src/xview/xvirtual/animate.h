//  1993 by M. Roth

#ifndef Animate_h
#define Animate_h 1

#include "list.h"

class Koerper;

#define ANIM_LOOP 1
#define ANIM_SLOWDOWN 2
#define ANIM_SPEEDUP 4

#define ANIM_MOVE 1
#define ANIM_ROTATE 2
#define ANIM_SCALE 3
#define ANIM_CENTRE 4
#define ANIM_HIDE 5
#define ANIM_SHOW 6
#define ANIM_MODE 7
#define ANIM_EXIT 8

class Animation;
class Aktion:public Node<Aktion>
{
 friend class Animation;
 private:
  int 			flag;
  float 		x,y,z;
  Koerper		*koerper;

 public:
  Aktion(float x=0,float y=0,float z=0,int flag=0,Koerper *k=0);
  void Animate(double si,double sc);
  Aktion &operator=(Aktion &a);
};
typedef NodePtr<Aktion> AktionPtr;

class Animation:public Node<Animation>
{
 private:
  int 			flag;
  double		sec;
  double		startTime;
  double		startSec;
  long			loopCount;
  double		beschleunigung;
  Koerper		*koerper;
  List<Animation>       animationLst;
  List<Aktion>		aktionLst;

 public:
   Animation(float s,int f,long c,Koerper *k);
   ~Animation();
   int Animate(double &time,double &sec);
   void add(NodePtr<Aktion> p) {aktionLst+=p;}
   void add(NodePtr<Animation> p) {animationLst+=p;}
   void detach(NodePtr<Aktion> p) {aktionLst-=p;}
   void detach(NodePtr<Animation> p) {animationLst-=p;}
   void StartTime(float t){startTime+=t*100;startSec=startTime;}
   friend int AnimateList(List<Animation>,double &t,double &s,int flag);
   Animation &operator=(Animation &a);
};
typedef NodePtr<Animation> AnimationPtr;

#endif