//  1993 by M. Roth

#include "animate.h"
#include "szene.h"
#include "main.h"

// Exponent
double exponent(double a,double b)
{
  if (a==0) return 0;
  return exp(b*log(a));
}

// Modulowert von Float-Zahlen
double module(double a,double b)
{
  long l;
  double f;

  f=a/b;
  l=(long)f;
  return b*(f-(float)l);
}

// Konstruktor
Animation::Animation(float s,int f,long c,Koerper *k)
{
  flag=f;
  sec=s*100;
  startTime=0;
  startSec=0;
  loopCount=c;
  koerper=k;
  if ((flag&ANIM_SPEEDUP) || (flag&ANIM_SLOWDOWN))
  {
    beschleunigung=sec/(sec*sec);
  }
}

// Destruktor
Animation::~Animation()
{
  animationLst.Flush();
  aktionLst.Flush();
}

// Eigentliche Animation
int Animation::Animate(double &t,double &s)
{
  AktionPtr    	aktPtr;
  AnimationPtr 	animPtr;
  double        animTime,animSec;

  if (startSec)
  {
    s+=startSec;
    startSec=0;
  }
  else
    t+=startTime;
  if (s==0) return 0;
  if (flag&ANIM_LOOP)
  {
    animSec=module(s,sec);
    animTime=module(t,sec);
    s=0;
  }
  else
  {
    if (t>sec)
    {
      t-=sec;
      return 0;
    }
    if ((t+s) > sec)
    {
      animSec=sec-t;
      s-=animSec;
      animTime=t;
      t=0;
    }
    else
    {
      animSec=s;
      animTime=t;
      s=0;
    }
  }
  if(flag&ANIM_SPEEDUP)
  {
    animSec=beschleunigung*(animTime+animSec)*(animTime+animSec);
    animTime=beschleunigung*animTime*animTime;
    animSec-=animTime;
  }
  if(flag&ANIM_SLOWDOWN)
  {
    animSec=sec-beschleunigung*(sec-animTime-animSec)*(sec-animTime-animSec);
    animTime=sec-beschleunigung*(sec-animTime)*(sec-animTime);
    animSec-=animTime;
  }
  aktPtr=aktionLst.First();
  if (aktPtr)
  {
    while(aktPtr)
    {
      aktPtr->Animate(sec,animSec);
      aktPtr++;
    }
    if (s==0)
      return 1;
    else
      return 0;
  }
  return AnimateList(animationLst,animTime,animSec,flag);
}

// Animationsliste bearbeiten
int AnimateList(List<Animation> animationLst,double &t,double &s,int flag)
{
  AnimationPtr 		animPtr;

  animPtr=animationLst.First();
  while(animPtr)
  {
    if(animPtr->Animate(t,s))
      return 1;
    animPtr++;
    if(!(animPtr) && s && (flag&ANIM_LOOP))
      animPtr=animationLst.First();
  }
  return 0;
}

// Konstruktor von Aktion
Aktion::Aktion(float xx,float yy,float zz,int f,Koerper *k)
{
  x=xx;
  y=yy;
  z=zz;
  flag=f;
  koerper=k;
}

// Eigentliche Animation
void Aktion::Animate(double secIntervall,double secCurrent)
{
  float xx,yy,zz;
  double bruch;

  if (secIntervall==0)
  {
    xx=x;
    yy=y;
    zz=z;
  }
  else
  {
    if (flag==ANIM_SCALE)
    {
      bruch=secCurrent / secIntervall;
      xx=exponent(x,bruch);
      yy=exponent(y,bruch);
      zz=exponent(z,bruch);
    }
    else
    {
      xx=x*secCurrent/secIntervall;
      yy=y*secCurrent/secIntervall;
      zz=z*secCurrent/secIntervall;
    }
  }
  switch (flag)
  {
    case (ANIM_ROTATE):
      koerper->Rotiere( Matrix(xx,yy,zz) );
      break;
    case (ANIM_MOVE):
      koerper->Verschiebe( Vektor(xx,yy,zz) );
      break;
    case (ANIM_SCALE):
      koerper->Scale( xx,yy,zz );
      break;
    case (ANIM_CENTRE):
      koerper->Centre( Vektor(xx,yy,zz) );
      break;
    case (ANIM_HIDE):
      koerper->Hide();
      break;
    case (ANIM_SHOW):
      koerper->Show();
      break;
    case (ANIM_MODE):
      SzeneDrawMode((int)x);
      break;
    case (ANIM_EXIT):
      Error("");
      break;
  }
}

// Aktionsk”rper suchen
KoerperPtr SucheAktionKoerper(KoerperPtr kneu,KoerperPtr kalt,KoerperPtr k)
{
  KoerperPtr		koerper=0;

  if ((kneu==0) || (kalt==k)) return kneu;
  kneu = kneu->KoerperLst().First();
  kalt = kalt->KoerperLst().First();
  while((kneu) && (koerper==0))
  {
    koerper=SucheAktionKoerper(kneu,kalt,k);
    kneu++;
    kalt++;
  }
  return koerper;
}

// Animation kopieren
Animation &Animation::operator=(Animation &a)
{
  AktionPtr		aktPtr,akt;
  AnimationPtr		animPtr,anim;

  sec		=a.sec;
  startTime	=a.startTime;
  startSec	=a.startSec;
  flag		=a.flag;
  loopCount	=a.loopCount;
  beschleunigung=a.beschleunigung;
  aktPtr=a.aktionLst.First();
  while(aktPtr)
  {
    akt=new Aktion(0,0,0,0,SucheAktionKoerper(koerper,a.koerper,aktPtr->koerper));
    *akt=*aktPtr;
    aktionLst+=akt;
    aktPtr++;
  }
  animPtr=a.animationLst.First();
  while(animPtr)
  {
    anim=new Animation(0,0,0,koerper);
    *anim=*animPtr;
    animationLst+=anim;
    animPtr++;
  }
  return a;
}

// Aktion kopieren
Aktion &Aktion::operator=(Aktion &a)
{
  KoerperPtr		koer;

  flag = a.flag;
  x = a.x;
  y = a.y;
  z = a.z;
  return a;
}
