//  1993 by M. Roth

#ifndef Szene_h
#define Szene_h 1

#include "list.h"
#include "vektor.h"
#include "koerper.h"

#define SOLID   1
#define WIRE    2
#define SHADOW  4
#define ANIMATE 8

void SzeneInit();
void SzeneEnd();
void SzeneSetLight(float x, float z);
void SzeneErstellen();
void SzeneDrawMode(int);
int SzeneGetDrawMode();
Vektor SzeneGetLight();
KoerperPtr SzeneGetKoerper(char *KoerperName);
void SzeneRotate(Matrix &);
void SzeneMove(Vektor &);
void SzeneSetRot(Matrix &);
void SzeneSetPos(Vektor &);
void SzeneGetMinMax(float &,float &,float &,float &,float &,float &);
void SzeneResetTime();

#endif
