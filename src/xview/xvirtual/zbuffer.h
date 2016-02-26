//  1993 by M. Roth

#ifndef _zbuffer_h
#define _zbuffer_h 1

struct ZBufferParm
{
  int y;
  int xv;
  int xb;
  int hv;
  int hp;
  long zv;
  long zp;
  int laenge;
  int vonX;
  int vonH;
};

void ZBufferClear();
void ZBufferInit();
void ZBufferEnd();
int ZBufferSolidTest(ZBufferParm &);
int ZBufferShadowTest(ZBufferParm &);

#endif