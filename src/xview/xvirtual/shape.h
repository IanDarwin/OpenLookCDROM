//  1993 by M. Roth

#ifndef _line_h
#define _line_h 1

#ifndef _zbuffer_h
#include "zbuffer.h"
#endif

struct ShapeParm
{
  float x;
  float y;
  float z;
  float h;
};

void ShapeInit();
void ShapeEnd();
void ShapePutPoint(ShapeParm &);
int  ShapeGetLine(ZBufferParm &);

#endif
