//  1993 by M. Roth

#include "shape.h"
#include "view.h"
#include "zbuffer.h"

struct ShapePoint
{
  float x;
  float y;
  float z;
  float h;
};

int shapeMin;
int shapeMax;
int shapeStart;
ShapePoint shapeFirst;
ShapePoint shapeFrom;
ShapePoint shapeTo;
int shapeLine;
int shapePoint;

struct ShapeTab
{
  struct
  {
    int count;
    struct
    {
      int x;
      long z;
      int h;
    } point[10];
  } line[480];
} *sTab;

// Puffer fÅr neues Polygon initialisieren
void ShapeClear()
{
  int c;

  if (shapeMin<shapeMax)
    for(c=shapeMin ; c<=shapeMax ; c++)
      sTab->line[c].count = 0;
  shapeMin= 9999;
  shapeMax=-9999;
  shapeStart=1;
  shapeLine=-1;
}

// Flaechentabelle reservieren und initialisieren
void ShapeInit()
{
  sTab = new ShapeTab;
  shapeMin=0;
  shapeMax=ViewGetMaxY()-1;
  ShapeClear();
}

// Speicher wieder freigeben
void ShapeEnd()
{
  delete sTab;
}

// Linie in Linientabelle aufnehmen
void ShapeLine()
{
  int dz;
  int c,t;
  int zv,zn;
  float x,y,h;
  float px,py,ph;
  struct ShapePoint *v,*n;

  if(shapeTo.z>shapeFrom.z)
  {
    v=&shapeFrom;
    n=&shapeTo;
  }
  else
  {
    n=&shapeFrom;
    v=&shapeTo;
  }
  zv=(int)(v->z);
  zn=(int)(n->z);
  dz=(zn-zv);
  if (dz==0)
    return;
  px=(n->x - v->x ) / dz;
  py=(n->y - v->y ) / dz;
  ph=(n->h - v->h ) / dz;
  x=v->x;
  y=v->y;
  h=v->h;
  for(c=zv ; c<zn ; c++)
  {
    t=sTab->line[c].count;
    sTab->line[c].count++;
    while (t>0 && x < sTab->line[c].point[t-1].x)
    {
      sTab->line[c].point[t] = sTab->line[c].point[t-1];
      t--;
    }
    sTab->line[c].point[t].x =(int) x;
    sTab->line[c].point[t].z =(int) y;
    sTab->line[c].point[t].h =(int) h;
    x+=px;
    y+=py;
    h+=ph;
  }
  if (zv < shapeMin)
    shapeMin=zv;
  if (zn > shapeMax)
    shapeMax=zn;
}

// FlÑchenpunkt aufnehmen
void ShapePutPoint(ShapeParm &sParm)
{
  shapeTo.x=sParm.x;
  shapeTo.y=sParm.y*2000000000*10;
  shapeTo.z=sParm.z;
  shapeTo.h=sParm.h*3840;
  if( shapeStart )
  {
    shapeFirst=shapeTo;
    shapeStart=0;
  }
  else
    ShapeLine();
  shapeFrom=shapeTo;
}

// Linie aus Flaechenpuffer auslesen
int ShapeGetLine(ZBufferParm &zbParm)
{
  long zb;
  int hb;

  if (shapeLine==-1)
  {
    if (shapeMin>=shapeMax)
    {
      ShapeClear();
      return 0;
    }
    shapeTo=shapeFirst;
    ShapeLine();
    shapeLine=shapeMin;
    shapePoint=0;
  }
  do
  {
    if (shapePoint>=sTab->line[ shapeLine ].count)
    {
      shapePoint=0;
      shapeLine++;
      if (shapeLine>=shapeMax)
      {
	ShapeClear();
	return 0;
      }
    }
    zbParm.xv=sTab->line[ shapeLine ].point[ shapePoint ].x;
    zbParm.xb=sTab->line[ shapeLine ].point[ shapePoint+1 ].x;
    zbParm.zv=sTab->line[ shapeLine ].point[ shapePoint ].z;
	   zb=sTab->line[ shapeLine ].point[ shapePoint+1 ].z;
    zbParm.hv=sTab->line[ shapeLine ].point[ shapePoint ].h;
	   hb=sTab->line[ shapeLine ].point[ shapePoint+1 ].h;
    shapePoint+=2;
  } while (zbParm.xv==zbParm.xb);
  zbParm.zp=( zb-zbParm.zv ) / ( zbParm.xb-zbParm.xv );
  zbParm.zv+=zbParm.zp>>1;
  zbParm.hp=( hb-zbParm.hv ) / ( zbParm.xb-zbParm.xv );
  zbParm.y=shapeLine;
  return 1;
}
