//  1993 by M. Roth

#include "shadow.h"
#include "view.h"
#include "main.h"
#include <stdlib.h>
#include <string.h>

int shadowSize;
int shadowMin=999999,shadowMax=-999999;
int shadowX,shadowY=-1;
int shadowBorder;

struct ShadowTab
{
  int count;
  int x[120];
} *shadow;

// Shadow loeschen
void ShadowClear()
{
  memset(shadow,0,shadowSize);
}

// Z-Buffer initialisieren
void ShadowInit()
{
  shadowSize = sizeof(ShadowTab)*ViewGetMaxY();
  shadow = (ShadowTab *)malloc(shadowSize);
  if (shadow==0) Error("Nicht genug Speicher");
  ShadowClear();
}

// Z-Buffer freigeben
void ShadowEnd()
{
  free(shadow);
}

void ShadowPut(int x,int y,int border)
{
  ShadowTab *shadowTab;
  int a,b,zw;
 
  x=(x<<1)+border;
  shadowTab=shadow+y;
  if (y>shadowMax) shadowMax=y;
  if (y<shadowMin) shadowMin=y;
  for(a=0 ; a<shadowTab->count ; a++)
  {
    if (x==shadowTab->x[a])
    {
      while (a<shadowTab->count)
      {
	shadowTab->x[a]=shadowTab->x[a+1];
	a++;
      }
      shadowTab->count--;
      return;
    }
    if (x<shadowTab->x[a])
    {
      for(b=shadowTab->count ; b>a ; b--)
      {
	shadowTab->x[b]=shadowTab->x[b-1];
      }
      shadowTab->x[a]=x;
      shadowTab->count++;
      return;
    }
  }
  shadowTab->x[shadowTab->count]=x;
  shadowTab->count++;
}

int ShadowGet(int &y,int &xVon,int &laenge)
{
  ShadowTab *shadowTab;
  static int border;
  static int shad;
  int xBis;

  if (shadowY==-1)
  {
    shadowY=shadowMin;
    shadowX=0;
    border=shad=0;
  }
  if (shadowY>shadowMax)
  {
    shadowY=-1;
    shadowMax=-999999;
    shadowMin=999999;
    return 0;
  }
  shadowTab=shadow+shadowY;
  do
  {
    while (shadowX>=shadowTab->count)
    {
      shadowTab->count=0;
      shadowY++;
      shadowX=0;
      border=shad=0;
      shadowTab=shadow+shadowY;
      if (shadowY>shadowMax)
      {
	shadowY=-1;
	shadowMax=-999999;
	shadowMin=999999;
	return 0;
      }
    }
    xVon=shadowTab->x[shadowX];
    shadowX++;
    xBis=shadowTab->x[shadowX];
    if (xVon&1)
       border^=1;
    else
       shad^=1;
    xVon>>=1;
    xBis>>=1;
  } while((shad!=1) || (border!=0) || (xVon==xBis));
  y=shadowY;
  laenge=xBis-xVon;
  return 1;
}


