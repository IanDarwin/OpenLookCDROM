//  1993 by M. Roth

#pragma inline

#include "zbuffer.h"
#include "view.h"
#include "main.h"
#include <stdlib.h>
#include <string.h>

int *zbuffer;
int zbufferWidth;
int zbufferHeight;
int zbufferSize;

// Z-Buffer initialisieren
void ZBufferInit()
{
  zbufferWidth = ViewGetMaxX() + 1;
  zbufferHeight = ViewGetMaxY() + 1;
  zbufferSize = zbufferWidth * zbufferHeight * 4;
  zbuffer = (int *)malloc(zbufferSize);
  if (zbuffer==0) Error("Nicht genug Speicher");
}

// Z-Buffer freigeben
void ZBufferEnd()
{
  free(zbuffer);
}

// ZBuffer loeschen
void ZBufferClear()
{
  memset(zbuffer,0,zbufferSize);
}

// Zeile am ZBuffer Pruefen
int ZBufferSolidTest(ZBufferParm &zbParm)
{
  int laenge;
  int count;
  int zv,zp;
  int *pos;

  if (zbParm.xb==zbParm.xv) return 0;
  pos=zbuffer + ((zbParm.y * zbufferWidth + zbParm.xv));
  laenge=zbParm.xb-zbParm.xv;
  count=0;
  zv=zbParm.zv;
  zp=zbParm.zp;
  while (laenge && zv < *pos)
    {
      zv+=zp;
      count++;
      laenge--;
      pos++;
    }
  zbParm.zv=zv;
  if (!(laenge))
    return 0;
  zbParm.xv+=count;
  zbParm.hv+=count*zbParm.hp;
  zbParm.vonX=zbParm.xv;
  zbParm.vonH=zbParm.hv;
  count=0;
  while (laenge && zv >= *pos)
    {
      *pos=zv;
      zv+=zp;
      count++;
      laenge--;
      pos++;
    }
  zbParm.zv=zv;
  zbParm.xv+=count;
  zbParm.hv+=count*zbParm.hp;
  zbParm.laenge=count;
  return 1;
}

// Schattenzeile am ZBuffer testen
int ZBufferShadowTest(ZBufferParm &zbParm)
{
  int laenge;
  int count;
  int zv,zp;
  int *pos;

  if (zbParm.xb==zbParm.xv) return 0;
  pos=zbuffer + ((zbParm.y * zbufferWidth + zbParm.xv));
  laenge=zbParm.xb-zbParm.xv;
  count=0;
  zv=zbParm.zv;
  zp=zbParm.zp;
  while (laenge && zv > *pos)
    {
      zv+=zp;
      count++;
      laenge--;
      pos++;
    }
  zbParm.zv=zv;
  if (!(laenge))
    return 0;
  zbParm.xv+=count;
  zbParm.hv+=count*zbParm.hp;
  zbParm.vonX=zbParm.xv;
  zbParm.vonH=zbParm.hv;
  count=0;
  while (laenge && zv <= *pos)
    {
      zv+=zp;
      count++;
      laenge--;
      pos++;
    }
  zbParm.zv=zv;
  zbParm.xv+=count;
  zbParm.hv+=count*zbParm.hp;
  zbParm.laenge=count;
  return 1;
}

/*
int ZBufferShadowTest(ZBufferParm &zbParm)
{
  long far *z;
  int p;
  int h;
  int s;
  unsigned int pos;
  int laenge;
  int count;
  long zv,zp;

  s=segment;
  h=handle;
  if (zbParm.xb==zbParm.xv) return 0;
  if ((zbParm.y>rowBis) || (zbParm.y<rowVon))
  {
    p=zbParm.y/rowAnz;
    asm {
	mov	ah,44h 		// Page laden
	mov     al,0h
	mov	bx,p
	mov	dx,h
	int	67h
    };
    rowVon=p*rowAnz;
    rowBis=rowVon+rowAnz-1;
  }
  pos=(zbParm.y-rowVon) * rowSize + (zbParm.xv<<2);
  laenge=zbParm.xb-zbParm.xv;
  count=0;
  zv=zbParm.zv;
  zp=zbParm.zp;
  asm { .486
	mov	es,s         // segment
	mov	di,pos
	mov	eax,zv                      }
loop1:                                      asm {
	cmp	eax,es:[di]
	jle	end1
	add	di,4
	add	eax,zp
	inc	word ptr count
	dec	word ptr laenge
	jne	loop1                     };
end1:                                     asm {
	mov	zv,eax
	mov	pos,di
  };
  zbParm.zv=zv;
  if (!(laenge))
    return 0;
  zbParm.xv+=count;
  zbParm.vonX=zbParm.xv;
  count=0;
  asm { .486
	mov	es,s
	mov	di,pos
	mov	eax,zv                     }
loop2:                                     asm {
	cmp	eax,es:[di]
	jg	end2
	add	di,4
	add	eax,zp
	inc	word ptr count
	dec	word ptr laenge
	jne	loop2                      };
end2:                             	   asm {
	mov	zv,eax
  };
  zbParm.zv=zv;
  zbParm.xv+=count;
  zbParm.laenge=count;
  return 1;
}*/

