//  1993 by M. Roth

#include "clipping.h"
#include "view.h"

#define less 0
#define greater 1

float tab[2][4][80];
int sourceCount;
int destCount=-1;
int source=0;
int dest=1;
float cutMinY;
float cutMinX,cutMaxX;
float cutMinZ,cutMaxZ;

// Quell- und Zieltabelle austauschen
void SwopTab()
{
  int z;

  z=source;
  source=dest;
  dest=z;
  sourceCount=destCount;
  destCount=-1;
}

/* Ein Linienzug wird an einer Grenze abgeschnitten */
void ClippingBorder(int cut,int Direction,float Border)
{
  int count,c;
  float v[4];
  float b[4];
  float DeltaNeu,DeltaAlt;
  int von,bis;

  if (sourceCount >= 0)
  {  /* es sind Punkte in der Punkttabelle */
    for(c=0 ; c<4 ; c++)
      v[c] = tab[ source ][ c ][ sourceCount ];
    for(count=0 ; count<=sourceCount ; count++)
    {  /* alle Punkte verarbeiten */
      for(c=0 ; c<4 ; c++)
	b[c] = tab[ source ][ c ][ count ];
      if(Direction==greater)
      {   /* Alle Punkte ueber der Grenze schneiden */
	if( v[cut] > Border ) von=greater; else von=less;
	if( b[cut] > Border ) bis=greater; else bis=less;
      }
      else
      {   /* Alle Punkte unter der Grenze schneiden */
	if( v[cut] < Border ) von=greater; else von=less;
	if( b[cut] < Border ) bis=greater; else bis=less;
      }
      if( (von == less) || (bis == less) )
      {   /* Start oder Endpunkt liegen innerhalb der Grenze */
	if( (von == greater) || (bis == greater) )
	{  /* Die Linie wird an der Grenze geschnitten */
	  destCount++;
	  /* um Rundungsfehler zu vermeiden muss, egal in
	     welcher Richtung die Linie verl„uft, gleich
	     gerechnet werden. */
	  if (von == greater)
	  {
	    DeltaAlt = b[cut]-v[cut];
	    DeltaNeu = b[cut]-Border;
	    for(c=0 ; c<4 ; c++)
	      if (c==cut)
		tab[ dest ][ c ][ destCount ] = Border;
	      else
		tab[ dest ][ c ][ destCount ] = b[c] - (DeltaNeu * (b[c] - v[c])) / DeltaAlt;
	  }
	  else
	  {
	    DeltaAlt = v[cut]-b[cut];
	    DeltaNeu = v[cut]-Border;
	    for(c=0 ; c<4 ; c++)
	      if (c==cut)
		tab[ dest ][ c ][ destCount ] = Border;
	      else
		tab[ dest ][ c ][ destCount ] = v[c] - (DeltaNeu * (v[c] - b[c])) / DeltaAlt;
	  }
	}
	if(bis==less)
	{  /* Punkt unver„ndert bernehmen */
	  destCount++;
	  for(c=0 ; c<4 ; c++)
	    tab[ dest ][ c ][ destCount ] = b[c];
	}
      }  /* Alter Punkt wird neuer Punkt. */
      for(c=0 ; c<4 ; c++)
	v[c]=b[c];
    }
    SwopTab();
  }
}

// Flaechen an den Bildgrenzen schneiden und projezieren
int Clipping()
{
  int c;

  SwopTab();
  cutMinX= 999999999;
  cutMaxX=-999999999;
  cutMinZ= 999999999;
  cutMaxZ=-999999999;

  if (cutMinY<100) ClippingBorder(1,less,   10);
  for(c=0 ; c<=sourceCount ; c++)
  {
    ViewProjektion(tab[ source ][0][ c ],
		   tab[ source ][1][ c ],
		   tab[ source ][2][ c ]);
    if( tab[ source ][0][ c ] < cutMinX ) cutMinX=tab[ source ][0][ c ];
    if( tab[ source ][2][ c ] < cutMinZ ) cutMinZ=tab[ source ][2][ c ];
    if( tab[ source ][0][ c ] > cutMaxX ) cutMaxX=tab[ source ][0][ c ];
    if( tab[ source ][2][ c ] > cutMaxZ ) cutMaxZ=tab[ source ][2][ c ];
  }
  if (cutMaxX>ViewGetMaxX())
    ClippingBorder(0,greater,ViewGetMaxX());
  if (cutMinX<0)
    ClippingBorder(0,less,   0);
  if (cutMaxZ>ViewGetMaxY())
    ClippingBorder(2,greater,ViewGetMaxY());
  if (cutMinZ<0)
    ClippingBorder(2,less,   0);
  return sourceCount;
}

// Punkt in die Tabelle aufnehmen
void ClippingPut(float x,float y,float z,float h)
{
  destCount++;
  if(destCount==0) cutMinY=999999999;
  if(y<cutMinY) cutMinY=y;
  tab[ dest ][0][ destCount ]=x;
  tab[ dest ][1][ destCount ]=y;
  tab[ dest ][2][ destCount ]=z;
  tab[ dest ][3][ destCount ]=h;
}

// Geclippten Punkt aus der Tabelle auslesen
int ClippingGet(ShapeParm &sParm)
{
  static getCount = -1;

  if (getCount == -1)
    if (Clipping()==0)
      return 0;
  getCount++;
  if( getCount > sourceCount )
  {
    getCount=-1;
    return 0;
  }
  sParm.x=tab[ source ][0][ getCount ];
  sParm.y=tab[ source ][1][ getCount ];
  sParm.z=tab[ source ][2][ getCount ];
  sParm.h=tab[ source ][3][ getCount ];
  return 1;
}

