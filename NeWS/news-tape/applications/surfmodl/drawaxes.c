#include "surfmodl.h"
void drawaxes(Xfotran,Yfotran,XYmax)
double Xfotran,Yfotran,XYmax;
{
 double OriginX, OriginY, OriginZ;    /* transformed (2-D) coords of origin */
 double XaxisX,  XaxisY,  XaxisZ;    /* transformed coords of end of X axis */
 double YaxisX,  YaxisY,  YaxisZ;    /* transformed coords of end of Y axis */
 double ZaxisX,  ZaxisY,  ZaxisZ;    /* transformed coords of end of Z axis */
 double Xoffset, Yoffset;    /* amt to offset axes from real origin */
 int Xchar, Ychar; /* half the size of a character */
 double Saturation;

  if ( (Showaxes > 0) ) {
    Xaxislen = 0.2*XYmax;
    Yaxislen = 0.2*XYmax;
    Zaxislen = 0.2*XYmax;
    perspect (0.0, 0.0, 0.0, &OriginX, &OriginY, &OriginZ);
    perspect (Xaxislen, 0.0, 0.0, &XaxisX, &XaxisY, &XaxisZ);
    perspect (0.0, Yaxislen, 0.0, &YaxisX, &YaxisY, &YaxisZ);
    perspect (0.0, 0.0, Zaxislen, &ZaxisX, &ZaxisY, &ZaxisZ);
    normalize (&OriginX, &OriginY, Xfotran, Yfotran, XYmax);
    normalize (&XaxisX,  &XaxisY,  Xfotran, Yfotran, XYmax);
    normalize (&YaxisX,  &YaxisY,  Xfotran, Yfotran, XYmax);
    normalize (&ZaxisX,  &ZaxisY,  Xfotran, Yfotran, XYmax);

/* Offset the origin of the axes to put it in the lower left corner */
    Xoffset = OriginX - Gxmin - (Gxmax - Gxmin) * 0.1;
    Yoffset = OriginY - Gymin - (Gymax - Gymin) * 0.1;

    OriginX = OriginX - Xoffset;
    OriginY = OriginY - Yoffset;
    XaxisX  = XaxisX  - Xoffset;
    XaxisY  = XaxisY  - Yoffset;
    YaxisX  = YaxisX  - Xoffset;
    YaxisY  = YaxisY  - Yoffset;
    ZaxisX  = ZaxisX  - Xoffset;
    ZaxisY  = ZaxisY  - Yoffset;

    Saturation = 1.0;	
    gcolor(Axiscolor,Saturation);
/* Draw the axes */
    gdraw (round(OriginX), round(OriginY), round(XaxisX), round(XaxisY));
    gdraw (round(OriginX), round(OriginY), round(YaxisX), round(YaxisY));
    gdraw (round(OriginX), round(OriginY), round(ZaxisX), round(ZaxisY));

/* Find the position for the axis labels, about 1.2 time the axis length
  from the origin */
    perspect (1.2*Xaxislen, 0.0, 0.0, &XaxisX, &XaxisY, &XaxisZ);
    perspect (0.0, 1.2*Yaxislen, 0.0, &YaxisX, &YaxisY, &YaxisZ);
    perspect (0.0, 0.0, 1.2*Zaxislen, &ZaxisX, &ZaxisY, &ZaxisZ);
    normalize (&XaxisX,  &XaxisY,  Xfotran, Yfotran, XYmax);
    normalize (&YaxisX,  &YaxisY,  Xfotran, Yfotran, XYmax);
    normalize (&ZaxisX,  &ZaxisY,  Xfotran, Yfotran, XYmax);

    XaxisX  = XaxisX  - Xoffset;
    XaxisY  = XaxisY  - Yoffset;
    YaxisX  = YaxisX  - Xoffset;
    YaxisY  = YaxisY  - Yoffset;
    ZaxisX  = ZaxisX  - Xoffset;
    ZaxisY  = ZaxisY  - Yoffset;

/* Now draw the labels */
    Xchar = (Gxmax - Gxmin) / 160;
    Ychar = (Gymax - Gymin) / 50;

/* Draw an X 
    gdraw (round(XaxisX-Xchar), round(XaxisY-Ychar),
           round(XaxisX+Xchar), round(XaxisY+Ychar));
    gdraw (round(XaxisX-Xchar), round(XaxisY+Ychar),
           round(XaxisX+Xchar), round(XaxisY-Ychar));
   */
   gmoveto(round(XaxisX),round(XaxisY));
   gshow("X");

/* Draw a Y 
    gdraw (round(YaxisX-Xchar), round(YaxisY+Ychar),
           round(YaxisX), round(YaxisY));
    gdraw (round(YaxisX+Xchar), round(YaxisY+Ychar),
           round(YaxisX), round(YaxisY));
    gdraw (round(YaxisX), round(YaxisY),
           round(YaxisX), round(YaxisY-Ychar));
*/
    gmoveto(round(YaxisX),round(YaxisY));
    gshow("Y");
/* Draw a Z 
    gdraw (round(ZaxisX-Xchar), round(ZaxisY-Ychar),
           round(ZaxisX+Xchar), round(ZaxisY-Ychar));
    gdraw (round(ZaxisX-Xchar), round(ZaxisY-Ychar),
           round(ZaxisX+Xchar), round(ZaxisY+Ychar));
    gdraw (round(ZaxisX-Xchar), round(ZaxisY+Ychar),
           round(ZaxisX+Xchar), round(ZaxisY+Ychar));
    */
    gmoveto(round(ZaxisX),round(ZaxisY));
    gshow("Z");
  } /* if Showaxes */
} /* procedure DRAWAXES */
