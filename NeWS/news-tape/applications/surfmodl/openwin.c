#include "surfmodl.h"
void openwin(Xa,Ya,Xb,Yb)
int Xa,Ya,Xb,Yb;
{
    int X, Y;
    int X1, Y1;
    int X2, Y2;

  X1 = Xa - 5;
  Y1 = Ya - 2;
  X2 = Xb + 5;
  Y2 = Yb + 2;
#ifdef MSDOS
  definetextwindow (1, 1, 1, 80, 25, 0);       /* use full screen */
#endif
#ifndef MSDOS
 /* window (1,1,80,25); */      /* use full screen */
#endif
  /* clrscr(); */
/*
  gotoXY (X1,Y1);
  printf (chr(201));
  for (X  = X1 ; X < X2; X++ )
    printf (chr(205));
  printf (chr(187));
  for ( Y = Y1 ; Y < Y2; Y++ ) {
     gotoXY(X1,Y);
    printf (chr(186));
    gotoXY (X2,Y);
    printf (chr(186));
  }
  gotoXY (X1,Y2);
  printf (chr(200));
  for (X  = X1 ; X < X2; X++ )
    printf (chr(205));
  printf (chr(188));
  
  window (Xa,Ya,Xb,Yb);
*/
#ifdef MSDOS
  definetextwindow (1, Xa, Ya, Xb, Yb, 0);
#endif
} /* procedure Openwin */

