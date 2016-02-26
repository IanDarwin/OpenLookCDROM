#include "surfmodl.h"
void colormod(Shade,System,Color,Fmod)
double Shade;
int System;
int *Color,*Fmod;
{
  if ( (Ncolors < 3) ) {
    printf ("Error: COLORMOD called with Ncolors = %d\n",Ncolors);
    printf ("Unrecoverable error; get help!\n");
    exit(1);
  }  else if ( (Ncolors < 7) ) {
    /* IBM Color Graphics Adapter in lo-res 4-color mode */
    /* This routine uses 3 colors and some simple dithering to create
      7 shades of grey on a monochrome screen */
    if ( (Shade == 0.0) ) {
      *Color = 0;
      *Fmod = 1;
    }  else {
	switch (trunc (Shade * 6.0)) { 
      case 0: 
        *Color = 1;
        *Fmod = 3;
	break;	
      case 1: 
        *Color = 1;
        *Fmod = 2;
        break;
      case 2: 
        *Color = 1;
        *Fmod = 1;
        break;
      case 3: 
        *Color = 3;
        *Fmod = 3;
        break;
      case 4: 
        *Color = 3;
        *Fmod = 2;
        break;
      default: 
        *Color = 3;
        *Fmod = 1;
	break;
      }
    } /* case */
/* end else if (Ncolors < 15) then begin */
/* The above line should be substituted for the line below if someone
  comes up with a good color intensity table for a 15-color system.
  Note that if you are adding code for a 15-color table, Fmod can
  always be 1; dithering is not necessary if you have that many
  intensities available.
*/
  }  else {
    /* This routine uses 7 colors and some simple dithering to create
      11 shades of grey on a monochrome screen */
    if ( (Shade == 0.0) ) {
        *Color = 0;
        *Fmod = 1;
    } else 
	switch (trunc (Shade * 10.0)) { 
      case 0: 
        *Color = 1;
        *Fmod = 2;
        break;
      case 1: 
        *Color = 4;
        *Fmod = 2;
        break;
      case 2: 
        *Color = 2;
        *Fmod = 2;
        break;
      case 3: 
        *Color = 2;
        *Fmod = 1;
        break;
      case 4: 
        *Color = 5;
        *Fmod = 1;
        break;
      case 5: 
        *Color = 7;
        *Fmod = 2;
        break;
      case 6: 
        *Color = 6;
        *Fmod = 1;
        break;
      case 7: 
        *Color = 7;
        *Fmod = -3;
        break;
      case 8: 
        *Color = 7;
        *Fmod = -4;
        break;
      default:
        *Color = 7;
        *Fmod = 1;
        break;
    } /* case */
  } /* if Ncolors */
} /* procedure COLORMOD */

