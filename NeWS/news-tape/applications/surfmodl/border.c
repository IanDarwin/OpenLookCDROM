#include "surfmodl.h"
void border(Surf,Bcolor,Bsaturate)
int Surf;
double Bcolor,Bsaturate;
{
 int Vert;                 /* vertex # being plotted */
 int Node1;                 /* first node of line */
 int Node2;                 /* second node of line */
 float Color,Saturate;                 /* actual color used */
  /* Make sure the color is legitimate */
  Color = Bcolor;
  Saturate = Bsaturate;
  if (Bcolor > 1.0)  Color = 1.0;
  if (Bsaturate > 1.0) Saturate = 1.0;
  gcolor(Color,Saturate);
  gsetgray(0.);

/* create path for surface */

  Surfpath(Surf);

  gstroke();

} /* procedure Border */
