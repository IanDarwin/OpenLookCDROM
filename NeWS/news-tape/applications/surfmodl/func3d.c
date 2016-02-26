#include <stdio.h>
main() /* FUNC3D; */
{
/* Create a 3-D function file for display via SOLMODL */

    FILE * Filout,*fopen();
    int i, j;
    double x, y, z;

  Filout = fopen("para.dat","w");
  fprintf (Filout,"%d %d\n",21, 21);          /* grid size */

  x = -1.0;
  for ( i = 0 ; i < 21; i++ ) {
    y = -1.0;
    for ( j = 0 ; j < 21 ; j++) {
      z = x * x + y * y;
      fprintf (Filout,"%f %f %f\n",x,  y , z);
      y = y + 0.1;
    }
    x  = x + 0.1;
  }
  fclose (Filout);
}  /* program FUNC3D */
