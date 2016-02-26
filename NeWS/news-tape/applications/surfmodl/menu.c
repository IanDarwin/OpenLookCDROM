#include "surfmodl.h"
void menu(Cmmd)
int *Cmmd;
{
    do {
      openwin (13,6,67,21);
      /*gotoXY(21,1); */
      printf ("SURFMODL 1.1\n");
      printf("\n");
      printf("            (c)1986,87  Kenneth Van Camp\n");
      printf("\n");
      printf ("                     MAIN MENU\n");
      printf("\n");
      printf ("               1  Change Parameters\n");
      printf ("               2  Wire Frame Plot\n");
      printf ("               3  Hidden Line Removal\n");
      printf ("               4  Surface Model\n");
      printf ("               0  Exit Program\n");
      printf("\n");
      printf ("Command: ");
      *Cmmd = getkey() - '0';
      if ( (*Cmmd < 0) || (*Cmmd > 4) )
        printf("\g");

    } while ((*Cmmd < 0) || (*Cmmd > 5));
    printf ("%d\n",*Cmmd);
} /* procedure Menu */

