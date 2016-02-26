#include "surfmodl.h"
void litemenu(Cmmd)
int Cmmd;
{
    int Num;          /* number of variables read */
    vartype Realvar[MAXVAR];      /* variables from input */
    text80 Comment[80];       /* user's comment */
    int Lite;         /* light source # */
    int Mat;          /* material # */

      switch (Cmmd)  {
        case 1: 
          Num = 0;
          while ( (Num != 1) ) {
            printf ("Enter material number: ");
            Num = inreal (stdin, Realvar, Comment, 0, TRUE);
            if ( (Num != 1) || (Realvar[0] < 1.0) || (Realvar[0] > Nmatl) )
              printf ("Error: Bad material number\n");
          } /* while */
          Mat = round(Realvar[0]);
          printf ("Old R1, R2, R3, Color, Ambient = %f , %f, %f, %f, %f\n",R1[Mat],
            R2[Mat], R3[Mat],Color[Mat], Ambient[Mat]);
          while ( (Num != 3) ) {
            printf ("Enter R1, R2, and R3 for Material %d:",Mat);
            Num = inreal (stdin, Realvar, Comment, 0, TRUE);
            if ( (Num != 3) )
              printf ("Error: Expecting 3 numeric values\n");
          } /* while */
          R1[Mat] = Realvar[0];
          R2[Mat] = Realvar[1];
          R3[Mat] = Realvar[2];
          while ( (Num != 2) ) {
            printf ("Enter Color and Ambient for Material %d:",Mat);
            Num = inreal (stdin, Realvar, Comment, 0, TRUE);
            if ( (Num != 2) )
              printf ("Error: Expecting 2 numeric values\n");
          } /* while */
          Color[Mat] = Realvar[0];
          Ambient[Mat] = Realvar[1];
          break; 
        case 2: 
          Num = 0;
          while ( (Num!=1) || (Realvar[0] <0.0) || (Realvar[0] >Nlite+1) ) {
	    printf ("There are %d Light sources\n",Nlite);
            printf ("Enter light source # to change\n");
            printf (" (0 to delete last light source)\n");
            printf (" (%d to add new light source): ",Nlite+1);
            Num = inreal (stdin, Realvar, Comment, 0, TRUE);
            if ( (Num != 1) || (Realvar[0] < 0.0) || (Realvar[0] > Nlite+1) )
              printf ("Error: Expecting value in range 0 - %d\n",Nlite+1);
          } /* while */
          Lite = round(Realvar[0]) - 1;
          if ( (Lite < 0) ) {
            Nlite--;
            if ( (Nlite < 1) ) {
              printf ("Error: Cant delete all light sources!\n");
              Nlite = 1;
            }
          }
          if ( (Lite + 1) > Nlite )
            Nlite = Lite + 1;
          if ( (Lite > -1) ) {
            printf ("Old X, Y and Z = %f , %f, %f\n",Xlite[Lite],Ylite[Lite],
              Zlite[Lite]);
            Num = 0;
            while ( (Num != 3) ) {
              printf ("Enter new X, Y and Z of light source %d\n",Lite + 1);
              Num = inreal (stdin, Realvar, Comment, 0, TRUE);
              if ( (Num != 3) )
                printf ("Error: Expecting 3 numeric values\n");
            }/* while */
            Xlite[Lite] = Realvar[0];
            Ylite[Lite] = Realvar[1];
            Zlite[Lite] = Realvar[2];
            printf ("Old intensity = %f\n", Intensity[Lite]);
            Num = 0;
            while ( (Num != 1) ) {
              printf ("Enter intensity of light source %d:\n",Lite+1);
              Num = inreal (stdin, Realvar, Comment, 0, TRUE);
              if ( (Num != 1) )
                printf ("Error: Expecting 1 numeric value\n");
            } /* while */
            Intensity[Lite] = Realvar[0];
          } /* if Lite */
          break; 
        case 3: 
          printf ("Epsilon: Largest Change in Shading to Interpolate.\n");
          printf ("Old Epsilon is %f\n",Epsilon);
          Num = 0;
          while ( (Num != 1) ) {
            printf ("Enter Epsilon (or 0.0 for no interpolation): \n");
            Num = inreal (stdin, Realvar, Comment, 0, TRUE);
            if ( (Num != 1) )
              printf ("Error: Expecting 1 numeric value\n");
          } /* while */
          if ( (Realvar[0] == 0.0) )
            Interpolate = FALSE;
           else {
            Interpolate = TRUE;
            Epsilon = Realvar[0];
          }
          break; 
        case 4: 
          if ( (Shadowing) )
            Shadowing = FALSE;
           else
            Shadowing = TRUE;
          break;
	default:
	  break;
      } /* case Cmmd */
} /* procedure Litemenu */

