#include "surfmodl.h"
#include <string.h>
void readini(Datafile)
text80 *Datafile;
{
    int Num;           /* #parameters read on line */
    vartype Realvar[MAXVAR];       /* temp array for storage of line input */
    FILE * Infile, *fopen();           /* file to read */
    text80 Comment[80];        /* comment at end of line */
    int Version;       /* version # of the INI file */
    int Line_num;      /* line # in input file */
    char *Period;        /* position of period in filename */
    int Cmmd;          /* selection # */
    boolean Filebad;       /* flag bad file */
    int Mat;          /* material # */
    int Lite;          /* light source # */
    char *rindex(), *malloc();

  /* Correctly name the INI file */
  Period = rindex (Datafile,'.');
  Inifile = malloc(32);
  strcpy(Inifile, Datafile);
  if ( (Period == 0) )
    Inifile = strcat(Inifile, ".ini");
  else
    Inifile = strcat(Inifile,".ini");
  Filebad = FALSE;
/* See if it exists */
  if ( (Infile = fopen(Inifile,"r")) != NULL ) {
    /* File exists; read it and set global parameters */
    Line_num = 1;
    Num = inreal (Infile, Realvar, Comment, Line_num, FALSE);
    if ( (Num != 1) ) {
      printf ("Bad INIfile: Reading version number.\n");
      Filebad = TRUE;
    }  else
      Version = round (Realvar[0]);

    if ( ((Version == 1) || (Version == 2)) && (! Filebad) ) {
      Cmmd = 0;
      while ( (Cmmd != 99) && (! Filebad) ) {
        Line_num++;
        Num = inreal (Infile, Realvar, Comment, Line_num, FALSE);
        if ( (Num < 1) ) {
          printf ("Bad INIfile\n");
          Filebad = TRUE;
        }
        if ( (Filebad) )
          Cmmd = 99;
         else
          Cmmd = round (Realvar[0]);

        switch (Cmmd ) {
          case 1:
            if ( (Num != 2) || (Realvar[1]<1.0) || (Realvar[1]>3.0) ) {
              printf ("INIfile: System number input bad\n");
              Filebad = TRUE;
            }  else {
              System = round(Realvar[1]);
#ifdef TOOLBOX
              System = 1;
#endif
#ifdef STDCGA
              System = 1;
#endif
            } /* if Num */
	    break;
          case 2:
            if ( (Num != 4) ) {
              printf ("INIfile: Eye coordinates bad\n");
              Filebad = TRUE;
            }  else {
              Xeye = Realvar[1];
              Yeye = Realvar[2];
              Zeye = Realvar[3];
            }
	    break;
          case 3:
            if ( (Num != 4) ) {
              printf ("INIfile: Focal point coordinates bad\n");
              Filebad = TRUE;
            }  else {
              Xfocal = Realvar[1];
              Yfocal = Realvar[2];
              Zfocal = Realvar[3];
            }
	    break;
          case 4:
            if ( (Num != 2) ) {
              printf ("INIfile: Bad magnification factor\n");
              Filebad = TRUE;
            }  else
              Magnify = Realvar[1];
	    break;
          case 5:
            if ( (Num != 2) || (Realvar[1]<0.0) || (Realvar[1]>3.0) ) {
              printf ("INIfile: Bad view type\n");
              Filebad = TRUE;
            }  else
              Viewtype = round(Realvar[1]);
	    break;
          case 6:; /* Autoresolve removed */
	    break;
          case 7:
            if ( (Version == 1) ) {
              if ( (Num != 6) || (Realvar[1]<1.0)||(Realvar[1]>Nmatl) ) {
                printf ("Warning: Bad matl data in INIfile.\n");
              }  else {
                Mat = round (Realvar[1]);
                R1[Mat] = Realvar[2];
                R2[Mat] = Realvar[3];
                R3[Mat] = Realvar[4];
                Color[Mat] = Realvar[5];
		Saturation[Mat] = 1.0;
                Ambient[Mat] = 0.1;
              }
            }  else {
              if ( (Num != 7) || (Realvar[1]<1.0)||(Realvar[1]>Nmatl) ) {
                printf ("Warning: Bad matl data in INIfile.\n");
              }  else {
                Mat = round (Realvar[1]);
                R1[Mat] = Realvar[2];
                R2[Mat] = Realvar[3];
                R3[Mat] = Realvar[4];
                if (Realvar[5] > 1.0) 
		    Color[Mat] = Realvar[5]/32;
		else
		    Color[Mat] = Realvar[5];
		Saturation[Mat] = 1.0;
                Ambient[Mat] = Realvar[6];
              }
            } /* if Version */
	    break;
          case 8:
            if ( (Num != 6) || (Realvar[1] < 1.0) || (Realvar[1] > MAXLITE)
                || (Realvar[1] > Nlite+1) ) {
              printf ("INIfile: Bad Light Source Data\n");
              Filebad = TRUE;
            }  else {
              Lite = round (Realvar[1]);
              if ( (Lite > Nlite) )
                Nlite = Lite;
              Xlite[Lite] = Realvar[2];
              Ylite[Lite] = Realvar[3];
              Zlite[Lite] = Realvar[4];
              Intensity[Lite] = Realvar[5];
            }
	    break;
          case 9:
            if ( (Num != 2) || (Version != 1) ) {
              printf ("INIfile: Bad ambient light\n");
              Filebad = TRUE;
            }  else
              for ( Mat = 0 ; Mat < Nmatl; Mat++ )
                Ambient[Mat] = Realvar[1];
	    break;
          case 10:
            if ( (Num != 2) ) {
              printf ("INIfile: Bad Epsilon\n");
              Filebad = TRUE;
            }  else
              if ( (Realvar[1] > 0.0) ) {
                Epsilon = Realvar[1];
                Interpolate = TRUE;
              }  else
                Interpolate = FALSE;
	    break;
          case 11:
            if ( (Num != 2) || (Realvar[1]<0.0) || (Realvar[1]>1.0) ) {
              printf ("INIfile: Bad Shadowing\n");
              Filebad = TRUE;
            }  else
              if ( (Realvar[1] == 0.0) )
                Shadowing = FALSE;
              else
                Shadowing = TRUE;
	    break;
          case 12:
             if ( (Num != 2) || (Realvar[1] < 0) ) {
               printf ("INIfile: Bad number of graphics characters\n");
               Filebad = TRUE;
             }  else
               Ngraphchar = round (Realvar[1]);
	     break;
          case 13:
             if ( (Num != 6) || (Realvar[1]<0) || (Realvar[1]>2) ||
                (Realvar[5]<0) || (Realvar[5]>32) ) {
               printf ("INIfile: Bad axis codes\n");
               Filebad = TRUE;
             }  else {
               Showaxes = round(Realvar[1]);
               Xaxislen = Realvar[2];
               Yaxislen = Realvar[3];
               Zaxislen = Realvar[4];
               Axiscolor = Realvar[5];
             }
	     break;
          case 14:
             if ( (Num != 2) ) {
               printf ("INIfile: Bad window number\n");
               Filebad = TRUE;
             }  else
               Nwindow = round(Realvar[1]);
	     break;
          case 15:
             if ( (Num != 3) || (Realvar[1] < 1) || (Realvar[1] > 32) ||
                (Realvar[2] < 0) || (Realvar[2] > 1) ) {
               printf ("INIfile: Bad system color data\n");
               Filebad = TRUE;
             }  else {
               Ncolors = round(Realvar[1]);
               if ( (Realvar[2] == 0) )
                 Mono = FALSE;
                else
                 Mono = TRUE;
             }
          case 99:
	    break;
	  default:
            printf ("Warning: INIfile has unknown code (%d)\n",Cmmd); 
	    break;
        } /* case Cmmd */
      } /* while */
    }  else {
      printf ("INIfile: Bad version number\n");
      Filebad = TRUE;
    } /* if Version */

    fclose (Infile);

#ifdef SANYOIBM
/* Change screen limits in case system is changed */
    if ( (System == 1) || (System == 3) ) {
     Gxmin  = 5;
     Gxmax  = 315;
     Gymin  = 5;
     Gymax = 195;
    }  else {
     Gxmin = 10;
     Gxmax = 630;
     Gymin = 5;
     Gymax = 195;
    }
#endif

    if ( (Filebad) ) {
      printf ("Error found in line %d of %s\n",Line_num,Inifile);
      printf ("Press any key to continue...");
      while ( (!keypressed() ) );
    }
  } /* if ioresult */
} /* procedure READINI */

