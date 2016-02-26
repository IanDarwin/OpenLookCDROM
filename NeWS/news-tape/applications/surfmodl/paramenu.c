#ifndef lint
static char *RCSid = "$Header: paramenu.c,v 1.3 88/03/01 08:05:29 dwf Locked $";
#endif

/*
 * $Log:	paramenu.c,v $
 * Revision 1.3  88/03/01  08:05:29  dwf
 * Fixed selection of Filename
 * 
 */

#include "surfmodl.h"
void paramenu(Cmmd)
int Cmmd;
{
    char Filename[41];  	 /* name of file to get data from */
    int Num;          		 /* number of variables read */
    vartype Realvar[MAXVAR];     /* variables from input */
    text80 Comment[80];          /* user's comment */
    int Lite;         		 /* light source # */
    char c;               	 /* input character */
#ifndef NEWS
	/* initial printing of menu */
  do {
/*    openwin (13,6,67,21);
    gotoXY (19,1);
    */
    printf ("PARAMETERS MENU\n");
    printf("\n"); printf("\n");
    printf ("1  Change Lighting Parameters\n");
    printf   ("2  System (");
#ifdef SANYOIBM
    if ( (System == 1) )
      printf ("IBM PC CGA)\n");
     else if ( (System == 2) )
      printf ("Sanyo Hi-Res)\n");
     else
      printf ("Sanyo [IBM Emulation])\n");
#endif
#ifdef STDCGA
    printf ("IBM PC CGA)\n");
#endif
#ifdef TBCGA
    printf ("IBM PC CGA - Toolbox)\n");
#endif
#ifdef EGA
    printf ("IBM PC EGA)"\n);
#endif
#ifdef HERCULES
    printf ("IBM PC Hercules)\n");
#endif
#ifdef Z100
    printf ("Heath/Zenith Z100)\n");
#endif
#ifdef IBM3270
    printf ("IBM 3270 PC)\n");
#endif
#ifdef ATT
    printf ("AT&T 6300 PC)\n");
#endif
    printf ("3  Eye Coordinates (%7.2f , %7.2f, %7.2f)\n",Xeye,Yeye,Zeye);
    printf ("4  Focal Point (%7.2f, %7.2f, %7.2f)\n",Xfocal,Yfocal,Zfocal);
    printf ("5  Magnification (%7.2f)\n",Magnify);
    printf   ("6  View Type (");
    if ( (Viewtype == 0) )
      printf ("Perspective)\n");
     else if ( (Viewtype == 1) )
      printf ("X-Y)\n");
     else if ( (Viewtype == 2) )
      printf ("X-Z)\n");
     else
      printf ("Y-Z)\n");
    printf ("7  Read New File From Disk\n");
    printf ("8  Save Current Settings (%s)\n",Inifile);
    printf ("0  Return To Main Menu\n");
    printf("\n");
    printf ("Command: ");
    Cmmd = getkey;
    if ( (Cmmd < 0) || ((Cmmd > 8) && (Cmmd != 10)) )
      printf("\g");
     else {
      printf ("%d\n",Cmmd);
#endif
      switch (Cmmd ) {
        case 1: litemenu();
		  break;
        case 2: 
          Num = 0;
          while ( (Num != 3) ) {
	    printf (" Xeye = %f, Yeye = %f, Zeye = %f\n",Xeye,Yeye,Zeye);
            printf ("Enter new X, Y and Z of eye: ");
            Num = inreal (stdin, Realvar, Comment, 0, TRUE);
            if ( (Num != 3) )
              printf ("Error: Expecting 3 numeric values\n");
          } /* while */
          Xeye = Realvar[0];
          Yeye = Realvar[1];
          Zeye = Realvar[2];
          break;
        case 3: 
          Num = 0;
          while ( (Num != 3) ) {
	    printf (" Xfocal = %f, Yfocal = %f, Zfocal = %f\n",Xfocal,Yfocal,Zfocal);
            printf ("Enter new X, Y and Z of focal point: \n");
            Num = inreal (stdin, Realvar, Comment, 0, TRUE);
            if ( (Num != 3) )
              printf ("Error: Expecting 3 numeric values\n");
          } /* while */
          Xfocal = Realvar[0];
          Yfocal = Realvar[1];
          Zfocal = Realvar[2];
          break;
        case 4: 
          Num = 0;
          while ( (Num != 1) ) {
	    printf (" magnification factor = %f\n",Magnify);
            printf ("Enter new magnification factor: \n");
            Num = inreal (stdin, Realvar, Comment, 0, TRUE);
            if ( (Num != 1) )
              printf ("Error: Expecting 1 numeric value\n");
          } /* while */
          Magnify = Realvar[0];
          break;
        case 5: 
          Num = 0;
          while ( (Num != 1) || (Realvar[0]<0) || (Realvar[0]>3) ) {
            printf ("Enter 0 for perspective plotting,\n      1 for X-Y plot,\n");
            printf ("      2 for X-Z plot, or\n      3 for Y-Z plot: \n");
            Num = inreal (stdin, Realvar, Comment, 0, TRUE);
          } /* while */
          Viewtype = round(Realvar[0]);
          Num = 0;
          while ( (Num != 1) || (Realvar[0]<0) || (Realvar[0]>1) ) {
            printf ("Enter 1 to show axes or 0 for no axes: \n");
            Num = inreal (stdin, Realvar, Comment, 0, TRUE);
          } /* while */
          Showaxes = round(Realvar[0]);
          if ( (Showaxes > 1) ) { /* just toggle Showaxes for now */
            Num = 0;
            while ( (Num != 3) || (Realvar[0]<0) || (Realvar[1]<0) ||
                  (Realvar[2]<0) ) {
              printf ("Enter axis lengths in X, Y and Z\n");
              printf ("Old lengths: (%f %f %f):\n",Xaxislen, Yaxislen,
                     Zaxislen);
              Num = inreal (stdin, Realvar, Comment, 0, TRUE);
              if ( (Realvar[0]<0) || (Realvar[1]<0) || (Realvar[2]<0) )
                printf ("Axis lengths must be >= 0\n");
            } /* while */
            Xaxislen = Realvar[0];
            Yaxislen = Realvar[1];
            Zaxislen = Realvar[2];
            Num = 0;
            while ( (Num != 1) || (Realvar[0]<0) || (Realvar[0]>32) ) {
              printf ("Enter axis color (now %f)\n",Axiscolor);
              Num = inreal (stdin, Realvar, Comment, 0, TRUE);
              if ( (Realvar[0]<0) || (Realvar[0]>32) )
                printf ("Axis color must be between 0 and 32\n");
            } /* while */
            Axiscolor = Realvar[0];
          } /* if Showaxes */
          break;
	case 6:
	  printf ("Zcutfar: %f, Zcutnear: %f\n",Zcutfar,Zcutnear);
	  printf (" Enter far and near Zcut values: ");
	  while (( Num = inreal(stdin,Realvar,Comment,0,TRUE)) != 2);
	  Zcutfar = Realvar[0];
	  Zcutnear = Realvar[1];
	  break;
        case 7: 
          printf ("Enter new file name: ");
          scanf ("%s",Filename);
	  setprintfile(Filename);
	  readfile(Filename);
          break;
        case 8: 
	  writeini();
	  break;
	case 10:
	case 0:
	default:
         /* Return to Main Menu */
         /* Return to Main Menu */
	    break;
      } /* case Cmmd */
#ifndef NEWS      
  } while (Cmmd !=0 !! Cmmd > 10)
#endif
} /* procedure Paramenu */

