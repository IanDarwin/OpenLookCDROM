#include "surfmodl.h"
void writeini()
{
    FILE *Outfile, *fopen();           /* file to write to */
    boolean Notopen;        /* flag file not open yet */
    char Ans;               /* answer to user query */
    int Mat;           /* material # */
    int Lite;           /* light source # */

  Outfile = fopen(Inifile,"w+");
  Notopen = TRUE;
  while ( (Notopen) ) {
    if ( (Outfile != 0) )
      Notopen = FALSE;
    else {
      printf ("Error writing file %s\n", Inifile);
      printf ("Try again (Y/N)? ");
      scanf ("%c",Ans);
      if ( (Ans == 'N') || (Ans == 'n') )
        exit(1);
    } /* if ioresult */
  } /* while */

  fprintf (Outfile,"%d\n", 2);
  fprintf (Outfile,"%d %d\n", 1,System);
  fprintf (Outfile,"%d %f %f %f\n", 2, Xeye, Yeye, Zeye);
  fprintf (Outfile,"%d %f %f %f\n", 3, Xfocal, Yfocal, Zfocal);
  fprintf (Outfile,"%d %f\n", 4, Magnify);
  fprintf (Outfile,"%d %d\n", 5,Viewtype);
  /* 6: Autoresolve removed */
  for ( Mat = 0 ; Mat < Nmatl; Mat++ )
    fprintf (Outfile,"%d %d %f %f %f %f %f\n", 7,Mat + 1, R1[Mat], R2[Mat], R3[Mat],
             Color[Mat], Ambient[Mat]);
  for ( Lite = 0 ; Lite < Nlite; Lite++ )
    fprintf (Outfile,"%d %d %f %f %f %f\n", 8 ,Lite + 1, Xlite[Lite], Ylite[Lite], Zlite[Lite],
             Intensity[Lite]);
/*  writeln (Outfile, 9, Ambient); */
  if ( (Interpolate) )
    fprintf (Outfile,"%d %d\n", 10, Epsilon);
  else
    fprintf (Outfile, "%d %f\n",10,0.0);
  if ( (Shadowing) )
    fprintf (Outfile,"%d %d\n", 11,1);
   else
    fprintf (Outfile,"%d %d\n", 11, 0);
    fprintf (Outfile,"%d %d\n", 12,Ngraphchar);
    fprintf (Outfile,"%d %d %f %f %f %f\n", 13, Showaxes, Xaxislen, Yaxislen, Zaxislen,
           Axiscolor);
    fprintf (Outfile,"%d %d\n", 14,Nwindow);
    /*
  if ( (Mono) )
    fprintf (Outfile,"%d %d %d\n", 15,Ncolors, 1);
  else
    fprintf (Outfile, "%d %d %d\n",15,Ncolors,0);
    */
    fprintf (Outfile,"%d\n", 99);
  fclose (Outfile);
} /* procedure Writeini */
