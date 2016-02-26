#include "surfmodl.h"
void openfile(Filename,Infile)
text80 *Filename;
FILE ** Infile;
{
  FILE *fopen();
  Fileread = FALSE;
  while ( (! Fileread) ) {
    if ( (*Infile = fopen(Filename,"r"))== NULL ) {
      printf ("Error: file ",Filename," does not exist.\n");
      printf ("Enter new file name (or <enter> to exit): ");
      scanf ("%s",Filename);
      if ( (Filename[0] == ' ') )
       ; 
    }  else
      Fileread = TRUE;
  }
}   /* procedure OPENFILE */

void readfile(Filename)
text80 *Filename;
{
  int Version;	      /* used for multiple verison input flag (only 4 now) */
  int j;             /* counter for looping and reading into arrays*/
  FILE * Infile;           /* file to read*/
  vartype Realvar[MAXVAR];       /* temporary array for storage of line input */
  int Num;           /* number of inputted values on the line */
  text80 Comment[80];        /* comment at end of line */
  int Line_num;      /* line number in input file */
  int Nvread;        /* #vertices read so far in this surface */
  int Vert;          /* vertex # */
  int Nscript;       /* #script inputs */
  int Cmmd;          /* script command number */
  int Mat;           /* material # */
  int Node;          /* node # */
  int Surf;          /* surface # */
  int Connection;    /* next connection number on surface */
  int nbytes;
  char *malloc();

  openfile (Filename, &Infile);
  Flpurpose = malloc(128);
  getline(Infile,&Flpurpose,&nbytes);
  Line_num = 2;
  Num = inreal (Infile, Realvar, Comment, Line_num, FALSE);
  if ( (Num != 1) ) {
    printf ("Bad input: Reading version number.\n");
    fclose (Infile);
    exit(1);
  }
  Version = round(Realvar[0]);
  if ( (Version == 1) ) {
    Line_num++;
    Num = inreal (Infile, Realvar, Comment, Line_num, FALSE);
    if ( (Num != 4) ) {
      fprintf ("Bad input: Reading #nodes, #surfaces, Maxvert and #materials (line %d)\n",
          Line_num);
      fclose (Infile);
      exit(1);
    }
    Nnodes = round(Realvar[0]);
    Nsurf = round(Realvar[1]);
    Maxvert = round(Realvar[2]);
    Nmatl = round(Realvar[3]);
    Nscript = 0;
    Nsides = 1;
  }  else if ( (Version == 2) ) {
    Line_num++;
    Num = inreal (Infile, Realvar, Comment, Line_num, FALSE);
    if ( (Num != 6) ) {
      printf ("Bad input: Reading #matl, #nodes, #surf, #script, Maxvert, #sides (line %d)\n",
          Line_num);
      fclose (Infile);
      exit(1);
    }
    Nmatl = round(Realvar[0]);
    Nnodes = round(Realvar[1]);
    Nsurf = round(Realvar[2]);
    Nscript = round(Realvar[3]);
    Maxvert = round(Realvar[4]);
    Nsides = round(Realvar[5]);
  }  else if ( (Version == 3) || (Version == 4) ) {
    Line_num++;
    Num = inreal (Infile, Realvar, Comment, Line_num, FALSE);
    if ( (Num != 5) ) {
      printf ("Bad input: Reading #matl, #nodes, #surf, Maxvert, #sides (line %d)\n",
          Line_num);
      fclose (Infile);
      exit(1);
    }
    Nmatl = round(Realvar[0]);
    Nnodes = round(Realvar[1]);
    Nsurf = round(Realvar[2]);
    Maxvert = round(Realvar[3]);
    Nsides = round(Realvar[4]);
  }  else {
    printf("Wrong data input version number specified\n");
    fclose (Infile);
    exit(1);
  }

    if (Xworld) free(Xworld);
    if (Yworld) free(Yworld);
    if (Zworld) free(Zworld);
    if (Xtran) free(Xtran);
    if (Ytran) free(Ytran);
    if (Ztran) free(Ztran);
    if (Connect) free(Connect);
    if (Nvert) free(Nvert);
    if (Matl) free(Matl);
    Xworld = (nodearray *)malloc(Nnodes * sizeof(nodearray));
    Yworld = (nodearray *)malloc(Nnodes * sizeof(nodearray));
    Zworld = (nodearray *)malloc(Nnodes * sizeof(nodearray));
    Xtran = (nodearray *)malloc(Nnodes * sizeof(nodearray));
    Ytran = (nodearray *)malloc(Nnodes * sizeof(nodearray));
    Ztran = (nodearray *)malloc(Nnodes * sizeof(nodearray));
    Connect = (int **)malloc(Nsurf * sizeof(int *));
    Nvert = (int *)malloc(Nsurf * sizeof(int ));
    Matl = (int *)malloc(Nsurf * sizeof(int ));


    if (  (Nmatl<=MAXMATL) &&
       (Nsides<=2) && (Nnodes>0) &&
       (Nsurf>0) && (Nmatl>0) ) {
      for ( Mat = 0 ; Mat < Nmatl; Mat++ ) {
        Line_num++;
        Num = inreal (Infile, Realvar, Comment, Line_num, FALSE);
        if ( (Version <= 2) ) {
          if ( (Num != 3) ) {
            printf ("Bad input: Reading data for material #%d (line %d)\n",Mat,
                Line_num);
            fclose (Infile);
            exit(1);
          }
          R1[Mat] = Realvar[0];
          R2[Mat] = Realvar[1];
          R3[Mat] = 0.0;
          if(Realvar[2] > 1.0) 
	      Color[Mat] = Realvar[2]/32;
	  else
	      Color[Mat] = Realvar[2];
	  Saturation[Mat] = 1.0;
          Ambient[Mat] = 0.1;
        }  else if ( (Version == 3) ) {
          if ( (Num != 4) ) {
            printf ("Bad input: Reading data for material #%d (line %d)\n",Mat,
                Line_num);
            fclose (Infile);
            exit(1);
          }
          R1[Mat] = Realvar[0];
          R2[Mat] = Realvar[1];
          R3[Mat] = Realvar[2];
          if(Realvar[3] > 1.0) 
	      Color[Mat] = Realvar[3]/32;
	  else
	      Color[Mat] = Realvar[3];
	  Saturation[Mat] = 1.0;
          Ambient[Mat] = 0.1;
        }  else {
          if ( (Num != 5) ) {
            printf ("Bad input: Reading data for material #%d (line %d)\n",Mat,
                Line_num);
            fclose (Infile);
            exit(1);
          }
          R1[Mat] = Realvar[0];
          R2[Mat] = Realvar[1];
          R3[Mat] = Realvar[2];
          if (Realvar[3] > 1.0)
	      Color[Mat] = Realvar[3]/32;
	  else
	      Color[Mat] = Realvar[3];
          Ambient[Mat] = Realvar[4];
	  Saturation[Mat] = 1.0;
        } /* if Version */
      }  /*for Mat*/
      for ( Node = 0 ; Node <Nnodes ; Node++) {
        Line_num++;
        Num = inreal (Infile, Realvar, Comment, Line_num, FALSE);
        if ( (Num != 3) ) {
          printf ("Bad input: Reading data for node #%d (line %d)\n",Node,
              Line_num);
          fclose (Infile);
          exit(1);
        }
        Xworld[Node] = Realvar[0];
        Yworld[Node] = Realvar[1];
        Zworld[Node] = Realvar[2];
      } /*for Node*/
      for ( Surf = 0 ; Surf < Nsurf; Surf++ ) {
        Line_num++;
        Num = inreal (Infile, Realvar, Comment, Line_num, FALSE);
        if ( (Num < 5) ) {
          printf ("Bad input: Reading data for surface #%d (line %d)\n",Surf,
              Line_num);
          if ( (Num > 2) )
            printf ("Must have at least 3 nodes on a surface!\n");
          fclose (Infile);
          exit(1);
        }
        Nvert[Surf] = round(Realvar[0]);
        Matl[Surf] = round(Realvar[1]);

	Connect[Surf] = (int *)malloc(Nvert[Surf]*sizeof(int));

        if ( (Nvert[Surf]<3) || (Nvert[Surf]>Maxvert) || (Nvert[Surf]<Num-2)
            || (Matl[Surf]<1) || (Matl[Surf]>Nmatl) ) {
         printf ("Error in surface %d (line %d):\n",Surf,Line_num);
          if ( (Nvert[Surf] < 3) )
            printf ("Must have at least 3 nodes per surface\n");
           else if ( (Nvert[Surf] > Maxvert) )
            printf ("#vertices exceeds Maxvert\n");
           else if ( (Matl[Surf]<1) || (Matl[Surf]>Nmatl) )
            printf ("Matl no. not in range 0..Nmatl (%d)\n",Nmatl);
           else
            printf ("#vertices specified does not match #arguments\n");
          fclose (Infile);
          exit(1);
        } /* if Nvert... */
        Nvread = Num - 2;
        for ( Vert = 0 ; Vert < Nvread; Vert++ ) {
          Connection = round(Realvar[Vert+2]);
          if ( (Connection<1) || (Connection>Nnodes) ) {
            printf ("Error in surface %d (line %d):\n",Surf,Line_num);
            printf ("Connection #%d not in range 0..Nnodes (%d",
              Vert,Nnodes);
            fclose (Infile);
            exit(1);
          }
          Connect[Surf][Vert] = Connection - 1;
        } /* for Vert */
        while ( (Nvread < Nvert[Surf]) ) {
          Line_num++;
          Num = inreal (Infile, Realvar, Comment, Line_num, FALSE);
          if ( (Num < 1) || (Nvread + Num > Nvert[Surf]) ) {
            printf ("Error in surface %d (line %d): ",Surf,Line_num);
            if ( (Num == 0) )
              printf ("No data read.\n");
            else if ( (Nvread + Num > Nvert[Surf]) )
              printf ("Too many vertices read.\n");
            close (Infile);
            exit(1);
          } /* if Num... */
          Vert = Nvread + 1;
          for ( j = 0 ; j < Num ; j++) {
            Connection = round(Realvar[j]);
            if ( (Connection<1) || (Connection>Nnodes) ) {
              printf ("Error in surface %d (line %d):\n",Surf,Line_num);
              printf ("Connection #%d not in range 0..Nnodes (%d)\n",Vert,
                Nnodes);
              fclose (Infile);
              exit(1);
            }
            Connect[Surf][Vert] = Connection - 1;
            Vert++;
          }
          Nvread = Nvread + Num;
        } /* while */
      } /* for Surf */
    }  else {
      if ( (Nmatl>MAXMATL) || (Nmatl<1) )
        printf("Nmatl (%d) must be between 1 and %d\n",Nmatl,MAXMATL);
      if ( (Nsides<1) || (Nsides>2) )
        printf("Nsides (%d) must be either 1 or 2\n",Nsides);
      fclose (Infile);
      exit(1);
    } /* if Nnodes... */

    XYmax = 0;
    fclose (Infile);
    readini (Filename);
    setprintfile(Filename);
    Sorted = FALSE;
} /* procedure READFILE */

