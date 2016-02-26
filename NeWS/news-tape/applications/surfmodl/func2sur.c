/*  func2sol.c: convert 3-d function data file into solmodl file. */
#include <stdio.h>
main (argc,argv)
int argc;
char **argv;
{
    FILE * fopen(), *filin, *filout;
    char *infile, *outfile;
    int ix , iy, nx, ny;
    int n1, n2, n3, n4;
    float x, y, z;
    if (argc == 3) {
       infile = argv[1];
       outfile = argv[2];
    } else exit(1);	
    filin = fopen (infile, "r");
    filout = fopen(outfile, "w");
    fscanf (filin," %d %d\n",&nx, &ny);
    fprintf(filout,"parabola test\n");
    fprintf (filout, "%d\n",2);     /* version number of data file */
    fprintf (filout,"%d %d %d %d %d %d\n",1, nx*ny, (nx-1)*(ny-1), 0, 4, 2);
    fprintf (filout,"%d %d %d\n", 1, 1, 3); /*material data */
    for ( ix = 0; ix < nx; ix++) {
	for (iy = 0 ; iy < ny ; iy++) {
     		fscanf(filin,"%f %f %f\n", &x, &y, &z);
     		fprintf (filout,"%f %f %f\n",x, y, z);
	}
    }
 /* end of nodal data */
    fclose (filin);
 /* begin surface connectivity */
    for ( ix = 0 ; ix < nx - 1; ix++) {
   	for (iy = 1; iy < ny  ; iy++) {
     		n1 = (ix)*ny + iy;
     		n2 = (ix)*ny + iy + 1;
     		n3 = (ix+1) * ny + iy + 1;
     		n4 = (ix+1) * ny + iy;
     		fprintf(filout,"%d %d %d %d %d %d\n", 4, 1, n1, n2, n3, n4);
  	}
    }
    fclose (filout);
}
