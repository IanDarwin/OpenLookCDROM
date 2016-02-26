/* ***************************************
   ************ READFEM ******************
   ***************************************
   Read Finite Element output data and convert to format for SURFMODL.
   Copyright 1986 by Kenneth Van Camp

*/
#include <stdio.h>
#define MXNODE 1000
#define MXSURF 500
#define MXELEM 1000
#define NN 8
#define NC 4
main()
{
        int XYZ[MXNODE][3], IELEM[MXELEM][NN], NFLAG[MXNODE], N[MXSURF][NC];
	int COUNT[MXNODE];
        static int ISURF[6][4] = { { 1,2,3,4},
		 { 8,7,6,5},
		 { 2,6,7,3},
		 { 3,7,8,4},
		 { 4,8,5,1},
		 { 1,5,6,2}};
	char A;
	int i,j,k;
	char Infile[10];
	int NNODE, NELEM, IPLANE;
	int CUTOFF, IORIENT, NSURF, NSNODE;
	char OFILE[10], TITL[30];
  	FILE *fp,*fpout, *fopen();
	printf("Enter file name for FEM data: ");
	scanf("%s",Infile);
	fp = fopen(Infile,"r");

	fscanf(fp,"%d %d\n",NNODE,NELEM);
	for (i=0;i < NNODE; i++)
	   fscanf(fp,"%d %d %d\n",XYZ[i][0],XYZ[i][1],XYZ[i][2]);
	for (i=0;i < NELEM; i++)
	   for (j=0; j< 8;j++)
	     fscanf(fp," %d",IELEM[i][j]);
	fclose(fp);
	printf("Data for %d nodes and %d elements read.\n",NNODE,NELEM);
	printf("Do you wish to make a slice thru the solid [Y or N]\n");
	scanf("%c",A);
	if (A != 'N' && A != 'n') {
	   while (IPLANE < 0 || IPLANE > 3) {
              printf("Enter 1, 2 or 3 to cut X, Y or Z plane respectively: "); 
   	      scanf("%d",IPLANE);
	   }

      	   printf("Cutoff value: "); scanf("%d",CUTOFF);
	   while (IORIENT !=1 || IORIENT != -1) {
              printf("Enter 1 to keep nodes > cutoff or -1 to keep nodes < cutoff: "); 
	      scanf( "%d",IORIENT);
	   }
	   for (i=0;i< NNODE;i++) {
		if (IORIENT >= 0)
			if (XYZ[i][IPLANE-1] >= CUTOFF)
				COUNT[i] = 0;
			else
				COUNT[i] = 0;
		else
			if (XYZ[i][IPLANE-1] <= CUTOFF)
				COUNT[i] = 0;
			else
				COUNT[i] = 8;
	   }
	   printf("Finding surface nodes ...\n");
/*
     Count the #elements that share each node. If a node is shared by 8
       or more elements, it is assumed to be an interior node. If it is
       shared by fewer than 8 elements, it is assumed to be exterior.
       An exterior surface is one that has all four of its corner nodes
       on the exterior.
*/     
/*         Warning: Degenerate elements will cause a node to be incorrectly
           considered an interior node!
*/
	   for (i = 0; i< NELEM; i++)
		for (j = 0; j < 8; j++)
			COUNT[IELEM[i][j]]  += 1;
           printf("Finding exterior surfaces ...\n");
	   NSURF = 0;
	   for (i= 0; i< NELEM; i++)	
	       for (j = 0; j < 4; j++) 
		   if (COUNT[IELEM[i][ISURF[j][k]]] < 7) {
		   	NSURF++;
		   	for (k = 0; k < 4; k++)
				N[NSURF][k] = ISURF[j][k];
		   }
/*     Count the surface nodes */
    	   NSNODE=0;
	   for (i = 0 ; i < NNODE; i++)
    	 	if (COUNT[i] < 8)  NSNODE = NSNODE + 1;
           printf("Enter file name for SURFMODL data: "); scanf("%s",OFILE);
	   printf("Enter title for plot: "); scanf("%s",TITL);
	   fpout = fopen(OFILE,"w");
           fprintf(fpout,"%s\n",TITL);
	   fprintf(fpout," 4\n");
	   fprintf(fpout," 1 %d %d 4 1\n",NSNODE,NSURF);
	   fprintf(fpout," 1 0 0 3 0.1\n");
	   for ( i= 0; i< NNODE; i++)
	     if (COUNT[i] < 8) fprintf(fpout,"%d %d %d",XYZ[i][0],XYZ[i][1],XYZ[i][2]);
	   for ( i = 0; i < NSURF; i++)
		fprintf(" 4 1 %d %d %d %d\n",N[i][0],N[i][1],N[i][2],N[i][3]);
	   fclose(fpout);
           printf("FINISHED CREATING %s\n",OFILE);
	}
}
