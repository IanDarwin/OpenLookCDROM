
#include <stdio.h>
#define TRUE 1
#define FALSE 0
/* CONST */
#define  MAXVAR 	20         /* maximum # of numeric inputs on a line */
#define  MAXNODES 	1024       /* maximum # of nodes allowed */
#define  MAXCONNECT 	8192       /* maximum # of connections allowed */
#define  MAXSURF	2730       /* maximum # of surfaces */
                                   /* (MAXSURF = MAXCONNECT / 3) */
#define  MAXOUTLN  	1024       /* maximum # of outline nodes */
#define  NOPTIONS 	5          /* options available 0..NOPTIONS */
#define  WORKINGVERS  	1          /* current # of working versions */
#define  MAXMATL 	20         /* maximum # of materials */

double rint();
#define round(x) (int)(rint(x))
#define sqr(x) ((x)*(x))
#define konnec(Surf,Vert) (Connect[Surf][Vert])

typedef double vartype;
typedef char text80;
typedef double vector;
typedef int boolean;


    text80 Filename[80];                 /* name of file to read */
    FILE * Infile;                       /* file to read */
    int Nnodes;                          /* number of nodes */
    int Nsurf;                           /* number of surfaces */
    double *World[3];           /* xworld, yworld, zworld */
    int **Connect;  			 /* connection array */
    int *Matl; 			 /* matl# of each surface */
    int *Nvert;
    int Nmatl;                      	 /* # materials */
    int Maxvert;                      	 /* max # of vert input */
    int Realmaxsurf;                     /* max #surfaces, based on */
                                               /* Maxvert and MAXCONNECT */
    int Nsides;                      	 /* #sides of surfaces used */
    int Mat;                      	 /* material# */
    int Line_num;                      	 /* line # of error */
    int Num;                     	 /* #inputted vals on line */
    vartype Realvar[MAXVAR];                   /* input array */
    text80 Comment[80];                  /* comment at end of line */
    char *Flpurpose;                 /* title for screen */
    int Continue;            	         /* flag end of input */
    int In_version;                      /* input version number */
    int Cmmd;                      	 /* preprocessor selection# */
    double R1[MAXMATL];    		 /* matl reflect constants */
    double R2[MAXMATL];   		 /* matl reflect constants */
    double R3[MAXMATL];    		 /* matl reflect constants */
    double Color[MAXMATL]; 		 /* material color hue */
    double Saturation[MAXMATL];          /* material color saturation */
    double Ambient[MAXMATL];   		 /* ambient light intensity */
    int Checkmax;                    /* flag to check Maxvert */
    int Line_num;

