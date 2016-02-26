/*  -------------------------------------------------------------------

This program is the property of:

                             Cornell University 
                        Center for Applied Mathematics 
                              Ithaca, NY 14853

and may be used, modified and distributed freely, subject to the 
following restrictions:

       Any product which incorporates source code from the dstool
       program or utilities, in whole or in part, is distributed
       with a copy of that source code, including this notice. You
       must give the recipients all the rights that you have with
       respect to the use of this software. Modifications of the
       software must carry prominent notices stating who changed
       the files and the date of any change.

DsTool is distributed in the hope that it will be useful, but 
WITHOUT ANY WARRANTY; without even the implied warranty of FITNESS 
FOR A PARTICULAR PURPOSE.  The software is provided as is without 
any obligation on the part of Cornell faculty, staff or students to 
assist in its use, correction, modification or enhancement.

  -----------------------------------------------------------------  */
#ifndef SAVELOAD_HEADER
#define SAVELOAD_HEADER

/* for the load window */
#define NO_KEY      	  -1
#define COMMENT            1
#define POSTMASTER         2
#define SET		   3



#define NEW_FUNC          19
#define CONFIGURATION     20
#define COLOR_ON 	  21
#define SYMBOL_ON    	  22


#define DSTOOL_DATA  		0
#define DEFAULT_TRAJ_LENGTH 	1000
#define DEFAULT_MULT_LENGTH 	100
#define DEFAULT_FP_LENGTH    	10
#define DEFAULT_CONT_LENGTH 	1000
#define DEFAULT_PARAM_LENGTH  	100
#define DEFAULT_SEL_PT_LENGTH 	100
#define min(A,B)  ((A) < (B) ? (A) : (B))
#define max(A,B)  ((A) > (B) ? (A) : (B))


/* constants for save/load windows */
#define         MAX_DOUBLE_PER_LINE     8
#define         MAX_INTEGER_PER_LINE    12
#define         SAVE_PRECISION          12
#define         N_OPTIONS               8

struct Key
{
  char      *word;
  int       index;
};

extern struct Key key[];
extern struct Key pm_key[];
extern struct Key old_key[];

extern int N_KEYWORD;
extern int N_PMKEY;
extern int N_OLDKEYWORD;


extern char *key_words[];
extern int  key_index[];

#endif
