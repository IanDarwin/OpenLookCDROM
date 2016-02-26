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
struct Lbmain_Ds{
	char			Fname[80];
	char			Dirname[80];
	char			Actname[80];
	int			Line;
	int			*active;
	int			N_Varbs;
	int			N_Params;
	int			N_Functions;
	int			*Active_Params;
	int			Locbif_Mode;
	int			Locbif_Dir;
	double			*State;
	double			*Params;
                   };

#define  LB_EQUIL     0
#define  LB_FPTS      1
#define  LB_PERIODIC_AUTO  2
#define  LB_PERIODIC_NAUTO  3

#define  LB_FORWARD   1
#define  LB_CONTINUE  0
#define  LB_BACKWARD  -1
