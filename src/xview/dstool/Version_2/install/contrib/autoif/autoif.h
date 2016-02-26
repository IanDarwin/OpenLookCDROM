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
struct Autoif_Ds{
		int	Show_Flag;
		int	Bv_Flag;
		int	Total_Blocks;
		int	Cur_Block;
		int	Block_Index;
		int	Aatm;
		int	Read_Mode;
		int	Save_Type;
		int	Ibr;
		int	Ntot;
		int	Itp;
		int	Lab;
		int	Nfpar;
		int	Isw;
		int	Ntpl;
		int	Nar;
		int	Nrowpr;
		int	Ntst;
		int	Ncol;
		int	Npar;
		int	*Icp;
		int	Color[3];
		double	*T;
		double	**U;
		double	*Par;
		FILE	*Auto_Fp;
		memory  Traj_Mem;
		memory  Cont_Mem;
		char    Dirname[80];
		char	Fname[80];
                };

#define		TRAJ			1001
#define		CONT			1002
#define		TRAJ_and_CONT		1003
#define		SEARCH			1004
#define		READ_ALL		1005
#define		AUTO_EOF		1006
#define		AUTOIF_ERROR		1007
#define		AUTOIF_MEM_INIT		1008
#define		AUTOIF_MEM_CLEAR	1009
