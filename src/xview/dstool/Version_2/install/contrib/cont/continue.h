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
#include <memory.h>
#include <manifold.h>

struct Cont_Cntl_Ds{
	int			Check_Switch;
	int			Cont_Hide_Settings;
	int			*Active_Param;
	int                     *i_workspace;
	int			Debug_Level;
	double                  *r_workspace;
	double			**jacobian_wrt_x;
	double			**jacobian_wrt_p;
	double                  *aug_varb_save;
	int                     aug_varb_alloc;
	memory			Continue_Mem_Ptr;
	Manifold		*manifold;
                   };


struct	Cont_DataS{
  		char	*Cont_Name;
		int	(*Cont_Init)();
		int	(*Cont_Driver)();
		int     (*Cont_Deriv)();
		int     (*Cont_Update)();
		int     (*Cont_Check)();
		int	Num_Req_Param;
		  };                      

extern  int	N_Cont, Cont_Cur_Choice;
extern  struct Cont_DataS	Cont_Sel[];

extern  int     static_init(),static_func(), static_dfunc(), static_check(), static_update();
extern  int     det_init(),det_func(), det_dfunc(), det_check(), det_update();
extern  int     sn_init(),sn_func(), sn_dfunc(), sn_check(), sn_update();
extern  int     hopf_bp_init(),hopf_bp_func(), hopf_bp_dfunc(), hopf_bp_check(), hopf_bp_update();
extern  int     bp_init(),bp_func(), bp_dfunc(), bp_check(), bp_update();
extern  int     sp_init(),sp_func(), sp_dfunc(), sp_check(), sp_update();
extern  int     kub1_init(),kub1_func(), kub1_dfunc(), kub1_check(), kub1_update();
