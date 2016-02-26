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
#ifndef TWOD_HEADER
#define TWOD_HEADER

#include <constants.h>
#include <xview/cms.h>

typedef struct {
	GC			Canvas_Gc;
	GC			Cbar_Gc;
	Cms                     Colormap_Cms; 
	int                     ColorTable[MAX_COLORS];
	unsigned long		*TwoD_Sys_Colors;
	unsigned long		*TwoD_Traj_Colors;
	int			Total_Sys_Colors;
	int			Total_Traj_Colors;
	int			Cbar_Index;
	int			Color_Choice;
	char			*Panel_Color;
	int			P_Origin_X;    
	int			P_Origin_Y;
	int			Px_Max;
	int			Py_Max;
	double			Hor_Min;
	double			Hor_Max;
	double			Vert_Min;
	double			Vert_Max;
	int                     Active_Hor_Type; 
	int                     Active_Ver_Type; 
	int                     Active_Hor_Index; 
	int                     Active_Ver_Index;
} TwoD_Win_Cntl_Ds_Objects;

typedef struct {
	TwoD_Win_Cntl_Ds_Objects	*TwoD_Win_Ds;
} TwoD_Ds_List_Item;

extern	TwoD_Ds_List_Item	**TwoD_Ds;

extern	char	*Panel_Color_Choice[]; 
extern	int     Num_Panel_Colors;


#endif
