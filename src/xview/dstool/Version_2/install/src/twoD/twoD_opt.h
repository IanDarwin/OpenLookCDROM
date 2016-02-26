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
#ifndef TWOD_OPT_HEADER
#define TWOD_OPT_HEADER

typedef struct {
	int			Symbol_Index;    
	int			Sym_Size_Index;
	int			TwoD_Disp_Prec;
	int			Bg_Color_Index; 
	int			Py_Max;
	char			*Cmap_Dir;
	char			*Cmap_File;
	int			Show_Cmap;
	int			Cmap_Type_Index;
	int                     Active_Depth_Type;
	int			Depth_Coord_Index;
	double			Depth_Coord_Min;
	double			Depth_Coord_Max;
} TwoD_Opt_Win_Cntl_Ds_Objects;

typedef struct {
	TwoD_Opt_Win_Cntl_Ds_Objects	*TwoD_Opt_Win_Ds;
} TwoD_Opt_Ds_List_Item;

extern  TwoD_Opt_Ds_List_Item       **TwoD_Opt_Ds;

#endif
