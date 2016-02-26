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
#ifndef TWOD_OPT_DEF_HEADER
#define TWOD_OPT_DEF_HEADER

typedef struct {
	int			Symbol_Index;   /* first index into  Panel_Sym_Codes defined in symbols_def.h */    
	int			Sym_Size_Index; /* second index into  Panel_Sym_Codes */
	int			TwoD_Disp_Prec; /* unused ? Defaults Precision now available from postmaster */
	int			Bg_Color_Index; /* index of background color into the colormap segment */
	int			Py_Max;         /* unused ? Available in TwoD_Win_Cntl_Ds_Objects*/
	char			*Cmap_Dir;      /* directory for reading ColorTables */
	char			*Cmap_File;     /* File containing ColorTable */
	int			Show_Cmap;      /* Boolean with value 1 meaning colorbar should be 
						   drawn on window */
	int			Cmap_Type_Index;/* Colormap type of 0, 1, or 2 corresponding to Alt,Pick,or Depth */
	int                     Active_Depth_Type;/* Type of variable (PHASE_SPACE_VARB, PARAMETER_VARB, 
						     or FUNCTION_VARB) described by depth colormap. */
	int			Depth_Coord_Index;/* index of variable in depth colormap. */
	double			Depth_Coord_Min; /* minimum of range displayed of variable in depth colormap */
	double			Depth_Coord_Max; /* maximum of range displayed of variable in depth colormap  */
} TwoD_Opt_Win_Cntl_Ds_Objects;

typedef struct {
	TwoD_Opt_Win_Cntl_Ds_Objects	*TwoD_Opt_Win_Ds;
} TwoD_Opt_Ds_List_Item;

#endif
