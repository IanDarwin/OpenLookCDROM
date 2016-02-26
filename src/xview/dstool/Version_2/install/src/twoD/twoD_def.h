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
#ifndef TWOD_DEF_HEADER
#define TWOD_DEF_HEADER


typedef struct {
	GC			Canvas_Gc; /* Gc for drawing in main display area */
	GC			Cbar_Gc; /* Gc for drawing in colorbar */
	Cms			Colormap_Cms; /* Unused ; cms now a static variable in set_colorsegment*/
	int			ColorTable[MAX_COLORS]; /* Colormap; index of a color_choice into  CMS */
                                /* Sys_Colors and Traj_Colors are pointers  to different parts of the index table 
				   of one CMS. */
        unsigned long           *TwoD_Sys_Colors; /* Index table from cms logical indices to pixel values for 
						    system use. An entry here determines a colorcell in the 
						    X11 colormap. Sys_Colors[i] is a pixel value.*/
	unsigned long           *TwoD_Traj_Colors; /* index table from cms logical indices to  pixel values for 
						     trajectory use; an entry here determines a colorcell in the
						     X11 colormap. Traj_Colors[i] is a pixel value */
	int                     Total_Sys_Colors; /* Total number of system colors in CMS */
	int                     Total_Traj_Colors; /* Total number of traj colors in CMS */
	int			Cbar_Index; /* index in colortable of color used in leftmost colorbar */
	int			Color_Choice; /* unused ? - Pick_Color_Choice available from postmaster as 
						 index into colortable */
	char                    *Panel_Color; /* unused ? Panel_Color_Choice list of strings now used to set 
						 background colors of panels. */
	int			P_Origin_X;   /*  minimum pixel x in canvas */
	int			P_Origin_Y;   /*  minimum pixel y in canvas */
	int			Px_Max;       /*  maximum pixel x in canvas */
	int			Py_Max;       /*  maximum pixel y in canvas */
	double			Hor_Min;      /* Minimum real value of horiz coordinate */
	double			Hor_Max;      /* Maximum real value of horiz coordinate */
	double			Vert_Min;     /* Minimum real value of vert coordinate */
	double			Vert_Max;     /* Maximum real value of vert coordinate */
	int			Active_Hor_Type; /* type of variable of horizontal  coordinate; 
						    PHASE_SPACE_VARB, PARAMETER_VARB, or FUNCTION_VARB */
	int			Active_Ver_Type; /* type of variable of vertical coordinate */
	int			Active_Hor_Index; /* index of horizontal coordinate in its list of variables.*/
	int			Active_Ver_Index; /* index of vertical coordinate in its list of variables */
} TwoD_Win_Cntl_Ds_Objects;

typedef struct {
	TwoD_Win_Cntl_Ds_Objects	*TwoD_Win_Ds;
} TwoD_Ds_List_Item;

char    *Panel_Color_Choice[]={"Light Blue","Wheat","Medium Aquamarine",
			       "Thistle","Tan"};
int     Num_Panel_Colors = 5;

#endif
