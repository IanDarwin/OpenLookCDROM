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
#ifndef PM_MAIN_HEADER
#define PM_MAIN_HEADER


/* object and element labels */

#define	Traj_Ds_Object	1150 
#define		Varb_Ic		1151
#define		Varb_Fc 	1152
#define		Param		1153
#define		Varb_Dim	1154
#define		Param_Dim	1155
#define 	Varb_Names	1156
#define 	Varb_Max	1157
#define 	Varb_Min	1158
#define 	Param_Max	1159
#define 	Param_Min	1160
#define		Param_Names	1162
#define		Function	1163
#define		Function_Max	1164
#define		Function_Min	1165
#define		Function_Dim	1166
#define		Function_Names	1167

/* for Periodic panel */
#define Fixed_Ds_Object 1200
#define		Vf_Period	1201
#define         Map_Period      1202
#define 	Algorithm       1203
#define 	Guess           1204
#define 	Guess_Num       1205
#define 	Setting		1206
#define 	Found		1207
#define         FD_Step         1209
#define 	Dups		1210
#define         Num_Iters       1211
#define 	Var_Conv	1212
#define 	Funct_Conv	1213
#define 	Eigen_Dist	1214
#define 	Stab_Points	1215
#define 	Stab_Steps	1216
#define 	Unstab_Points	1217
#define 	Unstab_Steps	1218

#define Colormap_Object 1300
#define		Sys_Colormap_Size	1301
#define		Traj_Colormap_Size	1302
#define		Plotting_Symbol		1303
#define		Pick_Color_Choice	1304
#define		Red_Table		1305
#define		Green_Table		1306
#define		Blue_Table		1307

#define Flow_Control	1400
#define		Stepsize		1401
#define		Start_Save_Points	1403
#define		Total_Iterates		1404
#define		Skip_Size		1405
#define         Diverg_Cutoff           1406
#define         Direction               1407
/* #define         Multiple                1408 */
#define         Stopping_Condition      1409
#define         Varb_Events             1410
#define         Funct_Events            1411
#define         Varb_Event_Values       1412
#define         Funct_Event_Values      1413
#define		Final_Time		1415
#define		Interrupt 		1416
#define         P_Section               1417

#define	Mem_Obj_Cntl	1500
#define		Traj_Mem_Ptr		1501
#define		Fixed_Mem_Ptr		1502
#define		Sel_Pt_Mem_Ptr		1503
#define		Param_Mem_Ptr		1504
#define		Cont_Mem_Ptr		1505
#define         Mult_Mem_Ptr            1506

#define	Struct_Cntl	1600
#define 	Ds_Sel_Ptr		1601


#define Defaults        1700
#define 	Disp_Points             1701
#define 	Bgnd_Color              1702
#define 	Clipping                1703
#define 	Recording               1704
#define 	Def_Ctable_File         1705
#define 	Def_Symbol_Index        1706
#define 	Def_Size_Index          1707
#define 	Precision		1708
#define         Show_Color              1709
#define         Depth_Coordinate        1710
#define         Colormap_Type           1711

#define Cont_Control	1800
#define 	Cont_Stpsize		1801
#define 	Cont_Mode		1802
#define 	Cont_Iters		1803
#define 	Cont_Direction		1805
#define 	Cont_Param		1806
#define 	Cont_Vary_Switch	1807
#define 	Cont_Jac_Update		1808
#define 	Cont_Abserr		1809 
#define 	Cont_Relerr		1810 
#define 	Cont_Maxstp		1811
#define 	Cont_Minstp		1812
#define 	Cont_Target		1813
#define		Cont_Param_Fc		1814
#define		Cont_Fc			1815
#define		Cont_Search		1816 /* fjw 8/17/92 */
#define		Cont_Plot_Type		1817 /* fjw 9/27/92 */

#define	Model_Cntl	1900
/* #define		Category		1901 */
#define		System_Name    		1902
#define		DS_Def			1903
#define		Initialization		1904
#define		DfDx			1905
#define		DfDt			1906
#define		DfDparam		1907
#define		Inverse			1908
#define		Aux_Function		1909
#define		Mapping_Flag		1910
#define		Inverse_Flag		1911
#define		Jacobian_Flag		1912

#define	Manifold_Cntl	2000
#define		Type			2001
#define		Periodic_Varb		2002
#define		Period_Start		2003
#define		Period_End		2004

#define Mult_Cntl  2100
#define 	Mult_Load_Choice 	2101
#define 	Mult_Transformation 	2102
#define 	Mult_Trans_Param 	2103
#define 	Mult_Radius 		2104
#define 	Mult_Points 		2105
#define 	Mult_Total 		2106
#define 	Mult_Ic 		2107
#define 	Mult_Fc 		2108
#define 	Images 			2109
#define         Mult_Center             2110

#define Config_Cntl  2200
#define		Config_Filename		2201
#define		Batch_Input		2202
#define		Batch_Output		2203
#define         Windows_Flag		2204

#define Print_Cntl  2300
#define		Owner			2301 /* int window number "owning" the print panel */
#define		Title			2302 /* string title */
#define		Hor_Label		2303 /* string label */
#define		Ver_Label		2304 /* string label */
#define         File_Flag		2305 /* TRUE if printing to file; FALSE if printer */
#define         Color_Flag		2306 /* TRUE if color PostScript; FALSE if BW */
#define         Printer_Name		2307 /* string name of printer */
#define         Directory		2308 /* string name of directory to print to */
#define         Filename		2309 /* string name of file to print to */
#define         Show_Settings		2310 /* TRUE if all settings are showing */
#define         Show_Info		2311 /* TRUE if to print sysstem info on picture */
#define         PrintFont		2312 /* int index into one of (currently) 12 fonts */
#define         Title_Pt		2313 /* int size of title font (pts) */
#define         Label_Pt		2314 /* int size of label font (pts) */
#define         Landscape		2315 /* TRUE if picture rotated 90 degrees sideways */
#define         Show_BBox		2316 /* TRUE if bounding box printed */
#define         Num_Hor_Ticks		2317 /* int */
#define         Num_Ver_Ticks		2318 /* int */
#define         BBox_Hor_Length		2319 /* int length (in pts) of bounding box */
#define         BBox_Ver_Length		2320 /* int height (in pts) */
#define         BBox_Hor_Offset		2321 /* int displacement in pts from left side of page */
#define         BBox_Ver_Offset		2322 /* int displacement in pts from bottom of page */
#define         Connect			2323 /* TRUE draw line between points of (traj) mem obj */
#define         Label_Range		2324 /* TRUE put indication of range along axes */
#define         HorMin			2330 /* double min value of horizontal plot range */
#define         HorMax			2331 /* double max value of horizontal plot range */
#define         VerMin			2332 /* double min value of vertical plot range */
#define         VerMax			2333 /* double max value of vertical plot range */

#define Save_Cntl  2400
/* Directory =  string name of directory to save to */
/* Filename  =  string name of file to save to */
#define		Settings_Flag		2403 /* TRUE if settings are to be saved */
#define		Config_Flag		2404 /* TRUE if configuration is to be saved */
#define		Traj_Flag		2405 /* TRUE if trajectories are to be saved */
#define		Fixpt_Flag		2406 /* TRUE if fixed pts are to be saved */
#define		Cont_Flag		2407 /* TRUE if continuation data is to be saved */
#define		Param_Flag		2408 /* TRUE if parameter pts are to be saved */
#define		Select_Flag		2409 /* TRUE if selected pts are to be saved */
#define		Func_Flag		2410 /* TRUE if function values are to be saved */

#define Load_Cntl  2500
/* Directory =  string name of directory to read from */
/* Filename  =  string name of file to read from */
#define		Format_Flag		2503 /* TRUE if expect to read file in dstool format */
#define		Data_Type		2504 /* Returns element of Mem_Obj_Cntl corresponding to */
					     /* object to be loaded, eg,Traj_Mem_Ptr or Sel_Pt_Mem_Ptr */
/* Color_Flag = TRUE if data contains color information */
#define		Symbol_Flag		2506 /* TRUE if data contains symbol information */
#define		Varb_Index		2507 /* returns *int with i_th element TRUE if data has info about it */
#define		Param_Index		2508 /* returns *int with i_th element TRUE if data has info about it */


#endif
