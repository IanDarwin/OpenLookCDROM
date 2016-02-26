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
#include "pm_main.h"
#include <pm.h>
#include <saveload.h>

/* Keywords for DsTool 2.0 data files */

struct Key key[] = {
  {"#", COMMENT},
  {"pm", POSTMASTER},
  {"SET",SET}
};

int N_KEYWORD = sizeof(key)/sizeof(struct Key);

struct  Key pm_key[] = {
  {"EXEC", EXEC},
  {"PUT", PUT},
  {"GET", GET},
  {"PUT_LIST", PUT_LIST},
  {"GET_LIST", GET_LIST},
  {"CLEAR", CLEAR},
  {"INIT", INIT},
  {"CREATE_OBJ", CREATE_OBJ},
  {"CREATE_ELEM", CREATE_ELEM}
};

int N_PMKEY = sizeof(pm_key)/sizeof(struct Key);


/* OLD KEYWORDS for DsTool 1.x data files */

  struct Key old_key[] = {
    {"SYSTEM_NAME",System_Name},
    {"VARB_DIM",Varb_Dim},
    {"PARAM_DIM",Param_Dim},
    {"VARB_IC",Varb_Ic},
    {"VARB_FC",Varb_Fc},
    {"VARB_MIN",Varb_Min},
    {"VARB_MAX",Varb_Max},
    {"PARAM",Param},
    {"PARAM_MIN",Param_Min},
    {"PARAM_MAX",Param_Max}, 
    {"FUNCTION_DIM",Function_Dim},
    {"FUNCTION_MIN",Function_Min},
    {"FUNCTION_MAX",Function_Max},
    {"VF_PERIOD",Vf_Period},
    {"MAP_PERIOD",Map_Period},
    {"ALGORITHM", Algorithm},
    {"GUESS",Guess},
    {"GUESS_NUM", Guess_Num},
    {"SETTING", Setting},
    {"NUM_ITERS", Num_Iters},
    {"DUPS", Dups},
    {"VAR_CONV", Var_Conv},
    {"FUNCT_CONV",Funct_Conv},
    {"FD_STEP", FD_Step},
    {"EIGEN_DIST", Eigen_Dist},
    {"STAB_POINTS",Stab_Points},
    {"STAB_STEPS",Stab_Steps},
    {"UNSTAB_POINTS",Unstab_Points},
    {"UNSTAB_STEPS",Unstab_Steps},
    {"START_SAVE_POINTS",Start_Save_Points},
    {"TOTAL_ITERATES", Total_Iterates},
    {"SKIP_SIZE", Skip_Size},
    {"STEPSIZE", Stepsize},
    {"STOPPING_CONDITION", Stopping_Condition},
    {"P_SECTION", P_Section},
    {"DIVERG_CUTOFF", Diverg_Cutoff},
    {"FINAL_TIME", Final_Time},
    {"VARB_EVENTS",Varb_Events},
    {"FUNCT_EVENTS",Funct_Events},
    {"VARB_EVENT_VALUES",Varb_Event_Values},
    {"FUNCT_EVENT_VALUES",Funct_Event_Values},
    {"DISP_POINTS",Disp_Points},
    {"CLIPPING",Clipping},
    {"RECORDING",Recording},
    {"DEF_SYMBOL_INDEX",Def_Symbol_Index},
    {"DEF_SIZE_INDEX",Def_Size_Index},
    {"PRECISION",Precision},
    {"MULT_LOAD_CHOICE",Mult_Load_Choice},
    {"MULT_TRANSFORMATION", Mult_Transformation},
    {"IMAGES", Images},
    {"MULT_TRANS_PARAM",Mult_Trans_Param},
    {"MULT_POINTS",Mult_Points},
    {"MULT_RADIUS",Mult_Radius},
    {"NEW_TRAJ",Traj_Mem_Ptr},
    {"NEW_FXPT",Fixed_Mem_Ptr},
    {"NEW_CONT",Cont_Mem_Ptr},
    {"NEW_PARAM",Param_Mem_Ptr},
    {"NEW_SEL_PT",Sel_Pt_Mem_Ptr},
    {"NEW_MULT",Mult_Mem_Ptr},
    {"NEW_FUNC",NEW_FUNC},
    {"CONFIGURATION",CONFIGURATION},
    {"COLOR_ON",COLOR_ON},
    {"SYMBOL_ON",SYMBOL_ON}
  };

int N_OLDKEYWORD = sizeof(old_key)/sizeof(struct Key);

