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
#ifndef CONSTANTS_HEADER
#define CONSTANTS_HEADER

#include <values.h>

#include <pm.h>

#define WINDOWS_MODE 1
#define TCL_MODE     2
#define PVM_SLAVE_MODE 3

#define		TRUE		1
#define		FALSE		0

/* used to here - paw */


#define		PI		M_PI
#define		TWOPI		(2*M_PI)

#define		MAX_LEN_DS_TITLE	80
#define		MAX_LEN_VARB_NAME	20
#define 	MAX_INT_STR 		12
#define 	MAX_DOUBLE_STR 		30
#define 	MAX_SHORT_STR 		31
#define 	MAX_LONG_STR 		255

#define MAX_PRECISION 17  /* for displaying double data in printf */



#define		MAX_COLORS		256
#define		PHASE_SPACE_VARB	100
#define		PARAMETER_VARB		101
#define		FUNCTION_VARB		102
#define		CBAR_CELL_WIDTH		7


/* default directory labels */
#define DSTOOL_DIR              101
#define DSTOOL_COLOR_DIR        102
#define DSTOOL_DATA_DIR         103
#define LPDEST          	104
#define PRINTER         	105
#define DSTOOL_PS_PROLOG 	106


/* general error conditions */
#define MINOR_ERROR 1
#define NO_ERROR 0
#define MAJOR_ERROR -1


/* constants for window types */
/* BASE_WINDOW is xview type FRAME */
#define BASE_WINDOW 10
/* POPUP_WINDOW is xview type FRAME_CMD */
#define POPUP_WINDOW 11

/* constants for the Memory object */
#define         COLOR_DIM               3

/* constants for the Memory object types */
#define		TRAJ_MEMORY		1
#define		PARAM_MEMORY		2
#define		FIXPT_MEMORY		3
#define		CONT_MEMORY		4
#define		SEL_PT_MEMORY		5
#define         MULT_MEMORY             6

/* constants for the symbol objects */
#define		NUM_SYM_SIZES 		4 
#define 	NUM_SYM_TYPES 		4

#define		SMALL_POINT		1
#define		MED_POINT		2
#define		LARGE_POINT		3
#define		HUGE_POINT		4
#define		SMALL_TRI		5
#define		MED_TRI			6
#define		LARGE_TRI		7
#define		HUGE_TRI		8
#define		SMALL_CROSS		9
#define		MED_CROSS		10
#define		LARGE_CROSS		11
#define		HUGE_CROSS		12
#define		SMALL_BOX		13
#define		MED_BOX		 	14
#define		LARGE_BOX		15
#define		HUGE_BOX		16

/* constants for the system colors */
#define         SYS_RED                 3
#define         SYS_GREEN               4
#define         SYS_BLUE                5

/* constants for the complib subsection */
#define		FORWARD		1
#define		BACKWARD	2
#define		CONTINUE        0  
#define         WORKSPACE       20
#define		PROP_NSTEP	11
#define		PROP_FSTOP	12
#define		PROP_TF   	13
#define		FIXED_STEP	14
#define		VARB_STEP	15
#define		PROP_POINCARE	16		/* paw  4/14/92 */
#define		FIXED_STEP_INIT	17
#define		VARB_STEP_INIT	18
#define		DEFAULT_INTEGRATOR	0
#define 	ANALYTIC        101
#define 	FORW_DIFF       102
#define 	CEN_DIFF        103
#define 	MONTE_CARLO     150
#define 	APPROX_INV      151
#define 	EXPLICIT_INV    152
#define		PM		153
#define		MP		154
#define		PM_and_MP	155
#define		PRE_STEP	156
#define		SOLVE_STEP	157
#define		POST_STEP	158

/* constants for propagation panel fields */
#define		FINITE_DIFFERENCE_STEP_STRING		"Finite Diff Step: "
#define		STOPPING_ERROR_STRING			"Stopping Error: "

/* keys for the windowing system */
#define         MODEL_MENU_ITEM_KEY     123
#define         TWOD_WINDOW_ITEM_KEY    124
#define         INT_MENU_ITEM_KEY       125
#define		VARB_ITEM_KEY		126

/* labels for the data structure objects */
#define		TWOD_DS			223

/* labels for twoD window handles */
#define		CANVAS_HANDLE		301
#define		CANVAS_PW_HANDLE	302
#define		CBAR_CANVAS_HANDLE	303
#define		CBAR_PW_HANDLE		304
#define		CBAR_LT_HANDLE		305
#define		CBAR_RT_HANDLE		306
#define		CBAR_LT_PAN_HANDLE	307
#define		CBAR_RT_PAN_HANDLE	308
#define		PANEL_HANDLE		309
#define		WINDOW_HANDLE		310

/* constants for the configuration of windows */
#define		DEFAULT_WIN_CONFIG	401
#define		SET_WIN_CONFIG		402

/* for I/O */
#define         SIZE_OF_DIR_PLUS_FNAME  240
#define         SIZE_OF_FNAME           80
#define         SIZE_OF_GEN_INPUT       80

/* for FIXED point stuff */
#define         NEWTON                  0
#define         SECANT                  1
#define         SADDLE                  0
#define         SOURCE                  1
#define         SINK                   -1
#define         SPIRAL_SOURCE           2
#define         SPIRAL_SINK            -2
#define         STAB_MAN                10
#define         UNSTAB_MAN             -10
#define         INDETERMINATE           5

#endif
