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
#define PM_OBJECT   100
#define PM_ELEMENT  101

#define MAX_LABEL_LEN 40

#define PM_SETUP_ERROR 1001 

#define INT             1050
#define INT_LIST        1052
#define DBL             1054
#define DBL_LIST        1056
#define STRNG           1058
#define STRNG_LIST      1060
#define ADDRS           1062
#define MEMRY           1064
/*#define FNCT            1130 */
#define FNCT            1066 

#define PUT             1120
#define GET             1121
#define INIT            1122
#define CLEAR           1123
#define PUT_LIST        1124
#define GET_LIST        1125
#define CREATE_OBJ      1126
#define RM_OBJ          1127
#define CREATE_ELEM     1128
#define RM_ELEM         1129
/*#define EXEC            1066*/
#define EXEC            1130
#define GET_SAVABLE	1131
#define PUT_SAVABLE	1132

/* values for savable field of a postamaster object or element */
#define SAVE_NONE 0		/* never save */
#define SAVE_CONFIG 1		/* save when config selected */
#define SAVE_SETTINGS 2		/* save when settings selected */

/* Postmaster Error Flags */
#define NULL_ADDRESS_ERROR 2
#define VARB_INITIALIZED_ERROR 1
#define MEMORY_ALLOC_ERROR -1
#define MULTIPLE_GET_ERROR -2
#define INAPT_OP_ERROR -4
#define LIST_NOT_ALLOC_ERROR -5
#define INDEX_BOUNDS_ERROR -6
#define RESET_POINTER_ERROR -7

extern void *pm();
extern void *pm_new();
