/* $XConsortium: globals.h,v 5.3 93/01/04 18:24:06 hersh Exp $ */

/*
 */
/*--------------------------------------------------------------------*\
|
|  Copyright (C) 1989,1990, 1991, National Computer Graphics Association
|
|  Permission is granted to any individual or institution to use, copy, or
|  redistribute this software so long as it is not sold for profit, provided
|  this copyright notice is retained.
|
|                         Developed for the
|                National Computer Graphics Association
|                         2722 Merrilee Drive
|                         Fairfax, VA  22031
|                           (703) 698-9600
|
|                                by
|                 SimGraphics Engineering Corporation
|                    1137 Huntington Drive  Unit A
|                      South Pasadena, CA  91030
|                           (213) 255-0900
|---------------------------------------------------------------------
|
| Author        :	SimGraphics Engineering Corportation
|
| File          :	globals.h
| Date          :	Fri Feb  9 10:46:55 PST 1990
| Project       :	PLB
| Description   :	
| Status        :	Version 1.0
|
| Revisions     :	
|
|      12/90            MFC Tektronix, Inc.: PEX-SI PEX5R1 Release.
|
\*--------------------------------------------------------------------*/
#ifndef MAIN_FUNCTION

#define VGLOBAL extern
#define GLOBAL_INIT(v)

#else

#define VGLOBAL 
#define GLOBAL_INIT(v) = v

#endif

/* ************************************************************ */ 
#define MATRIX_TABLE_SIZE 256
#define MATRIX_STACK_SIZE 64
/* WORKING: This should be dynamically set based on the actual size */
#define VIEW_TABLE_SIZE wk_info.view_size
#define VIEW_TB_SIZE 256

VGLOBAL int matrix_stack_depth GLOBAL_INIT(0);
VGLOBAL float matrix_stack[MATRIX_STACK_SIZE][4][4];
VGLOBAL float matrix_table[MATRIX_TABLE_SIZE][4][4];
VGLOBAL float vo_matrix_public[VIEW_TB_SIZE][4][4];
VGLOBAL float vm_matrix_public[VIEW_TB_SIZE][4][4];

VGLOBAL BIF_Traverser_state bif_base_state;
VGLOBAL BIF_Traverser_state *traverser_state GLOBAL_INIT(&bif_base_state);
VGLOBAL int base_state_stid;

#ifdef EXTERNALNOTE
	/* The following line adds a global variable to hold a bitwise
	encoded record of datafile and hardware exceptions generated during
	the plb run. The defines for the errors are in bifmacro.h along
	with macros for insertion where the exceptions occur. */
#endif
/* global hardware and datafile exception variable. */
VGLOBAL int plb_exception;

typedef struct
{
	int	workid;
	int border_stid;
	int base_state_stid;
	int test_stid;
	int nrs_stid;
	int border_vid;
	int base_state_vid;
} Bench_setup;

typedef struct
{
    	int	phigs_open;
	int	cmap_size;
	int     dcue_size;
	int     light_size;
	int     view_size;
	float	aspect_ratio;
	int color_model;
	int color_mode;
	int buffer_mode;
	int x;
	int y;
} Wk_info;

	/* The following globals apply to the timer code. struct timeb is
	declared in sys/timeb.h  */

#include <sys/types.h>
#ifdef NOTREQUIRED
#include <sys/timeb.h>

VGLOBAL struct timeb StartTime;
VGLOBAL struct timeb StopTime;
VGLOBAL struct timeb LapTime;
#endif /* NOTREQUIRED */

#define RESERVED_BASE 21400
#ifdef MAIN_FUNCTION
VGLOBAL Bench_setup bench_setup =
{
	1,			/* Workstation ID		*/
	RESERVED_BASE,		/* Display Border Structure	*/
	RESERVED_BASE + 1,	/* Base State structure		*/
	RESERVED_BASE + 2,	/* Test Loop structure		*/
	RESERVED_BASE + 3,	/* "Non-retained" structure	*/
 	0,
	0
};
VGLOBAL Wk_info wk_info = 
{
    	0,			/* Phigs Open State            */
        0,			/* Color Map Size              */
	20,			/* Depth Cue Table Entries     */
	8,			/* Light Table Entries         */
	VIEW_TB_SIZE,		/* View Table Entries	       */
	1,			/* Aspect Ratio                */
	0,			/* Color Model                 */
	0,			/* Color Mode                  */
        0,			/* Buffer Mode                 */
	900,720			/* Window Size                 */
};
#else
VGLOBAL Bench_setup bench_setup;
VGLOBAL Wk_info     wk_info;
#endif /* MAIN_FUNCTION */

#ifdef USING_PHIGS
VGLOBAL int user_background;
VGLOBAL int Argc;
VGLOBAL char **Argv;

#ifdef MAIN_FUNCTION
VGLOBAL XrmOptionDescRec opTable[] = {
    {"-display",	".display",	XrmoptionSepArg, (caddr_t)NULL},
    {"-geometry",	".geometry",	XrmoptionSepArg, (caddr_t)NULL},
    {"-bd",		".borderColor",	XrmoptionSepArg, (caddr_t)NULL},
    {"-bg",		".background", 	XrmoptionSepArg, (caddr_t)NULL},
    {"-bw",		".borderWidth",	XrmoptionSepArg, (caddr_t)NULL},
    {"-hlhsr",		".hlhsrMode",	XrmoptionSepArg, (caddr_t)NULL},
    {"-buff",		".bufMode",	XrmoptionSepArg, (caddr_t)NULL},
};
#else
VGLOBAL XrmOptionDescRec opTable[];
#endif /* MAIN_FUNCTION */

#define NUM_OPTIONS (sizeof(opTable) / sizeof(opTable[0]))
VGLOBAL XrmDatabase gpc_res_db;
#endif /* USING_PHIGS */

VGLOBAL char *Prog_name;
VGLOBAL slList nrs_link;
