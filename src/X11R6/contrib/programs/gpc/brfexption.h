/* $XConsortium: brfexption.h,v 5.1 91/02/16 10:07:22 rws Exp $ */

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
| Author        :	mfr / SimGraphics Engineering Corportation
|
| File          :	brfexception.h
| Date          :	2/13/20
| Project       :	PLB
|
| Descriptioni	:	Exception handler defines
|
| Status        :	Version 1.0
|
| Revisions     :	
|
|      12/90            MFC Tektronix, Inc.: PEX-SI PEX5R1 Release.
|
\*--------------------------------------------------------------------*/

/* ---------------------------------------------------------------------*\
| Local MACROS                                        
\*--------------------------------------------------------------------- */

#ifdef EXTERNALNOTE
	/* the following defines represent the remnants of an 
	early exception reporting system. The defines have been 
	left in the code along with a vestige 
	of the old system to track configuration dependant errors, which
	are the only exceptions which cannot be tagged with the
	tools in brf_except.c. */
#endif

#define POW2(n) (1<<(n))
#define NUM_GLOBAL_ERRORS 6


#define BIF_EX_NONSUP POW2(0) 		/* non-supported function */
#define BIF_EX_BADOPT POW2(1) 		/* non-supported option for this function */
#define BIF_EX_NOTRUECOLOR POW2(2) 	/* true color call in pseudo color mode */
#define BIF_EX_BUFFMODE POW2(3)     	/* user changed buffer mode */
#define BIF_EX_WINSIZE POW2(4)     	/* user changed window size */
#define BIF_EX_BACKCOLOR POW2(5)	/* user changed background color */

/* Note that there is a text string for each defined exception above */

static char *brf_exception_txt[] = 
{

"|  Non-supported function.                                             |\n",
"|  Non-supported funtion for this option.                              |\n",
"|  True Color called on Pseudo Color Config.                           |\n",
"|  User preference replaced BUFFER_MODE.                               |\n",
"|  User preference replaced WINDOW_SIZE.                               |\n",
"|  User preference replaced BACKGROUND_COLOR.                          |\n",
};

/* end old stuff */

#define PLB_EXCEPTION(plberror) \
{\
	plb_exception = plb_exception | plberror;\
} /* End macro PLB_EXCEPTION */

#define EXCEPTION(ent,plberror) \
{\
	ent->exception = ent->exception | plberror;\
} /* End macro EXCEPTION */

#define MAX_EXCEPTION 200



VGLOBAL int brf_num_ex_tests; /* number of exception tests we are doing.*/
VGLOBAL int num_exception; /* how many exception messages are stored */
/* newer exception handler stuff */

VGLOBAL int brf_num_ex_tests;/*number of known exceptions we are testing for*/

/* define the structure for the exception reporter */

#define RANGESTART 0
#define RANGESTOP 1
#define BRF_OK 0
#define BRF_WARNING 1
#define BRF_ERROR 2
#define BRF_FAILURE 3
#define BRF_UNBELIEVABLE_NIGHTMARE 4
#define BRF_TOTAL_DISASTER 5
#define NO_RANGE_DATA -999


#ifdef EXTERNALNOTE
	/* the following structure represents an exceptions test.
	The first 4 fields to which tests are applied.
	If a field is filled with a 0 or NULL, then the test
	does not apply to the entity. In the case of range
	data, where 0 and negative numbers are possible, the
	value of -999 or -999.0 are considered non-tested
	values. 
	The fifth field holds a degree of error value,
	which modifies the status of the exceptions output,
	and can control execution of the PLB i so desired.
	Feilds 6 and 7 hold a pointer to
	a custom testing routine, and a text message. Both are
	used if the above tests all test TRUE. brf_ex_numhits
	counts how many times that specific exception occured.
	brf_ex_logic contains logical operator codes to
	invert, or otherwise modify the actions of special handlers
	 */
#endif


typedef struct
{
        int     brf_ex_type; 		/* entity type */
        int     brf_ex_optional[7];	/* seperate exception flags */
        int     brf_ex_range[2];	/* Range integers */
        float   brf_ex_rrange[2];	/* Range floats */
        int     brf_ex_flag; 		/* degree of error */
        int     (*brf_ex_custom_handler)(); /* pointer to custom handler */
        char    *brf_ex_message;	/* text of message for this exception */
        int     brf_ex_logic;   	/* logical operator */
        int     brf_ex_numhits;  	/* number of hits */
} BRF_exception;






