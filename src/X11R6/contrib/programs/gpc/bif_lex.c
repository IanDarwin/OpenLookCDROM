/* $XConsortium: bif_lex.c,v 5.5 94/04/17 20:44:17 eswu Exp $ */
/*

Copyright (c) 1989, 1990, 1991  X Consortium

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the X Consortium shall
not be used in advertising or otherwise to promote the sale, use or
other dealings in this Software without prior written authorization
from the X Consortium.

*/
/***********************************************************
Copyright (c) 1989,1990, 1991 by Sun Microsystems, Inc.

						All Rights Reserved

Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in
supporting documentation, and that Sun Microsystems
not be used in advertising or publicity
pertaining to distribution of the software without specific, written
prior permission.

SUN MICROSYSTEMS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT
SHALL SUN MICROSYSTEMS BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

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
| Author        :	mjf / SimGraphics Engineering Corportation
|
| File          :	bif_lex.c
| Date          :	3/12/89
| Project       :	GPC / PLB
| Description   :	
| Status	:	Version 1.0
|
|			Most lex parsing is working.
|			NOTE: The number parser does not currently check
|			for errors created by the misuse of + and - such
|			as ++3+45.3e+8+9
|
| Revisions     :	
|
|       2/90            MFC Tektronix, Inc.: PEX-SI API implementation.
|
|       5/90            MFC Tektronix, Inc.: PEX-SI API Binding change.
|
|      12/90            MFC Tektronix, Inc.: PEX-SI PEX5R1 Release.
|
\*--------------------------------------------------------------------*/

/*---------------------------------------------------------------------*\
|Include files
\*---------------------------------------------------------------------*/
#define MAIN_FUNCTION
#define REMOVE_UNDERSCORES
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#define STRLENGTH 80
typedef char StringType[STRLENGTH];
#include "biftypes.h"
#include "bifparse.h"
#include "globals.h"
#include "ph_map.h"
#include "brfexption.h"
#ifdef USING_PHIGS
#include <X11/Xlib.h>
#include <X11/Xresource.h>
#endif /* USING_PHIGS */

#ifdef STAND_ALONE
#include "bifparse.h"
#endif /* ifdef STAND_ALONE */
/*---------------------------------------------------------------------*\
|Local #define
\*---------------------------------------------------------------------*/
#ifndef FALSE
#define FALSE 0
#endif /* ifndef FALSE */
#ifndef TRUE
#define TRUE (!FALSE)
#endif /* ifndef TRUE */

#define EXCEPTION_HANDLER 1

#define bNONE -1/* Guaranteed not to be a character type                */
#define bEF 0   /* end of file value  (not EOF)                         */
#define bNL 1   /* end of line                                          */
#define bSP 2   /* white space                                          */
#define bCT 3   /* Comment mark   "%"                                   */
#define bDQ 4   /* Double quote                                         */
#define bSQ 5   /* Single quote                                         */
#define bOB 6   /* Open   bracket "{"                                   */
#define bCB 7   /* Closed bracket "}"                                   */
#define bSC 8   /* Semi colen     ";"                                   */
#define bEe 9   /* Cap "E" or low "e"                                   */
#define bAI 10  /* ASCII printable except any called out seperately     */
#define bNM 11  /* Numbers        "0" to "9" and "."                    */
#define bOT 12  /* Eight bit extended charactors                        */
#define bOP 13  /* Open perentasy                                       */
#define bCP 14  /* Close perantisy                                      */
#define bCC 15  /* Unused controle codes                                */
#define bTOP 16 /* One grater then the last character group             */

#define NORMAL_TERMINATION 0 /* as defined in yacc documentation */
#define UNRECOVERABLE 1      /* as defined in yacc documentation */
#define bEOF  0
#ifdef STAND_ALONE
/* NOTE: yylval is defined by yacc when not STAND_ALONE */

#define LONG    255
#define REAL    254
#define QSTRING  253
#define READ_GEOMETRY_FILE     252
#define BEGIN_TEST             251
#define END_TEST               250
#define CALL_STRUCTURE         249
#define MARKER                 248
#define UNRECOGNIZED_KEYWORD   247
#ifndef BUFFERLENGTH
#define BUFFERLENGTH 255
#endif /* ifndef BUFFERLENGTH */
#endif /* ifdef STAND_ALONE */


/* ---------------------------------------------------------------------*\
| Local MACROS                                                           |
\*--------------------------------------------------------------------- */
#define CLOSE_ST_TOKEN { \
val_str[indx++] = '\0';\
yylex_info->yacc_pack_not_done = FALSE;\
yylex_info->last_char_unused   = TRUE;}

#define CLOSE_FL_TOKEN \
CLOSE_ST_TOKEN;\
sscanf(val_str,"%lf",&temp_float);\
*val_float = temp_float;


#define CLOSE_LI_TOKEN \
CLOSE_ST_TOKEN;\
sscanf(val_str,"%ld",&temp_long);\
*val_long = temp_long;

/* ---------------------------------------------------------------------*\
| Local global variables                                                 |
\*--------------------------------------------------------------------- */
typedef void VoidFunc();
typedef VoidFunc *VoidFuncPtr;
typedef int IntFunc();
typedef IntFunc *IntFuncPtr;
typedef struct
{
	int     kw_token;
	char    *kw_string;
} Keyword_table;

#ifdef STAND_ALONE
/* NOTE: yylval is defined by yacc when not STAND_ALONE */
typedef char StringType[BUFFERLENGTH];
typedef union   {
	long            l;
	double          f;
	StringType      str;
	} YYSTYPE;
YYSTYPE yylval;
#endif /* ifdef STAND_ALONE */

typedef struct
{
	int     ch1, ch1_group, yacc_pack_not_done,
		new_yacc_pack,  last_char_unused,
		return_code,    init_flag, line_no;
	VoidFuncPtr last_manager;
	char    *filenm;
	YYSTYPE *yylval;
} Yylex_info;

int	    workid;

FILE *active_file, *last_file = NULL;
char input_filenm[128], last_filenm[128];
int lineno,last_lineno;
/* ---------------------------------------------------------------------*\
| Contents:                                                              |
\*--------------------------------------------------------------------- */
#ifdef FULL_FUNCT_PROTO_TYPE
/* ---------------------------------------------------------------------*/
int yyparse(void);
void main(int,char**);
int yylex(void);
void start_yacc_pack    (Yylex_info*);
void continue_yacc_pack (Yylex_info*);
void eof_manager        (Yylex_info*);
void keyw_manager       (Yylex_info*);
void string_manager     (Yylex_info*);
void number_manager     (Yylex_info*);
void comment_manager    (Yylex_info*);
void line_manager       (Yylex_info*);
void space_manager      (Yylex_info*);
void junk_manager       (Yylex_info*);
int  find_keyword       (char*);
/* ---------------------------------------------------------------------*/
#else /* ifdef FULL_FUNCT_PROTO_TYPE */
/* ---------------------------------------------------------------------*/
int yyparse();
void main();
int yylex();
void start_yacc_pack();
void continue_yacc_pack();
void eof_manager();
void keyw_manager();
void string_manager();
void number_manager();
void comment_manager();
void line_manager();
void space_manager();
void junk_manager();
/* ---------------------------------------------------------------------*/
#endif /* ifdef FULL_FUNCT_PROTO_TYPE */


/* ---------------------------------------------------------------------*\
| BEGIN PROCEDURE CODE                                                   |
\*--------------------------------------------------------------------- */

Usage()
{
    fprintf(stderr,
	    "usage:\n        %s [-options ...] verb_file_name(s)\n\n",
	    Prog_name);
    fprintf(stderr,"where options include:\n");
#ifdef USING_PHIGS
    fprintf(stderr,
	    "    -display displaystring         X server to connect to\n");
    fprintf(stderr,
	    "    -geometry geomstring           size and location\n");
    fprintf(stderr,
	    "    -bd color                      border color\n");
    fprintf(stderr,
	    "    -bg color                      background color\n");
    fprintf(stderr,
	    "    -bw pixels                     border width\n");
    fprintf(stderr,
	    "    -hlhsr mode                    one of the following:\n");
    fprintf(stderr,
	    "                            NONE ZBUFF PAINTERS SCANLINE LINE\n");
    fprintf(stderr,
	    "    -buff mode                     one of the following:\n");
    fprintf(stderr,
	    "                                       SINGLE DOUBLE\n");
#endif /* USING_PHIGS */
    fprintf(stderr,
	    "    -h                             this message\n");
    exit(1);
}

    
/*----------------------------------------------------------------------*\
| Procedure: void main(int,char**);                                      |
|------------------------------------------------------------------------|
| Description:  Opens file(s) passed in argv.  Calls yyparse onec for each
|		file. 
|------------------------------------------------------------------------|
| Return:  0 when complete with all inupt                                |
|          anything else when not complete with input                    |
\*----------------------------------------------------------------------*/
void main(argc,argv)
int argc;
char **argv;
{
        int i,exit_code;

	printf("\n");
        exit_code = NORMAL_TERMINATION;
	if (Prog_name = strrchr(argv[0],'/'))
	    Prog_name++;
	else 
	    Prog_name = argv[0];

#ifdef USING_PHIGS
	Argc = argc;  /* Save original copies for init_bench */
	Argv = argv;
	XrmInitialize();
	XrmParseCommand(&gpc_res_db, opTable, NUM_OPTIONS, Prog_name,
			&argc, argv);
#endif /* USING_PHIGS */

#ifdef EXTERNALNOTE
        /* added for exception reporting */
#endif
#ifdef EXCEPTION_HANDLER
        BRF_define_exceptions();        /* load exception tables */
        plb_exception = 0; /* init the global error variable */
        num_exception= 0; /* init the global error counter */
#endif

	active_file = (FILE *)NULL;
	for(i=1;i<argc;i++)
	{
	    char *arg = argv[i];
	    
	    if (arg[0] == '-') {
		switch (arg[1]) {
		  case 'h':
		    Usage();
		  default:
		    Usage();
		}
	    }

	    active_file = fopen(argv[i],"r");
	    lineno = 1;
	    if (active_file == NULL )
	    {
		fprintf(stderr,
			"FATAL ERROR: unable to open file %s for reading\n",
			argv[i]);
		exit_code = UNRECOVERABLE;
		break;
	    } else
	    {
		strcpy(input_filenm,argv[i]); 
		if ( (exit_code = yyparse()) ==
		    UNRECOVERABLE )
		{
		    break;
		}
	    }
	}
	if (!active_file && (exit_code == NORMAL_TERMINATION))
	    Usage();
	bif_closewk();
	exit(exit_code);
}

/*----------------------------------------------------------------------*\
| Procedure: int yylex(void)                                             |
|------------------------------------------------------------------------|
| Description:  Performs lexical analysis for the file pointed to by the
|		global variable active_file. 
|------------------------------------------------------------------------|
| Return:  0 when complete with all inupt                                |
|          anything else is a token for yyparse.  A listings of all tokens
|	   can be found in bifparse.y
\*----------------------------------------------------------------------*/
int yylex()
{
static int char_table[256] = {
bSP,bSP,bSP,bSP,  bSP,bSP,bSP,bSP,  bSP,bSP,bNL,bNL,  bNL,bNL,bSP,bSP,
bSP,bSP,bSP,bSP,  bSP,bSP,bSP,bSP,  bSP,bSP,bSP,bSP,  bSP,bSP,bSP,bSP,
bSP,bAI,bDQ,bAI,  bAI,bCT,bAI,bSQ,  bOP,bCP,bAI,bNM,  bAI,bNM,bNM,bAI,
bNM,bNM,bNM,bNM,  bNM,bNM,bNM,bNM,  bNM,bNM,bAI,bSC,  bAI,bAI,bAI,bAI,

bAI,bAI,bAI,bAI,  bAI,bEe,bAI,bAI,  bAI,bAI,bAI,bAI,  bAI,bAI,bAI,bAI,
bAI,bAI,bAI,bAI,  bAI,bAI,bAI,bAI,  bAI,bAI,bAI,bAI,  bAI,bAI,bAI,bAI,
bAI,bAI,bAI,bAI,  bAI,bEe,bAI,bAI,  bAI,bAI,bAI,bAI,  bAI,bAI,bAI,bAI,
bAI,bAI,bAI,bAI,  bAI,bAI,bAI,bAI,  bAI,bAI,bAI,bOB,  bAI,bCB,bAI,bOT,

bOT,bOT,bOT,bOT,  bOT,bOT,bOT,bOT,  bOT,bOT,bOT,bOT,  bOT,bOT,bOT,bOT,
bOT,bOT,bOT,bOT,  bOT,bOT,bOT,bOT,  bOT,bOT,bOT,bOT,  bOT,bOT,bOT,bOT,
bOT,bOT,bOT,bOT,  bOT,bOT,bOT,bOT,  bOT,bOT,bOT,bOT,  bOT,bOT,bOT,bOT,
bOT,bOT,bOT,bOT,  bOT,bOT,bOT,bOT,  bOT,bOT,bOT,bOT,  bOT,bOT,bOT,bOT,

bOT,bOT,bOT,bOT,  bOT,bOT,bOT,bOT,  bOT,bOT,bOT,bOT,  bOT,bOT,bOT,bOT,
bOT,bOT,bOT,bOT,  bOT,bOT,bOT,bOT,  bOT,bOT,bOT,bOT,  bOT,bOT,bOT,bOT,
bOT,bOT,bOT,bOT,  bOT,bOT,bOT,bOT,  bOT,bOT,bOT,bOT,  bOT,bOT,bOT,bOT,
bOT,bOT,bOT,bOT,  bOT,bOT,bOT,bOT,  bOT,bOT,bOT,bOT,  bOT,bOT,bOT,bOT  };

	static Yylex_info yylex_info =
	{
		0,  0,  0,
		0,  0,
		0,  0,  0,
		NULL,
		"no_file",
		&yylval
	};
	static first_time_used = TRUE;

	if (first_time_used)
	{
		first_time_used = FALSE;
		yylex_info.return_code        = FIRST_FILE;
	} else
	{
		yylex_info.last_char_unused    = FALSE;
		yylex_info.new_yacc_pack        = TRUE;
		yylex_info.yacc_pack_not_done   = TRUE;
		yylex_info.return_code          = 1;
		yylex_info.line_no = lineno;
		yylex_info.filenm  = input_filenm;
		while ( yylex_info.yacc_pack_not_done )
		{
			yylex_info.last_char_unused    = FALSE;
			yylex_info.ch1 = fgetc(active_file);
#ifdef EXTERNALNOTE
					/* WORKING: active_file needs more
				   	intigration to develope the stack
				   	workings and interactions of the
				   	*_yack_pack procedures */
#endif /* ifdef EXTERNALNOTE */
			if ( yylex_info.ch1 == EOF ) yylex_info.ch1_group = bEF;
			else
			yylex_info.ch1_group = char_table[yylex_info.ch1];
			if (yylex_info.new_yacc_pack)
			{
				start_yacc_pack(    &yylex_info );
			}
			else
			{
				continue_yacc_pack( &yylex_info );
			}
			if (yylex_info.last_char_unused)
			{
				ungetc(yylex_info.ch1,active_file);
			}
		}
	
		lineno = yylex_info.line_no;
	}
	return(yylex_info.return_code);
}

/*----------------------------------------------------------------------*\
| Procedure: void start_yacc_pack(Yylex_info*)
|------------------------------------------------------------------------|
| Description: Calls the appropriate token manager if no token manager is
|		currently active.
|------------------------------------------------------------------------|
| Return:  void                                                          |
\*----------------------------------------------------------------------*/
void start_yacc_pack( yylex_info )
Yylex_info *yylex_info;
{

static VoidFuncPtr maniger_table[] =
{
eof_manager    ,/* end of file                                          */
line_manager   ,/* end of line                                          */
space_manager  ,/* white space                                          */
comment_manager,/* Comment mark   "%"                                   */
string_manager ,/* Double quote                                         */
string_manager ,/* Single quote                                         */
keyw_manager   ,/* Open   bracket "{"                                   */
keyw_manager   ,/* Closed bracket "}"                                   */
keyw_manager   ,/* Semi colen     ";"                                   */
keyw_manager   ,/* Cap "E" or low "e"                                   */
keyw_manager   ,/* ASCII printable except any called out seperately     */
number_manager ,/* Numbers        "0" to "9" and "."                    */
junk_manager   ,/* Eight bit extended charactors                        */
keyw_manager   ,/* Open perentasy                                       */
keyw_manager   ,/* Close perantisy                                      */
junk_manager    /* Unused controle codes                                */
};

VoidFuncPtr last_manager;

	yylex_info->new_yacc_pack = FALSE;
	yylex_info->init_flag = TRUE;
	last_manager = maniger_table[yylex_info->ch1_group];
	(*last_manager)( yylex_info );
	yylex_info->last_manager = last_manager;
}

/*----------------------------------------------------------------------*\
| Procedure: void continue_yacc_pack(Yylex_info*)                        |
|------------------------------------------------------------------------|
| Description: Calls the active token manager
|------------------------------------------------------------------------|
| Return:  void                                                          |
\*----------------------------------------------------------------------*/
void continue_yacc_pack( yylex_info )
Yylex_info *yylex_info;
{
VoidFuncPtr last_manager;

	yylex_info->init_flag = FALSE;
	last_manager = yylex_info->last_manager;
	(*last_manager)( yylex_info );
}

/*----------------------------------------------------------------------*\
| Procedure: void eof_manager(Yylex_info*)                               |
|------------------------------------------------------------------------|
| Description:                                                           |
|------------------------------------------------------------------------|
| Return:  void                                                          |
\*----------------------------------------------------------------------*/
/*----------------------------------------------------------------------*\
| WORKING: This routine currently alters global variables directly.  This
|	    will be corrected in future releases.
\*----------------------------------------------------------------------*/
void eof_manager( yylex_info )
Yylex_info *yylex_info;
{
	if ( last_file == NULL )
	{
		yylex_info->yacc_pack_not_done = FALSE;
		yylex_info->return_code = bEOF;
	} else
	{
		if ( fclose(active_file) != 0 )
		{
			fprintf(stderr,"Error: closing file %s\n",input_filenm);
			exit(-1);
		}
		active_file = last_file;
		last_file   = NULL;
		yylex_info->line_no = last_lineno;
		strcpy(yylex_info->filenm,last_filenm); 
		yylex_info->yacc_pack_not_done = FALSE;
		yylex_info->return_code = END_GEOM_FILE;
	}
}
/*----------------------------------------------------------------------*\
| Procedure: void keyw_manager(Yylex_info*)                              |
|------------------------------------------------------------------------|
| Description:                                                           |
|------------------------------------------------------------------------|
| Return:  void                                                          |
\*----------------------------------------------------------------------*/
void keyw_manager( yylex_info )
Yylex_info *yylex_info;
{
static indx;
char *val_str;
	val_str = yylex_info->yylval->str;
	if (yylex_info->init_flag == TRUE)
	{
		switch(yylex_info->ch1_group)
		{
		case bOB :  /* Open   bracket "{"      */
		case bCB :  /* Closed bracket "}"      */
		case bSC :  /* Semi colen     ";"      */
			val_str[0] = (char)yylex_info->ch1;
			val_str[1] = '\0';
			yylex_info->return_code    = yylex_info->ch1;
			yylex_info->yacc_pack_not_done = FALSE;
			break;
		case bEe :  /* Cap "E" or low "e"      */
		case bAI :  /* ASCII printable ...     */
		case bOP :  /* Open perentasy          */
		case bCP :  /* Close perantisy         */
			indx=0;
			val_str[indx++] = (char)yylex_info->ch1;
			break;
		default  :
			yylex_info->new_yacc_pack = TRUE;
			fprintf(stderr,
				"Program logic error in keyw_manager\n");
#ifdef EXTERNALNOTE
		Possible place for branch to exception handler.
#endif /* ifdef EXTERNALNOTE */
			break;
		}
	} else
	{
		switch(yylex_info->ch1_group)
		{
		case bEF :  /* end of file             */
		case bNL :  /* end of line             */
		case bSP :  /* white space             */
		case bCT :  /* Comment mark   "%"      */
		case bDQ :  /* Double quote            */
		case bSQ :  /* Single quote            */
		case bOB :  /* Open   bracket "{"      */
		case bCB :  /* Closed bracket "}"      */
		case bSC :  /* Semi colen     ";"      */
		case bOT :  /* Eight bit extended      */
		case bCC :  /* Unused controle codes   */
			CLOSE_ST_TOKEN;
			yylex_info->return_code =
				find_keyword(val_str);
			break;

		case bEe :  /* Cap "E" or low "e"      */
		case bAI :  /* ASCII printable ...     */
		case bNM :  /* Numbers "0"-"9" and "." */
		case bOP :  /* Open perentasy          */
		case bCP :  /* Close perantisy         */
			val_str[indx++] = (char)yylex_info->ch1;
			break;
		default  :
			fprintf(stderr,
				"Program logic error in keyw_manager\n");
#ifdef EXTERNALNOTE
		Possible place for branch to exception handler.
#endif /* ifdef EXTERNALNOTE */
			break;
		}
	}
}
/*----------------------------------------------------------------------*\
| Procedure: void string_manager(Yylex_info*)                            |
|------------------------------------------------------------------------|
| Description:                                                           |
|------------------------------------------------------------------------|
| Return:  void                                                          |
\*----------------------------------------------------------------------*/
void string_manager( yylex_info )
Yylex_info *yylex_info;
{
static indx;
static start_qtype;
static status;
char *val_str;
	val_str = yylex_info->yylval->str;
	if (yylex_info->init_flag == TRUE)
	{
		switch(yylex_info->ch1_group)
		{
		case bDQ :  /* Double quote            */
			start_qtype = bDQ;
			status = bNONE;
			indx=0;
			break;
		case bSQ :  /* Single quote            */
			start_qtype = bSQ;
			status = bNONE;
			indx=0;
			break;
		default  :
			yylex_info->new_yacc_pack = TRUE;
			fprintf(stderr,
				"Program logic error in string_manager\n");
#ifdef EXTERNALNOTE
		Possible place for branch to exception handler.
#endif /* ifdef EXTERNALNOTE */
			break;
		}
	} else
	{
		switch(yylex_info->ch1_group)
		{
		case bEF :  /* end of file             */
			if (status == bNONE)
			{
				fprintf(stderr,
				 "WARNING: %s:%d: EOF encountered while a string was open\n",
				 yylex_info->filenm, yylex_info->line_no);
			}
			CLOSE_ST_TOKEN;
			yylex_info->return_code        = QSTRING;
			break;
		case bNL :  /* end of line             */
		case bSP :  /* white space             */
		case bCT :  /* Comment mark   "%"      */
		case bOB :  /* Open   bracket "{"      */
		case bCB :  /* Closed bracket "}"      */
		case bSC :  /* Semi colen     ";"      */
		case bEe :  /* Cap "E" or low "e"      */
		case bAI :  /* ASCII printable ...     */
		case bNM :  /* Numbers "0"-"9" and "." */
		case bOP :  /* Open perentasy          */
		case bCP :  /* Close perantisy         */
			if (status == bNONE)
			{
				if (yylex_info->ch1_group == bNL)
					yylex_info->line_no++;
				val_str[indx++] = (char)yylex_info->ch1;
			}else
			{
				CLOSE_ST_TOKEN;
				yylex_info->return_code        = QSTRING;
			}
			break;

		case bOT :  /* Eight bit extended      */
		case bCC :  /* Unused controle codes   */
			if (status == bNONE)
			{
				fprintf(stderr,
				 "WARNING: %s:%d: Unprintable characters in string ignored\n",
				 yylex_info->filenm,yylex_info->line_no);
			}else
			{
				CLOSE_ST_TOKEN;
				yylex_info->return_code        = QSTRING;
			}
			break;

		case bDQ :  /* Double quote            */
			switch(status)
			{
			case bNONE :
				if(start_qtype==bDQ) status = bDQ;
				else
				   val_str[indx++] = (char)yylex_info->ch1;
				break;
			case bDQ   :
				status = bNONE;
				val_str[indx++] = (char)yylex_info->ch1;
				break;
			case bSQ   :
				CLOSE_ST_TOKEN;
				yylex_info->return_code        = QSTRING;
				break;
			}
			break;
		case bSQ :  /* Single quote            */
			switch(status)
			{
			case bNONE :
				if(start_qtype==bSQ) status = bSQ;
				else
				   val_str[indx++] = (char)yylex_info->ch1;
				break;
			case bSQ   :
				status = bNONE;
				val_str[indx++] = (char)yylex_info->ch1;
				break;
			case bDQ   :
				CLOSE_ST_TOKEN;
				yylex_info->return_code        = QSTRING;
				break;
			}
			break;
		default  :
			fprintf(stderr,"Program logic error in keyw_manager\n");
#ifdef EXTERNALNOTE
		Possible place for branch to exception handler.
#endif /* ifdef EXTERNALNOTE */
			break;
		}
	}
}
/*----------------------------------------------------------------------*\
| Procedure: void number_manager(Yylex_info*)                            |
|------------------------------------------------------------------------|
| Description:                                                           |
|------------------------------------------------------------------------|
| Return:  void                                                          |
\*----------------------------------------------------------------------*/
void number_manager( yylex_info )
Yylex_info *yylex_info;
{
static indx;
static status;
char *val_str;
double *val_float, temp_float;
long   *val_long,  temp_long;
	val_long  = &(yylex_info->yylval->l);
	val_float = &(yylex_info->yylval->f);
	val_str = yylex_info->yylval->str;
	if (yylex_info->init_flag == TRUE)
	{
		if( yylex_info->ch1_group == bNM )
		{
			indx = 0;
			status = bNM;
			if( yylex_info->ch1 == '.' ) status = bTOP;
			val_str[indx++] = (char)yylex_info->ch1;
		}else
		{
			yylex_info->new_yacc_pack = TRUE;
			fprintf(stderr,"Program logic error in number_manager\n");
#ifdef EXTERNALNOTE
		Possible place for branch to exception handler.
#endif /* ifdef EXTERNALNOTE */
		}
	} else
	{
		switch(yylex_info->ch1_group)
		{
		case bEe :  /* Cap "E" or low "e"      */
			if (status == bEe)
			{
				fprintf(stderr,
				 "WARNING: %s:%d: Invalid number second attempt at an exponent ignored\n",
				 yylex_info->filenm,yylex_info->line_no);
				CLOSE_FL_TOKEN;
				yylex_info->return_code        = REAL;
			} else
			{
				status = bEe;
				val_str[indx++] = (char)yylex_info->ch1;
			}
			break;
		case bNM :  /* Numbers "0"-"9" and "." */
			switch(status)
			{
			case bTOP:
				if( yylex_info->ch1 == '.' )
				{
					fprintf(stderr,
					 "WARNING: %s:%d: Invalid number second attempt at a decimal point ignored\n",
					 yylex_info->filenm,yylex_info->line_no);
					CLOSE_FL_TOKEN;
				} else
				val_str[indx++] = (char)yylex_info->ch1;
				break;
			case bEe:
				if( yylex_info->ch1 == '.' )
				{
					fprintf(stderr,
					 "WARNING: %s:%d: Invalid number decimal point after exponent ignored\n",
					 yylex_info->filenm,yylex_info->line_no);
					CLOSE_FL_TOKEN;
				} else
				val_str[indx++] = (char)yylex_info->ch1;
				break;
			default:
				if( yylex_info->ch1 == '.' ) status = bTOP;
				val_str[indx++] = (char)yylex_info->ch1;
				break;
			}
			break;
		default  :
			if (status == bNM)
			{
				CLOSE_LI_TOKEN;
				yylex_info->return_code        = LONG;
			} else
			{
				CLOSE_FL_TOKEN;
				yylex_info->return_code        = REAL;
			}
			break;
		}
	}
}
/*----------------------------------------------------------------------*\
| Procedure: void comment_manager(Yylex_info*)                           |
|------------------------------------------------------------------------|
| Description:                                                           |
|------------------------------------------------------------------------|
| Return:  void                                                          |
\*----------------------------------------------------------------------*/
void comment_manager( yylex_info )
Yylex_info *yylex_info;
{
	if (yylex_info->init_flag == TRUE)
	{
		if(yylex_info->ch1_group != bCT)
		{
			yylex_info->new_yacc_pack = TRUE;
			fprintf(stderr,"Program logic error in comment_manager\n");
#ifdef EXTERNALNOTE
		Possible place for branch to exception handler.
#endif /* ifdef EXTERNALNOTE */
		}
	} else
	{
		switch(yylex_info->ch1_group)
		{
		case bEF:
			fprintf(stderr,
			 "WARNING: %s:%d: EOF encountered while a comment was open\n",
			 yylex_info->filenm, yylex_info->line_no);
			yylex_info->last_char_unused   = TRUE;
			yylex_info->new_yacc_pack = TRUE;
			break;
		case bNL:
			yylex_info->line_no++;
			break;
		case bCT:
			yylex_info->new_yacc_pack = TRUE;
			break;
		}
	}
}
/*----------------------------------------------------------------------*\
| Procedure: void line_manager(Yylex_info*)                              |
|------------------------------------------------------------------------|
| Description:                                                           |
|------------------------------------------------------------------------|
| Return:  void                                                          |
\*----------------------------------------------------------------------*/
void line_manager( yylex_info )
Yylex_info *yylex_info;
{
	yylex_info->line_no++;
	yylex_info->new_yacc_pack = TRUE;
}
/*----------------------------------------------------------------------*\
| Procedure: void space_manager(Yylex_info*)                             |
|------------------------------------------------------------------------|
| Description:                                                           |
|------------------------------------------------------------------------|
| Return:  void                                                          |
\*----------------------------------------------------------------------*/
void space_manager( yylex_info )
Yylex_info *yylex_info;
{
	yylex_info->new_yacc_pack = TRUE;
}

/*----------------------------------------------------------------------*\
| Procedure: void junk_manager(Yylex_info*)                              |
|------------------------------------------------------------------------|
| Description:                                                           |
|------------------------------------------------------------------------|
| Return:  void                                                          |
\*----------------------------------------------------------------------*/
void junk_manager( yylex_info )
Yylex_info *yylex_info;
{
	yylex_info->new_yacc_pack = TRUE;
}

static Keyword_table keyword_table[] =
{
	/*----------------------------------------------------------------*\
	|	Structures
	\*----------------------------------------------------------------*/
	BEGIN_STRUCTURE,"BEGINSTRUCTURE",
	END_STRUCTURE,"ENDSTRUCTURE",

	/*----------------------------------------------------------------*\
	|	Graphics Primitives
	\*----------------------------------------------------------------*/
	LABEL,"LABEL",
	MARKER,"MARKER",
	MARKER3,"MARKER3",
	LINE,"LINE",
	LINE3,"LINE3",
	POLYGON,"POLYGON",
	POLYGON3,"POLYGON3",
	FILL_AREA_SET,"FILLAREASET",
	FILL_AREA_SET3,"FILLAREASET3",
	TRIANGLE3,"TRIANGLE3",
	QUAD_MESH3,"QUADMESH3",
	INDEX_POLYGONS3,"INDEXPOLYGONS3",
	GEN_SPHERE3,"GENSPHERE3",
	GEN_CIRCLE,"GENCIRCLE",
	GEN_CIRCLE3,"GENCIRCLE3",
	TEXT,"TEXT",
	TEXT3,"TEXT3",
	ANNOTATION_TEXT3,"ANNOTATIONTEXT3",
	PIXEL_MAP3,"PIXELMAP3",
	NON_UNIFORM_BSPLINE_CURVE,"NONUNIFORMBSPLINECURVE",       		/* ver 1.0 */
	NON_UNIFORM_BSPLINE_SURFACE,"NONUNIFORMBSPLINESURFACE",     		/* ver 1.0 */

	/*----------------------------------------------------------------*\
	|	Primitive Attributes
	\*----------------------------------------------------------------*/
	CURVE_APPROXIMATION_CRITERIA,"CURVEAPPROXIMATIONCRITERIA",			/* ver 1.0 */
	TRIMCURVE_APPROXIMATION_CRITERIA,"TRIMCURVEAPPROXIMATIONCRITERIA",		/* ver 1.0 */
	SURFACE_APPROXIMATION_CRITERIA,"SURFACEAPPROXIMATIONCRITERIA",		/* ver 1.0 */
	MARKER_TYPE,"MARKERTYPE",
	MARKER_SIZE,"MARKERSIZE",
	MARKER_COLOR,"MARKERCOLOR",
	MARKER_COLOR_INDEX,"MARKERCOLORINDEX",
	LINE_TYPE,"LINETYPE",
	LINE_WIDTH,"LINEWIDTH",
	LINE_COLOR,"LINECOLOR",
	LINE_COLOR_INDEX,"LINECOLORINDEX",
	LINE_SHADING,"LINESHADING",
	INTERIOR_STYLE,"INTERIORSTYLE",
	INTERIOR_PATTERN_INDEX,"INTERIORPATTERNINDEX",
	INTERIOR_COLOR,"INTERIORCOLOR",
	INTERIOR_COLOR_INDEX,"INTERIORCOLORINDEX",
	BACKFACE_INTERIOR_COLOR,"BACKFACEINTERIORCOLOR",
	BACKFACE_INTERIOR_COLOR_INDEX,"BACKFACEINTERIORCOLORINDEX",
	INTERIOR_SHADING,"INTERIORSHADING",
	INTERIOR_LIGHTING,"INTERIORLIGHTING",
	SURFACE_PROPERTIES,"SURFACEPROPERTIES",
	BACKFACE_PROPERTIES,"BACKFACEPROPERTIES",
	BACKFACE_PROCESSING,"BACKFACEPROCESSING",
	EDGE_FLAG,"EDGEFLAG",
	EDGE_TYPE,"EDGETYPE",
	EDGE_WIDTH,"EDGEWIDTH",
	EDGE_COLOR,"EDGECOLOR",
	EDGE_COLOR_INDEX,"EDGECOLORINDEX",
	TEXT_FONT,"TEXTFONT",
	TEXT_PREC,"TEXTPREC",
	TEXT_COLOR,"TEXTCOLOR",
	TEXT_COLOR_INDEX,"TEXTCOLORINDEX",
	TEXT_PATH,"TEXTPATH",
	TEXT_ALIGN,"TEXTALIGN",
	CHAR_HEIGHT,"CHARHEIGHT",
	CHAR_EXP,"CHAREXP",
	CHAR_SPACE,"CHARSPACE",
	CHAR_UP_VECTOR,"CHARUPVECTOR",
	ANNO_TEXT_CHAR_HEIGHT,"ANNOTEXTCHARHEIGHT",
	ANNO_TEXT_CHAR_UP_VECTOR,"ANNOTEXTCHARUPVECTOR",
	ANNO_TEXT_STYLE,"ANNOTEXTSTYLE",

	/*----------------------------------------------------------------*\
	|	Rendering Attributes
	\*----------------------------------------------------------------*/
	LIGHT_STATE,"LIGHTSTATE",
	DEPTHCUE_INDEX,"DEPTHCUEINDEX",
	HLHS_REMOVAL,"HLHSREMOVAL",

	/*----------------------------------------------------------------*\
	|	Matrix Manipulation Entities
	\*----------------------------------------------------------------*/
	IDENTITY3,"IDENTITY3",
	CONCAT_MATRIX3,"CONCATMATRIX3",
	INVERT_MATRIX3,"INVERTMATRIX3",
	ROTATE3,"ROTATE3",
	ROTATE_XYZ3,"ROTATEXYZ3",
	TRANSLATE3,"TRANSLATE3",
	SCALE3,"SCALE3",
	MATRIX3,"MATRIX3",
	GET_MATRIX3,"GETMATRIX3",
	PUSH_MATRIX3,"PUSHMATRIX3",
	POP_MATRIX3,"POPMATRIX3",
	GLOBAL_TRANSFORMATION3,"GLOBALTRANSFORMATION3",
	LOCAL_TRANSFORMATION3,"LOCALTRANSFORMATION3",
	APPLY_TO_GLOBAL3,"APPLYTOGLOBAL3",
	APPLY_TO_LOCAL3,"APPLYTOLOCAL3",
	VIEW_ORIENTATION3,"VIEWORIENTATION3",
	VIEW_MAPPING3,"VIEWMAPPING3",
	ACTIVE_VIEW,"ACTIVEVIEW",

	/*----------------------------------------------------------------*\
	|	Structure Hierarchy
	\*----------------------------------------------------------------*/
	EXECUTE_STRUCTURE,"EXECUTESTRUCTURE",
	CALL_STRUCTURE,"CALLSTRUCTURE",

	/*----------------------------------------------------------------*\
	|	Verb File Entities
	\*----------------------------------------------------------------*/
	READ_GEOMETRY_FILE,"READGEOMETRYFILE",
	CLEAR_GEOMETRY,"CLEARGEOMETRY",
	BEGIN_TEST,"BEGINTEST",
	END_TEST,"ENDTEST",
	PAUSE,"PAUSE",
	SLEEP,"SLEEP",
	INVOKE_AT_FRAME,"INVOKEATFRAME",
	DEFINE_COLOR,"DEFINECOLOR",
	BACKGROUND_COLOR,"BACKGROUNDCOLOR",
	BACKGROUND_COLOR_INDEX,"BACKGROUNDCOLORINDEX",
	DEFINE_VIEW_SPECIFICATION,"DEFINEVIEWSPECIFICATION",
	DEFAULT_VIEW_SPECIFICATION,"DEFAULTVIEWSPECIFICATION",
	DEFINE_LIGHT,"DEFINELIGHT",
	DEFINE_DEPTHCUE,"DEFINEDEPTHCUE",
	CONFIGURATION,"CONFIGURATION",

	/*----------------------------------------------------------------*\
	|	Additional Keywords
	\*----------------------------------------------------------------*/
	KNOTS,"KNOTS",					/* ver 1.0 */
	CTRL_POINTS,"CTRLPOINTS",				/* ver 1.0 */
	UKNOTS,"UKNOTS",				/* ver 1.0 */
	VKNOTS,"VKNOTS",				/* ver 1.0 */
	TRIMLOOP,"TRIMLOOP",				/* ver 1.0 */
	TRIMCURVE,"TRIMCURVE",				/* ver 1.0 */
	RATIONAL,"RATIONAL",				/* ver 1.0 */
	NON_RATIONAL,"NONRATIONAL",				/* ver 1.0 */
	IGNORE_GROUP,"IGNOREGROUP",
	VERTEX_COLORS,"VERTEXCOLORS",
	VERTEX_NORMALS,"VERTEXNORMALS",
	VERTEX_COLOR_INDICES,"VERTEXCOLORINDICES",
	FACET_COLORS,"FACETCOLORS",
	FACET_NORMALS,"FACETNORMALS",
	FACET_COLOR_INDICES,"FACETCOLORINDICES",
	VERTEX_COORDINATES,"VERTEXCOORDINATES",
	EDGE_VISIBILITY,"EDGEVISIBILITY",
	ENABLE,"ENABLE",
	DISABLE,"DISABLE",
	STRING,"STRING",
	CHAR,"CHAR",
	STROKE,"STROKE",
	WORLD,"WORLD",
	MODELLING,"MODELLING",
	AMBIENT_LIGHT,"AMBIENTLIGHT",
	DIRECTIONAL_LIGHT,"DIRECTIONALLIGHT",
	POSITIONAL_LIGHT,"POSITIONALLIGHT",
	SPOT_LIGHT,"SPOTLIGHT",
	LD_TRANSFORM,"LDTRANSFORM",
	ACTIVATE_LIST,"ACTIVATELIST",
	DEACTIVATE_LIST,"DEACTIVATELIST",
	HLHSRID,"HLHSRID",
	HLHS_DISABLE,"HLHSDISABLE",
	HLHS_ENABLE,"HLHSENABLE",
	MAKE_RAMP,"MAKERAMP",
	COLOR_LIST,"COLORLIST",
	RGB,"RGB",
	CIE,"CIE",
	HSV,"HSV",
	HLS,"HLS",
	PRECONCAT,"PRECONCAT",
	POSTCONCAT,"POSTCONCAT",
	REPLACE,"REPLACE",
	X_AXIS,"XAXIS",
	Y_AXIS,"YAXIS",
	Z_AXIS,"ZAXIS",
	PERSPECTIVE,"PERSPECTIVE",
	PARALLEL,"PARALLEL",
	MATCH_VIEW_AREA,"MATCHVIEWAREA",
	ADJUST_X,"ADJUSTX",
	ADJUST_Y,"ADJUSTY",
	GROW,"GROW",
	SHRINK,"SHRINK",
	XY_CLIP,"XYCLIP",
	NO_XY_CLIP,"NOXYCLIP",
	FRONT_CLIP,"FRONTCLIP",
	NO_FRONT_CLIP,"NOFRONTCLIP",
	BACK_CLIP,"BACKCLIP",
	NO_BACK_CLIP,"NOBACKCLIP",
	HOLLOW,"HOLLOW",
	SOLID,"SOLID",
	PATTERN,"PATTERN",
	EMPTY,"EMPTY",
	VIEW_MAPPING,"VIEWMAPPING",
	VIEW_ORIENTATION,"VIEWORIENTATION",
	GLOBAL_MODELLING,"GLOBALMODELLING",
	LOCAL_MODELLING,"LOCALMODELLING",
	COMPOSITE_MODELLING,"COMPOSITEMODELLING",
	SPECIFY_REPORT_FILE,"SPECIFYREPORTFILE",
	TO,"TO",
	END,"END",
	EXECUTE,"EXECUTE",
	CALL,"CALL",
	WINDOW_SIZE,"WINDOWSIZE",
	DOUBLE_BUFFER,"DOUBLEBUFFER",
	SINGLE_BUFFER,"SINGLEBUFFER",
	TRUE_COLOR,"TRUECOLOR",
	PSEUDO_COLOR,"PSEUDOCOLOR",
	FACET_COLOR_INDICES,"FACETCOLORINDICES",
	FACET_CONNECTIVITY,"FACETCONNECTIVITY",
	EXACT,"EXACT",
	CENTER,"CENTER",
	RADIUS,"RADIUS",
	SCALE_FACTORS,"SCALEFACTORS",
	NORMAL,"NORMAL",
	TEXT_DIRECTION,"TEXTDIRECTION",
	ADD,"ADD",
	AND,"AND",
	CLEAR,"CLEAR",
	INVERT,"INVERT",
	NAND,"NAND",
	NOOP,"NOOP",
	NOR,"NOR",
	OR,"OR",
	SET,"SET",
	SUBTRACT_DEST,"SUBTRACTDEST",
	SUBTRACT_SOURCE,"SUBTRACTSOURCE",
	XOR,"XOR",
	PIXEL_VALUES,"PIXELVALUES",
	INTENSITY_VALUES,"INTENSITYVALUES",
	PIXEL_VALUE_SEGMENTS,"PIXELVALUESEGMENTS",
	INTENSITY_VALUE_SEGMENTS,"INTENSITYVALUESEGMENTS"
};
static kw_table_size = sizeof(keyword_table)/sizeof(Keyword_table);


/*--------------------------------------------------------------------*\
| Procedure: int find_keyword(char*)                                  
|---------------------------------------------------------------------
| Description: Finds a key word by matching strings in an arrayed struct
|              with str_word the corresponding keyword token is returned
|---------------------------------------------------------------------
| Return:  0 if ok  1 if error                                        
\*--------------------------------------------------------------------*/
int  find_keyword(str_word)
char *str_word;
{
int i, return_value;
int  str_length, i_target;
char tmp_word[STRLENGTH];

	str_length =  strlen(str_word);
	i_target = 0;
	for(i=0;i<str_length;i++)
	{
		if ( islower(str_word[i]) )
		{
			/*--------------------------------------------*\
			|	The SUN version of toupper ONLY works in
			|	the input IS LOWER CASE (Ugh!)
			\*--------------------------------------------*/
			tmp_word[i_target] = toupper(str_word[i]);
		}
		else
			tmp_word[i_target] = str_word[i];

#ifdef REMOVE_UNDERSCORES
		if (tmp_word[i_target] != '_') i_target++;
#else /* ifdef REMOVE_UNDERSCORES */
		i_target++;
#endif /* ifdef REMOVE_UNDERSCORES */
	}
	tmp_word[i_target] = '\0';
	return_value = UNRECOGNIZED_KEYWORD;
	for(i=0;i<kw_table_size;i++)
	{
		if (!strcmp(tmp_word, keyword_table[i].kw_string))
		{
			return_value = keyword_table[i].kw_token;
			break;
		}
	}
	return(return_value);
}

/*--------------------------------------------------------------------*\
| Procedure: char *find_keyword_token(int)                                  
|---------------------------------------------------------------------
| Description: Finds a keyword string associated with a given token
|---------------------------------------------------------------------
| Return:  The pointer to the keyword string
\*--------------------------------------------------------------------*/
char *find_keyword_token(token)
BIF_INT token ;
{
	int i;
	char *return_value;
	return_value = "UNRECOGNIZED_KEYWORD";
	for(i=0;i<kw_table_size;i++)
	{
		if (token == keyword_table[i].kw_token )
		{
			return_value = keyword_table[i].kw_string;
			break;
		}
	}
	return(return_value);
}

/*----------------------------------------------------------------------*\
| Procedure: int bif_readgeom(qBIFfile)
|------------------------------------------------------------------------|
| Description: Receive a READ_GEOMETRY_FILE entity from the parser
|		qBIFfile	Name of geometry file to read
|------------------------------------------------------------------------|
| Return: Error Code
\*----------------------------------------------------------------------*/

int bif_readgeom(qBIFfile)
char *qBIFfile;
{
	printf("READ_GEOMETRY_FILE : (%s) \n",qBIFfile);
	last_file = active_file;
	strcpy(last_filenm,input_filenm); 
	strcpy(input_filenm,qBIFfile); 

	active_file = fopen(qBIFfile,"r");
	last_lineno = lineno;
	lineno = 1;
	if (active_file == NULL )
	{
	   fprintf(stderr,
	      "FATAL ERROR: unable to open file (%s) for reading\n",
	      qBIFfile);
	   fprintf(stderr,"Called from (%s) on line (%d)\n",
	      last_filenm,last_lineno);
	   exit(-1);
	} 
} /* End procedure bif_readgeom */



/*----------------------------------------------------------------------*\
| Procedure: void yyerror(char *)
|------------------------------------------------------------------------|
| Description: Receives error messages form yyparse and prints them to
|		stderr
|------------------------------------------------------------------------|
| Return: Error Code
\*----------------------------------------------------------------------*/

void yyerror(s)
char *s;
{
	fflush(stdout);
	fflush(stderr);
	fprintf(stderr,"(%s:Line %d) %s\n",input_filenm,lineno,s);
	fflush(stderr);
}

/*----------------------------------------------------------------------*\
| Procedure: void bif_openwk()
|------------------------------------------------------------------------|
| Description: Opens up the workstation as well as setting up 
|		the default structure
|------------------------------------------------------------------------|
| Return: Error Code
\*----------------------------------------------------------------------*/
bif_openwk()
{
	workid = bench_setup.workid;
	base_state_stid = bench_setup.base_state_stid;

	init_bench(&bench_setup,&wk_info);

#ifdef TEST_PRINT
	printf(
	  "The size of the color map is %d, the aspect_ratio is %f\n",
	  wk_info.cmap_size,
	  wk_info.aspect_ratio);
#endif /* ifdef TEST_PRINT */

#ifdef USING_PHIGS
	/*------------------------------------------------------------*\
	|	Open and initialize the Non-retatined structure.
	|	The Non-retatined structure is an unposted black hole
	|	for elements that live outside of a test-loop. It is
	|	cleared each test-loop.
	\*------------------------------------------------------------*/
	fxopns();
	pcopy_all_elems_struct((Pint)bench_setup.base_state_stid);
#endif /* USING_PHIGS */
	/*------------------------------------------------------------*\
	|	Initialize the BIF traverser_state.
	\*------------------------------------------------------------*/
	do_endstructure(traverser_state,NULL);
	traverser_state->nrs_state           = 1;
	traverser_state->push_level          = 0;
	traverser_state->id_active_view      = 0;
	mx_identity(vm_matrix_public[traverser_state->id_active_view]);
	mx_identity(vo_matrix_public[traverser_state->id_active_view]);
	traverser_state->currentFrame        = 0;
#ifndef REFER_STRUCTURE_EXISTS
	traverser_state->tol		     = &nrs_link;
	traverser_state->eol		     = &nrs_link;
	traverser_state->tol->next	     = NULL;
	traverser_state->tol->data	     = bench_setup.nrs_stid;
#endif /* REFER_STRUCTURE_EXISTS */
	mx_identity(traverser_state->global    );
	mx_identity(traverser_state->local     );
	mx_identity(traverser_state->composite );
}

/*----------------------------------------------------------------------*\
| Procedure: void bif_closwk()
|------------------------------------------------------------------------|
| Description: WORKING: closes up the workstation as well as setting up 
|		the default structure
|------------------------------------------------------------------------|
| Return: Error Code
\*----------------------------------------------------------------------*/
bif_closewk()
{
#ifdef USING_PHIGS
#ifdef TEST_PRINT
    fprintf(stderr,"Closing window %d\n",workid);
    fflush(stderr);
#endif /* ifdef TEST_PRINT */
    /* Close the non-retained structure */
    if(wk_info.phigs_open) {
	pclose_struct();
	pclose_ws((Pint)workid);
	pclose_phigs();
    }
#endif /* USING_PHIGS */
}

#ifdef STAND_ALONE
/*----------------------------------------------------------------------*\
| Procedure: int yyparse(void)                                           |
|------------------------------------------------------------------------|
| Description: Temporary until attached to yacc                          |
|------------------------------------------------------------------------|
| Return:  0 if ok  1 if error                                           |
\*----------------------------------------------------------------------*/
int yyparse()
{
int return_value;
	while ( (return_value = yylex()) != 0)
	{
		printf("return_token is %d its value is ",return_value);
		switch(return_value)
		{
		case UNRECOVERABLE   :
			printf(" UNRECOVERABLE\n");
			break;
		case bEOF            :
			printf(" END OF FILE 1\n");
			break;
		case REAL           :
			printf(" the float %le\n",yylval.f);
			break;
		case LONG            :
			printf(" the long  %ld\n",yylval.l);
			break;
		case QSTRING          :
			printf(" the string '%s'\n",yylval.str);
			break;
		default              :
			printf(" some default value\n");
			break;
		}
	}
		printf("return_token is %d its value is ",return_value);
		printf(" END OF FILE 2\n");
	return(0);
}
#endif /* STAND_ALONE */
