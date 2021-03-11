/* $XConsortium: brf_trv.c,v 5.2 94/04/17 20:44:31 rws Exp $ */
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
Copyright(c) 1989,1990, 1991 by Sun Microsystems, Inc.

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
| Author        :	SimGraphics Engineering Corportation
|
| File          :	brf_trv.c
| Date          :	Fri Feb  9 10:46:55 PST 1990
| Project       :	PLB
| Description   :	Traverses the BRF structures, collecting usage
|			information on each bif element, and generates
|			the BRF report.
| Status        :	Version 1.0
|
| Revisions     :	
|
|       5/90            MFC Tektronix, Inc.: PEX-SI API Binding change.
|
|      12/90            MFC Tektronix, Inc.: PEX-SI PEX5R1 Release.
|
\*--------------------------------------------------------------------*/

/*--------------------------------------------------------------------*\
|	Table of Contents
|
|	void brf_traverser(	*BIF_Traverser_state, *BIF_All, *BRF_state)
|		:	Walks throuth the BRF structures built for a 
|					givren test
|	void brf_branch (	*BIF_Traverser_state, *BIF_All, *BRF_state)
|		:	NOTE: An improvement to this code would be to add a
|	void brf_report (	*BRF_state )
|		:	Main routine involved in printing out a BRF report
|	void brf_init (	)
|		:	Initialized BRF tables
|	void brf_start_stopwatch ()
|		:	Starts the stop watch
|	void brf_stop_stopwatch ( *BRF_state )
|		:	Stops the stop watch
|
\*--------------------------------------------------------------------*/

#define STRLENGTH 80
#include <stdio.h>
#ifdef SYSV
#include <sys/types.h>
#include <sys/times.h>
#include <time.h>
#endif
#include <sys/time.h>
#include "brf_ents.h"
#include "biftypes.h"
#include "bifmacro.h"
#include "bifparse.h"
#include "brftypes.h"
#include "brf_set.h"
#include "globals.h"
#include "ph_map.h"
#include "brfexption.h"
#include "stopwatch.h"

void brf_branch ();
void brf_report ();
void brf_init ();
extern	char brf_filename[STRLENGTH];
extern	FILE *brf_file;
extern	int  brf_used_flag;
extern  char input_filenm[128];
extern BRF_exception brf_exception[MAX_EXCEPTION];

/*----------------------------------------------------------------------*\
| Program: brf_trv.c
|
\*----------------------------------------------------------------------*/


#define EXCEPTION_HANDLER 1
#ifdef EXTERNALNOTE


	ATTENTION END PORT PROGRAMMERS!!!!!!!!!!!!

  	  Part of the mechanism for the exceptions reporting is inside this
	code, marked with "#ifdef EXCEPTION_HANDLER". These are the global
	plb exception messages, as defined in bifmacro.h and set in bldgeneric.c
	and bifbuild.c. Use this code as an example of how to add exception
	reporters to customized ports. For a better example of exceptions reporting,
	see the code in brf_ents.c

#endif

BRF_table brf_group1[] = 
{
	(BRF_table) &brf_label,
	(BRF_table) &brf_marker,
	(BRF_table) &brf_marker3,
	(BRF_table) &brf_line,
	(BRF_table) &brf_line3,
	(BRF_table) &brf_text,
	(BRF_table) &brf_text3,
	(BRF_table) &brf_annotationtext3,
	(BRF_table) &brf_pixelmap3,
	(BRF_table) &brf_gensphere3,
	(BRF_table) &brf_gencircle,
	(BRF_table) &brf_gencircle3
};
int brf_group1_len = sizeof(brf_group1)/sizeof(BRF_table);

BRF_table brf_group2[] = 
{
	(BRF_table) &brf_linetype,
	(BRF_table) &brf_interiorshading,
	(BRF_table) &brf_interiorlighting,
	(BRF_table) &brf_backfaceprocessing,
	(BRF_table) &brf_edgeflag,
	(BRF_table) &brf_textfont,
	(BRF_table) &brf_textprec,
	(BRF_table) &brf_lightstate,
	(BRF_table) &brf_depthcueindex,
	(BRF_table) &brf_hlhsremoval,
	(BRF_table) &brf_readgeometryfile,
	(BRF_table) &brf_cleargeometry,
	(BRF_table) &brf_beginstructure,
	(BRF_table) &brf_endstructure
};
int brf_group2_len = sizeof(brf_group2)/sizeof(BRF_table);

BRF_table brf_group3[] = 
{
	(BRF_table) &brf_executestructure,
	(BRF_table) &brf_callstructure,
	(BRF_table) &brf_invokeatframe
};
int brf_group3_len = sizeof(brf_group3)/sizeof(BRF_table);

BRF_table brf_group4[] = 
{
	(BRF_table) &brf_defineviewspecification,
	(BRF_table) &brf_definelight,
	(BRF_table) &brf_markertype,
	(BRF_table) &brf_markersize,
	(BRF_table) &brf_markercolor,
	(BRF_table) &brf_markercolorindex,
	(BRF_table) &brf_linewidth,
	(BRF_table) &brf_linecolor,
	(BRF_table) &brf_linecolorindex,
	(BRF_table) &brf_lineshading,
	(BRF_table) &brf_interiorstyle,
	(BRF_table) &brf_interiorpatternindex,
	(BRF_table) &brf_interiorcolor,
	(BRF_table) &brf_interiorcolorindex,
	(BRF_table) &brf_backfaceinteriorcolor,
	(BRF_table) &brf_backfaceinteriorcolorindex,
	(BRF_table) &brf_surfaceproperties,
	(BRF_table) &brf_backfaceproperties,
	(BRF_table) &brf_edgetype,
	(BRF_table) &brf_edgewidth,
	(BRF_table) &brf_edgecolor,
	(BRF_table) &brf_edgecolorindex,
	(BRF_table) &brf_textcolor,
	(BRF_table) &brf_textcolorindex,
	(BRF_table) &brf_textpath,
	(BRF_table) &brf_textalign,
	(BRF_table) &brf_charheight,
	(BRF_table) &brf_charexp,
	(BRF_table) &brf_charspace,
	(BRF_table) &brf_charupvector,
	(BRF_table) &brf_annotextcharheight,
	(BRF_table) &brf_annotextcharupvector,
	(BRF_table) &brf_annotextstyle
};
int brf_group4_len = sizeof(brf_group4)/sizeof(BRF_table);

BRF_table brf_group5[] = 
{
	(BRF_table) &brf_identity3,
	(BRF_table) &brf_concatmatrix3,
	(BRF_table) &brf_invertmatrix3,
	(BRF_table) &brf_rotate3,
	(BRF_table) &brf_rotatexyz3,
	(BRF_table) &brf_translate3,
	(BRF_table) &brf_scale3,
	(BRF_table) &brf_matrix3,
	(BRF_table) &brf_getmatrix3,
	(BRF_table) &brf_pushmatrix3,
	(BRF_table) &brf_popmatrix3,
	(BRF_table) &brf_globaltransformation3,
	(BRF_table) &brf_localtransformation3,
	(BRF_table) &brf_applytoglobal3,
	(BRF_table) &brf_applytolocal3,
	(BRF_table) &brf_vieworientation3,
	(BRF_table) &brf_viewmapping3,
	(BRF_table) &brf_activeview
};
int brf_group5_len = sizeof(brf_group5)/sizeof(BRF_table);

BRF_table brf_group6[] = 
{
	(BRF_table) &brf_begintest,
	(BRF_table) &brf_endtest,
	(BRF_table) &brf_pause,
	(BRF_table) &brf_sleep,
	(BRF_table) &brf_definecolor,
	(BRF_table) &brf_backgroundcolor,
	(BRF_table) &brf_backgroundcolorindex,
	(BRF_table) &brf_defaultviewspecification,
	(BRF_table) &brf_definedepthcue,
	(BRF_table) &brf_configuration
};
int brf_group6_len = sizeof(brf_group6)/sizeof(BRF_table);

BRF_table brf_group7[] = 
{
	(BRF_table) &brf_polygon,
	(BRF_table) &brf_polygon3,
	(BRF_table) &brf_fillareaset,
	(BRF_table) &brf_fillareaset3,
	(BRF_table) &brf_triangle3,
	(BRF_table) &brf_quadmesh3,
	(BRF_table) &brf_indexpolygons3
};
int brf_group7_len = sizeof(brf_group7)/sizeof(BRF_table);


/*----------------------------------------------------------------------*\
| Procedure   : void brf_traverser(	*BIF_Traverser_state, *BIF_All, 
|					*BRF_state)
|------------------------------------------------------------------------
| Description : Walks throuth the BRF structures built for a givren test
|			loop.
|------------------------------------------------------------------------
| Return      : 
\*----------------------------------------------------------------------*/
brf_traverser( traverser_state, bif_entity, brf_state)
BIF_Traverser_state *traverser_state;
BIF_All *bif_entity;
BRF_state *brf_state;
{

	while ( bif_entity != NULL )
	{

#ifdef EXCEPTION_HANDLER
			BRF_do_exceptionsearch(bif_entity,brf_state);
#endif
		brf_branch(traverser_state, bif_entity, brf_state);
		bif_entity = bif_entity->any.next;
	}
} /* End BIF_execute_traverser */
/*----------------------------------------------------------------------*\
| Procedure   : void brf_branch (	*BIF_Traverser_state, *BIF_All, 
|					*BRF_state)
|------------------------------------------------------------------------
| Description : NOTE: An improvement to this code would be to add a
|		pointer to the BRF_data structures in the BIF_data
|		structures, and to fill these pointers in the 
|		new_(entities) calls.  This would eliminate the need for
|		the search done by this procedure, and eliminate the 
|		need for the brf_table
|------------------------------------------------------------------------
| Return      : 
\*----------------------------------------------------------------------*/
void brf_branch ( traverser_state, bif_entity , brf_state)
BIF_Traverser_state *traverser_state;
BIF_All *bif_entity;
BRF_state *brf_state;
{
int i, entity_type;
int (*brf_function)();
	entity_type = bif_entity->entity_type;
	brf_state->brf_flag = BRF_COLLECT;
	for(i=0;i< brf_table_size;i++)
	{
		if(entity_type == brf_table[i]->any.entity_type)
		{
			brf_function = brf_table[i]->any.handler;
			(*brf_function)
		     	(traverser_state,bif_entity,brf_table[i],brf_state);
			break;
		}
	}
}

/*----------------------------------------------------------------------*\
| Procedure   : void brf_report (	*BRF_state )
|------------------------------------------------------------------------
| Description : Main routine involved in printing out a BRF report
|------------------------------------------------------------------------
| Return      : 
\*----------------------------------------------------------------------*/
void brf_report (brf_state )
BRF_state *brf_state;
{
int i;
int (*brf_function)();
char	temp_str[80];
int	num_exceptions;
int	num_ents_used;
static int loop_seq_no = 1;
static char *sep1 =
"------------------------------------------------------------------------\n";
static char *sep2 =
"|----------------------------------------------------------------------|\n";
static char *sep3 =
"| -------                       --------  --------  --------  -------- |\n";

	brf_state->brf_flag = BRF_REPORT;
/*----------------------------------------------------------------------*/
	/* Open the default report if one has not already been specified */
	check_default_report("gpc_brf.out");

#ifdef EXTERNALNOTE
	/* open a gap and print a marker to seperate the test
	loop data from the header/system data. */
#endif

	fprintf(brf_file,"\n\n\n");
	fprintf(brf_file,sep1);
	sprintf(temp_str,"|      Beginning Of Test Loop %d", loop_seq_no++);
	BRF_appendEndBar(temp_str);
	fprintf(brf_file,temp_str);
	fprintf(brf_file,sep1);



	fprintf(brf_file,"\n\n%s%s%s%s%s%s%s%s",
"------------------------------------------------------------------------\n",
"|       Graphics Primitives: Polygon Summary Table                     |\n",
"|                                                                      |\n",
"|  Average                       Average   Average   Average   Average |\n",
"|  Times                       Verticies    Facets     Edges  contours |\n",
"|  Called                      Per Frame Per Frame Per Frame Per Frame |\n",
"|  Per Frame                                                           |\n",
"|----------------------------------------------------------------------|\n");



	num_ents_used = 0;
	for(i=0;i< brf_group7_len   ;i++)
	{
		brf_function = brf_group7[i]->any.handler;
		if ( (*brf_function)
	     		(NULL,NULL,brf_group7[i],brf_state) )
			num_ents_used++ == 0;
	}
	fprintf(brf_file,sep3);
		do_brftotalpoly(NULL,NULL,
			&total_with_data, brf_state);
	if ( num_ents_used == 0) fprintf( brf_file,
"|       No Polygon Primitives used in this test loop.                  |\n");

	fprintf(brf_file,sep1);


	fflush(brf_file);
	


	brf_state->brf_flag = BRF_REPORT2;
	fprintf(brf_file,"\n\n%s%s%s%s%s%s%s%s",
"------------------------------------------------------------------------\n",
"|       Graphics Primitives: Polygon Optional Data Summary Table       |\n",
"|                                                                      |\n",
"|  Average                       Average   Average   Average   Average |\n",
"|  Times                          Vertex    Vertex     Facet     Facet |\n",
"|  Called                         Colors   Normals     Color   Normals |\n",
"|  Per Frame                   Per Frame Per Frame Per Frame Per Frame |\n",
"|----------------------------------------------------------------------|\n");


	num_ents_used = 0;
	for(i=0;i< brf_group7_len   ;i++)
	{
		brf_function = brf_group7[i]->any.handler;
		if ( (*brf_function)
	     		(NULL,NULL,brf_group7[i],brf_state) )
			num_ents_used++ == 0;
	}
	fprintf(brf_file,sep3);
		do_brftotalpoly((BIF_Traverser_state *)NULL,NULL,
			&total_with_data, brf_state);
	if ( num_ents_used == 0) fprintf( brf_file,
"|       No Polygon Optional Data in this test loop.                    |\n");

	fprintf(brf_file,sep1);


	fprintf(brf_file,"\n\n%s%s%s%s%s%s%s%s",
"------------------------------------------------------------------------\n",
"|       Graphics Primitives: Polygon Edge Visibility Table             |\n",
"|                                                                      |\n",
"|  Average                       Average                               |\n",
"|  Times                           Edges                               |\n",
"|  Called                        Visible                               |\n",
"|  Per Frame                   Per Frame                               |\n",
"|----------------------------------------------------------------------|\n");
	brf_state->brf_flag = BRF_REPORT3;
	num_ents_used = 0;
	for(i=0;i< brf_group7_len   ;i++)
	{
		brf_function = brf_group7[i]->any.handler;
		if ( (*brf_function)
	     		(NULL,NULL,brf_group7[i],brf_state) )
			num_ents_used++ == 0;
	}
	fprintf(brf_file,sep3);
		do_brftotalpoly((BIF_Traverser_state *)NULL,NULL,
			&total_with_data, brf_state);
	if ( num_ents_used == 0) fprintf( brf_file,
"|       No Polygon with visible edges in this loop.                    |\n");

	fprintf(brf_file,sep1);

	brf_state->brf_flag = BRF_REPORT;


	fprintf(brf_file,"\n\n%s%s%s%s%s%s%s%s",
"------------------------------------------------------------------------\n",
"|       Graphics Primitives: Line and Text Summary Table               |\n",
"|                                                                      |\n",
"|  Average                                                             |\n",
"|  Times                                                               |\n",
"|  Called                                                              |\n",
"|  Per Frame                                                           |\n",
"|----------------------------------------------------------------------|\n");

	num_ents_used = 0;
	for(i=0;i< brf_group1_len   ;i++)
	{
		brf_function = brf_group1[i]->any.handler;
		if ( (*brf_function)
	     		(NULL,NULL,brf_group1[i],brf_state) )
			num_ents_used++ == 0;
	}
	if ( num_ents_used == 0) fprintf( brf_file,
"|       No Line or Text calls to report in this loop.                  |\n");

	fprintf(brf_file,sep1);

	fprintf(brf_file,"\n\n%s%s%s%s%s%s%s%s",
"------------------------------------------------------------------------\n",
"|       Graphics Attributes: Summary Table                             |\n",
"|                                                                      |\n",
"|  Average                                                             |\n",
"|  Times                                                               |\n",
"|  Called                                                              |\n",
"|  Per Frame                                                           |\n",
"|----------------------------------------------------------------------|\n");

	num_ents_used = 0;
	for(i=0;i< brf_group4_len   ;i++)
	{
		brf_function = brf_group4[i]->any.handler;
		if ( (*brf_function)
	     		(NULL,NULL,brf_group4[i],brf_state) )
			num_ents_used++ == 0;
	}

	for(i=0;i< brf_group2_len   ;i++)
	{
		brf_function = brf_group2[i]->any.handler;
		if ( (*brf_function)
	     		(NULL,NULL,brf_group2[i],brf_state) )
			num_ents_used++ == 0;
	}
	if ( num_ents_used == 0) fprintf( brf_file,
"|       No Attribute calls to report in this loop.                     |\n");

	fprintf(brf_file,sep1);

	fprintf(brf_file,"\n\n%s%s%s%s%s%s%s%s",
"------------------------------------------------------------------------\n",
"|       Matrix Operations: Summary Table                               |\n",
"|                                                                      |\n",
"|  Average                                                             |\n",
"|  Times                                                               |\n",
"|  Called                                                              |\n",
"|  Per Frame                                                           |\n",
"|----------------------------------------------------------------------|\n");

	num_ents_used = 0;
	for(i=0;i< brf_group5_len   ;i++)
	{
		brf_function = brf_group5[i]->any.handler;
		if ( (*brf_function)
	     		(NULL,NULL,brf_group5[i],brf_state) )
			num_ents_used++ == 0;
	}
	if ( num_ents_used == 0) fprintf( brf_file,
"|       No Matrix calls to report in this loop.                        |\n");

	fprintf(brf_file,sep1);

	fprintf(brf_file,"\n\n%s%s%s%s%s%s%s%s",
"------------------------------------------------------------------------\n",
"|       Structure Calls: Summary Table                                 |\n",
"|                                                                      |\n",
"|  Average                                                             |\n",
"|  Times                                                               |\n",
"|  Called                                                              |\n",
"|  Per Frame                                                           |\n",
"|----------------------------------------------------------------------|\n");

	num_ents_used = 0;
	for(i=0;i< brf_group3_len   ;i++)
	{
		brf_function = brf_group3[i]->any.handler;
		if ( (*brf_function)
	     		(NULL,NULL,brf_group3[i],brf_state) )
			num_ents_used++ == 0;
	}
	if ( num_ents_used == 0) fprintf( brf_file,
"|       No Structure calls to report in this loop.                     |\n");

	fprintf(brf_file,sep1);
	fprintf(brf_file,"\n\n");
/*----------------------------------------------------------------------*/

	fprintf(brf_file,"%s%s%s%s%s",
"------------------------------------------------------------------------\n",
"|                                                                      |\n",
"|       Global Exception: Summary Table                                |\n",
"|                                                                      |\n",
"|----------------------------------------------------------------------|\n");

#ifdef EXTERNALNOTE
	/* this is the routine which prints out the global exceptions messages. 
	The errors are defined in brfexception.h along with the number
	of error messages and their exact text strings. This is the last
	of an older system which has been superceded by brf_except.c.*/
#endif
#ifdef EXCEPTION_HANDLER
/* add error message buffer printout right here. */
	num_exceptions = 0;
	/* global error messages */
	{
	int i;
	for(i=0;i<NUM_GLOBAL_ERRORS;i++)
		{
        	   if((plb_exception & POW2(i)) != 0)
		   {
			num_exceptions++;
               		fprintf(brf_file,brf_exception_txt[i]);
		   }
		}
	}
	for(i=0;i<brf_num_ex_tests;i++)
	{
		if(brf_exception[i].brf_ex_numhits > 0)
		{
			num_exceptions++;
			sprintf(temp_str,"|%s",
			     nice_float_8(
				(float)brf_exception[i].brf_ex_numhits
				/brf_state->brf_num_frames));
			strcat(temp_str,brf_exception[i].brf_ex_message);
			BRF_appendEndBar(temp_str);
			fprintf(brf_file,temp_str);
		}
	}
	if ( num_exceptions == 0) fprintf( brf_file,
"|  No Exceptions to report in this loop.                               |\n");


	fprintf(brf_file,sep1);
	fprintf(brf_file,"\n\n");
	fprintf(brf_file,"\n\n");

#endif
}

/*----------------------------------------------------------------------*\
| Procedure   : void brf_init (	)
|------------------------------------------------------------------------
| Description : Initialized BRF tables
|------------------------------------------------------------------------
| Return      : 
\*----------------------------------------------------------------------*/
void brf_init ( )
{
int i;
int (*brf_function)();
BRF_state brf_state;
	brf_state.brf_flag = BRF_INITIALIZE;
	for(i=0;i< brf_table_size;i++)
	{
		brf_function = brf_table[i]->any.handler;
		(*brf_function)
	     	(NULL,NULL,brf_table[i],&brf_state);
	}
}

/*----------------------------------------------------------------------*\
| Procedure   : void brf_start_stopwatch ()
|------------------------------------------------------------------------
| Description : Starts the stop watch
|------------------------------------------------------------------------
| Return      : 
\*----------------------------------------------------------------------*/
brf_start_stopwatch()
{
float sttime;


	sttime = stopwatch(WATCH_START);
	sttime = stopwatch(WATCH_RESET);
	if(sttime != 0)
	{
		fprintf(stderr,"Failure on timer init.\n");
		fprintf(stderr,"Time should read 0.\n");
		fprintf(stderr,"Timer init returns %f.\n\n\n",sttime);
		exit(-1);
	}
#ifdef DEBUG
	fprintf(stderr,"Stopwatch running. Time reads %f\n",sttime);
	fflush(stderr);
#endif


	/* ST = start your timing here */
}

/*----------------------------------------------------------------------*\
| Procedure   : void brf_stop_stopwatch ( *BRF_state )
|------------------------------------------------------------------------
| Description : Stops the stop watch
|------------------------------------------------------------------------
| Return      : 
\*----------------------------------------------------------------------*/
brf_stop_stopwatch(brf_state)
BRF_state *brf_state;
{
    float ettime;
    float tttime;
    int i = 1; /* flag value for pqcr */
    int coli = 1; /* dummy value for pqcr */
    int red; /* dummy value for pqcr */
    int green; /* dummy value for pqcr */
    int blue; /* dummy value for pqcr */
    int errind; /* dummy value for pqcr */

    /* TT = stop take a split here (lap time?) */
    ettime = stopwatch(WATCH_SPLIT);
#ifdef EXTERNALNOTE
    /* sync your graphics pipeline here */
#endif

#ifdef USING_PHIGS
    {
	Pcolr_rep rep;
	Pint errind;

	pinq_colr_rep((Pint)bench_setup.workid, (Pint)0,
		      PINQ_REALIZED, &errind, &rep);
	
    }
#endif /* USING_PHIGS */

    /* ET = stop your timing here */
    tttime = stopwatch(WATCH_SPLIT);
#ifdef EXTERNALNOTE
    /* 

      TIMING METHODOLOGY

      The overall time reported is from first graphics call issued to
      graphics device to end of last pipe synchronization. Under some 
      circumstances, it would be appropriate to run an empty loop
      to obtain system overhead, then subtract that overhead to get
      the real "Cost of Graphics" figure. However, this method
      is less than accurate for systems with asynchronous graphics
      device accelerators. In the interests of portability among
      different systems, the overhead costs are left intact.  This is the
      same methodology as used in the Level 1 GPC Benchmark.

      */
#endif

    BRF_loopTiming( brf_state->brf_num_frames, 
		   tttime,       /* total elapsed time (+ Tdelay) in seconds */
		   (tttime - ettime),        /* ET - TT in seconds */
		   brf_state->brf_num_frames, /* num frames */ 
		   input_filenm,  /* Current input file name */
		   brf_file);     /* Current open report file pointer */
}
