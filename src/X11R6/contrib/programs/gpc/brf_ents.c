/* $XConsortium: brf_ents.c,v 5.3 94/04/17 20:44:27 rws Exp $ */
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
| Author        :	SimGraphics Engineering Corportation
|
| File          :	brf_ents.c
| Date          :	Fri Feb  9 10:46:55 PST 1990
| Project       :	PLB
| Description   :	Initializes, Counts, and Prints to BRF file
|			usage information about each entity type.
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
|	do_brfgeneric( BIF_Traverser_state,union, 
|				BIF_All BRF_Any_With_Data BRF_state
|		:	Records and reports usage information for the BRF
|	do_brfcallstructure( BIF_Traverser_state,union, 
|				BIF_All BRF_Any_With_Data BRF_state
|		:	call the specified structure
|	do_brfexecutestructure ( BIF_Traverser_state,union, 
|				BIF_All BRF_Any_With_Data BRF_state
|		:	call the specified structure
|	do_brfinvokeatframe ( BIF_Traverser_state,union, 
|				BIF_All BRF_Any_With_Data BRF_state
|		:	call the specified structure
|	do_brftotalpoly ( BIF_Traverser_state,union, 
|				BIF_All BRF_Any_With_Data BRF_state
|		:	call the specified structure
|	do_brfpolygon( BIF_Traverser_state, BIF_Polygon, 
|				BRF_Any_With_Data, BRF_state)
|		:	Records and reports usage information for the BRF
|	do_brfpolygon3( BIF_Traverser_state, BIF_Polygon, 
|				BRF_Any_With_Data, BRF_state)
|		:	Records and reports usage information for the BRF
|	do_brf_fillareaset( BIF_Traverser_state, BIF_Polygon, 
|				BRF_Any_With_Data, BRF_state)
|		:	Records and reports usage information for the BRF
|	do_brffillareaset( BIF_Traverser_state, BIF_Polygon, 
|				BRF_Any_With_Data, BRF_state)
|		:	Records and reports usage information for the BRF
|	do_brftriangle3( BIF_Traverser_state, BIF_Polygon, 
|				BRF_Any_With_Data, BRF_state)
|		:	Records and reports usage information for the BRF
|	do_brfquadmesh( BIF_Traverser_state, BIF_Polygon, 
|				BRF_Any_With_Data, BRF_state)
|		:	Records and reports usage information for the BRF
|	do_brfindexpolygons( BIF_Traverser_state, BIF_Polygon, 
|				BRF_Any_With_Data, BRF_state)
|		:	Records and reports usage information for the BRF
|	open_reportfile( *char )
|		:	Opens the BRF report file
|	close_reportfile()
|		:	Closes the BRF report file
|	check_default_report(char *)
|		:	Opens report file
|	nice_float_8(float)
|		:	Fromats a nice floating point number
|	brf_utl_report1(int,BRF_Any_With_Data* )
|		:	Producde line of printout for BRF_REPORT
|	brf_utl_report2(int,BRF_Any_With_Data* )
|		:	Producde line of printout for BRF_REPORT2
|	brf_utl_report3(int,BRF_Any_With_Data* )
|		:	Producde line of printout for BRF_REPORT3
|
\*--------------------------------------------------------------------*/
#define STRLENGTH 80
#include <math.h>
#include <stdio.h>
#include <string.h>
#include "biftypes.h"
#include "globals.h"
#include "bifparse.h"
#include "brf_ents.h"
#include "brftypes.h"
#include "bifmacro.h"
#include "brfexption.h"

#define UNUSED 0 
#define UNOPENED 1
#define USED 2
#define NICE_SPRINTF8(STR,VALUE) sprintf(STR,"|%s",nice_float_8(VALUE));
#define NICE_CAT8(STR,VALUE)     strcat (STR,      nice_float_8(VALUE));
#define NICE_CAT_ENT_NAME                                              \
{	char temp_str2[80];                                            \
	int k;                                                         \
	strncpy(temp_str2, brf_entity->name, 19); temp_str2[19] = '\0';\
	strcat(temp_str2, ":");					       \
	strcat (temp_str, temp_str2);                                  \
	for (k=strlen(brf_entity->name);k<20;k++)                      \
	   strcat(temp_str," ");                                       \
}
#define NICE_CAT_ENT_NAME_D                                            \
{	char temp_str2[80];                                            \
	int k;                                                         \
	strncpy(temp_str2, brf_entity->name, 19); temp_str2[19] = '\0';\
	strcat(temp_str2, ":");					       \
	strcat (temp_str, temp_str2);                                  \
	for (k=strlen(brf_entity->name);k<20;k++)                      \
	   if (k%2)strcat(temp_str,"."); else strcat(temp_str," ");    \
}

char brf_filename[STRLENGTH];
FILE *brf_file;
int  brf_used_flag = UNOPENED;
int i; /* generic counter */
char *nice_float_8();

/*----------------------------------------------------------------------*\
| Program: brf_ents.c
| among other things, this code generates most of the report code
|
\*----------------------------------------------------------------------*/


/*--------------------------------------------------------------------*\
| Procedure     :	do_brfgeneric( BIF_Traverser_state,union,
|				       BIF_All BRF_Any_With_Data BRF_state
|---------------------------------------------------------------------
| Description   :	Records and reports usage information for the BRF
|---------------------------------------------------------------------
| Return        :	FALSE if nothing printed in for BRF_REPORT,2,3
\*--------------------------------------------------------------------*/
int do_brfgeneric( traverser_state,bif_entity,brf_entity, brf_state)
BIF_Traverser_state *traverser_state;
union BIF_All *bif_entity;
BRF_Any_With_Data *brf_entity;
BRF_state *brf_state;
{/* do_brfgeneric */
    char    temp_str[80];
    int k, brf_flag, num_frames;
    int return_value;
    return_value = TRUE;

    brf_flag = brf_state->brf_flag;
    num_frames = brf_state->brf_num_frames;
    switch(brf_flag)
    {
      case BRF_COLLECT:
	brf_entity->times_called += num_frames;
	break;
      case BRF_INITIALIZE:
	brf_entity->times_called = 0;
	break;
      case BRF_REPORT:
	if ( brf_entity->times_called > 0 )
	{
	    NICE_SPRINTF8( temp_str, 
		( (float)brf_entity->times_called/ (float)num_frames ));
	    NICE_CAT_ENT_NAME
		BRF_appendEndBar(temp_str);
	    fprintf(brf_file,temp_str);
	} else return_value = FALSE;
	break;
    }
    return(return_value);
}/* do_brfgeneric */

/*----------------------------------------------------------------------*\
| Procedure	:	do_brfcallstructure( BIF_Traverser_state,union,
|				       BIF_All BRF_Any_With_Data BRF_state
|------------------------------------------------------------------------
| Description	:	call the specified structure
|------------------------------------------------------------------------
| Return	:	FALSE if nothing printed in for BRF_REPORT,2,3
\*----------------------------------------------------------------------*/
int do_brfcallstructure(traverser_state,bif_entity,brf_entity, brf_state)
BIF_Traverser_state *traverser_state;
BIF_All *bif_entity;
BRF_Any *brf_entity;
BRF_state *brf_state;
{
    char    temp_str[80];
    int k, brf_flag, num_frames;
    int return_value;
    return_value = TRUE;

    num_frames = brf_state->brf_num_frames;
    brf_flag = brf_state->brf_flag;

    switch(brf_flag)
    {
      case BRF_COLLECT:
	brf_entity->times_called += num_frames;
	brf_traverser (traverser_state,
		       bif_entity->callstructure.structure_ptr->top_of_list,
		       brf_state);
	break;
      case BRF_INITIALIZE:
	brf_entity->times_called = 0;
	break;
      case BRF_REPORT:
	if ( brf_entity->times_called > 0 )
	{
	    NICE_SPRINTF8( temp_str, 
		   ( (float)brf_entity->times_called/ (float)num_frames ));
	    NICE_CAT_ENT_NAME
		BRF_appendEndBar(temp_str);
	    fprintf(brf_file,temp_str);
	} else return_value = FALSE;
	break;
    }
    return(return_value);
} /* End do_brfcallstructure */




/*----------------------------------------------------------------------*\
| Procedure	:	do_brfexecutestructure ( BIF_Traverser_state,union,
|				       BIF_All BRF_Any_With_Data BRF_state
|------------------------------------------------------------------------
| Description	:	call the specified structure
|------------------------------------------------------------------------
| Return	:	FALSE if nothing printed in for BRF_REPORT,2,3
\*----------------------------------------------------------------------*/
int do_brfexecutestructure(traverser_state,bif_entity,brf_entity, brf_state)
BIF_Traverser_state *traverser_state;
BIF_All *bif_entity;
BRF_Any *brf_entity;
BRF_state *brf_state;
{
    char    temp_str[80];
    int k, brf_flag, num_frames;
    int return_value;
    return_value = TRUE;

    num_frames = brf_state->brf_num_frames;
    brf_flag = brf_state->brf_flag;
/* Call the traverser */

    switch(brf_flag)
    {
      case BRF_COLLECT:
	brf_entity->times_called += num_frames;
	brf_traverser (traverser_state,
		       bif_entity->executestructure.structure_ptr->top_of_list,
		       brf_state);
	break;
      case BRF_INITIALIZE:
	brf_entity->times_called = 0;
	break;
      case BRF_REPORT:
	if ( brf_entity->times_called > 0 )
	{
	    NICE_SPRINTF8( temp_str, 
		   ( (float)brf_entity->times_called/ (float)num_frames ));
	    NICE_CAT_ENT_NAME
		BRF_appendEndBar(temp_str);
	    fprintf(brf_file,temp_str);
	} else return_value = FALSE;
	break;
    }
    return(return_value);
} /* End do_executestructure */


/*----------------------------------------------------------------------*\
| Procedure	:	do_brfinvokeatframe ( BIF_Traverser_state,union,
|				       BIF_All BRF_Any_With_Data BRF_state
|------------------------------------------------------------------------
| Description	:	call the specified structure
|------------------------------------------------------------------------
| Return	:	FALSE if nothing printed in for BRF_REPORT,2,3
\*----------------------------------------------------------------------*/
int do_brfinvokeatframe(traverser_state,bif_entity,brf_entity, brf_state)
BIF_Traverser_state *traverser_state;
BIF_All *bif_entity;
BRF_Any *brf_entity;
BRF_state *brf_state;
{
    char    temp_str[80];
    int k, brf_flag; 
    int     num_frames,     start_frame,     end_frame;
    int iaf_num_frames, iaf_start_frame, iaf_end_frame;
    BIF_InvokeAtFrame *fent;
    int return_value;

    return_value = TRUE;

    brf_flag = brf_state->brf_flag;
    num_frames = brf_state->brf_num_frames;

/* Call the traverser */

	switch(brf_flag)
	{
	case BRF_COLLECT:
        	fent = &bif_entity->invokeatframe;
		start_frame 	= brf_state->brf_start_frame;
		end_frame  	= brf_state->brf_end_frame;

		if( fent->endFrame == -1 ) 
		{
			iaf_num_frames = num_frames - fent->startFrame + 1;
			iaf_end_frame  = num_frames;
		} else
		{
			iaf_num_frames = fent->endFrame - fent->startFrame + 1;
			iaf_end_frame  = fent->endFrame;
		}
		iaf_start_frame  = fent->startFrame;



		brf_entity->times_called += num_frames;

        	brf_state->brf_start_frame = iaf_start_frame;
        	brf_state->brf_end_frame   = iaf_end_frame;
		brf_state->brf_num_frames  = iaf_num_frames;

		brf_traverser (traverser_state,
		   fent->invoke.structure_ptr->top_of_list,
		   brf_state);

        	brf_state->brf_start_frame = start_frame;
        	brf_state->brf_end_frame   = end_frame;
		brf_state->brf_num_frames  = num_frames;;
	break;
	case BRF_INITIALIZE:
		brf_entity->times_called 	= 0;
	break;
	case BRF_REPORT:
	   if ( brf_entity->times_called > 0 )
	   {
		NICE_SPRINTF8( temp_str, 
		   ( (float)brf_entity->times_called/ (float)num_frames ));
		NICE_CAT_ENT_NAME
		BRF_appendEndBar(temp_str);
		fprintf(brf_file,temp_str);
	   } else return_value = FALSE;
	break;
	}
	return(return_value);
} /* End do_invokeatframe */

/*----------------------------------------------------------------------*\
| Procedure	:	do_brftotalpoly ( BIF_Traverser_state,union,
|				       BIF_All BRF_Any_With_Data BRF_state
|------------------------------------------------------------------------
| Description	:	call the specified structure
|------------------------------------------------------------------------
| Return	:	FALSE if nothing printed in for BRF_REPORT,2,3
\*----------------------------------------------------------------------*/
int do_brftotalpoly(traverser_state,bif_entity,brf_entity, brf_state)
BIF_Traverser_state *traverser_state;
BIF_All *bif_entity;
BRF_Any_With_Data *brf_entity;
BRF_state *brf_state;
{
char    temp_str[80];
int k, brf_flag, type, tmp;
int num_frames;
BIF_Polygon *polygon;
BIF_Polygon3 *polygon3;
BIF_Fillareaset *fillareaset;
BIF_Fillareaset3 *fillareaset3;
BIF_Triangle3 *triangle3;
BIF_Quadmesh3 *quadmesh3;
BIF_Indexpolygons3 *indexpolygons3;
int return_value;
	return_value = TRUE;

#ifdef EXTERNALNOTE
	/* The exception testing normally found in other brf traversal
	routines is not included here as this routine is called after
	the same tests have already been applied elsewhere. */
#endif

	brf_flag = brf_state->brf_flag;
/* Call the traverser */

	num_frames = brf_state->brf_num_frames;
	switch(brf_flag)
	{
	case BRF_COLLECT:
	type = bif_entity->entity_type;
		switch ( type )
		{
		case POLYGON:
			polygon = (BIF_Polygon *)bif_entity;
			brf_entity->times_called+= num_frames;
			brf_entity->numCoords   += polygon->number*num_frames;
			brf_entity->numFacets+= num_frames;
			brf_entity->numEdges    += polygon->number*num_frames;
			brf_entity->numContours+= num_frames;
		break;
		case POLYGON3:
			polygon3 = (BIF_Polygon3 *)bif_entity;
			brf_entity->times_called+= num_frames;
			brf_entity->numCoords   += polygon3->numCoords
						*num_frames;
			brf_entity->numFacets+= num_frames;
			brf_entity->numEdges    += polygon3->numEdges
						*num_frames;
			brf_entity->numContours += polygon3->numFacets
                                                *num_frames;
			if(polygon3->with_data_flag[VCOLORT]
			|| polygon3->with_data_flag[VCOLORI])
			brf_entity->vcolr += polygon3->numCoords
						*num_frames;
			if(polygon3->with_data_flag[FCOLORT]
			|| polygon3->with_data_flag[FCOLORI])
			brf_entity->fcolr += polygon3->numFacets
						*num_frames;
			if(polygon3->with_data_flag[VNORM])
			brf_entity->vnorm += polygon3->numCoords
						*num_frames;
			if(polygon3->with_data_flag[FNORM])
			brf_entity->fnorm += polygon3->numFacets
						*num_frames;
			if(polygon3->with_data_flag[EDATA])
			brf_entity->evisi += polygon3->numEdges
						*num_frames;
		break;
		case FILL_AREA_SET:
			fillareaset = (BIF_Fillareaset *)bif_entity;
			brf_entity->times_called+= num_frames;
			tmp = fillareaset->sets->num_points;
			brf_entity->numCoords   += tmp*num_frames;
			brf_entity->numFacets+= num_frames;
			brf_entity->numEdges    += tmp;
			brf_entity->numContours += fillareaset->numContours
						*num_frames;
		break;
		case FILL_AREA_SET3:
			fillareaset3 = (BIF_Fillareaset3 *)bif_entity;
			brf_entity->times_called+= num_frames;
			brf_entity->numCoords   += fillareaset3->numCoords
						*num_frames;
			brf_entity->numFacets+= num_frames;
			brf_entity->numEdges    += fillareaset3->numEdges
						*num_frames;
			brf_entity->numContours += fillareaset3->numContours
						*num_frames;
			if(fillareaset3->with_data_flag[VCOLORT]
			|| fillareaset3->with_data_flag[VCOLORI])
			brf_entity->vcolr += fillareaset3->numCoords
						*num_frames;
			if(fillareaset3->with_data_flag[FCOLORT]
			|| fillareaset3->with_data_flag[FCOLORI])
			brf_entity->fcolr += fillareaset3->numFacets
						*num_frames;
			if(fillareaset3->with_data_flag[VNORM])
			brf_entity->vnorm += fillareaset3->numCoords
						*num_frames;
			if(fillareaset3->with_data_flag[FNORM])
			brf_entity->fnorm += fillareaset3->numFacets
						*num_frames;
			if(fillareaset3->with_data_flag[EDATA])
			brf_entity->evisi += fillareaset3->numEdges
						*num_frames;
		break;
		case TRIANGLE3:
			triangle3 = (BIF_Triangle3 *)bif_entity;
			brf_entity->times_called+= num_frames;
			brf_entity->numCoords   += triangle3->numCoords
						*num_frames;
			brf_entity->numFacets   += triangle3->numFacets
						*num_frames;
			brf_entity->numEdges    += triangle3->numEdges
						*num_frames;
			brf_entity->numContours += triangle3->numFacets
						*num_frames;
			if(triangle3->with_data_flag[VCOLORT]
			|| triangle3->with_data_flag[VCOLORI])
			brf_entity->vcolr += triangle3->numCoords
						*num_frames;
			if(triangle3->with_data_flag[FCOLORT]
			|| triangle3->with_data_flag[FCOLORI])
			brf_entity->fcolr += triangle3->numFacets
						*num_frames;
			if(triangle3->with_data_flag[VNORM])
			brf_entity->vnorm += triangle3->numCoords
						*num_frames;
			if(triangle3->with_data_flag[FNORM])
			brf_entity->fnorm += triangle3->numFacets
						*num_frames;
			if(triangle3->with_data_flag[EDATA])
			brf_entity->evisi += triangle3->numEdges
						*num_frames;
		break;
		case QUAD_MESH3:
			quadmesh3 = (BIF_Quadmesh3 *)bif_entity;
			brf_entity->times_called+= num_frames;
			brf_entity->numCoords   += quadmesh3->numCoords
						*num_frames;
			brf_entity->numFacets   += quadmesh3->numFacets
						*num_frames;
			brf_entity->numEdges    += quadmesh3->numEdges
						*num_frames;
			brf_entity->numContours += quadmesh3->numFacets
						*num_frames;
			if(quadmesh3->with_data_flag[VCOLORT]
			|| quadmesh3->with_data_flag[VCOLORI])
			brf_entity->vcolr += quadmesh3->numCoords
						*num_frames;
			if(quadmesh3->with_data_flag[FCOLORT]
			|| quadmesh3->with_data_flag[FCOLORI])
			brf_entity->fcolr += quadmesh3->numFacets
						*num_frames;
			if(quadmesh3->with_data_flag[VNORM])
			brf_entity->vnorm += quadmesh3->numCoords
						*num_frames;
			if(quadmesh3->with_data_flag[FNORM])
			brf_entity->fnorm += quadmesh3->numFacets
						*num_frames;
			if(quadmesh3->with_data_flag[EDATA])
			brf_entity->evisi += quadmesh3->numEdges
						*num_frames;
		break;
		case INDEX_POLYGONS3:
			indexpolygons3 = (BIF_Indexpolygons3 *)bif_entity;
			brf_entity->times_called+= num_frames;
			brf_entity->numCoords   += indexpolygons3->numCoords
						*num_frames;
			brf_entity->numFacets   += indexpolygons3->numFacets
						*num_frames;
			brf_entity->numEdges    += indexpolygons3->numEdges
						*num_frames;
			brf_entity->numContours += indexpolygons3->numFacets
						*num_frames;
			if(indexpolygons3->with_data_flag[VCOLORT]
			|| indexpolygons3->with_data_flag[VCOLORI])
			brf_entity->vcolr += indexpolygons3->numCoords
						*num_frames;
			if(indexpolygons3->with_data_flag[FCOLORT]
			|| indexpolygons3->with_data_flag[FCOLORI])
			brf_entity->fcolr += indexpolygons3->numFacets
						*num_frames;
			if(indexpolygons3->with_data_flag[VNORM])
			brf_entity->vnorm += indexpolygons3->numCoords
						*num_frames;
			if(indexpolygons3->with_data_flag[FNORM])
			brf_entity->fnorm += indexpolygons3->numFacets
						*num_frames;
			if(indexpolygons3->with_data_flag[EDATA])
			brf_entity->evisi += indexpolygons3->numEdges
						*num_frames;
		break;
		}
	break;
	case BRF_INITIALIZE:
		brf_entity->times_called 	= 0;
#ifdef EXTERNALNOTE
        /* Here we init the brf tagged exception counters. When used with the
        global variable plb_exception and the exceptions variable in the bif
        structure, exceptions from all segments of the code can be tagged. It then
        remains to the brf report generator to sweep up these tags and print
        them. In the case of exceptions encountered during building of entities,
        the data_groups count for each unsupported optional data group called
	gets incremented (if appropriate) in the COLLECT case above. 
	The REPORT case below prints all non-zero values for those optional 
	data groups.
	*/
#endif
                brf_entity->exceptions = 0;
                brf_entity->optional_data[VCOLORT] = 0;
                brf_entity->optional_data[VCOLORI] = 0;
                brf_entity->optional_data[VNORM] = 0;
                brf_entity->optional_data[FCOLORT] = 0;
                brf_entity->optional_data[FCOLORI] = 0;
                brf_entity->optional_data[FNORM] = 0;
                brf_entity->optional_data[EDATA] = 0;
		brf_entity->numCoords 		= 0;
		brf_entity->numFacets 		= 0;
		brf_entity->numEdges 		= 0;
		brf_entity->numContours 	= 0;
		brf_entity->vcolr		= 0;
		brf_entity->vnorm		= 0;
		brf_entity->fcolr		= 0;
		brf_entity->fnorm		= 0;
		brf_entity->evisi		= 0;
	break;
	case BRF_REPORT:
		return_value = brf_utl_report1(num_frames,brf_entity);
	break;
	case BRF_REPORT2:
		return_value = brf_utl_report2(num_frames,brf_entity);
	break;
	case BRF_REPORT3:
		return_value = brf_utl_report3(num_frames,brf_entity);
	break;
	}
	return(return_value);

} /* End do_totalpoly */

/*--------------------------------------------------------------------*\
| Procedure     :	do_brfpolygon( BIF_Traverser_state, BIF_Polygon, 
|				       BRF_Any_With_Data, BRF_state)
|---------------------------------------------------------------------
| Description   :	Records and reports usage information for the BRF
|---------------------------------------------------------------------
| Return        :	FALSE if nothing printed in for BRF_REPORT,2,3
\*--------------------------------------------------------------------*/
int do_brfpolygon( traverser_state,polygon,brf_entity, brf_state)
BIF_Traverser_state *traverser_state;
BIF_Polygon *polygon;
BRF_Any_With_Data *brf_entity;
BRF_state *brf_state;
{/* do_brfpolygon */
char    temp_str[80];
int k, brf_flag, num_frames;
int return_value;
	return_value = TRUE;
	brf_flag = brf_state->brf_flag;
	num_frames = brf_state->brf_num_frames;
	switch(brf_flag)
	{
	case BRF_COLLECT:
		brf_entity->times_called	+= num_frames;
		brf_entity->numCoords   	+= polygon->number*num_frames;
		brf_entity->numFacets		+= num_frames;
		brf_entity->numEdges    	+= polygon->number*num_frames;
		brf_entity->numContours		+= num_frames;
		do_brftotalpoly(traverser_state,polygon,
				(BRF_Any_With_Data *)brf_entity->brf_totals,
				brf_state);
	break;
	case BRF_INITIALIZE:
		brf_entity->times_called 	= 0;
#ifdef EXTERNALNOTE
        /* see comment at line 44   */
#endif
        /* added for error reporting */
                brf_entity->exceptions = 0;
                brf_entity->optional_data[VCOLORT] = 0;
                brf_entity->optional_data[VCOLORI] = 0;
                brf_entity->optional_data[VNORM] = 0;
                brf_entity->optional_data[FCOLORT] = 0;
                brf_entity->optional_data[FCOLORI] = 0;
                brf_entity->optional_data[FNORM] = 0;
                brf_entity->optional_data[EDATA] = 0;
		brf_entity->numCoords 		= 0;
		brf_entity->numFacets 		= 0;
		brf_entity->numEdges 		= 0;
		brf_entity->numContours 	= 0;
		brf_entity->vcolr		= 0;
		brf_entity->vnorm		= 0;
		brf_entity->fcolr		= 0;
		brf_entity->fnorm		= 0;
		brf_entity->evisi		= 0;
	break;
	case BRF_REPORT:
		return_value = brf_utl_report1(num_frames,brf_entity);
	break;
	}
	return(return_value);
}/* do_brfpolygon */
/*--------------------------------------------------------------------*\
| Procedure     :	do_brfpolygon3( BIF_Traverser_state, BIF_Polygon, 
|				       BRF_Any_With_Data, BRF_state)
|---------------------------------------------------------------------
| Description   :	Records and reports usage information for the BRF
|---------------------------------------------------------------------
| Return        :	FALSE if nothing printed in for BRF_REPORT,2,3
\*--------------------------------------------------------------------*/
int do_brfpolygon3( traverser_state,polygon3,brf_entity, brf_state)
BIF_Traverser_state *traverser_state;
BIF_Polygon3 *polygon3;
BRF_Any_With_Data *brf_entity;
BRF_state *brf_state;
{/* do_brfpolygon3 */
char    temp_str[80];
int k, brf_flag, num_frames;
int return_value;
	return_value = TRUE;
	brf_flag = brf_state->brf_flag;
	num_frames = brf_state->brf_num_frames;
	switch(brf_flag)
	{
	case BRF_COLLECT:
		brf_entity->times_called	+= num_frames;
		brf_entity->numCoords   += polygon3->numCoords*num_frames;;
		brf_entity->numFacets		+= num_frames;
		brf_entity->numEdges    += polygon3->numEdges*num_frames;;
		brf_entity->numContours 	+= num_frames;
		if (polygon3->with_data_flag[VCOLORT] ||
		    polygon3->with_data_flag[VCOLORI])
			brf_entity->vcolr+= polygon3->numCoords*num_frames;
		if (polygon3->with_data_flag[VNORM])
			brf_entity->vnorm+= polygon3->numCoords*num_frames;
		if (polygon3->with_data_flag[FCOLORT] ||
		    polygon3->with_data_flag[FCOLORI])
			brf_entity->fcolr	+= num_frames;
		if (polygon3->with_data_flag[FNORM])
			brf_entity->fnorm	+= num_frames;
		if (polygon3->with_data_flag[EDATA])
			brf_entity->evisi += polygon3->numEdges*num_frames;
		do_brftotalpoly(traverser_state,polygon3,
			brf_entity->brf_totals, brf_state);

	break;
	case BRF_INITIALIZE:
		brf_entity->times_called 	= 0;
#ifdef EXTERNALNOTE
        /* see comment at line 44   */
#endif
        /* added for error reporting */
                brf_entity->exceptions = 0;
                brf_entity->optional_data[VCOLORT] = 0;
                brf_entity->optional_data[VCOLORI] = 0;
                brf_entity->optional_data[VNORM] = 0;
                brf_entity->optional_data[FCOLORT] = 0;
                brf_entity->optional_data[FCOLORI] = 0;
                brf_entity->optional_data[FNORM] = 0;
                brf_entity->optional_data[EDATA] = 0;
		brf_entity->numCoords 		= 0;
		brf_entity->numFacets 		= 0;
		brf_entity->numEdges 		= 0;
		brf_entity->numContours 	= 0;
		brf_entity->vcolr		= 0;
		brf_entity->vnorm		= 0;
		brf_entity->fcolr		= 0;
		brf_entity->fnorm		= 0;
		brf_entity->evisi		= 0;
	break;
	case BRF_REPORT:
		return_value = brf_utl_report1(num_frames,brf_entity);
	break;
	}
	return(return_value);
}/* do_brfpolygon3*/
/*--------------------------------------------------------------------*\
| Procedure     :	do_brf_fillareaset( BIF_Traverser_state, 
|			BIF_Polygon, BRF_Any_With_Data, BRF_state)
|---------------------------------------------------------------------
| Description   :	Records and reports usage information for the BRF
|---------------------------------------------------------------------
| Return        :	FALSE if nothing printed in for BRF_REPORT,2,3
\*--------------------------------------------------------------------*/
int do_brffillareaset( traverser_state,fillareaset,brf_entity, brf_state)
BIF_Traverser_state *traverser_state;
BIF_Fillareaset *fillareaset;
BRF_Any_With_Data *brf_entity;
BRF_state *brf_state;
{/* do_brffillareaset*/
char    temp_str[80];
int k, brf_flag, num_frames, tmp;
int return_value;
	return_value = TRUE;
	brf_flag = brf_state->brf_flag;
	num_frames = brf_state->brf_num_frames;
	switch(brf_flag)
	{
	case BRF_COLLECT:
		brf_entity->times_called	+= num_frames;
		tmp = fillareaset->sets->num_points;
		brf_entity->numCoords   	+= tmp * num_frames;
		brf_entity->numFacets		+= num_frames;
		brf_entity->numEdges    	+= tmp * num_frames;
		brf_entity->numContours 	+= fillareaset->numContours
						 * num_frames;
		do_brftotalpoly(traverser_state,fillareaset,
				(BRF_Any_With_Data *)brf_entity->brf_totals,
				brf_state);
	break;
	case BRF_INITIALIZE:
		brf_entity->times_called 	= 0;
#ifdef EXTERNALNOTE
        /* see comment at line 44   */
#endif
        /* added for error reporting */
                brf_entity->exceptions = 0;
                brf_entity->optional_data[VCOLORT] = 0;
                brf_entity->optional_data[VCOLORI] = 0;
                brf_entity->optional_data[VNORM] = 0;
                brf_entity->optional_data[FCOLORT] = 0;
                brf_entity->optional_data[FCOLORI] = 0;
                brf_entity->optional_data[FNORM] = 0;
                brf_entity->optional_data[EDATA] = 0;
		brf_entity->numCoords 		= 0;
		brf_entity->numFacets 		= 0;
		brf_entity->numEdges 		= 0;
		brf_entity->numContours 	= 0;
		brf_entity->vcolr		= 0;
		brf_entity->vnorm		= 0;
		brf_entity->fcolr		= 0;
		brf_entity->fnorm		= 0;
		brf_entity->evisi		= 0;
	break;
	case BRF_REPORT:
		return_value = brf_utl_report1(num_frames,brf_entity);
	break;
	case BRF_REPORT2:
		return_value = brf_utl_report2(num_frames,brf_entity);
	break;
	case BRF_REPORT3:
		return_value = brf_utl_report3(num_frames,brf_entity);
	break;
	}
	return(return_value);
}/* do_brffillareaset*/
/*--------------------------------------------------------------------*\
| Procedure     :	do_brffillareaset( BIF_Traverser_state, 
|			BIF_Polygon, BRF_Any_With_Data, BRF_state)
|---------------------------------------------------------------------
| Description   :	Records and reports usage information for the BRF
|---------------------------------------------------------------------
| Return        :	FALSE if nothing printed in for BRF_REPORT,2,3
\*--------------------------------------------------------------------*/
int do_brffillareaset3( traverser_state,fillareaset3,brf_entity, brf_state)
BIF_Traverser_state *traverser_state;
BIF_Fillareaset3 *fillareaset3;
BRF_Any_With_Data *brf_entity;
BRF_state *brf_state;
{/*do_brffillareaset3*/
char    temp_str[80];
int k, brf_flag, num_frames;
int return_value;
	return_value = TRUE;
	brf_flag = brf_state->brf_flag;
	num_frames = brf_state->brf_num_frames;
	switch(brf_flag)
	{
	case BRF_COLLECT:
		brf_entity->times_called+= num_frames;
		brf_entity->numCoords   += fillareaset3->numCoords*num_frames;
		brf_entity->numFacets	+= num_frames;
		brf_entity->numEdges    += fillareaset3->numEdges*num_frames;
		brf_entity->numContours += fillareaset3->numContours*num_frames;
		if (fillareaset3->with_data_flag[VCOLORT] ||
		    fillareaset3->with_data_flag[VCOLORI])
			brf_entity->vcolr+= fillareaset3->numCoords*num_frames;
		if (fillareaset3->with_data_flag[VNORM])
			brf_entity->vnorm+= fillareaset3->numCoords*num_frames;

		if (fillareaset3->with_data_flag[FCOLORT] ||
		    fillareaset3->with_data_flag[FCOLORI])
			brf_entity->fcolr	+= num_frames;
		if (fillareaset3->with_data_flag[FNORM])
			brf_entity->fnorm	+= num_frames;
		if (fillareaset3->with_data_flag[EDATA])
			brf_entity->evisi += fillareaset3->numEdges*num_frames;
		do_brftotalpoly(traverser_state,fillareaset3,
				(BRF_Any_With_Data *)brf_entity->brf_totals,
				brf_state);
	break;
	case BRF_INITIALIZE:
		brf_entity->times_called 	= 0;
#ifdef EXTERNALNOTE
        /* see comment at line 44   */
#endif
        /* added for error reporting */
                brf_entity->exceptions = 0;
                brf_entity->optional_data[VCOLORT] = 0;
                brf_entity->optional_data[VCOLORI] = 0;
                brf_entity->optional_data[VNORM] = 0;
                brf_entity->optional_data[FCOLORT] = 0;
                brf_entity->optional_data[FCOLORI] = 0;
                brf_entity->optional_data[FNORM] = 0;
                brf_entity->optional_data[EDATA] = 0;
		brf_entity->numCoords 		= 0;
		brf_entity->numFacets 		= 0;
		brf_entity->numEdges 		= 0;
		brf_entity->numContours 	= 0;
		brf_entity->vcolr		= 0;
		brf_entity->vnorm		= 0;
		brf_entity->fcolr		= 0;
		brf_entity->fnorm		= 0;
		brf_entity->evisi		= 0;
	break;
	case BRF_REPORT:
		return_value = brf_utl_report1(num_frames,brf_entity);
	break;
	case BRF_REPORT2:
		return_value = brf_utl_report2(num_frames,brf_entity);
	break;
	case BRF_REPORT3:
		return_value = brf_utl_report3(num_frames,brf_entity);
	break;
	}
	return(return_value);
}/*do_brffillareaset3*/
/*--------------------------------------------------------------------*\
| Procedure     :	do_brftriangle3( BIF_Traverser_state, 
|			BIF_Polygon, BRF_Any_With_Data, BRF_state)
|---------------------------------------------------------------------
| Description   :	Records and reports usage information for the BRF
|---------------------------------------------------------------------
| Return        :	FALSE if nothing printed in for BRF_REPORT,2,3
\*--------------------------------------------------------------------*/
int do_brftriangle3( traverser_state,triangle3,brf_entity, brf_state)
BIF_Traverser_state *traverser_state;
BIF_Triangle3 *triangle3;
BRF_Any_With_Data *brf_entity;
BRF_state *brf_state;
{/*do_brftriangle3*/
char    temp_str[80];
int k, brf_flag, num_frames;
int return_value;
	return_value = TRUE;
	brf_flag = brf_state->brf_flag;
	num_frames = brf_state->brf_num_frames;
	switch(brf_flag)
	{
	case BRF_COLLECT:
		brf_entity->times_called+= num_frames;
		brf_entity->numCoords   += triangle3->numCoords*num_frames;
		brf_entity->numFacets   += triangle3->numFacets*num_frames;
		brf_entity->numEdges    += triangle3->numEdges*num_frames;
		brf_entity->numContours += triangle3->numFacets*num_frames;
		if (triangle3->with_data_flag[VCOLORT] ||
		    triangle3->with_data_flag[VCOLORI])
			brf_entity->vcolr+= triangle3->numCoords*num_frames;
		if (triangle3->with_data_flag[VNORM])
			brf_entity->vnorm+= triangle3->numCoords*num_frames;

		if (triangle3->with_data_flag[FCOLORT] ||
		    triangle3->with_data_flag[FCOLORI])
			brf_entity->fcolr+= triangle3->numFacets*num_frames;
		if (triangle3->with_data_flag[FNORM])
			brf_entity->fnorm+= triangle3->numFacets*num_frames;
		if (triangle3->with_data_flag[EDATA])
			brf_entity->evisi += triangle3->numEdges*num_frames;
		do_brftotalpoly(traverser_state,triangle3,
				(BRF_Any_With_Data *)brf_entity->brf_totals,
				brf_state);
	break;
	case BRF_INITIALIZE:
		brf_entity->times_called 	= 0;
#ifdef EXTERNALNOTE
        /* see comment at line 44   */
#endif
        /* added for error reporting */
                brf_entity->exceptions = 0;
                brf_entity->optional_data[VCOLORT] = 0;
                brf_entity->optional_data[VCOLORI] = 0;
                brf_entity->optional_data[VNORM] = 0;
                brf_entity->optional_data[FCOLORT] = 0;
                brf_entity->optional_data[FCOLORI] = 0;
                brf_entity->optional_data[FNORM] = 0;
                brf_entity->optional_data[EDATA] = 0;
		brf_entity->numCoords 		= 0;
		brf_entity->numFacets 		= 0;
		brf_entity->numEdges 		= 0;
		brf_entity->numContours 	= 0;
		brf_entity->vcolr		= 0;
		brf_entity->vnorm		= 0;
		brf_entity->fcolr		= 0;
		brf_entity->fnorm		= 0;
		brf_entity->evisi		= 0;
	break;
	case BRF_REPORT:
		return_value = brf_utl_report1(num_frames,brf_entity);
	break;
	case BRF_REPORT2:
		return_value = brf_utl_report2(num_frames,brf_entity);
	break;
	case BRF_REPORT3:
		return_value = brf_utl_report3(num_frames,brf_entity);
	break;
	}
	return(return_value);
}/*do_brftriangle3*/
/*--------------------------------------------------------------------*\
| Procedure     :	do_brfquadmesh( BIF_Traverser_state, 
|			BIF_Polygon, BRF_Any_With_Data, BRF_state)
|---------------------------------------------------------------------
| Description   :	Records and reports usage information for the BRF
|---------------------------------------------------------------------
| Return        :	FALSE if nothing printed in for BRF_REPORT,2,3
\*--------------------------------------------------------------------*/
int do_brfquadmesh3( traverser_state,quadmesh3,brf_entity, brf_state)
BIF_Traverser_state *traverser_state;
BIF_Quadmesh3 *quadmesh3;
BRF_Any_With_Data *brf_entity;
BRF_state *brf_state;
{/*do_brfquadmesh3*/
char    temp_str[80];
int k, brf_flag, num_frames;
int return_value;
	return_value = TRUE;
	brf_flag = brf_state->brf_flag;
	num_frames = brf_state->brf_num_frames;
	switch(brf_flag)
	{
	case BRF_COLLECT:
		brf_entity->times_called+= num_frames;
		brf_entity->numCoords   += quadmesh3->numCoords*num_frames;
		brf_entity->numFacets   += quadmesh3->numFacets*num_frames;
		brf_entity->numEdges    += quadmesh3->numEdges*num_frames;
		brf_entity->numContours += quadmesh3->numFacets*num_frames;
		if (quadmesh3->with_data_flag[VCOLORT] ||
		    quadmesh3->with_data_flag[VCOLORI])
			brf_entity->vcolr+= quadmesh3->numCoords*num_frames;
		if (quadmesh3->with_data_flag[VNORM])
			brf_entity->vnorm+= quadmesh3->numCoords*num_frames;

		if (quadmesh3->with_data_flag[FCOLORT] ||
		    quadmesh3->with_data_flag[FCOLORI])
			brf_entity->fcolr+= quadmesh3->numFacets*num_frames;
		if (quadmesh3->with_data_flag[FNORM])
			brf_entity->fnorm+= quadmesh3->numFacets*num_frames;
		if (quadmesh3->with_data_flag[EDATA])
			brf_entity->evisi += quadmesh3->numEdges*num_frames;
		do_brftotalpoly(traverser_state,quadmesh3,
				(BRF_Any_With_Data *)brf_entity->brf_totals,
				brf_state);
	break;
	case BRF_INITIALIZE:
		brf_entity->times_called 	= 0;
#ifdef EXTERNALNOTE
        /* see comment at line 44   */
#endif
        /* added for error reporting */
                brf_entity->exceptions = 0;
                brf_entity->optional_data[VCOLORT] = 0;
                brf_entity->optional_data[VCOLORI] = 0;
                brf_entity->optional_data[VNORM] = 0;
                brf_entity->optional_data[FCOLORT] = 0;
                brf_entity->optional_data[FCOLORI] = 0;
                brf_entity->optional_data[FNORM] = 0;
                brf_entity->optional_data[EDATA] = 0;
		brf_entity->numCoords 		= 0;
		brf_entity->numFacets 		= 0;
		brf_entity->numEdges 		= 0;
		brf_entity->numContours 	= 0;
		brf_entity->vcolr		= 0;
		brf_entity->vnorm		= 0;
		brf_entity->fcolr		= 0;
		brf_entity->fnorm		= 0;
		brf_entity->evisi		= 0;
	break;
	case BRF_REPORT:
		return_value = brf_utl_report1(num_frames,brf_entity);
	break;
	case BRF_REPORT2:
		return_value = brf_utl_report2(num_frames,brf_entity);
	break;
	case BRF_REPORT3:
		return_value = brf_utl_report3(num_frames,brf_entity);
	break;
	}
	return(return_value);
}/*do_brfquadmesh3*/
/*--------------------------------------------------------------------*\
| Procedure     :	do_brfindexpolygons( BIF_Traverser_state, 
|			BIF_Polygon, BRF_Any_With_Data, BRF_state)
|---------------------------------------------------------------------
| Description   :	Records and reports usage information for the BRF
|---------------------------------------------------------------------
| Return        :	FALSE if nothing printed in for BRF_REPORT,2,3
\*--------------------------------------------------------------------*/
int do_brfindexpolygons3( traverser_state,indexpolygons3,brf_entity, brf_state)
BIF_Traverser_state *traverser_state;
BIF_Indexpolygons3 *indexpolygons3;
BRF_Any_With_Data *brf_entity;
BRF_state *brf_state;
{/*do_brfindexpolygons3*/
int k, brf_flag, num_frames;
char    temp_str[80];
char    temp_str2[80];
int return_value;
	return_value = TRUE;
	brf_flag = brf_state->brf_flag;
	num_frames = brf_state->brf_num_frames;

	switch(brf_flag)
	{
	case BRF_COLLECT:
		brf_entity->times_called+= num_frames;
		brf_entity->numCoords   += indexpolygons3->numCoords*num_frames;
		brf_entity->numFacets   += indexpolygons3->numFacets*num_frames;
		brf_entity->numEdges    += indexpolygons3->numEdges*num_frames;
		brf_entity->numContours += indexpolygons3->numFacets*num_frames;
		if (indexpolygons3->with_data_flag[VCOLORT] ||
		    indexpolygons3->with_data_flag[VCOLORI])
			brf_entity->vcolr+= indexpolygons3->numCoords
						*num_frames;
		if (indexpolygons3->with_data_flag[VNORM])
			brf_entity->vnorm+= indexpolygons3->numCoords
						*num_frames;

		if (indexpolygons3->with_data_flag[FCOLORT] ||
		    indexpolygons3->with_data_flag[FCOLORI])
			brf_entity->fcolr+= indexpolygons3->numFacets
						*num_frames;
		if (indexpolygons3->with_data_flag[FNORM])
			brf_entity->fnorm+= indexpolygons3->numFacets
						*num_frames;
		if (indexpolygons3->with_data_flag[EDATA])
			brf_entity->evisi += indexpolygons3->numEdges
						*num_frames;
		do_brftotalpoly(traverser_state,indexpolygons3,
				(BRF_Any_With_Data *)brf_entity->brf_totals,
				brf_state);
	break;
	case BRF_INITIALIZE:
		brf_entity->times_called 	= 0;
#ifdef EXTERNALNOTE
        /* see comment at line 44   */
#endif
        /* added for error reporting */
                brf_entity->exceptions = 0;
                brf_entity->optional_data[VCOLORT] = 0;
                brf_entity->optional_data[VCOLORI] = 0;
                brf_entity->optional_data[VNORM] = 0;
                brf_entity->optional_data[FCOLORT] = 0;
                brf_entity->optional_data[FCOLORI] = 0;
                brf_entity->optional_data[FNORM] = 0;
                brf_entity->optional_data[EDATA] = 0;
		brf_entity->numCoords 		= 0;
		brf_entity->numFacets 		= 0;
		brf_entity->numEdges 		= 0;
		brf_entity->numContours 	= 0;
		brf_entity->vcolr		= 0;
		brf_entity->vnorm		= 0;
		brf_entity->fcolr		= 0;
		brf_entity->fnorm		= 0;
		brf_entity->evisi		= 0;
	break;
	case BRF_REPORT:
		return_value = brf_utl_report1(num_frames,brf_entity);
	break;
	case BRF_REPORT2:
		return_value = brf_utl_report2(num_frames,brf_entity);
	break;
	case BRF_REPORT3:
		return_value = brf_utl_report3(num_frames,brf_entity);
	break;
	}
	return(return_value);
}/*end do_brfindexpolygons3*/

/*----------------------------------------------------------------------*\
| Procedure	:	open_reportfile( *char )
|------------------------------------------------------------------------
| Description	:	Opens the BRF report file
|------------------------------------------------------------------------
| Return	: 
\*----------------------------------------------------------------------*/
open_reportfile(file_name)
char *file_name;
{/*open_reportfile*/
	if ( strlen(file_name) > STRLENGTH )
	{
		yyerror("Report file name too long. Open ignored.");
	} else
	{
		strcpy(brf_filename,file_name);
		brf_file = fopen(file_name,"w");
		brf_used_flag = UNUSED;
		if ( brf_file == NULL )
		{
			yyerror("FATAL ERROR: Unable to open report file");
			exit(-1);
		}
		BRF_printHeader ( brf_file );
	}
}/*open_reportfile*/

/*----------------------------------------------------------------------*\
| Procedure	:	close_reportfile()
|------------------------------------------------------------------------
| Description	:	Closes the BRF report file
|------------------------------------------------------------------------
| Return	: 
\*----------------------------------------------------------------------*/
close_reportfile()
{/*close_reportfile*/
	strcpy(brf_filename,"");
	if (brf_used_flag != UNOPENED)
		fclose(brf_file);
	brf_used_flag = UNOPENED;
}/*close_reportfile*/

/*----------------------------------------------------------------------*\
| Procedure	:	check_default_report(char *)
|------------------------------------------------------------------------
| Description	:	Opens report file
|------------------------------------------------------------------------
| Return	: 
\*----------------------------------------------------------------------*/
check_default_report(file_name)
char *file_name;
{/*check_default_report*/
	if ( strlen(file_name) > STRLENGTH )
	{
		yyerror("Report file name too long. Open ignored.");
	} else
	{
		if(brf_used_flag == UNOPENED)
			{
			strcpy(brf_filename,file_name);
			brf_file = fopen(file_name,"w");
			brf_used_flag = UNUSED;
			if ( brf_file == NULL )
			{
				yyerror(
				"FATAL ERROR: Unable to open report file");
				exit(-1);
			}
			BRF_printHeader ( brf_file );
		}
	}
}/*check_default_report*/



/*----------------------------------------------------------------------*\
| Procedure	:	nice_float_8(float)
|------------------------------------------------------------------------
| Description	:	Fromats a nice floating point number
|------------------------------------------------------------------------
| Return	: 
\*----------------------------------------------------------------------*/
char *nice_float_8(f_value)
float f_value;
{
	static char	out_string[80];

	if ( fabs(f_value) >= 1.0e+100 )
	   sprintf(out_string,"REAL BIG");
	else if ( fabs(f_value) > 999999.0 )
	   sprintf(out_string,"%8.2e", f_value);
	else if ( (fabs(f_value) < 1.0) && (fabs(f_value) > 0.000010) )
	   sprintf(out_string," %7.5f", f_value);
	else if ( (fabs(f_value) <= 0.000010) && (fabs(f_value) > 1.0e-100) )
	   sprintf(out_string,"%8.2e", f_value);
	else 
	   sprintf(out_string,"%8.1f", f_value);
	strcat (out_string,"  ");
	return(out_string);
}

/*----------------------------------------------------------------------*\
| Procedure	:	brf_utl_report1(int,BRF_Any_With_Data* )
|------------------------------------------------------------------------
| Description	:	Producde line of printout for BRF_REPORT
|------------------------------------------------------------------------
| Return	: 
\*----------------------------------------------------------------------*/
brf_utl_report1(num_frames,brf_entity)
int num_frames;
BRF_Any_With_Data *brf_entity;
{
char	temp_str[80];
int return_value;
	return_value = TRUE;
	if(num_frames >0)
	{
	   if ( brf_entity->times_called > 0 )
	   {
		NICE_SPRINTF8( temp_str, 
		   ( (float)brf_entity->times_called/ (float)num_frames ));

		NICE_CAT_ENT_NAME_D

		NICE_CAT8(temp_str,
			(float)brf_entity->numCoords/
	 		(float)brf_entity->times_called);

		NICE_CAT8(temp_str,
			(float)brf_entity->numFacets/
			(float)brf_entity->times_called);

		NICE_CAT8(temp_str,
			(float)brf_entity->numEdges/
			(float)brf_entity->times_called);

		NICE_CAT8(temp_str,
			(float)brf_entity->numContours/
			(float)brf_entity->times_called);

		BRF_appendEndBar(temp_str);
		fprintf(brf_file,temp_str);
	   } else return_value = FALSE;
	}
	return(return_value);
}
/*----------------------------------------------------------------------*\
| Procedure	:	brf_utl_report2(int,BRF_Any_With_Data* )
|------------------------------------------------------------------------
| Description	:	Producde line of printout for BRF_REPORT2
|------------------------------------------------------------------------
| Return	:	FALSE if nothing printed in for BRF_REPORT,2,3
\*----------------------------------------------------------------------*/
brf_utl_report2(num_frames,brf_entity)
int num_frames;
BRF_Any_With_Data *brf_entity;
{
char	temp_str[80];
int return_value;
	return_value = TRUE;
	if(num_frames >0)
	{
	   if ( brf_entity->times_called > 0 )
	   {
		NICE_SPRINTF8( temp_str, 
		   ( (float)brf_entity->times_called/ (float)num_frames ));

		NICE_CAT_ENT_NAME_D
		NICE_CAT8(temp_str,
			(float)brf_entity->vcolr/
	 		(float)brf_entity->times_called);

		NICE_CAT8(temp_str,
			(float)brf_entity->vnorm/
			(float)brf_entity->times_called);

		NICE_CAT8(temp_str,
			(float)brf_entity->fcolr/
			(float)brf_entity->times_called);

		NICE_CAT8(temp_str,
			(float)brf_entity->fnorm/
			(float)brf_entity->times_called);

		BRF_appendEndBar(temp_str);
		fprintf(brf_file,temp_str);
	   } else return_value = FALSE;
	}
	return(return_value);
}
/*----------------------------------------------------------------------*\
| Procedure	:	brf_utl_report3(int,BRF_Any_With_Data* )
|------------------------------------------------------------------------
| Description	:	Producde line of printout for BRF_REPORT3
|------------------------------------------------------------------------
| Return	:	FALSE if nothing printed in for BRF_REPORT,2,3
\*----------------------------------------------------------------------*/
brf_utl_report3(num_frames,brf_entity)
int num_frames;
BRF_Any_With_Data *brf_entity;
{
char	temp_str[80];
int return_value;
	return_value = TRUE;
	if(num_frames >0)
	{
	   if ( brf_entity->times_called > 0 )
	   {
		NICE_SPRINTF8( temp_str, 
		   ( (float)brf_entity->times_called/ (float)num_frames ));

		NICE_CAT_ENT_NAME_D
		NICE_CAT8(temp_str,
			(float)brf_entity->evisi/
	 		(float)brf_entity->times_called);
		BRF_appendEndBar(temp_str);
		fprintf(brf_file,temp_str);
	   } else return_value = FALSE;
	}
	return(return_value);
}
