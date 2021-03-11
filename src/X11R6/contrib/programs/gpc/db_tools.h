/* $XConsortium: db_tools.h,v 5.1 91/02/16 10:07:26 rws Exp $ */

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
| Author        :	jmz / SimGraphics Engineering Corportation
|
| File          :	db_tools.h
| Date          :	3/18/89
| Project       :	PLB
| Description	:	The return types of the do_tools functions.
| Status	:	Version 1.0
|
| Revisions     :	
|
\*--------------------------------------------------------------------*/

int			contour_counter();
int			contour_sizes();
BIF_All			*end_of_list();
void			free_all_list();
BIF_Beginstructure	*db_inq_str();
void			db_add_str();
BIF_Beginstructure	*db_get_str();
void			db_empty_str();
void			db_clear_all();
int			setNextLabel();
int			getNextLabel();
int			addToList();
int			delFromList();
int			expandAllStructures();
int			expandStructure();
int			collapseAllStructures();
int			collapseStructure();
int			openUsingLinkList();

