/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms and restrictions.
 */

#ifndef DIMPL_IMPL_DEFINED
#define DIMPL_IMPL_DEFINED

/* @(#)dimage_impl.h 1.3 92/07/08 */

#include <xview/svrimage.h>
 
typedef struct {
	Server_image	image;
	Server_image	mask;	/* must be one bit deep, same size as image */
	short		depth;
	short		width;
	short		height;
} Drawimage_image;

typedef struct drawimage_info {
	Drawimage_image image1; /*normal*/
	Drawimage_image image2; /*highlight*/
} Drawimage_info;

#define DRAWIMAGE_PRIVATE(drawimage)	\
		XV_PRIVATE(Drawimage_info, Drawimage_struct, drawimage)

EXTERN_FUNCTION(void drawimage_set_attr, (Drawimage_info*, Attr_attribute, void*));
EXTERN_FUNCTION(void drawimage_calc_rect, (Drawimage));

/* Used by drawicon */
extern Drawimage_info	*drawicon_private_diinfo;

#endif

