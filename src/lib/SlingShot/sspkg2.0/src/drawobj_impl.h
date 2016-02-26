/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms and restrictions.
 */

#ifndef DRAWOBJ_IMPL_DEFINED
#define DRAWOBJ_IMPL_DEFINED

/* @(#)drawobj_impl.h 1.17 91/11/09 */

#include <sspkg/rectobj.h>

typedef struct {
        Rectobj_struct  parent_data;
        Xv_opaque       private_data;
} Drawrect_struct;

typedef struct {
        Rectobj_struct  parent_data;
        Xv_opaque       private_data;
} Drawline_struct;

typedef struct {
        Rectobj_struct  parent_data;
        Xv_opaque       private_data;
} Drawimage_struct;
 
typedef struct {
        Rectobj_struct  parent_data;
        Xv_opaque       private_data;
} Drawtext_struct;
 
typedef struct {
        Rectobj_struct  parent_data;
        Xv_opaque       private_data;
} Drawicon_struct;
 
typedef struct {
        Rectobj_struct  parent_data;
        Xv_opaque       private_data;
} Drawarea_struct;
 
typedef struct {
        Rectobj_struct  parent;
        Xv_opaque       private_data;
} Tacho_struct;
 
typedef struct {
        Drawarea_struct parent_data;
        Xv_opaque       private_data;
} Clockobj_struct;
 
typedef struct {
	Drawimage_struct	parent_data;
	Xv_opaque		private_data;
} Grip_struct;

#endif

