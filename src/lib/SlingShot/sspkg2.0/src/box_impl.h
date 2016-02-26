/*
 *	(c) Copyright 1991 Sun Microsystems, Inc.  All rights reserved.
 *	See LEGAL_NOTICE file for terms and restrictions.
 */

#ifndef DRAWOBJ_IMPL_DEFINED
#define DRAWOBJ_IMPL_DEFINED

/* @(#)box_impl.h 1.4 92/05/04 */

#include <sspkg/rectobj.h>
#include <sspkg/box.h>

typedef struct {
        Rectobj_struct  parent_data;
        Xv_opaque       private_data;
} Cbox_struct;

typedef struct {
        Rectobj_struct  parent_data;
        Xv_opaque       private_data;
} Box_struct;

#endif

