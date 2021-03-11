
/*
 * xmphone.h - xmphone header information
 *
 * Author:      Richard Bingle
 *              Dept. of Computer Sciences
 *              Purdue University
 */

/***********************************************************************
* Copyright 1991-1994 by Richard Bingle and Purdue University.  All rights 
* reserved.  Some individual files may be covered by other copyrights.
* 
* Redistribution and use in source and binary forms are permitted
* provided that this entire copyright notice is duplicated in all such
* copies.  Neither the name of the University, nor the name of the author
* may be used to endorse or promote products derived from this material 
* without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
* WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
* MERCHANTIBILITY AND FITNESS FOR ANY PARTICULAR PURPOSE.
************************************************************************/


#include <stdio.h>
#include <X11/Xos.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <Xm/Xm.h>
#include <Xm/BulletinB.h>
#include <Xm/CascadeB.h>
#include <Xm/DialogS.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/MainW.h>
#include <Xm/MessageB.h>
#include <Xm/PushB.h>
#include <Xm/RowColumn.h>
#include <Xm/ScrollBar.h>
#include <Xm/ScrolledW.h>
#include <Xm/SelectioB.h>
#include <Xm/Separator.h>
#include <Xm/Text.h>
#include <Xm/ToggleB.h>
#include <X11/Xmp/Table.h>
#include <pwd.h>

#define MAX_ARGS 10
#define MAX_WIDTH 72

struct msg_struct {
    Widget to;
    Widget who;
    Widget of;
    Widget pn;
    Widget togs[MAX_ARGS];
    Widget msg;
};

struct resource_struct {
    Boolean bcc;
    Boolean verify;
    int numtogs;
    int msgwidth;
} resources;
