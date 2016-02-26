
/**************************************************************************
   Touchup a bitmap graphics editor for the Sun Workstation running SunView
   Copyright (c) 1988 by Raymond Kreisel
   1/22/88 @ Suny Stony Brook

   This program may be redistributed without fee as long as this copyright
   notice is intact.

==> PLEASE send comments and bug reports to one of the following addresses:

	   Ray Kreisel
	   CS Dept., SUNY at Stony Brook, Stony Brook NY 11794

	   UUCP: {allegra, philabs, pyramid, research}!sbcs!rayk   
	   ARPA-Internet: rayk@sbcs.sunysb.edu			
	   CSnet: rayk@suny-sb
	   (If nobody is home at any of the above addresses try:
		S72QKRE@TOWSONVX.BITNET			        )

 "If I get home before daylight, I just might get some sleep tonight...."

**************************************************************************/

#include <pixrect/pixrect_hs.h>

/**************************************************************************
	file: brush.c
	purpose: This file contains all that differents brush styles.
		Brushes can be created with iconedit, using the upper
		righthand 32x32 bits.  The 64x64 image from iconedit
		is then stripped to 32x32 by strip_icon32x32

	modifications:
		date:	Tue Mar 22 22:04:58 EST 1988
		author:	rayk
		changes:add comments
**************************************************************************/

static short brush1_data[] = {
#include "brush1.icon.pat"
};
static mpr_static(brush1_pr, 32, 32, 1, brush1_data);

static short brush2_data[] = {
#include "brush2.icon.pat"
};
static mpr_static(brush2_pr, 32, 32, 1, brush2_data);

static short brush3_data[] = {
#include "brush3.icon.pat"
};
static mpr_static(brush3_pr, 32, 32, 1, brush3_data);

static short brush4_data[] = {
#include "brush4.icon.pat"
};
static mpr_static(brush4_pr, 32, 32, 1, brush4_data);

static short brush5_data[] = {
#include "brush5.icon.pat"
};
static mpr_static(brush5_pr, 32, 32, 1, brush5_data);

static short brush6_data[] = {
#include "brush6.icon.pat"
};
static mpr_static(brush6_pr, 32, 32, 1, brush6_data);

static short brush7_data[] = {
#include "brush7.icon.pat"
};
static mpr_static(brush7_pr, 32, 32, 1, brush7_data);
