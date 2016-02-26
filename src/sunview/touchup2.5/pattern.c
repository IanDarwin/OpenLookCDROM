
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
/**************************************************************************
	file: pattern.c
	purpose: this file has all of the paint patterns

	modifications:
		date:	Tue Mar 22 22:04:58 EST 1988
		author:	rayk
		changes:add comments
**************************************************************************/

#include <pixrect/pixrect_hs.h>


static short pattern1_data[] = {
#include "pattern1.icon.pat"
};
static mpr_static(pattern1_pr, 32, 32, 1, pattern1_data);

static short pattern2_data[] = {
#include "pattern2.icon.pat"
};
static mpr_static(pattern2_pr, 32, 32, 1, pattern2_data);

static short pattern3_data[] = {
#include "pattern3.icon.pat"
};
static mpr_static(pattern3_pr, 32, 32, 1, pattern3_data);

static short pattern4_data[] = {
#include "pattern4.icon.pat"
};
static mpr_static(pattern4_pr, 32, 32, 1, pattern4_data);

static short pattern5_data[] = {
#include "pattern5.icon.pat"
};
static mpr_static(pattern5_pr, 32, 32, 1, pattern5_data);

static short pattern6_data[] = {
#include "pattern6.icon.pat"
};
static mpr_static(pattern6_pr, 32, 32, 1, pattern6_data);

static short pattern7_data[] = {
#include "pattern7.icon.pat"
};
static mpr_static(pattern7_pr, 32, 32, 1, pattern7_data);

static short pattern8_data[] = {
#include "pattern8.icon.pat"
};
static mpr_static(pattern8_pr, 32, 32, 1, pattern8_data);

static short pattern9_data[] = {
#include "pattern9.icon.pat"
};
static mpr_static(pattern9_pr, 32, 32, 1, pattern9_data);

static short pattern10_data[] = {
#include "pattern10.icon.pat"
};
static mpr_static(pattern10_pr, 32, 32, 1, pattern10_data);

static short pattern11_data[] = {
#include "pattern11.icon.pat"
};
static mpr_static(pattern11_pr, 32, 32, 1, pattern11_data);

static short pattern12_data[] = {
#include "pattern12.icon.pat"
};
static mpr_static(pattern12_pr, 32, 32, 1, pattern12_data);

static short pattern13_data[] = {
#include "pattern13.icon.pat"
};
static mpr_static(pattern13_pr, 32, 32, 1, pattern13_data);

static short pattern14_data[] = {
#include "pattern14.icon.pat"
};
static mpr_static(pattern14_pr, 32, 32, 1, pattern14_data);

static short pattern15_data[] = {
#include "pattern15.icon.pat"
};
static mpr_static(pattern15_pr, 32, 32, 1, pattern15_data);

static short pattern16_data[] = {
#include "pattern16.icon.pat"
};
static mpr_static(pattern16_pr, 32, 32, 1, pattern16_data);

static short pattern17_data[] = {
#include "pattern17.icon.pat"
};
static mpr_static(pattern17_pr, 32, 32, 1, pattern17_data);

static short pattern18_data[] = {
#include "pattern18.icon.pat"
};
static mpr_static(pattern18_pr, 32, 32, 1, pattern18_data);

static short pattern19_data[] = {
#include "pattern19.icon.pat"
};
static mpr_static(pattern19_pr, 32, 32, 1, pattern19_data);

static short pattern20_data[] = {
#include "pattern20.icon.pat"
};
static mpr_static(pattern20_pr, 32, 32, 1, pattern20_data);

static short pattern21_data[] = {
#include "pattern21.icon.pat"
};
static mpr_static(pattern21_pr, 32, 32, 1, pattern21_data);

static short pattern22_data[] = {
#include "pattern22.icon.pat"
};
static mpr_static(pattern22_pr, 32, 32, 1, pattern22_data);

static short pattern23_data[] = {
#include "pattern23.icon.pat"
};
static mpr_static(pattern23_pr, 32, 32, 1, pattern23_data);

static short pattern24_data[] = {
#include "pattern24.icon.pat"
};
static mpr_static(pattern24_pr, 32, 32, 1, pattern24_data);

static short pattern25_data[] = {
#include "pattern25.icon.pat"
};
static mpr_static(pattern25_pr, 32, 32, 1, pattern25_data);

static short pattern26_data[] = {
#include "pattern26.icon.pat"
};
static mpr_static(pattern26_pr, 32, 32, 1, pattern26_data);

static short pattern27_data[] = {
#include "pattern27.icon.pat"
};
static mpr_static(pattern27_pr, 32, 32, 1, pattern27_data);

static short pattern28_data[] = {
#include "pattern28.icon.pat"
};
static mpr_static(pattern28_pr, 32, 32, 1, pattern28_data);

static short pattern29_data[] = {
#include "pattern29.icon.pat"
};
static mpr_static(pattern29_pr, 32, 32, 1, pattern29_data);

static short pattern30_data[] = {
#include "pattern30.icon.pat"
};
static mpr_static(pattern30_pr, 32, 32, 1, pattern30_data);

static short pattern31_data[] = {
#include "pattern31.icon.pat"
};
static mpr_static(pattern31_pr, 32, 32, 1, pattern31_data);

static short pattern32_data[] = {
#include "pattern32.icon.pat"
};
static mpr_static(pattern32_pr, 32, 32, 1, pattern32_data);

static short pattern33_data[] = {
#include "pattern33.icon.pat"
};
static mpr_static(pattern33_pr, 32, 32, 1, pattern33_data);

static short pattern34_data[] = {
#include "pattern34.icon.pat"
};
static mpr_static(pattern34_pr, 32, 32, 1, pattern34_data);

static short pattern35_data[] = {
#include "pattern35.icon.pat"
};
static mpr_static(pattern35_pr, 32, 32, 1, pattern35_data);

static short pattern36_data[] = {
#include "pattern36.icon.pat"
};
static mpr_static(pattern36_pr, 32, 32, 1, pattern36_data);

static short pattern37_data[] = {
#include "pattern37.icon.pat"
};
static mpr_static(pattern37_pr, 32, 32, 1, pattern37_data);

static short pattern38_data[] = {
#include "pattern38.icon.pat"
};
static mpr_static(pattern38_pr, 32, 32, 1, pattern38_data);

static short pattern39_data[] = {
#include "pattern39.icon.pat"
};
static mpr_static(pattern39_pr, 32, 32, 1, pattern39_data);

static short pattern40_data[] = {
#include "pattern40.icon.pat"
};
static mpr_static(pattern40_pr, 32, 32, 1, pattern40_data);


