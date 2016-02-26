
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
	file: command.c
	purpose: This file contains the icon data for the command icons
		in the command menu of the righthand side of the main window

	modifications:
		date:	Tue Mar 22 22:04:58 EST 1988
		author:	rayk
		changes:add comments
**************************************************************************/
#include <pixrect/pixrect_hs.h>


static short command1_data[] = {
#include "laso.cicon.pat"
};
static mpr_static(command1_pr, 48, 48, 1, command1_data);

static short command2_data[] = {
#include "circle_h.cicon.pat"
};
static mpr_static(command2_pr, 48, 48, 1, command2_data);

static short command3_data[] = {
#include "draw.cicon.pat"
};
static mpr_static(command3_pr, 48, 48, 1, command3_data);

static short command4_data[] = {
#include "line.cicon.pat"
};
static mpr_static(command4_pr, 48, 48, 1, command4_data);

static short command5_data[] = {
#include "mag.cicon.pat"
};
static mpr_static(command5_pr, 48, 48, 1, command5_data);

static short command6_data[] = {
#include "ffill.cicon.pat"
};
static mpr_static(command6_pr, 48, 48, 1, command6_data);

static short command7_data[] = {
#include "oval_h.cicon.pat"
};
static mpr_static(command7_pr, 48, 48, 1, command7_data);

static short command8_data[] = {
#include "poly_f.cicon.pat"
};
static mpr_static(command8_pr, 48, 48, 1, command8_data);

static short command9_data[] = {
#include "poly_h.cicon.pat"
};
static mpr_static(command9_pr, 48, 48, 1, command9_data);

static short command10_data[] = {
#include "rectan_f.cicon.pat"
};
static mpr_static(command10_pr, 48, 48, 1, command10_data);

static short command11_data[] = {
#include "rectan_h.cicon.pat"
};
static mpr_static(command11_pr, 48, 48, 1, command11_data);

static short command12_data[] = {
#include "text.cicon.pat"
};
static mpr_static(command12_pr, 48, 48, 1, command12_data);

static short command13_data[] = {
#include "sel_reg.cicon.pat"
};
static mpr_static(command13_pr, 48, 48, 1, command13_data);

static short command14_data[] = {
#include "sel_point.cicon.pat"
};
static mpr_static(command14_pr, 48, 48, 1, command14_data);

static short command15_data[] = {
#include "paint.cicon.pat"
};
static mpr_static(command15_pr, 48, 48, 1, command15_data);

static short command16_data[] = {
#include "erase.cicon.pat"
};
static mpr_static(command16_pr, 48, 48, 1, command16_data);

/*
 * SELECTED REGION COMMANDS
 */

static short reg_command1_data[] = {
#include "cut.cicon.pat"
};
static mpr_static(reg_command1_pr, 48, 48, 1, reg_command1_data);

static short reg_command2_data[] = {
#include "flip_hor.cicon.pat"
};
static mpr_static(reg_command2_pr, 48, 48, 1, reg_command2_data);

static short reg_command3_data[] = {
#include "flip_ver.cicon.pat"
};
static mpr_static(reg_command3_pr, 48, 48, 1, reg_command3_data);

static short reg_command4_data[] = {
#include "inverse.cicon.pat"
};
static mpr_static(reg_command4_pr, 48, 48, 1, reg_command4_data);

static short reg_command5_data[] = {
#include "copy.cicon.pat"
};
static mpr_static(reg_command5_pr, 48, 48, 1, reg_command5_data);

static short reg_command6_data[] = {
#include "paste.cicon.pat"
};
static mpr_static(reg_command6_pr, 48, 48, 1, reg_command6_data);

static short reg_command7_data[] = {
#include "rotate.cicon.pat"
};
static mpr_static(reg_command7_pr, 48, 48, 1, reg_command7_data);

static short reg_command8_data[] = {
#include "move.cicon.pat"
};
static mpr_static(reg_command8_pr, 48, 48, 1, reg_command8_data);

static short reg_command9_data[] = {
#include "scale.cicon.pat"
};
static mpr_static(reg_command9_pr, 48, 48, 1, reg_command9_data);

/*
 * TEXT COMMANDS
 */

static short text_center_data[] = {
#include "center.cicon.pat"
};
static mpr_static(text_center_pr, 48, 48, 1, text_center_data);

static short text_right_data[] = {
#include "right.cicon.pat"
};
static mpr_static(text_right_pr, 48, 48, 1, text_right_data);

static short text_left_data[] = {
#include "left.cicon.pat"
};
static mpr_static(text_left_pr, 48, 48, 1, text_left_data);
