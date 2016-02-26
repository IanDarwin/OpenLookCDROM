/*
 * XView port of Mahjongg done by Stan K. Tazuma, 9/91
 *
 * Changes made to this file were mostly cosmetic (pathnames).
 * Also moved the definition of the black/white mahjongg icon into this
 * file from mahjongg.c.
 *
 * $Header: /home/sirius/skt/cmds/src/sun/mj/RCS/icons.c,v 2.3 92/12/21 18:53:24 skt Exp $
 *
 */

/*
 *	Copyright 1988, Mark Holm
 *			Exceptions
 *
 *	Acknowledgments to Dorothy Robinson for her artistic
 *	 abilities in drawing the icons and to Jim Batch for
 *	 technical support and graphical concepts (which I abandoned in favor
 *       of the easy way out).
 *
 *	Permission is given to copy and distribute for non-profit purposes.
 *
 */

/*
 * This file includes some lines that are needed either just for SunView
 * or just for XView.  The lines can be left in for both versions, and
 * the result, when compiled, can be used for linking both the SunView
 * and the XView executable.
 *
 * As set up, the default is to compile in the lines so that icons.o
 * can be shared.  By defining either XVIEW or SUNVIEW, a toolkit
 * specific version of icons.o will be built.
 */
#if ! defined(XVIEW) && ! defined(SUNVIEW)
#define DO_FOR_BOTH
#endif

/*   This is the file for building the icon images for the */
/*	images both color and b/w.                             */

#include <sys/types.h>
#ifdef SUNVIEW
#include <pixrect/pixrect.h>
#include <suntool/sunview.h>
#else
#include <xview/xview.h>
#endif

/* color frame closed icon */

static short		icon_cimage[] = {
#ifdef SWAP
#include "images/swap/mahjongg.icon"
#else
#include "images/color/mahjongg.icon"
#endif
};
mpr_static(cicon_image, 64, 64, 8, icon_cimage);

/* Black and white closed icon image */

short		icon_image[] = {
#include "images/bandw/mahjongg.icon"
};

#if ! defined(SUNVIEW) || defined(DO_FOR_BOTH)
mpr_static(icon_image_pr, 64, 64, 1, icon_image);
#endif

/* cursor icons */

short		stick_image[] = {
#include "images/bandw/mahjongg.cursor"
};
#if defined(SUNVIEW) || defined(DO_FOR_BOTH)
mpr_static(stick, 16, 16, 1, stick_image);
#endif

short		wait_image[] = {
#include "images/hglass.cursor"
};
#if defined(SUNVIEW) || defined(DO_FOR_BOTH)
mpr_static(waiting, 16, 16, 1, wait_image);
#endif

short		confirm_image[] = {
#include "images/confirm.cursor"
};
#if defined(SUNVIEW) || defined(DO_FOR_BOTH)
mpr_static(confirm, 16, 16, 1, confirm_image);
#endif

/* Number images (color) */

static short		NUM0_cimage[] = {
#ifdef SWAP
#include "images/swap/0"
#else
#include "images/color/0"
#endif
};
mpr_static(cNUM0, 64, 64, 8, NUM0_cimage);

static short		NUM1_cimage[] = {
#ifdef SWAP
#include "images/swap/1"
#else
#include "images/color/1"
#endif
};
mpr_static(cNUM1, 64, 64, 8, NUM1_cimage);

static short		NUM2_cimage[] = {
#ifdef SWAP
#include "images/swap/2"
#else
#include "images/color/2"
#endif
};
mpr_static(cNUM2, 64, 64, 8, NUM2_cimage);

static short		NUM3_cimage[] = {
#ifdef SWAP
#include "images/swap/3"
#else
#include "images/color/3"
#endif
};
mpr_static(cNUM3, 64, 64, 8, NUM3_cimage);

static short		NUM4_cimage[] = {
#ifdef SWAP
#include "images/swap/4"
#else
#include "images/color/4"
#endif
};
mpr_static(cNUM4, 64, 64, 8, NUM4_cimage);

static short		NUM5_cimage[] = {
#ifdef SWAP
#include "images/swap/5"
#else
#include "images/color/5"
#endif
};
mpr_static(cNUM5, 64, 64, 8, NUM5_cimage);

static short		NUM6_cimage[] = {
#ifdef SWAP
#include "images/swap/6"
#else
#include "images/color/6"
#endif
};
mpr_static(cNUM6, 64, 64, 8, NUM6_cimage);

static short		NUM7_cimage[] = {
#ifdef SWAP
#include "images/swap/7"
#else
#include "images/color/7"
#endif
};
mpr_static(cNUM7, 64, 64, 8, NUM7_cimage);

static short		NUM8_cimage[] = {
#ifdef SWAP
#include "images/swap/8"
#else
#include "images/color/8"
#endif
};
mpr_static(cNUM8, 64, 64, 8, NUM8_cimage);

static short		NUM9_cimage[] = {
#ifdef SWAP
#include "images/swap/9"
#else
#include "images/color/9"
#endif
};
mpr_static(cNUM9, 64, 64, 8, NUM9_cimage);

/* Number images (B/W) */

static short		NUM0_image[] = {
#include "images/bandw/0"
};
mpr_static(NUM0, 64, 64, 1, NUM0_image);

static short		NUM1_image[] = {
#include "images/bandw/1"
};
mpr_static(NUM1, 64, 64, 1, NUM1_image);

static short		NUM2_image[] = {
#include "images/bandw/2"
};
mpr_static(NUM2, 64, 64, 1, NUM2_image);

static short		NUM3_image[] = {
#include "images/bandw/3"
};
mpr_static(NUM3, 64, 64, 1, NUM3_image);

static short		NUM4_image[] = {
#include "images/bandw/4"
};
mpr_static(NUM4, 64, 64, 1, NUM4_image);

static short		NUM5_image[] = {
#include "images/bandw/5"
};
mpr_static(NUM5, 64, 64, 1, NUM5_image);

static short		NUM6_image[] = {
#include "images/bandw/6"
};
mpr_static(NUM6, 64, 64, 1, NUM6_image);

static short		NUM7_image[] = {
#include "images/bandw/7"
};
mpr_static(NUM7, 64, 64, 1, NUM7_image);

static short		NUM8_image[] = {
#include "images/bandw/8"
};
mpr_static(NUM8, 64, 64, 1, NUM8_image);

static short		NUM9_image[] = {
#include "images/bandw/9"
};
mpr_static(NUM9, 64, 64, 1, NUM9_image);

/* Playing Tiles (color) */

static short		BLANK_cimage[] = {
#ifdef SWAP
#include "images/swap/blank"
#else
#include "images/color/blank"
#endif
};
mpr_static(cBLANK, 64, 64, 8, BLANK_cimage);

static short		BAM1_cimage[] = {
#ifdef SWAP
#include "images/swap/bam1"
#else
#include "images/color/bam1"
#endif
};
mpr_static(cBAM1, 64, 64, 8, BAM1_cimage);

static short		BAM2_cimage[] = {
#ifdef SWAP
#include "images/swap/bam2"
#else
#include "images/color/bam2"
#endif
};
mpr_static(cBAM2, 64, 64, 8, BAM2_cimage);

static short		BAM3_cimage[] = {
#ifdef SWAP
#include "images/swap/bam3"
#else
#include "images/color/bam3"
#endif
};
mpr_static(cBAM3, 64, 64, 8, BAM3_cimage);

static short		BAM4_cimage[] = {
#ifdef SWAP
#include "images/swap/bam4"
#else
#include "images/color/bam4"
#endif
};
mpr_static(cBAM4, 64, 64, 8, BAM4_cimage);

static short		BAM5_cimage[] = {
#ifdef SWAP
#include "images/swap/bam5"
#else
#include "images/color/bam5"
#endif
};
mpr_static(cBAM5, 64, 64, 8, BAM5_cimage);

static short		BAM6_cimage[] = {
#ifdef SWAP
#include "images/swap/bam6"
#else
#include "images/color/bam6"
#endif
};
mpr_static(cBAM6, 64, 64, 8, BAM6_cimage);

static short		BAM7_cimage[] = {
#ifdef SWAP
#include "images/swap/bam7"
#else
#include "images/color/bam7"
#endif
};
mpr_static(cBAM7, 64, 64, 8, BAM7_cimage);

static short		BAM8_cimage[] = {
#ifdef SWAP
#include "images/swap/bam8"
#else
#include "images/color/bam8"
#endif
};
mpr_static(cBAM8, 64, 64, 8, BAM8_cimage);

static short		BAM9_cimage[] = {
#ifdef SWAP
#include "images/swap/bam9"
#else
#include "images/color/bam9"
#endif
};
mpr_static(cBAM9, 64, 64, 8, BAM9_cimage);

static short		DOT1_cimage[] = {
#ifdef SWAP
#include "images/swap/circ1"
#else
#include "images/color/circ1"
#endif
};
mpr_static(cDOT1, 64, 64, 8, DOT1_cimage);

static short		DOT2_cimage[] = {
#ifdef SWAP
#include "images/swap/circ2"
#else
#include "images/color/circ2"
#endif
};
mpr_static(cDOT2, 64, 64, 8, DOT2_cimage);

static short		DOT3_cimage[] = {
#ifdef SWAP
#include "images/swap/circ3"
#else
#include "images/color/circ3"
#endif
};
mpr_static(cDOT3, 64, 64, 8, DOT3_cimage);

static short		DOT4_cimage[] = {
#ifdef SWAP
#include "images/swap/circ4"
#else
#include "images/color/circ4"
#endif
};
mpr_static(cDOT4, 64, 64, 8, DOT4_cimage);

static short		DOT5_cimage[] = {
#ifdef SWAP
#include "images/swap/circ5"
#else
#include "images/color/circ5"
#endif
};
mpr_static(cDOT5, 64, 64, 8, DOT5_cimage);

static short		DOT6_cimage[] = {
#ifdef SWAP
#include "images/swap/circ6"
#else
#include "images/color/circ6"
#endif
};
mpr_static(cDOT6, 64, 64, 8, DOT6_cimage);

static short		DOT7_cimage[] = {
#ifdef SWAP
#include "images/swap/circ7"
#else
#include "images/color/circ7"
#endif
};
mpr_static(cDOT7, 64, 64, 8, DOT7_cimage);

static short		DOT8_cimage[] = {
#ifdef SWAP
#include "images/swap/circ8"
#else
#include "images/color/circ8"
#endif
};
mpr_static(cDOT8, 64, 64, 8, DOT8_cimage);

static short		DOT9_cimage[] = {
#ifdef SWAP
#include "images/swap/circ9"
#else
#include "images/color/circ9"
#endif
};
mpr_static(cDOT9, 64, 64, 8, DOT9_cimage);

static short		CHA1_cimage[] = {
#ifdef SWAP
#include "images/swap/char1"
#else
#include "images/color/char1"
#endif
};
mpr_static(cCHA1, 64, 64, 8, CHA1_cimage);

static short		CHA2_cimage[] = {
#ifdef SWAP
#include "images/swap/char2"
#else
#include "images/color/char2"
#endif
};
mpr_static(cCHA2, 64, 64, 8, CHA2_cimage);

static short		CHA3_cimage[] = {
#ifdef SWAP
#include "images/swap/char3"
#else
#include "images/color/char3"
#endif
};
mpr_static(cCHA3, 64, 64, 8, CHA3_cimage);

static short		CHA4_cimage[] = {
#ifdef SWAP
#include "images/swap/char4"
#else
#include "images/color/char4"
#endif
};
mpr_static(cCHA4, 64, 64, 8, CHA4_cimage);

static short		CHA5_cimage[] = {
#ifdef SWAP
#include "images/swap/char5"
#else
#include "images/color/char5"
#endif
};
mpr_static(cCHA5, 64, 64, 8, CHA5_cimage);

static short		CHA6_cimage[] = {
#ifdef SWAP
#include "images/swap/char6"
#else
#include "images/color/char6"
#endif
};
mpr_static(cCHA6, 64, 64, 8, CHA6_cimage);

static short		CHA7_cimage[] = {
#ifdef SWAP
#include "images/swap/char7"
#else
#include "images/color/char7"
#endif
};
mpr_static(cCHA7, 64, 64, 8, CHA7_cimage);

static short		CHA8_cimage[] = {
#ifdef SWAP
#include "images/swap/char8"
#else
#include "images/color/char8"
#endif
};
mpr_static(cCHA8, 64, 64, 8, CHA8_cimage);

static short		CHA9_cimage[] = {
#ifdef SWAP
#include "images/swap/char9"
#else
#include "images/color/char9"
#endif
};
mpr_static(cCHA9, 64, 64, 8, CHA9_cimage);

static short		EAST_cimage[] = {
#ifdef SWAP
#include "images/swap/c_east"
#else
#include "images/color/c_east"
#endif
};
mpr_static(cEAST, 64, 64, 8, EAST_cimage);

static short		WEST_cimage[] = {
#ifdef SWAP
#include "images/swap/c_west"
#else
#include "images/color/c_west"
#endif
};
mpr_static(cWEST, 64, 64, 8, WEST_cimage);

static short		SOUT_cimage[] = {
#ifdef SWAP
#include "images/swap/c_south"
#else
#include "images/color/c_south"
#endif
};
mpr_static(cSOUT, 64, 64, 8, SOUT_cimage);

static short		NORT_cimage[] = {
#ifdef SWAP
#include "images/swap/c_north"
#else
#include "images/color/c_north"
#endif
};
mpr_static(cNORT, 64, 64, 8, NORT_cimage);

static short		GRED_cimage[] = {
#ifdef SWAP
#include "images/swap/d_green"
#else
#include "images/color/d_green"
#endif
};
mpr_static(cGRED, 64, 64, 8, GRED_cimage);

static short		REDD_cimage[] = {
#ifdef SWAP
#include "images/swap/d_red"
#else
#include "images/color/d_red"
#endif
};
mpr_static(cREDD, 64, 64, 8, REDD_cimage);

static short		WHTD_cimage[] = {
#ifdef SWAP
#include "images/swap/d_white"
#else
#include "images/color/d_white"
#endif
};
mpr_static(cWHTD, 64, 64, 8, WHTD_cimage);

static short		 AUT_cimage[] = {
#ifdef SWAP
#include "images/swap/s_autumn"
#else
#include "images/color/s_autumn"
#endif
};
mpr_static( cAUT, 64, 64, 8, AUT_cimage);

static short		 SPR_cimage[] = {
#ifdef SWAP
#include "images/swap/s_spring"
#else
#include "images/color/s_spring"
#endif
};
mpr_static( cSPR, 64, 64, 8, SPR_cimage);

static short		 SUM_cimage[] = {
#ifdef SWAP
#include "images/swap/s_summer"
#else
#include "images/color/s_summer"
#endif
};
mpr_static( cSUM, 64, 64, 8, SUM_cimage);

static short		 WIN_cimage[] = {
#ifdef SWAP
#include "images/swap/s_winter"
#else
#include "images/color/s_winter"
#endif
};
mpr_static( cWIN, 64, 64, 8, WIN_cimage);

static short		 BAM_cimage[] = {
#ifdef SWAP
#include "images/swap/f_bam"
#else
#include "images/color/f_bam"
#endif
};
mpr_static( cBAM, 64, 64, 8, BAM_cimage);

static short		 MUM_cimage[] = {
#ifdef SWAP
#include "images/swap/f_mum"
#else
#include "images/color/f_mum"
#endif
};
mpr_static( cMUM, 64, 64, 8, MUM_cimage);

static short		 ORC_cimage[] = {
#ifdef SWAP
#include "images/swap/f_orch"
#else
#include "images/color/f_orch"
#endif
};
mpr_static( cORC, 64, 64, 8, ORC_cimage);

static short		 PLM_cimage[] = {
#ifdef SWAP
#include "images/swap/f_plum"
#else
#include "images/color/f_plum"
#endif
};
mpr_static( cPLM, 64, 64, 8, PLM_cimage);


/* Playing Tiles (B/W) */

static short		BLANK_image[] = {
#include "images/bandw/blank"
};
mpr_static(BLANK, 64, 64, 1, BLANK_image);

static short		BAM1_image[] = {
#include "images/bandw/bam1"
};
mpr_static(BAM1, 64, 64, 1, BAM1_image);

static short		BAM2_image[] = {
#include "images/bandw/bam2"
};
mpr_static(BAM2, 64, 64, 1, BAM2_image);

static short		BAM3_image[] = {
#include "images/bandw/bam3"
};
mpr_static(BAM3, 64, 64, 1, BAM3_image);

static short		BAM4_image[] = {
#include "images/bandw/bam4"
};
mpr_static(BAM4, 64, 64, 1, BAM4_image);

static short		BAM5_image[] = {
#include "images/bandw/bam5"
};
mpr_static(BAM5, 64, 64, 1, BAM5_image);

static short		BAM6_image[] = {
#include "images/bandw/bam6"
};
mpr_static(BAM6, 64, 64, 1, BAM6_image);

static short		BAM7_image[] = {
#include "images/bandw/bam7"
};
mpr_static(BAM7, 64, 64, 1, BAM7_image);

static short		BAM8_image[] = {
#include "images/bandw/bam8"
};
mpr_static(BAM8, 64, 64, 1, BAM8_image);

static short		BAM9_image[] = {
#include "images/bandw/bam9"
};
mpr_static(BAM9, 64, 64, 1, BAM9_image);

static short		DOT1_image[] = {
#include "images/bandw/circ1"
};
mpr_static(DOT1, 64, 64, 1, DOT1_image);

static short		DOT2_image[] = {
#include "images/bandw/circ2"
};
mpr_static(DOT2, 64, 64, 1, DOT2_image);

static short		DOT3_image[] = {
#include "images/bandw/circ3"
};
mpr_static(DOT3, 64, 64, 1, DOT3_image);

static short		DOT4_image[] = {
#include "images/bandw/circ4"
};
mpr_static(DOT4, 64, 64, 1, DOT4_image);

static short		DOT5_image[] = {
#include "images/bandw/circ5"
};
mpr_static(DOT5, 64, 64, 1, DOT5_image);

static short		DOT6_image[] = {
#include "images/bandw/circ6"
};
mpr_static(DOT6, 64, 64, 1, DOT6_image);

static short		DOT7_image[] = {
#include "images/bandw/circ7"
};
mpr_static(DOT7, 64, 64, 1, DOT7_image);

static short		DOT8_image[] = {
#include "images/bandw/circ8"
};
mpr_static(DOT8, 64, 64, 1, DOT8_image);

static short		DOT9_image[] = {
#include "images/bandw/circ9"
};
mpr_static(DOT9, 64, 64, 1, DOT9_image);

static short		CHA1_image[] = {
#include "images/bandw/char1"
};
mpr_static(CHA1, 64, 64, 1, CHA1_image);

static short		CHA2_image[] = {
#include "images/bandw/char2"
};
mpr_static(CHA2, 64, 64, 1, CHA2_image);

static short		CHA3_image[] = {
#include "images/bandw/char3"
};
mpr_static(CHA3, 64, 64, 1, CHA3_image);

static short		CHA4_image[] = {
#include "images/bandw/char4"
};
mpr_static(CHA4, 64, 64, 1, CHA4_image);

static short		CHA5_image[] = {
#include "images/bandw/char5"
};
mpr_static(CHA5, 64, 64, 1, CHA5_image);

static short		CHA6_image[] = {
#include "images/bandw/char6"
};
mpr_static(CHA6, 64, 64, 1, CHA6_image);

static short		CHA7_image[] = {
#include "images/bandw/char7"
};
mpr_static(CHA7, 64, 64, 1, CHA7_image);

static short		CHA8_image[] = {
#include "images/bandw/char8"
};
mpr_static(CHA8, 64, 64, 1, CHA8_image);

static short		CHA9_image[] = {
#include "images/bandw/char9"
};
mpr_static(CHA9, 64, 64, 1, CHA9_image);

static short		EAST_image[] = {
#include "images/bandw/c_east"
};
mpr_static(EAST, 64, 64, 1, EAST_image);

static short		WEST_image[] = {
#include "images/bandw/c_west"
};
mpr_static(WEST, 64, 64, 1, WEST_image);

static short		SOUT_image[] = {
#include "images/bandw/c_south"
};
mpr_static(SOUT, 64, 64, 1, SOUT_image);

static short		NORT_image[] = {
#include "images/bandw/c_north"
};
mpr_static(NORT, 64, 64, 1, NORT_image);

static short		GRED_image[] = {
#include "images/bandw/d_green"
};
mpr_static(GRED, 64, 64, 1, GRED_image);

static short		REDD_image[] = {
#include "images/bandw/d_red"
};
mpr_static(REDD, 64, 64, 1, REDD_image);

static short		WHTD_image[] = {
#include "images/bandw/d_white"
};
mpr_static(WHTD, 64, 64, 1, WHTD_image);

static short		 AUT_image[] = {
#include "images/bandw/s_autumn"
};
mpr_static( AUT, 64, 64, 1,  AUT_image);

static short		 SPR_image[] = {
#include "images/bandw/s_spring"
};
mpr_static( SPR, 64, 64, 1,  SPR_image);

static short		 SUM_image[] = {
#include "images/bandw/s_summer"
};
mpr_static( SUM, 64, 64, 1,  SUM_image);

static short		 WIN_image[] = {
#include "images/bandw/s_winter"
};
mpr_static( WIN, 64, 64, 1,  WIN_image);

static short		 BAM_image[] = {
#include "images/bandw/f_bam"
};
mpr_static( BAM, 64, 64, 1,  BAM_image);

static short		 MUM_image[] = {
#include "images/bandw/f_mum"
};
mpr_static( MUM, 64, 64, 1,  MUM_image);

static short		 ORC_image[] = {
#include "images/bandw/f_orch"
};
mpr_static( ORC, 64, 64, 1,  ORC_image);

static short		 PLM_image[] = {
#include "images/bandw/f_plum"
};
mpr_static( PLM, 64, 64, 1,  PLM_image);
