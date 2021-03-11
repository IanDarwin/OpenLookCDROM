/*
 * *****************************************************************
 * *                                                               *
 * *    Copyright (c) Digital Equipment Corporation, 1993          *
 * *                                                               *
 * *   All Rights Reserved.  Unpublished rights  reserved  under   *
 * *   the copyright laws of the United States.                    *
 * *                                                               *
 * *   The software contained on this media  is  proprietary  to   *
 * *   and  embodies  the  confidential  technology  of  Digital   *
 * *   Equipment Corporation.  Possession, use,  duplication  or   *
 * *   dissemination of the software and media is authorized only  *
 * *   pursuant to a valid written license from Digital Equipment  *
 * *   Corporation.                                                *
 * *                                                               *
 * *   RESTRICTED RIGHTS LEGEND   Use, duplication, or disclosure  *
 * *   by the U.S. Government is subject to restrictions  as  set  *
 * *   forth in Subparagraph (c)(1)(ii)  of  DFARS  252.227-7013,  *
 * *   or  in  FAR 52.227-19, as applicable.                       *
 * *                                                               *
 * *****************************************************************
 */

/*
 * msb_gainLimits.h - table describing the gain limits for
 * 		each of the channels on the microsoft sound board
 */

#ifndef MSB_GAIN_LIMITS_H
#define MSB_GAIN_LIMITS_H

#include "msb.h"

typedef struct {
  int min;
  int max;
  int step;
} msb_gain_limits_t;


msb_gain_limits_t msb_gain_limits[MSB_CHANNEL_TOTAL][MSB_MAX_CODECS] = {
  { {0,225,15}, 	/* LINE_SRC J-Grade */
    {0,225,15} },	/* LINE_SRC K-Grade */
  { {0,225,15},		/* AUX1 SRC J-Grade */
    {0,225,15} },	/* AUX1 SRC K-Grade */
  { {0,425,15},		/* MIC SRC  J-Grade */
    {0,425,15} },	/* MIC SRC  K-Grade */
  { {0,225,15},		/* PM_DAC SRC J-Grade */
    {0,225,15} },	/* PM_DAC SRC K-Grade */
  { {0,-225,-15},	/* AUX1 J-Grade */
    {12,-345,-15} },	/* AUX1 K-Grade */
  { {0,-225,-15},	/* AUX2 J-Grade */
    {12,-345,-15} },	/* AUX2 K-Grade */
  { {0,-945,-15},	/* DAC J-Grade */
    {0,-945,-15} },	/* DAC K-Grade */
  { {0,-945,-15},	/* MIX_CTL J-Grade */
    {0,-945,-15} }	/* MIX_CTL K-Grade */
};

#endif
