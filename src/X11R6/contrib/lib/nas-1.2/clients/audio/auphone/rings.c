/**
 * Copyright 1994 Network Computing Devices, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and
 * its documentation for any purpose is hereby granted without fee, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name Network Computing Devices, Inc. not be
 * used in advertising or publicity pertaining to distribution of this
 * software without specific, written prior permission.
 *
 * THIS SOFTWARE IS PROVIDED `AS-IS'.  NETWORK COMPUTING DEVICES, INC.,
 * DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT
 * LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NONINFRINGEMENT.  IN NO EVENT SHALL NETWORK
 * COMPUTING DEVICES, INC., BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING
 * SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES, INCLUDING LOSS OF USE, DATA,
 * OR PROFITS, EVEN IF ADVISED OF THE POSSIBILITY THEREOF, AND REGARDLESS OF
 * WHETHER IN AN ACTION IN CONTRACT, TORT OR NEGLIGENCE, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * $NCDId: @(#)rings.c,v 1.1 1994/02/28 23:00:52 greg Exp $
 */

#include <audio/audiolib.h>
#include "ringin.h"
#include "ringout.h"
#include "busy.h"

int             ringinFormat = ringinDataFormat,
                ringinRate = ringinSampleRate,
                ringinTracks = ringinNumTracks,
                ringinSize = ringinNumSamples;
unsigned char  *ringinData = ringinSamples;

int             ringoutFormat = ringoutDataFormat,
                ringoutRate = ringoutSampleRate,
                ringoutTracks = ringoutNumTracks,
                ringoutSize = ringoutNumSamples;
unsigned char  *ringoutData = ringoutSamples;

int             busyFormat = busyDataFormat,
                busyRate = busySampleRate,
                busyTracks = busyNumTracks,
                busySize = busyNumSamples;
unsigned char  *busyData = busySamples;
