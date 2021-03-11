/*
 * Copyright 1993 Network Computing Devices, Inc.
 * 
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
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
 * $NCDId: @(#)auconfig.c,v 1.7 1994/04/27 17:45:54 greg Exp $
 */
#include	"misc.h"
#include	"dixstruct.h"
#include	<audio/audio.h>
#include	<audio/Aproto.h>
#include	"au.h"

extern AuBool AuInitPhysicalDevices();
extern void AuCreateResourceTypes();

ComponentPtr   *auServerDevices,		/* array of devices */
               *auServerBuckets,		/* array of server owned
						 * buckets */
               *auServerRadios,			/* array of server owned
						 * radios */
                auDevices,			/* list of all devices */
                auBuckets,			/* list of all buckets */
                auRadios;			/* list of all radios */
AuUint32        auNumServerDevices,		/* number of devices */
                auNumActions,			/* number of defined actions */
                auNumServerBuckets,		/* number of server owned
						 * buckets */
                auNumServerRadios;		/* number of server owned
						 * radios */

#define auNumFormats NUMARRAY(auFormats)
AuUint8         auFormats[] =			/* data formats supported */
{
    AuFormatULAW8,
    AuFormatLinearUnsigned8,
    AuFormatLinearSigned8,
    AuFormatLinearSigned16MSB,
    AuFormatLinearUnsigned16MSB,
    AuFormatLinearSigned16LSB,
    AuFormatLinearUnsigned16LSB,
};

#define auNumElementTypes NUMARRAY(auElementTypes)
AuUint8         auElementTypes[] =		/* element types supported */
{
    AuElementTypeImportClient,
    AuElementTypeImportDevice,
    AuElementTypeImportBucket,
    AuElementTypeImportWaveForm,
    /* AuElementTypeImportRadio, */
    AuElementTypeBundle,
    AuElementTypeMultiplyConstant,
    AuElementTypeAddConstant,
    AuElementTypeSum,
    AuElementTypeExportClient,
    AuElementTypeExportDevice,
    AuElementTypeExportBucket,
    /* AuElementTypeExportRadio, */
    AuElementTypeExportMonitor,
};

#define auNumWaveForms NUMARRAY(auWaveForms)
AuUint8         auWaveForms[] =			/* wave forms supported */
{
    AuWaveFormSquare, AuWaveFormSine,		/* AuWaveFormConstant,
						 * AuWaveFormSaw, */
};

#define auNumActions NUMARRAY(auActions)
AuUint8         auActions[] =			/* actions supported */
{
    AuElementActionChangeState,
    AuElementActionSendNotify,
    AuElementActionNoop,
};

AuBool
AuInitDevice(auSetup, len)
auConnSetup    *auSetup;
int            *len;
{
    AuUint32        auServerDeviceListSize,
                    auServerBucketListSize,
                    auServerRadioListSize,
                    auServerMinRate,
                    auServerMaxRate,
                    i;

    /* free up old arrays */
    if (auServerDevices)
	aufree(auServerDevices);
    if (auServerBuckets)
	aufree(auServerBuckets);
    if (auServerRadios)
	aufree(auServerRadios);

    for (i = 0; i < AuMaxCB; i++)
	AuCallbacks[i] = 0;

    if (!AuInitPhysicalDevices())
	return AuFalse;

    AuCreateResourceTypes();
    AuCallback(AuCreateServerComponentsCB,
	       (&auServerDeviceListSize, &auServerBucketListSize,
		&auServerRadioListSize, &auServerMinRate, &auServerMaxRate));

    auSetup->minSampleRate = auServerMinRate;
    auSetup->maxSampleRate = auServerMaxRate;
    auSetup->maxTracks = auMaxTracks;
    auSetup->numFormats = auNumFormats;
    auSetup->numElementTypes = auNumElementTypes;
    auSetup->numWaveForms = auNumWaveForms;
    auSetup->numActions = auNumActions;
    auSetup->numDevices = auNumServerDevices;
    auSetup->numBuckets = auNumServerBuckets;
    auSetup->numRadios = auNumServerRadios;

    *len = PAD4(auNumFormats) +
	PAD4(auNumElementTypes) +
	PAD4(auNumWaveForms) +
	PAD4(auNumActions) +
	auServerDeviceListSize +
	auServerBucketListSize +
	auServerRadioListSize;

    return AuTrue;
}
