/*
 * Copyright 1993 by Digital Equipment Corporation, Maynard, Massachusetts.
 * 
 * Permission to use, copy, modify, distribute, and sell this software and its 
 * documentation for any purpose is hereby granted without fee, provided that 
 * the above copyright notice appear in all copies and that both that 
 * copyright notice and this permission notice appear in supporting 
 * documentation, and that the name of Digital not be used in advertising or 
 * publicity pertaining to distribution of the software without specific, 
 * written prior permission.  Digital makes no representations about the 
 * suitability of this software for any purpose.  It is provided "as is" 
 * without express or implied warranty.
 * 
 * DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL 
 * DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN 
 * AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF 
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */
#ifndef AUDIODEV_H
#define AUDIODEV_H

#include <audio.h>
#include <misc.h>
#include <property.h>
#include <propstr.h>
#include <ac.h>

typedef enum {
    PH_UNKNOWN = 0,
    PH_LOOPSTART = 1
} PhoneType;

typedef enum  {
    STATE_INIT,
    STATE_OPEN,
    STATE_CLOSED
} StateType;

typedef struct _AudioDevice {
    int			index;		/* Index of audiodev device.	*/

    PropertyPtr		userProps;	/* properties for this device */
   /* DDA hangs a physical device structure here */
    pointer 		devPtr;
    /* Audiodev device private information is attached here.    */
    pointer		privPtr;

   /* Some time information. */
   ATime			time0;		/* last computed. */
   ATime			oldDevTime;	/* old device time for delta. */
   ATime			dsptime;	/* Holds dsp time at update. */

    /* Describe the I/O capability */
    int numberOfInputs;		/* number of input sources */
    int numberOfOutputs;	/* number of output destinations */
    unsigned int inputsFromPhone; /* mask of inputs connected to phone line */
    unsigned int outputsToPhone;  /* maks of outputs connected to phone line */

    /* Describe the play buffer type and size. */
    unsigned int playSampleFreq;/* Sampling frequency, in samples per second.*/
    AEncodeType	 playBufType;	/* Data type supported.			    */
    unsigned int playNchannels;	/* number of channels, e.g. 2 for stereo L&R.*/
    unsigned int playNSamplesBuf;/* Length in samples of play buffer.*/

    /* Describe the record buffer type and size. */
    unsigned int recSampleFreq; /* Sampling frequency, in samples per second.*/
    unsigned int recNchannels;	/* number of channels, e.g. 2 for stereo L&R.*/
    AEncodeType	 recBufType;	/* Data type supported.		    */
    unsigned int recNSamplesBuf; /* Length in samples of record buffer.*/

    /* Server Update Information */
    ATime	timeLastUpdated;	/* ATime of last update.		*/
    ATime	timeNextUpdate;		/* ATime at start of next 
					 * update interval. 
					 */
    ATime	timeLastValid;		/* time of last valid play data */
    ATime	timeRecLastUpdated;	/* ATime of last record update.	*/

    /* reference counts */
    int		recRefCount;		/* number of open record streams */

    /* Server Buffer Data */
    pointer   playBuf;			/* Server's play buffer.	*/
    pointer   recBuf;			/* Server's record buffer.	*/

    /* AGC information...? */

    /* Random audiodev device procedures */

    /* AC */
    ABool (* CreateAC)(ACPtr);		/* pAC */

    /* os layer procedures */
    int 	(*BlockHandler)();
    int 	(*WakeupHandler)();
    pointer 	blockData;
    pointer 	wakeupData;

    /* ATime and Misc. */
    ATime 	(*GetTime)(struct _AudioDevice *);

    /* Telephone Specific Procedures */
    int 	(*Dial)(struct _AudioDevice *, char *);
    int 	(*HookSwitch)(struct _AudioDevice *, int);
    int 	(*FlashHook)(struct _AudioDevice *, int);
    int 	(*HookSwitchState)(struct _AudioDevice *);
    int 	(*LoopCurrentState)(struct _AudioDevice *);
    int 	(*PokeTLI)();	    		/* Keep alive.	*/
    int 	(*TelephoneCtl)();
    int		(*TLICraftHookSwitchEvent)();

    /* Device control procedures */
    void	(*ChangeOutput)(struct _AudioDevice *, int, int, int *, int *);
    void	(*ChangeInput)(struct _AudioDevice *, int, int, int *, int *);
    void	(*ChangePassThrough)(struct _AudioDevice *, int, int, int *, int *);
    int		(*QueryOutputGain)(struct _AudioDevice *, int *, int *);
    int		(*QueryInputGain)(struct _AudioDevice *, int *, int *);
    int		(*SelectOutputGain)(struct _AudioDevice *, int);
    int		(*SelectInputGain)(struct _AudioDevice *, int);
    int		(*ChangeGainCtl)(struct _AudioDevice *, int, int, int *, int *);

} AudioDeviceRec;

typedef struct _AudioDevice *AudioDevicePtr;
typedef struct _AudioDeviceInfo {
    int		numDevices;
    int		arraySize;
    AudioDevicePtr	devices[MAXDEVICES];
} AudioDeviceInfo;

extern AudioDeviceInfo audioDeviceInfo;

AudioDevicePtr MakeDevice(void);
void AddEnabledDevice(int /* file descriptor */, int /* closure tag */);

#endif
