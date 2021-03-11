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

#if !defined(lint) && !defined(SABER)
static char event_c_rcsid[]="$Header: /crl/audio/AF/server/dda/msb/RCS/msbdda.c,v 1.6 1994/04/04 12:52:14 stewart Exp $";
#endif

#include <sys/timers.h>
#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <server/include/misc.h>
#include <server/include/audiodev.h>
#include <server/include/input.h>
#include <audioproto.h>
#include <audio.h>
#include "msbdda.h"
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <dia.h>
#include "msb.h"
#include "msb_gainLimits.h"
#include <errno.h>

#define BadDevOpen -9
#define BadDevGet -10
#define BadSetRate -11
#define BadDevStart -12
#define BadDevSet -13

extern int ioctl();

/*
  This is the Hifi sample rate, as set by the -hrate command line option.
*/
int hifi_rate = 8000;
int hifi_rate_cr = MSB_RATE_8000;
int eventLog = 0;       /* print log events */

/* local procedures */

void msb_record_start(msbPhysDevice *pDev)
{
  int status;
  msb_io_t cmd;
  
  /* Turn on input */
  cmd.cmd = MSB_IO_START;
  cmd.inputOffset = 0; /* actually ignored */
  status = ioctl(pDev->fd, MSB_READ, &cmd);
  if (status < 0) FatalError("Amsb: MSB_READ MSB_IO_START failed\n");
  pDev->hR.zeroTime = pDev->time0 - pDev->hR.size;
  pDev->hR.baseTime = pDev->time0 - pDev->hR.size;
  pDev->sR.zeroTime = pDev->time0 - pDev->sR.size;
  pDev->sR.baseTime = pDev->time0 - pDev->sR.size;
  pDev->lastRecOffset = 0;
  pDev->timeEarliestValid = pDev->time0;
  pDev->recDMARunning = TRUE;
}



void msb_record_stop(msbPhysDevice *pDev)
{
  int status;
  msb_io_t cmd;
  
  cmd.cmd = MSB_IO_STOP;
  cmd.inputOffset = 0; /* actually ignored */
  status = ioctl(pDev->fd, MSB_READ, &cmd);
  if (status < 0) FatalError("Amsb: MSB_READ MSB_IO_STOP failed\n");
  pDev->lastMilliClock = GetTimeInMillis();
  pDev->recDMARunning = FALSE;
}

void msb_playback_start(msbPhysDevice *pDev)
{
  int status;
  msb_io_t cmd;
  
  /* Turn on input */
  cmd.cmd = MSB_IO_START;
  cmd.inputOffset = 0; /* actually ignored */
  status = ioctl(pDev->fd, MSB_WRITE, &cmd);
  if (status < 0) FatalError("Amsb: MSB_WRITE MSB_IO_START failed\n");
  pDev->hP.zeroTime = pDev->time0;
  pDev->hP.baseTime = pDev->time0;
  pDev->sP.zeroTime = pDev->time0 + pDev->hP.size;
  pDev->sP.baseTime = pDev->time0 + pDev->hP.size;
  pDev->lastPlayOffset = 0;
  pDev->timeLastValid = pDev->time0;
  pDev->playDMARunning = TRUE;
}

void msb_playback_stop(msbPhysDevice *pDev)
{
  int status;
  msb_io_t cmd;
  
  cmd.cmd = MSB_IO_STOP;
  cmd.inputOffset = 0; /* actually ignored */
  status = ioctl(pDev->fd, MSB_WRITE, &cmd);
  if (status < 0) FatalError("Amsb: MSB_WRITE MSB_IO_STOP failed\n");
  pDev->lastMilliClock = GetTimeInMillis();
  pDev->playDMARunning = FALSE;
}

void msb_hardware_restart(msbPhysDevice *pDev)
{
  /* If we get here, the hardware has stopped due to
   * to the deadman timer in the driver going off.
   */
  if (pDev->playDMARunning) {
    msb_playback_start(pDev);
    WrapZero(&pDev->hP, pDev->hP.zeroTime, pDev->hP.size);
  }
  if (pDev->recDMARunning) msb_record_start(pDev);
}

void msb_update_times(msbPhysDevice *pDev)
{
    msb_io_t ring;
    int status, advance;
    if (pDev->playDMARunning) {  /* first choice, play buffer */
      ring.cmd = MSB_IO_UPDATE;
      ring.inputOffset = pDev->lastRecOffset;
      ring.outputOffset = pDev->lastPlayOffset;
      status = ioctl(pDev->fd, MSB_WRITE, &ring);
      if (status < 0) {
	ErrorF("Amsb: ioctl:MSB_WRITE failed\n");
	if (errno == EPIPE) msb_hardware_restart(pDev);
	else FatalError("errno %d \n", errno);
      } else {
        ring.inputOffset &= ~31;  /* multiples of 8 samples */
        ring.outputOffset &= ~31;
	advance = ((((pDev->hP.size << 2) + ring.outputOffset) 
		   - pDev->lastPlayOffset) >> 2) % pDev->hP.size;
	
	pDev->lastRecOffset = ring.inputOffset;
	pDev->lastPlayOffset = ring.outputOffset;
	
	pDev->time0 += advance;
      }
    }
    else if (pDev->recDMARunning) { /* second choice, record buffer */
      ring.cmd = MSB_IO_UPDATE;
      ring.inputOffset = pDev->lastRecOffset;
      ring.outputOffset = pDev->lastPlayOffset;
      status = ioctl(pDev->fd, MSB_READ, &ring);
      if (status < 0) {
	ErrorF("Amsb: ioctl:MSB_READ failed\n");
	if (errno == EPIPE) msb_hardware_restart(pDev);
	else FatalError("errno %d \n", errno);
      } else {
        ring.inputOffset &= ~31;  /* multiples of 8 samples */
        ring.outputOffset &= ~31;
	advance = ((((pDev->hR.size << 2) + ring.inputOffset) 
		   - pDev->lastRecOffset) >> 2) % pDev->hR.size;

	pDev->lastRecOffset = ring.inputOffset;
	pDev->lastPlayOffset = ring.outputOffset;
	
	pDev->time0 += advance;
      }
    }
    else { /* neither is running, estimate */
      int milliClock, advance;
      milliClock = (int) GetTimeInMillis();
      advance = ((milliClock - pDev->lastMilliClock) * pDev->sampleRate)
	/ 1000;
      pDev->lastMilliClock = milliClock;
      pDev->time0 += advance;
    }
    {
      int i;
      for (i = 0; i < HIFI_NUM_DEV; i += 1)
	pDev->aDevs[i]->time0 = pDev->time0;
    }
}

int
msbAssertRate(int rate)
{
  switch(rate){
  case  5512: hifi_rate_cr = MSB_RATE_5512_5; break;
  case  6615: hifi_rate_cr = MSB_RATE_6615;   break;
  case  8000: hifi_rate_cr = MSB_RATE_8000;   break;
  case  9600: hifi_rate_cr = MSB_RATE_9600;   break;
  case 11025: hifi_rate_cr = MSB_RATE_11025;  break;
  case 16000: hifi_rate_cr = MSB_RATE_16000;  break;
  case 18900: hifi_rate_cr = MSB_RATE_18900;  break;
  case 22050: hifi_rate_cr = MSB_RATE_22050;  break;
  case 27428: hifi_rate_cr = MSB_RATE_27428_57; break;
  case 32000: hifi_rate_cr = MSB_RATE_32000;  break;
  case 33075: hifi_rate_cr = MSB_RATE_33075;  break;
  case 37800: hifi_rate_cr = MSB_RATE_37800;  break;
  case 44100: hifi_rate_cr = MSB_RATE_44100;  break;
  case 48000: hifi_rate_cr = MSB_RATE_48000;  break;
  default:
    ErrorF("-hrate must be {5512,6615,8000,9600,16000,18900,22050\n");
    ErrorF("                27428,32000,33075,37800,44100,or 48000}\n");
    return 0;
  }
  return 1;
}

int msb_set_output_gain(msbPhysDevice *pDev, int channel, int gdB)
{
  msb_gain_t gain;
  int setl, setr;
  int status, origGain;

  origGain = gdB;
  setl = setr = (channel == HIFI_STEREO);
  setl |= (channel == HIFI_LEFT);
  setr |= (channel == HIFI_RIGHT);
  gdB *= 10;
  /* Range check the output gain. Note that it's always a negative
   * value, but gets sent to the driver as a positive value.
   */
  if ((gdB < msb_gain_limits[MSB_DAC][pDev->codecType].max) ||
      (gdB > msb_gain_limits[MSB_DAC][pDev->codecType].min) )
    return (ABadValue);

  gain.enable = MSB_ENABLE;
  gain.value = gdB;

  if (setl) pDev->gainOutL = origGain;
  if (setr) pDev->gainOutR = origGain;

  gain.channel_mask_left = (setl) ? MSB_M_DAC : 0;
  gain.channel_mask_right = (setr) ? MSB_M_DAC : 0;
  status = ioctl(pDev->fd, MSB_SET_GAIN, &gain);
  if (status < 0) ErrorF("msbSelectOutputGain ioctl failed\n");
  return (ASuccess);
}

void msb_change_input(msbPhysDevice *pDev, int channel, int onoff,
		    int nmask, int *omaskp, int *amaskp)
{
  int status;
  int junk;
  if (omaskp == NULL) omaskp = &junk;
  if (amaskp == NULL) amaskp = &junk;
  /* selectedInput.left is IN[0..3], as is selectedInputR */
  if (omaskp != NULL) *omaskp = msbGetInputs(channel, pDev);
  if (nmask) {
    if (channel == HIFI_STEREO) {
      if (onoff) {
	if (nmask & 1) pDev->selectedInput.left = MSB_AUX1_SRC;
	else if (nmask & 2) pDev->selectedInput.left = MSB_MIC_SRC;
	else if (nmask & 4) pDev->selectedInput.left = MSB_PM_DAC_SRC;
	if (nmask & 8) pDev->selectedInput.right = MSB_AUX1_SRC;
	else if (nmask & 16) pDev->selectedInput.right = MSB_MIC_SRC;
	else if (nmask & 32) pDev->selectedInput.right = MSB_PM_DAC_SRC;
      }
      else {
	if (nmask & (1 + 2 + 4)) pDev->selectedInput.left = MSB_LINE_SRC;
	if (nmask & (8 + 16 + 32)) pDev->selectedInput.right = MSB_LINE_SRC;
      }
    }
    else if (channel == HIFI_LEFT) {
      if (onoff) {
	if (nmask & 1) pDev->selectedInput.left = MSB_AUX1_SRC;
	else if (nmask & 2) pDev->selectedInput.left = MSB_MIC_SRC;
	else if (nmask & 4) pDev->selectedInput.left = MSB_PM_DAC_SRC;
      }
      else {
	if (nmask & (1 + 2 + 4)) pDev->selectedInput.left = MSB_LINE_SRC;
      }
    }
    else if (channel== HIFI_RIGHT) {
      if (onoff) {
	if (nmask & 1) pDev->selectedInput.right = MSB_AUX1_SRC;
	else if (nmask & 2) pDev->selectedInput.right = MSB_MIC_SRC;
	else if (nmask & 4) pDev->selectedInput.right = MSB_PM_DAC_SRC;
      }
      else {
	if (nmask & (1 + 2 + 4)) pDev->selectedInput.right = MSB_LINE_SRC;
      }
    }
    status = ioctl(pDev->fd, MSB_SELECT_INPUT, &pDev->selectedInput);
    if (status < 0) ErrorF("msbChangeInput: ioctl failed\n");
  }
  if (amaskp != NULL) *amaskp = msbGetInputs(channel, pDev);
}







void
msbClose(msbPhysDevice *pDev)
{
  close(pDev->fd);
  pDev->state = STATE_CLOSED;
}

int
msbInit(char *devName, msbPhysDevice *pDev)
{
  msb_io_info_t msb_info;
  msb_dev_info_t dev_info;
  int status;

  pDev->eventLog = eventLog;

  /* open the device */
  pDev->fd = open(devName, O_RDWR, 0);
  if (pDev->fd < 0) {
    ErrorF("msbInit: Cannot open %s\n", devName);
    perror("");
    return(BadDevOpen);
  }
  /*
   * Get the codec type and stuff it away for later
   */
  status = ioctl (pDev->fd, MSB_GET_DEV_INFO, &dev_info);
  if (status < 0) return (BadDevGet);
  pDev->codecType = dev_info.codec_ident.type;

  /* get the ring buffer addresses */
  status = ioctl(pDev->fd, MSB_GET_IO_INFO, &msb_info);
  if (status < 0) return(BadDevGet);

  /* fill output buffer with silence */

  memset( msb_info.write.start, (int) SILENCE, msb_info.write.size);

  /* select sample rate */
  {
    msb_sample_type_t sample_type;
    sample_type.rate = hifi_rate_cr;
    sample_type.format = MSB_FORMAT_PCM_16;
    sample_type.type = MSB_MODE_STEREO;
    status = ioctl(pDev->fd, MSB_SET_SAMPLE_TYPE, &sample_type);
    if (status < 0) return(BadSetRate);
  }

  pDev->sampleRate = hifi_rate;
  pDev->recRefCount = 0;

  pDev->hP.size = msb_info.write.size / (sizeof(HSAMP) * 2);
  pDev->hP.buf = (CARD32 *) msb_info.write.start;
  pDev->hR.size = msb_info.read.size / (sizeof(HSAMP) * 2);
  pDev->hR.buf = (CARD32 *) msb_info.read.start;

  pDev->sP.size = 4 * pDev->sampleRate;
  pDev->sP.buf = (CARD32 *) AFalloc(pDev->sP.size * sizeof(CARD32));

  if (pDev->sP.buf == NULL) {
    ErrorF("Amsb can't alloc play buffer\n");
    return(BadDevOpen);
  }
  memset(pDev->sP.buf, (int) SILENCE, pDev->sP.size * sizeof(CARD32));	

  pDev->sR.size = 4 * pDev->sampleRate;
  pDev->sR.buf = (CARD32 *) AFalloc(pDev->sR.size * sizeof(CARD32));

  if (pDev->sR.buf == NULL) {
    ErrorF("Amsb can't alloc play buffer\n");
    return(BadDevOpen);
  }
  memset(pDev->sR.buf, (int) SILENCE, pDev->sR.size * sizeof(CARD32));	

  /*
   * We try to call the update procedure four times per buffer rev.
  */
  pDev->updateBandMs = (pDev->hP.size * 1000) / (pDev->sampleRate * 4);

  pDev->time0 = 0;
  pDev->lastMilliClock = GetTimeInMillis();


  msb_change_input(pDev, HIFI_STEREO, 1, MSB_MIC_SRC, NULL, NULL);

  /* set gains */
  {
    msb_gain_t gain;

    pDev->gainInL[0] = 0;
    pDev->gainInL[1] = 0;
    pDev->gainInL[2] = 0;
    pDev->gainInL[3] = 0;
    pDev->gainInR[0] = 0;
    pDev->gainInR[1] = 0;
    pDev->gainInR[2] = 0;
    pDev->gainInR[3] = 0;
    gain.channel_mask_left = MSB_M_ALL_INPUT;
    gain.channel_mask_right = MSB_M_ALL_INPUT;
    gain.value = 0;
    gain.enable = MSB_ENABLE;
    status = ioctl(pDev->fd, MSB_SET_GAIN, &gain);
    if (status < 0) return(BadDevSet);

    gain.channel_mask_left = MSB_M_ALL_OUTPUT;
    gain.channel_mask_right = MSB_M_ALL_OUTPUT;
    gain.value = 0;
    gain.enable = MSB_DISABLE;
    status = ioctl(pDev->fd, MSB_SET_GAIN, &gain);
    if (status < 0) return(BadDevSet);
  }

  /* We don't start the hardware until someone uses it
   *  msb_record_start(pDev);
   *  msb_playback_start(pDev);
   */
  pDev->playDMARunning = FALSE;
  pDev->recDMARunning = FALSE;

  msb_set_output_gain(pDev, HIFI_STEREO, 0);
  return(0);
}

/* work procedures */

int msbQueryInputGain(AudioDevicePtr aDev, int *minp, int *maxp)
{
  msbPhysDevice *pDev = (msbPhysDevice *) aDev->devPtr;
  HiFiPrivate *hDev = (HiFiPrivate *) aDev->privPtr;
  int gain;
  if(minp != NULL) *minp = 0;
  if (hDev->channel_type == HIFI_RIGHT) {
    if(maxp != NULL)
      *maxp = 
	(pDev->selectedInput.right == MSB_MIC_SRC) 
	  ? msb_gain_limits[MSB_MIC_SRC][pDev->codecType].max/10 
	    : msb_gain_limits[MSB_LINE_SRC][pDev->codecType].max/10;
  }
  else {
    if(maxp != NULL)
      *maxp = (pDev->selectedInput.left == MSB_MIC_SRC) 
	? msb_gain_limits[MSB_MIC_SRC][pDev->codecType].max/10
	  : msb_gain_limits[MSB_LINE_SRC][pDev->codecType].max/10;
  }
  if (hDev->channel_type == HIFI_RIGHT) 
    gain = pDev->gainInR[pDev->selectedInput.right];
  else gain = pDev->gainInL[pDev->selectedInput.left];
  return(gain);
}

int msbQueryOutputGain(AudioDevicePtr aDev, int *minp, int *maxp)
{
  msbPhysDevice *pDev = (msbPhysDevice *) aDev->devPtr;
  HiFiPrivate *hDev = (HiFiPrivate *) aDev->privPtr;
  int gain;
  if(minp != NULL) *minp = msb_gain_limits[MSB_DAC][pDev->codecType].max/10;
  if(maxp != NULL) *maxp = 0;
  if (hDev->channel_type == HIFI_RIGHT) gain = pDev->gainOutR;
  else gain = pDev->gainOutL;
  return(gain);
}

int msbSelectInputGain(AudioDevicePtr aDev, int gdB )
{
  msbPhysDevice *pDev = (msbPhysDevice *) aDev->devPtr;
  HiFiPrivate *hDev = (HiFiPrivate *) aDev->privPtr;
  msb_gain_t gain;
  int setl, setr;
  int status, origGain;

  origGain = gdB;
  setl = setr = (hDev->channel_type == HIFI_STEREO);
  setl |= (hDev->channel_type == HIFI_LEFT);
  setr |= (hDev->channel_type == HIFI_RIGHT);
  gdB *= 10;

  /* Range check the gain settings, remember the driver deals in
   * 10ths of db, so multiply by 10 before comparing.
   */
  if (gdB < msb_gain_limits[pDev->selectedInput.left][pDev->codecType].min ||
      gdB > msb_gain_limits[pDev->selectedInput.left][pDev->codecType].max)
    return (ABadValue);

  if (gdB < msb_gain_limits[pDev->selectedInput.right][pDev->codecType].min ||
      gdB > msb_gain_limits[pDev->selectedInput.right][pDev->codecType].max)
    return (ABadValue);

  gain.enable = MSB_ENABLE;
  gain.channel_mask_left = 0;
  gain.channel_mask_right = 0;
  gain.value = gdB;

  if (setl) {
    gain.channel_mask_left = 0;
    if (pDev->selectedInput.left != 0) 
      gain.channel_mask_left = 1 << pDev->selectedInput.left;
  }
  if (setr) {
    gain.channel_mask_right = 0;
    if (pDev->selectedInput.right != 0) 
      gain.channel_mask_right = 1 << pDev->selectedInput.right;
  }
  if (setl) pDev->gainInL[pDev->selectedInput.left] = origGain;
  if (setr) pDev->gainInR[pDev->selectedInput.right] = origGain;

  status = ioctl(pDev->fd, MSB_SET_GAIN, &gain);
  if (status < 0) ErrorF("msbSelectInputGain ioctl failed\n");
  return (ASuccess);
}

int msbSelectOutputGain(AudioDevicePtr aDev, int gdB)
{
  msbPhysDevice *pDev = (msbPhysDevice *) aDev->devPtr;
  HiFiPrivate *hDev = (HiFiPrivate *) aDev->privPtr;
  return(msb_set_output_gain(pDev, hDev->channel_type, gdB));
}


int msbGetInputs(int channel_type, msbPhysDevice *pDev)
{
  int l, r;
  l = r = 0;
  if (pDev->selectedInput.left != 0) 
    l = (1 << (pDev->selectedInput.left - 1));
  if (pDev->selectedInput.right != 0) 
    r = (1 << (pDev->selectedInput.right - 1));
  switch(channel_type)
    {
    case HIFI_STEREO:
      return ((r << 3) | l);
    case HIFI_LEFT:
      return(l);
    case HIFI_RIGHT:
      return(r);
    }
  return(0);
}

void msbChangeInput(AudioDevicePtr aDev, int onoff,
		    int nmask, int *omaskp, int *amaskp)
{
  msbPhysDevice *pDev = (msbPhysDevice *) aDev->devPtr;
  HiFiPrivate *hDev = (HiFiPrivate *) aDev->privPtr;
  msb_change_input(pDev, hDev->channel_type, onoff, nmask, omaskp, amaskp);
}

void msbChangeOutput(AudioDevicePtr aDev, int onoff,
		     int nmask, int *omaskp, int *amaskp)
{
  if (omaskp != NULL) *omaskp = 1;
  if (amaskp != NULL) *amaskp = 1;
}



