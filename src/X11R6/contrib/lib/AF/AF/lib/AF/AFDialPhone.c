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

#include "audio.h"
#include "AFlib.h"
#include "Alibint.h"
#include "AFUtils.h"
#include <string.h>
#include <ctype.h>

/* AFDial.c
   L. Stewart
   Feb 2, 1993
 */

/*
 *
 * Fri Oct 12 16:53:06 EDT 1990 tml
 *	Certified Lo-Fi to pt. 68 and DOC at DS&G. 
 *******************************************************************
 *****  WARNING WARNING WARNING WARNING WARNING WARNING WARNING ****
 *******************************************************************
 * These parameters have been set during testing for compliance with 
 * DOC and FCC Pt. 68.  These parameters should not be changed 
 * without awareness of the pertinent regulatory issues.
 */

/* Dialing */
#define DTMF_ID		50		/* Certified in range [50,1000] ms */
#define	DTMF_ON		60		/* Certified in range [60,1000] ms */
#define	DTMF_ATGLO	(-4.0)		/* Twist */
#define	DTMF_ATGHI	(-2.0)		/* Twist */

static struct tonedata
{ 
  char *name;
  double f1, db1, f2, db2, onms, offms;
  } standardtones[] =
{
/*
  { "dialtone",	350, -13, 440, -13,   1000,    0 },
  { "ringback",	440, -19, 480, -19,   1000, 3000 },
  { "busy",	480, -12, 620, -12,    500,  500 },
  { "fastbusy",	480, -12, 620, -12,    250,  250 },
*/
  { "1", 697, DTMF_ATGLO, 1209, DTMF_ATGHI, DTMF_ON, DTMF_ID },
  { "2", 697, DTMF_ATGLO, 1336, DTMF_ATGHI, DTMF_ON, DTMF_ID },
  { "3", 697, DTMF_ATGLO, 1477, DTMF_ATGHI, DTMF_ON, DTMF_ID },
  { "A", 697, DTMF_ATGLO, 1633, DTMF_ATGHI, DTMF_ON, DTMF_ID },
  { "4", 770, DTMF_ATGLO, 1209, DTMF_ATGHI, DTMF_ON, DTMF_ID },
  { "5", 770, DTMF_ATGLO, 1336, DTMF_ATGHI, DTMF_ON, DTMF_ID },
  { "6", 770, DTMF_ATGLO, 1477, DTMF_ATGHI, DTMF_ON, DTMF_ID },
  { "B", 770, DTMF_ATGLO, 1633, DTMF_ATGHI, DTMF_ON, DTMF_ID },
  { "7", 852, DTMF_ATGLO, 1209, DTMF_ATGHI, DTMF_ON, DTMF_ID },
  { "8", 852, DTMF_ATGLO, 1336, DTMF_ATGHI, DTMF_ON, DTMF_ID },
  { "9", 852, DTMF_ATGLO, 1477, DTMF_ATGHI, DTMF_ON, DTMF_ID },
  { "C", 852, DTMF_ATGLO, 1633, DTMF_ATGHI, DTMF_ON, DTMF_ID },
  { "*", 941, DTMF_ATGLO, 1209, DTMF_ATGHI, DTMF_ON, DTMF_ID },
  { "0", 941, DTMF_ATGLO, 1336, DTMF_ATGHI, DTMF_ON, DTMF_ID },
  { "#", 941, DTMF_ATGLO, 1477, DTMF_ATGHI, DTMF_ON, DTMF_ID },
  { "D", 941, DTMF_ATGLO, 1633, DTMF_ATGHI, DTMF_ON, DTMF_ID },
  { ",",   0,  0, 0, 0, 0, 500 },
  {   (char *)NULL,   0,  0, 0, 0, 0,   0 }
};

static struct tonedata *findtone(char c)
{
  struct tonedata *p = standardtones;
  char cs[2];
  cs[0] = c;
  cs[1] = 0;
  while (p->name != NULL)
    {
      if (strcmp(cs, p->name) == 0) return(p);
      p += 1;
    }
  return(NULL);
}

static void AFEnableGainControl(AC ac, AMask mask, 
			   AMask *old_state, AMask *new_state)
{
    register AFAudioConn *aud = ac->connection; 
    register aEnableGainControlReq *req;
    aControlIOReply reply;

    LockConnection(aud);
    GetReq(EnableGainControl, req);
    req->mask = mask;
    req->ac = ac->acontext;
    _AReply(aud, (aReply *) &reply, 0, aTrue);
    UnlockConnection(aud);
    SyncHandle();
    *old_state = reply.oldState;
    *new_state = reply.newState;
    return;
}

static void AFDisableGainControl(AC ac, AMask mask, 
			   AMask *old_state, AMask *new_state)
{
    register AFAudioConn *aud = ac->connection; 
    register aDisableGainControlReq *req;
    aControlIOReply reply;

    LockConnection(aud);
    GetReq(DisableGainControl, req);
    req->mask = mask;
    req->ac = ac->acontext;
    _AReply(aud, (aReply *) &reply, 0, aTrue);
    UnlockConnection(aud);
    SyncHandle();
    *old_state = reply.oldState;
    *new_state = reply.newState;
    return;
}



int AFDialPhone(AC ac, char *number)
{
  char c;
  unsigned char *onbuffer;
  AMask oldgc, newgc;
  int tcount;
  struct tonedata *p;
  double ramp = 3.0;
  int minog, maxog, oldgain, newpass, oldpass;
  int srate, sampleSize, nSamples;
  ATime t;
  AFSetACAttributes attributes, oldattributes;
  ABool res;

  /* give up if this is not a phone device */
  /* if (ac->device->outputsToPhone == 0) return (-1); */
  srate = ac->device->playSampleFreq;
  /*  if (ac->device->playBufType != MU255) return (-2); */

  /* save the old AC gain and set it to 0 */
  oldattributes.play_gain = ac->attributes.play_gain;
  attributes.play_gain = 0;
  AFChangeACAttributes(ac, ACPlayGain, &attributes);

  /* save previous state,  set nopassthrough, preset output gain */
  oldgain = AFQueryOutputGain(ac, &minog, &maxog);
  if (ac->device->outputsToPhone) {
    AFSetOutputGain(ac, 3);  /* compromise for DECaudio */

    AFEnablePassThrough(ac, 0, &newpass, &oldpass);
    newpass = 0;
    if (oldpass) AFDisablePassThrough(ac, 1, &oldpass, &newpass);

    /* disable gain control in TLI */
    AFDisableGainControl(ac, 1, &oldgc, &newgc);
  }
  nSamples = (DTMF_ON * 2 * srate) / 1000;
  sampleSize = AF_sample_sizes[ac->device->playBufType].bytes_per_unit;
  if (sampleSize == 0) sampleSize = 8;
  onbuffer = (unsigned char *) malloc(nSamples * sampleSize);

  t = AFGetTime(ac) + (0.3 * srate);  /* schedule 1/3 sec in the future */

  while ((c = *number++) != 0)
    {
      p = findtone(toupper(c));
      if (p != NULL)
	{
	  tcount = ((int) ((p->onms * srate) / 1000));
	  res = AFTonePair(p->f1, p->db1, p->f2, p->db2, (double) srate,
		     ac->device->playBufType,
		     (int) ((ramp * srate) / 1000), onbuffer, tcount);
	  if (!res) break;
	  (void) AFPlaySamples(ac, t, tcount, onbuffer);
	  t += tcount;
	  t += ((int) ((p->offms * srate) / 1000));
	}
    }
  /* wait for dialling to finish */
  AFRecordSamples(ac, t, 8, onbuffer, ABlock);  /* hacky */
  /* restore controls */
  if (ac->device->outputsToPhone) {

    /* reenable gain control in TLI */
    if (oldgc) AFEnableGainControl(ac, 1, &oldgc, &newgc); 

    AFSetOutputGain(ac, oldgain);
    if (oldpass) AFEnablePassThrough(ac, 1, &oldpass, &newpass);
  }
  AFChangeACAttributes(ac, ACPlayGain, &oldattributes);
  free(onbuffer);
  return((res) ? 0 : -1);
}
