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

#include <stdio.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/devio.h>

#include "jv_reg.h"
#include "jv.h"

#define         BadDevOpen            -9
#define         BadDevGet            -10
#define         BadDevMap            -11


JvInit(device, jvr)
     char *device;
     struct JvRegs *jvr;
{
  struct devget dev_inf;
  volatile unsigned char *jv_base;
  int fd;
  
  if ( (fd = open(device, O_RDWR, 0)) < 0 ) {
    fprintf(stderr, "jvopen %s", device);
    perror("");
    return (BadDevOpen);
  }
  
  /* Get device status about the device */
  if (ioctl(fd, DEVIOCGET, &dev_inf) < 0) return (BadDevGet);
  
  /* Map the device registers to user space addresses */
  if (ioctl(fd, JVGETMAP, &jv_base) < 0) return (BadDevMap);
  
  jv_AddrInit (jv_base, jvr);
  
  return(fd);
}

/* Initialization routine which takes all the address #defines and loads
 * them into the pointer data structures.  `base' is the base address
 * for the J-Video board.  jvr is a pointer to the J-Video register
 * address structure.
 */
jv_AddrInit (base, jvr)
     unsigned char *base;
     struct JvRegs *jvr;
{
  /* Audio */
  jvr->audio.eeprom      = (JvEeprom *) (base + EEPROM);
  jvr->audio.icr_write   = (uint32 *) (base + ICR_WRITE);
  jvr->audio.cvr_write   = (uint32 *) (base + CVR_WRITE);
  jvr->audio.isr_write   = (uint32 *) (base + ISR_WRITE);
  jvr->audio.rxtxh_write = (uint32 *) (base + RXTXH_WRITE);
  jvr->audio.rxtxm_write = (uint32 *) (base + RXTXM_WRITE);
  jvr->audio.rxtxl_write = (uint32 *) (base + RXTXL_WRITE);
  jvr->audio.icr_read    = (uint32 *) (base + ICR_READ);
  jvr->audio.cvr_read    = (uint32 *) (base + CVR_READ);
  jvr->audio.isr_read    = (uint32 *) (base + ISR_READ);
  jvr->audio.rxtxh_read  = (uint32 *) (base + RXTXH_READ);
  jvr->audio.rxtxm_read  = (uint32 *) (base + RXTXM_READ);
  jvr->audio.rxtxl_read  = (uint32 *) (base + RXTXL_READ);
  jvr->audio.ram         = (JvDspRam *) (base + DSP_RAM);
  jvr->audio.csr         = (uint32 *)(base + AUDIO_CSR);
  
  /* Timer */
  jvr->timer.csr.reg     = (uint32 *) (base + TIMER_CSR);
  jvr->timer.comparator  = (uint32 *) (base + TIMER_COMPARATOR);
  jvr->timer.val_lsb     = (uint32 *) (base + TIMER_VAL_LSB);
  jvr->timer.val_msb     = (uint32 *) (base + TIMER_VAL_MSB);
  jvr->timer.latch_lsb   = (uint32 *) (base + TIMER_LATCH_LSB);
  jvr->timer.latch_msb   = (uint32 *) (base + TIMER_LATCH_MSB);
  
  /* Interrupts */
  jvr->intr.status.reg   = (uint32 *) (base + INT_STATUS);
  jvr->intr.mask.reg     = (uint32 *) (base + INT_MASK);
}

