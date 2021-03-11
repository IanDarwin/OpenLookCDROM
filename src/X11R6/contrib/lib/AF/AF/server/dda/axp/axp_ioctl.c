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
static char pscodec_c_rcsid[] = "$Header: /crl/audio/AF/server/dda/axp/RCS/axp_ioctl.c,v 1.7 1993/11/15 21:19:36 tml Exp $";
#endif

#include <stdio.h>
#include <stropts.h>
#include <server/include/dia.h>
#include <server/include/misc.h>
#include <server/include/acstruct.h>
#include <server/include/task.h>
#include <io/dec/tc/amd79c30.h>
#include <io/dec/tc/amd79c30_codec.h>
#include "ringbuffers.h"
#include "pscodec.h"
#include "max_io.h"
#include "physdevice.h"
#include <AF/AFUtils.h>
#include "uatables.h"

axp_Ioctl (fd, cmd, data, len)
	int fd, cmd, len;
	char *data;
{
	struct strioctl ioc;

	ioc.ic_cmd = cmd;
	ioc.ic_timout = 10;
	ioc.ic_len = len;
	ioc.ic_dp = data;

	return ioctl (fd, I_STR, &ioc);
}


codec_read(int fd, int creg, unsigned char *data, int num)
{
    int i;
    struct {
	struct bba_indirect ds;
	unsigned char data[10];
    } is;

    is.ds.bi_regno = creg;
    is.ds.bi_len = num;
    is.ds.bi_direction = BBA_CODEC_READ;
    if (axp_Ioctl (fd, BBAIOC_CODEC_INDIRECT, &is, sizeof(is)) < 0)
    {
	perror("CODEC_INDIRECT: codec_read");
	exit(1);
    }
    for (i=0;i<num;i++)
	*data++ = is.data[i];
}

codec_write(int fd, int creg, unsigned char *data, int num)
{
    int i;
    struct {
	struct bba_indirect ds;
	unsigned char data[10];
    } is;

    for (i=0;i<num;i++)
	is.data[i] = *data++;

    is.ds.bi_regno = creg;
    is.ds.bi_len = num;
    is.ds.bi_direction = BBA_CODEC_WRITE;
	
    if (axp_Ioctl (fd, BBAIOC_CODEC_INDIRECT, &is, sizeof(is)) < 0)
    {
	perror("CODEC_INDIRECT: codec_write");
	exit(1);
    }
}

axp_disable_stg(int fd)
{
	unsigned char data[2];

	data[0] = 0x08;
	data[1] = 0x90;
	codec_write(fd, MAP_STG, data, 2);
}

axp_get_input(int fd)
{
  unsigned char d0,mute;
  int mask = 0;
  unsigned char data[2];

  codec_read(fd, MAP_MMR3, data, 1);
  mute = data[0];
  if((mute&MAP_MMR3_BITS_MUTE) != 0) return 0;

  codec_read(fd, MAP_MMR2, data, 1);
  d0 = data[0];
  mask = ((d0&MAP_MMR2_BITS_AINB)==0 ? 1<<0 : 1<<1);
  return mask;
}

axp_set_input(int fd, int enable, int mask)
{
  unsigned char d0, mute;
  int dismask;
  unsigned char data[2];

  if (mask == 0) return;

  codec_read(fd, MAP_MMR2, data, 1); 
  d0 = data[0];
  codec_read(fd, MAP_MMR3, data, 1);
  mute = data[0];

  if(((mute&MAP_MMR3_BITS_MUTE)!=0) && (!enable)) return; /* do nothing */

  if(((mute&MAP_MMR3_BITS_MUTE)!=0) && (enable)){
	/* If currently muted and being enabled, turn off mute. */
	mute &= ~(MAP_MMR3_BITS_MUTE);
        data[0] = mute;
        codec_write(fd, MAP_MMR3, data, 1);
  }

  if (!enable){
	/* add in input already disabled. */
	dismask = mask | ((d0 & MAP_MMR2_BITS_AINB)==0 ? 0x2 : 0x1);
	if (dismask == 0x3){
		 /* Mute */
		mute |= MAP_MMR3_BITS_MUTE;
                data[0] = mute;
        	codec_write(fd, MAP_MMR3, data, 1);
		return;
	}
  }

  /* Enable  one of the inputs. */
  d0 &= ~(MAP_MMR2_BITS_AINB);
  if (enable) d0 |= ((mask&0x1)!=0 ? 0 : MAP_MMR2_BITS_AINB);
  else d0 |= ((mask&0x1)!=0 ? MAP_MMR2_BITS_AINB : 0);
  data[0] = d0;
  codec_write(fd, MAP_MMR2, data, 1);
}

axp_get_output(int fd)
{
  unsigned char data[2];
  int mask=0;

  codec_read(fd, MAP_MMR3, data, 1);
  mask |= ((data[0]&MAP_MMR3_BITS_BOTH) != 0 ? 0x03 : 0);

  codec_read(fd, MAP_MMR2, data, 1);
  mask |= ((data[0]&MAP_MMR2_BITS_LS) != 0 ? 0x02 : 0x01);

  return mask;
}

axp_set_output(int fd, int enable, int rmask)
{
  unsigned char data[2];
  int	mask;

  if (rmask == 0) return;

  mask=axp_get_output(fd);
  if (enable==TRUE){
      if (mask == rmask) 
	return;
      mask |= rmask;		/* Leave old on and turn on requested. */
      if(mask == 0x3){ 
	codec_read(fd, MAP_MMR3, data, 1);
	data[0] &= ~(MAP_MMR3_BITS_BOTH);
	data[0] |= MAP_MMR3_BITS_BOTH;
	codec_write(fd,  MAP_MMR3, data, 1);
      }
      else
      {  /* speaker or ear */
	codec_read(fd,  MAP_MMR2, data, 1);
	data[0] &= ~(MAP_MMR2_BITS_LS);
	data[0] |= (mask == 0x2 ? MAP_MMR2_BITS_LS : 0);
	codec_write(fd,  MAP_MMR2, data, 1);
	codec_read(fd,  MAP_MMR3, data, 1);
	data[0] &= ~(MAP_MMR3_BITS_BOTH);
	codec_write(fd,  MAP_MMR3, data, 1);
      }
  }else{
      mask &= rmask;		/* Turn off requested. */
      if (mask == 0)
	return;
      /* When disabling outputs on CODEC, 1 must remain enabled. */
      if(mask == 0x3){ 
	codec_read(fd,  MAP_MMR3, data, 1);
	data[0] &= ~(MAP_MMR3_BITS_BOTH);
	codec_write(fd,  MAP_MMR3, data, 1);
      }
      else
      {  /* speaker or ear */
	codec_read(fd,  MAP_MMR2, data, 1);
	data[0] &= ~(MAP_MMR2_BITS_LS);
	data[0] |= (mask == 0x2 ? 0 : MAP_MMR2_BITS_LS);
	codec_write(fd,  MAP_MMR2, data, 1);
	codec_read(fd,  MAP_MMR3, data, 1);
	data[0] &= ~(MAP_MMR3_BITS_BOTH);
	codec_write(fd,  MAP_MMR3, data, 1);
      }
  }
}
