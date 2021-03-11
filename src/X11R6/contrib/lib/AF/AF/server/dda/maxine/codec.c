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
#include	<stdio.h>
#include	<time.h>
#include	"audio.h"
#include	"bba_reg.h"
#include	"bba.h"
#include	<sys/file.h>
#include	<sys/ioctl.h>
#include	"ringbuffers.h"

#define		_CODEC_C_
#include	"codec.h"

#include	"codec_tables.c"

#define	NDATA	16

#define	LIMIT(low, val, high) \
	  if (val < (low)) val = (low); \
	  else \
	  if ((val) > (high)) val = (high);

static void xx_codec_indirect();
static void codec_indirect();
static char Mcodec_read_1();
static void Mcodec_read_2();
static void Mcodec_write_1();
static void Mcodec_write_2();

/* Enable the audio section
 * Init register
 * Mux control 
 */
void Mcodec_init(struct bba_info *info)
{
	volatile struct codec_reg *p;
	unsigned char data[NDATA];
	double	    scratch;

	p = PCODEC(info);

	/* Don't enable interrupts */
	data[0] =  0;
	xx_codec_indirect(info, p, MUX_MCR4, 1, data, 1);
	/* voice and data */
	data[0] = INIT_PM_ACTIVE_VOICE_DATA;
	xx_codec_indirect(info, p, INIT_1, 1, data, 1);
	/* map section */
	data[0] = 0;
	xx_codec_indirect(info, p, MAP_MMR1, 1, data, 1); 
	xx_codec_indirect(info, p, MAP_MMR2, 1, data, 1); 
	xx_codec_indirect(info, p, MAP_MMR3, 1, data, 1); 

	(void)Mcodec_preamp(info, 0.0);
	(void)Mcodec_gx(info, 0.0);
	Mcodec_stg(info, -70.0);
	Mcodec_gr(info, 0);
	Mcodec_ger(info, 0);
	Mcodec_atg(info, 0.0, 0.0, &scratch, &scratch);
	/* configure SSB PP mode */
	data[0] = 1;
	xx_codec_indirect(info, p, PP_PPCR1, 1, data, 1);

	/* route the map to the PP */
	data[0] = (MUX_PORT_BA << 4) | MUX_PORT_BD;
	xx_codec_indirect(info, p, MUX_MCR1, 1, data, 1);
	data[0] =  0;
	xx_codec_indirect(info, p, MUX_MCR2, 1, data, 1);
	xx_codec_indirect(info, p, MUX_MCR3, 1, data, 1);

}

/*
 * Select the  input.
 *
 * bit 0 	a input
 * bit 1	b input
 */ 
void Mcodec_set_input(struct bba_info *bba, int enable, int mask)
{
  unsigned char data,mute;
  int dismask;

  if (mask == 0) return;

  data = Mcodec_read_1(bba, MAP_MMR2);
  mute = Mcodec_read_1(bba, MAP_MMR3);

  if(((mute&MAP_MMR3_BITS_MUTE)!=0) && (!enable)) return; /* do nothing */

  if(((mute&MAP_MMR3_BITS_MUTE)!=0) && (enable)){
	/* If currently muted and being enabled, turn off mute. */
	mute &= ~(MAP_MMR3_BITS_MUTE);
        Mcodec_write_1(bba, MAP_MMR3, mute);
  }

  if (!enable){
	/* add in input already disabled. */
	dismask = mask | ((data & MAP_MMR2_BITS_AINB)==0 ? 0x2 : 0x1);
	if (dismask == 0x3){
		 /* Mute */
		mute |= MAP_MMR3_BITS_MUTE;
        	Mcodec_write_1(bba, MAP_MMR3, mute);
		return;
	}
  }

  /* Enable  one of the inputs. */
  data &= ~(MAP_MMR2_BITS_AINB);
  if (enable) data |= ((mask&0x1)!=0 ? 0 : MAP_MMR2_BITS_AINB);
  else data |= ((mask&0x1)!=0 ? MAP_MMR2_BITS_AINB : 0);
  Mcodec_write_1(bba, MAP_MMR2, data);
}

int Mcodec_get_input(struct bba_info *bba)
{
  unsigned char data,mute;
  int mask = 0;

  mute = Mcodec_read_1(bba, MAP_MMR3);
  if((mute&MAP_MMR3_BITS_MUTE) != 0) return 0;

  data = Mcodec_read_1(bba, MAP_MMR2);
  mask = ((data&MAP_MMR2_BITS_AINB)==0 ? 1<<0 : 1<<1);
  return mask;
}

/*
 * Select output, ear, speaker, or both.
 * bit 0 	Ear
 * bit 1	Speaker
 *
 */
void Mcodec_set_output(struct bba_info *bba, int enable, int rmask)
{
  unsigned char data;
  int	mask;

  if (rmask == 0) return;

  mask=Mcodec_get_output(bba);
  if (enable==TRUE){
      if (mask == rmask) 
		return;
      mask |= rmask;		/* Leave old on and turn on requested. */
      if(mask == 0x3){ 
	data = Mcodec_read_1(bba, MAP_MMR3);
	data &= ~(MAP_MMR3_BITS_BOTH);
	data |= MAP_MMR3_BITS_BOTH;
	Mcodec_write_1(bba, MAP_MMR3, data);
      }
      else
      {  /* speaker or ear */
	data = Mcodec_read_1(bba, MAP_MMR2);
	data &= ~(MAP_MMR2_BITS_LS);
	data |= (mask == 0x2 ? MAP_MMR2_BITS_LS : 0);
	Mcodec_write_1(bba, MAP_MMR2, data);
	data = Mcodec_read_1(bba, MAP_MMR3);
	data &= ~(MAP_MMR3_BITS_BOTH);
	Mcodec_write_1(bba, MAP_MMR3, data);
      }
  }else{
      mask &= rmask;		/* Turn off requested. */
      if (mask == 0)
	return;

      /* When disabling outputs on CODEC, 1 must remain enabled. */
      if(mask == 0x3){ 
	data = Mcodec_read_1(bba, MAP_MMR3);
	data &= ~(MAP_MMR3_BITS_BOTH);
	Mcodec_write_1(bba, MAP_MMR3, data);
      }
      else
      {  /* speaker or ear */
	data = Mcodec_read_1(bba, MAP_MMR2);
	data &= ~(MAP_MMR2_BITS_LS);
	data |= (mask == 0x2 ? 0 : MAP_MMR2_BITS_LS);
	Mcodec_write_1(bba, MAP_MMR2, data);
	data = Mcodec_read_1(bba, MAP_MMR3);
	data &= ~(MAP_MMR3_BITS_BOTH);
	Mcodec_write_1(bba, MAP_MMR3, data);
      }
  }
}

int Mcodec_get_output(struct bba_info *bba)
{
  unsigned char data;
  int mask=0;

  data = Mcodec_read_1(bba, MAP_MMR3);
  mask |= ((data&MAP_MMR3_BITS_BOTH) != 0 ? 0x03 : 0);

  data = Mcodec_read_1(bba, MAP_MMR2);
  mask |= ((data&MAP_MMR2_BITS_LS) != 0 ? 0x02 : 0x01);

  return mask;
}

float Mcodec_set_preamp(bba, gain1)
struct bba_info	*bba;
float			gain1;
{
  unsigned char data;
  unsigned char newga;
  float		aga;

  data = Mcodec_read_1(bba, MAP_MMR3);

  aga = 0.0; newga = MAP_MMR3_BITS_GA0;
  if (gain1>6.0) {aga = 6.0; newga = MAP_MMR3_BITS_GA12; }
  if (gain1>12.0) {aga = 12.0; newga = MAP_MMR3_BITS_GA18; }
  if (gain1>18.0) {aga = 18.0; newga = MAP_MMR3_BITS_GA24; }

  data &= ~(MAP_MMR3_BITS_GA);
  data |= newga;
  Mcodec_write_1(bba, MAP_MMR3, data);

  return(aga);
}

float Mcodec_get_preamp(bba)
struct bba_info	*bba;
{
  unsigned char data;
  float		oldga;

  data = Mcodec_read_1(bba, MAP_MMR3);

  switch(data & MAP_MMR3_BITS_GA){
  case MAP_MMR3_BITS_GA0:
	  oldga = 0.0;
	  break;
  case MAP_MMR3_BITS_GA6:
	  oldga = 6.0;
	  break;
  case MAP_MMR3_BITS_GA12:
	  oldga = 12.0;
	  break;
  case MAP_MMR3_BITS_GA18:
	  oldga = 18.0;
	  break;
  case MAP_MMR3_BITS_GA24:
	  oldga = 24.0;
	  break;
  default:
	  oldga = 0.0;
  }
  return(oldga);
}

/* Set the codec analog input gain.  
 * Gain is set to min/max when requested gain is outside of the
 * range allowed.
 * The preamp gain is a function of input mic type:
 *
 * Empirical evidence suggests:
 * 1) the AT&T headset does not work well < 18db, however the SNR
 *    is low.
 * 2) NT handsets work well even around 0-6 db.
 * 3) Use 0 db for line level input.
 * 4) others?
 */
double Mcodec_preamp(info, gain1)
struct bba_info	*info;
double			gain1;
{
	volatile struct codec_reg *p;
	unsigned char data[NDATA];
	unsigned char newga;
	double		oldga;

	p = PCODEC(info);

	xx_codec_indirect(info, p, MAP_MMR3, 1, data, 0);

	newga = MAP_MMR3_BITS_GA0;
	if (gain1>0.0) newga = MAP_MMR3_BITS_GA6;
	if (gain1>6.0) newga = MAP_MMR3_BITS_GA12;
	if (gain1>12.0) newga = MAP_MMR3_BITS_GA18;
	if (gain1>18.0) newga = MAP_MMR3_BITS_GA24;

	switch(data[0] & MAP_MMR3_BITS_GA){
	case MAP_MMR3_BITS_GA0:
		oldga = 0.0;
		break;
	case MAP_MMR3_BITS_GA6:
		oldga = 6.0;
		break;
	case MAP_MMR3_BITS_GA12:
		oldga = 12.0;
		break;
	case MAP_MMR3_BITS_GA18:
		oldga = 18.0;
		break;
	case MAP_MMR3_BITS_GA24:
		oldga = 24.0;
		break;
	default:
		printf("unknown gain from Mcodec_preamp, %02x\n",data[0]);
		oldga = 0.0;
	}
	data[0] &= ~(MAP_MMR3_BITS_GA);
	data[0] |= newga;
	xx_codec_indirect(info, p, MAP_MMR3, 1, data, 1);

	return(oldga);
}

/*
 * Set the gx register gain.
 * Gain is set to min/max when requested gain is outside of the
 * range allowed.
 */
double Mcodec_gx(struct bba_info *info, int gain1)
{
	volatile struct codec_reg *p;
	unsigned char data[NDATA];
	unsigned char gx0,gx1;
	struct GX *gp1;

	p = PCODEC(info);

	if (gain1 < 0) gain1 = 0;
	else if (gain1 > 12) gain1 = 12;

	for(gp1 = &Mgx[0]; gp1 <= &Mgx[sizeof(Mgx)/sizeof(Mgx[0])]; gp1++){
		if (gain1 <= gp1->g) break;
	}

	xx_codec_indirect(info, p, MAP_GX, 2, data, 0);
	gx0 = data[0];
	gx1 = data[1];

	data[0] = gp1->lsb; data[1] = gp1->msb;
	xx_codec_indirect(info, p, MAP_GX, 2, data, 1);

	xx_codec_indirect(info, p, MAP_MMR1, 1, data, 0);
	data[0] &= ~(MAP_MMR1_BITS_GX);
	data[0] |= MAP_MMR1_BITS_GX;
	
	xx_codec_indirect(info, p, MAP_MMR1, 1, data, 1);

	for(gp1 = &Mgx[0]; gp1 <= &Mgx[sizeof(Mgx)/sizeof(Mgx[0])]; gp1++){
		if ((gx0 == gp1->lsb) && (gx1 == gp1->msb) ) 
		    return(gp1->g);

	}
	return(0.0);
}

/*
 * Set the side tone gain.
 * Gain is set to min/max when requested gain is outside of the
 * range allowed.
 */
double Mcodec_stg(info, gain1)
struct bba_info *info;
double	gain1;
{
	volatile struct codec_reg *p;
	unsigned char data[NDATA];
	unsigned char stg0,stg1;
	struct STG *gp1;

	p = PCODEC(info);

	if (gain1 < -70.0) gain1 = -70.0;
	else if (gain1 > 0.0) gain1 = 0.0;

	for(gp1 = &Mstg[0]; gp1 <= &Mstg[sizeof(Mstg)/sizeof(Mstg[0])]; gp1++){
		if (gain1 <= gp1->g) break;
	}

	xx_codec_indirect(info, p, MAP_STG, 2, data, 0);
	stg0 = data[0];
	stg1 = data[1];

	data[0] = gp1->lsb; data[1] = gp1->msb;
	xx_codec_indirect(info, p, MAP_STG, 2, data, 1);

	xx_codec_indirect(info, p, MAP_MMR1, 1, data, 0);
	data[0] &= ~(MAP_MMR1_BITS_STG);
	data[0] |= MAP_MMR1_BITS_STG;
	
	xx_codec_indirect(info, p, MAP_MMR1, 1, data, 1);

	for(gp1 = &Mstg[0]; gp1 <= &Mstg[sizeof(Mstg)/sizeof(Mstg[0])]; gp1++){
		if ((stg0 == gp1->lsb) && (stg1 == gp1->msb) ) 
		    return(gp1->g);

	}
	return(0.0);
}

/*
 * Set the gr gain register.
 * Gain is set to min/max when requested gain is outside of the
 * range allowed.
 */
void Mcodec_gr(struct bba_info *info, int gain1)
{
	volatile struct codec_reg *p;
	unsigned char data[NDATA];
	struct GR *gp1;

	p = PCODEC(info);

	if (gain1 < -70) gain1 = -70;
	else if (gain1 > 0) gain1 = 0;

	for(gp1 = &Mgr[0]; gp1 <= &Mgr[sizeof(Mgr)/sizeof(Mgr[0])]; gp1++){
		if (gain1 <= gp1->g) break;
	}

	xx_codec_indirect(info, p, MAP_GR, 2, data, 0);

	data[0] = gp1->lsb; data[1] = gp1->msb;
	xx_codec_indirect(info, p, MAP_GR, 2, data, 1);

	xx_codec_indirect(info, p, MAP_MMR1, 1, data, 0);
	data[0] &= ~(MAP_MMR1_BITS_GR);
	data[0] |= MAP_MMR1_BITS_GR;
	
	xx_codec_indirect(info, p, MAP_MMR1, 1, data, 1);
}




/*
 * Set the output volume ger  gain register.
 * Gain is set to min/max when requested gain is outside of the
 * range allowed.
 */
float Mcodec_set_ger(struct bba_info *bba, float gain)
{
  unsigned char data;
  struct GER *gp;
  unsigned char oldger0,oldger1;

  LIMIT(-70.0, gain, 18.0);
  for(gp = &Mger[0]; gp < &Mger[sizeof(Mger)/sizeof(Mger[0])]; gp++){
	  if (gain <= gp->g) break;
  }
  Mcodec_read_2(bba, MAP_GER, &oldger0, &oldger1);
  Mcodec_write_2(bba, MAP_GER, gp->lsb, gp->msb);
  data = Mcodec_read_1(bba, MAP_MMR1);
  data &= ~(MAP_MMR1_BITS_GER);
  data |= MAP_MMR1_BITS_GER;
  Mcodec_write_1(bba, MAP_MMR1, data);
  for(gp = &Mger[0]; gp < &Mger[sizeof(Mger)/sizeof(Mger[0])]; gp++){
	  if ((oldger0 == gp->lsb) && (oldger1 == gp->msb))
	      return(gp->g);
  }
  return(0.0);
}


float Mcodec_get_ger(struct bba_info *bba)
{
  struct GER *gp;
  unsigned char oldger0,oldger1;

  Mcodec_read_2(bba, MAP_GER, &oldger0, &oldger1);

  for(gp = &Mger[0]; gp < &Mger[sizeof(Mger)/sizeof(Mger[0])]; gp++){
	  if ((oldger0 == gp->lsb) && (oldger1 == gp->msb)) {
	      return(gp->g);
	  }
  }
  return(0.0);
}

/*
 * Set the output volume ger  gain register.
 * Gain is set to min/max when requested gain is outside of the
 * range allowed.
 */
void Mcodec_ger(struct bba_info *info, int gain)
{
	volatile struct codec_reg *p;
	unsigned char data[NDATA];
	struct GER *gp;

	p = PCODEC(info);

	if (gain < -70) gain = -70;
	else if (gain > 18) gain = 18;

	for(gp = &Mger[0]; gp <= &Mger[sizeof(Mger)/sizeof(Mger[0])]; gp++){
		if (gain <= gp->g) break;
	}
	xx_codec_indirect(info, p, MAP_GER, 2, data, 0);

	data[0] = gp->lsb; data[1] = gp->msb;
	xx_codec_indirect(info, p, MAP_GER, 2, data, 1);

	xx_codec_indirect(info, p, MAP_MMR1, 1, data, 0);
	data[0] &= ~(MAP_MMR1_BITS_GER);
	data[0] |= MAP_MMR1_BITS_GER;
	
	xx_codec_indirect(info, p, MAP_MMR1, 1, data, 1);

}

/*
 * Set the tone generator gain registers.
 * Gain is set to min/max when requested gain is outside of the
 * range allowed.
 */
void Mcodec_atg(struct bba_info *info, double gain1, double gain2,
		double *pg1, double *pg2)
{
	volatile struct codec_reg *p;
	unsigned char data[NDATA];
	unsigned char atg0,atg1;
	struct ATG *gp1,*gp2;

	p = PCODEC(info);

	if (gain1 < -18.0) gain1 = -18.0;
	else if (gain1 > 0.0) gain1 = 0.0;

	if (gain2 < -18.0) gain2 = -18.0;
	else if (gain2 > 0.0) gain2 = 0.0;

	for(gp1 = &Matg[0]; gp1 <= &Matg[sizeof(Matg)/sizeof(Matg[0])]; gp1++){
		if (gain1 <= gp1->g) break;
	}
	for(gp2 = &Matg[0]; gp2 <= &Matg[sizeof(Matg)/sizeof(Matg[0])]; gp2++){
		if (gain2 <= gp2->g) break;
	}
	xx_codec_indirect(info, p, MAP_SEQ_ATGR1_to_2, 2, data, 0);
	atg0 = data[0];
	atg1 = data[1];

	data[0] = gp1->lsb; data[1] = gp2->lsb;
	xx_codec_indirect(info, p, MAP_SEQ_ATGR1_to_2, 2, data, 1);

	*pg1 = 0.0; *pg2 = 0.0;
	for(gp1 = &Matg[0]; gp1 <= &Matg[sizeof(Matg)/sizeof(Matg[0])]; gp1++){
		if (atg0 == gp1->lsb) {
		    *pg1 = gp1->g;
		    break;
		}
	}
	for(gp2 = &Matg[0]; gp2 <= &Matg[sizeof(Matg)/sizeof(Matg[0])]; gp2++){
		if (atg1 == gp2->lsb) {
		    *pg2 = gp2->g;
		    break;
		}
	}
}



/*
 * Hidden interface where the codec address is known.
 */
static char Mcodec_read_1(struct bba_info *bba, int reg)
{
  unsigned char data[2];
  codec_indirect(bba, reg, 1, data, 0);
  return data[0];
}

static void Mcodec_read_2(struct bba_info *bba, int reg, 
	unsigned char *datap1, unsigned char *datap2)
{
  unsigned char data[2];
  codec_indirect(bba, reg, 2, data, 0);
  *datap1 = data[0];
  *datap2 = data[1];
}

static void Mcodec_write_1(struct bba_info *bba, int reg, 
	unsigned char data1)
{
  unsigned char data[2];
  data[0] = data1;
  codec_indirect(bba, reg, 1, data, 1);
}

static void Mcodec_write_2(struct bba_info *bba, int reg, 
	unsigned char data1, unsigned char data2)
{
  unsigned char data[2];
  data[0] = data1; data[1] = data2;
  codec_indirect(bba, reg, 2, data, 1);
}

/* 
 * Read or write a sequence of codec registers.
 */
static void codec_indirect(info, reg, len, data, dir)
struct bba_info *info;
int	reg, len, dir;
unsigned char	*data;
{
	volatile struct codec_reg *p;

	p = PCODEC(info);
	xx_codec_indirect(info, p, reg, len, data, dir);
}

/*
 * Hidden interface where the codec address is known.
 */
static
void xx_codec_indirect(info, p, reg, len, data, dir)
struct bba_info *info;
volatile struct codec_reg *p;
int	reg, len, dir;
unsigned char	*data;
{
    register int i;
    register unsigned char *cp;

/*
printf("reg[0x%X]@0x%X= ",reg,&p[0].reg_data);
*/
    p[0].reg_data =  reg;
    for(i = 0, cp = data; i < len ; i++, cp++) {
	    /* 
	     * This code path should enforce a recovery time at
	     * the codec of > 400 ns.  Mmsleep(info, 1) is probably the
	     * wrong way to achieve this.
	     */
	    (void)Mxx_waste_time(info);
	    if(dir) {
		    p[1].reg_data = *cp;
/*
printf("w0x%X, ",*cp);
*/
	    }else {
		    *(cp) = p[1].reg_data;
/*
printf("r0x%X, ",*cp);
*/
	    }
    }
/*
printf("\n");
*/

}


#define TIMETOWASTE 50
int
Mxx_waste_time(info)
struct bba_info *info;
{
	int i;
	for(i = 0; i < TIMETOWASTE; i++);
}

