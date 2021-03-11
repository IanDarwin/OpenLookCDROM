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

#include	"codec.h"
#include	"lofi_io.h"
#include	"lofi_tli.h"
#include	"codec_tables.h"


#define	LIMIT(low, val, high) \
	  if (val < (low)) val = (low); \
	  else \
	  if ((val) > (high)) val = (high);


static char codec_read_1(struct lofi_info *lofi, int psflag, int reg);
static void codec_read_2(struct lofi_info *lofi, int psflag, int reg, unsigned char *datap1, unsigned char *datap2);
static void codec_write_1(struct lofi_info *lofi, int psflag, int reg, unsigned char data1);
static void codec_write_2(struct lofi_info *lofi, int psflag, int reg, unsigned char data1, unsigned char data2);
static void codec_write_16(struct lofi_info *lofi, int psflag, int reg, unsigned char *dp);

/* Enable the audio section (CSR)
 * Init register
 * Mux control 
 */
void codec_init(lofi, psflag)
struct lofi_info *lofi;
int	psflag;
{
  float	    scratch;

  /* Make sure the codecs are enabled (if not already)	*/
  LoFiSetCSR(lofi->us_reg, FEA, 1);

  /* voice and data */
  codec_write_1(lofi, psflag, INIT_1, INIT_PM_ACTIVE_VOICE_DATA);

  /* map section */
  codec_write_1(lofi, psflag, MAP_MMR1, 0);
  codec_write_1(lofi, psflag, MAP_MMR2, 0);
  codec_write_1(lofi, psflag, MAP_MMR3, 0);

  (void) codec_set_preamp(lofi, psflag, 0.0);
  (void) codec_gx(lofi, psflag, 0.0);
  codec_stg(lofi, psflag, -70.0);
  (void) codec_gr(lofi, psflag, 0.0);
  (void) codec_set_ger(lofi, psflag, 0.0);
  codec_atg(lofi, psflag, 0.0, 0.0, &scratch, &scratch);

  /* configure primary vs. slave IOM-2 mode */
  if(psflag == CODEC_PRIMARY){
	  codec_write_1(lofi, psflag, PP_PPCR1, 
		PP_PPCR1_BITS_IOMM | PP_PPCR1_BITS_ACTIVE );
  }else{
	  codec_write_1(lofi, psflag, PP_PPCR1, 
		PP_PPCR1_BITS_IOMS);
  }	

  /* enable mux interrupt on primary only.
   * mux select 
   *   map <-> mpi
   * Mux connection 3 left for "higher priority" pass through connection.
   */
  codec_write_1(lofi, psflag, MUX_MCR1, 0);
  codec_write_1(lofi, psflag, MUX_MCR2, (MUX_PORT_BA << 4) | MUX_PORT_BB);
  codec_write_1(lofi, psflag, MUX_MCR3, 0);

  if(psflag == CODEC_PRIMARY)
    codec_write_1(lofi, psflag, MUX_MCR4, MUX_MCR4_ENABLE);
  else
    codec_write_1(lofi, psflag, MUX_MCR4, 0);

}

#define	CODEC_CONNECT_IOM	((MUX_PORT_BD<<4) | (MUX_PORT_BA))
static int 	conn_state=0;
void codec_connect(lofi, flag, mask)
struct lofi_info *lofi;
int	flag;
int 	mask;
{
    if (mask == 0) return;
    if (flag){
	codec_write_1(lofi, CODEC_PRIMARY, MUX_MCR3, CODEC_CONNECT_IOM);
	codec_write_1(lofi, CODEC_SECONDARY, MUX_MCR3, CODEC_CONNECT_IOM);
	conn_state = 1;
    }else{
	codec_write_1(lofi, CODEC_PRIMARY, MUX_MCR3, 0);
	codec_write_1(lofi, CODEC_SECONDARY, MUX_MCR3, 0);
	conn_state = 0;
    }
}

int
codec_get_connect(void)
{
	return conn_state;	
}

/*
 * Disable a codec.
 */
void codec_stop(lofi, psflag)
struct lofi_info *lofi;
int	psflag;
{
  /* IR */
  codec_write_1(lofi, psflag, 0, 0);
  codec_write_1(lofi, psflag, INIT_1, INIT_PM_IDLE);
}

/*
 * Select the  input.
 *
 * bit 0 	a input
 * bit 1	b input
 */ 
void codec_set_input(lofi, psflag, enable, mask)
struct lofi_info *lofi;
int	psflag;
int	enable;
int	mask;
{
  unsigned char data,mute;
  int dismask;

  if (mask == 0) return;

  data = codec_read_1(lofi, psflag, MAP_MMR2);
  mute = codec_read_1(lofi, psflag, MAP_MMR3);

  if(((mute&MAP_MMR3_BITS_MUTE)!=0) && (!enable)) return; /* do nothing */

  if(((mute&MAP_MMR3_BITS_MUTE)!=0) && (enable)){
	/* If currently muted and being enabled, turn off mute. */
	mute &= ~(MAP_MMR3_BITS_MUTE);
        codec_write_1(lofi, psflag, MAP_MMR3, mute);
  }

  if (!enable){
	/* add in input already disabled. */
	dismask = mask | ((data & MAP_MMR2_BITS_AINB)==0 ? 0x2 : 0x1);
	if (dismask == 0x3){
		 /* Mute */
		mute |= MAP_MMR3_BITS_MUTE;
        	codec_write_1(lofi, psflag, MAP_MMR3, mute);
		return;
	}
  }

  /* Enable  one of the inputs. */
  data &= ~(MAP_MMR2_BITS_AINB);
  if (enable) data |= ((mask&0x1)!=0 ? 0 : MAP_MMR2_BITS_AINB);
  else data |= ((mask&0x1)!=0 ? MAP_MMR2_BITS_AINB : 0);
  codec_write_1(lofi, psflag, MAP_MMR2, data);

}

int codec_get_input(lofi, psflag)
struct lofi_info *lofi;
int	psflag;
{
  unsigned char data,mute;
  int mask = 0;

  mute = codec_read_1(lofi, psflag, MAP_MMR3);
  if((mute&MAP_MMR3_BITS_MUTE) != 0) return 0;

  data = codec_read_1(lofi, psflag, MAP_MMR2);
  mask = ((data&MAP_MMR2_BITS_AINB)==0 ? 1<<0 : 1<<1);
  return mask;
}

/*
 * Select output, ear, speaker, or both.
 * bit 0 	Ear
 * bit 1	Speaker
 *
 */
void codec_set_output(lofi, psflag, enable, rmask)
struct lofi_info *lofi;
int	psflag;
int	enable;
int	rmask;
{
  unsigned char data;
  int	mask;

  if (rmask == 0) return;

  mask=codec_get_output(lofi, psflag);
  if (enable==TRUE){
      if (mask == rmask) 
	return;
      mask |= rmask;		/* Leave old on and turn on requested. */
      if(mask == 0x3){ 
	data = codec_read_1(lofi, psflag, MAP_MMR3);
	data &= ~(MAP_MMR3_BITS_BOTH);
	data |= MAP_MMR3_BITS_BOTH;
	codec_write_1(lofi, psflag, MAP_MMR3, data);
      }
      else
      {  /* speaker or ear */
	data = codec_read_1(lofi, psflag, MAP_MMR2);
	data &= ~(MAP_MMR2_BITS_LS);
	data |= (mask == 0x2 ? MAP_MMR2_BITS_LS : 0);
	codec_write_1(lofi, psflag, MAP_MMR2, data);
	data = codec_read_1(lofi, psflag, MAP_MMR3);
	data &= ~(MAP_MMR3_BITS_BOTH);
	codec_write_1(lofi, psflag, MAP_MMR3, data);
      }
  }else{
      mask &= rmask;		/* Turn off requested. */
      if (mask == 0)
	return;
      /* When disabling outputs on CODEC, 1 must remain enabled. */
      if(mask == 0x3){ 
	data = codec_read_1(lofi, psflag, MAP_MMR3);
	data &= ~(MAP_MMR3_BITS_BOTH);
	codec_write_1(lofi, psflag, MAP_MMR3, data);
      }
      else
      {  /* speaker or ear */
	data = codec_read_1(lofi, psflag, MAP_MMR2);
	data &= ~(MAP_MMR2_BITS_LS);
	data |= (mask == 0x2 ? 0 : MAP_MMR2_BITS_LS);
	codec_write_1(lofi, psflag, MAP_MMR2, data);
	data = codec_read_1(lofi, psflag, MAP_MMR3);
	data &= ~(MAP_MMR3_BITS_BOTH);
	codec_write_1(lofi, psflag, MAP_MMR3, data);
      }
  }
}

int codec_get_output(lofi, psflag)
struct lofi_info *lofi;
int	psflag;
{
  unsigned char data;
  int mask=0;

  data = codec_read_1(lofi, psflag, MAP_MMR3);
  mask |= ((data&MAP_MMR3_BITS_BOTH) != 0 ? 0x03 : 0);

  data = codec_read_1(lofi, psflag, MAP_MMR2);
  mask |= ((data&MAP_MMR2_BITS_LS) != 0 ? 0x02 : 0x01);

  return mask;
}

/*
 * (Un)Mute the input. 
 */
void codec_mute(lofi, psflag, flag)
struct lofi_info	*lofi;
int			psflag;
int			flag;
{
  unsigned char data;
  data = codec_read_1(lofi, psflag, MAP_MMR3);
  data &= ~(MAP_MMR3_BITS_MUTE);
  data |= (flag ? MAP_MMR3_BITS_MUTE : 0);
  codec_write_1(lofi, psflag, MAP_MMR3, data);
}

/* 
 * Set the codec analog input gain.  
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
float codec_set_preamp(lofi, psflag, gain1)
struct lofi_info	*lofi;
int			psflag;
float			gain1;
{
  unsigned char data;
  unsigned char newga;
  float		aga;

  data = codec_read_1(lofi, psflag, MAP_MMR3);

  aga = 0.0; newga = MAP_MMR3_BITS_GA0;
  if (gain1>6.0) {aga = 6.0; newga = MAP_MMR3_BITS_GA12; }
  if (gain1>12.0) {aga = 12.0; newga = MAP_MMR3_BITS_GA18; }
  if (gain1>18.0) {aga = 18.0; newga = MAP_MMR3_BITS_GA24; }

  data &= ~(MAP_MMR3_BITS_GA);
  data |= newga;
  codec_write_1(lofi, psflag, MAP_MMR3, data);

  return(aga);
}

float codec_get_preamp(lofi, psflag)
struct lofi_info	*lofi;
int			psflag;
{
  unsigned char data;
  float		oldga;

  data = codec_read_1(lofi, psflag, MAP_MMR3);

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

/*
 * Set the gx register gain.
 * Gain is set to min/max when requested gain is outside of the
 * range allowed.
 */
float codec_gx(lofi, psflag, gain1)
struct lofi_info *lofi;
int	psflag;
float	gain1;
{
  unsigned char data;
  unsigned char gx0,gx1;
  GX *gp1;

  LIMIT(0.0, gain1, 12.0);
  for(gp1 = &gx[0]; gp1 < &gx[gx_size]; gp1++){
	  if (gain1 <= gp1->g) break;
  }
  codec_read_2(lofi, psflag, MAP_GX, &gx0, &gx1);
  codec_write_2(lofi, psflag, MAP_GX, gp1->lsb, gp1->msb);
  data = codec_read_1(lofi, psflag, MAP_MMR1);
  data &= ~(MAP_MMR1_BITS_GX);
  data |= MAP_MMR1_BITS_GX;
  codec_write_1(lofi, psflag, MAP_MMR1, data);
  for(gp1 = &gx[0]; gp1 <= &gx[gx_size]; gp1++){
    if ((gx0 == gp1->lsb) && (gx1 == gp1->msb) ) 
        return(gp1->g);
  }
  return(0.0);
}



void
codec_xfilter(lofi, psflag,  onoff)
struct lofi_info *lofi;
int psflag;
int onoff;
{
  unsigned char data;

  data = codec_read_1(lofi, psflag, MAP_MMR1);
  if (onoff) 
	  data |= MAP_MMR1_BITS_X;
  else
  	data &= ~(MAP_MMR1_BITS_GX);
  codec_write_1(lofi, psflag, MAP_MMR1, data);

}

void
codec_xfilter_coeff(lofi,  psflag, p)
struct lofi_info *lofi;
int psflag;
unsigned char *p;
{
  codec_write_16(lofi, psflag, MAP_X, p);
}

/*
 * Set the side tone gain.
 * Gain is set to min/max when requested gain is outside of the
 * range allowed.
 */
float codec_stg(lofi, psflag, gain1)
struct lofi_info *lofi;
int	psflag;
float	gain1;
{
  unsigned char data;
  unsigned char stg0,stg1;
  STG *gp1;

  LIMIT(-70.0, gain1, 0.0);
  for(gp1 = &stg[0]; gp1 < &stg[stg_size]; gp1++){
	  if (gain1 <= gp1->g) break;
  }
  codec_read_2(lofi, psflag, MAP_STG, &stg0, &stg1);
  codec_write_2(lofi, psflag, MAP_STG, gp1->lsb, gp1->msb);
  data = codec_read_1(lofi, psflag, MAP_MMR1);
  data &= ~(MAP_MMR1_BITS_STG);
  data |= MAP_MMR1_BITS_STG;
  codec_write_1(lofi, psflag, MAP_MMR1, data);
  for(gp1 = &stg[0]; gp1 < &stg[stg_size]; gp1++){
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
float codec_gr(lofi, psflag, gain1)
struct lofi_info *lofi;
int	psflag;
float	gain1;
{
  unsigned char data;
  unsigned char gr0,gr1;
  GR *gp1;

  LIMIT(-70.0, gain1, 0.0);
  for(gp1 = &gr[0]; gp1 < &gr[gr_size]; gp1++){
	  if (gain1 <= gp1->g) break;
  }
  codec_read_2(lofi, psflag, MAP_GR, &gr0, &gr1);
  codec_write_2(lofi, psflag, MAP_GR, gp1->lsb, gp1->msb);
  data = codec_read_1(lofi, psflag, MAP_MMR1);
  data &= ~(MAP_MMR1_BITS_GR);
  data |= MAP_MMR1_BITS_GR;
  codec_write_1(lofi, psflag, MAP_MMR1, data);
  for(gp1 = &gr[0]; gp1 < &gr[gr_size]; gp1++){
	  if ((gr0 == gp1->lsb) && (gr1 == gp1->msb) ) 
	      return(gp1->g);

  }
  return(0.0);
}


/*
 * Set the output volume ger  gain register.
 * Gain is set to min/max when requested gain is outside of the
 * range allowed.
 */
float codec_set_ger(lofi, psflag, gain)
struct lofi_info *lofi;
int	psflag;
float	gain;
{
  unsigned char data;
  GER *gp;
  unsigned char oldger0,oldger1;

  LIMIT(-70.0, gain, 18.0);
  for(gp = &ger[0]; gp < &ger[ger_size]; gp++){
	  if (gain <= gp->g) break;
  }
  codec_read_2(lofi, psflag, MAP_GER, &oldger0, &oldger1);
  codec_write_2(lofi, psflag, MAP_GER, gp->lsb, gp->msb);
  data = codec_read_1(lofi, psflag, MAP_MMR1);
  data &= ~(MAP_MMR1_BITS_GER);
  data |= MAP_MMR1_BITS_GER;
  codec_write_1(lofi, psflag, MAP_MMR1, data);
  for(gp = &ger[0]; gp < &ger[ger_size]; gp++){
	  if ((oldger0 == gp->lsb) && (oldger1 == gp->msb))
	      return(gp->g);
  }
  return(0.0);
}


float codec_get_ger(lofi, psflag)
struct lofi_info *lofi;
int	psflag;
{
  GER *gp;
  unsigned char oldger0,oldger1;

  codec_read_2(lofi, psflag, MAP_GER, &oldger0, &oldger1);

  for(gp = &ger[0]; gp < &ger[ger_size]; gp++){
	  if ((oldger0 == gp->lsb) && (oldger1 == gp->msb))
	      return(gp->g);
  }
  return(0.0);
}

/*
 * Set the tone generator gain registers.
 * Gain is set to min/max when requested gain is outside of the
 * range allowed.
 */
void codec_atg(lofi, psflag, gain1, gain2, pg1, pg2)
struct lofi_info *lofi;
int	psflag;
float	gain1,gain2;
float	*pg1,*pg2;
{
  unsigned char atg0,atg1;
  ATG *gp1,*gp2;

  LIMIT(-18.0, gain1, 0.0);
  LIMIT(-18.0, gain2, 0.0);
  for(gp1 = &atg[0]; gp1 < &atg[atg_size]; gp1++){
	  if (gain1 <= gp1->g) break;
  }
  for(gp2 = &atg[0]; gp2 < &atg[atg_size]; gp2++){
	  if (gain2 <= gp2->g) break;
  }
  codec_read_2(lofi, psflag, MAP_SEQ_ATGR1_to_2, &atg0, &atg1);
  codec_write_2(lofi, psflag, MAP_SEQ_ATGR1_to_2, gp1->lsb, gp2->lsb);
  *pg1 = 0.0; *pg2 = 0.0;
  for(gp1 = &atg[0]; gp1 < &atg[atg_size]; gp1++){
	  if (atg0 == gp1->lsb) {
	      *pg1 = gp1->g;
	      break;
	  }
  }
  for(gp2 = &atg[0]; gp2 < &atg[atg_size]; gp2++){
	  if (atg1 == gp2->lsb) {
	      *pg2 = gp2->g;
	      break;
	  }
  }
}

/*
 * Set the dual tone generator frequency registers.
 * This takes the exact ftg register values.
 * The data sheet computation for DTMF freq values does
 * not comply on 2/16 digits.  
 */
void codec_dtmf_ftg(lofi, psflag, f1, f2, pf1, pf2)
struct lofi_info *lofi;
int	psflag;
unsigned char	f1,f2;
unsigned char	*pf1, *pf2;
{
  codec_read_2(lofi, psflag, MAP_SEQ_FTGR1_to_2, pf1, pf2);
  codec_write_2(lofi, psflag, MAP_SEQ_FTGR1_to_2, f1, f2);
}

/* 
 * Generate a dtmf burst.
 */ 
int codec_dtmf(lofi, psflag, ls, digit, duration)
struct lofi_info *lofi;
int	psflag, ls;
char	digit;
int	duration;
{
    unsigned char   data;
    unsigned char   mmr2;
    unsigned char   f1,f2,ftg0,ftg1;
    float	    atg0,atg1;
    float	    oldstg,oldger,oldgr;

    switch((int) digit){
    case '1':
	    /*	    freq1 = 697; freq2 = 1209;	*/
	    f1 = 0x5a; f2 = 0x9b;
	    break;
    case '2':
	    /*	    freq1 = 697; freq2 = 1336;	*/
	    f1 = 0x5a; f2 = 0xab;
	    break;
    case '3':
	    /*	    freq1 = 697; freq2 = 1477;	*/
	    f1 = 0x5a; f2 = 0xbf;
	    break;
    case '4':
	    /*	    freq1 = 770; freq2 = 1209;	*/
	    f1 = 0x63; f2 = 0x9b;
	    break;
    case '5':
	    /*	    freq1 = 770; freq2 = 1336;	*/
	    f1 = 0x63; f2 = 0xab;
	    break;
    case '6':
	    /*	    freq1 = 770; freq2 = 1477;	*/
	    f1 = 0x63; f2 = 0xbf;
	    break;
    case '7':
	    /*	    freq1 = 852; freq2 = 1209;	*/
	    f1 = 0x6e; f2 = 0x9b;
	    break;
    case '8':
	    /*	    freq1 = 852; freq2 = 1336;	*/
	    f1 = 0x6e; f2 = 0xab;
	    break;
    case '9':
	    /*	    freq1 = 852; freq2 = 1477;	*/
	    f1 = 0x6e; f2 = 0xbf;
	    break;
    case '0':
	    /*	    freq1 = 941; freq2 = 1336;	*/
	    f1 = 0x79; f2 = 0xab;
	    break;
    case '*':
	    /*	    freq1 = 941; freq2 = 1209;	*/
	    f1 = 0x79; f2 = 0x9b;
	    break;
    case '#':
	    /*	    freq1 = 941; freq2 = 1477;	*/
	    f1 = 0x79; f2 = 0xbf;
	    break;
    case 'A':
	    /*	    freq1 = 697; freq2 = 1633;	*/
	    f1 = 0x5a; f2 = 0xd3;
	    break;
    case 'B':
	    /*	    freq1 = 770; freq2 = 1633;	*/
	    f1 = 0x63; f2 = 0xd3;
	    break;
    case 'C':
	    /*	    freq1 = 852; freq2 = 1633;	*/
	    f1 = 0x6e; f2 = 0xd3;
	    break;
    case 'D':
	    /*	    freq1 = 941; freq2 = 1633;	*/
	    f1 = 0x79; f2 = 0xd3;
	    break;
    default:
	    return(-1);
    }
    /* set frequency registers */
    codec_dtmf_ftg(lofi, psflag, f1, f2, &ftg0, &ftg1);

    /* set gain for dtmf -> ls/ear path */
    codec_atg(lofi, psflag, DTMF_ATGLO, DTMF_ATGHI, &atg0, &atg1);
    oldstg = codec_stg(lofi, psflag, DTMF_STG);
    oldger = codec_set_ger(lofi, psflag, DTMF_GER);
    oldgr = codec_gr(lofi, psflag, -70.0);
    
    /* previous mmr2 state */
    mmr2 = codec_read_1(lofi, psflag, MAP_MMR2);

    data = mmr2;
    data &= ~(MAP_MMR2_BITS_TGEN|MAP_MMR2_BITS_TRING|MAP_MMR2_BITS_DTMF);
    data |= (ls == CODEC_SPEAKER ? MAP_MMR2_BITS_LS:0);
    codec_write_1(lofi, psflag, MAP_MMR2, data);

    /* check parameters */
    LIMIT(DTMF_ON_MIN, duration, DTMF_ON_MAX);

    /* start on cycle */
    data |= MAP_MMR2_BITS_DTMF;
    codec_write_1(lofi, psflag, MAP_MMR2, data);
    msleep(lofi->us_reg, duration);

    /* start off at end cycle */
    data &= ~MAP_MMR2_BITS_DTMF;
    codec_write_1(lofi, psflag, MAP_MMR2, data);
    msleep(lofi->us_reg, DTMF_ID);

    /* restore old MMR2, ATG, FTG */
    data = mmr2;
    codec_write_1(lofi, psflag, MAP_MMR2, data);
    codec_dtmf_ftg(lofi, psflag, ftg0, ftg1, &f1, &f2);
    codec_atg(lofi, psflag, atg0, atg1, &atg0, &atg1);
    oldstg = codec_stg(lofi, psflag, oldstg);
    oldger = codec_set_ger(lofi, psflag, oldger);
    oldgr = codec_gr(lofi, psflag, oldgr);

    return(1);
}

/*
 * Set the dual tone generator frequency registers.
 * Freq is set to min/max when requested gain is outside of the
 * range allowed.
 * Warning:
 * Frequencies in the 2450 - 2750 band are rejected as well
 * to comply with pt 68 / DOC.
 */
void codec_ftg(lofi, psflag, freq1, freq2, pf1, pf2)
struct lofi_info *lofi;
int	psflag;
int	freq1,freq2;
int	*pf1, *pf2;
{
  unsigned char f1,f2;
  unsigned char ftg0,ftg1;
  float		tmp;

  LIMIT(320,freq1,3200);
  LIMIT(320,freq2,3200);
  if ((freq1 > 2450) && (freq1 < 2750)) freq1 = 2750;
  if ((freq2 > 2450) && (freq2 < 2750)) freq2 = 2750;

  /* empirical and calculated from data sheet */
  tmp = (float)freq1 / 15.50;
  f1 = (unsigned char) tmp;
  tmp = (float)freq2 / 15.50;
  f2 = (unsigned char) tmp;
  codec_read_2(lofi, psflag, MAP_SEQ_FTGR1_to_2, &ftg0, &ftg1);
  codec_write_2(lofi, psflag, MAP_SEQ_FTGR1_to_2, f1, f2);
  /* convert to frequency and store */
  *pf1 = (int)(15.50 * ftg0);
  *pf2 = (int)(15.50 * ftg1);
}

/*
 * Generate a tone burst, prompt.
 */ 
void codec_tone(lofi, psflag, trflag, freq1, gain1, tonetime)
struct lofi_info *lofi;
int	psflag;
int	trflag;
int	freq1;
float	gain1;
int	tonetime;
{
    unsigned char data;
    int		f1,f2;
    float	g1,g2;
    unsigned	char mmr2;

    /* get old mmr2 value and turn off tone generators */
    data = codec_read_1(lofi, psflag, MAP_MMR2);
    mmr2 = data;
    data &= ~(MAP_MMR2_BITS_TGEN|MAP_MMR2_BITS_TRING|MAP_MMR2_BITS_DTMF);
    codec_write_1(lofi, psflag, MAP_MMR2, data);

    /* set to requested values */
    codec_ftg(lofi, psflag, freq1, freq1, &f1, &f2);
    codec_atg(lofi, psflag, gain1, gain1, &g1, &g2);

    /* turn on tone generator or tone ringer */
    data |= (trflag == 1 ? MAP_MMR2_BITS_TRING : MAP_MMR2_BITS_TGEN);
    codec_write_1(lofi, psflag, MAP_MMR2, data);

    /* sleep */
    msleep(lofi->us_reg, tonetime);

    /* restore previous state */
    data = mmr2 & ~(MAP_MMR2_BITS_TGEN|MAP_MMR2_BITS_TRING|MAP_MMR2_BITS_DTMF);
    codec_write_1(lofi, psflag, MAP_MMR2, data);
    codec_ftg(lofi, psflag, f1, f2, &f1, &f2);
    codec_atg(lofi, psflag, g1, g2, &g1, &g2);
    codec_write_1(lofi, psflag, MAP_MMR2, mmr2);
}

/*
 * Set the second tone ringer gain register.
 * Gain is set to min/max when requested gain is outside of the
 * range allowed.
 */
void codec_strg(lofi, psflag, gain1, pg1)
struct lofi_info *lofi;
int	psflag;
float	gain1;
float	*pg1;
{
  unsigned char strg0;
  STRG *gp1;

  LIMIT(-70.0,gain1,0.0);
  for(gp1 = &strg[0]; gp1 < &strg[strg_size]; gp1++){
	  if (gain1 <= gp1->g) break;
  }
  strg0 = codec_read_1(lofi, psflag, MAP_STRA);
  codec_write_1(lofi, psflag, MAP_STRA, gp1->lsb);
  *pg1 = 0.0;
  for(gp1 = &strg[0]; gp1 < &strg[strg_size]; gp1++){
	  if (strg0 == gp1->lsb) {
	      *pg1 = gp1->g;
	      break;
	  }
  }
}

/*
 * Set the second tone ringer freq register.
 * Gain is set to min/max when requested gain is outside of the
 * range allowed.
 */
void codec_strf(lofi, psflag, freq1, pf1)
struct lofi_info *lofi;
int	psflag;
float	freq1;
float	*pf1;
{
  unsigned char strf0;
   STRF *fp1;

  LIMIT(188.2,freq1,12000);
  for(fp1 = &strf[0]; fp1 < &strf[strf_size]; fp1++){
	  if (freq1 <= fp1->f) break;
  }
  strf0 = codec_read_1(lofi, psflag, MAP_STRF);
  codec_write_1(lofi, psflag, MAP_STRF, fp1->lsb);
  *pf1 = 0.0;
  for(fp1 = &strf[0]; fp1 < &strf[strf_size]; fp1++){
	  if (strf0 == fp1->lsb) {
	      *pf1 = fp1->f;
	      break;
	  }
  }
}

/*
 * Generate a tone burst, prompt.
 */ 
void codec_sringer(lofi, psflag, freq1, gain1, tonetime)
struct lofi_info *lofi;
int	psflag;
float	freq1;
float	gain1;
int	tonetime;
{
  unsigned char data;
  float	f1;
  float	g1;
  unsigned	char mmr3;

  /* get old mmr3 value and turn off second ringer */
  data = codec_read_1(lofi, psflag, MAP_MMR3);
  mmr3 = data;
  data &= ~(MAP_MMR3_BITS_STR);
  codec_write_1(lofi, psflag, MAP_MMR3, data);

  /* set to requested values */
  codec_strf(lofi, psflag, freq1, &f1);
  codec_strg(lofi, psflag, gain1, &g1);

  /* turn on tone generator or tone ringer */
  data |=  MAP_MMR3_BITS_STR;
  codec_write_1(lofi, psflag, MAP_MMR3, data);

  /* sleep */
  msleep(lofi->us_reg, tonetime);

  /* restore previous state */
  data = mmr3 & ~MAP_MMR3_BITS_STR;
  codec_write_1(lofi, psflag, MAP_MMR3, data);
  codec_strf(lofi, psflag, f1, &f1);
  codec_strg(lofi, psflag, g1, &g1);
  codec_write_1(lofi, psflag, MAP_MMR3, mmr3);
}


/*
 * Hidden interface where the codec address is known.
 */
static char codec_read_1(struct lofi_info *lofi, int psflag, int reg)
{
  CARD32 *p;
  p = PCODEC(lofi->us_reg, psflag);

  p[0] =  reg<<8;
  xx_waste_time(lofi);
  return(p[1]>>8);
}

static void codec_read_2(struct lofi_info *lofi, int psflag, int reg, 
	unsigned char *datap1, unsigned char *datap2)
{
  CARD32 *p;
  p = PCODEC(lofi->us_reg, psflag);

  p[0] =  reg<<8;
  xx_waste_time(lofi);
  *datap1 = p[1]>>8;
  xx_waste_time(lofi);
  *datap2 = p[1]>>8;
}

static void codec_write_1(struct lofi_info *lofi, int psflag, int reg, 
	unsigned char data1)
{
  CARD32 *p;
  p = PCODEC(lofi->us_reg, psflag);

  p[0] =  reg<<8;
  xx_waste_time(lofi);
  p[1] = (CARD32) data1<<8;
}

static void codec_write_2(struct lofi_info *lofi, int psflag, int reg, 
	unsigned char data1, unsigned char data2)
{
  CARD32 *p;
  p = PCODEC(lofi->us_reg, psflag);

  p[0] =  reg<<8;
  xx_waste_time(lofi);
  p[1] = (CARD32) data1<<8;
  xx_waste_time(lofi);
  p[1] = (CARD32) data2<<8;
}

static void codec_write_16(struct lofi_info *lofi, int psflag, int reg, unsigned char *dp)
{
  CARD32 *p;
  int i;

  p = PCODEC(lofi->us_reg, psflag);

  p[0] =  reg<<8;
  xx_waste_time(lofi);
  for(i=0;i<16;i++) {
  	p[1] = (CARD32) dp[i]<<8;
	  xx_waste_time(lofi);
  }
}

xx_waste_time(lofi)
struct lofi_info *lofi;
{
	MB();
	return(lofi->us_reg->rd_csr);
}

