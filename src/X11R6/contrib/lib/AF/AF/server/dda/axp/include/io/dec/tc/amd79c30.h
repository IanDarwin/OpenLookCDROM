/*
 * @DEC_COPYRIGHT@
 */
/*
 * HISTORY
 * $Log: amd79c30.h,v $
 * Revision 1.2  1993/11/12  17:23:35  tml
 * Copyright notice
 *
 * Revision 1.1  1993/10/26  21:16:12  tml
 * endif fix
 *
 * Revision 1.1.2.2  1993/03/15  17:39:41  Craig_Peterson
 * 	Changes to support additional ioctls.
 * 	[1993/03/12  21:50:58  Craig_Peterson]
 *
 * $EndLog$
 */
/*
 * @(#)$RCSfile: amd79c30.h,v $ $Revision: 1.2 $ (DEC) $Date: 1993/11/12 17:23:35 $
 */

/*
 * HISTORY
 * Revision 1.1.2.8  92/11/18  10:36:30  Narayan_Mohanram
 * 	"Changes for HDLC and bug fixes"
 * 
 * Revision 1.1.5.2  92/11/11  14:45:50  Narayan_Mohanram
 * 	Changed the size of DMA -narayan
 * 
 * Revision 1.1.2.7  92/10/14  16:59:00  Narayan_Mohanram
 * 	Increased minor devices to 8
 * 	[92/10/14  16:41:28  Narayan_Mohanram]
 * 
 * Revision 1.1.2.6  92/08/19  16:04:49  Narayan_Mohanram
 * 	Updated for alpha changes -narayan
 * 	[92/08/19  15:56:26  Narayan_Mohanram]
 * 
 * Revision 1.1.2.5  92/08/03  14:02:22  Narayan_Mohanram
 * 	Fixed ODE branch screwups
 * 	[92/08/03  13:49:08  Narayan_Mohanram]
 * 
 * Revision 1.1.3.2  92/06/22  10:52:29  Narayan_Mohanram
 * 	Added define to support 56KB conn --narayan
 * 
 * Revision 1.1.2.3  92/06/08  08:44:37  Narayan_Mohanram
 * 	Made this work like lofi in BL3
 * 		-narayan-
 * 	[92/06/05  13:33:43  Narayan_Mohanram]
 * 
 * Revision 1.1.2.2  92/04/19  17:04:23  Ron_Bhanukitsiri
 * 	"BL2 - Bellcore Certification"
 * 	[92/04/19  17:03:24  Ron_Bhanukitsiri]
 */


#ifndef BBA_H
#define BBA_H
/************************************************************************
 * Modification History
 *
 *
 *	7/17/90 
 *	   prototype bba driver hacked by Rich Hyde
 *	
 *
 ************************************************************************/
#include <sys/time.h>
#include <sys/stream.h>
#include <io/dec/tc/amd79c30_hdlc.h>

#define BBA_CODEC_READ 0
#define BBA_CODEC_WRITE 1

#define BYTESHIFT 0
#define BYTEMASK (0xff << BYTESHIFT)

#define TRUE 1
#define FALSE 0

typedef  enum {
	  C_IR = 1, 
	  C_ERR = 2, 
	  UNKNOWN = 3
    } EventType;

/*
 *							
 *  These need to be mapped into user space.  
 *							
 */

struct	interrupt_event{
	EventType ev_type;		/*  event type */
	struct timeval ev_time;	/* Systems notion of time when it occured */
	long	ev_clkhand;	/* clock hand when it occurred */
	long ev_seq;		/* sequence number of the event */
#define c_ir codec_data.codec_ir
#define c_dsr1 codec_data.codec_dsr1
#define c_derr codec_data.codec_derr
#define c_dsr2 codec_data.codec_dsr2
#define c_lsr codec_data.codec_lsr
#define c_mfsb codec_data.codec_mfsb
#define c_ppsr codec_data.codec_ppsr
	struct codec_intr {
		char codec_ir;
		char codec_derr;
		char codec_dsr1;
		char codec_dsr2;
		char codec_lsr;
		char codec_mfsb;
		char codec_ppsr;
	} codec_data;
};


#if defined (mips) || defined (__alpha)
#define DMA_BOUNDARY 4096	/* on mips && alpha */
#else
xxxxx
#endif

#define BBA_DMASIZE (256/4)	/* number of bytes transfered between DMA interrupts
			 * for each bchan (must be less that NBPG/sizeof(long)
			 */
#define BCHANBUFFSIZE (32 * 1024)	/* size of b channel buffer ~4sec */
/* The following constants are hardware dependent and should not be changed */
#define BBA_BD 0	/* identify B channel buffers */
#define BBA_BE 1
#define BBA_BF 2

#define BBA_MAXMINOR	8	/* Max Minor devices		*/

#define BBA_MAINT 0           /* Maintainance Port 			*/
#define BBA_DCHAN 1           /* Dchan minro device 			*/
#define BBA_AUDIO 2           /* Audio Minor Device			*/
#define BBA_BCHAN 3           /* Bchan minor device			*/

struct codec_reg {
	unsigned char reg_data;
	unsigned char reg_fill[(1<<6) - 1];
};

struct BBA_open {
	queue_t *bo_rdq;		/* Read queue (upstream)	*/
	short bo_flags;			/* Open flags			*/
	short bo_chan;			/* Chan open for this		*/
	struct proc *bo_proc;		/* Which proc has it open	*/
	mblk_t *bo_rmblk;		/* MBLK for the hdlc for this	*/
	struct hdlc_info bo_hdlcin;	/* Input HDLC			*/
	struct hdlc_info bo_hdlcout;	/* Output HDLC			*/
};
	
	
struct BBA_softc {
	short bs_status;		/* Status of the device		*/
	short bs_nopens;		/* Number of openers		*/
	short bs_dunit;			/* DLLD unit			*/
	short bs_rind;			/* Current DMA buffer		*/
	short bs_xind;			/* Current DMA buffer		*/
	short bs_gx;			/* GX coeff current		*/
	short bs_gr;			/* GR coeff current		*/
	short bs_ger;			/* GER coeff current		*/
#if defined(mips)
	volatile struct codec_reg *bs_bbamap;/* Codec registers		*/
#elif defined(__alpha)
	volatile u_long *bs_bbamap;	/* Codec registers here		*/
#else
	xxx
#endif
	struct BBA_open bs_open[BBA_MAXMINOR];	/* Number of minors	*/
	struct BBA_open *bs_b1;		/* Which minor has B1 open	*/
	struct BBA_open *bs_b2;		/* Which minor has B2 open	*/
	struct BBA_open *bs_mapin;	/* Which minor has Map open	*/
	struct BBA_open *bs_mapout;	/* Which minor has Map output	*/
	unsigned char *bs_rbuf [2];	/* Rcv Dma buffer		*/
	unsigned char *bs_xbuf [2];	/* Transmit Dma buffer		*/
#ifdef __alpha
	unsigned int bs_rcv_dma_addr [2];/* Dma address location	*/
	unsigned int bs_xmt_dma_addr [2];/* Dma address location	*/
#else
	unsigned long bs_rcv_dma_addr [2];/* Dma address location	*/
	unsigned long bs_xmt_dma_addr [2];/* Dma address location	*/
#endif
	struct BBA_open *bs_snoop;	/* Snooper program		*/
};

extern struct BBA_softc BBA_softc;
/*
 * Status Bits (in BBA_softc.bs_status)
 */
#define BBAS_DLLDINIT		01	/* DLLD has been inited 	*/
#define BBAS_MAPENABLED		02	/* MAP-Codec has been enab. 	*/
#define BBAS_B1			04	/* B1 chan being USed		*/
#define BBAS_B2			0x8	/* B2 chan being USed		*/
#define BBAS_B1VOICE		0x10	/* B1 chan being used for voice	*/
#define BBAS_B2VOICE		0x20	/* B2 chan being used for voice	*/
#define BBAS_ENABLED		0x40	/* BBA is enabled for intrs	*/
#define BBAS_HANDSET		0x80	/* Handset is enabled		*/
#define BBAS_EAR		0x100	/* Ear is enabled		*/
#define BBAS_SPEAKER1		0x200	/* Speaker is enabled		*/
#define BBAS_MAPOUT		0x400	/* Output to Map		*/
#define BBAS_DMA_ENABLED	0x800	/* DMA has been enabled		*/
/*
 * Status bits in BBA_softc.bs_open [].bo_flags
 */
#define BOS_ENABLED		01	/* Lofi-Bchan enabled		*/
#define BOS_SENDING		02	/* B chan send going on		*/
#define BOS_VOICE		04	/* Voice Connection		*/
#define BOS_HANDSET		010	/* Voice Connection		*/
#define BOS_EAR			020	/* Voice Connection		*/
#define BOS_SPEAKER1		040	/* Speaker1 Connection		*/
#define BOS_DIAL_TONE		0x100	/* Dial tone is on		*/
#define BOS_56KB		0x200	/* This is a 56KB conn		*/
#define BOS_NOHDLC		0x400	/* Don't HDLC encode		*/

struct bba_intr_info {
	int bii_nintr;		/* Total number of interrupts		*/
	int bii_notup;		/* Interrupts before enabled		*/
	int bii_isdn;		/* ISDN interrupts			*/
	int bii_dmaerr;		/* DMA Errors				*/
	int bii_dmatx;		/* DMA transmit done			*/
	int bii_dmarx;		/* DMA receive done			*/
	int bii_b1tx;		/* B1 channell transmit			*/
	int bii_b2tx;		/* B2 Channell transmit			*/
	int bii_b1rx;		/* B1 chan recv				*/
	int bii_b2rx;		/* B2 chan recv				*/
	int bii_frame_comp;	/* Frame complete			*/
	int bii_b1flags_sent;	/* B1 sent out 256 flags		*/
	int bii_b2flags_sent;	/* B1 sent out 256 flags		*/
	int bii_silence;	/* Number of silence voice writes	*/
	int bii_voice_writes;	/* Voice Writes				*/
	int bii_send_busy;	/* Send was busy			*/
	int bii_send_frame;	/* Send was done..			*/
};
#define splbba splimp
/*
 * Ioctls..
 */
#define BBAIOCAUDIOENA      (('L' << 8) | 9) /* Connect Up Audio.      */
#define BBAIOCAUDIODIS      (('L' << 8) | 10)/* Disable Audio on codec1*/
extern struct bba_intr_info bba_intr_info;
extern struct streamtab bbatab;
extern struct controller *bba_ctlr;
extern struct DRV_funcs bba_drv_funcs;
void bba_codecindirect (int regno, int length, unsigned char *data, int dir);
void bba_codec_write (int dunit, int regno, unsigned char value);
void bba_qenable (int dunit);
void bba_recover (void);
void bba_enable_intr (void);
void bba_disable_intr (void);

extern int _bba_debug;
#ifdef DEBUG_BBA
#define BBA_DEBUG(mask,arg) if (_bba_debug & (mask)) printf arg
#define BBA_INCREMENT(x)	(bba_intr_info.x++)
#else
#define BBA_DEBUG(mask,arg)
#define BBA_INCREMENT(x)
#endif

#define BBAD_INTR	(1<<0)		/* BBa Interrupts		*/
#define BBAD_CONF	(1<<1)		/* Config/Probe			*/
#define BBAD_BUF	(1<<2)
#define BBAD_MISC	(1<<3)
#define BBAD_RDDSC	(1<<4)		/* Rd of DSC register		*/
#define BBAD_WRDSC	(1<<5)		/* Wr of DSC register		*/
#define BBAD_OPEN	(1<<6)		/* Open Requests		*/
#define BBAD_USRERR	(1<<7)		/* Errors from User		*/
#define BBAD_PUMP	(1<<8)          /* Board Inits          	*/
#define BBAD_EVENT	(1<<9)		/* Generation of Events 	*/
#define BBAD_IOC	(1<<10)		/* Various Ioctls       	*/
#define BBAD_Q		(1<<11)		/* Q from bottom		*/
#define BBAD_CODEC	(1<<12)		/* Codec                	*/
#define BBAD_DSCINTR	(1<<13)		/* DSC Intrrupts        	*/
#define BBAD_ENABLE	(1<<14)		/* Enable/Disable Bch		*/
#define BBAD_SEND	(1<<15)		/* Send on B chan		*/
#define BBAD_XMITDONE	(1<<16)		/* Transmit done		*/
#define BBAD_RCVDONE	(1<<17)		/* Receive Done			*/
#define BBAD_DSPINTR	(1<<18)		/* Dsp Host Intr		*/
#define BBAD_FRAMECOMP	(1<<19)		/* Frame Completion		*/
#define BBAD_TXINTR	(1<<20)		/* Transmit Interrupts		*/
#define BBAD_RXINTR	(1<<20)		/* Receive Interrupts		*/
#define BBAD_SPEAKER	(1<<21)		/* Speaker/Map operations	*/
#define BBAD_STRALLOC	(1<<22)		/* Allocb trace			*/
#define BBAD_INTRERR	(1<<23)		/* Interrupt Errors		*/
#define BBAD_CODINDIR	(1<<24)		/* Codec Indirect		*/

#define MBLK_TO_DT(bp,type) (type)((bp)->b_rptr)
#define MBLK_BLEN(bp) ((bp)->b_wptr - (bp)->b_rptr)
#define MBLK_TYPE(bp) ((bp)->b_datap->db_type)
#define MBLK_SETSIZE(bp,sz) ((bp)->b_wptr = (bp)->b_rptr + (sz))
#define MBLK_NULL ((mblk_t *)NULL)
#define MBLK_ISNULL(mblk)	 ((mblk) == (mblk_t *)NULL)

#define spllofi splimp
#define DEV_BBA_STR "BBA  "
#define BBA_TIMEOUT     (hz/16)

#endif
