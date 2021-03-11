/*
 * @DEC_COPYRIGHT@
 */
/*
 * HISTORY
 * $Log: amd79c30_hdlc.h,v $
 * Revision 1.2  1993/11/12  17:23:44  tml
 * Copyright notice
 *
 * Revision 1.1  1993/10/26  21:16:27  tml
 * endif fix
 *
 * Revision 1.1.5.2  1993/07/14  18:17:08  SJ_Lee
 * 	add #ifndef _AMD_HDLC_H_ to avoid multiple inclusion.
 * 	[1993/07/14  15:17:21  SJ_Lee]
 *
 * Revision 1.1.2.2  1993/03/15  17:40:03  Craig_Peterson
 * 	Changes to support additional ioctls.
 * 	[1993/03/12  21:51:19  Craig_Peterson]
 * 
 * $EndLog$
 */
/*
 * @(#)$RCSfile: amd79c30_hdlc.h,v $ $Revision: 1.2 $ (DEC) $Date: 1993/11/12 17:23:44 $
 */

/*
 * HISTORY

 * Revision 1.2.2.5  92/11/18  10:31:35  Narayan_Mohanram
 * 	"PPP additions"
 * 
 * Revision 1.2.4.2  92/11/11  14:38:20  Narayan_Mohanram
 * 	Fixed 56KB bit that is robbed by the switch -narayan
 * 
 * Revision 1.2.2.4  92/08/03  14:41:35  Narayan_Mohanram
 * 	Fix ode screwups of branches
 * 	[92/08/03  14:40:29  Narayan_Mohanram]
 * 
 * Revision 1.2.3.2  92/06/22  11:00:55  Narayan_Mohanram
 * 	added new defines for 56KB conns -- narayan
 * 
 * Revision 1.2.2.2  92/04/19  18:01:18  Ron_Bhanukitsiri
 * 	"BL2 - Bellcore Certification"
 * 	[92/04/19  18:00:34  Ron_Bhanukitsiri]
 * 
 */


#ifndef _AMD_HDLC_H_
#define _AMD_HDLC_H_

/*
 * hdlc.h
 * hdlc_info contains the current state of the protocol for 
 * incoming and outgoing  hdlc packets
 */
struct hdlc_info {
	short state;		/* The current state of the protocol	*/
	unsigned short fcs;	/* Current CRC accumulated so far	*/
	unsigned short flag;	/* Last flag transmitted		*/
	int len;		/* The number of bits we have so far	*/
	int num_ones;		/* The number of ones we have rcvd	*/
	unsigned char *base;	/* Base of this buffer			*/
};
/*
 * States..
 */
#define HDLCS_FLAG_SEARCH	1	/* In flag search mode		*/
#define HDLCS_GOT_ZERO		2	/* Got zero searching for flag	*/
#define HDLCS_GOT_FLAG		3	/* Got flag (gathering data)	*/
#define HDLCS_DONE_XMIT		4	/* Transmit done (idling)	*/
#define HDLCS_XMITTING		5	/* Transmitting user buffer	*/

#define HDLC_FLAG		0x7E	/* Flag at either end..		*/
#define HDLC_INITFCS		0xffff	/* Initial value for FCS calcs	*/
#define HDLC_GOODFCS		0xf0b8	/* Good Final Value of FCS	*/
/*
 * Macros adding bits to the stream
 */
#define BIT_LOCATION(hi) ((BITS_PER_OCTET -1) - ((hi)->len & 0x7))
#define BIT_ADDED(hi) ((hi)->len++)
/**
 ** BBA mask manipulations..
 **/
#define BBA_IN_BIT_LOCATION(hi) ((hi)->len & 7)
#define CREAT_BBA_IN_BIT_MASK(hi,imask) \
	(imask) = 1 << BBA_IN_BIT_LOCATION (hi);
#define BBA_OCTET_LOC(hi) (hi->len >>3)
#define BBA_OCTET_CHANGED(hi) ((hi)->base [BBA_OCTET_LOC (hi)])
/*
 * The below defn's are done for the 56KB transmission case
 * where 7 bits are sent and 1 bit is used for other purposes by the switch
 * We can really use the same macros as that for 64KB when calculating
 * the bit locations in octets.
 */
#define NUM_56KB_BITS	7
#define HDLC_56KB_LOWBIT 1
#define HDLC_56KB_HIGHBIT 0x80
#define BBA_OCTET_CHANGED_56KB(hi) BBA_OCTET_CHANGED(hi)
#define BIT_LOCATION_56KB(hi) BIT_LOCATION(hi)
/*
 * Insert bits in the incoming stream. We have to insert them low-order
 * to high order (that is the way they are transmitted)
 */
#define CHANGE_IN_BBA_MASK(hi,dst,imask) { \
		if ((imask <<= 1) == 0x100) { \
			*++dst = 0; \
			(imask) = 1; \
		} \
		hi->len++; \
	}
#define IN_BBA_DST_MASK_BIT(hi,dst,imask) { \
		*(dst) |= (imask); \
		CHANGE_IN_BBA_MASK(hi, dst, imask) \
	}
#define IN_BBA_DST_NOT_MASK_BIT(hi,dst,imask) { \
		CHANGE_IN_BBA_MASK(hi, dst, imask) \
	}
#define CHANGE_OUT_BBA_MASK(dst, omask) \
		if (!(omask >>= 1)) { \
			*++dst = 0; \
			omask = 0x80; \
		}
#define OUT_BBA_MASK_BIT(hi,dst,omask) { \
		*(dst) |= (omask); \
		CHANGE_OUT_BBA_MASK(dst, omask) \
		(hi)->len++; \
	}
#define OUT_BBA_NOT_MASK_BIT(hi,dst,omask) { \
		CHANGE_OUT_BBA_MASK(dst, omask) \
		(hi)->len++; \
	}
/*
 * When the bit goes over to the next octet, we increase
 * the lenght of the data so that we can get the
 * right bit location calculation
 */
#define CHANGE_OUT_BBA_56KB_MASK(hi,dst, omask) \
		if ((omask >>= 1) == HDLC_56KB_LOWBIT) { \
			*++dst = 0; \
			omask = HDLC_56KB_HIGHBIT; \
			hi->len++; \
		}
#define OUT_BBA_56KB_MASK_BIT(hi,dst,omask) { \
		*(dst) |= (omask); \
		CHANGE_OUT_BBA_56KB_MASK(hi,dst, omask) \
		(hi)->len++; \
	}
#define OUT_BBA_56KB_NOT_MASK_BIT(hi,dst,omask) { \
		CHANGE_OUT_BBA_56KB_MASK(hi,dst, omask) \
		(hi)->len++; \
	}





#define INSERT_MASK_BIT(hi,mask) { \
		OCTET_CHANGED (hi) |= (mask); \
		BIT_ADDED (hi);\
		(mask) >>= 1; \
	}	
#define INSERT_NOT_MASK_BIT(hi,mask) { \
		OCTET_CHANGED (hi) &= ~(mask); \
		BIT_ADDED (hi);\
		(mask) >> = 1;	\
	}	
#define INSERT_ONE_BIT(hi)	{ \
		OCTET_CHANGED (hi) |= (1 << BIT_LOCATION (hi));\
		BIT_ADDED (hi); \
	}
/**#define BIT_TO_TRANSMIT(hi)  ((BITS_PER_WORD -1) - ((hi)->off % BITS_PER_WORD))**/
/**#define WORD_TO_TRANSMIT(hi) ((hi)->base [(hi)->off / BITS_PER_WORD])**/





#define NEXT_OCTET_TO_TRANSMIT(hi) ((hi)->base [1 + ((hi)->off/BITS_PER_OCTET)])
	
#define BITS_LEFT(hi)	((hi)->len - (hi)->off)


#define DPRINTF(x) printf x;
#define LOFI_HDLC_DEBUG(mask,arg) if (lofi_hdlc_debug & (mask)) printf arg
#define BBA_HDLC_DEBUG(mask,arg) if (bba_hdlc_debug & (mask)) printf arg
#define HD_ENCODE	01	/* Debug Encoding	*/
#define HD_BADFRAME	02	/* Frame is Bad		*/
#define HD_FRAME	04	/* frame recvd		*/
#define HD_FLAG_ENCODE	0x8	/* Encode Flags		*/
#define HD_RECV		0x10	/* Recv debug		*/
#define HD_COMPLETE	0x20	/* Frame complete	*/

/*#define DPRINTF(x) */
#define OCTETS_PER_WORD		4
#define BITS_PER_OCTET		8
#define BITS_PER_WORD		(OCTETS_PER_WORD * BITS_PER_OCTET)
#define HDLC_MAX_BUF		1514		/* Input-Buffer-Size	*/
#define HDLC_MAX_FRAME		(HDLC_MAX_BUF * 8)	/* In bits	*/
#ifdef LOFI
/*
 * Insert bits in the incoming stream. We have to insert them low-order
 * to high order (that is the way they are transmitted)
 */
#define IN_LOFI_DST_MASK_BIT(hi,dst,omask) { \
		*dst |= (omask); \
		if ((++hi->len & 0x7) == 0) { \
		      *++dst = 0; \
			(omask) = 1; \
		} else \
			(omask) <<= 1; \
	}
/*
 * Don't need to insert zero bits as the byte has already been cleared
 */
#define IN_LOFI_DST_NOT_MASK_BIT(hi,dst,imask) { \
		if ((++hi->len & 0x7) == 0) { \
			*++dst = 0; imask = 1;\
		} else \
			(imask) <<= 1;	\
	}
#define LOFI_IN_BIT_LOCATION(hi) ((hi)->len & 7)
#define CREAT_LOFI_IN_BIT_MASK(hi,imask) \
	(imask) = 1 << LOFI_IN_BIT_LOCATION (hi);
/*
 * The lofi macros are for supporting the LOFI device. They deal
 * with 24 bit memory on the DSP.
 */
#define LOFI_OCTET_LOC(hi) ((hi->len /BITS_PER_OCTET) + \
		 ((hi->len /BITS_PER_OCTET)/3))
#define LOFI_NEXT_OCTET_LOC(hi) (((hi->len+8) /BITS_PER_OCTET) + \
		 (((hi->len + 8) /BITS_PER_OCTET)/3))
#define LOFI_OCTET_CHANGED(hi) ((hi)->base [LOFI_OCTET_LOC (hi)])
#define LOFI_NEXT_OCTET_CHANGED(hi) ((hi)->base [LOFI_NEXT_OCTET_LOC (hi)])

#define CHANGE_OUT_LOFI_MASK(lp,cval,shift_left) \
		if (--shift_left < 8) {	\
			*lp++ = cval; \
			shift_left = 31; \
			cval = 0; \
		}
#define OUT_LOFI_MASK_BIT(hi,lp,cval,nshift) { \
		cval |= (1 << (nshift)); \
		CHANGE_OUT_LOFI_MASK(lp,cval,nshift) \
		hi->len++; \
	}
/*
 * Don't bother inserting zero's
 */
#define OUT_LOFI_NOT_MASK_BIT(hi,lp,cval,nshift) { \
		CHANGE_OUT_LOFI_MASK(lp,cval,nshift) \
		hi->len++; \
	}
#define IN_LOFI_56KB_DST_MASK_BIT(hi,dst,omask) { \
		*dst |= (omask); \
		if ((++hi->len & 7) == 0) { \
		      *++dst = 0; (omask) = 1; \
		} else \
			(omask) <<= 1; \
	}
#define IN_LOFI_56KB_DST_NOT_MASK_BIT(hi,dst,imask) { \
		if ((++hi->len & 7) == 0) { \
			*++dst = 0; imask = 1;\
		} else \
			(imask) <<= 1;	\
	}
#define LOFI_56KB_IN_BIT_LOCATION(hi) ((hi)->len & 7)
#define CREAT_LOFI_56KB_IN_BIT_MASK(hi,imask) \
	(imask) = 1 << LOFI_56KB_IN_BIT_LOCATION (hi);

#define LOFI_56KB_OCTET_LOC(hi) ((hi->len / NUM_56KB_BITS) + \
		 ((hi->len / NUM_56KB_BITS)/3))
#define LOFI_56KB_NEXT_OCTET_LOC(hi) (((hi->len+ NUM_56KB_BITS)  \
						/ NUM_56KB_BITS) + \
		 (((hi->len +  NUM_56KB_BITS) / NUM_56KB_BITS)/3))
#define LOFI_56KB_OCTET_CHANGED(hi) ((hi)->base [LOFI_56KB_OCTET_LOC (hi)])
#define LOFI_56KB_NEXT_OCTET_CHANGED(hi) \
		((hi)->base [LOFI_56KB_NEXT_OCTET_LOC (hi)])

#define CHANGE_OUT_LOFI_56KB_MASK(lp,cval,shift_left) \
		if (--shift_left < 8) {	\
			*lp++ = cval; \
			shift_left = 30; \
			cval = 0; \
		}
#define OUT_LOFI_56KB_MASK_BIT(hi,lp,cval,nshift) { \
		if ((nshift) == 23 || (nshift) == 15) \
			nshift--; \
		cval |= (1 << (nshift)); \
		CHANGE_OUT_LOFI_56KB_MASK(lp,cval,nshift) \
		hi->len++; \
	}
/*
 * Don't bother inserting zero's
 */
#define OUT_LOFI_56KB_NOT_MASK_BIT(hi,lp,cval,nshift) { \
		if ((nshift) == 23 || (nshift) == 15) \
			nshift--; \
		CHANGE_OUT_LOFI_56KB_MASK(lp,cval,nshift) \
		hi->len++; \
	}
#endif

#endif
