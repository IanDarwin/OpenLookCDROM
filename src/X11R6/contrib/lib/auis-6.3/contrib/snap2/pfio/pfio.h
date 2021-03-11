/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/



#ifndef H_pfio
#define H_pfio

#ifdef _IBMR2
#include <sys/ltypes.h>
#else /* _IBMR2 */
#ifndef int16
#define int8 char
#define uint8 unsigned char
#define int16 short
#define uint16 unsigned short
#define int32 long
#define uint32 unsigned long
#define uint unsigned int
#define ulong unsigned long
#endif /* int16 */
#endif /* _IBMR2 */

#undef NIL
#define NIL (0L)

#ifndef TRUE
#define TRUE (0==0)
#define FALSE (0==1)
#endif

#ifndef imin
#define imin(xx1,xx2) (((xx1)<(xx2))?(xx1):(xx2))
#define imax(xx1,xx2) (((xx1)>(xx2))?(xx1):(xx2))
#define iabs(xx1) ((xx1)<0)?(-(xx1)):(xx1)
#endif

#define TO_pfstr(xx_arg) ((uint8 *)(xx_arg))
#define FM_pfstr(xx_arg) ((char *)(xx_arg))

/*
 * an incomming or outgoing message message
 */
struct PFM_R {
  uint8 *initial;		/*pointer to start of data*/
  uint8 *limit;			/*pointer to just after data*/
  uint8 *buf;			/*pointer to remaining bytes*/
  void (*parerr)();		/*error handler*/
};
typedef struct PFM_R PFM,*PFM_pt;

#define IHEAD_LEN (2)		/*length of packet physical header*/
#define MIN_PAK (2)		/*length of minimum enclosed packet*/

#define PFM_used(xx_arg) \
  ((xx_arg).buf-(xx_arg).initial)

#define PFM_remaining(xx_arg) \
  ((xx_arg).limit-(xx_arg).buf)

#define BYTES_IN_BUF(xx_buf) ((xx_buf).buf-(xx_buf).initial)
#define PFM_FETCH16(xx_arg) \
  (((*(xx_arg))<<8)+(*((xx_arg)+1)))
#define PFM_PUT16(xx_dst,xx_val) \
  {(*(xx_dst))=((xx_val)>>8);(*((xx_dst)+1))=(xx_val);}

/*
 * send opcodes
 */
#define LS_int8 (1)
#define LS_int16 (2)
#define LS_int32 (3)
#define LS_rstr (4)
#define LS_end (5)
#define LS_cstr (6)
#define LS_alocop (7)
#define LS_dropoff (8)

/*specify packet size*/
#define LS_aloc(xx_arg) (LS_alocop),(xx_arg)

#define LS_msend_num(xx_type,xx_arg) (xx_type),((long)(xx_arg))
#define LS_msend(xx_type,xx_arg) (xx_type),(xx_arg)

#define LS_msend_str(xx_len,xx_str) (LS_cstr),((long)xx_len),xx_str
#define LS_msend_rstr(xx_len,xx_str) (LS_rstr),((long)xx_len),xx_str

/*
 * read opcodes (stream format)
 */
#define LR_int8 (1)
#define LR_int16 (2)
#define LR_int24 (3)
#define LR_int32 (4)
#define LR_strr (5)
#define LR_strlen (6)
#define LR_str (7)

/*
 * read opcodes (host format)
 */
#define LRh_int8 (1)
#define LRh_int16 (2)
#define LRh_int32 (4)
#define LRh_int (sizeof(int))
#define LRh_long (sizeof(long))
#define LRh_str (0xFE)
#define LRh_str_pt (0xFC)

/*
 * macro to combine host and stream format
 */
#define LR_get_stream(xx_arg) ((xx_arg)&0xff)
#define LR_get_mach(xx_arg) (((xx_arg)>>8)&0xff)
#define LR_mread(xx_fmt1,xx_fmt2,xx_data) (((xx_fmt1)<<8)+(xx_fmt2)),xx_data
#define LR_end (0)
#endif
