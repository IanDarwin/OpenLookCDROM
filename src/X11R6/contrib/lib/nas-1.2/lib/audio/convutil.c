/**
 * Copyright 1993 Network Computing Devices, Inc.
 * 
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name Network Computing Devices, Inc. not be
 * used in advertising or publicity pertaining to distribution of this
 * software without specific, written prior permission.
 * 
 * THIS SOFTWARE IS PROVIDED 'AS-IS'.  NETWORK COMPUTING DEVICES, INC.,
 * DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING WITHOUT
 * LIMITATION ALL IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
 * PARTICULAR PURPOSE, OR NONINFRINGEMENT.  IN NO EVENT SHALL NETWORK
 * COMPUTING DEVICES, INC., BE LIABLE FOR ANY DAMAGES WHATSOEVER, INCLUDING
 * SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES, INCLUDING LOSS OF USE, DATA,
 * OR PROFITS, EVEN IF ADVISED OF THE POSSIBILITY THEREOF, AND REGARDLESS OF
 * WHETHER IN AN ACTION IN CONTRACT, TORT OR NEGLIGENCE, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 * 
 * $NCDId: @(#)convutil.c,v 1.2 1994/04/07 20:42:26 greg Exp $
 */


/* data format conversion routines */

#include "Alibint.h"
#include <audio/fileutil.h>

#undef convert
#define convert(_type, _convert)					      \
do									      \
{									      \
    _type x;								      \
									      \
    x = *(_type *) s;							      \
    *d-- = _convert;							      \
    s -= sizeof(x);							      \
} while (--samples)

#define ulawToLinear(_x) ulawToLinearTable[_x]

static unsigned short ulawToLinearTable[] =
{
    0x8284, 0x8684, 0x8a84, 0x8e84, 0x9284, 0x9684, 0x9a84, 0x9e84,
    0xa284, 0xa684, 0xaa84, 0xae84, 0xb284, 0xb684, 0xba84, 0xbe84,
    0xc184, 0xc384, 0xc584, 0xc784, 0xc984, 0xcb84, 0xcd84, 0xcf84,
    0xd184, 0xd384, 0xd584, 0xd784, 0xd984, 0xdb84, 0xdd84, 0xdf84,
    0xe104, 0xe204, 0xe304, 0xe404, 0xe504, 0xe604, 0xe704, 0xe804,
    0xe904, 0xea04, 0xeb04, 0xec04, 0xed04, 0xee04, 0xef04, 0xf004,
    0xf0c4, 0xf144, 0xf1c4, 0xf244, 0xf2c4, 0xf344, 0xf3c4, 0xf444,
    0xf4c4, 0xf544, 0xf5c4, 0xf644, 0xf6c4, 0xf744, 0xf7c4, 0xf844,
    0xf8a4, 0xf8e4, 0xf924, 0xf964, 0xf9a4, 0xf9e4, 0xfa24, 0xfa64,
    0xfaa4, 0xfae4, 0xfb24, 0xfb64, 0xfba4, 0xfbe4, 0xfc24, 0xfc64,
    0xfc94, 0xfcb4, 0xfcd4, 0xfcf4, 0xfd14, 0xfd34, 0xfd54, 0xfd74,
    0xfd94, 0xfdb4, 0xfdd4, 0xfdf4, 0xfe14, 0xfe34, 0xfe54, 0xfe74,
    0xfe8c, 0xfe9c, 0xfeac, 0xfebc, 0xfecc, 0xfedc, 0xfeec, 0xfefc,
    0xff0c, 0xff1c, 0xff2c, 0xff3c, 0xff4c, 0xff5c, 0xff6c, 0xff7c,
    0xff88, 0xff90, 0xff98, 0xffa0, 0xffa8, 0xffb0, 0xffb8, 0xffc0,
    0xffc8, 0xffd0, 0xffd8, 0xffe0, 0xffe8, 0xfff0, 0xfff8, 0x0000,
    0x7d7c, 0x797c, 0x757c, 0x717c, 0x6d7c, 0x697c, 0x657c, 0x617c,
    0x5d7c, 0x597c, 0x557c, 0x517c, 0x4d7c, 0x497c, 0x457c, 0x417c,
    0x3e7c, 0x3c7c, 0x3a7c, 0x387c, 0x367c, 0x347c, 0x327c, 0x307c,
    0x2e7c, 0x2c7c, 0x2a7c, 0x287c, 0x267c, 0x247c, 0x227c, 0x207c,
    0x1efc, 0x1dfc, 0x1cfc, 0x1bfc, 0x1afc, 0x19fc, 0x18fc, 0x17fc,
    0x16fc, 0x15fc, 0x14fc, 0x13fc, 0x12fc, 0x11fc, 0x10fc, 0x0ffc,
    0x0f3c, 0x0ebc, 0x0e3c, 0x0dbc, 0x0d3c, 0x0cbc, 0x0c3c, 0x0bbc,
    0x0b3c, 0x0abc, 0x0a3c, 0x09bc, 0x093c, 0x08bc, 0x083c, 0x07bc,
    0x075c, 0x071c, 0x06dc, 0x069c, 0x065c, 0x061c, 0x05dc, 0x059c,
    0x055c, 0x051c, 0x04dc, 0x049c, 0x045c, 0x041c, 0x03dc, 0x039c,
    0x036c, 0x034c, 0x032c, 0x030c, 0x02ec, 0x02cc, 0x02ac, 0x028c,
    0x026c, 0x024c, 0x022c, 0x020c, 0x01ec, 0x01cc, 0x01ac, 0x018c,
    0x0174, 0x0164, 0x0154, 0x0144, 0x0134, 0x0124, 0x0114, 0x0104,
    0x00f4, 0x00e4, 0x00d4, 0x00c4, 0x00b4, 0x00a4, 0x0094, 0x0084,
    0x0078, 0x0070, 0x0068, 0x0060, 0x0058, 0x0050, 0x0048, 0x0040,
    0x0038, 0x0030, 0x0028, 0x0020, 0x0018, 0x0010, 0x0008, 0x0000,
};

int
AuConvertDataToShort(dataFormat, numBytes, data)
int             dataFormat,
                numBytes;
AuPointer       data;
{
    char           *s;
    short          *d;
    int             samples;

    samples = numBytes / AuSizeofFormat(dataFormat);

    if (!samples)
	return 0;

    s = ((char *) data) + numBytes - AuSizeofFormat(dataFormat);
    d = (short *) (((char *) data) + samples * sizeof(short)) - 1;

    switch (dataFormat)
    {
	case AuFormatULAW8:
	    convert(unsigned char, ulawToLinear(x));
	    break;
	case AuFormatLinearUnsigned8:
	    convert(unsigned char, (x - 128) << 8);
	    break;
	case AuFormatLinearSigned8:
	    convert(char, x << 8);
	    break;
	case AuFormatLinearSigned16MSB:
	    if (LITTLE_ENDIAN)
		convert(short, ((((unsigned short) x) >> 8) | (x << 8)));
#if 0
	    else
		convert(short, x);
#endif
	    break;
	case AuFormatLinearUnsigned16MSB:
	    if (LITTLE_ENDIAN)
		convert(short,
			(((((unsigned short) x) >> 8) | (x << 8)) ^ 0x8000));
	    else
		convert(short, x ^ 0x8000);
	    break;
	case AuFormatLinearSigned16LSB:
	    if (BIG_ENDIAN)
		convert(short, ((((unsigned short) x) >> 8) | (x << 8)));
#if 0
	    else
		convert(short, x);
#endif
	    break;
	case AuFormatLinearUnsigned16LSB:
	    if (BIG_ENDIAN)
		convert(short,
			(((((unsigned short) x) >> 8) | (x << 8)) ^ 0x8000));
	    else
		convert(short, x ^ 0x8000);
	    break;
	default:
	    return -1;
    }

    return 0;
}

#undef convert
#define convert(_type, _convert)					      \
{									      \
    _type *_d = (_type *) d;						      \
    short x;								      \
									      \
    do									      \
    {									      \
	x = *s++;							      \
	*_d++ = _convert;						      \
    } while (--samples);						      \
}

/**
 * This routine converts from linear to ulaw.
 *
 * Craig Reese: IDA/Supercomputing Research Center
 * Joe Campbell: Department of Defense
 * 29 September 1989
 *
 * References:
 * 1) CCITT Recommendation G.711  (very difficult to follow)
 * 2) "A New Digital Technique for Implementation of Any
 *     Continuous PCM Companding Law," Villeret, Michel,
 *     et al. 1973 IEEE Int. Conf. on Communications, Vol 1,
 *     1973, pg. 11.12-11.17
 * 3) MIL-STD-188-113,"Interoperability and Performance Standards
 *     for Analog-to_Digital Conversion Techniques,"
 *     17 February 1987
 *
 * Input: Signed 16 bit linear sample
 * Output: 8 bit ulaw sample
 */

#if 0
#define ZEROTRAP				/* turn on the trap as per
						 * the MIL-STD */
#define CLIP 32635
#endif

#define BIAS 0x84				/* define the add-in bias for
						 * 16 bit samples */

static unsigned char
linearToUlaw(sample)
short           sample;
{
    static int      exp_lut[256] =
    {
	0, 0, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3,
	4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
	5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
	5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5,
	6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
	6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
	6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
	6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6,
	7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
	7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
	7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
	7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
	7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
	7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
	7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
	7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7
    };
    short           sign,
                    exponent,
                    mantissa;
    unsigned char   ulawbyte;

    /* Get the sample into sign-magnitude. */
    sign = (sample >> 8) & 0x80;/* set aside the sign */

    if (sign)
	sample = -sample;	/* get magnitude */
#ifdef CLIP
    if (sample > CLIP)
	sample = CLIP;		/* clip the magnitude */
#endif
    /* Convert from 16 bit linear to ulaw. */
    sample = sample + BIAS;
    exponent = exp_lut[(sample >> 7) & 0xff];
    mantissa = (sample >> (exponent + 3)) & 0xf;
    ulawbyte = ~(sign | (exponent << 4) | mantissa);
#ifdef ZEROTRAP
    if (!ulawbyte)
	ulawbyte = 0x02;	/* optional CCITT trap */
#endif

    return ulawbyte;
}

int
AuConvertShortToData(dataFormat, numBytes, data)
int             dataFormat,
                numBytes;
AuPointer          data;
{
    char           *d;
    short          *s;
    int             samples;

    samples = numBytes / sizeof(short);

    if (!samples)
	return 0;

    s = (short *) data;
    d = (char *) data;

    switch (dataFormat)
    {
	case AuFormatULAW8:
	    convert(unsigned char, linearToUlaw(x));
	    break;
	case AuFormatLinearUnsigned8:
	    convert(unsigned char, (x >> 8) + 128);
	    break;
	case AuFormatLinearSigned8:
	    convert(char, x >> 8);
	    break;
	case AuFormatLinearSigned16MSB:
	    if (LITTLE_ENDIAN)
		convert(short, ((((unsigned short) x) >> 8) | (x << 8)));
#if 0
	    else
	    {
		convert(short, x);
	    }
#endif
	    break;
	case AuFormatLinearUnsigned16MSB:
	    if (LITTLE_ENDIAN)
	    {
		convert(short,
			(((((unsigned short) x) >> 8) | (x << 8)) ^ 0x8000));
	    }
	    else
	    {
		convert(short, x ^ 0x8000);
	    }
	    break;
	case AuFormatLinearSigned16LSB:
	    if (BIG_ENDIAN)
		convert(short, ((((unsigned short) x) >> 8) | (x << 8)));
#if 0
	    else
	    {
		convert(short, x);
	    }
#endif
	    break;
	case AuFormatLinearUnsigned16LSB:
	    if (BIG_ENDIAN)
	    {
		convert(short,
			(((((unsigned short) x) >> 8) | (x << 8)) ^ 0x8000));
	    }
	    else
	    {
		convert(short, x ^ 0x8000);
	    }
	    break;
	default:
	    return -1;
    }

    return 0;
}
