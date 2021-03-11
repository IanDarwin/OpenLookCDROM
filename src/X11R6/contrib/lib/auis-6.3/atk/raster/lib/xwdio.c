/*
 Copyright MIT 1990, 1991 - All Rights Reserved
*/

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

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/raster/lib/RCS/xwdio.c,v 1.12 1993/01/08 16:33:17 rr2b R6tape $";
#endif


/* $Author: rr2b $ */

 

/*  
  Enable import and export of X Window Dump (version 7) files	
  paul@athena.mit.edu  1/90		
  "...it becomes natural, like a third sense." -- Homer Simpson 
 */		

#include <andrewos.h>
#include <stdio.h>
#include <sys/stat.h>
#include <class.h>
#include <xwdio.eh>
#include <pixelimg.ih>	
#include <dataobj.ih>

#include <X11/Xlib.h>
#include <X11/XWDFile.h>
#define XWD_FILE_VERSION 7

#define MaxIntensity  65536	/* maximum possible Intensity */

#define MaxGrey       32768	/* limits on the grey levels used */
#define Threshold     16384	/* in the dithering process */
#define MinGrey           0

static unsigned int tone_scale_adjust();
static void         LeftToRight();
static void         RightToLeft();

/* RGB intensity tables.  red is (val * 0.30), green is (val * 0.59), blue
 * is (val * .11), where val is intensity >> 8.  these are used by the
 * colorIntensity() macro in images.h.
 */

static unsigned short RedIntensity[256]= {
      0,    76,   153,   230,   307,   384,   460,   537,
    614,   691,   768,   844,   921,   998,  1075,  1152,
   1228,  1305,  1382,  1459,  1536,  1612,  1689,  1766,
   1843,  1920,  1996,  2073,  2150,  2227,  2304,  2380,
   2457,  2534,  2611,  2688,  2764,  2841,  2918,  2995,
   3072,  3148,  3225,  3302,  3379,  3456,  3532,  3609,
   3686,  3763,  3840,  3916,  3993,  4070,  4147,  4224,
   4300,  4377,  4454,  4531,  4608,  4684,  4761,  4838,
   4915,  4992,  5068,  5145,  5222,  5299,  5376,  5452,
   5529,  5606,  5683,  5760,  5836,  5913,  5990,  6067,
   6144,  6220,  6297,  6374,  6451,  6528,  6604,  6681,
   6758,  6835,  6912,  6988,  7065,  7142,  7219,  7296,
   7372,  7449,  7526,  7603,  7680,  7756,  7833,  7910,
   7987,  8064,  8140,  8217,  8294,  8371,  8448,  8524,
   8601,  8678,  8755,  8832,  8908,  8985,  9062,  9139,
   9216,  9292,  9369,  9446,  9523,  9600,  9676,  9753,
   9830,  9907,  9984, 10060, 10137, 10214, 10291, 10368,
  10444, 10521, 10598, 10675, 10752, 10828, 10905, 10982,
  11059, 11136, 11212, 11289, 11366, 11443, 11520, 11596,
  11673, 11750, 11827, 11904, 11980, 12057, 12134, 12211,
  12288, 12364, 12441, 12518, 12595, 12672, 12748, 12825,
  12902, 12979, 13056, 13132, 13209, 13286, 13363, 13440,
  13516, 13593, 13670, 13747, 13824, 13900, 13977, 14054,
  14131, 14208, 14284, 14361, 14438, 14515, 14592, 14668,
  14745, 14822, 14899, 14976, 15052, 15129, 15206, 15283,
  15360, 15436, 15513, 15590, 15667, 15744, 15820, 15897,
  15974, 16051, 16128, 16204, 16281, 16358, 16435, 16512,
  16588, 16665, 16742, 16819, 16896, 16972, 17049, 17126,
  17203, 17280, 17356, 17433, 17510, 17587, 17664, 17740,
  17817, 17894, 17971, 18048, 18124, 18201, 18278, 18355,
  18432, 18508, 18585, 18662, 18739, 18816, 18892, 18969,
  19046, 19123, 19200, 19276, 19353, 19430, 19507, 19584
};

static unsigned short GreenIntensity[256]= {
     0,  151,  302,  453,  604,  755,  906, 1057,
  1208, 1359, 1510, 1661, 1812, 1963, 2114, 2265,
  2416, 2567, 2718, 2869, 3020, 3171, 3322, 3473,
  3624, 3776, 3927, 4078, 4229, 4380, 4531, 4682,
  4833, 4984, 5135, 5286, 5437, 5588, 5739, 5890,
  6041, 6192, 6343, 6494, 6645, 6796, 6947, 7098,
  7249, 7400, 7552, 7703, 7854, 8005, 8156, 8307,
  8458, 8609, 8760, 8911, 9062, 9213, 9364, 9515,
  9666, 9817, 9968,10119,10270,10421,10572,10723,
 10874,11025,11176,11328,11479,11630,11781,11932,
 12083,12234,12385,12536,12687,12838,12989,13140,
 13291,13442,13593,13744,13895,14046,14197,14348,
 14499,14650,14801,14952,15104,15255,15406,15557,
 15708,15859,16010,16161,16312,16463,16614,16765,
 16916,17067,17218,17369,17520,17671,17822,17973,
 18124,18275,18426,18577,18728,18880,19031,19182,
 19333,19484,19635,19786,19937,20088,20239,20390,
 20541,20692,20843,20994,21145,21296,21447,21598,
 21749,21900,22051,22202,22353,22504,22656,22807,
 22958,23109,23260,23411,23562,23713,23864,24015,
 24166,24317,24468,24619,24770,24921,25072,25223,
 25374,25525,25676,25827,25978,26129,26280,26432,
 26583,26734,26885,27036,27187,27338,27489,27640,
 27791,27942,28093,28244,28395,28546,28697,28848,
 28999,29150,29301,29452,29603,29754,29905,30056,
 30208,30359,30510,30661,30812,30963,31114,31265,
 31416,31567,31718,31869,32020,32171,32322,32473,
 32624,32775,32926,33077,33228,33379,33530,33681,
 33832,33984,34135,34286,34437,34588,34739,34890,
 35041,35192,35343,35494,35645,35796,35947,36098,
 36249,36400,36551,36702,36853,37004,37155,37306,
 37457,37608,37760,37911,38062,38213,38364,38515
};

static unsigned short BlueIntensity[256]= {
     0,   28,   56,   84,  112,  140,  168,  197,
   225,  253,  281,  309,  337,  366,  394,  422,
   450,  478,  506,  535,  563,  591,  619,  647,
   675,  704,  732,  760,  788,  816,  844,  872,
   901,  929,  957,  985, 1013, 1041, 1070, 1098,
  1126, 1154, 1182, 1210, 1239, 1267, 1295, 1323,
  1351, 1379, 1408, 1436, 1464, 1492, 1520, 1548,
  1576, 1605, 1633, 1661, 1689, 1717, 1745, 1774,
  1802, 1830, 1858, 1886, 1914, 1943, 1971, 1999,
  2027, 2055, 2083, 2112, 2140, 2168, 2196, 2224,
  2252, 2280, 2309, 2337, 2365, 2393, 2421, 2449,
  2478, 2506, 2534, 2562, 2590, 2618, 2647, 2675,
  2703, 2731, 2759, 2787, 2816, 2844, 2872, 2900,
  2928, 2956, 2984, 3013, 3041, 3069, 3097, 3125,
  3153, 3182, 3210, 3238, 3266, 3294, 3322, 3351,
  3379, 3407, 3435, 3463, 3491, 3520, 3548, 3576,
  3604, 3632, 3660, 3688, 3717, 3745, 3773, 3801,
  3829, 3857, 3886, 3914, 3942, 3970, 3998, 4026,
  4055, 4083, 4111, 4139, 4167, 4195, 4224, 4252,
  4280, 4308, 4336, 4364, 4392, 4421, 4449, 4477,
  4505, 4533, 4561, 4590, 4618, 4646, 4674, 4702,
  4730, 4759, 4787, 4815, 4843, 4871, 4899, 4928,
  4956, 4984, 5012, 5040, 5068, 5096, 5125, 5153,
  5181, 5209, 5237, 5265, 5294, 5322, 5350, 5378,
  5406, 5434, 5463, 5491, 5519, 5547, 5575, 5603,
  5632, 5660, 5688, 5716, 5744, 5772, 5800, 5829,
  5857, 5885, 5913, 5941, 5969, 5998, 6026, 6054,
  6082, 6110, 6138, 6167, 6195, 6223, 6251, 6279,
  6307, 6336, 6364, 6392, 6420, 6448, 6476, 6504,
  6533, 6561, 6589, 6617, 6645, 6673, 6702, 6730,
  6758, 6786, 6814, 6842, 6871, 6899, 6927, 6955,
  6983, 7011, 7040, 7068, 7096, 7124, 7152, 7180
};

#define colorIntensity(R,G,B) \
  (RedIntensity[(R) >> 8] + GreenIntensity[(G) >> 8] + BlueIntensity[(B) >> 8])

long xwdio__ReadRow(ClassID, file, row, nbytes)

struct classheader *ClassID;
FILE *file;		
unsigned char *row;	
long nbytes;   
{

    if (fread(row, sizeof(char), nbytes, file) < 0) 
	return dataobject_PREMATUREEOF;
    return dataobject_NOREADERROR;
}

static void reverse_video(location, output, nbytes)
/* invert black/white values:
  raster:  0 = white, 1 = black
  xwd:     0 = black, 1 = white
  */
unsigned char *location, *output;
long nbytes;
{
   unsigned char *end = location + nbytes;
   
   while(location < end)
	*output++ = ~(*location++);

}

void xwdio__WriteRow(ClassID, file, row, nbytes)

struct classheader *ClassID;
FILE *file;
unsigned char *row;
long nbytes;
{

    unsigned char reversebuf[BUFBITS >> 3];
    
    reverse_video(row, reversebuf, nbytes);
    fwrite(reversebuf, nbytes, 1, file);

}

static void _swapshort (bp, n)
     register char *bp;
     register unsigned n;
{
  register char c;
  register char *ep = bp + n;
  
  while (bp < ep) 
    {
      c = *bp;
      *bp = *(bp + 1);
      bp++;
      *bp++ = c;
    }
}

static void _swaplong (bp, n)
register char *bp;
register unsigned n;
{
    register char c;
    register char *ep = bp + n;
    register char *sp;

    while (bp < ep) 
    {
	sp = bp + 3;
	c = *sp;
	*sp = *bp;
	*bp++ = c;
	sp = bp + 1;
	c = *sp;
	*sp = *bp;
	*bp++ = c;
	bp += 2;

    }
}

static void reverse_bit_order(location, output, nbytes)
unsigned char *location, *output;
long nbytes;
{
unsigned char *end = location + nbytes;
static unsigned char reverse_table[256] = {
    0x00,0x80,0x40,0xc0,0x20,0xa0,0x60,0xe0,0x10,0x90,0x50,0xd0,0x30,0xb0,0x70,0xf0,
    0x08,0x88,0x48,0xc8,0x28,0xa8,0x68,0xe8,0x18,0x98,0x58,0xd8,0x38,0xb8,0x78,0xf8,
    0x04,0x84,0x44,0xc4,0x24,0xa4,0x64,0xe4,0x14,0x94,0x54,0xd4,0x34,0xb4,0x74,0xf4,
    0x0c,0x8c,0x4c,0xcc,0x2c,0xac,0x6c,0xec,0x1c,0x9c,0x5c,0xdc,0x3c,0xbc,0x7c,0xfc,
    0x02,0x82,0x42,0xc2,0x22,0xa2,0x62,0xe2,0x12,0x92,0x52,0xd2,0x32,0xb2,0x72,0xf2,
    0x0a,0x8a,0x4a,0xca,0x2a,0xaa,0x6a,0xea,0x1a,0x9a,0x5a,0xda,0x3a,0xba,0x7a,0xfa,
    0x06,0x86,0x46,0xc6,0x26,0xa6,0x66,0xe6,0x16,0x96,0x56,0xd6,0x36,0xb6,0x76,0xf6,
    0x0e,0x8e,0x4e,0xce,0x2e,0xae,0x6e,0xee,0x1e,0x9e,0x5e,0xde,0x3e,0xbe,0x7e,0xfe,
    0x01,0x81,0x41,0xc1,0x21,0xa1,0x61,0xe1,0x11,0x91,0x51,0xd1,0x31,0xb1,0x71,0xf1,
    0x09,0x89,0x49,0xc9,0x29,0xa9,0x69,0xe9,0x19,0x99,0x59,0xd9,0x39,0xb9,0x79,0xf9,
    0x05,0x85,0x45,0xc5,0x25,0xa5,0x65,0xe5,0x15,0x95,0x55,0xd5,0x35,0xb5,0x75,0xf5,
    0x0d,0x8d,0x4d,0xcd,0x2d,0xad,0x6d,0xed,0x1d,0x9d,0x5d,0xdd,0x3d,0xbd,0x7d,0xfd,
    0x03,0x83,0x43,0xc3,0x23,0xa3,0x63,0xe3,0x13,0x93,0x53,0xd3,0x33,0xb3,0x73,0xf3,
    0x0b,0x8b,0x4b,0xcb,0x2b,0xab,0x6b,0xeb,0x1b,0x9b,0x5b,0xdb,0x3b,0xbb,0x7b,0xfb,
    0x07,0x87,0x47,0xc7,0x27,0xa7,0x67,0xe7,0x17,0x97,0x57,0xd7,0x37,0xb7,0x77,0xf7,
    0x0f,0x8f,0x4f,0xcf,0x2f,0xaf,0x6f,0xef,0x1f,0x9f,0x5f,0xdf,0x3f,0xbf,0x7f,0xff,
};

while(location < end)
    *output++ = reverse_table[*location++];
}

long xwdio__ReadImage(ClassID, file, pix)

struct classheader *ClassID;
FILE *file;		
struct pixelimage *pix;	
{
    unsigned char *row;
    unsigned int rownum, rowsize, i = 0;
    struct _xwd_file_header hdr;
    int win_name_size = 0, ncolors = 0;
    char win_name[512];
    unsigned long swaptest = 1;
    XColor colors[512];
    boolean reduce_pixmap = FALSE, swap = FALSE;

    /* Check byte order of this machine */
    if (*(char *) &swaptest) 
    {
	swap = TRUE;
    }

    /* Read the header -- Note that the window name is not part
      of the actual header structure, but its size is included in 
      hdr.header_size.
      */

    if (fread(&hdr, sizeof(hdr), 1, file) < 0) 
	return dataobject_PREMATUREEOF;
    
    /* Swap header bytes if necessary */

    if (swap) _swaplong((char *) &hdr, sizeof(hdr));

    if (hdr.pixmap_depth == 8 && hdr.pixmap_format == ZPixmap)	  
	reduce_pixmap = TRUE;
    else if (hdr.pixmap_depth == 8 && hdr.pixmap_format == XYPixmap)
    {
	fprintf(stderr, "Xwd file is in 8 bit/pixel XYPixmap format, only ZPixmap is supported for 8-bit\n");
	fflush(stderr);
	return dataobject_BADFORMAT;
    }
    else if (hdr.pixmap_depth == 8)
    {
	fprintf(stderr, "Xwd file is in 8 bit/pixel format of unknown type -- only ZPixmap is supported\n");
	fflush(stderr);
	return dataobject_BADFORMAT;
    }
    else if (hdr.pixmap_depth != 1 && hdr.pixmap_depth !=8)
    {
	fprintf(stderr,	"Xwd file is %d	bits/pixel; only 1 bit/pixel or 8-bit ZPixmap are supported\n",
		hdr.pixmap_depth  );
	fflush(stderr);
	return dataobject_BADFORMAT;
    }



    win_name_size = (hdr.header_size - sizeof(hdr));
    fread(win_name, sizeof(char), win_name_size, file);

    /* Read the colormap */

    ncolors = hdr.ncolors;
    fread((char *) colors, sizeof(XColor), ncolors, file);

    /* Swap colormap bytes if necessary */
    if (swap)
    {
	for (i = 0; i < ncolors; i++) 
	{
	    
	  _swaplong((char *) &colors[i].pixel, sizeof(long));
	  _swapshort((char *) &colors[i].red, sizeof(short));
	  _swapshort((char *) &colors[i].green, sizeof(short));
	  _swapshort((char *) &colors[i].blue, sizeof(short));

	}
    }
  

    /* initialize pixelimage for loading */

    rowsize = hdr.bytes_per_line;
    pixelimage_Resize(pix, (rowsize * 8), hdr.pixmap_height);
    pixelimage_Clear(pix);
    row = pixelimage_GetBitsPtr(pix);
    
    /* read the actual pixelimage 
      Note that rows must be padded out to short-word (16-bit) multiples since pixelimage__Create does so when setting size for efficient handling, otherwise image would be skewed by eight bits per line */


    if (reduce_pixmap) { /* 8-bit ZPixmap version */
	unsigned char  *src;		/* source data */
	int            *curr;		/* current line buffer */
	int            *next;		/* next line buffer */
	int            *swap;		/* for swapping line buffers */
	unsigned int   *grey;		/* grey map for source image */
	unsigned char	color;		/* an offset into the colormap */
	unsigned int	level;		/* grey level */

	if (hdr.pixmap_depth <= 16) {
	    grey = (unsigned int *)calloc(ncolors, sizeof(unsigned int));
	    for (i = 0; i < ncolors; i++)
		grey[i] =
		  (colorIntensity(colors[i].red,
				  colors[i].green,
				  colors[i].blue) >> 1);
	    for (i = 0; i < ncolors; i++)
		grey[i] = tone_scale_adjust(grey[i]);
	}
	else
	    grey = NULL;

	src   = (unsigned char *)calloc(rowsize, sizeof(unsigned char));
	curr  = (int *)calloc((rowsize + 2), sizeof(int));
	next  = (int *)calloc((rowsize + 2), sizeof(int));
	curr += 1;
	next += 1;

	for (rownum = 0; rownum < hdr.pixmap_height; 
	row += ((rowsize + 1) / 2) * 2, rownum++) {	
	    xwdio_ReadRow(file, src, rowsize);
	    for (i = 0; i < rowsize; i++) {
		color = *(src + i);
		if (grey == NULL)
		    level =
		      tone_scale_adjust(colorIntensity(colors[color].red,
						       colors[color].green,
						       colors[color].blue) >> 1);
		else
		    level = grey[color];
		curr[i] += level;
	    }

	    /* dither the current line */
	    if (rownum & 0x01)
		RightToLeft(curr, next, rowsize);
	    else
		LeftToRight(curr, next, rowsize);

	    /* copy the dithered line to the destination image */
	    for (i = 0; i < rowsize; i++)
		if (curr[i] < Threshold)
		    row[i/8] |= 1 << (7 - (i & 7));

	    /* circulate the line buffers */
	    swap = curr;
	    curr = next;
	    next = swap;
	    for (i = 0; i < rowsize; i++) {
		next[i] = 0;
		src[i] = (unsigned char) 0;
	    }
	}
	free((unsigned char *)grey);
	free((unsigned char *)(curr-1));
	free((unsigned char *)(next-1));
	free((unsigned char *)src);
    }

    else { /* bitmap version */
	for (rownum = 0; rownum < hdr.pixmap_height; 
	rownum++, row += ((rowsize + 1) / 2) * 2)
	{	
	    /* modify image directly, since it is 
	     useless in byte-swapped negative-image form
	     */
	    xwdio_ReadRow(file, row, rowsize);
	    reverse_video(row, row, rowsize);
	    if (!hdr.bitmap_bit_order)  
		reverse_bit_order(row, row, rowsize);
	}
    }
    pixelimage_Resize(pix, hdr.pixmap_width, hdr.pixmap_height);
    pixelimage_NotifyObservers(pix, pixelimage_DATACHANGED);
    return dataobject_NOREADERROR;
}

void xwdio__WriteImage(ClassID, file, pix, sub)

struct classheader *ClassID;
register FILE *file;		
register struct pixelimage *pix;
register struct rectangle *sub;
{
    long left, top, width, height;
    long buf[BUFBITS>>5];		
    struct _xwd_file_header	hdr;	/* XWD V7 file header */
    long row, bytes_per_row;
    char name[7];
    int ncolors = 2;    /* black & white*/
    XColor colors[2];
    int i = 0;
    unsigned long swaptest = 1;
    boolean swap = FALSE;

    /* Check byte order of this machine */
    if (! *(char *) &swaptest) 
    {
	swap = TRUE;
    }




    rectangle_GetRectSize(sub, &left, &top, &width, &height);

    /* pad rows out to a multiple of 8 bits */
    bytes_per_row = (width + 7) / 8;

    /* fill in hdr  - many values are hardwired  
      since this isn't  a real X11 window dump */

    strcpy(name,"raster");  

    hdr.header_size = (xwdval) (sizeof(hdr) + sizeof(name));
    hdr.file_version = (xwdval) XWD_FILE_VERSION;
    hdr.pixmap_format = (xwdval) ZPixmap;
    hdr.pixmap_depth = (xwdval) 1;	
    hdr.pixmap_width = (xwdval) width;
    hdr.pixmap_height = (xwdval) height;
    hdr.xoffset = (xwdval) 0;
    hdr.byte_order = (xwdval) 1;
    hdr.bitmap_unit = (xwdval) 8;
    hdr.bitmap_bit_order = (xwdval) 1;
    hdr.bitmap_pad = (xwdval) 8;
    hdr.bits_per_pixel = (xwdval) 1;
    hdr.bytes_per_line = (xwdval) bytes_per_row;
    hdr.visual_class = (xwdval) 0;
    hdr.red_mask = (xwdval) 0;
    hdr.green_mask = (xwdval) 0;
    hdr.blue_mask = (xwdval) 0;
    hdr.bits_per_rgb = (xwdval) 1; 
    hdr.colormap_entries = (xwdval) ncolors; 
    hdr.ncolors = (xwdval) ncolors;
    hdr.window_width = (xwdval) width;
    hdr.window_height = (xwdval) height;
    hdr.window_x = 0;  /* not xwdval */
    hdr.window_y = 0;   /* not xwdval */
    hdr.window_bdrwidth = (xwdval) 0;

    /* Create colormap */
    colors[0].pixel	= 0;	/* black */
    colors[0].red = 0;
    colors[0].green = 0;
    colors[0].blue	= 0;
    colors[1].pixel	= 1;	/* white */
    colors[1].red = ~0;
    colors[1].green = ~0;
    colors[1].blue = ~0;

    /* swap byte order if necessary */

    if (!swap)
    { 
	_swaplong((char *) &hdr, sizeof(hdr));
	for(i = 0; i < ncolors; i++)
	{
	    _swaplong((char *) &colors[i].pixel, sizeof(long));
	    _swapshort((char *) &colors[i].red, sizeof(short));
	    _swapshort((char *) &colors[i].green, sizeof(short));
	    _swapshort((char *) &colors[i].blue, sizeof(short));
	}
    }


    /* write out  header*/
    fwrite(&hdr, sizeof(hdr), 1, file);

    /* write out window name */
    fwrite(name, sizeof(name), 1, file);

    /* write out colormap */
    fwrite((char *) colors, sizeof(XColor), ncolors, file);

    /* write out pixelimage */
    for (row = top;  row < top + height; row++) 
    {
	pixelimage_GetRow(pix, left, row, width, buf);
	xwdio_WriteRow(file, (unsigned char *)buf, bytes_per_row);
    }
}





static void pixmap_to_bitmap(location, output, nbytes)
unsigned char *location, *output;
long nbytes;
{
  register int x, c, b;

/* Simple method: take each byte (which represents a pixel), and convert 
 it to a 0 if it is 0 or a 1 if it is any othre value, and make one byte out
of each eight such converted values */

  c = 0; b = 1;
  for (x = 0; x < nbytes;)
  {
      if (*(location + x)) c |= b;
      b <<= 1;
      if (!(++x & 7))
      {
	  *(output++) = c;
	  c = 0; b = 1;
      }
  }
}

/*
 * a _very_ simple tone scale adjustment routine. provides a piecewise
 * linear mapping according to the following:
 *
 *      input:          output:
 *     0 (MinGrey)    0 (MinGrey)
 *     Threshold      Threshold/2
 *     MaxGrey        MaxGrey
 * 
 * this should help things look a bit better on most displays.
 */
static unsigned int tone_scale_adjust(val)
     unsigned int val;
{
  unsigned int rslt;
  
  if (val < Threshold)
    rslt = val / 2;
  else
    rslt = (((val - Threshold) * (MaxGrey-(Threshold/2))) /
	    (MaxGrey-Threshold)) + (Threshold/2);

  return rslt;
}


/*
 * dither a line from left to right
 */
static void LeftToRight(curr, next, width)
     int *curr;
     int *next;
     int  width;
{
  int idx;
  int error;
  int output;

  for (idx=0; idx<width; idx++)
  {
    output       = (curr[idx] > Threshold) ? MaxGrey : MinGrey;
    error        = curr[idx] - output;
    curr[idx]    = output;
    next[idx-1] += error * 3 / 16;
    next[idx]   += error * 5 / 16;
    next[idx+1] += error * 1 / 16;
    curr[idx+1] += error * 7 / 16;
  }
}


/*
 * dither a line from right to left
 */
static void RightToLeft(curr, next, width)
     int *curr;
     int *next;
     int  width;
{
  int idx;
  int error;
  int output;

  for (idx=(width-1); idx>=0; idx--)
  {
    output       = (curr[idx] > Threshold) ? MaxGrey : MinGrey;
    error        = curr[idx] - output;
    curr[idx]    = output;
    next[idx+1] += error * 3 / 16;
    next[idx]   += error * 5 / 16;
    next[idx-1] += error * 1 / 16;
    curr[idx-1] += error * 7 / 16;
  }
}
