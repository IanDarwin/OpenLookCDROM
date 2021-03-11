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

/*
 * Copyright 1989, 1990, 1991 Jim Frost
 *
 * Permission to use, copy, modify, distribute, and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  The author makes no representations
 * about the suitability of this software for any purpose.  It is
 * provided "as is" without express or implied warranty.
 *
 * THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN
 * NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS
 * OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
 * OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE
 * USE OR PERFORMANCE OF THIS SOFTWARE.
 */

/* $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/common/RCS/image.c,v 1.35 1994/02/24 19:54:30 rr2b Exp $ */
/* $ACIS:graphic.c 1.11$ */
/* $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/common/RCS/image.c,v $ */


#ifndef LINT
	char image_rcsid[] = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/common/RCS/image.c,v 1.35 1994/02/24 19:54:30 rr2b Exp $";
#endif /* LINT */

/* image.c
 */

#include <andrewos.h>
#include <class.h>
#include <ctype.h>
#include <netinet/in.h>	/* Get the definition for ntohl. */

/* for maxpathlen */
#include <sys/param.h>
	
#include <attribs.h>
#include <environ.ih>
#include <jpeg.ih>
#include <gif.ih>
#include <image.eh>

extern int errno;
#define MAXFILELINE 255
#define DEFAULT_SAVE_QUALITY (75)

static char *imageTypeLabels[NUM_IMAGE_TYPES] = {
    "IBITMAP",
    "IGREYSCALE",
    "IRGB",
    "ITRUE"
};

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

/* 4x4 arrays used for dithering, arranged by nybble
 */

#define GRAYS    17 /* ((4 * 4) + 1) patterns for a good dither */
#define GRAYSTEP ((unsigned long)(65536 / GRAYS))

static byte DitherBits[GRAYS][4] = {
  0xf, 0xf, 0xf, 0xf,
  0xe, 0xf, 0xf, 0xf,
  0xe, 0xf, 0xb, 0xf,
  0xa, 0xf, 0xb, 0xf,
  0xa, 0xf, 0xa, 0xf,
  0xa, 0xd, 0xa, 0xf,
  0xa, 0xd, 0xa, 0x7,
  0xa, 0x5, 0xa, 0x7,
  0xa, 0x5, 0xa, 0x5,
  0x8, 0x5, 0xa, 0x5,
  0x8, 0x5, 0x2, 0x5,
  0x0, 0x5, 0x2, 0x5,
  0x0, 0x5, 0x0, 0x5,
  0x0, 0x4, 0x0, 0x5,
  0x0, 0x4, 0x0, 0x1,
  0x0, 0x0, 0x0, 0x1,
  0x0, 0x0, 0x0, 0x0
};

static unsigned long DepthToColorsTable[] = {
  /*  0 */ 1,
  /*  1 */ 2,
  /*  2 */ 4,
  /*  3 */ 8,
  /*  4 */ 16,
  /*  5 */ 32,
  /*  6 */ 64,
  /*  7 */ 128,
  /*  8 */ 256,
  /*  9 */ 512,
  /* 10 */ 1024,
  /* 11 */ 2048,
  /* 12 */ 4096,
  /* 13 */ 8192,
  /* 14 */ 16384,
  /* 15 */ 32768,
  /* 16 */ 65536,
  /* 17 */ 131072,
  /* 18 */ 262144,
  /* 19 */ 524288,
  /* 20 */ 1048576,
  /* 21 */ 2097152,
  /* 22 */ 4194304,
  /* 23 */ 8388608,
  /* 24 */ 16777216,
  /* 25 */ 33554432,
  /* 26 */ 67108864,
  /* 27 */ 134217728,
  /* 28 */ 268435456,
  /* 29 */ 536870912,
  /* 30 */ 1073741824,
  /* 31 */ 2147483648,
  /* 32 */ 2147483648 /* bigger than unsigned int; this is good enough */
};

#define depthToColors(n) DepthToColorsTable[((n) < 32 ? (n) : 32)]

boolean
image__InitializeClass( classID )
    struct classheader *classID;
{
  return(TRUE);
}

boolean
image__InitializeObject( classID, self )
    struct classheader *classID;
    struct image *self;
{
    char *saveformat;

    image_Type(self) = 0;
    if(!(self->rgb = (RGBMap *) calloc(1, sizeof(RGBMap))) )
	return(FALSE);
    image_Width(self) = 0;
    image_Height(self) = 0;
    image_Depth(self) = 0;
    image_Pixlen(self) = 0;
    image_Data(self) = NULL;
    self->inited = FALSE;
    self->jpegSaveQuality = environ_GetProfileInt("imagesavequality", DEFAULT_SAVE_QUALITY);
    self->origData = NULL;
    self->origDataSize = 0;
    self->lastModified = image_GetModified(self);
    if(!(saveformat = environ_GetProfile("imagesaveformat")))
	saveformat = "gif";
    self->saveformatstring = (char *) malloc(strlen(saveformat) + 1);
    strcpy(self->saveformatstring, saveformat);
    return(TRUE);
}

void
image__Duplicate( self, target )
    struct image *self;
    struct image *target;
{ register int i;
  int size = 0;

    image_Type(target) = image_Type(self);
    switch(image_Type(self)) {
	case IRGB:
	case IGREYSCALE:
	    size = (image_Width(self) * image_Height(self));
	    break;
	case ITRUE:
	    size = (image_Width(self) * image_Height(self) * 3);
	    break;
	case IBITMAP:
	    size = (((image_Width(self) / 8) + (image_Width(self) % 8 ? 1 : 0)) * image_Height(self));
	    break;
    }

    if (!TRUEP(target))
	image_newRGBMapData(target, image_RGBSize(self)); 
    for(i = 0; i < image_RGBSize(self); i++) {
	image_RedPixel(target, i) = image_RedPixel(self, i);
	image_GreenPixel(target, i) = image_GreenPixel(self, i);
	image_BluePixel(target, i) = image_BluePixel(self, i);
      }
    image_RGBUsed(target) = image_RGBUsed(self);
    target->rgb->compressed = self->rgb->compressed;

    image_Width(target) = image_Width(self);
    image_Height(target) = image_Height(self);
    image_Pixlen(target) = image_Pixlen(self);
    image_Depth(target) = image_Depth(self);
    target->jpegSaveQuality = image_GetJPEGSaveQuality(self);

    if(image_Data(target))
	free(image_Data(target));
    if(size>0) image_Data(target) = (unsigned char*) malloc(size);
    else image_Data(target)=NULL;

    if(image_Data(self)!=NULL && image_Data(target)==NULL) bzero(image_Data(self), size);
    if(image_Data(self) && image_Data(target)) bcopy(image_Data(self), image_Data(target), size);
    
    target->inited = FALSE;
    target->lastModified = self->lastModified;
}

void
image__FinalizeObject( classID, self )
    struct classheader *classID;
    struct image *self;
{
    image_freeImageData(self);
    if(self->origData) {
	free(self->origData);
	self->origData = NULL;
	self->origDataSize = 0;
    }
    if(self->saveformatstring)
	free(self->saveformatstring);
}

static char Basis[65] =
	"0123456789:=ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";

static unsigned char DigVals[96] = {
	0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 0, 0,		/* 040 thru 057 */
	0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 0, 0, 11, 0, 0,		/* 060 thru 077 */
	0, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26,	/* 0100 thru 0117 */
	27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 0, 0, 0, 0, 10,	/* 0120 thru 0137 */
	0, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52,	/* 0140 thru 0157 */
	53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 0, 0, 0, 0, 0	/* 0160 thru 0177 */
};

static char *convlongto64(num, pad)
/* unsigned */ long num;
/* unsigned */ int pad;
{
    static char Answer[7];

    Answer[6] =	0;	/* Initialize null termination */
    Answer[5] = Basis[(num & 077)];
    Answer[4] = Basis[(num >> 6) & 077];
    Answer[3] = Basis[(num >> 12) & 077];
    Answer[2] = Basis[(num >> 18) & 077];
    Answer[1] = Basis[(num >> 24) & 077];
    Answer[0] = Basis[((num >> 30) & 003) | ((pad & 017) << 2)];

    return(Answer);
}

/* Note that the following routine throws away the first 4 of 36 bits */

static unsigned long conv64tolong(xnum)
register char *xnum;
{
    register int digits;
    unsigned long Answer = 0;

    digits = strlen(xnum);
    if (digits > 6) digits = 6;
    switch(digits) {
	case 6: Answer |= DigVals[(*xnum)-040] << 30; ++xnum;
	case 5: Answer |= DigVals[(*xnum)-040] << 24; ++xnum;
	case 4: Answer |= DigVals[(*xnum)-040] << 18; ++xnum;
	case 3: Answer |= DigVals[(*xnum)-040] << 12; ++xnum;
	case 2: Answer |= DigVals[(*xnum)-040] << 6; ++xnum;
	case 1: Answer |= DigVals[(*xnum)-040];
    }
    return(Answer);
}

#define RANDOMARRAYSIZE 64
static char RandomSpace[RANDOMARRAYSIZE + 1];	/* For random number generation routines */
extern long random();
#define ChooseRandomBits(n) (((unsigned long) random()) >> (32-n))

static char *genid()
{
    static char IDBuf[20];
    static long mycounter = 0, MyPid = -1;
    static unsigned long MyHostAddr;
/*    struct timeval tp;
    struct timezone tz; */
    struct osi_Times tp;
    int quadmillisecs; /* for 8 bits of sub-second time resolution */

/*    gettimeofday(&tp, &tz); */
    osi_GetTimes(&tp);
    if (MyPid < 0) {
	MyPid = getpid();
	MyHostAddr = (unsigned long) getaddr();
	MyHostAddr = ntohl(MyHostAddr);	/* so it's in the same space for all machines */
	initstate(tp.Secs ^ MyHostAddr ^ MyPid, RandomSpace, RANDOMARRAYSIZE);
    }
    quadmillisecs = (tp.USecs <<8) / 1000000;
    strcpy(IDBuf, convlongto64(tp.Secs, quadmillisecs & 0xF));
    strcat(IDBuf, convlongto64(MyHostAddr, (quadmillisecs >> 4) & 0xF));
#ifdef USESHORTFILENAMES
	IDBuf[12] = Basis[(++mycounter) & 0x3F];
	IDBuf[13] = '\0';
#else
	strcat(IDBuf, convlongto64(MyPid<<16 | (((++mycounter)<<8) & 0x0000FF00) |ChooseRandomBits(8), ChooseRandomBits(4)));
#endif
    return(IDBuf);
}

long
image__GetBeginData(self, file, id)
    struct image *self;
    FILE *file;
    long id;
{
    int tc;
    if(file == NULL) 
	return(dataobject_PREMATUREEOF);
    ungetc(tc = getc(file), file);
    if(tc == '\\') { /* this really shouldn't be true */
	long discardid;
	if (fscanf(file, "\\begindata{image,%ld", &discardid) != 1
	    || getc(file) != '}' || getc(file) != '\n') {
	    return(dataobject_NOTATKDATASTREAM);
	}
    }
    return(dataobject_NOREADERROR);
}

long
image__GetImageData(self, file)
    struct image *self;
    FILE *file;
{
    char tmpName1[100], tmpName2[100];
    char format[64];
    FILE *tmpFile1, *tmpFile2; 
    unsigned char buf[BUFSIZ];
    long status;

    *format = (char)0;
    if(fscanf(file, "format: %s", format) == 0) {  /* empty image ? Better be! */
	while(fgets(buf, sizeof(buf), file))
	    if(!strncmp(buf, "\\enddata", 8)) break;
	return(dataobject_NOREADERROR);
    }
    if(*format == (char)0) {
	fprintf(stderr, "image: No save-format found in image data\n");
	return(dataobject_BADFORMAT);
    }
    else {
	if(self->saveformatstring)
	    free(self->saveformatstring);
	self->saveformatstring = (char *) malloc(strlen(format) + 1);
	strcpy(self->saveformatstring, format);
    }

    /* open temp files */
    sprintf(tmpName1, "/tmp/%s", genid());
    sprintf(tmpName2, "/tmp/%s", genid());
    if( (tmpFile1 = fopen(tmpName1, "w")) == NULL || 
       (tmpFile2 = fopen(tmpName2, "w")) == NULL ) { /* Error */	
	if(tmpFile1) {
	    fclose(tmpFile1);
	    unlink(tmpName1);
	}
	fprintf(stderr, "image: couldn't open %s for writing.\n", tmpFile1 == NULL ? tmpName1 : tmpName2);
	return(dataobject_OBJECTCREATIONFAILED);
    }

    /* Write image bytes to tmpFile1 */
    while(fgets(buf, sizeof(buf), file)) {
	if(!strncmp(buf, "\\enddata", 8)) {
	    fclose(tmpFile1);
	    break;
	}
	fputs(buf, tmpFile1);
    }

    /* open temp1 and decode base64 data into temp2 */
    if(tmpFile1 = fopen(tmpName1, "r")) {
	from64(tmpFile1, tmpFile2);
	fclose(tmpFile1); unlink(tmpName1);
	fclose(tmpFile2);
    }
    else {
	fprintf(stderr, "image: couldn't open %s for reading.\n", tmpName1);
	return(dataobject_OBJECTCREATIONFAILED);
    }

    /* open temp2 and decompress/load file */
    if(!(tmpFile2 = fopen(tmpName2, "r"))) {
	fprintf(stderr, "image: couldn't open %s for reading.\n", tmpName2);
	return(dataobject_OBJECTCREATIONFAILED);
    }
    else {
	struct image *savedimage;
	if(!(savedimage = (struct image *) class_NewObject(format))) {
	    fclose(tmpFile2);
	    unlink(tmpName2);
	    fprintf(stderr, "image: couldn't get new object of type %s.\n", format);
	    return(-1);
	}
	image_Type(savedimage) = image_Type(self);
	if(image_Load(savedimage, NULL, tmpFile2) < 0) {
	    fclose(tmpFile2);
	    unlink(tmpName2);
	    image_Destroy(savedimage);
	    fprintf(stderr, "image: couldn't load image of type %s.\n", format);
	    return(-1);
	}

	/* reset self and copy in data */
	image_Reset(self);
	image_Duplicate(savedimage, self);
	image_Destroy(savedimage);
    }
    fclose(tmpFile2);
    unlink(tmpName2);
    return(dataobject_NOREADERROR);
}

long
image__GetEndData(self, file, id)
    struct image *self;
    FILE *file;
    long id;
{
/* This is a noop because GetImageData can deal with or without the enddata */
    return(dataobject_NOREADERROR);
}

static long
WriteImageToTempFile( self, file )
    struct image *self;
    FILE *file;
{
    char tmpName[MAXPATHLEN];
    char buf[BUFSIZ];
    FILE *f;
    int size = 0;
    long retval = dataobject_NOREADERROR;
    
    if(self->origData) {
	free(self->origData);
	self->origDataSize = 0;
    }
    sprintf(tmpName, "/tmp/%s", genid());
    if(f = fopen(tmpName, "w")) {
	int savepos = ftell(file), cnt;

	while(fgets(buf, sizeof(buf), file)) {
	    char *newln= strchr(buf, '\n');
	    if(!strncmp(buf, "\\enddata", 8)) break;
	    if (!newln) {
		/* this is some big nasty binary file -RSK*/
		retval= dataobject_NOTATKDATASTREAM;
		break; /* otherwise the subsequent calculations will make fwrite() crash */
	    }
	    cnt = newln - buf + 1;
	    size += cnt;
	    fwrite(buf, sizeof(char), cnt, f);
	}

	fclose(f);
	if(self->origData = (char*) calloc(size, sizeof(char))) {
	    if(f = fopen(tmpName, "r")) {
		if((cnt = fread(self->origData, sizeof(char), size, f)) != size) {
		    fprintf(stderr, "image: short read on image data.\n");
		    retval= dataobject_PREMATUREEOF; /*RSK*/
		    free(self->origData);
		    self->origData = NULL;
		    self->origDataSize = 0;
		}
		else {
		    self->origDataSize = size;
		}
	    }
	    else {
		fprintf(stderr, "image: couldn't open temp file for writing.\n");
		retval= dataobject_OBJECTCREATIONFAILED; /* not quite true, but close enough -RSK*/
		self->origData = NULL;
		self->origDataSize = 0;
	    }
	}
	fclose(f);
	unlink(tmpName);
	fseek(file, savepos, 0);
    }
    else {
	fprintf(stderr, "image: couldn't open temp file for writing.\n");
	retval= dataobject_OBJECTCREATIONFAILED; /* not quite true, but close enough -RSK*/
	self->origData = NULL;
	self->origDataSize = 0;
    }
    return retval;
}

long
image__Read( self, file, id )
    struct image *self;
    FILE *file;
    long id;
{
    long status;

    if((status=WriteImageToTempFile(self, file))==0 && /* RSK */
       (status = image_GetBeginData(self, file, id)) == 0 &&
       (status = image_GetImageData(self, file)) == 0 &&
       (status = image_GetEndData(self, file, id)) == 0) {
	if(!self->rgb->compressed )
	    image_Compress(self);
	image_NotifyObservers(self, image_NEW);
    }
    return(status);
}

long
image__SendBeginData(self, file, writeID, level)
    struct image *self;
    FILE *file;
    long writeID;
    int level;
{
    long id = image_UniqueID(self);
    self->header.dataobject.writeID = writeID;
    if(fprintf(file, "\\begindata{image,%d}\n", id) < 0)
	return(-1);
    else
	return(id);
}

long
image__SendImageData(self, file)
    struct image *self;
    FILE *file;
{
	if(image_Data(self)) {
	    if(image_GetModified(self) > self->lastModified || self->origData == NULL) {
		struct image *savedimage = (struct image *) class_NewObject(self->saveformatstring);
		char tmpName[100];
		FILE *tmpFile; 

		sprintf(tmpName, "/tmp/%s", genid());
		if(tmpFile = fopen(tmpName, "w")) {

	/* duplicate self into savedimage and write native format data into tmp file */
		    image_Duplicate(self, savedimage);
		    if(strcmp(self->saveformatstring, "jpeg") == 0)
			jpeg_SetSaveQuality((struct jpeg*) savedimage, image_GetJPEGSaveQuality(self));
		    image_WriteNative(savedimage, tmpFile, NULL);
		    image_Destroy(savedimage);
		    fclose(tmpFile);

	/* open tmp file and encode base64 into file */
		    if(!(tmpFile = fopen(tmpName, "r"))) {
			printf("image: failed to open temp file %s for reading.", tmpName);
			unlink(tmpName);
			return(-1);
		    }
		    fprintf(file,"format: %s\n", self->saveformatstring);
		    to64(tmpFile, file);
		    fclose(tmpFile);
		    unlink(tmpName);
		}
		else {
		    if(savedimage)
			image_Destroy(savedimage);
		    printf("image: failed to open temp file %s for writing.", tmpName);
		    return(-1);
		}
	    }
	    else if(self->origData) {
		fwrite(self->origData, self->origDataSize, sizeof(char), file);
	    }
	}
	else { /* important for reading an empty image datastream */
	    fprintf(file, "\n");
	}
    self->lastModified = image_GetModified(self);
    return(0);
}    

long
image__SendEndData(self, file, writeID, id)
    struct image *self;
    FILE *file;
    long writeID;
    int id;
{
    image_SetWriteID(self, writeID);
    image_SetID(self, id);
    if(fprintf(file, "\\enddata{image, %d}\n", id) < 0)
	return(-1);
    return(0);
}

long
image__Write( self, file, writeID, level )
    struct image *self;
    FILE *file;
    long writeID;
    int level;
{   long id = image_SendBeginData(self, file, writeID, level);
    long status;    
    if( (id > 0) &&
       (status = image_SendImageData(self, file)) == 0 &&
       (status = image_SendEndData(self, file, writeID, id)) == 0)
	return(id);
    else
	return(status);
}

char *
image__ViewName( self )
    struct image *self;
{
  return("imagev");
}

void
image__Reset( self )
    struct image *self;
{ 
  image_freeImageData(self);
  image_Type(self) = 0;
  image_Width(self) = 0;
  image_Height(self) = 0;
  image_Depth(self) = 0;
  image_Pixlen(self) = 0;
  self->inited = FALSE;
  self->jpegSaveQuality = environ_GetProfileInt("imagesavequality", DEFAULT_SAVE_QUALITY);
 }

static unsigned long 
colorsToDepth(ncolors)
    unsigned long ncolors;
{ unsigned long a;

  for(a = 0; (a < 32) && (DepthToColorsTable[a] < ncolors); a++);
  return(a);
}

static void 
newRGBMapData( rgb, size )
    RGBMap *rgb;
    unsigned int  size;
{ 
  rgb->used = 0;
  rgb->size = size;
  rgb->compressed = 0;
  rgb->red = (Intensity *)malloc(sizeof(Intensity) * size);
  rgb->green = (Intensity *)malloc(sizeof(Intensity) * size);
  rgb->blue = (Intensity *)malloc(sizeof(Intensity) * size);
}

void 
image__newRGBMapData( self, size )
    struct image *self;
    unsigned int  size;
{
  if(self->rgb)
      newRGBMapData(self->rgb, size);
}

void 
image__freeRGBMapData( self )
    struct image *self;
{
  if(self->rgb) {
      if(image_RedMap(self)) {
	  free((byte*) image_RedMap(self));
	  image_RedMap(self) = NULL;
      }
      if(image_GreenMap(self)) {
	  free((byte*) image_GreenMap(self));
	  image_GreenMap(self) = NULL;
      }
      if(image_BlueMap(self)) {
	  free((byte*) image_BlueMap(self));
	  image_BlueMap(self) = NULL;
      }
      image_RGBUsed(self) = image_RGBSize(self) = 0;
      self->rgb->compressed = 0;
  }
}

void
image__newBitImage( self, width, height )
    struct image *self;
    unsigned int width, height;
{ unsigned int linelen;

  image_Type(self) = IBITMAP;
  image_newRGBMapData(self, (unsigned int)2);
  image_RedPixel(self, 0) = image_GreenPixel(self, 0) = image_BluePixel(self, 0) = 65535;
  image_RedPixel(self, 1) = image_GreenPixel(self, 1) = image_BluePixel(self, 1) = 0;
  image_RGBUsed(self) = 2;
  image_Width(self) = width;
  image_Height(self) = height;
  image_Depth(self) = 1;
  image_Pixlen(self) = 1;
  linelen = (width / 8) + (width % 8 ? 1 : 0); /* thanx johnh@amcc.com */
  image_Data(self) = (unsigned char *) calloc(linelen, height);
}

void
image__newGreyImage( self, width, height, depth )
    struct image *self;
    unsigned int width, height, depth;
{
  image_Type(self) = IGREYSCALE;
  image_newRGBMapData(self, depthToColors(depth));
  image_Width(self) = width;
  image_Height(self) = height;
  image_Depth(self) = depth;
  image_Pixlen(self) = 1; /* in bytes */
  image_Data(self) = (unsigned char *) malloc(width * height);
}

void
image__newRGBImage( self, width, height, depth )
    struct image *self;
    unsigned int width, height, depth;
{ unsigned int pixlen, numcolors;

  pixlen = depth / 8 + (depth %	8 ? 1 :	0); /* in bytes */
  if (pixlen == 0) /* special case for `zero' depth image, which is */
    pixlen = 1;     /* sometimes interpreted as `one color' */
  numcolors = depthToColors(depth);
  image_Type(self) = IRGB;
  image_newRGBMapData(self, numcolors);
  image_Width(self) = width;
  image_Height(self) = height;
  image_Depth(self) = depth;
  image_Pixlen(self) = pixlen;
  image_Data(self) = (unsigned char *) malloc(pixlen * width * height);
}

void
image__newTrueImage( self, width, height )
    struct image *self;
    unsigned int width, height;
{ unsigned int  pixlen, numcolors, a;

  image_Type(self) = ITRUE;
  image_RGBUsed(self) = image_RGBSize(self) = 0;
  image_Width(self) = width;
  image_Height(self) = height;
  image_Depth(self) = 24;
  image_Pixlen(self) = 3;
  image_Data(self) = (unsigned char *) malloc(3 * width * height);
}

void 
image__freeImageData( self )
    struct image *self;
{
  if (!TRUEP(self) && self->rgb) {
    image_freeRGBMapData(self);
  }
  if(image_Data(self))
      free(image_Data(self));
  image_Data(self) = NULL;
}

/* alter an image's brightness by a given percentage
 */

void 
image__Brighten( self, percent )
    struct image *self;
    unsigned int  percent;
{ int          a;
  unsigned int newrgb;
  float        fperc;
  unsigned int size;
  byte        *destptr;

  if (BITMAPP(self)) /* we're AT&T */
    return;

  fperc = (float)percent / 100.0;

  switch (image_Type(self)) {
      case IGREYSCALE:
	  for (a = 0; a < image_RGBUsed(self); a++) {
	      newrgb = image_RedPixel(self, a) * fperc;
	      if (newrgb > 65535)
		  newrgb = 65535;
	      image_RedPixel(self, a) = 
		image_GreenPixel(self, a) = 
		image_BluePixel(self, a) = newrgb;
	  }
	  break;
      case IRGB:
	  for (a = 0; a < image_RGBUsed(self); a++) {
	      newrgb = image_RedPixel(self, a) * fperc;
	      if (newrgb > 65535)
		  newrgb = 65535;
	      image_RedPixel(self, a) = newrgb;
	      newrgb = image_GreenPixel(self, a) * fperc;
	      if (newrgb > 65535)
		  newrgb = 65535;
	      image_GreenPixel(self, a) = newrgb;
	      newrgb = image_BluePixel(self, a) * fperc;
	      if (newrgb > 65535)
		  newrgb = 65535;
	      image_BluePixel(self, a) = newrgb;
	  }
	  break;
      case ITRUE:
	  size = image_Width(self) * image_Height(self) * 3;
	  destptr = image_Data(self);
	  for (a = 0; a < size; a++) {
	      newrgb = *destptr * fperc;
	      if (newrgb > 255)
		  newrgb = 255;
	      *(destptr++) = newrgb;
	  }
	  break;
  } 
}

/*****************************************************************
 * TAG( make_gamma )
 * 
 * Makes a gamma compenstation map.
 * Inputs:
 *  gamma:			desired gamma
 * 	gammamap:		gamma mapping array
 * Outputs:
 *  Changes gamma array entries.
 */
static 
make_gamma( gamma, gammamap )
    double gamma;
    int gammamap[256];
{   register int i;

    for (i = 0; i < 256; i++ ) {
#ifdef BYTEBUG
		int byteb1;
		
		byteb1 = (int)(0.5 + 255 * pow( i / 255.0, 1.0/gamma ));
		gammamap[i] = byteb1;
#else
		gammamap[i] = (int)(0.5 + 255 * pow( i / 255.0, 1.0/gamma ));
#endif
    }
}

void 
image__GammaCorrect( self, disp_gam )
    struct image *self;
    float  disp_gam;
{ int a;
  int gammamap[256];
  unsigned int size;
  byte *destptr;

  if (BITMAPP(self)) /* we're AT&T */
    return;

  make_gamma(disp_gam,gammamap);

  switch (image_Type(self)) {
      case IGREYSCALE:
	  for (a = 0; a < image_RGBUsed(self); a++) {
	      image_RedPixel(self, a) = 
		image_GreenPixel(self, a) = 
		image_BluePixel(self, a) = 
		gammamap[image_RedPixel(self, a) >> 8] << 8;
	  }
	  break;
      case IRGB:
	  for (a = 0; a < image_RGBUsed(self); a++) {
	      image_RedPixel(self, a) = gammamap[image_RedPixel(self,a) >> 8] << 8;
	      image_GreenPixel(self, a) = gammamap[image_GreenPixel(self, a) >> 8] << 8;
	      image_BluePixel(self, a) = gammamap[image_BluePixel(self, a) >> 8] << 8;
	  }
	  break;
      case ITRUE:
	  size = image_Width(self) * image_Height(self) * 3;
	  destptr = image_Data(self);
	  for (a = 0; a < size; a++) {
	      *destptr = gammamap[*destptr];
	      destptr++;
	  }
	  break;
  }
}

/* this initializes a lookup table for doing normalization
 */

static void 
setupNormalizationArray( min, max, array )
    unsigned int min, max;
    byte *array;
{ int a;
  unsigned int new;
  float factor;

  factor = 256.0 / (max - min);
  for (a = min; a <= max; a++) {
    new = (float)(a - min) * factor;
    array[a] = (new > 255 ? 255 : new);
  }
}

/* normalize an image.
 */

struct image *
image__Normalize( self )
    struct image *self;
{ unsigned int  a, x, y;
  unsigned int  min, max;
  Pixel         pixval;
  struct image *newimage;
  byte         *srcptr, *destptr;
  byte          array[256];

  if (BITMAPP(self))
    return(self);

  switch (image_Type(self)) {
      case IGREYSCALE:
      case IRGB:
	  min = 256;
	  max = 0;
	  for (a = 0; a < image_RGBUsed(self); a++) {
	      byte red, green, blue;

	      red = image_RedPixel(self, a) >> 8;
	      green = image_GreenPixel(self, a) >> 8;
	      blue = image_BluePixel(self, a) >> 8;
	      if (red < min)
		  min = red;
	      if (red > max)
		  max = red;
	      if (green < min)
		  min = green;
	      if (green > max)
		  max = green;
	      if (blue < min)
		  min = blue;
	      if (blue > max)
		  max = blue;
	  }
	  setupNormalizationArray(min, max, array);

	  image_newTrueImage(newimage = image_New(), image_Width(self), image_Height(self));
	  newimage->jpegSaveQuality = image_GetJPEGSaveQuality(self);
	  srcptr = image_Data(self);
	  destptr = image_Data(newimage);
	  for (y = 0; y < image_Height(self); y++)
	      for (x = 0; x < image_Width(self); x++) {
		  pixval = memToVal(srcptr, image_Pixlen(self));
		  *destptr = array[image_RedPixel(self, pixval) >> 8];
		  *destptr++;
		  *destptr = array[image_GreenPixel(self, pixval) >> 8];
		  *destptr++;
		  *destptr = array[image_BluePixel(self, pixval) >> 8];
		  *destptr++;
		  srcptr += image_Pixlen(self);
	      }
	  image_Duplicate(newimage, self);
	  image_Destroy(newimage);
	  break;

      case ITRUE:
	  srcptr = image_Data(self);
	  min = 255;
	  max = 0;
	  for	(y = 0; y < image_Height(self); y++)
	      for (x = 0; x < image_Width(self); x++) {
		  if (*srcptr < min)
		      min = *srcptr;
		  if (*srcptr > max)
		      max = *srcptr;
		  srcptr++;
		  if (*srcptr < min)
		      min = *srcptr;
		  if (*srcptr > max)
		      max = *srcptr;
		  srcptr++;
		  if (*srcptr < min)
		      min = *srcptr;
		  if (*srcptr > max)
		      max = *srcptr;
		  srcptr++;
	      }
	  setupNormalizationArray(min, max, array);

	  srcptr = image_Data(self);
	  for (y = 0; y < image_Height(self); y++)
	      for (x = 0; x < image_Width(self); x++) {
		  *srcptr = array[*srcptr];
		  srcptr++;
		  *srcptr = array[*srcptr];
		  srcptr++;
		  *srcptr = array[*srcptr];
		  srcptr++;
	      }
  }
  return(self);
}

/* convert to grayscale
 */

void 
image__Gray( self )
    struct image *self;
{ int a;
  unsigned int size;
  Intensity intensity, red, green, blue;
  byte *destptr;

  if (BITMAPP(self) || GREYSCALEP(self))
    return;

  switch (image_Type(self)) {
      case IRGB:
	  for (a = 0; a < image_RGBUsed(self); a++) {
	      intensity = colorIntensity(image_RedPixel(self, a),
					 image_GreenPixel(self, a),
					 image_BluePixel(self, a));
	      image_RedPixel(self, a) = intensity;
	      image_GreenPixel(self, a) = intensity;
	      image_BluePixel(self, a) = intensity;
	  }
	  break;

      case ITRUE:
	  size = image_Width(self) * image_Height(self);
	  destptr = image_Data(self);
	  for (a = 0; a < size; a++) {
	      red = *destptr << 8;
	      green = *(destptr + 1) << 8;
	      blue = *(destptr + 2) << 8;
	      intensity = colorIntensity(red, green, blue) >> 8;
	      *(destptr++) = intensity; /* red */
	      *(destptr++) = intensity; /* green */
	      *(destptr++) = intensity; /* blue */
	  }
	  break;
  }
}

#define TLA_TO_15BIT(TABLE,PIXEL)           \
  ((((TABLE)->red[PIXEL] & 0xf800) >> 1) |   \
   (((TABLE)->green[PIXEL] & 0xf800) >> 6) | \
   (((TABLE)->blue[PIXEL] & 0xf800) >> 11))

/* this converts a 24-bit true color pixel into a 15-bit true color pixel
 */

#define TRUE_TO_15BIT(PIXEL)     \
  ((((PIXEL) & 0xf80000) >> 9) | \
   (((PIXEL) & 0x00f800) >> 6) | \
   (((PIXEL) & 0x0000f8) >> 3))

/* these macros extract color intensities from a 15-bit true color pixel
 */

#define RED_INTENSITY(P)   (((P) & 0x7c00) >> 10)
#define GREEN_INTENSITY(P) (((P) & 0x03e0) >> 5)
#define BLUE_INTENSITY(P)   ((P) & 0x001f)

#define NIL_PIXEL 0xffffffff

void 
image__Compress( self )
    struct image *self;
{ Pixel         hash_table[32768];
  Pixel        *pixel_table;
  Pixel        *pixel_map;
  Pixel         index, oldpixval, newpixval;
  byte         *pixptr;
  unsigned int  x, y, badcount, dupcount;
  RGBMap       *rgb;

  if (!RGBP(self) || self->rgb->compressed) /* we're AT&T */
    return;

/* initialize hash table and allocate new RGB intensity tables
 */

  for (x = 0; x < 32768; x++)
    hash_table[x] = NIL_PIXEL;
  rgb = (RGBMap*) calloc(1, sizeof(RGBMap));
  newRGBMapData(rgb, image_RGBUsed(self));
  rgb->size = image_RGBUsed(self);
  rgb->used = 0;
  pixel_table = (Pixel *)malloc(sizeof(Pixel) * image_RGBUsed(self));
  pixel_map = (Pixel *)malloc(sizeof(Pixel) * image_RGBUsed(self));
  for (x = 0; x < image_RGBUsed(self); x++)
    pixel_map[x] = NIL_PIXEL;

  pixptr = image_Data(self);
  dupcount= badcount= 0;
  for (y = 0; y < image_Height(self); y++)
    for (x = 0; x < image_Width(self); x++) {
      oldpixval = memToVal(pixptr, image_Pixlen(self));
      if (oldpixval > image_RGBUsed(self)) {
	badcount++;
	oldpixval= 0;
      }

/* if we don't already know what value the new pixel will have,
 * look for a similar pixel in hash table.
 */

      if (pixel_map[oldpixval] == NIL_PIXEL) {
	index = TLA_TO_15BIT(self->rgb, oldpixval);

/* nothing similar
 */

	if (hash_table[index] == NIL_PIXEL) {
	  newpixval = rgb->used++;
	  hash_table[index] = newpixval;
	}

/* we've seen one like this before; try to find out if it's an
 * exact match
 */

	else {
	  newpixval = hash_table[index];
	  for (;;) {

/* if the color is the same as another color we've seen,
 * use the pixel that the other color is using
 */

	    if ((rgb->red[newpixval] == image_RedPixel(self, oldpixval)) &&
		(rgb->green[newpixval] == image_GreenPixel(self, oldpixval)) &&
		(rgb->blue[newpixval] == image_BluePixel(self, oldpixval))) {
	      pixel_map[oldpixval] = newpixval; /* same color */
	      dupcount++;
	      goto move_pixel;
	    }

/* if we're at the end of the chain, we're the first pixel
 * of this color
 */

	    if (pixel_table[newpixval] == NIL_PIXEL) /* end of the chain */
	      break;
	    newpixval = pixel_table[newpixval];
	  }
	  pixel_table[newpixval] = rgb->used;
	  newpixval = rgb->used++;
	}
	pixel_map[oldpixval] = newpixval;
	pixel_table[newpixval] = NIL_PIXEL;
	rgb->red[newpixval] = image_RedPixel(self, oldpixval);
	rgb->green[newpixval] = image_GreenPixel(self, oldpixval);
	rgb->blue[newpixval] = image_BluePixel(self, oldpixval);
      }

/* change the pixel
 */

    move_pixel:
      valToMem(pixel_map[oldpixval], pixptr, image_Pixlen(self));
      pixptr += image_Pixlen(self);
    }
  free(pixel_table);
  free(pixel_map);

/* image is converted; now fix up its colormap
 */

  image_freeRGBMapData(self);
  free(self->rgb);
  self->rgb = rgb;
  self->rgb->compressed = 1;
}

static unsigned int *
buildZoomIndex( width, zoom, rwidth )
    unsigned int  width;
    unsigned int  zoom;
    unsigned int *rwidth;
{ float         fzoom;
  unsigned int *index;
  unsigned int  a;

  if (!zoom) {
    fzoom = 100.0;
    *rwidth = width;
  }
  else {
    fzoom = (float)zoom / 100.0;
    *rwidth = fzoom * width;
  }
  index = (unsigned int *)malloc(sizeof(unsigned int) * *rwidth);
  for (a = 0; a < *rwidth; a++)
    if (zoom)
      *(index + a) = (float)a / fzoom;
    else
      *(index + a) = a;
  return(index);
}

/* Client is responsible for destroying the scaled (zoomed) image */

struct image *
image__Zoom( self, xzoom, yzoom )
    struct image *self;
    unsigned int  xzoom, yzoom;
{ char          buf[BUFSIZ];
  struct image *newimage;
  unsigned int *xindex, *yindex;
  unsigned int  xwidth, ywidth;
  unsigned int  x, y, xsrc, ysrc;
  unsigned int  pixlen;
  unsigned int  srclinelen;
  unsigned int  destlinelen;
  byte         *srcline, *srcptr;
  byte         *destline, *destptr;
  byte          srcmask, destmask, bit;
  Pixel         value;

  if (!xzoom && !yzoom) /* stupid user */
    return(self);

  xindex = buildZoomIndex(image_Width(self), xzoom, &xwidth);
  yindex = buildZoomIndex(image_Height(self), yzoom, &ywidth);

  switch (image_Type(self)) {
      case IBITMAP:
	  newimage = image_New();
	  image_newBitImage(newimage, xwidth, ywidth);
	  for (x = 0; x < image_RGBUsed(self); x++) {
	      image_RedPixel(newimage, x) = image_RedPixel(self, x);
	      image_GreenPixel(newimage, x) = image_GreenPixel(self, x);
	      image_BluePixel(newimage, x) = image_BluePixel(self, x);
	  }
	  image_RGBUsed(newimage) = image_RGBUsed(self);
	  destline = image_Data(newimage);
	  destlinelen = (xwidth / 8) + (xwidth % 8 ? 1 : 0);
	  srcline = image_Data(self);
	  srclinelen = (image_Width(self) / 8) + (image_Width(self) % 8 ? 1 : 0);
	  for (y = 0, ysrc = *(yindex + y); y < ywidth; y++) {
	      while (ysrc != *(yindex + y)) {
		  ysrc++;
		  srcline += srclinelen;
	      }
	      srcptr = srcline;
	      destptr = destline;
	      srcmask = 0x80;
	      destmask = 0x80;
	      bit= srcmask & *srcptr;
	      for (x = 0, xsrc = *(xindex + x); x < xwidth; x++) {
		  if (xsrc != *(xindex + x)) {
		      do {
			  xsrc++;
			  if (!(srcmask >>= 1)) {
			      srcmask = 0x80;
			      srcptr++;
			  }
		      } while (xsrc != *(xindex + x));
		      bit = srcmask & *srcptr;
		  }
		  if (bit)
		      *destptr |= destmask;
		  if (!(destmask >>= 1)) {
		      destmask = 0x80;
		      destptr++;
		  }
	      }
	      destline += destlinelen;
	  }
	  break;
      case IGREYSCALE:
      case IRGB:
	  newimage = image_New();
	  image_newRGBImage(newimage, xwidth, ywidth, image_Depth(self));
	  for (x = 0; x < image_RGBUsed(self); x++) {
	      image_RedPixel(newimage, x) = image_RedPixel(self, x);
	      image_GreenPixel(newimage, x) = image_GreenPixel(self, x);
	      image_BluePixel(newimage, x) = image_BluePixel(self, x);
	  }
	  image_RGBUsed(newimage) = image_RGBUsed(self);
	  /* FALLTHRU */

      case ITRUE:
	  if (!RGBP(newimage)) {
	      newimage = image_New();
	      image_newTrueImage(newimage, xwidth, ywidth);
	  }
	  pixlen = image_Pixlen(self);
	  destptr = image_Data(newimage);
	  srcline = image_Data(self);
	  srclinelen = image_Width(self) * pixlen;
	  for (y = 0, ysrc = *(yindex + y); y < ywidth; y++) {
	      while (ysrc != *(yindex + y)) {
		  ysrc++;
		  srcline += srclinelen;
	      }

	      srcptr = srcline;
	      value = memToVal(srcptr, pixlen);
	      for (x = 0, xsrc = *(xindex + x); x < xwidth; x++) {
		  if (xsrc != *(xindex + x)) {
		      do {
			  xsrc++;
			  srcptr += image_Pixlen(newimage);
		      } while (xsrc != *(xindex + x));
		      value = memToVal(srcptr, pixlen);
		  }
		  valToMem(value, destptr, pixlen);
		  destptr += pixlen;
	      }
	  }
	  break;
  }

  free((byte *)xindex);
  free((byte *)yindex);

  newimage->jpegSaveQuality = image_GetJPEGSaveQuality(self);
  image_SetModified(newimage);
  return(newimage);
}

/* this structure defines a color area which is made up of an array of pixel
 * values and a count of the total number of image pixels represented by
 * the area.  color areas are kept in a list sorted by the number of image
 * pixels they represent.
 */

struct color_area {
    unsigned short    *pixels;       /* array of pixel values in this area */
    unsigned short     num_pixels;   /* size of above array */
    int              (*sort_func)(); /* predicate func to sort with before
				      * splitting */
    unsigned long      pixel_count;  /* # of image pixels we represent */
    struct color_area *prev, *next;
};

/* predicate functions for qsort
 */

static 
sortRGB(p1, p2)
    unsigned short *p1, *p2;
{ unsigned int red1, green1, blue1, red2, green2, blue2;

  red1 = RED_INTENSITY(*p1);
  green1 = GREEN_INTENSITY(*p1);
  blue1 = BLUE_INTENSITY(*p1);
  red2 = RED_INTENSITY(*p2);
  green2 = GREEN_INTENSITY(*p2);
  blue2 = BLUE_INTENSITY(*p2);

  if (red1 == red2)
    if (green1 == green2)
      if (blue1 < blue2)
	return(-1);
      else
	return(1);
    else if (green1 < green2)
      return(-1);
    else
      return(1);
  else if (red1 < red2)
    return(-1);
  else
    return(1);
}

static 
sortRBG(p1, p2)
    unsigned short *p1, *p2;
{ unsigned int red1, green1, blue1, red2, green2, blue2;

  red1 = RED_INTENSITY(*p1);
  green1 = GREEN_INTENSITY(*p1);
  blue1 = BLUE_INTENSITY(*p1);
  red2 = RED_INTENSITY(*p2);
  green2 = GREEN_INTENSITY(*p2);
  blue2 = BLUE_INTENSITY(*p2);

  if (red1 == red2)
    if (blue1 == blue2)
      if (green1 < green2)
	return(-1);
      else
	return(1);
    else if (blue1 < blue2)
      return(-1);
    else
      return(1);
  else if (red1 < red2)
    return(-1);
  else
    return(1);
}

static 
sortGRB(p1, p2)
    unsigned short *p1, *p2;
{ unsigned int red1, green1, blue1, red2, green2, blue2;

  red1 = RED_INTENSITY(*p1);
  green1 = GREEN_INTENSITY(*p1);
  blue1 = BLUE_INTENSITY(*p1);
  red2 = RED_INTENSITY(*p2);
  green2 = GREEN_INTENSITY(*p2);
  blue2 = BLUE_INTENSITY(*p2);

  if (green1 == green2)
    if (red1 == red2)
      if (blue1 < blue2)
	return(-1);
      else
	return(1);
    else if (red1 < red2)
      return(-1);
    else
      return(1);
  else if (green1 < green2)
    return(-1);
  else
    return(1);
}

static 
sortGBR(p1, p2)
    unsigned short *p1, *p2;
{ unsigned int red1, green1, blue1, red2, green2, blue2;

  red1 = RED_INTENSITY(*p1);
  green1 = GREEN_INTENSITY(*p1);
  blue1 = BLUE_INTENSITY(*p1);
  red2 = RED_INTENSITY(*p2);
  green2 = GREEN_INTENSITY(*p2);
  blue2 = BLUE_INTENSITY(*p2);

  if (green1 == green2)
    if (blue1 == blue2)
      if (red1 < red2)
	return(-1);
      else
	return(1);
    else if (blue1 < blue2)
      return(-1);
    else
      return(1);
  else if (green1 < green2)
    return(-1);
  else
    return(1);
}

static 
sortBRG(p1, p2)
    unsigned short *p1, *p2;
{ unsigned int red1, green1, blue1, red2, green2, blue2;

  red1 = RED_INTENSITY(*p1);
  green1 = GREEN_INTENSITY(*p1);
  blue1 = BLUE_INTENSITY(*p1);
  red2 = RED_INTENSITY(*p2);
  green2 = GREEN_INTENSITY(*p2);
  blue2 = BLUE_INTENSITY(*p2);

  if (blue1 == blue2)
    if (red1 == red2)
      if (green1 < green2)
	return(-1);
      else
	return(1);
    else if (red1 < red2)
      return(-1);
    else
      return(1);
  else if (blue1 < blue2)
    return(-1);
  else
    return(1);
}

static 
sortBGR(p1, p2)
    unsigned short *p1, *p2;
{ unsigned int red1, green1, blue1, red2, green2, blue2;

  red1 = RED_INTENSITY(*p1);
  green1 = GREEN_INTENSITY(*p1);
  blue1 = BLUE_INTENSITY(*p1);
  red2 = RED_INTENSITY(*p2);
  green2 = GREEN_INTENSITY(*p2);
  blue2 = BLUE_INTENSITY(*p2);

  if (blue1 == blue2)
    if (green1 == green2)
      if (red1 < red2)
	return(-1);
      else
	return(1);
    else if (green1 < green2)
      return(-1);
    else
      return(1);
  else if (blue1 < blue2)
    return(-1);
  else
    return(1);
}

/* this does calculations on a color area following a split and inserts
 * the color area in the list of color areas.
 */

static 
insertColorArea(pixel_counts, rlargest, rsmallest, area)
    unsigned long *pixel_counts;
    struct color_area **rlargest, **rsmallest, *area;
{ int a;
  unsigned int red, green, blue;
  unsigned int min_red, min_green, min_blue;
  unsigned int max_red, max_green, max_blue= 0;
  struct color_area *largest, *smallest, *tmp_area;

  min_red = min_green = min_blue= 31;
  max_red = max_green = max_blue= 0;

/* update pixel count for this area and find RGB intensity widths
 */

  area->pixel_count = 0;
  for (a = 0; a < area->num_pixels; a++) {
    area->pixel_count += pixel_counts[area->pixels[a]];
    red = RED_INTENSITY(area->pixels[a]);
    green = GREEN_INTENSITY(area->pixels[a]);
    blue = BLUE_INTENSITY(area->pixels[a]);
    if (red < min_red)
      min_red = red;
    if (red > max_red)
      max_red = red;
    if (green < min_green)
      min_green = green;
    if (green > max_green)
      max_green = green;
    if (blue < min_blue)
      min_blue = blue;
    if (blue > max_blue)
      max_blue = blue;
  }

  /* calculate widths and determine which predicate function to use based
   * on the result
   */

  red = max_red - min_red;
  green = max_green - min_green;
  blue = max_blue - min_blue;

  if (red > green)
    if (green > blue)
      area->sort_func = sortRGB;
    else if (red > blue)
      area->sort_func = sortRBG;
    else
      area->sort_func = sortBRG;
  else if (green > blue)
    if (red > blue)
      area->sort_func = sortGRB;
    else
      area->sort_func = sortGBR;
  else
    area->sort_func = sortBGR;

  /* insert color area in color area list sorted by number of pixels that
   * the area represents
   */

  largest = *rlargest;
  smallest = *rsmallest;

  if (!largest) {
    largest = smallest = area;
    area->prev = area->next = (struct color_area *)NULL;
  }

  /* if we only have one element, our pixel count is immaterial so we get
   * stuck on the end of the list.
   */

  else if (area->num_pixels < 2) {
    smallest->next = area;
    area->prev = smallest;
    area->next = (struct color_area *)NULL;
    smallest = area;
  }

  /* insert node into list
   */

  else {
    for (tmp_area = largest; tmp_area; tmp_area = tmp_area->next)
      if ((area->pixel_count > tmp_area->pixel_count) ||
	  (tmp_area->num_pixels < 2)) {
	area->prev = tmp_area->prev;
	area->next = tmp_area;
	tmp_area->prev = area;
	if (area->prev)
	  area->prev->next = area;
	else
	  largest = area;
	break;
      }
    if (!tmp_area) {
      area->prev = smallest;
      area->next = (struct color_area *)NULL;
      smallest->next = area;
      smallest = area;
    }
  }
  *rlargest = largest;
  *rsmallest = smallest;
}

/* Reduce an image to n colors: also 24 --> 8 if necessary */

struct image *
image__Reduce( self, n )
    struct image *self;
    unsigned int n;
{ unsigned long pixel_counts[32768]; /* pixel occurrance histogram */
  unsigned short pixel_array[32768];
  unsigned long count, midpoint;
  int x, y, num_pixels, allocated, depth, ncolors;
  byte *pixel, *dpixel;
  struct color_area *areas, *largest_area, *smallest_area;
  struct color_area *new_area, *old_area;
  struct image *new_image;
  char buf[BUFSIZ];

  if (n > 32768) /* max # of colors we can handle */
    n = 32768;

  /* create a histogram of particular pixel occurrances
   */

  bzero(pixel_counts, 32768 * sizeof(unsigned long));
  switch (image_Type(self)) {
  case IBITMAP:
      return(self);

  case IGREYSCALE:
  case IRGB:
    if (image_RGBUsed(self) <= n)
      return(self);
    pixel = image_Data(self);
    for (y = 0; y < image_Height(self); y++)
      for (x = 0; x < image_Width(self); x++) {
	pixel_counts[TLA_TO_15BIT(self->rgb,
				  memToVal(pixel, image_Pixlen(self)))]++;
	pixel += image_Pixlen(self);
      }
    break;

  case ITRUE:
    if (image_Pixlen(self) != 3) {
      fprintf(stderr, "reduce: true color image has strange pixel length?\n");
      return(self);
    }

    pixel = image_Data(self);
    for (y= 0; y < image_Height(self); y++)
      for (x= 0; x < image_Width(self); x++) {
	pixel_counts[TRUE_TO_15BIT(memToVal(pixel, 3))]++;
	pixel += 3;
      }
    break;

  default:
      return(self); /* not something we can reduce, thank you anyway */
  }

  /* create array of 15-bit pixel values that actually occur in the image
   */

  num_pixels = 0;
  for (x = 0; x < 32768; x++)
    if (pixel_counts[x] > 0)
      pixel_array[num_pixels++] = (short)x;

  /* create color area array and initialize first element
   */

  areas = (struct color_area *)malloc(n * sizeof(struct color_area));
  areas[0].pixels = pixel_array;
  areas[0].num_pixels = num_pixels;
  largest_area = smallest_area = (struct color_area *)NULL;
  insertColorArea(pixel_counts, &largest_area, &smallest_area, areas);
  allocated = 1;

  /* keep splitting the color area until we have as many color areas as we
   * need
   */

  while (allocated < n) {

    /* if our largest area can't be broken down, we can't even get the
     * number of colors they asked us to
     */

    if (largest_area->num_pixels < 2)
      break;

    /* find midpoint of largest area and do split
     */

    qsort(largest_area->pixels, largest_area->num_pixels, sizeof(short),
	  largest_area->sort_func);
    count = 0;
    midpoint = largest_area->pixel_count / 2;
    for (x = 0; x < largest_area->num_pixels; x++) {
      count += pixel_counts[largest_area->pixels[x]];
      if (count > midpoint)
	break;
    }
    if (x == 0) /* degenerate case; divide in half */
      x = 1;
    new_area = areas + allocated;
    new_area->pixels = largest_area->pixels + x;
    new_area->num_pixels = largest_area->num_pixels - x;
    largest_area->num_pixels = x;
    old_area = largest_area;
    largest_area = largest_area->next;
    if (largest_area)
      largest_area->prev = (struct color_area *)NULL;
    else
      smallest_area = (struct color_area *)NULL;

    /* recalculate for each area of split and insert in the area list
     */

    insertColorArea(pixel_counts, &largest_area, &smallest_area, old_area);
    insertColorArea(pixel_counts, &largest_area, &smallest_area, new_area);

    allocated++;
  }

  /* get destination image
   */

  depth = colorsToDepth(allocated);
  new_image = image_New();
  image_newRGBImage(new_image, image_Width(self), image_Height(self), depth);

  /* calculate RGB table from each color area.  this should really calculate
   * a new color by weighting the intensities by the number of pixels, but
   * it's a pain to scale so this just averages all the intensities.  it
   * works pretty well regardless.
   */

  for (x = 0; x < allocated; x++) {
    long red, green, blue, count, pixel;

    red = green = blue = 0;
    count = areas[x].pixel_count;
    for (y = 0; y < areas[x].num_pixels; y++) {
      pixel = areas[x].pixels[y];
      red += RED_INTENSITY(pixel);
      green += GREEN_INTENSITY(pixel);
      blue += BLUE_INTENSITY(pixel);
      pixel_counts[pixel] = x;
    }
    red /= areas[x].num_pixels;
    green /= areas[x].num_pixels;
    blue /= areas[x].num_pixels;
    image_RedPixel(new_image, x) = (unsigned short)(red << 11);
    image_GreenPixel(new_image, x) = (unsigned short)(green << 11);
    image_BluePixel(new_image, x) = (unsigned short)(blue << 11);
  };
  image_RGBUsed(new_image) = allocated;
  new_image->rgb->compressed = 1;

  free(areas);

  /* copy old image into new image
   */

  pixel = image_Data(self);
  dpixel = image_Data(new_image);

  switch(image_Type(self)) {
      case IGREYSCALE:	
      case IRGB:
	  for (y = 0; y < image_Height(self); y++)
	      for (x = 0; x < image_Width(self); x++) {
		  valToMem(pixel_counts[TLA_TO_15BIT(self->rgb,	    memToVal(pixel, image_Pixlen(self)))], dpixel,  image_Pixlen(new_image));
		  pixel += image_Pixlen(self);
		  dpixel += image_Pixlen(new_image);
	      }
	  break;

      case ITRUE:
	  for (y = 0; y < image_Height(self); y++)
	      for (x = 0; x < image_Width(self); x++) {
		  valToMem(pixel_counts[TRUE_TO_15BIT(memToVal(pixel, 3))],
			   dpixel, image_Pixlen(new_image));
		  pixel += 3;
		  dpixel += image_Pixlen(new_image);
	      }
	  break;
  }
  new_image->jpegSaveQuality = image_GetJPEGSaveQuality(self);
  image_Reset(self);
  image_Duplicate(new_image, self);
  image_Destroy(new_image);
  return(self);
}

/* expand an image into a true color image
 */

struct image *
image__Expand( self )
    struct image *self;
{
  struct image *new_image;
  int x, y;
  Pixel spixval;
  byte *spixel, *dpixel, *line;
  unsigned int linelen;
  byte mask;

  if(TRUEP(self))
    return(self);

  new_image = image_New();
  image_newTrueImage(new_image, image_Width(self), image_Height(self));

  switch (image_Type(self)) {
      case IBITMAP:
	  line = image_Data(self);
	  dpixel = image_Data(new_image);
	  linelen = image_Width(self) / 8 + (image_Width(self) % 8 ? 1 : 0);
	  for (y = 0; y < image_Height(self); y++) {
	      spixel = line;
	      mask = 0x80;
	      for (x = 0; x < image_Width(self); x++) {
		  valToMem((mask & *spixel ? 0L : 0xffffff), dpixel, 3);
		  mask >>= 1;
		  if (!mask) {
		      mask = 0x80;
		      spixel++;
		  }
		  dpixel += image_Pixlen(new_image);
	      }
	      line += linelen;
	  }
	  break;
      case IGREYSCALE:
      case IRGB:
	  spixel = image_Data(self);
	  dpixel = image_Data(new_image);
	  for (y = 0; y < image_Height(self); y++)
	      for (x = 0; x < image_Width(self); x++) {
		  spixval= memToVal(spixel, image_Pixlen(self));
		  valToMem(RGB_TO_TRUE(image_RedPixel(self, spixval),
				       image_GreenPixel(self, spixval),
				       image_BluePixel(self, spixval)),
			   dpixel, image_Pixlen(new_image));
		  spixel += image_Pixlen(self);
		  dpixel += image_Pixlen(new_image);
	      }
	  break;
  }
  new_image->jpegSaveQuality = image_GetJPEGSaveQuality(self);
  image_Reset(self);
  image_Duplicate(new_image, self);
  image_Destroy(new_image);
  return(self);
}

struct image *
image__Bit2Grey( self )
  struct image *self;
{
  struct image *new_image;
  int x, y;
  byte *spixel, *dpixel, *line;
  unsigned int linelen;
  byte mask;

  if(TRUEP(self) || RGBP(self) || GREYSCALEP(self))
    return(self);

  new_image = image_New();
  image_newGreyImage(new_image, image_Width(self), image_Height(self), 1);
  image_RedPixel(new_image, 0) = image_GreenPixel(new_image, 0) = image_BluePixel(new_image, 0) = 0;
  image_RedPixel(new_image, 1) = image_GreenPixel(new_image, 1) = image_BluePixel(new_image, 1) = 65535;
  image_RGBUsed(new_image) = 2;

  line = image_Data(self);
  dpixel = image_Data(new_image);
  linelen = (image_Width(self) / 8) + (image_Width(self) % 8 ? 1 : 0);
  for (y = 0; y < image_Height(self); y++) {
      spixel = line;
      mask = 0x80;
      for (x = 0; x < image_Width(self); x++) {
	  valToMem(((mask & *spixel) ? 0L :1L), dpixel, image_Pixlen(new_image));
	  mask >>= 1;
	  if (!mask) {
	      mask = 0x80;
	      spixel++;
	  }
	  dpixel += image_Pixlen(new_image);
      }
      line += linelen;
  }
  new_image->jpegSaveQuality = image_GetJPEGSaveQuality(self);
  image_Reset(self);
  image_Duplicate(new_image, self);
  image_Destroy(new_image);
  return(self);
}

int
image__depthToColors( classID, n )
    struct classheader *classID;
    int n;
{
  return(depthToColors(n));
}

int
image__colorsToDepth( classID, n )
    struct classheader *classID;
    int n;
{
  return(colorsToDepth(n));
}

#define MaxIntensity  65536	/* maximum possible Intensity */
#define MaxGrey       32768	/* limits on the grey levels used */
#define Threshold     16384	/* in the dithering process */
#define MinGrey           0

static unsigned int tone_scale_adjust();
static void LeftToRight();
static void RightToLeft();

/*
 * simple floyd-steinberg dither with serpentine raster processing
 */

struct image *
image__Dither( self )
    struct image *self;
{
  struct image   *image;	/* destination image */
  unsigned int   *grey;		/* grey map for source image */
  unsigned int    spl;		/* source pixel length in bytes */
  unsigned int    dll;		/* destination line length in bytes */
  unsigned char  *src;		/* source data */
  unsigned char  *dst;		/* destination data */
  int            *curr;		/* current line buffer */
  int            *next;		/* next line buffer */
  int            *swap;		/* for swapping line buffers */
  Pixel           color;	/* pixel color */
  unsigned int    level;	/* grey level */
  unsigned int    i, j;		/* loop counters */

  /*
   * check the source image
   */
  if (BITMAPP(self))
    return(self);

  /*
   * allocate destination image
   */
  image = image_New();
  image_newBitImage(image, image_Width(self), image_Height(self));

  /*
   * if the number of entries in the colormap isn't too large, compute
   * the grey level for each entry and store it in grey[]. else the
   * grey levels will be computed on the fly.
   */
  if (RGBP(self) && (image_Depth(self) <= 16)) {
    grey = (unsigned int *)malloc(sizeof(unsigned int) * image_RGBUsed(self));
    for (i = 0; i < image_RGBUsed(self); i++)
      grey[i] =
	(colorIntensity(image_RedPixel(self, i),
			image_GreenPixel(self, i),
			image_BluePixel(self, i)) >> 1);

    for (i = 0; i < image_RGBUsed(self); i++)
      grey[i] = tone_scale_adjust(grey[i]);
  }
  else
  {
    grey = NULL;
  }

  /*
   * dither setup
   */
  spl = image_Pixlen(self);
  dll = (image_Width(image) / 8) + (image_Width(image) % 8 ? 1 : 0);
  src = image_Data(self);
  dst = image_Data(image);

  curr = (int *) malloc(sizeof(int) * (image_Width(self) + 2));
  next = (int *) malloc(sizeof(int) * (image_Width(self) + 2));
  curr += 1;
  next += 1;
  for (j = 0; j < image_Width(self); j++) {
    curr[j] = 0;
    next[j] = 0;
  }

  /*
   * primary dither loop
   */
  for (i = 0; i < image_Height(self); i++) {
    /* copy the row into the current line */
    for (j = 0; j < image_Width(self); j++) {
      color = memToVal(src, spl);
      src += spl;
      
      if (RGBP(self)) {
	if (grey == NULL)
	  level =
	    tone_scale_adjust(colorIntensity(image_RedPixel(self, color),
					     image_GreenPixel(self, color),
					     image_BluePixel(self, color)) >> 1);
	else
	  level = grey[color];
      }
      else {
	level =
	  tone_scale_adjust(colorIntensity((TRUE_RED(color) << 8),
					   (TRUE_GREEN(color) << 8),
					   (TRUE_BLUE(color) << 8)) >> 1);
      }
      curr[j] += level;
    }

    /* dither the current line */
    if (i & 0x01)
      RightToLeft(curr, next, image_Width(self));
    else
      LeftToRight(curr, next, image_Width(self));

    /* copy the dithered line to the destination image */
    for (j = 0; j < image_Width(self); j++)
      if (curr[j] < Threshold)
	dst[j / 8] |= 1 << (7 - (j & 7));
    dst += dll;
    
    /* circulate the line buffers */
    swap = curr;
    curr = next;
    next = swap;
    for (j = 0; j < image_Width(self); j++)
      next[j] = 0;
  }

  /*
   * clean up
   */
  free((byte *)grey);
  free((byte *)(curr-1));
  free((byte *)(next-1));

  image->jpegSaveQuality = image_GetJPEGSaveQuality(self);
  image_Reset(self);
  image_Duplicate(image, self);
  image_Destroy(image);
  return(self);
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
static unsigned int 
tone_scale_adjust(val)
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
static void 
LeftToRight(curr, next, width)
     int *curr;
     int *next;
     int  width;
{
  int idx;
  int error;
  int output;

  for (idx = 0; idx < width; idx++) {
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
static void 
RightToLeft(curr, next, width)
     int *curr;
     int *next;
     int  width;
{
  int idx;
  int error;
  int output;

  for (idx = (width-1); idx >= 0; idx--) {
    output       = (curr[idx] > Threshold) ? MaxGrey : MinGrey;
    error        = curr[idx] - output;
    curr[idx]    = output;
    next[idx+1] += error * 3 / 16;
    next[idx]   += error * 5 / 16;
    next[idx-1] += error * 1 / 16;
    curr[idx-1] += error * 7 / 16;
  }
}

/* simple dithering algorithm, really optimized for the 4x4 array
 */

struct image *
image__Halftone( self )
     struct image *self;
{ struct image  *image;
  unsigned char *sp, *dp, *dp2; /* data pointers */
  unsigned int   dindex;        /* index into dither array */
  unsigned int   spl;           /* source pixel length in bytes */
  unsigned int   dll;           /* destination line length in bytes */
  Pixel          color;         /* pixel color */
  unsigned int  *index;         /* index into dither array for a given pixel */
  unsigned int   a, x, y;       /* random counters */

  if (BITMAPP(self))
    return(self);

  /* set up
   */

  image = image_New();
  image_newBitImage(image, image_Width(self) * 4, image_Height(self) * 4);
  spl = image_Pixlen(self);
  dll = (image_Width(image) / 8) + (image_Width(image) % 8 ? 1 : 0);

  /* if the number of possible pixels isn't very large, build an array
   * which we index by the pixel value to find the dither array index
   * by color brightness.  we do this in advance so we don't have to do
   * it for each pixel.  things will break if a pixel value is greater
   * than (1 << depth), which is bogus anyway.  this calculation is done
   * on a per-pixel basis if the colormap is too big.
   */

  if (RGBP(self) && (image_Depth(self) <= 16)) {
    index = (unsigned int *)malloc(sizeof(unsigned int) * image_RGBUsed(self));
    for (x = 0; x < image_RGBUsed(self); x++) {
      *(index + x) =
	((unsigned long)colorIntensity(image_RedPixel(self, x),
				       image_GreenPixel(self, x),
				       image_BluePixel(self, x))) / GRAYSTEP;
      if (*(index + x) >= GRAYS) /* rounding errors can do this */
	*(index + x) = GRAYS - 1;
    }
  }
  else
    index = NULL;

  /* dither each pixel
   */

  sp = image_Data(self);
  dp = image_Data(image);
  for (y = 0; y < image_Height(self); y++) {
    for (x = 0; x < image_Width(self); x++) {
      dp2 = dp + (x >> 1);
      color = memToVal(sp, spl);
      if (RGBP(self)) {
	if (index)
	  dindex = *(index + color);
	else {
	  dindex = 
	    ((unsigned long)colorIntensity(image_RedPixel(self, color),
					   image_GreenPixel(self, color),
					   image_BluePixel(self, color))) / GRAYSTEP;
	}
      }
      else {
	dindex =
	  ((unsigned long)colorIntensity((TRUE_RED(color) << 8),
					 (TRUE_GREEN(color) << 8),
					 (TRUE_BLUE(color) << 8))) / GRAYSTEP;
      }
      if (dindex >= GRAYS) /* rounding errors can do this */
	dindex = GRAYS - 1;

      /* loop for the four Y bits in the dither pattern, putting all
       * four X bits in at once.  if you think this would be hard to
       * change to be an NxN dithering array, you're right, since we're
       * banking on the fact that we need only shift the mask based on
       * whether x is odd or not.  an 8x8 array wouldn't even need that,
       * but blowing an image up by 64x is probably not a feature.
       */

      if (x & 1)
	for (a = 0; a < 4; a++, dp2 += dll)
	  *dp2 |= DitherBits[dindex][a];
      else
	for (a = 0; a < 4; a++, dp2 += dll)
	  *dp2 |= (DitherBits[dindex][a] << 4);
      sp += spl;
    }
    dp += (dll << 2); /* (dll * 4) but I like shifts */
  }

  image->jpegSaveQuality = image_GetJPEGSaveQuality(self);
  image_Reset(self);
  image_Duplicate(image, self);
  image_Destroy(image);
  return(self);
}

static int tmpfilectr = 0;

long image__WriteOtherFormat(self, file, writeID, level, usagetype, boundary)
struct image *self;
FILE *file;
long writeID;
int level;
int usagetype;
char *boundary;
{
    FILE *tmpfp;
    char Fnam[1000];
    struct gif *gif = gif_New();

    if(self->header.dataobject.writeID == writeID)  return(self->header.dataobject.id);
    self->header.dataobject.writeID = writeID;

    fprintf(file, "\n--%s\nContent-type: image/gif\nContent-Transfer-Encoding: base64\n\n", boundary);
    sprintf(Fnam, "/tmp/imagegif.%d.%d", getpid(), tmpfilectr++);
    if(!(tmpfp = fopen(Fnam, "w"))) {
	gif_Destroy(gif);
	return(0);
    }
    if(image_Data(self)) image_Duplicate(self, (struct image *) gif);
    else {
	gif_newRGBImage(gif, 1, 1, 8);
	gif_RedPixel(gif, 0) = gif_GreenPixel(gif, 0) = gif_BluePixel(gif, 0) = 65535;
	gif_RedPixel(gif, 1) = gif_GreenPixel(gif, 1) = gif_BluePixel(gif, 1) = 0;
	gif_RGBUsed(gif) = 2;
	if(gif_Data(gif)) *gif_Data(gif)=0;
    }
    gif_WriteNative((struct gif *) gif, tmpfp, NULL);
    fclose(tmpfp);
    gif_Destroy(gif);
    if(!(tmpfp = fopen(Fnam, "r"))) {
	unlink(Fnam);
	return(0);
    }
    to64(tmpfp, file);
    fclose(tmpfp);
    unlink(Fnam);
    return(self->header.dataobject.id);
}

boolean
image__ReadOtherFormat(self, file, fmt, encoding, desc)
    struct image *self;
    FILE *file;
    char *fmt;
    char *encoding;
    char *desc;
{
    char TmpFile[250];
    FILE *tmpfp = NULL;
    int code;
    struct image *pix;

    if (strcmp(fmt, "image/gif")
	&& strcmp(fmt, "image/x-gif")
	&& strcmp(fmt, "image/pbm")
	&& strcmp(fmt, "image/pbm")
	&& strcmp(fmt, "image/ppm")
	&& strcmp(fmt, "image/pgm")
	&& strcmp(fmt, "image/jpeg") ) return(FALSE);

    /* Need to decode base64 or q-p here */
    if (!strncmp(encoding, "base64", 6)
	 || !strncmp(encoding, "quoted-printable", 16)) {
	sprintf(TmpFile, "/tmp/imagegif.%d.%d", getpid(), tmpfilectr++);
	tmpfp = fopen(TmpFile, "w");
	if (!tmpfp) return(FALSE);
	if (!strncmp(encoding, "base64", 6)) {
	    from64(file, tmpfp);
	} else {
	    fromqp(file, tmpfp);
	}
	fclose(tmpfp);
	tmpfp = fopen(TmpFile, "r");
	if (!tmpfp) return(FALSE);
	file = tmpfp;
    }

    code = image_Read(self, file, -1);
    if (tmpfp) {
	fclose(tmpfp);
	unlink(TmpFile); 
    }
    if (code == dataobject_NOREADERROR) {
	return(TRUE);
    } else {
	return (FALSE);
    }
}

long
image__WriteNative( self, file, filename )
    struct image *self;
    FILE *file;
    char *filename;
{
    printf("image_WriteNative\n");
}

int
image__Load( image, fullname, fp )
  struct image *image;
  char *fullname;
  FILE *fp;
{
/* This method should be overridden by subclasses of image */
  return(0);
}

void
image__SetSaveFormatString( self, format )
    struct image *self;
    char *format;
{
    if(self->saveformatstring)
	free(self->saveformatstring);
    self->saveformatstring = NULL;
    if(format && *format != (char) 0) {
	char *c;
	self->saveformatstring = (char *) malloc(strlen(format) + 1);
	for( c = format; *c ; c++ )
	    *c = tolower(*c);
	strcpy(self->saveformatstring, format);
    }
}
