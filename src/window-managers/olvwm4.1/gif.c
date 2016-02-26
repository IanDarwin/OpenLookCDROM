/* +-------------------------------------------------------------------+ */
/* | Copyright 1990, David Koblas.                                     | */
/* |   Permission to use, copy, modify, and distribute this software   | */
/* |   and its documentation for any purpose and without fee is hereby | */
/* |   granted, provided that the above copyright notice appear in all | */
/* |   copies and that both that copyright notice and this permission  | */
/* |   notice appear in supporting documentation.  This software is    | */
/* |   provided "as is" without express or implied warranty.           | */
/* +-------------------------------------------------------------------+ */


#include <stdio.h>
#include <X11/X.h>
#include <X11/Xatom.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#include <X11/keysymdef.h>
#include <X11/XWDFile.h>
#include "mem.h"


#define MAXCOLORMAPSIZE         256

#define TRUE    1
#define FALSE   0

#define CM_RED          0
#define CM_GREEN        1
#define CM_BLUE         2

#define MAX_LWZ_BITS            12

#define INTERLACE               0x40
#define LOCALCOLORMAP   0x80
#define BitSet(byte, bit)       (((byte) & (bit)) == (bit))

#define ReadOK(file,buffer,len) (fread(buffer, len, 1, file) != 0)

#define LM_to_uint(a,b)                 (((b)<<8)|(a))

struct {
	unsigned int    Width;
	unsigned int    Height;
	XColor          *ColorMap;
	unsigned int    BitPixel;
	unsigned int    ColorResolution;
	unsigned int    Background;
	unsigned int    AspectRatio;
} GifScreen;

struct {
	int     transparent;
	int     delayTime;
	int     inputFlag;
	int     disposal;
} Gif89 = { -1, -1, -1, 0 };

extern XImage* ReadImage();

XImage *ReadGIF(dpy, fd, pNcolors, pColors)
Display *dpy;
FILE *fd;
int *pNcolors;
XColor **pColors;

{
	XImage *in_image;
	unsigned char   buf[16];
	unsigned char   c;
	int             useGlobalColormap;
	int             bitPixel;
	int             imageCount = 0;
	char            version[4];
	int imageNumber = 1;

	rewind (fd);

	if (! ReadOK(fd,buf,6))
	{
		return NULL;
	}

	if (strncmp(buf,"GIF",3) != 0)
	{
		return NULL;
	}

	strncpy(version, buf + 3, 3);
	version[3] = '\0';

	if ((strcmp(version, "87a") != 0) && (strcmp(version, "89a") != 0))
	{
		return NULL;
	}

	if (! ReadOK(fd,buf,7))
	{
		return NULL;
	}

	GifScreen.Width           = LM_to_uint(buf[0],buf[1]);
	GifScreen.Height          = LM_to_uint(buf[2],buf[3]);
	GifScreen.BitPixel        = 2<<(buf[4]&0x07);
	GifScreen.ColorResolution = (((buf[4]&0x70)>>3)+1);
	GifScreen.Background      = buf[5];
	GifScreen.AspectRatio     = buf[6];

	if (BitSet(buf[4], LOCALCOLORMAP)) {    /* Global Colormap */
		GifScreen.ColorMap  = (XColor*)MemAllocN(GifScreen.BitPixel *
						sizeof (XColor));
		if (ReadColorMap(fd,GifScreen.BitPixel,GifScreen.ColorMap) !=
		1)
		{
			MemFree (GifScreen.ColorMap);
			return NULL;
		}
	}

	for (;;)
	{
		if (! ReadOK(fd,&c,1))
		{
			MemFree (GifScreen.ColorMap);
			return NULL;
		}

		switch (c)
		{
		case ';':               /* GIF terminator */
			if (imageCount < imageNumber)
			{
				MemFree (GifScreen.ColorMap);
				return NULL;
			}
			*pNcolors = GifScreen.BitPixel;
			*pColors = GifScreen.ColorMap;
			return in_image;
			break;

		case '!':               /* Extension */
			if (! ReadOK(fd,&c,1))
			{
				MemFree (GifScreen.ColorMap);
				return NULL;
			}
			DoExtension(fd, c);
			break;

		case ',':               /* a valid start character */
			++imageCount;

			if (! ReadOK(fd,buf,9))
			{
				MemFree (GifScreen.ColorMap);
				return NULL;
			}

			useGlobalColormap = ! BitSet(buf[8], LOCALCOLORMAP);

			bitPixel = 1<<((buf[8]&0x07)+1);

			if (!useGlobalColormap)
			{
				MemFree (GifScreen.ColorMap);
				GifScreen.ColorMap  =
					(XColor*)MemAllocN(GifScreen.BitPixel *
					sizeof (XColor));
				if (ReadColorMap(fd, bitPixel,
				GifScreen.ColorMap) != 1)
				{
					MemFree (GifScreen.ColorMap);
					return NULL;
				}
			}

			in_image = ReadImage(dpy, fd,
			LM_to_uint(buf[4],buf[5]),
				  LM_to_uint(buf[6],buf[7]),
				  GifScreen.ColorMap,
				  BitSet(buf[8], INTERLACE), imageCount !=
				  imageNumber);
			break;

		default:        /* Not a valid start character */
			break;
		}
	}
}

static int
ReadColorMap(fd,number,buffer)
FILE            *fd;
int             number;
XColor          *buffer;
{
	int             i;
	unsigned char   rgb[3];


	for (i = 0; i < number; ++i) {
		if (! ReadOK(fd, rgb, sizeof(rgb)))
			return -1;

		buffer[i].pixel = i;
		buffer[i].red   = rgb[0] << 8;
		buffer[i].green = rgb[1] << 8;
		buffer[i].blue  = rgb[2] << 8;
		buffer[i].flags  = 0;
	}
	return 1;
}

static int
DoExtension(fd, label)
FILE    *fd;
int     label;
{
	static char     buf[256];
	char            *str;

	switch (label) {
	case 0x01:              /* Plain Text Extension */
		str = "Plain Text Extension";
#ifdef notdef
		if (GetDataBlock(fd, (unsigned char*) buf) == 0)
			;

		lpos   = LM_to_uint(buf[0], buf[1]);
		tpos   = LM_to_uint(buf[2], buf[3]);
		width  = LM_to_uint(buf[4], buf[5]);
		height = LM_to_uint(buf[6], buf[7]);
		cellw  = buf[8];
		cellh  = buf[9];
		foreground = buf[10];
		background = buf[11];

		while (GetDataBlock(fd, (unsigned char*) buf) != 0)
		{
			XPutPixel(in_image, xpos, ypos, v);
			++index;
		}

		return 1;
#else
		break;
#endif
	case 0xff:              /* Application Extension */
		str = "Application Extension";
		break;
	case 0xfe:              /* Comment Extension */
		str = "Comment Extension";
		while (GetDataBlock(fd, (unsigned char*) buf) != 0) {
		}
		return 1;
	case 0xf9:              /* Graphic Control Extension */
		str = "Graphic Control Extension";
		(void) GetDataBlock(fd, (unsigned char*) buf);
		Gif89.disposal    = (buf[0] >> 2) & 0x7;
		Gif89.inputFlag   = (buf[0] >> 1) & 0x1;
		Gif89.delayTime   = LM_to_uint(buf[1],buf[2]);
		if ((buf[0] & 0x1) != 0)
			Gif89.transparent = buf[3];

		while (GetDataBlock(fd, (unsigned char*) buf) != 0)
			;
		return 1;
	default:
		str = buf;
		sprintf(buf, "UNKNOWN (0x%02x)", label);
		break;
	}

	while (GetDataBlock(fd, (unsigned char*) buf) != 0)
		;

	return 1;
}

int     ZeroDataBlock = FALSE;

static int
GetDataBlock(fd, buf)
FILE            *fd;
unsigned char   *buf;
{
	unsigned char   count;

	if (! ReadOK(fd,&count,1)) {
		return 0;
	}

	ZeroDataBlock = count == 0;

	if ((count != 0) && (! ReadOK(fd, buf, count))) {
		return 0;
	}

	return count;
}

static int
GetCode(fd, code_size, flag)
FILE    *fd;
int     code_size;
int     flag;
{
	static unsigned char    buf[280];
	static int              curbit, lastbit, done, last_byte;
	int                     i, j, ret;
	unsigned char           count;

	if (flag) {
		curbit = 0;
		lastbit = 0;
		done = FALSE;
		return 0;
	}

	if ( (curbit+code_size) >= lastbit) {
		if (done) {
			if (curbit >= lastbit)
				return -1;
			return 1;
		}
		buf[0] = buf[last_byte-2];
		buf[1] = buf[last_byte-1];

		if ((count = GetDataBlock(fd, &buf[2])) == 0)
			done = TRUE;

		last_byte = 2 + count;
		curbit = (curbit - lastbit) + 16;
		lastbit = (2+count)*8 ;
	}

	ret = 0;
	for (i = curbit, j = 0; j < code_size; ++i, ++j)
		ret |= ((buf[ i / 8 ] & (1 << (i % 8))) != 0) << j;

	curbit += code_size;

	return ret;
}

static int
LWZReadByte(fd, flag, input_code_size)
FILE    *fd;
int     flag;
int     input_code_size;
{
	static int      fresh = FALSE;
	int             code, incode;
	static int      code_size, set_code_size;
	static int      max_code, max_code_size;
	static int      firstcode, oldcode;
	static int      clear_code, end_code;
	static int      table[2][(1<< MAX_LWZ_BITS)];
	static int      stack[(1<<(MAX_LWZ_BITS))*2], *sp;
	register int    i;

	if (flag) {
		set_code_size = input_code_size;
		code_size = set_code_size+1;
		clear_code = 1 << set_code_size ;
		end_code = clear_code + 1;
		max_code_size = 2*clear_code;
		max_code = clear_code+2;

		GetCode(fd, 0, TRUE);

		fresh = TRUE;

		for (i = 0; i < clear_code; ++i) {
			table[0][i] = 0;
			table[1][i] = i;
		}
		for (; i < (1<<MAX_LWZ_BITS); ++i)
			table[0][i] = table[1][0] = 0;

		sp = stack;

		return 0;
	} else if (fresh) {
		fresh = FALSE;
		do {
			firstcode = oldcode =
				GetCode(fd, code_size, FALSE);
		} while (firstcode == clear_code);
		return firstcode;
	}

	if (sp > stack)
		return *--sp;

	while ((code = GetCode(fd, code_size, FALSE)) >= 0) {
		if (code == clear_code) {
			for (i = 0; i < clear_code; ++i) {
				table[0][i] = 0;
				table[1][i] = i;
			}
			for (; i < (1<<MAX_LWZ_BITS); ++i)
				table[0][i] = table[1][i] = 0;
			code_size = set_code_size+1;
			max_code_size = 2*clear_code;
			max_code = clear_code+2;
			sp = stack;
			firstcode = oldcode =
					GetCode(fd, code_size, FALSE);
			return firstcode;
		} else if (code == end_code) {
			int             count;
			unsigned char   buf[260];

			if (ZeroDataBlock)
				return -2;

			while ((count = GetDataBlock(fd, buf)) > 0)
				;

			return -2;
		}

		incode = code;

		if (code >= max_code) {
			*sp++ = firstcode;
			code = oldcode;
		}

		while (code >= clear_code) {
			*sp++ = table[1][code];
			if (code == table[0][code])
				return -1;
			code = table[0][code];
		}

		*sp++ = firstcode = table[1][code];

		if ((code = max_code) <(1<<MAX_LWZ_BITS)) {
			table[0][code] = oldcode;
			table[1][code] = firstcode;
			++max_code;
			if ((max_code >= max_code_size) &&
				(max_code_size < (1<<MAX_LWZ_BITS))) {
				max_code_size *= 2;
				++code_size;
			}
		}

		oldcode = incode;

		if (sp > stack)
			return *--sp;
	}
	return code;
}

static XImage*
ReadImage(dpy, fd, len, height, cmap, interlace, ignore)
Display *dpy;
FILE    *fd;
int     len, height;
XColor  *cmap;
int     interlace, ignore;
{
	unsigned long   swaptest = 1;
	XImage          *in_image;
	int             screen = DefaultScreen(dpy);
	unsigned char   c;
	int             v;
	int             xpos = 0, ypos = 0, pass = 0;

	/*
	**  Initialize the Compression routines
	*/
	if (! ReadOK(fd,&c,1))
		return NULL;

	if (LWZReadByte(fd, TRUE, c) < 0)
		return NULL;

	/*
	**  If this is an "uninteresting picture" ignore it.
	*/
	if (ignore) {
		while (LWZReadByte(fd, FALSE, c) >= 0)
			;
		return NULL;
	}

	/* initialize the input image */
	in_image = XCreateImage (dpy,
			DefaultVisual (dpy, screen),
			8,                      /* Depth */
			ZPixmap,                /* format */
			0,                      /* offset */
			NULL,                   /* data */
			len,                    /* width */
			height,                 /* Height */
			XBitmapPad(dpy),        /* Scan line quantum */
			0                       /* Bytes per line */
		);
	in_image->data = MemAllocN (ImageSize(in_image));

	while ((v = LWZReadByte(fd,FALSE,c)) >= 0 )
	{
		if (*(char*) &swaptest)
			_swaplong ((char*) &v, sizeof(long));
		XPutPixel(in_image, xpos, ypos, v);

		++xpos;
		if (xpos == len) {
			xpos = 0;
			if (interlace) {
				switch (pass) {
				case 0:
				case 1:
					ypos += 8; break;
				case 2:
					ypos += 4; break;
				case 3:
					ypos += 2; break;
				}

				if (ypos >= height) {
					++pass;
					switch (pass) {
					case 1:
						ypos = 4; break;
					case 2:
						ypos = 2; break;
					case 3:
						ypos = 1; break;
					default:
						goto fini;
					}
				}
			} else {
				++ypos;
			}
		}
		if (ypos >= height)
			break;
	}

fini:
	LWZReadByte(fd,FALSE,c);

	return in_image;
}
