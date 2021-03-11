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
#ifndef CODEC_TABLES_H
#define CODEC_TABLES_H
typedef struct {
	float	      g;
	unsigned char lsb;	
}ATG;
typedef struct {
	float	      g;
	unsigned char lsb;	
	unsigned char msb;	
}GER;
typedef struct {
	float	      g;
	unsigned char lsb;	
	unsigned char msb;	
}GR;
typedef struct {
	float	      g;
	unsigned char lsb;	
	unsigned char msb;	
}GX;
typedef struct {
	float	      g;
	unsigned char lsb;	
	unsigned char msb;	
}STG;
typedef struct {
	float	      f;
	unsigned char lsb;	
}STRF;
typedef struct {
	float	      g;
	unsigned char lsb;	
}STRG;

extern ATG atg[];
extern GER ger[];
extern GR gr[];
extern GX gx[];
extern STG stg[];
extern STRF strf[];
extern STRG strg[];

extern int atg_size;
extern int ger_size;
extern int gr_size;
extern int gx_size;
extern int stg_size;
extern int strf_size;
extern int strg_size;

#endif	/* CODEC_TABLES_H */
