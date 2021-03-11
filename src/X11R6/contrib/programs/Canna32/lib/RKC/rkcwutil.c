/* Copyright 1992 NEC Corporation, Tokyo, Japan.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without
 * fee, provided that the above copyright notice appear in all copies
 * and that both that copyright notice and this permission notice
 * appear in supporting documentation, and that the name of NEC
 * Corporation not be used in advertising or publicity pertaining to
 * distribution of the software without specific, written prior
 * permission.  NEC Corporation makes no representations about the
 * suitability of this software for any purpose.  It is provided "as
 * is" without express or implied warranty.
 *
 * NEC CORPORATION DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN 
 * NO EVENT SHALL NEC CORPORATION BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF 
 * USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR 
 * OTHER TORTUOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR 
 * PERFORMANCE OF THIS SOFTWARE. 
 */

#if !defined(lint) && !defined(__CODECENTER__)
static char rcs_id[] = "$Id: rkcwutil.c,v 4.20 1994/03/09 12:32:42 kon Exp $";
#endif

#include "rkcw.h"
#include "sglobal.h"

#ifdef HAVE_WCHAR_OPERATION
#include <locale.h>
#endif

int
ushort2eucsize(src, srclen)
Ushort *src;
int srclen;
{
  register int i, j;
  register Ushort wc;

  for (i = 0, j = 0 ; i < srclen ; i++) {
    wc = src[i];
    switch (wc & 0x8080) {
    case 0:
      /* ASCII */
      j++;
      break;
    case 0x0080:
      /* 半角カナ */
      j += 2;
      break;
    case 0x8000:
      /* 外字 */
      j += 3;
      break;
    case 0x8080:
      /* 漢字 */
      j += 2;
      break;
    }
  }
  return j;
}

int
ushort2euc(src, srclen, dest, destlen)
Ushort *src;
char *dest;
int srclen, destlen;
{
  register int i, j;
  register Ushort wc;

  for (i = 0, j = 0 ; i < srclen && j + 2 < destlen ; i++) {
    wc = src[i];
    switch (wc & 0x8080) {
    case 0:
      /* ASCII */
      dest[j++] = (unsigned char)((unsigned)wc & 0x7f);
      break;
    case 0x0080:
      /* 半角カナ */
      dest[j++] = 0x8e; /* SS2 */
      dest[j++] = (unsigned char)(((unsigned)wc & 0x7f) | 0x80);
      break;
    case 0x8000:
      /* 外字 */
      dest[j++] = 0x8f; /* SS3 */
      dest[j++] = (unsigned char)((((unsigned)wc & 0x7f00) >> 8) | 0x80);
      dest[j++] = (unsigned char)(((unsigned)wc & 0x7f) | 0x80);
      break;
    case 0x8080:
      /* 漢字 */
      dest[j++] = (unsigned char)((((unsigned)wc & 0x7f00) >> 8) | 0x80);
      dest[j++] = (unsigned char)(((unsigned)wc & 0x7f) | 0x80);
      break;
    }
  }
  dest[j] = (unsigned char)0;
  return j;
}

int
eucchars(src, srclen)
unsigned char *src;
int srclen;
{
  register int i, j;
  register unsigned char ec;

  for (i = 0, j = 0 ; i < srclen ; j++) {
    ec = src[i++];
    if (ec & 0x80) {
      i++;
      if (ec == 0x8f) i++; /* SS3 */
    }
  }
  return j;
}

int
euc2ushort(src, srclen, dest, destlen)
char *src;
Ushort *dest;
int srclen, destlen;
{
  register int i, j;
  register unsigned ec;

  for (i = 0, j = 0 ; i < srclen && j + 1 < destlen ; i++) {
    ec = (unsigned)(unsigned char)src[i];
    if (ec & 0x80) {
      switch (ec) {
      case 0x8e: /* SS2 */
	dest[j++] = (Ushort)(0x80 | ((unsigned)src[++i] & 0x7f));
	break;
      case 0x8f: /* SS3 */
	dest[j++] = (Ushort)(0x8000
			      | (((unsigned)src[i + 1] & 0x7f) << 8)
			      | ((unsigned)src[i + 2] & 0x7f));
	i += 2;
	break;
      default:
	dest[j++] = (Ushort)(0x8080 | (((unsigned)src[i] & 0x7f) << 8)
			      | ((unsigned)src[i + 1] & 0x7f));
	i++;
	break;
      }
    }
    else {
      dest[j++] = (Ushort)ec;
    }
  }
  dest[j] = (wchar_t)0;
  return j;
}

static int
wchar2ushort32(src, srclen, dest, destlen)
register wchar_t *src;
register Ushort *dest;
int srclen, destlen;
{
  register int i;

  for (i = 0 ; i < srclen && i + 1 < destlen ; i++) {
    switch (((unsigned long)*src & 0xf0000000) >> 28) {
    case 0:
      /* ASCII */
      *dest = (Ushort)((unsigned)*src & 0x7f);
      break;
    case 1:
      /* 半角カナ */
      *dest = (Ushort)(0x80 | ((unsigned)*src & 0x7f));
      break;
    case 2:
      /* 外字 */
      *dest = (Ushort)(0x8000
			     | (((unsigned)*src & 0x3f80) << 1)
			     | ((unsigned)*src & 0x7f));
      break;
    case 3:
      /* 漢字 */
      *dest = (Ushort)(0x8080 
			     | (((unsigned)*src & 0x3f80) << 1)
			     | ((unsigned)*src & 0x7f));
      break;
    }
    src++;
    dest++;
  }
  *dest = (Ushort)0;
  return i;
}

static int
ushort2wchar32(src, srclen, dest, destlen)
register Ushort *src;
register wchar_t *dest;
int srclen, destlen;
{
  register int i;

  for (i = 0 ; i < srclen && i + 1 < destlen ; i++) {
    switch (*src & 0x8080) {
    case 0:
      /* ASCII */
      *dest = (wchar_t)(*src & 0x7f);
      break;
    case 0x0080:
      /* 半角カナ */
     * dest = (wchar_t)((0x1 << 28) | (*src & 0x7f));
      break;
    case 0x8000:
      /* 外字 */
      *dest = (wchar_t)((0x2 << 28)
			| (((unsigned long)*src & 0x7f00) >> 1)
			| ((unsigned long)*src & 0x7f));
      break;
    case 0x8080:
      /* 漢字 */
      *dest = (wchar_t)((0x3 << 28)
			| (((unsigned long)*src & 0x7f00) >> 1)
			| ((unsigned long)*src & 0x7f));
      break;
    }
    src++;
    dest++;
  }
  *dest = (wchar_t)0;
  return i;
}

static int
wchar2ushort16(src, srclen, dest, destlen)
wchar_t *src;
Ushort *dest;
int srclen, destlen;
{
  register int i;

  for (i = 0 ; (i < srclen) && ((i + 1) < destlen) ; i++)
      *dest++ = (Ushort)*src++;

  *dest = (Ushort)0;
  return i;
}

static int
ushort2wchar16(src, srclen, dest, destlen)
Ushort *src;
wchar_t *dest;
int srclen, destlen;
{
  register int i;

  for (i = 0 ; (i < srclen) && ((i + 1) < destlen) ; i++)
      *dest++ = (wchar_t)*src++;

  *dest = (wchar_t)0;
  return i;
}

/*
 * ワイドキャラクタオペレーション
 *
 */

static BYTE wchar_type; /* ワイドキャラクタのタイプ(下を見よ) */

#define CANNA_WCTYPE_16 0  /* 16ビット表現 */
#define CANNA_WCTYPE_32 1  /* 32ビット表現 */
#define CANNA_WCTYPE_OT 99 /* その他の表現 */

/*
 rkcWCinit() -- ワイドキャラクタとしてどれが使われているかを確認する

        この関数が呼び出されるまえに setlocale がなされていなければならない
 */

#define TYPE16A 0x0000a4a2L
#define TYPE32A 0x30001222L

int
rkcWCinit()
{
#ifdef HAVE_WCHAR_OPERATION
  unsigned char *a = (unsigned char *)"あ"; /* 0xa4a2 */
  wchar_t wc[24];
  unsigned long	l;

  if (mbstowcs(wc, a, sizeof(wc) / sizeof(wchar_t)) != 1) {
    /* 多分 setlocale がなされていない */
    setlocale(LC_CTYPE, "");
    if (mbstowcs(wc, a, sizeof(wc) / sizeof(wchar_t)) != 1) {
      setlocale(LC_CTYPE, JAPANESE_LOCALE);
      if (mbstowcs(wc, a, sizeof(wc) / sizeof(wchar_t)) != 1) {
	return -1;
      }
    }
  }
  l = (unsigned long)wc[0];
  if (l == TYPE16A) {
    wchar_type = CANNA_WCTYPE_16;
  }
#ifndef WCHAR16
  else if (l == TYPE32A) {
    wchar_type = CANNA_WCTYPE_32;
  }
#endif
  else {
    wchar_type = CANNA_WCTYPE_OT;
  }
#else /* !HAVE_WCHAR_OPERATION */

#ifdef	WCHAR16
    wchar_type = CANNA_WCTYPE_16;
#else /* WCHAR16 */
  if (sizeof(wchar_t) == 2)
    wchar_type = CANNA_WCTYPE_16;
  else
    wchar_type = CANNA_WCTYPE_32;
#endif /* WCHAR16 */

#endif /* !HAVE_WCHAR_OPERATION */

  return 0;
}

int
wchar2ushort(src, slen, dst, dlen)
wchar_t *src;
Ushort *dst;
int slen, dlen;
{
    if( wchar_type == CANNA_WCTYPE_16 )
	return( wchar2ushort16( src, slen, dst, dlen ) );
    else
	return( wchar2ushort32( src, slen, dst, dlen ) );
}

int
ushort2wchar(src, slen, dst, dlen)
Ushort *src;
wchar_t *dst;
int slen, dlen;
{
    if( wchar_type == CANNA_WCTYPE_16)
	return( ushort2wchar16( src, slen, dst, dlen ) );
    else
	return( ushort2wchar32( src, slen, dst, dlen ) );
}

int
wcharstrlen(ws)
wchar_t *ws;
{
  register wchar_t *p = ws;
  while (*p)
    p++;
  return p - ws;
}

int
ushortstrlen(ws)
Ushort *ws;
{
  register Ushort *p = ws;
  while (*p)
    p++;
  return p - ws;
}

int
ushortstrcpy(wd, ws)
Ushort *wd, *ws;
{
  register int res = 0;
  while ((*wd++ = *ws++) != (Ushort)0) {
    res++;
  }
  return res;
}

int
ushortstrncpy(wd, ws, n)
Ushort *wd, *ws;
int n;
{
  register int res = 0;
  while (n > res && (*wd = *ws) != (Ushort)0) {
    wd++; ws++; res++;
  }
  *wd = 0;
  return res;
}

