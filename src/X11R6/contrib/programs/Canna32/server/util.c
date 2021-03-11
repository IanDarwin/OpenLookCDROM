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
static char rcs_id[] = "$Id: util.c,v 4.15 1994/04/12 12:59:09 kon Exp $";
#endif

#include "widedef.h"
#include "IR.h"

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
      dest[j++] = (char)((unsigned)wc & 0x7f);
      break;
    case 0x0080:
      /* 半角カナ */
      dest[j++] = 0x8e; /* SS2 */
      dest[j++] = (char)(((unsigned)wc & 0x7f) | 0x80);
      break;
    case 0x8000:
      /* 外字 */
      dest[j++] = 0x8f; /* SS3 */
      dest[j++] = (char)((((unsigned)wc & 0x7f00) >> 8) | 0x80);
      dest[j++] = (char)(((unsigned)wc & 0x7f) | 0x80);
      break;
    case 0x8080:
      /* 漢字 */
      dest[j++] = (char)((((unsigned)wc & 0x7f00) >> 8) | 0x80);
      dest[j++] = (char)(((unsigned)wc & 0x7f) | 0x80);
      break;
    }
  }
  dest[j] = '\0';
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
      case 0xef: /* SS3 */
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

int
wchar2ushort32(src, srclen, dest, destlen)
register wchar_t *src;
register Ushort *dest;
int srclen, destlen;
{
  register int i;

  for (i = 0 ; i < srclen && i + 1 < destlen ; i++) {
    switch ((unsigned)(*src & 0xf0000000) >> 28) {
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

int
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
			| ((unsigned)(*src & 0x7f00) >> 1) | (*src & 0x7f));
      break;
    case 0x8080:
      /* 漢字 */
      *dest = (wchar_t)((0x3 << 28)
			| ((unsigned)(*src & 0x7f00) >> 1) | (*src & 0x7f));
      break;
    }
    src++;
    dest++;
  }
  *dest = (wchar_t)0;
  return i;
}

int
wchar2ushort16(src, srclen, dest, destlen)
register wchar_t *src;
register Ushort *dest;
int srclen, destlen;
{
  register int i;

  for (i = 0 ; i < srclen && i + 1 < destlen ; i++)
      *dest++ = (Ushort)*src++;

  *dest = (Ushort)0;
  return i;
}

int
ushort2wchar16(src, srclen, dest, destlen)
register Ushort *src;
register wchar_t *dest;
int srclen, destlen;
{
  register int i;

  for (i = 0 ; i < srclen && i + 1 < destlen ; i++)
      *dest++ = (wchar_t)*src++;

  *dest = (wchar_t)0;
  return i;
}

int
ushortstrlen(ws)
Ushort *ws;
{
  int res = 0;
  while (*ws++) {
    res++;
  }
  return res;
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

  while (res < n && (*wd = *ws) != (Ushort)0) {
    wd++; ws++; res++;
  }
  *wd = 0;
  return res;
}

/*
  WidenClientContext

  クライアント構造体で管理しているコンテキスト配列をちょっと大きくする。

  cl -- クライアント構造体へのポインタ
  n  -- どのくらい大きくするか

  返り値
   1  -- 成功
   0  -- 失敗

 */

WidenClientContext(cl, n)
ClientPtr cl;
int n;
{
  int *new, *old, i;

  new = (int *)malloc((cl->cfsize + n) * sizeof(int));
  if (new) {
    old = cl->context_flag;
    for (i = 0 ; i < cl->ncon ; i++) {
      new[i] = old[i];
    }
    if (cl->ncon > 0)
      free((char *)old);
    cl->context_flag = new;
    cl->cfsize += n;
    return 1;
  }
  return 0;
}

/*
  set_cxt -- そのクライアントが指定されたコンテキストを使っていることを記録

  返り値:
    1 -- 成功
    0 -- 失敗
 */

int
set_cxt(cl, n)
ClientPtr cl;
int n;
{
  if (!(cl->ncon < cl->cfsize) && !WidenClientContext(cl, N_ADD_CONTEXTS)) {
    return 0;
  }
  else {
    cl->context_flag[cl->ncon++] = n;
    return 1;
  }
}

/*
  off_cxt -- そのクライアントで、指定されたコンテキストをもう使わなくなった
 */

void
off_cxt(cl, cn)
ClientPtr cl;
int cn;
{
  int i, n = cl->ncon, *contexts = cl->context_flag;

  for (i = 0 ; i < n ; i++) {
    if (contexts[i] == cn) {
      break;
    }
  }
  n--;
  for (; i < n ; i++) {
    contexts[i] = contexts[i + 1];
  }
  cl->ncon = n;
}

/*
  chk_cxt -- そのクライアントで、指定されたコンテキストを使ってもいいの？

   1 -- 使ってもいい
   0 -- いけない
 */

int
chk_cxt(cl, cn)
ClientPtr cl;
int cn;
{
  int i, n = cl->ncon, *contexts = cl->context_flag;
  
  for (i = 0 ; i < n ; i++) {
    if (contexts[i] == cn) {
      return 1;
    }   
  }
  return 0;
}
