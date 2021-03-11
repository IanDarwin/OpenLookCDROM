/*
 * ctext.c -- Compound Text <-> Japanese Wide Character String converter
 */

/******************************************************************************

・指示 (designation)
		1byte			multi-byte
		94char      96char	94char       96char
	-------------------------------------------------------
	G0  :	ESC ( F	 |  -none-	ESC $ ( F  |  -none-
	G1  :	ESC ) F  |  ESC - F	ESC $ ) F  |  ESC $ - F

・終端文字 F
	1byte
	    94chars
		B	ASCII
		I	JIS KANA
		J	JIS-ROMAN
	    96chars
		A	8859/1 right half
		B	8859/2 right half
		C	8859/3 right half
		D	8859/4 right half
		F	8859/7 right half
		G	8859/6 right half
		H	8859/8 right half
		M	8859/9 (DIS) right half
	multi-byte
	    94chars ^ 2
		A	GB Hanzi
		B	JIS Kanji 1983
		C	KS Hangul/Hanja

-------------------------------------------------------------------------------
COMPOUND_TEXT の仕様 (Comopund Text Encoding Version 1 -- MIT X Consortium Standard)
・G0 G1 のみを使用する。G2 G3 は使用しない。
・G0 が GL、G1 が GR に呼び出されており、それを変更することはできない。
  つまり、Locking Shift および Single Shift は使用しない。
・初期設定として ISO Latin-1 が G0/G1 に指示されている。
・マルチバイトの文字を G0 に指示するのに、ESC-$-F は使用しない。
  ESC-$-(-F を使用する。
・使用できる終端文字は、上に書かれた通り。
・C0 で使用できる文字は、NL TAB ESC のみとする。
・C1 で使用できる文字は CSI のみとする。
・テキストの描画方向のシーケンスが含まれる。
	左から右
	右から左
	元の方向に戻る
******************************************************************************/

/*
 * Copyright (c) 1989  Software Research Associates, Inc.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted, provided
 * that the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Software Research Associates not be
 * used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.  Software Research
 * Associates makes no representations about the suitability of this software
 * for any purpose.  It is provided "as is" without express or implied
 * warranty.
 *
 * Author:  Makoto Ishisone, Software Research Associates, Inc., Japan
 *		ishisone@sra.co.jp
 */

#ifndef lint
static char *rcsid = "$Id: ctext.c,v 2.3 1993/02/02 02:38:41 ishisone Rel $";
#endif

typedef unsigned short	wchar;

#define NULL	0

#define CS96	0x100	/* 96chars CS */
#define MBCS	0x200	/* Multibyte CS */

/* convJWStoCT -- Japanese Wide Character String -> COMPOUND_TEXT */
int
convJWStoCT(wstr, xstr, jisroman)
register wchar *wstr;
register unsigned char *xstr;
int jisroman;	/* true ならば G0 のキャラクタセットとして JIS ROMAN を、
		 * false ならば ASCII を使用する
		 */
/* Wide Character string wstr を COMPOUND_TEXT xstr に変換し、
 * 変換後のバイト数を返す(最後の null byte は含まない)。もし xstr が
 * NULL ならば変換はせず、文字数のみを返す。
 */
{
	register int	c;
	register int	g0, g1;
	register int	n = 0;
	int		g0cs;
	
	g0cs = jisroman ? 'J' : 'B';

	g0 = 'B';
	g1 = CS96|'A';
	
	/*
	 * G0, G1 は次のように使い分ける
	 *  G0: ASCII / JIS-ROMAN
	 *  G1: 漢字 / かな
	 */
	
	while (c = *wstr++) {
		switch (c & 0x8080) {
		case 0:		/* ASCII or C0 or DEL */
			if (c < ' ' || c == 0x7f) {
				/* C0 or DEL */
				if (c == '\t' || c == '\n') {
					if (xstr) *xstr++ = c;
					n++;
				}
				break;
			}
			if (g0 != g0cs) {
				if (xstr) {
					*xstr++ = '\033';
					*xstr++ = '(';
					*xstr++ = g0cs;
				}
				n += 3;
				g0 = g0cs;
			}
			if (xstr) *xstr++ = c & 0x7f;
			n++;
			break;
		case 0x80:	/* かな or C1 */
			if (0x80 <= c && c <= 0x9f) break;
			if (g1 != 'I') {
				if (xstr) {
					*xstr++ = '\033';
					*xstr++ = ')';
					*xstr++ = 'I';
				}
				n += 3;
				g1 = 'I';
			}
			if (xstr) *xstr++ = c & 0xff;
			n++;
			break;
		case 0x8080:	/* 漢字 */
			if (g1 != (MBCS|'B')) {
				if (xstr) {
					*xstr++ = '\033';
					*xstr++ = '$';
					*xstr++ = ')';
					*xstr++ = 'B';
				}
				n += 4;
				g1 = MBCS|'B';
			}
			if (xstr) {
				*xstr++ = (c >> 8) & 0xff;
				*xstr++ = c & 0xff;
			}
			n += 2;
			break;
		default:
			/* 無視する */
			break;
		}
	}
	if (xstr) *xstr = '\0';
	return n;
}

static unsigned char *
getesc(str, len)
unsigned char *str;
int len;
{
	register int	c;

	/* エスケープシーケンスの、エスケープに続く
	 * 中間文字と終端文字を調べる
	 */
	/* 中間文字は 02/00 から 02/15 まで */
	while (len > 0) {
		c = *str;
		if (c < 0x20 || 0x2f < c)
			break;
		len--, str++;
	}
	/* 終端文字は 03/00 から 07/14 まで */
	if (--len < 0 || (c = *str++) < 0x30 || 0x7e < c)
		return (unsigned char *)NULL;

	return str;
}

static unsigned char *
getcsi(str, len)
unsigned char *str;
int len;
{
	register int	c;

	/* CSI シーケンスの、CSI に続く
	 * パラメタ文字・中間文字と終端文字を調べる
	 */
	/* パラメタは 03/00 から 03/15 まで */
	while (len > 0) {
		c = *str;
		if (c < 0x30 || 0x3f < c)
			break;
		len--, str++;
	}
	/* 中間文字は 02/00 から 02/15 まで */
	while (len > 0) {
		c = *str;
		if (c < 0x20 || 0x2f < c)
			break;
		len--, str++;
	}
	/* 終端文字は 04/00 から 07/14 まで */
	if (--len < 0 || (c = *str++) < 0x40 || 0x7e < c)
		return (unsigned char *)NULL;

	return str;
}

/* convCTtoJWS -- COMPOUND_TEXT -> Japanese Wide Character String */
int
convCTtoJWS(xstr, len, wstr)
register unsigned char *xstr;
int len;
wchar *wstr;
/* COMPOUND_TEXT xstr を Wide Character string wstr に変換し、
 * 変換後の文字数を返す(最後の null 文字は含まない)。もし wstr が
 * NULL ならば変換はせず、文字数のみを返す。
 */
{
	register int	c;
	int	nskip;
	int	n = 0;
	int	g0, g1, gs;
	unsigned char	*xstr1;

	/*
	 * Compound Text 中には null octet が含まれる可能性がある
	 * そこで文字列の長さ len を引数で指定できるようにしてあるのだが、
	 * 0 あるいは負の時には (null octet はないものとして) strlen() で
	 * 長さを調べる
	 */
	if (len <= 0) {
		len = strlen((char *)xstr);
	}

	/* 初期状態は、ISO 8859/1 が G0/G1 に入っている */
	g0 = 'B';	/* ASCII -> G0 */
	g1 = CS96|'A';	/* Latin/1 right hand part -> G1 */

	while (len-- > 0) {
		switch (c = *xstr++) {
		case '\n':	/* NEWLINE */
		case '\t':	/* TAB */
			if (wstr) *wstr++ = c;
			n++;
			break;
		case 0x9b:	/* CSI */
			/*
			 * CSI の一般形は
			 *	CSI {P} {I} F
			 * パラメタ P は 03/00 から 03/15、
			 * 中間文字 I は 02/00 から 02/15、
			 * 終端文字 F は 04/00 から 07/14 の範囲
			 */
			/*
			 * 現在定義されているのは directionality だけで、
			 * それは
			 *	CSI-1-]		begin left-to-right text
			 *	CSI-2-]		begin right-to-left text
			 *	CSI-]		end of string
			 * である
			 * がとりあえず今はこれを無視するので、CSI の
			 * シーケンスはすべて無視、ということになる
			 */
			xstr1 = getcsi(xstr, len);
			if (xstr1 == NULL)
				return -1;
			len -= xstr1 - xstr;
			xstr = xstr1;
			break;
		case '\033':	/* ESC */
			/*
			 * エスケープシーケンスの一般形は
			 *	ESC {I} F
			 * 中間文字 I は 02/00 から 02/15 で、
			 * 終端文字 F は 03/00 から 07/14 の範囲
			 */
			/*
			 * 現在定義されているのは、
			 *   スタンダードキャラクタセット
			 *	ESC-(-F
			 *	ESC-$-(-F
			 *	ESC-)-F
			 *	ESC---F
			 *	ESC-$-)-F
			 *   ノンスタンダードキャラクタセット
			 *	ESC-%-/-[0123]
			 * スタンダードなキャラクタセットは正しく解釈
			 * しなくてはならないし、ノンスタンダードなものは
			 * 無視するけれどもデータをスキップする必要がある
			 */
			xstr1 = getesc(xstr, len);
			if (xstr1 == NULL)
				return -1;
			len -= xstr1 - xstr;
			switch (xstr1 - xstr) {
			case 2:		/* ESC - I - F */
				switch (*xstr++) {
				case '(':	/* 94chars CS -> G0 */
					g0 = *xstr;
					break;
				case ')':	/* 94chars CS -> G1 */
					g1 = *xstr;
					break;
				case '-':	/* 96chars CS -> G1 */
					g1 = *xstr | CS96;
					break;
				default:	/* ignore */
					break;
				}
				break;
			case 3:		/* ESC - I - I - F */
				switch (*xstr++) {
				case '$':
					switch (*xstr++) {
					case '(':	/* 94chars MBCS -> G0 */
						g0 = *xstr | MBCS;
						break;
					case ')':	/* 94chars MBCS -> G1 */
						g1 = *xstr | MBCS;
						break;
					case '-':	/* 96chars MBCS -> G1 */
						g1 = *xstr | CS96 | MBCS;
						break;
					default:	/* ignore */
						break;
					}
					break;
				case '%':
					if (*xstr++ != '/') {
						/* unknown sequence */
						break;
					}
					/*
					 * プライベートエンコーディング
					 * 完全に無視する
					 * ただしそのあとに続くデータを
					 * スキップする必要がある
					 *	ESC-%-/-F-M-L
					 */
					len -= 2;
					if (len < 0)
						return -1;
					nskip = (*xstr1 & 0x7f) * 128 +
					    (*(xstr1 + 1) & 0x7f);
					if ((len -= nskip) < 0)
						return -1;
					xstr1 += nskip + 2;
					break;
				default:
					break;
				}
				break;
			default:
				break;
			}
			xstr = xstr1;
			break;
		default:
			if (!(c & 0x60)) {
				/*
				 * NL/TAB/ESC/CSI 以外の C0 or C1
				 * これは明らかにエラー
				 */
				return -1;
			}
			gs = (c & 0x80) ? g1 : g0;
			c &= 0x7f;
			if (gs & MBCS) {
				switch (gs & 0x70) {
				case 0x70:	/* 4byte/char */
					if (--len < 0) return -1;
					c = (c << 8) | (*xstr++ & 0x7f);
				case 0x60:	/* 3byte/char */
					if (--len < 0) return -1;
					c = (c << 8) | (*xstr++ & 0x7f);
				case 0x50:	/* 2byte/char */
				case 0x40:	/* 2byte/char */
					if (--len < 0) return -1;
					c = (c << 8) | (*xstr++ & 0x7f);
					break;
				default:
					return -1;
				}
			}
			if (wstr) {
				switch (gs) {
				case 'B':
				case 'J':
					*wstr++ = c;
					n++;
					break;
				case 'I':
					*wstr++ = 0x80 | c;
					n++;
					break;
				case MBCS|'B':
					*wstr++ = 0x8080 | c;
					n++;
					break;
				}
			} else {
				switch (gs) {
				case 'B':
				case 'J':
				case 'I':
					n++;
					break;
				case MBCS|'B':
					n++;
					break;
				}
			}
			break;
		}
	}
	if (wstr) *wstr = 0;
	return n;
}
