/* $Id: b2g.c,v 1.4 1993/08/04 05:16:58 ygz Exp $ */

#ifndef lint
static char *rcs_id="$Id: b2g.c,v 1.4 1993/08/04 05:16:58 ygz Exp $";
#endif /* lint */

#include "io.h"

#define	BtoG_bad1 0xa1
#define	BtoG_bad2 0xf5
#define	GtoB_bad1 0xa1
#define	GtoB_bad2 0xbc

extern unsigned char GtoB[], BtoG[];
extern int GtoB_count, BtoG_count;

#define	c1	(unsigned char)(s[0])
#define	c2	(unsigned char)(s[1])

static void g2b(s)
     register char *s;
{
  register unsigned int i;

	if ((c2 >= 0xa1) && (c2 <= 0xfe)) {
		if ((c1 >= 0xa1) && (c1 <= 0xa9)) {
			i = ((c1 - 0xa1) * 94 + (c2 - 0xa1)) * 2;
			s[0] = GtoB[i++];  s[1] = GtoB[i];
			return;
		} else if ((c1 >= 0xb0) && (c1 <= 0xf7)) {
			i = ((c1 - 0xb0 + 9) * 94 + (c2 - 0xa1)) * 2;
			s[0] = GtoB[i++];  s[1] = GtoB[i];
			return;
		}
	}
	s[0] = GtoB_bad1;  s[1] = GtoB_bad2;
}

static void b2g(s)
     register char *s;
{
  register int i;

	if ((c1 >= 0xa1) && (c1 <= 0xf6)) {
		if ((c2 >= 0x40) && (c2 <= 0x7e)) {
			i = ((c1 - 0xa1) * 157 + (c2 - 0x40)) * 2;
			s[0] = BtoG[i++];  s[1] = BtoG[i];
			return;
		} else if ((c2 >= 0xa1) && (c2 <= 0xfe)) {
			i = ((c1 - 0xa1) * 157 + (c2 - 0xa1) + 63) * 2;
			s[0] = BtoG[i++];  s[1] = BtoG[i];
			return;
		}
	} else if ((c1 == 0xf7) && (c2 >= 0x40) && (c2 <= 0x55)) {
		i = ((c1 - 0xa1) * 157 + (c2 - 0x40)) * 2;
		s[0] = BtoG[i++];  s[1] = BtoG[i];
		return;
	}
	s[0] = BtoG_bad1;  s[1] = BtoG_bad2;
}

#undef c1
#undef c2

extern char *hzconvert ();
static char gb2big_savec[MAX_MODULE];
static char big2gb_savec[MAX_MODULE];

int gb2big_init (arg)
     char *arg;
{
  static int gb2big_inst = 0;

	gb2big_savec[gb2big_inst] = '\0';
	return (gb2big_inst++);
}

int big2gb_init (arg)
     char *arg;
{
  static int big2gb_inst = 0;

	big2gb_savec[big2gb_inst] = '\0';
	return (big2gb_inst++);
}

char *gb2big (s,plen,inst)
     char *s;
     int *plen;
     int inst;
{
	return (hzconvert (s, plen, &gb2big_savec[inst], g2b));
}

char *big2gb (s,plen,inst)
     char *s;
     int *plen;
     int inst;
{
	return (hzconvert (s, plen, &big2gb_savec[inst], b2g));
}

