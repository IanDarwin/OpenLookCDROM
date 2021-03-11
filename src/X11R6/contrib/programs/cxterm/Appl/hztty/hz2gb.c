/* $Id: hz2gb.c,v 1.2 1992/07/31 03:31:40 ygz Exp $ */

#ifndef lint
static char *rcs_id="$Id: hz2gb.c,v 1.2 1992/07/31 03:31:40 ygz Exp $";
#endif /* lint */

#include "io.h"

#define	hzb(c)	((((c) & 0x7f) >= 0x21) && (((c) & 0x7f) <= 0x7e))

#define	S_ASC	0
#define	S_PHZ	1
#define	S_HZ	2
#define	S_HZ2	3
#define	S_HZA	4
#define	S_HZT	5
#define	S_PZW	6
#define	S_ZW	7
#define	S_ZW2	8
#define	S_ZWA	9

static int state[MAX_MODULE];
static int eoln[MAX_MODULE];

static char hzdecode(c, inst)
     char c;
     int inst;	/* instant number */
{
	switch (state[inst]) {
	  case S_ASC:
		switch (c) {
		  case '~':	state[inst] = S_PHZ;  break;
		  case 'z':	state[inst] = (eoln[inst] ? S_PZW : S_ASC);
				break;
		  default:	state[inst] = S_ASC;  break;
		}
		break;

	  case S_PHZ:
		switch (c) {
		  case '{':	state[inst] = S_HZ;   break;
		  default:	state[inst] = S_ASC;  break;
		}
		break;
	  case S_HZ:
		switch (c) {
		  case '~':	state[inst] = S_HZT;   break;
		  case ' ':	state[inst] = S_HZA;   break;
		  case '\b':	state[inst] = S_HZ2;   break;
		  default:	if (hzb(c)) {
					c |= 0x80;  state[inst] = S_HZ2;
				} else  state[inst] = S_ASC;
				break;
		}
		break;
	  case S_HZA:
		state[inst] = S_HZ;
		break;
	  case S_HZ2:
		switch (c) {
		  case ' ':
		  case '\b':	state[inst] = S_HZ;   break;
		  default:	if (hzb(c)) {
					c |= 0x80;  state[inst] = S_HZ;
				} else  state[inst] = S_ASC;
				break;
		}
		break;
	  case S_HZT:
		switch (c) {
		  case '}':	state[inst] = S_ASC;  break;
		  default:	state[inst] = S_HZ;   break;
		}
		break;

	  case S_PZW:
		switch (c) {
		  case 'W':	state[inst] = S_ZW;   break;
		  default:	state[inst] = S_ASC;  break;
		}
		break;
	  case S_ZW:
		switch (c) {
		  case ' ':	state[inst] = S_ZWA;   break;
		  case '\n':	state[inst] = S_ASC;   break;
		  case '\r':	state[inst] = S_ASC;   break;
		  default:	if (hzb(c)) {
					c |= 0x80;  state[inst] = S_ZW2;
				} else  state[inst] = S_ASC;
				break;
		}
		break;
	  case S_ZWA:	state[inst] = S_ZW;  break;
	  case S_ZW2:
		switch (c) {
		  case ' ':
		  case '\b':	state[inst] = S_ZW;   break;
		  default:	if (hzb(c)) {
					c |= 0x80;  state[inst] = S_ZW;
				} else  state[inst] = S_ASC;
				break;
		}
		break;
	}
	return (c);
}



char *hz2gb (s,plen,inst)
     char *s;
     int *plen;
     int inst;
{
  int i;

	for (i = 0; i < *plen; i++) {
		s[i] = hzdecode (s[i], inst);
		eoln[inst] = (s[i] == '\n' || s[i] == '\r') ? 1 : 0;
	}
	return (s);
}

char *gb2hz (s,plen,inst)
     char *s;
     int *plen;
     int inst;
{
  register int i;

	for (i = 0; i < *plen; i++)
		s[i] &= 0x7f;
	return (s);
}

int hz2gb_init (arg)
     char *arg;
{
  static hz2gb_inst = 0;

	eoln[hz2gb_inst] = 0;
	state[hz2gb_inst] = S_ASC;
	return (hz2gb_inst++);
}

int gb2hz_init (arg)
     char *arg;
{
	return (0);
}
