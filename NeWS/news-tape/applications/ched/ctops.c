/*
 * This file is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify this file without charge, but are not authorized to
 * license or distribute it to anyone else except as part of a product
 * or program developed by the user.
 * 
 * THIS FILE IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 * 
 * This file is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 * 
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS FILE
 * OR ANY PART THEREOF.
 * 
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even
 * if Sun has been advised of the possibility of such damages.
 * 
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */

/*
 * This file is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify this file without charge, but are not authorized to
 * license or distribute it to anyone else except as part of a product
 * or program developed by the user.
 * 
 * THIS FILE IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 * 
 * This file is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 * 
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS FILE
 * OR ANY PART THEREOF.
 * 
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even
 * if Sun has been advised of the possibility of such damages.
 * 
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 */

#ifndef lint
static char sccsid[] = "%Z%%M% %I% %E% Copyright 1987 Sun Micro";
#endif

/*
 * Copyright (c) 1987 by Sun Microsystems, Inc.
 */
/*
 * A client-side library for communicating with postscript.
 * 
 */

#include <sys/ioctl.h>

#ifdef SYSVREF
#include <fcntl.h>
#include <tiuser.h>
#include <stropts.h>
#ifdef INTERLANTCP
#include <interlan/il_types.h>
#include <interlan/socket.h>
#include <interlan/in.h>
#include <interlan/if.h>
#else	/* INTERLANTCP */
#include <sys/types.h>
#endif	/* INTERLANTCP */
#else	/* SYSVREF */
#include <sys/types.h>
#endif	/* SYSVREF */

#ifdef REF
#include <ref/config.h>
#endif

#ifndef SYSVREF
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <netinet/in.h>
#include <net/if.h>
#endif /* SYSVREF */

#include <ctype.h>
#include <varargs.h>
#include "encoding.h"
#include "psio.h"

struct	ieee_double {
#if defined(mc68000) || defined(sparc)
	unsigned int   sign:1;
	unsigned int   exp:11;
	unsigned int   mant1:20;
	unsigned int   mant2;
#endif
#ifdef i386
	unsigned int   mant2;
	unsigned int   mant1:20;
	unsigned int   exp:11;
	unsigned int   sign:1;
#endif
#if defined(vax)
	unsigned int	mant1:20;
	unsigned int	exp:11;
	unsigned int	sign:1;
	unsigned int	mant2;
#endif
};

#if defined(vax)
struct	ieee_single {
	unsigned int	mant:23;
	unsigned int	exp:8;
	unsigned int	sign:1;
};
#endif


PSFILE     *PostScript,
           *PostScriptInput;
int	    ps_next_user_token;

PSFILE       *
ps_open_PostScript()
{
#ifndef SYSVREF
    if (PostScript == 0) {
	char       *server;
	register char *p;
	int         port;
	int         fd;
	char *semi;
	char *dot;
	register struct hostent *hp;
	struct sockaddr_in sin;
	server = (char *) getenv("NEWSSERVER");
	fd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
	if (fd < 0)
	    return 0;
	if (server) {
	    extern char *rindex();
	    register char *t;
	    if (semi = rindex(server, ';'))
		*semi = 0;
	    if (dot = rindex(server, '.')) {
		*dot = 0;
		sin.sin_port = htons(atoi(dot + 1));
	    }
	    sin.sin_addr.s_addr = inet_addr(server);
	    if (dot) *dot = '.';
	    if (semi) *semi = ';';
	}
	else {

#ifdef USEIFCONFIG
	    struct ifreq ifr[10];
	    struct ifconf ifc;
	    ifc.ifc_len = sizeof(ifr);
	    ifc.ifc_req = ifr;
	    if (ioctl(fd, SIOCGIFCONF, &ifc) < 0
		    || ifc.ifc_len < sizeof(struct ifreq)
		    || ioctl(fd, SIOCGIFADDR, ifr) < 0)
		return 0;
	    sin = *((struct sockaddr_in *) (&ifr[0].ifr_addr));
#else

#ifdef IN_SET_LOOPBACK_ADDR
	    IN_SET_LOOPBACK_ADDR(&sin);
#else

#ifdef INADDR_LOOPBACK
	    sin.sin_addr.s_addr = INADDR_LOOPBACK;
#else
	    sin.sin_addr.s_addr = htonl((127 << 24) + 1);
#endif

#endif

#endif

	    sin.sin_port = htons(2000);
	}
	sin.sin_family = AF_INET;
	if (connect(fd, &sin, sizeof sin) < 0) {
	    close(fd);
	    return 0;
	}
	PostScript = psio_fdopen(fd, "w");
    }
#else	/* SYSVREF */
    if (PostScript == 0) {
        char       *server;
        struct t_call *sndcall;
        int         fd;
        char *semi;
        char *dot;
        struct sockaddr_in sin;

        if ((fd = t_open("/dev/it", O_RDWR, NULL)) < 0) {
            return(0);
        }

        if (t_bind(fd, NULL, NULL) < 0) {
	    t_close(fd);
	    return(0);
        }

        if ((sndcall = (struct t_call *)t_alloc(fd, T_CALL, T_ADDR)) ==
NULL) { 
	    t_close(fd);
	    return(0);
        }

    	server = (char *) getenv("NEWSSERVER");
        if (server) {
            register char *t;
            if (semi = rindex(server, ';'))
                *semi = 0;
            if (dot = rindex(server, '.')) {
                *dot = 0;
                sin.sin_port = htons((short) atoi(dot + 1));
            }
            sin.sin_addr.s_addr = inet_addr(server);
            if (dot) *dot = '.';
            if (semi) *semi = ';';
        }
        else {

#ifdef USEIFCONFIG
            struct ifreq ifr[10];
            struct ifconf ifc;
            ifc.ifc_len = sizeof(ifr);
            ifc.ifc_req = ifr;
            if (ioctl(fd, SIOCGIFCONF, &ifc) < 0
                    || ifc.ifc_len < sizeof(struct ifreq)
                    || ioctl(fd, SIOCGIFADDR, ifr) < 0)
                return 0;
            sin = *((struct sockaddr_in *) (&ifr[0].ifr_addr));
#else

#ifdef IN_SET_LOOPBACK_ADDR
            IN_SET_LOOPBACK_ADDR(&sin.sin_addr);
#else   

#ifdef INADDR_LOOPBACK
            sin.sin_addr.s_addr = INADDR_LOOPBACK;
#else   
            sin.sin_addr.s_addr = htonl((127 << 24) + 1);
#endif  

#endif   

#endif   

            sin.sin_port = htons(2000);
        }
        sin.sin_family = htons(AF_INET);

        sndcall->addr.len = 8;
        *(short *)sndcall->addr.buf = sin.sin_family;
        *(short *)(sndcall->addr.buf + sizeof(short)) = sin.sin_port;
        *(int *)(sndcall->addr.buf + sizeof(int)) = sin.sin_addr.s_addr;

	if (t_connect(fd, sndcall, NULL) < 0) {
	    t_close(fd);
 	    return(0);
        }

        if (ioctl(fd, I_PUSH, "tirdwr") < 0) {
	    t_close(fd);
	    return(0);
        }

	PostScript = (PSFILE *)psio_fdopen(fd, "w");
    }
#endif	/* SYSVREF */

    if (PostScript == 0)
	return 0;
    {
	register struct psiobuf *ret = (struct psiobuf *) malloc(sizeof(struct psiobuf));
	ret->file = psio_fileno(PostScript);
	ret->cnt = 0;
	ret->bufsiz = 0;
	ret->base = 0;
	ret->ptr = 0;
	ret->flag = PSREAD;
	ret->protected = 0;
	ret->outputside = PostScript;
	PostScriptInput = (PSFILE *)ret;
	return (PSFILE *)ret;
    }
}

pprintf(f, fmt, fmtlen, va_alist /* , arg2 ... */ )
    register PSFILE *f;
    register char *fmt;
va_dcl
{
    register va_list arglist;
    register char *limit = fmt + fmtlen;
    va_start(arglist);

    if (!f) {
	fprintf(stderr,
	     "Error: CPS has not initialized or server connection failed\n");
	exit(1);
    }

    while (fmt < limit)
	if (*fmt != '%') {
	    psio_putc(*fmt++, f);
	}
	else {
	    register int width = -1;
    continue_parse:		/* I really hate labels, but C doesnt have a
				 * multi-level break or continue */
	    switch (*++fmt) {
	    case '*':
		width = va_arg(arglist, int);
		goto continue_parse;
	    case '0':
	    case '1':
	    case '2':
	    case '3':
	    case '4':
	    case '5':
	    case '6':
	    case '7':
	    case '8':
	    case '9':
		if (width < 0)
		    width = *fmt - '0';
		else
		    width = width * 10 + *fmt - '0';
		goto continue_parse;
	    case 's':
		{
		    register char *string = va_arg(arglist, char *);
		    if (width < 0)
			width = strlen(string);
		    if (width < 16)
			psio_putc(width + enc_short_string, f);
		    else {
			if (width < 256)
			    psio_putc(enc_string, f);
			else {
			    if (width < (1 << 15))
				psio_putc(enc_string + 1, f);
			    else {
				psio_putc(enc_string + 2, f);
				psio_putc(width >> 24, f);
				psio_putc(width >> 16, f);
			    }
			    psio_putc(width >> 8, f);
			}
			psio_putc(width, f);
		    }
		    while (--width >= 0)
			psio_putc(*string++, f);
		}
		break;
	    case 'p':
		{
		    register char *string = va_arg(arglist, char *);
		    if (width < 0)
			width = strlen(string);
		    while (--width >= 0)
			psio_putc(*string++, f);
		}
		break;
	    case 'd':
		{
		    register   *np;
		    register    n,
		                an,
		                cnt;
		    if (width < 0) {
			n = va_arg(arglist, int);
			cnt = 0;
		    }
		    else {
			if ((cnt = width) <= 0)
			    break;
			np = va_arg(arglist, int *);
			n = *np++;
		    }
		    while (1) {
			an = n;
			if (an < 0)
			    an = -an;
			if (an < 128)
			    psio_putc(enc_int, f);
			else {
			    if (an < (1 << 15))
				psio_putc(enc_int + 1, f);
			    else {
				if (an < (1 << 23))
				    psio_putc(enc_int + 2, f);
				else {
				    psio_putc(enc_int + 3, f);
				    psio_putc(n >> 24, f);
				}
				psio_putc(n >> 16, f);
			    }
			    psio_putc(n >> 8, f);
			}
			psio_putc(n, f);
			if (--cnt <= 0)
			    break;
			n = *np++;
		    }
		}
		break;
	    case 'e':
		{
		    register   *np;
		    register    n,
		                cnt;
		    if ((cnt = width) < 0)
			n = va_arg(arglist, int);
		    else {
			np = va_arg(arglist, int *);
			n = *np++;
		    }
		    while (1) {
			/* Assumes fract with two bytes after binary point */
			register    fractbytes = 2 << 2;
			register    an = n < 0 ? -n : n;
			if ((n & 0377) == 0)
			    n >>= 8, fractbytes -= 1 << 2;
			if ((n & 0377) == 0)
			    n >>= 8, fractbytes -= 1 << 2;
			if (an < 128) {
			    psio_putc(fractbytes + enc_int, f);
			}
			else {
			    if (an < (1 << 15)) {
				psio_putc(fractbytes + enc_int + 1, f);
			    }
			    else {
				if (an < (1 << 23)) {
				    psio_putc(fractbytes + enc_int + 2, f);
				}
				else {
				    psio_putc(fractbytes + enc_int + 3, f);
				    psio_putc(n >> 24, f);
				}
				psio_putc(n >> 16, f);
			    }
			    psio_putc(n >> 8, f);
			}
			psio_putc(n, f);
			if (--cnt <= 0)
			    break;
			n = *np++;
		    }
		}
		break;
	    case 'f':
	    case 'F':
		{
		    register union double_t {
			struct ieee_double	u;
			double	 		d;
		    }          *dp;
		    register    cnt;
#if defined(vax)
		    double	save_d;
#endif

		    if ((cnt = width) < 0)
			dp = &va_arg(arglist, union double_t);
		    else
			dp = va_arg(arglist, union double_t *);
#if defined(vax)
		    /* save the double for the else clause so we can let the
		     * native compiler do the conversion to float
		     */
		    save_d = dp->d;
		    /* the first dp is used as a double,
		     * the second as a struct ieee_double return value.
		     * Note This function is not very safe...
		     */
		    vax_ieee_double(&(dp->d), &(dp->u));
#endif
		    while (1) {
			register    exp = dp->u.exp;
			if (1022 - 4 <= exp && exp <= 1022 + 15) {
			    register    n;
			    register    fractbytes = 2 << 2;
			    register    an;
			    n = ((1 << 31) | (dp->u.mant1 << 11) | (dp->u.mant2 >> 21)) >> (1022 + 16 - dp->u.exp);
			    if ((n & 0377) == 0)
				n >>= 8, fractbytes -= 1 << 2;
			    if ((n & 0377) == 0)
				n >>= 8, fractbytes -= 1 << 2;
			    an = n;
			    if (dp->u.sign)
				n = -n;
			    if (an < 128)
				psio_putc(fractbytes + enc_int, f);
			    else {
				if (an < (1 << 15))
				    psio_putc(fractbytes + enc_int + 1, f);
				else {
				    if (an < (1 << 23))
					psio_putc(fractbytes + enc_int + 2, f);
				    else {
					psio_putc(fractbytes + enc_int + 3, f);
					psio_putc(n >> 24, f);
				    }
				    psio_putc(n >> 16, f);
				}
				psio_putc(n >> 8, f);
			    }
			    psio_putc(n, f);
			}
			else {
			    union {
				float			f;
				unsigned char		b[4];
#if defined(vax)
				struct ieee_single	ieee_f;
#endif
			    }           u;
			    psio_putc(enc_IEEEfloat, f);

#if defined(mc68000) || defined(sparc) || defined(i386)
			    u.f = dp++->d;
#endif
#if defined(vax)
			    /* We want the native compiler to do the
			     * conversion from double to float.
			     */
			    u.f = save_d;
			    dp++;
		    	    /* the first u is used as a float,
		     	     * the second as a struct ieee_single return value.
		     	     * Note This function is not very safe...
		     	     */
			    vax_ieee_float(&u.f, &u.ieee_f);
#endif

#if defined(mc68000) || defined(sparc)
			    psio_putc(u.b[0], f);
			    psio_putc(u.b[1], f);
			    psio_putc(u.b[2], f);
			    psio_putc(u.b[3], f);
#endif
#if defined(vax) || defined(i386)
			    psio_putc(u.b[3], f);
			    psio_putc(u.b[2], f);
			    psio_putc(u.b[1], f);
			    psio_putc(u.b[0], f);
#endif
			}
			if (--cnt <= 0)
			    break;
			dp++;
		    }
		}
		break;
	    case 'u':{
		    register    userindex = va_arg(arglist, int);
		    if (userindex < 32)
			psio_putc(enc_usercommon + userindex, f);
		    else {
			psio_putc(enc_lusercommon + ((userindex >> 8) & 3), f);
			psio_putc(userindex, f);
		    }
		}
		break;
	    case '%':
		psio_putc(*fmt, f);
		break;
	    default:
		fprintf(stderr,
			"Warning: CPS currently does not implement this format: %%%c\n", *fmt);
		break;
	    }
	    fmt++;
	}
}

#if defined(vax)

/* Vax double precision floating point */
struct  vax_double {
	unsigned int	mant1 : 7;
	unsigned int	exp   : 8;
	unsigned int	sign  : 1;
	unsigned int	mant2 : 16;
	unsigned int	mant3 : 16;
	unsigned int	mant4 : 16;
};

#define VAX_DBL_BIAS	0x81
#define IEEE_DBL_BIAS	0x3ff
#define MASK(nbits)	((1 << nbits) - 1)

static struct dbl_limits {
	struct	vax_double d;
	struct	ieee_double ieee;
} dbl_limits[2] = {
	{{ 0x7f, 0xff, 0x0, 0xffff, 0xffff, 0xffff },	/* Max Vax */
	{ 0x0, 0x7ff, 0x0, 0x0 }},			/* Max IEEE */
	{{ 0x0, 0x0, 0x0, 0x0, 0x0, 0x0},		/* Min Vax */
	{ 0x0, 0x0, 0x0, 0x0 }}				/* Min IEEE */
};

/*
 *	The value rdp is a return value only. It will generally be
 * the same pointer as 'dp'. This is done since functions can not return
 * compound types (like struct ieee_double).
 */
static
vax_ieee_double(dp, rdp)
	double *dp;
	struct ieee_double *rdp;
{
	register long *lp;
	struct	ieee_double id;
	struct	vax_double vd;
	register struct dbl_limits *lim;
	int i;

	vd = *((struct vax_double *)dp);
	for (i = 0, lim = dbl_limits;
		i < sizeof(dbl_limits)/sizeof(struct dbl_limits);
		i++, lim++) {
		if ((vd.mant4 == lim->d.mant4) &&
			(vd.mant3 == lim->d.mant3) &&
			(vd.mant2 == lim->d.mant2) &&
			(vd.mant1 == lim->d.mant1) &&
			(vd.exp == lim->d.exp)) {
			id = lim->ieee;
			goto returnit;
		}
	}
	id.exp = vd.exp - VAX_DBL_BIAS + IEEE_DBL_BIAS;
	id.mant1 = (vd.mant1 << 13) | (vd.mant2 >> 3);
	id.mant2 = ((vd.mant2 & MASK(3)) << 29) |
			(vd.mant3 << 13) |
			((vd.mant4 >> 3) & MASK(13));
returnit:
	id.sign = vd.sign;
	rdp->mant1 = id.mant1;
	rdp->mant2 = id.mant2;
	rdp->exp = id.exp;
	rdp->sign = id.sign;
}


/* Vax single precision floating point */
struct	vax_single {
	unsigned int	mant1:7;
	unsigned int	exp:8;
	unsigned int	sign:1;
	unsigned int	mant2:16;
};

#define VAX_SNG_BIAS	0x81
#define IEEE_SNG_BIAS	0x7f

static struct sgl_limits {
	struct vax_single s;
	struct ieee_single ieee;
} sgl_limits[2] = {
	{{ 0x7f, 0xff, 0x0, 0xffff },	/* Max Vax */
	{ 0x0, 0xff, 0x0 }},		/* Max IEEE */
	{{ 0x0, 0x0, 0x0, 0x0 },	/* Min Vax */
	{ 0x0, 0x0, 0x0 }}		/* Min IEEE */
};

/*
 *	The value rfp is a return value only. It will generally be
 * the same pointer as 'fp'. This is done since functions can not return
 * compound types (like struct ieee_single).
 */
static
vax_ieee_float(fp, rfp)
	float			*fp;
	struct ieee_single	*rfp;
{
	struct ieee_single is;
	struct vax_single vs;
	struct sgl_limits *lim;
	int i;

	vs = *((struct vax_single *)fp);
	for (i = 0, lim = sgl_limits;
		i < sizeof(sgl_limits)/sizeof(struct sgl_limits);
		i++, lim++) {
		if ((vs.mant2 == lim->s.mant2) &&
			(vs.exp == lim->s.exp) &&
			(vs.mant1 == lim->s.mant1)) {
			is = lim->ieee;
			goto returnit;
		}
	}
	is.exp = vs.exp - VAX_SNG_BIAS + IEEE_SNG_BIAS;
	is.mant = (vs.mant1 << 16) | vs.mant2;
returnit:
	is.sign = vs.sign;
	rfp->mant = is.mant;
	rfp->exp = is.exp;
	rfp->sign = is.sign;
}
#endif /* vax */
