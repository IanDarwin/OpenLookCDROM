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

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/lib/RCS/getla.c,v 2.20 1993/11/30 20:02:44 gk5g Exp $";
#endif

#include <andrewos.h>
#include <fdplumb.h>
#include <sys/ioctl.h>
#include <sys/param.h>
#include <nlist.h>

#if defined(ultrix) && defined(mips)
#include <sys/fixpoint.h>
#endif

#ifdef NeXT
double getla(indx)
int indx;
{
    return (double) 0.0;
}
#else /* NeXT */

static struct nlist Nl[] =
{
#ifdef PMAX_ENV
	{ "avenrun" },
#else /* PMAX_ENV */
	{ "_avenrun" },
#endif /* PMAX_ENV */
#define	X_AVENRUN	0
	{ 0 },
};

static int kmem = -1, fpastate = -1, fpacount = 0;

double getla(indx)
int indx;
{
#ifdef sun
    long avenrun[3];	/* For any kind of sun */
#else /* sun */
#if defined(ultrix) && defined(mips)
    fix avenrun[3];
#else /* mips & ultrix */
    double avenrun[3];
#endif /* mips & ultrix */
#endif /* sun */

    if (kmem < 0) {
	kmem = open("/dev/kmem", 0);
	if (kmem < 0) return -1.0;
#ifdef FIOCLEX
	(void) ioctl(kmem, FIOCLEX, 0);
#endif /* FIOCLEX */
	nlist("/vmunix", Nl);
	if (Nl[0].n_type == 0) {
	    close(kmem);
	    kmem = -1;
	    return -1.0;
	}
    }
    if (fpastate < 0 || (fpastate > 0 && fpacount <= 0)) {
	fpastate = (fpacheck() == 0 ? 1 : 0);
	fpacount = 100;	/* Check it every 100 calls */
    }
    if (fpacount >= 0) --fpacount;
    if (fpastate <= 0) return -1.0;	/* Default if the FPA is bad. */

    if (lseek(kmem, (long) Nl[X_AVENRUN].n_value, 0) == -1 ||
	 read(kmem, avenrun, sizeof(avenrun)) < sizeof(avenrun)) return -1.0;

#ifdef SUN_ENV
#ifdef MACH
#define FSCALE (1<<8)
#endif
    return (double) avenrun[indx] / FSCALE;
#else /* SUN_ENV */
#if defined(mips) && defined(ultrix)
    return FIX_TO_DBL(avenrun[indx]);
#else /* mips & ultrix */
    return avenrun[indx];
#endif /* mips & ultrix */
#endif /* SUN_ENV */
}
#endif /* NeXT */

getla_ShutDown()
{
    if (kmem >= 0) {
	close(kmem);
	kmem = -1;
    }
}










