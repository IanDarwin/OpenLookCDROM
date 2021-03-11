

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/fxlib/rpc3.9/rpcsvc/RCS/rstat_xdr.c,v 1.3 1992/12/15 21:55:23 rr2b R6tape $";
#endif
#include <rpc/rpc.h>
#include "rstat.h"


bool_t
xdr_rstat_timeval(xdrs, objp)
	XDR *xdrs;
	rstat_timeval *objp;
{
	if (!xdr_u_int(xdrs, &objp->tv_sec)) {
		return (FALSE);
	}
	if (!xdr_u_int(xdrs, &objp->tv_usec)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_statstime(xdrs, objp)
	XDR *xdrs;
	statstime *objp;
{
	if (!xdr_vector(xdrs, (char *)objp->cp_time, CPUSTATES, sizeof(int), xdr_int)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->dk_xfer, DK_NDRIVE, sizeof(int), xdr_int)) {
		return (FALSE);
	}
	if (!xdr_u_int(xdrs, &objp->v_pgpgin)) {
		return (FALSE);
	}
	if (!xdr_u_int(xdrs, &objp->v_pgpgout)) {
		return (FALSE);
	}
	if (!xdr_u_int(xdrs, &objp->v_pswpin)) {
		return (FALSE);
	}
	if (!xdr_u_int(xdrs, &objp->v_pswpout)) {
		return (FALSE);
	}
	if (!xdr_u_int(xdrs, &objp->v_intr)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->if_ipackets)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->if_ierrors)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->if_oerrors)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->if_collisions)) {
		return (FALSE);
	}
	if (!xdr_u_int(xdrs, &objp->v_swtch)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->avenrun, 3, sizeof(int), xdr_int)) {
		return (FALSE);
	}
	if (!xdr_rstat_timeval(xdrs, &objp->boottime)) {
		return (FALSE);
	}
	if (!xdr_rstat_timeval(xdrs, &objp->curtime)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->if_opackets)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_statsswtch(xdrs, objp)
	XDR *xdrs;
	statsswtch *objp;
{
	if (!xdr_vector(xdrs, (char *)objp->cp_time, CPUSTATES, sizeof(int), xdr_int)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->dk_xfer, DK_NDRIVE, sizeof(int), xdr_int)) {
		return (FALSE);
	}
	if (!xdr_u_int(xdrs, &objp->v_pgpgin)) {
		return (FALSE);
	}
	if (!xdr_u_int(xdrs, &objp->v_pgpgout)) {
		return (FALSE);
	}
	if (!xdr_u_int(xdrs, &objp->v_pswpin)) {
		return (FALSE);
	}
	if (!xdr_u_int(xdrs, &objp->v_pswpout)) {
		return (FALSE);
	}
	if (!xdr_u_int(xdrs, &objp->v_intr)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->if_ipackets)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->if_ierrors)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->if_oerrors)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->if_collisions)) {
		return (FALSE);
	}
	if (!xdr_u_int(xdrs, &objp->v_swtch)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->avenrun, 3, sizeof(u_int), xdr_u_int)) {
		return (FALSE);
	}
	if (!xdr_rstat_timeval(xdrs, &objp->boottime)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->if_opackets)) {
		return (FALSE);
	}
	return (TRUE);
}




bool_t
xdr_stats(xdrs, objp)
	XDR *xdrs;
	stats *objp;
{
	if (!xdr_vector(xdrs, (char *)objp->cp_time, CPUSTATES, sizeof(int), xdr_int)) {
		return (FALSE);
	}
	if (!xdr_vector(xdrs, (char *)objp->dk_xfer, DK_NDRIVE, sizeof(int), xdr_int)) {
		return (FALSE);
	}
	if (!xdr_u_int(xdrs, &objp->v_pgpgin)) {
		return (FALSE);
	}
	if (!xdr_u_int(xdrs, &objp->v_pgpgout)) {
		return (FALSE);
	}
	if (!xdr_u_int(xdrs, &objp->v_pswpin)) {
		return (FALSE);
	}
	if (!xdr_u_int(xdrs, &objp->v_pswpout)) {
		return (FALSE);
	}
	if (!xdr_u_int(xdrs, &objp->v_intr)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->if_ipackets)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->if_ierrors)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->if_oerrors)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->if_collisions)) {
		return (FALSE);
	}
	if (!xdr_int(xdrs, &objp->if_opackets)) {
		return (FALSE);
	}
	return (TRUE);
}


