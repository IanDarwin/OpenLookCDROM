/*  -------------------------------------------------------------------

This program is the property of:

                             Cornell University 
                        Center for Applied Mathematics 
                              Ithaca, NY 14853

and may be used, modified and distributed freely, subject to the 
following restrictions:

       Any product which incorporates source code from the dstool
       program or utilities, in whole or in part, is distributed
       with a copy of that source code, including this notice. You
       must give the recipients all the rights that you have with
       respect to the use of this software. Modifications of the
       software must carry prominent notices stating who changed
       the files and the date of any change.

DsTool is distributed in the hope that it will be useful, but 
WITHOUT ANY WARRANTY; without even the implied warranty of FITNESS 
FOR A PARTICULAR PURPOSE.  The software is provided as is without 
any obligation on the part of Cornell faculty, staff or students to 
assist in its use, correction, modification or enhancement.

  -----------------------------------------------------------------  */
/*
 *	PVM 2.4
 *	University of Tennesee, Knoxville, TN.
 *	Oak Ridge National Laboratory, Oak Ridge, TN.
 *	31 Dec 1991
 *
 *	Pvmuser.h
 *
 *	Interface defines for libpvm.
 *
$Log: pvmuser.h,v $
 * Revision 1.4  1992/01/24  19:20:41  manchek
 * changed PvmSyserr to PvmSysErr
 *
 * Revision 1.3  1992/01/23  23:23:28  manchek
 * added PvmAlready error code
 *
 * Revision 1.2  1992/01/22  00:53:22  manchek
 * added function definitions/prototypes
 *
 * Revision 1.1  1991/12/31  23:16:36  manchek
 * Initial revision
 *
 *
 */

/*
*	Error return codes from libpvm calls
*/

#define	PvmOk			0	/* okay */
#define	PvmSysErr		-1	/* some system error */
#define	PvmBadParam		-2	/* bad parameter (neg msg id, etc) */
#define	PvmMismatch		-3	/* barrier count mismatch */
#define	PvmTooLong		-4	/* name too long (32) */
#define	PvmNoData		-5	/* read past end of buffer */
#define	PvmNoHost		-6	/* no such host */
#define	PvmNoFile		-7	/* no such executable */
#define	PvmNoComp		-8	/* no such process component */
#define	PvmNoMsg		-9	/* no message available */
#define	PvmNoMem		-10	/* can't get memory */
#define	PvmAlready		-11	/* process already enrolled */

#ifdef __ProtoGlarp__
#undef __ProtoGlarp__
#endif
#ifdef __STDC__
#define __ProtoGlarp__(x) x
#else
#define __ProtoGlarp__(x) ()
#endif

int		barrier		__ProtoGlarp__((char *name, int num));
int		enroll		__ProtoGlarp__((char *proc));
int		getbytes	__ProtoGlarp__((char *cp, int n));
int		getncplx	__ProtoGlarp__((float *xp, int n));
int		getndcplx	__ProtoGlarp__((double *zp, int n));
int		getndfloat	__ProtoGlarp__((double *dp, int n));
int		getnfloat	__ProtoGlarp__((float *fp, int n));
int		getnint		__ProtoGlarp__((int *np, int n));
int		getnlong	__ProtoGlarp__((long *np, int n));
int		getnshort	__ProtoGlarp__((short *np, int n));
int		getstring	__ProtoGlarp__((char *cp));
int		initiate	__ProtoGlarp__((char *proc, char *arch));
int		initiateM	__ProtoGlarp__((char *proc, char *host));
void	initsend	();
void	leave		();
int		probe		__ProtoGlarp__((int mt));
int		probemulti	__ProtoGlarp__((int ntypes, int *mta));
int		pstatus		__ProtoGlarp__((int *ncpu, int *mixed));
int		putbytes	__ProtoGlarp__((char *cp, int n));
int		putncplx	__ProtoGlarp__((float *xp, int n));
int		putndcplx	__ProtoGlarp__((double *zp, int n));
int		putndfloat	__ProtoGlarp__((double *dp, int n));
int		putnfloat	__ProtoGlarp__((float *fp, int n));
int		putnint		__ProtoGlarp__((int *np, int n));
int		putnlong	__ProtoGlarp__((long *np, int n));
int		putnshort	__ProtoGlarp__((short *np, int n));
int		putstring	__ProtoGlarp__((char *cp));
int		rcv			__ProtoGlarp__((int mt));
int		rcvinfo		__ProtoGlarp__((int *len, int *mt, char *proc, int *inst));
int		rcvmulti	__ProtoGlarp__((int ntypes, int *mta));
int		ready		__ProtoGlarp__((char *event));
int		snd			__ProtoGlarp__((char *proc, int inst, int mt));
int		status		__ProtoGlarp__((char *proc, int inst));
int		terminate	__ProtoGlarp__((char *proc, int inst));
int		vrcv		__ProtoGlarp__((int mt));
int		vrcvmulti	__ProtoGlarp__((int ntypes, int *mta));
int		vsnd		__ProtoGlarp__((char *proc, int inst, int mt));
int		waituntil	__ProtoGlarp__((char *event));
int		whoami		__ProtoGlarp__((char *proc, int *inst));

