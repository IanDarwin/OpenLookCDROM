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


 

#ifdef VMMON_DODECL
struct nlist   RawStatistics[] =
{
#endif /* VMMON_DODECL */
#define	X_CPTIME	0
#ifdef VMMON_DODECL
   {
      "_cp_time"
   },
#endif /* VMMON_DODECL */
#define	X_RATE		1
#ifdef VMMON_DODECL
   {
      "_rate"
   },
#endif /* VMMON_DODECL */
#define X_TOTAL		2
#ifdef VMMON_DODECL
   {
      "_total"
   },
#endif /* VMMON_DODECL */
#define	X_DEFICIT	3
#ifdef VMMON_DODECL
   {
      "_deficit"
   },
#endif /* VMMON_DODECL */
#define	X_BOOTIME	4
#ifdef VMMON_DODECL
   {
      "_boottime"
   },
#endif /* VMMON_DODECL */
#define	X_DKXFER	5
#ifdef VMMON_DODECL
   {
      "_dk_xfer"
   },
#endif /* VMMON_DODECL */
#define X_PGIN		6
#ifdef VMMON_DODECL
   {
      "_pgintime"
   },
#endif /* VMMON_DODECL */
#define X_NDSTAT	7
#ifdef VMMON_DODECL
   {
      "_ndstat"
   },
#endif /* VMMON_DODECL */
#define X_NSWAPMAP	8
#ifdef VMMON_DODECL
   {
       "_nswapmap"
   },
#endif /* VMMON_DODECL */
#define X_SWAPMAP	9
#ifdef VMMON_DODECL
   {
       "_swapmap"
   },
#endif /* VMMON_DODECL */
#define X_NSWAPBLKS	10
#ifdef VMMON_DODECL
   {
       "_nswap"
   },
#endif /* VMMON_DODECL */
#define X_DMMAX		11
#ifdef VMMON_DODECL
   {
       "_dmmax"
   },
#endif /* VMMON_DODECL */
#define	X_PROC		12
#ifdef VMMON_DODECL
   {
      "_proc"
   },
#endif /* VMMON_DODECL */
#define	X_NPROC		13
#ifdef VMMON_DODECL
   {
      "_nproc"
   },
#endif /* VMMON_DODECL */
#ifndef MAXUPRC
#define X_MAXUPRC	14
#ifdef VMMON_DODECL
   {
	"_maxuprc"
   },
#endif /* VMMON_DODECL */
#endif /* defined(vax) | defined(hpux) */
#ifdef VMMON_DODECL
   {
      0
   },
};
#endif /* VMMON_DODECL */
