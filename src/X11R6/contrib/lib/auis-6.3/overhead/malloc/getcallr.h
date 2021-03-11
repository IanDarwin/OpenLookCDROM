/* ********************************************************************** *\
 *         Copyright IBM Corporation 1991 - All Rights Reserved      *
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






/* In routine f the statement
	GETCALLER(a, x)
   where a is the first parater to f and x is of type (char *)
   will store in x the return address within the caller of f.  
   This cannot be guaranteed to always work,
   so it cannot be recommended for anything other than diagnostic infomation.
*/


#ifndef _GETCALLR_
#define _GETCALLR_


#ifdef ibm032
#define RETADDROFF (6)
#else /* ! ibm032 */

#ifdef _IBMR2

#define RETADDROFF (4)

#else	/* ! _IBMR2  &&  ! ibm032 */

#define RETADDROFF (1)

#endif /* ! _IBMR2 */
#endif /* ! ibm032 */


#if (defined(sun4) || defined(sparc))

	/* the sun4 requires an assembler routine to find the caller of the caller */

#define GETCALLER(a, x) 	{extern char *getcaller(); x = getcaller();}

#else /* (defined(sun4) || defined(sparc)) */
#if (defined(mips) || defined(pmax))

#define GETCALLER(a, x)    {extern char *store_return_address; x = store_return_address;}

#else  /* defined(mips) || defined(pmax) */

	/* the vax compilers require the extra variable 'addrloc'
	others do not, but there is no great advantage in making a third case.  */

#define GETCALLER(a, x) 	{  \
	register char **addrloc;  \
	addrloc = (((char **)&a) - RETADDROFF);  \
	x = *addrloc;  \
}

#endif /* defined(mips) || defined(pmax) */
#endif /* (defined(sun4) || defined(sparc)) */

#if (defined(mips) || defined(pmax))
#if __STDC__
#define procdef(procname) procname##_sidedoor
#else
#define procdef(procname) procname/**/_sidedoor
#endif
#else /* (defined(mips) || defined(pmax)) */
#define procdef(procname) procname
#endif /* (defined(mips) || defined(pmax)) */


#endif  /* _GETCALLR_ */

