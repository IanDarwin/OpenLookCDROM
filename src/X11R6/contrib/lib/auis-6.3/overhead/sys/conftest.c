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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/sys/RCS/conftest.c,v 1.13 1993/09/21 21:10:33 gk5g Exp $";
#endif

/*
	conftest.c
	Test the current Andrew configuration for reasonableness.
*/

#include <stdio.h>
#include <andrewos.h>

/* ./conftest "$(RESOLVLIB)" */

int main (argc, argv)
int argc; char **argv;
{
    int Problem, OneProb;

    Problem = 0;
    printf("conftest: Checking configuration for sys type ``%s'', full name ``%s''\n", SYS_NAME, OPSYSNAME);
    if ((SY_B4x + SY_U5x + SY_AIXx) == 0) {
	printf("conftest: This system type, %s, does not define any SY_xxx value\n", SYS_NAME);
	printf("          in its system.h file.\n");
	Problem = 1;
    } else {
	printf("Sys type %s is most like %s.\n", SYS_NAME,
	    (SY_B4x ? "4.3 BSD" :
	     (SY_U5x ? "System V" :
	      (SY_AIXx ? "AIX" : "NO SYSTEM!" ))) );
    }
/* #ifdef NDBM_ENV
/*    printf("I'm told that this system provides the NDBM package in its libc.a.\n");
/*#else /* NDBM_ENV */
/*    printf("I'm told that this system does not provide the NDBM package in its libc.a.\n");
/*#endif /* NDBM_ENV */ 

    printf("conftest: Checking for problems in the allsys.h/system.h/site.h files.\n");
    OneProb = 0;

#ifndef	SY_B42
    printf("SY_B42 is not defined.  It is turned off by defining it to have\n");
    printf("a value of zero, not undefining it.  It must be defined.\n");
    OneProb = 1;
#define SY_B42 0
#endif /* SY_B42 */
#ifndef	SY_B43
    printf("SY_B43 is not defined.  It is turned off by defining it to have\n");
    printf("a value of zero, not undefining it.  It must be defined.\n");
    OneProb = 1;
#define SY_B43 0
#endif /* SY_B43 */
#ifndef	SY_U51
    printf("SY_U51 is not defined.  It is turned off by defining it to have\n");
    printf("a value of zero, not undefining it.  It must be defined.\n");
    OneProb = 1;
#define SY_U51 0
#endif /* SY_U51 */
#ifndef	SY_U52
    printf("SY_U52 is not defined.  It is turned off by defining it to have\n");
    printf("a value of zero, not undefining it.  It must be defined.\n");
    OneProb = 1;
#define SY_U52 0
#endif /* SY_U52 */
#ifndef	SY_U53
    printf("SY_U53 is not defined.  It is turned off by defining it to have\n");
    printf("a value of zero, not undefining it.  It must be defined.\n");
    OneProb = 1;
#define SY_U53 0
#endif /* SY_U53 */
#ifndef	SY_U54
    printf("SY_U54 is not defined.  It is turned off by defining it to have\n");
    printf("a value of zero, not undefining it.  It must be defined.\n");
    OneProb = 1;
#define SY_U54 0
#endif /* SY_U54 */
#ifndef	SY_AIX11
    printf("SY_AIX11 is not defined.  It is turned off by defining it to have\n");
    printf("a value of zero, not undefining it.  It must be defined.\n");
    OneProb = 1;
#define SY_AIX11 0
#endif /* SY_AIX11 */
#ifndef	SY_AIX12
    printf("SY_AIX12 is not defined.  It is turned off by defining it to have\n");
    printf("a value of zero, not undefining it.  It must be defined.\n");
    OneProb = 1;
#define SY_AIX12 0
#endif /* SY_AIX12 */
#ifndef	SY_AIX221
    printf("SY_AIX221 is not defined.  It is turned off by defining it to have\n");
    printf("a value of zero, not undefining it.  It must be defined.\n");
    OneProb = 1;
#define SY_AIX221 0
#endif /* SY_AIX221 */
#ifndef	SY_B4x
    printf("SY_B4x is not defined.  It is turned off by defining it to have\n");
    printf("a value of zero, not undefining it.  It must be defined.\n");
    OneProb = 1;
#define SY_B4x 0
#endif /* SY_B4x */
#ifndef	SY_U5x
    printf("SY_U5x is not defined.  It is turned off by defining it to have\n");
    printf("a value of zero, not undefining it.  It must be defined.\n");
    OneProb = 1;
#define SY_U5x 0
#endif /* SY_U5x */
#ifndef	SY_AIXx
    printf("SY_AIXx is not defined.  It is turned off by defining it to have\n");
    printf("a value of zero, not undefining it.  It must be defined.\n");
    OneProb = 1;
#define SY_AIXx 0
#endif /* SY_AIXx */
#if ((SY_B4x + SY_U5x + SY_AIXx) > 1)
    printf("Your system.h file overspecifies the system kind.\n");
    printf("That is, it claims that this system is like more than one of\n");
    printf("the set 4.3BSD (SY_B4x), System V (SY_U5x), and AIX (SY_AIXx).\n");
    OneProb = 1;
#else /* (SY_B4x + SY_U5x + SY_AIXx) > 1 */
#if ((SY_B4x + SY_U5x + SY_AIXx) != 1)
    printf("Your system.h file underspecifies the system kind.\n");
    printf("That is, it does not claim that it is like any of\n");
    printf("the set 4.3BSD (SY_B4x), System V (SY_U5x), and AIX (SY_AIXx).\n");
    OneProb = 1;
#endif /* (SY_B4x + SY_U5x + SY_AIXx) != 1 */
#endif /* (SY_B4x + SY_U5x + SY_AIXx) > 1 */

    if (OneProb != 0) {
	printf("You should correct the defined status of the SY_xxx variables\n");
	printf("in your new system.h file.\n");
	Problem = 1;
	OneProb = 0;
    }

/* RUN_AMDS_ENV => AMS_DELIVERY_ENV */
#ifdef RUN_AMDS_ENV
#ifndef AMS_DELIVERY_ENV
    printf("RUN_AMDS_ENV is defined, but AMS_DELIVERY_ENV is not.\n");
    printf("Either define AMS_DELIVERY_ENV or undefine RUN_AMDS_ENV .\n");
    OneProb = 1;
#endif /* AMS_DELIVERY_ENV */
#endif /* RUN_AMDS_ENV */

/* AMS_DELIVERY_ENV => AMS_ENV */
#ifdef AMS_DELIVERY_ENV
#ifndef AMS_ENV
    printf("AMS_DELIVERY_ENV is defined, but AMS_ENV is not.\n");
    printf("Either define AMS_ENV or undefine AMS_DELIVERY_ENV .\n");
    OneProb = 1;
#endif /* AMS_ENV */
#endif /* AMS_DELIVERY_ENV */

/* AFS_ENV => WHITEPAGES_ENV */
#ifdef AFS_ENV
#ifndef WHITEPAGES_ENV
    printf("AFS_ENV is defined, but WHITEPAGES_ENV is not.\n");
    printf("Either define WHITEPAGES_ENV or undefine AFS_ENV .\n");
    OneProb = 1;
#endif /* WHITEPAGES_ENV */
#endif /* AFS_ENV */

/* AMS_DELIVERY_ENV => WHITEPAGES_ENV */
#ifdef AMS_DELIVERY_ENV
#ifndef WHITEPAGES_ENV
    printf("AMS_DELIVERY_ENV is defined, but WHITEPAGES_ENV is not.\n");
    printf("Either define WHITEPAGES_ENV or undefine AMS_DELIVERY_ENV .\n");
    OneProb = 1;
#endif /* WHITEPAGES_ENV */
#endif /* AMS_DELIVERY_ENV */

/* AFS30_ENV => AFS_ENV */
#ifdef AFS30_ENV
#ifndef AFS_ENV
    printf("AFS30_ENV is defined, but AFS_ENV is not.\n");
    printf("Either define AFS_ENV or undefine AFS30_ENV .\n");
    OneProb = 1;
#endif /* AFS_ENV */
#endif /* AFS30_ENV */

/* DEBUG_MALLOC_ENV => ANDREW_MALLOC_ENV */
#ifdef DEBUG_MALLOC_ENV
#ifndef ANDREW_MALLOC_ENV
    printf("DEBUG_MALLOC_ENV is defined, but ANDREW_MALLOC_ENV is not.\n");
    printf("Either define ANDREW_MALLOC_ENV or undefine DEBUG_MALLOC_ENV .\n");
    OneProb = 1;
#endif /* ANDREW_MALLOC_ENV */
#endif /* DEBUG_MALLOC_ENV */

/* FONTS_TO_BDF_ENV => FONTS_TO_PCF_ENV */
#ifdef FONTS_TO_BDF_ENV
#ifdef FONTS_TO_PCF_ENV
    printf("Both FONTS_TO_BDF_ENV and FONTS_TO_PCF_ENV are defined.\n");
    printf("You must undef FONTS_TO_PCF_ENV or remove \n");
    printf("the define of FONTS_TO_BDF_ENV in site.h.\n");
    OneProb = 1;
#endif /* FONTS_TO_PCF_ENV */
#endif /* FONTS_TO_BDF_ENV */

#ifndef X11_ENV
#ifndef WM_ENV
    printf("Neither X11_ENV nor WM_ENV is defined.\n");
    printf("ATK won't build for you.\n");
    OneProb = 1;
#endif /* WM_ENV */
#endif /* X11_ENV */

    if (OneProb != 0) {
	printf("conftest: You should correct the defined status of the _ENV variables\n");
	printf("in your config/site.h file.\n");
	Problem = 1;
	OneProb = 0;
    } else {
	printf("conftest: No inconsistencies detected in the allsys.h/system.h/site.h files.\n");
    }

    if (Problem == 0) {
	printf("conftest: No inconsistencies found.  Continuing.\n");
	exit(0);
    } else {
	printf("conftest: Inconsistent options selected for system build.  ABORTING!\n");
	exit(1);
    }
}
