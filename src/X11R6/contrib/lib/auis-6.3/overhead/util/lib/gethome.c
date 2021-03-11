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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/util/lib/RCS/gethome.c,v 2.9 1992/12/15 21:09:01 rr2b R6tape $";
#endif


 

#include <pwd.h>
#include <stdio.h>
#include <errno.h>
#include <util.h>

#ifndef NULL
#define NULL 0
#endif

extern char *getenv();

extern int errno;

static char home[100]="";

char *gethome(name)
char *name;
{
    struct passwd *pw=NULL;
    char *h=NULL;

    if (name==NULL) {
	if(home[0]!='\0')
	    return home;

	h=getenv("HOME");

	/* Do not trust env vbl HOME if it is "/" because we
	    may be spawned by the guardian -- nsb */
	if(h==NULL || strcmp(h,"/")==0) {
	    pw = getpwuid(getuid());
	    if (pw != NULL) h = pw->pw_dir;
	}
	if (h != NULL) strncpy(home, h, sizeof(home));
    } else {
	errno = 0;
	pw=getpwnam(name);
	if (pw != NULL) h = pw->pw_dir;
    }

    return h;
}
#ifdef TESTINGONLYTESTING
main()
{
    char Buf[500], *Ques, *Ans;

    for (;;) {
	printf("home dir of: "); fflush(stdout);
	if (gets(Buf) == NULL) exit(0);
	Ques = Buf[0] == '\0' ? NULL : Buf;
	Ans = gethome(Ques);
	printf("gethome(\"%s\") returns ``%s''.\n",
	       Ques == NULL ? "NULL" : Ques,
	       Ans == NULL ? "NULL" : Ans);
    }
}
#endif /* TESTINGONLYTESTING */
