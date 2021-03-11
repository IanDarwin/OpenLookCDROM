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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/text/RCS/indexpro.c,v 1.9 1993/10/26 22:43:58 gk5g Exp $";
#endif

#include <andrewos.h>
#include <stdio.h>
#include <ctype.h>

#define GAP 1
#define BUFSIZE 5120
#define NUMSIZE 512
#define DCHAR '+'
static char header[] = "\
.SH \n\
Index\n\
.LP \n\
.nr PS 8\n\
.nr VS 9\n\
.MC 1.9i\n\
.na	\n\
.de XX	\n\
.br	\n\
.ti -.2i\n\
.ne 2	\n\
..\n\
.de YY 	\n\
.sp 1.5\n\
.ne 3\n\
.ce 1\n\
- \\\\$1 -\n\
.sp .5\n\
..\n\
.in .2i	\n\
.hy 0\
";
int cmp(a,b)
int *a,*b;
{
    return(*a - *b);
}
static char lbuf[512];
output(buf,n,np)
char *buf;
int *n,*np;
{
    int *tp;
    static char lastc = ' ';
    char *lc;
    int head = 0;
    if(lastc != *buf) {
	printf(".XX\n.YY %c\n.XX\n",*buf);
	lastc = *buf;
	head++;
    }
    if(np > n + 2) qsort((char *)n,np - n,sizeof(int),cmp);
    if((lc = (char *)index(buf,DCHAR)) != NULL && lc[1] == DCHAR &&
	lc[2] != ' ' && lc[2] != '\0'){
	int len = lc - buf;
	if((strlen(lbuf) == len) && strncmp(lbuf,buf,(len)) == 0){
	    buf = lc + 2;
	}
	else {
	    strncpy(lbuf,buf,len);
	    lbuf[len] = '\0';
	    if (head == 0) puts(".XX\n");
	    puts(lbuf);
	    buf = lc + 2;
	}
	putchar(' ');
    }
    else {
	if (head == 0) puts(".XX\n");
	strcpy(lbuf,buf);
    }
    fputs(buf,stdout);
    putchar(',');putchar(' ');
    while(n < np){
	printf("%d",*n);
	if(n + 1 == np) break;
	for(tp = n + 1;tp != np; tp++){
	    if(*tp == *n) continue;
	    if(*tp <= *(tp - 1) + GAP) continue;
	    break;
	}
	if(tp == n + 1){
	    putchar(',');
	    n++;
	    continue;
	}
	tp--;
	if(*tp != *n) printf("-%d",*(tp));
	n = tp + 1;
	if(n != np) putchar(',');
    }
    putchar('\n');
}

main(argc,argv)
int argc;
char *argv[];
{
    char buf[BUFSIZE],rbuf[BUFSIZE],*begin, *end,*c;
    int num[NUMSIZE],*np;
    puts(header);
    *buf = '\0';
    *lbuf = '\0';
    np = num;
    begin = rbuf + 4; /* skip over "ix: " */
    while(gets(rbuf) != NULL){
	end = rindex(rbuf,'\t');
	if(end == NULL) continue;
	for(c = end ; isspace(*c) && c > rbuf; c--)
	    *c = '\0';
	end++;
	if(islower(*begin)) *begin = toupper(*begin);
	if(*buf == '\0')
	    strcpy(buf,begin);
	else if(strcmp(begin,buf) != 0){
	    output(buf,num,np);
	    np = num;
	    strcpy(buf,begin);
	}
	*np++ = atoi(end);
    }
    if(*buf) output(buf,num,np);
    exit(0);
}

	
