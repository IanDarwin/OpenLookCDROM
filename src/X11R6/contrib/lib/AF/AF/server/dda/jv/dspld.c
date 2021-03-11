/*
 * Copyright 1993 by Digital Equipment Corporation, Maynard, Massachusetts.
 * 
 * Permission to use, copy, modify, distribute, and sell this software and its 
 * documentation for any purpose is hereby granted without fee, provided that 
 * the above copyright notice appear in all copies and that both that 
 * copyright notice and this permission notice appear in supporting 
 * documentation, and that the name of Digital not be used in advertising or 
 * publicity pertaining to distribution of the software without specific, 
 * written prior permission.  Digital makes no representations about the 
 * suitability of this software for any purpose.  It is provided "as is" 
 * without express or implied warranty.
 * 
 * DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL 
 * DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN 
 * AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF 
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

#include <stdio.h>
#include <ctype.h>
#include <strings.h>
#include <server/include/misc.h>
#include "jv_io.h"

#define	EOS	'\0'
#define	NEWREC	'_'
#define	NIBUF	256

#define	FIELD	0
#define	RECORD	1

#define	RNULL	0
#define	RSTART	1
#define	RDATA	2
#define	RBDATA	3
#define	REND	4
#define	RCOMMENT 5
#define	RSYMBOL	6

int 
dspLoad(JvRegs *jv, char *file)
{
    FILE	*fp;
    int		type;
    char	ibuf[NIBUF];
    int		current=RNULL;
    int		lda, ldd;
    volatile CARD32	*p;

    if ((file == NULL) || ((fp=fopen(file,"r"))==NULL)){
	printf("cannot read %s\n",file);
	return FALSE;
    } 

    p = (volatile CARD32 *)jv->audio.ram;
    for(;;){
	if(current == RNULL){
	    if((type = get_field(fp,ibuf)) == EOF) goto out;
	    if(type == RECORD)
		current = RecordType(ibuf);
	}

	switch(current){
	case RSTART:
	    if(get_field(fp,ibuf) == EOF) goto out; /* id */
	    if(get_field(fp,ibuf) == EOF) goto out; /* version */
	    if(get_field(fp,ibuf) == EOF) goto out; /* revision */
	    if(get_comment(fp, ibuf) == EOF) goto out;
	    current = RNULL;
	    break;
	case REND:
	    if((type=get_field(fp,ibuf)) == EOF) goto out; /* optional addr */
	    if(type == RECORD)
	        current = RecordType(ibuf);
	    break;
	case RSYMBOL:
	    fprintf(stderr,"do not understand symbols\n");
	    goto out;
	    /**/
	case RDATA:
	    if(get_field(fp,ibuf) == EOF) goto out; /* memory */
	    if(get_field(fp,ibuf) == EOF) goto out; /* addr */
	    sscanf(ibuf,"%X",&lda);
	    while((type=get_field(fp, ibuf))!=EOF && type != RECORD){
		sscanf(ibuf,"%X",&ldd);
		if(lda > 0x8000) lda -= 0x8000;
		p[lda] = ldd<<8;		
/*		printf("%08x: %08x\n",lda,ldd);		 */

		++lda;
	    }
	    current = RecordType(ibuf);
	    break;
	case RBDATA:
	    if(get_field(fp,ibuf) == EOF) goto out; /* memory */
	    if(get_field(fp,ibuf) == EOF) goto out; /* addr */
	    if(get_field(fp,ibuf) == EOF) goto out; /* count */
	    if(get_field(fp,ibuf) == EOF) goto out; /* value */
	    fprintf(stderr,"Do not understand block data yet.\n");
	    current = RNULL;
	    break;
	case RCOMMENT:
	    if (get_comment(fp,ibuf) == EOF) goto out;
	    current = RNULL;
	    break;
	case RNULL:
	default:
	    fprintf(stderr,"null record type, %s\n",ibuf);
	    exit(1);
	}
    }
out:	fclose(fp);
	MB();
	return TRUE;
}

RecordType(buf)
    char	*buf;
{
    if (strcmp("_START",buf)==0) return(RSTART);
    if (strcmp("_SYMBOL",buf)==0) return(RSYMBOL);
    if (strcmp("_END",buf)==0) return(REND);
    if (strcmp("_DATA",buf)==0) return(RDATA);
    if (strcmp("_BLOCKDATA",buf)==0) return(RBDATA);
    if (strcmp("_COMMENT",buf)==0) return(RCOMMENT);

    return(RNULL);
}

/* 
 * EOF, 0, or 1
 */ 
get_field(fp, buf)
    FILE	*fp;
    char	*buf;
{
    register int	c;
    register char	*p;

    while((c=fgetc(fp))!=EOF && isspace(c))
	    ;
    if (c == EOF) return (EOF);
    for(p=buf, *p++=c; (c=fgetc(fp))!=EOF && !isspace(c); *p++=c)
	    ;
    *p = EOS;
    if (c!=EOF) ungetc(c, fp);
    return(*buf == NEWREC ? RECORD : FIELD);	
}

get_comment(fp, buf)
    FILE	*fp;
    char	*buf;
{
    register int	c;
    register char	*p;

    while((c=fgetc(fp))!=EOF && c!='\n' && isspace(c))
	    ;
    if (c == EOF || c!='\n') return (EOF);
    for(p=buf; (c=fgetc(fp))!=EOF && c!='\n'; *p++=c)
	    ;
    *p = EOS;
    return(0);
}
