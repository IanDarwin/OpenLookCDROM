/* File dogtags.c created by R S Kemmetmueller
    dogtags, an automatic 'stamper' for files as they're loaded in. */


/* Copyright 1993, 1994 Carnegie Mellon University and IBM All rights reserved.
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
#include <andrewos.h>
#include <class.h>
#include <ctype.h>


#include <pwd.h>
#include <sys/time.h>
#include <sys/param.h> /* for MAXPATHLEN */
#include <filetype.ih> /* for CanonicalizeFilename */
#include <buffer.ih>
#include <text.ih>

#ifndef NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/srctext/RCS/dogtags.c,v 1.4 1994/02/28 21:24:33 rr2b Exp $";
#endif

static char *makeupper(str)
char *str;
{
    char *st;
    st=str;
    while(*str!=0) {
	if(islower(*str)) *str=toupper(*str);
	str++;
    }
    return st;
}

/* nextDogtagPos() returns the position of the next "<@" in the file, or returns "length" if not found */
static long nextDogtagPos(self, pos, length)
struct text *self;
long pos,length;
{
    long p=pos;
    while (p<length) {
	p= text_Index(self,p, '<',length-p);
	if (p<0) break;
	if (text_GetChar(self, p+1)=='@') return p;
	p++;
    }
    return length;
}

/* dogtagSubstitution() returns a string that should replace whatever is in the file from dtpos of length dtlen.  If underscores are found within the delimiters, they will be counted, and the substitution string will try to fit itself into that number of characters, as well as being left- or right-justified, or centered if underscores were found on BOTH sides of the dogtag.  */
static char *dogtagSubstitution(self, dtpos,dtlen)
struct text *self;
long dtpos, dtlen;
{
    static char subst[1024], padded[1024], firstletter;
    int leading=0, trailing=0, count=0;
    boolean justifiable=TRUE; /* set this to FALSE if it's not something that can be left- or right-justified */

    subst[0]= '\0';
    dtpos+= 2; dtlen-= 4; /* ignore the <@ @> delimiters */
    /* count up leading underscores */
    while (dtlen>0 && text_GetChar(self,dtpos) == '_') {
	++dtpos; --dtlen;
	++leading;
    }
    /* count up trailing underscores */
    while (dtlen>0 && text_GetChar(self,dtpos+dtlen-1) == '_') {
	--dtlen;
	++trailing;
    }
    if (dtlen<=1) {
	/* nothing there but a bunch of underscores, fill it in with whitespace (or the lone char) */
	char ch=(dtlen==1)?text_GetChar(self,dtpos):' ';
	while (count < leading+trailing)
	    subst[count++]= ch;
	subst[count]= '\0';
	return subst;
    }
    /* we've got a useful string; let's see if it matches any known dogtags */
    firstletter= text_GetChar(self,dtpos);
    if (!text_Strncmp(self,dtpos, "pathname",dtlen) || !text_Strncmp(self,dtpos, "filename",dtlen) || !text_Strncmp(self,dtpos, "name",dtlen) || !text_Strncmp(self,dtpos, "NAME",dtlen)) {
	struct buffer *buf= buffer_FindBufferByData(self);
	if (buf!=NULL) {
	    if (firstletter=='p') /* pathname */
		strcpy(subst,buffer_GetFilename(buf));
	    else {
		char fullpath[1024], *start;
		long length;
		strcpy(fullpath,buffer_GetFilename(buf));
		start= rindex(fullpath,'/')+1;
		if (firstletter=='n' || firstletter=='N') { /* name or NAME */
		    strncpy(subst,start,length=(index(start,'.')-start));
		    subst[length]='\0';
		    if (firstletter=='N') /* NAME */
			makeupper(subst);
		} else /* filename */
		    strcpy(subst,start);
	    }
	}
    } else if (!text_Strncmp(self,dtpos, "year",dtlen) || !text_Strncmp(self,dtpos, "yr",dtlen) || !text_Strncmp(self,dtpos, "date",dtlen) || !text_Strncmp(self,dtpos, "day",dtlen) || !text_Strncmp(self,dtpos, "day0",dtlen) || !text_Strncmp(self,dtpos, "time",dtlen) || !text_Strncmp(self,dtpos, "mon",dtlen) || !text_Strncmp(self,dtpos, "mon0",dtlen)) {
	char info[27];
	struct timeval tv[1];
	struct timezone tz[1];
	gettimeofday(tv,tz);
	strncpy(info,ctime(&(tv->tv_sec)),27);
	if (firstletter=='y') { /* year or yr */
	    if (text_GetChar(self, dtpos+1)=='r') { /* yr */
		strncpy(subst,info+22,2); subst[2]='\0';
	    } else { /* year */
		strncpy(subst,info+20,4); subst[4]='\0';
	    }
	} else if (firstletter=='d') { /* date or day or day0 */
	    if (text_GetChar(self, dtpos+2)=='y') { /* day or day0 */
		char *day=subst;
		if (isdigit(*(info+8)))
		    *(day++)= *(info+8);
		else if (text_GetChar(self,dtpos+3)=='0') /* day0 */
		    *(day++)= '0';
		*(day++)= *(info+9);
		*day= '\0';
	    } else { /* date */
		strncpy(subst,info,10); subst[10]=' ';
		strncpy(subst+11,info+20,4); subst[15]='\0';
	    }
	} else if (firstletter=='m') { /* mon or mon0 */
	    char *mon;
	    boolean mon0=(text_GetChar(self, dtpos+3)=='0');
	    switch (*(info+4)) {
		case 'A':
		    if (*(info+5)=='p') /* Apr */
			mon= mon0?"04":"4";
		    else /* Aug */
			mon= mon0?"08":"8";
		    break;
		case 'D': /* Dec */
		    mon= "12";
		    break;
		case 'F': /* Feb */
		    mon= mon0?"02":"2";
		    break;
		case 'J':
		    if (*(info+5)=='a') /* Jan */
			mon= mon0?"01":"1";
		    else if (*(info+6)=='n') /* Jun */
			mon= mon0?"06":"6";
		    else /* Jul */
			mon= mon0?"07":"7";
		    break;
		case 'M':
		    if (*(info+6)=='r') /* Mar */
			mon= mon0?"03":"3";
		    else /* May */
			mon= mon0?"05":"5";
		    break;
		case 'N': /* Nov */
		    mon= "11";
		    break;
		case 'O':
		    mon= "10";
		    break;
		case 'S':
		    mon= mon0?"09":"9";
		    break;
		default: /* unforeseen error */
		    mon= "??";
		    break;
	    }
	    strcpy(subst,mon);
	} else { /* time */
	    strncpy(subst,info+11,8); subst[8]='\0';
	}
    } else if (!text_Strncmp(self,dtpos, "programmer",dtlen) || !text_Strncmp(self,dtpos, "userid",dtlen)) {
	struct passwd *pw;
	pw= getpwuid(getuid());
	if (!pw) /* yoikes! the call failed! No /etc/passwd file? */
	    strcpy(subst,"Mystery User");
	else if (firstletter=='p') { /* programmer */
	    strcpy(subst,pw->pw_gecos);
#ifdef USERID_COMMA_SERIALNUM
	    /* define USERID_COMMA_SERIALNUM, if the /etc/passwd file at your site adds a comma and other gunk after your name, and the gunk isn't appropriate or useful enough to make it appear in your source code. */
	    if (index(subst,','))
		*(index(subst,','))= '\0';
#endif /*USERID_COMMA_SERIALNUM*/
	    if (strlen(subst)<=0) /* no name found */
		strcpy(subst,"Nameless User");
	}
	else /* userid */
	    strcpy(subst,pw->pw_name);
    } else if (!text_Strncmp(self,dtpos, "log",dtlen)) {
	justifiable= FALSE;
	strcpy(subst,"\074@log@>\n\n\074@date@>  \074@time@>  by \074@programmer@>\n<reason><version><Brief description and why change was made.>");
    } else if (!text_Strncmp(self,dtpos, "file:",5)) {
	int i=0;
	justifiable= FALSE;
	subst[0]= '\0'; /* make it look like an empty string. "hide" the filename right after it */
	while (i<dtlen) {
	    subst[i+1]= text_GetChar(self, dtpos+i);
	    ++i;
	}
	subst[i+1]= '\0';
    }
    /* pad it out if it needs justifying */
    if (justifiable && (leading || trailing)) {
	char *pad=padded;
	if (leading && trailing) {
	    /* center it */
	    int frontpad=(int)((leading+trailing-(int)strlen(subst)) / 2);
	    for (count=0; count<frontpad; count++)
		*pad++= ' ';
	    strcpy(pad, subst);
	    pad+= strlen(pad);
	    count= pad-padded;
	    while (count++ < leading+trailing)
		*pad++= ' ';
	    *pad= '\0';
	} else if (leading) {
	    /* right-justify it */
	    int frontpad=leading-strlen(subst);
	    for (count=0; count<frontpad; count++)
		*pad++= ' ';
	    strcpy(pad, subst);
	} else {
	    /* left-justify it */
	    count= strlen(subst);
	    strcpy(pad, subst);
	    pad+= count;
	    while (count++ < trailing)
		*pad++= ' ';
	    *pad= '\0';
	}
	return padded;
    }
    return subst;
}

/* dogtags_substituteregion() will replace dogtags with the appropriate information, but only in the region specified */
void dogtags_substituteregion(self, posa,lena)
struct text *self;
long *posa, *lena; /* *posa and *lena passed by reference; *lena will probably be changing! */
{
    register long pos;
    long srclen=(*posa)+(*lena), dtstart=nextDogtagPos(self, *posa, srclen);
    while (dtstart<srclen) {
	pos= text_Index(self,dtstart+2, '@',srclen-dtstart-2);
	if (pos>0 && pos<=srclen-2 && text_GetChar(self,pos+1)=='>') {
	    long substlen, dtlen=pos-dtstart+2;
	    char *subst;
	    /* figure out what to substitute it with */
	    subst= dogtagSubstitution(self, dtstart,dtlen);
	    substlen= strlen(subst);
	    /* do the substitution and compensate for the change in length */
	    if (substlen>0 && text_ReplaceCharacters(self, dtstart,dtlen, subst,substlen)) {
		if (!text_Strncmp(self,dtstart, "\074@log@>",7))
		    dtstart+= 7; /* only skip over the log dogtag itself, we've still got work to do inside */
		else dtstart+= substlen;
		srclen+= substlen-dtlen;
	    } else {
		if (substlen==0 && strncmp(subst+1,"file:",5)==0) {
		    /* it's a filename we "hid" there */
		    if (text_DeleteCharacters(self, dtstart,dtlen)) {
			char fname[MAXPATHLEN];
			filetype_CanonicalizeFilename(fname, subst+6, MAXPATHLEN);
			substlen= text_InsertFile(self, NULL, fname, dtstart);
			srclen+= substlen-dtlen;
			/* leave dtstart where it is, to catch dogtags WITHIN the file we inserted */
		    } else /* DeleteCharacters failed (readonly buffer?), just skip over it */
			dtstart+= dtlen;
		} else /* ReplaceCharacters failed (readonly buffer?), just skip over it */
		    dtstart+= dtlen;
	    }
	} else
	    ++dtstart; /* false alarm, not a dogtag, skip over the '<' */
	dtstart= nextDogtagPos(self, dtstart, srclen);
    }
    *lena= srclen-(*posa);
    return;
}

/* dogtags_substitute() will buzz through the entire text object looking for dogtags, which are delimited by <@Dogtagname@>, and substitute the appropriate actual information */
void dogtags_substitute(self)
struct text *self;
{
    long pos=0, len=text_GetLength(self);
    dogtags_substituteregion(self, &pos,&len);
    return;
}
