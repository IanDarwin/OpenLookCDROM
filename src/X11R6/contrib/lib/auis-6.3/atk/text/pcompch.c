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

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/text/RCS/pcompch.c,v 1.27 1993/12/07 20:50:38 Zarf Exp $";
#endif

#include <andrewos.h> /* sys/file.h and string(s).h */
#include <class.h>
#include <ctype.h>

#include <environ.ih>
#include <envrment.ih>
#include <text.ih>
#include <textv.ih>
#include <mark.ih>

#include <pcompch.eh>
#define MAXMACROLEN 15

static struct composites *composites[MAXCHAR+1];
static char *troffmagic[256];
static char *asciimagic[256];
static unsigned char order[256];

static char hex[]="0123456789abcdef";
static char octal[]="01234567";

static int parseBackslashed();

static void cleanmagic()
{
    int i;
    for(i=0;i<=255;i++) {
	if(troffmagic[i]) free(troffmagic[i]);
	if(asciimagic[i]) free(asciimagic[i]);
    }
}

/* The following two function were taken from basics/common/init.c on June 12, 1990 */
/* Translate a key sequence that has ^A, \ddd, and \c conventions. */
static int TranslateKeySequence(from, to)
char *from;
char *to;
{
    while (*from != '\0') {
	if (*from == '\\') {

	    int temp = parseBackslashed(&from);

	    if (temp == -1)
		return -1;
	    else
		*to++ = temp;
	}
	else if (*from == '^') {
	    ++from;
	    if (*from == 0)
		return -1;
	    *to++ = (*from++) & 0x1f;
	}
	else
	    *to++ = *from++;
    }
    *to++ = 0;
    return 0;
}

static int parseBackslashed(fromChars)
char **fromChars;
{

    int returnChar;
    char *from = *fromChars;
    static char *bsSource = "ebrnt";
    static char *bsDest = "\033\b\r\n\t";

    if (*from == '\\') {
	++from;
	if (*from == 0)
	    return -1;
	if (isdigit(*from)) {

	    int sum = 0, i;

	    for (i = 0; i < 3; ++i) {
		if (!isdigit(*from))
		    break;
		if (*from == '8' || *from == '9')
		    return -1;
		sum = sum * 8 + *from - '0';
		++from;
	    }
	    returnChar = sum;
	}
	else {

	    char *p;

	    p = index(bsSource, *from);
	    if (p != NULL)
		returnChar = bsDest[p-bsSource];
	    else
		returnChar = *from;
	    ++from;
	}
    }
    else
	return -1;
    *fromChars = from;
    return returnChar;
}

/* ahotoi: used by pcompch_insert to parse a hexadecimal or octal number depending on if base is 3 or 4. */
static unsigned char ahotoi(ptr,base2)
char *ptr;
int base2;
{
    unsigned char result;
    char c,*i,*base=(base2==3)?octal:hex;
    while(*ptr && *ptr!=' ') {
	result<<=base2;
	c=(*ptr);
	if(isupper(c)) c=tolower(c);
	i=index(base,c);
	if(!i) return '\0';
	result+=i-base;
	ptr++;
    }
    return result;
}

/* parsecode: parse a code in decimal,hex, octal or simply set the high bit of an ASCII character. */
static unsigned short parsecode(ptr)
char *ptr;
{
    switch(*ptr) {
	case '|':
	    return ptr[1]|0x80;
	case 'x':
	    return ahotoi(ptr+1,4);
	case 'o':
	    return ahotoi(ptr+1,3);
	case 'd':
	    ptr++;
	default:
	    return atoi(ptr);
    }
}

static void scanerr(msg,key,new,line)
char *msg;
char key;
struct composites *new;
long line;
{
    fprintf(stderr,msg,line);
    printf(msg,line);
    if(key) fprintf(stderr,"Key: %c\n",key);
    if(new) free(new);
}

/* scanstring: returns a pointer to the first un-escaped terminator in a string */
static char *scanstring(str)
char **str;
{
    boolean flag=FALSE;
    char *end=(*str);
    if(*end=='\"') {
	(*str)++;
	end++;
	while(*end && (flag || *end!='\"')) {
	    if(flag) flag=FALSE;
	    else if(*end=='\\') flag=TRUE;
	    end++;
	}
    } else {
	/* find the terminator, a space or tab */
	while(*end && *end!=' ' && *end!='\t') end++;
    }
    return end;
}

/* scancomposites: reads in (from the line pointed to by PTR) the extensions which may be used with the character KEY and puts the entries into the composites table giving the extensions and corresponding character code.
 */
static boolean scancomposites(ptr,line)
char *ptr;
long line;
{
    char *end,*ptr2,*style=NULL;
    unsigned short code;
    struct composites *new=NULL;
    /* Skip leading spaces */
    while(*ptr==' ' || *ptr =='\t') ptr++;

    /* Handle  a quoted key sequence,  setting end to the ending
      quote if there is one */
    end=scanstring(&ptr);
    
    /* Terminate the keysequence string with a nul. */
    if(*end) *end='\0';
    else {
	/* if there is no code assume this is a request to delete
	    the given key sequence from the table */
	pcompch_DeleteComposite(*ptr,ptr+1);
	return TRUE;
    }
    ptr2=end;
    /* Skip any white space after the key sequence */
    while(*++ptr2==' ' || *ptr2=='\t');

    if(!*ptr2) {
	scanerr("No character code in line %ld\n",*ptr,new,line);
	return FALSE;
    }
    
    code=parsecode(ptr2);

    /* skip the character code and any whitespace following
      it */
    while(*ptr2 && *ptr2!=' ' && *ptr2!='\t') ptr2++;
    while(*ptr2==' ' || *ptr2=='\t') ptr2++;
    
    /* if there is anything after the trailing white space
	assume it is a troff macro for the character */
    if(*ptr2&&*ptr2!='!') {
	for(end=ptr2+1;*end && *end!=' ' && *end!='\t';end++);
	if(*end) *end++='\0';
	if(strlen(ptr2)>MAXMACROLEN) {
	    scanerr("troff macro too long in line %ld\n",*ptr,new,line);
	    return FALSE;
	}
	if(troffmagic[code]) free(troffmagic[code]);
	troffmagic[code]=(char *)malloc(strlen(ptr2)+1);
	if(!troffmagic[code]) {
	    scanerr("Memory error in line %ld\n",*ptr,new,line);
	    return FALSE;
	}
	strcpy(troffmagic[code],ptr2);
	ptr2=end;
    } else if (*ptr2=='!') ptr2++;
    
    while(*ptr2==' ' || *ptr2=='\t') ptr2++;

    if(*ptr2) {
	end=scanstring(&ptr2);
	if(*end) *end++='\0';
	if(*ptr2) style=ptr2;
	ptr2=end;
	while(*ptr2==' ' || *ptr2=='\t') ptr2++;
	if(*ptr2 && *ptr2!='!') {
	    end=scanstring(&ptr2);
	    if(*end) *end++='\0';
	    if(*ptr2) {
		if(asciimagic[code]) free(asciimagic[code]);
		asciimagic[code]=(char *)malloc(strlen(ptr2)+1);
		if(!asciimagic[code]) {
		    scanerr("Memory error in line %ld\n",*ptr,new,line);
		    return FALSE;
		}
		strcpy(asciimagic[code],ptr2);
	    }
	} else if(*ptr2=='!') ptr2++;
    }

    /* Add the keysequence to the compositions table */
    new=(struct composites *) malloc(sizeof(struct composites));
    
    if(!new) {
	scanerr("Memory error while reading composites file.\n", 0, new, line);
	return FALSE;
    }

    if(*ptr) {
	if(strlen(ptr)>sizeof(new->exts)) {
	    scanerr("Extension too long in line %ld.\n",*ptr, new, line);
	    return FALSE;
	}
	TranslateKeySequence(ptr,ptr);
	strcpy((char *) new->exts,ptr+1);
	if(style) strcpy((char *) new->style,style);
	else new->style[0]='\0';
	new->code=code;
	pcompch_DeleteComposite(*ptr,new->exts);
	new->next=composites[*ptr];
	composites[*ptr]=new;
    }
    return TRUE;
}

    
/* fix_fgets: just changes the first newline in BUF to a null.  This is just to make things easy after using fgets. */  
static void fix_fgets(buf)
char *buf;
{
    char *p=index(buf,'\n');
    if(p) *p='\0';
}

void pcompch__ATKToASCII(classID,text,pos,len,func,rock)
struct classheader *classID;
struct text *text;
long pos,len;
procedure func;
long rock;
{
    struct mark *area;
    if(len==0) {
	len=text_GetLength(text);
	pos=0;
    }
    
    area=text_CreateMark(text,pos,len);
    if(!area) return;

    while(pos<mark_GetPos(area)+mark_GetLength(area)) {
	long ch;
	ch=text_GetChar(text,pos);
	if(ch==EOF) return;
	ch &= 0xff;
	if(asciimagic[ch]) pos=func(text,pos,asciimagic[ch],rock);
	else pos++;
    }
    text_RemoveMark(text,area);
    mark_Destroy(area);
}

void pcompch__ASCIIToATK(classID,text,pos,len,func,rock)
struct classheader *classID;
struct text *text;
long pos,len;
procedure func;
long rock;
{
    int i,j;
    long end;
    struct mark *area;
    if(len==0) {
	len=text_GetLength(text);
	pos=0;
    }
    area=text_CreateMark(text,pos,len);
    if(!area) return;
    for(i=0;i<256;i++) {
	pos=mark_GetPos(area);
	j=order[i];
	if(asciimagic[j]) {
	    long len=strlen(asciimagic[j]);
	    end=mark_GetPos(area)+mark_GetLength(area);
	    do {
		pos=text_Index(text, pos, asciimagic[j][0], end-pos);
		if(pos==EOF) break;
		if(pos+len<=end && !text_Strncmp(text, pos, asciimagic[j], len)) {
		    pos=func(text,pos,(unsigned char)j,asciimagic[j],rock);
		} else pos++;
		end=mark_GetPos(area)+mark_GetLength(area);
	    } while(pos!=EOF && pos+len <= end);
	}
    }
    text_RemoveMark(text,area);
    mark_Destroy(area);
}

static int lenorder(e1,e2)
unsigned char *e1,*e2;
{
    if(!asciimagic[*e2] || !asciimagic[*e1]) return 0;
    return strlen(asciimagic[*e2])-strlen(asciimagic[*e1]);
}
	

/* pcompch_ReadCompositesFile: reads COMPFILE and places the composites defined in it into the composites table. */
boolean pcompch__ReadCompositesFile(classID,compfile)
struct classheader *classID;
char *compfile;
{
    char buf[1024],*ptr=buf;
    boolean end=FALSE,err=FALSE;
    FILE *fp=fopen(compfile,"r");
    long line=0,i;
    if(!fp) return FALSE;
    while(!feof(fp)&!end) {
	*buf='\0';
	if(!fgets(buf,sizeof(buf),fp)) end=TRUE;
	fix_fgets(buf);
	line++;
	if(*buf=='#'||!*buf) continue;
	while(*ptr==' ' || *ptr=='\t') ptr++;
	if(!scancomposites(ptr,line)) err=end=TRUE;
    }
    for(i=0;i<=255;i++) order[i]=(unsigned char)i;
    qsort(order,256,1,lenorder);
    return !err;
}

/* pcompch_ClearComposites: clears out all defined compositions,
  not tested yet but should be correct. */
void pcompch__ClearComposites(classID)
struct classheader *classID;
{
    int i;
    for(i=1;i<=MAXCHAR;i++) {
	struct composites *next=composites[i];
	while(next) {
	    struct composites *this=next;
	    next=this->next;
	    free(this);
	}
	composites[i]=NULL;
    }
}

/* pcompch_DeleteComposite: finds and deletes one composite
  from the composites table.  */
void pcompch__DeleteComposite(classID,key,exts)
struct classheader *classID;
unsigned char key;
unsigned char *exts;
{
    struct composites *next=composites[key],*last=NULL;
    while(next) {
	struct composites *this=next;
	next=this->next;
	if(strcmp((char *) exts, (char *) this->exts)) last=this;
	else {
	    if(last) last->next=this->next;
	    else composites[key]=this->next;
	    free(this);
	}
    }
}

/* pcompch_EnumerateComposites: the function FUNC is called on each
  composite of key until it returns a non-zero value or there are no more
  composites.
  The call to func is made as below:
    func(key,composite,rock);
  where	composite is of type struct composites. */
long pcompch__EnumerateComposites(classID,key,func,rock)
struct classheader *classID;
char key;
procedure func;
long rock;
{
    long result;
    struct composites *next=composites[key];
    while(next) {
	struct composites *this=next;
	next=this->next;
	if(result=func(key,this,rock)) return result;
    }
    return 0;
}

char *pcompch__CharacterToTroff(classID,ch,env,tv)
struct classheader *classID;
unsigned char ch;
struct environment *env;
struct textview *tv;
{
    return troffmagic[ch];
}

unsigned char *pcompch__StringToTroff(classID,str,buf,bufsize,env,tv)
struct classheader *classID;
unsigned char *str;
struct environment *env;
struct textview *tv;
char *buf;
long bufsize;
{
    char buf2[16],*bp=buf;
    bzero(buf,bufsize);
    while(*str && buf-bp<bufsize-1) {
	if(*str=='\\'&& buf-bp<bufsize-2) {
	    *bp++='\\';
	    *bp++='\\';
	    str++;
	    continue;
	}    
	if(isprint(*str) || isascii(*str) || *str == '\t') *bp++=(*str++);
	else {
	    unsigned char *ccp=(unsigned char*) pcompch_CharacterToTroff(*str,env,tv);
	    if(ccp) {
		if((int)(buf-bp+strlen((char *) ccp))>=bufsize-1) return str;
		while(*ccp) *bp++=(*ccp++);
	    } else {
#ifdef GROFF_ENV
		sprintf(buf2,"%c", *str); /* groff groks 8-bit chars. At least, that's what this patch implies. Hope it's right. */
#else
		sprintf(buf2,"\\\\%3.3o",*str);
#endif
		if((int)(buf-bp+strlen(buf2))>=bufsize-1) return str;
		strcat(bp,buf2);
		bp+=strlen(buf2);
	    }
	    str++;
	}
    }
    return str;
}
	    

boolean pcompch__InitializeClass(ClassID)
struct classheader *ClassID;
{
    char *compfile=environ_GetProfile("compositesfile");
    boolean override= environ_GetProfileSwitch("overridecomposites",FALSE);
    char *libfile=environ_AndrewDir("/lib/compchar/comps");
    int i;

    for(i=0;i<=MAXCHAR;i++) composites[i]=NULL;
    
    for(i=0;i<=255;i++) {
	troffmagic[i]=NULL;
	asciimagic[i]=NULL;
    }
    
    if(override || access(libfile,F_OK)) {
	if(!compfile) {
	    fprintf(stderr,"No composites file given in preferences.\n");
	    return FALSE;
	}
    } else if(!override && !pcompch_ReadCompositesFile(libfile)) {
	fprintf(stderr,"Error reading default composites file:%s\n", libfile);
	pcompch_ClearComposites();
	cleanmagic();
	return FALSE;
    }
    if(compfile && !pcompch_ReadCompositesFile(compfile)) {
	fprintf(stderr,"Error reading composites file:%s.\n", compfile);
	pcompch_ClearComposites();
	cleanmagic();
	return FALSE;
    }
    return TRUE;
}
