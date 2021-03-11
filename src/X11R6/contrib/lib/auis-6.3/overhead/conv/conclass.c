/*LIBS: libutil.a libclass.a
*/

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/conv/RCS/conclass.c,v 1.9 1992/12/15 21:00:33 rr2b R6tape $";
#endif

/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
/*
  reads from a file or stdin.
  converts an atk file w/old style includes to new style
  -h is used to convert old .H to .ch files
  -r reverses the process to convert new style back to old style
     this should be usefull to people who have to maintain programs in both worlds.
  compile  cc -I. -I/usr/amos/include  fixincld.c  /usr/amos/lib/libclass.a /usr/amos/lib/libutil.a
  */

 
#define CONVERTFILE AndrewDir("/lib/convincl")
#include <stdio.h>
#include <class.h>
#include <ctype.h>
#include <mapping.h>
#include <sys/param.h>
#include <andrewos.h> /* sys/file.h */

struct namelist {
    char *oldname,*newname;
};
static struct namelist nl[512];
FILE *fout;
finit(fn)
char *fn;
{
    FILE *f;
    char buf[512],*s;
    struct namelist *n;
    int cnt;
    if((f = fopen(fn,"r")) == NULL) {
	fprintf(stderr,"Warning : Can't Open %s\n",fn);
	return FALSE;
    }
    n = nl;cnt = 0;
    while((fgets(buf,511,f)) != NULL){
	s = malloc(strlen(buf) + 1);
	strcpy(s,buf);
	while(isspace(*s)) s++;
	n->newname = s;
	while(*s != '\0' && !isspace(*s)) s++;
	if(*s == '\0') continue;
	*s++ = '\0';
	if(*s == '\0') continue;
	while(isspace(*s)) s++;
	if(*s == '\0') continue;
	n->oldname = s;
	while(*s != '\0' && !isspace(*s)) s++;
	*s = '\0';
	n++;
	if(++cnt > 510) break;
    }
    n->newname = NULL;
    n->oldname = NULL;
    fclose(f);
    return TRUE;
}
char *getnm(c)
char *c;
{
    struct MapEntryStruct *mp;
    if(((mp = RetrieveByName(c,class_VERSIONNOTKNOWN)) == NULL )|| (mp->Key == NULL) || (mp->Name == mp->Key) || (strcmp(mp->Name,mp->Key) == 0)){
	return c;
    }
    else {
	return mp->Key;
    }

}
char *rgetnm(c)
char *c;
{
    struct MapEntryStruct *mp;
    if(((mp = RetrieveByKey(c,class_VERSIONNOTKNOWN)) == NULL )|| (mp->Name == NULL) || (mp->Name == mp->Key) || (strcmp(mp->Name,mp->Key) == 0)){
	return c;
    }
    else {
	return mp->Name;
    }

}
int hflag;
getf(f,buf,src)
FILE *f;
char *buf;
char *src;
{
    char ending[1024];
    boolean foundio,foundic;
    register char *c,*cp;
    struct namelist *n;
    struct MapEntryStruct *mp;
    foundio = foundic = FALSE;
    while(fgets(buf,2048,f) != NULL){
	if(hflag){
	    for(c = buf; isspace(*c); c++) ;
	    if(strncmp(c,"class ",6) == 0){
		c = strchr(buf,':');
		if(c != NULL) cp =  strchr(c,'[');
		if(c != NULL && cp == NULL) {
		    c++;
		    while(isspace(*c)) c++;
		    for(cp = c; !isspace(*cp) && *cp != '{'; cp++) ;
		    strcpy(ending,cp);
		    *cp = '\0';
		    if(((mp = RetrieveByName(c,class_VERSIONNOTKNOWN)) == NULL )|| (mp->Key == NULL) || (mp->Name == mp->Key) || (strcmp(mp->Name,mp->Key) == 0)){
			*cp = *ending;
		    }
		    else {
			sprintf(cp,"[%s]%s",mp->Key,ending);
		    }
		}
	    }
	    else {
		if(foundio == FALSE && strncmp(c,"InitializeObject",16) == 0)
		    foundio = TRUE;
		else if(foundic == FALSE && strncmp(c,"InitializeClass",15) == 0)
		    foundic = TRUE;
	    }
	}
	if(*buf == '#'){
	    for(c = buf + 1; isspace(*c); c++);
	    if(isspace(c[7]) && strncmp(c,"include",7) == 0 ) {
		for(c = c + 8; isspace(*c); c++);
		c++;
		cp = strchr(c,'.');
		if(cp != NULL && (*(cp + 1) == 'i' || *(cp + 1) == 'e')){
		    *cp = '\0';
		    if(((mp = RetrieveByName(c,class_VERSIONNOTKNOWN)) == NULL )|| (mp->Key == NULL) || (mp->Name == mp->Key) || (strcmp(mp->Name,mp->Key) == 0)){
			*cp = '.';
		    }
		    else {
			strcpy(ending,cp +1);
			sprintf(c,"%s.%s",mp->Key,ending);
		    }
		}
		else {
		    for(n = nl; n->oldname != NULL; n++){
			if(*(n->oldname) == *c &&
			   strncmp(n->oldname,c,strlen(n->oldname)) == 0){
			    strcpy(ending,c + strlen(n->oldname));
			    sprintf(c,"%s%s",n->newname,ending);
			}
		    }
		}
	    }
	}
	fputs(buf,fout);
    }
    if(hflag){
	if(src == NULL) src = "Standard Input";
	if(foundic == FALSE && foundio == FALSE)
	    fprintf(stderr,"Warning: neither InitializeObject nor InitializeClass procedures are declared in %s\nThey must be added if they exist in the corresponding .c file\n",src);
	else if(foundic == FALSE)
	    fprintf(stderr,"Warning:The InitializeClass procedure is not declared in %s\n\tIt must be added if it exist in the corresponding .c file\n",src);
	else if(foundio == FALSE)
	    fprintf(stderr,"Warning:The InitializeObject procedure is not declared in %s\n\tIt MUST be added if it exists in the corresponding .c file\n",src);
	else return;
	fflush(stderr);
    }
}
rgetf(f,buf)
FILE *f;
char *buf;
{
    boolean brak;
    char ending[1024];
    register char *c,*cp;
    struct namelist *n;
    struct MapEntryStruct *mp;
    while(fgets(buf,2048,f) != NULL){
	if(hflag){
	    if(strncmp(buf,"class ",6) == 0){
		for(c = buf,brak = FALSE; *c != '\0'; c++){
		    if(*c == '['){
			brak = TRUE;
			continue;
		    }
		    if(!brak) putchar(*c);
		    if(*c == ']'){
			brak = FALSE;
		    }
		}
		continue;
	    }	  
	}
	if(*buf == '#'){
	    for(c = buf + 1; isspace(*c); c++);
	    if(isspace(c[7]) && strncmp(c,"include",7) == 0 ) {
		for(c = c + 8; isspace(*c); c++);
		c++;
		cp = strchr(c,'.');
		if(cp != NULL && (*(cp + 1) == 'i' || *(cp + 1) == 'e')){
		    *cp = '\0';
		    if(((mp = RetrieveByKey(c,class_VERSIONNOTKNOWN)) == NULL )|| (mp->Name == NULL) || (mp->Name == mp->Key) || (strcmp(mp->Name,mp->Key) == 0)){
			*cp = '.';
		    }
		    else {
			strcpy(ending,cp +1);
			sprintf(c,"%s.%s",mp->Name,ending);
		    }
		}
		else {
		    for(n = nl; n->newname != NULL; n++){
			if(*(n->newname) == *c &&
			   strncmp(n->newname,c,strlen(n->newname)) == 0){
			    strcpy(ending,c + strlen(n->newname));
			    sprintf(c,"%s%s",n->oldname,ending);
			}
		    }
		}
	    }
	}
	fputs(buf,fout);
    }
}
main(argc,argv)
int argc;
char *argv[];
{
    boolean reverse;
    char **src,*cvfile,*AndrewDir(),*srclist[512],*dirfile;
    char outbase[256],*op,*fo,outfile[1024];
    FILE *f;
    int i;
    char buf[2048];
    class_Init(AndrewDir("/dlib/atk"));
    hflag = FALSE;
    f =	stdin;
    fout = stdout;
    cvfile = CONVERTFILE;
    reverse = FALSE;
    src = srclist;
    dirfile = NULL;
    *src = NULL;
    for (i = 1; i < argc; i++){
	if(*argv[i] == '-') {
	    switch(argv[i][1]){
		case 'h':
		    hflag = TRUE;
		    break;
		case 'r':
		    reverse = TRUE;
		    break;
		case 'c':
		    if(argv[i][2] == '\0'){
			if(++i == argc) usage(argv[0]);
			cvfile = argv[i];
		    }
		    else cvfile =  &(argv[i][2]);
		    break;
		case 'd':
		    if(argv[i][2] == '\0'){
			if(++i == argc) usage(argv[0]);
			dirfile = argv[i];
		    }
		    else dirfile =  &(argv[i][2]);
		    break;
		default:
		    usage(argv[0]);
	    }
	}
	else {
	    *src++ = argv[i];
	    *src = NULL;
	}
    }
    src = srclist;
    finit(cvfile);
    if(*src == NULL) {
	/* deal w/stdin */
	if(dirfile != NULL) usage(argv[0]);
	if(reverse) rgetf(f,buf);
	else getf(f,buf,*src);
	exit(0);
    }
    while(*src != NULL){
	if(access(*src,R_OK) == -1 || (f = fopen(*src,"r")) == NULL){
	    fprintf(stderr,"Can't Open %s\n",*src);
	    exit(1);
	}   
	if(dirfile){
	    if(*src == NULL) usage(argv[0]);
	    if((op = strrchr(*src,'/')) == NULL) op = *src;
	    for(fo =outbase;*op != '.' && *op != '\0';op++,fo++) 
		*fo = *op;
	    *fo = '\0';
	    if(*op) op++;
	    printf("%s -> %s\n",outbase,  rgetnm(outbase));
	    sprintf(outfile,"%s/%s.",dirfile,
		    (reverse ? rgetnm(outbase):getnm(outbase)));
	    if(strcmp(op,"ch") == 0 && reverse) 
		strcat(outfile,"H");
	    else if(strcmp(op,"H") == 0 && !reverse)
		strcat(outfile,"ch");
	    else strcat(outfile,op);
	    if((fout = fopen(outfile,"w")) == NULL){
		fprintf(stderr,"Can't open %s\n",outfile);
		usage(argv[0]);
	    }
	}
	if(reverse) rgetf(f,buf);
	else getf(f,buf,*src);
	fclose(f);
	fclose(fout);
	if(dirfile == NULL) break;
	src++;
    }
}

usage(nm)
char *nm;
{
    fprintf(stderr,"usage: %s [-h] [-r] [-c<conversion file>] filename <filename> [-d<directory>]\n-h is used to convert old .H to .ch files\n-r reverses the process to convert new style back to old style\n-c<conversion file> will use the named file for additional conversion information\n\tthe default conversion file is %s\n-d<directory> will write the file[s] out to the indicated directory, converting the name if necessary\n\t-d, if present, it must be provided at least one filename\n" ,nm,CONVERTFILE);
    exit(1);
}
