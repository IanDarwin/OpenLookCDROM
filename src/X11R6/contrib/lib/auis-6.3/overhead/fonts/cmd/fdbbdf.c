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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/fonts/cmd/RCS/fdbbdf.c,v 2.7 1992/12/15 21:02:39 rr2b R6tape $";
#endif


 

static int Coffset = 0;
#include <ctype.h>

/* 
*
*
*	fdbbdf: A Program To Convert Andrew fdb fonts 
*		To X bdf fonts.
*
*	compile hc fdbbdf.c -o fdbbdf 
*	or	cc fdbbdf.c -o fdbbdf 
*
*	Usage fdbbdf [-mask] [-Soffset] [fontname.fdb]
*
*	The resulting bdf output may be piped into fc, 
*	or redirected into a file.
*	If no filename is given, fdbbdf will read from stdin.
*	The Andrew program wmfdb may be used to convert Andrew fwm
*	fonts to fdb fonts.
*
*	options:
*		The -mask option will produce a font of blank characters that may
*		be used as masks when the original font is used for cursors.
*		(This shouldn't be necessary, X should handle NULL masks properly)
*
* No longer produces dummy characters, instead it counts the number
* of characters actually defined and uses that value for the CHARS value 
* in the header.
* Now takes a -Soffset flag where offset is an int to add to the encoding value,
* for use by people who wish to build up an xfont with more than 128 characters
* out of 2 or more wm fonts.
* 
 */

#define MAX(A,B) ((A > B)? A:B)
#include <stdio.h>
#define otherwise break; case
static char *names[] = {
    "magic ",
    "fontname ",
    "familyname ",
    "rotation ",
    "pointsize ",
    "MaxNWtoOrigin ",
    "MaxNtoS ",
    "MaxWtoE ",
    "MaxWbase ",
    "MaxNewline ",
    "FontRepresentationType ",
    "NIcons ",
    "character ",
    "spacing ",
    "box ",
    "origin ",
    "raster",
    "end",
    "COMMENT",
    "facecodes",
    ""
};
#define magic 0
#define fontname 1
#define familyname 2
#define rotation 3
#define pointsize 4
#define MaxNWtoOrigin 5
#define MaxNtoS 6
#define MaxWtoE 7
#define MaxWbase 8
#define MaxNewline 9
#define FontRepresentationType 10
#define NIcons 11
#define character 12
#define spacing 13
#define box 14
#define origin 15
#define raster 16
#define end 17
#define Comment 18
#define facecodes 19
#define LSTSIZE 20
struct st {
	char *s;
	short len;
};
static struct st lst[LSTSIZE],*endlst;
static char comments[2048];
static int maskflag = 0;
static int padflag = 0;
int padsize,ypad;
static used[256];
int HeaderPrinted = 0;
int minChar;

usage(s)
char *s;
{
	fprintf(stderr,"usage: %s [-mask] [-Soffset] [filename]  \n",s);
	exit(1);
}
initlst(){
	register struct st *lstp;
	register char **c;
	*comments = '\0';
	for (c =names,lstp = lst; **c != '\0'; c++,lstp++){
		lstp->s = *c;
		lstp->len = strlen(*c);
	}
	endlst = lstp;
}

printdummys(ed,fout)
int ed;
FILE *fout;
/* Writes out blank definitions for undefined characters */
{
    register int i;
    for(i = 0; i < ed; i++)  {
	if (used[i] == 0){
	    if(isprint(i + Coffset)) 
		fprintf(fout,"STARTCHAR %c\nENCODING -1 %d\nSWIDTH 0 0\nDWIDTH 0 0\nBBX 0 0 0 0\nBITMAP\nENDCHAR\n",i + Coffset,i + Coffset);		
	    else
		fprintf(fout,"STARTCHAR ch-%d\nENCODING -1 %d\nSWIDTH 0 0\nDWIDTH 0 0\nBBX 0 0 0 0\nBITMAP\nENDCHAR\n",i + Coffset,i + Coffset);
	}
    }
}
main(argc,argv)
char *argv[];
{
    int i,count;FILE *f;
    initlst();
    for(i = 1; i < argc; i++){
	if(*argv[i] == '-'){
	    switch(argv[i][1]){
		case 'm':
		    maskflag++;
		    break;
		case 'p':
		    padflag++;
		    break;
		case 'S':
		    Coffset = atoi(&argv[i][2]);
		    break;
		default:
		    usage(argv[0]);
	    }
	}
	else if((f = fopen(argv[i],"r")) != NULL){
	    count = fontcount(f);
	    rewind(f);
	    fontcvt(f,stdout,count);
	    fclose(f);
	    exit(0);
	}
	else{
	    fprintf(stderr,"Can't open %s\n",argv[i]);
	    exit(-2);
	}
    }
    fontcvt(stdin,stdout);
    exit(0);
}

char *lookup(s,i)
register char *s;
int *i;
{
	register struct st *lstp;
	for(lstp = lst; lstp != endlst; lstp++){
		if((*(lstp->s) == *s) && (strncmp(lstp->s,s,lstp->len) == 0)){
		 	*i = (lstp - lst);
			return(s + lstp->len);
		}
	}
	for(lstp = lst; lstp != endlst; lstp++){
		register char *p = lstp->s;
		register char *q = s;

		while (*p != '\0' && *q != '\0' && ((isupper(*p) ? tolower(*p) : *p)) == ((isupper(*q) ? tolower(*q) : *q)))  {
		    p++;
		    q++;
		}
		if(*p == '\0'){
		 	*i = (lstp - lst);
			return(s + lstp->len);
		}
	}
	*i = -1;
	return(NULL);
}
hexout(c,f)
register char *c;
register FILE *f;
{
	register int w = 0;
	while(*c != '\0'){
	    if(*c == '\n')break;
	    else if(maskflag) putc('0',f);
	    else {
		if(isupper(*c)) *c = tolower(*c);
		putc(*c,f);
	    }
	    c++;w++;
	}
	if(padflag)
		while(w++ < padsize) putc('0',f);
	putc('\n',f);
}
static int psize,maxnwx,maxnwy,maxntosx,maxntosy,maxwtoex,maxwtoey,maxwbx,maxwby,maxnewlx,maxnewly,nicons;

PrintHeader(fout,count)
FILE *fout;
int count;
{
	if(padflag ){
		int size = MAX( maxwtoex + maxnwx,maxntosy + maxnwy) ; 
		size = (size / 32) * 32 +( (size % 32 > 0) ? 32 : 0 );
		maxwtoex  = size - maxnwx;
		maxntosy = size - maxnwy;
	}
	fprintf(fout,"SIZE %d %d %d\n",psize,78,78);
/* 	fprintf(fout,"FONTBOUNDINGBOX %d %d %d %d\n",maxwtoex,maxntosy,-maxnwx,-maxnwy);
 */	fprintf(fout,"FONTBOUNDINGBOX %d %d %d %d\n", maxwtoex, maxntosy,-maxnwx,maxnwy-maxntosy);
	fprintf(fout,"STARTPROPERTIES 4\n");
	fprintf(fout,"Ownership \" %s \"\n",comments);
/* 	fprintf(fout,"FONT_ASCENT %d\n",maxntosy - maxnwy);
	fprintf(fout,"FONT_DESCENT %d\n",maxnwy);
 */	fprintf(fout,"FONT_ASCENT %d\n",maxnwy);
	fprintf(fout,"FONT_DESCENT %d\n",maxntosy-maxnwy);
	fprintf(fout, "DEFAULT_CHAR %d\n", minChar);
	fprintf(fout,"ENDPROPERTIES\n");
/*	fprintf(fout,"CHARS %d\n",nicons); */
	fprintf(fout,"CHARS %d\n",count);
	HeaderPrinted = 1;
}
static int cvt,spx,spy,orx,ory,bx,by; 
writechar(fout)
FILE *fout;
/* writes the character information */
{
if(padflag && bx > 0){
	int size =  MAX(bx,by);
	size = (size / 32) * 32 +( (size % 32 > 0) ? 32 : 0 );
	padsize = size / 4;
	ypad = size - by;
	bx = size;
	by = size;
	}
fprintf(fout,"SWIDTH %d %d\n",0,0);
	/* ??? fc expects an SWIDTH, but it ignores its arguments. */
fprintf(fout,"DWIDTH %d %d\n",spx,spy);
fprintf(fout,"BBX %d %d %d %d\n",bx,by,-orx ,ory - by );
fprintf(fout,"BITMAP\n");
}

fontcount(f)
FILE *f;
{
    char buf[256];
    int result,count = 0;
    int cvt;
    char *ss;

    minChar = 256;
    while((fgets(buf,256,f)) != NULL){
	if(*buf != '$'){
	    continue;
	}
	ss = lookup(buf+1,&result);
	if(result == character)   {
	    count++;
	    sscanf(ss,"%d",&cvt);
	    if (minChar != 32 && (cvt < minChar || cvt == 32))  {
		minChar = cvt;
	    }
	}

    }
    return count;
}
fontcvt(fin,fout,count)
FILE *fin,*fout;
int count;
/* Font Conversion filter */
{
	char buf[256],*ss;
	int result,foundchar = 0;
	char cc[256];
	register int i;
	fprintf(fout,"STARTFONT 2.1\nCOMMENT Created by fdbbdf\n");
	
	while((fgets(buf,256,fin)) != NULL){
		if(*buf != '$'){
		    if (foundchar == 2)
			hexout(buf,fout);
		    continue;
		}
		ss = lookup(buf+1,&result);
		switch(result){
		case -1:
			fprintf(stderr,"??? %s",buf);
			break;
		case magic:
		case familyname:
		case rotation:
		case facecodes:
		case  FontRepresentationType:
			/* This information (except magic) 
			should probably be saved as
			PROPERTIES, but I have no documentation
			or examples to show me the correct format */
			break;
		case fontname:
			fprintf(fout,"FONT %s",ss);
		otherwise  pointsize :
			sscanf(ss,"%d",&psize);
		otherwise  MaxNWtoOrigin :
			sscanf(ss,"%d,%d",&maxnwx,&maxnwy);
		otherwise  MaxNtoS :
			sscanf(ss,"%d,%d",&maxntosx,&maxntosy);
		otherwise  MaxWtoE :
			sscanf(ss,"%d,%d",&maxwtoex,&maxwtoey);
		otherwise  MaxWbase :
			sscanf(ss,"%d,%d",&maxwbx,&maxwby);
		otherwise  MaxNewline :
			sscanf(ss,"%d,%d",&maxnewlx,&maxnewly);
		otherwise Comment:
			{
			char *c;
			for(c = ss; *c != '\0'; c++) 
				if(*c == '\n') *c = ' ';
			strcat(comments,ss);
			}
		otherwise  NIcons :
			sscanf(ss,"%d",&nicons);
			for (i = 0; i < nicons; i++)
			    used[i] = 0;
		otherwise  character :
		        if(!HeaderPrinted) PrintHeader(fout,count);
			switch(foundchar){
			case 1:
				writechar(fout);
				fprintf(fout,"ENDCHAR\n");
				break;
			case 2:
				if(padflag)
					while(ypad--) hexout("",fout);
				fprintf(fout,"ENDCHAR\n");
			case 0:
				break;
			}
			spx=spy=orx=ory=bx=by = 0;
			sscanf(ss,"%d %s",&cvt,cc);
			if (used[cvt] == 0)  {
			    if(isprint(cvt + Coffset)) 
				fprintf(fout,"STARTCHAR %s\nENCODING -1 %d\n",cc,cvt + Coffset);
			    else fprintf(fout,"STARTCHAR ch-%d\nENCODING -1 %d\n",cvt + Coffset,cvt + Coffset);
			    used[cvt] = 1;
			    foundchar = 1;
			}
			else  {
			    fprintf(stderr, "Duplicate Entry %d\n", cvt);
			    fflush(stderr);
			    foundchar = 0;
			}
		otherwise  box :
			sscanf(ss,"%d,%d",&bx,&by);
		otherwise  origin :
			sscanf(ss,"%d,%d",&orx,&ory);
		otherwise  spacing :
			sscanf(ss,"%d,%d",&spx,&spy);
		otherwise  raster :
		        if (foundchar == 1)  {
		             writechar(fout);
		             foundchar = 2;
		        }
		otherwise  end :
			switch(foundchar){
			case 1:
				writechar(fout);
				fprintf(fout,"ENDCHAR\n");
				break;
			case 2:
				if(padflag)
					while(ypad--) hexout("",fout);
				fprintf(fout,"ENDCHAR\n");
			case 0:
				break;
			}
/*			printdummys(nicons,fout); */
			fprintf(fout,"ENDFONT\n");
		}
	}
}
