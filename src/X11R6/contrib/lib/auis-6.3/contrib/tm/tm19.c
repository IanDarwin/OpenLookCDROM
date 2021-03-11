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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/tm/RCS/tm19.c,v 2.8 1992/12/15 21:57:11 rr2b R6tape $";
#endif


 

/*
 * subclass of termulator implementing h19 escape sequences
 */

#include <stdio.h>
#include <class.h>

#include <tm19.eh>

/* this is slightly more complicated than necessary to handle an entire
 * escape sequence in one pass, but faster...
 */
static char *escapeChar(self,buf,len)
struct tm19 *self;
char *buf;
int len;
{
    boolean cont=TRUE;
    char *pbuf=tm19_GetParseBuf(self);
    int num,plen=tm19_GetParseBufLen(self);

#define Xpos tm19_GetX(self)
#define Ypos tm19_GetY(self)
#define Width tm19_GetWidth(self)
#define Height tm19_GetHeight(self)

    do{ /* process one or more escape sequences */

	/* fold some simple sequences if possible */
	if(plen==0){
	    num=1;
	    while(len>3 && buf[2]=='\033' && buf[3]==buf[1]){
		buf+=2;
		len-=2;
		num++;
	    }
	}else
	    num=self->foldCount;
/*
	if(num>1)
	    fprintf(stderr,"tm19: ESC-%c occured %d times.\n",buf[1],num);
*/
	do{ /* process one escape sequence */
	    cont=FALSE; /* punt unless told otherwise */
	    pbuf[plen++]= *buf++;
	    len--;
	    switch(plen){
		case 1:
		case 3:
		    cont=TRUE;
		    break;
		case 2:
		    switch(pbuf[1]){
			case 'H': /* home */
			    tm19_GotoXY(self,0,0);
			    break;
			case 'C': /* cursor forward */
			    tm19_GotoXY(self,Xpos+num,Ypos);
			    break;
			case 'D': /* cursor backward */
			    tm19_Backspace(self,num);
			    break;
			case 'B': /* cursor down */
			    tm19_GotoXY(self,Xpos,Ypos+num);
			    break;
			case 'A': /* cursor up */
			    tm19_GotoXY(self,Xpos,Ypos-num);
			    break;
			case 'I': /* reverse index */
			    num-=Ypos;
			    if(num<=0)
				tm19_GotoXY(self,Xpos,-num);
			    else{
				tm19_GotoXY(self,0,0);
				tm19_InsertLines(self,num);
			    }
			    break;
			case 'n': /* report cursor position */
			    {
			    char scratchBuf[80];
			    sprintf(scratchBuf,"\033Y%c%c",Ypos+' ',Xpos+' ');
			    tm19_ProcessInput(self,scratchBuf,4);
			    }
			    break;
			case 'j': /* save cursor position */
			    self->savedX=Xpos;
			    self->savedY=Ypos;
			    break;
			case 'k': /* restore cursor position */
			    tm19_GotoXY(self,self->savedX,self->savedY);
			    break;
			case 'Y': /* direct cursor positioning */
			    cont=TRUE;
			    break;
			case 'E': /* clear screen */
			    tm19_ClearLines(self,0,Height);
			    tm19_GotoXY(self,0,0);
			    break;
			case 'b': /* erase to beginning of screen */
			    tm19_ClearLines(self,0,Ypos-1);
			    tm19_ClearChars(self,0,Xpos+1);
			    break;
			case 'J': /* erase to end of screen */
			    tm19_ClearChars(self,Xpos,Width-Xpos);
			    tm19_ClearLines(self,Ypos,Height-Ypos);
			    break;
			case 'l': /* clear current line */
			    tm19_ClearLines(self,Ypos,1);
			    break;
			case 'K': /* clear to end of line */
			    tm19_ClearChars(self,Xpos,Width-Xpos);
			    break;
			case 'o': /* clear to beginning of line */
			    tm19_ClearChars(self,0,Xpos+1);
			    break;
			case 'L': /* insert line */
			    tm19_InsertLines(self,num);
			    break;
			case 'M': /* delete line */
			    tm19_DeleteLines(self,num);
			    break;
			case '@': /* enter insert mode */
			    tm19_SetInsert(self,TRUE);
			    break;
			case 'O': /* exit insert mode */
			    tm19_SetInsert(self,FALSE);
			    break;
			case 'N': /* delete character */
			    tm19_DeleteChars(self,num);
			    break;
			case 'p': /* standout mode */
			    tm19_SetStandout(self,TRUE);
			    break;
			case 'q': /* standout mode off */
			    tm19_SetStandout(self,FALSE);
			    break;
		    }
		    break;
		case 4:
		    /* this should happen only with the cm sequence */
		    tm19_GotoXY(self,pbuf[3]-' ',pbuf[2]-' ');
	    }
	}while(cont && len>0);

	if(len==0)
	    break;

	plen=0;
    }while(*buf=='\033');

    if(cont){
	self->foldCount=num;
	tm19_SetParseBufLen(self,plen);
	tm19_SetParseState(self,escapeChar);
    }else{
	tm19_SetParseBufLen(self,0);
	tm19_SetParseState(self,NULL);
    }
	
    return buf;
}

boolean tm19__InitializeObject(classID,self)
struct classheader *classID;
struct tm19 *self;
{
    tm19_SetEscape(self,'\033',escapeChar);
    self->savedX=0;
    self->savedY=0;
    return TRUE;
}

static char tcbuf[2000]; /* stupid HC can't handle statics inside functions */

char *tm19__GetTermcap(self)
struct tm19 *self;
{
    sprintf(tcbuf,
	    "tm19|termulator h19 emulation:\
cr=^M:nl=^J:al=5*\\EL:le=^H:bs:cd=\\EJ:ce=\\EK:cl=\\EE:\
cm=\\EY%%+ %%+ :co#%d:dc=\\EN:dl=5*\\EM:do=\\EB:ei=\\EO:\
ho=\\EH:im=\\E@:li#%d:mi:nd=\\EC:ms:ta=^I:pt:sr=\\EI:up=\\EA:\
so=\\Ep:se=\\Eq:",tm19_GetWidth(self),tm19_GetHeight(self));
    return tcbuf;
}

char *tm19__GetTerm(self)
{
    return "tm19";
}
