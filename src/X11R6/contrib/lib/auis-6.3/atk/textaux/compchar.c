/*********************************************************************** *\
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/textaux/RCS/compchar.c,v 1.19 1992/12/15 21:45:38 rr2b R6tape $";
#endif


 

#include <andrewos.h> /* sys/file.h and string(s).h */
#include <class.h>
#include <ctype.h>

#include <proctbl.ih>
#include <compchar.eh>
#include <textv.ih>
#include <text.ih>
#include <environ.ih>
#include <message.ih>
#include <observe.ih>
#include <frame.ih>
#include <framemsg.ih>
#include <style.ih>
#include <stylesht.ih>
#include <fontdesc.ih>
#include <buffer.ih>
#include <keymap.ih>
#include <keystate.ih>
#include <envrment.ih>
#include <pcompch.ih>
#include <im.ih>

static struct style *boldulined=NULL,*fixed=NULL;

static struct proctable_Entry *nop=NULL,*insertchar=NULL;

static procedure textview_SelfInsertCmd=NULL;

static char leftaccent[]="AEIOUYaeiouy";
static unsigned char leftaccentcodes[]={0xc1,0xc9,0xcd,0xd3,0xda,0xdd,0xe1,0xe9, 0xed,0xf3,0xfa,0xfd};

static char rightaccent[]="AEIOUaeiou";
static unsigned char rightaccentcodes[]={0xc0,0xc8,0xcc,0xd2,0xd9,0xe0,0xe8,0xec, 0xf2,0xf9};

static char hat[]="AEIOUaeiou";
static unsigned char hatcodes[]={0xc2,0xca,0xce,0xd4,0xdb,0xe2,0xea,0xee,0xf4,0xfb};

static char tilde[]="ANOano";
static unsigned char tildecodes[]={0xc3,0xd1,0xd5,0xe3,0xf1,0xf5};

static char umlaut[]="AEIOUaeiouy";
static unsigned char umlautcodes[]={0xc4,0xcb,0xcf,0xd6,0xdc,0xe4,0xeb,0xef,0xf6, 0xfc,0xff};

static char hex[]="0123456789abcdef";
static char octal[]="01234567";

/* ahotoi: used by compchar_insert to parse a hexadecimal or octal number depending on if base is 3 or 4. */
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
static unsigned char parsecode(ptr)
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

static void SelfInsertCmd(tv,code,style)
struct textview*tv;
unsigned char code;
char *style;
{
    struct text *t=(struct text *)textview_GetDataObject(tv);
    struct stylesheet *s=text_GetStyleSheet(t);
    struct style *st;
    struct environment *env;
    long pos=textview_GetDotPosition(tv),pos2;
    char *tmpl2,*tmpl;
    textview_SelfInsertCmd(tv,code);
    if(style[0]) {
	char *tstyle=(char *)malloc(strlen(style)+1);
	if(!tstyle) return;
	if(!s) {
	    message_DisplayString(tv,0,"No styles available.\n");
	    return;
	}
	pos2=textview_GetDotPosition(tv);
	if(pos2<=pos) return;
	
	strcpy(tstyle,style);
	tmpl=index(tstyle,',');
	if(tmpl) {
	    tmpl2=tmpl+1;
	    *tmpl='\0';
	    while(*tmpl2==' ') tmpl2++;
	} else tmpl2="symbol";

	st=stylesheet_Find(s,tstyle);
	if(!st) {
	    if(text_ReadTemplate(t,tmpl2,FALSE)) {
		message_DisplayString(tv,99,"Warning: proper template could not be loaded.");
		return;
	    }
	    st=stylesheet_Find(s,tstyle);
	}
	free(tstyle);
	if(!st) {
	    message_DisplayString(tv,0,"Couldn't find style.\n");
	    return;
	}
	env=text_AddStyle(t,pos,pos2-pos,st);
	if(!env) {
	    message_DisplayString(tv,0,"Couldn't add style to document\n");
	    return;
	}
	environment_SetStyle(env,FALSE,FALSE);
	text_NotifyObservers(t,0);
    }
}

/* compchar_insert: inserts an arbitrary character specified in an init file.  The following characters are treated specially when they occur as the first character of the argument:
      '|': inserts the character following it with the high bit set.
      'x': inserts the character whose code is given in hexadecimal.
      'o': inserts the character whose code is given in octal.
      'd': inserts the character whose code is given in decimal.
  If the first character of the argument isn't one of the above it is taken to be the decimal code for the character to be inserted. */
static void compchar_insert(tv,ptr)
struct textview *tv;
char *ptr;
{
    if((long)ptr<BADCHAR) {
	message_DisplayString(tv,0,"compchar-insert must be called with an argument from an initfile.");
	return;
    }
    textview_SelfInsertCmd(tv,parsecode(ptr));
}

/* compchar_modifier: if ptr isn't a valid character assumes it to be a pointer to an argument as provided by an .*init file.  In this case it inserts the character pointed to with the appropriate code as given by list and codes.  If ptr is a valid character then the character before the cursor is changed to the appropriate code as given by list and codes. */
static void compchar_modifier(tv,ptr,list,codes)
struct textview *tv;
char *ptr;
char *list;
unsigned char *codes;
{
    struct text *t=(struct text *)textview_GetDataObject(tv);
    /* if given an arg in a  .*init insert it with an accent */
    if((long)ptr>MAXCHAR) {
	unsigned char *i=(unsigned char *)index(list,*ptr);
	if(!i) {
	    message_DisplayString(tv,0,"Character not available.");
	    return;
	}
	textview_SelfInsertCmd(tv,codes[i-(unsigned char *)list]);
    } else {
	long pos=textview_GetDotPosition(tv)-1;
	unsigned char *i;
	if(pos<0) {
	    message_DisplayString(tv,0,"No character before the cursor.");
	    return;
	}
	i=(unsigned char *)index(list,text_GetChar(t,pos));
	if(!i) {
	    message_DisplayString(tv,0,"Character not available.");
	    return;
	}
	text_ReplaceCharacters(t,pos, 1, codes+(i-(unsigned char *)list),1);
    }
    text_NotifyObservers(t,observable_OBJECTCHANGED);
}

struct arock {
    struct textview *tv;
    char *list;
    unsigned char *codes;
    procedure oldoverride;
    long oldrock;
};

/* doafter: the keystate override function for applying a umlaut, hat, tilde, etc to the next character typed;
  */
static enum keymap_Types doafter(rock,key,ppe,prock)
struct arock *rock;
char key;
struct proctable_Entry **ppe;
long *prock;
{
    struct textview *tv=rock->tv;
    unsigned char *i=(unsigned char *)index(rock->list,key);
    if(!i) {
	keystate_SetOverride(tv->keystate,rock->oldoverride, rock->oldrock);
	free(rock);
	message_DisplayString(tv,0,"No such character.");
	*prock=0;
	*ppe=nop;
	return keymap_Proc;
    }
    keystate_SetOverride(tv->keystate,rock->oldoverride, rock->oldrock);
    free(rock);
    *prock=rock->codes[i-(unsigned char *)rock->list];
    *ppe=insertchar;
    return keymap_Proc;
}

/* after: the function which actually handles the work for all the compchar_*after functions, sets doafter as the override function on the textviews keystate, if the override function is already doafter it cancels the operation and if the .*init file so indicated will insert a character. */
static void after(tv,list,codes,ch)
struct textview *tv;
char *list;
unsigned char *codes;
char *ch;
{
    struct arock *rock;
    rock=(struct arock *)malloc(sizeof(struct arock));
    if(!rock) return;
    rock->tv=tv;
    rock->list=list;
    rock->codes=codes;
    keystate_GetOverride(tv->keystate,&rock->oldoverride, &rock->oldrock);
    if(rock->oldoverride==(procedure)doafter) {
	free(rock);
	free((struct arock *)rock->oldrock);
	if((long)ch>MAXCHAR) textview_SelfInsertCmd(tv,*ch);
	keystate_SetOverride(tv->keystate,NULL,0);
	return;
    }
    keystate_SetOverride(tv->keystate,(procedure)doafter ,(long)rock);
}
/*
  compchar_leftaccentafter:
  compchar_rightaccentafter:
  compchar_umlautafter:
  compchar_tildeafter:
  compchar_hatafter:
      front-end functions to pass the appropriate list and codes
  arguments to after. */

static void compchar_leftaccentafter(tv,rock)
struct textview *tv;
char *rock;
{
    after(tv,leftaccent,leftaccentcodes,rock);
}

static void compchar_rightaccentafter(tv,rock)
struct textview *tv;
char *rock;
{
    after(tv,rightaccent,rightaccentcodes,rock);
}

static void compchar_umlautafter(tv,rock)
struct textview *tv;
char *rock;
{
    after(tv,umlaut,umlautcodes,rock);
}

static void compchar_tildeafter(tv,rock)
struct textview *tv;
char *rock;
{
    after(tv,tilde,tildecodes,rock);
}

static void compchar_hatafter(tv,rock)
struct textview *tv;
char *rock;
{
    after(tv,hat,hatcodes,rock);
}

/*
  compchar_leftaccent:
  compchar_rightaccent:
  compchar_umlaut:
  compchar_tilde:
  compchar_hat:
      front-end functions to pass the appropriate list and codes
  arguments to compchar_modifier. */
 
static void compchar_leftaccent(tv,ptr)
struct textview *tv;
char *ptr;
{
    compchar_modifier(tv,ptr,leftaccent,leftaccentcodes);
}

static void compchar_rightaccent(tv,ptr)
struct textview *tv;
char *ptr;
{
    compchar_modifier(tv,ptr,rightaccent,rightaccentcodes);
}

static void compchar_umlaut(tv,ptr)
struct textview *tv;
char *ptr;
{
    compchar_modifier(tv,ptr,umlaut,umlautcodes);
}


static void compchar_tilde(tv,ptr)
struct textview *tv;
char *ptr;
{
    compchar_modifier(tv,ptr,tilde,tildecodes);
}

static void compchar_hat(tv,ptr)
struct textview *tv;
char *ptr;
{
    compchar_modifier(tv,ptr,hat,hatcodes);
}


/* composework: called from pcompch_EnumerateComposites with ROCK being the textview in which the compose operation began and C being the composite to be checked.  If the current composite is equal to the user's entry the appropriate character is inserted in the text and a value of one is returned to terminate the search.  Otherwise it simply returns 0. */ 
static struct composites *composework(key,c,exts)
char key;
struct composites *c;
char *exts;
{
   if(strcmp((char *) c->exts,exts)) return NULL;
   else return c;
}

struct helpRock {
    procedure HelpWork;
    struct text *text;
    long rock;
    char *partial;
};

/* match: called from compchar_EnumerateComposites with ROCK being a struct helpRock as defined above.  If the user's entry up until this point matches one of the composites of KEY then that composite will be displayed in the help buffer. */
static long match(key,c,rock)
char key;
struct composites *c;
long rock;
{
    struct helpRock *h=(struct helpRock *)rock;
    unsigned char buf[256];
    unsigned char ch[2],*t;
    if(*(h->partial)&&strncmp(h->partial+1,(char *) c->exts, strlen(h->partial+1))) return 0;
    ch[0]=(unsigned char)c->code;
    ch[1]='\0';
    buf[0]=(unsigned char)key;
    strcpy((char *) buf+1,(char *) c->exts);
    strcat((char *) buf,"\t    ");
    strcat((char *) buf, (char *) ch);
    strcat((char *) buf,"\t\t");
    (*h->HelpWork)(h->rock,message_HelpGenericItem,buf,NULL);
    if(c->style[0]) {
	struct style *st; 
	unsigned char *tmpl;
	strcpy((char *) buf,(char *) c->style);
	tmpl=(unsigned char *)index((char *) buf,',');
	if(tmpl) {
	    t=tmpl+1;
	    *tmpl='\0';
	    while(*t==' ') t++;
	} else t=(unsigned char *)"symbol";
	text_ReadTemplate(h->text,t,FALSE);
	st=stylesheet_Find(text_GetStyleSheet(h->text),buf);
	if(st) {
	    struct environment *env=text_AddStyle(h->text, text_GetLength(h->text)-3, 1,st);
	    if(env) environment_SetStyle(env,FALSE,FALSE);
	}
	if(tmpl) *tmpl=',';
    } else buf[0]='\0';
    strcat((char *) buf,"\n");
    (*h->HelpWork)(h->rock,message_HelpGenericItem,buf,NULL);
    return 0;
}

/* helpProc: called by frameview__Help after having been set as the help procedure for a message_AskForStringCompleted.  Initializes data needed by match for it to see if a composite matches the user's entry so far and procedes to use pcompch_EnumerateComposites to display a list of available composites in the help buffer. */
static void helpProc(partial,myrock,HelpWork,rock)
char *partial;
struct helpRock *myrock;
procedure HelpWork;
long rock;
{
    int i;
    struct text *t=myrock->text;
    text_ReadTemplate(myrock->text,"default",FALSE);
    (*HelpWork)(rock,
		    message_HelpGenericItem,
		    "Entry\tCharacter\tStyle\n",
		    NULL);
    
    myrock->HelpWork=HelpWork;
    myrock->rock=rock;
    myrock->partial=partial;
    if(*partial) (void)pcompch_EnumerateComposites(*partial,
					   (procedure)match,
					   (long)myrock);
    else for(i=1;i<=MAXCHAR;i++) {
	(void)pcompch_EnumerateComposites(i,
					   (procedure)match,
					   (long)myrock);
    }
    text_SetGlobalStyle(t,fixed);
    text_AddStyle(t,0,5,boldulined);
    text_AddStyle(t,6,9,boldulined);
    text_AddStyle(t,16,5,boldulined);
}

/* compchar_compose: if an argument is given for this function in a .*init file then it will be taken as a string designating the composite character to be inserted. Otherwise this function prompts the user to specify a character to insert in the text.  Typing a '?' will cause a list of available characters to be displayed. */
static void compchar_compose(tv,ptr)
struct textview *tv;
char *ptr;
{
    char buf[1024];
    struct composites *c;
    struct helpRock myrock;
    int result;
    struct framemessage *fmsg= (struct framemessage *)textview_WantHandler(tv,"message");
    if((long)ptr>MAXCHAR) {
	c=(struct composites *)pcompch_EnumerateComposites(*ptr, (procedure)composework,(long)ptr+1);
	if(!c) {
	    message_DisplayString(tv,0,"No such composition found.\n");
	    return;
	}
	SelfInsertCmd(tv,c->code,c->style);
	return;
    }
    if(fmsg) {
	struct buffer *b=frame_GetHelpBuffer(fmsg->frame);
	myrock.text=(struct text *)buffer_GetData(b);
    } else myrock.text=NULL;
    result=message_AskForStringCompleted(tv,
      					      0,
					      "Character: " ,
					      NULL, /* no default */
					      buf,
					      sizeof(buf),
					      NULL, /* no special keymap */
					      NULL,/* no completion */
					      helpProc, /* will give help */
					      (long)&myrock,
					      message_NoInitialString);
    if(result<0) {
	message_DisplayString(tv,0,"Character composition cancelled.");
	return;
    }
    c=(struct composites *)pcompch_EnumerateComposites(*buf, (procedure)composework,(long)buf+1);
    if(!c) {
	message_DisplayString(tv,0,"No such composition found.\n");
	return;
    }
    SelfInsertCmd(tv,c->code,c->style);
}

/* YARock: Yet Another Rock for keeping track of information as a composition is being typed. */
struct YARock{
    struct textview *tv;
    procedure oldoverride;
    long oldrock;
    struct composites *c;
    unsigned short key;
    unsigned char exts[9];
    int count;
    unsigned char possibilities[512];
};

/* keywork: used by handle key to determine what the possible matches for the current composition are. */
static unsigned char keywork(key,c,rock)
char key;
struct composites *c;
struct YARock *rock;
{
    char buf[6];
    if(strncmp((char *) c->exts,(char *) rock->exts,strlen((char *) rock->exts))) return 0;
    if(!strcmp((char *) c->exts,(char *) rock->exts)) {
	rock->c=c;
	return c->code;
    }
    if(strlen((char *) rock->possibilities)+4<sizeof(rock->possibilities)) {
	sprintf(buf,"%s%c-%c, ",c->style[0]?"*":"",c->code,c->exts[strlen((char *) rock->exts)]);
	strcat((char *) rock->possibilities,buf);
    }
    rock->count++;
    return 0;
}

/* handlekey: add a key to the composition in progress, if the composition is then completely specified return the appropriate code, otherwise list the current possibilities. */
static long handlekey(rock,key)
struct YARock *rock;
char key;
{
    char buf[16];
    unsigned char c;
    buf[1]='\0';
    if(key==127||key==8) {
	if(!*rock->exts) rock->key=BADCHAR;
	else {
	    rock->exts[strlen((char *) rock->exts)-1]='\0';
	}
    } else {
	if(rock->key==BADCHAR) rock->key=key;
	else if(strlen((char *) rock->exts)<sizeof(rock->exts)-1) {
	    buf[0]=key;
	    strcat((char *) rock->exts,buf);
	}
    }
    rock->count=0;
    if(rock->key==BADCHAR) {
	strcpy(rock->possibilities,"Initial character: ");
	return 0;
    } else {
	strcpy((char *) rock->possibilities,"Possible completions: "); 
	c=(unsigned char)pcompch_EnumerateComposites(rock->key, (procedure)keywork,(long)rock);
	if(c) return c;
	else {
	    int len=strlen((char *) rock->possibilities);
	    if(len>=2) {
		if(rock->possibilities[len-2]==',') rock->possibilities[len-2]='\0';
	    }
	    if(rock->count==0) return -1;
	    else return 0;
	}
    }
}

/* docomposing: intercepts lookups on the textview keystate to see if a valid composition is taking place, if not it removes the override on the textviews keystate and frees the rock.  Otherwise It checks to see if the composition has been completely specified yet.  If it has the appropriate character is inserted in the text. */
static enum keymap_Types docomposing(rock,key,ppe,prock)
struct YARock *rock;
char key;
struct proctable_Entry **ppe;
long *prock;
{
    enum keymap_Types code;
    struct keystate  *ks=rock->tv->keystate;
    long handle=handlekey(rock,key);
    message_DisplayString(rock->tv,0,"");
    if(handle>0) {
	keystate_SetOverride(ks,rock->oldoverride,rock->oldrock);
	SelfInsertCmd(rock->tv,handle,rock->c->style);
	free(rock);
	*ppe=nop;
	*prock=0;
	return keymap_Proc;
    }
    if(handle>=0) {
	*ppe=nop;
	*prock=(long) rock->possibilities;
	return keymap_Proc;
    }
    if(rock->oldoverride)
	code=(enum keymap_Types) (*rock->oldoverride)(rock->oldrock,key,ppe,prock);
    else if(!ks->curMap) return keymap_Empty;
    else code=keymap_Lookup(ks->curMap,key,ppe,prock);
    keystate_SetOverride(ks,rock->oldoverride,rock->oldrock);
    free(rock);
    return code;
}

/* compchar_nop: semi bogus proctable function so that the keys taken by docomposing don't have any other side-effects, except perhaps to display the list of possible completions for the composition */
static void compchar_nop(tv, possibilities)
struct textview *tv;
char *possibilities;
{
    if((long)possibilities>MAXCHAR) {
	message_DisplayString(tv, 0, possibilities);
    }
}

/* compchar_compose2: basically the same as compchar_compose except that '?' help is not available and no return is needed at the end of the composed character. However, continuous help is given in the form of information in the message line.  Automatically notices the end of a composition. */
static void compchar_compose2(tv,ch)
struct textview *tv;
char *ch;
{
    struct YARock *rock;
    rock=(struct YARock *)malloc(sizeof(struct YARock));
    rock->tv=tv;
    rock->key=MAXCHAR+1;
    rock->exts[0]='\0';
    rock->c=NULL;
    keystate_GetOverride(tv->keystate,&rock->oldoverride, &rock->oldrock);
    if(rock->oldoverride==(procedure)docomposing) {
	free(rock);
	free((struct YARock *)rock->oldrock);
	if((long)ch>MAXCHAR) textview_SelfInsertCmd(tv,*ch);
	keystate_SetOverride(tv->keystate,NULL,0);
	message_DisplayString(tv,0,"");
	return;
    }
    keystate_SetOverride(tv->keystate,(procedure)docomposing ,(long)rock);
    message_DisplayString(tv,0,"Initial character: ");
}


struct SARock {
    struct textview *tv;
    boolean ask;
};

static long doatkreplacement(text,pos,ascii,r)
struct text *text;
long pos;
char *ascii;
struct SARock *r;
{
    int c,dpos;
    textview_SetDotPosition(r->tv, pos);
    textview_SetDotLength(r->tv, 1);
    textview_FrameDot(r->tv, pos);
    do {
	if(r->ask) {
	    textview_Update(r->tv);
	    c=im_GetCharacter(textview_GetIM(r->tv));
	} else c=' ';
	if(c=='?') {
	    message_DisplayString(r->tv,0,"One of 'q'-quit, 'n'-next, '!'-replace all, or ' '-replace and continue.");
	}
    } while (c=='?');
    switch(c) {
	case '\007':
	case '\003':
	case 'q':
	case EOF:
	    pos=EOF;
	    break;
	case 'n':
	    pos++;
	    break;
	case '!':
	    r->ask=FALSE;
	default:
	    text_AlwaysReplaceCharacters(text, pos, 1, ascii, strlen(ascii));
	    if(c=='.') return EOF;
	    pos+=strlen(ascii);
    }
    dpos=pos>=text_GetLength(text)?pos-1:pos;
    textview_SetDotPosition(r->tv,dpos);
    textview_SetDotLength(r->tv,0);
    textview_FrameDot(r->tv,dpos);
    return pos;
}

static void compchar_ATKToASCII(tv,rock)
struct textview *tv;
long rock;
{
    long pos,len;
    struct SARock r;
    r.tv=tv;
    if(rock<BADCHAR) r.ask=TRUE;
    else r.ask=FALSE;
    pos=textview_GetDotPosition(tv);
    pcompch_ATKToASCII(textview_GetDataObject(tv), pos, textview_GetDotLength(tv), doatkreplacement, &r);
    len=text_GetLength((struct text *)textview_GetDataObject(tv));
    if(pos>=len) pos=len-1;
    textview_SetDotPosition(tv,pos);
    textview_SetDotLength(tv,0);
    textview_FrameDot(tv,pos);
    
}


static long doasciireplacement(text,pos,ch,ascii,r)
struct text *text;
long pos;
unsigned char ch;
char *ascii;
struct SARock *r;
{
    int c;
    textview_SetDotPosition(r->tv, pos);
    textview_SetDotLength(r->tv, strlen(ascii));
    textview_FrameDot(r->tv, pos);
    do {
	if(r->ask) {
	    textview_Update(r->tv);
	    c=im_GetCharacter(textview_GetIM(r->tv));
	} else c=' ';
	if(c=='?') {
	    message_DisplayString(r->tv,0,"One of 'q'-quit, 'n'-next, '!'-replace all, or ' '-replace and continue.");
	}
    } while (c=='?');
    switch(c) {
	case '\007':
	case '\003':
	case 'q':
	case EOF:
	    pos=EOF;
	    break;
	case 'n':
	    pos+=strlen(ascii);
	    break;
	case '!':
	    r->ask=FALSE;
	default:
	    text_AlwaysReplaceCharacters(text, pos, strlen(ascii), &ch, 1);
	    if(c=='.') return EOF;
	    pos++;
    }
    if(pos>=text_GetLength(text)) pos=text_GetLength(text)-1;
    textview_SetDotPosition(r->tv,pos);
    textview_SetDotLength(r->tv,0);
    textview_FrameDot(r->tv,pos);
    return pos;
}

static void compchar_ASCIIToATK(tv,rock)
struct textview *tv;
long rock;
{
    long pos,len;
    struct SARock r;
    r.tv=tv;
    if(rock<BADCHAR) r.ask=TRUE;
    else r.ask=FALSE;
    pos=textview_GetDotPosition(tv);
    pcompch_ASCIIToATK(textview_GetDataObject(tv), pos, textview_GetDotLength(tv), doasciireplacement, &r);
    len=text_GetLength((struct text *)textview_GetDataObject(tv));
    if(pos>=len) pos=len-1;
    textview_SetDotPosition(tv,pos);
    textview_SetDotLength(tv,0);
    textview_FrameDot(tv,pos);
    
}

boolean compchar__InitializeClass(ClassID)
struct classheader *ClassID;
{
    struct classinfo *textviewtype = class_Load("textview");
    
    if(!class_Load("pcompch")) {
	fprintf(stderr,"Unable to load pcompch.\n");
	return FALSE;
    }
    /* Get the styles to use in the help buffer. */
    boldulined=style_New();
    if(!boldulined) {
	fprintf(stderr,"Unable to get bold underlined text style.\n");
	return FALSE;
    }
    fixed=style_New();
    if(!fixed) {
	style_Destroy(boldulined);
	fprintf(stderr,"Unable to get fixed-width text style.\n");
	return FALSE;
    }
    
    style_AddNewFontFace(fixed,fontdesc_Fixed);
    style_SetFontFamily(fixed,"andytype");
    
    style_AddNewFontFace(boldulined,fontdesc_Bold);
    style_AddUnderline(boldulined);
    
    /* Code from swedish.c to get the textview function to insert a single character in the text. */
    if((insertchar = proctable_Lookup("textview-self-insert")) != NULL && proctable_Defined(insertchar) ){
	textview_SelfInsertCmd = proctable_GetFunction(insertchar) ;
    } else {
	fprintf(stderr,"Couldn't get textview-self-insert procedure.\n");
	return FALSE;
    }

    nop=proctable_DefineProc("compchar-nop",compchar_nop, textviewtype,NULL,"nop for use with compchar.");


    proctable_DefineProc("compchar-ASCIIToATK",compchar_ASCIIToATK, textviewtype,NULL,"map local ASCII conventions to ATK ISO characters"); 
    proctable_DefineProc("compchar-ATKToASCII",compchar_ATKToASCII, textviewtype,NULL,"map ATK ISO characters to local ASCII");
    proctable_DefineProc("compchar-compose2", compchar_compose2, textviewtype,NULL,"improved compchar-compose.");
    proctable_DefineProc("compchar-compose",compchar_compose, textviewtype,NULL,"start the composition of a character");
    
    proctable_DefineProc("compchar-acuteaccent-after", compchar_leftaccentafter,textviewtype,NULL,"put a left accent over the next character");
    proctable_DefineProc("compchar-graveaccent-after", compchar_rightaccentafter,textviewtype,NULL,"put a right accent over the next character");
    proctable_DefineProc("compchar-circumflex-after", compchar_hatafter,textviewtype,NULL,"put a hat over the next character");
    proctable_DefineProc("compchar-tilde-after", compchar_tildeafter,textviewtype,NULL,"put a tilde over the next character");
    proctable_DefineProc("compchar-umlaut-after", compchar_umlautafter,textviewtype,NULL,"put an umlaut over the next character");
   
    proctable_DefineProc("compchar-acuteaccent",compchar_leftaccent, textviewtype,NULL,"add a left accent to the character to the left of the cursor.");
    proctable_DefineProc("compchar-graveaccent", compchar_rightaccent, textviewtype,NULL,"add a right accent to the character to the left of the cursor.");
    proctable_DefineProc("compchar-circumflex",compchar_hat, textviewtype,NULL,"add a hat to the character to the left of the cursor.");
    proctable_DefineProc("compchar-tilde",compchar_tilde, textviewtype,NULL,"add a tilde to the character to the left of the cursor.");
    proctable_DefineProc("compchar-umlaut",compchar_umlaut, textviewtype,NULL,"add a umlaut to the character to the left of the cursor.");
       
    proctable_DefineProc("compchar-insert",compchar_insert, textviewtype,NULL,"inserts an arbitrary character specified in an initfile.");

    return TRUE;
}
