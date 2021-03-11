/* ********************************************************************** */

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/util/RCS/headrtv.c,v 1.9 1993/12/07 02:32:25 rr2b Exp $";
#endif

/*         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
#include <class.h>
#include <andrewos.h>
#include <text.ih>
#include <textv.ih>
#include <observe.ih>
#include <view.ih>
#include <fontdesc.ih>
#include <style.ih>
#include <graphic.ih>
#include <bind.ih>
#include <keymap.ih>
#include <keystate.ih>
#include <proctbl.ih>
#include <message.ih>
#include <environ.ih>
#include <lpair.ih>
#include <rect.h>

#include <header.ih>
#include <headrtv.eh>

#include <ctype.h>

#include <pcompch.ih>

#define Data(self)  ((struct header *)headrtv_GetDataObject(self))
#define View(self) ((struct view *)self)
#define Text(self,x) (Data(self)->texts[x])
#define Textv(self,x) ((self)->textvs[x])

#define BASEYEAR 1900

static struct keymap *newKeymap;
static char **hvars=NULL;
static int hvarcount=0;

typedef char *strings[3];
static strings strs[]={
    {"LT","CT","RT"},
    {"LB","CB","RB"}
};

static char *keywords[]={
    "24hourtime",
    "blank",
    "date",
    "Edate",
    "Fdate",
    "day",
    "Fday",
    "month",
    "Fmonth",
    "page",
    "shortyear",
    "time",
    "timeofday",
    "year"
};

static char *months[]={
    "January",
    "February",
    "March",
    "April",
    "May",
    "June",
    "July",
    "August",
    "September",
    "October",
    "November",
    "December"
};

static char *fmonths[]={
    "Janvier",
    "Fevrier",
    "Mars",
    "Avril",
    "Mai",
    "Juin",
    "Juillet",
    "Aout",
    "Septembre",
    "Octobre",
    "Novembre",
    "Decembre"
};

static char *day[]={
    "Sunday",
    "Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday",
    "Saturday"
};

static char *fday[]={
    "Dimanche",
    "Lundi",
    "Mardi",
    "Mercredi",
    "Jeudi",
    "Vendredi",
    "Samedi"
};

struct tm *GetCurrentTime() {
    long t=time(0);
    return localtime(&t);
}

void TwentyFourHourTime(file)
FILE *file;
{
    struct tm *lt=GetCurrentTime();
    fprintf(file,"%d:%s%d",lt->tm_hour, (lt->tm_min>9)?"":"0",lt->tm_min);
}

void Blank(file)
FILE *file;
{
    fprintf(file, "");
}

void Date(file)
FILE *file;
{
    struct tm *lt=GetCurrentTime();
    fprintf(file,"%s %d, %d", months[lt->tm_mon], lt->tm_mday,BASEYEAR+lt->tm_year);
}

void Date2(file)
FILE *file;
{
    struct tm *lt=GetCurrentTime();
    fprintf(file," %d %s %d",lt->tm_mday, months[lt->tm_mon], BASEYEAR+lt->tm_year);
}

void FDate(file)
FILE *file;
{
    struct tm *lt=GetCurrentTime();
    fprintf(file," %d %s %d",lt->tm_mday, fmonths[lt->tm_mon], BASEYEAR+lt->tm_year);
}

void Day(file)
FILE *file;
{
    struct tm *lt=GetCurrentTime();
    fputs(day[lt->tm_wday],file);
}

void FDay(file)
FILE *file;
{
    struct tm *lt=GetCurrentTime();
    fputs(fday[lt->tm_wday],file);
}

void Month(file)
FILE *file;
{
    struct tm *lt=GetCurrentTime();
    fputs(months[lt->tm_mon],file);
}

void FMonth(file)
FILE *file;
{
    struct tm *lt=GetCurrentTime();
    fputs(fmonths[lt->tm_mon],file);
}

void Page(file)
FILE *file;
{
    fprintf(file,"\\\\n%%");
}

void ShortYear(file)
FILE *file;
{
    struct tm *lt=GetCurrentTime();
    fprintf(file,"%d",lt->tm_year);
}

void TimeofDay(file)
FILE *file;
{
    struct tm *lt=GetCurrentTime();
    boolean am=TRUE;
    if(lt->tm_hour>12) {
	am=FALSE;
	lt->tm_hour-=12;
    }
    else if (lt->tm_hour == 12) am = FALSE;

    fprintf(file,"%d:%s%d %s",lt->tm_hour, (lt->tm_min>9)?"":"0",lt->tm_min,am?"AM":"PM");
}

void Time(file)
FILE *file;
{
    long t=time(0);
    char *ct=ctime(&t),*i;
    i=index(ct,'\n');
    if(i) *i='\0';
    fputs(ct,file);
}

void Year(file)
FILE *file;
{
    struct tm *lt=GetCurrentTime();
    fprintf(file,"%d",BASEYEAR+lt->tm_year);
}
static procedure procs[]={
    (procedure)TwentyFourHourTime,
    (procedure)Blank,
    (procedure)Date,
    (procedure)Date2,
    (procedure)FDate,
    (procedure)Day,
    (procedure)FDay,
    (procedure)Month,
    (procedure)FMonth,
    (procedure)Page,
    (procedure)ShortYear,
    (procedure)Time,
    (procedure)TimeofDay,
    (procedure)Year
};

#define NKEYWORDS (sizeof(keywords)/sizeof(char *))

#if 0
static int findincommalist(list,sn)
char *list,*sn;
{
    int i,count=0;
    char buf[256],*p;
    for(i=0;i<strlen(list);i++,count++) {
	p=buf;
	while(list[i] && list[i]!=',' && p-buf<256) *(p++)=list[i],i++;
	*p='\0';
	if(!strcmp(buf,sn)) return count;
    }
    return -1;
}
#endif

#define DEFAULTHEADERVARS 2
static void InstallHeaderVariables()
{
    char *headervars, *p;
    int count=0,i;

    headervars = environ_GetProfile("headervars");
    if (headervars) {
	p=headervars;
	while(p && *p) {
	    count++;
	    p=index(p+1,',');
	}
	if(DEFAULTHEADERVARS+count>9) {
	    fprintf(stderr,"Warning too many header variables maximum allowable is 10.\n");
	    return;
	}

	p=(char *)malloc(strlen(headervars)+1);
	if(!p) return;
	strcpy(p,headervars);
    } else {
	p = malloc(1);
	if (!p) return;
	p[0] = '\0';
    }
    hvars=(char **)malloc(sizeof(char *)*(DEFAULTHEADERVARS+count));
    if(!hvars) return;
    hvars[0]="chapter";
    hvars[1]="section";
    for(i=0;i<count;i++) {
	hvars[DEFAULTHEADERVARS+i]=p;
	while(*p && *p!=',') p++;
	*p++='\0';
    }
    hvarcount=DEFAULTHEADERVARS+count;
}
    
	
static void PrintLine(fp,string)
FILE *fp;
char *string;
{
    long pos,len=strlen(string),c,index=0;
    char *ptr;
    boolean keyword=FALSE,pass=FALSE;
    for(pos=0;pos<len;pos++) {
	c=string[pos];
	if(keyword) {
	    keyword=FALSE;
	    if(c=='\\') pass=!pass;
	    if(c=='$') fputc(c,fp);
	    else {
		for(index=NKEYWORDS-1;index>=0;index--) {
		    ptr = string + pos;
		    if(*keywords[index]==c && !strncmp(ptr,keywords[index], strlen(keywords[index]))) {
			if(procs[index]) procs[index](fp);
			pos+=strlen(keywords[index])-1;
			break;
		    }
		}
		if(index<0) {
		    for(index=hvarcount-1;index>=0;index--) {
			ptr = string + pos;
			if(*hvars[index]==c && !strncmp(ptr,hvars[index], strlen(hvars[index]))) {
			    fprintf(fp,"\\*(H%d\n",index);
			    pos+=strlen(hvars[index])-1;
			    break;
			}
		    }

		}
	    }
	} else {
	    switch(c) {
		case '\n':
		    break;
		case '\\':
		    if(!pass) fprintf(fp,"\\\\\\\\");
		    else fputc(c,fp);
		    break;
		case '$':
		    keyword=TRUE;
		    break;
		default:
		    fputc(c,fp);
	    }
	}
    }
}

static char *headrtv_GetInput(textobj)
struct text *textobj;
{
    int len,pos;
    char *string;

    len = text_GetLength(textobj);
    pos = text_GetFence(textobj);
    len = len - pos;
    string = (char *) malloc(len + 1);

    text_CopySubString(textobj, pos, len, string, TRUE);
    
    return string;
}

void headrtv__Print(self,file,processor,finalFormat, topLevel)
struct headrtv *self;
FILE *file;
char *processor;
char *finalFormat;
boolean topLevel;
{
    char *string;
    int i;

    if(!strcmp(processor,"troff")) {
	for(i=header_ltext;i<header_TEXTS;i++) {

	    string = headrtv_GetInput(Text(self, i));
	    if (string && *string != '\0') {
		fprintf(file,".ds %s ",strs[Data(self)->where][i]);
		PrintLine(file, string);
		fputs("\n",file);
	    }
	    if (string) free(string);
	}
    }
}

enum view_DSattributes headrtv__DesiredSize(self, width, height, pass, desiredwidth, desiredheight)
struct headrtv *self;
long width, height;
enum view_DSpass pass;
long *desiredwidth, *desiredheight;
{
    enum view_DSattributes result;
    result=lpair_DesiredSize(self->sections,width, height, pass, desiredwidth, desiredheight);
    *desiredwidth = width;
    if(self->open) *desiredheight+=self->top*4;
    else *desiredheight=self->top;
    if(*desiredheight>self->top*5) {
	*desiredheight=self->top*5;
    }
    return result;
}

static char *hdrtv_where[]={"Header","Footer"};
static char *hdrtv_close="Close";
static char *hdrtv_open="Open";

#define ALIGNMENT (graphic_ATLEFT|graphic_ATBASELINE)
static void DrawBorder(self,vb)
struct headrtv *self;
struct rectangle *vb;
{
    int width,junk,left;
    char *type;
   
    type=hdrtv_where[Data(self)->where];
    headrtv_SetTransferMode(self,graphic_COPY);
    headrtv_FillRectSize(self, vb->left+1, vb->top+1, vb->width-2, self->top-1,headrtv_WhitePattern(self));
    headrtv_SetTransferMode(self,graphic_BLACK);
    headrtv_DrawRectSize(self,vb->left, vb->top,  vb->width-1, vb->height-1);
    headrtv_MoveTo(self,0,self->top-1);
    headrtv_DrawLineTo(self, vb->width-1,self->top-1); 
    headrtv_SetFont(self,self->font);
    headrtv_MoveTo(self,5,self->top-5);
    headrtv_DrawString(self,type,ALIGNMENT);
    fontdesc_TextSize(self->font,headrtv_GetDrawable(self), type, strlen(type),&width,&junk);
    left=15+width;
    headrtv_MoveTo(self, width + 10, 0);
    headrtv_DrawLineTo(self, width + 10, self->top-1);
    self->wherebox = left;
   
    type=(self->open)?hdrtv_close:hdrtv_open;
    fontdesc_StringSize(self->font,headrtv_GetDrawable(self),type, &width, &junk);
    self->closebox=vb->width-width-5;
    headrtv_MoveTo(self,self->closebox,self->top-5);
    headrtv_SetFont(self,self->font);
    headrtv_DrawString(self,type,ALIGNMENT);
    headrtv_MoveTo(self, self->closebox - 5, 0);
    headrtv_DrawLineTo(self, self->closebox - 5, self->top-1);
}

static struct textview *GetTextView(self)
struct view *self;
{
    while(self) {
	if(class_IsTypeByName(class_GetTypeName(self),"textview")) return (struct textview *)self;
	self=self->parent;
    }
    return (struct textview *)self;
}

static void headrtv_MoveOn(self, rock)
struct headrtv *self;
long rock;
/* This is the routine bound to the Return key - it moves the focus from
  self->name to self->number, or, if self->number already has the focus,
      then it calls turnin_TurninGo
      */
{
    if (self->my_focus >= header_rtext) self->my_focus = header_ltext;
    else self->my_focus++;
    
    headrtv_WantInputFocus(self, Textv(self, self->my_focus));
}

struct view *headrtv__Hit(self, action, x, y, numberOfClicks)
struct headrtv *self;
enum view_MouseAction action;
long x;
long y;
long numberOfClicks;
{
    if(y>self->top) {
	int i;
	long testy = self->top;
	struct view *tmpv;
	struct rectangle r;

	headrtv_WantInputFocus(self,self);
	tmpv = (struct view *)lpair_Hit(self->sections, action, x, y-self->top, numberOfClicks);
	for(i=header_ltext;i<header_TEXTS;i++) {
	    textview_GetEnclosedBounds(Textv(self, i), &r);
	    testy += r.height;
	    if (y < testy) {
		self->my_focus = i;
		break;
	    }
	}
	return tmpv;
	
    }
    switch(action) {
	case view_LeftUp:
	case view_RightUp:
	    if(x>self->closebox||!self->open) {
		if(self->open) {
		    struct rectangle mvb;
		    struct textview *tv=GetTextView(View(self));
		    rectangle_SetRectSize(&mvb,0,0,0,0);
		    self->open=FALSE;
		    headrtv_WantNewSize(self,self);
		    lpair_InsertView(self->sections,self,&mvb);
		    textview_WantInputFocus(tv,tv);
		} else headrtv_WantInputFocus(self,self);
		return View(self);
	    }
	    if(x<self->wherebox) {
		if(Data(self)->where==header_HEADER)
		    Data(self)->where=header_FOOTER;
		else Data(self)->where=header_HEADER;
	    }
	    header_NotifyObservers(Data(self),0);
	    break;
    }
    return View(self);
}

void headrtv__FullUpdate(self, type, left, top, width, height)
struct headrtv *self;
enum view_UpdateType type;
long left,top,width,height;
{
    struct rectangle mvb;
   
    switch(type) {
	case view_PartialRedraw:
	case view_LastPartialRedraw:
	case view_FullRedraw:
	    headrtv_GetVisualBounds(self,&mvb);
	   
	    DrawBorder(self,&mvb);
	    if(self->open) {
		mvb.top+=self->top;
		mvb.left++;
		mvb.width-=2;
		mvb.height-=self->top+1;
		lpair_InsertView(self->sections,self,&mvb);
		lpair_FullUpdate(self->sections, type, left,top, width, height);
	    }
    }
}

void headrtv__Update(self)
struct headrtv *self;
{
    struct rectangle mvb;
    super_Update(self);
    headrtv_GetVisualBounds(self,&mvb);
    DrawBorder(self,&mvb);
}

void headrtv__WantInputFocus(self,requestor)
struct headrtv *self;
struct view *requestor;
{
    if(View(self)!=requestor || !self->open) super_WantInputFocus(self,requestor);
}

void headrtv__LoseInputFocus(self)
struct headrtv *self;
{
    if(self->sections) lpair_LoseInputFocus(self->sections);
}

void headrtv__PostKeyState(self, keystate)
struct headrtv *self;
struct keystate *keystate;
{
    struct keystate *k;
    k=keystate_AddBefore(self->keystate,keystate);
    super_PostKeyState(self,k);
}
    
void headrtv__ReceiveInputFocus(self)
struct headrtv *self;
{
    
    if(!self->open) {
	self->open=TRUE;
	headrtv_WantNewSize(self,self);
	if (Textv(self, header_ltext)) {
	    self->my_focus = header_ltext;
	    headrtv_WantInputFocus(self, Textv(self, header_ltext));
	}
    } else
	if(self->sections) {
	    lpair_ReceiveInputFocus(self->sections);
	}
}

boolean headrtv__CanView(self,name)
struct headrtv *self;
char *name;
{
    if(!strcmp(name,"header")) return TRUE;
    return FALSE;
}

void headrtv__LinkTree(self,parent)
struct headrtv *self;
struct view *parent;
{
    super_LinkTree(self,parent);
    if(self->sections) lpair_LinkTree(self->sections,self);
}

void headrtv__SetDataObject(self,object)
struct headrtv *self;
struct header *object;
{
    int i, pos;
    super_SetDataObject(self,object);
    
    for(i=header_ltext;i<header_TEXTS;i++) {
	textview_SetDataObject(Textv(self, i), Text(self, i));
	pos = text_GetFence(Text(self, i));
	textview_SetDotPosition(Textv(self, i), pos);
    }
}

boolean headrtv__InitializeObject(classID,self)
struct classheader *classID;
struct headrtv *self;
{
    struct FontSummary *fontSummary;
    struct lpair *bottom;
    int i;

    self->open=FALSE;
    self->bfont=fontdesc_Create("andysans",fontdesc_Bold,12);
    self->font=fontdesc_Create("andysans",fontdesc_Plain,12);
    self->bifont=fontdesc_Create("andysans",fontdesc_Bold | fontdesc_Italic,12);
    self->ifont=fontdesc_Create("andysans",fontdesc_Italic,12);
    if(!self->ifont || !self->font || !self->bfont || !self->bifont) return FALSE;
    if ((fontSummary = fontdesc_FontSummary(self->font, headrtv_GetDrawable(self))) == NULL)
	return FALSE;
    self->top=fontSummary->maxHeight+5;

    self->my_focus = header_ltext;
    for(i=header_ltext;i<header_TEXTS;i++) {
	Textv(self, i) = textview_New();
	if(!Textv(self, i)) {
	    for(i--;i>=header_ltext;i--) textview_Destroy(Textv(self, i));
	    return FALSE;
	}
    }

    bottom = lpair_New();
    self->sections = lpair_New();
    lpair_VSplit (self->sections, Textv(self, header_ltext), bottom, 66, FALSE);
    lpair_VSplit (bottom, Textv(self, header_ctext), Textv(self, header_rtext), 50, FALSE);
    
    self->keystate = keystate_Create(self,newKeymap);
    if(!self->keystate) return FALSE;

    lpair_LinkTree(self->sections, self);

    return TRUE;
}

#if 0
static void newline(tv,rock)
struct headrtv *tv;
long rock;
{
    message_DisplayString(tv,0,"Headers and footers can be only one line long.");
}
#endif

boolean headrtv__InitializeClass(classID)
struct classheader *classID;
{
    struct proctable_Entry *tempProc;
    struct classinfo *textvClassInfo=class_Load("textview");
    if(!textvClassInfo) return FALSE;
    newKeymap = keymap_New();
    if(!newKeymap) return FALSE;
    tempProc = proctable_DefineProc("headrtv-newline", headrtv_MoveOn ,&headrtv_classinfo,NULL, "Goes to next section of the header/footer.");
    keymap_BindToKey(newKeymap,"\n",tempProc,0);
    keymap_BindToKey(newKeymap,"\r",tempProc,0);
    InstallHeaderVariables();
    return TRUE;
}
