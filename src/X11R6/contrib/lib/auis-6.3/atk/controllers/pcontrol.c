/* user code begins here for HeaderInfo */

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/controllers/RCS/pcontrol.c,v 2.14 1992/12/15 21:29:20 rr2b R6tape $";
#endif

/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

 
/* user code ends here for HeaderInfo */
#include <andrewos.h>
#include <class.h>
#include <proctbl.ih>
#include <view.ih>
#include <arbiterv.ih>
#include <pcontrol.eh>
#include <celv.ih>
#include <text.ih>
#include <controlv.ih>
#include <cel.ih>
#include <lsetv.ih>
#include <lset.ih>
#include <value.ih>
#include <textv.ih>
/* user code begins here for includes */
#include <attribs.h>
#include <sys/ioctl.h>
#include <message.ih>
#include <complete.ih>
#include <im.ih>
#define NUM 4
#define MAXNAMES 40
#define WHOLE 0
#define HALF 1
#define QUARTER 2
#define EIGHTH 3
#define getspeed(self) ((self->duration) ? (8 << ((3 - value_GetValue(self->duration)))) : self->lastnoteval)

#if defined (sys_rt_r3) || defined (sys_rt_aos4)

#include  <machineio/speakerio.h>
#endif /* defined (sys_rt_r3) || defined (sys_rt_aos4) */
struct nh {
    char *note;
    float freq;
};
float arr[61];
setbl(b,freq)
struct spk_blk *b;
float freq;
{
#if defined (sys_rt_r3) || defined (sys_rt_aos4)
    if (freq < 23) {
	b->freqhigh=0;
	b->freqlow=SPKOLOMIN;
    } else if (freq < 46) {
	b->freqhigh=64;
	b->freqlow = (char) ((6000.0 /(float) freq) - 9.31);
    } else if (freq < 91) {
	b->freqhigh=32;
	b->freqlow = (char) ((12000.0 /(float) freq) - 9.37);
    } else if (freq < 182) {
	b->freqhigh=16;
	b->freqlow = (char) ((24000.0 /(float) freq) - 9.48);
    } else if (freq < 363) {
	b->freqhigh=8;
	b->freqlow = (char) ((48000.0 /(float) freq) - 9.71);
    } else if (freq < 725) {
	b->freqhigh=4;
	b->freqlow = (char) ((96000.0 /(float) freq) - 10.18);
    } else if (freq < 1433) {
	b->freqhigh=2;
	b->freqlow = (char) ((192000.0 /(float) freq) - 11.10);
    } else if (freq < 12020) {
	b->freqhigh=1;
	b->freqlow = (char) ((384000.0 /(float) freq) - 12.95);
    } else {
	b->freqhigh=0;
	b->freqlow=SPKOLOMIN;
    }
#endif /* defined (sys_rt_r3) || defined (sys_rt_aos4) */
}
static int sp = 0;
play(buf,Speed)
char *buf;
int Speed;
{
#if defined (sys_rt_r3) || defined (sys_rt_aos4)
    char *note;
    int v,d;
    float f;
    struct spk_blk  b;
    char *rindex();
    sscanf(buf,"%d,%d,%f",&v,&d,&f);
    b.volume = v;
    b.duration = d + (d * Speed / 50) ;
    if((note = rindex(buf,',')) == NULL){
	printf("bad format\n");
	return;
    }
    note++;
    setbl(&b,f);
    write(sp,&b,sizeof(b));
    b.volume = 0;	
    b.duration = 1;
    write(sp,&b,sizeof(b));
#endif /* defined (sys_rt_r3) || defined (sys_rt_aos4) */
}

#define openspk() ((sp = open("/dev/speaker",1)) > 0)
#define closespk() close(sp)

static int masks[] = {1,2,4,8,16,32,64,128,256,512,1024,2048,4096 };
char *names[] = { "","C","C#","D","D#","E","F","F#","G","G#","A","A#","B","C" };
/* user code ends here for includes */

static struct pcontrol *firstpcontrol;
static struct pcontrol *FindSelf(v)
struct view *v;
{
	struct pcontrol *self,*last = NULL;
	for(self= firstpcontrol; self != NULL; self = self->next){
		if(self->v == v) return self;
		last = self;
		}
	self = pcontrol_New();
	initself(self,v);
	if(last == NULL) firstpcontrol = self;
	else last->next = self;
	return self;
}
static void replayCallBack(self,val,r1,r2)
struct pcontrol *self;
struct value *val;
long r1,r2;
{
/* user code begins here for replayCallBack */
    char buf[256],*cp;
    int i,len;
    int Speed =  value_GetValue(self->speed);
    if(self->score == NULL || !openspk()) return;
    len = text_GetLength(self->score);
    for(i = 0, cp = buf; i < len; i++){
	*cp = text_GetChar(self->score,i);
	if(*cp == '\n'){
	    *cp = '\0';
	    play (buf,Speed);
	    cp = buf;
	}
	else cp++;
    }
    closespk();
/* user code ends here for replayCallBack */
}
static void kbCallBack(self,val,r1,r2)
struct pcontrol *self;
struct value *val;
long r1,r2;
{
/* user code begins here for kbCallBack */
    char buf[64];
    long w,i,v;
    int Speed =  value_GetValue(self->speed);
    if(self->score == NULL) return;
    w = value_GetValue(val);
    if(w == 0 || !openspk()) return;
    for(i = 1; i < 13; i++){
	if(masks[i] & w){
	    v = i + 2 + (r1 * 12);
	    sprintf(buf,"%d,%d,%f %s",value_GetValue(self->volume),
		    getspeed(self),arr[v],
		    names[i]);
	    play(buf,Speed);
	    if(value_GetValue(self->mode)){
		self->lastlen = text_GetLength(self->score);
		strcat(buf,"\n");
		text_InsertCharacters(self->score,self->lastlen,buf,strlen(buf));
		text_NotifyObservers(self->score,0);
	    }
	}
    }
    value_SetValue(val,0);
    closespk();
/* user code ends here for kbCallBack */
}
static void volumeCallBack(self,val,r1,r2)
struct pcontrol *self;
struct value *val;
long r1,r2;
{
/* user code begins here for volumeCallBack */
/* user code ends here for volumeCallBack */
}
static void clearCallBack(self,val,r1,r2)
struct pcontrol *self;
struct value *val;
long r1,r2;
{
/* user code begins here for clearCallBack */
    text_Clear(self->score);
    text_NotifyObservers(self->score,0);
/* user code ends here for clearCallBack */
}
static void modeCallBack(self,val,r1,r2)
struct pcontrol *self;
struct value *val;
long r1,r2;
{
/* user code begins here for modeCallBack */
/* user code ends here for modeCallBack */
}
static void ntCallBack(self,val,r1,r2)
struct pcontrol *self;
struct value *val;
long r1,r2;
{
/* user code begins here for ntCallBack */
    static int len[] = { 96,64,50,32,24,16,12,8,6,4,3,2};
    static char s[2];
    self->lastnoteval = len[r1];
    s[0] = '@' + r1;
    s[1] = '\0';
    value_SetString(self->noteout,s);
/* user code ends here for ntCallBack */
}
static void speedCallBack(self,val,r1,r2)
struct pcontrol *self;
struct value *val;
long r1,r2;
{
/* user code begins here for speedCallBack */
/* user code ends here for speedCallBack */
}
static void ReadCallBack(self,val,r1,r2)
struct pcontrol *self;
struct value *val;
long r1,r2;
{
/* user code begins here for ReadCallBack */

    char filename[1024];
    FILE *file;

    if (im_GetDirectory(filename) != NULL) /* get the current directory */
	strcat(filename, "/");
    if (completion_GetFilename(self->v, "file to read: ", filename, filename, sizeof(filename), FALSE, TRUE) == -1 )
	return;
    if((file = fopen(filename,"r")) != NULL){
	text_Clear(self->score);
	text_Read(self->score,file,0);
	text_NotifyObservers(self->score,0);
	message_DisplayString(self->v,0,"Done");
    }
    else{
	message_DisplayString(self->v,0,"Can't write file");
    }
/* user code ends here for ReadCallBack */
}
static void noteoutCallBack(self,val,r1,r2)
struct pcontrol *self;
struct value *val;
long r1,r2;
{
/* user code begins here for noteoutCallBack */
/* user code ends here for noteoutCallBack */
}
static void undoCallBack(self,val,r1,r2)
struct pcontrol *self;
struct value *val;
long r1,r2;
{
/* user code begins here for undoCallBack */
    int i;
    self->lastlen = text_GetLength(self->score);
    for(i = self->lastlen -1 ;--i > 0 ;){
	if(text_GetChar(self->score,i) == '\n'){
	    i++;
	    text_DeleteCharacters(self->score,i,self->lastlen - i);
	    text_NotifyObservers(self->score,0);
	    break;
	}
    }
/* user code ends here for undoCallBack */
}
static void restCallBack(self,val,r1,r2)
struct pcontrol *self;
struct value *val;
long r1,r2;
{
/* user code begins here for restCallBack */
    char buf[64];
    sprintf(buf,"0,%d,0.0 rest",
	     getspeed(self));
/*    play(buf, value_GetValue(self->speed)); */
    if(value_GetValue(self->mode)){
	self->lastlen = text_GetLength(self->score);
	strcat(buf,"\n");
	text_InsertCharacters(self->score,self->lastlen,buf,strlen(buf));
	text_NotifyObservers(self->score,0);
    }
/* user code ends here for restCallBack */
}
static void SaveCallBack(self,val,r1,r2)
struct pcontrol *self;
struct value *val;
long r1,r2;
{
/* user code begins here for SaveCallBack */

    char filename[1024];
    FILE *file;

    if (im_GetDirectory(filename) != NULL) /* get the current directory */
	strcat(filename, "/");
    if (completion_GetFilename(self->v, "file to save: ", filename, filename, sizeof(filename), FALSE, FALSE) == -1 )
	return;
    if((file = fopen(filename,"w")) != NULL){
	text_Write(self->score,file,im_GetWriteID(),0);
	fclose(file);
	message_DisplayString(self->v,0,"Wrote song");
    }
    else{
	message_DisplayString(self->v,0,"Can't write file");
    }
/* user code ends here for SaveCallBack */
}
static initself(self,v)
struct pcontrol *self;
struct view *v;
{
	self->v = v;
	self->kb_2View = (struct pianoV *)arbiterview_GetNamedView(v,"kb-2");
	self->kb_2 = (struct value *)arbiterview_GetNamedObject(v,"kb-2");
	if(self->kb_2) value_AddCallBackObserver(self->kb_2, self,kbCallBack,2);
	self->kb_3View = (struct pianoV *)arbiterview_GetNamedView(v,"kb-3");
	self->kb_3 = (struct value *)arbiterview_GetNamedObject(v,"kb-3");
	if(self->kb_3) value_AddCallBackObserver(self->kb_3, self,kbCallBack,3);
	self->replayView = (struct buttonV *)arbiterview_GetNamedView(v,"replay");
	self->replay = (struct value *)arbiterview_GetNamedObject(v,"replay");
	if(self->replay) value_AddCallBackObserver(self->replay, self,replayCallBack,0);
	self->volumeView = (struct fourwayV *)arbiterview_GetNamedView(v,"volume");
	self->volume = (struct value *)arbiterview_GetNamedObject(v,"volume");
	if(self->volume) value_AddCallBackObserver(self->volume, self,volumeCallBack,0);
	self->nt_10View = (struct buttonV *)arbiterview_GetNamedView(v,"nt-10");
	self->nt_10 = (struct value *)arbiterview_GetNamedObject(v,"nt-10");
	if(self->nt_10) value_AddCallBackObserver(self->nt_10, self,ntCallBack,10);
	self->clearView = (struct buttonV *)arbiterview_GetNamedView(v,"clear");
	self->clear = (struct value *)arbiterview_GetNamedObject(v,"clear");
	if(self->clear) value_AddCallBackObserver(self->clear, self,clearCallBack,0);
	self->nt_11View = (struct buttonV *)arbiterview_GetNamedView(v,"nt-11");
	self->nt_11 = (struct value *)arbiterview_GetNamedObject(v,"nt-11");
	if(self->nt_11) value_AddCallBackObserver(self->nt_11, self,ntCallBack,11);
	self->nt_0View = (struct buttonV *)arbiterview_GetNamedView(v,"nt-0");
	self->nt_0 = (struct value *)arbiterview_GetNamedObject(v,"nt-0");
	if(self->nt_0) value_AddCallBackObserver(self->nt_0, self,ntCallBack,0);
	self->nt_1View = (struct buttonV *)arbiterview_GetNamedView(v,"nt-1");
	self->nt_1 = (struct value *)arbiterview_GetNamedObject(v,"nt-1");
	if(self->nt_1) value_AddCallBackObserver(self->nt_1, self,ntCallBack,1);
	self->modeView = (struct onoffV *)arbiterview_GetNamedView(v,"mode");
	self->mode = (struct value *)arbiterview_GetNamedObject(v,"mode");
	if(self->mode) value_AddCallBackObserver(self->mode, self,modeCallBack,0);
	self->nt_2View = (struct buttonV *)arbiterview_GetNamedView(v,"nt-2");
	self->nt_2 = (struct value *)arbiterview_GetNamedObject(v,"nt-2");
	if(self->nt_2) value_AddCallBackObserver(self->nt_2, self,ntCallBack,2);
	self->nt_3View = (struct buttonV *)arbiterview_GetNamedView(v,"nt-3");
	self->nt_3 = (struct value *)arbiterview_GetNamedObject(v,"nt-3");
	if(self->nt_3) value_AddCallBackObserver(self->nt_3, self,ntCallBack,3);
	self->nt_4View = (struct buttonV *)arbiterview_GetNamedView(v,"nt-4");
	self->nt_4 = (struct value *)arbiterview_GetNamedObject(v,"nt-4");
	if(self->nt_4) value_AddCallBackObserver(self->nt_4, self,ntCallBack,4);
	self->nt_5View = (struct buttonV *)arbiterview_GetNamedView(v,"nt-5");
	self->nt_5 = (struct value *)arbiterview_GetNamedObject(v,"nt-5");
	if(self->nt_5) value_AddCallBackObserver(self->nt_5, self,ntCallBack,5);
	self->nt_6View = (struct buttonV *)arbiterview_GetNamedView(v,"nt-6");
	self->nt_6 = (struct value *)arbiterview_GetNamedObject(v,"nt-6");
	if(self->nt_6) value_AddCallBackObserver(self->nt_6, self,ntCallBack,6);
	self->nt_7View = (struct buttonV *)arbiterview_GetNamedView(v,"nt-7");
	self->nt_7 = (struct value *)arbiterview_GetNamedObject(v,"nt-7");
	if(self->nt_7) value_AddCallBackObserver(self->nt_7, self,ntCallBack,7);
	self->nt_8View = (struct buttonV *)arbiterview_GetNamedView(v,"nt-8");
	self->nt_8 = (struct value *)arbiterview_GetNamedObject(v,"nt-8");
	if(self->nt_8) value_AddCallBackObserver(self->nt_8, self,ntCallBack,8);
	self->nt_9View = (struct buttonV *)arbiterview_GetNamedView(v,"nt-9");
	self->nt_9 = (struct value *)arbiterview_GetNamedObject(v,"nt-9");
	if(self->nt_9) value_AddCallBackObserver(self->nt_9, self,ntCallBack,9);
	self->speedView = (struct sliderV *)arbiterview_GetNamedView(v,"speed");
	self->speed = (struct value *)arbiterview_GetNamedObject(v,"speed");
	if(self->speed) value_AddCallBackObserver(self->speed, self,speedCallBack,0);
	self->ReadView = (struct buttonV *)arbiterview_GetNamedView(v,"Read");
	self->Read = (struct value *)arbiterview_GetNamedObject(v,"Read");
	if(self->Read) value_AddCallBackObserver(self->Read, self,ReadCallBack,0);
	self->noteoutView = (struct stringV *)arbiterview_GetNamedView(v,"noteout");
	self->noteout = (struct value *)arbiterview_GetNamedObject(v,"noteout");
	if(self->noteout) value_AddCallBackObserver(self->noteout, self,noteoutCallBack,0);
	self->undoView = (struct buttonV *)arbiterview_GetNamedView(v,"undo");
	self->undo = (struct value *)arbiterview_GetNamedObject(v,"undo");
	if(self->undo) value_AddCallBackObserver(self->undo, self,undoCallBack,0);
	self->scoreView = (struct textview *)arbiterview_GetNamedView(v,"score");
	self->score = (struct text *)arbiterview_GetNamedObject(v,"score");
	self->ab1View = (struct arbiterview *)arbiterview_GetNamedView(v,"ab1");
	self->ab1 = (struct arbiter *)arbiterview_GetNamedObject(v,"ab1");
	self->ab2View = (struct lsetview *)arbiterview_GetNamedView(v,"ab2");
	self->ab2 = (struct lset *)arbiterview_GetNamedObject(v,"ab2");
	self->restView = (struct buttonV *)arbiterview_GetNamedView(v,"rest");
	self->rest = (struct value *)arbiterview_GetNamedObject(v,"rest");
	if(self->rest) value_AddCallBackObserver(self->rest, self,restCallBack,0);
	self->kb_0View = (struct pianoV *)arbiterview_GetNamedView(v,"kb-0");
	self->kb_0 = (struct value *)arbiterview_GetNamedObject(v,"kb-0");
	if(self->kb_0) value_AddCallBackObserver(self->kb_0, self,kbCallBack,0);
	self->SaveView = (struct buttonV *)arbiterview_GetNamedView(v,"Save");
	self->Save = (struct value *)arbiterview_GetNamedObject(v,"Save");
	if(self->Save) value_AddCallBackObserver(self->Save, self,SaveCallBack,0);
	self->kb_1View = (struct pianoV *)arbiterview_GetNamedView(v,"kb-1");
	self->kb_1 = (struct value *)arbiterview_GetNamedObject(v,"kb-1");
	if(self->kb_1) value_AddCallBackObserver(self->kb_1, self,kbCallBack,1);
}
pcontrol_start(v,dat)
struct view *v;
 long dat;
{
struct pcontrol *self;
if((self = FindSelf(v)) == NULL) return;
/* user code begins here for pcontrol_start */
arbiterview_SetIgnoreUpdates(v, TRUE);
self->duration = (struct value *)arbiterview_GetNamedObject(v,"duration");
/* user code ends here for pcontrol_start */
}
boolean pcontrol__InitializeClass(ClassID)
struct classheader *ClassID;
{
struct classinfo *viewtype = class_Load("view");
firstpcontrol = NULL;
proctable_DefineProc("pcontrol-start",pcontrol_start, viewtype,NULL,"pcontrol start");
/* user code begins here for InitializeClass */
#if defined (sys_rt_r3) || defined (sys_rt_aos4)
    int i;
/*
    if((sp = open("/dev/speaker",1)) < 0) {
	printf("can't open speaker\n");
	return FALSE;
    }
    ioctl(sp,TIOCNXCL,NULL);
*/
    arr[60] = 1760.0 * 4.0;
    for(i = 59; i >= 0; i--){
	arr[i] = arr[i + 1] - (arr[i + 1] / 17.817);
	/*	  printf("\"%s\",%4.2f,\n",notes[12 - (i %12)],arr[i]); */
    }
#endif /* defined (sys_rt_r3) || defined (sys_rt_aos4) */
#if !( defined (sys_rt_r3) || defined (sys_rt_aos4) )
    return FALSE;
#else /* defined (sys_rt_r3) || defined (sys_rt_aos4) */
/* user code ends here for InitializeClass */
return TRUE;
#endif /* defined (sys_rt_r3) || defined (sys_rt_aos4) */
}
boolean pcontrol__InitializeObject(ClassID,self)
struct classheader *ClassID;
struct pcontrol *self;
{
self->kb_2 = NULL;
self->kb_2View = NULL;
self->kb_3 = NULL;
self->kb_3View = NULL;
self->replay = NULL;
self->replayView = NULL;
self->volume = NULL;
self->volumeView = NULL;
self->nt_10 = NULL;
self->nt_10View = NULL;
self->clear = NULL;
self->clearView = NULL;
self->nt_11 = NULL;
self->nt_11View = NULL;
self->nt_0 = NULL;
self->nt_0View = NULL;
self->nt_1 = NULL;
self->nt_1View = NULL;
self->mode = NULL;
self->modeView = NULL;
self->nt_2 = NULL;
self->nt_2View = NULL;
self->nt_3 = NULL;
self->nt_3View = NULL;
self->nt_4 = NULL;
self->nt_4View = NULL;
self->nt_5 = NULL;
self->nt_5View = NULL;
self->nt_6 = NULL;
self->nt_6View = NULL;
self->nt_7 = NULL;
self->nt_7View = NULL;
self->nt_8 = NULL;
self->nt_8View = NULL;
self->nt_9 = NULL;
self->nt_9View = NULL;
self->speed = NULL;
self->speedView = NULL;
self->Read = NULL;
self->ReadView = NULL;
self->noteout = NULL;
self->noteoutView = NULL;
self->undo = NULL;
self->undoView = NULL;
self->score = NULL;
self->scoreView = NULL;
self->ab1 = NULL;
self->ab1View = NULL;
self->ab2 = NULL;
self->ab2View = NULL;
self->rest = NULL;
self->restView = NULL;
self->kb_0 = NULL;
self->kb_0View = NULL;
self->Save = NULL;
self->SaveView = NULL;
self->kb_1 = NULL;
self->kb_1View = NULL;
self->v = NULL;
self->next = NULL;
/* user code begins here for InitializeObject */
self->lastlen = 0;
self->lastnoteval = 16;
self->duration = NULL;
/* user code ends here for InitializeObject */
return TRUE;}
/* user code begins here for Other Functions */
/* user code ends here for Other Functions */
