/* ********************************************************************** *\
 *         Copyright IBM Corporation 1991 - All Rights Reserved           * 
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/supportviews/RCS/sbutton.c,v 1.12 1993/01/17 22:26:53 rr2b Exp $";
#endif


 

#include <stdio.h>
#include <sys/param.h>
#include <andrewos.h>
#include <atom.ih>
#include <environ.ih>
#include <fontdesc.ih>
#include <observe.ih>
#include <owatch.ih>
#include <util.h>
#include <rect.h>

#include "sbutton.eh"

/* Defined constants and macros */

#define MAX_LINE_LENGTH 70  /* can't be less than 6 */

/* External declarations */

/* Forward Declarations */
static void WriteLine();
static char *GlomStrings(), *ReadLine(), *EncodeFont();

/* Global variables */
static struct atom *buttonpushed=NULL;
static boolean linehascontrol=FALSE;
static char *True="True";
static char *False="False";
static char *EmptyString="";

static char *Intern(str)
char *str;
{
    struct atom *a;
    if(str==NULL) return NULL;
    a=atom_Intern(str);
    if(a!=NULL) return atom_Name(a);
    else return NULL;
}

boolean sbutton__InitializeClass(c)
struct classheader *c;
{
    /* 
      Initialize all the class data.
      */
    buttonpushed=atom_Intern("buttonpushed");
    if(buttonpushed==NULL) return FALSE;
    observable_DefineTrigger(&sbutton_classinfo, buttonpushed);
    return(TRUE);
}


static void init(self, i, j)
struct sbutton *self;
int i, j;
{
    while(i<=j)	{
	self->buttons[i].rock=0;
	self->buttons[i].lit=FALSE;
	self->buttons[i].label=NULL;
	self->buttons[i].trigger=NULL;
	self->buttons[i].prefs=self->prefs;
	self->prefs->refcount++;
	i++;
    }
}

static boolean SetupInitialState(self)
struct sbutton *self;
{
    self->prefs=sbutton_GetNewPrefs(NULL);
    if(self->prefs==NULL) return FALSE;
    self->prefs->name=NewString("Default");
    self->prefs->writeid= sbutton_GetWriteID(self);
    return TRUE;
}

boolean sbutton__EnsureSize(self, ind)
struct sbutton *self;
int ind;
{
    int newsize,i=self->maxcount;
    if(ind<0) return FALSE;
    if(self->prefs==NULL && !SetupInitialState(self)) return FALSE;
    if(ind>=i) {
	if(ind<i*2) newsize=i*2;
	else newsize=ind+1;
	if(self->buttons) self->buttons=(struct sbutton_info *)realloc(self->buttons, sizeof(struct sbutton_info)*newsize);
	else self->buttons=(struct sbutton_info *)malloc(sizeof(struct sbutton_info)*newsize);
	if(!self->buttons) {
	    self->count=0;
	    self->maxcount=0;
	    return FALSE;
	}

	init(self, i, newsize-1);
	self->maxcount=newsize;
    }
    
    if(ind>=self->count) {
	self->count=ind+1;
	sbutton_SetChangeFlag(self, sbutton_SIZECHANGED);
	if(self->rows*self->cols<self->count) {
	    switch(self->sizepolicy) {
		case sbutton_SizeFixed:
		    break;
		case sbutton_GrowRows:
		    self->rows=self->count/self->cols;
		    if(self->count%self->cols) self->rows++;
		    break;
		case sbutton_GrowColumns:
		    self->cols=self->count/self->rows;
		    if(self->count%self->rows) self->cols++;
		    break;
	    }
	}

    }
    return TRUE;
}


char *colorprefs[sbutton_COLORS]={
    "background",
    "topshadow",
    "top", 
    "bottomshadow",
    "foreground",
    "labelforeground",
    "labelbackground"
};

static char *defprefname="sbutton";

void sbutton__InitPrefs(classID, prefs, name)
struct classheader *classID;
struct sbutton_prefs *prefs;
char *name;
{
    char buf[1024], *font;
    long style, size;
    int i;
    for(i=0;i<sbutton_COLORS;i++) {
	char *value;
	strncpy(buf, name, sizeof(buf)-strlen(colorprefs[i])-1);
	strcat(buf, colorprefs[i]);
	value=environ_GetProfile(buf);
	if(value != NULL) prefs->colors[i] = Intern(value);
    }
    strncpy(buf, name, sizeof(buf)-6); /* reserve space for the word style too. */
    strcat(buf,"style");
    prefs->style=environ_GetProfileInt(buf, prefs->style);
    strncpy(buf, name, sizeof(buf)-5); /* reserve space for the word font too. */
    strcat(buf,"font");
    font=environ_GetProfile(buf);
    if (fontdesc_ExplodeFontName(font,buf,sizeof(buf), &style, &size)) prefs->font=fontdesc_Create(buf,style,size);
}

struct sbutton_prefs *sbutton__GetNewPrefs(classID, name)
struct classhead *classID;
char *name;
{
    char buf[1024];
    struct sbutton_prefs *new=(struct sbutton_prefs *)malloc(sizeof(struct sbutton_prefs));
    if(new==NULL) return NULL;
    bzero(new, sizeof(struct sbutton_prefs));
    new->style=(-1);
    new->refcount++;
    new->name=NULL;
    new->bdepth=(-1);
    
    if(name) {
	strncpy(buf, name, sizeof(buf)-8);
	strcat(buf, "Inherit");
    }
    if(name==NULL || environ_GetProfileSwitch("SbuttonAlwaysInherit", FALSE) ||  environ_GetProfileSwitch(buf, FALSE)) {
	sbutton_InitPrefs(new, defprefname);
    }
    return new;
}

void sbutton__WritePrefs(c, fp, prefs)
struct classheader *c;
FILE *fp;
struct sbutton_prefs *prefs;
{
    int i;
    char buf[1024];
    fprintf(fp, "\\newprefs\n\n");
    if(sbutton_GetStyle(prefs)>0) {
	fprintf(fp, "\\style\n");
	sprintf(buf, "%d", sbutton_GetStyle(prefs));
	WriteLine(fp, buf);
    }
    if(sbutton_GetFont(prefs)) {
	char *encfont;
	encfont =  EncodeFont(sbutton_GetFont(prefs));
	if (encfont) {
	    fprintf(fp, "\\font\n");
	    WriteLine(fp, encfont);
	    free(encfont);
	}
    }
    for(i=0;i<sbutton_COLORS;i++) {
	if(prefs->colors[i]) {
	    fprintf(fp, "\\%s\n",colorprefs[i]);
	    WriteLine(fp, prefs->colors[i]);
	}
    }
    fprintf(fp, "\\name\n");
    WriteLine(fp, prefs->name?prefs->name:EmptyString);
    fprintf(fp,"\\endprefs\n\n");
}

void sbutton__FreePrefs(c, prefs)
struct classheader *c;
struct sbutton_prefs *prefs;
{
    prefs->refcount--;
    if(prefs->refcount<=0) {
	if(prefs->name!=NULL) free(prefs->name);
	free(prefs);
    }
}

struct sbutton_prefs *sbutton__DuplicatePrefs(c, prefs, name)
struct classheader *c;
struct sbutton_prefs *prefs;
char *name;
{
    struct sbutton_prefs *result=(struct sbutton_prefs *)malloc(sizeof(struct sbutton_prefs));
    if(result==NULL) return result;
    *result=(*prefs);
    result->refcount=0;
    if(name!=NULL) result->name=NewString(name);
    else result->name=NULL;
    return result;
}

boolean sbutton__InitializeObject(c, self)
struct classheader *c;
struct sbutton *self;
{
    /*
      Inititialize the object instance data.
      */
    self->singleactivation=TRUE;

    self->activated=(-1);
    
    
    self->triggerchange=(-1);
    
    
    self->hitfunc=NULL;
    self->resizefunc=NULL;
    
    self->sizepolicy=sbutton_GrowRows;
    self->maxcount=0;
    self->count=0;
    self->buttons=NULL;
   
    self->prefs=NULL;

    /* self->prefs=sbutton_GetNewPrefs(NULL);
    if(self->prefs==NULL) return FALSE;
    self->prefs->name=NewString("Default");
    self->prefs->writeid= sbutton_GetWriteID(self);
*/
    self->matteprefs = NULL;
      
    self->rows=1;
    self->cols=1;
    
    self->change=0;
    
    return(TRUE);
}

void sbutton__FinalizeObject(c, self)
struct classheader *c;
struct sbutton *self;
{
    /*
      Finalize the object instance data.
      */
    int i=self->count;
    if(self->prefs) sbutton_FreePrefs(self->prefs);
    if(self->matteprefs) sbutton_FreePrefs(self->matteprefs);
    
    while(--i>=0) {
	struct sbutton_info *bi=self->buttons+i;
	if(bi->label) free(bi->label);
	if(bi->prefs) sbutton_FreePrefs(bi->prefs);
    }
    if(self->buttons) {
	free(self->buttons);
	self->buttons=NULL;
    }
    return;
}

void sbutton__SetChangeFlag(self, on)
struct sbutton *self;
long on;
{
    self->change|=on;
}


long sbutton__GetChangeType(self)
struct sbutton *self;
{
    return self->change;
}

static char *sizepolicies[]={
    "NoneSet",
    "Fixed",
    "Rows",
    "Columns",
    "Proc"
};

static void sbutton__WriteDataPart(self, fp)
struct sbutton *self;
FILE *fp;
{
    /*
      Write the object data out onto the datastream.
      */
    char buf[1024];

    int j, c=0;
    int i=self->count;

    for(j=0;j<self->count;j++) {
	self->buttons[j].prefs->writeid=(-1);
    }

    for(j=0;j<self->count;j++) {
	if(self->buttons[j].prefs->writeid==-1 && self->buttons[j].prefs!=self->prefs) {
	    c++;
	    self->buttons[j].prefs->writeid=(-2);
	}
    }
    
    sprintf(buf, "%d %d %d %s %d", i, self->rows, self->cols, sizepolicies[(unsigned int)self->sizepolicy], c+1);
    WriteLine(fp, buf);

    if(self->prefs) {
	sbutton_WritePrefs(fp, self->prefs);
	self->prefs->writeid=0;
    }
    j=0;
    while(--i>=0) {
	if(self->buttons[i].prefs && self->buttons[i].prefs->writeid<0) {
	    self->buttons[i].prefs->writeid=(++j);
	    sbutton_WritePrefs(fp, self->buttons[i].prefs);
	}
	if(sbutton_GetLabel(self, i)) {
	    fprintf(fp, "\\label\n");
	    WriteLine(fp, sbutton_GetLabel(self, i));
	} else fprintf(fp, "\\label\n\n");
	fprintf(fp,"\\litp\n");
	WriteLine(fp, self->buttons[i].lit?True:False);
	fprintf(fp,"\\prefs\n");
	sprintf(buf,"%d", self->buttons[i].prefs->writeid);
	WriteLine(fp, buf);
	if(self->buttons[i].trigger) {
	    fprintf(fp, "\\trigger\n");
	    WriteLine(fp, atom_Name(self->buttons[i].trigger));
	}
	
	if(i>0) fprintf(fp,"\\end\n\n");
    }
    fprintf(fp, "\\done\n\n");
}


long sbutton__Write(self, fp, id, level)
struct sbutton *self;
FILE *fp;
long id;
int level;
{
    long uniqueid = sbutton_UniqueID(self);

    if (id != sbutton_GetWriteID(self)) {
	/* New Write Operation */
	sbutton_SetWriteID(self, id);

	fprintf(fp, "\\begindata{%s,%d}\nDatastream version: %d\n",
		class_GetTypeName(self), 
		uniqueid, DS_VERSION);

	sbutton__WriteDataPart(self, fp);

	fprintf(fp, "\\enddata{%s,%d}\n", class_GetTypeName(self), uniqueid);
    }
    return(uniqueid);
}
struct read_status {
    int lastbutton;
    int lastprefs, maxprefs;
    struct sbutton_prefs **prefs;
};

static boolean newprefsproc(self, rock, buf)
struct sbutton *self;
struct read_status *rock;
char *buf;
{
    if(rock->prefs==NULL) return TRUE;
    rock->lastprefs++;
    if(rock->lastprefs<0 || rock->lastprefs>=rock->maxprefs) {
	rock->lastprefs=rock->maxprefs-1;
	return TRUE;
    }
    return FALSE;
}

static boolean labelproc(self, rock, buf)
struct sbutton *self;
struct read_status *rock;
char *buf;
{
    rock->lastbutton--;
    if(rock->lastbutton<0) return TRUE;
    if(*buf!='\0') sbutton_SetLabel(self, rock->lastbutton, buf);
    return FALSE;
}

static boolean litpproc(self, rock, buf)
struct sbutton *self;
struct read_status *rock;
char *buf;
{
    sbutton_SetLit(self, rock->lastbutton, *buf=='T');
    return FALSE;
}

static boolean styleproc(self, rock, buf)
struct sbutton *self;
struct read_status *rock;
char *buf;
{
    if(rock->lastprefs<0 || rock->lastprefs>=rock->maxprefs) return TRUE;
    sbutton_GetStyle(rock->prefs[rock->lastprefs]) = atoi(buf);
    return FALSE;
}

static boolean fontproc(self, rock, buf)
struct sbutton *self;
struct read_status *rock;
char *buf;
{
    struct fontdesc *font=NULL;
    char buf2[1024];
    long style, size;
    
    if(rock->lastprefs<0 || rock->lastprefs>=rock->maxprefs) return TRUE;
    
    if(*buf=='\0') return FALSE;
    
    if(fontdesc_ExplodeFontName(buf, buf2, sizeof(buf2), &style, &size))	{
	font=fontdesc_Create(buf2,style,size);
	sbutton_GetFont(rock->prefs[rock->lastprefs]) = font;
    }
    return FALSE;
}

static boolean bgproc(self, rock, buf)
struct sbutton *self;
struct read_status *rock;
char *buf;
{
    if(rock->lastprefs<0 || rock->lastprefs>=rock->maxprefs) return TRUE;
    if(*buf!='\0') sbutton_GetBackground(rock->prefs[rock->lastprefs]) = Intern(buf);
    return FALSE;
}

static boolean fgproc(self, rock, buf)
struct sbutton *self;
struct read_status *rock;
char *buf;
{
    if(rock->lastprefs<0 || rock->lastprefs>=rock->maxprefs) return TRUE;
    if(*buf!='\0') sbutton_GetForeground(rock->prefs[rock->lastprefs]) = Intern(buf);
    return FALSE;
}


static boolean tsproc(self, rock, buf)
struct sbutton *self;
struct read_status *rock;
char *buf;
{
    if(rock->lastprefs<0 || rock->lastprefs>=rock->maxprefs) return TRUE;
    if(*buf!='\0') sbutton_GetTopShadow(rock->prefs[rock->lastprefs]) = Intern(buf);
    return FALSE;
}

static boolean topproc(self, rock, buf)
struct sbutton *self;
struct read_status *rock;
char *buf;
{
    if(rock->lastprefs<0 || rock->lastprefs>=rock->maxprefs) return TRUE;
    if(*buf!='\0') sbutton_GetTop(rock->prefs[rock->lastprefs]) = Intern(buf);
    return FALSE;
}


static boolean bsproc(self, rock, buf)
struct sbutton *self;
struct read_status *rock;
char *buf;
{
    if(rock->lastprefs<0 || rock->lastprefs>=rock->maxprefs) return TRUE;
    if(*buf!='\0') sbutton_GetBottomShadow(rock->prefs[rock->lastprefs]) = Intern(buf);
    return FALSE;
}


static boolean lfgproc(self, rock, buf)
struct sbutton *self;
struct read_status *rock;
char *buf;
{
    if(rock->lastprefs<0 || rock->lastprefs>=rock->maxprefs) return TRUE;
    if(*buf!='\0') sbutton_GetLabelFG(rock->prefs[ rock->lastprefs]) = Intern(buf);
    return FALSE;
}


static boolean lbgproc(self, rock, buf)
struct sbutton *self;
struct read_status *rock;
char *buf;
{
    if(rock->lastprefs<0 || rock->lastprefs>=rock->maxprefs) return TRUE;
   if(*buf!='\0') sbutton_GetLabelBG(rock->prefs[rock->lastprefs]) = Intern(buf);
   return FALSE;
}

static boolean nameproc(self, rock, buf)
struct sbutton *self;
struct read_status *rock;
char *buf;
{
    if(rock->lastprefs<0 || rock->lastprefs>=rock->maxprefs) return TRUE;
    if(*buf!='\0') sbutton_GetName(rock->prefs[rock->lastprefs]) = NewString(buf);
    else {
	char buf2[256];
	sprintf(buf2, rock->lastprefs?"Default %d":"Default", rock->lastprefs);
	sbutton_GetName(rock->prefs[rock->lastprefs]) = NewString(buf2);
    }
    return FALSE;}

static boolean prefsproc(self, rock, buf)
struct sbutton *self;
struct read_status *rock;
char *buf;
{
    int i=atoi(buf);
    if(i<0 || i>=rock->maxprefs) return TRUE;
    sbutton_SetPrefs(self, rock->lastbutton, rock->prefs[i]);
    return FALSE;
}

static boolean doneproc(self, rock, buf)
struct sbutton *self;
struct read_status *rock;
char *buf;
{
    return TRUE;
}

static boolean triggerproc(self, rock, buf)
struct sbutton *self;
struct read_status *rock;
char *buf;
{
    sbutton_SetTrigger(self, rock->lastbutton, buf);
    return FALSE;
}

static struct dataprocs {
    char *name;
    procedure func;
} sprocs[]={
    {"newprefs", (procedure)newprefsproc},
    {"label", (procedure)labelproc},
    {"litp", (procedure)litpproc},
    {"prefs", (procedure)prefsproc},
    {"style", (procedure)styleproc},
    {"font", (procedure)fontproc},
    {"background", (procedure)bgproc},
    {"topshadow", (procedure)tsproc},
    {"top", (procedure)topproc},
    {"bottomshadow", (procedure)bsproc},
    {"foreground", (procedure)fgproc},
    {"labelforeground", (procedure)lfgproc},
    {"labelbackground", (procedure)lbgproc},
    {"name", (procedure)nameproc},
    {"trigger", (procedure)triggerproc},
    {"end", (procedure) NULL},
    {"done", (procedure)doneproc},
    {NULL, NULL}
};

static long dostuff(self, fp, rock, procs)
struct sbutton *self;
FILE *fp;
long rock;
struct dataprocs *procs;
{
    char *buf, *buf2;
    boolean done=FALSE;
    while(!done) {
	struct dataprocs *dps;
	buf=ReadLine(fp);
	if(buf==NULL) return dataobject_PREMATUREEOF;
	if(!linehascontrol) {
	    free(buf);
	    continue;
	}
	buf2=ReadLine(fp);
	if(buf2==NULL) {
	    free(buf);
	    return dataobject_PREMATUREEOF;
	}
	for(dps=procs;dps->name;dps++) {
	    if(!strcmp(dps->name, buf+1)) {
		if(dps->func) done=((boolean (*)())dps->func)(self, rock, buf2);
		break;
	    }
	}
	free(buf);
	free(buf2);
    }
    return dataobject_NOREADERROR;
}


 
static long sbutton__ReadDataPart(self, fp, dsversion)
struct sbutton *self;
FILE *fp;
int dsversion;
{
    /*
      Read in the object from the file.
      */
    char *buf, *p;
    int rows, cols, count;
    enum sbutton_sizepolicy policy;
    long err=0;
    struct read_status rs;
    rs.maxprefs=0;
    rs.lastprefs=(-1);
    rs.lastbutton=(-1);
    rs.prefs=NULL;
    if ((buf = ReadLine(fp)) == NULL)
	    return(dataobject_PREMATUREEOF);
    count=atoi(buf);
    p=index(buf,' ');
    if(p) {
	rows=atoi(p+1);
	p=index(p+1, ' ');
	if(p) {
	   cols=atoi(p+1);
	   p=index(p+1, ' ');
	   if(p) {
	       switch(p[1]) {
		   case 'N':
		       policy=sbutton_NoneSet;
		       break;
		   case 'F':
		       policy=sbutton_SizeFixed;
		       break;
		   case 'R':
		       policy=sbutton_GrowRows;
		       break;
		   case 'C':
		       policy=sbutton_GrowColumns;
		       break;
		   case 'P':
		       policy=sbutton_ResizeFunction;
		       break;
		   default:
		       policy=sbutton_GrowRows;
	       }
	       sbutton_SetLayout(self, rows, cols, policy);
	       p=index(p+1, ' ');
	       if(p) rs.maxprefs=atoi(p+1);
	   }
	}
    }
    free(buf);
    if(count<=0) return dataobject_PREMATUREEOF;

    rs.lastbutton=count;
    if(rs.maxprefs>0 && rs.maxprefs<=count+1) {
	int i;
	rs.prefs=(struct sbutton_prefs **)malloc(sizeof(struct sbutton_prefs *)*rs.maxprefs);
	if(rs.prefs==NULL) return dataobject_PREMATUREEOF;
	for(i=0;i<rs.maxprefs;i++) {
	    if(self->prefs==NULL) SetupInitialState(self);
	    rs.prefs[i] = sbutton_DuplicatePrefs(self->prefs, NULL);
	    if(rs.prefs[i]==NULL) return dataobject_PREMATUREEOF;
	    rs.prefs[i]->refcount=1;
	}
    } else return dataobject_NOREADERROR;
    err = dostuff(self, fp, (long)&rs, sprocs);
    if(rs.prefs!=NULL) {
	int i;
	if(self->prefs) sbutton_FreePrefs(self->prefs);
	self->prefs=rs.prefs[0];
	for(i=1;i<rs.maxprefs;i++) {
	    if(rs.prefs[i]) sbutton_FreePrefs(rs.prefs[i]);
	}
    }
    return err;
}


static long sbutton_SanelyReturnReadError(self, fp, id, code)
struct sbutton *self;
FILE *fp;
long id;
long code;
{
    /*
      Suck up the file until our enddata, then return the error code.
      */
    char *buf, buf2[255];

    buf = NULL;
    sprintf(buf2, "\\enddata{%s,%ld}", class_GetTypeName(self), id);
    do {
	if (buf != NULL) free(buf);
	if ((buf = ReadLine(fp)) == NULL)
	    return(dataobject_PREMATUREEOF);
    } while (strncmp(buf, "\\enddata{", 9) != 0); /* find an enddata */

    if (strcmp(buf, buf2) != 0) {
	free(buf);
	return(dataobject_MISSINGENDDATAMARKER); /* not ours! */
    }
    free(buf);

    return(code);
}


long sbutton__Read(self, fp, id)
struct sbutton *self;
FILE *fp;
long id;
{

  char *buf;
  int dsversion;
  long err=dataobject_NOREADERROR;
  
  sbutton_SetID(self, sbutton_UniqueID(self));
  
  if ((buf = ReadLine(fp)) == NULL) err=dataobject_PREMATUREEOF;
  else if (strncmp(buf,"Datastream version:",19)) {
      err=dataobject_BADFORMAT;
  } else if ((dsversion = atoi(buf+19)) > DS_VERSION)	{
      err=dataobject_BADFORMAT;
  }
  if(buf) free(buf);

  if(err==dataobject_NOREADERROR) {
      return sbutton_SanelyReturnReadError(self, fp, id, sbutton__ReadDataPart(self, fp, dsversion));
  } else return(sbutton_SanelyReturnReadError(self, fp, id, err));
}


char *sbutton__ViewName(self)
struct sbutton *self;
{
    return "sbttnav";
}

void sbutton__ActivateButton(self, ind)
struct sbutton *self;
int ind;
{
    if(ind<0 || ind>=self->count) return;
    if(self->singleactivation && self->activated>=0 && self->activated<self->count && self->activated!=ind) sbutton_DeActivateButton(self, self->activated);
    if(self->buttons[ind].lit) return;
    self->activated=ind;
    self->buttons[ind].lit=TRUE;
    sbutton_SetModified(self);
    sbutton_SetChangeFlag(self, sbutton_ACTIVATIONCHANGED);
    sbutton_NotifyObservers(self, ind+sbutton_CHANGEBASE);
}

void sbutton__DeActivateButton(self, ind)
struct sbutton *self;
int ind;
{
    if(ind<0 || ind>=self->count) return;
    if(self->activated==ind) self->activated=(-1);
    if(!self->buttons[ind].lit) return;
    self->buttons[ind].lit=FALSE;
    sbutton_SetModified(self);
    sbutton_SetChangeFlag(self, sbutton_ACTIVATIONCHANGED);
    sbutton_NotifyObservers(self, ind+sbutton_CHANGEBASE);
}

void sbutton__Actuate(self, ind)
struct sbutton *self;
int ind;
{
    struct owatch_data *w1=owatch_Create(self);
    if(ind>=0 && ind<self->count && self->hitfunc) self->hitfunc(self, self->hitfuncrock, ind, self->buttons[ind].rock);
    if(owatch_CheckAndDelete(w1)) {
	if(self->buttons[ind].trigger) sbutton_PullTrigger(self, self->buttons[ind].trigger);
	else sbutton_PullTrigger(self, buttonpushed);
    }
}

void sbutton__SetRock(self, ind, rock)
struct sbutton *self;
int ind;
long rock;
{
    if(!sbutton_EnsureSize(self, ind)) return;

    self->buttons[ind].rock=rock;
}

void sbutton__SetLit(self, ind, onoff)
struct sbutton *self;
int ind;
boolean onoff;
{
    if(!sbutton_EnsureSize(self, ind)) return;
    self->buttons[ind].lit=onoff;
    sbutton_SetModified(self);
    sbutton_SetChangeFlag(self, sbutton_ACTIVATIONCHANGED);
    sbutton_NotifyObservers(self, ind+sbutton_CHANGEBASE);
}


void sbutton__SetPrefs(self, ind, prefs)
struct sbutton *self;
int ind;
struct sbutton_prefs *prefs;
{
    if(!sbutton_EnsureSize(self, ind) || prefs==NULL) return;
    sbutton_FreePrefs(self->buttons[ind].prefs);
    self->buttons[ind].prefs=prefs;
    prefs->refcount++;
    sbutton_SetModified(self);
    sbutton_SetChangeFlag(self, sbutton_FONTCHANGED|sbutton_STYLECHANGED|sbutton_SIZECHANGED);
    sbutton_NotifyObservers(self, ind+sbutton_CHANGEBASE);
}

void sbutton__SetLayout(self, rows, cols, policy)
struct sbutton *self;
int rows, cols;
enum sbutton_sizepolicy policy;
{
    if(rows>0) self->rows=rows;
    if(cols>0) self->cols=cols;
    if(policy!=sbutton_NoneSet) self->sizepolicy=policy;
    if(self->rows*self->cols<self->count) {
	switch(self->sizepolicy) {
	    case sbutton_SizeFixed:
		break;
	    case sbutton_GrowRows:
		self->rows=self->count/self->cols;
		if(self->count%self->cols) self->rows++;
		break;
	    case sbutton_GrowColumns:
		self->cols=self->count/self->rows;
		if(self->count%self->rows) self->cols++;
		break;
	    case sbutton_ResizeFunction:
		if(self->resizefunc) self->resizefunc(self, self->resizerock);
	}
    }
    sbutton_SetModified(self);
    sbutton_SetChangeFlag(self, sbutton_SIZECHANGED);
    sbutton_NotifyObservers(self, observable_OBJECTCHANGED);
}

void sbutton__Delete(self, ind)
struct sbutton *self;
int ind;
{
    int i;
    struct sbutton_prefs *prefs;
    char *label;
    if(ind<0 || ind>=self->count) return;
    
    prefs=sbutton_GetPrefs(self, ind);
    label=sbutton_GetLabel(self, ind);
    
    if(prefs) sbutton_FreePrefs(prefs);
    if(label) free(label);
    
    for(i=ind;i<self->count-1;i++) {
	self->buttons[i]=self->buttons[i+1];
    }
    
    self->count--;
    init(self, self->count, self->count);
    
    sbutton_EnsureSize(self, 0);
    
    sbutton_SetModified(self);
    sbutton_SetChangeFlag(self, sbutton_SIZECHANGED);
    sbutton_NotifyObservers(self, observable_OBJECTCHANGED);
    return;
}

void sbutton__Swap(self, i1, i2)
struct sbutton *self;
int i1, i2;
{
    struct sbutton_info si;
    if(i1<0 || i1>=self->count) return;
    if(i2<0 || i2>=self->count) return;

    si=self->buttons[i1];
    self->buttons[i1]=self->buttons[i2];
    self->buttons[i2]=si;
    sbutton_SetModified(self);
    sbutton_SetChangeFlag(self, sbutton_ALLCHANGED);
    sbutton_NotifyObservers(self, observable_OBJECTCHANGED);
}
	
void sbutton__SetLabel(self, ind, txt)
struct sbutton *self;
int ind;
char *txt;
{
/*
  Set the text label for this object.
*/
    if(!sbutton_EnsureSize(self, ind)) return;
    
    if (self->buttons[ind].label) {
      if(txt && !strcmp(self->buttons[ind].label, txt)) return;
      free(self->buttons[ind].label);
      self->buttons[ind].label = NULL;
    }
    
    if (txt)self->buttons[ind].label = NewString(txt);
    sbutton_SetModified(self);
    sbutton_SetChangeFlag(self, sbutton_LABELCHANGED);
    sbutton_NotifyObservers(self, ind+sbutton_CHANGEBASE);
}

void sbutton__SetTrigger(self, ind, txt)
struct sbutton *self;
int ind;
char *txt;
{
/*
  Set the text label for this object.
*/
    if(!sbutton_EnsureSize(self, ind)) return;
    self->triggerchange=ind;
    if(*txt=='\0') {
	self->buttons[ind].trigger=NULL;
    } else {
	self->buttons[ind].trigger=atom_Intern(txt);
	if(self->buttons[ind].trigger!=NULL) {
	    observable_DefineTrigger(self, self->buttons[ind].trigger);
	}
    }
    sbutton_SetModified(self);
    sbutton_SetChangeFlag(self, sbutton_TRIGGERCHANGED);
    sbutton_NotifyObservers(self, ind+sbutton_CHANGEBASE);
}

int sbutton__Enumerate(self, func, rock)
struct sbutton *self;
boolean (*func)();
long rock;
{
    int i;
    for(i=0;i<self->count;i++) {
	if(func(self, i, &self->buttons[i], rock)) return i;
    }
    return -1;
}

static void WriteLine(f, l)
FILE *f;
char *l;
{
    /* 
      Output a single line onto the data stream, quoting
      back slashes and staying within line length limits.
      Warning:  this routine wasn't meant to handle embedded
  newlines.
  */

    char buf[MAX_LINE_LENGTH];
    int i = 0;

    for (;*l != '\0'; ++l) {
	if (i > (MAX_LINE_LENGTH - 5)) {
	    buf[i++] = '\\';  /* signal for line continuation */
	    buf[i++] = '\n';
	    buf[i++] = '\0';
	    fputs(buf,f);
	    i = 0;
	} /* if (i > ...) */
	switch (*l) {
	    case '\\': 
		/* if a backslash, quote it. */
		buf[i++] = '\\';
		buf[i++] = *l;
		break;
	    default:
		buf[i++] = *l;
	} /* switch (*l) */
    } /* for (; *l != ... ) */

    /* Need to empty buffer */
    if ((i > 0) && (buf[i-1]==' ')) {
	/* don't allow trailing whitespace */
	buf[i++] = '\\';
	buf[i++] = '\n';
	buf[i++] = '\0';
	fputs(buf,f);
	fputs("\n",f);
    } else {
	buf[i++] = '\n';
	buf[i++] = '\0';
	fputs(buf,f);
    }
}


static char *GlomStrings(s, t)
char *s, *t;
{
    /* 
      Safely (allocs more memory) concatenates the two strings, 
      freeing the first.  Meant to build a new string of unknown length.
      */

    char *r;

    if (r = (char *)malloc(strlen(s)+strlen(t)+1)) {
	*r = '\0';
	strcpy(r,s);
	free(s);
	strcat(r,t);
	return(r);
    } else {
	free(s);
	return(NULL);
    }
}

static char *ReadLine(f)
FILE *f;
{
    /* 
      Reads from the datastream, attempting to return a single string.
      Undoes quoting and broken lines.
      Warning:  this routine wasn't meant to handle embedded
	 newlines.
	 Warning:  possible source of memory leaks;  remember to 
	   free the returned string when finished with it!
	   */

    char buf[MAX_LINE_LENGTH], /* (BUG) What if the datastream is broken? */
    buf2[MAX_LINE_LENGTH],
    *result;
    int i,j;

    linehascontrol=FALSE;
    if (result = (char *)malloc(1)) {
	*result = '\0';

	while (fgets(buf,sizeof(buf),f)) {
	    for (i = 0, j = 0; buf[i] != '\0'; ++i) {
		switch (buf[i]) {
		    case '\\':
			/* Unquote backslash or splice line */
			switch (buf[++i]) {
			    case '\\':
				/* Unquote the backslash */
				buf2[j++] = buf[i];
				break;
			    case '\n':
				/* broke long line */
				break;
			    default:
				/* things like \enddata come through here */
				linehascontrol=TRUE;
				buf2[j++] = '\\';
				buf2[j++] = buf[i];
				break;
			} /* switch (buf[++i]) */
			break;
		    case '\n':
			/* An unquoted newline means end of string */
			buf2[j++] = '\0';
			result = GlomStrings(result, buf2);
			return(result);
		    default:
			buf2[j++] = buf[i];
			break;
		} /* switch (buf[i]) */
	    } /* for (i = 0, ...) */
	    buf2[j++] = '\0';
	    result = GlomStrings(result, buf2);
	} /* while (fgets...) */
	/* Should not get here... it means we went off the end
	 of the data stream.  Ooops. */
    } /* if (result = ... ) */
    return(NULL);
}

static char *EncodeFont(font)
struct fontdesc *font;
{
    /*
      Returns a string representing the name of the font for this object.
	  (BUG) I shouldn't have to do this function, it should be a method
		of the fontdesc object.  In any case, I handle only Bold, Italic,
		    and fixed styles.
		    */

    char *buf, type[15];
    long myfonttype, myfontsize;
    char *myfontname;

    *type = '\0';
    myfontname = fontdesc_GetFontFamily(font);
    myfontsize = fontdesc_GetFontSize(font);
    myfonttype = fontdesc_GetFontStyle(font);
    if (myfonttype & fontdesc_Bold) strcpy(type,"b");
    if (myfonttype & fontdesc_Italic) strcpy(type,"i");
    if (myfonttype & fontdesc_Fixed) strcpy(type,"f");
    if (buf = (char *)malloc(strlen(myfontname)+25)) {
	sprintf(buf,"%s%d%s", myfontname, myfontsize, type);
	return (buf);
    } else {
	return(NULL);
    }
}


struct sbutton *sbutton__CreateSButton(classID, prefs)
struct classheader *classID;
struct sbutton_prefs *prefs;
{
    struct sbutton *self=sbutton_New();
    int i;
    if(!self) return self;

    /* get rid of default contents... */

    if(self->prefs) sbutton_FreePrefs(self->prefs);
    
    i=self->count;
    if(self->buttons) {
	while(--i>=0) {
	    struct sbutton_info *bi=self->buttons+i;
	    if(bi->prefs) sbutton_FreePrefs(bi->prefs);
	    if(bi->label) free(bi->label);

	}
	free(self->buttons);
	self->buttons=NULL;
    }

    self->prefs=prefs;
    prefs->refcount++;
    
    self->count=0;
    self->maxcount=0;


    (void) sbutton_EnsureSize(self, 0);

    self->change=0;
    
    return self;
}


struct sbutton *sbutton__CreateFilledSButton(classID, prefs, blist)
struct classheader *classID;
struct sbutton_prefs *prefs;
struct sbutton_list *blist;
{
    struct sbutton *self=sbutton_CreateSButton(prefs);
    int count=0;
    struct sbutton_list *bl=blist;

    if(!self) return self;

    if(bl) while(bl->label) {
	count++;
	bl++;
    }
    while(--count>=0) {
	sbutton_SetLabel(self, count, blist[count].label);
	if(blist[count].trigger) sbutton_SetTrigger(self, count, blist[count].trigger);
	sbutton_SetRock(self, count, blist[count].rock);
	sbutton_SetLit(self, count, blist[count].lit);
    }
    return self;
}

void sbutton__NotifyObservers(self, v)
struct sbutton *self;
long v;
{
    /* Do all the notifications, then reset the change flag. */
    super_NotifyObservers(self, v);
    self->change=0;
}

