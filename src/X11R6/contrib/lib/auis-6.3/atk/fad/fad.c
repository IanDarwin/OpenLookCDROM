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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/fad/RCS/fad.c,v 2.9 1992/12/15 21:35:02 rr2b R6tape $";
#endif

#include <class.h>
#include <dataobj.ih>
#include <fad.eh>
#include <fontdesc.ih>
#include <attribs.h>
#define STARTHEIGHT 256
/* *********
struct fad *fad__NewData()
{
	register struct fad *np;
	np = (struct fad *)malloc(sizeof(struct fad));
	fad_InitData(np);
	return(np);
	}
***************** */
static struct fontdesc *my_DefineFont(fname)
char *fname;
{
	char familyname[256];
	long fontStyle;
	long  fontSize;
	fontdesc_ExplodeFontName(fname,familyname, sizeof(familyname), &fontStyle, &fontSize);
	return fontdesc_Create(familyname,  fontStyle, fontSize);
	}
static struct vector *newvector(p1,p2)
struct fadpoint *p1,*p2;
{
	register struct vector *nv;
	nv = (struct vector *)malloc( sizeof(struct vector));
	nv->p1 = p1;
	nv->p2 = p2;
	nv->v = NULL;
	nv->label = NULL;
	nv->mode = LINEMODE;
	return(nv);
	}
void fad__SetAttributes(self, attributes)
struct fad *self;
struct attributes *attributes;
{

    super_SetAttributes(self, attributes);
    while (attributes) {
	if (strcmp(attributes->key, "readonly") == 0) {
	    fad_SetReadOnly(self, attributes->value.integer);
	}
	attributes = attributes->next;
    }
}
void fad__SetReadOnly(self, readOnly)
struct fad *self;
boolean readOnly;
{
    self->readonly = readOnly;
}
struct fadpoint *fad__newpoint(self,x,y)
struct fad *self;
long x,y;
{
	struct fadpoint *np,**iip;
	np = (struct fadpoint *)malloc(sizeof(struct fadpoint));
	np->x = x;
	np->y = y;
	np->p = NULL;
	if(ISICON(x)){
		for(iip = self->iconpoints; iip < self->iconpointend;iip++){
			if((*iip)->x == x && (*iip)->y == y) break;
			}
		 if(iip == self->iconpointend) *(self->iconpointend++) = np;
		}
	return(np);
	}

struct fad_frame *fad__newframe(self)
struct fad *self;
{
	register struct fad_frame *fp;
	fp = (struct fad_frame *)malloc(sizeof(struct fad_frame));
	fp->f = NULL;
	fp->p = NULL;
	fp->v = NULL;
	fp->f = NULL;
	return(fp);
	}

boolean fad__InitializeObject(classID, self)
    struct classheader *classID;
struct fad *self;
{
    int i;
	self->f = fad_newframe(self);
	self->bf = self->f;
	self->deleated = NULL;
	self->Frames = 30;
        *(self->fadname) = '\0';
	*(self->cfname) = '\0';
	*(self->currentistr) = '\0'; 
	self->iconpointend = self->iconpoints;
	self->iconpointnow = NULL;self->iconmode = 0;
	self->topinmp = 1; self->fad_square = 0;self->frtime = FRTIME;
	self->initializedfonts = 1;
	self->readonly = 0;self->currentfont = NULL;
	self->fp = NULL; self->lp = NULL;self ->pltnum = 0;
	self->ox = 0; self->oy = 0;
	self->desw = MAXWIDTH; self ->desh = STARTHEIGHT;
	strcpy(self->labelfontname,"andy12");
	self->labelfont = my_DefineFont("andy12");
	self->currentfont = NULL;
	self->mode = LINEMODE;
	for(i = 0; i < 15; i++)
	    self->fontpt[i] = NULL;
	for(i = 0; i < 200; i++)
	    self->iconpoints[i] = NULL;
	return TRUE;
	}

void fad__SetName(self,name)
struct fad *self;
{
	strcpy(self->fadname,name);
}
struct fadpoint *fad__setpoint(self,x,y,type,f)
struct fad *self;
long x,y;
int type;
struct fad_frame *f;
{
	register struct fadpoint *pt;
	if((pt = f->p) == NULL){
		if(type == OLD) return(NULL);
		f->p = fad_newpoint(self,x,y);
		return(f->p);
		}
	while(1){
		if(pointmatch(self,pt,x,y)) return(pt);
		if(pt->p == NULL) break;
		pt = pt->p;
		}
	if(type == OLD) return(NULL);
	pt->p = fad_newpoint(self,x,y);
	return(pt->p);
	}
static pointmatch(self,pt,x,y)
struct fad *self;
struct fadpoint *pt;
long x,y;
{
	if(ISICONORLABEL(x))
		return(x == pt->x && y == pt->y);
	return((x < pt->x + PFUG) && (x > pt->x - PFUG )&& 
	  (y < pt->y + PFUG) &&( y > pt->y - PFUG)) ;
	}
struct vector *fad__setvector(self,pp1,pp2,f)
struct fad *self;
struct fadpoint *pp1,*pp2;
struct fad_frame *f;
{
	register struct vector *vec,*pv;
	vec = newvector(pp1,pp2);
	if(f->v == NULL)
		 f->v = vec;
	else{
		for(pv = f->v; pv->v != NULL; pv = pv->v) ;
		pv->v = vec;
		}
	return(vec);
	}
void fad__delvector(self,f)
struct fad *self;
struct fad_frame *f;
{
	register struct vector *vec,*pv;
	vec = NULL;
	if(f == NULL) return;
	pv = f->v;
	if(f->v != NULL){
		for(pv = f->v; pv->v != NULL; pv = pv->v)
			vec = pv ;
		if(vec == NULL) f->v = NULL;
		else vec->v = NULL;
		if(pv->label) free(pv->label);
		free((char *)pv);
		}
	return;
	}
short fad__iconnum(self,s)
struct fad *self;
char *s;
{
	int i;
	char *c;
	for(i = 1; i < self->topinmp; i++){
		if(strcmp(s,self->inmp[i]) == 0) break;
		}
	if(i == self->topinmp){
		/* need test for too many fonts */
		if(self->topinmp == 1)  c = self->iconnamebuf;
		else for(c = self->inmp[self->topinmp - 1]; *c != '\0'; c++) ;
		self->inmp[self->topinmp++] = ++c;
		strcpy(c,s);
		self->currentfont = my_DefineFont(c);
		self->fontpt[i] = self->currentfont;
		}
	else self->currentfont = self->fontpt[i]; 
#ifdef DEBUG
	fprintf(stderr,"i = %d, self->topinmp = %d,s = %s\n",
		i,self->topinmp,s);
#endif /* DEBUG */
	return(-i);
	}
		
void fad__freeframe(self,ff)
struct fad *self;
struct fad_frame *ff;
{
	register struct fadpoint *pt,*nextpt;
	struct vector *vv,*nextvv;
	for(pt = ff->p;pt != NULL; pt = nextpt){
		nextpt = pt->p;
		free((char*)pt);
		}
	for(vv = ff->v; vv != NULL; vv = nextvv){
		nextvv = vv->v;
                if(vv->label) free(vv->label);
		free((char *)vv);
		}
	}
long fad__Write(self,f,writeid,level)
struct fad *self;
FILE *f;
long writeid; 
int level;
{
	int i;
	struct vector *vv;
	struct fad_frame *ff;
    if (self->header.dataobject.writeID == writeid)  return fad_GetID(self);
    self->header.dataobject.writeID = writeid;
	fprintf(f,"\\begindata{fad,%ld}\n",self->header.dataobject.id);
	for(i = 1; i < self->topinmp; i++)
		fprintf(f,"$N %s\n",self->inmp[i]);
	fprintf(f,"$C %d\n",self->Frames);
	fprintf(f,"$T %d\n",self->frtime);
	fprintf(f,"$L %s\n",self->labelfontname);
	fprintf(f,"$P %d,%d,%d,%d\n",self->ox,self->oy,self->desw,self->desh);
	for(ff = self->bf; ff != NULL; ff= ff->f){
		fprintf(f,"$F\n");
		for(vv = ff->v; vv != NULL; vv = vv->v){
		        if(vv->mode == BOXMODE)
				fprintf(f,"$B %d,%d %d,%d\n",vv->p1->x,vv->p1->y,vv->p2->x,vv->p2->y);
		        else if(vv->mode == ANIMATEMODE)
				fprintf(f,"$A %d,%d %d,%d\n",vv->p1->x,vv->p1->y,vv->p2->x,vv->p2->y);
			else if(vv->label == NULL)
				fprintf(f,"$V %d,%d %d,%d\n",vv->p1->x,vv->p1->y,vv->p2->x,vv->p2->y);
			else
				fprintf(f,"$S %d,%d\n%s\n",vv->p1->x,vv->p1->y,vv->label);
			}
		}
	fprintf(f,"$$\n");
	fprintf(f,"\\enddata{fad,%ld}\n",self->header.dataobject.id);
	return fad_GetID(self);
	}
long fad__Read(self,f,id)
struct fad *self;
FILE *f;
long id;
{
	char *c,s[256],*cc,*cp,str[256];
	int p1x,p1y,p2x,p2y,newf = 0,szz;
	struct vector *vv;
	struct fad_frame *ff = NULL;
	struct fadpoint *fp,*lp;
        if(id != 0L)self->header.dataobject.id = dataobject_UniqueID(self);
        self->header.dataobject.modified = 0;
    /*Not currently concerned with embedded objects */
	c = s;
	c++;
	cc = c + 2;
	while(fgets(s,256,f) != NULL){
		if(*s == '\\'){
                    if(strncmp(s,"\\enddata",8) == 0) break;
                    if(strncmp(s,"\\begindata{",11) == 0){
                        char nnm[64]; long foo;
                        sscanf(s,"\\begindata{%s,%ld}",nnm,&foo);
                        if(strcmp(nnm,"fad") == 0) self->header.dataobject.id = foo;
                    }
                }
		if(*s != '$') continue;
		switch(*c){
		case 'T': 
			self->frtime  = atoi(c+2);
			break;
		case 'C':
			self->Frames = atoi(c+2);
			break;
		case 'L':
			strcpy(self->labelfontname,c+2);
			for(cp = self->labelfontname; *cp != '\n' && *cp != '\0'; cp++);
			*cp = '\0';
			self->labelfont = my_DefineFont(self->labelfontname);
			break;
		case 'N':
			for(cp = cc; *cp != '\n' && *cp != '\0'; cp++);
			*cp = '\0';
			fad_iconnum(self,cc);
			break;
		case 'F':
			if(newf){
				ff->f = fad_newframe(self);
				ff = ff->f;
				}
			else newf = 1;
			break;
		case 'P':
			sscanf(s,"$P %d,%d,%d,%d\n",&p1x,&p1y,&p2x,&p2y);
			self->ox = 0; self->oy = 0; /* should be read as 0 */
			self->desw = p2x; self->desh = p2y;
			ff = self->f;
			self->bf = ff;
			newf = 0;
			break;
		case 'V':
			sscanf(s,"$V %d,%d %d,%d\n",&p1x,&p1y,&p2x,&p2y);
			fp = fad_setpoint(self,p1x,p1y,NEW,ff);
			lp = fad_setpoint(self,p2x,p2y,NEW,ff);
			vv = fad_setvector(self,fp,lp,ff);
			if(ISICON(p2x)) self->currenticon = p2y;
			break;
		case 'A':
			sscanf(s,"$A %d,%d %d,%d\n",&p1x,&p1y,&p2x,&p2y);
			fp = fad_setpoint(self,p1x,p1y,NEW,ff);
			lp = fad_setpoint(self,p2x,p2y,NEW,ff);
			vv = fad_setvector(self,fp,lp,ff);
			vv->mode = ANIMATEMODE;
			break;
		case 'B':
			sscanf(s,"$B %d,%d %d,%d\n",&p1x,&p1y,&p2x,&p2y);
			fp = fad_setpoint(self,p1x,p1y,NEW,ff);
			lp = fad_setpoint(self,p2x,p2y,NEW,ff);
			vv = fad_setvector(self,fp,lp,ff);
			vv->mode = BOXMODE;
			break;
		case 'S':
			sscanf(s,"$S %d,%d\n",&p1x,&p1y);
			fgets(str,256,f);
			fp = fad_setpoint(self,p1x,p1y,NEW,ff);
			lp = fad_setpoint(self,LABELFLAG,LABELFLAG,NEW,ff);	
			vv = fad_setvector(self,fp,lp,ff);
			szz = strlen(str);
			str[szz - 1] = '\0';
			vv->label = malloc(szz);
			strcpy(vv->label,str);
			break;
		case '$':
			break;
			}
	}
	fad_NotifyObservers(self,fad_NEWFAD);
	return dataobject_NOREADERROR;
}
void fad__FinalizeObject(classID, self)
    struct classheader *classID;
struct fad *self;
{/* bug : label strings not currently freed */
	struct fad_frame *lf,*sf;
	for(lf = self->bf ;  lf != NULL ; lf = sf){
            sf = lf->f;
            fad_freeframe(self,lf);
        }
}

fad__flipicons(cp)
struct fad *cp;
{
	register struct fadpoint *iip;
	if(cp->iconpointend == cp->iconpoints) return(0);
	if(cp->iconpointnow == NULL
		 || ++(cp->iconpointnow) ==cp->iconpointend )
			cp->iconpointnow = cp->iconpoints;
	iip  = *(cp->iconpointnow);
	cp->currentfont = cp->fontpt[-(iip->x)];
	cp->currentfontindex = iip->x;
	cp->currenticon = iip->y;
#ifdef DEBUG 
	fprintf(stderr,"cf = %d, ci = %d at %d\n",
		iip->x,iip->y,(int)(cp->iconpointend - cp->iconpoints));
#endif /* DEBUG  */
	return(iip->y);
	}
int
fad__unflipicons(cp)
struct fad *cp;
{
	register struct fadpoint *iip;
	if(cp->iconpointend == cp->iconpoints) return(0);
	if(cp->iconpointnow == NULL)
			cp->iconpointnow = cp->iconpoints;
	else if ( cp->iconpointnow ==cp->iconpoints ){
		cp->iconpointnow = cp->iconpointend - 1;
		}
	else 	cp->iconpointnow--;
		
	iip  = *(cp->iconpointnow);
	cp->currentfont = cp->fontpt[-(iip->x)];
	cp->currentfontindex = iip->x;
	cp->currenticon = iip->y;
#ifdef DEBUG 
	fprintf(stderr,"cf = %d, ci = %d at %d\n",
		iip->x,iip->y,(int)(cp->iconpointend - cp->iconpoints));
#endif /* DEBUG  */
	return(iip->y);
	}
