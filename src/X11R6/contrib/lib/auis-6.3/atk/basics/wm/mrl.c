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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/wm/RCS/mrl.c,v 2.9 1992/12/15 21:26:22 rr2b R6tape $";
#endif


 

#include <class.h>
#include <andyenv.h>
#include <graphic.ih>
#include <mrl.eh>
#ifdef WM_ENV  /* avoid makedepend "errors" */
#include <wmclient.h>
#endif /* WM_ENV   */
#include <rect.h>

void mrl__FinalizeObject(classID,self)
struct classheader *classID;
struct mrl *self;
{
	if(self->wmregion > 0) wm_DestroyRegion(self->wmregion);
}

boolean mrl__InitializeObject(classID,self)
struct classheader *classID;
struct mrl *self;
{
	self->wmregion = -1;
	self->next = NULL;
	self->left  = 0;
	self->right = 0;
	self->top = 0;
	self->bottom = 0;
	return TRUE;
}

struct mrl *mrl__Create(classID,region,next,r)
struct classheader *classID;
long region;
struct mrl *next;
struct rectangle *r;
{
    struct mrl *self;
    self = mrl_New();
    self->wmregion = region;
    if(r) {
	self->bottom = r->top + r->height;
        self->top = r->top;
        self->left = r->left;
        self->right = r->left + r->width;
    }
    self->next = next;
    return(self);
}

static struct mrl *mrC(region,next,left,top,right,bottom)
int region;
struct mrl *next;
long left,top,right, bottom;
{
	struct mrl *self;
	self = mrl_New();
	self->wmregion = region;
	self->left  = left;
	self->right = right;
	self->top = top;
	self->bottom = bottom;
	self->next = next;
	return(self);
}
struct mrl *mrl__Reset(self,r)
struct mrl *self;
struct rectangle *r;
{
    struct mrl *mp,*tmp;
    if(self == NULL) return(NULL);
    for(mp = self->next; mp != NULL;mp = tmp){
	tmp = mp->next;
	mrl_Destroy(mp);
	}
    if(r == NULL) {
	mrl_Destroy(self);
	return(NULL);
	}
    self->bottom = r->top + r->height;
    self->top = r->top;
    self->left = r->left;
    self->right = r->left + r->width;
    self->next = NULL;
    return(self);
}

#define FINISH   	if(next) self->next = mrl_Disect(self->next,r); return(self);
#define MAKENEW(A,B,C,D) self->next = mrC( -1,self->next,A,B,C,D)
struct mrl *mrl__Disect(self,r)
struct mrl *self;
struct rectangle *r;
  {
    int ob, ot, ol, or, nb, nt, nl, nr;
    struct mrl *next = self->next;
    ob = self->bottom ;
    ot = self->top;
    ol = self->left;
    or = self->right;
    nb = r->top + r->height;
    nt = r->top;
    nl = r->left;
    nr = r->left + r->width;
    
    if (nb <= ot || nt >= ob || nr <= ol || nl >= or){
	/* rectangles not related */
	FINISH
	}
    if(ot <= nt && ol <= nl && or >=nr && ob >= nb){
	/* Assume WM will do the right thing with completely enclosed rectangles */
	FINISH
    }
    if(nt <= ot && nl <= ol && nr >=or && nb >= ob){
/* 	delete old rectangle enclosed by new one 
 */ 	mrl_Destroy(self);
	if (next) return( mrl_Disect(next,r));
	else return(NULL);
	}
    if(self->wmregion > 0) {
	wm_DestroyRegion(self->wmregion);
	self->wmregion = -1;
	}
    if(nl >= ol){
	self->right = nl;
	if(nt <= ot){
	    if(nb >= ob) { 
		if(nr >= or){
		    FINISH  /* fig 1 */
		}
		/* fig 5 */
		MAKENEW(nr,ot,or,ob);
		FINISH
	    }
	    if(nr >= or) { /* fig 13 */
	        MAKENEW(nl,nb,or,ob);
		FINISH
	    }
	    /*  fig 9 */
	    MAKENEW(nr,ot,or,ob);
	    MAKENEW(nl,nb,nr,ob);
	    FINISH
	}
	else {
	    if(nb >= ob) {
		if(nr >= or){ /* fig 14 */
			MAKENEW(nl,ot,or,nt);
			 FINISH
		}
		/* fig 7 */
		MAKENEW(nl,ot,nr,nt);
		MAKENEW(nr,ot,or,ob);
		FINISH
	    }
	    if(nr >= or) {
		/* fig 10 */
	        MAKENEW(nl,ot,or,nt);
		MAKENEW(nl,nb,or,ob);
		FINISH
	    }
	   /*  fig 15 */
	    MAKENEW(nl,ot,or,nt);
	    MAKENEW(nr,nt,or,ob);
	    MAKENEW(nl,nb,nr,ob);
	    FINISH
	}
    }
    else {
	if(nt <= ot){
	    if(nb >= ob) { 
		/* fig 2 */
		self->left = nr;
		FINISH
		}
	    if(nr >= or) {/*  fig 4 */
		self->top = nb;
		FINISH
		}
	/*      fig 12 */
	    self->top = nb;
	    MAKENEW(nr,ot,or,nb);
	    FINISH
	}
	else {
	    self->bottom = nt;
	    if(nb >= ob) {
		if(nr >= or){ /* fig 3 */
			FINISH
			}
		/* fig 11 */
		MAKENEW(nr,nt,or,ob);
		FINISH
		}
	    if(nr >= or) {
		/* fig 6 */
	        MAKENEW(ol,nb,or,ob);
		FINISH
		}
	    /* fig 8 */
	    MAKENEW(nr,nt,or,ob);
	    MAKENEW(ol,nb,nr,ob);
	    FINISH
	}
    }
	    
}
