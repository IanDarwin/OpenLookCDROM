/* Copyright 1992 by the Andrew Toolkit Consortium and Carnegie Mellon University All rights Reserved. */

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/prefed/RCS/pvalslv.c,v 1.4 1992/12/15 21:39:11 rr2b R6tape $";
#endif



 


#include <andrewos.h>
#include <math.h>
#include <class.h>

#include <scroll.ih>
#include <prefval.ih>
#include "pvalslv.eh"

#define zfree(x) do { if(x) { free(x); (x)=NULL;}} while (0)
#define DATA(self) ((struct prefval *)pvalslv_GetDataObject(self))
#define VALUE(self) (prefval_GetValue(DATA(self))?prefval_GetValue(DATA(self))->v.ival:0)

boolean pvalslv__InitializeObject(classID, self)
struct classheader *classID;
struct pvalslv *self;
{
    pvalslv_SetLocation(self, scroll_BOTTOM);
    return TRUE;
}

void pvalslv__FinalizeObject(classID, self)
struct classheader *classID;
struct pvalslv *self;
{
}

void pvalslv__GetInfo(self, total, seen, dot)
struct pvalslv *self;
struct range *total, *seen, *dot;
{

    total->beg=prefval_GetRangeLow(DATA(self));
    total->end=prefval_GetRangeHigh(DATA(self));

    switch(DATA(self)->type) {
	case prefval_Integer:
	    seen->beg=VALUE(self);
	    break;
	default: ;
    }
    seen->end=seen->beg;

    dot->beg = -1;
    dot->end = -1;
}

long pvalslv__WhatIsAt(self, numerator, denominator)
struct pvalslv *self;
long numerator, denominator;
{
    long coord;

    coord = numerator * (prefval_GetRangeHigh(DATA(self)) - prefval_GetRangeLow(DATA(self)));
    coord /= denominator;

    if(DATA(self)->type==prefval_Integer) {
	coord+=VALUE(self);
    }
    
    return coord;
}

void pvalslv__SetFrame(self, pos, num, denom)
struct pvalslv *self;
long pos, num, denom;
{
    struct prefval_value v;
    long coord;

    coord = num * (prefval_GetRangeHigh(DATA(self)) - prefval_GetRangeLow(DATA(self)));
    coord /= denom;

    prefval_InitValue(DATA(self), &v);
    if(DATA(self)->type==prefval_Integer) {
	v.v.ival=pos-coord;
	if(v.v.ival<prefval_GetRangeLow(DATA(self))) v.v.ival=prefval_GetRangeLow(DATA(self));
	else if(v.v.ival>prefval_GetRangeHigh(DATA(self))) v.v.ival=prefval_GetRangeHigh(DATA(self));
    }
    prefval_SetValue(DATA(self), &v);
    prefval_NotifyObservers(DATA(self), prefval_ValuesChanged);
    prefval_FreeValue(DATA(self), &v);

}

void pvalslv__Endzone(self, end, action)
struct pvalslv *self;
int end;
enum view_MouseAction action;
{
    long diff=0;
    struct prefval_value v;

    
    if(DATA(self)->type!=prefval_Integer || !prefval_GetValue(DATA(self))) return;

    if(action != view_LeftDown && action != view_RightDown) return;

    prefval_InitValue(DATA(self), &v);
    prefval_CopyValue(DATA(self), &v, prefval_GetValue(DATA(self)));
    
    if(end== scroll_MOTIFTOPENDZONE) end=scroll_TOPENDZONE;
    else if(end==scroll_MOTIFBOTTOMENDZONE) end=scroll_BOTTOMENDZONE;
    
    if(action == view_LeftDown) {
	if(end==scroll_BOTTOMENDZONE) diff=prefval_GetRangeHigh(DATA(self))-v.v.ival;
	else if(end==scroll_TOPENDZONE) diff=prefval_GetRangeLow(DATA(self))-v.v.ival;
    } else {
	if(end==scroll_BOTTOMENDZONE) diff=1;
	else if(end==scroll_TOPENDZONE) diff=(-1);
    }
    v.v.ival+=diff;
    if(v.v.ival<prefval_GetRangeLow(DATA(self))) v.v.ival=prefval_GetRangeLow(DATA(self));
    else if(v.v.ival>prefval_GetRangeHigh(DATA(self))) v.v.ival=prefval_GetRangeHigh(DATA(self));
 
    prefval_SetValue(DATA(self), &v);
    prefval_NotifyObservers(DATA(self), prefval_ValuesChanged);
    prefval_FreeValue(DATA(self), &v);
}

