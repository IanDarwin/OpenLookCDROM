/* figio.c - various format conversion packages */
/*
  Copyright Carnegie Mellon University 1992 - All rights reserved
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
char *figio_c_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/figure/RCS/figio.c,v 1.4 1994/02/05 03:22:26 rr2b Exp $";
#endif

#include <andrewos.h>
#include <math.h>

#include "figio.eh"

#include "dataobj.ih"

#include "figure.ih"
#include "figobj.ih"
#include "figattr.ih"
#include "figorect.ih"
#include "figoell.ih"
#include "figorrec.ih"
#include "figoplin.ih"
#include "figospli.ih"
#include "figoins.ih"
#include "figotext.ih"
#include "fontdesc.ih"

#include "point.h"

static struct figattr *dummyattr;

static char *buf = NULL;
static long bufsize = 0;
#define BUFSIZESTEP (40)
#define TWOPI (6.28318530718)
#define figio_ArrowHead (80)

static struct point *pts = NULL;
static long pts_size = 0;

static void EnsurePts();

#define FromZipX(val)  (long)(((double)(1300+(val)) * ratio))
#define FromZipY(val)  (long)(((double)(1300-(val)) * ratio))
#define FromZipW(val)  (long)(((double)(val) * ratio))
#define FromZipH(val)  (long)(((double)(-(val)) * ratio))

boolean figio__InitializeClass(ClassID)
struct classhdr *ClassID;
{
    bufsize = BUFSIZESTEP;
    buf = malloc(bufsize);

    dummyattr = figattr_New();
    EnsurePts(4);

    if (!buf || !dummyattr || !pts)
	return FALSE;

    return TRUE;
}

static void EnsurePts(num)
int num;
{
    if (num <= pts_size)
	return;

    if (!pts) {
	pts_size = num;
	pts = (struct point *)malloc(sizeof(struct point) * pts_size);
    }
    else {
	while (!(num <= pts_size)) 
	    pts_size *= 2;

	pts = (struct point *)realloc(pts, sizeof(struct point) * pts_size);
    }
}

/* yank data into buf until a newline, extending buf if necessary */
static char *GetString(fl)
{
    char *ctmp, *res;

    ctmp = fgets(buf, bufsize-1, fl);
    if (!ctmp) return NULL;

    while (1) {
	res = buf+(strlen(buf)-1);
	if (*res=='\n')
	    return buf;

	bufsize += BUFSIZESTEP;
	buf = realloc(buf, bufsize);
	res = buf+(strlen(buf));
	ctmp = fgets(res, BUFSIZESTEP-1, fl);
	if (!ctmp) return NULL;
    }
}

/* eats lines into buf until (first char IN list) == inlist 
  always reads at least one line
  returns NULL if EOF */
static char *EatLinesUntil(fl, list, inlist)
FILE *fl;
char *list;
boolean inlist;
{
    char *ctmp, *res;

    while (1) {
	ctmp = GetString(fl);
	if (!ctmp) return NULL;

	res = index(list, buf[0]);
	if ((!res && !inlist) || (res && inlist))
	    return buf;
    }
}

/* remove leading T and turn \n into newline */
static void SquishZipTextInput(pt)
char *pt;
{
    char *rept;

    for (rept = pt+1; *rept; rept++) {
	if (*rept == '\\' && *(rept+1) == 'n') {
	    *pt = '\n';
	    pt++;
	    rept++;
	}
	else {
	    *pt = *rept;
	    pt++;
	}
    }
    *pt = '\0';
}

/* file should be ready to read in first (*D...) line */
long figio__ReadZipFile(ClassID, fl, fig, parent, ratio)
struct classhdr *ClassID;
FILE *fl;
struct figure *fig;
long parent;
double ratio;
{
    char *ctmp, objtype;
    long starx, stary, altx, alty, greyval;
    int res;

    ratio = ratio * 1.5;
    buf[0] = '\0';

    ctmp = GetString(fl);
    if (!ctmp) {
	return dataobject_PREMATUREEOF;
    }
    while (ctmp && buf[0]=='%') {
	ctmp = GetString(fl);
    }

    while (ctmp) {
	if (buf[0] != '*') {
	    return dataobject_BADFORMAT;
	}
	ctmp = index(buf, ';');
	if (!ctmp) {
	    return dataobject_BADFORMAT;
	}
	res = sscanf(ctmp, ";%ld,%ld", &starx, &stary);
	if (res!=2) {
	    return dataobject_BADFORMAT;
	}
	objtype = buf[1];
	switch (objtype) {
	    case 'P': {
		ctmp = GetString(fl);
		if (!ctmp) return dataobject_PREMATUREEOF;

		ctmp = EatLinesUntil(fl, "*", TRUE);
		}
		break;

	    case 'G': {
		struct figorect *o;
		ctmp = EatLinesUntil(fl, "G>", TRUE);
		if (!ctmp) return dataobject_PREMATUREEOF;

		greyval = figattr_ShadeClear;
		if (buf[0]=='G') {
		    greyval = atol(buf+1);
		    greyval = greyval * figattr_ShadeDenom / 100;
		    ctmp = EatLinesUntil(fl, ">", TRUE);
		    if (!ctmp) return dataobject_PREMATUREEOF;
		}

		res = sscanf(buf, ">%ld,%ld", &altx, &alty);
		if (res!=2) {
		    return dataobject_BADFORMAT;
		}
		o = figorect_Create(FromZipX(starx), FromZipY(stary), FromZipW(altx-starx), FromZipH(alty-stary));
		if (!o) {
		    return dataobject_OBJECTCREATIONFAILED;
		}
		figure_InsertObject(fig, o, parent, -1);
		figattr_SetShade(dummyattr, greyval);
		figorect_UpdateVAttributes(o, dummyattr, (1<<figattr_Shade));
		ctmp = EatLinesUntil(fl, "*", TRUE);
		}
		break;

	    case 'I': {
		struct figoins *o;
		struct dataobject *dobj;
		char namebuf[128];
		long tid;

		ctmp = EatLinesUntil(fl, "SG", FALSE);
		if (!ctmp) return dataobject_PREMATUREEOF;

		res = sscanf(buf, ">%ld,%ld", &altx, &alty);
		if (res!=2) {
		    return dataobject_BADFORMAT;
		}
		
		ctmp = GetString(fl);
		if (!ctmp) return dataobject_PREMATUREEOF;
		res = sscanf(buf, "\\begindata{%[^,],%ld}", namebuf, &tid);
		if (res!=2) return dataobject_BADFORMAT;

		o = figoins_Create(FromZipX(starx), FromZipY(stary), FromZipW(altx-starx), FromZipH(alty-stary), namebuf);
		if (!o) {
		    return dataobject_OBJECTCREATIONFAILED;
		}
		dobj = figoins_GetDataObject(o);

		res = dataobject_Read(dobj, fl, tid);
		if (res != dataobject_NOREADERROR) {
		    return res;
		}

		figure_InsertObject(fig, o, parent, -1);
		ctmp = EatLinesUntil(fl, "*", TRUE);
		}
		break;

	    case 'J':
	    case 'L': {
		struct figoell *o;
		ctmp = EatLinesUntil(fl, "G>", TRUE);
		if (!ctmp) return dataobject_PREMATUREEOF;

		greyval = figattr_ShadeClear;
		if (buf[0]=='G') {
		    greyval = atol(buf+1);
		    greyval = greyval * figattr_ShadeDenom / 100;
		    ctmp = EatLinesUntil(fl, ">", TRUE);
		    if (!ctmp) return dataobject_PREMATUREEOF;
		}

		res = sscanf(buf, ">%ld,%ld", &altx, &alty);
		if (res!=2) {
		    return dataobject_BADFORMAT;
		}
		if (objtype=='J') {
		    if (altx < alty) altx = alty;
		    else alty = altx;
		}
		o = figoell_Create(FromZipX(starx-altx), FromZipY(stary-alty), FromZipW(altx*2), FromZipH(alty*2));
		if (!o) {
		    return dataobject_OBJECTCREATIONFAILED;
		}
		figure_InsertObject(fig, o, parent, -1);
		figattr_SetShade(dummyattr, greyval);
		figoell_UpdateVAttributes(o, dummyattr, (1<<figattr_Shade));
		ctmp = EatLinesUntil(fl, "*", TRUE);
		}
		break;

	    case 'M': {
		struct figoell *o;
		long t1, t2, t3, t4;
		ctmp = EatLinesUntil(fl, "G>", TRUE);
		if (!ctmp) return dataobject_PREMATUREEOF;

		greyval = figattr_ShadeClear;
		if (buf[0]=='G') {
		    greyval = atol(buf+1);
		    greyval = greyval * figattr_ShadeDenom / 100;
		    ctmp = EatLinesUntil(fl, ">", TRUE);
		    if (!ctmp) return dataobject_PREMATUREEOF;
		}

		res = sscanf(buf, ">%ld,%ld;%ld,%ld;%ld,%ld", &t1, &t2, &t3, &t4, &altx, &alty);
		if (res!=6) {
		    return dataobject_BADFORMAT;
		}
		o = figoell_Create(FromZipX(starx-altx), FromZipY(stary-alty), FromZipW(altx*2), FromZipH(alty*2));
		if (!o) {
		    return dataobject_OBJECTCREATIONFAILED;
		}
		figure_InsertObject(fig, o, parent, -1);
		figattr_SetShade(dummyattr, greyval);
		figoell_UpdateVAttributes(o, dummyattr, (1<<figattr_Shade));
		ctmp = EatLinesUntil(fl, "*", TRUE);
		}
		break;

	    case 'N': {
		struct figorrec *o;
		ctmp = EatLinesUntil(fl, "G>", TRUE);
		if (!ctmp) return dataobject_PREMATUREEOF;

		greyval = figattr_ShadeClear;
		if (buf[0]=='G') {
		    greyval = atol(buf+1);
		    greyval = greyval * figattr_ShadeDenom / 100;
		    ctmp = EatLinesUntil(fl, ">", TRUE);
		    if (!ctmp) return dataobject_PREMATUREEOF;
		}

		res = sscanf(buf, ">%ld,%ld", &altx, &alty);
		if (res!=2) {
		    return dataobject_BADFORMAT;
		}
		o = figorrec_Create(FromZipX(starx), FromZipY(stary), FromZipW(altx-starx), FromZipH(alty-stary));
		if (!o) {
		    return dataobject_OBJECTCREATIONFAILED;
		}
		figure_InsertObject(fig, o, parent, -1);
		figattr_SetShade(dummyattr, greyval);
		figorrec_UpdateVAttributes(o, dummyattr, (1<<figattr_Shade));
		ctmp = EatLinesUntil(fl, "*", TRUE);
		}
		break;

	    case 'C': {
		struct figoplin *o;
		ctmp = EatLinesUntil(fl, "G>", TRUE);
		if (!ctmp) return dataobject_PREMATUREEOF;

		greyval = figattr_ShadeClear;
		if (buf[0]=='G') {
		    greyval = atol(buf+1);
		    greyval = greyval * figattr_ShadeDenom / 100;
		    ctmp = EatLinesUntil(fl, ">", TRUE);
		    if (!ctmp) return dataobject_PREMATUREEOF;
		}

		res = sscanf(buf, ">%ld,%ld", &altx, &alty);
		if (res!=2) {
		    return dataobject_BADFORMAT;
		}
		pts[0].x = FromZipX(starx);
		pts[0].y = FromZipY(stary);
		pts[1].x = FromZipX(altx) - pts[0].x;
		pts[1].y = FromZipY(alty) - pts[0].y;
		o = figoplin_Create(pts, 2, FALSE);
		if (!o) {
		    return dataobject_OBJECTCREATIONFAILED;
		}
		figure_InsertObject(fig, o, parent, -1);
		ctmp = EatLinesUntil(fl, "*", TRUE);
		}
		break;

	    case 'O': {
		struct figoplin *o;
		double theta;
		long count;

		ctmp = EatLinesUntil(fl, ">", TRUE);
		if (!ctmp) return dataobject_PREMATUREEOF;

		pts[0].x = FromZipX(starx);
		pts[0].y = FromZipY(stary);

		if (buf[0]!='>')  {
		    return dataobject_BADFORMAT;
		}
		ctmp = buf;
		count = 1;
		do {
		    ctmp++;
		    res = sscanf(ctmp, "%ld,%ld", &altx, &alty);
		    if (res!=2)  {
			return dataobject_BADFORMAT;
		    }
		    EnsurePts(count+1);
		    pts[count].x = FromZipX(altx) - pts[0].x;
		    pts[count].y = FromZipY(alty) - pts[0].y;
		    count++;
		    ctmp = index(ctmp, ';');
		} while (ctmp);

		altx = pts[count-1].x + pts[0].x;
		alty = pts[count-1].y + pts[0].y;
		if (count==2) {
		    starx = pts[0].x;
		    stary = pts[0].y;
		}
		else {
		    starx = pts[count-2].x + pts[0].x;
		    stary = pts[count-2].y + pts[0].y;
		}

		o = figoplin_Create(pts, count, FALSE);
		if (!o) {
		    return dataobject_OBJECTCREATIONFAILED;
		}
		figure_InsertObject(fig, o, parent, -1);

		/* ideally, star is the tail and alt is the head of the final line segment. */
		theta = TWOPI / 2.0 + atan2((double)(alty-stary), (double)(altx-starx));
		pts[0].x = altx + (long)((double)(FromZipW(figio_ArrowHead)) * cos(theta+0.5));
		pts[0].y = alty + (long)((double)(FromZipW(figio_ArrowHead)) * sin(theta+0.5));
		pts[1].x = altx - pts[0].x;
		pts[1].y = alty - pts[0].y;
		pts[2].x = altx + (long)((double)(FromZipW(figio_ArrowHead)) * cos(theta-0.5)) - pts[0].x;
		pts[2].y = alty + (long)((double)(FromZipW(figio_ArrowHead)) * sin(theta-0.5)) - pts[0].y;
		o = figoplin_Create(pts, 3, FALSE);
		if (!o) {
		    return dataobject_OBJECTCREATIONFAILED;
		}
		figure_InsertObject(fig, o, parent, -1);

		ctmp = EatLinesUntil(fl, "*", TRUE);
		}
		break;

	    case 'D': 
	    case 'H': {
		struct figoplin *o;
		long count;

		ctmp = EatLinesUntil(fl, "G>", TRUE);
		if (!ctmp) return dataobject_PREMATUREEOF;

		greyval = figattr_ShadeClear;
		if (buf[0]=='G') {
		    greyval = atol(buf+1);
		    greyval = greyval * figattr_ShadeDenom / 100;
		    ctmp = EatLinesUntil(fl, ">", TRUE);
		    if (!ctmp) return dataobject_PREMATUREEOF;
		}

		if (buf[0]!='>')  {
		    return dataobject_BADFORMAT;
		}
		ctmp = buf;
		count = 0;
		EnsurePts(count+1);
		pts[0].x = FromZipX(starx);
		pts[0].y = FromZipY(stary);
		count++;

		do {
		    ctmp++;
		    res = sscanf(ctmp, "%ld,%ld", &altx, &alty);
		    if (res!=2)  {
			return dataobject_BADFORMAT;
		    }
		    EnsurePts(count+1);
		    /* count>0 */
		    pts[count].x = FromZipX(altx) - pts[0].x;
		    pts[count].y = FromZipY(alty) - pts[0].y;
		    count++;
		    ctmp = index(ctmp, ';');
		} while (ctmp);

		res = (starx==altx && stary==alty);
		if (res)
		    count--;

		if (count >= 2) {
		    o = figoplin_Create(pts, count, res);
		    if (!o) {
			return dataobject_OBJECTCREATIONFAILED;
		    }
		    figure_InsertObject(fig, o, parent, -1);
		    figattr_SetShade(dummyattr, greyval);
		    figoplin_UpdateVAttributes(o, dummyattr, (1<<figattr_Shade));
		}
		ctmp = EatLinesUntil(fl, "*", TRUE);
		}
		break;

	    case 'E': {
		struct figoplin *o;
		double thetadiv, thetaoff;
		long dx, dy, ix;

		ctmp = EatLinesUntil(fl, "G>", TRUE);
		if (!ctmp) return dataobject_PREMATUREEOF;

		greyval = figattr_ShadeClear;
		if (buf[0]=='G') {
		    greyval = atol(buf+1);
		    greyval = greyval * figattr_ShadeDenom / 100;
		    ctmp = EatLinesUntil(fl, ">", TRUE);
		    if (!ctmp) return dataobject_PREMATUREEOF;
		}

		res = sscanf(buf, ">%ld,%ld", &alty, &altx);
		if (res!=2) {
		    return dataobject_BADFORMAT;
		}
		EnsurePts(altx+1);
		thetadiv = TWOPI / (double)altx;
		if ((altx%4)==2)
		    thetaoff = (TWOPI / 4.0);
		else
		    thetaoff = 0.0;
		for (ix=0; ix<altx; ix++) {
		    dx = starx + (long)((double)alty * sin((double)ix * thetadiv + thetaoff));
		    dy = stary + (long)((double)alty * cos((double)ix * thetadiv + thetaoff));
		    if (ix==0) {
			pts[0].x = FromZipX(dx);
			pts[0].y = FromZipY(dy);
		    }
		    else {
			pts[ix].x = FromZipX(dx) - pts[0].x;
			pts[ix].y = FromZipY(dy) - pts[0].y;
		    }
		}
		o = figoplin_Create(pts, altx, TRUE);
		if (!o) {
		    return dataobject_OBJECTCREATIONFAILED;
		}
		figure_InsertObject(fig, o, parent, -1);
		figattr_SetShade(dummyattr, greyval);
		figoplin_UpdateVAttributes(o, dummyattr, (1<<figattr_Shade));
		ctmp = EatLinesUntil(fl, "*", TRUE);
		}
		break;

	    case 'A': {
		struct figotext *o;
		char fname[64], extra[16];
		long fsize, fstyle, tpos;

		ctmp = GetString(fl);
		if (!ctmp) return dataobject_PREMATUREEOF;

		strcpy(fname, "andy");
		fsize = 12;
		fstyle = fontdesc_Plain;
		if (buf[0]=='F') {
		    res = sscanf(buf, "F%[a-zA-Z]%ld%s", fname, &fsize, extra);
		    if (res==2) {
			res=3;
			strcpy(extra, "");
		    }
		    if (res!=3) {
			return dataobject_BADFORMAT;
		    }
		    if (index(extra, 'b'))
			fstyle |= fontdesc_Bold;
		    if (index(extra, 'i'))
			fstyle |= fontdesc_Italic;
		    ctmp = GetString(fl);
		    if (!ctmp) return dataobject_PREMATUREEOF;
		}
		if (buf[0]!='T')  {
		    return dataobject_BADFORMAT;
		}
		SquishZipTextInput(buf);
		o = figotext_Create(buf, FromZipX(starx), FromZipY(stary));
		if (!o) {
		    return dataobject_OBJECTCREATIONFAILED;
		}
		ctmp = EatLinesUntil(fl, "*M", TRUE);
		tpos = figattr_PosCenter;
		if (buf[0]=='M') {
		    switch (buf[1]) {
			case 'L':
			    tpos = figattr_PosLeft;
			    break;
			case 'R':
			    tpos = figattr_PosRight;
			    break;
			case 'C':
			default:
			    tpos = figattr_PosCenter;
			    break;
		    }
		    ctmp = EatLinesUntil(fl, "*", TRUE);
		}
		figure_InsertObject(fig, o, parent, -1);
		figattr_SetFontFamily(dummyattr, fname);
		figattr_SetFontSize(dummyattr, fsize);
		figattr_SetFontStyle(dummyattr, fstyle);
		figattr_SetTextPos(dummyattr, tpos);
		figotext_UpdateVAttributes(o, dummyattr, ((1<<figattr_FontFamily) | (1<<figattr_FontSize) | (1<<figattr_FontStyle) | (1<<figattr_TextPos)));

		}
		break;

	    default:
		return dataobject_BADFORMAT;
	}
    }

    return dataobject_NOREADERROR;
}
