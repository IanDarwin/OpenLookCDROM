/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>
#include <math.h>

#include "ice_defines.h"
#include "ice_externs.h"
#include "Intobj.h"
#include "Text.h"
#include "Vector.h"
#include "Curve.h"
#include "Marker.h"
#include "Rectangle.h"
#include "Polygon.h"
#include "Axis.h"

extern void		ice_err(char *, int);
extern void		pg_draw();
extern Composite *	sel_cmpobj(int, int);

static Composite *attr_cmp;

void
cmpattrsel_proc(Menu *m, Menu_item *mi)
{
	attr_cmp= (Composite *) NULL;
	ice_op= CMP_ATTR;
	return;
}

void
cmpattr_event(XEvent *event)
{
	XButtonEvent *bevt;
	Menu_item *mi;
	void attrcmp_proc(Menu *, Menu_item *);

	if (attr_cmp != (Composite *) NULL)
		return;

	if (event->type != ButtonPress)
		return;

	bevt= (XButtonEvent *) event;

	if ((attr_cmp= sel_cmpobj(bevt->x, bevt->y)) == (Composite *) NULL) {
		ice_err("Cannot locate selected composite object.", NONFATAL);
		ice_op= MAIN_MENU;
		return;
	}
	mi= menuitem_find(cmpattr_menu, LXMI_CLIENTDATA, (char *) attr_cmp);
	attrcmp_proc(cmpattr_menu, mi);
	return;
}

void
attrcmp_proc(Menu *m, Menu_item *mi)
{
	if ((attr_cmp= (Composite *) menuitem_get(mi, LXMI_CLIENTDATA)) == (Composite *) NULL) {
		ice_err("Cannot locate selected composite object.", NONFATAL);
		return;
	}

	panelitem_set(cmpattr_panel, cmpattr_name, LXPTEXT_VALUE, attr_cmp->getname(), LXPTEXT_RDONLY, TRUE, LXPI_NULL);

	panelitem_set(cmpattr_panel, cmpattr_scale, LXPTEXT_VALUE, "1", LXPI_NULL);
	panelitem_set(cmpattr_panel, cmpattr_scaleattr, LXPENUM_VALUE, CMP_NOSCALEATTR, LXPI_NULL);

	panelitem_set(cmpattr_panel, cmpattr_rot, LXPTEXT_VALUE, "0", LXPI_NULL);

	ice_op= CMP_ATTR;
	XMapRaised(dpy, cmpattr_frame);
	return;
}

void
cmpattrcont_proc(Panel *p, Panel_item *pi)
{
	char *buf, *cptr;
	int scattr, natoms, i, j;
	double scale, rot;
	Grobj **atoms, *g;
	float oxorig, oyorig, nxorig, nyorig, *xvert, *yvert;
	int ix, iy, nvert;
	float r, ox, oy, nx, ny;
	float w, h;
	void loc_transform(double, double, float, float, float *, float *);

	XUnmapWindow(dpy, cmpattr_frame);
	ice_op= MAIN_MENU;

	buf= (char *) panelitem_get(cmpattr_panel, cmpattr_scale, LXPTEXT_VALUE);
	scale= strtod(buf, &cptr);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid scale value.", NONFATAL);
		return;
	}
	if (scale <= 0.) {
		ice_err("Scale value must be greater than zero.", NONFATAL);
		return;
	}

	scattr= *((int *) panelitem_get(cmpattr_panel, cmpattr_scaleattr, LXPENUM_VALUE));

	buf= (char *) panelitem_get(cmpattr_panel, cmpattr_rot, LXPTEXT_VALUE);
	rot= strtod(buf, &cptr);
	if ((cptr == buf) || (*cptr != '\0')) {
		ice_err("Invalid rotation value.", NONFATAL);
		return;
	}
	while (rot < 0.)
		rot+= 360.;
	while (rot >= 360.)
		rot-= 360.;

	if ((scale == 1.) && (rot == 0.))
		return;

	XDefineCursor(dpy, pg_cwin, hg_cursor);
	XSync(dpy, False);

	attr_cmp->getatoms(&natoms, &atoms);
	for (i= 0; i < natoms; i++) {
		g= atoms[i];
		g->getloc(&oxorig, &oyorig, &ix, &iy);
		loc_transform(scale, rot, oxorig, oyorig, &nxorig, &nyorig);
		g->setfloc(nxorig, nyorig, (float) pg_dpi);
		switch (g->gettype()) {
		case GROBJ_PSDOC:
			r= g->getrotation();
			r+= rot;
			while (r >= 360.)
				r-= 360.;
			g->setrotation(r);
			if (scattr == CMP_SCALEATTR) {
				float hs, vs;

				g->getscale(&hs, &vs);
				hs*= scale;
				vs*= scale;
				g->setscale(hs, vs);
			}
			break;
		case GROBJ_RASTER:
			break;
		case GROBJ_INTOBJ:
			switch (((Intobj *) g)->getintobjtype()) {
			case INTOBJ_TEXT:
				r= g->getrotation();
				r+= rot;
				while (r >= 360.)
					r-= 360.;
				g->setrotation(r);
				if (scattr == CMP_SCALEATTR) {
					int fs;
					float s, l;

					((Text *) g)->getfontsize(&fs, &s, &l);
					if (fs != TEXT_GLOBALFONTSZ) {
						s*= scale;
						l*= scale;
						((Text *) g)->setfontsize(fs, s, l);
					}
				}
				break;
			case INTOBJ_VECTOR:
				((Vector *) g)->getend(&ox, &oy, &ix, &iy);
				loc_transform(scale, rot, ox, oy, &nx, &ny);
				((Vector *) g)->setfend(nx, ny, (float) pg_dpi);
				if (scattr == CMP_SCALEATTR) {
					int lw, ls, ds, dpl, p, ps;
					float w, *dp, dpo, *ndp, pw, pol, pil;

					((Vector *) g)->getwidth(&lw, &w);
					if (lw != VECTOR_GLOBALWIDTH) {
						w*= scale;
						((Vector *) g)->setwidth(lw, w);
					}
					ls= ((Vector *) g)->getlinestyle();
					if (ls == VECTOR_DASHED) {
						((Vector *) g)->getdashstyle(&ds, &dp, &dpl, &dpo);
						if ((dpl <= 0) ||
						    ((ndp= new float[dpl]) == (float *) NULL))
							ice_err("Cannot adjust dash pattern.", NONFATAL);
						else {
							for (j= 0; j < dpl; j++)
								ndp[j]= dp[j]*scale;
							dpo*= scale;
							((Vector *) g)->setdashstyle(ds, ndp, dpl, dpo);
						}
					}
					((Vector *) g)->getptrstyle(&p, &ps, &pw, &pol, &pil);
					if (p != VECTOR_NOPTR) {
						pw*= scale;
						pol*= scale;
						pil*= scale;
						((Vector *) g)->setptrstyle(p, ps, pw, pol, pil);
					}
				}
				break;
			case INTOBJ_CURVE:
				((Curve *) g)->getcontrol1(&ox, &oy, &ix, &iy);
				loc_transform(scale, rot, ox, oy, &nx, &ny);
				((Curve *) g)->setfcontrol1(nx, ny, (float) pg_dpi);
				((Curve *) g)->getcontrol2(&ox, &oy, &ix, &iy);
				loc_transform(scale, rot, ox, oy, &nx, &ny);
				((Curve *) g)->setfcontrol2(nx, ny, (float) pg_dpi);
				((Curve *) g)->getend(&ox, &oy, &ix, &iy);
				loc_transform(scale, rot, ox, oy, &nx, &ny);
				((Curve *) g)->setfend(nx, ny, (float) pg_dpi);
				if (scattr == CMP_SCALEATTR) {
					int lw, ls, ds, dpl;
					float w, *dp, dpo, *ndp;

					((Curve *) g)->getwidth(&lw, &w);
					if (lw != CURVE_GLOBALWIDTH) {
						w*= scale;
						((Curve *) g)->setwidth(lw, w);
					}
					ls= ((Curve *) g)->getlinestyle();
					if (ls == CURVE_DASHED) {
						((Curve *) g)->getdashstyle(&ds, &dp, &dpl, &dpo);
						if ((dpl <= 0) ||
						    ((ndp= new float[dpl]) == (float *) NULL))
							ice_err("Cannot adjust dash pattern.", NONFATAL);
						else {
							for (j= 0; j < dpl; j++)
								ndp[j]= dp[j]*scale;
							dpo*= scale;
							((Curve *) g)->setdashstyle(ds, ndp, dpl, dpo);
						}
					}
				}
				break;
			case INTOBJ_MARKER:
				r= g->getrotation();
				r+= rot;
				while (r >= 360.)
					r-= 360.;
				g->setrotation(r);
				if (scattr == CMP_SCALEATTR) {
					int s, bm, bw, bc;
					float rd, w;
					unsigned char r, gr, b;

					((Marker *) g)->getsize(&s, &rd);
					if (s != MARKER_GLOBALSIZE) {
						rd*= scale;
						((Marker *) g)->setsize(s, rd);
					}
					((Marker *) g)->getboundary(&bm, &bw, &w, &bc, &r, &gr, &b);
					if (bw != MARKER_GLOBALBNDWIDTH) {
						w*= scale;
						((Marker *) g)->setboundary(bm, bw, w, bc, r, gr, b);
					}
				}
				break;
			case INTOBJ_RECTANGLE:
				r= g->getrotation();
				r+= rot;
				while (r >= 360.)
					r-= 360.;
				g->setrotation(r);
				((Rectangle *) g)->getsize(&w, &h);
				w*= scale;
				h*= scale;
				((Rectangle *) g)->setsize(w, h);
				if (scattr == CMP_SCALEATTR) {
					int bw;

					((Rectangle *) g)->getwidth(&bw, &w);
					if (bw != RECTANGLE_GLOBALBNDWIDTH) {
						w*= scale;
						((Rectangle *) g)->setwidth(bw, w);
					}
				}
				break;
			case INTOBJ_POLYGON:
				((Polygon *) g)->getvertices(&nvert, &xvert, &yvert);
				xvert[0]= yvert[0]= 0.;
				for (j= 1; j < nvert; j++) {
					ox= xvert[j]+oxorig;
					oy= yvert[j]+oyorig;
					loc_transform(scale, rot, ox, oy, &nx, &ny);
					xvert[j]= nx-nxorig;
					yvert[j]= ny-nyorig;
				}
				if (scattr == CMP_SCALEATTR) {
					int bm, bw, bc;
					float w;
					unsigned char r, gr, b;

					((Polygon *) g)->getboundary(&bm, &bw, &w, &bc, &r, &gr, &b);
					if (bw != POLYGON_GLOBALBNDWIDTH) {
						w*= scale;
						((Polygon *) g)->setboundary(bm, bw, w, bc, r, gr, b);
					}
				}
				break;
			case INTOBJ_AXIS:
				((Axis *) g)->getend(&ox, &oy, &ix, &iy);
				loc_transform(scale, rot, ox, oy, &nx, &ny);
				((Axis *) g)->setfend(nx, ny, (float) pg_dpi);
				if (scattr == CMP_SCALEATTR) {
					int aw, tl, tw, fs, fl, fo;
					float w, pt, st, tt, s, foff;

					((Axis *) g)->getaxiswidth(&aw, &w);
					if (aw != AXIS_GLOBALWIDTH) {
						w*= scale;
						((Axis *) g)->setaxiswidth(aw, w);
					}
					((Axis *) g)->gettick(&tl, &pt, &st, &tt, &tw, &w);
					if (tl != AXIS_NOLOC) {
						pt*= scale;
						st*= scale;
						tt*= scale;
						if (tw != AXIS_GLOBALWIDTH)
							w*= scale;
						((Axis *) g)->settick(tl, pt, st, tt, tw, w);
					}
					((Axis *) g)->getfontsize(&fs, &s);
					if (fs != AXIS_GLOBALFONTSZ) {
						s*= scale;
						((Axis *) g)->setfontsize(fs, s);
					}
					((Axis *) g)->getlabelattr(&fl, &fo, &foff);
					if (fl != AXIS_NOLOC) {
						foff*= scale;
						((Axis *) g)->setlabelattr(fl, fo, foff);
					}
				}
				break;
			}
			break;
		}
	}

	pg_draw();
	XDefineCursor(dpy, pg_cwin, std_cursor);
	XSync(dpy, False);

	return;
}

void
cmpattrabort_proc(Panel *p, Panel_item *pi)
{
	XUnmapWindow(dpy, cmpattr_frame);
	ice_op= MAIN_MENU;
	return;
}

void
loc_transform(double scale, double rot, float ox, float oy, float *nx, float *ny)
{
	double odx, ody, ndx, ndy;
	double rad, cs, sn;

	if (rot == 0.) {
		*nx= (float) (ox*scale);
		*ny= (float) (oy*scale);
		return;
	}

	if ((ox == 0.) && (oy == 0.)) {
		*nx= ox;
		*ny= oy;
		return;
	}

	rad= (rot*PI)/180.;
	odx= (double) ox;
	ody= (double) oy;
	cs= cos(rad);
	sn= sin(rad);
	ndx= ((cs*odx)-(sn*ody))*scale;
	ndy= ((sn*odx)+(cs*ody))*scale;
	*nx= (float) ndx;
	*ny= (float) ndy;

	return;
}
