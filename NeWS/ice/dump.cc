/*
 *	Copyright (c) 1990 by Columbia University.
 */

#include <stdio.h>
#include <ctype.h>
#include <pwd.h>
#include <time.h>
#include <sys/param.h>
#include <sys/time.h>
#include <pixrect/pixrect_hs.h>
#include <NeWS/psio.h>

#if defined(sun3)
#include "icecps-sun3.h"
#elif defined(sun4)
#include "icecps-sun4.h"
#endif

#include "ice_defines.h"
#include "ice_externs.h"
#include "Psdoc.h"
#include "Raster.h"
#include "Intobj.h"
#include "Text.h"
#include "Axis.h"
#include "Fontobj.h"

extern "C" {
void		bcopy(char *, char *, int);
int		getuid();
int		gethostname(char *, int);
int		pprintf( ... );
int		ps_checkfor(PSFILE *, int, int);
int		strcmp(char *, char *);
int		strlen(char *);
int		strncmp(char *, char *, int);
}

extern FILE *	doc_open(char *, char *);
extern void	ice_err(char *, int);
extern void	pg_draw();
extern void	pg_draworiginhlt(boolean);

extern char psdbuf[];

Grobj *dmp_obj;

void
dmpallips_proc(Menu *m, Menu_item *mi)
{
	panelitem_set(dmpips_panel, dmpips_filenm, LXPTEXT_VALUE, "", LXPI_NULL);
	panelitem_set(dmpips_panel, dmpips_mode, LXPENUM_VALUE, DMPIPS_ICE | DMPIPS_PS, LXPI_NULL);
#if 0
	panelitem_set(dmpips_panel, dmpips_ras, LXPI_STATE, LXPI_ACTIVE, LXPENUM_VALUE, DMPIPS_RASEXCLUDE, LXPI_NULL);
#endif

	ice_op= DUMP_ALL;
	XMapRaised(dpy, dmpips_frame);
	return;
}

void
dmpobj_proc(Menu *m, Menu_item *mi)
{
	if ((dmp_obj= (Grobj *) menuitem_get(mi, LXMI_CLIENTDATA)) == (Grobj *) NULL) {
		ice_err("Cannot locate selected object.", NONFATAL);
		ice_op= MAIN_MENU;
		return;
	}

	panelitem_set(dmpips_panel, dmpips_filenm, LXPTEXT_VALUE, "", LXPI_NULL);
	panelitem_set(dmpips_panel, dmpips_mode, LXPENUM_VALUE, DMPIPS_ICE | DMPIPS_PS, LXPI_NULL);
#if 0
	panelitem_set(dmpips_panel, dmpips_ras, LXPI_STATE, LXPI_ACTIVE, LXPENUM_VALUE, DMPIPS_RASEXCLUDE, LXPI_NULL);
#endif

	ice_op= DUMP_OBJECT;
	XMapRaised(dpy, dmpips_frame);
	return;
}

void
dmpipsmode_proc(Panel *p, Panel_item *pi)
{
#if 0
	int mode;

	mode= *((int *) panelitem_get(p, pi, LXPENUM_VALUE));
	if (mode & DMPIPS_PS)
		panelitem_set(p, dmpips_ras, LXPI_STATE, LXPI_ACTIVE, LXPI_NULL);
	else
		panelitem_set(p, dmpips_ras, LXPI_STATE, LXPI_INACTIVE, LXPI_NULL);
	return;
#endif
}

void
dmpipscont_proc(Panel *p, Panel_item *pi)
{
	char *fn;
	int mode, ras;
	FILE *fp;
	struct passwd *pw;
	char hostname[MAXHOSTNAMELEN+1];
	char errmsg[MAX_ERRMSGLEN+1];
	struct timeval tvs;
	Grobj *gr, **atoms;
	int natoms, atom;
	void dmpips_declfonts(FILE *);
	boolean dmp_psdobjs(), dmp_pathtextobjs(), dmp_vecobjs();
	void dmpips_psddef(FILE *);
	void dmpips_pathtextdef(FILE *);
	void dmpips_vectordef(FILE *);
	void dmpips_pthobjs(FILE *);
	void dmpips_pg(FILE *);
	void dmpips_gdf(FILE *);
	void dmpips_cmpobjs(FILE *);

	XUnmapWindow(dpy, dmpips_frame);

	fn= (char *) panelitem_get(dmpips_panel, dmpips_filenm, LXPTEXT_VALUE);
	if (fn == (char *) NULL) {
		ice_err("Null filename.", NONFATAL);
		ice_op= MAIN_MENU;
		return;
	}
	if (strlen(fn) == 0) {
		ice_err("Empty filename.", NONFATAL);
		ice_op= MAIN_MENU;
		return;
	}
	if ((fp= fopen(fn, "w")) == (FILE *) NULL) {
		(void) sprintf(errmsg, "Cannot open file '%s'.", fn);
		ice_err(errmsg, NONFATAL);
		ice_op= MAIN_MENU;
		return;
	}

	mode= *((int *) panelitem_get(dmpips_panel, dmpips_mode, LXPENUM_VALUE));
	if (mode == 0) {
		ice_err("Dump file must contain either ICE commands or PostScript.", NONFATAL);
		ice_op= MAIN_MENU;
		return;
	}

#if 0
	ras= *((int *) panelitem_get(dmpips_panel, dmpips_ras, LXPENUM_VALUE));
#endif
	ras= DMPIPS_RASEXCLUDE;


	/* make sure that object list is properly sorted by
	   sequence, object type and dump transparency key */
	if (grobjs != (Grobj *) NULL)
		grobjs->sortsequence(&grobjs);

	if (mode & DMPIPS_PS) {
		fprintf(fp, "%%!PS-Adobe-2.0 EPSF-2.0\n");
		if (((pw= getpwuid(getuid())) != (struct passwd *) NULL) &&
		    (gethostname(hostname, MAXHOSTNAMELEN) == 0))
			fprintf(fp, "%%%%Creator: %s:%s (%s)\n", hostname, pw->pw_name, pw->pw_gecos);
		if (gettimeofday(&tvs, (struct timezone *) NULL) == 0)
			fprintf(fp, "%%%%CreationDate: %s", asctime(localtime(&(tvs.tv_sec))));
		fprintf(fp, "%%%%BoundingBox: 0 0 %1d %1d\n", (int) ((pg_width*72.)+1), (int) ((pg_height*72.)+1));
		dmpips_declfonts(fp);
		fprintf(fp, "%%%%EndComments\n");
		if (dmp_psdobjs())
			dmpips_psddef(fp);
		if (dmp_pathtextobjs())
			dmpips_pathtextdef(fp);
		if (dmp_vecobjs())
			dmpips_vectordef(fp);
	}

	if (mode & DMPIPS_ICE) {
		dmpips_pthobjs(fp);
		dmpips_pg(fp);
		dmpips_gdf(fp);
	}

	if (mode & DMPIPS_PS) {
		if (pg_clip != (Path *) NULL) {
			int flags;

			flags= PATH_DRAWCLOSE | PATH_DRAWCLIP;
			pg_clip->draw(flags, fp);
		}

		fprintf(fp, "%%%%EndProlog\n");
	}

	if (mode & DMPIPS_ICE) {
		int tag;

		tag= GROBJ_NULLIOTAG+1;
		for (gr= grobjs; gr != (Grobj *) NULL; gr= (Grobj *) gr->succ())
			gr->setiotag(tag++);

		tag= GROBJ_NULLIOTAG+1;
		for (gr= (Grobj *) cmpobjs; gr != (Grobj *) NULL; gr= (Grobj *) gr->succ())
			gr->setiotag(tag++);
	}

	if (ice_op == DUMP_ALL)
		gr= grobjs;
	else {
		atom= 0;
		if (dmp_obj->gettype() == GROBJ_COMPOSITE) {
			((Composite *) dmp_obj)->getatoms(&natoms, &atoms);
			gr= atoms[atom];
		}
		else {
			natoms= 1;
			gr= dmp_obj;
		}
	}

	while (gr != (Grobj *) NULL) {
		switch (gr->gettype()) {
		case GROBJ_PSDOC:
			if (mode & DMPIPS_ICE) {
				if (gr->icedump(fp) != GROBJ_SUCCESS) {
					(void) sprintf(errmsg, "Error dumping ICE directives for '%s'.", ((Psdoc *) gr)->getname());
					ice_err(errmsg, NONFATAL);
				}
			}
			if (mode & DMPIPS_PS) {
				if (((Psdoc *) gr)->draw(fp) != GROBJ_SUCCESS) {
					(void) sprintf(errmsg, "Error dumping '%s'.", ((Psdoc *) gr)->getname());
					ice_err(errmsg, NONFATAL);
				}
			}
			break;
		case GROBJ_RASTER:
			if (mode & DMPIPS_ICE) {
				if (gr->icedump(fp) != GROBJ_SUCCESS) {
					(void) sprintf(errmsg, "Error dumping ICE directives for '%s'.", ((Raster *) gr)->getname());
					ice_err(errmsg, NONFATAL);
				}
			}
			if ((mode & DMPIPS_PS) && (ras == DMPIPS_RASINCLUDE)) {
			}
			break;
		case GROBJ_INTOBJ:
			if (mode & DMPIPS_ICE) {
				if (gr->icedump(fp) != GROBJ_SUCCESS) {
					(void) sprintf(errmsg, "Error dumping ICE directives for '%s'.", ((Intobj *) gr)->getname());
					ice_err(errmsg, NONFATAL);
				}
			}
			if (mode & DMPIPS_PS) {
				if (((Intobj *) gr)->draw(fp) != GROBJ_SUCCESS) {
					(void) sprintf(errmsg, "Error dumping '%s'.", ((Intobj *) gr)->getname());
					ice_err(errmsg, NONFATAL);
				}
			}
			break;
		}

		if (ice_op == DUMP_ALL)
			gr= (Grobj *) gr->succ();
		else {
			atom+= 1;
			if (atom == natoms)
				gr= (Grobj *) NULL;
			else
				gr= atoms[atom];
		}
	}

	if (mode & DMPIPS_PS)
		fprintf(fp, "showpage\n");

	if (mode & DMPIPS_ICE)
		dmpips_cmpobjs(fp);

	(void) fclose(fp);
	ice_op= MAIN_MENU;
	return;
}

void
dmpips_declfonts(FILE *fp)
{
	Fontobj *fontobjs, *fo;
	Grobj *g, **atoms;
	FILE *psf;
	boolean found;
	int f, natoms, atom;
	char *fname, *c, *d;
	void dmpips_delfonts(Fontobj *);

	if (grobjs == (Grobj *) NULL)
		return;

	fontobjs= (Fontobj *) NULL;

	if (ice_op == DUMP_ALL)
		g= grobjs;
	else {
		atom= 0;
		if (dmp_obj->gettype() == GROBJ_COMPOSITE) {
			((Composite *) dmp_obj)->getatoms(&natoms, &atoms);
			g= atoms[atom];
		}
		else {
			natoms= 1;
			g= dmp_obj;
		}
	}

	while (g != (Grobj *) NULL) {
		switch (g->gettype()) {
		case GROBJ_PSDOC:
			if ((psf= doc_open(((Psdoc *) g)->getname(), "r")) == (FILE *) NULL) {
				ice_err("Cannot open external PS document.", NONFATAL);
				break;
			}
			found= FALSE;
			while (fgets(psdbuf, PSDOC_MAXLINELEN+1, psf) != (char *) NULL) {
				if (!strncmp(psdbuf, "%%DocumentFonts:", strlen("%%DocumentFonts:"))) {
					found= TRUE;
					break;
				}
			}
			if (found == FALSE) {
				(void) fclose(psf);
				break;
			}
			c= psdbuf+strlen("%%DocumentFonts:");
			found= FALSE;
			while (*c != '\0') {
				for (fname= c; isspace(*fname); fname++);
				if (*fname == '\0')
					break;
				for (d= fname; isgraph(*d); d++);
				if (*d == '\0')
					c= d;
				else {
					*d= '\0';
					c= d+1;
				}
				for (fo= fontobjs; fo != (Fontobj *) NULL; fo= (Fontobj *) fo->succ()) {
					if (!strcmp(fname, fo->getfont()))
						break;
				}
				if (fo == (Fontobj *) NULL) {
					if ((fo= new Fontobj((Dlnk **) &fontobjs, fname)) == (Fontobj *) NULL) {
						ice_err("Cannot create font object.", NONFATAL);
						dmpips_delfonts(fontobjs);
						return;
					}
				}
			}
			while (fgets(psdbuf, PSDOC_MAXLINELEN+1, psf) != (char *) NULL) {
				if (strncmp(psdbuf, "%%+", strlen("%%+")))
					break;

				c= psdbuf+strlen("%%+");
				found= FALSE;
				while (*c != '\0') {
					for (fname= c; isspace(*fname); fname++);
					if (*fname == '\0')
						break;
					for (d= fname; isgraph(*d); d++);
					if (*d == '\0')
						c= d;
					else {
						*d= '\0';
						c= d+1;
					}
					for (fo= fontobjs; fo != (Fontobj *) NULL; fo= (Fontobj *) fo->succ()) {
						if (!strcmp(fname, fo->getfont()))
							break;
					}
					if (fo == (Fontobj *) NULL) {
						if ((fo= new Fontobj((Dlnk **) &fontobjs, fname)) == (Fontobj *) NULL) {
							ice_err("Cannot create font object.", NONFATAL);
							dmpips_delfonts(fontobjs);
							return;
						}
					}
				}
			}
			(void) fclose(psf);
			break;
		case GROBJ_INTOBJ:
			switch (((Intobj *) g)->getintobjtype()) {
			case INTOBJ_TEXT:
				((Text *) g)->getfont(&f, &fname);
				for (fo= fontobjs; fo != (Fontobj *) NULL; fo= (Fontobj *) fo->succ()) {
					if (!strcmp(fname, fo->getfont()))
						break;
				}
				if (fo == (Fontobj *) NULL) {
					if ((fo= new Fontobj((Dlnk **) &fontobjs, fname)) == (Fontobj *) NULL) {
						ice_err("Cannot create font object.", NONFATAL);
						dmpips_delfonts(fontobjs);
						return;
					}
				}
				break;
			case INTOBJ_AXIS:
				((Axis *) g)->getfont(&f, &fname);
				for (fo= fontobjs; fo != (Fontobj *) NULL; fo= (Fontobj *) fo->succ()) {
					if (!strcmp(fname, fo->getfont()))
						break;
				}
				if (fo == (Fontobj *) NULL) {
					if ((fo= new Fontobj((Dlnk **) &fontobjs, fname)) == (Fontobj *) NULL) {
						ice_err("Cannot create font object.", NONFATAL);
						dmpips_delfonts(fontobjs);
						return;
					}
				}
				break;
			}
		}

		if (ice_op == DUMP_ALL)
			g= (Grobj *) g->succ();
		else {
			atom+= 1;
			if (atom == natoms)
				g= (Grobj *) NULL;
			else
				g= atoms[atom];
		}
	}

	if (fontobjs == (Fontobj *) NULL)
		return;

	for (fo= fontobjs; fo != (Fontobj *) NULL; fo= (Fontobj *) fo->succ()) {
		if (fo == fontobjs)
			fprintf(fp, "%%%%DocumentFonts: %s\n", fo->getfont());
		else
			fprintf(fp, "%%%%+ %s\n", fo->getfont());
	}

	dmpips_delfonts(fontobjs);
	return;
}

void
dmpips_delfonts(Fontobj *fl)
{
	Fontobj *fo;

	while (fl != (Fontobj *) NULL) {
		fo= (Fontobj *) fl->succ();
		fl->unlink();
		delete fl;
		fl= fo;
	}
	return;
}

boolean
dmp_psdobjs()
{
	int i, natoms;
	Grobj **atoms;

	if (ice_op == DUMP_ALL) {
		if (npsdocs > 0)
			return TRUE;
		else
			return FALSE;
	}

	switch (dmp_obj->gettype()) {
	case GROBJ_PSDOC:
		return TRUE;
	case GROBJ_RASTER:
	case GROBJ_INTOBJ:
		return FALSE;
	case GROBJ_COMPOSITE:
		((Composite *) dmp_obj)->getatoms(&natoms, &atoms);
		for (i= 0; i < natoms; i++) {
			if (atoms[i]->gettype() == GROBJ_PSDOC)
				return TRUE;
		}
		return FALSE;
	}
}

boolean
dmp_pathtextobjs()
{
	int i, natoms;
	Grobj *gr, **atoms;

	if (ice_op == DUMP_ALL) {
		if (ntexts == 0)
			return FALSE;

		for (gr= grobjs; gr != (Grobj *) NULL; gr= (Grobj *) gr->succ()) {
			if (gr->gettype() == GROBJ_INTOBJ) {
				if (((Intobj *) gr)->getintobjtype() == INTOBJ_TEXT) {
					if (((Text *) gr)->getjustify() == TEXT_PATH)
						return TRUE;
				}
			}
		}
		return FALSE;
	}

	switch (dmp_obj->gettype()) {
	case GROBJ_PSDOC:
	case GROBJ_RASTER:
		return FALSE;
	case GROBJ_INTOBJ:
		if (((Intobj *) dmp_obj)->getintobjtype() == INTOBJ_TEXT) {
			if (((Text *) dmp_obj)->getjustify() == TEXT_PATH)
				return TRUE;
		}
		return FALSE;
	case GROBJ_COMPOSITE:
		((Composite *) dmp_obj)->getatoms(&natoms, &atoms);
		for (i= 0; i < natoms; i++) {
			if (atoms[i]->gettype() == GROBJ_INTOBJ) {
				if (((Intobj *) atoms[i])->getintobjtype() == INTOBJ_TEXT) {
					if (((Text *) atoms[i])->getjustify() == TEXT_PATH)
						return TRUE;
				}
			}
		}
		return FALSE;
	}
}

boolean
dmp_vecobjs()
{
	int i, natoms;
	Grobj **atoms;

	if (ice_op == DUMP_ALL) {
		if (nvectors > 0)
			return TRUE;
		else
			return FALSE;
	}

	switch (dmp_obj->gettype()) {
	case GROBJ_PSDOC:
	case GROBJ_RASTER:
		return FALSE;
	case GROBJ_INTOBJ:
		if (((Intobj *) dmp_obj)->getintobjtype() == INTOBJ_VECTOR)
			return TRUE;
		else
			return FALSE;
	case GROBJ_COMPOSITE:
		((Composite *) dmp_obj)->getatoms(&natoms, &atoms);
		for (i= 0; i < natoms; i++) {
			if (atoms[i]->gettype() == GROBJ_INTOBJ) {
				if (((Intobj *) atoms[i])->getintobjtype() == INTOBJ_VECTOR)
					return TRUE;
			}
		}
		return FALSE;
	}
}

void
dmpips_psddef(FILE *fp)
{
	fprintf(fp, "/ICEInitExt {\n");
	fprintf(fp, "\t/ICEnoperands count def\n");
	fprintf(fp, "\t/ICEndicts countdictstack def\n");
	fprintf(fp, "\t/ICEsave save def\n");
	fprintf(fp, "\t/showpage { } def\n");
	fprintf(fp, "} bind def\n");
	fprintf(fp, "/ICETermExt {\n");
	fprintf(fp, "\tcount ICEnoperands gt {\n");
	fprintf(fp, "\t\t/ICEpopoperands count ICEnoperands sub def\n");
	fprintf(fp, "\t\t1 1 ICEpopoperands { pop pop } for\n");
	fprintf(fp, "\t} if\n");
	fprintf(fp, "\tcountdictstack ICEndicts gt {\n");
	fprintf(fp, "\t\t/ICEpopdicts countdictstack ICEndicts sub def\n");
	fprintf(fp, "\t\t1 1 ICEpopdicts { pop end } for\n");
	fprintf(fp, "\t} if\n");
	fprintf(fp, "\tICEsave restore\n");
	fprintf(fp, "} bind def\n");
}

void
dmpips_pathtextdef(FILE *fp)
{
	fprintf(fp, "/ICEPathTextDict 35 dict def\n");
	fprintf(fp, "ICEPathTextDict begin\n");
	fprintf(fp, "/PathTextSetchar {\n");
	fprintf(fp, "\t/currchar str charcount 1 getinterval def\n");
	fprintf(fp, "\t/charcount charcount 1 add def\n");
	fprintf(fp, "\t/currw currchar stringwidth pop def\n");
	fprintf(fp, "\t/dw currw hscale mul 2 div ltrspace add def\n");
	fprintf(fp, "\tcharcount str length lt {\n");
	fprintf(fp, "\t\t/nextchar str charcount 1 getinterval def\n");
	fprintf(fp, "\t\t/nextw nextchar stringwidth pop def\n");
	fprintf(fp, "\t\t/dw nextw hscale mul 2 div dw add def\n");
	fprintf(fp, "\t} {\n");
	fprintf(fp, "\t\t/nextw 0 def\n");
	fprintf(fp, "\t} ifelse\n");
	fprintf(fp, "\tgsave\n");
	fprintf(fp, "\tcpx cpy itransform translate\n");
	fprintf(fp, "\tdy dx atan rotate\n");
	fprintf(fp, "\thscale vscale scale\n");
	fprintf(fp, "\tcurrw 2 div neg 0 moveto\n");
	fprintf(fp, "\tcurrchar show\n");
	fprintf(fp, "\tltrspace hscale div nextw 2 div add 0 rmoveto\n");
	fprintf(fp, "\tcurrentpoint transform\n");
	fprintf(fp, "\t/cpy exch def\n");
	fprintf(fp, "\t/cpx exch def\n");
	fprintf(fp, "\tgrestore\n");
	fprintf(fp, "\t/setdist setdist dw add def\n");
	fprintf(fp, "} def\n");
	fprintf(fp, "/PathTextMovetoproc {\n");
	fprintf(fp, "\t/newy exch def\n");
	fprintf(fp, "\t/newx exch def\n");
	fprintf(fp, "\t/firstx newx def\n");
	fprintf(fp, "\t/firsty newy def\n");
	fprintf(fp, "\tpathstarted 1 eq {\n");
	fprintf(fp, "\t\t/ovr 0 def\n");
	fprintf(fp, "\t} {\n");
	fprintf(fp, "\t\t/ovr setdist def\n");
	fprintf(fp, "\t} ifelse\n");
	fprintf(fp, "\t/pathstarted 1 def\n");
	fprintf(fp, "\tnewx newy transform\n");
	fprintf(fp, "\t/cpy exch def\n");
	fprintf(fp, "\t/cpx exch def\n");
	fprintf(fp, "} def\n");
	fprintf(fp, "/PathTextLinetoproc {\n");
	fprintf(fp, "\t/oldx newx def\n");
	fprintf(fp, "\t/oldy newy def\n");
	fprintf(fp, "\t/newy exch def\n");
	fprintf(fp, "\t/newx exch def\n");
	fprintf(fp, "\t/dx newx oldx sub def\n");
	fprintf(fp, "\t/dy newy oldy sub def\n");
	fprintf(fp, "\t/dist dx dup mul dy dup mul add sqrt def\n");
	fprintf(fp, "\tdist 0 ne {\n");
	fprintf(fp, "\t\t/dsx dx dist div ovr mul def\n");
	fprintf(fp, "\t\t/dsy dy dist div ovr mul def\n");
	fprintf(fp, "\t\toldx dsx add oldy dsy add transform\n");
	fprintf(fp, "\t\t/cpy exch def\n");
	fprintf(fp, "\t\t/cpx exch def\n");
	fprintf(fp, "\t\t/pathdist pathdist dist add def\n");
	fprintf(fp, "\t\t{\n");
	fprintf(fp, "\t\t\tsetdist pathdist le {\n");
	fprintf(fp, "\t\t\t\tcharcount str length lt {\n");
	fprintf(fp, "\t\t\t\t\tPathTextSetchar\n");
	fprintf(fp, "\t\t\t\t} {\n");
	fprintf(fp, "\t\t\t\t\texit\n");
	fprintf(fp, "\t\t\t\t} ifelse\n");
	fprintf(fp, "\t\t\t} {\n");
	fprintf(fp, "\t\t\t\t/ovr setdist pathdist sub def\n");
	fprintf(fp, "\t\t\t\texit\n");
	fprintf(fp, "\t\t\t} ifelse\n");
	fprintf(fp, "\t\t} loop\n");
	fprintf(fp, "\t} if\n");
	fprintf(fp, "} def\n");
	fprintf(fp, "/PathTextCurvetoproc {\n");
	fprintf(fp, "\t(ERROR: No curveto after flattenpath.) print\n");
	fprintf(fp, "} def\n");
	fprintf(fp, "/closepathproc {\n");
	fprintf(fp, "\tfirstx firsty PathTextLinetoproc\n");
	fprintf(fp, "\tfirstx firsty PathTextMovetoproc\n");
	fprintf(fp, "} def\n");
	fprintf(fp, "/PathText {\n");
	fprintf(fp, "\t/vscale exch def\n");
	fprintf(fp, "\t/hscale exch def\n");
	fprintf(fp, "\t/ltrspace exch def\n");
	fprintf(fp, "\t/offset exch def\n");
	fprintf(fp, "\t/str exch def\n");
	fprintf(fp, "\t/charcount 0 def\n");
	fprintf(fp, "\t/pathstarted 0 def\n");
	fprintf(fp, "\t/currchar str charcount 1 getinterval def\n");
	fprintf(fp, "\t/dw currchar stringwidth pop hscale mul 2 div def\n");
	fprintf(fp, "\t/setdist offset dw add def\n");
	fprintf(fp, "\t/pathdist 0 def\n");
	fprintf(fp, "\tgsave\n");
	fprintf(fp, "\tflattenpath\n");
	fprintf(fp, "\t{ PathTextMovetoproc } { PathTextLinetoproc }\n");
	fprintf(fp, "\t{ PathTextCurvetoproc } { PathTextClosepathproc } pathforall\n");
	fprintf(fp, "\tgrestore\n");
	fprintf(fp, "\tnewpath\n");
	fprintf(fp, "} def\n");
	fprintf(fp, "end\n");
	return;
}

void
dmpips_vectordef(FILE *fp)
{
	fprintf(fp, "/ICEVectorDict 25 dict def\n");
	fprintf(fp, "ICEVectorDict begin\n");
	fprintf(fp, "/Pointer {\n");
	fprintf(fp, "\tgsave\n");
	fprintf(fp, "\ttranslate\n");
	fprintf(fp, "\trotate\n");
	fprintf(fp, "\t0 setlinejoin\n");
	fprintf(fp, "\t[] 0 setdash\n");
	fprintf(fp, "\tptrstyle 0 eq {\n");
	fprintf(fp, "\t\tnewpath\n");
	fprintf(fp, "\t\tptrolen ptrwd moveto\n");
	fprintf(fp, "\t\t0 0 lineto\n");
	fprintf(fp, "\t\tptrolen ptrwd neg lineto\n");
	fprintf(fp, "\t\tstroke\n");
	fprintf(fp, "\t} {\n");
	fprintf(fp, "\t\tnewpath\n");
	fprintf(fp, "\t\t0 0 moveto\n");
	fprintf(fp, "\t\tptrolen ptrwd lineto\n");
	fprintf(fp, "\t\tptrilen 0 lineto\n");
	fprintf(fp, "\t\tptrolen ptrwd neg lineto\n");
	fprintf(fp, "\t\tclosepath\n");
	fprintf(fp, "\t\tfill\n");
	fprintf(fp, "\t} ifelse\n");
	fprintf(fp, "\tgrestore\n");
	fprintf(fp, "} def\n");
	fprintf(fp, "/Vector {\n");
	fprintf(fp, "\t/ptrilen exch def\n");
	fprintf(fp, "\t/ptrolen exch def\n");
	fprintf(fp, "\t/ptrwd exch 2 div def\n");
	fprintf(fp, "\t/ptrstyle exch def\n");
	fprintf(fp, "\t/ptr exch def\n");
	fprintf(fp, "\t/linewd exch def\n");
	fprintf(fp, "\t/ystop exch 72 mul def\n");
	fprintf(fp, "\t/xstop exch 72 mul def\n");
	fprintf(fp, "\t/ystart exch 72 mul def\n");
	fprintf(fp, "\t/xstart exch 72 mul def\n");
	fprintf(fp, "\t/xdiff xstop xstart sub def\n");
	fprintf(fp, "\t/ydiff ystop ystart sub def\n");
	fprintf(fp, "\t/veclen xdiff xdiff mul ydiff ydiff mul add sqrt def\n");
	fprintf(fp, "\t/angle ydiff xdiff atan def\n");
	fprintf(fp, "\tgsave\n");
	fprintf(fp, "\tlinewd setlinewidth\n");
	fprintf(fp, "\txstart ystart translate\n");
	fprintf(fp, "\tangle rotate\n");
	fprintf(fp, "\tgsave\n");
	fprintf(fp, "\t1 ptr eq {\n");
	fprintf(fp, "\t\tnewpath\n");
	fprintf(fp, "\t\t0 0 moveto\n");
	fprintf(fp, "\t\tptrolen ptrwd lineto\n");
	fprintf(fp, "\t\tptrolen linewd lineto\n");
	fprintf(fp, "\t\tveclen linewd add linewd lineto\n");
	fprintf(fp, "\t\tveclen linewd add linewd neg lineto\n");
	fprintf(fp, "\t\tptrolen linewd neg lineto\n");
	fprintf(fp, "\t\tptrolen ptrwd neg lineto\n");
	fprintf(fp, "\t\tclosepath\n");
	fprintf(fp, "\t\tclip\n");
	fprintf(fp, "\t} if\n");
	fprintf(fp, "\t2 ptr eq {\n");
	fprintf(fp, "\t\tnewpath\n");
	fprintf(fp, "\t\tlinewd neg linewd moveto\n");
	fprintf(fp, "\t\tveclen ptrolen sub linewd lineto\n");
	fprintf(fp, "\t\tveclen ptrolen sub ptrwd lineto\n");
	fprintf(fp, "\t\tveclen 0 lineto\n");
	fprintf(fp, "\t\tveclen ptrolen sub ptrwd neg lineto\n");
	fprintf(fp, "\t\tveclen ptrolen sub linewd neg lineto\n");
	fprintf(fp, "\t\tlinewd neg linewd neg lineto\n");
	fprintf(fp, "\t\tclosepath\n");
	fprintf(fp, "\t\tclip\n");
	fprintf(fp, "\t} if\n");
	fprintf(fp, "\t3 ptr eq {\n");
	fprintf(fp, "\t\tnewpath\n");
	fprintf(fp, "\t\t0 0 moveto\n");
	fprintf(fp, "\t\tptrolen ptrwd lineto\n");
	fprintf(fp, "\t\tptrolen linewd lineto\n");
	fprintf(fp, "\t\tveclen ptrolen sub linewd lineto\n");
	fprintf(fp, "\t\tveclen ptrolen sub ptrwd lineto\n");
	fprintf(fp, "\t\tveclen 0 lineto\n");
	fprintf(fp, "\t\tveclen ptrolen sub ptrwd neg lineto\n");
	fprintf(fp, "\t\tveclen ptrolen sub linewd neg lineto\n");
	fprintf(fp, "\t\tptrolen linewd neg lineto\n");
	fprintf(fp, "\t\tptrolen ptrwd neg lineto\n");
	fprintf(fp, "\t\tclosepath\n");
	fprintf(fp, "\t\tclip\n");
	fprintf(fp, "\t} if\n");
	fprintf(fp, "\tnewpath\n");
	fprintf(fp, "\t0 0 moveto\n");
	fprintf(fp, "\tveclen 0 lineto\n");
	fprintf(fp, "\tstroke\n");
	fprintf(fp, "\tgrestore\n");
	fprintf(fp, "\t1 ptr eq {\n");
	fprintf(fp, "\t\t0 0 0 Pointer\n");
	fprintf(fp, "\t} if\n");
	fprintf(fp, "\t2 ptr eq {\n");
	fprintf(fp, "\t\t180 veclen 0 Pointer\n");
	fprintf(fp, "\t} if\n");
	fprintf(fp, "\t3 ptr eq {\n");
	fprintf(fp, "\t\t0 0 0 Pointer\n");
	fprintf(fp, "\t\t180 veclen 0 Pointer\n");
	fprintf(fp, "\t} if\n");
	fprintf(fp, "\tgrestore\n");
	fprintf(fp, "} def\n");
	fprintf(fp, "end\n");
	return;
}

void
dmpips_pthobjs(FILE *fp)
{
	Path *pth;
	Grobj *g, **atoms;
	int natoms, atom;
	float po, l;

	if (paths == (Path *) NULL)
		return;

	if (ice_op == DUMP_ALL) {
		for (pth= paths; pth != (Path *) NULL; pth= (Path *) pth->succ())
			(void) pth->icedump(fp);
		return;
	}

	for (pth= paths; pth != (Path *) NULL; pth= (Path *) pth->succ())
		pth->setdump(PATH_NODUMP);
	if (pg_clip != (Path *) NULL)
		pg_clip->setdump(PATH_DUMP);

	atom= 0;
	if (dmp_obj->gettype() == GROBJ_COMPOSITE) {
		((Composite *) dmp_obj)->getatoms(&natoms, &atoms);
		g= atoms[atom];
	}
	else {
		natoms= 1;
		g= dmp_obj;
	}
	while (g != (Grobj *) NULL) {
		if ((pth= g->getclip()) != (Path *) NULL)
			pth->setdump(PATH_DUMP);
		if (g->gettype() == GROBJ_INTOBJ) {
			if (((Intobj *) g)->getintobjtype() == INTOBJ_TEXT) {
				if (((Text *) g)->getjustify() == TEXT_PATH) {
					((Text *) g)->getpath(&pth, &po, &l);
					if (pth != (Path *) NULL)
						pth->setdump(PATH_DUMP);
				}
			}
		}

		atom+= 1;
		if (atom < natoms)
			g= atoms[atom];
		else
			g= (Grobj *) NULL;
	}

	for (pth= paths; pth != (Path *) NULL; pth= (Path *) pth->succ()) {
		if (pth->getdump() == PATH_DUMP)
			(void) pth->icedump(fp);
	}

	return;
}

void
dmpips_cmpobjs(FILE *fp)
{
	Composite *cmp;
	Grobj *g, **atoms;
	int natoms, atom;
	char errmsg[MAX_ERRMSGLEN+1];

	if (cmpobjs == (Composite *) NULL)
		return;

	if (ice_op == DUMP_ALL) {
		for (cmp= cmpobjs; cmp != (Composite *) NULL; cmp= (Composite *) cmp->succ()) {
			if (cmp->icedump(fp) != GROBJ_SUCCESS) {
				(void) sprintf(errmsg, "Error dumping ICE directives for '%s'.", cmp->getname());
				ice_err(errmsg, NONFATAL);
			}
		}
		return;
	}

	for (cmp= cmpobjs; cmp != (Composite *) NULL; cmp= (Composite *) cmp->succ())
		cmp->setdump(COMPOSITE_NODUMP);

	atom= 0;
	if (dmp_obj->gettype() == GROBJ_COMPOSITE) {
		((Composite *) dmp_obj)->setdump(COMPOSITE_DUMP);
		((Composite *) dmp_obj)->getatoms(&natoms, &atoms);
		g= atoms[atom];
	}
	else {
		natoms= 1;
		g= dmp_obj;
	}
	while (g != (Grobj *) NULL) {
		while ((g= g->getparent()) != (Grobj *) NULL)
			((Composite *) g)->setdump(COMPOSITE_DUMP);

		atom+= 1;
		if (atom < natoms)
			g= atoms[atom];
		else
			g= (Grobj *) NULL;
	}

	for (cmp= cmpobjs; cmp != (Composite *) NULL; cmp= (Composite *) cmp->succ()) {
		if (cmp->getdump() == COMPOSITE_DUMP) {
			if (cmp->icedump(fp) != GROBJ_SUCCESS) {
				(void) sprintf(errmsg, "Error dumping ICE directives for '%s'.", cmp->getname());
				ice_err(errmsg, NONFATAL);
			}
		}
	}

	return;
}

void
dmpips_pg(FILE *fp)
{
	fprintf(fp, "%%%%ICE-Pg: Begin\n");
	fprintf(fp, "%%%%ICE-Pg: Dim %4.2f %4.2f %1d\n", pg_width, pg_height, pg_dpi);

	fprintf(fp, "%%%%ICE-Pg: Flags %1d %1d\n", pg_update, pg_units);

	fprintf(fp, "%%%%ICE-Pg: Loc %1d\n", pg_loc);
	fprintf(fp, "%%%%ICE-Pg: Locdpy %1d\n", pg_locdisplay);

	fprintf(fp, "%%%%ICE-Pg: SI %4.2f %4.2f\n", pg_hsi, pg_vsi);
	fprintf(fp, "%%%%ICE-Pg: RI %4.2f %4.2f\n", pg_xri, pg_yri);
	fprintf(fp, "%%%%ICE-Pg: RU %4.2f %4.2f\n", pg_xru, pg_yru);

	fprintf(fp, "%%%%ICE-Pg: Ohlt %1d\n", pg_originhlt);

	fprintf(fp, "%%%%ICE-Pg: BG %1d %1d %1d %1d\n", pg_bg, (int) pg_rbg, (int) pg_gbg, (int) pg_bbg);

	if (pg_clip != (Path *) NULL)
		fprintf(fp, "%%%%ICE-Pg: Clip %s\n", pg_clip->getname());

	fprintf(fp, "%%%%ICE-Pg: End\n");
	return;
}

void
dmpips_gdf(FILE *fp)
{
	fprintf(fp, "%%%%ICE-Gdf: Begin\n");
	fprintf(fp, "%%%%ICE-Gdf: Font %s\n", gdf_fontname);

	fprintf(fp, "%%%%ICE-Gdf: Fontsz %4.2f %4.2f\n", gdf_fontsize, gdf_fontlead);

	fprintf(fp, "%%%%ICE-Gdf: Line %6.4f\n", gdf_linewidth);

	fprintf(fp, "%%%%ICE-Gdf: FG %1d %1d %1d %1d\n", gdf_fg, (int) gdf_rfg, (int) gdf_gfg, (int) gdf_bfg);
	fprintf(fp, "%%%%ICE-Gdf: BG %1d %1d %1d %1d\n", gdf_bg, (int) gdf_rbg, (int) gdf_gbg, (int) gdf_bbg);

	fprintf(fp, "%%%%ICE-Gdf: Mark %1d %6.4f\n", gdf_mrktype, gdf_mrksize);

	fprintf(fp, "%%%%ICE-Gdf: Bnd %6.4f %1d %1d %1d %1d\n", gdf_bndwidth, gdf_bnd, (int) gdf_rbnd, (int) gdf_gbnd, (int) gdf_bbnd);

	fprintf(fp, "%%%%ICE-Gdf: Fill %1d %1d %1d %1d\n", gdf_fill, (int) gdf_rfill, (int) gdf_gfill, (int) gdf_bfill);

	fprintf(fp, "%%%%ICE-Gdf: DTK %1d %1d %1d %1d\n", gdf_dtk, (int) gdf_rdtk, (int) gdf_gdtk, (int) gdf_bdtk);

	fprintf(fp, "%%%%ICE-Gdf: End\n");
	return;
}

void
dmpipsabort_proc(Panel *p, Panel_item *pi)
{
	XUnmapWindow(dpy, dmpips_frame);
	ice_op= MAIN_MENU;
	return;
}

void
dmpallras_proc(Menu *m, Menu_item *mi)
{
	int val;
	char filenm[LXADEF_MAXSTORE+1];
	char errmsg[MAX_ERRMSGLEN+1];
	FILE *fp;
	Pixrect *pr;
	void dmpras_screenres(FILE *, Pixrect *);
	void dmpras_32(FILE *, Pixrect *);

	if (alert_prompt(progname, dpy, &val,
			LXA_TEXT, "Filename:", "", filenm,
			LXA_BUTTON, "Continue", 0,
			LXA_BUTTON, "Abort", 1,
			LXA_NULL) == LX_ERROR)
		ice_err("Alert failure.", FATAL);

	if (val == 1)
		return;

	if (strlen(filenm) == 0) {
		ice_err("No filename specified.", NONFATAL);
		return;
	}
	if ((fp= fopen(filenm, "w")) == (FILE *) NULL) {
		(void) sprintf(errmsg, "Cannot open file '%s'.", filenm);
		ice_err(errmsg, NONFATAL);
		return;
	}

	XFillRectangle(dpy, pg_cpm, pg_bggc, 0, 0, pg_pixwidth, pg_pixheight);
	canvas_flush(pg_canvas);
	XDefineCursor(dpy, pg_cwin, hg_cursor);
	XSync(dpy, False);

	if (nrasters == 0) {
		if ((pr= mem_create(pg_pixwidth, pg_pixheight, pg_pixdepth)) == (Pixrect *) NULL) {
			ice_err("Cannot create dump pixrect.", NONFATAL);
			(void) fclose(fp);
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			return;
		}
		dmpras_screenres(fp, pr);
	}
	else {
		if ((pr= mem_create(pg_pixwidth, pg_pixheight, 32)) == (Pixrect *) NULL) {
			ice_err("Cannot create dump pixrect.", NONFATAL);
			(void) fclose(fp);
			XDefineCursor(dpy, pg_cwin, std_cursor);
			XSync(dpy, False);
			return;
		}
		dmpras_32(fp, pr);
	}

	(void) fclose(fp);
	pr_destroy(pr);
	XDefineCursor(dpy, pg_cwin, std_cursor);
	XSync(dpy, False);

	return;
}

void
dmpras_screenres(FILE *fp, Pixrect *pr)
{
	char errmsg[MAX_ERRMSGLEN+1];
	Grobj *gr;
	Path *pth;
	int i, pix, flags, x, y;
	colormap_t suncmap;
	XImage *xim;

	switch (pg_pixdepth) {
	case 1:
		suncmap.type= RMT_NONE;
		suncmap.length= 0;
		suncmap.map[0]= suncmap.map[1]= suncmap.map[2]= (unsigned char *) NULL;
		break;
	case 8:
		suncmap.type= RMT_EQUAL_RGB;
		suncmap.length= PSEUDOCOLOR_MAPSZ;
		if ((suncmap.map[0]= new unsigned char[PSEUDOCOLOR_MAPSZ*3]) == (unsigned char *) NULL) {
			ice_err("Memory allocation error.", NONFATAL);
			pg_forceupdate= TRUE;
			pg_draw();
			pg_forceupdate= FALSE;
			return;
		}
		suncmap.map[1]= suncmap.map[0]+PSEUDOCOLOR_MAPSZ;
		suncmap.map[2]= suncmap.map[1]+PSEUDOCOLOR_MAPSZ;
		for (i= 0; i < PSEUDOCOLOR_MAPSZ; i++) {
			suncmap.map[0][i]= def_colors[i].red >> 8;
			suncmap.map[1][i]= def_colors[i].green >> 8;
			suncmap.map[2][i]= def_colors[i].blue >> 8;
		}
		break;
	default:
		ice_err("Unsupported display depth.", NONFATAL);
		return;
	}

	news_setcanvas((int) pg_cpm, pg_dpi);

	if (pg_clip != (Path *) 0) {
		flags= PATH_DRAWCLOSE | PATH_DRAWCLIP;
		pg_clip->draw(flags, (FILE *) NULL);
	}

	/* make sure that object list is properly sorted by
	   sequence, object type and dump transparency key */
	if (grobjs != (Grobj *) NULL)
		grobjs->sortsequence(&grobjs);

	for (gr= grobjs; gr != (Grobj *) NULL; gr= (Grobj *) gr->succ()) {
		XSync(dpy, False);

		switch (gr->gettype()) {
		case GROBJ_PSDOC:
			if (((Psdoc *) gr)->draw((FILE *) NULL) != GROBJ_SUCCESS) {
				(void) sprintf(errmsg, "Error rendering '%s'.", ((Psdoc *) gr)->getname());
				ice_err(errmsg, NONFATAL);
				pg_forceupdate= TRUE;
				pg_draw();
				pg_forceupdate= FALSE;
				delete suncmap.map[0];
				return;
			}
			break;
		case GROBJ_INTOBJ:
			if (((Intobj *) gr)->draw((FILE *) NULL) != GROBJ_SUCCESS) {
				(void) sprintf(errmsg, "Error rendering '%s'.", ((Intobj *) gr)->getname());
				ice_err(errmsg, NONFATAL);
				pg_forceupdate= TRUE;
				pg_draw();
				pg_forceupdate= FALSE;
				delete suncmap.map[0];
				return;
			}
			break;
		}
	}

	XSync(dpy, False);

	/* copy all pixels into the output raster */
	if ((xim= XGetImage(dpy, pg_cpm, 0, 0, pg_pixwidth, pg_pixheight, AllPlanes, ZPixmap)) == (XImage *) NULL) {
		ice_err("XImage creation error.", NONFATAL);
		pg_forceupdate= TRUE;
		pg_draw();
		pg_forceupdate= FALSE;
		delete suncmap.map[0];
		return;
	}
	for (y= 0; y < pg_pixheight; y++) {
		for (x= 0; x < pg_pixwidth; x++) {
			pix= (int) XGetPixel(xim, x, y);
			pr_put(pr, x, y, pix);
		}
	}
	XDestroyImage(xim);

	if (pr_dump(pr, fp, &suncmap, RT_STANDARD, 0) != 0) {
		ice_err("Rasterfile write error.", NONFATAL);
		pg_forceupdate= TRUE;
		pg_draw();
		pg_forceupdate= FALSE;
		delete suncmap.map[0];
		return;
	}
	delete suncmap.map[0];

	for (pth= paths; pth != (Path *) NULL; pth= (Path *) pth->succ()) {
		if (pth->getvisibility() == PATH_VISIBLE)
			pth->x11draw(pg_dpi, pg_pixheight, pg_cpm);
	}

	if (pg_originhlt == PG_ORIGINHLT)
		pg_draworiginhlt((boolean) FALSE);

	canvas_flush(pg_canvas);

	return;
}

void
dmpras_32(FILE *fp, Pixrect *pr)
{
	int bgcolor;
	char errmsg[MAX_ERRMSGLEN+1];
	Grobj *gr;
	int mapsz;

	switch (pg_pixdepth) {
	case 1:
		mapsz= 2;
		break;
	case 8:
		mapsz= 256;
		break;
	default:
		ice_err("Unsupported display depth.", NONFATAL);
		return;
	}

	bgcolor= (pg_bbg << 16) | (pg_gbg << 8) | pg_rbg;
	pr_rop(pr, 0, 0, pg_pixwidth, pg_pixheight, PIX_COLOR(bgcolor) | PIX_SRC, (Pixrect *) NULL, 0, 0);

	/* make sure that object list is properly sorted by
	   sequence, object type and dump transparency key */
	if (grobjs != (Grobj *) NULL)
		grobjs->sortsequence(&grobjs);

	for (gr= grobjs; gr != (Grobj *) NULL; gr= (Grobj *) gr->succ()) {
		Grobj *start, *stop, *next;
		unsigned char ucs[4], ucn[4];
		int flags, sdtk, ndtk;
		unsigned int spix, npix;
		unsigned long dtkpix;
		XImage *xim;
		int x, y, srcpix, dstpix;
		char mask[32];
		int i, r, b, g;
		XGCValues gcv;
		GC clrgc;

		ucs[0]= ucn[0]= 0;
		switch (gr->gettype()) {
		case GROBJ_PSDOC:
		case GROBJ_INTOBJ:

			/* batch all consecutive Psdoc and Intobj objects
			   using the same dump transparency key */
			for (start= stop= gr; stop->succ() != (Dlnk *) NULL; stop= (Grobj *) stop->succ()) {
				next= (Grobj *) stop->succ();

				/* Raster encountered */
				if (next->gettype() == GROBJ_RASTER)
					break;

				/* check for dump transparency key match */
				stop->getdtk(&sdtk, &ucs[3], &ucs[2], &ucs[1]);
				bcopy((char *) ucs, (char *) &spix, sizeof(int));
				next->getdtk(&ndtk, &ucn[3], &ucn[2], &ucn[1]);
				bcopy((char *) ucn, (char *) &npix, sizeof(int));
				if (npix != spix)
					break;
			}

			/* create a mask of all pixel values with
			   the same primary intensities as dtkpix */
			dtkpix= start->getdtkpix();
			bzero(mask, 32);
			r= def_colors[(int) dtkpix].red >> 8;
			g= def_colors[(int) dtkpix].green >> 8;
			b= def_colors[(int) dtkpix].blue >> 8;
			for (i= 0; i < mapsz; i++) {
				if ((r == (int) (def_colors[i].red >> 8)) &&
		    		(g == (int) (def_colors[i].green >> 8)) &&
		    		(b == (int) (def_colors[i].blue >> 8)))
					mask[i/8]|= (0x1 << (i%8));
			}

			/* initialize all of pg_cpm to dtkpix */
			gcv.function= GXcopy;
			gcv.foreground= dtkpix;
			gcv.plane_mask= AllPlanes;
			if ((clrgc= XCreateGC(dpy, pg_cpm, (GCFunction | GCForeground | GCPlaneMask), &gcv)) == None) {
				ice_err("GC creation error.", NONFATAL);
				pg_forceupdate= TRUE;
				pg_draw();
				pg_forceupdate= FALSE;
				return;
			}
			XFillRectangle(dpy, pg_cpm, clrgc, 0, 0, pg_pixwidth, pg_pixheight);
			XFreeGC(dpy, clrgc);
			XSync(dpy, False);

			news_setcanvas((int) pg_cpm, pg_dpi);

			if (pg_clip != (Path *) 0) {
				flags= PATH_DRAWCLOSE | PATH_DRAWCLIP;
				pg_clip->draw(flags, (FILE *) NULL);
			}

			/* draw all of the batched objects */
			for (gr= start; TRUE; gr= (Grobj *) gr->succ()) {
				switch (gr->gettype()) {
				case GROBJ_PSDOC:
					if (((Psdoc *) gr)->draw((FILE *) NULL) != GROBJ_SUCCESS) {
						(void) sprintf(errmsg, "Error rendering '%s'.", ((Psdoc *) gr)->getname());
						ice_err(errmsg, NONFATAL);
						pg_forceupdate= TRUE;
						pg_draw();
						pg_forceupdate= FALSE;
						return;
					}
					break;
				case GROBJ_INTOBJ:
					if (((Intobj *) gr)->draw((FILE *) NULL) != GROBJ_SUCCESS) {
						ice_err("Internal object dump error.", NONFATAL);
						pg_forceupdate= TRUE;
						pg_draw();
						pg_forceupdate= FALSE;
						return;
					}
					break;
				}
				if (gr == stop)
					break;
			}

			/* copy all non-dtkpix pixels into the output raster */
			if ((xim= XGetImage(dpy, pg_cpm, 0, 0, pg_pixwidth, pg_pixheight, AllPlanes, ZPixmap)) == (XImage *) NULL) {
				ice_err("XImage creation error.", NONFATAL);
				pg_forceupdate= TRUE;
				pg_draw();
				pg_forceupdate= FALSE;
				return;
			}
			ucs[0]= 0;
			for (y= 0; y < pg_pixheight; y++) {
				for (x= 0; x < pg_pixwidth; x++) {
					srcpix= (int) XGetPixel(xim, x, y);
		
					/* ignore pixels with the same
			   		primary intensities as dtkpix */
					if (mask[srcpix/8] & (0x1 << (srcpix%8)))
						continue;
		
					ucs[1]= def_colors[(int) srcpix].blue >> 8;
					ucs[2]= def_colors[(int) srcpix].green >> 8;
					ucs[3]= def_colors[(int) srcpix].red >> 8;
					bcopy((char *) ucs, (char *) &dstpix, sizeof(int));
					pr_put(pr, x, y, dstpix);
				}
			}
			XDestroyImage(xim);
			break;

		case GROBJ_RASTER:
			if (((Raster *) gr)->dump(pr, pg_pixwidth, pg_pixheight) != GROBJ_SUCCESS) {
				ice_err("Raster dump error.", NONFATAL);
				pg_forceupdate= TRUE;
				pg_draw();
				pg_forceupdate= FALSE;
				return;
			}
			break;
		}
	}

	if (pr_dump(pr, fp, (colormap_t *) NULL, RT_STANDARD, 0) != 0) {
		ice_err("Rasterfile write error.", NONFATAL);
		return;
	}

	pg_forceupdate= TRUE;
	pg_draw();
	pg_forceupdate= FALSE;

	return;
}

void
dmppth_proc(Menu *m, Menu_item *mi)
{
	Path *pth;
	int val;
	char filenm[LXADEF_MAXSTORE+1];
	char errmsg[MAX_ERRMSGLEN+1];
	FILE *fp;

	if ((pth= (Path *) menuitem_get(mi, LXMI_CLIENTDATA)) == (Path *) NULL) {
		ice_err("Cannot locate selected path.", NONFATAL);
		return;
	}

	if (alert_prompt(progname, dpy, &val,
			LXA_TEXT, "Filename:", "", filenm,
			LXA_BUTTON, "Continue", 0,
			LXA_BUTTON, "Abort", 1,
			LXA_NULL) == LX_ERROR)
		ice_err("Alert failure.", FATAL);

	if (val == 1)
		return;

	if (strlen(filenm) == 0) {
		ice_err("No filename specified.", NONFATAL);
		return;
	}
	if ((fp= fopen(filenm, "w")) == (FILE *) NULL) {
		(void) sprintf(errmsg, "Cannot open file '%s'.", filenm);
		ice_err(errmsg, NONFATAL);
		return;
	}
	(void) pth->icedump(fp);
	(void) fclose(fp);

	return;
}
