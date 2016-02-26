/*
	params.c - read/write parameter files

	Much of this is to maintain compatibility with older versions.

	$Header: params.c,v 1.11 89/09/02 10:08:04 pturner Locked $
*/

#include <stdio.h>
#include "globals.h"
#include "defines.h"
#include "objdefs.h"


static char *cstr5 = "gr parameter file 5";
static char *cstr6 = "gr parameter file 6";
static char *cstr7 = "##grtool parameter file";

extern int errno;
extern char *sys_errlist[];

extern plotstr legstr[];
extern plotstr pstr[];

static char readbuf[256];

getparms(plfile)
    char plfile[];

{
    int icheck;
    char s[256];
    FILE *pp;

    if ((pp = fopen(plfile, "r")) == NULL) {
	sprintf(readbuf, "Parameter file %s not found", plfile);
	errwin(readbuf);
	plfile[0] = 0;
    } else {
	fgets(s, 255, pp);
	s[strlen(s) - 1] = 0;
	icheck = 0;
	if (!strcmp(s, cstr5))
	    icheck = 5;
	else if (!strcmp(s, cstr6))
	    icheck = 6;
	else if (!strcmp(s, cstr7))
	    icheck = 7;
	if (!icheck) {
	    sprintf(readbuf, "File %s not legal parameter file", plfile);
	    errwin(readbuf);
	    fclose(pp);
	    return;
	}
	switch (icheck) {
	case 5:
	    read5(pp);
	    return;
	    break;
	case 6:
	    read6(pp);
	    return;
	    break;
	case 7:
	    fclose(pp);
	    getparms0(plfile);
	    return;
	    break;
	}
    }
}

read5(pp)
    FILE *pp;
{
    int i, ls, ps, cs;
    char s[256];

    fgets(s, 255, pp);
    sscanf(s, "%lf %lf %lf %lf", &xg1, &xg2, &yg1, &yg2);
    fgets(s, 255, pp);
    sscanf(s, "%lf %lf %lf %lf", &xv1, &xv2, &yv1, &yv2);
    fgets(s, 255, pp);
    sscanf(s, "%lf %lf %lf %lf", &xt1, &xt2, &yt1, &yt2);
    fgets(s, 255, pp);
    sscanf(s, "%d %d", &xform, &yform);
    for (i = 0; i < maxplot; i++) {
	fgets(s, 255, pp);
	sscanf(s, "%d %d %d", &ls, &ps, &cs);
	setlinesym(i, ls);
	setplotsym(i, ps);
	setplotcolor(i, cs);
    }
    fgets(s, 255, pp);
    fgets(xlabel, 79, pp);
    xlabel[strlen(xlabel) - 1] = 0;
    fgets(ylabel, 79, pp);
    ylabel[strlen(ylabel) - 1] = 0;
    fgets(title, 79, pp);
    title[strlen(title) - 1] = 0;
    fgets(stitle, 79, pp);
    stitle[strlen(stitle) - 1] = 0;
    fgets(s, 255, pp);
    legendflag = 0;
    sscanf(s, "%d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d %d",
	   &xticsintflag, &yticsintflag, &boxflag, &boxon, &xticflag, &yticflag, &fformx, &fformy, &xticlflag, &yticlflag, &xgridflag,
	   &ygridflag, &xticslog, &yticslog, &xzflag, &yzflag, &xztflag, &yztflag, &legendflag);
    if (legendflag) {
	fgets(s, 255, pp);
	fgets(s, 255, pp);
	sscanf(s, "%lf %lf", &legx, &legy);
	for (i = 0; i < maxplot; i++) {
	    fgets(legstr[i].s, 79, pp);
	    legstr[i].s[strlen(legstr[i].s) - 1] = 0;
	}
    }
    fclose(pp);
}

read6(pp)
    FILE *pp;
{
    int i, ls, ps, cs, mtmp, stmp;
    char s[256];

    fgets(s, 255, pp);
    sscanf(s, "%lf %lf %lf %lf", &xg1, &xg2, &yg1, &yg2);
    fgets(s, 255, pp);
    sscanf(s, "%lf %lf %lf %lf", &xv1, &xv2, &yv1, &yv2);
    fgets(s, 255, pp);
    sscanf(s, "%lf %lf %lf %lf", &xt1, &xt2, &yt1, &yt2);
    fgets(s, 255, pp);
    sscanf(s, "%d %d", &xform, &yform);
    fgets(s, 255, pp);
    sscanf(s, "%d", &mtmp);
    for (i = 0; i < mtmp; i++) {
	fgets(s, 255, pp);
	sscanf(s, "%d %d %d", &ls, &ps, &cs);
	setlinesym(i, ls);
	setplotsym(i, ps);
	setplotcolor(i, cs);
    }
    fgets(s, 255, pp);
    fgets(xlabel, 79, pp);
    xlabel[strlen(xlabel) - 1] = 0;
    fgets(ylabel, 79, pp);
    ylabel[strlen(ylabel) - 1] = 0;
    fgets(title, 79, pp);
    title[strlen(title) - 1] = 0;
    fgets(stitle, 79, pp);
    stitle[strlen(stitle) - 1] = 0;
    fgets(s, 255, pp);
    sscanf(s, "%d %d", &xticsintflag, &yticsintflag);
    fgets(s, 255, pp);
    sscanf(s, "%d %d", &boxflag, &boxon);
    fgets(s, 255, pp);
    sscanf(s, "%d %d", &xticflag, &yticflag);
    fgets(s, 255, pp);
    sscanf(s, "%d %d", &fformx, &fformy);
    fgets(s, 255, pp);
    sscanf(s, "%d %d", &xticlflag, &yticlflag);
    fgets(s, 255, pp);
    sscanf(s, "%d %d", &xgridflag, &ygridflag);
    fgets(s, 255, pp);
    sscanf(s, "%d %d", &xticslog, &yticslog);
    fgets(s, 255, pp);
    sscanf(s, "%d %d", &xzflag, &yzflag);
    fgets(s, 255, pp);
    sscanf(s, "%d %d", &xztflag, &yztflag);
    fgets(s, 255, pp);
    fgets(s, 255, pp);
    sscanf(s, "%d", &legendflag);
    fgets(s, 255, pp);
    sscanf(s, "%lf %lf", &legx, &legy);
    for (i = 0; i < mtmp; i++) {
	fgets(legstr[i].s, 79, pp);
	legstr[i].s[strlen(legstr[i].s) - 1] = 0;
    }
    fgets(s, 255, pp);
    fgets(s, 255, pp);
    sscanf(s, "%d", &stmp);
    for (i = 0; i < stmp; i++) {
	fgets(pstr[i].s, 79, pp);
	pstr[i].s[strlen(pstr[i].s) - 1] = 0;
	fgets(s, 255, pp);
	sscanf(s, "%lf %lf", &pstr[i].x, &pstr[i].y);
	fgets(s, 255, pp);
	sscanf(s, "%lf", &pstr[i].size);
	fgets(s, 255, pp);
	sscanf(s, "%d", &pstr[i].rot);
	fgets(s, 255, pp);
	sscanf(s, "%d", &pstr[i].font);
    }
    if (fgets(s, 255, pp) != NULL) {
	nboxes = 0;
	fgets(s, 255, pp);
	sscanf(s, "%d", &stmp);
	for (i = 0; i < stmp; i++) {
	    fgets(s, 255, pp);
	    sscanf(s, "%le %le %le %le", &boxes[i].x1, &boxes[i].y1, &boxes[i].x2, &boxes[i].y2);
	    fgets(s, 255, pp);
	    sscanf(s, "%d %d %d", &boxes[i].color, &boxes[i].style, &boxes[i].active);
	    if (boxes[i].active)
		nboxes++;
	}
    }
    if (fgets(s, 255, pp) != NULL) {
	nlines = 0;
	fgets(s, 255, pp);
	sscanf(s, "%d", &stmp);
	for (i = 0; i < stmp; i++) {
	    fgets(s, 255, pp);
	    sscanf(s, "%le %le %le %le", &lines[i].x1, &lines[i].y1, &lines[i].x2, &lines[i].y2);
	    fgets(s, 255, pp);
	    sscanf(s, "%d %d %d %d", &lines[i].color, &lines[i].style, &lines[i].arrow, &lines[i].active);
	    if (lines[i].active)
		nlines++;
	}
    }
    fclose(pp);
}

#define	BOX0    1
#define	BOXTYPE 2
#define	DEFFONT 3
#define	ERRBAR	4
#define	ERRBARTYPE 5
#define	LEGSTR  6
#define	LEGFLAG 7
#define	LEGLOC  8
#define	LINES   9
#define	NBOXES  10
#define	NLINES  11
#define	NSETS   12
#define	NSTRINGS 13
#define	SETPROPS 14
#define	STRING   15
#define STRINGDEF 16
#define	SUBTITLE 17
#define	TICS     18
#define	TICSABS  19
#define	TICSFLAG 20
#define	TICSGRID 21
#define	TICSINOUT  22
#define	TICSINT    23
#define	TICSLFLAG  24
#define	TICSLOG    25
#define	TICSLTOP   26
#define	TICSLTYPE  27
#define	TICSOP   28
#define	TICSPREC   29
#define	TITLE      30
#define	VIEW       31
#define	WORLD      32
#define	XLABEL     33
#define	XTICANG     34
#define	YLABEL     35
#define	ZEROAXIS   36
#define	ZEROTICS   37

#define MAXPAR 37

struct pnames {
    char *s;
    int type;
} parmnames[] = {

    "box", BOX0,
    "boxtypes", BOXTYPE,
    "deffont", DEFFONT,
    "errorbar", ERRBAR,
    "errorbartype", ERRBARTYPE,
    "legendflag", LEGFLAG,
    "legendstr", LEGSTR,
    "legloc", LEGLOC,
    "line", LINES,
    "nboxes", NBOXES,
    "nlines", NLINES,
    "nsets", NSETS,
    "nstrings", NSTRINGS,
    "setprops", SETPROPS,
    "string", STRING,
    "stringdef", STRINGDEF,
    "subtitle", SUBTITLE,
    "ticflags", TICSFLAG,
    "ticlabels", TICSLFLAG,
    "tics", TICS,
    "ticsabs", TICSABS,
    "ticsgrid", TICSGRID,
    "ticsinout", TICSINOUT,
    "ticsint", TICSINT,
    "ticslog", TICSLOG,
    "ticsltop", TICSLTOP,
    "ticsltype", TICSLTYPE,
    "ticsop", TICSOP,
    "ticsprec", TICSPREC,
    "title", TITLE,
    "view", VIEW,
    "world", WORLD,
    "xlabel", XLABEL,
    "xticang", XTICANG,
    "ylabel", YLABEL,
    "zeroflag", ZEROAXIS,
    "zerotics", ZEROTICS
};

putparms(psfile)
    char psfile[];

{
    int i;
    FILE *pp;

    if (fexists(psfile)) {
	psfile[0] = '\0';
	return;
    }
    if ((pp = fopen(psfile, "w")) != NULL) {
	fprintf(pp, "##grtool parameter file\n");
	fprintf(pp, "# Do not touch the first line of this file!!!\n");
	fprintf(pp, "# $Header: params.c,v 1.11 89/09/02 10:08:04 pturner Locked $\n");
	fprintf(pp, "world %le %le %le %le \tworld xmin, xmax, ymin, ymax\n", xg1, xg2, yg1, yg2);
	fprintf(pp, "view %le %le %le %le \tviewport xmin, xmax, ymin, ymax\n", xv1, xv2, yv1, yv2);
	fprintf(pp, "tics %le %le %le %le \txmajor, xminor, ymajor, yminor\n", xt1, xt2, yt1, yt2);
	fprintf(pp, "ticsprec %d %d \tformat for x tic labels, format for y tic labels\n", xform, yform);
	fprintf(pp, "deffont %d \tnumber of sets\n", curfont);
	fprintf(pp, "nsets %d \tnumber of sets\n", maxplot);
	for (i = 0; i < maxplot; i++) {
	    fprintf(pp, "setprops %d %d %d %d \tset#, line sym, plot sym, color\n", i, getsetlinesym(i), getsetplotsym(i), getsetcolor(i));
	    fprintf(pp, "errorbar %d %d \tset#, error bar flag (-1 = not an error bar otherwise attached set#\n", i, iserrbar(i));
	    fprintf(pp, "errorbartype %d %d \tset#, type of error bar#\n", i, getseterrbarxy(i));
	}
	fprintf(pp, "# The following 4 strings are: xlabel, ylabel, title, sub-title\n");
	fprintf(pp, "xlabel %s\nylabel %s\ntitle %s\nsubtitle %s\n", xlabel, ylabel, title, stitle);
	fprintf(pp, "ticsint %d %d \ttics integer flag\n", xticsintflag, yticsintflag);
	fprintf(pp, "boxtypes %d %d \tboxflag, box on or off flag\n", boxflag, boxon);
	fprintf(pp, "ticflags %d %d \txtics flag, ytics flag\n", xticflag, yticflag);
	fprintf(pp, "ticsltop %d %d \tx/y tic labels on top/right\n", xtopflag, ytopflag);
	fprintf(pp, "ticsop %d %d \tx/y tic labels on opposite side\n", xticopflag, yticopflag);
	fprintf(pp, "xticang %d \txtic labels angle\n", xticangle);
	fprintf(pp, "ticsltype %d %d \tformat for x, format for y tic labels\n", fformx, fformy);
	fprintf(pp, "ticlabels %d %d \txtics label flag, ytics label flag\n", xticlflag, yticlflag);
	fprintf(pp, "ticsabs %d %d \ttics labels absolute value\n", xabsflag, yabsflag);
	fprintf(pp, "ticsgrid %d %d \txtics grid flag, ytics grid flag\n", xgridflag, ygridflag);
	fprintf(pp, "ticsinout %d %d \tx/ytics in or out\n", xticinoutflag, yticinoutflag);
	fprintf(pp, "ticslog %d %d \txtics logarithmic, ytics logarithmic\n", xticslog, yticslog);
	fprintf(pp, "zeroflag %d %d \ty=0 flag, x=0 flag\n", xzflag, yzflag);
	fprintf(pp, "zerotics %d %d \ty=0 tics flag, x=0 tics flag\n", xztflag, yztflag);

	fprintf(pp, "legendflag %d\n", legendflag);
	fprintf(pp, "# legx, legy, followed by the legends\n");
	fprintf(pp, "legloc %le %le \n", legx, legy);
	for (i = 0; i < maxplot; i++) {
	    fprintf(pp, "legendstr %s\n", legstr[i].s);
	}
	fprintf(pp, "# string (x,y), size, rotation, font\n");
	fprintf(pp, "nstrings %d\n", MAXSTR);
	for (i = 0; i < MAXSTR; i++) {
	    fprintf(pp, "string %le %le %le %d %d\n",
	     pstr[i].x, pstr[i].y, pstr[i].size, pstr[i].rot, pstr[i].font);
	    fprintf(pp, "stringdef %s\n", pstr[i].s);
	}
	fprintf(pp, "# box (x1,y1,x2,y2), color, style, active\n");
	fprintf(pp, "nboxes %d\n", MAXBOXES);
	for (i = 0; i < MAXBOXES; i++) {
	    fprintf(pp, "box %le %le %le %le %d %d %d\n", boxes[i].x1, boxes[i].y1,
		    boxes[i].x2, boxes[i].y2, boxes[i].color, boxes[i].style, boxes[i].active);
	}
	fprintf(pp, "# line (x1,y1,x2,y2), color, style, arrow, active\n");
	fprintf(pp, "nlines %d\n", MAXLINES);
	for (i = 0; i < MAXLINES; i++) {
	    fprintf(pp, "line %le %le %le %le %d %d %d %d\n", lines[i].x1, lines[i].y1,
		    lines[i].x2, lines[i].y2, lines[i].color, lines[i].style, lines[i].arrow,
		    lines[i].active);
	}
	fflush(pp);
	fclose(pp);
    } else {
	sprintf(readbuf, "Error opening parameter file %s", psfile);
	errwin(readbuf);
	psfile[0] = 0;
    }
}

getparms0(psfile)
    char psfile[];

{
    int itmp, type, mtmp, stmp, ltmp, btmp, nleg, nstr, ls, cs, ps;
    FILE *pp;
    char s[256];
    char name[256];

    if ((pp = fopen(psfile, "r")) != NULL) {
	nboxes = 0;
	nlines = 0;
	nleg = 0;
	nstr = 0;
	while (fgets(s, 255, pp) != NULL) {
	    if (s[0] != '#') {
		sscanf(s, "%s", name);
		type = findf(parmnames, name, MAXPAR);
		if (type >= 0) {
		    type = parmnames[type].type;
		    /* printf("%s %d\n",name,type); */
		    switch (type) {
		    case BOX0:
			sscanf(s, "%*s %le %le %le %le %d %d %d", &boxes[nboxes].x1, &boxes[nboxes].y1,
			       &boxes[nboxes].x2, &boxes[nboxes].y2, &boxes[nboxes].color,
			       &boxes[nboxes].style, &boxes[nboxes].active);
			nboxes++;
			break;
		    case BOXTYPE:
			sscanf(s, "%*s %d %d", &boxflag, &boxon);
			break;
		    case DEFFONT:
			sscanf(s, "%*s %d", &curfont);
			break;
		    case ERRBAR:
			sscanf(s, "%*s %d %d", &stmp, &itmp);
			makeseterrbar(stmp, itmp);
			break;
		    case ERRBARTYPE:
			sscanf(s, "%*s %d %d", &stmp, &itmp);
			seterrbarxy(stmp, itmp);
			break;
		    case LEGSTR:
			strcpy(legstr[nleg].s, s + 10);
			legstr[nleg].s[strlen(legstr[nleg].s) - 1] = 0;
			nleg++;
			break;
		    case LEGFLAG:
			sscanf(s, "%*s %d", &legendflag);
			break;
		    case LEGLOC:
			sscanf(s, "%*s %lf %lf", &legx, &legy);
			break;
		    case LINES:
			sscanf(s, "%*s %le %le %le %le %d %d %d %d", &lines[nlines].x1,
			       &lines[nlines].y1, &lines[nlines].x2, &lines[nlines].y2,
			       &lines[nlines].color, &lines[nlines].style, &lines[nlines].arrow,
			       &lines[nlines].active);
			nlines++;
			break;
		    case NBOXES:
			sscanf(s, "%*s %d", &btmp);
			break;
		    case NLINES:
			sscanf(s, "%*s %d", &ltmp);
			break;
		    case NSETS:
			sscanf(s, "%*s %d", &mtmp);
			break;
		    case NSTRINGS:
			sscanf(s, "%*s %d", &stmp);
			break;
		    case SETPROPS:
			sscanf(s, "%*s %d %d %d %d", &itmp, &ls, &ps, &cs);
			setlinesym(itmp, ls);
			setplotsym(itmp, ps);
			setplotcolor(itmp, cs);
			break;
		    case STRING:
			sscanf(s, "%*s %lf %lf %lf %d %d",
			     &pstr[nstr].x, &pstr[nstr].y, &pstr[nstr].size,
			       &pstr[nstr].rot, &pstr[nstr].font);
			break;
		    case STRINGDEF:
			strcpy(pstr[nstr].s, s + 10);
			pstr[nstr].s[strlen(pstr[nstr].s) - 1] = 0;
			nstr++;
			break;
		    case SUBTITLE:
			strcpy(stitle, s + 9);
			stitle[strlen(stitle) - 1] = 0;
			break;
		    case TICS:
			sscanf(s, "%*s %lf %lf %lf %lf", &xt1, &xt2, &yt1, &yt2);
			break;
		    case TICSABS:
			sscanf(s, "%*s %d %d", &xabsflag, &yabsflag);
			break;
		    case TICSFLAG:
			sscanf(s, "%*s %d %d", &xticflag, &yticflag);
			break;
		    case TICSGRID:
			sscanf(s, "%*s %d %d", &xgridflag, &ygridflag);
			break;
		    case TICSINT:
			sscanf(s, "%*s %d %d", &xticsintflag, &yticsintflag);
			break;
		    case TICSINOUT:
			sscanf(s, "%*s %d %d", &xticinoutflag, &yticinoutflag);
			break;
		    case TICSLFLAG:
			sscanf(s, "%*s %d %d", &xticlflag, &yticlflag);
			break;
		    case TICSLOG:
			sscanf(s, "%*s %d %d", &xticslog, &yticslog);
			break;
		    case XTICANG:
			sscanf(s, "%*s %lf", &xticangle);
			break;
		    case TICSOP:
			sscanf(s, "%*s %d %d", &xticopflag, &yticopflag);
			break;
		    case TICSLTOP:
			sscanf(s, "%*s %d %d", &xtopflag, &ytopflag);
			break;
		    case TICSLTYPE:
			sscanf(s, "%*s %d %d", &fformx, &fformy);
			break;
		    case TICSPREC:
			sscanf(s, "%*s %d %d", &xform, &yform);
			break;
		    case TITLE:
			strcpy(title, s + 6);
			title[strlen(title) - 1] = 0;
			break;
		    case VIEW:
			sscanf(s, "%*s %lf %lf %lf %lf", &xv1, &xv2, &yv1, &yv2);
			break;
		    case WORLD:
			sscanf(s, "%*s %lf %lf %lf %lf", &xg1, &xg2, &yg1, &yg2);
			break;
		    case XLABEL:
			strcpy(xlabel, s + 7);
			xlabel[strlen(xlabel) - 1] = 0;
			break;
		    case YLABEL:
			strcpy(ylabel, s + 7);
			ylabel[strlen(ylabel) - 1] = 0;
			break;
		    case ZEROAXIS:
			sscanf(s, "%*s %d %d", &xzflag, &yzflag);
			break;
		    case ZEROTICS:
			sscanf(s, "%*s %d %d", &xztflag, &yztflag);
			break;
		    }
		}
	    }
	}
    }
}
