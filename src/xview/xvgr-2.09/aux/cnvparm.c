/* $Id$
 *
 * Convert 1.0x parameter files to 2.xx
 * Doesn't do everything, but should help.
 *
 */
#include <stdio.h>

void main(argc, argv)
    int argc;
    char **argv;
{
    int d = 0;
    if (argc < 2) {
	fprintf(stderr, "Usage: %s [-d] [version 1.0x parameter file]\n", argv[0]);
	fprintf(stderr, "Where -d indicates that the file is a data file with imbedded parameter settings\n");
	exit(1);
    }
    if (!strcmp(argv[1], "-d")) {
	d++;
	argv++;
    }
    getparms(argv[1], d);
    exit(0);
}

void errwin(s)
    char *s;
{
    fprintf(stderr, "%s\n", s);
}

#define	BOX0    1
#define	BOXCOL  52
#define	BOXLIN  53
#define	BOXTYPE 2
#define	BOXWID  57
#define	DEFCHAR 48
#define	DEFCOLOR 3
#define	DEFFONT 4
#define	DEFLINE 5
#define	DEFNTICS 55
#define	DEFSYMSIZE 54
#define	ERRBAR	6
#define	ERRBARPER 7
#define	ERRBARTYPE 8
#define	GRAPHHID  9
#define	GRAPHLAB  10
#define	GRAPHNO  11
#define	GRAPHTYPE  12
#define	LABDEF  47
#define	LEGSTR  13
#define	LEGFLAG 14
#define	LEGGAP  15
#define	LEGLEN  16
#define	LEGLOC  17
#define	LINES   18
#define	NBOXES  19
#define	NLINES  20
#define	NSETS   21
#define	NSTRINGS 22
#define	SETPROPS 23
#define	STRING0   24
#define STRINGDEF 25
#define	SUBTITLE 26
#define	TICS     27
#define	TICSABS  28
#define	TICSFLAG 29
#define	TICSGRID 30
#define	TICSINOUT  31
#define	TICSINT    32
#define	TICSLFLAG  33
#define	TICSLOG    34
#define	TICSLTOP   35
#define	TICSLTYPE  36
#define	TICSOP   37
#define	TICSPREC   38
#define	TICSSIZE   56
#define	TITLE      39
#define	VIEW       40
#define	WORLD      41
#define	XLABEL     42
#define	XTICANG     43
#define	XTICSKIP     50
#define	YLABEL     44
#define	YTICANG     49
#define	YTICSKIP     51
#define	ZEROAXIS   45
#define	ZEROTICS   46

#define MAXPAR 57

static char cstr[] = "##grtool parameter file";	/* the standard version */

struct funcs {
    char *s;
    int type;
};

int findf(key, s, tlen)
    struct funcs key[];
char *s;
int tlen;

{
    int low, high, mid;

    low = 0;
    high = tlen - 1;
    while (low <= high) {
	mid = (low + high) / 2;
	if (strcmp(s, key[mid].s) < 0) {
	    high = mid - 1;
	} else {
	    if (strcmp(s, key[mid].s) > 0) {
		low = mid + 1;
	    } else {
		return (mid);
	    }
	}
    }
    return (-1);
}

char readbuf[512];
int nleg, nstr;
int curset = 0;			/* for legends */

getparms(plfile, d)
    char plfile[];

{
    int icheck, ptype;
    char s[256];
    FILE *pp;

    if ((pp = fopen(plfile, "r")) == NULL) {
	sprintf(readbuf, "Can't open parameter file %s", plfile);
	errwin(readbuf);
	plfile[0] = 0;
    } else {
	fgets(s, 255, pp);
	s[strlen(s) - 1] = 0;
	icheck = strcmp(s, cstr);
	if (icheck) {
	    sprintf(readbuf, "File %s not legal parameter file", plfile);
	    errwin(readbuf);
	    fclose(pp);
	    return;
	}
	while (fgets(readbuf, 511, pp) != NULL) {
	    if (d) {
		if (readbuf[0] = '@') {
		    ptype = read_param(readbuf);
		} else {
		    puts(readbuf);
		}
	    } else {
		ptype = read_param(readbuf);
	    }
	}
	fclose(pp);
    }
}

struct funcs parmnames[] = {
			    "box", BOX0,
			    "boxcolor", BOXCOL,
			    "boxlinestyle", BOXLIN,
			    "boxlinewidth", BOXWID,
			    "boxtypes", BOXTYPE,
			    "defchar", DEFCHAR,
			    "defcolor", DEFCOLOR,
			    "deffont", DEFFONT,
			    "defline", DEFLINE,
			    "defntics", DEFNTICS,
			    "defsymsize", DEFSYMSIZE,
			    "errorbar", ERRBAR,
			    "errorbarper", ERRBARPER,
			    "errorbartype", ERRBARTYPE,
			    "graphhid", GRAPHHID,
			    "graphlab", GRAPHLAB,
			    "graphno", GRAPHNO,
			    "graphtype", GRAPHTYPE,
			    "labdef", LABDEF,
			    "legendflag", LEGFLAG,
			    "legendstr", LEGSTR,
			    "leggap", LEGGAP,
			    "leglen", LEGLEN,
			    "legloc", LEGLOC,
			    "line", LINES,
			    "nboxes", NBOXES,
			    "nlines", NLINES,
			    "nsets", NSETS,
			    "nstrings", NSTRINGS,
			    "setprops", SETPROPS,
			    "string", STRING0,
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
			    "ticssize", TICSSIZE,
			    "title", TITLE,
			    "view", VIEW,
			    "world", WORLD,
			    "xlabel", XLABEL,
			    "xticang", XTICANG,
			    "xticskip", XTICSKIP,
			    "ylabel", YLABEL,
			    "yticang", YTICANG,
			    "yticskip", YTICSKIP,
			    "zeroflag", ZEROAXIS,
			    "zerotics", ZEROTICS
};

int read_param(s)
    char *s;
{
    int didit = 0, gnum, glab, ghid;
    int i, itmp, itmp1, itmp2, type, mtmp, stmp, ltmp, btmp, ls, cs, ps;
    char name[256], title[256], xlabel[256], ylabel[256];
    double dtmp, x1, x2, y1, y2;
    int color, style, active, loctype;
    int xabsflag;
    int yabsflag;
    int xticflag;
    int yticflag;
    int xgridflag;
    int ygridflag;
    int xticsintflag;
    int yticsintflag;
    int xticinoutflag;
    int yticinoutflag;
    int xticlflag;
    int yticlflag;
    int xticslog;
    int yticslog;
    int xticangle;
    int yticangle;
    int xticlskip;
    int yticlskip;
    int xticopflag;
    int yticopflag;
    int xtopflag;
    int ytopflag;
    int ix;
    int iy;
    int fformx;
    int fformy;
    int xzflag;
    int yzflag;
    int xztflag;
    int yztflag;


    if (s[0] == '@') {
	s++;
    }
    if (s[0] != '#') {
	sscanf(s, "%s", name);
	type = findf(parmnames, name, MAXPAR);
	if (type >= 0) {
	    type = parmnames[type].type;
	    /* printf("%s %d\n",name,type); */
	    switch (type) {
	    case BOX0:
		sscanf(s, "%*s %le %le %le %le %d %d %d %d", &x1, &y1, &x2, &y2, &color, &style, &active, &loctype);
		printf("with box\n");
		printf("box on\n");
		printf("box loctype %s\n", loctype ? "VIEW" : "WORLD");
		if (loctype == 0) {
		    printf("box g0\n");
		}
		printf("box %lf, %lf, %lf, %lf\n", x1, y1, x2, y2);
		printf("box linestyle %d\n", style);
		printf("box linewidth %d\n", color);
		printf("box def\n");
		break;
	    case BOXCOL:
		sscanf(s, "%*s %d", &itmp);
		printf("frame linewidth %d\n", itmp);
		break;
	    case BOXLIN:
		sscanf(s, "%*s %d", &itmp);
		printf("frame linestyle %d\n", itmp);
		break;
	    case BOXWID:
		sscanf(s, "%*s %d", &itmp);
		printf("frame linewidth %d\n", itmp);
		break;
	    case BOXTYPE:
		sscanf(s, "%*s %d %d", &itmp1, &itmp2);
		printf("frame %s\n", itmp1 ? "on" : "off");
		printf("frame type %d\n", !itmp2);
		break;
	    case DEFNTICS:
		sscanf(s, "%*s %d %d", &itmp1, &itmp2);
		printf("xaxis tick default %d\n", itmp1);
		printf("yaxis tick default %d\n", itmp2);
		break;
	    case DEFSYMSIZE:
		sscanf(s, "%*s %lf", &dtmp);
		for (i = 0; i < 30; i++) {
		    printf("s%1d symbol size %lf\n", i, dtmp);
		}
		break;
	    case ERRBAR:
		sscanf(s, "%*s %d %d", &stmp, &itmp);
/*
		makeseterrbar(stmp, itmp);
*/
		break;
	    case ERRBARPER:
		sscanf(s, "%*s %lf", &dtmp);
		break;
	    case ERRBARTYPE:
		sscanf(s, "%*s %d %d", &stmp, &itmp);
/*
		seterrbarxy(stmp, itmp);
*/
		break;
	    case LABDEF:
		sscanf(s, "%*s %d", &itmp);
		break;
	    case LEGSTR:
		strcpy(name, s + 10);
		name[strlen(name) - 1] = 0;
		if (strlen(name)) {
		    printf("legend string %d \"%s\"\n", curset++, name);
		}
		break;
	    case LEGFLAG:
		sscanf(s, "%*s %d", &itmp);
		printf("legend %s\n", itmp ? "ON" : "OFF");
		break;
	    case LEGGAP:
		sscanf(s, "%*s %d", &itmp);
		printf("legend vgap %d\n", itmp);
		break;
	    case LEGLEN:
		sscanf(s, "%*s %d", &itmp);
		printf("legend length %d\n", itmp);
		break;
	    case LEGLOC:
		{
		    int ncheck;

		    ncheck = sscanf(s, "%*s %lf %lf %d", &x1, &y1, &itmp);
		    printf("legend %lf, %lf\n", x1, y1);
		    if (ncheck == 2) {
		    } else {
			printf("legend loctype %s\n", itmp ? "VIEW" : "WORLD");
		    }
		}
		break;
	    case LINES:
		{
		    int ncheck, arrow;
		    double asize;

		    ncheck = sscanf(s, "%*s %le %le %le %le %d %d %d %d %d %le", &x1, &y1, &x2, &y2,
				    &color, &style, &arrow,
				    &active, &loctype, &asize);
		    if (ncheck == 9) {
			asize = 1.0;
		    }
		    printf("with line\n");
		    printf("line on\n");
		    printf("line loctype %s\n", loctype ? "VIEW" : "WORLD");
		    if (loctype == 0) {
			printf("line g0\n");
		    }
		    printf("line %lf, %lf, %lf, %lf\n", x1, y1, x2, y2);
		    printf("line linestyle %d\n", style + 1);
		    printf("line linewidth %d\n", color);
		    printf("line arrow %d\n", arrow);
		    printf("line arrow size %lf\n", asize);
		    printf("line def\n");
		    break;
		}
	    case SETPROPS:
		{
		    int ls, ps, cs;

		    sscanf(s, "%*s %d %d %d %d", &itmp, &ls, &ps, &cs);
		    printf("s%1d color %d\n", itmp, cs);
		    printf("s%1d linestyle %d\n", itmp, ls);
		    printf("s%1d symbol %d\n", itmp, ps);
		}
		break;
	    case STRING0:
		if (nstr >= 0) {
		    double size, rot;
		    int font, just;

		    sscanf(s, "%*s %lf %lf %lf %d %d %d %d &d",
		     &x1, &y1, &size, &rot, &font, &color, &loctype, &just);
		    printf("with string\n");
		    printf("string on\n");
		    printf("string loctype %s\n", loctype ? "VIEW" : "WORLD");
		    if (loctype == 0) {
			printf("string g0\n");
		    }
		    printf("string %lf, %lf\n", x1, y1);
		    printf("string color %d\n", color);
		}
		break;
	    case STRINGDEF:
		if (nstr >= 0) {
		    strcpy(name, s + 10);
		    name[strlen(name) - 1] = 0;
		    printf("string def \"%s\"\n", name);
		}
		break;
	    case SUBTITLE:
		strcpy(title, s + 9);
		title[strlen(title) - 1] = 0;
		printf("subtitle \"%s\"\n", title);
		break;
	    case TICS:
		sscanf(s, "%*s %lf %lf %lf %lf", &x1, &x2, &y1, &y2);
		printf("xaxis tick major %lf\n", x1);
		printf("xaxis tick minor %lf\n", x2);
		printf("yaxis tick major %lf\n", y1);
		printf("yaxis tick minor %lf\n", y2);
		break;
	    case TICSABS:
		sscanf(s, "%*s %d %d", &xabsflag, &yabsflag);
		printf("xaxis ticklabel sign normal\n");
		printf("yaxis ticklabel sign normal\n");
		break;
	    case TICSFLAG:
		sscanf(s, "%*s %d %d", &xticflag, &yticflag);
		printf("xaxis tick %s\n", xticflag ? "on" : "off");
		printf("yaxis tick %s\n", yticflag ? "on" : "off");
		break;
	    case TICSGRID:
		sscanf(s, "%*s %d %d", &xgridflag, &ygridflag);
		printf("xaxis tick major grid %s\n", xgridflag ? "on" : "off");
		printf("yaxis tick major grid %s\n", ygridflag ? "on" : "off");
		break;
	    case TICSINT:
		sscanf(s, "%*s %d %d", &xticsintflag, &yticsintflag);
		break;
	    case TICSINOUT:
		sscanf(s, "%*s %d %d", &xticinoutflag, &yticinoutflag);
		break;
	    case TICSLFLAG:
		sscanf(s, "%*s %d %d", &xticlflag, &yticlflag);
		printf("xaxis ticklabel %s\n", xticflag ? "on" : "off");
		printf("yaxis ticklabel %s\n", yticflag ? "on" : "off");
		break;
	    case TICSLOG:
		sscanf(s, "%*s %d %d", &xticslog, &yticslog);
		break;
	    case XTICANG:
		sscanf(s, "%*s %d", &xticangle);
		break;
	    case YTICANG:
		sscanf(s, "%*s %d", &yticangle);
		break;
	    case XTICSKIP:
		sscanf(s, "%*s %d", &xticlskip);
		break;
	    case YTICSKIP:
		sscanf(s, "%*s %d", &yticlskip);
		break;
	    case TICSOP:
		sscanf(s, "%*s %d %d", &xticopflag, &yticopflag);
		break;
	    case TICSLTOP:
		sscanf(s, "%*s %d %d", &xtopflag, &ytopflag);
		sscanf(s, "%*s %d %d", &ix, &iy);
		break;
	    case TICSLTYPE:
		sscanf(s, "%*s %d %d", &fformx, &fformy);
		sscanf(s, "%*s %d %d", &ix, &iy);
		break;
	    case TICSPREC:
		sscanf(s, "%*s %d %d", &ix, &iy);
		printf("xaxis ticklabel prec %d\n", ix);
		printf("yaxis ticklabel prec %d\n", iy);
		break;
	    case TICSSIZE:
		sscanf(s, "%*s %lf %lf", &x1, &y1);
		printf("xaxis tick size %lf\n", x1);
		printf("yaxis tick size %lf\n", x1);
		break;
	    case TITLE:
		strcpy(title, s + 6);
		title[strlen(title) - 1] = 0;
		printf("title \"%s\"\n", title);
		break;
	    case VIEW:
		sscanf(s, "%*s %lf %lf %lf %lf", &x1, &x2, &y1, &y2);
		printf("view xmin %lf\n", x1);
		printf("view xmax %lf\n", x2);
		printf("view ymin %lf\n", y1);
		printf("view ymax %lf\n", y2);
		break;
	    case WORLD:
		sscanf(s, "%*s %lf %lf %lf %lf", &x1, &x2, &y1, &y2);
		printf("world xmin %lf\n", x1);
		printf("world xmax %lf\n", x2);
		printf("world ymin %lf\n", y1);
		printf("world ymax %lf\n", y2);
		break;
	    case XLABEL:
		strcpy(xlabel, s + 7);
		xlabel[strlen(xlabel) - 1] = 0;
		printf("xaxis label \"%s\"\n", xlabel);
		break;
	    case YLABEL:
		strcpy(ylabel, s + 7);
		ylabel[strlen(ylabel) - 1] = 0;
		printf("yaxis label \"%s\"\n", ylabel);
		break;
	    case ZEROAXIS:
		sscanf(s, "%*s %d %d", &xzflag, &yzflag);
		printf("zeroxaxis bar %s\n", xzflag ? "on" : "off");
		printf("zeroyaxis bar %s\n", yzflag ? "on" : "off");
		break;
	    case ZEROTICS:
		sscanf(s, "%*s %d %d", &xztflag, &yztflag);
		printf("zeroxaxis tick %s\n", xztflag ? "on" : "off");
		printf("zeroyaxis tick %s\n", yztflag ? "on" : "off");
		break;
	    }			/* end switch */
	    return (type);	/* type found */
	}			/* end if */
    }				/* end if */
}
