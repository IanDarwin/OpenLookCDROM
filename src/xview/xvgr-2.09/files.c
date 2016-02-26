/* $Id: files.c,v 1.18 92/07/22 08:01:43 pturner Exp Locker: pturner $
 *
 * read data files
 *
 */

#include <stdio.h>
#include "globals.h"

int realtime = 0;
int change_gno;			/* if the graph number changes on read in */
static int cur_gno;		/* if the graph number changes on read in */
int change_type;		/* current set type */
static int cur_type;		/* current set type */

#define MAX_LINE_LEN 512

/*
 * number of doubles to allocate for each call to realloc
 */
#define BUFSIZE  512

static int readerror = 0;

int getdata(gno, fn, src, type)
    int gno;
    char *fn;
    int src, type;
{
    FILE *fp;
    int retval;

    switch (src) {
    case DISK:
	fp = fopen(fn, "r");
	break;
    case PIPE:
	fp = popen(fn, "r");
	break;
    case 2:
	fp = stdin;
	break;
    }
    if (fp == NULL) {
	sprintf(buf, "Can't open file %s", fn);
	errwin(buf);
	return 0;
    }
    cur_gno = gno;
    change_type = cur_type = type;
    retval = -1;
    while (retval == -1) {
	retval = 0;
	switch (cur_type) {
	case XY:
	    retval = readxy(cur_gno, fn, fp, 0);
	    break;
	case NXY:
	    retval = readnxy(cur_gno, fn, fp);
	    break;
	case IHL:
	    retval = readihl(cur_gno, fn, fp);
	    break;
	case BIN:
	    retval = readbinary(cur_gno, fn, fp);
	    break;
	case XYDX:
	case XYDY:
	case XYDXDX:
	case XYDYDY:
	case XYDXDY:
	case XYZ:
	case XYRT:
	case XYHILO:
	    retval = readxxyy(cur_gno, fn, fp, cur_type);
	    break;
	case XYSTRING:
	    retval = readxystring(cur_gno, fn, fp);
	    break;
	case 5:
	    retval = readblockdata(cur_gno, fn, fp);
	    break;
	}
    }
    if (src == PIPE) {
	pclose(fp);
    } else {
	if (fp != stdin) {	/* leave stdin open */
	    fclose(fp);
	}
    }
    return retval;
}

int getdata_step(gno, fn, src, type)
    int gno;
    char *fn;
    int src, type;
{
    static FILE *fp;
    int retval;

    if (fp == NULL) {
	switch (src) {
	case DISK:
	    fp = fopen(fn, "r");
	    break;
	case PIPE:
	    fp = popen(fn, "r");
	    break;
	case 2:
	    fp = stdin;
	    break;
	case 3:
	    if (fp) {
		if (src == PIPE) {
		    pclose(fp);
		} else {
		    if (fp != stdin) {	/* leave stdin open */
			fclose(fp);
		    }
		}
	    }
	    fp = NULL;
	    return;
	    break;
	}
    }
    if (fp == NULL) {
	sprintf(buf, "Can't open file %s", fn);
	errwin(buf);
	fp = NULL;
	return 0;
    }
    cur_gno = gno;
    change_type = cur_type = type;
    retval = -1;
    while (retval == -1) {
	retval = 0;
	switch (cur_type) {
	case XY:
	    retval = readxy(cur_gno, fn, fp, 1);
	    break;
	case NXY:
	    retval = readnxy(cur_gno, fn, fp);
	    break;
	case IHL:
	    retval = readihl(cur_gno, fn, fp);
	    break;
	case BIN:
	    retval = readbinary(cur_gno, fn, fp);
	    break;
	case XYDX:
	case XYDY:
	case XYDXDX:
	case XYDYDY:
	case XYDXDY:
	case XYZ:
	case XYRT:
	case XYHILO:
	    retval = readxxyy(cur_gno, fn, fp, cur_type);
	    break;
	case XYSTRING:
	    retval = readxystring(cur_gno, fn, fp);
	    break;
	case 5:
	    retval = readblockdata(cur_gno, fn, fp);
	    break;
	}
    }
    if (retval != -2) {		/* means it returned because a single set was
				 * read */
	if (src == PIPE) {
	    pclose(fp);
	} else {
	    if (fp != stdin) {	/* leave stdin open */
		fclose(fp);
	    }
	}
    }
    return retval;
}

/*
 * read file type 0
 */
int readxy(gno, fn, fp, readone)
    int gno;
    char *fn;
    FILE *fp;
    int readone;
{
    int i = 0, j, pstat, readset = 0, ptype, retval = 0;
    double *x, *y;

    x = (double *) calloc(BUFSIZE, sizeof(double));
    y = (double *) calloc(BUFSIZE, sizeof(double));
    if (x == NULL || y == NULL) {
	errwin("Insufficient memory for set");
	cxfree(x);
	cxfree(y);
	return (0);
    }
    while (fgets(buf, MAX_LINE_LEN, fp) != NULL) {
	if (buf[0] == '#') {
	    continue;
	}
	if (buf[0] == '@') {
	    change_gno = -1;
	    change_type = cur_type;
	    read_param(buf + 1);
	    if (change_gno >= 0) {
		cur_gno = gno = change_gno;
	    }
	    if (change_type != cur_type) {
		cur_type = change_type;
		retval = -1;
		break;		/* exit this module and store any set */
	    }
	    continue;
	}
	convertchar(buf);
	/* count the number of items scanned */
	if ((pstat = sscanf(buf, "%lf %lf", &x[i], &y[i])) >= 1) {
	    /* supply x if missing (y winds up in x) */
	    if (pstat == 1) {
		y[i] = x[i];
		x[i] = i;
	    }
	    if (realtime == 1 && inwin) {
		drawpolysym(&x[i], &y[i], 1, 3, 0, 0, 1.0);
	    }
	    /* got x and y so increment */
	    i++;
	    if (i % BUFSIZE == 0) {
		x = (double *) realloc(x, (i + BUFSIZE) * sizeof(double));
		y = (double *) realloc(y, (i + BUFSIZE) * sizeof(double));
	    }
	} else {
	    if (i != 0) {
		if ((j = nextset(gno)) == -1) {
		    cxfree(x);
		    cxfree(y);
		    return (readset);
		}
		activateset(gno, j);
		settype(gno, j, XY);
		setcol(gno, x, j, i, 0);
		setcol(gno, y, j, i, 1);
		setcomment(gno, j, fn);
		updatesetminmax(gno, j);
		if (realtime == 2 && inwin) {
		    drawsetxy(g[gno].p[j], j);
		}
		readset++;
	    } else {
		readerror++;
		if (readerror > 10) {
		    if (yesno("Lots of errors, abort?", "Press YES or NO", "YES", "NO")) {
			cxfree(x);
			cxfree(y);
			return (0);
		    } else {
			readerror = 0;
		    }
		}
	    }
	    i = 0;
	    x = (double *) calloc(BUFSIZE, sizeof(double));
	    y = (double *) calloc(BUFSIZE, sizeof(double));
	    if (x == NULL || y == NULL) {
		errwin("Insufficient memory for set");
		cxfree(x);
		cxfree(y);
		return (readset);
	    }
	    if (readone) {
		return (-2);
	    }
	}
    }
    if (i != 0) {
	if ((j = nextset(gno)) == -1) {
	    cxfree(x);
	    cxfree(y);
	    return (readset);
	}
	activateset(gno, j);
	settype(gno, j, XY);
	setcol(gno, x, j, i, 0);
	setcol(gno, y, j, i, 1);
	setcomment(gno, j, fn);
	updatesetminmax(gno, j);
	if (realtime == 2 && inwin) {
	    /*
	     * TODO ??? drawsetxy(g[gno].p[j]);
	     */
	}
	readset++;
    } else {
	cxfree(x);
	cxfree(y);
    }
    if (retval == -1) {
	return retval;
    } else {
	return readset;
    }
}

/*
 * read IHL format
 */
int readihl(gno, fn, fp)
    int gno;
    char *fn;
    FILE *fp;
{
    int i, j, pstat, npts;
    double *x, *y, tmp;

    i = 0;
    pstat = 0;
    if ((j = nextset(gno)) == -1) {
	return 0;
    }
    if (fgets(buf, MAX_LINE_LEN, fp) == NULL) {
	errwin("Can't read from file");
	killset(gno, j);
	return 0;
    }
    pstat = sscanf(buf, "%d", &npts);
    if (npts == 0) {
	errwin("Number of points = 0");
	killset(gno, j);
	return 0;
    }
    activateset(gno, j);
    settype(gno, j, XY);
    setlength(gno, j, npts);
    setcomment(gno, j, fn);
    x = getx(gno, j);
    y = gety(gno, j);
    for (i = 0; i < npts; i++) {
	if (fgets(buf, MAX_LINE_LEN, fp) == NULL) {
	    errwin("Premature EOF");
	    updatesetminmax(gno, j);
	    return 1;
	}
	convertchar(buf);
	pstat = sscanf(buf, "%lf %lf %lf", &tmp, &x[i], &y[i]);
    }
    updatesetminmax(gno, j);
    return 1;
}

/*
 * read x1 y1 y2 ... y30 formatted files
 * note that the maximum number of sets is 30
 */
#define MAXSETN 30

int readnxy(gno, fn, fp)
    int gno;
    char *fn;
    FILE *fp;
{
    int i, j, pstat, rcnt, cnt, scnt[MAXSETN], setn[MAXSETN], ptype, retval = 0;
    double atof(), *x[MAXSETN], *y[MAXSETN], xval, yr[MAXSETN];
    char *s, *strtok(), buf[1024], tmpbuf[1024];
    int readerror = 0;
    int do_restart = 0;

/* if more than one set of nxy data is in the file,
 * leap to here after each is read - the goto is at the
 * bottom of this module.
 */
restart:;

    i = 0;
    pstat = 0;
    cnt = 0;
    while ((fgets(buf, MAX_LINE_LEN, fp) != NULL) && ((buf[0] == '#') || (buf[0] == '@'))) {
	if (buf[0] == '@') {
	    read_param(buf + 1);
	}
    }
    convertchar(buf);

    /*
     * count the columns
     */
    strcpy(tmpbuf, buf);
    s = tmpbuf;
    while ((s = strtok(s, " \t\n")) != NULL) {
	cnt++;
	s = NULL;
    }
    if (cnt > MAXPLOT) {
	errwin("Maximum number of columns exceeded, reading first 31");
	cnt = 31;
    }
    s = buf;
    s = strtok(s, " \t\n");
    if (s == NULL) {
	errwin("Read ended by a blank line at or near the beginning of file");
	return 0;
    }
    pstat = sscanf(s, "%lf", &xval);
    if (pstat == 0) {
	errwin("Read ended, non-numeric found on line at or near beginning of file");
	return 0;
    }
    s = NULL;
    for (j = 0; j < cnt - 1; j++) {
	s = strtok(s, " \t\n");
	if (s == NULL) {
	    yr[j] = 0.0;
	    errwin("Number of items in column incorrect");
	} else {
	    yr[j] = atof(s);
	}
	s = NULL;
    }
    if (cnt > 1) {
	for (i = 0; i < cnt - 1; i++) {
	    if ((setn[i] = nextset(gno)) == -1) {
		for (j = 0; j < i; j++) {
		    killset(gno, setn[j]);
		}
		return 0;
	    }
	    activateset(gno, setn[i]);
	    settype(gno, setn[i], XY);
	    x[i] = (double *) calloc(BUFSIZE, sizeof(double));
	    y[i] = (double *) calloc(BUFSIZE, sizeof(double));
	    if (x[i] == NULL || y[i] == NULL) {
		errwin("Insufficient memory for set");
		cxfree(x[i]);
		cxfree(y[i]);
		for (j = 0; j < i + 1; j++) {
		    killset(gno, setn[j]);
		}
		return (0);
	    }
	    *(x[i]) = xval;
	    *(y[i]) = yr[i];
	    scnt[i] = 1;
	}
	while (!do_restart && (fgets(buf, MAX_LINE_LEN, fp) != NULL)) {
	    if (buf[0] == '#') {
		continue;
	    }
	    if (buf[0] == '@') {
		change_gno = -1;
		change_type = cur_type;
		read_param(buf + 1);
		if (change_gno >= 0) {
		    cur_gno = gno = change_gno;
		}
		if (change_type != cur_type) {
		    cur_type = change_type;
		    retval = -1;
		    break;	/* exit this module and store any set */
		}
		continue;
	    }
	    convertchar(buf);
	    s = buf;
	    s = strtok(s, " \t\n");
	    if (s == NULL) {
		continue;
	    }
/* check for set separator */
	    pstat = sscanf(s, "%lf", &xval);
	    if (pstat == 0) {
		do_restart = 1;
		continue;
	    } else {
		s = NULL;
		for (j = 0; j < cnt - 1; j++) {
		    s = strtok(s, " \t\n");
		    if (s == NULL) {
			yr[j] = 0.0;
			errwin("Number of items in column incorrect");
		    } else {
			yr[j] = atof(s);
		    }
		    s = NULL;
		}
		for (i = 0; i < cnt - 1; i++) {
		    *(x[i] + scnt[i]) = xval;
		    *(y[i] + scnt[i]) = yr[i];
		    scnt[i]++;
		    if (scnt[i] % BUFSIZE == 0) {
			x[i] = (double *) realloc(x[i], (scnt[i] + BUFSIZE) * sizeof(double));
			y[i] = (double *) realloc(y[i], (scnt[i] + BUFSIZE) * sizeof(double));
		    }
		}
	    }
	}
	for (i = 0; i < cnt - 1; i++) {
	    setcol(gno, x[i], setn[i], scnt[i], 0);
	    setcol(gno, y[i], setn[i], scnt[i], 1);
	    setcomment(gno, setn[i], fn);
	    updatesetminmax(gno, setn[i]);
	}
	if (!do_restart) {
	    if (retval == -1) {
		return retval;
	    } else {
		return 1;
	    }
	} else {
	    do_restart = 0;
	    goto restart;
	}
    }
    return 0;
}

int readbinary(gno, fn, fp)
    int gno;
    char *fn;
    FILE *fp;
{
    int i, type, setn, npts, n;
    double *x, *y;

/*
    fread(&type, sizeof(int), 1, fp);
*/
    fread(&npts, sizeof(int), 1, fp);
    for (i = 0; i < npts; i++) {
	fread(&n, sizeof(int), 1, fp);
	x = (double *) calloc(n, sizeof(double));
	if (x == NULL) {
	    errwin("Can't calloc in readbinary");
	    return 0;
	}
	y = (double *) calloc(n, sizeof(double));
	if (y == NULL) {
	    errwin("Can't calloc in readbinary");
	    cxfree(x);
	    return 0;
	}
	fread(x, sizeof(double), n, fp);
	fread(y, sizeof(double), n, fp);
	if ((setn = nextset(gno)) == -1) {
	    cxfree(x);
	    cxfree(y);
	    return 0;
	}
	activateset(gno, setn);
	settype(gno, setn, XY);
	setcol(gno, x, setn, n, 0);
	setcol(gno, y, setn, n, 1);
	setcomment(gno, setn, fn);
	updatesetminmax(gno, setn);
    }
    return 1;
}

int readxystring()
{
}

/*
 * read file types using dx and/or dy
 */
int readxxyy(gno, fn, fp, type)
    int gno;
    char *fn;
    FILE *fp;
    int type;
{
    int i = 0, j, pstat, readset = 0, ptype, retval = 0;
    double *x, *y, *dx, *dy, *dz;
    double xtmp, ytmp, dxtmp, dytmp, dztmp;

    x = y = dx = dy = dz = NULL;
    x = (double *) calloc(BUFSIZE, sizeof(double));
    y = (double *) calloc(BUFSIZE, sizeof(double));
    switch (type) {
    case XYZ:
    case XYRT:
    case XYDX:
    case XYDY:
	dx = (double *) calloc(BUFSIZE, sizeof(double));
	break;
    case XYDXDX:
    case XYDYDY:
    case XYDXDY:
	dx = (double *) calloc(BUFSIZE, sizeof(double));
	dy = (double *) calloc(BUFSIZE, sizeof(double));
	break;
    case XYHILO:
	dx = (double *) calloc(BUFSIZE, sizeof(double));
	dy = (double *) calloc(BUFSIZE, sizeof(double));
	dz = (double *) calloc(BUFSIZE, sizeof(double));
	break;
    default:
	dx = (double *) calloc(BUFSIZE, sizeof(double));
	dy = (double *) calloc(BUFSIZE, sizeof(double));
	break;
    }
    if (x == NULL || y == NULL) {
	errwin("Insufficient memory for set");
	cxfree(x);
	cxfree(y);
	cxfree(dx);
	cxfree(dy);
	cxfree(dz);
	return (0);
    }
    while (fgets(buf, MAX_LINE_LEN, fp) != NULL) {
	if (buf[0] == '#') {
	    continue;
	}
	if (buf[0] == '@') {
	    change_gno = -1;
	    change_type = cur_type;
	    read_param(buf + 1);
	    if (change_gno >= 0) {
		cur_gno = gno = change_gno;
	    }
	    if (change_type != cur_type) {
		if (change_type != cur_type) {
		    cur_type = change_type;
		    retval = -1;
		    break;	/* exit this module and store any set */
		}
	    }
	    continue;
	}
	convertchar(buf);
	/* count the number of items scanned */
	if ((pstat = sscanf(buf, "%lf %lf %lf %lf %lf", &xtmp, &ytmp, &dxtmp, &dytmp, &dztmp)) >= 1) {
	    /* got x and y so increment */
	    x[i] = xtmp;
	    y[i] = ytmp;
	    if (type == XYDX || type == XYDY || type == XYZ || type == XYRT) {
		dx[i] = dxtmp;
	    } else if (type == XYHILO) {
		dx[i] = dxtmp;
		dy[i] = dytmp;
		dz[i] = dztmp;
	    } else {
		dx[i] = dxtmp;
		dy[i] = dytmp;
	    }
	    i++;
	    if (i % BUFSIZE == 0) {
		x = (double *) realloc(x, (i + BUFSIZE) * sizeof(double));
		y = (double *) realloc(y, (i + BUFSIZE) * sizeof(double));
		switch (type) {
		case XYDX:
		case XYDY:
		case XYZ:
		case XYRT:
		    dx = (double *) realloc(dx, (i + BUFSIZE) * sizeof(double));
		    break;
		case XYDXDX:
		case XYDYDY:
		case XYDXDY:
		    dx = (double *) realloc(dx, (i + BUFSIZE) * sizeof(double));
		    dy = (double *) realloc(dy, (i + BUFSIZE) * sizeof(double));
		    break;
		case XYHILO:
		    dx = (double *) realloc(dx, (i + BUFSIZE) * sizeof(double));
		    dy = (double *) realloc(dy, (i + BUFSIZE) * sizeof(double));
		    dz = (double *) realloc(dz, (i + BUFSIZE) * sizeof(double));
		    break;
		default:
		    dx = (double *) realloc(dx, (i + BUFSIZE) * sizeof(double));
		    dy = (double *) realloc(dy, (i + BUFSIZE) * sizeof(double));
		    break;
		}
	    }
	} else {
	    if (i != 0) {
		if ((j = nextset(gno)) == -1) {
		    cxfree(x);
		    cxfree(y);
		    cxfree(dx);
		    cxfree(dy);
		    cxfree(dz);
		    return readset;
		}
		activateset(gno, j);
		settype(gno, j, type);
		setcol(gno, x, j, i, 0);
		setcol(gno, y, j, i, 1);
		setcol(gno, dx, j, i, 2);
		setcol(gno, dy, j, i, 3);
		setcol(gno, dz, j, i, 4);
		setcomment(gno, j, fn);
		updatesetminmax(gno, j);
		readset++;
	    } else {
		readerror++;
		if (readerror > 10) {
		    if (yesno("Lots of errors, abort?", "Press YES or NO", "YES", "NO")) {
			cxfree(x);
			cxfree(y);
			cxfree(dx);
			cxfree(dy);
			cxfree(dz);
			return (0);
		    } else {
			readerror = 0;
		    }
		}
	    }
	    i = 0;
	    x = (double *) calloc(BUFSIZE, sizeof(double));
	    y = (double *) calloc(BUFSIZE, sizeof(double));
	    switch (type) {
	    case XYDX:
	    case XYZ:
	    case XYRT:
	    case XYDY:
		dx = (double *) calloc(BUFSIZE, sizeof(double));
		break;
	    case XYDXDX:
	    case XYDYDY:
	    case XYDXDY:
		dx = (double *) calloc(BUFSIZE, sizeof(double));
		dy = (double *) calloc(BUFSIZE, sizeof(double));
		break;
	    case XYHILO:
		dx = (double *) calloc(BUFSIZE, sizeof(double));
		dy = (double *) calloc(BUFSIZE, sizeof(double));
		dz = (double *) calloc(BUFSIZE, sizeof(double));
		break;
	    default:
		dx = (double *) calloc(BUFSIZE, sizeof(double));
		dy = (double *) calloc(BUFSIZE, sizeof(double));
		break;
	    }
	    if (x == NULL || y == NULL) {
		errwin("Insufficient memory for set");
		cxfree(x);
		cxfree(y);
		cxfree(dx);
		cxfree(dy);
		cxfree(dz);
		killset(gno, j);
		return (readset);
	    }
	}
    }
    if (i != 0) {
	if ((j = nextset(gno)) == -1) {
	    cxfree(x);
	    cxfree(y);
	    cxfree(dx);
	    cxfree(dy);
	    cxfree(dz);
	    return readset;
	}
	activateset(gno, j);
	settype(gno, j, type);
	setcol(gno, x, j, i, 0);
	setcol(gno, y, j, i, 1);
	setcol(gno, dx, j, i, 2);
	setcol(gno, dy, j, i, 3);
	setcol(gno, dz, j, i, 4);
	setcomment(gno, j, fn);
	updatesetminmax(gno, j);
	readset++;
    } else {
	cxfree(x);
	cxfree(y);
	cxfree(dx);
	cxfree(dy);
	cxfree(dz);
    }
    if (retval == -1) {
	return retval;
    } else {
	return readset;
    }
}

double *blockdata[30];
int blocklen;
int blockncols;

/*
 * read block data
 */
int readblockdata(gno, fn, fp)
    int gno;
    char *fn;
    FILE *fp;
{
    int i = 0, j, k, gotcol, ncols, pstat, readset = 0, ptype, retval = 0;
    int first = 1, readerror = 0;
    double *data[30], atof();
    char tmpbuf[1024], *s, *strtok();

    i = 0;
    pstat = 0;
    while ((s = fgets(buf, MAX_LINE_LEN, fp)) != NULL) {
	if (buf[0] == '#') {
	    continue;
	}
	if (buf[0] == '@') {
	    read_param(buf + 1);
	    continue;
	}
	if (strlen(buf) > 1) {
	    convertchar(buf);
	    if (first) {	/* count the number of columns */
		ncols = 0;
		strcpy(tmpbuf, buf);
		s = tmpbuf;
		while (*s == ' ' || *s == '\t' || *s == '\n')
		    s++;
		while ((s = strtok(s, " \t\n")) != NULL) {
		    ncols++;
		    s = NULL;
		}
		if (ncols < 1 || ncols > 30) {
		    errwin("Column count incorrect");
		    return 0;
		}
		for (j = 0; j < 30; j++) {
		    cxfree(blockdata[j]);
		    blockdata[j] = (double *) NULL;
		}
		for (j = 0; j < ncols; j++) {
		    data[j] = (double *) calloc(BUFSIZE, sizeof(double));
		    if (data[j] == NULL) {
			errwin("Insufficient memory for block data");
			for (k = 0; k < j; k++) {
			    cxfree(data[k]);
			}
			return 0;
		    }
		}
		first = 0;
	    }
	    s = buf;
	    while (*s == ' ' || *s == '\t' || *s == '\n')
		s++;
	    for (j = 0; j < ncols; j++) {
		s = strtok(s, " \t\n");
		if (s == NULL) {
		    data[j][i] = 0.0;
		    errwin("Number of items in column incorrect");
		    readerror++;
		    if (readerror > 10) {
			if (yesno("Lots of errors, abort?", "Press YES or NO", "YES", "NO")) {
			    for (k = 0; k < ncols; k++) {
				cxfree(data[k]);
			    }
			    return (0);
			} else {
			    readerror = 0;
			}
		    }
		} else {
		    data[j][i] = atof(s);
		}
		s = NULL;
	    }
	    i++;
	    if (i % BUFSIZE == 0) {
		for (j = 0; j < ncols; j++) {
		    data[j] = (double *) realloc(data[j], (i + BUFSIZE) * sizeof(double));
		    if (data[j] == NULL) {
			errwin("Insufficient memory for block data");
			for (k = 0; k < j; k++) {
			    cxfree(data[k]);
			}
			return 0;
		    }
		}
	    }
	}
    }
    for (j = 0; j < ncols; j++) {
	blockdata[j] = data[j];
    }
    blocklen = i;
    blockncols = ncols;
    return 1;
}
