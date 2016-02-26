/*
	read data

	$Header: files.c,v 1.5 89/08/27 11:55:48 pturner Locked $
*/

#include <stdio.h>

extern char fname[];
extern int maxarr;
double *getx(), *gety();

static int readerror = 0;
extern int errno;
extern char *sys_errlist[];

int getdata(fn, type)
    char *fn;
    int type;
{
    int i, j, pstat;
    char buf[132];
    double *x, *y;
    FILE *fp;

    i = 0;
    pstat = 0;
    switch (type) {
    case 0:
	fp = fopen(fn, "r");
	break;
    case 1:
	return readxy(fn);
	break;
    case 2:
	fp = popen(fn, "r");
	break;
    case 3:
	return readihl(fn);
	break;
    }
    if (fp != NULL) {
	if ((j = nextset()) == -1) {
	    return (0);
	}
	x = getx(j);
	y = gety(j);
	while (fgets(buf, 132, fp) != NULL) {
	    convertchar(buf, ',', ' ');
	    convertchar(buf, 'D', 'e');
	    pstat = sscanf(buf, "%lf %lf", &x[i], &y[i]);
	    i++;
	    if ((pstat == 0) || (i == maxarr)) {
		if (i == maxarr) {
		    setlength(j, i);
		} else
		    setlength(j, i - 1);
		activateset(j);
		if (getsetlength(j)) {
		    setcomment(j, fn);
		    updatesetminmax(j);
		} else {
		    killset(j);
		    readerror++;
		    if (readerror > 10) {
			if (yesno("Lots of errors, abort?","Press YES or NO","YES","NO")) {
			    fclose(fp);
			    strcpy(fname, fn);
			    return (0);
			} else
			    readerror = 0;
		    }
		}
		i = 0;
		if ((j = nextset()) == -1) {
		    return (0);
		}
		x = getx(j);
		y = gety(j);
	    }
	}
	setlength(j, i);
	activateset(j);
	if (getsetlength(j)) {
	    setcomment(j, fn);
	    updatesetminmax(j);
	} else {
	    killset(j);
	}
	fclose(fp);
	strcpy(fname, fn);
	return (1);
    }
    fname[0] = '\0';
    return (0);
}

int readihl(fn)
    char *fn;
{
    int i, j, pstat, npts, itmp;
    char buf[132];
    double *x, *y;
    FILE *fp;

    i = 0;
    pstat = 0;
    fp = fopen(fn, "r");
    if (fp != NULL) {
	if ((j = nextset()) == -1) {
	    return 0;
	}
	x = getx(j);
	y = gety(j);
	if (fgets(buf, 132, fp) == NULL) {
	    errwin("Can't read from file");
	    killset(j);
	    return 0;
	}
	pstat = sscanf(buf, "%d", &npts);
	for (i = 0; i < npts; i++) {
	    fgets(buf, 132, fp);
	    convertchar(buf, ',', ' ');
	    convertchar(buf, 'D', 'e');
	    pstat = sscanf(buf, "%d %lf %lf", &itmp, &x[i], &y[i]);
	}
	setlength(j, npts);
	activateset(j);
	setcomment(j, fn);
	updatesetminmax(j);
	fclose(fp);
	return 1;
    }
    return 0;
}

static char *format = "%lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf";

#define MAXSETN 10

int readxy(fn)
    char *fn;
{
    int i, j, pstat, rcnt, cnt, scnt[MAXSETN], setn[MAXSETN];
    char buf[256];
    double *x[MAXSETN], *y[MAXSETN], xval, yr[MAXSETN];
    FILE *fp;

    i = 0;
    pstat = 0;
    fp = fopen(fn, "r");
    if (fp == NULL) {
	return 0;
    }
    if (fgets(buf, 255, fp) != NULL) {
	convertchar(buf, ',', ' ');
	convertchar(buf, 'D', 'e');
	cnt = sscanf(buf, format, &xval, &yr[0], &yr[1], &yr[2],
		     &yr[3], &yr[4], &yr[5], &yr[6], &yr[7], &yr[8], &yr[9]);
    }
    if (cnt > 2) {
	for (i = 0; i < cnt - 1; i++) {
	    if ((setn[i] = nextset()) == -1) {
		for (j = 0; j < i; j++) {
		    killset(j);
		}
		return 0;
	    }
	    activateset(setn[i]);
	    x[i] = getx(setn[i]);
	    y[i] = gety(setn[i]);
	    *(x[i]) = xval;
	    *(y[i]) = yr[i];
	    scnt[i] = 1;
	}
	while (fgets(buf, 255, fp) != NULL) {
	    convertchar(buf, ',', ' ');
	    convertchar(buf, 'D', 'e');
	    rcnt = sscanf(buf, format, &xval, &yr[0], &yr[1], &yr[2],
		    &yr[3], &yr[4], &yr[5], &yr[6], &yr[7], &yr[8], &yr[9]);
	    for (i = 0; i < cnt - 1; i++) {
		*(x[i] + scnt[i]) = xval;
		*(y[i] + scnt[i]) = yr[i];
		scnt[i]++;
	    }
	}
	for (i = 0; i < cnt - 1; i++) {
	    setlength(setn[i], scnt[i]);
	    setcomment(setn[i], fn);
	    updatesetminmax(setn[i]);
	}
	fclose(fp);
	return 1;
    }
    return 0;
}
