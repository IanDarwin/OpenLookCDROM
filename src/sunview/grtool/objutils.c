/*
	operations on obects (lines and boxes)

	$Header: objutils.c,v 1.2 89/08/13 22:24:47 pturner Locked $
*/

#include <math.h>
#include "globals.h"
#include "defines.h"

#define FIGURE 1
#define BOX 2
#define LINE 3
#define POLYLINE 4
#define POLYGON 5

int nboxes = 0;

struct boxstruct {
    double x1, y1, x2, y2;
    int color;
    int style;
    int active;
} boxes[MAXBOXES];

int nlines = 0;

struct linestruct {
    double x1, y1, x2, y2;
    int color;
    int style;
    int arrow;
    int active;
} lines[MAXLINES];

void find_item(x, y, type, numb)
    double x, y;
    int *type, *numb;
{
    int i;
    double tmp, m = 1e308, dx = xg2 - xg1, dy = yg2 - yg1;

    *type = -1;
    for (i = 0; i < MAXBOXES; i++) {
	if (isactive_box(i)) {
	    tmp = hypot((x - boxes[i].x1) / dx, (y - boxes[i].y1) / dy);
	    if (m > tmp) {
		*type = BOX;
		*numb = i;
		m = tmp;
	    }
	    tmp = hypot((x - boxes[i].x1) / dx, (y - boxes[i].y2) / dy);
	    if (m > tmp) {
		*type = BOX;
		*numb = i;
		m = tmp;
	    }
	    tmp = hypot((x - boxes[i].x2) / dx, (y - boxes[i].y1) / dy);
	    if (m > tmp) {
		*type = BOX;
		*numb = i;
		m = tmp;
	    }
	    tmp = hypot((x - boxes[i].x2) / dx, (y - boxes[i].y2) / dy);
	    if (m > tmp) {
		*type = BOX;
		*numb = i;
		m = tmp;
	    }
	}
    }
    for (i = 0; i < MAXLINES; i++) {
	if (isactive_line(i)) {
	    tmp = hypot((x - lines[i].x1) / dx, (y - lines[i].y1) / dy);
	    if (m > tmp) {
		*type = LINE;
		*numb = i;
		m = tmp;
	    }
	    tmp = hypot((x - lines[i].x2) / dx, (y - lines[i].y2) / dy);
	    if (m > tmp) {
		*type = LINE;
		*numb = i;
		m = tmp;
	    }
	}
    }
}

int isactive_line(lineno)
    int lineno;
{
    if (0 <= lineno && lineno < MAXLINES)
	return (lines[lineno].active);
    return (0);
}

int isactive_box(boxno)
    int boxno;
{
    if (0 <= boxno && boxno < MAXBOXES)
	return (boxes[boxno].active);
    return (0);
}

int next_line()
{
    int i;

    for (i = 0; i < MAXLINES; i++) {
	if (!isactive_line(i)) {
	    lines[i].active = TRUE;
	    nlines++;
	    return (i);
	}
    }
    errwin("Error - no lines available");
    return (-1);
}

int next_box()
{
    int i;

    for (i = 0; i < MAXBOXES; i++) {
	if (!isactive_box(i)) {
	    boxes[i].active = TRUE;
	    nboxes++;
	    return (i);
	}
    }
    errwin("Error - no boxes available");
    return (-1);
}

kill_box(boxno)
    int boxno;
{
    if (isactive_box(boxno)) {
	boxes[boxno].active = FALSE;
	nboxes--;
    }
}

kill_line(lineno)
    int lineno;
{
    if (isactive_line(lineno)) {
	lines[lineno].active = FALSE;
	nlines--;
    }
}
