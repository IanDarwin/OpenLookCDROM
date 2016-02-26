/*
	definitions of objects

	$Header: objdefs.h,v 1.2 89/08/24 22:27:27 pturner Locked $
*/
#define FIGURE 1
#define BOX 2
#define LINE 3
#define POLYLINE 4
#define POLYGON 5

extern int nlines, nboxes;

typedef struct {
    double x1, y1, x2, y2;
    int color;
    int style;
    int active;
} boxtype;

extern boxtype boxes[];

typedef struct {
    double x1, y1, x2, y2;
    int color;
    int style;
    int arrow;
    int active;
} linetype;

extern linetype lines[];

#define MAXSTRLEN 140

typedef struct {
    char s[MAXSTRLEN+1];
    double x, y;
    double size;
    int rot;
    int font;
} plotstr;

