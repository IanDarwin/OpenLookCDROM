/*
 *
 * Global variables for gr
 *
 * $Header: globals.h,v 1.8 89/09/12 05:11:03 pturner Locked $
 */

/* global variables */

extern char *open_err_msg;

extern int inwin;	/* true if running sunview */

extern int maxarr, maxplot;
extern double xg1, xg2, yg1, yg2;	/* world coordinates */
extern double xv1, xv2, yv1, yv2;	/* viewpoint coordinates */
extern double xt1, xt2, yt1, yt2;

extern char xlabel[];
extern char ylabel[];
extern char title[];
extern char stitle[];

extern char plfile[], psfile[];
extern char resfile[];

extern int xform, yform, device, tdevice, hdevice;
extern int fitflag, ideg;
extern int hardcopyflag;

extern int repflag;		/* toggle graphics cursor */

extern int novice;
extern int inplotter;

extern int xticsintflag;
extern int yticsintflag;
extern int boxflag;
extern int boxon;
extern int xticflag;
extern int yticflag;
extern int fformx;
extern int fformy;
extern int xticlflag;
extern int yticlflag;
extern int xgridflag;
extern int ygridflag;		/* tics grid */
extern int xticslog;
extern int yticslog;
extern int logtransflag;
extern int xticinoutflag;
extern int yticinoutflag;
extern int xticopflag;
extern int yticopflag;
extern int xtopflag;
extern int ytopflag;
extern int xticangle;
extern int xabsflag;
extern int yabsflag;

extern int xzflag;
extern int yzflag;
extern int xztflag;
extern int yztflag;

extern int defline;	/* default linestyle */
extern int curfont;	/* default font */

extern int legendflag;
extern int lgap,llen;
extern double legx, legy;

extern double errbarper;

extern char fname[];
extern int nsets;
