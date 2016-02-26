
# line 2 "pars.yacc"

/* $Id: pars.c,v 1.1 1992/07/17 14:04:33 pturner Exp pturner $
 * 
 * evaluate expressions, commands, parameter files
 * 
 */

#define PARS			/* to overide some defines in defines.h */

#include <stdio.h>
#include <math.h>
#include <ctype.h>

#include "globals.h"

#ifndef M_PI
#     define M_PI  3.14159265358979323846
#endif

#ifndef TRUE
#     define TRUE 1
#endif

#ifndef FALSE
#     define FALSE 0
#endif

double result, resx, resy;	/* return value if expression */
double nonl_parms[10];

double drand48();
long lrand48();

double rnorm(), fx(), normp(), invnorm(), invt();
void yyerror();

static int interr;

static double *freelist[100]; 	/* temporary vectors */
static int fcnt;		/* number allocated */

int naxis = 0;	/* current axis */
int curline, curbox, curstring, curleg, curgrid;

int gotbatch, gotparams, gotread; /* these guys attempt to avoid reentrancy problems */
int readtype, readsrc;
char batchfile[256], paramfile[256], readfile[256];

static char f_string[512];	/* buffer for string to parse */
static int pos = 0;
static double *aa, *bb, *cc, *dd, *xx, *yy;
static int setindex, lxy, ls;
static int setsetno;
static int whichgraph;
static int whichset;

extern int change_gno;
extern int change_type;

/* may add these later TODO
*/


# line 66 "pars.yacc"
typedef union  {
    double val;
    int ival;
    double *ptr;
    int func;
    int pset;
    char *str;
} YYSTYPE;
# define ABS 257
# define ACOS 258
# define ASIN 259
# define ATAN 260
# define ATAN2 261
# define CEIL 262
# define COS 263
# define DEG 264
# define DX 265
# define DY 266
# define ERF 267
# define ERFC 268
# define EXP 269
# define FLOOR 270
# define HYPOT 271
# define INDEX 272
# define INT 273
# define INVN 274
# define INVT 275
# define IRAND 276
# define LGAMMA 277
# define LN 278
# define LOG 279
# define LOGISTIC 280
# define MAXP 281
# define MINP 282
# define MOD 283
# define NORM 284
# define NORMP 285
# define PI 286
# define RAD 287
# define RAND 288
# define RNORM 289
# define SETNO 290
# define SIN 291
# define SQR 292
# define SQRT 293
# define TAN 294
# define INUM 295
# define VX1 296
# define VX2 297
# define VY1 298
# define VY2 299
# define WX1 300
# define WX2 301
# define WY1 302
# define WY2 303
# define DELAYP 304
# define DOUBLEBUFFER 305
# define DOWN 306
# define ABSOLUTE 307
# define ABORT 308
# define ACTIVATE 309
# define ACTIVE 310
# define ALT 311
# define ALTERNATE 312
# define ALTXAXIS 313
# define ALTYAXIS 314
# define ANGLE 315
# define ANNOTATE 316
# define APPEND 317
# define AREA 318
# define ARROW 319
# define AUTO 320
# define AUTOSCALE 321
# define AUTOTICKS 322
# define AVG 323
# define AXIS 324
# define AXES 325
# define BACKBUFFER 326
# define BACKGROUND 327
# define BAR 328
# define BATCH 329
# define BIN 330
# define BOTH 331
# define BOTTOM 332
# define BOX 333
# define CELLS 334
# define CENTER 335
# define CHAR 336
# define CHRSTR 337
# define CLEAR 338
# define CLICK 339
# define CMAP 340
# define COLOR 341
# define COMMENT 342
# define COPY 343
# define CYCLE 344
# define DECIMAL 345
# define DEF 346
# define DEFAULT 347
# define DELETE 348
# define DEVICE 349
# define DFT 350
# define DIFFERENCE 351
# define DISK 352
# define DRAW2 353
# define DXDX 354
# define DXP 355
# define DYDY 356
# define DYP 357
# define ECHO 358
# define EDIT 359
# define ELSE 360
# define END 361
# define ERRORBAR 362
# define EXIT 363
# define EXPONENTIAL 364
# define FALSEP 365
# define FFT 366
# define FILEP 367
# define FILL 368
# define FIND 369
# define FIXEDPOINT 370
# define FLUSH 371
# define FOCUS 372
# define FOLLOWS 373
# define FONTP 374
# define FOREGROUND 375
# define FORMAT 376
# define FRONTBUFFER 377
# define FRAMEP 378
# define GETP 379
# define GRAPH 380
# define GRAPHNO 381
# define GRAPHS 382
# define GRAPHTYPE 383
# define GRID 384
# define HARDCOPY 385
# define HBAR 386
# define HGAP 387
# define HIDDEN 388
# define HORIZONTAL 389
# define HPGLL 390
# define HPGLP 391
# define HISTO 392
# define IF 393
# define IHL 394
# define IN 395
# define INIT 396
# define INITGRAPHICS 397
# define INOUT 398
# define INTEGRATE 399
# define INTERP 400
# define INVDFT 401
# define INVFFT 402
# define JUST 403
# define KILL 404
# define LABEL 405
# define LAYOUT 406
# define LEAVE 407
# define LEAVEGRAPHICS 408
# define LEFT 409
# define LEGEND 410
# define LENGTH 411
# define LEVEL 412
# define LEVELS 413
# define LINE 414
# define LINESTYLE 415
# define LINETO 416
# define LINEWIDTH 417
# define LINK 418
# define LOAD 419
# define LOCATOR 420
# define LOCTYPE 421
# define LOGX 422
# define LOGY 423
# define LOGXY 424
# define MAJOR 425
# define MIFL 426
# define MIFP 427
# define MINOR 428
# define MISSINGP 429
# define MOVE 430
# define MOVE2 431
# define MOVETO 432
# define NEGATE 433
# define NO 434
# define NONE 435
# define NORMAL 436
# define NXY 437
# define OFF 438
# define OFFSETX 439
# define OFFSETY 440
# define ON 441
# define OP 442
# define ORIENT 443
# define OUT 444
# define PAGE 445
# define PARA 446
# define PARALLEL 447
# define PARAMETERS 448
# define PARAMS 449
# define PATTERN 450
# define PERIMETER 451
# define PERP 452
# define PERPENDICULAR 453
# define PIE 454
# define PIPE 455
# define PLACE 456
# define POINT 457
# define POLAR 458
# define POWER 459
# define PREC 460
# define PREPEND 461
# define PRINT 462
# define PS 463
# define PSCOLORP 464
# define PSMONOP 465
# define PSCOLORL 466
# define PSMONOL 467
# define PUSH 468
# define POP 469
# define PUTP 470
# define READ 471
# define REDRAW 472
# define REGRESS 473
# define REGNUM 474
# define REGIONS 475
# define RENDER 476
# define REVERSE 477
# define RIGHT 478
# define RISER 479
# define ROT 480
# define RUNAVG 481
# define RUNMED 482
# define RUNSTD 483
# define RUNMIN 484
# define RUNMAX 485
# define SAMPLE 486
# define SCALE 487
# define SCIENTIFIC 488
# define SET 489
# define SETNUM 490
# define SETS 491
# define SIGN 492
# define SIZE 493
# define SKIP 494
# define SLEEP 495
# define SLICE 496
# define SOURCE 497
# define SPEC 498
# define SPECIFIED 499
# define SPECTRUM 500
# define STACK 501
# define STACKEDBAR 502
# define STACKEDHBAR 503
# define STACKEDLINE 504
# define STAGGER 505
# define START 506
# define STARTTYPE 507
# define STATUS 508
# define STOP 509
# define STRING 510
# define SUBTITLE 511
# define SWAPBUFFER 512
# define SYMBOL 513
# define TICK 514
# define TICKLABEL 515
# define TICKMARKS 516
# define TITLE 517
# define TO 518
# define TOP 519
# define TRUEP 520
# define TYPE 521
# define UP 522
# define VELOCITY 523
# define VERTICAL 524
# define VGAP 525
# define VIEW 526
# define WITH 527
# define WORLD 528
# define WRITE 529
# define X1 530
# define X2 531
# define X3 532
# define X4 533
# define X5 534
# define XAXES 535
# define XAXIS 536
# define XCOR 537
# define XMAX 538
# define XMIN 539
# define XY 540
# define XYARC 541
# define XYBOX 542
# define XYFIXED 543
# define XYHILO 544
# define XYRT 545
# define XYSEG 546
# define XYSTRING 547
# define XYDX 548
# define XYDY 549
# define XYDXDX 550
# define XYDYDY 551
# define XYDXDY 552
# define XYX2Y2 553
# define XYXX 554
# define XYYY 555
# define XYZ 556
# define XYZW 557
# define Y1 558
# define Y2 559
# define Y3 560
# define Y4 561
# define Y5 562
# define YAXES 563
# define YAXIS 564
# define YES 565
# define YMAX 566
# define YMIN 567
# define ZEROXAXIS 568
# define ZEROYAXIS 569
# define ABOVE 570
# define BELOW 571
# define POLYI 572
# define POLYO 573
# define GENERAL 574
# define DDMMYY 575
# define MMDDYY 576
# define MMYY 577
# define MMDD 578
# define MONTHDAY 579
# define DAYMONTH 580
# define MONTHS 581
# define MONTHL 582
# define DAYOFWEEKS 583
# define DAYOFWEEKL 584
# define DAYOFYEAR 585
# define HMS 586
# define MMDDHMS 587
# define MMDDYYHMS 588
# define DEGREESLON 589
# define DEGREESMMLON 590
# define DEGREESMMSSLON 591
# define MMSSLON 592
# define DEGREESLAT 593
# define DEGREESMMLAT 594
# define DEGREESMMSSLAT 595
# define MMSSLAT 596
# define DOT 597
# define STAR 598
# define PLUS 599
# define CROSS 600
# define CIRCLE 601
# define SQUARE 602
# define DIAMOND 603
# define TRIANGLE1 604
# define TRIANGLE2 605
# define TRIANGLE3 606
# define TRIANGLE4 607
# define SVAR 608
# define VAR 609
# define X 610
# define Y 611
# define NUMBER 612
# define FITPARM 613
# define OR 614
# define AND 615
# define GT 616
# define LT 617
# define LE 618
# define GE 619
# define EQ 620
# define NE 621
# define UMINUS 622
# define NOT 623
#define yyclearin yychar = -1
#define yyerrok yyerrflag = 0
extern int yychar;
extern int yyerrflag;
#ifndef YYMAXDEPTH
#define YYMAXDEPTH 150
#endif
YYSTYPE yylval, yyval;
typedef int yytabelem;
# define YYERRCODE 256

# line 3396 "pars.yacc"


void fixupstr(val)
    char *val;
{
    int vl = strlen(val);
    lowtoupper(val);
    val[vl + 1] = 0;
    val[vl] = '\n';
}

void scanner(s, x, y, len, a, b, c, d, lenscr, i, setno, errpos)
    char s[];
    double *x, *y, *a, *b, *c, *d;
    int i, len, lenscr, setno, *errpos;

{
    interr = 0;
    whichgraph = cg;
    whichset = setno;
    if (s[0] == '#') {
	return;
    }
    pos = 0;
    aa = a;
    bb = b;
    cc = c;
    dd = d;
    xx = x;
    yy = y;
    lxy = len;
    ls = lenscr;
    setindex = i + 1;
    curset = setsetno = setno;
    strcpy(f_string, s);
    fcnt = 0;
    yyparse();
    *errpos = interr;
    for (i = 0; i < fcnt; i++) {
	cfree(freelist[i]);
	freelist[i] = NULL;
    }
}

void runbatch(bfile)
char *bfile;
{
    double x, y, a, b, c, d;
    int i, setno, errpos, lcnt = 1;
    char stext[256];
    FILE *fp;
    if (strcmp("stdin", bfile)) {
	fp = fopen(bfile, "r");
    }
    else {
	fp = stdin;
    }
    if (fp == NULL) {
        fprintf(stderr, "Error opening batch file \"%s\"\n", bfile);
        exit(1);
    }
    while(fgets(stext, 255, fp) != NULL) {
        if (stext[0] == '#') {
            continue;
        }
        lowtoupper(stext);
/* TODO check on 0, 0 here for index and setno */
        scanner(stext, &x, &y, 1, ax, bx, cx, dx, 1, 0, 0, &errpos);
        stext[0] = 0;
        if (gotparams && paramfile[0]) {
            if (!getparms(cg, paramfile)) {
            }
            gotparams = 0;
        } else if (gotread && readfile[0]) {
            if (getdata(cg, readfile, readsrc, readtype)) {
                drawgraph();
            }
            gotread = 0;
        }
    }
    if (fp != stdin) {
	fclose(fp);
    }
}

#define MAXFUN 369

int maxparms = MAXFUN;
int maxfunc = MAXFUN;

symtab_entry key[] = {
	"A", VAR,
	"A0", FITPARM,
	"A1", FITPARM,
	"A2", FITPARM,
	"A3", FITPARM,
	"A4", FITPARM,
	"A5", FITPARM,
	"A6", FITPARM,
	"A7", FITPARM,
	"A8", FITPARM,
	"A9", FITPARM,
	"ABORT", ABORT,
	"ABOVE", ABOVE,
	"ABS", ABS,
	"ABSOLUTE", ABSOLUTE,
	"ACOS", ACOS,
	"ACTIVATE", ACTIVATE,
	"ACTIVE", ACTIVE,
	"ALT", ALT,
	"ALTERNATE", ALTERNATE,
	"ALTXAXIS", ALTXAXIS,
	"ALTYAXIS", ALTYAXIS,
	"AND", AND,
	"ANGLE", ANGLE,
	"ANNOTATE", ANNOTATE,
	"APPEND", APPEND,
	"AREA", AREA,
	"ARROW", ARROW,
	"ASIN", ASIN,
	"ATAN", ATAN,
	"ATAN2", ATAN2,
	"AUTO", AUTO,
	"AUTOSCALE", AUTOSCALE,
	"AUTOTICKS", AUTOTICKS,
	"AVG", AVG,
	"AXES", AXES,
	"AXIS", AXIS,
	"B", VAR,
	"BACKBUFFER", BACKBUFFER,
	"BACKGROUND", BACKGROUND,
	"BAR", BAR,
	"BATCH", BATCH,
	"BELOW", BELOW,
	"BIN", BIN,
	"BOTH", BOTH,
	"BOTTOM", BOTTOM,
	"BOX", BOX,
	"C", VAR,
	"CEIL", CEIL,
	"CELLS", CELLS,
	"CENTER", CENTER,
	"CHAR", CHAR,
	"CHRSTR", CHRSTR,
	"CLEAR", CLEAR,
	"CLICK", CLICK,
	"CMAP", CMAP,
	"CO", COLOR,
	"COLOR", COLOR,
	"COMMENT", COMMENT,
	"COPY", COPY,
	"COS", COS,
	"CYCLE", CYCLE,
	"D", VAR,
	"DAYMONTH", DAYMONTH,
	"DAYOFWEEKL", DAYOFWEEKL,
	"DAYOFWEEKS", DAYOFWEEKS,
	"DAYOFYEAR", DAYOFYEAR,
	"DDMMYY", DDMMYY,
	"DECIMAL", DECIMAL,
	"DEF", DEF,
	"DEFAULT", DEFAULT,
	"DEG", DEG,
	"DEGREESLAT", DEGREESLAT,
	"DEGREESLON", DEGREESLON,
	"DEGREESMMLAT", DEGREESMMLAT,
	"DEGREESMMLON", DEGREESMMLON,
	"DEGREESMMSSLAT", DEGREESMMSSLAT,
	"DEGREESMMSSLON", DEGREESMMSSLON,
	"DELAY", DELAYP,
	"DELETE", DELETE,
	"DEVICE", DEVICE,
	"DFT", DFT,
	"DIFF", DIFFERENCE,
	"DIFFERENCE", DIFFERENCE,
	"DISK", DISK,
	"DOUBLEBUFFER", DOUBLEBUFFER,
	"DOWN", DOWN,
	"DRAW2", DRAW2,
	"DX", DX,
	"DXDX", DXDX,
	"DY", DY,
	"DYDY", DYDY,
	"ECHO", ECHO,
	"EDIT", EDIT,
	"ELSE", ELSE,
	"END", END,
	"EQ", EQ,
	"ER", ERRORBAR,
	"ERF", ERF,
	"ERFC", ERFC,
	"ERRORBAR", ERRORBAR,
	"EXIT", EXIT,
	"EXP", EXP,
	"EXPONENTIAL", EXPONENTIAL,
	"FALSE", FALSEP,
	"FFT", FFT,
	"FILE", FILEP,
	"FILL", FILL,
	"FIND", FIND,
	"FIXEDPOINT", FIXEDPOINT,
	"FLOOR", FLOOR,
	"FLUSH", FLUSH,
	"FOCUS", FOCUS,
	"FOLLOWS", FOLLOWS,
	"FONT", FONTP,
	"FOREGROUND", FOREGROUND,
	"FORMAT", FORMAT,
	"FRAME", FRAMEP,
	"FRONTBUFFER", FRONTBUFFER,
	"GE", GE,
	"GENERAL", GENERAL,
	"GETP", GETP,
	"GRAPH", GRAPH,
	"GRAPHS", GRAPHS,
	"GRAPHTYPE", GRAPHTYPE,
	"GRID", GRID,
	"GT", GT,
	"HARDCOPY", HARDCOPY,
	"HBAR", HBAR,
	"HGAP", HGAP,
	"HIDDEN", HIDDEN,
	"HISTO", HISTO,
	"HMS", HMS,
	"HORIZONTAL", HORIZONTAL,
	"HPGLL", HPGLL,
	"HPGLP", HPGLP,
	"HYPOT", HYPOT,
	"IF", IF,
	"IHL", IHL,
	"IN", IN,
	"INDEX", INDEX,
	"INIT", INIT,
	"INITGRAPHICS", INITGRAPHICS,
	"INOUT", INOUT,
	"INT", INT,
	"INTEGRATE", INT,
	"INTERP", INTERP,
	"INUM", INUM,
	"INVDFT", INVDFT,
	"INVFFT", INVFFT,
	"INVN", INVN,
	"INVT", INVT,
	"IRAND", IRAND,
	"JUST", JUST,
	"KILL", KILL,
	"LABEL", LABEL,
	"LAYOUT", LAYOUT,
	"LE", LE,
	"LEAVE", LEAVE,
	"LEAVEGRAPHICS", LEAVEGRAPHICS,
	"LEFT", LEFT,
	"LEGEND", LEGEND,
	"LENGTH", LENGTH,
	"LEVEL", LEVEL,
	"LEVELS", LEVELS,
	"LGAMMA", LGAMMA,
	"LINE", LINE,
	"LINESTYLE", LINESTYLE,
	"LINETO", LINETO,
	"LINEWIDTH", LINEWIDTH,
	"LINK", LINK,
	"LN", LN,
	"LOAD", LOAD,
	"LOCATOR", LOCATOR,
	"LOCTYPE", LOCTYPE,
	"LOG", LOG,
	"LOGISTIC", LOGISTIC,
	"LOGX", LOGX,
	"LOGXY", LOGXY,
	"LOGY", LOGY,
	"LS", LINESTYLE,
	"LT", LT,
	"LW", LINEWIDTH,
	"MAJOR", MAJOR,
	"MAX", MAXP,
	"MIFL", MIFL,
	"MIFP", MIFP,
	"MIN", MINP,
	"MINOR", MINOR,
	"MISSING", MISSINGP,
	"MMDD", MMDD,
	"MMDDHMS", MMDDHMS,
	"MMDDYY", MMDDYY,
	"MMDDYYHMS", MMDDYYHMS,
	"MMSSLAT", MMSSLAT,
	"MMSSLON", MMSSLON,
	"MMYY", MMYY,
	"MOD", MOD,
	"MONTHDAY", MONTHDAY,
	"MONTHL", MONTHL,
	"MONTHS", MONTHS,
	"MOVE", MOVE,
	"MOVE2", MOVE2,
	"MOVETO", MOVETO,
	"NE", NE,
	"NEGATE", NEGATE,
	"NO", NO,
	"NONE", NONE,
	"NORM", NORM,
	"NORMAL", NORMAL,
	"NORMP", NORMP,
	"NOT", NOT,
	"NUMBER", NUMBER,
	"NXY", NXY,
	"OFF", OFF,
	"OFFSETX", OFFSETX,
	"OFFSETY", OFFSETY,
	"ON", ON,
	"OP", OP,
	"OR", OR,
	"ORIENT", ORIENT,
	"OUT", OUT,
	"PAGE", PAGE,
	"PARA", PARA,
	"PARALLEL", PARALLEL,
	"PARAMETERS", PARAMETERS,
	"PARAMS", PARAMS,
	"PATTERN", PATTERN,
	"PERIMETER", PERIMETER,
	"PERP", PERP,
	"PERPENDICULAR", PERPENDICULAR,
	"PI", PI,
	"PIE", PIE,
	"PIPE", PIPE,
	"PLACE", PLACE,
	"POINT", POINT,
	"POLAR", POLAR,
	"POLYI", POLYI,
	"POLYO", POLYO,
	"POP", POP,
	"POWER", POWER,
	"PREC", PREC,
	"PREPEND", PREPEND,
	"PRINT", PRINT,
	"PS", PS,
	"PSCOLORL", PSCOLORL,
	"PSCOLORP", PSCOLORP,
	"PSMONOL", PSMONOL,
	"PSMONOP", PSMONOP,
	"PUSH", PUSH,
	"PUTP", PUTP,
	"RAD", RAD,
	"RAND", RAND,
	"READ", READ,
	"REDRAW", REDRAW,
	"REGRESS", REGRESS,
	"RENDER", RENDER,
	"REVERSE", REVERSE,
	"RIGHT", RIGHT,
	"RISER", RISER,
	"RNORM", RNORM,
	"ROT", ROT,
	"RUNAVG", RUNAVG,
	"RUNMAX", RUNMAX,
	"RUNMED", RUNMED,
	"RUNMIN", RUNMIN,
	"RUNSTD", RUNSTD,
	"SAMPLE", SAMPLE,
	"SCALE", SCALE,
	"SCIENTIFIC", SCIENTIFIC,
	"SET", SET,
	"SETNO", SETNO,
	"SETS", SETS,
	"SIGN", SIGN,
	"SIN", SIN,
	"SIZE", SIZE,
	"SKIP", SKIP,
	"SLEEP", SLEEP,
	"SLICE", SLICE,
	"SOURCE", SOURCE,
	"SPEC", SPEC,
	"SPECIFIED", SPECIFIED,
	"SPECTRUM", SPECTRUM,
	"SQR", SQR,
	"SQRT", SQRT,
	"STACK", STACK,
	"STACKEDBAR", STACKEDBAR,
	"STACKEDHBAR", STACKEDHBAR,
	"STACKEDLINE", STACKEDLINE,
	"STAGGER", STAGGER,
	"START", START,
	"STARTTYPE", STARTTYPE,
	"STATUS", STATUS,
	"STOP", STOP,
	"STRING", STRING,
	"SUBTITLE", SUBTITLE,
	"SVAR", SVAR,
	"SWAPBUFFER", SWAPBUFFER,
	"SYMBOL", SYMBOL,
	"TAN", TAN,
	"TICK", TICK,
	"TICKLABEL", TICKLABEL,
	"TICKMARKS", TICKMARKS,
	"TITLE", TITLE,
	"TO", TO,
	"TOP", TOP,
	"TRUE", TRUEP,
	"TYPE", TYPE,
	"UP", UP,
	"VAR", VAR,
	"VELOCITY", VELOCITY,
	"VERTICAL", VERTICAL,
	"VGAP", VGAP,
	"VIEW", VIEW,
	"VX1", VX1,
	"VX2", VX2,
	"VY1", VY1,
	"VY2", VY2,
	"WITH", WITH,
	"WORLD", WORLD,
	"WRITE", WRITE,
	"WX1", WX1,
	"WX2", WX2,
	"WY1", WY1,
	"WY2", WY2,
	"X", SVAR,
	"X1", X1,
	"X2", X2,
	"X3", X3,
	"X4", X4,
	"X5", X5,
	"XAXES", XAXES,
	"XAXIS", XAXIS,
	"XCOR", XCOR,
	"XMAX", XMAX,
	"XMIN", XMIN,
	"XY", XY,
	"XYARC", XYARC,
	"XYBOX", XYBOX,
	"XYDX", XYDX,
	"XYDXDX", XYDXDX,
	"XYDXDY", XYDXDY,
	"XYDY", XYDY,
	"XYDYDY", XYDYDY,
	"XYFIXED", XYFIXED,
	"XYHILO", XYHILO,
	"XYRT", XYRT,
	"XYSEG", XYSEG,
	"XYSTRING", XYSTRING,
	"XYX2Y2", XYX2Y2,
	"XYXX", XYXX,
	"XYYY", XYYY,
	"XYZ", XYZ,
	"XYZW", XYZW,
	"Y", SVAR,
	"Y1", Y1,
	"Y2", Y2,
	"Y3", Y3,
	"Y4", Y4,
	"Y5", Y5,
	"YAXES", YAXES,
	"YAXIS", YAXIS,
	"YES", YES,
	"YMAX", YMAX,
	"YMIN", YMIN,
	"ZEROXAXIS", ZEROXAXIS,
	"ZEROYAXIS", ZEROYAXIS,
};

int findf(key, s, tlen)
    symtab_entry key[];
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

int getcharstr()
{
    if (pos >= strlen(f_string))
	 return EOF;
    return (f_string[pos++]);
}

void ungetchstr()
{
    if (pos > 0)
	pos--;
}

int yylex()
{
    int c, i;
    int found;
    static char s[256];
    char sbuf[256];

    while ((c = getcharstr()) == ' ' || c == '\t');
    if (c == EOF) {
	return (0);
    }
    if (c == '"') {
	i = 0;
	while ((c = getcharstr()) != '"' && c != EOF) {
	    if (c == '\\') {
		int ctmp;
		ctmp = getcharstr();
		if (ctmp != '"') {
		    ungetchstr();
		}
		else {
		    c = ctmp;
		}
	    }
	    s[i] = c;
	    i++;
	}
	if (c == EOF) {
	    sprintf(sbuf, "Nonterminating string\n");
	    yyerror(sbuf);
	    return 0;
	}
	s[i] = '\0';
	yylval.str = s;
	return CHRSTR;
    }
    if (c == '.' || isdigit(c)) {
	char stmp[80];
	double d;
	int i, gotdot = 0;

	i = 0;
	while (c == '.' || isdigit(c)) {
	    if (c == '.') {
		if (gotdot) {
		    yyerror("Reading number, too many dots");
	    	    return 0;
		} else {
		    gotdot = 1;
		}
	    }
	    stmp[i++] = c;
	    c = getcharstr();
	}
	if (c == 'E' || c == 'e') {
	    stmp[i++] = c;
	    c = getcharstr();
	    if (c == '+' || c == '-') {
		stmp[i++] = c;
		c = getcharstr();
	    }
	    while (isdigit(c)) {
		stmp[i++] = c;
		c = getcharstr();
	    }
	}
	if (gotdot && i == 1) {
	    ungetchstr();
	    return '.';
	}
	stmp[i] = '\0';
	ungetchstr();
	sscanf(stmp, "%lf", &d);
	yylval.val = d;
	return NUMBER;
    }
/* graphs, sets, regions resp. */
    if (c == 'G' || c == 'S' || c == 'R') {
	char stmp[80];
	double d;
	int i = 0, ctmp = c, gn, sn, rn;
	c = getcharstr();
	while (isdigit(c)) {
	    stmp[i++] = c;
	    c = getcharstr();
	}
	if (i == 0) {
	    c = ctmp;
	    ungetchstr();
	} else {
	    ungetchstr();
	    if (ctmp == 'G') {
	        stmp[i] = '\0';
		gn = atoi(stmp);
		if (gn >= 0 && gn < maxgraph) {
		    yylval.ival = gn;
		    whichgraph = gn;
		    return GRAPHNO;
		}
	    } else if (ctmp == 'S') {
	        stmp[i] = '\0';
		sn = atoi(stmp);
		if (sn >= 0 && sn < g[cg].maxplot) {
		    lxy = getsetlength(cg, sn);
		    yylval.ival = sn;
		    whichset = sn;
		    return SETNUM;
		}
	    } else if (ctmp == 'R') {
	        stmp[i] = '\0';
		rn = atoi(stmp);
		if (rn >= 0 && rn < MAXREGION) {
		    yylval.ival = rn;
		    return REGNUM;
		}
	    }
	}
    }
    if (isalpha(c)) {
	char *p = sbuf;
	int gno = -1, setno = -1, xy = -1, elno = -1;

	do {
	    *p++ = c;
	} while ((c = getcharstr()) != EOF && isalnum(c));
	ungetchstr();
	*p = '\0';
        if (debuglevel == 2) {
	    printf("->%s<-\n", sbuf);
	}
	if ((found = findf(key, sbuf, MAXFUN)) >= 0) {
	    if (key[found].type == VAR) {
		switch (sbuf[0]) {
		case 'A':
		    yylval.ptr = aa;
		    return VAR;
		case 'B':
		    yylval.ptr = bb;
		    return VAR;
		case 'C':
		    yylval.ptr = cc;
		    return VAR;
		case 'D':
		    yylval.ptr = dd;
		    return VAR;
		}
	    }
	    else if (key[found].type == SVAR) {
		switch (sbuf[0]) {
		case 'X':
		    yylval.ptr = xx;
		    return SVAR;
		case 'Y':
		    yylval.ptr = yy;
		    return SVAR;
		}
	    }
	    else if (key[found].type == FITPARM) {
		int index = sbuf[1] - '0';
		yylval.val = nonl_parms[index];
		return FITPARM;
	    }
	    else { /* set up special cases */
		switch (key[found].type) {
		case XAXIS:
		    naxis = 0;
		    break;
		case YAXIS:
		    naxis = 1;
		    break;
		case ZEROXAXIS:
		    naxis = 2;
		    break;
		case ZEROYAXIS:
		    naxis = 3;
		    break;
		case ALTXAXIS:
		    naxis = 4;
		    break;
		case ALTYAXIS:
		    naxis = 5;
		    break;
		case AXES:
		    naxis = 6;
		    break;
		case XAXES:
		    naxis = 7;
		    break;
		case YAXES:
		    naxis = 8;
		    break;
		case GRAPHS:
		    yylval.ival = -1;
		    whichgraph = -1;
		    return GRAPHS;
		    break;
		case SETS:
		    yylval.ival = -1;
		    whichset = -1;
		    return SETS;
		    break;
		default:
		    break;
		}
	    }
	    yylval.func = key[found].type;
	    return key[found].type;
	} else {
	    strcat(sbuf, ": No such function or variable");
	    yyerror(sbuf);
	    return 0;
	}
    }
    switch (c) {
    case '>':
	return follow('=', GE, GT);
    case '<':
	return follow('=', LE, LT);
    case '=':
	return follow('=', EQ, '=');
    case '!':
	return follow('=', NE, NOT);
    case '|':
	return follow('|', OR, '|');
    case '&':
	return follow('&', AND, '&');
    case '\n':
	return '\n';
    default:
	return c;
    }
}

int follow(expect, ifyes, ifno)
{
    int c = getcharstr();

    if (c == expect) {
	return ifyes;
    }
    ungetchstr();
    return ifno;
}

void yyerror(s)
    char *s;
{
    int i;
    char buf[256];
    sprintf(buf, "Error: %s: %s", s, f_string);
    i = strlen(buf);
    buf[i - 1] = 0;
    errwin(buf);
    interr = 1;
}

#define C1 0.1978977093962766
#define C2 0.1352915131768107

double rnorm(mean, sdev)
    double mean, sdev;
{
    double u = drand48();

    return mean + sdev * (pow(u, C2) - pow(1.0 - u, C2)) / C1;
}

double fx(x)
    double x;
{
    return 1.0 / sqrt(2.0 * M_PI) * exp(-x * x * 0.5);
}

double normp(b, s)
    double b, *s;
{
    double sum, dx, a = -8.0, fx();
    int i, n = 48;

    sum = fx(a) + fx(b);
    dx = (b - a) / n;
    for (i = 1; i <= ((n - 1) / 2); i++)
	sum = sum + 4.0 * fx(a + (2.0 * i - 1.0) * dx) + 2.0 * fx(a + 2.0 * i * dx);
    sum = sum + 4.0 * fx(b - dx);
    *s = fx(b);
    return sum * dx / 3.0;
}

double invnorm(p)
    double p;
{
    double s, x, z, temp, normp();

    if (p > 0.5)
	x = 1.0 - p;
    else
	x = p;
    s = sqrt(-2.0 * log(x));
    x = ((-7.49101 * s - 448.047) * s - 1266.846);
    x = x / (((s + 109.8371) * s + 748.189) * s + 498.003) + s;
    if (p < 0.5)
	x = -x;
    z = p - normp(x, &s);
    z = z / s;
    s = x * x;
    return (((((((((720.0 * s + 2556.0) * s + 1740.0) * s + 127.0) * z / 7.0 +
		 ((120.0 * s + 326.0) * s + 127.0) * x) * z / 6.0 + (24 * s + 46.0) * s + 7.0) * z / 40.0 +
	       (0.75 * s + 0.875) * x) * z + s + 0.5) * z / 3.0 + x * 0.5) * z + 1.0) * z + x + 0.832e-24 * x;
}

double invt(p, n)
    double p;

    int n;
{
    double sign, temp, a, b, c, d, x, y;

    sign = 1.0;
    if (p < 0.5) {
	p = 1.0 - p;
	sign = -1.0;
    }
    p = (1 - p) * 2;
    if (n == 2) {
	temp = sqrt(2.0 / (p * (2.0 - p)) - 2.0);
	temp = sign * temp;
	return temp;
    } else if (n == 1) {
	p = p * M_PI / 2.0;
	return sign * cos(p) / sin(p);
    } else {
	a = 1.0 / (n - 0.5);
	b = 48.0 / (a * a);
	c = ((20700 * a / b - 98.0) * a - 16.0) * a + 96.36;
	d = ((94.5 / (b + c) - 3.0) / b + 1.0) * sqrt(a * M_PI / 2.0) * n;
	x = d * p;
	y = exp((2.0 / n) * log(x));
	if (y > (0.05 + a)) {
	    x = invnorm(p * 0.5);
	    y = x * x;
	    if (n < 5)
		c = c + 0.3 * (n - 4.5) * (x + 0.6);
	    c = (((0.05 * d * x - 5.0) * x - 7.0) * x - 2.0) * x + b + c;
	    y = (((((0.4 * y + 6.3) * y + 36.0) * y + 94.5) / c - y - 3.0) / b + 1.0) * x;
	    y = a * y * y;
	    if (y > 0.002)
		y = exp(y) - 1.0;
	    else
		y = 0.5 * y * y + y;
	} else
	    y = ((1.0 / (((n + 0.6) / (n * y) - 0.089 * d - 0.822) * (n + 2.0) * 3.0) + 0.5 / (n + 4.0)) * y - 1.0) * (n + 1.0) / (n + 2.0) + 1.0 / y;
	return sign * sqrt(n * y);
    }
}
yytabelem yyexca[] ={
-1, 1,
	0, -1,
	-2, 0,
-1, 28,
	37, 586,
	-2, 514,
-1, 29,
	37, 587,
	-2, 515,
-1, 30,
	37, 588,
	-2, 516,
-1, 44,
	37, 610,
	-2, 525,
-1, 45,
	37, 611,
	-2, 526,
-1, 59,
	37, 625,
	-2, 541,
-1, 60,
	37, 626,
	-2, 542,
-1, 61,
	37, 627,
	-2, 543,
-1, 244,
	10, 485,
	44, 485,
	41, 485,
	-2, 643,
-1, 254,
	10, 485,
	44, 485,
	41, 485,
	-2, 645,
-1, 521,
	10, 485,
	44, 485,
	41, 485,
	-2, 486,
-1, 523,
	10, 485,
	44, 485,
	41, 485,
	-2, 490,
-1, 525,
	37, 575,
	10, 485,
	44, 485,
	41, 485,
	-2, 494,
-1, 527,
	37, 576,
	10, 485,
	44, 485,
	41, 485,
	-2, 498,
-1, 529,
	37, 578,
	10, 485,
	44, 485,
	41, 485,
	-2, 502,
-1, 532,
	616, 0,
	617, 0,
	618, 0,
	619, 0,
	620, 0,
	621, 0,
	-2, 635,
-1, 533,
	616, 0,
	617, 0,
	618, 0,
	619, 0,
	620, 0,
	621, 0,
	-2, 636,
-1, 534,
	616, 0,
	617, 0,
	618, 0,
	619, 0,
	620, 0,
	621, 0,
	-2, 637,
-1, 535,
	616, 0,
	617, 0,
	618, 0,
	619, 0,
	620, 0,
	621, 0,
	-2, 638,
-1, 536,
	616, 0,
	617, 0,
	618, 0,
	619, 0,
	620, 0,
	621, 0,
	-2, 639,
-1, 537,
	616, 0,
	617, 0,
	618, 0,
	619, 0,
	620, 0,
	621, 0,
	-2, 640,
-1, 541,
	10, 485,
	44, 485,
	41, 485,
	-2, 489,
-1, 543,
	10, 485,
	44, 485,
	41, 485,
	-2, 493,
-1, 545,
	10, 485,
	44, 485,
	41, 485,
	-2, 497,
-1, 547,
	10, 485,
	44, 485,
	41, 485,
	-2, 501,
-1, 548,
	10, 485,
	44, 485,
	41, 485,
	-2, 504,
-1, 550,
	616, 0,
	617, 0,
	618, 0,
	619, 0,
	620, 0,
	621, 0,
	-2, 552,
-1, 552,
	616, 0,
	617, 0,
	618, 0,
	619, 0,
	620, 0,
	621, 0,
	-2, 553,
-1, 553,
	616, 0,
	617, 0,
	618, 0,
	619, 0,
	620, 0,
	621, 0,
	-2, 554,
-1, 554,
	616, 0,
	617, 0,
	618, 0,
	619, 0,
	620, 0,
	621, 0,
	-2, 555,
-1, 555,
	616, 0,
	617, 0,
	618, 0,
	619, 0,
	620, 0,
	621, 0,
	-2, 556,
-1, 556,
	616, 0,
	617, 0,
	618, 0,
	619, 0,
	620, 0,
	621, 0,
	-2, 557,
-1, 971,
	37, 579,
	-2, 506,
-1, 1002,
	37, 615,
	-2, 531,
-1, 1311,
	37, 593,
	-2, 524,
-1, 1312,
	37, 614,
	-2, 529,
-1, 1325,
	37, 628,
	-2, 547,
	};
# define YYNPROD 646
# define YYLAST 6495
yytabelem yyact[]={

   551,     5,   933,   897,  1138,   270,   856,  1160,   157,  1390,
   833,  1378,  1347,   714,   712,  1400,   713,  1184,   715,   336,
   783,   767,  1342,  1329,  1302,  1294,   157,  1293,  1292,  1289,
  1282,   714,   712,  1398,   713,   157,   715,  1281,  1280,  1279,
   714,   712,  1396,   713,  1277,   715,  1276,   169,   167,   768,
   168,   157,   170,  1275,  1274,  1273,   714,   712,  1260,   713,
  1258,   715,  1257,  1256,  1234,   716,  1233,   157,  1211,   244,
   252,   254,   714,   712,  1205,   713,  1204,   715,  1202,  1201,
  1199,  1198,  1197,   716,  1195,  1194,   273,   274,   157,  1193,
  1192,   277,   716,   714,   712,  1382,   713,  1191,   715,   171,
  1190,   926,   928,   335,  1189,  1180,  1179,  1178,   716,   642,
     6,  1176,   343,   396,   405,  1175,   420,   426,   568,  1165,
   157,  1164,   463,  1389,   716,   714,   712,  1381,   713,  1159,
   715,  1151,  1146,  1137,   925,   782,  1136,  1135,  1133,  1112,
   766,  1110,   157,  1108,  1107,   716,  1376,   714,   712,  1106,
   713,  1102,   715,   521,   523,   525,   527,   529,   531,   532,
   533,   534,   535,   536,   537,   538,   539,   934,   541,   543,
   545,   547,   548,  1101,  1098,  1097,  1096,   716,   243,   251,
   253,  1095,  1093,  1092,  1076,  1059,  1058,   559,   561,   562,
   564,   565,  1037,   966,   965,  1185,   963,   950,   157,   716,
   949,   584,   585,   714,   712,  1374,   713,   948,   715,   613,
   614,   947,   946,   617,   620,   622,   624,   626,   628,   630,
   632,   634,   636,   638,   640,   643,   645,   646,   936,   650,
   652,   654,   656,   658,   660,   662,   664,   666,   668,   670,
   672,   674,   676,   677,   930,   570,   569,   896,   157,   895,
   829,   828,  1375,   714,   712,   716,   713,   827,   715,   927,
   929,   826,   522,   524,   526,   528,   530,   825,   824,   823,
   822,   898,   821,   572,   571,   811,   935,   540,   542,   544,
   546,   549,   550,   552,   553,   554,   555,   556,   557,   558,
   899,   810,   809,   808,   803,   802,   801,   560,   157,   563,
   800,   799,   798,   714,   712,   716,   713,   797,   715,  1186,
   796,   795,   794,   793,   792,  1060,   791,   780,   779,   294,
   778,   775,   618,   619,   621,   623,   625,   627,   629,   631,
   633,   635,   637,   639,   774,   644,   647,   709,   649,   651,
   653,   655,   657,   659,   661,   663,   665,   667,   669,   671,
   673,   675,   773,   932,  1373,   716,   772,   771,   157,   765,
   764,   763,   722,   714,   712,   721,   713,   720,   715,   157,
   700,   512,   695,   694,   714,   712,  1371,   713,   924,   715,
   693,   692,   689,   687,   648,   900,   612,   611,   193,   203,
   610,   509,   758,   759,   760,   157,   276,   568,   275,   568,
   714,   712,  1370,   713,   258,   715,   157,  1339,  1327,  1226,
  1219,   714,   712,  1369,   713,   716,   715,  1036,   615,   719,
   272,   718,   786,   787,   788,   789,   716,  1324,   169,   167,
  1254,   168,  1253,   170,   698,   696,   690,  1188,   931,  1156,
   272,  1177,   951,   499,   820,   784,   781,   292,   293,   776,
   934,   805,   716,   602,  1380,   937,  1379,  1042,   289,   940,
  1041,   939,  1040,   716,  1288,   818,   819,  1039,   287,   705,
   704,   703,   702,   303,   305,  1286,   606,   701,   255,   310,
   171,   616,   157,   341,   394,   403,  1368,   714,   712,   894,
   713,   285,   715,   456,   472,   484,  1149,   486,   942,  1144,
   901,   902,   903,   904,   905,   906,   907,   908,   909,   910,
   911,   912,   913,   914,   915,   916,   917,   918,   919,   920,
   921,   922,   923,   941,   570,   569,   570,   569,  1394,   960,
   202,   269,  1331,   201,  1330,  1038,   157,   312,  1153,   716,
  1366,   714,   712,   707,   713,   607,   715,   202,  1158,   959,
   201,   510,   572,   571,   572,   571,   290,   291,   310,   935,
   268,   479,   831,   157,   257,   938,   582,   288,   714,   712,
   597,   713,   934,   715,  1157,   271,   583,   286,   483,  1169,
  1290,   191,   311,  1210,   441,   165,   164,   158,   159,   160,
   161,   162,   163,   716,  1035,   271,   566,  1143,  1140,  1155,
  1154,   964,   962,   165,   164,   158,   159,   160,   161,   162,
   163,   302,   165,   164,   158,   159,   160,   161,   162,   163,
   716,   172,   173,   174,   175,   176,   177,   686,   165,   164,
   158,   159,   160,   161,   162,   163,  1104,   604,   605,   608,
   609,   691,  1287,   334,   165,   164,   158,   159,   160,   161,
   162,   163,   157,  1285,  1111,   482,   300,   714,   712,  1365,
   713,   311,   715,  1049,   944,   165,   164,   158,   159,   160,
   161,   162,   163,   898,  1150,  1141,  1183,  1145,   943,  1032,
  1033,   935,  1182,   643,   301,   777,   309,   706,   202,   279,
   724,   201,   899,   338,   278,   716,   256,   165,   164,   158,
   159,   160,   161,   162,   163,  1170,   171,   961,  1168,   716,
   493,   282,  1052,  1053,  1054,  1055,  1056,  1057,   156,   165,
   164,   158,   159,   160,   161,   162,   163,   124,  1357,  1061,
  1062,  1063,  1064,  1065,  1066,  1067,  1068,  1069,  1070,  1071,
  1072,  1073,  1074,  1075,  1142,  1077,  1078,  1079,  1080,  1081,
  1082,  1083,  1084,  1085,  1086,  1087,  1088,  1089,  1090,   280,
  1251,   679,  1091,   157,   480,   481,   157,   678,   714,   712,
  1094,   713,  1387,   715,   339,   165,   164,   158,   159,   160,
   161,   162,   163,  1163,   726,  1139,  1099,   900,  1208,  1348,
  1207,  1100,   283,   169,   167,   169,   168,   157,   170,   725,
   170,  1213,   714,   712,   210,   713,   123,   715,  1212,   969,
  1301,   202,  1361,   446,   201,  1360,  1105,   717,  1109,  1358,
   716,  1262,  1250,   156,   484,   165,   164,   158,   159,   160,
   161,   162,   163,   957,   812,   189,  1115,  1122,  1128,  1129,
  1300,  1131,  1132,  1113,   187,   171,   445,   171,   453,   830,
   710,   995,   834,   857,   716,   889,   681,  1162,   727,   699,
   697,   337,  1148,   956,  1152,   188,   683,   591,   281,   681,
   340,  1171,  1173,  1166,   186,   165,   164,   158,   159,   160,
   161,   162,   163,   680,   511,  1359,  1326,   443,   284,   444,
   952,  1187,  1291,   574,   575,   576,   577,   578,   579,   580,
   581,   451,   901,   902,   903,   904,   905,   906,   907,   908,
   909,   910,   911,   912,   913,   914,   915,   916,   917,   918,
   919,   920,   921,   922,   923,   592,  1161,   452,  1252,  1221,
  1181,  1147,  1196,   140,   141,  1200,   164,   158,   159,   160,
   161,   162,   163,  1203,   945,   144,   165,   164,   158,   159,
   160,   161,   162,   163,   263,  1048,  1214,  1215,  1217,  1218,
   958,   588,   589,   590,  1047,   447,  1220,  1046,  1223,  1224,
  1225,   890,   165,   164,   158,   159,   160,   161,   162,   163,
  1045,  1228,  1044,   165,   164,   158,   159,   160,   161,   162,
   163,   417,   190,  1043,  1230,  1232,   416,   595,   478,   970,
   179,   178,   172,   173,   174,   175,   176,   177,   688,  1393,
  1235,  1236,   476,  1238,   806,  1240,  1367,  1242,   598,  1337,
  1335,   708,  1244,  1246,  1334,   893,  1333,   804,   157,   418,
   519,   520,  1247,   714,   712,  1338,   713,  1313,   715,   477,
  1312,   593,   594,   266,  1255,   891,   261,   892,  1259,  1305,
  1267,  1261,  1002,   757,   756,   755,   754,   265,   753,   165,
   164,   158,   159,   160,   161,   162,   163,  1216,   202,   436,
   414,   201,   415,   439,   752,   751,   750,   749,   748,   587,
   202,   747,   596,   201,   185,   716,   474,  1051,   475,  1227,
   746,   745,   267,   744,   743,   742,   308,   741,   740,   205,
   739,   738,   599,  1229,  1231,   737,   437,  1272,   736,   202,
   455,   454,   201,   165,   164,   158,   159,   160,   161,   162,
   163,  1237,   735,  1239,   734,  1241,   264,   733,   732,   157,
   307,  1243,  1245,   731,   714,   712,  1336,   713,   304,   715,
   165,   164,   158,   159,   160,   161,   162,   163,  1284,   440,
   730,   729,   157,   728,   682,   145,   138,   714,   712,  1332,
   713,   157,   715,   601,   888,  1325,   154,   152,   419,   153,
   262,   155,   208,   206,   184,   207,   169,   167,   299,   168,
   298,   170,   600,   146,   139,  1103,   716,   297,   142,   143,
   296,   295,   473,   242,   241,   202,   202,    69,   201,   201,
   240,   431,    70,   239,   834,   434,   238,  1114,  1121,   716,
  1295,  1130,   448,  1296,  1297,   762,  1134,   446,   156,   237,
   236,   235,   234,  1303,   770,   438,   233,   857,   171,   165,
   164,   158,   159,   160,   161,   162,   163,   157,   432,   157,
   183,   232,   714,   712,   714,   713,   306,   715,   231,   715,
   445,   157,  1328,   230,   449,  1323,   154,   152,   229,   153,
   260,   155,   814,   228,  1340,   227,  1341,   226,   225,  1343,
  1344,  1345,  1346,   224,   223,   807,  1322,   169,   167,   204,
   168,   435,   170,   222,   221,   220,   450,   219,   218,   813,
   217,   443,   216,   444,   716,   215,   716,   214,   209,   213,
  1349,  1350,  1352,   212,   157,  1353,   211,   182,   156,   714,
   712,  1315,   713,  1206,   715,  1354,  1355,   574,   575,   576,
   577,   578,   579,   580,   581,   157,   181,  1356,   180,   171,
   154,   150,   149,  1362,   148,   155,   815,  1363,   816,  1364,
   165,   164,   158,   159,   160,   161,   162,   163,   879,  1372,
   855,   832,   485,    11,    10,     9,     1,   433,  1377,   202,
   567,   716,   201,    97,    96,   723,  1383,     7,   442,   447,
  1384,  1385,  1386,   119,   331,  1388,   158,   159,   160,   161,
   162,   163,   156,  1391,   125,   586,   603,     8,  1392,   259,
  1167,   843,     4,     3,     2,  1395,     0,  1397,     0,  1399,
     0,  1401,     0,     0,     0,     0,     0,     0,     0,     0,
     0,  1351,     0,    12,    21,    22,    23,    24,    25,    26,
    27,    28,    29,    30,    31,    32,    33,    34,    35,    44,
    46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
    56,    57,    58,    59,    60,    61,    62,    45,    63,    64,
    65,    66,     0,    36,    37,    38,    39,    40,    41,    42,
    43,    84,    79,     0,     0,    85,    89,     0,     0,     0,
   140,   141,     0,     0,     0,     0,     0,    72,   100,     0,
    20,   330,   144,    81,    74,     0,     0,     0,   157,     0,
   111,     0,  1278,   714,   712,   110,   713,    75,   715,  1283,
    90,   108,     0,     0,   114,     0,   127,   134,    99,     0,
     0,  1034,     0,     0,     0,    73,     0,     0,     0,     0,
    76,     0,     0,   135,     0,     0,     0,     0,    93,   102,
     0,     0,     0,     0,    80,   122,    86,     0,    16,   120,
     0,     0,   128,     0,  1298,   716,     0,     0,     0,    98,
    67,     0,     0,     0,     0,   508,     0,     0,   136,   137,
     0,    92,     0,     0,   392,     0,     0,   121,     0,   393,
     0,   112,     0,   507,     0,    78,    94,   101,   506,   505,
     0,     0,     0,     0,   313,   314,   315,    91,   316,   317,
   318,   319,   320,   321,   322,   323,   324,   325,   326,   327,
   328,   329,    77,     0,     0,   165,   164,   158,   159,   160,
   161,   162,   163,     0,   504,   503,     0,     0,     0,   126,
     0,     0,     0,     0,     0,   106,   107,    87,   105,    71,
    95,    17,     0,     0,     0,     0,     0,     0,   129,   131,
   130,   133,   132,     0,     0,     0,     0,    15,   147,     0,
     0,     0,    83,   501,   103,   502,     0,     0,   109,     0,
     0,     0,     0,     0,     0,     0,     0,   113,   118,    82,
     0,   157,     0,     0,   117,  1321,   714,   712,   104,   713,
     0,   715,     0,   116,    88,   115,     0,     0,     0,     0,
     0,   157,   145,   138,     0,  1319,   714,   712,     0,   713,
     0,   715,     0,     0,     0,     0,   165,   164,   158,   159,
   160,   161,   162,   163,     0,     0,     0,     0,     0,     0,
   146,   139,     0,     0,     0,   142,   143,     0,   716,   165,
   164,   158,   159,   160,   161,   162,   163,     0,   165,   164,
   158,   159,   160,   161,   162,   163,     0,     0,   716,   178,
   172,   173,   174,   175,   176,   177,     0,   574,   575,   576,
   577,   578,   579,   580,   581,    14,    13,     0,     0,    18,
    19,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    68,   353,   354,   355,   356,   357,   358,   359,   360,   361,
   362,   363,   364,   365,   366,   367,   368,   370,   371,   372,
   373,   374,   375,   376,   377,   378,   379,   380,   381,   382,
   383,   384,   385,   386,   369,   387,   388,   389,   390,     0,
    36,    37,    38,    39,    40,    41,    42,    43,   165,   164,
   158,   159,   160,   161,   162,   163,     0,     0,   157,     0,
     0,     0,  1317,   714,   712,     0,   713,    20,   715,   179,
   178,   172,   173,   174,   175,   176,   177,   462,     0,     0,
   466,     0,     0,     0,   157,   470,   392,     0,  1311,   154,
   152,   393,   153,     0,   155,     0,     0,     0,     0,     0,
     0,   165,   164,   158,   159,   160,   161,   162,   163,     0,
     0,     0,     0,     0,     0,   716,   157,     0,   467,     0,
     0,   714,   712,  1314,   713,   425,   715,     0,     0,     0,
   157,   460,     0,     0,  1309,   154,   152,    67,   153,   157,
   155,   156,     0,  1307,   714,   712,     0,   713,     0,   715,
   458,     0,     0,     0,     0,   461,     0,     0,     0,   468,
     0,   469,   157,     0,     0,   457,     0,   714,   712,  1304,
   713,   573,   715,   716,     0,     0,   517,     0,     0,     0,
     0,   513,   202,   157,     0,   201,     0,   156,   714,   712,
  1299,   713,   157,   715,     0,     0,   716,   714,   712,  1271,
   713,     0,   715,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,   516,   157,     0,     0,   250,   716,
   714,   712,  1270,   713,   518,   715,     0,   157,     0,     0,
     0,  1030,   714,   712,   352,   713,   157,   715,     0,     0,
   716,   714,   712,  1269,   713,     0,   715,     0,     0,   716,
     0,     0,     0,     0,   471,   515,     0,   514,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,   459,
     0,     0,   716,     0,   464,     0,     0,     0,   202,     0,
     0,   201,     0,     0,   716,   165,   164,   158,   159,   160,
   161,   162,   163,   716,     0,     0,     0,     0,     0,     0,
     0,     0,   465,   353,   354,   355,   356,   357,   358,   359,
   360,   361,   362,   363,   364,   365,   366,   367,   368,   370,
   371,   372,   373,   374,   375,   376,   377,   378,   379,   380,
   381,   382,   383,   384,   385,   386,   369,   387,   388,   389,
   390,     0,    36,    37,    38,    39,    40,    41,    42,    43,
   392,     0,   351,   350,     0,   393,    18,    19,     0,     0,
     0,     0,     0,     0,     0,     0,   157,   391,     0,    20,
  1028,   714,   712,     0,   713,     0,   715,     0,     0,     0,
   157,     0,   412,     0,  1026,   714,   712,   408,   713,     0,
   715,   157,   413,     0,     0,  1024,   714,   712,     0,   713,
   157,   715,     0,     0,     0,   714,   712,  1268,   713,     0,
   715,     0,     0,     0,     0,     0,     0,     0,     0,     0,
   410,     0,     0,   716,   157,     0,     0,   404,  1020,   714,
   712,     0,   713,     0,   715,     0,     0,   716,     0,    67,
     0,     0,     0,     0,     0,     0,   157,     0,   716,   411,
  1018,   714,   712,     0,   713,     0,   715,   716,     0,     0,
     0,     0,     0,   407,     0,     0,     0,   406,   165,   164,
   158,   159,   160,   161,   162,   163,  1320,   169,   167,     0,
   168,   716,   170,     0,   202,     0,     0,   201,   165,   164,
   158,   159,   160,   161,   162,   163,     0,     0,     0,   157,
     0,     0,     0,   716,   714,   712,  1016,   713,   157,   715,
     0,     0,     0,   714,   712,  1014,   713,     0,   715,     0,
   250,     0,     0,     0,     0,     0,   409,     0,     0,   171,
     0,     0,     0,     0,     0,     0,   352,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,   716,     0,     0,     0,
     0,     0,     0,     0,     0,   716,     0,   353,   354,   355,
   356,   357,   358,   359,   360,   361,   362,   363,   364,   365,
   366,   367,   368,   370,   371,   372,   373,   374,   375,   376,
   377,   378,   379,   380,   381,   382,   383,   384,   385,   386,
   369,   387,   388,   389,   390,     0,    36,    37,    38,    39,
    40,    41,    42,    43,   392,     0,     0,     0,     0,   393,
     0,     0,     0,     0,     0,     0,     0,     0,     0,   401,
     0,     0,     0,    20,     0,   165,   164,   158,   159,   160,
   161,   162,   163,     0,     0,     0,     0,     0,     0,     0,
     0,   400,     0,     0,   351,   350,   402,     0,    18,    19,
     0,   165,   164,   158,   159,   160,   161,   162,   163,   391,
     0,     0,     0,     0,     0,     0,     0,     0,     0,   157,
     0,     0,     0,     0,   714,   712,  1012,   713,     0,   715,
     0,   395,     0,   165,   164,   158,   159,   160,   161,   162,
   163,     0,     0,    67,     0,     0,     0,   165,   164,   158,
   159,   160,   161,   162,   163,     0,   165,   164,   158,   159,
   160,   161,   162,   163,   157,   399,     0,   398,  1008,   714,
   712,   397,   713,     0,   715,     0,   716,     0,     0,   165,
   164,   158,   159,   160,   161,   162,   163,     0,   202,     0,
     0,   201,     0,     0,     0,     0,     0,     0,     0,     0,
   165,   164,   158,   159,   160,   161,   162,   163,     0,   165,
   164,   158,   159,   160,   161,   162,   163,   169,   167,  1021,
   168,   716,   170,     0,   250,     0,     0,     0,     0,     0,
     0,     0,   165,   164,   158,   159,   160,   161,   162,   163,
   352,     0,     0,     0,   165,   164,   158,   159,   160,   161,
   162,   163,     0,   165,   164,   158,   159,   160,   161,   162,
   163,     0,     0,     0,     0,     0,     0,     0,     0,   171,
     0,   353,   354,   355,   356,   357,   358,   359,   360,   361,
   362,   363,   364,   365,   366,   367,   368,   370,   371,   372,
   373,   374,   375,   376,   377,   378,   379,   380,   381,   382,
   383,   384,   385,   386,   369,   387,   388,   389,   390,     0,
    36,    37,    38,    39,    40,    41,    42,    43,   392,     0,
     0,     0,     0,   393,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,   157,     0,     0,    20,     0,   714,
   712,  1010,   713,     0,   715,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,   347,     0,     0,   351,   350,
   349,     0,    18,    19,   169,   167,  1015,   168,     0,   170,
   157,     0,     0,   391,  1006,   714,   712,     0,   713,     0,
   715,     0,   348,   165,   164,   158,   159,   160,   161,   162,
   163,   716,     0,     0,     0,   342,     0,   165,   164,   158,
   159,   160,   161,   162,   163,     0,     0,    67,   165,   164,
   158,   159,   160,   161,   162,   163,   171,   165,   164,   158,
   159,   160,   161,   162,   163,     0,     0,   716,   157,   345,
     0,   346,  1004,   714,   712,   344,   713,     0,   715,     0,
     0,   165,   164,   158,   159,   160,   161,   162,   163,     0,
     0,     0,   202,     0,     0,   201,     0,     0,     0,     0,
     0,     0,     0,   165,   164,   158,   159,   160,   161,   162,
   163,  1318,   169,   167,     0,   168,   157,   170,     0,     0,
     0,   714,   712,  1266,   713,   716,   715,     0,   250,   179,
   178,   172,   173,   174,   175,   176,   177,     0,     0,     0,
     0,     0,     0,     0,   352,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,   165,   164,   158,   159,
   160,   161,   162,   163,   171,   165,   164,   158,   159,   160,
   161,   162,   163,   716,     0,   353,   354,   355,   356,   357,
   358,   359,   360,   361,   362,   363,   364,   365,   366,   367,
   368,   370,   371,   372,   373,   374,   375,   376,   377,   378,
   379,   380,   381,   382,   383,   384,   385,   386,   369,   387,
   388,   389,   390,     0,    36,    37,    38,    39,    40,    41,
    42,    43,   392,     0,     0,     0,     0,   393,     0,   157,
     0,   884,   880,   999,   714,   712,   886,   713,   157,   715,
     0,    20,   997,   714,   712,     0,   713,   508,   715,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,  1124,
     0,     0,   351,   350,     0,   507,    18,    19,     0,   885,
   506,   505,     0,     0,     0,     0,     0,   391,     0,     0,
     0,     0,     0,     0,     0,     0,   716,     0,     0,     0,
     0,     0,     0,     0,     0,   716,     0,     0,   883,   425,
     0,   881,  1127,     0,     0,     0,   504,   503,     0,     0,
     0,    67,   887,   157,     0,     0,     0,     0,   714,   712,
  1265,   713,   157,   715,     0,     0,   992,   714,   712,     0,
   713,     0,   715,  1126,     0,  1125,   165,   164,   158,   159,
   160,   161,   162,   163,   157,   501,     0,   502,   990,   714,
   712,   882,   713,     0,   715,     0,   202,     0,     0,   201,
     0,   157,     0,     0,     0,   988,   714,   712,     0,   713,
   716,   715,     0,     0,     0,     0,     0,     0,     0,   716,
     0,   165,   164,   158,   159,   160,   161,   162,   163,     0,
     0,     0,   250,     0,     0,     0,     0,     0,   500,     0,
     0,   716,     0,     0,     0,     0,     0,     0,   352,     0,
     0,  1123,     0,     0,     0,     0,     0,     0,   716,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,   179,
   178,   172,   173,   174,   175,   176,   177,     0,     0,   353,
   354,   355,   356,   357,   358,   359,   360,   361,   362,   363,
   364,   365,   366,   367,   368,   370,   371,   372,   373,   374,
   375,   376,   377,   378,   379,   380,   381,   382,   383,   384,
   385,   386,   369,   387,   388,   389,   390,     0,    36,    37,
    38,    39,    40,    41,    42,    43,   392,     0,     0,     0,
     0,   393,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,   157,     0,     0,    20,   986,   714,   712,     0,
   713,   157,   715,     0,     0,   984,   714,   712,     0,   713,
     0,   715,     0,  1117,     0,     0,   351,   350,   157,     0,
    18,    19,   982,   714,   712,     0,   713,     0,   715,   157,
     0,   391,     0,     0,   714,   712,   980,   713,     0,   715,
     0,   165,   164,   158,   159,   160,   161,   162,   163,   716,
     0,     0,     0,   425,   157,     0,  1120,     0,   716,   714,
   712,  1263,   713,     0,   715,    67,   179,   178,   172,   173,
   174,   175,   176,   177,     0,   716,     0,   165,   164,   158,
   159,   160,   161,   162,   163,     0,   716,  1119,   157,  1118,
     0,     0,   978,   714,   712,     0,   713,   157,   715,     0,
     0,   976,   714,   712,     0,   713,     0,   715,     0,     0,
   202,   716,     0,   201,   157,     0,   392,     0,   974,   714,
   712,   393,   713,     0,   715,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,   165,   164,   158,   159,   160,
   161,   162,   163,     0,     0,   716,   250,     0,     0,     0,
     0,     0,     0,     0,   716,     0,     0,     0,     0,     0,
     0,     0,   352,     0,     0,  1116,     0,     0,     0,     0,
     0,   716,     0,     0,   179,   178,   172,   173,   174,   175,
   176,   177,     0,   165,   164,   158,   159,   160,   161,   162,
   163,     0,     0,   353,   354,   355,   356,   357,   358,   359,
   360,   361,   362,   363,   364,   365,   366,   367,   368,   370,
   371,   372,   373,   374,   375,   376,   377,   378,   379,   380,
   381,   382,   383,   384,   385,   386,   369,   387,   388,   389,
   390,     0,    36,    37,    38,    39,    40,    41,    42,    43,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,    20,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
   351,   350,     0,     0,    18,    19,     0,     0,     0,     0,
     0,     0,     0,     0,     0,   391,   165,   164,   158,   159,
   160,   161,   162,   163,     0,   165,   164,   158,   159,   160,
   161,   162,   163,     0,     0,     0,     0,   425,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,    67,
     0,     0,     0,   353,   354,   355,   356,   357,   358,   359,
   360,   361,   362,   363,   364,   365,   366,   367,   368,   370,
   371,   372,   373,   374,   375,   376,   377,   378,   379,   380,
   381,   382,   383,   384,   385,   386,   369,   387,   388,   389,
   390,     0,    36,    37,    38,    39,    40,    41,    42,    43,
   165,   164,   158,   159,   160,   161,   162,   163,     0,   165,
   164,   158,   159,   160,   161,   162,   163,   157,     0,    20,
     0,  1264,   714,   712,     0,   713,     0,   715,     0,     0,
   250,   165,   164,   158,   159,   160,   161,   162,   163,     0,
     0,     0,     0,     0,     0,     0,   352,     0,   165,   164,
   158,   159,   160,   161,   162,   163,   157,     0,     0,     0,
     0,   714,   712,     0,   713,     0,   715,     0,     0,     0,
     0,     0,     0,     0,   716,     0,   157,   425,     0,     0,
     0,   714,   712,     0,   713,     0,   715,     0,     0,    67,
     0,     0,     0,     0,   428,   427,     0,     0,     0,     0,
     0,   157,     0,   392,     0,   685,   714,   712,   393,   713,
     0,   715,  1249,   716,     0,     0,     0,     0,     0,     0,
   157,     0,   430,   429,     0,   714,   712,  1222,   713,     0,
   715,   157,  1248,   716,     0,     0,   714,   712,  1209,   713,
   157,   715,     0,     0,     0,   714,   712,  1050,   713,   157,
   715,     0,     0,  1031,   714,   712,     0,   713,   716,   715,
     0,     0,     0,     0,   351,   350,     0,     0,    18,    19,
   250,   392,     0,     0,     0,     0,   393,   716,     0,   391,
     0,     0,     0,     0,     0,     0,   352,     0,   716,   165,
   164,   158,   159,   160,   161,   162,   163,   716,   165,   164,
   158,   159,   160,   161,   162,   163,   716,     0,     0,     0,
     0,     0,     0,     0,     0,   165,   164,   158,   159,   160,
   161,   162,   163,     0,     0,     0,   165,   164,   158,   159,
   160,   161,   162,   163,   422,   421,     0,     0,     0,     0,
   392,     0,     0,     0,     0,   393,     0,     0,     0,     0,
     0,   165,   164,   158,   159,   160,   161,   162,   163,     0,
     0,     0,   424,   423,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,   165,   164,   158,   159,   160,
   161,   162,   163,     0,   165,   164,   158,   159,   160,   161,
   162,   163,     0,     0,   351,   350,     0,     0,    18,    19,
     0,   165,   164,   158,   159,   160,   161,   162,   163,   391,
   353,   354,   355,   356,   357,   358,   359,   360,   361,   362,
   363,   364,   365,   366,   367,   368,   370,   371,   372,   373,
   374,   375,   376,   377,   378,   379,   380,   381,   382,   383,
   384,   385,   386,   369,   387,   388,   389,   390,     0,    36,
    37,    38,    39,    40,    41,    42,    43,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,    20,     0,   353,   354,
   355,   356,   357,   358,   359,   360,   361,   362,   363,   364,
   365,   366,   367,   368,   370,   371,   372,   373,   374,   375,
   376,   377,   378,   379,   380,   381,   382,   383,   384,   385,
   386,   369,   387,   388,   389,   390,     0,    36,    37,    38,
    39,    40,    41,    42,    43,     0,     0,     0,     0,     0,
     0,     0,     0,     0,   425,     0,     0,     0,     0,     0,
     0,     0,     0,     0,    20,     0,    67,   353,   354,   355,
   356,   357,   358,   359,   360,   361,   362,   363,   364,   365,
   366,   367,   368,   370,   371,   372,   373,   374,   375,   376,
   377,   378,   379,   380,   381,   382,   383,   384,   385,   386,
   369,   387,   388,   389,   390,     0,    36,    37,    38,    39,
    40,    41,    42,    43,     0,   157,     0,     0,     0,  1030,
   154,   152,   425,   153,     0,   155,     0,     0,     0,     0,
     0,   157,     0,    20,    67,  1028,   154,   152,     0,   153,
     0,   155,     0,     0,     0,     0,     0,   250,    69,     0,
     0,     0,     0,    70,     0,     0,   157,     0,     0,     0,
  1026,   154,   152,   352,   153,     0,   155,     0,     0,   157,
     0,     0,   156,  1024,   154,   152,     0,   153,     0,   155,
     0,     0,     0,     0,     0,     0,     0,     0,   156,     0,
     0,   425,     0,     0,  1174,     0,     0,     0,     0,     0,
     0,     0,     0,    67,   165,   164,   158,   159,   160,   161,
   162,   163,     0,   156,     0,   250,     0,    69,     0,     0,
     0,     0,    70,     0,     0,     0,   156,     0,     0,     0,
   157,   352,     0,     0,     0,   154,   152,  1022,   153,     0,
   155,     0,     0,   165,   164,   158,   159,   160,   161,   162,
   163,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,  1172,   165,   164,   158,   159,   160,   161,   162,
   163,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,   351,   350,     0,   250,    18,    19,   156,   165,   164,
   158,   159,   160,   161,   162,   163,   391,     0,     0,     0,
   352,     0,     0,     0,     0,     0,     0,   165,   164,   158,
   159,   160,   161,   162,   163,     0,     0,     0,   165,   164,
   158,   159,   160,   161,   162,   163,     0,   165,   164,   158,
   159,   160,   161,   162,   163,     0,   165,   164,   158,   159,
   160,   161,   162,   163,     0,     0,     0,     0,     0,   351,
   350,     0,     0,    18,    19,     0,     0,     0,     0,     0,
     0,     0,     0,     0,   391,    21,    22,    23,    24,    25,
    26,    27,    28,    29,    30,    31,    32,    33,    34,    35,
    44,   249,    47,    48,    49,    50,    51,    52,    53,    54,
    55,    56,    57,    58,    59,    60,    61,    62,    45,    63,
    64,    65,    66,     0,    36,    37,    38,    39,    40,    41,
    42,    43,     0,     0,     0,     0,     0,     0,   351,   350,
     0,     0,    18,    19,     0,     0,     0,     0,     0,     0,
     0,    20,     0,   391,    21,    22,    23,    24,    25,    26,
    27,    28,    29,    30,    31,    32,    33,    34,    35,    44,
   249,    47,    48,    49,    50,    51,    52,    53,    54,    55,
    56,    57,    58,    59,    60,    61,    62,    45,    63,    64,
    65,    66,     0,    36,    37,    38,    39,    40,    41,    42,
    43,     0,   157,     0,     0,     0,  1020,   154,   152,   247,
   153,     0,   155,     0,     0,     0,     0,     0,   157,     0,
    20,    67,  1018,   154,   152,     0,   153,   157,   155,     0,
     0,     0,   154,   152,  1016,   153,   157,   155,     0,     0,
     0,   154,   152,  1014,   153,     0,   155,   157,     0,     0,
     0,     0,   154,   152,  1012,   153,   157,   155,     0,   156,
     0,   154,   152,  1010,   153,     0,   155,     0,     0,     0,
     0,     0,     0,     0,     0,   156,     0,   157,   247,     0,
     0,  1008,   154,   152,   156,   153,     0,   155,     0,     0,
    67,   157,     0,   156,     0,  1006,   154,   152,     0,   153,
     0,   155,   250,     0,   156,     0,   157,     0,     0,     0,
  1004,   154,   152,   156,   153,   157,   155,     0,   248,     0,
   154,   152,  1000,   153,     0,   155,     0,   157,     0,     0,
     0,   999,   154,   152,   156,   153,   157,   155,     0,     0,
   997,   154,   152,     0,   153,   157,   155,     0,   156,     0,
   154,   152,   994,   153,   157,   155,     0,     0,   992,   154,
   152,     0,   153,   156,   155,     0,     0,     0,     0,   157,
     0,   250,   156,   990,   154,   152,     0,   153,     0,   155,
     0,     0,     0,     0,   156,     0,   157,   641,     0,     0,
   988,   154,   152,   156,   153,   157,   155,     0,     0,   986,
   154,   152,   156,   153,     0,   155,     0,     0,     0,     0,
     0,   156,   165,   164,   158,   159,   160,   161,   162,   163,
     0,  1316,   169,   167,     0,   168,   156,   170,   165,   164,
   158,   159,   160,   161,   162,   163,   246,   245,     0,   157,
    18,    19,     0,   156,   154,   152,   980,   153,     0,   155,
     0,    68,   156,   165,   164,   158,   159,   160,   161,   162,
   163,     0,     0,     0,     0,     0,   165,   164,   158,   159,
   160,   161,   162,   163,   171,     0,     0,     0,   157,     0,
     0,     0,   984,   154,   152,     0,   153,   157,   155,     0,
     0,   982,   154,   152,     0,   153,   156,   155,     0,     0,
     0,     0,     0,     0,     0,   246,   245,   157,     0,    18,
    19,   978,   154,   152,     0,   153,   157,   155,     0,     0,
    68,   714,   712,   968,   713,     0,   715,   165,   164,   158,
   159,   160,   161,   162,   163,   156,     0,   157,     0,     0,
     0,   976,   154,   152,   156,   153,   157,   155,     0,     0,
   974,   154,   152,     0,   153,   157,   155,     0,     0,   971,
   154,   152,     0,   153,   156,   155,     0,     0,     0,   157,
     0,     0,     0,   716,   714,   712,   967,   713,   157,   715,
     0,     0,     0,   714,   712,   955,   713,   157,   715,     0,
     0,     0,   714,   712,   156,   713,   157,   715,     0,     0,
     0,   714,   712,   156,   713,     0,   715,     0,   157,     0,
     0,     0,   156,   714,   712,   817,   713,   157,   715,     0,
     0,     0,   714,   712,   790,   713,   716,   715,     0,     0,
     0,   157,     0,     0,     0,   716,   714,   712,   785,   713,
   157,   715,     0,   954,   716,   714,   712,   769,   713,   157,
   715,     0,   953,   716,   714,   712,   761,   713,   151,   715,
     0,   157,     0,     0,     0,   716,   714,   712,   711,   713,
     0,   715,   157,     0,   716,     0,   685,   154,   152,     0,
   153,     0,   155,     0,     0,   157,     0,     0,   716,     0,
   154,   152,     0,   153,   157,   155,     0,   716,     0,   714,
   712,     0,   713,   157,   715,     0,   716,     0,   154,   152,
     0,   153,     0,   155,     0,  1310,   169,   167,   716,   168,
     0,   170,     0,     0,  1308,   169,   167,     0,   168,   156,
   170,     0,     0,     0,  1306,   169,   167,     0,   168,     0,
   170,     0,   156,     0,     0,     0,     0,     0,     0,     0,
     0,   716,     0,     0,     0,     0,     0,     0,     0,     0,
   156,     0,     0,     0,     0,     0,     0,     0,   171,  1029,
   169,   167,     0,   168,     0,   170,     0,   171,     0,   165,
   164,   158,   159,   160,   161,   162,   163,   171,  1027,   169,
   167,     0,   168,     0,   170,   165,   164,   158,   159,   160,
   161,   162,   163,     0,   165,   164,   158,   159,   160,   161,
   162,   163,     0,   165,   164,   158,   159,   160,   161,   162,
   163,     0,   171,     0,   165,   164,   158,   159,   160,   161,
   162,   163,     0,   165,   164,   158,   159,   160,   161,   162,
   163,   171,     0,     0,     0,     0,     0,  1025,   169,   167,
     0,   168,     0,   170,   165,   164,   158,   159,   160,   161,
   162,   163,     0,     0,     0,     0,     0,     0,   165,   164,
   158,   159,   160,   161,   162,   163,     0,     0,     0,     0,
     0,     0,     0,   165,   164,   158,   159,   160,   161,   162,
   163,     0,   165,   164,   158,   159,   160,   161,   162,   163,
   171,     0,     0,     0,   165,   164,   158,   159,   160,   161,
   162,   163,     0,   165,   164,   158,   159,   160,   161,   162,
   163,     0,   165,   164,   158,   159,   160,   161,   162,   163,
     0,   165,   164,   158,   159,   160,   161,   162,   163,  1023,
   169,   167,     0,   168,     0,   170,   165,   164,   158,   159,
   160,   161,   162,   163,  1019,   169,   167,     0,   168,     0,
   170,     0,     0,   165,   164,   158,   159,   160,   161,   162,
   163,     0,   165,   164,   158,   159,   160,   161,   162,   163,
  1017,   169,   167,     0,   168,     0,   170,     0,     0,     0,
     0,     0,   171,     0,   179,   178,   172,   173,   174,   175,
   176,   177,     0,     0,     0,     0,     0,   171,     0,     0,
     0,     0,     0,     0,     0,     0,   165,   164,   158,   159,
   160,   161,   162,   163,   169,   167,  1013,   168,     0,   170,
     0,     0,     0,   171,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,   169,
   167,  1011,   168,     0,   170,   165,   164,   158,   159,   160,
   161,   162,   163,     0,   165,   164,   158,   159,   160,   161,
   162,   163,     0,     0,     0,     0,   171,     0,     0,     0,
     0,     0,     0,     0,   165,   164,   158,   159,   160,   161,
   162,   163,     0,   165,   164,   158,   159,   160,   161,   162,
   163,   171,   169,   167,  1009,   168,     0,   170,     0,     0,
     0,     0,     0,     0,   165,   164,   158,   159,   160,   161,
   162,   163,     0,   165,   164,   158,   159,   160,   161,   162,
   163,     0,   165,   164,   158,   159,   160,   161,   162,   163,
     0,     0,     0,     0,     0,     0,   165,   164,   158,   159,
   160,   161,   162,   163,   171,   165,   164,   158,   159,   160,
   161,   162,   163,     0,   165,   164,   158,   159,   160,   161,
   162,   163,     0,   165,   164,   158,   159,   160,   161,   162,
   163,     0,     0,     0,     0,   165,   164,   158,   159,   160,
   161,   162,   163,     0,   165,   164,   158,   159,   160,   161,
   162,   163,     0,     0,     0,     0,     0,     0,   165,   164,
   158,   159,   160,   161,   162,   163,     0,   165,   164,   158,
   159,   160,   161,   162,   163,     0,   165,   164,   158,   159,
   160,   161,   162,   163,     0,     0,     0,     0,   165,   164,
   158,   159,   160,   161,   162,   163,     0,     0,     0,   165,
   164,   158,   159,   160,   161,   162,   163,     0,     0,     0,
     0,     0,   165,   164,   158,   159,   160,   161,   162,   163,
     0,   165,   164,   158,   159,   160,   161,   162,   163,     0,
   165,   164,   158,   159,   160,   161,   162,   163,   179,   178,
   172,   173,   174,   175,   176,   177,     0,   179,   178,   172,
   173,   174,   175,   176,   177,     0,     0,   179,   178,   172,
   173,   174,   175,   176,   177,   864,     0,   861,  1007,   169,
   167,     0,   168,     0,   170,     0,  1005,   169,   167,     0,
   168,     0,   170,     0,     0,     0,   874,     0,     0,     0,
     0,   876,   179,   178,   172,   173,   174,   175,   176,   177,
     0,  1003,   169,   167,     0,   168,     0,   170,     0,     0,
   192,   179,   178,   172,   173,   174,   175,   176,   177,     0,
     0,   171,     0,     0,   875,     0,   860,     0,     0,   171,
   169,   167,  1001,   168,     0,   170,     0,   873,     0,     0,
   998,   169,   167,     0,   168,     0,   170,     0,     0,     0,
     0,     0,     0,   865,   171,     0,   863,     0,     0,   996,
   169,   167,     0,   168,     0,   170,     0,   877,     0,     0,
   179,   178,   172,   173,   174,   175,   176,   177,   169,   167,
   993,   168,   171,   170,     0,     0,     0,     0,   202,     0,
     0,   201,   868,   171,   991,   169,   167,     0,   168,     0,
   170,     0,     0,     0,     0,     0,     0,     0,     0,     0,
   859,   862,   171,     0,     0,     0,     0,   989,   169,   167,
     0,   168,     0,   170,     0,     0,     0,     0,     0,     0,
   171,     0,   987,   169,   167,     0,   168,     0,   170,     0,
     0,     0,   869,     0,   866,     0,     0,   171,   985,   169,
   167,     0,   168,     0,   170,   867,   870,     0,     0,   871,
     0,     0,   179,   178,   172,   173,   174,   175,   176,   177,
   171,   858,     0,     0,     0,   872,     0,   179,   178,   172,
   173,   174,   175,   176,   177,   171,   983,   169,   167,     0,
   168,     0,   170,     0,     0,     0,     0,     0,     0,     0,
     0,   171,     0,   179,   178,   172,   173,   174,   175,   176,
   177,   981,   169,   167,     0,   168,     0,   170,     0,     0,
     0,     0,     0,     0,     0,     0,     0,   169,   167,   979,
   168,     0,   170,   166,   977,   169,   167,     0,   168,   171,
   170,     0,     0,     0,     0,   199,   179,   178,   172,   173,
   174,   175,   176,   177,   975,   169,   167,     0,   168,     0,
   170,     0,   878,     0,   171,   169,   167,     0,   168,     0,
   170,   179,   178,   172,   173,   174,   175,   176,   177,   171,
   973,   169,   167,     0,   168,   195,   170,   171,   972,   169,
   167,     0,   168,     0,   170,     0,   684,   169,   167,     0,
   168,     0,   170,     0,     0,   169,   167,   171,   168,     0,
   170,   446,   498,     0,     0,     0,   844,   171,   841,   840,
     0,     0,     0,     0,   179,   178,   172,   173,   174,   175,
   176,   177,   496,   171,   198,     0,     0,     0,   494,     0,
     0,   171,     0,     0,   489,     0,   491,     0,   839,   171,
     0,     0,   196,     0,     0,     0,     0,   171,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,   854,   194,
     0,     0,   333,   331,     0,     0,     0,     0,   846,     0,
   332,     0,     0,     0,   842,   443,     0,   444,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,   488,
     0,     0,   202,     0,     0,   201,     0,     0,   202,     0,
     0,   201,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
   490,     0,   852,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,   847,   200,     0,     0,     0,     0,
     0,     0,   835,   447,   495,   836,     0,     0,     0,     0,
     0,     0,     0,     0,     0,   202,   837,   838,   201,   848,
   330,   853,     0,   492,     0,   197,     0,     0,     0,     0,
     0,   487,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,   497,     0,     0,     0,
   845,     0,     0,     0,     0,   850,     0,     0,     0,     0,
     0,   179,   178,   172,   173,   174,   175,   176,   177,   179,
   178,   172,   173,   174,   175,   176,   177,     0,   849,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,   179,   178,   172,   173,   174,   175,
   176,   177,     0,   313,   314,   315,     0,   316,   317,   318,
   319,   320,   321,   322,   323,   324,   325,   326,   327,   328,
   329,     0,   179,   178,   172,   173,   174,   175,   176,   177,
     0,     0,     0,   179,   178,   172,   173,   174,   175,   176,
   177,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,   179,   178,   172,   173,   174,   175,   176,   177,
     0,     0,     0,     0,     0,     0,     0,     0,     0,   851,
   179,   178,   172,   173,   174,   175,   176,   177,     0,     0,
     0,     0,     0,     0,     0,     0,     0,   179,   178,   172,
   173,   174,   175,   176,   177,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
   179,   178,   172,   173,   174,   175,   176,   177,     0,     0,
     0,     0,     0,     0,     0,   179,   178,   172,   173,   174,
   175,   176,   177,     0,     0,     0,     0,     0,     0,     0,
     0,   179,   178,   172,   173,   174,   175,   176,   177,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,   179,
   178,   172,   173,   174,   175,   176,   177,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,   179,   178,   172,   173,   174,   175,
   176,   177,     0,     0,     0,     0,     0,     0,     0,   179,
   178,   172,   173,   174,   175,   176,   177,   179,   178,   172,
   173,   174,   175,   176,   177,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,   179,   178,   172,
   173,   174,   175,   176,   177,     0,     0,   179,   178,   172,
   173,   174,   175,   176,   177,     0,     0,     0,     0,     0,
     0,     0,     0,   179,   178,   172,   173,   174,   175,   176,
   177,   179,   178,   172,   173,   174,   175,   176,   177,   179,
   178,   172,   173,   174,   175,   176,   177,   179,   178,   172,
   173,   174,   175,   176,   177 };
yytabelem yypact[]={

  1157, -1000,  1324,  1322,  1321,  4908,  5833,  1318,  1316,  1297,
  1230,  1164,  1074,   783,   774,   535,  5574,   758, -1000, -1000,
  1266,  1263,  1259,  1257,  1255,  1252,  1250,  1248, -1000, -1000,
 -1000,  1247,  1245,  1244,  1243,  1234, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000,  1233,  1228,  1227,  1225,
  1223,  1218,  1213,  1208,  1201,  1186,  1182,  1181,  1180, -1000,
 -1000, -1000,  1179,  1166,  1163,  1160,  1154,  1153,  4088,  4088,
  4088, -1000,     6,   359,   223,  -208, -1000,   648,    86,    75,
    75,    75, -1000,  -214,  -216,    75,   357,   352,   378,     1,
    87,    77,    66, -1000,  -290,  1151,  1150,  1147,  1140,  1138,
   121,    92,   757,   127,  1044,  5653, -1000, -1000, -1000,  -509,
   360,  2354,  2090,  1826,   655,  3286,  3146,   864,   732,   876,
   620,  1524,   671,   250, -1000,  5580,  2580,  -221,   202, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000,  1620,  1620,  1620, -1000, -1000, -1000,
 -1000, -1000,  4088,  4088,  4088,  4088,  4088,  3790,  3790,  3790,
  3790,  3790,  3790,  3790,  3790,  3790, -1000,  4088,  4088,  4088,
  4088,  4088,  4088,  4088,  4088,  4088,  4088,  4088,  4088,  4088,
 -1000, -1000, -1000, -1000, -1000, -1000,  3790,  4088,  3790,  4088,
  3790,   -12,  1461, -1000,    92,    55,    75,   539,   642,   -38,
 -1000, -1000, -1000, -1000,    67,  -222,  -225,  -226,  3790,  3790,
  -190,    -9,  4088,  4088,  4088,  4088,  4088,  4088,  4088,  4088,
  4088,  4088,  4088,  4088,  4157,  4088,  4088,  -228,  4088,  4088,
  4088,  4088,  4088,  4088,  4088,  4088,  4088,  4088,  4088,  4088,
  4088,  4088,  3790, -1000, -1000,   676,   670,   837,   823,  1114,
   820,  5865,  4895, -1000, -1000,    92, -1000,  -229,   964, -1000,
 -1000,  -230, -1000, -1000, -1000, -1000, -1000, -1000,   -82,    92,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000,  -231,  -232,  -239,  -240,   -83,   814,   -84,   813,
 -1000, -1000, -1000, -1000,  -242,   -13,   -18,   -19,   -20,   -21,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000,   350,   206, -1000,  3790, -1000, -1000, -1000,
 -1000, -1000,   804,  4884,  -107,  -245,  -247,  -250,   349, -1000,
   676,   670,   812,  1113,  1111,  1110,  1093,  1088,  1087,  1084,
 -1000, -1000, -1000,  1082,  1068,  1065,  1061,  1060, -1000, -1000,
  1058,  1057,  1055,  1054,  1053,  1051,  1050,  1041,  1038,  1037,
  1036,  1035,  1034, -1000, -1000, -1000,  1018,  1016,  1015,  1014,
  1013,  3790,  3790,  3790, -1000,   804,  4872,  -107,  -251,  -252,
  -253,  -472, -1000, -1000,   804,  4863,  -107,  -255,  -256,  -260,
  -278,  -291,   -44,   348,  -292,  -294,  -295,   -47,  -477,   -48,
  4854,  3790,  3790,  3790,  3790,   804,  4840,  -296,  -298,  -299,
  -300, -1000,  -301,  -302,  -305,  -310, -1000,  -311,  -312,  -316,
  -317,  -318,   472, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000,   -40,   250, -1000, -1000, -1000, -1000,  -107,  -319,  -320,
  -321,  -337,   921,  4831,  3790,  3790,   -49,  -340,  -342,  -343,
  -344,  -345, -1000,  -351,  -355,  -361,  -362,    92,   221, -1000,
  5647,  5260,  2595,   630, -1000, -1000, -1000,  1044,  3790,  -363,
  -365,   328,  -234,  -368,  -174,  -384,    44,  3790,   341,   327,
  1188, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
  -400, -1000, -1000,  -401,  -405,  -412,  -415,   -51,    92, -1000,
 -1000,  1288,   753,  1288,   753,   624,   612,   624,   612,   624,
   612,   601,  1200,  1200,  1200,  1200,  1200,  1200,   760,   321,
   753,  1288,   753,  1288,   612,   729,   612,   729,   729,   612,
   751,  4926,   751,   751,   751,   751,   751,     5,  1134,  4819,
  5873,  4926,  4810,  5873,  4926,  4801,   772, -1000, -1000, -1000,
 -1000, -1000, -1000,   503, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000,   209, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,   328,  -416,
  3790,  -418,  -419, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000,  4792,  4739,   748,   955,  4778,  5857,  5849,
  4769,  5823,  4760,  5803,  4730,  5795,  4662,  5780,  4710,  5755,
  4701,  5717,  4618,  5701,  4609,  5686,  4592,  5663,  4577,  5646,
  4568,   810,  5628,  4559,  5609,  4550,  4538,  5598,  1011,  5570,
  4529,  5545,  4514,  5537,  4500,  5320,  4479,  5267,  4470,  5242,
  4459,  2662,  4450,  5199,  4441,  5173,  4425,  2515,  4173,  5158,
  4112,  5066,  4099,  5007,  4074,  4988,  4058,  3702,  3790,  3790,
  1021,   -14,  4088,  -191, -1000, -1000, -1000, -1000,  -420, -1000,
   154, -1000, -1000, -1000, -1000, -1000,   -23,   -28,   -30,   -33,
   949,   938,   936,   923,   920,   911, -1000, -1000,   326,  3693,
   597,  3790,  3790,  3790,  3790,  3790,  3790, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000,  -426,  -427, -1000,  -293,  3790,  3790,
  3790,  3790,  3790,  3790,  3790,  3790,  3790,  3790,  3790,  3790,
  3790,  3790,  3790,  -428,  3790,  3790,  3790,  3790,  3790,  3790,
  3790,  3790,  3790,  3790,  3790,  3790,  3790,  3790, -1000,  3654,
 -1000,  3790, -1000, -1000, -1000, -1000, -1000,  -429,  -430,  3790,
 -1000, -1000, -1000, -1000, -1000, -1000,  -431, -1000, -1000, -1000,
 -1000,  -436, -1000,  -437,  -438,  3790,  4917,  4917,  4917,  4917,
  3790, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000,  -439,  -461, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000,   109,  -463,  -468,  -469,  3790,  4917,  4917,
  -471, -1000, -1000, -1000, -1000,   317, -1000, -1000, -1000, -1000,
 -1000,  -473,  5647, -1000, -1000,  2882,  2618,  3790,  3790,    92,
  3790,  3790,  -474, -1000,    92,  -475,  -476,  -479,   266,   179,
  -480,   887, -1000, -1000, -1000,  5260, -1000, -1000,   176,  -481,
   -74,   263,   262,    50,  -483,   448,  -491,  -493,   266,   272,
  3721,  3653,  -497,  -501,   -52,  -505,  -506,  -507,   886, -1000,
 -1000,   230,  -303,   448,   -56,  -508,  -512,  -515, -1000, -1000,
  -522,  -523,  -527, -1000,  4917, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000,  -528,    75,  -530,  -531,  -532,
 -1000, -1000,   231,  -533, -1000, -1000, -1000,  -534,   266,  -536,
  -538,   373,  3684, -1000, -1000,   246, -1000, -1000, -1000, -1000,
 -1000,  -544, -1000,   747,   740,  3790,  3790,  4088,  3790,  -198,
 -1000, -1000,   328,   885,  3673, -1000, -1000,  3790,  3790,  3790,
  -199, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,  4088,
  3790, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000,  4088,  4088, -1000, -1000, -1000, -1000, -1000,
  -546,  -548, -1000, -1000, -1000, -1000, -1000, -1000, -1000,  3790,
  3790,  4088,  3790,  4088,  3790,  4088,  3790, -1000, -1000, -1000,
 -1000,  4088,  4088, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000,  3790,  3629,  3609,   776,   669, -1000,   884, -1000, -1000,
   -86, -1000,   -88,  3790,  -549,  -550,  -552,  3790,  -554, -1000,
  3790,   775,  3227,  1202,  1202,   601,   601,   601, -1000, -1000,
   669,  3570,  3287,  3270,  3261,  3202,  3191,  3174,  3165,  3024,
  3007,  2985,  2976,  2901,  2892,  2779,  1009,  2731,  2673,  2467,
  2637,  2422,  2251,  2242,  2189,  2167,  2143,  2134,  2123,  2109,
  1970,  1979, -1000, -1000,  4917, -1000, -1000, -1000, -1000,  1958,
  1935, -1000, -1000, -1000,   231,  -557, -1000, -1000, -1000,  4917,
 -1000, -1000, -1000, -1000, -1000,  4917,  -558,  -559,  -566,  -568,
    92, -1000,  4917,  -573,  -574,  -575,  -582,    92,  4917,  4917,
 -1000,  4917,  4917, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000,  3790, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000,  4917,   155,  4917,   144, -1000, -1000,  -583, -1000, -1000,
 -1000,   243, -1000, -1000, -1000, -1000,   848, -1000,  -584, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000, -1000,  -585,  -587,  3790,
 -1000, -1000,  3790,  3790,  4917,  1451,  5873,  4926,  1926,   749,
 -1000,  -588,  3790,  1905,  4917,  4917,  1008,  4953,  1882,  4943,
  1873,  4934,  1827,   999,   996,  1859,  1267,  4640,  1801,  2770,
  1654,  2215,  1634,  1235,  1214,   386,  1124,   526, -1000, -1000,
  -200,  3790,  -589,   153,   151,  1115,   985,   983,   979,  1092,
   978,   991,  -201,  3790, -1000,  3790,  -590, -1000,  3790,  3790,
  3790,  3790, -1000, -1000, -1000, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000,  4917, -1000, -1000, -1000, -1000, -1000,
 -1000,  -600, -1000, -1000, -1000,  4917,  4917,  4917,   728,  3790,
  3790,  4088, -1000,  4917,  3790, -1000, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000,  3790,  3790, -1000, -1000, -1000, -1000,
 -1000, -1000, -1000, -1000, -1000, -1000,  3790,   637,   726,   841,
   769,   766,  3790, -1000, -1000, -1000,  3790, -1000,  3790,   637,
   615,   499,   975,   445,   369,   358,   332, -1000,  3790,  4917,
   261,  5873,  4926,   161,   211,   105,  4917,  3790, -1000,  -601,
   -34,   -36,  4917,    83,    51,  3790, -1000, -1000, -1000,  3790,
  3790,  3790,  4917,   711,  3790, -1000, -1000,    30, -1000, -1000,
 -1000,  -603,  3790,  4917,  4917,  4917,  4917,  3790,  4917, -1000,
   968,    14,  4917, -1000,  3790,    -2,  3790,   -11,  3790,   -29,
  3790,  4917 };
yytabelem yypgo[]={

     0,     0,   109,  1394,  1393,  1392,   371,     2,     5,   817,
     3,  1391,  1390,     7,     4,  1389,   443,  1387,  1386,  1385,
  1384,  1373,   584,  1368,  1367,  1365,   686,   537,  1364,  1363,
  1360,  1356,  1355,  1354,  1353,  1352,   806,   561,   727,   884,
  1351,  1350,  1348,  1164,    10,     6 };
yytabelem yyr1[]={

     0,    31,    31,    31,    31,    31,    31,    31,    31,    31,
    31,    31,    31,    34,    34,    34,    34,    34,    34,    16,
    16,    16,    16,    16,    16,    16,    16,    17,    17,    17,
    17,    17,    17,    17,    17,    24,    24,    24,    24,    24,
    24,    24,    24,    24,    24,    24,    24,    24,    24,    24,
    24,    24,    24,    24,    24,    24,    24,    24,    24,    24,
    24,    24,    24,    24,    24,    24,    24,    24,    24,    24,
    24,    24,    24,    24,    24,    24,    24,    24,    24,    24,
    24,    24,    24,    24,    24,    24,    24,    24,    24,    24,
    24,    24,    24,    24,    24,    24,    24,    24,    24,    24,
    24,    24,    24,    24,    24,    24,    24,    24,    24,    24,
    24,    24,    24,    24,    24,    24,    24,    24,    24,    24,
    24,    24,    24,    24,    24,    24,    24,    24,    24,    24,
    24,    24,    24,    24,    24,    24,    24,    24,    24,    24,
    24,    24,    24,    24,    24,    24,    24,    24,    24,    24,
    24,    24,    24,    24,    24,    24,    24,    24,    24,    24,
    24,    24,    24,    24,    24,    24,    24,    24,    24,    24,
    24,    24,    24,    24,    24,    24,    24,    24,    24,    24,
    24,    24,    24,    24,    24,    24,    24,    24,    24,    24,
    24,    24,    24,    24,    24,    24,    24,    24,    24,    24,
    24,    24,    24,    24,    24,    24,    24,    24,    24,    24,
    24,    23,    23,    23,    27,    27,    27,    27,    27,    27,
    27,    27,    27,    27,    27,    27,    27,    27,    27,    27,
    27,    27,    27,    19,    19,    19,    19,    19,    19,    19,
    19,    19,    19,    18,    18,    18,    18,    18,    18,    33,
    35,    35,    35,    35,    35,    35,    35,    35,    35,    35,
    35,    35,    35,    35,    35,    35,    35,    35,    35,    35,
    35,    35,    35,    35,    35,    35,    32,    32,    32,    32,
    32,    32,    36,    36,    36,    36,    36,    36,    38,    38,
    38,    39,    39,    39,    39,    39,    39,    39,    37,    37,
    37,    37,    37,    40,    40,    44,    44,    44,    44,    44,
    44,    44,    44,    44,    44,    44,    44,    44,    44,    44,
    44,    44,    44,    44,    44,    44,    44,    44,    44,    44,
    44,    44,    44,    44,    44,    44,    41,    41,    45,    45,
    45,    45,    45,    45,    45,    45,    45,    45,    45,    45,
    45,    45,    45,    45,    45,    45,    45,    45,    45,    45,
    45,    45,    45,    45,    45,    45,    45,    45,    42,    42,
    42,    42,    42,    42,    42,    42,    42,    42,    42,    43,
    43,    43,    43,    20,    20,    20,    20,    20,    20,    21,
    21,    22,    22,    22,    22,    22,     6,     6,     7,     7,
    28,    28,    28,    28,    28,    29,    29,    29,    29,    26,
    26,    25,    25,    25,    14,    14,    14,    14,    14,    13,
    13,    13,    30,    30,    30,    30,     8,     8,    11,    11,
    11,    10,    10,    10,    10,    10,    10,    10,    10,    10,
    10,    10,    10,    10,    10,    10,    10,    10,    10,    10,
    10,    10,    10,    10,    10,    10,    10,    12,    12,    12,
    15,    15,    15,    15,    15,    15,     9,     9,     3,     3,
     3,     3,     5,     4,     4,     4,     4,     4,     4,     4,
     4,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     2,     2,     2,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
     1,     1,     1,     1,     1,     1 };
yytabelem yyr2[]={

     0,     0,     4,     4,     4,     5,     5,     4,     4,     4,
     4,     4,     5,     7,     9,     7,     5,     7,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     5,     7,     7,
     7,     7,    19,    11,     9,     3,     7,     5,     7,    17,
     3,     5,     5,     7,     7,     5,     5,     5,     3,     5,
     5,     5,     5,     5,     5,     5,     7,    11,    15,     9,
    17,     9,    17,     5,     5,     5,     5,     3,    15,    13,
    13,    13,    21,    13,     9,     3,     5,     5,     5,     5,
     5,     5,     5,     5,     5,     5,     5,     5,     7,     7,
     9,     3,     3,     3,     5,    35,     5,     5,     5,     7,
     5,     5,    17,     7,     7,     7,     7,     7,     9,     9,
     5,     5,     7,     5,     5,     5,    17,     7,     7,     7,
     7,     7,     9,     9,     5,     5,     5,     7,     5,     5,
     9,     7,     7,     7,     7,     7,     7,     9,     7,     7,
     7,     7,     9,     7,     9,     9,    17,     7,     7,     7,
     7,    17,     7,     7,     7,     7,     5,     7,     7,     7,
     7,     5,     7,     7,     7,     7,     7,     9,     9,     5,
     7,     7,     7,     7,     7,     7,     9,    11,    11,     9,
     9,     9,     9,     7,     7,     9,     7,     7,     7,     7,
     9,     5,     7,     7,     7,     7,     7,     9,     5,     7,
     9,     9,     7,     7,     7,     7,    11,    13,    13,     9,
     9,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     2,     2,     2,     2,     2,     2,     4,
     3,     5,     5,     5,     5,     5,     5,     7,     7,     7,
     7,     7,     5,     5,     7,     7,     5,     7,     7,     7,
     7,     7,     9,     9,     9,     5,     2,     4,     2,     4,
     6,     4,     2,     2,     2,     2,     2,     2,     4,     4,
     4,     3,     5,     5,     5,     5,     7,     5,     4,     4,
     4,     4,     3,     2,     4,     3,     5,     5,     5,     5,
     5,     5,     5,     5,     5,     5,     3,     5,     5,     7,
     7,     5,     5,     7,     7,     7,     7,     7,     7,     7,
     7,     5,     5,     5,     5,     7,     2,     4,     3,     5,
     5,     5,     5,     5,     5,     5,     5,     5,     5,     5,
     5,     5,     5,     5,     5,     5,     5,     7,     7,     7,
     7,     5,     5,     7,     5,     5,     5,     7,     3,     5,
     5,     5,     5,     9,     5,     7,     5,     5,     5,     3,
     5,     5,     5,     7,     3,     3,     5,     5,     5,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     3,     3,     3,     3,     3,     3,    13,    13,
    17,    21,    11,     7,     7,    11,    15,     7,     7,    11,
    15,     3,     3,    11,     7,     3,     7,     7,     7,     7,
     7,     7,     7,     7,     7,     7,     7,     7,     7,     7,
     7,     7,     7,     7,     7,     7,     9,     9,     9,     9,
     9,    13,     9,     9,     3,     3,     3,     9,     9,     9,
     9,    13,    13,    13,    13,     3,     3,     9,     9,    13,
    13,     9,     9,     9,     9,    17,    13,    13,    13,     9,
     9,     3,     3,     3,    13,    13,    13,    13,     9,     9,
     9,     9,     7,     7,     7,     7,     7,     7,     7,     7,
     5,     7,     5,     2,     3,     9,     9,     7,    13,     7,
     7,    13,    17,     7,     7,     7,     7,     7,     7,     9,
     9,     9,     9,    13,     9,     9,     3,     3,     3,     9,
     9,     9,     9,    13,     7,     7,     7,     7,     7,     7,
     7,     7,     3,     3,     3,     3,     3,     3,     3,     3,
     3,     3,     9,     9,    13,     9,     9,     9,     9,    17,
    13,    13,    13,     9,     9,     3,     3,     3,    13,     9,
     9,     9,     9,    11,    15,     7,     7,     7,     7,     7,
     7,     7,     7,     5,     7,     5 };
yytabelem yychk[]={

 -1000,   -31,    -3,    -4,    -5,    -1,    -2,   -24,   -17,   -32,
   -33,   -34,   256,   609,   608,   490,   381,   474,   612,   613,
   323,   257,   258,   259,   260,   261,   262,   263,   264,   265,
   266,   267,   268,   269,   270,   271,   296,   297,   298,   299,
   300,   301,   302,   303,   272,   290,   273,   274,   275,   276,
   277,   278,   279,   280,   281,   282,   283,   284,   285,   286,
   287,   288,   289,   291,   292,   293,   294,   393,   623,    40,
    45,   472,   320,   358,   327,   340,   363,   445,   418,   305,
   377,   326,   512,   495,   304,   308,   379,   470,   527,   309,
   343,   430,   404,   371,   419,   473,   -28,   -29,   392,   351,
   321,   420,   372,   497,   521,   471,   468,   469,   344,   501,
   338,   333,   414,   510,   347,   528,   526,   517,   511,   -21,
   382,   410,   378,   -36,   -38,   -20,   462,   349,   385,   481,
   483,   482,   485,   484,   350,   366,   401,   402,   536,   564,
   313,   314,   568,   569,   325,   535,   563,   491,    10,    10,
    10,    10,    43,    45,    42,    47,    94,    37,   616,   617,
   618,   619,   620,   621,   615,   614,    10,    43,    45,    42,
    47,    94,   616,   617,   618,   619,   620,   621,   615,   614,
    10,    10,    10,    10,    10,    10,    91,    61,    91,    61,
   457,    46,    46,    -6,   405,   321,   388,   521,   370,   281,
   491,   441,   438,    -6,   521,   341,   415,   417,   414,   540,
    46,    40,    40,    40,    40,    40,    40,    40,    40,    40,
    40,    40,    40,    40,    40,    40,    40,    40,    40,    40,
    40,    40,    40,    40,    40,    40,    40,    40,    40,    40,
    40,    40,    40,    -2,    -1,   609,   608,   381,   490,   273,
   474,    -2,    -1,    -2,    -1,   472,   337,   341,   612,   -15,
   612,   398,   522,   306,   478,   409,   395,   444,   474,   445,
    -8,   520,   365,    -8,    -8,   612,   612,    -8,   337,   337,
   381,   490,   333,   414,   510,   490,   490,   381,   490,   381,
   490,   491,   381,   382,   609,    40,    40,    40,    40,    40,
   535,   563,   490,    -6,   381,    -6,   489,   373,   339,   -26,
   352,   455,   -27,   540,   541,   542,   544,   545,   546,   547,
   548,   549,   550,   551,   552,   553,   554,   555,   556,   557,
   437,   330,   337,   329,   -27,   612,   528,   501,   333,   414,
   510,    -6,   381,    -1,   421,   415,   417,   341,   368,   346,
   609,   608,   490,   257,   258,   259,   260,   261,   262,   263,
   264,   265,   266,   267,   268,   269,   270,   271,   272,   290,
   273,   274,   275,   276,   277,   278,   279,   280,   281,   282,
   283,   284,   285,   286,   287,   288,   289,   291,   292,   293,
   294,   623,    40,    45,    -6,   381,    -1,   421,   417,   415,
   341,   319,   346,    -6,   381,    -1,   421,   417,   341,   480,
   374,   403,   336,   346,   415,   417,   341,   336,   374,   513,
    -1,   539,   538,   567,   566,   381,    -1,   539,   538,   567,
   566,   337,   374,   493,   341,   417,   337,   374,   493,   341,
   417,   -22,   -23,   415,   417,   374,   341,   493,   336,   378,
   410,   281,   -36,   -38,   491,   490,    -6,   421,   406,   525,
   387,   411,   333,    -1,   530,   558,   336,   374,   415,   417,
   341,   510,    -6,   521,   415,   417,   341,   368,   327,   -37,
   514,   515,   405,   328,    -6,   -35,    -6,   521,   429,   374,
   460,   376,   513,   -22,   368,   494,   362,   556,   342,   -16,
   518,   465,   467,   427,   426,   391,   390,   385,   367,   612,
   349,   -39,    -6,   341,   417,   415,   374,   336,   384,   -39,
   -39,    -1,    -2,    -1,    -2,    -1,    -2,    -1,    -2,    -1,
    -2,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -2,    -1,    -2,    -1,    -2,    -1,    -2,    -1,    -1,    -2,
    -2,    -1,    -2,    -2,    -2,    -2,    -2,    -2,    -2,    -1,
    -2,    -1,    -1,    -2,    -1,    -1,   608,   -30,   411,   539,
   538,   567,   566,   490,   296,   297,   298,   299,   300,   301,
   302,   303,    -6,   521,    -8,    -8,   -19,   540,   422,   423,
   424,   328,   386,   502,   503,   458,   543,    -6,   376,   460,
   540,   521,   491,   -18,   570,   571,   409,   478,   572,   573,
   612,   612,   612,    -1,    -1,   608,   490,    -1,    -2,    -2,
    -1,    -2,    -1,    -2,    -1,    -2,    -1,    -2,    -1,    -2,
    -1,    -2,    -1,    -2,    -1,    -2,    -1,    -2,    -1,    -2,
    -1,   490,    -2,    -1,    -2,    -1,    -1,    -2,   612,    -2,
    -1,    -2,    -1,    -2,    -1,    -2,    -1,    -2,    -1,    -2,
    -1,    -2,    -1,    -2,    -1,    -2,    -1,    -2,    -1,    -2,
    -1,    -2,    -1,    -2,    -1,    -2,    -1,    -1,    91,    91,
    46,    46,    40,    46,    41,    41,    -6,   612,    44,   612,
   518,    -6,   612,   612,   612,   612,   518,    46,   518,    46,
   612,   490,   490,   490,   490,   490,   337,   337,   -26,    -1,
    46,    44,    43,    45,    42,    47,    94,    -9,   528,   526,
   612,   612,   612,   -25,   341,   450,   435,    46,    40,    40,
    40,    40,    40,    40,    40,    40,    40,    40,    40,    40,
    40,    40,    40,    40,    40,    40,    40,    40,    40,    40,
    40,    40,    40,    40,    40,    40,    40,    40,    -1,    -1,
    -1,    44,    -9,   612,   612,   612,   612,   493,   521,    44,
    -9,   612,   612,   612,   612,   612,   493,   337,   612,   612,
   612,   493,   612,   497,   493,    44,    -1,    -1,    -1,    -1,
    44,   612,   612,   612,   612,   612,   612,   612,   612,   612,
   612,   612,   612,   612,   -22,   491,   -37,    -9,   612,   612,
   612,   612,    -6,   368,   341,   415,   417,    44,    -1,    -1,
   493,   612,   612,   612,   612,   612,   612,   612,   612,   612,
    -6,   341,   -40,   -44,    -6,   425,   428,   439,   440,   311,
   282,   281,   347,   -11,   279,   493,   341,   417,   442,   521,
   498,   612,   395,   444,   331,   -41,   -45,    -6,   521,   460,
   376,   317,   461,   406,   315,   403,   494,   505,   442,   492,
   506,   509,   525,   387,   336,   374,   341,   417,   612,   -42,
   337,   406,   456,   403,   336,   374,   341,   417,   -43,    -6,
   341,   415,   417,   -27,    -1,   612,   612,   -10,   345,   364,
   459,   574,   575,   576,   577,   578,   579,   580,   581,   582,
   583,   584,   585,   586,   587,   588,   589,   590,   591,   592,
   593,   594,   595,   596,   612,   368,   335,   493,   336,   494,
   612,   612,   527,    -7,   341,   450,   612,   411,   521,   417,
   415,   479,    -1,   337,   337,   -16,   612,   612,   612,   612,
   612,   493,    -6,    93,    93,    44,    91,    61,   457,    46,
   320,   498,   -10,   612,    -1,   612,   612,    44,    44,    61,
    44,    41,    41,    41,    41,    41,    41,    41,    41,    44,
    44,    41,    41,    41,    41,    41,    41,    41,    41,    41,
    41,    41,    41,    44,    44,    41,    41,    41,    41,    41,
    44,    44,    41,    41,    41,    41,    41,    41,    41,    44,
    44,    44,    44,    44,    44,    44,    44,    41,    41,    41,
    41,    44,    44,    41,    41,    41,    41,    41,    41,    41,
    41,    41,    -1,    -1,   490,   608,   608,   612,   381,   490,
   490,   490,   490,    44,    44,    44,    44,    44,    44,   337,
    44,   490,    -1,    -1,    -1,    -1,    -1,    -1,   612,   612,
   608,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,   612,    -1,    -1,    -1,
    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    -1,    -1,   612,   612,    -1,   612,   612,   612,   612,    -1,
    -1,   612,   612,    -6,   527,    -7,   612,   612,   612,    -1,
   612,   337,   612,   -44,    -6,    -1,   493,   341,   417,   415,
   384,    -6,    -1,   493,   341,   417,   415,   384,    -1,    -1,
    -6,    -1,    -1,   612,    -6,   612,   612,   612,   -14,   519,
   332,   409,   478,   331,   320,   498,   612,    44,   -45,   320,
   498,   612,   -10,   612,   337,   337,   389,   524,   498,   612,
   -13,   478,   409,   335,   612,   612,   -14,   -12,   436,   307,
   433,    -1,   521,    -1,   521,   612,   612,   493,   612,   612,
   612,    44,   452,   446,   320,   498,   612,   -13,   493,   612,
   612,   612,   612,   612,   612,   612,    -8,   612,   612,   612,
    -7,   612,   612,   -14,   612,   612,    -6,   417,   415,    44,
   337,   612,    61,    61,    -1,    -1,    -2,    -1,    -1,   608,
   -10,    44,    44,    -1,    -1,    -1,   608,    -2,    -1,    -2,
    -1,    -2,    -1,   612,   612,    -1,    -1,    -2,    -1,    -2,
    -1,    -2,    -1,    -2,    -1,    -2,    -1,    -1,    93,    93,
    46,    91,    44,   518,   518,    -1,   612,   612,   612,    -1,
   612,    -1,    46,    44,    41,    44,    44,    41,    44,    44,
    44,    44,    -7,   612,   612,   612,   612,   612,    -6,   612,
   612,   612,   612,    -6,    -1,   498,   320,   498,   320,   612,
   337,    44,   612,   612,   612,    -1,    -1,    -1,    93,    44,
    91,    61,   612,    -1,    44,    41,    41,    41,    41,    41,
    41,    41,    41,    41,    44,    44,    41,    41,    41,    41,
    41,    41,    41,    41,    41,    41,   360,   608,    -1,   612,
   381,   381,    44,    41,    41,    41,    44,    41,    44,   608,
    -1,    -1,   612,    -1,    -1,    -1,    -1,   612,    61,    -1,
    -1,    -2,    -1,    -1,    -1,    -1,    -1,    91,    93,    44,
    46,    46,    -1,    -1,    -1,    44,    41,    41,    41,    44,
    44,    44,    -1,    93,    44,    41,    41,    -1,   612,   490,
   490,    44,    44,    -1,    -1,    -1,    -1,    61,    -1,    93,
   612,    -1,    -1,    41,   514,    -1,    44,    -1,    44,    -1,
    44,    -1 };
yytabelem yydef[]={

     1,    -2,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,   481,   482,   384,   389,     0,   563,   564,
     0,     0,     0,     0,     0,     0,     0,     0,    -2,    -2,
    -2,     0,     0,     0,     0,     0,   602,   603,   604,   605,
   606,   607,   608,   609,    -2,    -2,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,    -2,
    -2,    -2,     0,     0,     0,     0,     0,     0,     0,     0,
     0,    35,     0,     0,     0,     0,    40,     0,     0,     0,
     0,     0,    48,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,    67,     0,     0,     0,     0,     0,     0,
    75,     0,     0,     0,     0,     0,    91,    92,    93,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
   390,     0,     0,   276,   278,     0,     0,     0,    18,   400,
   401,   402,   403,   404,   405,   406,   407,   408,   282,   283,
   284,   285,   286,   287,     0,     0,     0,   385,     2,     3,
     4,     5,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     6,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     7,     8,     9,    10,    11,    12,     0,     0,     0,     0,
     0,     0,     0,   198,     0,     0,     0,     0,     0,     0,
   386,   396,   397,    27,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,   560,    -2,   481,   482,     0,     0,     0,
     0,     0,     0,   562,    -2,     0,    37,     0,     0,    41,
    42,     0,   460,   461,   462,   463,   464,   465,     0,     0,
    45,   426,   427,    46,    47,    49,    50,    51,    52,    53,
    54,    55,    98,   111,   126,     0,     0,     0,     0,     0,
    63,    64,    65,    66,     0,     0,     0,     0,     0,     0,
    76,    77,    78,    79,    80,    81,    82,    83,    84,    85,
   409,   410,    86,   214,   215,   216,   217,   218,   219,   220,
   221,   222,   223,   224,   225,   226,   227,   228,   229,   230,
   231,   232,    87,     0,     0,    94,     0,    96,    97,   113,
   125,   100,   101,     0,     0,     0,     0,     0,     0,   110,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
   586,   587,   588,     0,     0,     0,     0,     0,   610,   611,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,   625,   626,   627,     0,     0,     0,     0,
     0,     0,     0,     0,   114,   115,     0,     0,     0,     0,
     0,     0,   124,   128,   129,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,   156,     0,     0,     0,     0,   161,     0,     0,     0,
     0,     0,     0,   391,   392,   393,   394,   395,   211,   212,
   213,     0,   279,   281,   387,   388,   169,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,   191,     0,     0,     0,     0,     0,     0,   277,
     0,     0,     0,     0,   302,   249,   250,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,    19,    20,    21,    22,    23,    24,    25,    26,    16,
     0,   288,   291,     0,     0,     0,     0,     0,     0,   289,
   290,    -2,   488,    -2,   492,    -2,   496,    -2,   500,    -2,
   503,   577,    -2,    -2,    -2,    -2,    -2,    -2,   641,   642,
   487,    -2,   491,    -2,   495,    -2,   499,    -2,    -2,   505,
    -2,   485,    -2,    -2,    -2,    -2,    -2,   558,   559,     0,
   473,   477,     0,   474,   478,     0,   484,   569,   570,   422,
   423,   424,   425,   383,   594,   595,   596,   597,   598,   599,
   600,   601,   199,     0,   202,   203,   204,   233,   234,   235,
   236,   237,   238,   239,   240,   241,   242,   205,     0,     0,
     0,     0,     0,    28,   243,   244,   245,   246,   247,   248,
    29,    30,    31,     0,     0,   567,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,   561,   644,    36,    38,     0,    43,
     0,    44,    99,   112,   127,    56,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,    88,    89,     0,     0,
     0,     0,     0,     0,     0,     0,     0,   103,   466,   467,
   104,   105,   106,   107,   412,   411,   413,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,   643,     0,
   645,     0,   117,   118,   119,   120,   121,     0,     0,     0,
   131,   132,   133,   134,   135,   136,     0,   138,   139,   140,
   141,     0,   143,     0,     0,     0,   147,   148,   149,   150,
     0,   152,   153,   154,   155,   157,   158,   159,   160,   162,
   163,   164,   165,   166,     0,     0,   280,   170,   171,   172,
   173,   174,   175,     0,     0,     0,     0,     0,   183,   184,
     0,   186,   187,   188,   189,     0,   192,   193,   194,   195,
   196,     0,   298,   303,   305,     0,     0,     0,     0,     0,
     0,     0,     0,   316,     0,     0,     0,     0,     0,     0,
     0,     0,   428,   429,   430,   299,   336,   338,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,   300,
   368,     0,     0,     0,     0,     0,     0,     0,   301,   379,
     0,     0,     0,   251,   252,   253,   254,   255,   431,   432,
   433,   434,   435,   436,   437,   438,   439,   440,   441,   442,
   443,   444,   445,   446,   447,   448,   449,   450,   451,   452,
   453,   454,   455,   456,   256,     0,     0,     0,     0,     0,
   262,   263,     0,     0,   398,   399,   266,     0,     0,     0,
     0,     0,     0,   275,    13,    15,    17,   292,   293,   294,
   295,     0,   297,   565,   566,     0,     0,     0,     0,     0,
   200,   201,     0,     0,     0,   209,   210,     0,     0,     0,
     0,    -2,   507,   508,   580,   509,   581,   510,   582,     0,
     0,   512,   584,   513,   585,   517,   589,   518,   590,   519,
   591,   520,   592,     0,     0,    74,   527,   612,   528,   613,
     0,     0,    -2,   532,   616,   533,   617,   534,   618,     0,
     0,     0,     0,     0,     0,     0,     0,   539,   623,   540,
   624,     0,     0,   548,   629,   549,   630,   550,   631,   551,
   632,     0,     0,     0,     0,   484,   567,     0,    34,    59,
     0,    61,     0,     0,     0,     0,     0,     0,     0,    90,
     0,     0,     0,   573,   574,   575,   576,   578,   108,   109,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,   122,   123,   130,   137,   142,   144,   145,     0,
     0,   167,   168,   176,     0,     0,   179,   180,   181,   182,
   185,   190,   197,   304,   306,   308,     0,     0,     0,     0,
     0,   307,   309,     0,     0,     0,     0,     0,   310,   311,
   312,   313,   314,   315,   317,   318,   321,   322,   331,   414,
   415,   416,   417,   418,   332,   333,   334,     0,   337,   339,
   340,   341,   342,   343,   344,   345,   346,   347,   348,   349,
   350,   419,   420,   421,   351,   352,   353,   354,   457,   458,
   459,   355,     0,   356,     0,   361,   362,     0,   364,   365,
   366,     0,   369,   370,   371,   372,     0,   374,     0,   376,
   377,   378,   380,   381,   382,   257,   258,   259,   260,   261,
   264,   265,   267,   268,   269,   270,   271,     0,     0,     0,
    14,   296,     0,     0,    57,     0,   475,   479,     0,   483,
   206,     0,     0,     0,    33,   472,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,     0,     0,     0,   633,   565,   566,
     0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     0,     0,     0,     0,   579,     0,     0,   615,     0,     0,
     0,     0,   177,   178,   319,   323,   325,   327,   329,   320,
   324,   326,   328,   330,   335,   357,   358,   359,   360,   363,
   367,     0,   375,   272,   273,   274,   468,   469,   568,     0,
     0,     0,   207,   208,     0,   571,   511,   583,   521,   523,
   522,    -2,    -2,   530,     0,     0,   536,   620,   537,   621,
   538,   622,   544,   546,   545,    -2,     0,   483,     0,     0,
     0,     0,     0,    69,    70,    71,     0,    73,     0,     0,
     0,     0,     0,     0,     0,     0,     0,   373,     0,    58,
     0,   476,   480,     0,     0,     0,   634,     0,   568,     0,
     0,     0,    68,     0,     0,     0,   593,   614,   628,     0,
     0,     0,   470,   572,     0,   535,   619,     0,    39,    60,
    62,     0,     0,   102,   116,   146,   151,     0,    32,   572,
     0,     0,   471,    72,     0,     0,     0,     0,     0,     0,
     0,    95 };
typedef struct { char *t_name; int t_val; } yytoktype;
#ifndef YYDEBUG
#	define YYDEBUG	1	/* allow debugging */
#endif

#if YYDEBUG

yytoktype yytoks[] =
{
	"ABS",	257,
	"ACOS",	258,
	"ASIN",	259,
	"ATAN",	260,
	"ATAN2",	261,
	"CEIL",	262,
	"COS",	263,
	"DEG",	264,
	"DX",	265,
	"DY",	266,
	"ERF",	267,
	"ERFC",	268,
	"EXP",	269,
	"FLOOR",	270,
	"HYPOT",	271,
	"INDEX",	272,
	"INT",	273,
	"INVN",	274,
	"INVT",	275,
	"IRAND",	276,
	"LGAMMA",	277,
	"LN",	278,
	"LOG",	279,
	"LOGISTIC",	280,
	"MAXP",	281,
	"MINP",	282,
	"MOD",	283,
	"NORM",	284,
	"NORMP",	285,
	"PI",	286,
	"RAD",	287,
	"RAND",	288,
	"RNORM",	289,
	"SETNO",	290,
	"SIN",	291,
	"SQR",	292,
	"SQRT",	293,
	"TAN",	294,
	"INUM",	295,
	"VX1",	296,
	"VX2",	297,
	"VY1",	298,
	"VY2",	299,
	"WX1",	300,
	"WX2",	301,
	"WY1",	302,
	"WY2",	303,
	"DELAYP",	304,
	"DOUBLEBUFFER",	305,
	"DOWN",	306,
	"ABSOLUTE",	307,
	"ABORT",	308,
	"ACTIVATE",	309,
	"ACTIVE",	310,
	"ALT",	311,
	"ALTERNATE",	312,
	"ALTXAXIS",	313,
	"ALTYAXIS",	314,
	"ANGLE",	315,
	"ANNOTATE",	316,
	"APPEND",	317,
	"AREA",	318,
	"ARROW",	319,
	"AUTO",	320,
	"AUTOSCALE",	321,
	"AUTOTICKS",	322,
	"AVG",	323,
	"AXIS",	324,
	"AXES",	325,
	"BACKBUFFER",	326,
	"BACKGROUND",	327,
	"BAR",	328,
	"BATCH",	329,
	"BIN",	330,
	"BOTH",	331,
	"BOTTOM",	332,
	"BOX",	333,
	"CELLS",	334,
	"CENTER",	335,
	"CHAR",	336,
	"CHRSTR",	337,
	"CLEAR",	338,
	"CLICK",	339,
	"CMAP",	340,
	"COLOR",	341,
	"COMMENT",	342,
	"COPY",	343,
	"CYCLE",	344,
	"DECIMAL",	345,
	"DEF",	346,
	"DEFAULT",	347,
	"DELETE",	348,
	"DEVICE",	349,
	"DFT",	350,
	"DIFFERENCE",	351,
	"DISK",	352,
	"DRAW2",	353,
	"DXDX",	354,
	"DXP",	355,
	"DYDY",	356,
	"DYP",	357,
	"ECHO",	358,
	"EDIT",	359,
	"ELSE",	360,
	"END",	361,
	"ERRORBAR",	362,
	"EXIT",	363,
	"EXPONENTIAL",	364,
	"FALSEP",	365,
	"FFT",	366,
	"FILEP",	367,
	"FILL",	368,
	"FIND",	369,
	"FIXEDPOINT",	370,
	"FLUSH",	371,
	"FOCUS",	372,
	"FOLLOWS",	373,
	"FONTP",	374,
	"FOREGROUND",	375,
	"FORMAT",	376,
	"FRONTBUFFER",	377,
	"FRAMEP",	378,
	"GETP",	379,
	"GRAPH",	380,
	"GRAPHNO",	381,
	"GRAPHS",	382,
	"GRAPHTYPE",	383,
	"GRID",	384,
	"HARDCOPY",	385,
	"HBAR",	386,
	"HGAP",	387,
	"HIDDEN",	388,
	"HORIZONTAL",	389,
	"HPGLL",	390,
	"HPGLP",	391,
	"HISTO",	392,
	"IF",	393,
	"IHL",	394,
	"IN",	395,
	"INIT",	396,
	"INITGRAPHICS",	397,
	"INOUT",	398,
	"INTEGRATE",	399,
	"INTERP",	400,
	"INVDFT",	401,
	"INVFFT",	402,
	"JUST",	403,
	"KILL",	404,
	"LABEL",	405,
	"LAYOUT",	406,
	"LEAVE",	407,
	"LEAVEGRAPHICS",	408,
	"LEFT",	409,
	"LEGEND",	410,
	"LENGTH",	411,
	"LEVEL",	412,
	"LEVELS",	413,
	"LINE",	414,
	"LINESTYLE",	415,
	"LINETO",	416,
	"LINEWIDTH",	417,
	"LINK",	418,
	"LOAD",	419,
	"LOCATOR",	420,
	"LOCTYPE",	421,
	"LOGX",	422,
	"LOGY",	423,
	"LOGXY",	424,
	"MAJOR",	425,
	"MIFL",	426,
	"MIFP",	427,
	"MINOR",	428,
	"MISSINGP",	429,
	"MOVE",	430,
	"MOVE2",	431,
	"MOVETO",	432,
	"NEGATE",	433,
	"NO",	434,
	"NONE",	435,
	"NORMAL",	436,
	"NXY",	437,
	"OFF",	438,
	"OFFSETX",	439,
	"OFFSETY",	440,
	"ON",	441,
	"OP",	442,
	"ORIENT",	443,
	"OUT",	444,
	"PAGE",	445,
	"PARA",	446,
	"PARALLEL",	447,
	"PARAMETERS",	448,
	"PARAMS",	449,
	"PATTERN",	450,
	"PERIMETER",	451,
	"PERP",	452,
	"PERPENDICULAR",	453,
	"PIE",	454,
	"PIPE",	455,
	"PLACE",	456,
	"POINT",	457,
	"POLAR",	458,
	"POWER",	459,
	"PREC",	460,
	"PREPEND",	461,
	"PRINT",	462,
	"PS",	463,
	"PSCOLORP",	464,
	"PSMONOP",	465,
	"PSCOLORL",	466,
	"PSMONOL",	467,
	"PUSH",	468,
	"POP",	469,
	"PUTP",	470,
	"READ",	471,
	"REDRAW",	472,
	"REGRESS",	473,
	"REGNUM",	474,
	"REGIONS",	475,
	"RENDER",	476,
	"REVERSE",	477,
	"RIGHT",	478,
	"RISER",	479,
	"ROT",	480,
	"RUNAVG",	481,
	"RUNMED",	482,
	"RUNSTD",	483,
	"RUNMIN",	484,
	"RUNMAX",	485,
	"SAMPLE",	486,
	"SCALE",	487,
	"SCIENTIFIC",	488,
	"SET",	489,
	"SETNUM",	490,
	"SETS",	491,
	"SIGN",	492,
	"SIZE",	493,
	"SKIP",	494,
	"SLEEP",	495,
	"SLICE",	496,
	"SOURCE",	497,
	"SPEC",	498,
	"SPECIFIED",	499,
	"SPECTRUM",	500,
	"STACK",	501,
	"STACKEDBAR",	502,
	"STACKEDHBAR",	503,
	"STACKEDLINE",	504,
	"STAGGER",	505,
	"START",	506,
	"STARTTYPE",	507,
	"STATUS",	508,
	"STOP",	509,
	"STRING",	510,
	"SUBTITLE",	511,
	"SWAPBUFFER",	512,
	"SYMBOL",	513,
	"TICK",	514,
	"TICKLABEL",	515,
	"TICKMARKS",	516,
	"TITLE",	517,
	"TO",	518,
	"TOP",	519,
	"TRUEP",	520,
	"TYPE",	521,
	"UP",	522,
	"VELOCITY",	523,
	"VERTICAL",	524,
	"VGAP",	525,
	"VIEW",	526,
	"WITH",	527,
	"WORLD",	528,
	"WRITE",	529,
	"X1",	530,
	"X2",	531,
	"X3",	532,
	"X4",	533,
	"X5",	534,
	"XAXES",	535,
	"XAXIS",	536,
	"XCOR",	537,
	"XMAX",	538,
	"XMIN",	539,
	"XY",	540,
	"XYARC",	541,
	"XYBOX",	542,
	"XYFIXED",	543,
	"XYHILO",	544,
	"XYRT",	545,
	"XYSEG",	546,
	"XYSTRING",	547,
	"XYDX",	548,
	"XYDY",	549,
	"XYDXDX",	550,
	"XYDYDY",	551,
	"XYDXDY",	552,
	"XYX2Y2",	553,
	"XYXX",	554,
	"XYYY",	555,
	"XYZ",	556,
	"XYZW",	557,
	"Y1",	558,
	"Y2",	559,
	"Y3",	560,
	"Y4",	561,
	"Y5",	562,
	"YAXES",	563,
	"YAXIS",	564,
	"YES",	565,
	"YMAX",	566,
	"YMIN",	567,
	"ZEROXAXIS",	568,
	"ZEROYAXIS",	569,
	"ABOVE",	570,
	"BELOW",	571,
	"POLYI",	572,
	"POLYO",	573,
	"GENERAL",	574,
	"DDMMYY",	575,
	"MMDDYY",	576,
	"MMYY",	577,
	"MMDD",	578,
	"MONTHDAY",	579,
	"DAYMONTH",	580,
	"MONTHS",	581,
	"MONTHL",	582,
	"DAYOFWEEKS",	583,
	"DAYOFWEEKL",	584,
	"DAYOFYEAR",	585,
	"HMS",	586,
	"MMDDHMS",	587,
	"MMDDYYHMS",	588,
	"DEGREESLON",	589,
	"DEGREESMMLON",	590,
	"DEGREESMMSSLON",	591,
	"MMSSLON",	592,
	"DEGREESLAT",	593,
	"DEGREESMMLAT",	594,
	"DEGREESMMSSLAT",	595,
	"MMSSLAT",	596,
	"DOT",	597,
	"STAR",	598,
	"PLUS",	599,
	"CROSS",	600,
	"CIRCLE",	601,
	"SQUARE",	602,
	"DIAMOND",	603,
	"TRIANGLE1",	604,
	"TRIANGLE2",	605,
	"TRIANGLE3",	606,
	"TRIANGLE4",	607,
	"SVAR",	608,
	"VAR",	609,
	"X",	610,
	"Y",	611,
	"NUMBER",	612,
	"FITPARM",	613,
	"=",	61,
	"OR",	614,
	"AND",	615,
	"GT",	616,
	"LT",	617,
	"LE",	618,
	"GE",	619,
	"EQ",	620,
	"NE",	621,
	"+",	43,
	"-",	45,
	"*",	42,
	"/",	47,
	"%",	37,
	"^",	94,
	"UMINUS",	622,
	"NOT",	623,
	"-unknown-",	-1	/* ends search */
};

char * yyreds[] =
{
	"-no such reduction-",
      "list : /* empty */",
      "list : asgn '\n'",
      "list : vasgn '\n'",
      "list : rasgn '\n'",
      "list : expr '\n'",
      "list : vexpr '\n'",
      "list : parmset '\n'",
      "list : regionset '\n'",
      "list : setaxis '\n'",
      "list : set_setprop '\n'",
      "list : setprint '\n'",
      "list : error '\n'",
      "setprint : PRINT printer CHRSTR",
      "setprint : PRINT TO printer CHRSTR",
      "setprint : PRINT TO printer",
      "setprint : DEVICE NUMBER",
      "setprint : HARDCOPY DEVICE NUMBER",
      "setprint : HARDCOPY",
      "printer : PSMONOP",
      "printer : PSMONOL",
      "printer : MIFP",
      "printer : MIFL",
      "printer : HPGLP",
      "printer : HPGLL",
      "printer : HARDCOPY",
      "printer : FILEP",
      "regionset : REGNUM onoff",
      "regionset : REGNUM TYPE regiontype",
      "regionset : REGNUM COLOR NUMBER",
      "regionset : REGNUM LINESTYLE NUMBER",
      "regionset : REGNUM LINEWIDTH NUMBER",
      "regionset : REGNUM LINE expr ',' expr ',' expr ',' expr",
      "regionset : REGNUM XY expr ',' expr",
      "regionset : LINK REGNUM TO GRAPHNO",
      "parmset : REDRAW",
      "parmset : AUTO REDRAW onoff",
      "parmset : ECHO CHRSTR",
      "parmset : BACKGROUND COLOR NUMBER",
      "parmset : CMAP NUMBER ',' NUMBER ',' NUMBER ',' NUMBER",
      "parmset : EXIT",
      "parmset : PAGE direction",
      "parmset : PAGE NUMBER",
      "parmset : PAGE INOUT NUMBER",
      "parmset : LINK PAGE onoff",
      "parmset : DOUBLEBUFFER torf",
      "parmset : FRONTBUFFER torf",
      "parmset : BACKBUFFER torf",
      "parmset : SWAPBUFFER",
      "parmset : SLEEP NUMBER",
      "parmset : DELAYP NUMBER",
      "parmset : ABORT torf",
      "parmset : GETP CHRSTR",
      "parmset : PUTP CHRSTR",
      "parmset : WITH GRAPHNO",
      "parmset : WITH SETNUM",
      "parmset : ACTIVATE SETNUM NUMBER",
      "parmset : SETNUM POINT expr ',' expr",
      "parmset : GRAPHNO '.' SETNUM POINT expr ',' expr",
      "parmset : COPY SETNUM TO SETNUM",
      "parmset : COPY GRAPHNO '.' SETNUM TO GRAPHNO '.' SETNUM",
      "parmset : MOVE SETNUM TO SETNUM",
      "parmset : MOVE GRAPHNO '.' SETNUM TO GRAPHNO '.' SETNUM",
      "parmset : KILL SETNUM",
      "parmset : KILL SETS",
      "parmset : KILL GRAPHNO",
      "parmset : KILL GRAPHS",
      "parmset : FLUSH",
      "parmset : LOAD VAR NUMBER ',' expr ',' expr",
      "parmset : REGRESS '(' SETNUM ',' NUMBER ')'",
      "parmset : runtype '(' SETNUM ',' NUMBER ')'",
      "parmset : ffttype '(' SETNUM ',' NUMBER ')'",
      "parmset : HISTO '(' SETNUM ',' expr ',' expr ',' NUMBER ')'",
      "parmset : DIFFERENCE '(' SETNUM ',' NUMBER ')'",
      "parmset : INT '(' SETNUM ')'",
      "parmset : AUTOSCALE",
      "parmset : AUTOSCALE XAXES",
      "parmset : AUTOSCALE YAXES",
      "parmset : AUTOSCALE SETNUM",
      "parmset : LOCATOR onoff",
      "parmset : FOCUS GRAPHNO",
      "parmset : FOCUS onoff",
      "parmset : FOCUS SET",
      "parmset : FOCUS FOLLOWS",
      "parmset : FOCUS CLICK",
      "parmset : SOURCE sourcetype",
      "parmset : TYPE xytype",
      "parmset : READ CHRSTR",
      "parmset : READ BATCH CHRSTR",
      "parmset : READ xytype CHRSTR",
      "parmset : READ xytype sourcetype CHRSTR",
      "parmset : PUSH",
      "parmset : POP",
      "parmset : CYCLE",
      "parmset : STACK NUMBER",
      "parmset : STACK WORLD expr ',' expr ',' expr ',' expr TICK expr ',' expr ',' expr ',' expr",
      "parmset : CLEAR STACK",
      "parmset : CLEAR BOX",
      "parmset : WITH BOX",
      "parmset : WITH BOX NUMBER",
      "parmset : BOX onoff",
      "parmset : BOX GRAPHNO",
      "parmset : BOX expr ',' expr ',' expr ',' expr",
      "parmset : BOX LOCTYPE worldview",
      "parmset : BOX LINESTYLE NUMBER",
      "parmset : BOX LINEWIDTH NUMBER",
      "parmset : BOX COLOR NUMBER",
      "parmset : BOX FILL filltype",
      "parmset : BOX FILL COLOR NUMBER",
      "parmset : BOX FILL PATTERN NUMBER",
      "parmset : BOX DEF",
      "parmset : WITH LINE",
      "parmset : WITH LINE NUMBER",
      "parmset : CLEAR LINE",
      "parmset : LINE onoff",
      "parmset : LINE GRAPHNO",
      "parmset : LINE expr ',' expr ',' expr ',' expr",
      "parmset : LINE LOCTYPE worldview",
      "parmset : LINE LINEWIDTH NUMBER",
      "parmset : LINE LINESTYLE NUMBER",
      "parmset : LINE COLOR NUMBER",
      "parmset : LINE ARROW NUMBER",
      "parmset : LINE ARROW SIZE NUMBER",
      "parmset : LINE ARROW TYPE NUMBER",
      "parmset : LINE DEF",
      "parmset : CLEAR STRING",
      "parmset : WITH STRING",
      "parmset : WITH STRING NUMBER",
      "parmset : STRING onoff",
      "parmset : STRING GRAPHNO",
      "parmset : STRING expr ',' expr",
      "parmset : STRING LOCTYPE worldview",
      "parmset : STRING LINEWIDTH NUMBER",
      "parmset : STRING COLOR NUMBER",
      "parmset : STRING ROT NUMBER",
      "parmset : STRING FONTP NUMBER",
      "parmset : STRING JUST NUMBER",
      "parmset : STRING CHAR SIZE NUMBER",
      "parmset : STRING DEF CHRSTR",
      "parmset : DEFAULT LINESTYLE NUMBER",
      "parmset : DEFAULT LINEWIDTH NUMBER",
      "parmset : DEFAULT COLOR NUMBER",
      "parmset : DEFAULT CHAR SIZE NUMBER",
      "parmset : DEFAULT FONTP NUMBER",
      "parmset : DEFAULT FONTP SOURCE NUMBER",
      "parmset : DEFAULT SYMBOL SIZE NUMBER",
      "parmset : WORLD expr ',' expr ',' expr ',' expr",
      "parmset : WORLD XMIN expr",
      "parmset : WORLD XMAX expr",
      "parmset : WORLD YMIN expr",
      "parmset : WORLD YMAX expr",
      "parmset : VIEW expr ',' expr ',' expr ',' expr",
      "parmset : VIEW XMIN NUMBER",
      "parmset : VIEW XMAX NUMBER",
      "parmset : VIEW YMIN NUMBER",
      "parmset : VIEW YMAX NUMBER",
      "parmset : TITLE CHRSTR",
      "parmset : TITLE FONTP NUMBER",
      "parmset : TITLE SIZE NUMBER",
      "parmset : TITLE COLOR NUMBER",
      "parmset : TITLE LINEWIDTH NUMBER",
      "parmset : SUBTITLE CHRSTR",
      "parmset : SUBTITLE FONTP NUMBER",
      "parmset : SUBTITLE SIZE NUMBER",
      "parmset : SUBTITLE COLOR NUMBER",
      "parmset : SUBTITLE LINEWIDTH NUMBER",
      "parmset : selectgraphs prop NUMBER",
      "parmset : selectgraphs thing prop NUMBER",
      "parmset : GRAPHS MAXP SETS NUMBER",
      "parmset : LEGEND onoff",
      "parmset : LEGEND LOCTYPE worldview",
      "parmset : LEGEND LAYOUT NUMBER",
      "parmset : LEGEND VGAP NUMBER",
      "parmset : LEGEND HGAP NUMBER",
      "parmset : LEGEND LENGTH NUMBER",
      "parmset : LEGEND BOX onoff",
      "parmset : LEGEND BOX FILL onoff",
      "parmset : LEGEND BOX FILL WITH colpat",
      "parmset : LEGEND BOX FILL colpat NUMBER",
      "parmset : LEGEND BOX COLOR NUMBER",
      "parmset : LEGEND BOX LINESTYLE NUMBER",
      "parmset : LEGEND BOX LINEWIDTH NUMBER",
      "parmset : LEGEND expr ',' expr",
      "parmset : LEGEND X1 expr",
      "parmset : LEGEND Y1 expr",
      "parmset : LEGEND CHAR SIZE NUMBER",
      "parmset : LEGEND FONTP NUMBER",
      "parmset : LEGEND LINESTYLE NUMBER",
      "parmset : LEGEND LINEWIDTH NUMBER",
      "parmset : LEGEND COLOR NUMBER",
      "parmset : LEGEND STRING NUMBER CHRSTR",
      "parmset : FRAMEP onoff",
      "parmset : FRAMEP TYPE NUMBER",
      "parmset : FRAMEP LINESTYLE NUMBER",
      "parmset : FRAMEP LINEWIDTH NUMBER",
      "parmset : FRAMEP COLOR NUMBER",
      "parmset : FRAMEP FILL onoff",
      "parmset : FRAMEP BACKGROUND COLOR NUMBER",
      "parmset : GRAPHNO onoff",
      "parmset : GRAPHNO LABEL onoff",
      "parmset : GRAPHNO AUTOSCALE TYPE AUTO",
      "parmset : GRAPHNO AUTOSCALE TYPE SPEC",
      "parmset : GRAPHNO AUTOSCALE torf",
      "parmset : GRAPHNO HIDDEN torf",
      "parmset : GRAPHNO TYPE graphtype",
      "parmset : GRAPHNO FIXEDPOINT onoff",
      "parmset : GRAPHNO FIXEDPOINT FORMAT formatchoice formatchoice",
      "parmset : GRAPHNO FIXEDPOINT PREC NUMBER ',' NUMBER",
      "parmset : GRAPHNO FIXEDPOINT XY expr ',' expr",
      "parmset : GRAPHNO FIXEDPOINT TYPE NUMBER",
      "parmset : GRAPHNO MAXP SETS NUMBER",
      "thing : CHAR",
      "thing : FRAMEP",
      "thing : LEGEND",
      "xytype : XY",
      "xytype : XYARC",
      "xytype : XYBOX",
      "xytype : XYHILO",
      "xytype : XYRT",
      "xytype : XYSEG",
      "xytype : XYSTRING",
      "xytype : XYDX",
      "xytype : XYDY",
      "xytype : XYDXDX",
      "xytype : XYDYDY",
      "xytype : XYDXDY",
      "xytype : XYX2Y2",
      "xytype : XYXX",
      "xytype : XYYY",
      "xytype : XYZ",
      "xytype : XYZW",
      "xytype : NXY",
      "xytype : BIN",
      "graphtype : XY",
      "graphtype : LOGX",
      "graphtype : LOGY",
      "graphtype : LOGXY",
      "graphtype : BAR",
      "graphtype : HBAR",
      "graphtype : STACKEDBAR",
      "graphtype : STACKEDHBAR",
      "graphtype : POLAR",
      "graphtype : XYFIXED",
      "regiontype : ABOVE",
      "regiontype : BELOW",
      "regiontype : LEFT",
      "regiontype : RIGHT",
      "regiontype : POLYI",
      "regiontype : POLYO",
      "set_setprop : selectsets setprop",
      "setprop : onoff",
      "setprop : TYPE xytype",
      "setprop : MISSINGP expr",
      "setprop : FONTP NUMBER",
      "setprop : PREC NUMBER",
      "setprop : FORMAT formatchoice",
      "setprop : SYMBOL NUMBER",
      "setprop : SYMBOL FILL NUMBER",
      "setprop : SYMBOL CENTER torf",
      "setprop : SYMBOL SIZE NUMBER",
      "setprop : SYMBOL CHAR NUMBER",
      "setprop : SYMBOL SKIP NUMBER",
      "setprop : prop NUMBER",
      "setprop : FILL NUMBER",
      "setprop : FILL WITH colpat",
      "setprop : FILL colpat NUMBER",
      "setprop : SKIP NUMBER",
      "setprop : ERRORBAR LENGTH NUMBER",
      "setprop : ERRORBAR TYPE opchoice",
      "setprop : ERRORBAR LINEWIDTH NUMBER",
      "setprop : ERRORBAR LINESTYLE NUMBER",
      "setprop : ERRORBAR RISER onoff",
      "setprop : ERRORBAR RISER LINEWIDTH NUMBER",
      "setprop : ERRORBAR RISER LINESTYLE NUMBER",
      "setprop : XYZ expr ',' expr",
      "setprop : COMMENT CHRSTR",
      "setaxis : axis",
      "setaxis : axis axisfeature",
      "setaxis : allaxes",
      "setaxis : GRAPHS axis",
      "setaxis : GRAPHS axis axisfeature",
      "setaxis : GRAPHS allaxes",
      "axis : XAXIS",
      "axis : YAXIS",
      "axis : ALTXAXIS",
      "axis : ALTYAXIS",
      "axis : ZEROXAXIS",
      "axis : ZEROYAXIS",
      "allaxes : AXES axesprops",
      "allaxes : XAXES axesprops",
      "allaxes : YAXES axesprops",
      "axesprops : onoff",
      "axesprops : COLOR NUMBER",
      "axesprops : LINEWIDTH NUMBER",
      "axesprops : LINESTYLE NUMBER",
      "axesprops : FONTP NUMBER",
      "axesprops : CHAR SIZE NUMBER",
      "axesprops : GRID onoff",
      "axisfeature : TICK tickdesc",
      "axisfeature : TICKLABEL ticklabeldesc",
      "axisfeature : LABEL axislabeldesc",
      "axisfeature : BAR axisbardesc",
      "axisfeature : onoff",
      "tickdesc : tickattr",
      "tickdesc : tickdesc tickattr",
      "tickattr : onoff",
      "tickattr : MAJOR onoff",
      "tickattr : MINOR onoff",
      "tickattr : MAJOR expr",
      "tickattr : MINOR expr",
      "tickattr : OFFSETX expr",
      "tickattr : OFFSETY expr",
      "tickattr : ALT onoff",
      "tickattr : MINP expr",
      "tickattr : MAXP expr",
      "tickattr : DEFAULT NUMBER",
      "tickattr : inoutchoice",
      "tickattr : LOG onoff",
      "tickattr : SIZE NUMBER",
      "tickattr : MAJOR SIZE NUMBER",
      "tickattr : MINOR SIZE NUMBER",
      "tickattr : COLOR NUMBER",
      "tickattr : LINEWIDTH NUMBER",
      "tickattr : MAJOR COLOR NUMBER",
      "tickattr : MINOR COLOR NUMBER",
      "tickattr : MAJOR LINEWIDTH NUMBER",
      "tickattr : MINOR LINEWIDTH NUMBER",
      "tickattr : MAJOR LINESTYLE NUMBER",
      "tickattr : MINOR LINESTYLE NUMBER",
      "tickattr : MAJOR GRID onoff",
      "tickattr : MINOR GRID onoff",
      "tickattr : OP opchoice",
      "tickattr : TYPE AUTO",
      "tickattr : TYPE SPEC",
      "tickattr : SPEC NUMBER",
      "tickattr : NUMBER ',' expr",
      "ticklabeldesc : ticklabelattr",
      "ticklabeldesc : ticklabeldesc ticklabelattr",
      "ticklabelattr : onoff",
      "ticklabelattr : TYPE AUTO",
      "ticklabelattr : TYPE SPEC",
      "ticklabelattr : PREC NUMBER",
      "ticklabelattr : FORMAT formatchoice",
      "ticklabelattr : FORMAT NUMBER",
      "ticklabelattr : APPEND CHRSTR",
      "ticklabelattr : PREPEND CHRSTR",
      "ticklabelattr : LAYOUT HORIZONTAL",
      "ticklabelattr : LAYOUT VERTICAL",
      "ticklabelattr : LAYOUT SPEC",
      "ticklabelattr : ANGLE NUMBER",
      "ticklabelattr : JUST justchoice",
      "ticklabelattr : SKIP NUMBER",
      "ticklabelattr : STAGGER NUMBER",
      "ticklabelattr : OP opchoice",
      "ticklabelattr : SIGN signchoice",
      "ticklabelattr : START expr",
      "ticklabelattr : STOP expr",
      "ticklabelattr : START TYPE SPEC",
      "ticklabelattr : START TYPE AUTO",
      "ticklabelattr : STOP TYPE SPEC",
      "ticklabelattr : STOP TYPE AUTO",
      "ticklabelattr : VGAP NUMBER",
      "ticklabelattr : HGAP NUMBER",
      "ticklabelattr : CHAR SIZE NUMBER",
      "ticklabelattr : FONTP NUMBER",
      "ticklabelattr : COLOR NUMBER",
      "ticklabelattr : LINEWIDTH NUMBER",
      "ticklabelattr : NUMBER ',' CHRSTR",
      "axislabeldesc : CHRSTR",
      "axislabeldesc : LAYOUT PERP",
      "axislabeldesc : LAYOUT PARA",
      "axislabeldesc : PLACE AUTO",
      "axislabeldesc : PLACE SPEC",
      "axislabeldesc : PLACE NUMBER ',' NUMBER",
      "axislabeldesc : JUST justchoice",
      "axislabeldesc : CHAR SIZE NUMBER",
      "axislabeldesc : FONTP NUMBER",
      "axislabeldesc : COLOR NUMBER",
      "axislabeldesc : LINEWIDTH NUMBER",
      "axisbardesc : onoff",
      "axisbardesc : COLOR NUMBER",
      "axisbardesc : LINESTYLE NUMBER",
      "axisbardesc : LINEWIDTH NUMBER",
      "selectsets : GRAPHNO '.' SETNUM",
      "selectsets : SETNUM",
      "selectsets : SETS",
      "selectsets : GRAPHNO SETS",
      "selectsets : GRAPHS SETS",
      "selectsets : GRAPHS SETNUM",
      "selectgraphs : GRAPHNO",
      "selectgraphs : GRAPHS",
      "prop : LINESTYLE",
      "prop : LINEWIDTH",
      "prop : FONTP",
      "prop : COLOR",
      "prop : SIZE",
      "onoff : ON",
      "onoff : OFF",
      "colpat : COLOR",
      "colpat : PATTERN",
      "runtype : RUNAVG",
      "runtype : RUNSTD",
      "runtype : RUNMED",
      "runtype : RUNMAX",
      "runtype : RUNMIN",
      "ffttype : DFT",
      "ffttype : FFT",
      "ffttype : INVDFT",
      "ffttype : INVFFT",
      "sourcetype : DISK",
      "sourcetype : PIPE",
      "filltype : PATTERN",
      "filltype : COLOR",
      "filltype : NONE",
      "opchoice : TOP",
      "opchoice : BOTTOM",
      "opchoice : LEFT",
      "opchoice : RIGHT",
      "opchoice : BOTH",
      "justchoice : RIGHT",
      "justchoice : LEFT",
      "justchoice : CENTER",
      "extremetype : XMIN",
      "extremetype : XMAX",
      "extremetype : YMIN",
      "extremetype : YMAX",
      "torf : TRUEP",
      "torf : FALSEP",
      "inoutchoice : IN",
      "inoutchoice : OUT",
      "inoutchoice : BOTH",
      "formatchoice : DECIMAL",
      "formatchoice : EXPONENTIAL",
      "formatchoice : POWER",
      "formatchoice : GENERAL",
      "formatchoice : DDMMYY",
      "formatchoice : MMDDYY",
      "formatchoice : MMYY",
      "formatchoice : MMDD",
      "formatchoice : MONTHDAY",
      "formatchoice : DAYMONTH",
      "formatchoice : MONTHS",
      "formatchoice : MONTHL",
      "formatchoice : DAYOFWEEKS",
      "formatchoice : DAYOFWEEKL",
      "formatchoice : DAYOFYEAR",
      "formatchoice : HMS",
      "formatchoice : MMDDHMS",
      "formatchoice : MMDDYYHMS",
      "formatchoice : DEGREESLON",
      "formatchoice : DEGREESMMLON",
      "formatchoice : DEGREESMMSSLON",
      "formatchoice : MMSSLON",
      "formatchoice : DEGREESLAT",
      "formatchoice : DEGREESMMLAT",
      "formatchoice : DEGREESMMSSLAT",
      "formatchoice : MMSSLAT",
      "signchoice : NORMAL",
      "signchoice : ABSOLUTE",
      "signchoice : NEGATE",
      "direction : UP",
      "direction : DOWN",
      "direction : RIGHT",
      "direction : LEFT",
      "direction : IN",
      "direction : OUT",
      "worldview : WORLD",
      "worldview : VIEW",
      "asgn : VAR '[' expr ']' '=' expr",
      "asgn : SVAR '[' expr ']' '=' expr",
      "asgn : SETNUM '.' SVAR '[' expr ']' '=' expr",
      "asgn : GRAPHNO '.' SETNUM '.' SVAR '[' expr ']' '=' expr",
      "rasgn : REGNUM '.' SVAR '=' expr",
      "vasgn : VAR '=' vexpr",
      "vasgn : SVAR '=' vexpr",
      "vasgn : SETNUM '.' SVAR '=' vexpr",
      "vasgn : GRAPHNO '.' SETNUM '.' SVAR '=' vexpr",
      "vasgn : VAR '=' expr",
      "vasgn : SVAR '=' expr",
      "vasgn : SETNUM '.' SVAR '=' expr",
      "vasgn : GRAPHNO '.' SETNUM '.' SVAR '=' expr",
      "vexpr : VAR",
      "vexpr : SVAR",
      "vexpr : GRAPHNO '.' SETNUM '.' SVAR",
      "vexpr : SETNUM '.' SVAR",
      "vexpr : expr",
      "vexpr : expr '+' expr",
      "vexpr : vexpr '+' vexpr",
      "vexpr : expr '+' vexpr",
      "vexpr : vexpr '+' expr",
      "vexpr : expr '-' expr",
      "vexpr : vexpr '-' vexpr",
      "vexpr : expr '-' vexpr",
      "vexpr : vexpr '-' expr",
      "vexpr : expr '*' expr",
      "vexpr : vexpr '*' vexpr",
      "vexpr : expr '*' vexpr",
      "vexpr : vexpr '*' expr",
      "vexpr : expr '/' expr",
      "vexpr : vexpr '/' vexpr",
      "vexpr : expr '/' vexpr",
      "vexpr : vexpr '/' expr",
      "vexpr : expr '^' expr",
      "vexpr : expr '^' vexpr",
      "vexpr : vexpr '^' expr",
      "vexpr : vexpr '^' vexpr",
      "vexpr : ABS '(' expr ')'",
      "vexpr : ABS '(' vexpr ')'",
      "vexpr : ACOS '(' vexpr ')'",
      "vexpr : ASIN '(' vexpr ')'",
      "vexpr : ATAN '(' vexpr ')'",
      "vexpr : ATAN2 '(' vexpr ',' vexpr ')'",
      "vexpr : CEIL '(' vexpr ')'",
      "vexpr : COS '(' vexpr ')'",
      "vexpr : DEG",
      "vexpr : DX",
      "vexpr : DY",
      "vexpr : ERF '(' vexpr ')'",
      "vexpr : ERFC '(' vexpr ')'",
      "vexpr : EXP '(' vexpr ')'",
      "vexpr : FLOOR '(' vexpr ')'",
      "vexpr : HYPOT '(' vexpr ',' vexpr ')'",
      "vexpr : HYPOT '(' expr ',' vexpr ')'",
      "vexpr : HYPOT '(' vexpr ',' expr ')'",
      "vexpr : HYPOT '(' expr ',' expr ')'",
      "vexpr : INDEX",
      "vexpr : SETNO",
      "vexpr : INT '(' vexpr ')'",
      "vexpr : INVN '(' vexpr ')'",
      "vexpr : INVT '(' expr ',' NUMBER ')'",
      "vexpr : INVT '(' vexpr ',' NUMBER ')'",
      "vexpr : IRAND '(' NUMBER ')'",
      "vexpr : LGAMMA '(' vexpr ')'",
      "vexpr : LN '(' vexpr ')'",
      "vexpr : LOG '(' vexpr ')'",
      "vexpr : LOGISTIC '(' vexpr ',' expr ',' expr ')'",
      "vexpr : MAXP '(' vexpr ',' vexpr ')'",
      "vexpr : MINP '(' vexpr ',' vexpr ')'",
      "vexpr : MOD '(' vexpr ',' vexpr ')'",
      "vexpr : NORM '(' vexpr ')'",
      "vexpr : NORMP '(' vexpr ')'",
      "vexpr : PI",
      "vexpr : RAD",
      "vexpr : RAND",
      "vexpr : RNORM '(' vexpr ',' vexpr ')'",
      "vexpr : RNORM '(' expr ',' vexpr ')'",
      "vexpr : RNORM '(' vexpr ',' expr ')'",
      "vexpr : RNORM '(' expr ',' expr ')'",
      "vexpr : SIN '(' vexpr ')'",
      "vexpr : SQR '(' vexpr ')'",
      "vexpr : SQRT '(' vexpr ')'",
      "vexpr : TAN '(' vexpr ')'",
      "vexpr : vexpr GT vexpr",
      "vexpr : vexpr LT vexpr",
      "vexpr : vexpr LE vexpr",
      "vexpr : vexpr GE vexpr",
      "vexpr : vexpr EQ vexpr",
      "vexpr : vexpr NE vexpr",
      "vexpr : vexpr AND vexpr",
      "vexpr : vexpr OR vexpr",
      "vexpr : NOT vexpr",
      "vexpr : '(' vexpr ')'",
      "vexpr : '-' vexpr",
      "expr : NUMBER",
      "expr : FITPARM",
      "expr : VAR '[' expr ']'",
      "expr : SVAR '[' expr ']'",
      "expr : REGNUM '.' SVAR",
      "expr : SETNUM '.' SVAR '[' expr ']'",
      "expr : SETNUM '.' extremetype",
      "expr : SETNUM '.' LENGTH",
      "expr : AVG '(' SETNUM ',' SVAR ')'",
      "expr : GRAPHNO '.' SETNUM '.' SVAR '[' expr ']'",
      "expr : expr '+' expr",
      "expr : expr '-' expr",
      "expr : expr '*' expr",
      "expr : expr '/' expr",
      "expr : expr '%' expr",
      "expr : expr '^' expr",
      "expr : ABS '(' expr ')'",
      "expr : ACOS '(' expr ')'",
      "expr : ASIN '(' expr ')'",
      "expr : ATAN '(' expr ')'",
      "expr : ATAN2 '(' expr ',' expr ')'",
      "expr : CEIL '(' expr ')'",
      "expr : COS '(' expr ')'",
      "expr : DEG",
      "expr : DX",
      "expr : DY",
      "expr : ERF '(' expr ')'",
      "expr : ERFC '(' expr ')'",
      "expr : EXP '(' expr ')'",
      "expr : FLOOR '(' expr ')'",
      "expr : HYPOT '(' expr ',' expr ')'",
      "expr : GRAPHNO '.' VX1",
      "expr : GRAPHNO '.' VX2",
      "expr : GRAPHNO '.' VY1",
      "expr : GRAPHNO '.' VY2",
      "expr : GRAPHNO '.' WX1",
      "expr : GRAPHNO '.' WX2",
      "expr : GRAPHNO '.' WY1",
      "expr : GRAPHNO '.' WY2",
      "expr : VX1",
      "expr : VX2",
      "expr : VY1",
      "expr : VY2",
      "expr : WX1",
      "expr : WX2",
      "expr : WY1",
      "expr : WY2",
      "expr : INDEX",
      "expr : SETNO",
      "expr : INT '(' expr ')'",
      "expr : INVN '(' expr ')'",
      "expr : INVT '(' expr ',' NUMBER ')'",
      "expr : IRAND '(' NUMBER ')'",
      "expr : LGAMMA '(' expr ')'",
      "expr : LN '(' expr ')'",
      "expr : LOG '(' expr ')'",
      "expr : LOGISTIC '(' expr ',' expr ',' expr ')'",
      "expr : MAXP '(' expr ',' expr ')'",
      "expr : MINP '(' expr ',' expr ')'",
      "expr : MOD '(' expr ',' expr ')'",
      "expr : NORM '(' expr ')'",
      "expr : NORMP '(' expr ')'",
      "expr : PI",
      "expr : RAD",
      "expr : RAND",
      "expr : RNORM '(' expr ',' expr ')'",
      "expr : SIN '(' expr ')'",
      "expr : SQR '(' expr ')'",
      "expr : SQRT '(' expr ')'",
      "expr : TAN '(' expr ')'",
      "expr : IF '(' expr ')' expr",
      "expr : IF '(' expr ')' expr ELSE expr",
      "expr : expr GT expr",
      "expr : expr LT expr",
      "expr : expr LE expr",
      "expr : expr GE expr",
      "expr : expr EQ expr",
      "expr : expr NE expr",
      "expr : expr AND expr",
      "expr : expr OR expr",
      "expr : NOT expr",
      "expr : '(' expr ')'",
      "expr : '-' expr",
};
#endif /* YYDEBUG */
/* @(#)27       1.7  com/cmd/lang/yacc/yaccpar, bos, bos320 8/12/91 16:09:43 */
/*
 * COMPONENT_NAME: (CMDLANG) Language Utilities
 *
 * FUNCTIONS: yyparse
 * ORIGINS: 03
 */
/*
** Skeleton parser driver for yacc output
*/

/*
** yacc user known macros and defines
*/
#ifdef YYSPLIT
#   define YYERROR      return(-2)
#else
#   define YYERROR      goto yyerrlab
#endif
#ifdef YACC_MSG
#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE
#endif
#include <nl_types.h>
nl_catd catd;
#endif
#define YYACCEPT        return(0)
#define YYABORT         return(1)
#ifndef YACC_MSG
#define YYBACKUP( newtoken, newvalue )\
{\
        if ( yychar >= 0 || ( yyr2[ yytmp ] >> 1 ) != 1 )\
        {\
                yyerror( "syntax error - cannot backup" );\
                YYERROR;\
        }\
        yychar = newtoken;\
        yystate = *yyps;\
        yylval = newvalue;\
        goto yynewstate;\
}
#else
#define YYBACKUP( newtoken, newvalue )\
{\
        if ( yychar >= 0 || ( yyr2[ yytmp ] >> 1 ) != 1 )\
        {\
                catd=catopen("yacc_user.cat",0);\
                yyerror(catgets(catd,1,1,"syntax error - cannot backup" ));\
                YYERROR;\
        }\
        yychar = newtoken;\
        yystate = *yyps;\
        yylval = newvalue;\
        goto yynewstate;\
}
#endif
#define YYRECOVERING()  (!!yyerrflag)
#ifndef YYDEBUG
#       define YYDEBUG  1       /* make debugging available */
#endif

/*
** user known globals
*/
int yydebug;                    /* set to 1 to get debugging */

/*
** driver internal defines
*/
#define YYFLAG          (-1000)

#ifdef YYSPLIT
#   define YYSCODE { \
                        extern int (*_yyf[])(); \
                        register int yyret; \
                        if (_yyf[yytmp]) \
                            if ((yyret=(*_yyf[yytmp])()) == -2) \
                                    goto yyerrlab; \
                                else if (yyret>=0) return(yyret); \
                   }
#endif

/*
** global variables used by the parser
*/
YYSTYPE yyv[ YYMAXDEPTH ];      /* value stack */
int yys[ YYMAXDEPTH ];          /* state stack */

YYSTYPE *yypv;                  /* top of value stack */
YYSTYPE *yypvt;                 /* top of value stack for $vars */
int *yyps;                      /* top of state stack */

int yystate;                    /* current state */
int yytmp;                      /* extra var (lasts between blocks) */

int yynerrs;                    /* number of errors */
int yyerrflag;                  /* error recovery flag */
int yychar;                     /* current input token number */



/*
** yyparse - return 0 if worked, 1 if syntax error not recovered from
*/
int
yyparse()
{
        /*
        ** Initialize externals - yyparse may be called more than once
        */
        yypv = &yyv[-1];
        yyps = &yys[-1];
        yystate = 0;
        yytmp = 0;
        yynerrs = 0;
        yyerrflag = 0;
        yychar = -1;
#ifdef YACC_MSG
        catd=catopen("yacc_user.cat",0);
#endif
        goto yystack;
        {
                register YYSTYPE *yy_pv;        /* top of value stack */
                register int *yy_ps;            /* top of state stack */
                register int yy_state;          /* current state */
                register int  yy_n;             /* internal state number info */

                /*
                ** get globals into registers.
                ** branch to here only if YYBACKUP was called.
                */
        yynewstate:
                yy_pv = yypv;
                yy_ps = yyps;
                yy_state = yystate;
                goto yy_newstate;

                /*
                ** get globals into registers.
                ** either we just started, or we just finished a reduction
                */
        yystack:
                yy_pv = yypv;
                yy_ps = yyps;
                yy_state = yystate;

                /*
                ** top of for (;;) loop while no reductions done
                */
        yy_stack:
                /*
                ** put a state and value onto the stacks
                */
#if YYDEBUG
                /*
                ** if debugging, look up token value in list of value vs.
                ** name pairs.  0 and negative (-1) are special values.
                ** Note: linear search is used since time is not a real
                ** consideration while debugging.
                */
                if ( yydebug )
                {
                        register int yy_i;

                        printf( "State %d, token ", yy_state );
                        if ( yychar == 0 )
                                printf( "end-of-file\n" );
                        else if ( yychar < 0 )
                                printf( "-none-\n" );
                        else
                        {
                                for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
                                        yy_i++ )
                                {
                                        if ( yytoks[yy_i].t_val == yychar )
                                                break;
                                }
                                printf( "%s\n", yytoks[yy_i].t_name );
                        }
                }
#endif /* YYDEBUG */
                if ( ++yy_ps >= &yys[ YYMAXDEPTH ] )    /* room on stack? */
                {
#ifndef YACC_MSG
                        yyerror( "yacc stack overflow" );
#else
                        yyerror(catgets(catd,1,2,"yacc stack overflow" ));
#endif
                        YYABORT;
                }
                *yy_ps = yy_state;
                *++yy_pv = yyval;

                /*
                ** we have a new state - find out what to do
                */
        yy_newstate:
                if ( ( yy_n = yypact[ yy_state ] ) <= YYFLAG )
                        goto yydefault;         /* simple state */
#if YYDEBUG
                /*
                ** if debugging, need to mark whether new token grabbed
                */
                yytmp = yychar < 0;
#endif
                if ( ( yychar < 0 ) && ( ( yychar = yylex() ) < 0 ) )
                        yychar = 0;             /* reached EOF */
#if YYDEBUG
                if ( yydebug && yytmp )
                {
                        register int yy_i;

                        printf( "Received token " );
                        if ( yychar == 0 )
                                printf( "end-of-file\n" );
                        else if ( yychar < 0 )
                                printf( "-none-\n" );
                        else
                        {
                                for ( yy_i = 0; yytoks[yy_i].t_val >= 0;
                                        yy_i++ )
                                {
                                        if ( yytoks[yy_i].t_val == yychar )
                                                break;
                                }
                                printf( "%s\n", yytoks[yy_i].t_name );
                        }
                }
#endif /* YYDEBUG */
                if ( ( ( yy_n += yychar ) < 0 ) || ( yy_n >= YYLAST ) )
                        goto yydefault;
                if ( yychk[ yy_n = yyact[ yy_n ] ] == yychar )  /*valid shift*/
                {
                        yychar = -1;
                        yyval = yylval;
                        yy_state = yy_n;
                        if ( yyerrflag > 0 )
                                yyerrflag--;
                        goto yy_stack;
                }

        yydefault:
                if ( ( yy_n = yydef[ yy_state ] ) == -2 )
                {
#if YYDEBUG
                        yytmp = yychar < 0;
#endif
                        if ( ( yychar < 0 ) && ( ( yychar = yylex() ) < 0 ) )
                                yychar = 0;             /* reached EOF */
#if YYDEBUG
                        if ( yydebug && yytmp )
                        {
                                register int yy_i;

                                printf( "Received token " );
                                if ( yychar == 0 )
                                        printf( "end-of-file\n" );
                                else if ( yychar < 0 )
                                        printf( "-none-\n" );
                                else
                                {
                                        for ( yy_i = 0;
                                                yytoks[yy_i].t_val >= 0;
                                                yy_i++ )
                                        {
                                                if ( yytoks[yy_i].t_val
                                                        == yychar )
                                                {
                                                        break;
                                                }
                                        }
                                        printf( "%s\n", yytoks[yy_i].t_name );
                                }
                        }
#endif /* YYDEBUG */
                        /*
                        ** look through exception table
                        */
                        {
                                register int *yyxi = yyexca;

                                while ( ( *yyxi != -1 ) ||
                                        ( yyxi[1] != yy_state ) )
                                {
                                        yyxi += 2;
                                }
                                while ( ( *(yyxi += 2) >= 0 ) &&
                                        ( *yyxi != yychar ) )
                                        ;
                                if ( ( yy_n = yyxi[1] ) < 0 )
                                        YYACCEPT;
                        }
                }

                /*
                ** check for syntax error
                */
                if ( yy_n == 0 )        /* have an error */
                {
                        /* no worry about speed here! */
                        switch ( yyerrflag )
                        {
                        case 0:         /* new error */
#ifndef YACC_MSG
                                yyerror( "syntax error" );
#else
                                yyerror(catgets(catd,1,3,"syntax error" ));
#endif
                                goto skip_init;
                        yyerrlab:
                                /*
                                ** get globals into registers.
                                ** we have a user generated syntax type error
                                */
                                yy_pv = yypv;
                                yy_ps = yyps;
                                yy_state = yystate;
                                yynerrs++;
                        skip_init:
                        case 1:
                        case 2:         /* incompletely recovered error */
                                        /* try again... */
                                yyerrflag = 3;
                                /*
                                ** find state where "error" is a legal
                                ** shift action
                                */
                                while ( yy_ps >= yys )
                                {
                                        yy_n = yypact[ *yy_ps ] + YYERRCODE;
                                        if ( yy_n >= 0 && yy_n < YYLAST &&
                                                yychk[yyact[yy_n]] == YYERRCODE)                                        {
                                                /*
                                                ** simulate shift of "error"
                                                */
                                                yy_state = yyact[ yy_n ];
                                                goto yy_stack;
                                        }
                                        /*
                                        ** current state has no shift on
                                        ** "error", pop stack
                                        */
#if YYDEBUG
#       define _POP_ "Error recovery pops state %d, uncovers state %d\n"
                                        if ( yydebug )
                                                printf( _POP_, *yy_ps,
                                                        yy_ps[-1] );
#       undef _POP_
#endif
                                        yy_ps--;
                                        yy_pv--;
                                }
                                /*
                                ** there is no state on stack with "error" as
                                ** a valid shift.  give up.
                                */
                                YYABORT;
                        case 3:         /* no shift yet; eat a token */
#if YYDEBUG
                                /*
                                ** if debugging, look up token in list of
                                ** pairs.  0 and negative shouldn't occur,
                                ** but since timing doesn't matter when
                                ** debugging, it doesn't hurt to leave the
                                ** tests here.
                                */
                                if ( yydebug )
                                {
                                        register int yy_i;

                                        printf( "Error recovery discards " );
                                        if ( yychar == 0 )
                                                printf( "token end-of-file\n" );
                                        else if ( yychar < 0 )
                                                printf( "token -none-\n" );
                                        else
                                        {
                                                for ( yy_i = 0;
                                                        yytoks[yy_i].t_val >= 0;
                                                        yy_i++ )
                                                {
                                                        if ( yytoks[yy_i].t_val
                                                                == yychar )
                                                        {
                                                                break;
                                                        }
                                                }
                                                printf( "token %s\n",
                                                        yytoks[yy_i].t_name );
                                        }
                                }
#endif /* YYDEBUG */
                                if ( yychar == 0 )      /* reached EOF. quit */
                                        YYABORT;
                                yychar = -1;
                                goto yy_newstate;
                        }
                }/* end if ( yy_n == 0 ) */
                /*
                ** reduction by production yy_n
                ** put stack tops, etc. so things right after switch
                */
#if YYDEBUG
                /*
                ** if debugging, print the string that is the user's
                ** specification of the reduction which is just about
                ** to be done.
                */
                if ( yydebug )
                        printf( "Reduce by (%d) \"%s\"\n",
                                yy_n, yyreds[ yy_n ] );
#endif
                yytmp = yy_n;                   /* value to switch over */
                yypvt = yy_pv;                  /* $vars top of value stack */
                /*
                ** Look in goto table for next state
                ** Sorry about using yy_state here as temporary
                ** register variable, but why not, if it works...
                ** If yyr2[ yy_n ] doesn't have the low order bit
                ** set, then there is no action to be done for
                ** this reduction.  So, no saving & unsaving of
                ** registers done.  The only difference between the
                ** code just after the if and the body of the if is
                ** the goto yy_stack in the body.  This way the test
                ** can be made before the choice of what to do is needed.
                */
                {
                        /* length of production doubled with extra bit */
                        register int yy_len = yyr2[ yy_n ];

                        if ( !( yy_len & 01 ) )
                        {
                                yy_len >>= 1;
                                yyval = ( yy_pv -= yy_len )[1]; /* $$ = $1 */
                                yy_state = yypgo[ yy_n = yyr1[ yy_n ] ] +
                                        *( yy_ps -= yy_len ) + 1;
                                if ( yy_state >= YYLAST ||
                                        yychk[ yy_state =
                                        yyact[ yy_state ] ] != -yy_n )
                                {
                                        yy_state = yyact[ yypgo[ yy_n ] ];
                                }
                                goto yy_stack;
                        }
                        yy_len >>= 1;
                        yyval = ( yy_pv -= yy_len )[1]; /* $$ = $1 */
                        yy_state = yypgo[ yy_n = yyr1[ yy_n ] ] +
                                *( yy_ps -= yy_len ) + 1;
                        if ( yy_state >= YYLAST ||
                                yychk[ yy_state = yyact[ yy_state ] ] != -yy_n )
                        {
                                yy_state = yyact[ yypgo[ yy_n ] ];
                        }
                }
                                        /* save until reenter driver code */
                yystate = yy_state;
                yyps = yy_ps;
                yypv = yy_pv;
        }
        /*
        ** code supplied by user is placed in this switch
        */

                switch(yytmp){

case 5:
# line 479 "pars.yacc"
{
	    result = yypvt[-1].val;
	} /*NOTREACHED*/ break;
case 6:
# line 482 "pars.yacc"
{
	    result = *yypvt[-1].ptr;
	} /*NOTREACHED*/ break;
case 12:
# line 490 "pars.yacc"
{
	    return 1;
	} /*NOTREACHED*/ break;
case 13:
# line 496 "pars.yacc"
{
	    if (yypvt[-1].pset == FILEP) {
		set_printer(FILEP, yypvt[-0].pset);
	    }
	    else {
		set_printer(yypvt[-1].pset, yypvt[-0].pset);
	    }
	} /*NOTREACHED*/ break;
case 14:
# line 504 "pars.yacc"
{
	    if (yypvt[-1].pset == FILEP) {
		set_printer(FILEP, yypvt[-0].pset);
	    }
	    else {
		set_printer(yypvt[-1].pset, yypvt[-0].pset);
	    }
	} /*NOTREACHED*/ break;
case 15:
# line 512 "pars.yacc"
{
	    if (yypvt[-0].pset == FILEP) {
		set_printer(FILEP, NULL);
	    }
	    else {
		set_printer(yypvt[-0].pset, NULL);
	    }
	} /*NOTREACHED*/ break;
case 16:
# line 520 "pars.yacc"
{
	    tdevice = (int) yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 17:
# line 523 "pars.yacc"
{
	    hdevice = (int) yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 18:
# line 526 "pars.yacc"
{
	    do_hardcopy();
	} /*NOTREACHED*/ break;
case 19:
# line 532 "pars.yacc"
{ yyval.pset = GR_PS_P; } /*NOTREACHED*/ break;
case 20:
# line 533 "pars.yacc"
{ yyval.pset = GR_PS_L; } /*NOTREACHED*/ break;
case 21:
# line 534 "pars.yacc"
{ yyval.pset = GR_MIF_P; } /*NOTREACHED*/ break;
case 22:
# line 535 "pars.yacc"
{ yyval.pset = GR_MIF_L; } /*NOTREACHED*/ break;
case 23:
# line 536 "pars.yacc"
{ yyval.pset = GR_HPGL_P; } /*NOTREACHED*/ break;
case 24:
# line 537 "pars.yacc"
{ yyval.pset = GR_HPGL_L; } /*NOTREACHED*/ break;
case 25:
# line 538 "pars.yacc"
{ yyval.pset = hdevice; } /*NOTREACHED*/ break;
case 26:
# line 539 "pars.yacc"
{ yyval.pset = FILEP; } /*NOTREACHED*/ break;
case 27:
# line 543 "pars.yacc"
{
	    rg[yypvt[-1].pset].active = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 28:
# line 546 "pars.yacc"
{
	    rg[yypvt[-2].pset].type = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 29:
# line 549 "pars.yacc"
{
	    rg[yypvt[-2].pset].color = checkon(COLOR, rg[yypvt[-2].pset].color, (int) yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 30:
# line 552 "pars.yacc"
{
	    rg[yypvt[-2].pset].lines = checkon(LINESTYLE, rg[yypvt[-2].pset].lines, (int) yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 31:
# line 555 "pars.yacc"
{
	    rg[yypvt[-2].pset].linew = checkon(LINEWIDTH, rg[yypvt[-2].pset].linew, (int) yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 32:
# line 559 "pars.yacc"
{
	    rg[yypvt[-8].pset].x1 = yypvt[-6].val;
	    rg[yypvt[-8].pset].y1 = yypvt[-4].val;
	    rg[yypvt[-8].pset].x2 = yypvt[-2].val;
	    rg[yypvt[-8].pset].y2 = yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 33:
# line 566 "pars.yacc"
{
	    if (rg[yypvt[-4].pset].x == NULL || rg[yypvt[-4].pset].n == 0) {
		rg[yypvt[-4].pset].n = 0;
		rg[yypvt[-4].pset].x = (double *) calloc(1, sizeof(double));
		rg[yypvt[-4].pset].y = (double *) calloc(1, sizeof(double));
	    } else {
		rg[yypvt[-4].pset].x = (double *) realloc(rg[yypvt[-4].pset].x, (rg[yypvt[-4].pset].n + 1) * sizeof(double));
		rg[yypvt[-4].pset].y = (double *) realloc(rg[yypvt[-4].pset].y, (rg[yypvt[-4].pset].n + 1) * sizeof(double));
	    }
	    rg[yypvt[-4].pset].x[rg[yypvt[-4].pset].n] = yypvt[-2].val;
	    rg[yypvt[-4].pset].y[rg[yypvt[-4].pset].n] = yypvt[-0].val;
	    rg[yypvt[-4].pset].n++;
	} /*NOTREACHED*/ break;
case 34:
# line 579 "pars.yacc"
{
	    rg[yypvt[-2].pset].linkto[yypvt[-0].pset] = TRUE;
	} /*NOTREACHED*/ break;
case 35:
# line 585 "pars.yacc"
{
	    drawgraph();
	} /*NOTREACHED*/ break;
case 36:
# line 588 "pars.yacc"
{
	    auto_redraw = (yypvt[-0].pset == ON);
	} /*NOTREACHED*/ break;
case 37:
# line 591 "pars.yacc"
{
	    if (inwin) {
		set_left_footer(yypvt[-0].pset);
	    }
	    else {
		printf("%s\n", yypvt[-0].pset);
	    }
	} /*NOTREACHED*/ break;
case 38:
# line 599 "pars.yacc"
{
	    setbgcolor((int) yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 39:
# line 602 "pars.yacc"
{
	    xlibsetcmap((int) yypvt[-6].val, (int) yypvt[-4].val, (int) yypvt[-2].val, (int) yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 40:
# line 605 "pars.yacc"
{
	    exit(0);
	} /*NOTREACHED*/ break;
case 41:
# line 609 "pars.yacc"
{
	    switch (yypvt[-0].pset) {
	    case UP:
		gwindup_proc();
		break;
	    case DOWN:
		gwinddown_proc();
		break;
	    case RIGHT:
		gwindright_proc();
		break;
	    case LEFT:
		gwindleft_proc();
		break;
	    case IN:
		gwindshrink_proc();
		break;
	    case OUT:
		gwindexpand_proc();
		break;
	    }
	} /*NOTREACHED*/ break;
case 42:
# line 631 "pars.yacc"
{
	    scroll_proc((int) yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 43:
# line 634 "pars.yacc"
{
	    scrollinout_proc((int) yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 44:
# line 637 "pars.yacc"
{
	    scrolling_islinked = yypvt[-0].pset == ON;
	} /*NOTREACHED*/ break;
case 45:
# line 640 "pars.yacc"
{
	    my_doublebuffer(yypvt[-0].pset == TRUEP);
	} /*NOTREACHED*/ break;
case 46:
# line 643 "pars.yacc"
{
	    my_frontbuffer(yypvt[-0].pset == TRUEP);
	} /*NOTREACHED*/ break;
case 47:
# line 646 "pars.yacc"
{
	    my_backbuffer(yypvt[-0].pset == TRUEP);
	} /*NOTREACHED*/ break;
case 48:
# line 649 "pars.yacc"
{
	    my_swapbuffer();
	} /*NOTREACHED*/ break;
case 49:
# line 652 "pars.yacc"
{
	    sleep((int) yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 50:
# line 655 "pars.yacc"
{	/* TODO add delay function */
	} /*NOTREACHED*/ break;
case 51:
# line 657 "pars.yacc"
{		/* TODO add abort flag and function */
	} /*NOTREACHED*/ break;
case 52:
# line 660 "pars.yacc"
{
	    gotparams = TRUE;
	    strcpy(paramfile, (char *) yypvt[-0].pset);
	} /*NOTREACHED*/ break;
case 53:
# line 665 "pars.yacc"
{
	    if (!fexists((char *) yypvt[-0].pset)) {
		FILE *pp = fopen((char *) yypvt[-0].pset, "w");
		if (pp != NULL) {
		    putparms(cg, pp, 0);
		    fclose(pp);
		} else {
		    errwin("Unable to write parameter file");
		}
	    }
	} /*NOTREACHED*/ break;
case 54:
# line 676 "pars.yacc"
{
	    cg = (int) yypvt[-0].pset;
	    g[cg].parmsread = TRUE;
	    change_gno = cg;
	} /*NOTREACHED*/ break;
case 55:
# line 681 "pars.yacc"
{
	    curset = (int) yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 56:
# line 684 "pars.yacc"
{
	    do_activateset(cg, yypvt[-1].pset, (int) yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 57:
# line 687 "pars.yacc"
{
	    add_point(cg, yypvt[-4].pset, yypvt[-2].val, yypvt[-0].val, 0.0, 0.0, XY);
	} /*NOTREACHED*/ break;
case 58:
# line 690 "pars.yacc"
{
	    add_point(yypvt[-6].pset, yypvt[-4].pset, yypvt[-2].val, yypvt[-0].val, 0.0, 0.0, XY);
	} /*NOTREACHED*/ break;
case 59:
# line 693 "pars.yacc"
{
	    do_copyset(cg, yypvt[-2].pset, cg, yypvt[-0].pset);
	} /*NOTREACHED*/ break;
case 60:
# line 696 "pars.yacc"
{
	    do_copyset(yypvt[-6].pset, yypvt[-4].pset, yypvt[-2].pset, yypvt[-0].pset);
	} /*NOTREACHED*/ break;
case 61:
# line 699 "pars.yacc"
{
	    do_moveset(cg, yypvt[-2].pset, cg, yypvt[-0].pset, 0);
	} /*NOTREACHED*/ break;
case 62:
# line 702 "pars.yacc"
{
	    do_moveset(yypvt[-6].pset, yypvt[-4].pset, yypvt[-2].pset, yypvt[-0].pset);
	} /*NOTREACHED*/ break;
case 63:
# line 706 "pars.yacc"
{
	    killset(cg, yypvt[-0].pset);
	} /*NOTREACHED*/ break;
case 64:
# line 710 "pars.yacc"
{
	    int i;
	    for (i = 0; i < g[cg].maxplot; i++) {
		killset(cg, i);
	    }
	} /*NOTREACHED*/ break;
case 65:
# line 717 "pars.yacc"
{
	    kill_graph(yypvt[-0].pset);
	} /*NOTREACHED*/ break;
case 66:
# line 721 "pars.yacc"
{
	    kill_graph(MAXGRAPH);
	} /*NOTREACHED*/ break;
case 67:
# line 725 "pars.yacc"
{
	    wipeout(0);
	} /*NOTREACHED*/ break;
case 68:
# line 729 "pars.yacc"
{
	    int i;
	    for (i = 0; i < (int) yypvt[-4].val; i++) {
		yypvt[-5].ptr[i] = yypvt[-2].val + yypvt[-0].val * i;
	    }
	} /*NOTREACHED*/ break;
case 69:
# line 736 "pars.yacc"
{
	    int i, setno = yypvt[-3].pset, ideg = (int) yypvt[-1].val, fitset;
	    if (!isactive(cg, setno)) {
		errwin("Set not active");
	    } else {
		fitset = nextset(cg);
		if (fitset != (-1)) {
		    activateset(cg, fitset);
		    setlength(cg, fitset, getsetlength(cg, setno));
		    copyx(cg, setno, fitset);
		    fitcurve(getx(cg, setno), gety(cg, setno), getsetlength(cg, setno), ideg, gety(cg, fitset));
		    sprintf(buf, "%d deg fit of set %d", ideg, setno);
		    setcomment(cg, fitset, buf);
		    updatesetminmax(cg, fitset);
		    update_set_status(cg, fitset);
		} else {
		    errwin("No sets");
		}
	    }
	} /*NOTREACHED*/ break;
case 70:
# line 757 "pars.yacc"
{
	    do_running_command(yypvt[-5].pset, yypvt[-3].pset, (int) yypvt[-1].val);
	} /*NOTREACHED*/ break;
case 71:
# line 761 "pars.yacc"
{
	    do_fourier_command(yypvt[-5].pset, yypvt[-3].pset, (int) yypvt[-1].val);
	} /*NOTREACHED*/ break;
case 72:
# line 765 "pars.yacc"
{
	    do_histo_command(yypvt[-7].pset, yypvt[-5].val, yypvt[-3].val, (int) yypvt[-1].val);
	} /*NOTREACHED*/ break;
case 73:
# line 769 "pars.yacc"
{
	    do_differ(yypvt[-3].pset, (int) yypvt[-1].val);
	} /*NOTREACHED*/ break;
case 74:
# line 773 "pars.yacc"
{
	    do_int(yypvt[-1].pset, 0);
	} /*NOTREACHED*/ break;
case 75:
# line 777 "pars.yacc"
{
	    if (activeset(cg)) {
		defaultgraph(cg);
		default_axis(cg, g[cg].auto_type, X_AXIS);
		default_axis(cg, g[cg].auto_type, ZX_AXIS);
		default_axis(cg, g[cg].auto_type, Y_AXIS);
		default_axis(cg, g[cg].auto_type, ZY_AXIS);
		update_world(cg);
		drawgraph();
	    } else {
		errwin("No active sets!");
	    }
	} /*NOTREACHED*/ break;
case 76:
# line 791 "pars.yacc"
{
	    if (activeset(cg)) {
		defaultx(cg, -1);
		default_axis(cg, g[cg].auto_type, X_AXIS);
		default_axis(cg, g[cg].auto_type, ZX_AXIS);
		update_world(cg);
		drawgraph();
	    } else {
		errwin("No active sets!");
	    }
	} /*NOTREACHED*/ break;
case 77:
# line 803 "pars.yacc"
{
	    if (activeset(cg)) {
		defaulty(cg, -1);
		default_axis(cg, g[cg].auto_type, Y_AXIS);
		default_axis(cg, g[cg].auto_type, ZY_AXIS);
		update_world(cg);
		drawgraph();
	    } else {
		errwin("No active sets!");
	    }
	} /*NOTREACHED*/ break;
case 78:
# line 815 "pars.yacc"
{
	    if (isactive_set(cg, yypvt[-0].pset)) {
		defaultsetgraph(cg, yypvt[-0].pset);
		default_axis(cg, g[cg].auto_type, X_AXIS);
		default_axis(cg, g[cg].auto_type, ZX_AXIS);
		default_axis(cg, g[cg].auto_type, Y_AXIS);
		default_axis(cg, g[cg].auto_type, ZY_AXIS);
		update_world(cg);
		drawgraph();
	    } else {
		errwin("Set not active");
	    }
	} /*NOTREACHED*/ break;
case 79:
# line 829 "pars.yacc"
{
	    extern int go_locateflag;
	    go_locateflag = (yypvt[-0].pset == ON);
	} /*NOTREACHED*/ break;
case 80:
# line 834 "pars.yacc"
{
	    draw_focus(cg);
	    cg = (int) yypvt[-0].pset;
	    defineworld(g[cg].w.xg1, g[cg].w.yg1, g[cg].w.xg2, g[cg].w.yg2);
	    viewport(g[cg].v.xv1, g[cg].v.yv1, g[cg].v.xv2, g[cg].v.yv2);
	    draw_focus(cg);
	    update_all(cg);
	} /*NOTREACHED*/ break;
case 81:
# line 842 "pars.yacc"
{
	    draw_focus_flag = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 82:
# line 845 "pars.yacc"
{
	    focus_policy = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 83:
# line 848 "pars.yacc"
{
	    focus_policy = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 84:
# line 851 "pars.yacc"
{
	    focus_policy = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 85:
# line 854 "pars.yacc"
{
	    cursource = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 86:
# line 857 "pars.yacc"
{
	    curtype = yypvt[-0].pset;
	    change_type = curtype;
	} /*NOTREACHED*/ break;
case 87:
# line 862 "pars.yacc"
{
	    gotread = TRUE;
	    readtype = curtype;
	    readsrc = cursource;
	    strcpy(readfile, yypvt[-0].pset);
	} /*NOTREACHED*/ break;
case 88:
# line 869 "pars.yacc"
{
	    gotbatch = TRUE;
	    strcpy(batchfile, yypvt[-0].pset);
	} /*NOTREACHED*/ break;
case 89:
# line 874 "pars.yacc"
{
	    gotread = TRUE;
	    readtype = yypvt[-1].pset;
	    readsrc = cursource;
	    strcpy(readfile, yypvt[-0].pset);
	} /*NOTREACHED*/ break;
case 90:
# line 881 "pars.yacc"
{
	    gotread = TRUE;
	    strcpy(readfile, yypvt[-0].pset);
	    readtype = yypvt[-2].pset;
	    readsrc = yypvt[-1].pset;
	} /*NOTREACHED*/ break;
case 91:
# line 887 "pars.yacc"
{
	    push_world();
	} /*NOTREACHED*/ break;
case 92:
# line 890 "pars.yacc"
{
	    pop_world();
	} /*NOTREACHED*/ break;
case 93:
# line 893 "pars.yacc"
{
	    cycle_world_stack();
	} /*NOTREACHED*/ break;
case 94:
# line 896 "pars.yacc"
{
	    if ((int) yypvt[-0].val > 0)
		show_world_stack((int) yypvt[-0].val - 1);
	} /*NOTREACHED*/ break;
case 95:
# line 901 "pars.yacc"
{
	    add_world(cg, yypvt[-14].val, yypvt[-12].val, yypvt[-10].val, yypvt[-8].val, yypvt[-6].val, yypvt[-4].val, yypvt[-2].val, yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 96:
# line 904 "pars.yacc"
{
	    clear_world_stack();
	} /*NOTREACHED*/ break;
case 97:
# line 907 "pars.yacc"
{
	    do_clear_boxes();
	} /*NOTREACHED*/ break;
case 98:
# line 910 "pars.yacc"
{
	    curbox = next_box();
	} /*NOTREACHED*/ break;
case 99:
# line 913 "pars.yacc"
{
	    curbox = (int) yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 100:
# line 916 "pars.yacc"
{
	    boxes[curbox].active = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 101:
# line 919 "pars.yacc"
{
	    boxes[curbox].gno = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 102:
# line 923 "pars.yacc"
{
	    if (curbox >= 0 && curbox < MAXBOXES) {
		boxes[curbox].x1 = yypvt[-6].val;
		boxes[curbox].y1 = yypvt[-4].val;
		boxes[curbox].x2 = yypvt[-2].val;
		boxes[curbox].y2 = yypvt[-0].val;
	    }
	} /*NOTREACHED*/ break;
case 103:
# line 931 "pars.yacc"
{
	    box_loctype = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 104:
# line 934 "pars.yacc"
{
	    box_lines = checkon(LINESTYLE, box_lines, (int) yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 105:
# line 937 "pars.yacc"
{
	    box_linew = checkon(LINEWIDTH, box_linew, (int) yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 106:
# line 940 "pars.yacc"
{
	    box_color = checkon(COLOR, box_color, (int) yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 107:
# line 943 "pars.yacc"
{
	    box_fill = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 108:
# line 946 "pars.yacc"
{
	    box_fillcolor = checkon(COLOR, box_fillcolor, (int) yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 109:
# line 949 "pars.yacc"
{
	    box_fillpat = checkon(PATTERN, box_fillpat, (int) yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 110:
# line 953 "pars.yacc"
{
	    if (curbox >= 0 && curbox < MAXBOXES) {
		boxes[curbox].lines = box_lines;
		boxes[curbox].linew = box_linew;
		boxes[curbox].color = box_color;
		boxes[curbox].fill = box_fill;
		boxes[curbox].fillcolor = box_fillcolor;
		boxes[curbox].fillpattern = box_fillpat;
		boxes[curbox].loctype = box_loctype;
	    }
	} /*NOTREACHED*/ break;
case 111:
# line 964 "pars.yacc"
{
	    curline = next_line();
	} /*NOTREACHED*/ break;
case 112:
# line 967 "pars.yacc"
{
	    curline = (int) yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 113:
# line 970 "pars.yacc"
{
	    do_clear_lines();
	} /*NOTREACHED*/ break;
case 114:
# line 973 "pars.yacc"
{
	    lines[curline].active = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 115:
# line 976 "pars.yacc"
{
	    lines[curline].gno = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 116:
# line 980 "pars.yacc"
{
	    lines[curline].x1 = yypvt[-6].val;
	    lines[curline].y1 = yypvt[-4].val;
	    lines[curline].x2 = yypvt[-2].val;
	    lines[curline].y2 = yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 117:
# line 986 "pars.yacc"
{
	    line_loctype = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 118:
# line 989 "pars.yacc"
{
	    line_linew = checkon(LINEWIDTH, line_linew, (int) yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 119:
# line 992 "pars.yacc"
{
	    line_lines = checkon(LINESTYLE, line_lines, (int) yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 120:
# line 995 "pars.yacc"
{
	    line_color = checkon(COLOR, line_color, (int) yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 121:
# line 998 "pars.yacc"
{
	    line_arrow = checkon(ARROW, line_arrow, (int) yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 122:
# line 1001 "pars.yacc"
{
	    line_asize = yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 123:
# line 1004 "pars.yacc"
{
	    line_atype = (int) yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 124:
# line 1008 "pars.yacc"
{
	    if (curline >= 0 && curline < MAXLINES) {
		lines[curline].lines = line_lines;
		lines[curline].linew = line_linew;
		lines[curline].color = line_color;
		lines[curline].arrow = line_arrow;
		lines[curline].asize = line_asize;
		lines[curline].atype = line_atype;
		lines[curline].loctype = line_loctype;
	    }
	} /*NOTREACHED*/ break;
case 125:
# line 1019 "pars.yacc"
{
	    do_clear_text();
	} /*NOTREACHED*/ break;
case 126:
# line 1022 "pars.yacc"
{
	    curstring = next_string();
	} /*NOTREACHED*/ break;
case 127:
# line 1025 "pars.yacc"
{
	    curstring = (int) yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 128:
# line 1028 "pars.yacc"
{
	    pstr[curstring].active = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 129:
# line 1031 "pars.yacc"
{
	    pstr[curstring].gno = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 130:
# line 1035 "pars.yacc"
{
	    pstr[curstring].x = yypvt[-2].val;
	    pstr[curstring].y = yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 131:
# line 1039 "pars.yacc"
{
	    string_loctype = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 132:
# line 1042 "pars.yacc"
{
	    string_linew = checkon(LINEWIDTH, string_linew, (int) yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 133:
# line 1045 "pars.yacc"
{
	    string_color = checkon(COLOR, string_color, (int) yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 134:
# line 1048 "pars.yacc"
{
	    string_rot = (int) yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 135:
# line 1051 "pars.yacc"
{
	    string_font = checkon(FONTP, string_font, (int) yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 136:
# line 1054 "pars.yacc"
{
	    string_just = checkon(JUST, string_just, (int) yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 137:
# line 1057 "pars.yacc"
{
	    string_size = yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 138:
# line 1061 "pars.yacc"
{
	    strcpy(pstr[curstring].s, (char *) yypvt[-0].pset);
	    pstr[curstring].linew = string_linew;
	    pstr[curstring].color = string_color;
	    pstr[curstring].font = string_font;
	    pstr[curstring].just = string_just;
	    pstr[curstring].loctype = string_loctype;
	    pstr[curstring].rot = string_rot;
	    pstr[curstring].charsize = string_size;
	} /*NOTREACHED*/ break;
case 139:
# line 1071 "pars.yacc"
{
	    g[cg].d.lines = (int) yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 140:
# line 1074 "pars.yacc"
{
	    g[cg].d.linew = (int) yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 141:
# line 1077 "pars.yacc"
{
	    g[cg].d.color = (int) yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 142:
# line 1080 "pars.yacc"
{
	    g[cg].d.charsize = yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 143:
# line 1083 "pars.yacc"
{
	    g[cg].d.font = (int) yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 144:
# line 1086 "pars.yacc"
{
	    g[cg].d.fontsrc = (int) yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 145:
# line 1089 "pars.yacc"
{
	    g[cg].d.symsize = yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 146:
# line 1093 "pars.yacc"
{
	    g[cg].w.xg1 = yypvt[-6].val;
	    g[cg].w.yg1 = yypvt[-4].val;
	    g[cg].w.xg2 = yypvt[-2].val;
	    g[cg].w.yg2 = yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 147:
# line 1099 "pars.yacc"
{
	    g[cg].w.xg1 = yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 148:
# line 1102 "pars.yacc"
{
	    g[cg].w.xg2 = yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 149:
# line 1105 "pars.yacc"
{
	    g[cg].w.yg1 = yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 150:
# line 1108 "pars.yacc"
{
	    g[cg].w.yg2 = yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 151:
# line 1112 "pars.yacc"
{
	    g[cg].v.xv1 = yypvt[-6].val;
	    g[cg].v.yv1 = yypvt[-4].val;
	    g[cg].v.xv2 = yypvt[-2].val;
	    g[cg].v.yv2 = yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 152:
# line 1118 "pars.yacc"
{
	    g[cg].v.xv1 = yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 153:
# line 1121 "pars.yacc"
{
	    g[cg].v.xv2 = yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 154:
# line 1124 "pars.yacc"
{
	    g[cg].v.yv1 = yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 155:
# line 1127 "pars.yacc"
{
	    g[cg].v.yv2 = yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 156:
# line 1130 "pars.yacc"
{
	    strcpy(g[cg].labs.title.s, yypvt[-0].pset);
	} /*NOTREACHED*/ break;
case 157:
# line 1133 "pars.yacc"
{
	    g[cg].labs.title.font = checkon(FONTP, g[cg].labs.title.font, (int) yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 158:
# line 1136 "pars.yacc"
{
	    g[cg].labs.title.charsize = yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 159:
# line 1139 "pars.yacc"
{
	    g[cg].labs.title.color = checkon(COLOR, g[cg].labs.title.color, (int) yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 160:
# line 1143 "pars.yacc"
{
	    g[cg].labs.title.linew = checkon(LINEWIDTH, g[cg].labs.title.linew, (int) yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 161:
# line 1146 "pars.yacc"
{
	    strcpy(g[cg].labs.stitle.s, yypvt[-0].pset);
	} /*NOTREACHED*/ break;
case 162:
# line 1150 "pars.yacc"
{
	    g[cg].labs.stitle.font = checkon(FONTP, g[cg].labs.stitle.font, (int) yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 163:
# line 1153 "pars.yacc"
{
	    g[cg].labs.stitle.charsize = yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 164:
# line 1157 "pars.yacc"
{
	    g[cg].labs.stitle.color = checkon(COLOR, g[cg].labs.stitle.color, (int) yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 165:
# line 1161 "pars.yacc"
{
	    g[cg].labs.stitle.linew = checkon(LINEWIDTH, g[cg].labs.stitle.color, (int) yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 166:
# line 1164 "pars.yacc"
{
	} /*NOTREACHED*/ break;
case 167:
# line 1166 "pars.yacc"
{
	} /*NOTREACHED*/ break;
case 168:
# line 1168 "pars.yacc"
{
	    realloc_plots((int) yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 169:
# line 1171 "pars.yacc"
{
	    g[cg].l.active = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 170:
# line 1174 "pars.yacc"
{
	    g[cg].l.loctype = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 171:
# line 1177 "pars.yacc"
{
	    g[cg].l.layout = (int) yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 172:
# line 1180 "pars.yacc"
{
	    g[cg].l.vgap = (int) yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 173:
# line 1183 "pars.yacc"
{
	    g[cg].l.hgap = (int) yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 174:
# line 1186 "pars.yacc"
{
	    g[cg].l.len = (int) yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 175:
# line 1189 "pars.yacc"
{
	    g[cg].l.box = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 176:
# line 1192 "pars.yacc"
{
	    g[cg].l.boxfill = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 177:
# line 1195 "pars.yacc"
{
	    g[cg].l.boxfillusing = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 178:
# line 1199 "pars.yacc"
{
	    if (yypvt[-1].pset == COLOR) {
		g[cg].l.boxfillcolor = (int) yypvt[-0].val;
	    } else {
		g[cg].l.boxfillpat = (int) yypvt[-0].val;
	    }
	} /*NOTREACHED*/ break;
case 179:
# line 1206 "pars.yacc"
{
	    g[cg].l.boxlcolor = checkon(COLOR, g[cg].l.boxlcolor, (int) yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 180:
# line 1209 "pars.yacc"
{
	    g[cg].l.boxlines = checkon(LINESTYLE, g[cg].l.boxlines, (int) yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 181:
# line 1212 "pars.yacc"
{
	    g[cg].l.boxlinew = checkon(LINEWIDTH, g[cg].l.boxlinew, (int) yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 182:
# line 1215 "pars.yacc"
{
	    g[cg].l.legx = yypvt[-2].val;
	    g[cg].l.legy = yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 183:
# line 1219 "pars.yacc"
{
	    g[cg].l.legx = yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 184:
# line 1222 "pars.yacc"
{
	    g[cg].l.legy = yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 185:
# line 1225 "pars.yacc"
{
	    g[cg].l.charsize = yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 186:
# line 1228 "pars.yacc"
{
	    g[cg].l.font = checkon(FONTP, g[cg].l.font, (int) yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 187:
# line 1231 "pars.yacc"
{
	    g[cg].l.lines = checkon(LINESTYLE, g[cg].l.lines, (int) yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 188:
# line 1234 "pars.yacc"
{
	    g[cg].l.linew = checkon(LINEWIDTH, g[cg].l.linew, (int) yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 189:
# line 1237 "pars.yacc"
{
	    g[cg].l.color = checkon(COLOR, g[cg].l.color, (int) yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 190:
# line 1240 "pars.yacc"
{
	    strcpy(g[cg].l.str[(int) yypvt[-1].val].s, (char *) yypvt[-0].pset);
	} /*NOTREACHED*/ break;
case 191:
# line 1243 "pars.yacc"
{
	    g[cg].f.active = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 192:
# line 1246 "pars.yacc"
{
	    g[cg].f.type = (int) yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 193:
# line 1249 "pars.yacc"
{
	    g[cg].f.lines = checkon(LINESTYLE, g[cg].f.lines, (int) yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 194:
# line 1252 "pars.yacc"
{
	    g[cg].f.linew = checkon(LINEWIDTH, g[cg].f.linew, (int) yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 195:
# line 1255 "pars.yacc"
{
	    g[cg].f.color = checkon(COLOR, g[cg].f.color, (int) yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 196:
# line 1258 "pars.yacc"
{
	    g[cg].f.fillbg = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 197:
# line 1261 "pars.yacc"
{
	    g[cg].f.bgcolor = (int) yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 198:
# line 1264 "pars.yacc"
{
	    g[yypvt[-1].pset].active = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 199:
# line 1267 "pars.yacc"
{
	    g[yypvt[-2].pset].label = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 200:
# line 1270 "pars.yacc"
{
	    g[yypvt[-3].pset].auto_type = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 201:
# line 1273 "pars.yacc"
{
	    g[yypvt[-3].pset].auto_type = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 202:
# line 1276 "pars.yacc"
{
	    g[yypvt[-2].pset].parmsread = (yypvt[-0].pset == FALSEP);
	} /*NOTREACHED*/ break;
case 203:
# line 1279 "pars.yacc"
{
	    g[yypvt[-2].pset].hidden = (yypvt[-0].pset == TRUEP);
	} /*NOTREACHED*/ break;
case 204:
# line 1282 "pars.yacc"
{
	    g[yypvt[-2].pset].type = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 205:
# line 1285 "pars.yacc"
{
	    g[yypvt[-2].pset].pointset = (yypvt[-0].pset == ON);
	} /*NOTREACHED*/ break;
case 206:
# line 1289 "pars.yacc"
{
	    g[yypvt[-4].pset].fx = yypvt[-1].pset;
	    g[yypvt[-4].pset].fy = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 207:
# line 1294 "pars.yacc"
{
	    g[yypvt[-5].pset].px = yypvt[-2].val;
	    g[yypvt[-5].pset].py = yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 208:
# line 1299 "pars.yacc"
{
	    g[yypvt[-5].pset].dsx = yypvt[-2].val;
	    g[yypvt[-5].pset].dsy = yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 209:
# line 1303 "pars.yacc"
{
	    g[yypvt[-3].pset].pt_type = (int) yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 210:
# line 1306 "pars.yacc"
{
	    realloc_graph_plots(yypvt[-3].pset, (int) yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 211:
# line 1312 "pars.yacc"
{
	    yyval.pset = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 212:
# line 1315 "pars.yacc"
{
	    yyval.pset = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 213:
# line 1318 "pars.yacc"
{
	    yyval.pset = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 214:
# line 1324 "pars.yacc"
{
	    yyval.pset = XY;
	} /*NOTREACHED*/ break;
case 215:
# line 1327 "pars.yacc"
{
	    yyval.pset = XYARC;
	} /*NOTREACHED*/ break;
case 216:
# line 1330 "pars.yacc"
{
	    yyval.pset = XYBOX;
	} /*NOTREACHED*/ break;
case 217:
# line 1333 "pars.yacc"
{
	    yyval.pset = XYHILO;
	} /*NOTREACHED*/ break;
case 218:
# line 1336 "pars.yacc"
{
	    yyval.pset = XYRT;
	} /*NOTREACHED*/ break;
case 219:
# line 1339 "pars.yacc"
{
	    yyval.pset = XYSEG;
	} /*NOTREACHED*/ break;
case 220:
# line 1342 "pars.yacc"
{
	    yyval.pset = XYSTRING;
	} /*NOTREACHED*/ break;
case 221:
# line 1345 "pars.yacc"
{
	    yyval.pset = XYDX;
	} /*NOTREACHED*/ break;
case 222:
# line 1348 "pars.yacc"
{
	    yyval.pset = XYDY;
	} /*NOTREACHED*/ break;
case 223:
# line 1351 "pars.yacc"
{
	    yyval.pset = XYDXDX;
	} /*NOTREACHED*/ break;
case 224:
# line 1354 "pars.yacc"
{
	    yyval.pset = XYDYDY;
	} /*NOTREACHED*/ break;
case 225:
# line 1357 "pars.yacc"
{
	    yyval.pset = XYDXDY;
	} /*NOTREACHED*/ break;
case 226:
# line 1360 "pars.yacc"
{
	    yyval.pset = XYX2Y2;
	} /*NOTREACHED*/ break;
case 227:
# line 1363 "pars.yacc"
{
	    yyval.pset = XYXX;
	} /*NOTREACHED*/ break;
case 228:
# line 1366 "pars.yacc"
{
	    yyval.pset = XYYY;
	} /*NOTREACHED*/ break;
case 229:
# line 1369 "pars.yacc"
{
	    yyval.pset = XYZ;
	} /*NOTREACHED*/ break;
case 230:
# line 1372 "pars.yacc"
{
	    yyval.pset = XYZW;
	} /*NOTREACHED*/ break;
case 231:
# line 1375 "pars.yacc"
{
	    yyval.pset = NXY;
	} /*NOTREACHED*/ break;
case 232:
# line 1378 "pars.yacc"
{
	    yyval.pset = BIN;
	} /*NOTREACHED*/ break;
case 233:
# line 1384 "pars.yacc"
{
	    yyval.pset = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 234:
# line 1387 "pars.yacc"
{
	    yyval.pset = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 235:
# line 1390 "pars.yacc"
{
	    yyval.pset = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 236:
# line 1393 "pars.yacc"
{
	    yyval.pset = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 237:
# line 1396 "pars.yacc"
{
	    yyval.pset = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 238:
# line 1399 "pars.yacc"
{
	    yyval.pset = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 239:
# line 1402 "pars.yacc"
{
	    yyval.pset = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 240:
# line 1405 "pars.yacc"
{
	    yyval.pset = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 241:
# line 1408 "pars.yacc"
{
	    yyval.pset = XY;		/* not active */
	} /*NOTREACHED*/ break;
case 242:
# line 1411 "pars.yacc"
{
	    yyval.pset = XY;		/* not active */
	} /*NOTREACHED*/ break;
case 250:
# line 1441 "pars.yacc"
{
	    set_prop(whichgraph, SET, SETNUM, whichset, ACTIVE, yypvt[-0].pset, 0);
	} /*NOTREACHED*/ break;
case 251:
# line 1444 "pars.yacc"
{
	    set_prop(whichgraph, SET, SETNUM, whichset, TYPE, yypvt[-0].pset, 0);
	} /*NOTREACHED*/ break;
case 252:
# line 1447 "pars.yacc"
{
	    set_prop(whichgraph, SET, SETNUM, whichset, MISSINGP, yypvt[-0].val, 0);
	} /*NOTREACHED*/ break;
case 253:
# line 1450 "pars.yacc"
{
	    set_prop(whichgraph, SET, SETNUM, whichset, FONTP, (int) yypvt[-0].val, 0);
	} /*NOTREACHED*/ break;
case 254:
# line 1453 "pars.yacc"
{
	    set_prop(whichgraph, SET, SETNUM, whichset, PREC, (int) yypvt[-0].val, 0);
	} /*NOTREACHED*/ break;
case 255:
# line 1456 "pars.yacc"
{
	    set_prop(whichgraph, SET, SETNUM, whichset, FORMAT, yypvt[-0].pset, 0);
	} /*NOTREACHED*/ break;
case 256:
# line 1459 "pars.yacc"
{
	    set_prop(whichgraph, SET, SETNUM, whichset, SYMBOL, TYPE, (int) yypvt[-0].val, 0);
	} /*NOTREACHED*/ break;
case 257:
# line 1462 "pars.yacc"
{
	    set_prop(whichgraph, SET, SETNUM, whichset, SYMBOL, FILL, (int) yypvt[-0].val, 0);
	} /*NOTREACHED*/ break;
case 258:
# line 1465 "pars.yacc"
{
	    set_prop(whichgraph, SET, SETNUM, whichset, SYMBOL, CENTER, (yypvt[-0].pset == TRUEP), 0);
	} /*NOTREACHED*/ break;
case 259:
# line 1468 "pars.yacc"
{
	    set_prop(whichgraph, SET, SETNUM, whichset, SYMBOL, SIZE, yypvt[-0].val, 0);
	} /*NOTREACHED*/ break;
case 260:
# line 1471 "pars.yacc"
{
	    set_prop(whichgraph, SET, SETNUM, whichset, SYMBOL, CHAR, (int) yypvt[-0].val, 0);
	} /*NOTREACHED*/ break;
case 261:
# line 1474 "pars.yacc"
{
	    set_prop(whichgraph, SET, SETNUM, whichset, SYMBOL, SKIP, (int) yypvt[-0].val, 0);
	} /*NOTREACHED*/ break;
case 262:
# line 1477 "pars.yacc"
{
	    set_prop(whichgraph, SET, SETNUM, whichset, yypvt[-1].pset, (int) yypvt[-0].val, 0);
	} /*NOTREACHED*/ break;
case 263:
# line 1480 "pars.yacc"
{
	    set_prop(whichgraph, SET, SETNUM, whichset, FILL, TYPE, (int) yypvt[-0].val, 0);
	} /*NOTREACHED*/ break;
case 264:
# line 1483 "pars.yacc"
{
	    set_prop(whichgraph, SET, SETNUM, whichset, FILL, WITH, yypvt[-0].pset, 0);
	} /*NOTREACHED*/ break;
case 265:
# line 1486 "pars.yacc"
{
	    set_prop(whichgraph, SET, SETNUM, whichset, FILL, yypvt[-1].pset, (int) yypvt[-0].val, 0);
	} /*NOTREACHED*/ break;
case 266:
# line 1489 "pars.yacc"
{
	    set_prop(whichgraph, SET, SETNUM, whichset, SKIP, (int) yypvt[-0].val, 0);
	} /*NOTREACHED*/ break;
case 267:
# line 1492 "pars.yacc"
{
	    set_prop(whichgraph, SET, SETNUM, whichset, ERRORBAR, LENGTH, yypvt[-0].val, 0);
	} /*NOTREACHED*/ break;
case 268:
# line 1495 "pars.yacc"
{
	    set_prop(whichgraph, SET, SETNUM, whichset, ERRORBAR, TYPE, yypvt[-0].pset, 0);
	} /*NOTREACHED*/ break;
case 269:
# line 1498 "pars.yacc"
{
	    set_prop(whichgraph, SET, SETNUM, whichset, ERRORBAR, LINEWIDTH, (int) yypvt[-0].val, 0);
	} /*NOTREACHED*/ break;
case 270:
# line 1501 "pars.yacc"
{
	    set_prop(whichgraph, SET, SETNUM, whichset, ERRORBAR, LINESTYLE, (int) yypvt[-0].val, 0);
	} /*NOTREACHED*/ break;
case 271:
# line 1504 "pars.yacc"
{
	    set_prop(whichgraph, SET, SETNUM, whichset, ERRORBAR, RISER, ACTIVE, yypvt[-0].pset, 0);
	} /*NOTREACHED*/ break;
case 272:
# line 1507 "pars.yacc"
{
	    set_prop(whichgraph, SET, SETNUM, whichset, ERRORBAR, RISER, LINEWIDTH, (int) yypvt[-0].val, 0);
	} /*NOTREACHED*/ break;
case 273:
# line 1510 "pars.yacc"
{
	    set_prop(whichgraph, SET, SETNUM, whichset, ERRORBAR, RISER, LINESTYLE, (int) yypvt[-0].val, 0);
	} /*NOTREACHED*/ break;
case 274:
# line 1513 "pars.yacc"
{
	    set_prop(whichgraph, SET, SETNUM, whichset, XYZ, yypvt[-2].val, yypvt[-0].val, 0);
	} /*NOTREACHED*/ break;
case 275:
# line 1516 "pars.yacc"
{
	    set_prop(whichgraph, SET, SETNUM, whichset, COMMENT, (char *) yypvt[-0].pset, 0);
	} /*NOTREACHED*/ break;
case 291:
# line 1546 "pars.yacc"
{
	    set_axis_prop(whichgraph, naxis, yypvt[-0].pset, 0.0);
	} /*NOTREACHED*/ break;
case 292:
# line 1549 "pars.yacc"
{
	    set_axis_prop(whichgraph, naxis, yypvt[-1].pset, yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 293:
# line 1552 "pars.yacc"
{
	    set_axis_prop(whichgraph, naxis, yypvt[-1].pset, yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 294:
# line 1555 "pars.yacc"
{
	    set_axis_prop(whichgraph, naxis, yypvt[-1].pset, yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 295:
# line 1558 "pars.yacc"
{
	    set_axis_prop(whichgraph, naxis, yypvt[-1].pset, yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 296:
# line 1561 "pars.yacc"
{
	    set_axis_prop(whichgraph, naxis, yypvt[-2].pset, yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 297:
# line 1564 "pars.yacc"
{
	    set_axis_prop(whichgraph, naxis, yypvt[-1].pset, yypvt[-0].pset);
	} /*NOTREACHED*/ break;
case 302:
# line 1574 "pars.yacc"
{
	    g[cg].t[naxis].active = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 305:
# line 1586 "pars.yacc"
{
	    g[cg].t[naxis].t_flag = yypvt[-0].pset;
	    g[cg].t[naxis].t_mflag = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 306:
# line 1590 "pars.yacc"
{
	    g[cg].t[naxis].t_flag = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 307:
# line 1593 "pars.yacc"
{
	    g[cg].t[naxis].t_mflag = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 308:
# line 1596 "pars.yacc"
{
	    g[cg].t[naxis].tmajor = yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 309:
# line 1599 "pars.yacc"
{
	    g[cg].t[naxis].tminor = yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 310:
# line 1602 "pars.yacc"
{
	    g[cg].t[naxis].offsx = yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 311:
# line 1605 "pars.yacc"
{
	    g[cg].t[naxis].offsy = yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 312:
# line 1608 "pars.yacc"
{
	    g[cg].t[naxis].alt = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 313:
# line 1611 "pars.yacc"
{
	    g[cg].t[naxis].tmin = yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 314:
# line 1614 "pars.yacc"
{
	    g[cg].t[naxis].tmax = yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 315:
# line 1617 "pars.yacc"
{
	    g[cg].t[naxis].t_num = (int) yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 316:
# line 1620 "pars.yacc"
{
	    g[cg].t[naxis].t_inout = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 317:
# line 1623 "pars.yacc"
{
	    g[cg].t[naxis].t_log = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 318:
# line 1626 "pars.yacc"
{
	    g[cg].t[naxis].t_size = yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 319:
# line 1629 "pars.yacc"
{
	    g[cg].t[naxis].t_size = yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 320:
# line 1632 "pars.yacc"
{
	    g[cg].t[naxis].t_msize = yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 321:
# line 1635 "pars.yacc"
{
	    g[cg].t[naxis].t_color = g[cg].t[naxis].t_mcolor = (int) yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 322:
# line 1638 "pars.yacc"
{
	    g[cg].t[naxis].t_linew = g[cg].t[naxis].t_mlinew = (int) yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 323:
# line 1641 "pars.yacc"
{
	    g[cg].t[naxis].t_color = (int) yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 324:
# line 1644 "pars.yacc"
{
	    g[cg].t[naxis].t_mcolor = (int) yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 325:
# line 1647 "pars.yacc"
{
	    g[cg].t[naxis].t_linew = (int) yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 326:
# line 1650 "pars.yacc"
{
	    g[cg].t[naxis].t_mlinew = (int) yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 327:
# line 1653 "pars.yacc"
{
	    g[cg].t[naxis].t_lines = (int) yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 328:
# line 1656 "pars.yacc"
{
	    g[cg].t[naxis].t_mlines = (int) yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 329:
# line 1659 "pars.yacc"
{
	    g[cg].t[naxis].t_gridflag = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 330:
# line 1662 "pars.yacc"
{
	    g[cg].t[naxis].t_mgridflag = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 331:
# line 1665 "pars.yacc"
{
	    g[cg].t[naxis].t_op = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 332:
# line 1668 "pars.yacc"
{
	    g[cg].t[naxis].t_type = AUTO;
	} /*NOTREACHED*/ break;
case 333:
# line 1671 "pars.yacc"
{
	    g[cg].t[naxis].t_type = SPEC;
	} /*NOTREACHED*/ break;
case 334:
# line 1674 "pars.yacc"
{
	    g[cg].t[naxis].t_spec = (int) yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 335:
# line 1677 "pars.yacc"
{
	    g[cg].t[naxis].t_specloc[(int) yypvt[-2].val] = yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 338:
# line 1688 "pars.yacc"
{
	    g[cg].t[naxis].tl_flag = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 339:
# line 1691 "pars.yacc"
{
	    g[cg].t[naxis].tl_type = AUTO;
	} /*NOTREACHED*/ break;
case 340:
# line 1694 "pars.yacc"
{
	    g[cg].t[naxis].tl_type = SPEC;
	} /*NOTREACHED*/ break;
case 341:
# line 1697 "pars.yacc"
{
	    g[cg].t[naxis].tl_prec = (int) yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 342:
# line 1700 "pars.yacc"
{
	    g[cg].t[naxis].tl_format = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 343:
# line 1703 "pars.yacc"
{
	    g[cg].t[naxis].tl_format = yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 344:
# line 1706 "pars.yacc"
{
	    strcpy(g[cg].t[naxis].tl_appstr, (char *) yypvt[-0].pset);
	} /*NOTREACHED*/ break;
case 345:
# line 1709 "pars.yacc"
{
	    strcpy(g[cg].t[naxis].tl_prestr, (char *) yypvt[-0].pset);
	} /*NOTREACHED*/ break;
case 346:
# line 1712 "pars.yacc"
{
	    g[cg].t[naxis].tl_layout = HORIZONTAL;
	} /*NOTREACHED*/ break;
case 347:
# line 1715 "pars.yacc"
{
	    g[cg].t[naxis].tl_layout = VERTICAL;
	} /*NOTREACHED*/ break;
case 348:
# line 1718 "pars.yacc"
{
	    g[cg].t[naxis].tl_layout = SPEC;
	} /*NOTREACHED*/ break;
case 349:
# line 1721 "pars.yacc"
{
	    g[cg].t[naxis].tl_angle = (int) yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 350:
# line 1724 "pars.yacc"
{
	    g[cg].t[naxis].tl_just = (int) yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 351:
# line 1727 "pars.yacc"
{
	    g[cg].t[naxis].tl_skip = (int) yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 352:
# line 1730 "pars.yacc"
{
	    g[cg].t[naxis].tl_staggered = (int) yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 353:
# line 1733 "pars.yacc"
{
	    g[cg].t[naxis].tl_op = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 354:
# line 1736 "pars.yacc"
{
	    g[cg].t[naxis].tl_sign = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 355:
# line 1739 "pars.yacc"
{
	    g[cg].t[naxis].tl_start = yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 356:
# line 1742 "pars.yacc"
{
	    g[cg].t[naxis].tl_stop = yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 357:
# line 1745 "pars.yacc"
{
	    g[cg].t[naxis].tl_starttype = (int) yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 358:
# line 1748 "pars.yacc"
{
	    g[cg].t[naxis].tl_starttype = (int) yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 359:
# line 1751 "pars.yacc"
{
	    g[cg].t[naxis].tl_stoptype = (int) yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 360:
# line 1754 "pars.yacc"
{
	    g[cg].t[naxis].tl_stoptype = (int) yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 361:
# line 1757 "pars.yacc"
{
	    g[cg].t[naxis].tl_vgap = yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 362:
# line 1760 "pars.yacc"
{
	    g[cg].t[naxis].tl_hgap = yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 363:
# line 1763 "pars.yacc"
{
	    g[cg].t[naxis].tl_charsize = yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 364:
# line 1766 "pars.yacc"
{
	    g[cg].t[naxis].tl_font = (int) yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 365:
# line 1769 "pars.yacc"
{
	    g[cg].t[naxis].tl_color = (int) yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 366:
# line 1772 "pars.yacc"
{
	    g[cg].t[naxis].tl_linew = (int) yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 367:
# line 1775 "pars.yacc"
{
	    strcpy(g[cg].t[naxis].t_speclab[(int) yypvt[-2].val].s, (char *) yypvt[-0].pset);
	} /*NOTREACHED*/ break;
case 368:
# line 1781 "pars.yacc"
{
	    strcpy(g[cg].t[naxis].label.s, (char *) yypvt[-0].pset);
	} /*NOTREACHED*/ break;
case 369:
# line 1784 "pars.yacc"
{
	    g[cg].t[naxis].label_layout = PERP;
	} /*NOTREACHED*/ break;
case 370:
# line 1787 "pars.yacc"
{
	    g[cg].t[naxis].label_layout = PARA;
	} /*NOTREACHED*/ break;
case 371:
# line 1790 "pars.yacc"
{
	    g[cg].t[naxis].label_place = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 372:
# line 1793 "pars.yacc"
{
	    g[cg].t[naxis].label_place = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 373:
# line 1796 "pars.yacc"
{
	    g[cg].t[naxis].label.x = yypvt[-2].val;
	    g[cg].t[naxis].label.y = yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 374:
# line 1800 "pars.yacc"
{
	    g[cg].t[naxis].label.just = (int) yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 375:
# line 1803 "pars.yacc"
{
	    g[cg].t[naxis].label.charsize = yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 376:
# line 1806 "pars.yacc"
{
	    g[cg].t[naxis].label.font = (int) yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 377:
# line 1809 "pars.yacc"
{
	    g[cg].t[naxis].label.color = (int) yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 378:
# line 1812 "pars.yacc"
{
	    g[cg].t[naxis].label.linew = (int) yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 379:
# line 1818 "pars.yacc"
{
	    g[cg].t[naxis].t_drawbar = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 380:
# line 1821 "pars.yacc"
{
	    g[cg].t[naxis].t_drawbarcolor = (int) yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 381:
# line 1824 "pars.yacc"
{
	    g[cg].t[naxis].t_drawbarlines = (int) yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 382:
# line 1827 "pars.yacc"
{
	    g[cg].t[naxis].t_drawbarlinew = (int) yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 383:
# line 1834 "pars.yacc"
{
	    whichgraph = yypvt[-2].pset;
	    whichset = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 384:
# line 1839 "pars.yacc"
{
	    whichgraph = cg;
	    whichset = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 385:
# line 1844 "pars.yacc"
{
	    whichgraph = cg;
	    whichset = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 386:
# line 1849 "pars.yacc"
{
	    whichgraph = yypvt[-1].pset;
	    whichset = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 387:
# line 1854 "pars.yacc"
{
	    whichgraph = yypvt[-1].pset;
	    whichset = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 388:
# line 1859 "pars.yacc"
{
	    whichgraph = yypvt[-1].pset;
	    whichset = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 389:
# line 1867 "pars.yacc"
{
	    whichgraph = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 390:
# line 1871 "pars.yacc"
{
	    whichgraph = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 391:
# line 1877 "pars.yacc"
{
	    yyval.pset = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 392:
# line 1880 "pars.yacc"
{
	    yyval.pset = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 393:
# line 1883 "pars.yacc"
{
	    yyval.pset = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 394:
# line 1886 "pars.yacc"
{
	    yyval.pset = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 395:
# line 1889 "pars.yacc"
{
	    yyval.pset = yypvt[-0].pset;
	} /*NOTREACHED*/ break;
case 396:
# line 1895 "pars.yacc"
{
	    yyval.pset = ON;
	} /*NOTREACHED*/ break;
case 397:
# line 1898 "pars.yacc"
{
	    yyval.pset = OFF;
	} /*NOTREACHED*/ break;
case 398:
# line 1904 "pars.yacc"
{
	    yyval.pset = COLOR;
	} /*NOTREACHED*/ break;
case 399:
# line 1907 "pars.yacc"
{
	    yyval.pset = PATTERN;
	} /*NOTREACHED*/ break;
case 400:
# line 1913 "pars.yacc"
{
	    yyval.pset = RUNAVG;
	} /*NOTREACHED*/ break;
case 401:
# line 1916 "pars.yacc"
{
	    yyval.pset = RUNSTD;
	} /*NOTREACHED*/ break;
case 402:
# line 1919 "pars.yacc"
{
	    yyval.pset = RUNMED;
	} /*NOTREACHED*/ break;
case 403:
# line 1922 "pars.yacc"
{
	    yyval.pset = RUNMAX;
	} /*NOTREACHED*/ break;
case 404:
# line 1925 "pars.yacc"
{
	    yyval.pset = RUNMIN;
	} /*NOTREACHED*/ break;
case 405:
# line 1931 "pars.yacc"
{
	    yyval.pset = DFT;
	} /*NOTREACHED*/ break;
case 406:
# line 1934 "pars.yacc"
{
	    yyval.pset = FFT;
	} /*NOTREACHED*/ break;
case 407:
# line 1937 "pars.yacc"
{
	    yyval.pset = INVDFT;
	} /*NOTREACHED*/ break;
case 408:
# line 1940 "pars.yacc"
{
	    yyval.pset = INVFFT;
	} /*NOTREACHED*/ break;
case 409:
# line 1946 "pars.yacc"
{
	    yyval.pset = DISK;
	} /*NOTREACHED*/ break;
case 410:
# line 1949 "pars.yacc"
{
	    yyval.pset = PIPE;
	} /*NOTREACHED*/ break;
case 411:
# line 1955 "pars.yacc"
{
	    yyval.pset = PATTERN;
	} /*NOTREACHED*/ break;
case 412:
# line 1958 "pars.yacc"
{
	    yyval.pset = COLOR;
	} /*NOTREACHED*/ break;
case 413:
# line 1961 "pars.yacc"
{
	    yyval.pset = NONE;
	} /*NOTREACHED*/ break;
case 414:
# line 1967 "pars.yacc"
{
	    yyval.pset = TOP;
	} /*NOTREACHED*/ break;
case 415:
# line 1970 "pars.yacc"
{
	    yyval.pset = BOTTOM;
	} /*NOTREACHED*/ break;
case 416:
# line 1973 "pars.yacc"
{
	    yyval.pset = LEFT;
	} /*NOTREACHED*/ break;
case 417:
# line 1976 "pars.yacc"
{
	    yyval.pset = RIGHT;
	} /*NOTREACHED*/ break;
case 418:
# line 1979 "pars.yacc"
{
	    yyval.pset = BOTH;
	} /*NOTREACHED*/ break;
case 419:
# line 1985 "pars.yacc"
{
	    yyval.pset = RIGHT;
	} /*NOTREACHED*/ break;
case 420:
# line 1988 "pars.yacc"
{
	    yyval.pset = LEFT;
	} /*NOTREACHED*/ break;
case 421:
# line 1991 "pars.yacc"
{
	    yyval.pset = CENTER;
	} /*NOTREACHED*/ break;
case 422:
# line 1997 "pars.yacc"
{
	    yyval.pset = XMIN;
	} /*NOTREACHED*/ break;
case 423:
# line 2000 "pars.yacc"
{
	    yyval.pset = XMAX;
	} /*NOTREACHED*/ break;
case 424:
# line 2003 "pars.yacc"
{
	    yyval.pset = YMIN;
	} /*NOTREACHED*/ break;
case 425:
# line 2006 "pars.yacc"
{
	    yyval.pset = YMAX;
	} /*NOTREACHED*/ break;
case 426:
# line 2012 "pars.yacc"
{
	    yyval.pset = TRUEP;
	} /*NOTREACHED*/ break;
case 427:
# line 2015 "pars.yacc"
{
	    yyval.pset = FALSEP;
	} /*NOTREACHED*/ break;
case 428:
# line 2021 "pars.yacc"
{
	    yyval.pset = IN;
	} /*NOTREACHED*/ break;
case 429:
# line 2024 "pars.yacc"
{
	    yyval.pset = OUT;
	} /*NOTREACHED*/ break;
case 430:
# line 2027 "pars.yacc"
{
	    yyval.pset = BOTH;
	} /*NOTREACHED*/ break;
case 431:
# line 2033 "pars.yacc"
{
	    yyval.pset = DECIMAL;
	} /*NOTREACHED*/ break;
case 432:
# line 2036 "pars.yacc"
{
	    yyval.pset = EXPONENTIAL;
	} /*NOTREACHED*/ break;
case 433:
# line 2039 "pars.yacc"
{
	    yyval.pset = POWER;
	} /*NOTREACHED*/ break;
case 434:
# line 2042 "pars.yacc"
{
	    yyval.pset = GENERAL;
	} /*NOTREACHED*/ break;
case 435:
# line 2045 "pars.yacc"
{
	    yyval.pset = DDMMYY;
	} /*NOTREACHED*/ break;
case 436:
# line 2048 "pars.yacc"
{
	    yyval.pset = MMDDYY;
	} /*NOTREACHED*/ break;
case 437:
# line 2051 "pars.yacc"
{
	    yyval.pset = MMYY;
	} /*NOTREACHED*/ break;
case 438:
# line 2054 "pars.yacc"
{
	    yyval.pset = MMDD;
	} /*NOTREACHED*/ break;
case 439:
# line 2057 "pars.yacc"
{
	    yyval.pset = MONTHDAY;
	} /*NOTREACHED*/ break;
case 440:
# line 2060 "pars.yacc"
{
	    yyval.pset = DAYMONTH;
	} /*NOTREACHED*/ break;
case 441:
# line 2063 "pars.yacc"
{
	    yyval.pset = MONTHS;
	} /*NOTREACHED*/ break;
case 442:
# line 2066 "pars.yacc"
{
	    yyval.pset = MONTHL;
	} /*NOTREACHED*/ break;
case 443:
# line 2069 "pars.yacc"
{
	    yyval.pset = DAYOFWEEKS;
	} /*NOTREACHED*/ break;
case 444:
# line 2072 "pars.yacc"
{
	    yyval.pset = DAYOFWEEKL;
	} /*NOTREACHED*/ break;
case 445:
# line 2075 "pars.yacc"
{
	    yyval.pset = DAYOFYEAR;
	} /*NOTREACHED*/ break;
case 446:
# line 2078 "pars.yacc"
{
	    yyval.pset = HMS;
	} /*NOTREACHED*/ break;
case 447:
# line 2081 "pars.yacc"
{
	    yyval.pset = MMDDHMS;
	} /*NOTREACHED*/ break;
case 448:
# line 2084 "pars.yacc"
{
	    yyval.pset = MMDDYYHMS;
	} /*NOTREACHED*/ break;
case 449:
# line 2087 "pars.yacc"
{
	    yyval.pset = DEGREESLON;
	} /*NOTREACHED*/ break;
case 450:
# line 2090 "pars.yacc"
{
	    yyval.pset = DEGREESMMLON;
	} /*NOTREACHED*/ break;
case 451:
# line 2093 "pars.yacc"
{
	    yyval.pset = DEGREESMMSSLON;
	} /*NOTREACHED*/ break;
case 452:
# line 2096 "pars.yacc"
{
	    yyval.pset = MMSSLON;
	} /*NOTREACHED*/ break;
case 453:
# line 2099 "pars.yacc"
{
	    yyval.pset = DEGREESLAT;
	} /*NOTREACHED*/ break;
case 454:
# line 2102 "pars.yacc"
{
	    yyval.pset = DEGREESMMLAT;
	} /*NOTREACHED*/ break;
case 455:
# line 2105 "pars.yacc"
{
	    yyval.pset = DEGREESMMSSLAT;
	} /*NOTREACHED*/ break;
case 456:
# line 2108 "pars.yacc"
{
	    yyval.pset = MMSSLAT;
	} /*NOTREACHED*/ break;
case 457:
# line 2114 "pars.yacc"
{
	    yyval.pset = NORMAL;
	} /*NOTREACHED*/ break;
case 458:
# line 2117 "pars.yacc"
{
	    yyval.pset = ABSOLUTE;
	} /*NOTREACHED*/ break;
case 459:
# line 2120 "pars.yacc"
{
	    yyval.pset = NEGATE;
	} /*NOTREACHED*/ break;
case 460:
# line 2126 "pars.yacc"
{
	    yyval.pset = UP;
	} /*NOTREACHED*/ break;
case 461:
# line 2129 "pars.yacc"
{
	    yyval.pset = DOWN;
	} /*NOTREACHED*/ break;
case 462:
# line 2132 "pars.yacc"
{
	    yyval.pset = RIGHT;
	} /*NOTREACHED*/ break;
case 463:
# line 2135 "pars.yacc"
{
	    yyval.pset = LEFT;
	} /*NOTREACHED*/ break;
case 464:
# line 2138 "pars.yacc"
{
	    yyval.pset = IN;
	} /*NOTREACHED*/ break;
case 465:
# line 2141 "pars.yacc"
{
	    yyval.pset = OUT;
	} /*NOTREACHED*/ break;
case 466:
# line 2147 "pars.yacc"
{
	    yyval.pset = WORLD;
	} /*NOTREACHED*/ break;
case 467:
# line 2150 "pars.yacc"
{
	    yyval.pset = VIEW;
	} /*NOTREACHED*/ break;
case 468:
# line 2157 "pars.yacc"
{
	    int itmp = (int) yypvt[-3].val - 1;
	    if (itmp >= ls) {
		yyerror("subscript out of range");
		return 1;
	    } else {
		yypvt[-5].ptr[itmp] = yypvt[-0].val;
		result = yypvt[-0].val;
	    }
	} /*NOTREACHED*/ break;
case 469:
# line 2168 "pars.yacc"
{
	    int itmp = (int) yypvt[-3].val - 1;
	    if (itmp >= lxy) {
		yyerror("subscript out of range");
		return 1;
	    } else {
		yypvt[-5].ptr[itmp] = yypvt[-0].val;
		result = yypvt[-0].val;
	    }
	    updatesetminmax(cg, curset);
	    update_set_status(cg, curset);
	} /*NOTREACHED*/ break;
case 470:
# line 2181 "pars.yacc"
{
	    /*
	     * what is on the stack is a pointer to x we need a pointer to
	     * the set SETNUM x or y 
	     */
	    int itmp = (int) yypvt[-3].val - 1;
	    if (yypvt[-5].ptr == xx) {
		g[cg].p[yypvt[-7].pset].ex[0][itmp] = yypvt[-0].val;
	    } else {
		g[cg].p[yypvt[-7].pset].ex[1][itmp] = yypvt[-0].val;
	    }
	    result = yypvt[-0].val;
	    updatesetminmax(cg, yypvt[-7].pset);
	    update_set_status(cg, yypvt[-7].pset);
	} /*NOTREACHED*/ break;
case 471:
# line 2197 "pars.yacc"
{
	    /*
	     * what is on the stack is a pointer to x we need a pointer to
	     * the set SETNUM x or y 
	     */
	    int itmp = (int) yypvt[-3].val - 1;
	    if (yypvt[-5].ptr == xx) {
		g[yypvt[-9].pset].p[yypvt[-7].pset].ex[0][itmp] = yypvt[-0].val;
	    } else if (yypvt[-5].ptr == yy) {
		g[yypvt[-9].pset].p[yypvt[-7].pset].ex[1][itmp] = yypvt[-0].val;
	    }
	    result = yypvt[-0].val;
	    updatesetminmax(yypvt[-9].pset, yypvt[-7].pset);
	    update_set_status(yypvt[-9].pset, yypvt[-7].pset);
	} /*NOTREACHED*/ break;
case 472:
# line 2216 "pars.yacc"
{
	    if (yypvt[-2].ptr == xx) {
		*xx = yypvt[-0].val;
	    } else {
		*yy = yypvt[-0].val;
	    }
	} /*NOTREACHED*/ break;
case 473:
# line 2227 "pars.yacc"
{
	    int i;
	    for (i = 0; i < lxy; i++) {
		yypvt[-2].ptr[i] = yypvt[-0].ptr[i];
	    }
	    result = yypvt[-0].ptr[0];
	} /*NOTREACHED*/ break;
case 474:
# line 2235 "pars.yacc"
{
	    int i;
	    for (i = 0; i < lxy; i++) {
		yypvt[-2].ptr[i] = yypvt[-0].ptr[i];
	    }
	    result = yypvt[-0].ptr[0];
	    updatesetminmax(cg, curset);
	    update_set_status(cg, curset);
	} /*NOTREACHED*/ break;
case 475:
# line 2245 "pars.yacc"
{
	    int i;
	    double *tmp;
	    if (!isactive_set(cg, yypvt[-4].pset)) {
		activateset(cg, yypvt[-4].pset);
		setlength(cg, yypvt[-4].pset, lxy);
		setcomment(cg, yypvt[-4].pset, "Created");
	    }
	    if (yypvt[-2].ptr == xx) {
		tmp = g[cg].p[yypvt[-4].pset].ex[0];
	    } else {
		tmp = g[cg].p[yypvt[-4].pset].ex[1];
	    }
	    for (i = 0; i < lxy; i++) {
		tmp[i] = yypvt[-0].ptr[i];
	    }
	    result = yypvt[-0].ptr[0];
	    updatesetminmax(cg, yypvt[-4].pset);
	    update_set_status(cg, yypvt[-4].pset);
	} /*NOTREACHED*/ break;
case 476:
# line 2266 "pars.yacc"
{
	    int i;
	    double *tmp;
	    if (!isactive_set(yypvt[-6].pset, yypvt[-4].pset)) {
		activateset(yypvt[-6].pset, yypvt[-4].pset);
		setlength(yypvt[-6].pset, yypvt[-4].pset, lxy);
		setcomment(yypvt[-6].pset, yypvt[-4].pset, "Created");
	    }
	    if (yypvt[-2].ptr == xx) {
		tmp = g[yypvt[-6].pset].p[yypvt[-4].pset].ex[0];
	    } else {
		tmp = g[yypvt[-6].pset].p[yypvt[-4].pset].ex[1];
	    }
	    for (i = 0; i < lxy; i++) {
		tmp[i] = yypvt[-0].ptr[i];
	    }
	    result = yypvt[-0].ptr[0];
	    updatesetminmax(yypvt[-6].pset, yypvt[-4].pset);
	    update_set_status(yypvt[-6].pset, yypvt[-4].pset);
	} /*NOTREACHED*/ break;
case 477:
# line 2287 "pars.yacc"
{
	    int i;
	    for (i = 0; i < lxy; i++) {
		yypvt[-2].ptr[i] = yypvt[-0].val;
	    }
	    result = yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 478:
# line 2295 "pars.yacc"
{
	    int i;
	    for (i = 0; i < lxy; i++) {
		yypvt[-2].ptr[i] = yypvt[-0].val;
	    }
	    result = yypvt[-0].val;
	    updatesetminmax(cg, curset);
	    update_set_status(cg, curset);
	} /*NOTREACHED*/ break;
case 479:
# line 2305 "pars.yacc"
{
	    int i;
	    double *tmp;
	    if (yypvt[-2].ptr == xx) {
		tmp = g[cg].p[yypvt[-4].pset].ex[0];
	    } else {
		tmp = g[cg].p[yypvt[-4].pset].ex[1];
	    }
	    for (i = 0; i < lxy; i++) {
		tmp[i] = yypvt[-0].val;
	    }
	    result = yypvt[-0].val;
	    updatesetminmax(cg, yypvt[-4].pset);
	    update_set_status(cg, yypvt[-4].pset);
	} /*NOTREACHED*/ break;
case 480:
# line 2321 "pars.yacc"
{
	    int i;
	    double *tmp;
	    if (yypvt[-2].ptr == xx) {
		tmp = g[yypvt[-6].pset].p[yypvt[-4].pset].ex[0];
	    } else {
		tmp = g[yypvt[-6].pset].p[yypvt[-4].pset].ex[1];
	    }
	    for (i = 0; i < lxy; i++) {
		tmp[i] = yypvt[-0].val;
	    }
	    result = yypvt[-0].val;
	    updatesetminmax(yypvt[-6].pset, yypvt[-4].pset);
	    update_set_status(yypvt[-6].pset, yypvt[-4].pset);
	} /*NOTREACHED*/ break;
case 481:
# line 2340 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = yypvt[-0].ptr[i];
	    }
	} /*NOTREACHED*/ break;
case 482:
# line 2349 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = yypvt[-0].ptr[i];
	    }
	} /*NOTREACHED*/ break;
case 483:
# line 2358 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		if (yypvt[-0].ptr == xx) {
		    yyval.ptr[i] = g[yypvt[-4].pset].p[yypvt[-2].pset].ex[0][i];
		} else {
		    yyval.ptr[i] = g[yypvt[-4].pset].p[yypvt[-2].pset].ex[1][i];
		}
	    }
	} /*NOTREACHED*/ break;
case 484:
# line 2371 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		if (yypvt[-0].ptr == xx) {
		    yyval.ptr[i] = g[cg].p[yypvt[-2].pset].ex[0][i];
		} else {
		    yyval.ptr[i] = g[cg].p[yypvt[-2].pset].ex[1][i];
		}
	    }
	} /*NOTREACHED*/ break;
case 485:
# line 2384 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = yypvt[-0].val;
	    }
	} /*NOTREACHED*/ break;
case 486:
# line 2393 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = yypvt[-2].val + yypvt[-0].val;
	    }
	} /*NOTREACHED*/ break;
case 487:
# line 2402 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = yypvt[-2].ptr[i] + yypvt[-0].ptr[i];
	    }
	} /*NOTREACHED*/ break;
case 488:
# line 2411 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = yypvt[-2].val + yypvt[-0].ptr[i];
	    }
	} /*NOTREACHED*/ break;
case 489:
# line 2420 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = yypvt[-2].ptr[i] + yypvt[-0].val;
	    }
	} /*NOTREACHED*/ break;
case 490:
# line 2429 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = yypvt[-2].val - yypvt[-0].val;
	    }
	} /*NOTREACHED*/ break;
case 491:
# line 2438 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = yypvt[-2].ptr[i] - yypvt[-0].ptr[i];
	    }
	} /*NOTREACHED*/ break;
case 492:
# line 2447 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = yypvt[-2].val - yypvt[-0].ptr[i];
	    }
	} /*NOTREACHED*/ break;
case 493:
# line 2456 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = yypvt[-2].ptr[i] - yypvt[-0].val;
	    }
	} /*NOTREACHED*/ break;
case 494:
# line 2465 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = yypvt[-2].val * yypvt[-0].val;
	    }
	} /*NOTREACHED*/ break;
case 495:
# line 2474 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = yypvt[-2].ptr[i] * yypvt[-0].ptr[i];
	    }
	} /*NOTREACHED*/ break;
case 496:
# line 2483 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = yypvt[-2].val * yypvt[-0].ptr[i];
	    }
	} /*NOTREACHED*/ break;
case 497:
# line 2492 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = yypvt[-2].ptr[i] * yypvt[-0].val;
	    }
	} /*NOTREACHED*/ break;
case 498:
# line 2501 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    if (yypvt[-0].val == 0.0) {
		yyerror("Divide by Zero");
		return 1;
	    }
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = yypvt[-2].val / yypvt[-0].val;
	    }
	} /*NOTREACHED*/ break;
case 499:
# line 2514 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		if (yypvt[-0].ptr[i] == 0.0) {
		    yyerror("Divide by Zero");
		    return 1;
		}
	    }
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = yypvt[-2].ptr[i] / yypvt[-0].ptr[i];
	    }
	} /*NOTREACHED*/ break;
case 500:
# line 2529 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		if (yypvt[-0].ptr[i] == 0.0) {
		    yyerror("Divide by Zero");
		    return 1;
		}
	    }
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = yypvt[-2].val / yypvt[-0].ptr[i];
	    }
	} /*NOTREACHED*/ break;
case 501:
# line 2544 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    if (yypvt[-0].val == 0.0) {
		yyerror("Divide by Zero");
		return 1;
	    }
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = yypvt[-2].ptr[i] / yypvt[-0].val;
	    }
	} /*NOTREACHED*/ break;
case 502:
# line 2557 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = pow(yypvt[-2].val, yypvt[-0].val);
	    }
	} /*NOTREACHED*/ break;
case 503:
# line 2566 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = pow(yypvt[-2].val, yypvt[-0].ptr[i]);
	    }
	} /*NOTREACHED*/ break;
case 504:
# line 2575 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = pow(yypvt[-2].ptr[i], yypvt[-0].val);
	    }
	} /*NOTREACHED*/ break;
case 505:
# line 2584 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = pow(yypvt[-2].ptr[i], yypvt[-0].ptr[i]);
	    }
	} /*NOTREACHED*/ break;
case 506:
# line 2593 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = fabs(yypvt[-1].val);
	    }
	} /*NOTREACHED*/ break;
case 507:
# line 2602 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = fabs(yypvt[-1].ptr[i]);
	    }
	} /*NOTREACHED*/ break;
case 508:
# line 2611 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = acos(yypvt[-1].ptr[i]);
	    }
	} /*NOTREACHED*/ break;
case 509:
# line 2620 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = asin(yypvt[-1].ptr[i]);
	    }
	} /*NOTREACHED*/ break;
case 510:
# line 2629 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = atan(yypvt[-1].ptr[i]);
	    }
	} /*NOTREACHED*/ break;
case 511:
# line 2638 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = atan2(yypvt[-3].ptr[i], yypvt[-1].ptr[i]);
	    }
	} /*NOTREACHED*/ break;
case 512:
# line 2647 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = ceil(yypvt[-1].ptr[i]);
	    }
	} /*NOTREACHED*/ break;
case 513:
# line 2656 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = cos(yypvt[-1].ptr[i]);
	    }
	} /*NOTREACHED*/ break;
case 514:
# line 2665 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] *= M_PI / 180.0;
	    }
	} /*NOTREACHED*/ break;
case 515:
# line 2674 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = xx[i];
	    }
	} /*NOTREACHED*/ break;
case 516:
# line 2683 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = yy[i];
	    }
	} /*NOTREACHED*/ break;
case 517:
# line 2692 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = erf(yypvt[-1].ptr[i]);
	    }
	} /*NOTREACHED*/ break;
case 518:
# line 2701 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = erfc(yypvt[-1].ptr[i]);
	    }
	} /*NOTREACHED*/ break;
case 519:
# line 2710 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = exp(yypvt[-1].ptr[i]);
	    }
	} /*NOTREACHED*/ break;
case 520:
# line 2719 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = floor(yypvt[-1].ptr[i]);
	    }
	} /*NOTREACHED*/ break;
case 521:
# line 2728 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = hypot(yypvt[-3].ptr[i], yypvt[-1].ptr[i]);
	    }
	} /*NOTREACHED*/ break;
case 522:
# line 2737 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = hypot(yypvt[-3].val, yypvt[-1].ptr[i]);
	    }
	} /*NOTREACHED*/ break;
case 523:
# line 2746 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = hypot(yypvt[-3].ptr[i], yypvt[-1].val);
	    }
	} /*NOTREACHED*/ break;
case 524:
# line 2755 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = hypot(yypvt[-3].val, yypvt[-1].val);
	    }
	} /*NOTREACHED*/ break;
case 525:
# line 2764 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = i + 1;
	    }
	} /*NOTREACHED*/ break;
case 526:
# line 2773 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = yypvt[-0].func;
	    }
	} /*NOTREACHED*/ break;
case 527:
# line 2782 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = (int) yypvt[-1].ptr[i];
	    }
	} /*NOTREACHED*/ break;
case 528:
# line 2791 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = invnorm(yypvt[-1].ptr[i]);
	    }
	} /*NOTREACHED*/ break;
case 529:
# line 2800 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = invt(yypvt[-3].val, (int) yypvt[-1].val);
	    }
	} /*NOTREACHED*/ break;
case 530:
# line 2809 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = invt(yypvt[-3].ptr[i], (int) yypvt[-1].val);
	    }
	} /*NOTREACHED*/ break;
case 531:
# line 2818 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = lrand48() % (long) (yypvt[-1].val);
	    }
	} /*NOTREACHED*/ break;
case 532:
# line 2827 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = lgamma(yypvt[-1].ptr[i]);
	    }
	} /*NOTREACHED*/ break;
case 533:
# line 2836 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = log(yypvt[-1].ptr[i]);
	    }
	} /*NOTREACHED*/ break;
case 534:
# line 2845 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = log10(yypvt[-1].ptr[i]);
	    }
	} /*NOTREACHED*/ break;
case 535:
# line 2854 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = 1.0 / (1.0 + exp(-(yypvt[-5].ptr[i] - yypvt[-3].val)/ yypvt[-1].val));
	    }
	} /*NOTREACHED*/ break;
case 536:
# line 2863 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = yypvt[-3].ptr[i] >= yypvt[-1].ptr[i] ? yypvt[-3].ptr[i] : yypvt[-1].ptr[i];
	    }
	} /*NOTREACHED*/ break;
case 537:
# line 2872 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = yypvt[-3].ptr[i] <= yypvt[-1].ptr[i] ? yypvt[-3].ptr[i] : yypvt[-1].ptr[i];
	    }
	} /*NOTREACHED*/ break;
case 538:
# line 2881 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = fmod(yypvt[-3].ptr[i], yypvt[-1].ptr[i]);
	    }
	} /*NOTREACHED*/ break;
case 539:
# line 2890 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = fx(yypvt[-1].ptr[i]);
	    }
	} /*NOTREACHED*/ break;
case 540:
# line 2899 "pars.yacc"
{
	    int i;
	    double tmp;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = normp(yypvt[-1].ptr[i], &tmp);
	    }
	} /*NOTREACHED*/ break;
case 541:
# line 2909 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = M_PI;
	    }
	} /*NOTREACHED*/ break;
case 542:
# line 2918 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = M_PI / 180.0;
	    }
	} /*NOTREACHED*/ break;
case 543:
# line 2927 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = (double) drand48();
	    }
	} /*NOTREACHED*/ break;
case 544:
# line 2936 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = rnorm(yypvt[-3].ptr[i], yypvt[-1].ptr[i]);
	    }
	} /*NOTREACHED*/ break;
case 545:
# line 2945 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = rnorm(yypvt[-3].val, yypvt[-1].ptr[i]);
	    }
	} /*NOTREACHED*/ break;
case 546:
# line 2954 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = rnorm(yypvt[-3].ptr[i], yypvt[-1].val);
	    }
	} /*NOTREACHED*/ break;
case 547:
# line 2963 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = rnorm(yypvt[-3].val, yypvt[-1].val);
	    }
	} /*NOTREACHED*/ break;
case 548:
# line 2972 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = sin(yypvt[-1].ptr[i]);
	    }
	} /*NOTREACHED*/ break;
case 549:
# line 2981 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = yypvt[-1].ptr[i] * yypvt[-1].ptr[i];
	    }
	} /*NOTREACHED*/ break;
case 550:
# line 2990 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = sqrt(yypvt[-1].ptr[i]);
	    }
	} /*NOTREACHED*/ break;
case 551:
# line 2999 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = tan(yypvt[-1].ptr[i]);
	    }
	} /*NOTREACHED*/ break;
case 552:
# line 3008 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = yypvt[-2].ptr[i] > yypvt[-0].ptr[i];
	    }
	} /*NOTREACHED*/ break;
case 553:
# line 3017 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = yypvt[-2].ptr[i] < yypvt[-0].ptr[i];
	    }
	} /*NOTREACHED*/ break;
case 554:
# line 3026 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = yypvt[-2].ptr[i] <= yypvt[-0].ptr[i];
	    }
	} /*NOTREACHED*/ break;
case 555:
# line 3035 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = yypvt[-2].ptr[i] >= yypvt[-0].ptr[i];
	    }
	} /*NOTREACHED*/ break;
case 556:
# line 3044 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = yypvt[-2].ptr[i] == yypvt[-0].ptr[i];
	    }
	} /*NOTREACHED*/ break;
case 557:
# line 3053 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = yypvt[-2].ptr[i] != yypvt[-0].ptr[i];
	    }
	} /*NOTREACHED*/ break;
case 558:
# line 3062 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = yypvt[-2].ptr[i] && yypvt[-0].ptr[i];
	    }
	} /*NOTREACHED*/ break;
case 559:
# line 3071 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = yypvt[-2].ptr[i] || yypvt[-0].ptr[i];
	    }
	} /*NOTREACHED*/ break;
case 560:
# line 3080 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = !(yypvt[-0].ptr[i]);
	    }
	} /*NOTREACHED*/ break;
case 561:
# line 3089 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = yypvt[-1].ptr[i];
	    }
	} /*NOTREACHED*/ break;
case 562:
# line 3097 "pars.yacc"
{
	    int i;
	    yyval.ptr = (double *) calloc(lxy, sizeof(double));
	    freelist[fcnt++] = yyval.ptr;
	    for (i = 0; i < lxy; i++) {
		yyval.ptr[i] = -yypvt[-0].ptr[i];
	    }
	} /*NOTREACHED*/ break;
case 564:
# line 3108 "pars.yacc"
{
	    yyval.val = yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 565:
# line 3111 "pars.yacc"
{
	    yyval.val = yypvt[-3].ptr[(int) yypvt[-1].val];
	} /*NOTREACHED*/ break;
case 566:
# line 3114 "pars.yacc"
{
	    yyval.val = yypvt[-3].ptr[(int) yypvt[-1].val - 1];
	} /*NOTREACHED*/ break;
case 567:
# line 3117 "pars.yacc"
{
	    yyval.val = (yypvt[-0].ptr == xx) ? *xx : *yy;
	} /*NOTREACHED*/ break;
case 568:
# line 3120 "pars.yacc"
{
	    if (yypvt[-3].ptr == xx) {
		yyval.val = g[cg].p[yypvt[-5].pset].ex[0][(int) yypvt[-1].val - 1];
	    } else if (yypvt[-3].ptr == yy) {
		yyval.val = g[cg].p[yypvt[-5].pset].ex[1][(int) yypvt[-1].val - 1];
	    }
	} /*NOTREACHED*/ break;
case 569:
# line 3127 "pars.yacc"
{
	    switch (yypvt[-0].pset) {
	    case XMIN:
		yyval.val = g[cg].p[yypvt[-2].pset].xmin;
		break;
	    case YMIN:
		yyval.val = g[cg].p[yypvt[-2].pset].ymin;
		break;
	    case XMAX:
		yyval.val = g[cg].p[yypvt[-2].pset].xmax;
		break;
	    case YMAX:
		yyval.val = g[cg].p[yypvt[-2].pset].ymax;
		break;
	    }
	} /*NOTREACHED*/ break;
case 570:
# line 3143 "pars.yacc"
{
	    yyval.val = g[cg].p[yypvt[-2].pset].len;
	} /*NOTREACHED*/ break;
case 571:
# line 3147 "pars.yacc"
{
	    double bar, sd;
	    if (yypvt[-1].ptr == xx) {
		stasum(getx(cg, yypvt[-3].pset), getsetlength(cg, yypvt[-3].pset), &bar, &sd, 0);
	    } else if (yypvt[-1].ptr == yy) {
		stasum(gety(cg, yypvt[-3].pset), getsetlength(cg, yypvt[-3].pset), &bar, &sd, 0);
	    }
	    yyval.val = bar;
	} /*NOTREACHED*/ break;
case 572:
# line 3156 "pars.yacc"
{
	    if (yypvt[-3].ptr == xx) {
		yyval.val = g[yypvt[-7].pset].p[yypvt[-5].pset].ex[0][(int) yypvt[-1].val - 1];
	    } else if (yypvt[-3].ptr == yy) {
		yyval.val = g[yypvt[-7].pset].p[yypvt[-5].pset].ex[1][(int) yypvt[-1].val - 1];
	    }
	} /*NOTREACHED*/ break;
case 573:
# line 3163 "pars.yacc"
{
	    yyval.val = yypvt[-2].val + yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 574:
# line 3166 "pars.yacc"
{
	    yyval.val = yypvt[-2].val - yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 575:
# line 3169 "pars.yacc"
{
	    yyval.val = yypvt[-2].val * yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 576:
# line 3173 "pars.yacc"
{
	    if (yypvt[-0].val != 0.0) {
		yyval.val = yypvt[-2].val / yypvt[-0].val;
	    } else {
		yyerror("Divide by Zero");
		return 1;
	    }
	} /*NOTREACHED*/ break;
case 577:
# line 3181 "pars.yacc"
{
	    yyval.val = fmod(yypvt[-2].val, yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 578:
# line 3184 "pars.yacc"
{
	    yyval.val = pow(yypvt[-2].val, yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 579:
# line 3187 "pars.yacc"
{
	    yyval.val = fabs(yypvt[-1].val);
	} /*NOTREACHED*/ break;
case 580:
# line 3190 "pars.yacc"
{
	    yyval.val = acos(yypvt[-1].val);
	} /*NOTREACHED*/ break;
case 581:
# line 3193 "pars.yacc"
{
	    yyval.val = asin(yypvt[-1].val);
	} /*NOTREACHED*/ break;
case 582:
# line 3196 "pars.yacc"
{
	    yyval.val = atan(yypvt[-1].val);
	} /*NOTREACHED*/ break;
case 583:
# line 3199 "pars.yacc"
{
	    yyval.val = atan2(yypvt[-3].val, yypvt[-1].val);
	} /*NOTREACHED*/ break;
case 584:
# line 3202 "pars.yacc"
{
	    yyval.val = ceil(yypvt[-1].val);
	} /*NOTREACHED*/ break;
case 585:
# line 3205 "pars.yacc"
{
	    yyval.val = cos(yypvt[-1].val);
	} /*NOTREACHED*/ break;
case 586:
# line 3208 "pars.yacc"
{
	    yyval.val = 180.0 / M_PI;
	} /*NOTREACHED*/ break;
case 587:
# line 3211 "pars.yacc"
{
	    yyval.val = *xx;
	} /*NOTREACHED*/ break;
case 588:
# line 3214 "pars.yacc"
{
	    yyval.val = *yy;
	} /*NOTREACHED*/ break;
case 589:
# line 3217 "pars.yacc"
{
	    yyval.val = erf(yypvt[-1].val);
	} /*NOTREACHED*/ break;
case 590:
# line 3220 "pars.yacc"
{
	    yyval.val = erfc(yypvt[-1].val);
	} /*NOTREACHED*/ break;
case 591:
# line 3223 "pars.yacc"
{
	    yyval.val = exp(yypvt[-1].val);
	} /*NOTREACHED*/ break;
case 592:
# line 3226 "pars.yacc"
{
	    yyval.val = floor(yypvt[-1].val);
	} /*NOTREACHED*/ break;
case 593:
# line 3229 "pars.yacc"
{
	    yyval.val = hypot(yypvt[-3].val, yypvt[-1].val);
	} /*NOTREACHED*/ break;
case 594:
# line 3232 "pars.yacc"
{
	    yyval.val = g[yypvt[-2].pset].v.xv1;
	} /*NOTREACHED*/ break;
case 595:
# line 3235 "pars.yacc"
{
	    yyval.val = g[yypvt[-2].pset].v.xv2;
	} /*NOTREACHED*/ break;
case 596:
# line 3238 "pars.yacc"
{
	    yyval.val = g[yypvt[-2].pset].v.yv1;
	} /*NOTREACHED*/ break;
case 597:
# line 3241 "pars.yacc"
{
	    yyval.val = g[yypvt[-2].pset].v.yv2;
	} /*NOTREACHED*/ break;
case 598:
# line 3244 "pars.yacc"
{
	    yyval.val = g[yypvt[-2].pset].w.xg1;
	} /*NOTREACHED*/ break;
case 599:
# line 3247 "pars.yacc"
{
	    yyval.val = g[yypvt[-2].pset].w.xg2;
	} /*NOTREACHED*/ break;
case 600:
# line 3250 "pars.yacc"
{
	    yyval.val = g[yypvt[-2].pset].w.yg1;
	} /*NOTREACHED*/ break;
case 601:
# line 3253 "pars.yacc"
{
	    yyval.val = g[yypvt[-2].pset].w.yg2;
	} /*NOTREACHED*/ break;
case 602:
# line 3256 "pars.yacc"
{
	    yyval.val = g[cg].v.xv1;
	} /*NOTREACHED*/ break;
case 603:
# line 3259 "pars.yacc"
{
	    yyval.val = g[cg].v.xv2;
	} /*NOTREACHED*/ break;
case 604:
# line 3262 "pars.yacc"
{
	    yyval.val = g[cg].v.yv1;
	} /*NOTREACHED*/ break;
case 605:
# line 3265 "pars.yacc"
{
	    yyval.val = g[cg].v.yv2;
	} /*NOTREACHED*/ break;
case 606:
# line 3268 "pars.yacc"
{
	    yyval.val = g[cg].w.xg1;
	} /*NOTREACHED*/ break;
case 607:
# line 3271 "pars.yacc"
{
	    yyval.val = g[cg].w.xg2;
	} /*NOTREACHED*/ break;
case 608:
# line 3274 "pars.yacc"
{
	    yyval.val = g[cg].w.yg1;
	} /*NOTREACHED*/ break;
case 609:
# line 3277 "pars.yacc"
{
	    yyval.val = g[cg].w.yg2;
	} /*NOTREACHED*/ break;
case 610:
# line 3280 "pars.yacc"
{
	    yyval.val = setindex;
	} /*NOTREACHED*/ break;
case 611:
# line 3283 "pars.yacc"
{
	    yyval.val = setsetno;
	} /*NOTREACHED*/ break;
case 612:
# line 3286 "pars.yacc"
{
	    yyval.val = (long) yypvt[-1].val;
	} /*NOTREACHED*/ break;
case 613:
# line 3289 "pars.yacc"
{
	    yyval.val = invnorm(yypvt[-1].val);
	} /*NOTREACHED*/ break;
case 614:
# line 3292 "pars.yacc"
{
	    yyval.val = invt(yypvt[-3].val, (int) yypvt[-1].val);
	} /*NOTREACHED*/ break;
case 615:
# line 3295 "pars.yacc"
{
	    yyval.val = lrand48() % (long) (yypvt[-1].val);
	} /*NOTREACHED*/ break;
case 616:
# line 3298 "pars.yacc"
{
	    yyval.val = lgamma(yypvt[-1].val);
	} /*NOTREACHED*/ break;
case 617:
# line 3301 "pars.yacc"
{
	    yyval.val = log(yypvt[-1].val);
	} /*NOTREACHED*/ break;
case 618:
# line 3304 "pars.yacc"
{
	    yyval.val = log10(yypvt[-1].val);
	} /*NOTREACHED*/ break;
case 619:
# line 3308 "pars.yacc"
{
	    yyval.val = 1.0 / (1.0 + exp(-(yypvt[-5].val - yypvt[-3].val)/ yypvt[-1].val));
	} /*NOTREACHED*/ break;
case 620:
# line 3311 "pars.yacc"
{
	    yyval.val = yypvt[-3].val >= yypvt[-1].val ? yypvt[-3].val : yypvt[-1].val;
	} /*NOTREACHED*/ break;
case 621:
# line 3314 "pars.yacc"
{
	    yyval.val = yypvt[-3].val <= yypvt[-1].val ? yypvt[-3].val : yypvt[-1].val;
	} /*NOTREACHED*/ break;
case 622:
# line 3317 "pars.yacc"
{
	    yyval.val = fmod(yypvt[-3].val, yypvt[-1].val);
	} /*NOTREACHED*/ break;
case 623:
# line 3320 "pars.yacc"
{
	    yyval.val = fx(yypvt[-1].val);
	} /*NOTREACHED*/ break;
case 624:
# line 3323 "pars.yacc"
{
	    double tmp;
	    yyval.val = normp(yypvt[-1].val, &tmp);
	} /*NOTREACHED*/ break;
case 625:
# line 3327 "pars.yacc"
{
	    yyval.val = M_PI;
	} /*NOTREACHED*/ break;
case 626:
# line 3330 "pars.yacc"
{
	    yyval.val = M_PI / 180.0;
	} /*NOTREACHED*/ break;
case 627:
# line 3333 "pars.yacc"
{
	    yyval.val = (double) drand48();
	} /*NOTREACHED*/ break;
case 628:
# line 3336 "pars.yacc"
{
	    yyval.val = rnorm(yypvt[-3].val, yypvt[-1].val);
	} /*NOTREACHED*/ break;
case 629:
# line 3339 "pars.yacc"
{
	    yyval.val = sin(yypvt[-1].val);
	} /*NOTREACHED*/ break;
case 630:
# line 3342 "pars.yacc"
{
	    yyval.val = pow(yypvt[-1].val, 2.0);
	} /*NOTREACHED*/ break;
case 631:
# line 3345 "pars.yacc"
{
	    yyval.val = sqrt(yypvt[-1].val);
	} /*NOTREACHED*/ break;
case 632:
# line 3348 "pars.yacc"
{
	    yyval.val = tan(yypvt[-1].val);
	} /*NOTREACHED*/ break;
case 633:
# line 3351 "pars.yacc"
{
	    if ((int) yypvt[-2].val)
		yyval.val = yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 634:
# line 3355 "pars.yacc"
{
	    if ((int) yypvt[-4].val) {
		yyval.val = yypvt[-2].val;
	    } else {
		yyval.val = yypvt[-0].val;
	    }
	} /*NOTREACHED*/ break;
case 635:
# line 3362 "pars.yacc"
{
	    yyval.val = yypvt[-2].val > yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 636:
# line 3365 "pars.yacc"
{
	    yyval.val = yypvt[-2].val < yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 637:
# line 3368 "pars.yacc"
{
	    yyval.val = yypvt[-2].val <= yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 638:
# line 3371 "pars.yacc"
{
	    yyval.val = yypvt[-2].val >= yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 639:
# line 3374 "pars.yacc"
{
	    yyval.val = yypvt[-2].val == yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 640:
# line 3377 "pars.yacc"
{
	    yyval.val = yypvt[-2].val != yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 641:
# line 3380 "pars.yacc"
{
	    yyval.val = yypvt[-2].val && yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 642:
# line 3383 "pars.yacc"
{
	    yyval.val = yypvt[-2].val || yypvt[-0].val;
	} /*NOTREACHED*/ break;
case 643:
# line 3386 "pars.yacc"
{
	    yyval.val = !(yypvt[-0].val);
	} /*NOTREACHED*/ break;
case 644:
# line 3389 "pars.yacc"
{
	    yyval.val = yypvt[-1].val;
	} /*NOTREACHED*/ break;
case 645:
# line 3392 "pars.yacc"
{
	    yyval.val = -yypvt[-0].val;
	} /*NOTREACHED*/ break;
}


        goto yystack;           /* reset registers in driver code */
}
