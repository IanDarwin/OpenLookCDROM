/*
 * Crumble your screen.
 *
 * This program must be run on a SUN workstation only.  However, you can
 * run it under either X or Suntools or on a bare sun monitor.  If you want
 * to do all out war (see below) or use the -w option, you must use suntools.
 *
 * Written by:
 *
 *	Don Hatch		Dan Heller
 *  splat@ucscb.ucsc.edu   island!argv@sun.com
 *
 * common usage:
 *   subtle (rlogin somewhere else and run this)
 *     crumble -hf
 *     crumble -hf
 *   fun and cute (let these run all night)
 *     crumble -d
 *     crumble -d -c
 *   destructive (but fun)
 *     crumble -C -c
 *     crumble -f -d -c
 *   all out WAR (and *LOTS* of fun! mwahahahaha!!)
 *     crumble -C -f -m 200 | rsh other_sun crumble -C -f (suntools required)
 *   (note: you should be able to _see_ the victim's console for best effect)
 *
 * \fBPlease send creative ideas for more usage to the authors!\fR
 *
 * compile: (triple click the next line and stuff it)
	cc -O -s crumble.c -lpixrect -o crumble
 *
 * Command line options:
 *
 * -v		verbose
 * -vf		flip chars vertically
 * -hf		swap chars horizontally
 * -s timeout	sleep between each char
 * -a timeout   I forget what this is for
 * -m number	max pixels to try to blow up
 * -u useconds  before explosion, flash this fast
 * -t		testing
 * -b		start at bottom of screen and eat up
 * -B		same, but grab the whole character
 * -c		blow up letters on contact
 * -f [N]	show target on fire button (default 1000)
 * -d		drop characters without crumbling
 *		note, if -c specified, chars drop until contact with something
 * -C		grab chunks of screen and obliterate them
 * -r		reverse video for monochrome monitors
 * -value	value is a color value for "off" pixels (color displays only)
 * +value	value is a color value for "on" pixels (color displays only)
 *
 * If there are remaining args (one is checked), it is the display device name.
 * For example, on a Sun3-110, you might want to use your greyscale screen,
 * not your monochrome screen.  So, you could specify:
 * crumble -255 +254 /dev/fb
 * for color, or:
 * crumble /dev/bwtwo0
 * for monochrome.
 *
 * Obviously, you can't mix some of these arguments.  Trail and error
 * is the best way to figure out which don't work.
 *
 * Environment variable: DEV_FB to default to a frame buffer.
 */
#include <stdio.h>
#include <sys/file.h>
#include <math.h>
#include <signal.h>
#include <ctype.h>
#include <pixrect/pixrect.h>
#include <pixrect/memvar.h>

#define round(x) floor((x)+.5)
#define ison(x,y) (pr_get(pr,x,y) == ON)
#define isoff(x,y) (pr_get(pr,x,y) == OFF)
#define put(x,y,z) pr_put(pr,x,y,z)
#define swap(a,b) (a ^= b ^= a ^= b)

int ON = 1, OFF = 0;
struct pixrect *pr, *pr2;
int MAX = 300;
int n;
struct pr_pos A[10000];
int verbose, sleepytime, usleepytime, ubombsleepytime, alarmtime, testing,
	hflip, vflip,
	do_crumble, bottom_up, bottom_up_2, do_drop, do_chunks,
	do_focus;
char *device = NULL;
#define DEFAULT_DEVICE "/dev/fb"

#define append(x,y) (n>=MAX ? 0 : (A[n].x=x,A[n].y=y,n++,1))
#define when break; case
#define otherwise break; default
char *getenv();

#define XOR (PIX_SRC ^ PIX_DST)

#define max(a,b) ((a)>(b)?(a):(b))
#define min(a,b) ((a)<(b)?(a):(b))
#define max4(a,b,c,d) max(max(max(a,b),c),d)
#define min4(a,b,c,d) min(min(min(a,b),c),d)

static void
unfetch()
{
    int i;
    for (i=0; i < n; ++i) {
	pr_put(pr2, A[i].x,A[i].y,1);
    }
}

int edge_ok = 1;

/*
 * return 1 if stayed on screen, -1 if went off screen or got too big.
 */
static int
_fetch(x,y)
register int x,y;
{
    int i;
    char ind[8];
    static struct pr_pos dirs[8] = {
    {-1,0}, {1,0}, {0,-1}, {0,1}, {-1,-1}, {-1,1}, {1,-1}, {1,1}
    };

    if (pr_get(pr,x,y) == PIX_ERR)
	return edge_ok ? 0 : -1;
    if (isoff(x,y) || !pr_get(pr2, x,y))
	return 0;
    for (i = 1; i < 8; ++i)
	ind[i] = i;
    if (append(x,y)) {
	pr_put(pr2,x,y,0);
	if (do_chunks) {
	    register int i1, i2;
	    for (i = 1; i <= 5; ++i) {
	        i1 = (rand() / 23) % 4;
	        i2 = (rand() / 23) % 4;	/* scramble first four directions */
		if (i1 != i2)
		    swap(ind[i1], ind[i2]);
	    }
	}
	for (i = 0; i < 8; ++i)
	    if (_fetch(x+dirs[ind[i]].x, y+dirs[ind[i]].y) == -1)
		return -1;
	return 1;
    } else
	return do_chunks ? 1 : -1; /* if do_chunks, then return the chunk */
}

/*
 * return 1 on success (got something)
 * 0 if got nothing,
 * -1 if went off edge
 */
static int
fetch(pr,x,y)
struct pixrect *pr;
int x, y;
{
    if (!ison(x,y))
	return 0;
    if (do_chunks && ison(x,y))
	while (ison(x,y+1))
	    y++;
    return _fetch(x,y);
}

static void
frame(pr, xmin, ymin, xmax, ymax, op, value)
struct pixrect *pr;
register int xmin, xmax, ymin, ymax, op, value;
{
    pr_vector(pr, xmin, ymin, xmax, ymin, op, 1);
    pr_vector(pr, xmax, ymin, xmax, ymax, op, 1);
    pr_vector(pr, xmax, ymax, xmin, ymax, op, 1);
    pr_vector(pr, xmin, ymax, xmin, ymin, op, 1);
}

static void
focus(pr, x,y)
struct pixrect *pr;
register int x,y;
{
    int size = max4(x, pr->pr_width-x, y, pr->pr_height-y);
    size = min(size, do_focus);

    for (; size >= 0; --size) {
	frame(pr, x-size, y-size, x+size, y+size, PIX_NOT(PIX_DST));
	frame(pr, x-size, y-size, x+size, y+size, PIX_NOT(PIX_DST));
    }
}

static void
buzz(pr, p, off_x, off_y)
struct pixrect *pr, *p;
register int off_x,off_y;
{
    int size = max4(off_x, pr->pr_width-off_x, off_y, pr->pr_height-off_x);
    size = min(size, do_focus);

    if (p)
    for (; size >= 0; --size) {
	pr_rop(pr, off_x, off_y, p->pr_width, p->pr_height, PIX_NOT(PIX_DST),
							NULL, 0,0);
	if (ubombsleepytime)
#ifdef NO_POLL
	    usleep(ubombsleepytime);
#else /*NO_POLL*/
	    poll(0, 0, ubombsleepytime/1000);
#endif /*NO_POLL*/
	pr_rop(pr, off_x, off_y, p->pr_width, p->pr_height, PIX_NOT(PIX_DST),
							NULL, 0,0);
	if (ubombsleepytime)
#ifdef NO_POLL
	    usleep(ubombsleepytime);
#else /*NO_POLL*/
	    poll(0, 0, ubombsleepytime/1000);
#endif /*NO_POLL*/
    }
}

static void
crumble(pr)
struct pixrect *pr;
{
    int moved, i;

    do {
	moved = 0;
	for (i = 0; i < n; ++i) {
	    struct pr_pos pos, newpos;
	    pos = newpos = A[i];
	    if ((rand() / 23) % 30 && isoff(pos.x, pos.y+1))
		newpos.y = pos.y+1;
	    else
		switch((rand() / 23) % 2) {
		    when 0:
			if (isoff(pos.x+1, pos.y+1)) {
			    newpos.x = pos.x+1;
			    newpos.y = pos.y+1;
			} else goto defaul;
		    when 1:
			if (isoff(pos.x-1, pos.y+1)) {
			    newpos.x = pos.x-1;
			    newpos.y = pos.y+1;
			} else goto defaul;
		    otherwise:
		    defaul:
			if (isoff(pos.x, pos.y+1))
			    newpos.y = pos.y+1;
		}
	    if (newpos.x != pos.x || newpos.y != pos.y) {
		moved = 1;
		if ((rand() / 23) % 30) {
		    pr_put(pr, pos.x, pos.y, OFF);
		    pr_put(pr, newpos.x, newpos.y, ON);
		    A[i] = newpos;
		}
	    }
	}
    } while (moved);
}

/*
 * Make a small pixrect containing the points currently stored in the A array.
 * Coordinates of the new pixrect with respect to the global pr are returned
 * in *x, *y.
 */
static struct pixrect *
make_letter_pr(x, y)
int *x, *y;
{
    int i;
    register int minx,maxx,miny,maxy;
    struct pixrect *p;
    minx=maxx=A[0].x;
    miny=maxy=A[0].y;

    for (i=1; i < n; ++i) {
	minx = min(minx, A[i].x);
	miny = min(miny, A[i].y);
	maxx = max(maxx, A[i].x);
	maxy = max(maxy, A[i].y);
    }
    *x = minx-1;
    *y = miny-1;
    p = mem_create(maxx-minx+3, maxy-miny+3, pr->pr_depth);
    for (i=0; i < n; ++i) {
	pr_put(p, A[i].x - *x, A[i].y - *y, ON);
    }
    return p;
}

static void
pr_reverse_rop(dpr,dx,dy,w,h,op,spr,sx,sy)
struct pixrect *spr, *dpr;
int dx, dy, w, h, op, sx, sy;
{
    int x,y;

    for (y=0;y<h;++y)
	for (x=0;x<w;++x)
	    pr_rop(dpr, dx+x, dy+y, 1, 1, op, spr, sx + (vflip? w-1-x : x),
						   sy + (hflip? h-1-y : y));
}

static void
reverse(pr,p,x,y,w,h)
struct pixrect *pr, *p;
int x, y, w, h;
{
    pr_rop(pr,x,y,w,h,XOR,p,0,0);
    pr_reverse_rop(pr,x,y,w,h,XOR,p,0,0);
}

static void
dump(p)
struct pixrect *p;
{
    int x,y;

    printf("size=%d,%d\n",p->pr_width, p->pr_height);
    puts("=================================");
    for (y=0; y < p->pr_height; ++y) {
        for (x=0; x < p->pr_width; ++x)
	    printf("%c ", (pr_get(p,x,y) == ON) ? '*' : ' ');
	puts("");
    }
    puts("=================================");
}

static int
canfall(pr,p,x,y,w,h)
struct pixrect *pr, *p;
int x, y, w, h;
{
    int i,j;

    for (j=0;j<h;++j)
    for (i=0;i<w;++i)
	if (pr_get(p,i,j) == ON && pr_get(p,i,j+1) != ON)
	    if (pr_get(pr,x+i,y+j) != OFF)
		return 0;
    return 1;
}

static struct pr_pos
drop(pr,p,x,y,w,h)
struct pixrect *pr, *p;
int x, y, w, h;
{
    register int X,Y;
    struct pr_pos dest;

    while (canfall(pr,p,x,y+1,w,h)) {
	for (Y=h-1; Y >= 0; Y--)
	    for (X=0; X < w; X++)
	    if (pr_get(p, X, Y) == ON) {
	        pr_put(pr, X+x, Y+y, OFF);
	        pr_put(pr, X+x, Y+y+1, ON);
	    }
	y++;
    }
    dest.x = x;
    dest.y = y;
    return dest;
}

static void
putlist()
{
    int i;
    int x,y;
    struct pr_pos dest;
    struct pixrect *p = make_letter_pr(&x, &y);

    if (verbose)
	dump(p);
    if (do_focus) {
	focus(pr, A[0].x, A[0].y);
	buzz(pr, p, x, y);
    }
    if (hflip || vflip)
	reverse(pr, p, x, y, p->pr_width, p->pr_height);
    if (do_drop) {
        dest = drop(pr, p, x, y, p->pr_width, p->pr_height);
	for (i = 0; i < n; ++i)
	    A[i].y += dest.y - y;
    }
    if (do_crumble)
	crumble(pr);
    pr_destroy(p);
}

static void
do_letter(x,y)
register int x,y;
{

    if (testing) {
	printf("%d ", pr_get(pr,x,y));
	return;
    }
    n = 0;
    if (ison(x,y) && fetch(pr,x,y) == 1) {
	unfetch();
	if (verbose)
	    printf("%d ", n);
	putlist();
	if (sleepytime)
	    sleep(sleepytime);
	if (usleepytime)
#ifdef NO_POLL
	    usleep(usleepytime);
#else /*NO_POLL*/
	    poll(0, 0, usleepytime/1000);
#endif /*NO_POLL*/
    } else
        unfetch();
}

static void
shoot(x,y)
int *x, *y;
{

    while (1) {
	if (!isoff(*x,*y))
	    break;
	if (verbose)
	    pr_put(pr,*x,*y,ON);
	--*x;
	if (!isoff(*x,*y))
	    break;
	if (verbose)
	    pr_put(pr,*x,*y,ON);
	--*y;
    }
}

void
main(argc, argv)
int argc;
char **argv;
{
    register int x,y;

    while (*++argv) {
	if (!strcmp(*argv, "-v"))
	    verbose = 1;
	else if (!strcmp(*argv, "-vf")) /* flip chars vertically */
	    vflip = 1;
	else if (!strcmp(*argv, "-hf")) /* swap chars horizontally */
	    hflip = 1;
	else if (!strcmp(*argv, "-s"))  /* be subtle, sleep between chars */
	    sleepytime = atoi(*++argv);
	else if (!strcmp(*argv, "-S"))  /* be subtle, usleep between chars */
	    usleepytime = atoi(*++argv);
	else if (!strcmp(*argv, "-a"))  /* i forget what this is for */
	    alarmtime = atoi(*++argv);
	else if (!strcmp(*argv, "-m"))  /* max pixels to try to blow up */
	    MAX = atoi(*++argv);
	else if (!strcmp(*argv, "-u"))  /* before explosion, flash this fast */
	    ubombsleepytime = atoi(*++argv);
	else if (!strcmp(*argv, "-t"))
	    testing = 1;
	else if (!strcmp(*argv, "-b"))  /* start at the bottom and eat up */
	    bottom_up = 1;
	else if (!strcmp(*argv, "-B"))  /* same, but grab the whole character */
	    bottom_up_2 = 1;
	else if (!strcmp(*argv, "-c"))  /* blow up (crumble) on contact */
	    do_crumble = 1;
	else if (!strcmp(*argv, "-f")) { /* show target on fire button */
	    if (argv[1] && isdigit(argv[1][0]))
		do_focus = atoi(*++argv);
	    else
	        do_focus = 1000;
	}
	else if (!strcmp(*argv, "-d"))  /* drop characters without crumbling */
	    do_drop = 1;  /* note, if -c specified, chars drop until contact */
	else if (!strcmp(*argv, "-C"))  /* grab chunks of screen */
	    do_chunks = 1;
	else if (!strcmp(*argv, "-r"))  /* reverse video for monochrome */
	    OFF = !(ON = !ON);
	else if (**argv == '-' && isdigit(argv[0][1]))
	    OFF = atoi(*argv + 1); /* set off pixels explicitly */
	else if (**argv == '+' && isdigit(argv[0][1]))
	    ON = atoi(*argv + 1); /* set on pixels explicitly */
	else
	    device = *argv; /* if other than /dev/fb */
    }
    if (!device && !(device = getenv("DEV_FB")))
	device = DEFAULT_DEVICE;

    if (!(pr = pr_open(device)))
	exit(1); /* pr_open does a perror anyway! how stupid */

    if (!(pr2 = mem_create(pr->pr_width, pr->pr_height, 1)))
	perror("mem_create"), exit(1);

    pr_rop(pr2,0,0,pr2->pr_width, pr2->pr_height,PIX_NOT(PIX_DST),NULL,0,0);

    srand(getpid());
    if (bottom_up) {
	for (y = pr->pr_height-1; y >= 0; --y) {
	    n = 0;
	    for (x = 0; x < pr->pr_width; ++x)
		if (ison(x,y)) {
		    A[n].x = x;
		    A[n].y = y;
		    n++;
		}
	    crumble(pr);
	}
    } else if (bottom_up_2)
	while (1)
	for (y = pr->pr_height-1; y >= 0; y -= 2)
	    for (x = 0; x < pr->pr_width; ++x)
		do_letter(x,y);
    else
	while (1) {
	    y = (rand() / 23) % (pr->pr_height-1);
	    x = (rand() / 23) % (pr->pr_width-1);
	    do_letter(x,y);
	}
}
