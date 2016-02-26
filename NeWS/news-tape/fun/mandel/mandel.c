#ifndef lint
static  char sccsid[] = "@(#)mandel.c Copyr 1987 Sun Micro";
#endif

/*
 * mandel.c
 *
 * 871022 - fixed problems with quitting and zapping
 * 870801 - fisrt release
 *
 * A NeWS demo from Corporate Sales Engineering.  (demobench@sun)
 *
 */

#include <stdio.h>
#include <signal.h>
#include <sys/ioctl.h>
#include "mandel.h"

#define WIDTH 600
#define HEIGHT 400
#define ITER 1024
#define STARTX -2.0
#define ENDX 1.0
#define STARTY -1.3
#define ENDY 1.3
#define NAREAS 15

#define FALSE 0
#define TRUE 1

int iterations;

int itera[] = { 3, 4, 5, 6, 7, 8, 10, 15, 25, 50, 100, 200, 400, 800, 1600 };


quit()
{
    ps_flush_PostScript();
    sleep(1);
    ps_close_PostScript();
    exit(0);
}


main()
{
register int x,y;
int i;
char data[WIDTH+1],hostname[25];
int fd;

    data[WIDTH] = '\0';
    ps_open_PostScript();
    gethostname(hostname,25);
    pps_init(WIDTH,HEIGHT,NAREAS,hostname);
    ps_flush_PostScript();
    fgetc(PostScriptInput);	/* Wait here for server to initialize */
    fgetc(PostScriptInput);
    signal(SIGHUP,quit);
    signal(SIGINT,quit);
    signal(SIGTERM,quit);
    for (i = 1; i <= NAREAS; i++) {
	pps_setcolor(i);
	iterations  = itera[i-1];
	draw_mand();
	pps_end();
	ps_flush_PostScript();
    }
    ps_flush_PostScript();
    sleep(-1);
    quit();
}

#define DELTAX ((ENDX - STARTX)/WIDTH)
#define DELTAY ((ENDY - STARTY)/HEIGHT)

#define MAND_LESS       	1 
#define MAND_MORE       	2 

int n_mand_members;
char a[HEIGHT][WIDTH];
int have_started;
int count, done;
int startp_x, startp_y;

draw_mand()
{
register int i,j,x,y;
int  zero_real, zero_im;
double start_real, start_im;

	have_started = FALSE;

	for (y=0; y<HEIGHT; y++)
	    for (x=0; x<WIDTH; x++)
		a[y][x] = 0;
	n_mand_members = 0;
	count = 0;
	done=FALSE;

	/* Check border of window. */
	start_real = STARTX;
	start_im = ENDY;
	/* Top */
	j = 0;
	for (i=0; i<WIDTH; i++) {
		mand_check_me_and_my_neighbors(i, j, start_real, start_im);
		start_real += DELTAX;
	}
	/* Right */
	i = WIDTH - 1;
	start_real = STARTX + DELTAX*i;
	start_im -= DELTAY;
	for (j=1; j<HEIGHT; j++) {
		mand_check_me_and_my_neighbors(i, j, start_real, start_im);
		start_im -= DELTAY;
	}
	/* Bottom */
	j = HEIGHT - 1;
	start_im = ENDY - DELTAY*j;
	start_real -= DELTAX;
	for (i=WIDTH-1; i>=0; i--) {
		mand_check_me_and_my_neighbors(i, j, start_real, start_im); 
                start_real -= DELTAX;
	}
	/* Left */
	i = 0;
	start_real = STARTX;
	start_im += DELTAY; 
        for (j=HEIGHT-1; j>=0; j--) {
		mand_check_me_and_my_neighbors(i, j, start_real, start_im); 
                start_im += DELTAY; 
        }

	/* If no points found, search from 0,0. */
	if (n_mand_members > 0)
		return;
	if (STARTX < 0.0  &&  STARTX + (WIDTH*DELTAX) > 0.0   &&
	    ENDY > 0.0   &&   ENDY - (HEIGHT*DELTAY) < 0.0) {
		i = zero_real = (int)((0.0 - STARTX) / DELTAX);
		j = zero_im = (int)(ENDY / DELTAY);
		start_real = STARTX + DELTAX*i;
		start_im = ENDY - DELTAY*j;
		while (i < WIDTH) {
			mand_check_me_and_my_neighbors(i, j, start_real, start_im);
			if (n_mand_members > 0)
				return;
			start_real += DELTAX;
			i++;
		}
		i = zero_real - 1;
		start_real = STARTX + DELTAX*i;
		while (i >= 0) {
			mand_check_me_and_my_neighbors(i, j, start_real, start_im); 
                        if (n_mand_members > 0) 
                                return; 
                        start_real -= DELTAX; 
                        i--;
		}
		i = zero_real;
		start_real = STARTX + DELTAX*i;
		while (j < HEIGHT) {
                        mand_check_me_and_my_neighbors(i, j, start_real, start_im);  
                        if (n_mand_members > 0) 
                                return; 
                        start_im -= DELTAY; 
			j++;
		}
		j = zero_im - 1;
		start_im = ENDY - DELTAY*j;
		while (j >= 0) { 
                        mand_check_me_and_my_neighbors(i, j, start_real, start_im);   
                        if (n_mand_members > 0)  
                                return;  
                        start_im += DELTAY;  
                        j++; 
                }
	}
	if (n_mand_members > 0)
                return;

	/* Still no luck!  Search exhaustively. */
	start_im = ENDY;
	for (j=1; j<HEIGHT-1; j++) {
		start_real = STARTX + DELTAX;
		for (i=1; i<WIDTH-1; i++) {
			mand_check_me_and_my_neighbors(i, j, start_real, start_im);
                        if (n_mand_members > 0)
                                return;
			start_real += DELTAX;
		}
		start_im -= DELTAY;
	}
}


/*
 * Checks the pixel (x,y) - which corresponds to the complex point
 * (rl,im) - to see if it belongs to the sought border.  If it
 * does, recursively checks all of its neighbors.
 *
 */

mand_check_me_and_my_neighbors(x, y, rl, im)
int x, y;
double rl, im;
{
    int val;
    int n;
    long arg;


    /* Pixel must be visible */
    if (x < 0  ||  x >= WIDTH  ||  y < 0  ||  y >= HEIGHT) {
	return;
    }

    /* Is this point already determined? */
    if (a[y][x] != 0) {
	return;			/* Yes, done */
    }

    /* No, is it in the sought boundary? */
    val = mand_set(x, y, rl, im);
    if (val == FALSE) {
	return;			/* No, done */
    }
    if (!have_started) {
	pps_start(x,y);
	startp_x = x;
	startp_y = y;
	have_started = TRUE;
    }
    else {
	ps_lineto(x, y);	/* Yes, turn pixel on */
    }
    count++;

    a[y][x] = 1;
    n_mand_members++;
    if (count > 50 && abs(startp_x - x) < 3 && abs(startp_y - y) < 3) {
	done = TRUE;
	return;
    }
    if (psio_availinputbytes(PostScriptInput) > 0 ||
          (ioctl(psio_fileno(PostScriptInput), FIONREAD, &arg), arg) > 0)
      check_input();

    /* Check neighbors */
    mand_check_me_and_my_neighbors(x-1, y, rl-DELTAX, im); /* W */
    if (!done)
      mand_check_me_and_my_neighbors(x-1, y+1, rl-DELTAX, im-DELTAY); /* SW*/
    if (!done)
      mand_check_me_and_my_neighbors(x, y+1, rl, im-DELTAY); /* S */
    if (!done)
      mand_check_me_and_my_neighbors(x+1, y+1, rl+DELTAX, im-DELTAY); /* SE*/
    if (!done)
      mand_check_me_and_my_neighbors(x+1, y, rl+DELTAX, im); /* E */
    if (!done)
      mand_check_me_and_my_neighbors(x+1, y-1, rl+DELTAX, im+DELTAY); /* NE*/
    if (!done)
      mand_check_me_and_my_neighbors(x, y-1, rl, im+DELTAY); /* N */
    if (!done)
      mand_check_me_and_my_neighbors(x-1, y-1, rl-DELTAX, im+DELTAY); /* NW*/
}

	



check_mand(real_0, im_0)
double real_0, im_0;
{
	int n_steps;
	double real, im, real_squared, im_squared;
 
        n_steps = 1;
        real = real_0;
        im = im_0;

        while (n_steps <= iterations) {
                real_squared = real*real;
                im_squared = im * im;
                if (real_squared + im_squared   >=   4)
                        return(MAND_LESS);
                n_steps++;
                im = 2*real*im + im_0;
                real = real_squared - im_squared + real_0;
        }
        return(MAND_MORE);
}


/*
 * Returns TRUE if specified point/pixel is member of Mandelbrot
 * boundary, otherwise returns FALSE.  A pixel is considered to be
 * a member of the sought boundary if at least one of its corners
 * is greater than the boundary value, and at least one corner is
 * not.
 *
 */

mand_set(x, y, rl, im)
int x, y;
double rl, im;
{
	int vote;
	double half_DELTAX, half_DELTAY;

	half_DELTAX = DELTAX / 2.0;
	half_DELTAY = DELTAY / 2.0;

	/* Compute upper-left corner */
	vote = check_mand(rl - half_DELTAX, im + half_DELTAY);

	/* Upper-right corner */
	vote |= check_mand(rl + half_DELTAX, im + half_DELTAY);
	if (vote == (MAND_LESS|MAND_MORE))
		return(TRUE);

	/* Lower-right corner */ 
        vote |= check_mand(rl + half_DELTAX, im - half_DELTAY); 
        if (vote == (MAND_LESS|MAND_MORE)) 
                return(TRUE); 
 
        /* Lower-left corner */ 
        vote = check_mand(rl - half_DELTAX, im - half_DELTAY);  
        if (vote == (MAND_LESS|MAND_MORE))  
                return(TRUE);

	return(FALSE);
}


int check_input()
{
    int i, x1, x2, y1, y2;

    if ((pscanf(PostScriptInput, "%d", &i) == EOF))
	quit();
    switch(i) {
      case NEWAREA:
	pscanf(PostScriptInput, "%d%d%d%d", &x1, &y1, &x2, &y2);
	break;
      case QUIT:
	quit();
    }
}
