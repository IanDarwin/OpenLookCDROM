#include <sys/types.h>
#include <sys/timeb.h>
#include <sys/time.h>
#include <X11/Xlib.h>
#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/notify.h>
#include "xvmines.h"

#define INITRAND        srandom(time((time_t *)0))
#define MAXRANDOM       2147483648.0	/* 2^31 */
#define RAND01          (((double)(random()))/MAXRANDOM)
#define FRAND(min,max)  (RAND01*((max)-(min))+(min))
#define RAND(min,max)   (int)FRAND(min,max)

extern Display *display;
extern GC       def_gc;

extern cell    *Cells;
extern int      width, height, first_tile, checks, mines_2_go, closed_tiles,
                mines, first, playing, quantum, secs;
extern char     buff[];
extern struct itimerval timer;
extern Pixmap   pmap, icons[NUM_ICONS];
extern Window   dr_win;
extern Frame    frame;
extern Panel_item opt;
extern Canvas   canvas;
extern Notify_func active_func;

#ifdef __STDC__
static void     settime(Notify_client, int);
#else
static void     settime();
#endif


#ifdef __STDC__
get_cell(int where_x, int where_y, int *i, int *j)
#else
get_cell(where_x, where_y, i, j)
	int             where_x, where_y, *i, *j;
#endif
{
	*i = where_y / IC_HEIGHT;
	*j = where_x / IC_WIDTH;
}

#ifdef __STDC__
place_tiles(int len, int num_mines, int keep_empty)
#else
place_tiles(len, num_mines, keep_empty)
	int             len, num_mines, keep_empty;
#endif
{
	register int    i, j;
	int             where, aux, cnt;

	for (i = 0; i < len; i++) {
		(Cells + i)->in_state = EMPTY;
		(Cells + i)->out_state = CLOSED;
	}
	(Cells + keep_empty)->in_state = MINE;

	INITRAND;

	for (i = 0; i < num_mines;) {
		aux = RAND(0, len);
		if ((Cells + aux)->in_state == MINE)
			continue;
		(Cells + aux)->in_state = MINE;
		i++;
	}

	(Cells + keep_empty)->in_state = EMPTY;

	for (j = 0; j < width; j++)
		for (i = 0; i < height; i++) {

			where = i * width + j;
			if ((Cells + where)->in_state == MINE)
				continue;

			cnt = 0;
			if (i - 1 >= 0) {
				if ((Cells + where - width)->in_state == MINE)
					cnt++;
				if (j - 1 >= 0)
					if ((Cells + where - width - 1)->in_state == MINE)
						cnt++;
				if (j + 1 < width)
					if ((Cells + where - width + 1)->in_state == MINE)
						cnt++;
			}
			if (i + 1 < height) {
				if ((Cells + where + width)->in_state == MINE)
					cnt++;
				if (j - 1 >= 0)
					if ((Cells + where + width - 1)->in_state == MINE)
						cnt++;
				if (j + 1 < width)
					if ((Cells + where + width + 1)->in_state == MINE)
						cnt++;

			}
			if (j - 1 >= 0)
				if ((Cells + where - 1)->in_state == MINE)
					cnt++;

			if (j + 1 < width)
				if ((Cells + where + 1)->in_state == MINE)
					cnt++;
			(Cells + where)->in_state = cnt;
		}

#ifdef DEBUG
	for (i = 0; i < height; i++) {
		for (j = 0; j < width; j++)
			if ((Cells + i * width + j)->in_state == MINE)
				printf("* ");
			else
				printf("%d ", (Cells + i * width + j)->in_state);
		printf("\n");
	}
#endif

}


#ifdef __STDC__
open_tile(int i, int j)
#else
open_tile(i, j)
	int             i, j;
#endif
{
	cell           *aux;

	if (first_tile) {
		notify_set_itimer_func(canvas, (Notify_func) settime, ITIMER_REAL, &timer, NULL);
		active_func = (Notify_func) (settime);
		/* setitimer(ITIMER_REAL,&timer,NULL); */
		first_tile = FALSE;
	}
	aux = Cells + i * width + j;
	if (aux->out_state != CLOSED && aux->out_state != MARKED && aux->out_state != QUEST)
		return FALSE;

	if (aux->in_state == MINE) {
		finish("  Game Over");
	} else if (aux->in_state == EMPTY)
		return auto_open(i, j);
	else {
		if (aux->out_state == MARKED) {
			checks--;
			sprintf(buff, "Checks: %d  \n", checks);
			xv_set(frame, FRAME_RIGHT_FOOTER, buff, NULL);
		}
		PLACE_ICON(icons[aux->in_state], i * IC_WIDTH,
			   j * IC_HEIGHT);
		aux->out_state = aux->in_state;
		closed_tiles--;
		return TRUE;
	}
}

#ifdef __STDC__
mark_tile(int i, int j)
#else
mark_tile(i, j)
	int             i, j;
#endif
{
	cell           *aux;

	aux = Cells + i * width + j;

	if (aux->out_state == MARKED || aux->out_state != CLOSED && aux->out_state != QUEST)
		return FALSE;

	PLACE_ICON(icons[MARKED], i * IC_WIDTH, j * IC_HEIGHT);
	aux->out_state = MARKED;
	checks++;

	if (aux->in_state == MINE)
		mines_2_go--;

	sprintf(buff, "Checks: %d\n", checks);
	xv_set(frame, FRAME_RIGHT_FOOTER, buff, NULL);
	return TRUE;
}

#ifdef __STDC__
auto_open(int i, int j)
#else
auto_open(i, j)
	int             i, j;
#endif
{
	int             where;
	cell           *ptr;

	ptr = Cells + i * width + j;
	if (ptr->out_state != CLOSED && ptr->out_state != MARKED && ptr->out_state != QUEST
	    || ptr->in_state > EIGHT)
		return FALSE;

	if (ptr->out_state == MARKED) {
		checks--;
		sprintf(buff, "Checks: %d\n", checks);
		xv_set(frame, FRAME_RIGHT_FOOTER, buff, NULL);
	}
	PLACE_ICON(icons[ptr->in_state], i * IC_WIDTH, j * IC_HEIGHT);
	ptr->out_state = ptr->in_state;
	closed_tiles--;

	if (ptr->in_state == EMPTY) {
		if (i - 1 >= 0) {
			auto_open(i - 1, j);
			if (j - 1 >= 0)
				auto_open(i - 1, j - 1);
			if (j + 1 < width)
				auto_open(i - 1, j + 1);
		}
		if (i + 1 < height) {
			auto_open(i + 1, j);
			if (j - 1 >= 0)
				auto_open(i + 1, j - 1);
			if (j + 1 < width)
				auto_open(i + 1, j + 1);
		}
		if (j - 1 >= 0)
			auto_open(i, j - 1);
		if (j + 1 < width)
			auto_open(i, j + 1);

		return TRUE;
	}
}

int
init()
{
	register int    i;

	mines_2_go = mines;
	first = playing = TRUE;
	closed_tiles = width * height;
	checks = 0;
	for (i = 0; i < width * height; i++)
#ifndef DEBUG
		PLACE_ICON(icons[CLOSED], (i / width) * IC_WIDTH, (i % width) * IC_HEIGHT);
#else
		PLACE_ICON(icons[(Cells + i)->in_state], (i / width) * IC_WIDTH, (i % width) * IC_HEIGHT);
#endif

	XCopyArea(display, pmap, dr_win, def_gc, 0, 0,
		  width * IC_WIDTH, height * IC_HEIGHT, 0, 0);
	print_string("Time->> 0:0", TIME_X, TIME_Y, CLEAR_YES);
	xv_set(frame, FRAME_RIGHT_FOOTER, "Checks: 0", NULL);
	secs = 0;
	first_tile = TRUE;
	notify_set_itimer_func(canvas, NOTIFY_FUNC_NULL, ITIMER_REAL, &timer, NULL);
	active_func = NOTIFY_FUNC_NULL;
}

#ifdef __STDC__
show_placements(int len)
#else
show_placements(len)
	int             len;
#endif
{
	register int    i;

	for (i = 0; i < len; i++)
		if ((Cells + i)->in_state == MINE)
			PLACE_ICON(icons[MINE], (i / width) * IC_WIDTH,
				   (i % width) * IC_HEIGHT);
	XCopyArea(display, pmap, dr_win, def_gc, 0, 0,
		  width * IC_WIDTH, height * IC_HEIGHT, 0, 0);
}

#ifdef __STDC__
static void
settime(Notify_client client, int which)
#else
static void
settime(client, which)
	Notify_client   client;
	int             which;
#endif
{
	secs += quantum;
	sprintf(buff, "Time->> %2d:%2d", secs / 60, secs % 60);
	print_string(buff, TIME_X, TIME_Y, CLEAR_YES);
}

#ifdef __STDC__
void
finish(char *result)
#else
void
finish(result)
	char           *result;
#endif
{
	struct itimerval ovalue;

	playing = FALSE;
	show_placements(width * height);
	xv_set(opt, PANEL_INACTIVE, FALSE, NULL);
	/* getitimer(ITIMER_REAL,&timer); */
	notify_set_itimer_func(canvas, NOTIFY_FUNC_NULL, ITIMER_REAL, &timer, &ovalue);
	active_func = NOTIFY_FUNC_NULL;
	secs += quantum - ovalue.it_value.tv_sec;
	sprintf(buff, "Time->> %2d:%2d", secs / 60, secs % 60);
	print_string(buff, TIME_X, TIME_Y, CLEAR_YES);
	print_string(result, RESULT_X, RESULT_Y, CLEAR_NO);
}
