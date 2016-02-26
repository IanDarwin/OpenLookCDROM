/*
 * @(#)$ month.h,v 8.4 88/04/04 10:46:35 hull Exp $
 */
#ifndef MONTH_H
#define MONTH_H

/*
 * truncated from the original to only include what I need
 * and appended month8.h onto the end.
 *
 * Bill Randle 2/22/89
 */

#define MAX_EVENT_STRING_LENGTH	70	/* maximum length of an event string */

#define READ_WRITE	0		/* open .month with read/write access */
#define READ_ONLY	1		/* open .month with read-only access */

/* structure of an area */
struct area_rec {
	short area;
	short row;
	short col;
};

/* structure of a date */
struct date_rec {
	short month;
	short day;
	short year;
};

/* structure of a time */
struct time_rec {
	short hour;
	short minute;
};

/* structure of an event pointer */
struct eptr_rec {
	struct event_rec *event;	/* event */
	struct eptr_rec *prev_event;	/* previous event */
	struct eptr_rec *next_event;	/* next event */
};

/* structure of a name */
struct name_rec {
	char *name;			/* name */
	struct name_rec *next_name;	/* next name */
};

/*
 * @(#) month8.h,v 8.0 87/11/13 22:50:14 hull Exp
 */

/* structure of an event */
struct event_rec {
	struct date_rec start_date;	/* starting date of the event */
	char monthly;		/* does event occur monthly? */
	char yearly;		/* does event occur yearly? */
	char every;		/* does event occur on every something? */
	char smtwtfs[7];	/* which days of the week are relevant? */
	char nth;		/* does event occur on an nth something? */
	char last;		/* does event occur on a last something? */
	char nth_is_on;		/* is 'nth' selected by user, n is nth above */
	struct time_rec start_time;	/* starting time of the event */
	struct time_rec duration;	/* duration of the event */
	struct time_rec warning;	/* warning of the event */
	struct time_rec end_time;	/* ending time of the event */
	char event_string[MAX_EVENT_STRING_LENGTH];  /* description of event */
	char private;		/* is event private? */
	char anti;		/* is this an anti-event? */
	int event_owner;	/* owner of event */
	char until;		/* does event occur until some date? */
	struct date_rec until_date;	/* date event goes until */
	struct event_rec *next_event;	/* next event */
};

#endif
