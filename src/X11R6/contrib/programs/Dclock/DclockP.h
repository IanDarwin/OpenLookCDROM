/*
 * Dclock.c -- a digital clock widget.
 * Copyright (c) 1988 Dan Heller <argv@sun.com>
 *     Modified  1991 Herve Soulard <soulard@sor.inria.fr>
 *          - support for English and French language
 *          - support for audio on Sparc station
 */
#ifndef _XtDclockP_h
#define _XtDclockP_h

#include <X11/CoreP.h>
#include "Dclock.h"

typedef struct {
    Pixel      		foreground;
    Boolean    		reverse;
    Boolean		tails;		/* put tails on 6 & 9 */
    Boolean		scroll;
    Boolean		fade;
    int			fade_rate;	/* millisec. betw. fade steps */
    Boolean		seconds;
    Boolean		bell;
    Boolean		miltime;
    Boolean		display_time;	/* when false, display alarm time */
    Boolean		alarm;		/* alarm goes off at alarm time */
    String		alarm_time;	/* time alarm goes off "14:30:00" */
    String		date_fmt;
    XFontStruct		*font;
    String              language;
    String              hours_snd;
    String              half_hours_snd;
    String              alarm_snd;

    /* non-resources (e.g. user can't set) */
    XtIntervalId	interval_id;
    GC			foreGC, backGC;
    int			digit_w, digit_h;
    Pixmap		digits[10];
    Pixmap		tiny_digits[10];
    Pixmap		colon[2];
} DclockPart;

typedef struct _DclockRec {
    CorePart	core;
    DclockPart	dclock;
} DclockRec;

typedef struct {int dummy;} DclockClassPart;

typedef struct _DclockClassRec {
    CoreClassPart	core_class;
    DclockClassPart	dclock_class;
} DclockClassRec;

extern DclockClassRec dclockClassRec;

#endif _XtDclockP_h
