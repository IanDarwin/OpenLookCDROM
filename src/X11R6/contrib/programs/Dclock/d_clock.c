/*
 * Dclock.c -- a digital clock widget.
 * Copyright (c) 1988 Dan Heller <argv@z-code.com>
 *     Modified  1991 Herve Soulard <soulard@sor.inria.fr>
 *          - support for English and French language
 *          - support for audio on Sparc station
 * 
 * Copyright (c) 1988-1994  Dan Heller
 * 
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR
 * OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
 * ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 * 
 * Except as contained in this notice, the name of the X Consortium shall
 * not be used in advertising or otherwise to promote the sale, use or
 * other dealings in this Software without prior written authorization
 * from the X Consortium.
 *
 * dclock -- program to demonstrate how to use the digital-clock widget.
 * To specify a date, the date format is a string of characters.  If a
 * character in the string is a %-sign, the next character is examined
 * and a value is inserted into the string.  Example:
 *    dclock -date "Today is %W"
 * The date string will print "Today is" and then the %W will be replaced
 * by the current weekday name.  The parameters are:
 *    %W	full weekday name
 *    %w	three-char weekday name (sun, mon, tue, wed...)
 *    %M	full month name
 *    %m        three-char abbreviation for that month (jan, feb, mar...)
 *    %d	The date (numerical number of the month)
 *    %Y	full year (4 digits)
 *    %y	2-digit year number
 *
 * To specify seconds to be displayed, use "-seconds" or use the resource
 * manager: *Dclock.seconds: on
 */

#include <stdio.h>
#include <X11/Intrinsic.h>
#include "Dclock.h"

static XrmOptionDescRec options[] = {
    {"-date",	 "*Dclock.date",	XrmoptionSepArg, NULL },
    {"-seconds", "*Dclock.seconds",	XrmoptionNoArg, "TRUE" },
    {"-miltime", "*Dclock.miltime",	XrmoptionNoArg, "TRUE" },
    {"-bell",	 "*Dclock.bell",	XrmoptionNoArg, "TRUE" },
    {"-scroll",  "*Dclock.scroll",	XrmoptionNoArg, "TRUE" },
    {"-noscroll","*Dclock.scroll",	XrmoptionNoArg, "FALSE" },
    {"-alarm",   "*Dclock.alarm",	XrmoptionNoArg, "TRUE" },
    {"-noalarm",   "*Dclock.alarm",	XrmoptionNoArg, "FALSE" },
    {"-alarmTime","*Dclock.alarmTime",	XrmoptionSepArg, NULL },
    {"-tails",	 "*Dclock.tails",	XrmoptionNoArg, "TRUE" },
    {"-fade",	 "*Dclock.fade",	XrmoptionNoArg, "TRUE" },
    {"-fadeRate", "*Dclock.fadeRate",	XrmoptionSepArg, NULL },
    {"-language", "*Dclock.language",   XrmoptionSepArg, NULL },
    {"-hours_snd", "*Dclock.hours_snd",  XrmoptionSepArg, NULL },
    {"-half_hours_snd", "*Dclock.half_hours_snd",  XrmoptionSepArg, NULL },
};

static void
Usage(name, args)
String name, *args;
{
    static char *help_message[] = {
	"where options include:",
	"    -bg color			background color",
	"    -fg color			foreground color",
	"    -fn font			font name",
	"    -geometry geom		size of mailbox",
	"    -display host:dpy		X server to contact",
	"    -seconds [on/off]		display seconds",
	"    -miltime [on/off]		display time in 24 hour format",
	"    -bell [on/off]		ring bell each half hour",
	"    -scroll [on/off]		turn on/off scrolling of numbers",
	"    -date \"date format\"	show the date in specified format",
	"    -alarm [on/off]		turn on/off alarm",
	"    -alarmTime hh:mm		Time alarm goes off",
	"    -tails			put tails on 6 and 9",
	"    -fade			fade numbers",
	"    -fadeRate			wait between fade steps (in msec)",
	"    -language			English or French",
	"    -hours_snd	filename	audio file instead of bell (sun only)",
	"    -half_hours_snd filename	audio file instead of bell (sun only)",
	NULL
    };
    char **cpp;

    fprintf(stderr, "Invalid Args:");
    while (*args)
	fprintf(stderr, " \"%s\"", *args++);
    fputc('\n', stderr);
    fprintf(stderr, "usage: %s [-options ...]\n", name);
    for (cpp = help_message; *cpp; cpp++)
	fprintf(stderr, "%s\n", *cpp);
    exit(1);
}

static void
quit()
{
    exit(0);
}

static XtActionsRec actionsList[] = {
    { "quit",	quit },
};

main(argc, argv)
char *argv[];
{
    Widget toplevel, clock_w;
    char *name, *rindex();

    if (name = rindex(argv[0], '/'))
	name++;
    else
	name = argv[0];

    toplevel = XtInitialize(name, "DClock", options, XtNumber(options),
			    &argc, argv);
    XtAddActions(actionsList, 1);

    if (argc != 1)
	Usage(name, argv+1);

    clock_w = XtCreateManagedWidget(name, dclockWidgetClass, toplevel, NULL, 0);
    XtOverrideTranslations(clock_w, XtParseTranslationTable("<Key>q: quit()"));

    XtRealizeWidget(toplevel);
    XtMainLoop();
}

