/* $Id: calendar.c,v 3.0 1992/09/14 19:42:48 jipping Exp $
 *
 * **********************************************************************
 *
 *    Calendar.c ==> Calendar-related routines.
 *
 *    This set of routines implements the calendar in the upper left of
 *    the todo list.  It stops short of implementing a calendar widget 
 *    for XView.
 *
 *    Copyright (C) 1991 by Mike Jipping and Hope College
 *
 * Revision History:
 *
 * $Log: calendar.c,v $
 * Revision 3.0  1992/09/14  19:42:48  jipping
 * Release 4.0 beta.
 * Changes: (1) Addition of #ifdefs for "tdl"
 *          (2) Addition of "yestercode" routine.
 *
 * Revision 2.0  1992/07/01  19:02:35  jipping
 * Initial release version.
 *
 * Revision 1.0  1991/07/02  12:02:21  jipping
 * Initial revision
 *
 *
 * **********************************************************************
 */

#include "globaldefs.h"

char *monthname[12] = {
   "January", "February", "March", "April", "May", "June", "July",
   "August", "September", "October", "November", "December"
};

/*
 * Determines whether a given year is a leap year
 */

int leapyear (year)
int year;
{
  if (year % 4 == 0 && (year % 100 != 0 || year % 400 == 0))
     return (1);
  else
     return (0);
}

/*
 *  Returns #days in month given month and year, taking
 *    leap years into account for February.
 */

int daysinmonth (month, year)
int month, year;
{
  if (month == 2)          /* Is it February?             */
     if (leapyear (year))  /* If so, is it a leap year?   */
        return (29);       /* 29 days in Feb in leap year */
     else
        return (28);       /* 28 days if not              */
  else{
     if (month > 7)        /* Is it August -> December?   */
        month++;           /* Invert even/odd state if so */
     if (month & 1)        /* Odd months have 31 days     */
        return (31);
     else
        return (30);       /* Even months have 30 days    */
  }
}

/*  Determines whether a given date is valid
 */

int validdate (month, day, year)
int month, day, year;
{
  if (month < 1 || month > 12 || day < 1 ||
     day > daysinmonth (month, year) ||
     year < 1583 || year > 9999)
        return (0);
  else
        return (1);
}

/*  Given a valid date (month, day, and year) Zeller will
 *    return an integer representing the day of week that
 *    date will fall on. 0 = Sunday, 6 = Saturday.
 */

int zeller (month, day, year)
int month, day, year;
{
  int century;
  month -= 2;       /* Years start on March 1 so adjust standard date */
  if (month < 1) {
     month += 12;
     year--;
  }
  century = year / 100;
  year = (int)year % (int)100;
  return ((int)((2.6 * month - 0.1) + day + year + year / 4  + century / 4 - century * 2) % 7);
}

/*
 * Utility to get the "yesterday" of a datecode
 */
int yestercode (dc)
int dc;
{
	int day, month, year;

	day = dc_day(dc);
	month = dc_month(dc);
	year = dc_year(dc);

	day--;
	if (day == 0) {
		month --;
		if (month == 0) {
			year --;
			month = 12;
		}
		day = daysinmonth(month, year);
	}

	return (year-1990)*10000 + month*100 + day;

}

#ifndef TDL

/*
 * print_header: prints the name of the month and the year
 */

print_header(month, year)
int month, year;
{
   Xv_font font;
   Display *dpy = (Display *)xv_get(calendar, XV_DISPLAY);
   XGCValues gcvalues;
   GC gc;
   Window xwin = xv_get(canvas_paint_window(calendar), XV_XID);
   char title[15];

   font = xv_find(calendar, FONT,
                  FONT_FAMILY,  FONT_FAMILY_LUCIDA,
                  FONT_STYLE,   FONT_STYLE_BOLD_ITALIC,
                  FONT_SCALE,   WIN_SCALE_LARGE,
                  0);
   gcvalues.font = xv_get(font, XV_XID);
   gcvalues.foreground = BlackPixel(dpy, DefaultScreen(dpy));
   gcvalues.background = WhitePixel(dpy, DefaultScreen(dpy));
   gcvalues.graphics_exposures = False;
   gc = XCreateGC(dpy, RootWindow(dpy, DefaultScreen(dpy)),
                  GCForeground | GCBackground | GCFont | GCGraphicsExposures,
                  &gcvalues);
      
   XClearWindow(dpy, xwin);
   sprintf(title, "%s %4d", monthname[month-1], year);
   XDrawString(dpy, xwin, gc, 10, 10, title, strlen(title));
}

/*
 * Given the day of the week, and the week number, this routine prints
 * the "day" (date) at comuted coordinates in the calendar.
 */

print_day(week, dow, day)
int week, dow, day;
{
   Display *dpy = (Display *)xv_get(calendar, XV_DISPLAY);
   GC gc = DefaultGC(dpy, DefaultScreen(dpy));
   Window xwin = xv_get(canvas_paint_window(calendar), XV_XID);
   char daystr[3];
   
   sprintf(daystr, "%2d", day);
   XDrawString(dpy, xwin, gc,
               24*dow, 30+12*(week-1),
               daystr, strlen(daystr));

}

/*
 * This routine adds a "highlight" (rectangle) on the current day. 
 */

highlite (week, dow)
int week, dow;
{
   Display *dpy = (Display *)xv_get(calendar, XV_DISPLAY);
   GC gc = DefaultGC(dpy, DefaultScreen(dpy));
   Window xwin = xv_get(canvas_paint_window(calendar), XV_XID);

   XDrawRectangle(dpy, xwin, gc,
                  24*dow-4, 30+12*(week-2)+1,
                  20, 12);
}

/*
 * Here we control the actual calendar generation --> calling
 * print_header and print_day as needed.
 */

print_calendar(month, year)
int month, year;
{
   int start, days, dow, week, day;

   start = zeller(month, 1, year);
   days = daysinmonth(month, year);

   print_header(month, year);
   week = 1;
   dow = start;
   for (day=1; day <= days; day++) {
      dow = dow % 7;
      if ( (dow == 0) && (day != 1) ) week++;
      print_day(week, dow, day);
      if (day == curr_day) highlite(week, dow);
      dow++;
   }
}

/*
 * event_proc()
 *      Called when an event is received in the canvas window.
 *      Only button events are monitored in this window, and any button
 *      event will cause a move to the day the event was on.  We first
 *      compute the day the button event occured over, then we move to
 *      that day.
 */
void
calendar_event_proc(window, event)
Xv_Window window;
Event    *event;
{
   int dow, week, first, date;

	if (! event_is_button(event)) return;
   if (event_is_up(event)) return;   /* only react on button-down events */

   /* Compute the date the event occured over */
   dow = event_x(event) / 24;
   week = (event_y(event) - 18) / 12 + 1;
   first = zeller(curr_month, 1, curr_year);
   date = 1 + (dow - first) + 7*(week-1);

   /*
    * The date is in terms of the current month.  If it is between 1 and
    * the number of days in the month, we only need to move in the
    * current month.  However, "date" could be negative, meaning we need
    * to move to last month, or > the days in the current month, meaning
    * we need to move to next month.
    */
   if ((date > 0) & (date <= daysinmonth(curr_month, curr_year))) {
      curr_day = date;
   } else if (date > daysinmonth(curr_month, curr_year)) {
      curr_day = date - daysinmonth(curr_month, curr_year);
      curr_month = (curr_month+1) % 12;
      if (curr_month == 0) curr_month = 12;
      if (curr_month == 1) curr_year++;
   } else {
      curr_month = (curr_month-1) % 12;
      if (curr_month == 0) {
         curr_month = 12;
         curr_year --;
      }
      curr_day = daysinmonth(curr_month, curr_year) + date;
   }

   /* Move to the date specified. */
   print_calendar(curr_month, curr_year);
   reset_date_message();
   display_list(curr_month, curr_day, curr_year);  
}

#endif
