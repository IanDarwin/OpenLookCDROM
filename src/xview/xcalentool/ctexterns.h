/*
 * $Id: ctexterns.h,v 1.3 1994/08/29 23:30:37 billr Exp $
 */
/*
 * ctexterns.h - external function defintions for calentool
 *
 * calentool - day/week/month/year-at-a-glance calendar for XView/Open Look
 * 
 * Copyright 1994 by Tektronix, Inc. - All Rights Reserved.
 * 
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Tektronix, Inc. not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission.
 * 
 * TEKTRONIX INCORPORATED MAKES NO REPRESENTATIONS ABOUT THE
 * SUITABILITY OF THIS SOFTWARE FOR ANY PURPOSE.  IT IS PROVIDED "AS IS"
 * WITHOUT EXPRESS OR IMPLIED WARRANTY.  TEKTRONIX INCORPORATED
 * DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  IN NO
 * EVENT SHALL TEKTRONIX INCORPORATED BE LIABLE FOR ANY SPECIAL,
 * INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
 * LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
 * OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 * 
 * Author: Bill Randle, Tektronix, Inc. <billr@saab.cna.tek.com>
 */ 
#ifndef CTEXTERNS_H
#define CTEXTERNS_H

#ifdef __cplusplus
extern "C" {
#endif


/*
 * First, we have functions that are a part of calentool.
 */
#if defined(__STDC__) || defined(__cplusplus)
extern int a_dates (int flag), c_dates (int flag), i_dates (int flag);
extern int j_dates (int flag), s_dates (int flag);
extern void add_to_slot (int, struct appt_entry *, int);
extern void check_calendar (void);
extern int chk_deleted (struct dayslot *slptr, struct appt_entry *aptr);
extern int chk_week (int repeat, int curday);
extern void cleanup (void);
extern int close_day (void);
extern void create_attr_frame (void);
extern void create_date_frame (void);
extern void create_file_frame (void);
extern void create_future_popup (void);
extern void create_main_window (void);
extern void create_panel (void);
extern void create_print_frame (void);
extern void datelib_init (int year);
extern int day_of_year (double day, int month, int year);
extern int days_remaining_in_year (double day, int month, int year);
extern void deactivate_slot (int, int);
extern int do_files (int window_prompt);
extern void draw_day (void);
extern void draw_day_appts (void);
extern void draw_month (void);
extern void draw_week (void);
extern void draw_year (void);
extern void err2console (int state);
extern void err_rpt (char *errstr, int fatal_flag);
extern void expire (int edays);
extern void find_date (struct appt_entry *appt);
extern void fix_current_day (void);
extern char *format_appt_nd (struct appt_entry *appt, int esc_parens);
extern void free_week_appts (void);
extern int get_aentry (FILE *apts_file, struct appt_entry *appt, int noInclude, int noUmkNotes, int target);
extern int get_day_appts (void);
extern int get_day_of_week (double day, int month, int year);
extern void get_fonts (void);
extern void get_today (void);
extern void get_week_appts (void);
extern void gregorian_date (double *p_day, int *p_month, int *p_year, double jday);
extern void init_pixrects (void);
extern void lastmonth (void);
extern void lastweek (void);
extern void lastyear (void);
extern int length_of_year (int year);
extern void lock_cursors (void);
extern int monthlength (int month);
extern void moon_data (long int seconds);
extern void moon_data_frame (void);
extern void next_appt (int bi, int dpyflag);
extern void nextmonth (void);
extern void nextweek (void);
extern void nextyear (void);
extern int parse_date (char *str, int cmdline);
extern void print_apts (int which, int dest);
extern void print_button (int state);
extern void print_calendar (int file_type);
extern void print_psday (FILE *fp, int noteflag);
extern void print_psmonth (FILE *fp, int noteflag);
extern void print_psweek (FILE *fp, int noteflag);
extern int put_aentry (FILE *apts_file, struct appt_entry *appt);
extern int read_schedule (char *, short);
extern void rewrite_string (int, int);
extern void riseset (long int epoch_seconds);
extern void set_attr (void);
extern void set_icon (void);
extern void sun_data_frame (void);
extern void sun_moon_buttons (int state);
extern void tomorrow (void);
extern void unlock_cursors (void);
extern void update_icon_time (void);
extern void ver1to2 (void);
extern char *version (void);
extern int week_number (void);
extern void working (int);
extern void write_times (void);
extern void xrename (char *from, char *to);
extern void yesterday (void);
extern int ymd2_compare (struct tm *day0, struct appt_entry *aday);
extern int ymd_compare (struct tm day0, struct tm day1);

#else
extern int a_dates (), c_dates (), i_dates ();
extern int j_dates (), s_dates ();
extern void add_to_slot ();
extern void check_calendar ();
extern int chk_deleted ();
extern int chk_week ();
extern void cleanup ();
extern int close_day ();
extern void create_attr_frame ();
extern void create_date_frame ();
extern void create_file_frame ();
extern void create_future_popup ();
extern void create_main_window ();
extern void create_panel ();
extern void create_print_frame ();
extern void datelib_init ();
extern int day_of_year ();
extern int days_remaining_in_year ();
extern void deactivate_slot ();
extern int do_files ();
extern void draw_day ();
extern void draw_day_appts ();
extern void draw_month ();
extern void draw_week ();
extern void draw_year ();
extern void err2console ();
extern void err_rpt ();
extern void expire ();
extern void find_date ();
extern void fix_current_day ();
extern char *format_appt_nd ();
extern void free_week_appts ();
extern int get_aentry ();
extern int get_day_appts ();
extern int get_day_of_week ();
extern void get_fonts ();
extern void get_today ();
extern void get_week_appts ();
extern void gregorian_date ();
extern void init_pixrects ();
extern void lastmonth ();
extern void lastweek ();
extern void lastyear ();
extern int length_of_year ();
extern void lock_cursors ();
extern int monthlength ();
extern void moon_data ();
extern void moon_data_frame ();
extern void next_appt ();
extern void nextmonth ();
extern void nextweek ();
extern void nextyear ();
extern int parse_date ();
extern void print_apts ();
extern void print_button ();
extern void print_calendar ();
extern void print_psday ();
extern void print_psmonth ();
extern void print_psweek ();
extern int put_aentry ();
extern int read_schedule ();
extern void rewrite_string ();
extern void riseset ();
extern void set_attr ();
extern void set_icon ();
extern void sun_data_frame ();
extern void sun_moon_buttons ();
extern void tomorrow ();
extern void unlock_cursors ();
extern void update_icon_time ();
extern void ver1to2 ();
extern int week_number ();
extern void working ();
extern void write_times ();
extern void xrename ();
extern void yesterday ();
extern int ymd2_compare ();
extern int ymd_compare ();
extern char *version ();
#endif

/*
 * Next, we have library functions for which we may need prototypes
 */
#ifdef NEEDS_EXTRA_PROTOS

#if defined(__STDC__) || defined(__cplusplus)
extern int fprintf (FILE *, const char *, ...);
extern int printf (const char *, ...);
extern int sscanf (const char *, const char *, ...);
extern int fputs (const char *, FILE *);
extern int fclose (FILE *);
extern int fflush (FILE *);
extern int fwrite (const void *, int, int, FILE *);
extern int system (const char *);
extern int rename (const char *, const char *);
extern int toupper (int);
extern void perror (const char *);
extern int getopt (int, char *const *, const char *);

#else

extern int fprintf(), printf(), sscanf(), getopt();
extern void perror();
extern int fputs(), fclose();
extern int fwrite(), fflush();
extern int system(), rename(), toupper();
#endif

#endif

#ifdef NEEDS_GETTIMEOFDAY_PROTO
/* Solaris 2.3 needs this, others may, too */
#ifdef __STDC__
#ifdef SVR4
extern int gettimeofday (struct timeval *);
#else
extern int gettimeofday (struct timeval *, struct timezone *);
#ifndef HAS_STRFTIME
extern char *timezone (int, int);
#endif
#endif
#else
extern int gettimeofday();
#ifndef HAS_STRFTIME
extern char *timezone();
#endif
#endif

#endif

/*
 * Finally, we have a few XView library functions that prototypes don't
 * exist for in earlier verisons of XView.
 */
#include <xview/xv_version.h>
#if (XV_VERSION_NUMBER < 3200)  /* don't know about 3.1; I know 3.0 needs these */
#if defined(__STDC__) || defined(__cplusplus)
#include <xview/frame.h>
#include <xview/rect.h>
void frame_get_rect (Frame, Rect *);
#else
void frame_get_rect ();
#endif
#endif

#ifdef __cplusplus
};
#endif

#endif
