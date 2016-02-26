/*
 *  Global (external) definitions for the XVTDL software
 */

#include <stdio.h>
#ifdef SVR4
#include <string.h>
#else
#include <strings.h>
#endif
#include <fcntl.h>
#include <time.h>
#include <pwd.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <errno.h>

#include <X11/X.h>
#include <X11/Xlib.h>

#include <xview/xview.h>
#include <xview/panel.h>
#include <xview/textsw.h>
#include <xview/svrimage.h>
#include <xview/font.h>
#include <xview/icon.h>
#include <xview/xv_xrect.h>
#include <xview/openmenu.h>
#include <xview/notify.h>
#include <xview/notice.h>
#include <xview/cursor.h>

#include "gcc.h"
#include "gcm.h"

#include "print_ui.h"
#include "entry_ui.h"
#include "props_ui.h"
#include "deadline_ui.h"

#if !defined(XVIEW2) && !defined(XVIEW3)
#       ifdef FRAME_FOCUS_UP_WIDTH
#       define XVIEW3
#       else
#       define XVIEW2
#       endif
#endif

#if defined(XVIEW2)
#undef XVIEW3
#endif
 
#if defined(XVIEW3)
#undef XVIEW2
#endif

#define NEW(A) ((A *) malloc(sizeof(A)))
#define EQUAL(s1,s2) strcmp(s1, s2) == 0

#define MASK(n)          ((unsigned long)(1 << (n)))
#define BIT_IS_SET(x,n)  ((x) & MASK(n))
#define SET_BIT(x,n)     x |= MASK(n)
#define UNSET_BIT(x,n)   x &= ~MASK(n)

#define FILENAMESIZ 200
#define LINESIZ     200
#define BIG        1000

#define LOG_AT_CHECKED 0
#define LOG_AT_QUIT    1
#define LOG_NEVER      2
#define LOG_TIMESTAMP  0
#define LOG_USER_SPEC  1

#define PRINT_ALL  1000
#define PRINT_TREE 1001

extern int debug;
extern int verbose;
extern int editing, copying, cutting;
extern int has_color;

extern entry_entry_frame_objects	*entry_editor;
extern print_print_base_objects   *print_print_base;
extern deadline_deadline_frame_objects	*deadline_deadline_frame;

extern char fname[FILENAMESIZ];
extern char default_printer[25], log_preference[10], log_file[LINESIZ];
extern char priority_listing[13], log_info_pref[25];
extern char sort_order[5], chron_listing[13], print_file[LINESIZ];
extern char default_print_dest[10];
extern char on_propagation[8], fgcolor[50], bgcolor[50];
extern int postscriptmode,logging,default_priority;
extern int multiclick_timeout;
extern time_t access_time, modify_time;

extern Frame tdlist, entry_frame, recurring_frame;

extern Icon tdl_icon, empty_tdl_icon;

extern struct tm current, *tm, today, *localtime();
extern int curr_month, curr_day, curr_year;

extern Server_image checked_on, checked_off;
extern Server_image checks[10];

extern Xv_cursor basic_cursor, copy_cursor, cut_cursor, edit_cursor;

extern Canvas calendar;
extern Panel recurring_panel;
extern Panel_item todo, entry_text, entry_done, categories, recurring;
extern Panel_item entry_type, entry_category, entry_category_name, entry_deadline;
extern Panel_item freq, day_of_week, day_of_month, weeks, week_of_month;
extern Panel_item months, recurring_cancel, recurring_done;
extern Panel_item category_name, list_all;
extern Menu list_menu;

extern int zeller(), daysinmonth();
extern int changed;

extern int log_level, log_info_level;

struct deadline_rec {
	int datecode, relative;
	int actions;
	int delete_time, delete_units;
	int priority_up_units, priority_down_units;
	char mail_on[LINESIZ];
	char mail_after[LINESIZ];
	int move_time, move_units;
};

extern struct deadline_rec default_deadline;

struct recurrence_list {
	int starting_day_code;
	char text[LINESIZ];
	int priority;
	int daily, weekly, biweekly, monthly, yearly;
	int dow, dom;
	int week_number, number_of_weeks, number_of_months;
	struct deadline_rec *deadline;
	struct recurrence_list *next;
};

struct day_entry {
	int starting_day_code;
	char text[LINESIZ];
	int checked, recurring_entry;
	int priority;
	struct deadline_rec *deadline;
	struct day_entry *prev, *next;
};

extern struct day_entry *cut_buffer;
extern struct recurrence_list *cut_rl_buffer;

struct day_entry_list {
	struct day_entry *de;
	struct day_entry_list *next, *prev;
};

extern struct day_entry_list *delhead, *delcurr, *delprev;

struct entry_list {
	int day_code;
	int recurring_checked;
	struct day_entry *first, *last, *rl_first, *rl_last;
	struct entry_list *next;
};

extern struct entry_list *entry_head, *entry_tail;

extern struct recurrence_list *rl_head, *rl_tail;

struct category_rec {
	char name[LINESIZ];
	struct entry_list *entry_head, *entry_tail;
	struct recurrence_list *rl_head, *rl_tail;
	struct category_rec *parent, *subcats;
	struct category_rec *next;
};

extern struct category_rec *category_head;
