/* compiles as read only version for remote sites */
/* #define READ_ONLY */
#define VERSION "hwtool: 1.4alpha\n"

/************************************************************************/
/*									*/
/*	hwtool								*/
/*	R. Keene, SUN Microsystems, Dec. 1988				*/
/*	sun!sunrock!dick						*/
/*									*/
/************************************************************************/

#include<stdio.h>
#include<sys/file.h>
#include<sys/types.h>
#include<sys/timeb.h>
#include<sys/stat.h>
#include<curses.h>
#include<ctype.h>
#include<strings.h>
#include<pwd.h>
#include<pixrect/pixrect_hs.h>
#include<suntool/sunview.h>
#include<suntool/panel.h>
#include<suntool/canvas.h>
#include<suntool/panel.h>
#include<suntool/icon.h>

#define HW_MONTH_JAN 0
#define HW_MONTH_FEB 1
#define HW_MONTH_MAR 2
#define HW_MONTH_APR 3
#define HW_MONTH_MAY 4
#define HW_MONTH_JUN 5
#define HW_MONTH_JUL 6
#define HW_MONTH_AUG 7
#define HW_MONTH_SEP 8
#define HW_MONTH_OCT 9
#define HW_MONTH_NOV 10
#define HW_MONTH_DEC 11

/* Time starts on Jan 1 1987 */
int hw_month_days[] = {31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
int hw_leaps[] = {1988, 1992, 1996, 0};
#define HW_1987_WEEKDAY  3 /* Thursday on Jan 1 1987 */
char hw_month_names[][16] = {"January", "February", "March", "April",
			"May", "June", "July", "August", "September",
			"October", "November", "December"};
char hw_day_names[][16] = {"Monday", "Tuesday", "Wednesday", "Thursday",
			   "Friday", "Saturday", "Sunday" };
struct hw_date {
	int year; /* eg 1988 */
	int month; /* eg HW_MONTH_FEB is 1 */
	int day; /* eg. 17 */
	int hour; /* eg. 15 , in this case half days are used so the */
	      /* only valid hours are 8 (morning) and 13(1:00 in afternoon) */
	};

#define HW_STRING_SIZE 128
#define HW_SERNO_SIZE 16
#define HW_MAX_SUB_ITEMS 64
#define HW_WHO_SIZE 64
#define MAX_COMMENT 20
#define MAX_COMMENT_INDEX 4
#define MAX_CLASS 3
struct sched_item {
	char serno[HW_SERNO_SIZE];
	char who[HW_WHO_SIZE]; /* login name, like "bob" */
	char whofor[HW_WHO_SIZE];
	char whosetup[HW_WHO_SIZE];
	struct hw_date start;
	struct hw_date end;
	char comment[MAX_COMMENT_INDEX][MAX_COMMENT];
	int deleted;
	};

struct hwitem {
	char serno[HW_SERNO_SIZE]; /* eg. "602D345" , the usual way to index */
	char name[HW_STRING_SIZE]; /* eg. "region 3/60 16MB" */
	char description[HW_STRING_SIZE]; /* eg. SUN 3/60 */
	char hostid[HW_STRING_SIZE]; /* eg. 3100013f */
	char class[MAX_CLASS][HW_STRING_SIZE];/* usualy location, or type */
	char subitems[HW_MAX_SUB_ITEMS][HW_SERNO_SIZE]; /* list of serial #'s */
	int is_subitem; /* is a sub item of something else */
	};

/* [policy not yet implemented] */
#define POLICY_FREE 0
#define POLICY_LIMITED 1
struct policyitem {
	char who[HW_WHO_SIZE]; /* login name, like "bob" */
	int limited; /* POLICY_FREE or ..._LIMITED */
	};

struct hw_date current_date; /* month/year where calander is */
int hw_current_day_0; /* day of week that is day#0 */
int current_machine; /* index in machines_list */
int current_group; /* index in group list */
char current_whofor[HW_WHO_SIZE];
char current_whosetup[HW_WHO_SIZE];
struct sched_item pick_sched; /* temporary sched_item for mouse clicks */
int pick_index; /* schedule_list item where mouse clicked */
int pick_list[500]; /* list of machine numbers for labels in month bar chart */
int pick_list_top;
int detail_index; /* tells barchart drawing to detail schedule_list_item */

#define HW_MACHINES 1000
struct hwitem machines_list[HW_MACHINES];

#define HW_DATES 5000
struct sched_item schedule_list[HW_DATES];
time_t sched_last_read; /* from stat call, last read time */

#define HW_POLICY 500
struct policyitem policy_list[HW_POLICY];

#define HW_GROUPS 500
char group[HW_GROUPS][HW_STRING_SIZE];

/* hwtool environment */
char hw_tool_home[1024];
char hw_tool_print[1024];
char *cmdname;
struct passwd *passwd; /* the users password entry */
int ascii_mode;
char blurb[1024]; /* so it's not always being re-allocated and cleared */

#define CAL_TITLE_HEIGHT 34
#define CALENDAR_CANVAS_WIDTH 900
#define CALENDAR_CANVAS_HEIGHT 800
#define INFO_CANVAS_WIDTH 200
#define BOX_WIDTH (CALENDAR_CANVAS_WIDTH / 7)
#define BOX_HEIGHT ((CALENDAR_CANVAS_HEIGHT - CAL_TITLE_HEIGHT) / 6)
#define BOX_LINE_WIDTH 2 /* 1 is double thick line, 2 is with a 1 pixel gap */

#define BAR_TITLE_HEIGHT 45
#define BAR_LINE_GAP 15
#define BAR_NAME_SIZE (8 * 10)
#define BAR_SERNO (BAR_NAME_SIZE + 5)
#define BAR_SERNO_SIZE (9 * 10)
#define BAR_DATE_START (BAR_SERNO + BAR_SERNO_SIZE)
#define BAR_DATE_GAP ((CALENDAR_CANVAS_WIDTH - BAR_DATE_START) / 32)
#define BAR_TICK_SIZE 3 /* pixels above and below line */
#define BAR_LINE_WIDTH 3
#define BAR_DAY_START(x) (BAR_DATE_START + (x) * BAR_DATE_GAP)
#define BAR_HOUR_START(d,h) (BAR_DAY_START(d) + ((h) * BAR_DATE_GAP)/24)

Frame base_frame;
Canvas schedule_canvas;
Canvas info_canvas;
/* annotate popup */
Frame anno_frame;
Panel anno_panel;
Panel_item anno_who_for;
Panel_item anno_who_setup;
Panel_item anno_apply;
Panel_item anno_cancel;
Panel_item anno[4];

Frame conf_frame;
Panel conf_panel;
Panel_item conf_txt;
Panel_item conf_image_item;
Frame reread_frame;
Panel reread_panel;

Frame by_frame;
Panel by_panel;
Panel_item by_apply;
Panel_item by_cancel;
Panel_item by_string;

static short icon_image[] = {
#include "hwtool.icon"
};
mpr_static(icon_pixrect, 64, 64, 1, icon_image);

static short conf_image[] = {
#include "conflict.icon"
};
mpr_static(conf_pr, 64, 64, 1, conf_image);

/* for highlighting a calendar point */
static short highlight_image[] = {
#include "highlight.cursor"
};
mpr_static(highlight_pixrect, 16, 16, 1, highlight_image);

Menu machine_menu;

Pixwin *schedule_pw;
Pixwin *info_pw;
char base_frame_label[256];
Pixfont *cal_number_pf;
Pixfont *cal_notes_pf;
Pixfont *cal_who_pf;
Pixfont *cal_weekday_pf;
Pixfont *cal_month_pf;
Pixfont *cal_data_pf;

Pixfont *bar_day_pf;
Pixfont *bar_month_pf;


/* pw draw translators for PostScript */
/* These write to the Pixwin and then output PostScript */
/* Thus after a screen re-draw the ps_buffer has the script in it */
#define PS_BUF_MAX 65535
int enable_post_script;
char ps_buf[PS_BUF_MAX];
ps_buf_i;
Pixfont *lastpf; /* to eliminate redundant font lookups */

/* stuf char in ps_buf */
ps_char(c)
  char c;
  {
  if(enable_post_script == 0)
  	return;
  if(ps_buf_i >= PS_BUF_MAX)
  	{
  	fprintf(stderr,"Post Script print buffer full, disableing printing\n");
  	enable_post_script = 0;
  	return;
  	}
  ps_buf[ps_buf_i++] = c;
  }

ps_string(s)
  char *s;
  {
  if(s == NULL)
  	return;
  while(*s != '\0')
  	ps_char(*s++);
  }

pw_vector_ps(pw, x1, y1, x2, y2, pwop, color)
  Pixwin *pw;
  int x1,y1,x2,y2,pwop,color;
  {
  
  if(ascii_mode == 0)
  	pw_vector(pw, x1, y1, x2, y2, pwop, color);
  if(enable_post_script)
  	{
  	if(pw == schedule_pw)
  		{
  		if(x1 > CALENDAR_CANVAS_WIDTH) x1 = CALENDAR_CANVAS_WIDTH;
  		if(x2 > CALENDAR_CANVAS_WIDTH) x2 = CALENDAR_CANVAS_WIDTH;
  		sprintf(blurb,"%d %d %d %d h\n", x2,
  			CALENDAR_CANVAS_HEIGHT - y2, x1,
  			CALENDAR_CANVAS_HEIGHT - y1);
  		}
  	else
  		{
  		if(x1 < 0) x1 = 0;
  		if(x2 < 0) x2 = 0;
		sprintf(blurb,
		    "%d %d %d %d h\n", x2 + CALENDAR_CANVAS_WIDTH,
  			CALENDAR_CANVAS_HEIGHT - y2,
  			x1 + CALENDAR_CANVAS_WIDTH,
  			CALENDAR_CANVAS_HEIGHT - y1);
  		}		
  	ps_string(blurb);
  	}
  }

/* Only generates PostScript on schedule_pw clears */
pw_clear_ps(pw)
  Pixwin *pw;
  {
  struct tm *local_time;
  int i;
  
  if(ascii_mode == 0)
  	pw_rop(pw, 0, 0, 5000, 5000, PIX_CLR, NULL, 0, 0);
  if((enable_post_script) && (pw == schedule_pw))
	{
	/* Page preamble */
	/* This could have been in an external file, this is straight forward */
	/* and allows sprintf to be used to xfer vars */
	/* clear buf */
	ps_buf_i = 0;
	lastpf = NULL;
	/* Post Script file indicator for laser writer */
	ps_string("%!\n");
	ps_string("% hwtool print file\n");

	/* set landscape */
	ps_string("clippath pathbbox\n");
	ps_string("pop exch translate pop\n"); /* get in lower right corner */
	ps_string("90 rotate\n");

	/* set scale */
	/* get llx lly urx ury on stack (llx=lly=0, so urx=dx ury=dy)*/
	ps_string("initclip clippath pathbbox\n");
	sprintf(blurb, "%d div exch %d div exch scale pop pop\n",
				CALENDAR_CANVAS_HEIGHT,
				CALENDAR_CANVAS_WIDTH + INFO_CANVAS_WIDTH);
	ps_string(blurb);
 	/* frame and Window division line */
 	ps_string("newpath clippath stroke\n");
 	sprintf(blurb, "newpath 4 setlinewidth %d 0 moveto %d %d lineto stroke\n",
 	 CALENDAR_CANVAS_WIDTH, CALENDAR_CANVAS_WIDTH, CALENDAR_CANVAS_HEIGHT);
 	ps_string(blurb);
 	/* Macros for general data compression to printer */
 	/* a macro for line drawing */
 	/* x2 y2 x1 y1 - */
 	ps_string(
 	    "/h { 1 setlinewidth newpath moveto lineto stroke } bind def\n");
 	/* fonts */
 	/* cal_number */
	ps_string("/f1 { /Times-Bold findfont 18 scalefont setfont } bind def\n");
	/* cal_notes */
	ps_string(
		"/f2 { /Times-Roman findfont 10 scalefont setfont } bind def\n");
	/* cal_who */
	ps_string("/f3 { /Times-Bold findfont 14 scalefont setfont } bind def\n");
	/* cal_weekday (carefull, same font as cal_data on a SUN !) */
	ps_string(
		"/f4 { /Times-Bold findfont 13 scalefont setfont } bind  def\n");
	/* cal_month */
	ps_string("/f5 { /Times-Bold findfont 16 scalefont setfont } bind def\n");
	/* cal_data */
	ps_string(
		"/f6 { /Times-Roman findfont 13 scalefont setfont } bind def\n");
	/* bar_day */
	ps_string("/f7 { /Times-Roman findfont 9 scalefont setfont } bind def\n");
	/* checkmark macro */
	/* x y - */
	ps_string("/k { newpath 3 setlinewidth moveto\n");
	ps_string("   0 -9 rmoveto 4 -6 rlineto 12 14 rlineto stroke\n");
	ps_string("   } bind def\n");
	
	/* moveshow routine */
	ps_string("/m { moveto show } bind def\n");
	
	/* when printed */
  	i = time((time_t *)0);
  	local_time = localtime(&i);
	sprintf(blurb,"%d %d moveto f7 (Printed %d:%d, %d %s, %d) show\n",
		CALENDAR_CANVAS_WIDTH + 8, 15,
		local_time->tm_hour, local_time->tm_min,
		local_time->tm_mday, hw_month_names[local_time->tm_mon],
		local_time->tm_year + 1900);
	ps_string(blurb);
	}
  }

pw_text_ps(pw, x, y, pwop, pf, text)
  Pixwin *pw;
  int x, y;
  int pwop;
  Pixfont *pf;
  char *text;
  {
  
  if(ascii_mode == 0)
  	pw_text(pw, x, y, pwop, pf, text);
  if(enable_post_script)
	{
	/* figure font */
	if(lastpf != pf)
		{
		lastpf = pf; /* see above */
		if(pf == cal_number_pf)       sprintf(blurb," f1\n");
		else if(pf == cal_notes_pf)   sprintf(blurb," f2\n");
		else if(pf == cal_who_pf)     sprintf(blurb," f3\n");
		else if(pf == cal_weekday_pf) sprintf(blurb," f4\n");
		else if(pf == cal_month_pf)   sprintf(blurb," f5\n");
		else if(pf == cal_data_pf)    sprintf(blurb," f6\n");
		else if(pf == bar_day_pf)     sprintf(blurb," f7\n");
		else
			{
			fprintf(stderr,"\007Bad font designator in pw_text_ps\n");
			sprintf(blurb, " ");
			}
		}
	ps_string(blurb);
	if(pw == schedule_pw)
		sprintf(blurb, "(%s) %d %d m\n", text, x,
  			CALENDAR_CANVAS_HEIGHT - y);
	else
		sprintf(blurb, "(%s) %d %d m\n", text, 
  			x + CALENDAR_CANVAS_WIDTH,
  			CALENDAR_CANVAS_HEIGHT - y);
	ps_string(blurb);
	}
  }

highlight_ps(pw, x, y)
  Pixwin *pw;
  int x,y;
  {
  						      /* (a check mark) */
  if(ascii_mode == 0)
  	pw_rop(schedule_pw, x, y + 4, 16, 16, PIX_SRC, &highlight_pixrect, 0,0);
  if(enable_post_script)
	{
	if(pw == schedule_pw)
		sprintf(blurb, "%d %d k\n", x, CALENDAR_CANVAS_HEIGHT - y); /* check mark */
	else
		sprintf(blurb, "%d %d k\n", x + CALENDAR_CANVAS_WIDTH,
  			CALENDAR_CANVAS_HEIGHT - y); /* check mark */
	ps_string(blurb);
	}
  }

ps_print_buf()
  {
  FILE *fp;
  
  lastpf = NULL;
  
  if(ps_buf_i <= 0)
  	{
  	fprintf(stderr,"\007Nothing to print\n");
  	return;
  	}
  
  ps_string("showpage\n");
  
  fp = popen(hw_tool_print, "w");
  if(fp == NULL)
  	{
  	fprintf(stderr,"\007Unable to find lpr command(%s)\n", hw_tool_print);
  	return;
  	}
  if(fwrite(ps_buf, ps_buf_i, 1, fp) != 1)
  	{
  	fprintf(stderr,"\007Broken pipe, lpr command\n");
  	return;
  	}
  pclose(fp);
  }

/* Date maniputlation stuff */

/* -1 if leapyear, else 0 */
is_leapyear(year)
  int year;
  {
  int i;
  
  for(i=0; hw_leaps[i] != 0; i++)
  	if(hw_leaps[i] == year)
  		return(-1);
  return(0);
  }

/* length in days. else 0 on error */
month_length(month,year)
  int month;
  int year;
  {
  if((month < 0) || (month > 11))
  	return(-1);
  if(is_leapyear(year) && (month == HW_MONTH_FEB))
  	return(29);
  else
  	return(hw_month_days[month]);
  }

/* in hours from Jan 1 1987 */
/* -1 on error */
/* No sooner dates than 1987 ! */
int relative_date_hours(a)
  struct hw_date *a;
  {
  int yearhours;
  int yearindex, monthindex;
  
  if((a == NULL) || (a->year < 1987))
  	return(-1);
  yearhours = 0;
  for(yearindex=1987; yearindex < a->year; yearindex++)
  	{
  	if(is_leapyear(yearindex))
  		yearhours = yearhours + 366 * 24;
  	else
  		yearhours = yearhours + 365 * 24;
  	}
  for(monthindex=0; monthindex < a->month; monthindex++)
  	{
  	yearhours = yearhours + month_length(monthindex, yearindex) * 24;
  	}
  yearhours = yearhours + a->day * 24 + a->hour;
  return(yearhours);
  }

/* adds n hours to date */
/* n must be + */
add_hours(a,n)
  struct hw_date *a;
  {
  
  if((n < 0) || (a == NULL))
  	return;
  /* pare it down */
  while(n >= 24)
  	{
  	n = n - 24;
  	add_hours(a, 24);
  	}
  a->hour = a->hour + n;
  if(a->hour >= 24)
  	{
  	a->hour = a->hour - 24;
  	a->day = a->day + 1;
  	if(a->day >= month_length(a->month, a->year))
  		{
  		a->day = 0;
  		a->month = a->month + 1;
  		if(a->month >= 12)
  			{
  			a->month = 0;
  			a->year = a->year + 1;
  			}
  		}
  	}
  }

/* 0 thru 6 where 0 is monday */
int week_day(a)
  struct hw_date *a;
  {
  int yearhours;
  
  yearhours = relative_date_hours(a) / 24;
  
  return((yearhours + HW_1987_WEEKDAY) % 7);
  }

/* returns a - b in hours */
/* not necessarily the number of hours but is montonic scalear */
int date_diff(a,b)
  struct hw_date *a;
  struct hw_date *b;
  {
  return(relative_date_hours(a) - relative_date_hours(b));
  }

/* 0 if no overlap, -1 if overlap */
date_overlaps(a_start, a_end, b_start, b_end)
  struct hw_date *a_start;
  struct hw_date *a_end;
  struct hw_date *b_start;
  struct hw_date *b_end;
  {
  int a1,b1;
  
  a1 = relative_date_hours(a_start);
  b1 = relative_date_hours(b_start);
  
  if(a1 < b1)
  	{
  	if(relative_date_hours(a_end) >= b1)
  		return(-1);
  	return(0);
  	}
  else if(b1 < a1)
  	{
  	if(relative_date_hours(b_end) >= a1)
  		return(-1);
  	return(0);
  	}
  else /* a1 == b1 */
  	return(-1);
  }

hw_getstr(s,n,fp)
  char *s;
  int n;
  FILE *fp;
  {
  
  if(n <= 0)
  	{
  	fprintf(stderr,"\007Invalid n in hw_getstr\n");
  	return;
  	}
  if(s == NULL)
  	{
  	fprintf(stderr,"\007NULL s in hw_getstr\n");
  	return;
  	}
  if(fp == NULL)
  	{
  	fprintf(stderr,"\007NULL fp in hw_getstr\n");
  	return;
  	}
  	
  if(n == 1)
  	{
  	*s = '\0';
  	return;
  	}

  while(-1)
  	{
  	if(n <= 1)
  		{
  		*s = '\0';
  		return;
  		}
  	*s = getc(fp);
  	if((*s == '\n') || (*s == '\r'))
  		{
  		*s = '\0';
  		return;
  		}
  	if(feof(fp))
  		{
  		s++;
  		*s = '\0';
  		return;
  		}
  	s++;
  	n--;
  	}
  }

/* 0=O.K. -1=nolock */
int get_lock_perm()
  {
  int fd;
  char filename[256];
  int tries;
  
  sprintf(filename,"%s/LOCK", hw_tool_home);
  fd = -1;
  tries = 30;
  while((fd < 0) && (tries))
  	{
  	fd = open(filename,O_RDWR | O_EXCL | O_CREAT, 02777);
  	sleep(1);
  	tries--;
  	}
  if(fd < 0)
  	{
  	fprintf(stderr,"\007Unble to get LOCK permission\n");
  	return(-1);
  	}
  close(fd);
  return(0);
  }

release_lock_perm()
  {
  char blurb[512];
  
  sprintf(blurb, "rm %s/LOCK", hw_tool_home);
  system(blurb);
  }

/* -1 = O.K., machines_list index if overlap start to end overlaps */
/* dont_hit is index in schedule_list to ignore, (use -1 for nothing) */
int check_schedule(startd, endd, serno, dont_hit)
  struct hw_date *startd;
  struct hw_date *endd;
  char *serno;
  int dont_hit; /* index in schedule_list to ignore */
  {
  int index;
    
  for(index = 0; schedule_list[index].serno[0] != '\0'; index++)
	{
	if(strcmp(serno, schedule_list[index].serno) == 0)
  		{ /* then check for date conflict */
  		if(date_overlaps(startd, endd,
				    &schedule_list[index].start,
  			  	    &schedule_list[index].end) &&
  			  	    (schedule_list[index].deleted == 0) &&
  			  	    (index != dont_hit))
  			return(index);
  		}
	}
  return(-1); /* no conflicts detected */
  }

/* return index or -1 */
int find_by_serno(s)
  char *s;
  {
  int i;
  for(i=0; machines_list[i].serno[0] != '\0'; i++)
  	if(strcmp(machines_list[i].serno, s) == 0)
  		return(i);
  return(-1); /* not found */
  }

/* set is_subitem tag on all machines if in a sublist of something else */
hash_machines_list()
  {
  register int i, j, k;
  
  if((machines_list[0].serno[0] == '\0') || (machines_list[1].serno[0] == '\0'))
  	return;
  for(i=0; machines_list[i].serno[0] != '\0'; i++)
  	{
  	/* tag machines in sub list */
  	for(j=0; (machines_list[i].subitems[j][0] != '\0') &&
  	         (j < HW_MAX_SUB_ITEMS); j++)
  		{
  		/* find by serno and tag */
  		k = find_by_serno(machines_list[i].subitems[j]);
  		if(k != -1)
  			machines_list[k].is_subitem = -1;
  		else
  			fprintf(stderr,"\007Warning: bad serial number [%s]\n",
  				machines_list[i].subitems[j]);
  		}
  	}
  }

/* find index in schedule_list[] given a machine, year, month, day, hour */
/* -1 on no match */
int find_schedindex(mach, y, m, d, h)
  int mach, y, m, d, h;
  {
  int i;
  struct hw_date hwd;
  
  hwd.year = y;
  hwd.month = m;
  hwd.day = d;
  hwd.hour = h;
  
  for(i=0; schedule_list[i].serno[0] != '\0'; i++)
  	{
  	if(strcmp(schedule_list[i].serno, machines_list[mach].serno) == 0)
  		{
  		if(date_overlaps(&hwd, &hwd, &schedule_list[i].start,
  						&schedule_list[i].end))
  			{
  			return(i);
  			}
  		}
  	}
  return(-1);
  }

/* return -1 if s is in groups list, else 0 */
int is_in_group(s, g)
  register char *s;
  register char g[][HW_STRING_SIZE];
  {
  register int i;
  
  for(i=0; (g[i][0] != '\0') && (i < HW_GROUPS); i++)
  	if(strcmp(g[i], s) == 0)
  		return(-1);
  return(0);
  }

/* -1 if in g in machine m's class list, else 0 */
int machine_is_in_group(m,g)
  int m; /* index in machines_list */
  char *g; /* group name */
  {
  int i;
  
  for(i=0; (machines_list[m].class[i] != '\0') && (i < MAX_CLASS); i++)
  	if(strcmp(machines_list[m].class[i], g) == 0)
  		return(-1);
  return(0);
  }

int add_to_group(s, g)
  char *s;
  register char g[][HW_STRING_SIZE];
  {
  register int i;
  
  for(i=0; (g[i][0] != '\0') && (i < HW_GROUPS); i++)
  	;
  if(i < HW_GROUPS)
  	{
  	strcpy(g[i],s);
  	i++;
  	if(i < HW_GROUPS)
  		g[i][0] = '\0';
  	
  	}
  else
  	{
  	fprintf(stderr,"\007Too many groups(max is %d)\n", HW_GROUPS);
  	exit(1);
  	}

  return(0);
  }

hash_groups_list()
  {
  int i,classindex;

  /* mark list cleared */
  group[0][0] = '\0';
  
  if(machines_list[0].serno[0] == '\0')
  	return;

  strcpy(group[0], "All");
  strcpy(group[1], "Sun3");
  strcpy(group[2], "Sun4");
  strcpy(group[3], "Sun386i");
  group[3][0] = '\0';
  for(i=0; machines_list[i].serno[0] != '\0'; i++)
  	{
  	if(machines_list[i].is_subitem == 0)
  		{
  		for(classindex=0; 
  			(machines_list[i].class[classindex][0] != '\0') &&
  							(classindex < MAX_CLASS);
  			classindex++)
  	   		if(is_in_group(machines_list[i].class[classindex],
  	   							group) == 0)
  	   			add_to_group(machines_list[i].class[classindex],
  	   								group);
  	   	}
  	}
  /* for(i=0; (group[i][0] != '\0') && (i < HW_GROUPS); i++)
  	printf("group[%d] = %s\n",i,group[i]); */
  }
 
read_machines()
  {
  FILE *fp;
  char filename[256];
  char command[64];
  int index;
  int subindex;
  int classindex;
  char blurb[1024];
   
  sprintf(filename, "%s/machines", hw_tool_home);
  fp = fopen(filename,"r");
  if(fp == NULL)
  	{
  	fprintf(stderr,"\007Unable to  find machine file\n");
  	exit(1);
  	}
  
  index = -1;
  machines_list[0].serno[0] = '\0';
  while(!feof(fp))
  	{
  	fscanf(fp,"%s", command);
  	getc(fp);  /* throw out \n or space */
  	if(strcmp(command,"serno") == 0)
  		hw_getstr(machines_list[index].serno, HW_SERNO_SIZE, fp);
  	else if(strcmp(command,"end") == 0)
  		{
  		index++;
  		if(index < HW_MACHINES)
			machines_list[index].serno[0] = '\0';
		if(machines_list[0].serno[0] == '\0')
  			fprintf(stderr,
  			     "\007WARNING--No machine info inmachines file\n");  		fclose(fp);
  		hash_machines_list();
  		hash_groups_list();
  		return;
  		}
  	else if(strcmp(command,"name") == 0)
  		{
  		index++;
  		if(index >= HW_MACHINES)
  			{
  			fprintf(stderr,
  				"\007Too many machines in machines file\n");
  			exit(2);
  			}
  			
  		subindex = 0;
  		classindex = 0;
  		machines_list[index].subitems[subindex][0] = '\0';
  		machines_list[index].class[classindex][0] = '\0';
  		machines_list[index].description[0] = '\0';
  		machines_list[index].hostid[0] = '\0';
  		machines_list[index].serno[0] = '\0';
  		machines_list[index].name[0] = '\0';
  		machines_list[index].is_subitem = 0;
  		
  		hw_getstr(machines_list[index].name, HW_STRING_SIZE, fp);
  		}
  	else if(strcmp(command,"description") == 0)
  		hw_getstr(machines_list[index].description, HW_STRING_SIZE, fp);
  	else if(strcmp(command,"hostid") == 0)
  		hw_getstr(machines_list[index].hostid, HW_STRING_SIZE, fp);
  	else if(strcmp(command,"#") == 0) /* Comment line */
  		hw_getstr(blurb, 1023, fp);
  	else if(strcmp(command,"class") == 0)
  		{
  		if(classindex < MAX_CLASS)
  			{
  			hw_getstr(machines_list[index].class[classindex],
  							HW_STRING_SIZE, fp);
			classindex++;
  			if(classindex < MAX_CLASS)
				machines_list[index].class[classindex][0] = '\0';
  			}
  		else
  			{
  			fprintf(stderr,
  				"\007Too many classes (max %d)\n", MAX_CLASS);
  			exit(1);
  			}
  		}
  	else if(strcmp(command,"subserno") == 0)
  		{ 
   		if(subindex < HW_MAX_SUB_ITEMS)
   			{
  			hw_getstr(machines_list[index].subitems[subindex], 
  							HW_SERNO_SIZE, fp);
  			subindex++;
  			if(subindex < HW_MAX_SUB_ITEMS)
  				machines_list[index].subitems[subindex][0] = '\0';
  			}
  		else
  			{
  			fprintf(stderr,
  			  "\007Too many sub serial numbers in item %s (max %d)\n",
  				machines_list[index].name, HW_MAX_SUB_ITEMS);
  			exit(2);
  			}
  		}
  	else
  		fprintf(stderr,
  		    "\007WARNING--unrecognized machine command [%s]\n",command);
  	}
  fclose(fp);
  if(index = 0)
  	fprintf(stderr,"\007WARNING--No hardware info in machines file\n");

  hash_machines_list();
  hash_groups_list();
  }

/* 0 on O.K. -1 on end of file */
int read_schedule_entry(fp,index)
  FILE *fp;
  int index; /* in schedule list to put data */
  {
  char buf[256];
  int i,j;
  
  schedule_list[index].deleted = 0;
  hw_getstr(schedule_list[index].serno,    HW_SERNO_SIZE, fp);
  if(strcmp(schedule_list[index].serno,"end") == 0)
  	{
	schedule_list[index].serno[0] = '\0';
  	if(schedule_list[0].serno[0]== '\0')
  	       fprintf(stderr, "\007WARNING-No schedule info in schedule file\n");
	fclose(fp);
  	set_sched_read_time();
  	return(-1);
  	}
  hw_getstr(schedule_list[index].who,      HW_WHO_SIZE,   fp);
  hw_getstr(schedule_list[index].whofor,      HW_WHO_SIZE,   fp);
  hw_getstr(schedule_list[index].whosetup,      HW_WHO_SIZE,   fp);
  hw_getstr(buf,                           128,           fp);
  sscanf(buf, "%d", &schedule_list[index].start.year);
  hw_getstr(buf,                           128,           fp);
  sscanf(buf, "%d", &schedule_list[index].start.month);
  hw_getstr(buf,                           128,           fp);
  sscanf(buf, "%d", &schedule_list[index].start.day);
  hw_getstr(buf,                           128,           fp);
  sscanf(buf, "%d", &schedule_list[index].start.hour);
  hw_getstr(buf,                           128,           fp);
  sscanf(buf, "%d", &schedule_list[index].end.year);
  hw_getstr(buf,                           128,           fp);
  sscanf(buf, "%d", &schedule_list[index].end.month);
  hw_getstr(buf,                           128,           fp);
  sscanf(buf, "%d", &schedule_list[index].end.day);
  hw_getstr(buf,                           128,           fp);
  sscanf(buf, "%d", &schedule_list[index].end.hour);
  
  for(i=0; i < MAX_COMMENT_INDEX; i++)
  	{
  	/* allow for \n at end of line that is droped */
  	hw_getstr(schedule_list[index].comment[i], MAX_COMMENT + 1, fp);
	schedule_list[index].comment[i][MAX_COMMENT - 1] = '\0';
	}
  return(0);
  }

write_schedule_entry(fp,index)
  FILE *fp;
  int index; /* in schedule list to get data from */
  {
  int i,j;
  
  if(schedule_list[index].deleted)
  	return; /* don't write deleted entries */

  fprintf(fp,"%s\n", schedule_list[index].serno);
  fprintf(fp,"%s\n", schedule_list[index].who);
  fprintf(fp,"%s\n", schedule_list[index].whofor);
  fprintf(fp,"%s\n", schedule_list[index].whosetup);
  fprintf(fp, "%d\n", schedule_list[index].start.year);
  fprintf(fp, "%d\n", schedule_list[index].start.month);
  fprintf(fp, "%d\n", schedule_list[index].start.day);
  fprintf(fp, "%d\n", schedule_list[index].start.hour);
  fprintf(fp, "%d\n", schedule_list[index].end.year);
  fprintf(fp, "%d\n", schedule_list[index].end.month);
  fprintf(fp, "%d\n", schedule_list[index].end.day);
  fprintf(fp, "%d\n", schedule_list[index].end.hour);
  for(i=0; i < MAX_COMMENT_INDEX; i++)
  	{
	schedule_list[index].comment[i][MAX_COMMENT - 1] = '\0';
  	fprintf(fp, "%s\n", schedule_list[index].comment[i]);
  	}
  }

read_schedule()
  {
  FILE *fp;
  char filename[256];
  char buf[256];
  int index;
  int i,j;
  
  sprintf(filename, "%s/schedule", hw_tool_home);
  fp = fopen(filename,"r");
  if(fp == NULL)
  	{
  	fprintf(stderr,"\007Unable to  find schedule file\n");
  	exit(1);
  	}
  
  index = 0;
  schedule_list[index].serno[0] = '\0';
  while(!feof(fp))
  	{
  	if(index >= HW_DATES)
  		{
  		fprintf(stderr,"\007Too many schedule dates\n");
  		exit(2);
  		}
  	if(read_schedule_entry(fp, index) != 0)
  		{
  		fclose(fp);
  		set_sched_read_time();
  		return;
  		}
  	index++;
  	}
  fclose(fp);
  set_sched_read_time();
  if(index = 0)
  	fprintf(stderr,"\007WARNING--No schedule info in schedule file\n");
  }

/* stat files to see if they have changed */
/* -1 if a change, 0 if no change thus no reread */
int reread_if_needed()
  {
  struct stat buf;
  char blurb[512];
  
  sprintf(blurb, "%s/schedule", hw_tool_home);
  stat(blurb, &buf);
  if(buf.st_mtime != sched_last_read)
  	{  	
  	window_bell(schedule_canvas);
  	window_set(reread_frame, WIN_SHOW, TRUE, 0);
  	read_schedule();
  	window_set(reread_frame, WIN_SHOW, FALSE, 0);
  	return(-1);
  	}
  return(0);
  }

write_schedule()
  {
  FILE *fp;
  char filename[256];
  char buf[256];
  int index;
  int i,j;
  
  sprintf(filename, "%s/schedule", hw_tool_home);
  fp = fopen(filename,"w+");
  if(fp == NULL)
  	{
  	fprintf(stderr,"\007Unable to open schedule file\n");
  	exit(1);
  	}
  
  for(index = 0; (schedule_list[index].serno[0] != '\0') && (index < HW_DATES); 
  									index++)
  	write_schedule_entry(fp, index);
  fprintf(fp,"end\n");
  fclose(fp);
  if(index = 0)
  	fprintf(stderr,
  		"\007WARNING--No schedule info written to schedule file\n");
  }

/* must already have lock perm */
/* -1 if O.K. , machines_list index if overlap, -2 on error (no pick) */
int insert_ok()
  {
  int index;
  int lastindex;
  FILE *fp;
  char filename[512];
  int ret;
  
  
  if(pick_index >= 0)
  	return(pick_index);

  if(pick_sched.start.year == 0)
  	return(-2);

  if(pick_sched.end.year == 0)
  	{
  	pick_sched.end.year  = pick_sched.start.year;
  	pick_sched.end.month = pick_sched.start.month;
  	pick_sched.end.day   = pick_sched.start.day;
  	pick_sched.end.hour  = pick_sched.start.hour;
  	}

  /* check for schedule overlap */
  ret = check_schedule(&pick_sched.start,
  		    &pick_sched.end,
  		    machines_list[current_machine].serno, -1);
  return(ret); /* will be -1 or machines_list index */
  }

read_policy()
  {
  FILE *fp;
  char filename[256];
  char buf[128];
  int index;
   
  sprintf(filename, "%s/policies", hw_tool_home);
  fp = fopen(filename,"r");
  if(fp == NULL)
  	{
  	fprintf(stderr,"\007Unable to  find policy file\n");
  	exit(1);
  	}
  
  index = 0;
  schedule_list[index].serno[0] = '\0';
  while(!feof(fp))
  	{
  	if(index >= HW_POLICY)
  		{
  		fprintf(stderr,"\007Too many policy entries\n");
  		exit(2);
  		}
  	hw_getstr(policy_list[index].who, HW_WHO_SIZE, fp);
  	if(strcmp(policy_list[index].who,"end") == 0)
  		{
  		policy_list[index].who[0] = '\0';
  		if(policy_list[index].who[0]= '\0')
  			fprintf(stderr,
  				"\007WARNING--No policy info in policies file\n");
  		fclose(fp);
  		return;
  		}
  	hw_getstr(buf,                    128,         fp);
  	if(strcmp(buf,"free") == 0)
  		policy_list[index].limited = POLICY_FREE;
  	else if(strcmp(buf,"limited") == 0)
  		policy_list[index].limited = POLICY_LIMITED;
  	else
  		{
  		fprintf(stderr,
  			"\007Error in policy file, must be limited or free\n");
  		fprintf(stderr,"Near [%s] bad word is [%s]\n",
  			policy_list[index].who, buf);
  		exit(2);
  		}
  	index++;
  	}
  fclose(fp);
  if(index = 0)
  	fprintf(stderr,"\007WARNING--No policy info in policies file\n");
  }

/* return -1 if no select, 0 if O.K. */
/* (Note: the first of the month is day# 0) */
/* dt designates month and year, is filled with day and hour on return */
/* dt.day unpredictable on error */
int canvasxy_to_date(x, y, dt)
  int x,y;
  struct hw_date *dt;
  {
  int days_this_month;
  struct hw_date td;
  int startx;
  int day;
   
  if(y < CAL_TITLE_HEIGHT)
  	return(-1); /* in title box */

  y = (y - CAL_TITLE_HEIGHT) / (BOX_HEIGHT / 2);
  if(y & 1)
  	dt->hour = 13;
  else
  	dt->hour = 8;
  y = y / 2;
  x = x / BOX_WIDTH;
  
  td.year = dt->year;
  td.month = dt->month;
  td.day = 0;
  td.hour = 0;
  startx = week_day(&td);
  if((y == 0) && (x < startx))
  	return(-1); /* before first of month */
  dt->day = (y * 7) + x - startx;
  if(dt->day >= month_length(dt->month, dt->year))
  	return(-1); /* after last of month */
  
  return(0);
  }

draw_schedule_period(sched, year, month)
  struct sched_item *sched;
  int year;
  int month;
  {
  struct hw_date td;
  char blurb[256];
  int i, j, k;
  int framex, framey;
  int commentx, commenty;
  
  /* scan period of the entry */
  td.year = sched->start.year;
  td.month = sched->start.month;
  td.day = sched->start.day;
  td.hour = sched->start.hour;
  while(relative_date_hours(&td) <= relative_date_hours(&sched->end))
  	{
  	if((td.year == year) && (td.month == month))
  		{ /* plot it */
  	 	/* get to corner of frame */
  	 	if(td.hour == 8) /* morning */
  	 	      	framey = CAL_TITLE_HEIGHT + 
 				((hw_current_day_0 + td.day) / 7) * BOX_HEIGHT;
  	 	else /* hour must be 13 (afternoon) */
  	 	      	framey = CAL_TITLE_HEIGHT +BOX_HEIGHT /2+ 
  	 	      		((hw_current_day_0 + td.day) / 7) * BOX_HEIGHT;
  	 	framex =((hw_current_day_0 +td.day)% 7) * BOX_WIDTH;
  		sprintf(blurb, "%s", sched->whofor);
  		pw_text_ps(schedule_pw, framex + 30, framey + 15,
  				PIX_SRC, cal_who_pf, blurb);
  		/* do comment */
  		commentx = framex + 4;
  		commenty = framey + 25;
  		for(j = 0; j < MAX_COMMENT_INDEX; j++)
  			{
  			pw_text_ps(schedule_pw, commentx, commenty,
  					PIX_SRC, cal_notes_pf, sched->comment[j]);
  		  	commenty = commenty + 11;
  			}
  		}
  	/* increment td to next half day */
  	if(td.hour == 8)
  		add_hours(&td, 5); /* get to 13:00 */
  	else /* must be 13:00 */
  		add_hours(&td, 19); /* get to 8:00 */
  	}
  }

highlight_schedule_period(schedstart, schedend, year, month)
  struct hw_date *schedstart;
  struct hw_date *schedend;
  int year;
  int month;
  {
  struct hw_date td;
  char blurb[256];
  int i, j, k;
  int framex, framey;
  
  /* scan period of the entry */
  td.year = schedstart->year;
  td.month = schedstart->month;
  td.day = schedstart->day;
  td.hour = schedstart->hour;
  while(relative_date_hours(&td) <= relative_date_hours(schedend))
  	{
  	if((td.year == year) && (td.month == month))
  		{ /* plot it */
  	 	/* get to corner of frame */
  	 	if(td.hour == 8) /* morning */
  	 	      	framey = CAL_TITLE_HEIGHT + 
 				((hw_current_day_0 + td.day) / 7) * BOX_HEIGHT;
  	 	else /* hour must be 13 (afternoon) */
  	 	      	framey = CAL_TITLE_HEIGHT +BOX_HEIGHT /2+ 
  	 	      		((hw_current_day_0 + td.day) / 7) * BOX_HEIGHT;
  	 	framex =((hw_current_day_0 +td.day)% 7) * BOX_WIDTH;
  	 	highlight_ps(schedule_pw, framex + BOX_WIDTH - 18, framey + 4);
  		}
  	/* increment td to next half day */
  	if(td.hour == 8)
  		add_hours(&td, 5); /* get to 13:00 */
  	else /* must be 13:00 */
  		add_hours(&td, 19); /* get to 8:00 */
  	}
  }

#define DATA_Y_INC 16
/* fill in info box */
/* machine_list[m], schedule_list[si] */
draw_data(m, si)
  int m;
  int si;
  {
  int datay;
  int i, j;
  char blurb[512];
  
  /* do data section on right */
  datay = 13;
  pw_text_ps(info_pw, 3, datay,
  	PIX_SRC, cal_month_pf, machines_list[m].name);
  datay = datay + DATA_Y_INC;
  sprintf(blurb, "Serial Number: %s", machines_list[m].serno);
  pw_text_ps(info_pw, 3, datay,
  	PIX_SRC, cal_data_pf, blurb);
  datay = datay + DATA_Y_INC;
  
  pw_text_ps(info_pw, 3, datay,
  	PIX_SRC, cal_data_pf, machines_list[m].description);
  datay = datay + DATA_Y_INC;
  
  sprintf(blurb, "Host Id: %s", machines_list[m].hostid);
  pw_text_ps(info_pw, 3, datay,
  	PIX_SRC, cal_data_pf, blurb);
  datay = datay + DATA_Y_INC;
  
  pw_text_ps(info_pw, 3, datay,
  	PIX_SRC, cal_data_pf, "\"Is a\" or \"in group\":");
  datay = datay + DATA_Y_INC;
  
  for(i=0; (machines_list[m].class[i] != '\0') && (i < MAX_CLASS); i++)
  	{
  	pw_text_ps(info_pw, 13, datay,
  		PIX_SRC, cal_data_pf, machines_list[m].class[i]);
  	datay = datay + DATA_Y_INC;
  	}
  if(i == 0)
  	{
  	pw_text_ps(info_pw, 13, datay,
  		PIX_SRC, cal_data_pf, "Not in any Group");
  	datay = datay + DATA_Y_INC;
  	}

  datay = datay + DATA_Y_INC;
  pw_text_ps(info_pw, 3, datay,
  	PIX_SRC, cal_data_pf, "Accessories:");
  datay = datay + DATA_Y_INC;
  
  for(i=0;(machines_list[m].subitems[i][0] != '\0')&& (i< HW_MAX_SUB_ITEMS); i++)
  	{
  	j = find_by_serno(machines_list[m].subitems[i]);
  	if(j != -1)
  		{
  		pw_text_ps(info_pw, 10, datay,
  			PIX_SRC, cal_data_pf, machines_list[j].description);
  		datay = datay + DATA_Y_INC;
  		pw_text_ps(info_pw, 65, datay,
  			PIX_SRC, cal_data_pf, machines_list[j].serno);
  		datay = datay + DATA_Y_INC;
  		}
  	}
  if(i == 0)
  	{
  	pw_text_ps(info_pw, 13, datay,
  		PIX_SRC, cal_data_pf, "No Accessories");
  	datay = datay + DATA_Y_INC;
  	}
  /* If picked on a scheduled item, show data */
  if(si >= 0)
  	{
  	datay = datay + DATA_Y_INC;
  	datay = datay + DATA_Y_INC;
  	pw_text_ps(info_pw, 3, datay,
  			PIX_SRC, cal_month_pf, "Schedule Info");
  	datay = datay + DATA_Y_INC;

  	sprintf(blurb, "For: %s", schedule_list[si].whofor);
  	pw_text_ps(info_pw, 3, datay, PIX_SRC, cal_data_pf, blurb);
  	datay = datay + DATA_Y_INC;

  	sprintf(blurb, "T.S.E setup: %s", schedule_list[si].whosetup);
  	pw_text_ps(info_pw, 3, datay, PIX_SRC, cal_data_pf, blurb);
  	datay = datay + DATA_Y_INC;

  	for(i=0;i< MAX_COMMENT_INDEX; i++)
  		{
  		pw_text_ps(info_pw, 3, datay,
  		     PIX_SRC, cal_data_pf, schedule_list[si].comment[i]);
  		datay = datay + DATA_Y_INC;
  		}
  	}
  
  }

draw_month_calendar(year, month, serno)
int year;
  int month;
  char *serno;
  {
  int i,j,k,m;
  char blurb[256];
  struct hw_date td;
  int framex;
  int framey;
  int days_this_month;
  int datay;
  
  /* clear canvases */
  pw_clear_ps(schedule_pw);
  pw_clear_ps(info_pw);
  
  /* draw boxes  (vertical lines) */
  for(i=0; i<6; i++)
      {
      /* main bold box */
      pw_vector_ps(schedule_pw, BOX_WIDTH + i * BOX_WIDTH, CAL_TITLE_HEIGHT,
      		BOX_WIDTH + i * BOX_WIDTH, CALENDAR_CANVAS_HEIGHT,
      		PIX_SRC, 1);
      pw_vector_ps(schedule_pw, BOX_WIDTH + i * BOX_WIDTH + BOX_LINE_WIDTH,
      		CAL_TITLE_HEIGHT,
      		BOX_WIDTH + i * BOX_WIDTH + BOX_LINE_WIDTH,
      		CALENDAR_CANVAS_HEIGHT,
      		PIX_SRC, 1);
      }
  /* (horizontal) */
  for(i=0; i<6; i++)
      {
      /* main bold */
      pw_vector_ps(schedule_pw, 
      		0, 
      		CAL_TITLE_HEIGHT + i * BOX_HEIGHT, 
      		CALENDAR_CANVAS_WIDTH, 
      		CAL_TITLE_HEIGHT +  i * BOX_HEIGHT,
      	 	PIX_SRC, 1);
      pw_vector_ps(schedule_pw, 
      		0, 
      		CAL_TITLE_HEIGHT + i * BOX_HEIGHT + BOX_LINE_WIDTH, 
      		CALENDAR_CANVAS_WIDTH, 
      		CAL_TITLE_HEIGHT + i * BOX_HEIGHT + BOX_LINE_WIDTH,
      		PIX_SRC, 1);
      /* half day divisions */
      pw_vector_ps(schedule_pw, 
      		0, 
      		CAL_TITLE_HEIGHT + i * BOX_HEIGHT + BOX_HEIGHT/2,
      		CALENDAR_CANVAS_WIDTH, 
      		CAL_TITLE_HEIGHT + i * BOX_HEIGHT + BOX_HEIGHT/2,
      		PIX_SRC, 1);
      }
  /* week days at top */
  for(i=0; i < 7 ; i++)
  	{
  	pw_text_ps(schedule_pw, i * BOX_WIDTH + 25, CAL_TITLE_HEIGHT - 5,
  			PIX_SRC, cal_weekday_pf, hw_day_names[i]);
  	}
  /* month */
  sprintf(blurb,"%s %d", hw_month_names[month], year);
  pw_text_ps(schedule_pw, (CALENDAR_CANVAS_WIDTH / 2) - 80, 13,
  	PIX_SRC, cal_month_pf, blurb);
  
  /* Machine Name and Serial # */
  m = find_by_serno(serno);
  if(m != -1)
  	sprintf(blurb,"%s [%s]", machines_list[m].name, serno);
  else
  	sprintf(blurb,"?????? [????????]");
  pw_text_ps(schedule_pw, 5, 13, PIX_SRC, cal_month_pf, blurb);
  
  /* do day numbers */
  td.year = year;
  td.month = month;
  td.day = 0;
  td.hour = 0;
  framex = week_day(&td);
  hw_current_day_0 = framex; /* (global, used in several routines) */
  framey = 0;
  days_this_month = month_length(month, year);
  for(i=0; i < days_this_month; i++)
  	{
  	sprintf(blurb, "%d", i+1);
  	pw_text_ps(schedule_pw, framex * BOX_WIDTH + 5, 
  			CAL_TITLE_HEIGHT + framey * BOX_HEIGHT + 17,
  			PIX_SRC, cal_number_pf, blurb);
  	framex++;
  	if(framex > 6)
  		{
  		framex = 0;
  		framey++;
  		}
  	}
  
  draw_data(m, pick_index);

  /* fill in reserved boxes */
  for(i=0; schedule_list[i].serno[0] != '\0'; i++)
  	{ /* scan schedule list */
  	if(strcmp(schedule_list[i].serno, serno) == 0)
		draw_schedule_period(&schedule_list[i], year, month);
  	}
  
  if(pick_sched.start.year !=0)
  	{
  	if(pick_sched.end.year != 0)
  		highlight_schedule_period(&pick_sched.start, 
					&pick_sched.end, 
					current_date.year, current_date.month);
	else
  		highlight_schedule_period(&pick_sched.start, 
					&pick_sched.start, 
					current_date.year, current_date.month);
	}
  if(pick_index >= 0)
  	{
  	highlight_schedule_period(&schedule_list[pick_index].start, 
					&schedule_list[pick_index].end, 
					current_date.year, current_date.month);
  	}
  } 
  
draw_bars(pw, m, y, year, month)
  Pixwin *pw;
  int m; /* machine index */
  int y; /* y position */
  int year, month;
  {
  int i,j;
  int starth, endh, months, monthe; /* start/end hours */
  struct hw_date td;
  int x1, x2;
  int daysinmonth;
  
  td.year = year;
  td.month = month;
  td.day = 0;
  td.hour = 1;
  months = relative_date_hours(&td);
  td.year = year;
  td.month = month;
  daysinmonth = month_length(month, year);
  td.day = daysinmonth - 1;
  td.hour = 23;
  monthe = relative_date_hours(&td);
  
  for(i=0; schedule_list[i].serno[0] != '\0'; i++)
  	{ /* scan schedule list */
  	if(strcmp(schedule_list[i].serno, machines_list[m].serno) == 0)
  		{ /* show period */
  		starth = relative_date_hours(&schedule_list[i].start);
  		endh = relative_date_hours(&schedule_list[i].end);
  		if((months < endh) && (monthe > starth))
  			{ /* plot it */
  			if(months > starth)
  				x1 = BAR_DATE_START;
  			else
  				x1 = BAR_HOUR_START(schedule_list[i].start.day,
  					schedule_list[i].start.hour);
  			if(monthe < endh)
  				x2 = BAR_HOUR_START(daysinmonth, 23);
  			else
  				x2 = BAR_HOUR_START(schedule_list[i].end.day,
  					schedule_list[i].end.hour);
  			for(j=0; j < BAR_LINE_WIDTH; j++)
  				pw_vector_ps(pw, x1, y + j - (BAR_LINE_WIDTH / 2),
  					x2, y + j - (BAR_LINE_WIDTH / 2),
  					PIX_SRC, 1);
  			/* do ticks */
  			if(months <= starth)
  				pw_vector_ps(pw,x1, y + BAR_TICK_SIZE,
  				             x1, y - BAR_TICK_SIZE,
  				             PIX_SRC, 1);
  			if(monthe >= endh)
  				pw_vector_ps(pw,x2, y + BAR_TICK_SIZE,
  				             x2, y - BAR_TICK_SIZE,
  				             PIX_SRC, 1);
  			}
  		}
  	}

  }

/* returns 0 if nothing drawn, else -1 */
int draw_bars_whofor(pw, w, m, y, year, month)
  Pixwin *pw;
  char *w; /* who for */
  int m; /* machines list index */
  int y; /* y position */
  int year, month;
  {
  int i,j;
  int starth, endh, months, monthe; /* start/end hours */
  struct hw_date td;
  int x1, x2;
  int daysinmonth;
  int drawn;
  
  drawn = 0;
  td.year = year;
  td.month = month;
  td.day = 0;
  td.hour = 1;
  months = relative_date_hours(&td);
  td.year = year;
  td.month = month;
  daysinmonth = month_length(month, year);
  td.day = daysinmonth - 1;
  td.hour = 23;
  monthe = relative_date_hours(&td);
  
  for(i=0; schedule_list[i].serno[0] != '\0'; i++)
  	{ /* scan schedule list */
  	if((strcmp(schedule_list[i].whofor, w) == 0) &&
  	   (strcmp(machines_list[m].serno, schedule_list[i].serno) == 0))
  		{ /* show period */
  		starth = relative_date_hours(&schedule_list[i].start);
  		endh = relative_date_hours(&schedule_list[i].end);
  		if((months < endh) && (monthe > starth))
  			{ /* plot it */
  			drawn = -1;
  			if(months > starth)
  				x1 = BAR_DATE_START;
  			else
  				x1 = BAR_HOUR_START(schedule_list[i].start.day,
  					schedule_list[i].start.hour);
  			if(monthe < endh)
  				x2 = BAR_HOUR_START(daysinmonth, 23);
  			else
  				x2 = BAR_HOUR_START(schedule_list[i].end.day,
  					schedule_list[i].end.hour);
  			for(j=0; j < BAR_LINE_WIDTH; j++)
  				pw_vector_ps(pw, x1, y + j - (BAR_LINE_WIDTH / 2),
  					x2, y + j - (BAR_LINE_WIDTH / 2),
  					PIX_SRC, 1);
  			/* do ticks */
  			if(months <= starth)
  				pw_vector_ps(pw,x1, y + BAR_TICK_SIZE,
  				             x1, y - BAR_TICK_SIZE,
  				             PIX_SRC, 1);
  			if(monthe >= endh)
  				pw_vector_ps(pw,x2, y + BAR_TICK_SIZE,
  				             x2, y - BAR_TICK_SIZE,
  				             PIX_SRC, 1);
  			}
  		}
  	}
  return(drawn);
  }

/* returns 0 if nothing plotted */
int draw_bars_whosetup(pw, w, m, y, year, month)
  Pixwin *pw;
  char *w; /* who for */
  int m; /* machine index */
  int y; /* y position */
  int year, month;
  {
  int i,j;
  int starth, endh, months, monthe; /* start/end hours */
  struct hw_date td;
  int x1, x2;
  int daysinmonth;
  int drawn;
  
  drawn = 0;
  td.year = year;
  td.month = month;
  td.day = 0;
  td.hour = 1;
  months = relative_date_hours(&td);
  td.year = year;
  td.month = month;
  daysinmonth = month_length(month, year);
  td.day = daysinmonth - 1;
  td.hour = 23;
  monthe = relative_date_hours(&td);
  
  for(i=0; schedule_list[i].serno[0] != '\0'; i++)
  	{ /* scan schedule list */
  	if((strcmp(schedule_list[i].whosetup, w) == 0) &&
   	   (strcmp(machines_list[m].serno, schedule_list[i].serno) == 0))
 		{ /* show period */
  		starth = relative_date_hours(&schedule_list[i].start);
  		endh = relative_date_hours(&schedule_list[i].end);
  		if((months < endh) && (monthe > starth))
  			{ /* plot it */
  			drawn = -1;
  			if(months > starth)
  				x1 = BAR_DATE_START;
  			else
  				x1 = BAR_HOUR_START(schedule_list[i].start.day,
  					schedule_list[i].start.hour);
  			if(monthe < endh)
  				x2 = BAR_HOUR_START(daysinmonth, 23);
  			else
  				x2 = BAR_HOUR_START(schedule_list[i].end.day,
  					schedule_list[i].end.hour);
  			for(j=0; j < BAR_LINE_WIDTH; j++)
  				pw_vector_ps(pw, x1, y + j - (BAR_LINE_WIDTH / 2),
  					x2, y + j - (BAR_LINE_WIDTH / 2),
  					PIX_SRC, 1);
  			/* do ticks */
  			if(months <= starth)
  				pw_vector_ps(pw,x1, y + BAR_TICK_SIZE,
  				             x1, y - BAR_TICK_SIZE,
  				             PIX_SRC, 1);
  			if(monthe >= endh)
  				pw_vector_ps(pw,x2, y + BAR_TICK_SIZE,
  				             x2, y - BAR_TICK_SIZE,
  				             PIX_SRC, 1);
  			}
  		}
  	}
  return(drawn);
  }

draw_month_barchart(year, month)
int year;
  int month;
  {
  int i,j,k,m;
  char blurb[256];
  struct hw_date td;
  int framex;
  int framey;
  int days_this_month;
  int commentx, commenty;
  int firstday;
  int labelpoint;
  
  /* clear canvas */
  pw_clear_ps(schedule_pw);
  pw_clear_ps(info_pw);
  
  /* show detail if enabled */
  if(detail_index >= 0)
  	{
  	}
  /* month */
  sprintf(blurb,"%s %d", hw_month_names[month], year);
  pw_text_ps(schedule_pw, (CALENDAR_CANVAS_WIDTH / 2) - 80, 13,
  	PIX_SRC, bar_month_pf, blurb);
  /* group name */
  if(current_group >= 0)
  	sprintf(blurb,"Group %s", group[current_group]);
  else if(current_whofor[0] != 0)
  	sprintf(blurb,"Group for %s", current_whofor);
  else if(current_whosetup[0] != 0)
  	sprintf(blurb,"Group setup by %s", current_whosetup);
  else
  	sprintf(blurb,"-ERROR-");
  pw_text_ps(schedule_pw, 5, 13, PIX_SRC, bar_month_pf, blurb);
  /* line under month name */
  pw_vector_ps(schedule_pw, 0, BAR_TITLE_HEIGHT, CALENDAR_CANVAS_WIDTH,
  		BAR_TITLE_HEIGHT, PIX_SRC, 1);
  pw_vector_ps(schedule_pw, 0, BAR_TITLE_HEIGHT + 2, CALENDAR_CANVAS_WIDTH,
  		BAR_TITLE_HEIGHT + 2, PIX_SRC, 1);
  
  /* do day numbers */
  days_this_month = month_length(month, year);
  if(days_this_month == 0)
  	return;
  td.year = year;
  td.month = month;
  td.day = 0;
  td.hour = 1;
  firstday = week_day(&td);
  
  for(i=0; i<days_this_month; i++, firstday++)
  	{
  	sprintf(blurb, "%d", i + 1);
  	pw_text_ps(schedule_pw,
  		BAR_DAY_START(i) + 3,
  		BAR_TITLE_HEIGHT - 5,
  		PIX_SRC, bar_day_pf, blurb);
  	/* do week day */
  	sprintf(blurb,"%s", hw_day_names[firstday % 7]);
  	blurb[2] = '\0';
  	pw_text_ps(schedule_pw,
  		BAR_DAY_START(i) + 3,
  		BAR_TITLE_HEIGHT - 5 - 14,
  		PIX_SRC, bar_day_pf, blurb);
  	if((firstday % 7) == 0)
  		{ /* do vertical line on monday */
  		pw_vector_ps(schedule_pw, 
  			BAR_DAY_START(i),
  			BAR_TITLE_HEIGHT,
  			BAR_DAY_START(i),
  			CALENDAR_CANVAS_HEIGHT, 
  			PIX_SRC, 1);
  		pw_vector_ps(schedule_pw, 
  			BAR_DAY_START(i) - 1,
  			BAR_TITLE_HEIGHT,
  			BAR_DAY_START(i) - 1,
  			CALENDAR_CANVAS_HEIGHT, 
  			PIX_SRC, 1);
  		pw_vector_ps(schedule_pw, 
  			BAR_DAY_START(i) - 2,
  			BAR_TITLE_HEIGHT,
  			BAR_DAY_START(i) - 2,
  			CALENDAR_CANVAS_HEIGHT, 
  			PIX_SRC, 1);
  		}
  	else
  		{ /* do vertical line on not-mondays */
  		pw_vector_ps(schedule_pw, 
  			BAR_DAY_START(i),
  			BAR_TITLE_HEIGHT,
  			BAR_DAY_START(i),
  			CALENDAR_CANVAS_HEIGHT, 
  			PIX_SRC, 1);
  		}
  	}
  /* draw labels and bars */
  pick_list_top = 0;
  labelpoint = BAR_TITLE_HEIGHT + 10;
  if(current_group >= 0)
      for(i=0; (machines_list[i].serno[0] != '\0') && (i < HW_MACHINES); i++)
  	{
  	if((machines_list[i].is_subitem == 0) &&           /* Group All is 0 */
  	   (machine_is_in_group(i, group[current_group]) || (current_group == 0)))
  		{ /* do label and bar */
  		/* label */
  		pw_text_ps(schedule_pw, 3, labelpoint, PIX_SRC, bar_day_pf,
  			machines_list[i].name);
  		pick_list[pick_list_top] = i;
  		pick_list_top++;
  		
  		draw_bars(schedule_pw, i, labelpoint - 4, year, month);
  		
  		labelpoint = labelpoint + 15;
  		}
  	}
  else if(current_whofor[0] != '\0')
     for(i=0; (machines_list[i].serno[0] != '\0') && (i < HW_MACHINES); i++)
  	{
  	if(machines_list[i].is_subitem == 0)
  		{ /* do label and bar */
  		
  		if(draw_bars_whofor(schedule_pw, current_whofor, i,
  					labelpoint - 4, year, month))
  			{
  			/* label */
  			pick_list[pick_list_top] = i;
  			pick_list_top++;
  			pw_text_ps(schedule_pw, 3, labelpoint,PIX_SRC, bar_day_pf,
  							machines_list[i].name);
  			labelpoint = labelpoint + 15;
  			}
  		}
  	}
  else if(current_whosetup[0] != '\0')
     for(i=0; (machines_list[i].serno[0] != '\0') && (i < HW_MACHINES); i++)
  	{
  	if(machines_list[i].is_subitem == 0)
  		{ /* do label and bar */
  		
  		if(draw_bars_whosetup(schedule_pw, current_whosetup, i,
  					labelpoint - 4, year, month))
  			{
  			/* label */
  			pick_list[pick_list_top] = i;
  			pick_list_top++;
  			pw_text_ps(schedule_pw, 3, labelpoint,PIX_SRC, bar_day_pf,
  							machines_list[i].name);
  			labelpoint = labelpoint + 15;
  			}
  		}
  	}
  
  if(detail_index != -1)
  	{
  	m = find_by_serno(schedule_list[detail_index].serno);
  	draw_data(m, detail_index);
  	}
  } 

/* copy file out fp (used to append ps frags) */
/* makes path be rootname/filename if rootname not NULL */
/* This is for ease of use as ps_copy_file(fp, "junk.ps", hw_tool_home); */
ps_copy_file(fp, filename, rootname)
  FILE *fp;
  char *filename;
  char *rootname;
  {
  FILE *infp;
  char blurb[256];
  
  if(rootname == NULL)
  	strcpy(blurb, filename);
  else
  	sprintf(blurb, "%s/%s", rootname, filename);

  infp = fopen(blurb, "r");
  if(infp == NULL)
  	{
  	fprintf(stderr,"\007Unable to open %s (postscript header)[2]\n", blurb);
  	exit(1);
  	}
  while(!feof(infp))
  	putc(getc(infp), fp);
  }

back_month()
  {
  current_date.month = current_date.month - 1;
  if(current_date.month < 0)
  	{
  	current_date.month = 11;
  	current_date.year = current_date.year - 1;
  	if(current_date.year < 1987)
  		{
  		current_date.year = 1987;
  		current_date.month = 0;
  		}
  	}
  }
 
forward_month()
  {
  current_date.month = current_date.month + 1;
  if(current_date.month > 11)
  	{
  	current_date.month = 0;
  	current_date.year = current_date.year + 1;
  	}
  }

/* The hwtool logo for messages */
print_logo(fp)
  FILE *fp;
  {
  fprintf(fp,"H   H  W   W  TTTTT  OOO    OOO   L\n");
  fprintf(fp,"H   H  W   W    T   O   O  O   O  L\n");
  fprintf(fp,"HHHHH  W W W    T   O   O  O   O  L\n");
  fprintf(fp,"H   H  WW WW    T   O   O  O   O  L\n");
  fprintf(fp,"H   H  W   W    T    OOO    OOO   LLLL\n\n");
  }

/* Sends schdule info to fp, using schedule_list[sched_i], title at top*/
file_message(fp, sched_i, title, nologo)
  FILE *fp;
  int sched_i;
  char *title;
  int nologo; /* no logo if not 0 */
  {
  char blurb[1024];
  int m, j;
  
  if((sched_i < 0) || (fp == NULL) || (title == NULL))
  	return;
  
  m = find_by_serno(schedule_list[sched_i].serno);
  if(m < 0)
  	{
  	fprintf(stderr,"\007Program, bad serno [001]\n");
  	return;
  	}
  if(nologo == 0)
  	print_logo(fp);
  fprintf(fp, "%s\n", title);
  fprintf(fp,
  	  "\nYou have machine %s [%s]\nFor %s %d, %d %d:00 to %s %d, %d %d:00\n",
  	machines_list[m].name,
  	schedule_list[sched_i].serno,
  	hw_month_names[schedule_list[sched_i].start.month],
  	schedule_list[sched_i].start.day + 1,
  	schedule_list[sched_i].start.year,
  	schedule_list[sched_i].start.hour,
  	hw_month_names[schedule_list[sched_i].end.month],
  	schedule_list[sched_i].end.day + 1,
  	schedule_list[sched_i].end.year,
  	schedule_list[sched_i].end.hour);
  fprintf(fp, "Scheduled by %s for %s, tse setting up will be %s\n",
  	schedule_list[sched_i].who,
  	schedule_list[sched_i].whofor,
  	schedule_list[sched_i].whosetup);
  for(j=0; j<MAX_COMMENT_INDEX; j++)
  	fprintf(fp, "*** %s\n", schedule_list[sched_i].comment[j]);
  fprintf(fp, "(Host Id:%s)\n", machines_list[m].hostid);
  }

/* Sends schdule E-mail to whoto, using schedule_list[sched_i], title at top*/
email_message(whoto, sched_i, title)
  char *whoto;
  int sched_i;
  char *title;
  {
  char blurb[1024];
  FILE *mailfp;
  int m;
  
  if((whoto == NULL) || (whoto[0] == '\0'))
  	return;

  m = find_by_serno(schedule_list[sched_i].serno);
  
  sprintf(blurb, "mail -s \"%s [%s] (hostid: %s)\" %s\n",
  						machines_list[m].name,
  						schedule_list[sched_i].serno,
  						machines_list[m].hostid,
  						whoto);
  mailfp = popen(blurb, "w");
  if(mailfp == NULL)
  	{
  	fprintf(stderr,"\007Unable to send E-mail to %s\n", whoto);
  	return;
  	}
  file_message(mailfp, sched_i, title, 0);
  pclose(mailfp);
  }

/* Sends schdule message to logfile, using schedule_list[sched_i], title at top*/
logfile_message(sched_i, title)
  int sched_i;
  char *title;
  {
  char blurb[1024];
  FILE *logfp;
  
  sprintf(blurb,"%s/hwlog", hw_tool_home);
  logfp = fopen(blurb, "a+");
  if(logfp == NULL)
  	{
  	fprintf(stderr,"\007Unable to write to hwlog file.\n");
  	return;
  	}
  file_message(logfp, sched_i, title, 1);
  fclose(logfp);
  }

email_scheduled(i)
  int i;
  {
  
  if((i < 0) || (schedule_list[i].deleted))
  	return;
  
  /* To who for */
  email_message(schedule_list[i].whofor, i, "MACHINE SCHEDULED");
  /* to TSE */
  email_message(schedule_list[i].whosetup, i, "T.S.E. Setup SCHEDULED");
  /* logfile */
  logfile_message(i, "SCHEDULED");
  }

email_annotated(i)
  int i;
  {
  
  if((i < 0) || (schedule_list[i].deleted))
  	return;
  
  /* To who for */
  email_message(schedule_list[i].whofor, i, "MACHINE RE-ANNOTATED");
  /* to TSE */
  email_message(schedule_list[i].whosetup, i, "T.S.E. Setup RE-ANNOTATED");
  /* logfile */
  logfile_message(i, "RE-ANNOTATED");
  }

email_unscheduled(i)
  int i;
  {
  
  if(i < 0)
  	return;
  
  /* To who for */
  email_message(schedule_list[i].whofor, i, "MACHINE UN-SCHEDULED");
  /* to TSE */
  email_message(schedule_list[i].whosetup, i, "T.S.E. Setup UN-SCHEDULED");
  /* logfile */
  logfile_message(i, "UN-SCHEDULED");
  }

schedule()
  {
  int i,j;
  int delete_conflicts;
  int delete_m;

  if(pick_index >= 0)
  	{
  	fprintf(stderr,
  	"\007 must delete first, can't re-schedule a machine that way!\n");
  	return; /* can't re-schedule a machine like that */
  	}

  if(get_lock_perm())
  	{
  	fprintf(stderr,"\007Can't get exclusive permision\n");
  	fprintf(stderr,"Schedule not updated\n");
  	return(-1);
  	}

  reread_if_needed();
  
  delete_conflicts = 0;
  if(insert_ok() != -1) /* must be clear to schedule the period */
  	{
  	window_bell(schedule_canvas);
  	i = (int)window_loop(conf_frame);
  	if(i == 0) /* cancel */
  		{
		release_lock_perm();
  		return;
  		}
  	delete_conflicts = -1;
  	}

  for(i=0; i < MAX_COMMENT_INDEX; i++)
  	panel_set_value(anno[i], "");
  panel_set_value(anno_who_for, passwd->pw_name);
  panel_set_value(anno_who_setup, passwd->pw_name);
  	
  i = (int)window_loop(anno_frame);
  if(i == 0) /* cancel */
  	{
	release_lock_perm();
  	return;
  	}

  if(delete_conflicts)
  	{ /* scan for and delete conflicts */
  	while((delete_m = check_schedule(&pick_sched.start, &pick_sched.end,
  		    		  machines_list[current_machine].serno, -1)) >= 0)
  		{
  		schedule_list[delete_m].deleted = -1;
  		}
  	}

  /* append to schedule list */
  /* get to end */
  for(i=0; (i<HW_DATES) && (schedule_list[i].serno[0] != '\0'); i++)
  	;
  if(i < HW_DATES)
  	{
  	/* make new entry */
  	strcpy(schedule_list[i].serno, machines_list[current_machine].serno);
	strcpy(schedule_list[i].who, passwd->pw_name);
	schedule_list[i].whofor[0] = '\0';
	strcpy(schedule_list[i].whofor, panel_get_value(anno_who_for));
	schedule_list[i].whosetup[0] = '\0';
	strcpy(schedule_list[i].whosetup,panel_get_value(anno_who_setup));
	schedule_list[i].start.year = pick_sched.start.year;
	schedule_list[i].end.year = pick_sched.end.year;
	schedule_list[i].start.month = pick_sched.start.month;
	schedule_list[i].end.month = pick_sched.end.month;
	schedule_list[i].start.day = pick_sched.start.day;
	schedule_list[i].end.day = pick_sched.end.day;
	schedule_list[i].start.hour = pick_sched.start.hour;
	schedule_list[i].end.hour = pick_sched.end.hour;
  	/* copy annotation data over */
  	for(j=0; j < MAX_COMMENT_INDEX; j++)
  		{
  		schedule_list[i].comment[j][0] = '\0';
  		strcpy(schedule_list[i].comment[j], panel_get_value(anno[j]));
  		schedule_list[i].comment[j][MAX_COMMENT-1] = '\0';
  		}
  	schedule_list[i].whofor[0] = '\0';
  	strcpy(schedule_list[i].whofor, panel_get_value(anno_who_for));
  	schedule_list[i].whosetup[0] = '\0';
  	strcpy(schedule_list[i].whosetup,panel_get_value(anno_who_setup));
		
	/* change pick pointers */
	pick_sched.start.year = 0;
	pick_sched.end.year = 0;
	pick_index = i;
		
	i++;
		
	if(i < HW_DATES)
		schedule_list[i].serno[0] = '\0';
	
	/* send E-mails */
	email_scheduled(pick_index);
		
	write_schedule();
	read_schedule();
  	}
  else
  	fprintf(stderr,"\007Out of memory for schedule_list[] table\n");
  release_lock_perm();  
  }

annotate()
  {
  int i;
  
  if(pick_index < 0)
  	return;
  
  /* get lock */
  if(get_lock_perm())
  	{
  	fprintf(stderr,"\007Can't get exclusive permision\n");
  	fprintf(stderr,"Schedule not updated\n");
  	return(-1);
  	}

  if(reread_if_needed())
  	{
	release_lock_perm();
  	return;
  	}

  /* copy annotation data over */
  for(i=0; i < MAX_COMMENT_INDEX; i++)
  	{
	schedule_list[pick_index].comment[i][MAX_COMMENT - 1] = '\0';
  	panel_set_value(anno[i], schedule_list[pick_index].comment[i]);
  	}
  panel_set_value(anno_who_for, schedule_list[pick_index].whofor);
  panel_set_value(anno_who_setup, schedule_list[pick_index].whosetup);
  
  i = (int)window_loop(anno_frame);
  if(i == 0)
  	{
	release_lock_perm();
  	return;
  	}
  
  /* copy annotation data over */
  for(i=0; i < MAX_COMMENT_INDEX; i++)
  	{
  	schedule_list[pick_index].comment[i][0] = '\0';
  	strcpy(schedule_list[pick_index].comment[i], panel_get_value(anno[i]));
  	schedule_list[pick_index].comment[i][MAX_COMMENT-1] = '\0';
  	}
  schedule_list[pick_index].whofor[0] = '\0';
  strcpy(schedule_list[pick_index].whofor, panel_get_value(anno_who_for));
  schedule_list[pick_index].whosetup[0] = '\0';
  strcpy(schedule_list[pick_index].whosetup, panel_get_value(anno_who_setup));

  email_annotated(pick_index);
  write_schedule();
  read_schedule();

  release_lock_perm();
  }

delete()
  {
  int i;
  
  if(pick_index < 0)
  	return; /* no pick */

  /* get lock */
  if(get_lock_perm())
  	{
  	fprintf(stderr,"\007Can't get exclusive permision\n");
  	fprintf(stderr,"Schedule not updated\n");
  	return(-1);
  	}

  if(reread_if_needed())
  	{
	release_lock_perm();
  	return; /* must re-do pick */
  	}

  window_bell(schedule_canvas);
  i = (int)window_loop(conf_frame);
  if(i == 0) /* cancel */
  	{
	release_lock_perm();
  	return;
  	}

  schedule_list[pick_index].deleted = -1;
  
  email_unscheduled(pick_index);

  write_schedule();
  read_schedule();
  
  pick_index = -1;
  pick_sched.start.year = 0;
  
  release_lock_perm();
  }

set_sched_read_time()
  {
  struct stat buf;
  char blurb[512];
  
  sprintf(blurb, "%s/schedule", hw_tool_home);
  stat(blurb, &buf);
  sched_last_read = buf.st_mtime;
  }

by_whofor()
  {
  int i;

  /* get lock */
  if(get_lock_perm())
  	{
  	fprintf(stderr,"\007Can't get exclusive permision\n");
  	fprintf(stderr,"Schedule not updated\n");
  	return(-1);
  	}

  if(reread_if_needed())
  	{
  	}

  release_lock_perm();

  window_bell(schedule_canvas);
  panel_set_value(by_string, passwd->pw_name);
  i = (int)window_loop(by_frame);
  if(i == 0) /* cancel */
  	{
  	return;
  	}

  pick_index = -1;
  pick_sched.start.year = 0;
  current_machine = -1;
  detail_index = -1;
  current_whosetup[0] = '\0';
  current_group = -1;
  strcpy(current_whofor, panel_get_value(by_string));
  }

by_whosetup()
  {
  int i;

  /* get lock */
  if(get_lock_perm())
  	{
  	fprintf(stderr,"\007Can't get exclusive permision\n");
  	fprintf(stderr,"Schedule not updated\n");
  	return(-1);
  	}

  if(reread_if_needed())
  	{
  	}

  release_lock_perm();

  window_bell(schedule_canvas);
  panel_set_value(by_string, passwd->pw_name);
  i = (int)window_loop(by_frame);
  if(i == 0) /* cancel */
  	{
  	return;
  	}

  pick_index = -1;
  pick_sched.start.year = 0;
  current_machine = -1;
  detail_index = -1;
  current_whofor[0] = '\0';
  current_group = -1;
  strcpy(current_whosetup, panel_get_value(by_string));
  }

make_machine_menu()
  {
  int i,j,k;
  Menu submenu;
  
  if(machine_menu != NULL)
  	{
  	menu_destroy(machine_menu);
  	}
  machine_menu = menu_create(MENU_STRINGS, 
  		"Next Month", /* sel=1 */
  		"Previous Month", /* sel = 2 */
  		"Schedule It",
  		"Annotate It",
  		"Delete It",
  		"Print Current Screen",
  		"By Person",
  		"By Who Sets Up",
  		0,
  	0);
#ifdef READ_ONLY
  { /* This block thanks to sun!sunuk!suncent!dogsbody!neilm */
  int     i;
  caddr_t item;
  for (i=3; i<6; i++)
  	{
  	item = menu_get(machine_menu, MENU_NTH_ITEM, i);
	menu_set(item, MENU_INACTIVE, TRUE, 0);
	}
  }
#endif

  for(i=0; (group[i][0] != '\0') && (i < HW_GROUPS); i++)
  	{
  	/* make submenu for that group */
  	submenu = menu_create(0);
  	menu_set(submenu,MENU_STRING_ITEM, "All", i + 9, 0);
  	
    	for(j=0; (machines_list[j].serno[0] != '\0') && ( j < HW_MACHINES); j++)
  	    for(k=0;(machines_list[j].class[k][0] != '\0')&& (k < MAX_CLASS);k++)
  		if((strcmp(group[i], machines_list[j].class[k]) == 0) && 
  			(group[i], machines_list[j].is_subitem == 0))
  			menu_set(submenu,MENU_STRING_ITEM, machines_list[j].name,
  								 j + 10000, 0);

  	/* set the pull right */
  	menu_set(machine_menu,
  		MENU_ITEM,
  			MENU_PULLRIGHT, submenu,
  			MENU_STRING, group[i], 
  			0,
  		0);
  	}
  }

apply_proc()
  {
  window_return(1);
  }

cancel_proc()
  {
  window_return(0);
  }
  
override_proc()
  {
  window_return(3);
  }

schedule_notify_proc(window, event)
  Window window;
  Event *event;
  {
  int i,j,k;
  int sel;
  int sx,sy; /* screen position */
  int tx,ty;
  /* int off_x,off_y; (used with scroll bars only) */
  int e; /* error return */
  struct hw_date dt;
  int found;
  int pday, phour, pmach;

  sx = event_x(event);
  sy = event_y(event);

  /* off_x = (int)scrollbar_get(scroll_horz, SCROLL_VIEW_START); */
  /* off_y = (int)scrollbar_get(scroll_vert, SCROLL_VIEW_START); */

  reread_if_needed();

  if(event_is_down(event) && 
  	((event_id(event) == MS_LEFT) ||
  	 (event_id(event) == MS_MIDDLE) ||
  	 (event_id(event) == MS_RIGHT)))
     {
     switch(event_id(event))
	{
	case MS_LEFT: /* designate start point */
		      /* or pick a machine on month bar chart */
	    if(current_machine >= 0)
		{
		pick_sched.start.month = current_date.month;
		pick_sched.start.year = current_date.year;
		pick_sched.end.year = 0;
		pick_index = -1;
		e = canvasxy_to_date(sx, sy, &pick_sched.start);
		if(e == -1)
			pick_sched.start.year = 0;
		/* check if pick on an existing schedule */
  		for(i = 0, found = 0; 
  			(schedule_list[i].serno[0] != '\0') && (found == 0);
  			i++)
  			{
  			if(strcmp(machines_list[current_machine].serno,
  			  			     schedule_list[i].serno) == 0)
  			  	if(date_overlaps( &pick_sched.start,
  			  		    		&pick_sched.end,
					    		&schedule_list[ i].start,
  			  		   		&schedule_list[ i].end))
  			  		    {
  			  		    found = -1;
  			  		    pick_sched.start.year = 0;
  			  		    pick_index = i;
  			  		    }
  			}
		}
	    if(pick_list_top > 0)
		{
		/* a pick on a machine in the month_bar_chart view */
		/* convert to pick list index */
		i = (event_y(event) - BAR_TITLE_HEIGHT) / 15;
		if((i >= 0) && (i < pick_list_top))
			{
			/* get time picked (day)*/
			pday = (event_x(event) - BAR_DATE_START) / BAR_DATE_GAP;
			if(pday >= 0) /* detail bar data */
				{
				/* get time picked (hour) */
				phour = (event_x(event) - BAR_DATE_START) -
							(pday * BAR_DATE_GAP);
				phour = phour * (24 / BAR_DATE_GAP);
				/* is it 8:00 or 13:00 ? */
				if(phour < 11) phour = 8;
				else phour = 13;
			
				pmach = pick_list[i]; /* in machine_list */
				/* find match given current_month, */
				/* machine (pmach), day (pday), and hour (phour)*/
				i = find_schedindex(pmach, current_date.year,
					current_date.month,
					pday, phour);
				if(i != -1)
					detail_index = i;
				else
					detail_index = -1;
				}
			else /* show calander of machine */
				{
				current_machine = pick_list[i];
				current_group = -1;
				current_whofor[0] = '\0';
				current_whosetup[0] = '\0';
				pick_list_top = 0;
				}
			}
		}
		break;

	case MS_MIDDLE: /* designate end point, or detail a line */
	    if(current_machine >= 0) /* end point */
		{
		if(pick_sched.start.year != 0)
			{
			pick_sched.end.month = current_date.month;
			pick_sched.end.year = current_date.year;
			e = canvasxy_to_date(sx, sy, &pick_sched.end);
			if(e == -1)
				pick_sched.end.year = 0;
			}
		else
			printf("\007Must pick start first\n");
		}
		break;

	case MS_RIGHT: /* pop menu */
		make_machine_menu();
	 	/* event_set_x(event, event_x(event) - off_x); */
		/* event_set_y(event, event_y(event) - off_y); */
		sel = (int)menu_show(machine_menu, window, event,0);
	 	/* event_set_x(event, event_x(event) + off_x); */
		/* event_set_y(event, event_y(event) + off_y); */

		if(sel)
			{
			if(sel < 10000)
				{
				if(sel == 1)
					{
					forward_month();
					}
				else if(sel == 2)
					{
					back_month();
					}
				else if(sel == 3) /* schedule */
					{
					schedule();
					}
				else if(sel == 4) /* annotate */
					{
					annotate();
					}
				else if(sel == 5) /* delete */
					{
					delete();
					}
				else if(sel == 6) /* Print It*/
					{
					ps_print_buf();
					}
				else if(sel == 7) /* By Person */
					{
					by_whofor();
					}
				else if(sel == 8) /* By Who Setup */
					{
					by_whosetup();
					}
				else if(sel >= 9) /* a group */
					{
					current_group = sel - 9;
					current_machine = -1;
					detail_index = -1;
					current_whofor[0] = '\0';
					current_whosetup[0] = '\0';
					}
				}
			else /* is a machine */
				{
				/*get machines_list index*/
				current_machine = sel  - 10000;
				current_group = -1;
				current_whofor[0] = '\0';
				current_whosetup[0] = '\0';
				}
			
			}
		break;
	default:
		break;
	}
	
     reread_if_needed();
     
     /* update display */
     if(current_machine >= 0)
  	     	draw_month_calendar(current_date.year,
  		     	current_date.month,
  		     	machines_list[current_machine].serno);
     else if((current_group >= 0) ||
     	     (current_whofor[0] != '\0')||
     	     (current_whosetup[0] != '\0'))
       		draw_month_barchart(current_date.year,
  	     		current_date.month);
     }
  }


make_frame_et_al(argc,argv)
  int argc;
  char *argv[];
  {
  int blurb[128];
  int i;
  
  base_frame = window_create(NULL, FRAME,
  	FRAME_LABEL, base_frame_label,
  	WIN_X, 10,
  	WIN_Y, 10,
  	FRAME_ICON, icon_create(ICON_IMAGE, &icon_pixrect, 0),
  	FRAME_ARGS, argc, argv, /* last so it can overide above settings */
  	0);
  
  schedule_canvas = window_create(base_frame, CANVAS,
  	WIN_Y, 0,
  	WIN_X, 0,
  	WIN_HEIGHT, CALENDAR_CANVAS_HEIGHT,
  	WIN_WIDTH, CALENDAR_CANVAS_WIDTH,
  	CANVAS_RETAINED, TRUE,
	WIN_EVENT_PROC, schedule_notify_proc,
	0);
  schedule_pw = canvas_pixwin(schedule_canvas);
  
  /* ie. 12 */
  cal_number_pf = pf_open("/usr/lib/fonts/fixedwidthfonts/cour.b.18");
  if(cal_number_pf == NULL)
  	cal_number_pf = pf_default();
  
  /* annotations */
  cal_notes_pf = pf_open("/usr/lib/fonts/fixedwidthfonts/cour.r.10");
  if(cal_notes_pf == NULL)
  	cal_notes_pf = pf_default();
  
  /* user id */
  cal_who_pf = pf_open("/usr/lib/fonts/fixedwidthfonts/screen.b.14");
  if(cal_who_pf == NULL)
  	cal_who_pf = pf_default();
  
  /* ie. Monday */
  cal_weekday_pf = pf_open("/usr/lib/fonts/fixedwidthfonts/screen.b.18");
  if(cal_weekday_pf == NULL)
  	cal_weekday_pf = pf_default();
  
  /* ie. March */
  cal_month_pf = pf_open("/usr/lib/fonts/fixedwidthfonts/screen.b.16");
  if(cal_month_pf == NULL)
  	cal_month_pf = pf_default();
  bar_month_pf = cal_month_pf;
  
  /* data section font */
  cal_data_pf = pf_open("/usr/lib/fonts/fixedwidthfonts/screen.r.10");
  if(cal_data_pf == NULL)
  	cal_data_pf = pf_default();

  /* ie. 12 */
  bar_day_pf = pf_open("/usr/lib/fonts/fixedwidthfonts/screen.r.7");
  if(bar_day_pf == NULL)
  	bar_day_pf = pf_default();
  
  info_canvas = window_create(base_frame, CANVAS,
  	WIN_X, 0,
  	WIN_RIGHT_OF, schedule_canvas,
  	WIN_HEIGHT, CALENDAR_CANVAS_HEIGHT,
  	WIN_WIDTH, INFO_CANVAS_WIDTH,
  	CANVAS_RETAINED, TRUE,
  	0);
  info_pw = canvas_pixwin(info_canvas);
  
  make_machine_menu();
  
  window_fit(base_frame);

  anno_frame = window_create(base_frame, FRAME,
  	WIN_X, 300,
  	WIN_Y, 300,
  	0);

  anno_panel = window_create(anno_frame, PANEL,
  	0);

  anno_apply = panel_create_item(anno_panel, PANEL_BUTTON,
  	PANEL_LABEL_IMAGE, panel_button_image(anno_panel, "Apply",0,0),
  	PANEL_NOTIFY_PROC, apply_proc, /* returns 1 */
  	PANEL_ITEM_X, ATTR_COL(1),
  	PANEL_ITEM_Y, ATTR_ROW(1),
  	0);
  anno_cancel = panel_create_item(anno_panel, PANEL_BUTTON,
  	PANEL_LABEL_IMAGE, panel_button_image(anno_panel, "Cancel",0,0),
  	PANEL_NOTIFY_PROC, cancel_proc, /* returns 0 */
  	PANEL_ITEM_X, ATTR_COL(10),
  	PANEL_ITEM_Y, ATTR_ROW(1),
  	0);
  anno_who_for = panel_create_item(anno_panel, PANEL_TEXT,
  	PANEL_LABEL_STRING, "Who Scheduled For:",
  	PANEL_VALUE, passwd->pw_name,
  	PANEL_VALUE_STORED_LENGTH, HW_WHO_SIZE - 1,
  	PANEL_VALUE_DISPLAY_LENGTH, HW_WHO_SIZE - 1,
  	PANEL_ITEM_X, ATTR_COL(1),
  	PANEL_ITEM_Y, ATTR_ROW(2),
  	0);
  anno_who_setup = panel_create_item(anno_panel, PANEL_TEXT,
  	PANEL_LABEL_STRING, "T.S.E. to set it up:",
  	PANEL_VALUE, passwd->pw_name,
  	PANEL_VALUE_STORED_LENGTH, HW_WHO_SIZE - 1,
  	PANEL_VALUE_DISPLAY_LENGTH, HW_WHO_SIZE - 1,
  	PANEL_ITEM_X, ATTR_COL(1),
  	PANEL_ITEM_Y, ATTR_ROW(3),
  	0);
  for(i=0; i<MAX_COMMENT_INDEX; i++)
  	{
  	sprintf(blurb, "Comment %d:", i+1);
  	anno[i] = panel_create_item(anno_panel, PANEL_TEXT,
  		PANEL_LABEL_STRING, blurb,
  		PANEL_VALUE, "",
  		PANEL_VALUE_STORED_LENGTH, MAX_COMMENT - 1,
  		PANEL_VALUE_DISPLAY_LENGTH, MAX_COMMENT - 1,
  		PANEL_ITEM_X, ATTR_COL(1),
  		PANEL_ITEM_Y, ATTR_ROW(i + 4),
  		0);
  	}
  window_fit(anno_panel);
  window_fit(anno_frame);
  
  conf_frame = window_create(base_frame, FRAME,
  	WIN_X, 300,
  	WIN_Y, 300,
  	0);

  conf_panel = window_create(conf_frame, PANEL,
  	0);

  conf_image_item = panel_create_item(conf_panel, PANEL_MESSAGE,
  	PANEL_LABEL_IMAGE, &conf_pr,
  	0);
  conf_txt = panel_create_item(conf_panel, PANEL_MESSAGE,
  	PANEL_LABEL_STRING, "Must delete conflicting items first.",
  	0);
  conf_txt = panel_create_item(conf_panel, PANEL_MESSAGE,
  	PANEL_LABEL_STRING, "If you hit \"Do it !\", E-mail will be sent.",
  	0);
  panel_create_item(conf_panel, PANEL_BUTTON,
  	PANEL_LABEL_IMAGE, panel_button_image(conf_panel, "Cancel",0,0),
  	PANEL_NOTIFY_PROC, cancel_proc, /* returns 0 */
  	0);
  panel_create_item(conf_panel, PANEL_BUTTON,
  	PANEL_LABEL_IMAGE, panel_button_image(conf_panel, "Do it !",0,0),
  	PANEL_NOTIFY_PROC, override_proc, /* returns 3 */
  	0);
  window_fit(conf_panel);
  window_fit(conf_frame);
  
  reread_frame = window_create(base_frame, FRAME,
  	WIN_X, 300,
  	WIN_Y, 300,
  	0);
  reread_panel = window_create(reread_frame, PANEL,
  	0);
  panel_create_item(reread_panel, PANEL_MESSAGE,
  	PANEL_LABEL_STRING, "Schedule has changed, Rereading schedule...",
  	0);
  window_fit(reread_panel);
  window_fit(reread_frame);
  
  by_frame = window_create(base_frame, FRAME,
  	WIN_X, 300,
  	WIN_Y, 300,
  	0);
  by_panel = window_create(by_frame, PANEL,
  	0);

  by_apply = panel_create_item(by_panel, PANEL_BUTTON,
  	PANEL_LABEL_IMAGE, panel_button_image(by_panel, "Apply",0,0),
  	PANEL_NOTIFY_PROC, apply_proc, /* returns 1 */
  	PANEL_ITEM_X, ATTR_COL(1),
  	PANEL_ITEM_Y, ATTR_ROW(1),
  	0);
  by_cancel = panel_create_item(by_panel, PANEL_BUTTON,
  	PANEL_LABEL_IMAGE, panel_button_image(by_panel, "Cancel",0,0),
  	PANEL_NOTIFY_PROC, cancel_proc, /* returns 0 */
  	PANEL_ITEM_X, ATTR_COL(10),
  	PANEL_ITEM_Y, ATTR_ROW(1),
  	0);
  by_string = panel_create_item(by_panel, PANEL_TEXT,
  	PANEL_LABEL_STRING, "Who scheduled for:",
  	PANEL_VALUE, passwd->pw_name,
  	PANEL_VALUE_STORED_LENGTH, HW_WHO_SIZE - 1,
  	PANEL_VALUE_DISPLAY_LENGTH, HW_WHO_SIZE - 1,
  	PANEL_ITEM_X, ATTR_COL(1),
  	PANEL_ITEM_Y, ATTR_ROW(3),
  	0);
  window_fit(by_panel);
  window_fit(by_frame);
  }

/* highlight characters */
hic(s)
  char *s;
  {
  /* standout(); */  /* look silly so turn it off for now */
  addstr(s);
  /* standend(); */
  }

#define ASCII_BAR_START 18
#define ASCII_BAR_DAY 2
ascii_bar(m, y, year, month)
  int m; /* machine index */
  int y; /* y position */
  int year, month;
  {
  int i,j;
  int starth, endh, months, monthe; /* start/end hours */
  struct hw_date td;
  int x1, x2;
  int daysinmonth;
  
  td.year = year;
  td.month = month;
  td.day = 0;
  td.hour = 1;
  months = relative_date_hours(&td);
  td.year = year;
  td.month = month;
  daysinmonth = month_length(month, year);
  td.day = daysinmonth - 1;
  td.hour = 23;
  monthe = relative_date_hours(&td);
  
  for(i=0; schedule_list[i].serno[0] != '\0'; i++)
  	{ /* scan schedule list */
  	if(strcmp(schedule_list[i].serno, machines_list[m].serno) == 0)
  		{ /* show period */
  		starth = relative_date_hours(&schedule_list[i].start);
  		endh = relative_date_hours(&schedule_list[i].end);
  		if((months < endh) && (monthe > starth))
  			{ /* plot it */
  			if(months > starth)
  				x1 = ASCII_BAR_START;
  			else
  				{
  				x1 = ASCII_BAR_START +
  					(schedule_list[i].start.day * 2);
   				if(schedule_list[i].start.hour > 12)
  						x1++;
  				}
  			if(monthe < endh)
  				x2 = 79;
  			else
  				{
  				x2 = ASCII_BAR_START +
  					(schedule_list[i].end.day * 2);
   				if(schedule_list[i].end.hour > 12)
  						x2++;
  				}
  			move(y, x1);
  			for(j=x1; j <= x2; j++)
  				addstr("*");
  			}
  		}
  	}

  }
/* returns group list index or -1 for all */
int ascii_group_barchart(g)
  {
  int i,m;
  char blurb[64];
  int lindex;
  int firstday;
  struct hw_date td;
  int days_this_month;
  
  reread_if_needed();
  clear();
  lindex = 2;
  /* Title line */
  move(0,0);
  sprintf(blurb, "Group %s", group[g]);
  addstr(blurb);
  
  /* do day letters */
  days_this_month = month_length(current_date.month, current_date.year);
  if(days_this_month == 0)
  	return;
  td.year = current_date.year;
  td.month = current_date.month;
  td.day = 0;
  td.hour = 1;
  firstday = week_day(&td);
  move(0,ASCII_BAR_START);
  for(i=0; i<days_this_month; i++, firstday++)
  	{
  	sprintf(blurb,"%s", hw_day_names[firstday % 7]);
  	blurb[1] = ' ';
  	blurb[2] = '\0';
  	addstr(blurb);
  	}
 
  move(1,ASCII_BAR_START);
  addstr("1 2 3 4 5 6 7 8 910 1 2 3 4 5 6 7 8 920 1 2 3 4 5 6 7 8 930 1");
  refresh();
  /* show each item in group */
  for(m=0; machines_list[m].serno[0] != '\0'; m++)
  	{
  	if(((strcmp(group[g],"All") == 0) || (machine_is_in_group(m,group[g]))) &&
  					(machines_list[m].is_subitem == 0))
  		{
  		move(lindex, 0);
  		addstr(machines_list[m].name);
  		move(lindex, ASCII_BAR_START - 1);
  		addstr("|");
  		clrtoeol();
  		ascii_bar(m, lindex, current_date.year, current_date.month);
  		
   		lindex++;
 		if(lindex > 22)
  			{
  			lindex = 0;
 			move(23,0);
  			addstr("Type C <return>:");
  			refresh();
  			scanf("%s", blurb);
			clear();
  			}
  		refresh();
  		}
  	}
  move(23,0);
  addstr("Type C <return>:");
  refresh();
  scanf("%s", blurb);
  }
  
int ascii_bars_whofor(w, m, y, year, month)
  char *w; /* who for */
  int m; /* machines list index */
  int y; /* y position */
  int year, month;
  {
  int i,j;
  int starth, endh, months, monthe; /* start/end hours */
  struct hw_date td;
  int x1, x2;
  int daysinmonth;
  int drawn;
  
  drawn = 0;
  td.year = year;
  td.month = month;
  td.day = 0;
  td.hour = 1;
  months = relative_date_hours(&td);
  td.year = year;
  td.month = month;
  daysinmonth = month_length(month, year);
  td.day = daysinmonth - 1;
  td.hour = 23;
  monthe = relative_date_hours(&td);
  
  for(i=0; schedule_list[i].serno[0] != '\0'; i++)
  	{ /* scan schedule list */
  	if((strcmp(schedule_list[i].whofor, w) == 0) &&
  	   (strcmp(machines_list[m].serno, schedule_list[i].serno) == 0))
  		{ /* show period */
  		starth = relative_date_hours(&schedule_list[i].start);
  		endh = relative_date_hours(&schedule_list[i].end);
  		if((months < endh) && (monthe > starth))
  			{ /* plot it */
  			if(months > starth)
  				x1 = ASCII_BAR_START;
  			else
  				{
  				x1 = ASCII_BAR_START +
  					(schedule_list[i].start.day * 2);
   				if(schedule_list[i].start.hour > 12)
  						x1++;
  				}
  			if(monthe < endh)
  				x2 = 79;
  			else
  				{
  				x2 = ASCII_BAR_START +
  					(schedule_list[i].end.day * 2);
   				if(schedule_list[i].end.hour > 12)
  						x2++;
  				}
  			move(y, x1);
  			for(j=x1; j <= x2; j++)
  				addstr("*");
  			drawn = -1;
  			}
  		}
  	}
  return(drawn);
  }

int ascii_bars_whosetup(w, m, y, year, month)
  char *w; /* who setup */
  int m; /* machines list index */
  int y; /* y position */
  int year, month;
  {
  int i,j;
  int starth, endh, months, monthe; /* start/end hours */
  struct hw_date td;
  int x1, x2;
  int daysinmonth;
  int drawn;
  
  drawn = 0;
  td.year = year;
  td.month = month;
  td.day = 0;
  td.hour = 1;
  months = relative_date_hours(&td);
  td.year = year;
  td.month = month;
  daysinmonth = month_length(month, year);
  td.day = daysinmonth - 1;
  td.hour = 23;
  monthe = relative_date_hours(&td);
  
  for(i=0; schedule_list[i].serno[0] != '\0'; i++)
  	{ /* scan schedule list */
  	if((strcmp(schedule_list[i].whosetup, w) == 0) &&
  	   (strcmp(machines_list[m].serno, schedule_list[i].serno) == 0))
  		{ /* show period */
  		starth = relative_date_hours(&schedule_list[i].start);
  		endh = relative_date_hours(&schedule_list[i].end);
  		if((months < endh) && (monthe > starth))
  			{ /* plot it */
  			if(months > starth)
  				x1 = ASCII_BAR_START;
  			else
  				{
  				x1 = ASCII_BAR_START +
  					(schedule_list[i].start.day * 2);
   				if(schedule_list[i].start.hour > 12)
  						x1++;
  				}
  			if(monthe < endh)
  				x2 = 79;
  			else
  				{
  				x2 = ASCII_BAR_START +
  					(schedule_list[i].end.day * 2);
   				if(schedule_list[i].end.hour > 12)
  						x2++;
  				}
  			move(y, x1);
  			for(j=x1; j <= x2; j++)
  				addstr("*");
  			drawn = -1;
  			}
  		}
  	}
  return(drawn);
  }

int ascii_by_whofor()
  {
  char blurb[64];
  int days_this_month;
  int firstday;
  int i, m, lindex;
  struct hw_date td;
  
  lindex = 2;
  /* get lock */
  if(get_lock_perm())
  	{
  	fprintf(stderr,"\007Can't get exclusive permision\n");
  	fprintf(stderr,"Schedule not updated\n");
  	return(-1);
  	}

  if(reread_if_needed())
  	{
  	}

  release_lock_perm();

  clear();
  move(2,0);
  addstr("     WHO FOR\n");
  addstr("Enter who for:");
  refresh();
  scanf("%s", current_whofor);
  pick_index = -1;
  pick_sched.start.year = 0;
  current_machine = -1;
  detail_index = -1;
  current_whosetup[0] = '\0';
  current_group = -1;

  /* Title line */
  move(0,0);
  sprintf(blurb, "For %s", current_whofor);
  addstr(blurb);
  
  /* do day letters */
  days_this_month = month_length(current_date.month, current_date.year);
  if(days_this_month == 0)
  	return;
  td.year = current_date.year;
  td.month = current_date.month;
  td.day = 0;
  td.hour = 1;
  firstday = week_day(&td);
  move(0,ASCII_BAR_START);
  for(i=0; i<days_this_month; i++, firstday++)
  	{
  	sprintf(blurb,"%s", hw_day_names[firstday % 7]);
  	blurb[1] = ' ';
  	blurb[2] = '\0';
  	addstr(blurb);
  	}
 
  move(1,ASCII_BAR_START);
  addstr("1 2 3 4 5 6 7 8 910 1 2 3 4 5 6 7 8 920 1 2 3 4 5 6 7 8 930 1");
  refresh();
  /* show each item in group */
  for(m=0; machines_list[m].serno[0] != '\0'; m++)
  	{
  	if((machines_list[m].is_subitem == 0) &&
  		ascii_bars_whofor(current_whofor, m, lindex, current_date.year, 
  							current_date.month))
  		{
  		move(lindex, 0);
  		addstr(machines_list[m].name);
   		lindex++;
 		if(lindex > 22)
  			{
  			lindex = 0;
 			move(23,0);
  			addstr("Type C <return>:");
  			refresh();
  			scanf("%s", blurb);
			clear();
  			}
  		refresh();
  		}
  	}
  move(23,0);
  addstr("Type C <return>:");
  refresh();
  scanf("%s", blurb);
  }
  
int ascii_by_whosetup()
  {
  char blurb[64];
  int days_this_month;
  int firstday;
  int i, m, lindex;
  struct hw_date td;
  
  lindex = 2;
  /* get lock */
  if(get_lock_perm())
  	{
  	fprintf(stderr,"\007Can't get exclusive permision\n");
  	fprintf(stderr,"Schedule not updated\n");
  	return(-1);
  	}

  if(reread_if_needed())
  	{
  	}

  release_lock_perm();

  clear();
  move(2,0);
  addstr("     By WHO SETS UP\n");
  addstr("Enter who sets up:");
  refresh();
  scanf("%s", current_whosetup);
  pick_index = -1;
  pick_sched.start.year = 0;
  current_machine = -1;
  detail_index = -1;
  current_whofor[0] = '\0';
  current_group = -1;

  /* Title line */
  move(0,0);
  sprintf(blurb, "For %s", current_whosetup);
  addstr(blurb);
  
  /* do day letters */
  days_this_month = month_length(current_date.month, current_date.year);
  if(days_this_month == 0)
  	return;
  td.year = current_date.year;
  td.month = current_date.month;
  td.day = 0;
  td.hour = 1;
  firstday = week_day(&td);
  move(0,ASCII_BAR_START);
  for(i=0; i<days_this_month; i++, firstday++)
  	{
  	sprintf(blurb,"%s", hw_day_names[firstday % 7]);
  	blurb[1] = ' ';
  	blurb[2] = '\0';
  	addstr(blurb);
  	}
 
  move(1,ASCII_BAR_START);
  addstr("1 2 3 4 5 6 7 8 910 1 2 3 4 5 6 7 8 920 1 2 3 4 5 6 7 8 930 1");
  refresh();
  /* show each item in group */
  for(m=0; machines_list[m].serno[0] != '\0'; m++)
  	{
  	if((machines_list[m].is_subitem == 0) &&
  		ascii_bars_whosetup(current_whosetup, m,lindex,current_date.year, 
  							current_date.month))
  		{
  		move(lindex, 0);
  		addstr(machines_list[m].name);
   		lindex++;
 		if(lindex > 22)
  			{
  			lindex = 0;
 			move(23,0);
  			addstr("Type C <return>:");
  			refresh();
  			scanf("%s", blurb);
			clear();
  			}
  		refresh();
  		}
  	}
  move(23,0);
  addstr("Type C <return>:");
  refresh();
  scanf("%s", blurb);
  }
  
/* returns same as the sunview menu */
int ascii_show_main_menu()
  {
  int i,j,k;
  char blurb[128];
  char tblurb[64];
  char sel[64];
  int seln;
  int ccount;
  
  strcpy(sel,"zz");
  while(strcmp(sel,"Q") != 0)
  	{
  	reread_if_needed();
  	clear();
  	move(0,0);
  	
  	strcpy(tblurb, hw_month_names[current_date.month]);
  	tblurb[3] = '\0'; /* truncate to abbreviation */
  	sprintf(blurb, "%s,%d | ", tblurb, current_date.year);
  	addstr(blurb);
  	hic("N"); addstr("ext ");
  	hic("P"); addstr("revious ");
  	addstr("p"); hic("R"); addstr("int ");
  	addstr("who"); hic("F"); addstr("or ");
  	addstr("who"); hic("S"); addstr("etup ");
  	hic("Q"); addstr("uit\n");
  	ccount = 0;
  	for(i=0; (group[i][0] != '\0') && (i < HW_GROUPS); i++)
  		{
  		sprintf(blurb,"[%d/%s] ", i + 1, group[i]); /* to find length */
  		if((ccount +  strlen(blurb)) > 80)
  			{
  			ccount = 0;
  			addstr("\n");
  			}
		ccount = ccount + strlen(blurb);
  		addstr("[");
		/* standout(); */
		sprintf(blurb,"%d", i + 1); addstr(blurb);
		/* standend(); */
		
  		sprintf(blurb,"/%s] ", group[i]); addstr(blurb);
		}
  	addstr("\nEnter selection and <return>: ");
  	refresh();
  	scanf("%s", sel);
  	/* conver to upper if a character */
  	if(islower(sel[0]))
  		sel[0] = toupper(sel[0]);
  	if(sel[0] == 'N')
  		forward_month();
  	else if(sel[0] == 'P')
  		back_month();
	else if(sel[0] == 'R') /* Print It*/
		ps_print_buf();
	else if(sel[0] == 'F')
		ascii_by_whofor();
	else if(sel[0] == 'S')
		ascii_by_whosetup();
	else if(sel[0] == 'Q')
		exit(0);
	else if(isdigit(sel[0])) /* is a group sub menu */
		{
		sscanf(sel,"%d", &seln);
		seln = seln - 1;
		current_group = seln;
		current_machine = -1;
		detail_index = -1;
		current_whofor[0] = '\0';
		current_whosetup[0] = '\0';
		ascii_group_barchart(seln);
		}
	else
		fprintf(stderr, "\007\n");
	}
  }

/* do menus and such */
ascii_loop()
  {
  /* start curses */
  initscr();
  
  while(-1)
  	{
 	 /* show menu */
  	ascii_show_main_menu();
  	}
  }

main(argc,argv)
  int argc;
  char *argv[];
  {
  char *s;
  struct tm *local_time;
  int i;
  
  enable_post_script = 1; /* do it */
  ascii_mode = 0;
  lastpf = NULL;
  
  s = (char *)getenv("WINDOW_PARENT");
  if(s == NULL)
  	{
  	printf("ascii terminal mode\n");
  	ascii_mode = 1;
  	}
  else
  	{
  	ascii_mode = 0;
  	}

  /* for debug of ascii mode */
  if(strcmp(argv[0], "hwtool.d") == 0)
  	ascii_mode = 1;
  
  printf(VERSION);
  cmdname = argv[0];
  
  s = (char *)getenv("HWHOME");
  if(s == NULL)
  	{
  	fprintf(stderr,
  		"\007Must setenv HWHOME <the home directory for %s>\n",
  		cmdname);
  	exit(1);
  	}
  strcpy(hw_tool_home,s);
  
  s = (char *)getenv("HWLPR");
  if(s == NULL)
  	{
  	fprintf(stderr,
  	"\007Must setenv HWLPR <the lpr string used to print\n",
  		cmdname);
  	fprintf(stderr,"usualy: setenv HWLPR lpr\n");
  	exit(1);
  	}
  strcpy(hw_tool_print,s);
  
  passwd = getpwuid(getuid());
  sprintf(base_frame_label, "Hardware Tool     user %s (%s)",
  	passwd->pw_name, passwd->pw_gecos);
  
  /* clear pick point */
  pick_sched.start.year = 0;
  pick_sched.end.year = 0;
  pick_index = -1;
  
  /* clear tables */
  machines_list[0].serno[0] = '\0';
  schedule_list[0].serno[0] = '\0';
  group[0][0] = '\0';
  detail_index = current_group = current_machine = -1;
  current_whofor[0] = current_whosetup[0] = '\0';
  
  read_machines();
  read_policy();
  read_schedule();
  
  if(ascii_mode == 0)
  	make_frame_et_al(argc,argv);
  
  i = time((time_t *)0);
  local_time = localtime(&i);
  current_date.month = local_time->tm_mon;
  current_date.year = local_time->tm_year + 1900;
  
  /* Init graphics */
  pw_clear_ps(schedule_pw);
  pw_clear_ps(info_pw);
  pw_text_ps(schedule_pw, 200, 200, PIX_SRC, cal_month_pf,
  	"Use the right mouse button to select a function.");
  if(ascii_mode == 0)
  	window_main_loop(base_frame);
  else
  	ascii_loop();
  }

