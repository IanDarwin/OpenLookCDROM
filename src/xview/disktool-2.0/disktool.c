/*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
 disktool.c Copyright(c)1993,1994 Shaun M. Finn

 Xview tool to monitor disk space on user selected partitions.

Questions, bug reports, modifications, etc can be sent to the following
address:

	sfinn@astro.ge.com

ALL RIGHTS ARE RESERVED, EXCEPT FOR THE FOLLOWING:

ANYONE CAN MODIFY `disktool' PROVIDED THEY CONTACT THE AUTHOR FIRST AND
REDISTRIBUTE THE MODIFIED VERSION FREE OF CHARGE AND DO NOT OMIT THIS
HEADER FROM THE DISTRIBUTION.

THERE IS NO WARRANTY FOR `disktool'.  IT IS PROVIDED AS IS.  I HAVE NO 
RESPONSIBILITY FOR ANY PROBLEMS OR DAMAGES CAUSED BY USE OF `disktool'.

 History:
 Date     Programmer   Rev  Description
 07/07/93 Shaun Finn   1.0  Initial coding.
 11/04/93 Shaun Finn   1.1  Changed the "OK" color to GREEN (was WHITE)
                            Added a timestamp for the last time disks were
			    polled to FRAME_LEFT_FOOTER.
			    (suggestions from <rohit@synoptics.com>)
			    Changed the ICONs to display the hostname of
			    the machine running disktool.
			    (suggestion from <rn@big.att.com>)
 04/15/94 Shaun Finn   1.2  Added check for existance of specified disks.
 05/10/94 Shaun Finn   1.3  Restructured program to use statfs() instead of
                            a combination of stat() and ustat() calls to
                            gather filesystem info. Removed "-range" cmdline
                            option and now do auto-calculate of range based
                            on filesystem size. Re-worked threshold cmdline
			    option to allow flexibility per gauge or 
			    consecutive range of gauges. Added "-spacing"
			    cmdline option for vertical spacing of gauges.
			    Also expanded filesystem "." to the actual path
			    for readability. Added "-lineup" cmdline option
			    for the # of gauges before starting a new row
			    or column. Added ability to save cmdline options
			    with a "SAVE_WORKSPACE".
 05/18/94 Shaun Finn   1.3a Added Solaris 2.x & HP-UX support. Added debug flag.
 05/25/94 Shaun Finn   1.3b Added Display of Thresholds (Suggestion from all
                            over the world!)
			    Added Auto-calc of spacing (Suggestion from John
			    Wingenbach w6i@dsunx1.dsrd.ornl.gov, expanded 
			    concept and some code thanks to Dan Grady 
			    dgrady@astro.ge.com).
			    kbyte_item's are now (pseudo-)centered in
			    vertical layout mode.
			    New items were added to the properties window to
			    allow adjusting of hort/vert mode, spacing and 
			    max gauges per row/column.
 08/11/94 Shaun Finn   1.3c Rewrote SOLARIS2 support after several suggestions.
 08/29/94 Shaun Finn   2.0  Added forced poll with middle mouse click.
 (Notice a pattern here?)   Added delta since last poll and cmdline switch.
			    Added keyboard accelerators. Added "-cmd" cmdline
			    option. Added check for MAX_GAUGES exceeded.
			    Added parsing of "KB" thresholds. Added ability to
			    add and delete filesystems "on the fly". Set a
			    minimum panel width so Timestamp is always 
			    readable. Added ability for multiple signals of
			    a critical condition every n polls and cmdline 
			    option. Set default spacing to zero to default to
			    auto-spacing. Optimized a number of routines.
			    Bolded the PANEL_TEXT for the filesystem names.
			    Added "-used" cmdline argument to reverse display
			    of gauges to show disk space used instead of the
			    default, available.

+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
#include "common.h"
/* ==========================================================================*/
void get_date(datestring)
char *datestring;
{
   struct tm *ptr;
   int month, day, year, hour, min, sec;
   time_t time_val;
   char smonth[15];

   time_val = time(&time_val);
   ptr    = localtime(&time_val);
   month = (ptr -> tm_mon) + 1;
   day   =  ptr -> tm_mday;
   year  =  ptr -> tm_year;
   hour  =  ptr -> tm_hour;
   min   =  ptr -> tm_min;
   sec   =  ptr -> tm_sec;
   if (min < 10)
   {
      if (sec < 10)
      sprintf(datestring,"%d:0%d:0%d",hour,min,sec);
      else
      sprintf(datestring,"%d:0%d:%d",hour,min,sec);
   }
   else
   {
      if (sec < 10)
      sprintf(datestring,"%d:%d:0%d",hour,min,sec);
      else
      sprintf(datestring,"%d:%d:%d",hour,min,sec);
   }
}

void get_date();

/* ==========================================================================*/
void timestamp()
{
   char     date[10];

   get_date(date);
   sprintf(poll_str, "Last Poll: %s", date);
   xv_set(frame, FRAME_LEFT_FOOTER, poll_str, NULL);
}

/* ==========================================================================*/
void build_fswidgets(fs)
char   fs[MAXPATHLEN];
{
   void    get_popup();
   part_item[fscnt] = xv_create(panel, PANEL_MESSAGE,
				PANEL_LABEL_BOLD,      TRUE,
				PANEL_ITEM_COLOR,
				CMS_CONTROL_COLORS + BLACK,
				PANEL_LABEL_STRING, fs,
				NULL);
   gauge[fscnt] = xv_create(panel, PANEL_GAUGE,
			    PANEL_ITEM_COLOR,
			    CMS_CONTROL_COLORS + OKCOLOR,
			    PANEL_MIN_VALUE,   0,
			    PANEL_MAX_VALUE,   100,
			    PANEL_GAUGE_WIDTH, 100,
			    PANEL_EVENT_PROC,  get_popup,
			    PANEL_TICKS,       5,
			    PANEL_VALUE,       0,
			    NULL);
   kbyte_item[fscnt] = xv_create(panel, PANEL_MESSAGE,
				 PANEL_ITEM_COLOR,
				 CMS_CONTROL_COLORS + BLACK,
				 PANEL_LABEL_STRING, "MB",
				 NULL);
   if(delta)
   delta_item[fscnt] = xv_create(panel, PANEL_MESSAGE,
				 PANEL_ITEM_COLOR,
				 CMS_CONTROL_COLORS + BLACK,
				 PANEL_LABEL_STRING, "(0)",
				 NULL);
   fscnt++;
}

/* ==========================================================================*/
Notify_value
get_values()
{
   int      n;
   int      critical;
   long     dnum;
   long     num;
#ifdef SOLARIS2
   struct   statvfs buf;
#else
   struct   statfs buf;
#endif
   char     sbuf[24];
   char     sysbuf[256];

   critical = FALSE;
   for(n = 0 ; n < fscnt ; ++n)
   {
#ifdef SOLARIS2
      statvfs(xv_get(part_item[n],PANEL_LABEL_STRING), &buf);
      num = (buf.f_bavail*buf.f_frsize/1024)/divisor[n];
#else
      statfs(xv_get(part_item[n],PANEL_LABEL_STRING), &buf);
      num = (buf.f_bavail*buf.f_bsize/1024)/divisor[n];
#endif
      if(delta)
      {
	 dnum = num - (long)atoi(xv_get(kbyte_item[n], PANEL_LABEL_STRING));
	 sprintf(sbuf, "(%ld)", abs(dnum));
	 if (dnum < 0)
	 xv_set(delta_item[n], PANEL_LABEL_STRING, sbuf, PANEL_ITEM_COLOR,
		CMS_CONTROL_COLORS + CRITCOL, NULL);
	 else if(dnum == 0)
	 xv_set(delta_item[n], PANEL_LABEL_STRING, sbuf, PANEL_ITEM_COLOR,
		CMS_CONTROL_COLORS + BLACK, NULL);
	 else if(dnum > 0)
	 xv_set(delta_item[n], PANEL_LABEL_STRING, sbuf, PANEL_ITEM_COLOR,
		CMS_CONTROL_COLORS + OKCOLOR, NULL);
      }
      if(debug) printf("Avail = %ld  Delta = %ld  Delta String = %s\n", num, dnum, sbuf);
#ifdef SOLARIS2
      xv_set(gauge[n], PANEL_MAX_VALUE, 
	     (buf.f_blocks*buf.f_frsize/1024)/divisor[n], NULL);
#else
      xv_set(gauge[n], PANEL_MAX_VALUE, 
	     (buf.f_blocks*buf.f_bsize/1024)/divisor[n], NULL);
#endif
      if(used)
#ifdef SOLARIS2
      xv_set(gauge[n], PANEL_VALUE, 
	     ((buf.f_blocks-buf.f_bfree)*buf.f_frsize/1024)/divisor[n], NULL);
#else
      xv_set(gauge[n], PANEL_VALUE, 
	     ((buf.f_blocks-buf.f_bfree)*buf.f_bsize/1024)/divisor[n], NULL);
#endif
      else
      xv_set(gauge[n], PANEL_VALUE, num, NULL);
      sprintf(sbuf, "%d %cB (%d)", num, (divisor[n] == 1000)?'M':'K', thresh[n]);
      xv_set(kbyte_item[n], PANEL_LABEL_STRING, sbuf, NULL);
      if (num <= thresh[n])
      {
	 critical = TRUE;
	 critcnt[n]++;
	 if(debug) printf("Dirname = %s  CritCnt = %d\n", 
			  xv_get(part_item[n], PANEL_LABEL_STRING), critcnt[n]);
	 if((critcnt[n] >= xv_get(rep_item, PANEL_VALUE) && xv_get(rep_item, PANEL_VALUE)) || 
	    (critcnt[n] == 1 && 
	     (xv_get(gauge[n], PANEL_ITEM_COLOR) != (CMS_CONTROL_COLORS + CRITCOL))))
	 {
	    xv_set(gauge[n], PANEL_ITEM_COLOR, CMS_CONTROL_COLORS + CRITCOL, NULL);
	    if (xv_get(pop_check, PANEL_VALUE))
	    xv_set(frame, FRAME_CLOSED, FALSE, NULL);
	    if (xv_get(cmd_item, PANEL_VALUE))
	    {
	       sprintf(sysbuf, "DISK=%s;BYTES='%s';%s",
		       xv_get(part_item[n],PANEL_LABEL_STRING),
		       sbuf, xv_get(cmd_item, PANEL_VALUE));
	       system(sysbuf);
	    }
	    if(critcnt[n] >= xv_get(rep_item, PANEL_VALUE)) critcnt[n] = 0;
	 }
      }
      else
      {
	 critcnt[n] = 0;
	 if (xv_get(gauge[n], PANEL_ITEM_COLOR) != (CMS_CONTROL_COLORS + OKCOLOR))
	 {
	    xv_set(gauge[n], PANEL_ITEM_COLOR, CMS_CONTROL_COLORS + OKCOLOR, NULL);
	 }	    
      }
   }
   if (critical && xv_get(icon, ICON_IMAGE) == image_ok)
   xv_set(icon, ICON_IMAGE, image_crit, NULL);
   if (!critical && xv_get(icon, ICON_IMAGE) == image_crit)
   xv_set(icon, ICON_IMAGE, image_ok, NULL);
   xv_set(disk_item, PANEL_ITEM_COLOR,
	  xv_get(gauge[curr_item], PANEL_ITEM_COLOR), NULL);
   timestamp();
   if (debug) printf("\n");
   return NOTIFY_DONE;
}

/* ========================================================================*/
void set_prop_values()
{
   xv_set(disk_item, PANEL_VALUE,
	  xv_get(part_item[curr_item], PANEL_LABEL_STRING), NULL);
   xv_set(disk_item, PANEL_ITEM_COLOR,
	  xv_get(gauge[curr_item], PANEL_ITEM_COLOR), NULL);
   if (divisor[curr_item] == 1000)
   {
      xv_set(units_item, PANEL_VALUE, 0, NULL);
      xv_set(thresh_item, PANEL_VALUE, thresh[curr_item], NULL);
      xv_set(thresh_item, PANEL_MAX_VALUE, 3000, NULL);
   }
   else
   {
      xv_set(units_item, PANEL_VALUE, 1, NULL);
      xv_set(thresh_item, PANEL_MAX_VALUE, 3000000, NULL);
      xv_set(thresh_item, PANEL_VALUE, thresh[curr_item], NULL);
   }
}

/* ========================================================================*/
void
get_popup(item, event)
Panel_item item;
Event      *event;
{
   int    j;

   if (event_action(event) == ACTION_MENU && event_is_down(event))
   {
      for(j=0; j < fscnt; ++j)
      {
	 if (gauge[j] == item) curr_item = j;
      }
      set_prop_values();
      xv_set(prop_frame, XV_SHOW, TRUE, NULL);
   }
   else if(event_action(event) == ACTION_ADJUST && event_is_down(event))
   get_values();          /* Middle mouse press to force a filesystem poll */
}

/* ========================================================================*/
void
hide_prop()
{
   xv_set(prop_frame, XV_SHOW, FALSE, NULL);
}

/* ========================================================================*/
int
generic_sel(item, event)
Panel_item item;
Event  *event;
{
   return;
}

/* ========================================================================*/
int
inc_sel(item, event)
Panel_item item;
Event  *event;
{
   Notify_value  get_values();

   timer.it_value.tv_sec = xv_get(inc_item, PANEL_VALUE);
   timer.it_interval.tv_sec = xv_get(inc_item, PANEL_VALUE);
   notify_set_itimer_func(frame, get_values,
			  ITIMER_REAL, &timer, NULL);
   return;
}
/* ========================================================================*/
int
units_sel(item, value, event)
Panel_item item;
int    value;
Event  *event;
{
   if (value && xv_get(thresh_item, PANEL_MAX_VALUE) == 3000)
   {
      xv_set(thresh_item, PANEL_MAX_VALUE, 3000000, NULL);
      xv_set(thresh_item, PANEL_VALUE, 
	     xv_get(thresh_item, PANEL_VALUE)*1000, NULL);
   }
   if (!value && xv_get(thresh_item, PANEL_MAX_VALUE) == 3000000)
   {
      xv_set(thresh_item, PANEL_VALUE, 
	     xv_get(thresh_item, PANEL_VALUE)/1000, NULL);
      xv_set(thresh_item, PANEL_MAX_VALUE, 3000, NULL);
   }
   return;
}

/* ========================================================================*/
void
reposition_widgets()
{
   int  hort, x, gcnt;
   int  col = 5, row = 5;
   int  width = 140;

   xv_set(panel, XV_SHOW, FALSE, NULL);
   hort = (int)xv_get(orient_item, PANEL_VALUE);
   for(x=0,gcnt=1;x < fscnt;++x,++gcnt)
   {
      xv_set(gauge[x], PANEL_DIRECTION, 
	     (hort)?PANEL_VERTICAL:PANEL_HORIZONTAL, NULL);
      xv_set(part_item[x], XV_X, col, XV_Y, row, NULL);
      xv_set(gauge[x], XV_X, col, XV_Y, row+15, NULL);
      if (hort)
      {
	 xv_set(kbyte_item[x], XV_X, col, XV_Y, row+140, NULL);
	 if (delta)
	 xv_set(delta_item[x], XV_X, col, XV_Y, row+152, NULL);
      }
      else
      {
	 xv_set(kbyte_item[x], XV_X, 
		col+56-(xv_get(kbyte_item[x], PANEL_LABEL_WIDTH)/2),
		XV_Y, row+41, NULL);
	 if(delta)
	 xv_set(delta_item[x], XV_X, 
		col+56-(xv_get(delta_item[x], PANEL_LABEL_WIDTH)/2),
		XV_Y, row+53, NULL);
      }
      if(hort)
      {
	 if((int)xv_get(spacing_item, PANEL_VALUE) >= MINSPACING) /* no autocalc */
	 col += xv_get(spacing_item, PANEL_VALUE);
	 else
	 col += (int)max(xv_get(kbyte_item[x], PANEL_LABEL_WIDTH), 
			 xv_get(part_item[x], PANEL_LABEL_WIDTH)) + 10;
      }
      else     /* vertical */
      {
	 if((int)xv_get(spacing_item, PANEL_VALUE) >= MINSPACING) /* no autocalc */
	 row += xv_get(spacing_item, PANEL_VALUE);
	 else
	 if(delta)
	 row += 70;
	 else
	 row += 60;
	 width = max(width,max(xv_get(kbyte_item[x], PANEL_LABEL_WIDTH), 
			       xv_get(part_item[x], PANEL_LABEL_WIDTH))+10);
      }
      if (xv_get(roc_item, PANEL_VALUE) == gcnt)
      {
	 if (hort)
	 {
	    if(delta)
	    row += 185;
	    else
	    row += 175;
	    col = 5;
	 }
	 else
	 {
	    row = 5;
	    col += width;
	    width = 140;
	 }
	 gcnt = 0;
      }
   }	    
   xv_set(panel, XV_SHOW, TRUE, NULL);
   window_fit(panel);
   if(xv_get(panel, XV_WIDTH) < 135) xv_set(panel, XV_WIDTH, 135, NULL);
   window_fit(frame);
}

/* ========================================================================*/
int
set_item()
{
   if (xv_get(units_item, PANEL_VALUE))
   divisor[curr_item] = 1;
   else
   divisor[curr_item] = 1000;
   thresh[curr_item] = xv_get(thresh_item, PANEL_VALUE);
   get_values();
   reposition_widgets();
   return;
}

/* ========================================================================*/
int
set_all_items()
{
   int   n;

   for(n = 0 ; n < fscnt ; ++n)
   {
      if (xv_get(units_item, PANEL_VALUE))
      divisor[n] = 1;
      else
      divisor[n] = 1000;
      thresh[n] = xv_get(thresh_item, PANEL_VALUE);
   }
   get_values();
   reposition_widgets();
   return;
}

/* ========================================================================*/
int
pop_sel(item, value, event)
Panel_item item;
int    value;
Event  *event;
{
   return;
}

/* ========================================================================*/
void refresh_delta()    /* A necessary evil to clean up deltas */
{
   get_values();
   get_values();
}

/* ========================================================================*/
void add_fs()
{

#ifdef SOLARIS2
   struct statvfs buf;
#else
   struct statfs buf;
#endif

   if(fscnt < MAX_GAUGES)
   {
#ifdef SOLARIS2
      if(!statvfs(xv_get(disk_item, PANEL_VALUE), &buf))
#else
      if(!statfs(xv_get(disk_item, PANEL_VALUE), &buf))
#endif
      {
	 thresh[fscnt] = xv_get(thresh_item, PANEL_VALUE);
	 if(xv_get(units_item, PANEL_VALUE))
	 divisor[fscnt] = 1;
	 else
	 divisor[fscnt] = 1000;
	 build_fswidgets((char *)xv_get(disk_item, PANEL_VALUE));
	 if(delta)
	 refresh_delta();
	 else
	 get_values();
	 reposition_widgets();
      }
      else
      {
	 fprintf(stderr, "ERROR: Filesystem %s does not exist or is not searchable.\n",
		 (char *)xv_get(disk_item, PANEL_VALUE));
      }
   }
   else
   {
      fprintf(stderr, "ERROR: Maximum number of Filesystems(%d) exceeded\n",
	      MAX_GAUGES);
   }
}

/* ========================================================================*/
void del_fs()
{
   int   n;

   if(fscnt == 1)
   {
      if (notice_prompt(panel, NULL,
			NOTICE_MESSAGE_STRINGS,
			"Do you wish to Quit",
			"disktool?",
			NULL,
			NOTICE_BUTTON, "Yes", 1,
			NOTICE_BUTTON, "No", 0,
			NULL))
      exit(0);
      else
      return;
   }
   else
   {
      xv_destroy(gauge[curr_item]);
      xv_destroy(part_item[curr_item]);
      xv_destroy(kbyte_item[curr_item]);
      if(delta) xv_destroy(delta_item[curr_item]);
      
      for(n = curr_item + 1 ; n < fscnt ; ++n)
      {
	 divisor[n-1] = divisor[n];
	 thresh[n-1] = thresh[n];
	 gauge[n-1] = gauge[n];
	 part_item[n-1] = part_item[n];
	 kbyte_item[n-1] = kbyte_item[n];
	 if(delta) delta_item[n-1] = delta_item[n];
      }
      
      fscnt--;
      if(curr_item >= fscnt && curr_item != 0) curr_item--;
      set_prop_values();
      get_values();
      reposition_widgets();
   }
}

/* ========================================================================*/
void get_events(dpanel, event)
Panel      dpanel;
Event      *event;
{
   char    sbuf[24];

   if(debug) printf("Panel event...");
   if(event_is_ascii(event) && event_is_down(event))
   {
      switch(event_action(event))
      {
 case '+':
	 if(debug) printf("Keypress = %c\n", event_action(event));
	 xv_set(inc_item, PANEL_VALUE, xv_get(inc_item, PANEL_VALUE)+10, NULL);
	 timer.it_value.tv_sec = xv_get(inc_item, PANEL_VALUE);
	 timer.it_interval.tv_sec = xv_get(inc_item, PANEL_VALUE);
	 notify_set_itimer_func(frame, get_values,
				ITIMER_REAL, &timer, NULL);
	 sprintf(sbuf, "Poll rate: %d", xv_get(inc_item, PANEL_VALUE));
	 xv_set(frame, FRAME_LEFT_FOOTER, sbuf, NULL);
	 break;
 case '-':
	 if(debug) printf("Keypress = %c\n", event_action(event));
	 xv_set(inc_item, PANEL_VALUE, xv_get(inc_item, PANEL_VALUE)-10, NULL);
	 timer.it_value.tv_sec = xv_get(inc_item, PANEL_VALUE);
	 timer.it_interval.tv_sec = xv_get(inc_item, PANEL_VALUE);
	 notify_set_itimer_func(frame, get_values,
				ITIMER_REAL, &timer, NULL);
	 sprintf(sbuf, "Poll rate: %d", xv_get(inc_item, PANEL_VALUE));
	 xv_set(frame, FRAME_LEFT_FOOTER, sbuf, NULL);
	 break;
 case '=':
	 if(debug) printf("Keypress = %c\n", event_action(event));
	 sprintf(sbuf, "Poll rate: %d", xv_get(inc_item, PANEL_VALUE));
	 xv_set(frame, FRAME_LEFT_FOOTER, sbuf, NULL);
	 sleep(1);
	 xv_set(frame, FRAME_LEFT_FOOTER, poll_str, NULL);
	 break;
 case 'h':
	 if(debug) printf("Keypress = %c\n", event_action(event));
         xv_set(orient_item, PANEL_VALUE, 1, NULL);
	 reposition_widgets();
	 break;
 case 'p':
	 get_values();
	 break;
 case 'v':
	 if(debug) printf("Keypress = %c\n", event_action(event));
         xv_set(orient_item, PANEL_VALUE, 0, NULL);
	 reposition_widgets();
	 break;
 case 'q':
 case 'x':
	 if(debug) printf("Keypress = %c\n", event_action(event));
	 exit(0);
	 break;
 default:
	 if(debug) printf("NoOp Keypress = %c\n", event_action(event));
	 break;
      }
   }
   else if(event_action(event) == ACTION_ADJUST && event_is_down(event))
   get_values();          /* Middle mouse press to force a filesystem poll */
}

/*======================== MAIN Program ===================================*/
main(argc, argv)
int argc;
char *argv[];
{
   int          arg_c;
   int          n, x;
   int          t1, t2;
   int          inc;
   int          rspace;
   int          newroc;
   int          hort;
   int          repeat;
   char         dirname[MAXPATHLEN];
   char         *cmd = 0;
   char         *arg_v[128];
   char         icon_label[MAXHOSTNAMELEN];
   char         cmd_label[30];
   Panel_item   prop_panel;
   Menu         appmenu;
#ifdef SOLARIS2
   struct       statvfs buf;
#else
   struct       statfs buf;
#endif
   static      Xv_singlecolor colors[] = {
      { 255, 255, 255 }, /* white */
      { 255,   0,   0 }, /* red   */
      {   0, 255,   0 }, /* green */
      {   0,   0,   0 }, /* black */
   };

   fscnt = hort = rspace = debug = delta = repeat = used = 0;
   inc = 180;
   newroc = 10;

   arg_c = argc - 1;
   for(n=1;n < argc;++n) arg_v[n-1] = argv[n];

   if (gethostname(icon_label, MAXHOSTNAMELEN))
   {
      fprintf(stderr, "Problem getting HOSTNAME\n");
      sprintf(icon_label, "disktool");
   }

   for(n=0;n < MAX_GAUGES;++n)
   {
      critcnt[n] = 0;
      thresh[n] = 5;
      divisor[n] = 1000;
   }
   xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv, NULL);

   cms = (Cms)xv_create(NULL, CMS,
			CMS_CONTROL_CMS,  TRUE,
			CMS_SIZE,         CMS_CONTROL_COLORS + NUM_COLORS,
			CMS_COLORS,       colors,
			NULL);

   frame = (Frame)xv_create(XV_NULL, FRAME,
			    XV_WIDTH,             1000,
			    XV_HEIGHT,            1000,
			    FRAME_LABEL,          DT_LABEL,
			    FRAME_SHOW_FOOTER,    TRUE,
			    FRAME_SHOW_RESIZE_CORNER,  FALSE,
			    FRAME_WM_COMMAND_ARGC_ARGV, arg_c, arg_v,
			    NULL);

   panel = (Panel)xv_create(frame, PANEL, 
			    WIN_CMS,          cms,
			    PANEL_LAYOUT, PANEL_VERTICAL,
			    PANEL_ACCEPT_KEYSTROKE, TRUE,
			    PANEL_BACKGROUND_PROC, get_events,
			    NULL);

   image_ok = (Server_image)xv_create(NULL, SERVER_IMAGE,
				      XV_WIDTH,    dt_ok_width,
				      XV_HEIGHT,   dt_ok_height,
				      SERVER_IMAGE_DEPTH,  1,
				      SERVER_IMAGE_X_BITS, dt_ok_bits,
				      NULL);
   image_crit = (Server_image)xv_create(NULL, SERVER_IMAGE,
				      XV_WIDTH,    dt_crit_width,
				      XV_HEIGHT,   dt_crit_height,
				      SERVER_IMAGE_DEPTH,  1,
				      SERVER_IMAGE_X_BITS, dt_crit_bits,
				      NULL);
   image_mask = (Server_image)xv_create(NULL, SERVER_IMAGE,
				      XV_WIDTH,    dt_mask_width,
				      XV_HEIGHT,   dt_mask_height,
				      SERVER_IMAGE_DEPTH,  1,
				      SERVER_IMAGE_X_BITS, dt_mask_bits,
				      NULL);
   icon = (Icon)xv_create(frame, ICON,
			  ICON_IMAGE,       image_ok,
			  ICON_MASK_IMAGE,  image_mask,
			  ICON_LABEL,       icon_label,
			  NULL);
   xv_set(frame, FRAME_ICON, icon, NULL);

   for(n = 1;n < argc;++n)
   {
      if (strstr("-DEBUG", argv[n]))
      debug = 1;
      else if (strstr("-cmd", argv[n]))
      {
	 cmd = (char*) malloc(strlen(argv[(++n)]));
	 strcpy (cmd, argv[n]);
      }
      else if (strstr("-interval", argv[n]))
      inc = abs(atoi(argv[(++n)]));
      else if (strstr("-delta", argv[n]))
      delta = 1;
      else if (strstr("-repeat", argv[n]))
      repeat = abs(atoi(argv[(++n)]));
      else if (strstr("-used", argv[n]))
      used = 1;
      else if (strstr("-spacing", argv[n]))
      rspace = abs(atoi(argv[(++n)]));
      else if (strstr("-lineup", argv[n]))
      newroc = abs(atoi(argv[(++n)]));
      else if (strstr("-threshold", argv[n]))
      {
	 n++;
	 for(x=fscnt;x < MAX_GAUGES;++x) 
	 {
	    thresh[x] = abs(atol(argv[n]));
	    if(strstr(argv[n], "K") || strstr(argv[n], "k")) 
	    divisor[x] = 1;
	    else
	    divisor[x] = 1000;
	 }
      }
      else if (strstr("-horizontal", argv[n]))
      hort = 1;
      else
      {
	 if(fscnt < MAX_GAUGES)
	 {
	    strcpy(dirname, argv[n]);
	    if(!strcmp(dirname, "."))
	    sprintf(dirname, "%s", getenv("PWD"));
#ifdef SOLARIS2
	    if(!statvfs(dirname, &buf))
#else
	    if(!statfs(dirname, &buf))
#endif
	    {
	       build_fswidgets(dirname);
	    }
	    else
	    {
	       fprintf(stderr, "ERROR: Filesystem %s does not exist or is not searchable.\n",
		       dirname);
	    }
	 }
	 else
	 {
	    fprintf(stderr, "ERROR: Maximum number of Filesystems(%d) exceeded\n",
		    MAX_GAUGES);
	 }
      }
   }
   if (!fscnt)
   {
      fprintf(stderr, "\n\
[%s Copyright(c)1993,1994 Shaun Finn - sfinn@astro.ge.com]\n\n\
 Usage: disktool <options> [-t <int>] filesystem [[-t <int>] filesystem ]... \n\n\
[-t, -threshold <int>] (value in MBs to determine a critical disk, default=5)\n\n\
 options:\n\
[-c, -cmd <cmdline>]   (command to be executed upon critical condition)\n\
[-d, -delta]           (display delta since previous poll)\n\
[-h, -horizontal]      (default is vertical layout)\n\
[-i, -interval <int>]  (in seconds, default=180)\n\
[-l, -lineup <int>]    (# of gauges to lineup before starting new row\n\
			or col{depending on layout}, default=10)\n\
[-r, -repeat <int>]    (repeat rate for critical condition signal, default=0)\n\
[-s, -spacing <int>]   (spacing, in pixels, between gauges, default=0{Auto})\n\
[-u, -used]            (reverses gauge display to show used instead of avail)\n\
\n", DT_LABEL);
      exit(1);
   }

/* =================== Properties Stuff =================================== */

   sprintf(cmd_label, "%s - Properties", DT_LABEL);
   prop_frame = xv_create(frame, FRAME_CMD,
			  FRAME_LABEL,       cmd_label,
			  FRAME_DONE_PROC,   hide_prop,
			  NULL);
   prop_panel = xv_get(prop_frame, FRAME_CMD_PANEL);
   xv_set(prop_panel, PANEL_LAYOUT, PANEL_VERTICAL, NULL);
   xv_set(prop_panel, WIN_CMS, cms, NULL);
   xv_create(prop_panel, PANEL_MESSAGE,
             PANEL_LABEL_BOLD,      TRUE,
	     PANEL_LABEL_STRING,    "Disktool Copyright(c)1993,1994",
	     NULL);
   xv_create(prop_panel, PANEL_MESSAGE,
             XV_Y,       17,
             PANEL_LABEL_BOLD,      TRUE,
	     PANEL_LABEL_STRING,    "Send Comments & Suggestions to:",
	     NULL);
   xv_create(prop_panel, PANEL_MESSAGE,
             XV_Y,       30,
             PANEL_LABEL_BOLD,      TRUE,
	     PANEL_LABEL_STRING,    "sfinn@astro.ge.com",
	     NULL);
   xv_create(prop_panel, PANEL_MESSAGE,
	     PANEL_LABEL_STRING,    "__________________________________",
	     PANEL_ITEM_COLOR,   CMS_CONTROL_COLORS + WHITE,
	     NULL);
   inc_item = xv_create(prop_panel, PANEL_NUMERIC_TEXT,
			PANEL_LABEL_STRING,  "Polling Interval",
			PANEL_MIN_VALUE,     MIN_POLL_INT,
			PANEL_MAX_VALUE,     MAX_POLL_INT,
			PANEL_VALUE,         inc,
			PANEL_VALUE_DISPLAY_LENGTH, 4,
			PANEL_VALUE_DISPLAY_WIDTH,  45,
			PANEL_NOTIFY_PROC,   inc_sel,
			NULL);
   cmd_item = xv_create(prop_panel, PANEL_TEXT,
			PANEL_VALUE_DISPLAY_LENGTH, 24,
			PANEL_VALUE,     cmd,
			PANEL_LABEL_STRING,    "Command",
			PANEL_NOTIFY_PROC,     generic_sel,
                        NULL);
   pop_check = xv_create(prop_panel, PANEL_CHECK_BOX,
                         PANEL_LABEL_BOLD,      TRUE,
			 PANEL_CHOICE_STRINGS, "Un-Iconify upon Problem",NULL,
			 PANEL_NOTIFY_PROC,    pop_sel,
			 PANEL_VALUE,          1,
			 NULL);
   rep_item = xv_create(prop_panel, PANEL_NUMERIC_TEXT,
			PANEL_LABEL_STRING,  "Repeat Signal every",
			PANEL_MIN_VALUE,     0,
			PANEL_MAX_VALUE,     25,
			PANEL_VALUE,         repeat,
			PANEL_VALUE_DISPLAY_LENGTH, 4,
			PANEL_VALUE_DISPLAY_WIDTH,  45,
			PANEL_NOTIFY_PROC,   generic_sel,
			NULL);
   xv_set(prop_panel, PANEL_LAYOUT, PANEL_HORIZONTAL, NULL);
   xv_create(prop_panel, PANEL_MESSAGE,
             PANEL_LABEL_BOLD,      TRUE,
	     PANEL_LABEL_STRING,    "Polls",
	     NULL);
   xv_set(prop_panel, PANEL_LAYOUT, PANEL_VERTICAL, NULL);
   xv_create(prop_panel, PANEL_MESSAGE,
	     PANEL_LABEL_STRING,    "__________________________________",
	     PANEL_ITEM_COLOR,   CMS_CONTROL_COLORS + WHITE,
	     NULL);
   orient_item = xv_create(prop_panel, PANEL_TOGGLE,
		 	   PANEL_LAYOUT,        PANEL_HORIZONTAL,
			   PANEL_LABEL_STRING,  "Layout",
			   PANEL_CHOICE_STRINGS, "Vertical", "Horizontal", NULL,
			   PANEL_CHOOSE_ONE,    TRUE,
			   PANEL_VALUE,         hort,
			   PANEL_NOTIFY_PROC,   generic_sel,
			   NULL);
   roc_item = xv_create(prop_panel, PANEL_NUMERIC_TEXT,
			  PANEL_LABEL_STRING,  "Gauges per row or column",
			  PANEL_MIN_VALUE,     1,
			  PANEL_MAX_VALUE,     20,
			  PANEL_VALUE,         newroc,
			  PANEL_VALUE_DISPLAY_LENGTH, 3,
			  PANEL_VALUE_DISPLAY_WIDTH,  35,
			  NULL);
   spacing_item = xv_create(prop_panel, PANEL_NUMERIC_TEXT,
		  	    PANEL_LABEL_STRING,  "Spacing of gauges(pixels)",
			    PANEL_MIN_VALUE,     0,
			    PANEL_MAX_VALUE,     250,
			    PANEL_VALUE,         rspace,
  			    PANEL_VALUE_DISPLAY_LENGTH, 4,
			    PANEL_VALUE_DISPLAY_WIDTH,  45,
			    NULL);
   xv_create(prop_panel, PANEL_MESSAGE,
	     PANEL_LABEL_STRING,    "__________________________________",
	     PANEL_ITEM_COLOR,   CMS_CONTROL_COLORS + WHITE,
	     NULL);
   disk_item = xv_create(prop_panel, PANEL_TEXT,
			PANEL_VALUE_DISPLAY_LENGTH, 24,
			PANEL_VALUE,     cmd,
	                PANEL_ITEM_COLOR,   CMS_CONTROL_COLORS + OKCOLOR,
			PANEL_LABEL_STRING,    "Filesystem",
                        NULL);
   units_item = xv_create(prop_panel, PANEL_TOGGLE,
			  PANEL_LAYOUT,        PANEL_HORIZONTAL,
			  PANEL_LABEL_STRING,  "Display Units",
			  PANEL_CHOICE_STRINGS, "MB", "KB", NULL,
			  PANEL_CHOOSE_ONE,    TRUE,
			  PANEL_VALUE,         0,
			  PANEL_NOTIFY_PROC,   units_sel,
			  NULL);
   thresh_item = xv_create(prop_panel, PANEL_NUMERIC_TEXT,
			  PANEL_LABEL_STRING,  "Critical Threshold",
			  PANEL_MIN_VALUE,     1,
			  PANEL_MAX_VALUE,     3000,
			  PANEL_VALUE,         thresh[0],
			  PANEL_VALUE_DISPLAY_LENGTH, 9,
			  PANEL_VALUE_DISPLAY_WIDTH,  75,
			  PANEL_NOTIFY_PROC,   generic_sel,
			  NULL);
  appmenu = (Menu)xv_create(NULL, MENU,
			    MENU_TITLE_ITEM,  "Apply",
			    MENU_ACTION_ITEM, "This Item Only", set_item,
			    MENU_ACTION_ITEM, "All Items", set_all_items,
			    NULL);

   xv_create(prop_panel, PANEL_BUTTON,
	     PANEL_LABEL_STRING,  " Apply ",
	     PANEL_ITEM_MENU,     appmenu,
	     NULL);

   xv_set(prop_panel, PANEL_LAYOUT, PANEL_HORIZONTAL, NULL);

   xv_create(prop_panel, PANEL_BUTTON,
             PANEL_LABEL_STRING,  "Delete",
             PANEL_NOTIFY_PROC, del_fs,
             NULL);
   xv_create(prop_panel, PANEL_BUTTON,
             PANEL_LABEL_STRING,  "Add Filesystem",
             PANEL_NOTIFY_PROC, add_fs,
             NULL);

   window_fit(prop_panel);
   window_fit(prop_frame);
   xv_set(prop_frame, XV_SHOW, FALSE, NULL);

/* =================== End of Properties ================================= */

   if (debug) printf("\n%s\n\n",DT_LABEL);

   if(delta)
   refresh_delta();  /* This is intentionally redundant to zero out deltas */
   else
   get_values();

   reposition_widgets();
   timer.it_value.tv_sec = inc;
   timer.it_interval.tv_sec = inc;
   notify_set_itimer_func(frame, get_values,
			  ITIMER_REAL, &timer, NULL);

   window_fit(panel);
   if(xv_get(panel, XV_WIDTH) < 135) xv_set(panel, XV_WIDTH, 135, NULL);
   window_fit(frame);
   xv_main_loop(frame);
}

/*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
