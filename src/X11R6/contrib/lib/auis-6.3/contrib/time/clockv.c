/* ********************************************************************** *\
 *         Copyright IBM Corporation 1991 - All Rights Reserved           *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/time/RCS/clockv.c,v 1.7 1994/01/31 22:23:34 rr2b Exp $";
#endif

#include <andrewos.h>
#include <math.h>
#include <class.h>
#include <graphic.ih>
#include <observe.ih>
#include <proctbl.ih>
#include <view.ih>
#include <clockv.eh>

/* Defined constants and macros */
#define MENUTITLE "Clock %s,%s"
#define PI (double)3.14159265358979
#define HOURSTORADIANS(x) ((15.0-(x))/6.0*PI)
#define MINUTESTORADIANS(x) ((75.0-(x))/30.0*PI)

/* External Declarations */

/* Forward Declarations */

/* Global Variables */
static struct menulist *clockview_menulist = NULL;
static char *label_set1[3][1] = {{"12"}, {"XII"}, {"Twelve"}};
static char *label_set4[3][4] = {{"12", "3", "6", "9"},
				   {"XII", "III", "VI", "IX"},
				   {"Twelve", "Three", "Six", "Nine"}};
static char *label_set12[3][12] = {{"12", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"},
				   {"XII", "I", "II", "III", "IIII", "V", "VI", "VII", "VIII", "IX", "X", "XI"},
				   {"Twelve", "One", "Two",
				      "Three", "Four", "Five",
				      "Six", "Seven", "Eight",
				      "Nine", "Ten", "Eleven"}};


static void MenuSetShape(self, format)
     struct clockview *self;
     char *format;
{
  struct clock *b = (struct clock *) clockview_GetDataObject(self);
  struct clock_options *options;

  options = clock_GetOptions(b);
  switch (format[0]) {
  case 'A':
    options->border_shape = circle;
    break;
  case 'B':
    options->border_shape = square;
    break;
  case '1':
    options->border_width = 0;
    break;
  case '2':
    options->border_width = 1;
    break;
  case '3':
    options->border_width = 2;
    break;
  }
  clock_SetOptions(b, options);
}


static void MenuSetLabels(self, format)
     struct clockview *self;
     char *format;
{
  struct clock *b = (struct clock *) clockview_GetDataObject(self);
  struct clock_options *options;
  int style;

  options = clock_GetOptions(b);
  if (options->num_labels > 0) {
    switch (options->labels[0][0]) {
    case '1':
      style = 0;
      break;
    case 'X':
      style = 1;
      break;
    case 'T':
      style = 2;
      break;
    default:
      style = 0;
      break;
    }
  } else {
    style = 0;
  }

  switch (format[0]) {
  case 'A':
    options->num_labels = 0;
    options->labels = NULL;
    break;
  case 'B':
    options->num_labels = 1;
    options->labels = label_set1[style];
    break;
  case 'C':
    options->num_labels = 4;
    options->labels = label_set4[style];
    break;
  case 'D':
    options->num_labels = 12;
    options->labels = label_set12[style];
    break;
  case '1':
  case '2':
  case '3':
    style = (int)(format[0] - '1');
    switch(options->num_labels) {
    case 1:
      options->labels = label_set1[style];
      break;
    case 4:
      options->labels = label_set4[style];
      break;
    default:
      options->labels = label_set12[style];
      options->num_labels = 12;
      break;
    }
  }
  clock_SetOptions(b, options);
}


static void MenuSetTicks(self, format)
     struct clockview *self;
     char *format;
{
  struct clock *b = (struct clock *) clockview_GetDataObject(self);
  struct clock_options *options;

  options = clock_GetOptions(b);
  switch (format[0]) {
  case 'A':
    options->major_ticks = 0;
    options->minor_ticks = 0;
    break;
  case 'B':
    options->major_ticks = 1;
    options->minor_ticks = 4;
    break;
  case 'C':
    options->major_ticks = 4;
    options->minor_ticks = 12;
    break;
  case 'D':
    options->major_ticks = 12;
    options->minor_ticks = 60;
    break;
  }
  clock_SetOptions(b, options);
}


static void MenuSetSeconds(self, format)
     struct clockview *self;
     char *format;
{
  struct clock *b = (struct clock *) clockview_GetDataObject(self);
  struct clock_options *options;

  options = clock_GetOptions(b);
  switch (format[0]) {
  case 'A':
    options->seconds_length = 0;
    break;
  case 'B':
    options->seconds_length = 100;
    break;
  case 'C':
    options->seconds_length = -20;
    break;
  }
  clock_SetOptions(b, options);
}


boolean
clockview__InitializeClass(c)
struct classheader *c;
{
/* 
  Initialize all the class data.
*/
  struct proctable_Entry *proc = NULL;
  char menuname[255];

  clockview_menulist = menulist_New();

  proc = proctable_DefineProc("clock-set-num-labels", MenuSetShape, &clockview_classinfo, NULL, "Set the clock inset's shape parameters.");
  sprintf(menuname, MENUTITLE, "Shape~10", "Circle~1");
  menulist_AddToML(clockview_menulist, NewString(menuname), proc, "A", 0);
  sprintf(menuname, MENUTITLE, "Shape~10", "Square~2");
  menulist_AddToML(clockview_menulist, NewString(menuname), proc, "B", 0);
  sprintf(menuname, MENUTITLE, "Shape~10", "No border~11");
  menulist_AddToML(clockview_menulist, NewString(menuname), proc, "1", 0);
  sprintf(menuname, MENUTITLE, "Shape~10", "Thin border~12");
  menulist_AddToML(clockview_menulist, NewString(menuname), proc, "2", 0);
  sprintf(menuname, MENUTITLE, "Shape~10", "Thick border~13");
  menulist_AddToML(clockview_menulist, NewString(menuname), proc, "3", 0);

  proc = proctable_DefineProc("clock-set-labels", MenuSetLabels, &clockview_classinfo, NULL, "Set the clock inset's label parameters.");
  sprintf(menuname, MENUTITLE, "Labels~11", "None~1");
  menulist_AddToML(clockview_menulist, NewString(menuname), proc, "A", 0);
  sprintf(menuname, MENUTITLE, "Labels~11", "1  (12)~2");
  menulist_AddToML(clockview_menulist, NewString(menuname), proc, "B", 0);
  sprintf(menuname, MENUTITLE, "Labels~11", "4 (12  3  6  9)~3");
  menulist_AddToML(clockview_menulist, NewString(menuname), proc, "C", 0);
  sprintf(menuname, MENUTITLE, "Labels~11", "12 (12  1  2 ...)~4");
  menulist_AddToML(clockview_menulist, NewString(menuname), proc, "D", 0);

  sprintf(menuname, MENUTITLE, "Labels~11", "Arabic (12)~11");
  menulist_AddToML(clockview_menulist, NewString(menuname), proc, "1", 0);
  sprintf(menuname, MENUTITLE, "Labels~11", "Roman (XII)~12");
  menulist_AddToML(clockview_menulist, NewString(menuname), proc, "2", 0);
  sprintf(menuname, MENUTITLE, "Labels~11", "English (Twelve)~13");
  menulist_AddToML(clockview_menulist, NewString(menuname), proc, "3", 0);

  proc = proctable_DefineProc("clock-set-ticks", MenuSetTicks, &clockview_classinfo, NULL, "Set the clock inset's tick count parameters.");
  sprintf(menuname, MENUTITLE, "Ticks~12", "None~1");
  menulist_AddToML(clockview_menulist, NewString(menuname), proc, "A", 0);
  sprintf(menuname, MENUTITLE, "Ticks~12", "1 / 4~2");
  menulist_AddToML(clockview_menulist, NewString(menuname), proc, "B", 0);
  sprintf(menuname, MENUTITLE, "Ticks~12", "4 / 12~3");
  menulist_AddToML(clockview_menulist, NewString(menuname), proc, "C", 0);
  sprintf(menuname, MENUTITLE, "Ticks~12", "12 / 60~4");
  menulist_AddToML(clockview_menulist, NewString(menuname), proc, "D", 0);

  proc = proctable_DefineProc("clock-set-seconds", MenuSetSeconds, &clockview_classinfo, NULL, "Set the clock inset's seconds hand parameters.");
  sprintf(menuname, MENUTITLE, "Seconds~13", "No Second Hand~1");
  menulist_AddToML(clockview_menulist, NewString(menuname), proc, "A", 0);
  sprintf(menuname, MENUTITLE, "Seconds~13", "Radial Hand~2");
  menulist_AddToML(clockview_menulist, NewString(menuname), proc, "B", 0);
  sprintf(menuname, MENUTITLE, "Seconds~13", "Floating Tick~3");
  menulist_AddToML(clockview_menulist, NewString(menuname), proc, "C", 0);

  return(TRUE);
}


boolean
clockview__InitializeObject(c, self)
struct classheader *c;
struct clockview *self;
{
/*
  Set up the data for each instance of the object.
*/
  self->need_full_update = TRUE;
  if (!(self->cursor = cursor_Create(self))) return(FALSE);
  cursor_SetStandard(self->cursor, Cursor_Gunsight);
  self->ml = menulist_DuplicateML(clockview_menulist, self);
  self->last_options_timestamp = 0;

  return(TRUE);
}


void
clockview__FinalizeObject(c, self)
struct classheader *c;
struct clockview *self;
{
  if (self->cursor) cursor_Destroy(self->cursor);
  self->cursor = NULL;
  if (self->ml) menulist_Destroy(self->ml);
  self->ml = NULL;
  return;
}



static void
PlotLabels(self, theta, radius, label, shape)
struct clockview *self;
double theta;
int radius;
char *label;
enum border_shapes shape;
{
  struct rectangle rect;
  long max_radius;
  long x0, y0, x1, y1, x2, y2;

  if (radius == 0) return;

  clockview_GetLogicalBounds(self, &rect);
  max_radius = MIN(rect.width, rect.height)/2;

  /* center */
  x0 = (rect.left + rect.width)/2;
  y0 = (rect.top + rect.height)/2;
  if (radius>0) {
    /* normal hand, from center */
    x2 = (x0) + (max_radius*((double)radius/100.0*cos(theta)));
    y2 = (y0) - (max_radius*((double)radius/100.0*sin(theta)));
    clockview_MoveTo(self, x0, y0);
  } else {
    /* hand from edge */
    if (shape == circle) {
      x1 = (x0) + (max_radius*((1.0 + (double)radius/100.0)*cos(theta)));
      y1 = (y0) - (max_radius*((1.0 + (double)radius/100.0)*sin(theta)));
      clockview_MoveTo(self, x1, y1);
    } else {
      /* Canonicalize theta */
      while (theta>2*PI) theta = theta-2*PI;
      if (theta<0) theta = theta+2*PI;

      if ((theta < PI/4.0) || (theta >= 7.0*PI/4.0)) {
	x1 = max_radius;
	y1 = max_radius*tan(theta);
      } else if ((theta >= PI/4.0) && (theta < 3.0*PI/4.0)) {
	x1 = -max_radius*tan(theta-PI/2.0);
	y1 = max_radius;
      } else if ((theta >= 3.0*PI/4.0) && (theta < 5.0*PI/4.0)) {
	x1 = -max_radius;
	y1 = -max_radius*tan(theta-PI);
      } else {
	y1 = -max_radius;
	x1 = max_radius*tan(theta-3.0*PI/2.0);
      }
      x1 = x0 + x1;
      y1 = y0 - y1;
      x2 = (x1) + (max_radius*((double)radius/-100.0*cos(theta+PI)));
      y2 = (y1) - (max_radius*((double)radius/-100.0*sin(theta+PI)));
      clockview_MoveTo(self, x2, y2);
    }
  }
  clockview_DrawString(self, label,  graphic_BETWEENLEFTANDRIGHT | graphic_BETWEENTOPANDBOTTOM);

  return;
}


static void
PlotPoints(self, theta, radius, thickness, shape)
struct clockview *self;
double theta;
int radius, thickness;
enum border_shapes shape;
{
  struct rectangle rect;
  long max_radius;
  long x0, y0, x1, y1, x2, y2;
  
  if (radius == 0) return;

  clockview_GetLogicalBounds(self, &rect);
  max_radius = MIN(rect.width-1, rect.height-1)/2;

  /* center */
  x0 = (rect.left + rect.width-1)/2;
  y0 = (rect.top + rect.height-1)/2;
  clockview_SetLineWidth(self, thickness);
  if (radius>0) {
    /* normal hand, from center */
    x2 = (x0) + (max_radius*((double)radius/100.0*cos(theta)));
    y2 = (y0) - (max_radius*((double)radius/100.0*sin(theta)));
    clockview_MoveTo(self, x0, y0);
  } else {
    /* hand from edge */
    if (shape == circle) {
      x1 = (x0) + (max_radius*((1.0 + (double)radius/100.0)*cos(theta)));
      y1 = (y0) - (max_radius*((1.0 + (double)radius/100.0)*sin(theta)));
      x2 = (x0) + (max_radius*cos(theta));
      y2 = (y0) - (max_radius*sin(theta));
    } else {
      /* Canonicalize theta */
      while (theta>2*PI) theta = theta-2*PI;
      if (theta<0) theta = theta+2*PI;

      if ((theta < PI/4.0) || (theta >= 7.0*PI/4.0)) {
	x1 = max_radius;
	y1 = max_radius*tan(theta);
      } else if ((theta >= PI/4.0) && (theta < 3.0*PI/4.0)) {
	x1 = -max_radius*tan(theta-PI/2.0);
	y1 = max_radius;
      } else if ((theta >= 3.0*PI/4.0) && (theta < 5.0*PI/4.0)) {
	x1 = -max_radius;
	y1 = -max_radius*tan(theta-PI);
      } else {
	y1 = -max_radius;
	x1 = max_radius*tan(theta-3.0*PI/2.0);
      }
      x1 = x0 + x1;
      y1 = y0 - y1;
      x2 = (x1) + (max_radius*((double)radius/-100.0*cos(theta+PI)));
      y2 = (y1) - (max_radius*((double)radius/-100.0*sin(theta+PI)));
    }
    clockview_MoveTo(self, x1, y1);
  }
  clockview_DrawLineTo(self, x2, y2);

  return;
}


static void
Redraw(self)
struct clockview *self;
{
/*
  Redisplay this object.
*/
  struct clock_time *clockface;
  struct clock_options *options;
  struct rectangle rect, border;
  long min_dimension;
  struct fontdesc *myfontdesc;

  clockface = clock_ReadClock((struct clock *) clockview_GetDataObject(self));
  options = clock_GetOptions((struct clock *) clockview_GetDataObject(self));
  if (self->last_options_timestamp != options->timestamp) {
    self->last_options_timestamp = options->timestamp;
    self->need_full_update = TRUE;
  }

  clockview_GetLogicalBounds(self, &rect);
  if (self->need_full_update) {
    /* need to redraw face and hands */
    clockview_SetTransferMode(self, graphic_SOURCE);
    clockview_EraseVisualRect(self);
    rect.width--;
    rect.height--;
    min_dimension = MIN(rect.width, rect.height);
    if (options->border_width > 0) {
      border.left = rect.left+(rect.width-min_dimension+options->border_width)/2;
      border.top = rect.top+(rect.height-min_dimension+options->border_width)/2;
      border.width = min_dimension - options->border_width + (rect.width-min_dimension+options->border_width)%2;
      border.height = min_dimension - options->border_width + (rect.height-min_dimension+options->border_width)%2;

      clockview_SetLineWidth(self, options->border_width);
      if (options->border_shape == circle) {
	clockview_DrawArc(self, &border, 0, 360);
      } else {
	clockview_DrawRect(self, &border);
      }
    }

#define FACELOOP(num, func, where, what) \
    if (num) { \
      int i; \
      for (i = 0; i < (num); ++i) { \
	(func)(self, PI/2.0 - ((double)i)/((double)(num))*2.0*PI, (where)*(options->tick_length), what, options->border_shape); \
      } \
    }

    FACELOOP(options->minor_ticks, PlotPoints, -1, 1);
    FACELOOP(options->major_ticks, PlotPoints, -2, 1);
    myfontdesc = fontdesc_Create(options->fontfamily, options->fontface, (options->tick_length*min_dimension/(options->major_ticks?150:75)));
    if (myfontdesc) clockview_SetFont(self, myfontdesc);
    FACELOOP(options->num_labels, PlotLabels, -3, options->labels[i]);

    clockview_SetTransferMode(self, graphic_XOR);
    PlotPoints(self, HOURSTORADIANS(clockface->hours), options->hours_length, options->hours_width, options->border_shape);
    PlotPoints(self, MINUTESTORADIANS(clockface->minutes), options->minutes_length, options->minutes_width, options->border_shape);
    PlotPoints(self, MINUTESTORADIANS(clockface->seconds), options->seconds_length, options->seconds_width, options->border_shape);
  } else {
    clockview_SetTransferMode(self, graphic_XOR);
    if (self->lastclockface.hours != clockface->hours) {
      /* redraw hour hand */
      PlotPoints(self, HOURSTORADIANS(self->lastclockface.hours), options->hours_length, options->hours_width, options->border_shape);
      PlotPoints(self, HOURSTORADIANS(clockface->hours), options->hours_length, options->hours_width, options->border_shape);
    }

    if (self->lastclockface.minutes != clockface->minutes) {
      /* redraw minute hand */
      PlotPoints(self, MINUTESTORADIANS(self->lastclockface.minutes), options->minutes_length, options->minutes_width, options->border_shape);
      PlotPoints(self, MINUTESTORADIANS(clockface->minutes), options->minutes_length, options->minutes_width, options->border_shape);
    }

    if (self->lastclockface.seconds != clockface->seconds) {
      /* redraw second hand */
      PlotPoints(self, MINUTESTORADIANS(self->lastclockface.seconds), options->seconds_length, options->seconds_width, options->border_shape);
      PlotPoints(self, MINUTESTORADIANS(clockface->seconds), options->seconds_length, options->seconds_width, options->border_shape);
    }
  }
  self->lastclockface.hours = clockface->hours;
  self->lastclockface.minutes = clockface->minutes;
  self->lastclockface.seconds = clockface->seconds;
  self->need_full_update = FALSE;
}


void
clockview__FullUpdate(self, type, left, top, width, height)
struct clockview *self;
enum view_UpdateType type;
long left, top, width, height;
{
/*
  Do an update.
*/
  if ((type == view_FullRedraw) || (type == view_LastPartialRedraw)) {
    self->need_full_update = TRUE;
    Redraw(self);
  }
}


void
clockview__Update(self)
struct clockview *self;  
{
  Redraw(self);
}


struct view *
clockview__Hit(self, action, x, y, numclicks)
struct clockview *self;
long x, y;
enum view_MouseAction action;
long numclicks;  
{
/*
  Handle the button event.  Currently, semantics are:
*/
  clockview_WantInputFocus(self, self);
  return((struct view *)self);
}


void
clockview__Print(self, file, processor, finalFormat, topLevel)
struct clockview *self;
FILE *file;
char *processor, *finalFormat;
boolean topLevel;
{
  long t;

  t = time(0);
  fprintf(file, "%s", ctime(&t));
}



void
clockview__PostMenus(self, ml)
struct clockview *self;
struct menulist *ml;
{
/*
  Enable the menus for this object.
*/

  menulist_ClearChain(self->ml);
  if (ml) menulist_ChainAfterML(self->ml, ml, ml);
  super_PostMenus(self, self->ml);
}
