/*
 * Copyright (C) 1993 Rudolf Koenig 
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2, or (at your option)
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program (see the file COPYING); if not, write to the
 * Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */

#include <xview/xview.h>
#include <xview/font.h>
#include <xview/canvas.h>
#include <xview/panel.h>
#include <xview/notify.h>
#include <xview/defaults.h>
#include <xview/icon.h>
#include "icon.xbm"
#include <rpc/rpc.h>
#include <rpcsvc/rstat.h>
#include <stdio.h>

extern int fork();

#define PROGNAME "perf"

#define CPU  1
#define PKTS 2
#define PAGE 3
#define SWAP 4
#define INTR 5
#define DISK 6
#define CTXT 7
#define LOAD 8
#define COLL 9
#define ERRS 10

#define NUMCOLS 6
#define WHITE 4
#define BLACK 5


struct pm
{
  int offset;
  int starttime;
  int posx, posy, width;
  GC fixed;
  GC colorgc[NUMCOLS];
  Pixmap pm;
} pm;

struct values
{
  int type; 
  char *name;
  char *subname[4];
  int numvalues;
  int **values, *last;
  int maxval, maxval_to_show;
  int active;
  unsigned int
      special:1,
      draw_line:1,
      no_smooth:1,
      draw_one:1,
      no_reset:1,
      selected:1;
  int scrollback;
  int sec_perpixel, lastsec;
  int xpos, ypos;
  int validx;
  struct pm *pm;
} values[] =
{
  CPU, "cpu", {"usr","nice","sys",0},3,0,0,100,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  DISK,"disk",{"1",  "2",   "3","4"},4,0,0,  1,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  PAGE,"page",{"in", "out",  0,   0},2,0,0,  1,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  SWAP,"swap",{"in", "out",  0,   0},2,0,0,  1,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  INTR,"interrupts",{0,0,    0,   0},1,0,0,  1,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  PKTS,"packets",{"in","out",0,   0},2,0,0,  1,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  ERRS,"errors", {"in","out",0,   0},2,0,0,  1,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  COLL,"collisions",{0,0,    0,   0},1,0,0,  1,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  CTXT,"context",{0,  0,     0,   0},1,0,0,  1,-1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  LOAD,"load",{0,     0,     0,   0},1,0,0,256, 1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  0,   0,     {0,     0,     0,   0},0,0,0,  0, 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
}, *active = 0;


#define VALUES_LENGTH(v) ((v->special?v->scrollback:scrollback)+width2)

Frame fr, pfr;
Canvas ca;
Panel pn;
Panel_item pi_sels, pi_mach, pi_check, pi_spp,  pi_cols, pi_msg,
           pi_apply, pi_hide, pi_fork, pi_nosp, pi_scbk;
Display *dpy;
Window win;
int screen;
Xv_Font font, fixedfont;
XFontStruct *font_info, *fixedfont_info;
GC bwingc, wingc;
int win_x, real_win_y, win_y = 0, 
    fontheight, depth, nrcurr_active,
    width1, width2, height1, lost_connection = 0;
char **argv;

/* Options */
int sec_perpixel = 2, lastsec = 0, scrollback = 600, columns = 1,
    no_reset = 0, draw_line = 0, no_smooth = 0, draw_one = 0;
char *host = 0;
int defcol[NUMCOLS] =
  { 0x000000, 0xa0a0a0, 0xc0c0c0, 0xe0e0e0, 0xffffff, 0x000000 };
int defidx[NUMCOLS];

struct opts
{
  char *name;
  int *param;
  int needsarg;
} opts[] = 
{
  { "drawline",   &draw_line,     0 },
  { "nosmooth",   &no_smooth,     0 },
  { "drawone",    &draw_one,      0 },
  { "sampletime", &sec_perpixel,  1 },
  { "noreset",    &no_reset,      0 },
  { "scrollback", &scrollback,    1 },
  { "columns",    &columns,       1 },
  { "col1",       &defcol[0],     1 },
  { "col2",       &defcol[1],     1 },
  { "col3",       &defcol[2],     1 },
  { "col4",       &defcol[3],     1 },
  { "foreground", &defcol[BLACK], 1 },
  { "background", &defcol[WHITE], 1 },
  { 0, 0, 0 },
};


struct statstime sx;
int just_started = 1, diffdisk, min_spp;
Notify_value update();

do_alarm(t)
  int t;
{
  struct itimerval timer;

  timer.it_value.tv_sec = timer.it_interval.tv_sec = t;
  timer.it_value.tv_usec = timer.it_interval.tv_usec = 50000;
  notify_enable_rpc_svc(1);
  notify_set_itimer_func(fr, update, ITIMER_REAL, &timer, NULL);
}

/* indx is needed by repaint, scrollback... */
static void
DrawPixels(val, p, indx, col)
  struct values *val;
  struct pm *p;
  int indx, col;
{
  int x, y, i, oldv, newv, old, new,
      **vals = val->values,
      vl = VALUES_LENGTH(val);

  old = new = 0;
  x = val->xpos * width1 + col; 
  y = val->ypos * height1 + height1 - fontheight;

  for(i = 0; i < val->numvalues; i++)
    {
      new += vals[i][indx];
      old += vals[i][(indx-1+vl) % vl];
    }
  if(new > val->maxval) new = val->maxval;
  if(old > val->maxval) old = val->maxval;

  for(i = 0; i < val->numvalues; i++)
    {
      newv = (double)((height1-fontheight) * new) / (double)val->maxval;
      oldv = (double)((height1-fontheight) * old) / (double)val->maxval;

      if(val->special ? val->draw_line : draw_line) 
        {
	  XDrawLine(dpy, p->pm, p->colorgc[i], x-1, y - oldv, x, y - newv);
        }
      else
        {
	  XDrawLine(dpy, p->pm, p->colorgc[i], x, y, x, y - newv);

	  if(val->special ? !val->no_smooth : !no_smooth)
	    {
	      if(newv > oldv)
		XDrawLine(dpy, p->pm, p->colorgc[i], 
			x - 1, y - oldv, x - 1, y - oldv - (newv - oldv ) / 2);
	      else
		XDrawLine(dpy, p->pm, p->colorgc[i],
			x, y - newv, x, y - newv - (oldv - newv ) / 2);
	    }
	}

      if(val->special ? val->draw_one : draw_one)
        break;
      new -= vals[i][indx];
      old -= vals[i][(indx-1+vl) % vl];
    }
}

static int 
nextmax(v)
  int v;
{
  int m;

  for(m = 10; ; m *= 10)
    if(v < m)
      break;
  m /= 10;
  if(v > m * 5)
    return m * 10;
  if(v > m * 2)
    return m *  5;
  else if(v > m)
    return m * 2;
  else
    return m;
}

/* from can be larger then to*/
static int
rescale(val, from, to)
  struct values *val;
  int from, to;
{
  int j, k, sum, max, tlen;
  int changed = 0;

  if(val->type == CPU)
    return 0;
  tlen = VALUES_LENGTH(val);
  max = (val->type == LOAD) ? 256 : 1;

  for(j = from; ; j = (j+1) % tlen)
    {
      for(sum = 0, k = val->numvalues - 1; k >= 0; k--)
	sum += val->values[k][j];
      if(sum > max)
	if(val->type == LOAD)
	  while(sum > max) max *= 2;
	else
	  max = sum;
      if(j == to)
        break;
    }
  if(val->type == LOAD)
    {
      val->maxval_to_show = max / 256;
      changed = (val->maxval > max);
      val->maxval = max;
    }
  else
    {
      changed = (val->maxval > nextmax(max));
      val->maxval = nextmax(max);
    }
  return changed;
}

static void
fill_last(sxp)
  struct statstime *sxp;
{
  int i, j, sum;
  struct values *v;

  for(i = 0; values[i].numvalues; i++)
    {
      v = &values[i];
      if(v->special && v->lastsec)
         continue;
      switch(v->type)
        {
          case CPU:
	    for(j = 0; j < 4; j++)
	      v->last[j] = sxp->cp_time[j];
	    break;
          case DISK:
	    for(sum = j = 0; j < 4; j++)
	      v->last[j] = sxp->dk_xfer[j];
	    break;
          case PAGE:
	    v->last[0] = sxp->v_pgpgin;
	    v->last[1] = sxp->v_pgpgout;
	    break;
          case SWAP:
	    v->last[0] = sxp->v_pswpin;
	    v->last[1] = sxp->v_pswpout;
	    break;
          case INTR:
	    v->last[0] = sxp->v_intr;
	    break;
          case PKTS:
	    v->last[0] = sxp->if_ipackets;
	    v->last[1] = sxp->if_opackets;
	    break;
          case ERRS:
	    v->last[0] = sxp->if_ierrors;
	    v->last[1] = sxp->if_oerrors;
	    break;
          case COLL:
	    v->last[0] = sxp->if_collisions;
	    break;
          case CTXT:
	    v->last[0] = sxp->v_swtch;
	    break;
          case LOAD:
	    break;
	}
    }
}

static void
inc()
{
  int i;
  struct values *v;

  for(i = 0; values[i].numvalues; i++)
    {
      v = &values[i];
      if(v->special ? !v->lastsec : !lastsec)
	v->validx = (v->validx+1) % VALUES_LENGTH(v);
      if(v->special)
	v->lastsec = (v->lastsec+min_spp) % v->sec_perpixel;
    }
  lastsec = (lastsec+min_spp) % sec_perpixel;
}

Notify_value
update()
{
  int i, j, redraw, from, to,
      sum, sum1, x, y;
  double d;
  struct values *v;
  static void dopaint();

  redraw = 0;

  if(!values[0].last) /* Called before initialization */
    return;
  if(rstat(host, &sx))
    {
      static char noconn[]="Lost connection";
      int dir, ascent, descent;
      XCharStruct or;

      /* Draw 'Lost Connection' in every field */
      if(lost_connection)
	return NOTIFY_DONE;
      XTextExtents(font_info, noconn, strlen(noconn),&dir,&ascent,&descent,&or);

      for(i = 0; values[i].numvalues; i++)
        if(values[i].active)
	  {
	    XDrawImageString(dpy, pm.pm, pm.colorgc[WHITE],
	          values[i].xpos * width2 + (width2 - or.width)/2,
		  values[i].ypos * height1 + (height1 - fontheight + ascent)/2,
		  noconn, strlen(noconn));
	  }
      lost_connection = 1;
      XCopyArea(dpy, pm.pm, win, wingc, 0, 0, win_x, real_win_y, 0, 0);
      XFlush(dpy);
      return NOTIFY_DONE;
    }

  if(lost_connection || just_started) /* Connection alive again */
    {
      if(just_started)
	{
	  if(sx.dk_xfer[0]+sx.dk_xfer[0]+sx.dk_xfer[0]+sx.dk_xfer[0] > 200)
	    diffdisk = 0;
	  else
	    diffdisk = 1;
	}
      just_started = lost_connection = 0;
      fill_last(&sx);
      sleep(1);
      rstat(host, &sx);
      redraw = 1;
    }

/* Let's scroll back */
  for(i = 0; values[i].numvalues; i++)
    {
      v = &values[i];
      if(v->active && (v->special ? (v->lastsec == 0) : (lastsec == 0)))
	{
	  x = v->xpos * width1;
	  y = v->ypos * height1;

	  XCopyArea(dpy, pm.pm, pm.pm, pm.colorgc[BLACK],
			       x+1, y, width2-1, height1-fontheight+1, x, y);
	  XDrawLine(dpy, pm.pm, pm.colorgc[WHITE],
			       x+width2-1, y, x+width2-1, y+height1);
	}
    }


/* Let's update */
  for(i = 0; values[i].numvalues; i++)
    {
      v = &values[i];
      if(v->special ? v->lastsec : lastsec)
        continue;
/*
printf("%s,", v->name); fflush(stdout);
*/
      switch(v->type)
	{
	  case CPU:
	    for(j = sum = 0; j < 4; j++)
	      sum += sx.cp_time[j] - v->last[j];
            if (sum == 0)
              sum = 1; /* Prevent floating point exception*/
	    for(j = 0; j < 3; j++)
	      {
		d = 100.0 * (sx.cp_time[j] - v->last[j])/(double)sum;
		v->values[j][v->validx] = (int)(d+0.5);
	      }
	    break;
	  case PKTS:
	    sum  = sx.if_ipackets - v->last[0];
	    sum1 = sx.if_opackets - v->last[1];
	    v->values[0][v->validx] = sum;
	    v->values[1][v->validx] = sum1;
#define CHECKMAX(a) if(a > v->maxval) { if(v->active) redraw = 1; v->maxval = nextmax(a); }
            CHECKMAX(sum+sum1);
	    break;
	  case PAGE:
	    sum  = sx.v_pgpgin  - v->last[0];
	    sum1 = sx.v_pgpgout - v->last[1];
	    v->values[0][v->validx] = sum;
	    v->values[1][v->validx] = sum1;
            CHECKMAX(sum+sum1);
	    break;
	  case SWAP:
	    sum  = sx.v_pswpin  - v->last[0];
	    sum1 = sx.v_pswpout - v->last[1];
	    v->values[0][v->validx] = sum;
	    v->values[1][v->validx] = sum1;
            CHECKMAX(sum+sum1);
	    break;
	  case INTR:
	    v->values[0][v->validx] = sx.v_intr - v->last[0];
            CHECKMAX(v->values[0][v->validx]);
	    break;
	  case DISK:
	    for(sum = j = 0; j < 4; j++)
	      {
		v->values[j][v->validx] = sx.dk_xfer[j];
		if(!diffdisk)
		  {
		    v->values[j][v->validx] -= v->last[j];
		    if(v->values[j][v->validx] < 0) 
		      {
	    /* It IS diffdisk, we should delete/reinterpret the data */
			v->values[j][v->validx] += v->last[j];
			diffdisk = 1;
		      }
		  }
		sum += v->values[j][v->validx];
	      }
	    CHECKMAX(sum);
	    break;
	  case CTXT:
	    v->values[0][v->validx] = sx.v_swtch - v->last[0];
            CHECKMAX(v->values[0][v->validx]);
	    break;
	  case COLL:
	    v->values[0][v->validx] = sx.if_collisions - v->last[0];
            CHECKMAX(v->values[0][v->validx]);
	    break;
	  case LOAD:
	    v->values[0][v->validx] = sx.avenrun[0];
	    while(v->values[0][v->validx] > v->maxval)
	      {
		v->maxval *= 2;
		v->maxval_to_show *= 2;
		if(v->active) redraw = 1;
	      }
	    break;
	  case ERRS:
	    sum  = sx.if_ierrors - v->last[0];
	    sum1 = sx.if_oerrors - v->last[1];
	    v->values[0][v->validx] = sum;
	    v->values[1][v->validx] = sum1;
            CHECKMAX(sum+sum1);
	    break;
	}
    }
  fill_last(&sx);

/* Let's rescale, if necessary */
  for(i = j = 0; values[i].numvalues; i++)
    {
      v = &values[i];
      if(v->special ? v->lastsec : lastsec)
        continue;
      from = (v->validx + 1 - width2 + VALUES_LENGTH(v)) % VALUES_LENGTH(v);
      to   = v->validx;

      if(!((v->special ? v->no_reset : no_reset) || v->pm || (v->validx % 20)))
	redraw += rescale(v, from, to);
    }

  if(redraw)
    {
      inc();
      dopaint(1);
    }
  else
    {
      for(i = 0; values[i].numvalues; i++)
        {
	  v = &values[i];
	  if(v->active && (v->special ? v->lastsec == 0 : lastsec == 0))
	    {
	      DrawPixels(v, &pm, v->validx, width2-1);
	      if(!v->pm)
	        {
		  x = v->xpos * width1; y = v->ypos * height1;
		  XCopyArea(dpy, pm.pm, win, wingc,
			    x, y, width2, height1-fontheight+1, x, y);
		}
	    }
	}
      inc();
      XFlush(dpy);
    }

  return NOTIFY_DONE;
}

static void
FreePM(p)
  struct pm *p;
{
  int i;

  if(p->pm)
    XFreePixmap(dpy, p->pm);
  for(i = 0; i < NUMCOLS; i++)
    if(p->colorgc[i])
      XFreeGC(dpy, p->colorgc[i]);
  if(p->fixed)
    XFreeGC(dpy, p->fixed);
}

static void
CreatePM(p, x, y)
  struct pm *p;
  int x, y;
{
  XGCValues gcv;
  int i;

  FreePM(p);
  p->pm = XCreatePixmap(dpy, win, x, y, depth);

  gcv.font = xv_get(font, XV_XID);

  for(i = 0; i < NUMCOLS; i++)
    {
      gcv.background =  i == WHITE ? defidx[BLACK] : defidx[WHITE];
      gcv.foreground = defidx[i];
      p->colorgc[i] = XCreateGC(dpy, p->pm,
                                        GCFont|GCBackground|GCForeground,&gcv);
    }

  gcv.font = xv_get(fixedfont, XV_XID);
  gcv.background = defidx[WHITE];
  gcv.foreground = defidx[BLACK];
  p->fixed = XCreateGC(dpy, p->pm, GCFont|GCBackground|GCForeground,&gcv);

  XFillRectangle(dpy, p->pm, p->colorgc[WHITE], 0, 0, x, y);
}

static void
realloc_values(v, oldlen, newlen)
  struct values *v;
  int oldlen, newlen;
{
  int j, l, k,
      *np, *op;

  if(oldlen == newlen)
    return;

  if(!v->values) /* Let's initialize */
    {
      v->values = (int **)malloc(sizeof(int *) * v->numvalues);
      for(l = 0; l < v->numvalues; l++)
	v->values[l] = (int *)calloc(sizeof(int), newlen);
      v->last = (int *)malloc(sizeof(int) * (v->type==CPU ? 4 : v->numvalues));
      v->validx = newlen - 1;
      return;
    }

/* Realloc a ringbuffer */
  for(l = 0; l < v->numvalues; l++)
    {
      np = (int *)calloc(sizeof(int), newlen);
      op = v->values[l];
      if(newlen > oldlen)
	{
	  k = newlen - oldlen;
	  for(j = 0; j < k; j++)
	    np[j] = 0;
	  
	  for(k = v->validx; j < newlen; j++, k = (k+1) % oldlen)
	    np[j] = op[k];
	}
      else
	{
	  k = (v->validx + oldlen - newlen + 1) % oldlen;
	  for(j = 0; j < newlen; j++)
	    np[j] = op[(k+j)%oldlen];
	}
      free(op);
      v->values[l] = np;
    }
  v->validx = newlen - 1;
}

static void
DrawText(v, drawtime)
  struct values *v;
  int drawtime;
{
  int j, where, dir, ascent, descent;
  int drone = v->special ? v->draw_one : draw_one;
  int height = (v->ypos+1) * height1;
  XCharStruct or;
  char *ti, buf[16];

  if(drawtime)
    {
      ti = (char *)ctime((time_t *)&drawtime) + 11; ti[8] = '\0';
      if(!pm.width)
        {
	  XTextExtents(fixedfont_info, ti, 8, &dir, &ascent, &descent, &or);
	  v->pm->posx = width2 - 1 - or.width;
	  v->pm->posy = height1 - 1 - descent;
	  v->pm->width = or.width;
        }
      XDrawImageString(dpy, pm.pm, pm.colorgc[BLACK],
		       v->xpos*width1  + v->pm->posx,
		       v->ypos*height1 + v->pm->posy, ti, 8);
      return;
    }

  /* Draw text (cpu) */
  XTextExtents(font_info, v->name, strlen(v->name), &dir,&ascent,&descent,&or);
  XDrawImageString(dpy, pm.pm, pm.colorgc[BLACK],
      v->xpos*width1+2, height-descent-2, v->name, strlen(v->name));
  where = v->xpos*width1 + 2 + or.width;

  /* Draw more text (sys nice usr) */
  for(j = 0; !drone && j < 4 && v->subname[j]; j++)
    {
      char *str = v->subname[j];

      XDrawImageString(dpy, pm.pm, pm.colorgc[j],
		    where + 5, height-descent-1, str, strlen(str));
      XTextExtents(font_info, str, strlen(str),
					&dir,&ascent,&descent,&or);
      where += 5 + or.width;
    }
  sprintf(buf, "%d", v->maxval_to_show == -1 ? v->maxval : v->maxval_to_show);
  XTextExtents(font_info,buf,strlen(buf),&dir,&ascent,&descent,&or);
  XDrawImageString(dpy, pm.pm, pm.colorgc[BLACK],
	     v->xpos * width1 + width2 - 2 - or.width,
	     height - descent - 1, buf, strlen(buf));
}
	      
static void
dopaint(doit)
  int doit;
{
  int x, y, i, j, k, vl, oldwidth2,
      dir, ascent, descent;
  XCharStruct or;
  struct values *v;

  x = xv_get(canvas_paint_window(ca), XV_WIDTH);
  y = xv_get(canvas_paint_window(ca), XV_HEIGHT) -1;

  if(win_x != x || real_win_y != y || doit)
    {
      CreatePM(&pm, x, y);

      win_x = x; win_y = real_win_y = y;
      if(strcmp(host, "localhost"))
        {
	  win_y = real_win_y - fontheight;
	  XTextExtents(font_info, host, strlen(host),
						 &dir, &ascent, &descent, &or);
	  XDrawImageString(dpy, pm.pm, pm.colorgc[BLACK],
	      (x-or.width)/2, real_win_y-descent-2, host, strlen(host));
	}
      oldwidth2 = width2;
      width1  = win_x / columns;
      width2  = width1 - 3;
      height1 = win_y / ((nrcurr_active + columns - 1) / columns);

      for(i = 0; values[i].numvalues; i++)
        {
	  v = &values[i];
	  vl = VALUES_LENGTH(v);
	  realloc_values(v,(v->special?v->scrollback:scrollback)+oldwidth2, vl);

	  if(v->active)
	    {
              DrawText(v, 0);
 
	      k = (v->validx - width2 + 1 + vl) % vl;
	      for(j = 1; j < width2; j++)
	        {
		  DrawPixels(&values[i], &pm, k, j);
		  k = (k+1) % vl;
		}
	    }
	}
    }
  /* Copy the stripes */
  XFillRectangle(dpy, win, wingc, 0, 0, x+1, y+1);
  for(i = 0; values[i].numvalues; i++)
    {
      v = &values[i];
      if(!v->active)
        continue;
      x = v->xpos * width1; y = v->ypos * height1;
      if(!v->pm)
	{
	  XCopyArea(dpy, pm.pm, win, wingc, x, y, width2, height1, x, y);
	}
      else
        {
	  XCopyArea(dpy, pm.pm, win, wingc, 
	    x, y+height1-fontheight+1, width2, fontheight,
	    x, y+height1-fontheight+1);
	  x = v->special ? v->scrollback : scrollback;
	  XCopyArea(dpy, v->pm->pm, win, wingc, 
	    x-v->pm->offset, 0, width2, height1-fontheight+1,
	    v->xpos * width1, v->ypos * height1);
	}
    }
  if(real_win_y - win_y)
    XCopyArea(dpy, pm.pm, win, wingc, 0,win_y,win_x,real_win_y-win_y,0,win_y);
  for(i = 1; i < columns; i++)
    {
      x = (width1 * (i-1)) + width2 + 1;
      XDrawLine(dpy, win, bwingc, x, 0, x, win_y);
    }
  XFlush(dpy);
}

static int
checkbox()
{
  int i, v, oldval = 0, from, to, vl;
  struct values *p;

  v = xv_get(pi_check, PANEL_VALUE);
  if(active)
    {
      oldval |= active->draw_line ? 0 : 1;
      oldval |= active->no_smooth ? 0 : 2;
      oldval |= active->draw_one  ? 4 : 0;
      oldval |= active->no_reset  ? 0 : 8;

      active->draw_line = (v & 1) ? 0 : 1;
      active->no_smooth = (v & 2) ? 0 : 1;
      active->draw_one  = (v & 4) ? 1 : 0;
      if((v&8) && active->no_reset)
        {
	  vl = VALUES_LENGTH(active);
	  from = (active->validx + 1 - width2 + vl) % vl;
	  to   = active->validx-1;
	  rescale(active, from, to);
	}
      active->no_reset  = (v & 8) ? 0 : 1;

      if(!active->draw_line && depth == 1)
	active->draw_one = 1;
      return (v != oldval);
    }
  else
    {
      oldval |= draw_line ? 0 : 1;
      oldval |= no_smooth ? 0 : 2;
      oldval |= draw_one  ? 4 : 0;
      oldval |= no_reset  ? 0 : 8;

      draw_line = !(v & 1);
      no_smooth = !(v & 2);
      draw_one  = (v & 4);
      if((v&8) && no_reset)
        {
	  for(i = 0; values[i].numvalues; i++)
	    {
	      p = &values[i];
	      if(p->special)
	        continue;
	      vl = VALUES_LENGTH(p);
	      from = (p->validx + 1 - width2 + vl) % vl;
	      to   = p->validx-1;
	      rescale(p, from, to);
	    }
	}
      no_reset  = !(v & 8);

      if(!draw_line && depth == 1)
	draw_one = 1;
      return (v != oldval);
    }
}

static int
selection(pi, v, e)
  Panel_item pi;
  int v;
  Event *e;
{
  int i, j, max;
  int oldnr, changed = 0;

  if(!v)
    {
      v = 1;
      xv_set(pi, PANEL_VALUE, v, NULL);
    }

  oldnr = nrcurr_active;
  for(i = 0; values[i].numvalues; i++)
    {
      if(values[i].active && !(v & (1<<i)))
	{
	  for(j = 0; values[j].numvalues; j++)
	    if(values[j].active > values[i].active)
	      values[j].active--;
	  values[i].active = 0;
	  nrcurr_active--;
	  changed++;
	}
      else if(!values[i].active && (v & (1<<i)))
	{
	  for(max = j = 0; values[j].numvalues; j++)
	    if(values[j].active > max)
	      max = values[j].active;
	  values[i].active = max+1;
	  nrcurr_active++;
	  changed++;
	}
    }
  if(oldnr != nrcurr_active)
    {
      int diff, newheight;

      newheight = height1*((nrcurr_active+columns-1)/columns)+real_win_y-win_y;
      diff = xv_get(fr, XV_HEIGHT) - xv_get(canvas_paint_window(ca), XV_HEIGHT);
      xv_set(canvas_paint_window(ca), XV_HEIGHT, newheight+1, NULL);
      xv_set(fr, XV_HEIGHT, newheight+1+diff, NULL);
      if(nrcurr_active < columns)
        {
	  columns = nrcurr_active;
	  diff = xv_get(fr,XV_WIDTH) - xv_get(canvas_paint_window(ca),XV_WIDTH);
	  xv_set(canvas_paint_window(ca),XV_WIDTH,width1*columns, NULL);
	  xv_set(fr, XV_WIDTH, width1*columns+diff, NULL);
        }
    }
    
  return changed;
}

static void
spp()
{
  int i, min, v = xv_get(pi_spp, PANEL_VALUE);

  if(active)
    {
      active->sec_perpixel = v;
      active->lastsec = 0;
    }
  else
    {
      sec_perpixel = v;
      lastsec = 0;
    }

/* Ahem.. correct gcd would be better... */
  min = sec_perpixel;
  for(i = 0; values[i].numvalues; i++)
    if(values[i].special && min != values[i].sec_perpixel)
      {
	if(min % values[i].sec_perpixel == 0)
	  min = values[i].sec_perpixel;
	else if(values[i].sec_perpixel % min != 0)
	  min = 1;
      }
        
  if(min != min_spp)
    {
      min_spp = min;
      do_alarm(min_spp);
    }
}

static int
machine()
{
  char *s = (char *)xv_get(pi_mach, PANEL_VALUE);
  int i, j;
  extern void bzero();
  struct values *v;

  if(!s || !*s)
    s = "localhost";
  if(!strcmp(s, host))
    return 0;
  for(i = 0; values[i].numvalues; i++)
    {
      v = &values[i];
      for(j = 0; j < v->numvalues; j++)
	bzero(v->values[j], sizeof(int) * VALUES_LENGTH(v));
      rescale(v, 0, VALUES_LENGTH(v)-1);
    }
  host = s;
  just_started = 1;

  return 1;
}

static void
set_columns()
{
  int i;

  for(i = 0; values[i].numvalues; i++)
    if(values[i].active)
      {
	values[i].xpos = (values[i].active-1) % columns;
	values[i].ypos = (values[i].active-1) / columns;
      }
}

static int
docolumns()
{
  int i, newheight, newwidth, diffw, diffh;
  if((i = xv_get(pi_cols, PANEL_VALUE)) == columns)
    return 0;
  columns = i > nrcurr_active ? nrcurr_active : i;

  newwidth  = width1 * columns;
  newheight = height1 * ((nrcurr_active+columns-1)/columns) + real_win_y-win_y;
  diffh = xv_get(fr, XV_HEIGHT) - xv_get(canvas_paint_window(ca), XV_HEIGHT);
  diffw = xv_get(fr, XV_WIDTH) - xv_get(canvas_paint_window(ca), XV_WIDTH);

  xv_set(canvas_paint_window(ca),
    XV_HEIGHT, newheight + 1, XV_WIDTH,  newwidth, NULL);
  xv_set(fr, XV_HEIGHT, newheight+diffh+1, XV_WIDTH, newwidth+diffw, NULL);
  return 1;
}

static void
doscrollback()
{
  int i, oldlen = 0, v = xv_get(pi_scbk, PANEL_VALUE);
  struct values *val;

  if(active)
    {
      oldlen = VALUES_LENGTH(active);
      active->special = 1;
      active->scrollback = v;
      realloc_values(active, oldlen, VALUES_LENGTH(active));
    }
  else
    {
      for(i = 0; values[i].numvalues; i++)
        {
	  val = &values[i];
	  if(!(val->special))
	    {
	      oldlen = VALUES_LENGTH(val);
	      break;
	    }
	}
      scrollback = v;
      for(i = 0; values[i].numvalues; i++)
        {
	  val = &values[i];
	  if(!(val->special))
	    realloc_values(val, oldlen, VALUES_LENGTH(val));
	}
    }
}

static int
apply(i, e)
  Panel_item i;
  Event *e;
{
  int redraw = 0;

  if(!active)
    {
      redraw += selection(pi_sels, xv_get(pi_sels, PANEL_VALUE), e);
      redraw += docolumns();
      redraw += machine();
      set_columns();
    }
  doscrollback();
  spp();
  redraw += checkbox();

  if(redraw)
    dopaint(1);
  return XV_OK;
}

static int
hide(i, e)
  Panel_item i;
  Event *e;
{
  xv_set(pfr, XV_SHOW, FALSE, NULL);
  return XV_OK;
}

static int
nosp_proc()
{
  int i;

  if(active)
    active->special = 0;
  else
    {
      for(i = 0; values[i].numvalues; i++)
        values[i].special = 0;
    }
  dopaint(1);
  return XV_OK;
}

static int
dofork()
{
  switch(fork())
    {
      case -1:
        perror("fork");
	return XV_OK;
      case 0:
/* Not very nice, but xview isn't reentrant */
	execvp(argv[0], argv);
        perror(argv[0]);
	return XV_OK;
      default:
	return XV_OK;
    }
}

static void
create_pfr()
{
  int i, v, y;

    
  pfr = xv_create(fr, FRAME_CMD, FRAME_LABEL, PROGNAME, NULL);
  pn = xv_get(pfr, FRAME_CMD_PANEL);
  xv_set(pn, WIN_ROW_GAP, xv_get(pn, WIN_ROW_GAP)/3, NULL);

  for(v = i = 0; values[i].numvalues; i++)
    if(values[i].active)
      v |= (1 << i);
  pi_sels = xv_create(pn, PANEL_TOGGLE,
    XV_X, xv_col(pn, 1),
    XV_Y, xv_row(pn, 0),
    PANEL_CHOICE_STRINGS,
      values[0].name, values[1].name, values[2].name, values[3].name,
      values[4].name, values[5].name, values[6].name, values[7].name,
      values[8].name, values[9].name, NULL,
    PANEL_CHOICE_NCOLS, 5,
    PANEL_VALUE, v,
    NULL);


  y = xv_get(pi_sels, XV_Y) + xv_get(pi_sels, XV_HEIGHT) + xv_row(pn, 1);

  pi_check = xv_create(pn, PANEL_CHECK_BOX,
    XV_Y, y,
    PANEL_LAYOUT, PANEL_VERTICAL,
    PANEL_CHOICE_STRINGS,
      "Solid graph", "Smooth if solid", "Show sum only", "Autoreset",
      NULL,
    NULL);
  xv_set(pi_check, 
    XV_X, xv_get(pi_sels, XV_X) + xv_get(pi_sels, XV_WIDTH) -
          xv_get(pi_check, XV_WIDTH),
    NULL);
  
  pi_mach = xv_create(pn, PANEL_TEXT,
    XV_Y, y, XV_X, xv_get(pi_sels, XV_X),
    PANEL_LABEL_STRING, "Machine:",
    PANEL_VALUE, host,
    PANEL_VALUE_DISPLAY_LENGTH, 16,
    NULL);
  
  pi_spp = xv_create(pn, PANEL_NUMERIC_TEXT,
    XV_Y, xv_get(pi_mach, XV_Y) + xv_get(pi_mach, XV_HEIGHT) + xv_row(pn, 1)/2,
    XV_X, xv_get(pi_sels, XV_X),
    PANEL_LABEL_STRING, "Sample time (sec):",
    PANEL_VALUE, sec_perpixel,
    PANEL_MIN_VALUE, 1,
    PANEL_VALUE_DISPLAY_LENGTH, 4,
    NULL);

  pi_scbk = xv_create(pn, PANEL_NUMERIC_TEXT,
    XV_Y, xv_get(pi_spp, XV_Y) + xv_get(pi_spp, XV_HEIGHT) + xv_row(pn, 1)/2,
    XV_X, xv_get(pi_sels, XV_X),
    PANEL_LABEL_STRING, "Scrollback (values):",
    PANEL_MIN_VALUE, 0,
    PANEL_MAX_VALUE, 20000,
    PANEL_VALUE, scrollback,
    PANEL_VALUE_DISPLAY_LENGTH, 5,
    NULL);

  pi_cols = xv_create(pn, PANEL_NUMERIC_TEXT,
    XV_Y, xv_get(pi_scbk, XV_Y) + xv_get(pi_scbk, XV_HEIGHT) + xv_row(pn, 1)/2,
    XV_X, xv_get(pi_sels, XV_X),
    PANEL_LABEL_STRING, "Columns:",
    PANEL_INACTIVE, TRUE,
    PANEL_MAX_VALUE, 10, PANEL_MIN_VALUE, 1,
    PANEL_VALUE, columns,
    PANEL_VALUE_DISPLAY_LENGTH, 2,
    NULL);
  
  pi_nosp = xv_create(pn, PANEL_BUTTON,
    XV_Y, xv_get(pi_cols, XV_Y) + xv_get(pi_cols, XV_HEIGHT) + xv_row(pn, 1)/2,
    XV_X, xv_get(pi_sels, XV_X),
    PANEL_LABEL_STRING, "No special flags",
    PANEL_NOTIFY_PROC, nosp_proc,
    NULL);
  pi_fork = xv_create(pn, PANEL_BUTTON,
    XV_Y, xv_get(pi_nosp, XV_Y),
    XV_X, xv_get(pi_nosp, XV_X) + xv_get(pi_nosp, XV_WIDTH) + xv_col(pn, 1),
    PANEL_LABEL_STRING, "Fork",
    PANEL_NOTIFY_PROC, dofork,
    NULL);
  pi_msg = xv_create(pn, PANEL_MESSAGE,
    XV_Y, xv_get(pi_nosp, XV_Y) + xv_get(pi_nosp, XV_HEIGHT) + xv_row(pn, 1)/3,
    XV_X, xv_get(pi_sels, XV_X),
    NULL);

  window_fit_width(pn);
  pi_apply = xv_create(pn, PANEL_BUTTON,
    XV_Y, xv_get(pi_fork, XV_Y) + xv_get(pi_fork, XV_HEIGHT) + xv_row(pn, 1),
    PANEL_LABEL_STRING, "Apply",
    PANEL_NOTIFY_PROC, apply,
    NULL);
  xv_set(pi_apply,
    XV_X, xv_get(pn, XV_WIDTH)/3 - xv_get(pi_apply, XV_WIDTH)/2,
    NULL);

  pi_hide = xv_create(pn, PANEL_BUTTON,
    XV_Y, xv_get(pi_apply, XV_Y),
    PANEL_LABEL_STRING, "Hide window",
    PANEL_NOTIFY_PROC, hide,
    NULL);
  xv_set(pi_hide,
    XV_X, 2*xv_get(pn, XV_WIDTH)/3 - xv_get(pi_hide, XV_WIDTH)/2,
    NULL);

  window_fit(pn);
  window_fit(pfr);
}

static void
grayout(val)
  struct values *val;
{
  int v = val ? 1 : 0;
  char buf[256];

  xv_set(pi_sels, PANEL_INACTIVE, v, NULL);
  xv_set(pi_mach, PANEL_INACTIVE, v, NULL);
  xv_set(pi_cols, PANEL_INACTIVE, v, NULL);
  xv_set(pi_fork, PANEL_INACTIVE, v, NULL);
  if(strcmp(host, "localhost"))
    sprintf(buf, "%s (%s)", val ? active->name : PROGNAME, host);
  else
    sprintf(buf, "%s", val ? active->name : PROGNAME);
  xv_set(pfr, FRAME_LABEL, buf, NULL);

  v = 0;
  if(val && val->special)
    {
      if(!val->draw_line) v |= 1; if(!val->no_smooth) v |= 2;
      if(val->draw_one) v |= 4;   if(!val->no_reset) v |= 8;
    }
  else
    {
      if(!draw_line) v |= 1; if(!no_smooth) v |= 2;
      if(draw_one) v |= 4;   if(!no_reset) v |= 8;
    }
  xv_set(pi_check, PANEL_VALUE, v, NULL);
  xv_set(pi_spp,
    PANEL_VALUE, (val && val->special) ? val->sec_perpixel : sec_perpixel,
    NULL);
  xv_set(pi_scbk,
    PANEL_VALUE, (val && val->special) ? val->scrollback : scrollback,
    NULL);
  xv_set(pi_msg, PANEL_LABEL_STRING, "", NULL);
}

static struct values *
postoval(e)
  Event *e;
{
  int i, j = event_y(e)/height1, k = event_x(e)/width1;

  for(i = 0; values[i].numvalues; i++)
    if(values[i].active && values[i].xpos == k && values[i].ypos == j)
      break;
  if(!values[i].numvalues)
    return 0;
  return &values[i];
}

static void
reset_scrolled(ifzero)
  int ifzero;
{
  int i, xx, yy;
  struct values *v;

  for(i = 0; values[i].numvalues; i++)
    if(values[i].pm)
      {
	if(!(ifzero && values[i].pm->offset))
	  {
	    v = &values[i];

	    xx = v->xpos * width1; yy = v->ypos * height1;

	    XFillRectangle(dpy, pm.pm, pm.colorgc[WHITE], 
	      xx + v->pm->posx, yy + height1 - fontheight + 1,
	      v->pm->width, fontheight);
	    DrawText(v, 0);

	    XCopyArea(dpy, pm.pm, win, wingc, 
	      xx + v->pm->posx, yy + height1 - fontheight,
	      v->pm->width,fontheight,
	      xx + v->pm->posx, yy + height1 - fontheight);

	    v->selected = 0;
	    FreePM(v->pm);
	    free(v->pm);
	    v->pm = 0;
	  }
      }
    else if(values[i].selected)
      values[i].selected = 0;
}

static void
event_proc(w, e)
  Xv_window w;
  Event *e;
{
  static int lastx, dragged;
  struct values *v;
  int i, x, y;

  if(event_is_ascii(e))
    {
      if(event_is_up(e))
	return;
      switch(event_action(e))
        {
	  case 'q': case 'Q':
	    exit(0);
	    break;
	  case 's': case 'S':
	    draw_line = !draw_line;
	    if(!draw_line && depth == 1)
	      draw_one = 1;
	    dopaint(1);
	    break;
	  case 'o': case 'O':
	    no_smooth = !no_smooth;
	    dopaint(1);
	    break;
	  case '1':
	    draw_one = !draw_one;
	    if(!draw_line && depth == 1)
	      draw_one = 1;
	    dopaint(1);
	    break;
	  case '?': case 'h':
	    if(!pfr)
	      create_pfr();
	    xv_set(pfr, XV_SHOW, TRUE, NULL);
	    break;
	  case 10: /* CR/NL */
	  case 13:
	    reset_scrolled(0);
	    dopaint(1);
	    return;
	}
      return;
    }
  switch(event_action(e))
    {
      case WIN_RESIZE:
        lost_connection = 0;
	x = e->ie_xevent->xconfigure.width;
	y = e->ie_xevent->xconfigure.height;
	if(x <= win_x && y <= real_win_y)
	  {
	    reset_scrolled(0);
	    dopaint(0);
	  }
	break;
      case WIN_REPAINT:
	x = xv_get(canvas_paint_window(ca), XV_WIDTH);
	y = xv_get(canvas_paint_window(ca), XV_HEIGHT) -1;
	if(x != win_x || y != real_win_y)
	  reset_scrolled(0);
	dopaint(0);
        break;
      case LOC_DRAG:
        if(!dragged)
	  return;
	dragged = 2;
	for(i = 0; values[i].numvalues; i++)
	  if(values[i].selected)
	    {
	      int k, off;

	      v = &values[i];

	      if(!v->pm)
		{
		  int mx, my, vl, j, k;

		  vl = VALUES_LENGTH(v);
		  if(!v->pm)
		    v->pm = (struct pm *)calloc(1, sizeof(pm));
		  CreatePM(v->pm, vl, height1-fontheight+1);
		  v->pm->width = v->pm->offset = 0;
		  v->pm->starttime = time(0);
		  mx = v->xpos; my = v->ypos; 
		  v->xpos = v->ypos = 0; 

		  k = (v->validx + 1) % vl;
		  for(j = 1; j < vl; k %= vl)
		    DrawPixels(v, v->pm, k++, j++);
		  v->xpos = mx; v->ypos = my; 
		}

	      off = v->pm->offset + event_x(e) - lastx;
	      k = v->special ? v->scrollback : scrollback;
	      if(off < 0) off = 0;
	      if(off > k) off = k;
	      if(off != v->pm->offset)
	        {
		  int xx = v->xpos * width1,
		      yy = v->ypos * height1;

		  v->pm->offset = off;
		  DrawText(v, v->pm->starttime - 
		     (off * (v->special ? v->sec_perpixel : sec_perpixel)));
		  XCopyArea(dpy, v->pm->pm, win, wingc, 
		    k - off, 0, width2, height1-fontheight+1, xx, yy);
		  XCopyArea(dpy, pm.pm, win, wingc, 
		    xx + v->pm->posx, yy + height1 - fontheight,
		    v->pm->width,fontheight,
		    xx + v->pm->posx, yy + height1 - fontheight);
		}
	    }
	lastx = event_x(e);
        break;
      case ACTION_ADJUST:
      case MS_MIDDLE:
        if(event_is_up(e) || !(v = postoval(e)))
	  return;
	v->selected = 1;
        break;
      case ACTION_SELECT:
      case MS_LEFT:
        if(event_is_down(e))
	  {
	    lastx = event_x(e);
	    dragged = 1;
	    if(!(v = postoval(e)))
	      return;
	    if(!v->selected)
	      {
	        for(i = 0; values[i].numvalues; i++)
		  values[i].selected = 0;
	        v->selected = 1;
	      }
	  }
	else
	  {
	    char buf[128];

	    if(dragged)
	      {
		lastx = -1;
		if(dragged == 2)
		  {
		    reset_scrolled(1);
		    dragged = 0;
		    return;
		  }
		dragged = 0;
	      }
	    if(!(v = postoval(e)))
	      return;
	    y = VALUES_LENGTH(v);
	    x = (v->validx - ((width1 * v->xpos) + width2 - event_x(e)) - 
						      (v->pm ? v->pm->offset : 0) + y) % y;
	    sprintf(buf, "%s: ", v->name);
	    if(v->type == LOAD)
	      {
		sprintf(buf+strlen(buf), "%.2f",(double)v->values[0][x]/256.0);
	      }
	    else
	      {
		for(y = 0; y < v->numvalues; y++)
		  sprintf(buf+strlen(buf), "%s %d ", 
		    v->subname[y] ? v->subname[y] : "", v->values[y][x]);
	      }
	    if(pi_msg)
	      xv_set(pi_msg, PANEL_LABEL_STRING, buf, NULL);
	  }
        break;
      case ACTION_MENU:
      case MS_RIGHT:
	if(event_is_up(e))
	  return;
	if(!pfr)
	  create_pfr();
	if(event_shift_is_down(e))
	  {
	    active = postoval(e);
	    grayout(active);
	  }
	else
	  {
	    active = NULL;
	    grayout(0);
	  }
	xv_set(pfr, XV_SHOW, TRUE, NULL);
	break;
    }

}
  
void
getdefault(name, value)
  char *name;
  int *value;
{
  static char none[] = "NONE";
  char *str, buffer[128];

  sprintf(buffer, "%s.%s", PROGNAME, name);
  str = defaults_get_string(buffer, buffer, none);
  if(!strcmp(str, none))
    return;
  if(!strcmp(str, "True") || !strcmp(str, "true") || 
     !strcmp(str, "Yes") || !strcmp(str, "yes"))
   {
     *value = 1;
     return;
   }
  if(*str == '#')
    *value = strtol(str+1, NULL, 16);
  else
    *value = strtol(str, NULL, 0);
}

void
main(ac, av)
  int ac;
  char *av[];
{
  XColor col;
  XGCValues gcv;
  int i, rows, len;

  argv = (char **)calloc(ac+1, sizeof(char *));
  for(i = 0; i < ac; i++)
    argv[i] = strdup(av[i]);
  xv_init(XV_INIT_ARGC_PTR_ARGV, &ac, av, NULL);

/* X - Resources */
  for(i = 0; opts[i].name; i++)
    getdefault(opts[i].name, opts[i].param);
  
/* Argument parsing... */
  for(av++; ac > 1; ac--, av++)
    {
      if(*av[0] != '-')
        {
	  host = av[0];
	  continue;
	}
      for(i = 0; opts[i].name; i++)
        if(!strcmp(av[0]+1, opts[i].name))
	  {
	    if(opts[i].needsarg)
	      {
		if(*av[1] == '#')
		  *opts[i].param = strtol(av[1]+1, NULL, 16);
		else
		  *opts[i].param = strtol(av[1], NULL, 0);
		av++; ac--;
	      }
	    else
	      *opts[i].param = 1;
	    goto next;
	  }
      if(!strcmp(av[0]+1, "a"))
        {
	  for(i = 0; values[i].numvalues; i++)
	    if(!values[i].active)
	      values[i].active = ++nrcurr_active;
	  goto next;
	}
      for(i = 0; values[i].numvalues; i++)
        if(!strcmp(av[0]+1, values[i].name))
	  {
	    if(!values[i].active)
	      values[i].active = ++nrcurr_active;
	    goto next;
	  }

/* No valid option: print usage */
      fprintf(stderr, "Usage: %s [-a]\n\t", PROGNAME);
      for(len = 8,i = 0; opts[i].name; i++)
        {
	  fprintf(stderr, "[-%s%s] ",opts[i].name,opts[i].needsarg?" <arg>":"");
	  len += strlen(opts[i].name) + 4 + (opts[i].needsarg ? 6 : 0);
	  if(len > 60)
	    {
	      fprintf(stderr, "\n\t");
	      len = 8;
	    }
	}
      for(i = 0; values[i].numvalues; i++)
        {
	  fprintf(stderr, "[-%s] ", values[i].name);
	  len += strlen(values[i].name) + 4;
	  if(len > 60)
	    {
	      fprintf(stderr, "\n\t");
	      len = 8;
	    }
	}
      fprintf(stderr, "[host]\n\nKeyboard accelerators: q,s,o,1,?,NL\n");
      exit(-1);
next:
      continue;
    }

  if(!host)
    host = "localhost";
  if(!nrcurr_active)
    {
      nrcurr_active = 1;
      values[0].active =1;
    }
  if(scrollback < 0) scrollback = 0;
  if(sec_perpixel < 1) sec_perpixel = 1;
  if(columns < 1) columns = 1;
  if(columns > 10) columns = 10;
  min_spp = sec_perpixel;
  set_columns();

  rows = (nrcurr_active + columns - 1) / columns;
  fr = xv_create(0, FRAME,
    FRAME_LABEL, PROGNAME,
    XV_WIDTH,  (columns > 3 ? columns * 100 : columns * 200) + 3,
    XV_HEIGHT, (rows > 3 ? rows * 60 : rows * 100) + 10,
    FRAME_SHOW_FOOTER, FALSE,
    FRAME_SHOW_RESIZE_CORNER, TRUE,
    FRAME_SHOW_HEADER, FALSE,
    NULL);

  do_alarm(min_spp);

  ca = xv_create(fr, CANVAS,
    XV_X, 0, XV_Y, 0,
    CANVAS_RETAINED,       FALSE,
    CANVAS_AUTO_CLEAR,     FALSE,
    CANVAS_AUTO_SHRINK,    TRUE,
    CANVAS_AUTO_EXPAND,    TRUE,
    NULL);
  window_fit(fr);
  xv_set(canvas_paint_window(ca),
    WIN_EVENT_PROC, event_proc,
    WIN_CONSUME_EVENTS,
      WIN_ASCII_EVENTS, WIN_RESIZE, WIN_REPAINT,
      WIN_MOUSE_BUTTONS, LOC_DRAG, NULL,
    NULL);

  dpy = (Display *)xv_get(fr, XV_DISPLAY);
  win = xv_get(canvas_paint_window(ca), XV_XID);
  screen = xv_get(xv_get(fr, XV_SCREEN), SCREEN_NUMBER);
  font = xv_get(fr, XV_FONT);
  fontheight = xv_get(font, FONT_SIZE) + 4;
  font_info =(XFontStruct *)xv_get(font, FONT_INFO);
  fixedfont = xv_find(fr, FONT,
    FONT_FAMILY, FONT_FAMILY_LUCIDA_FIXEDWIDTH,
    FONT_SIZE, xv_get(font, FONT_SIZE), NULL);
  if(!fixedfont)
    {
      fixedfont = font;
      fixedfont_info = font_info;
    }
  else
    fixedfont_info =(XFontStruct *)xv_get(fixedfont, FONT_INFO);


/* ICON */
  {
    Pixmap pm;
    Server_image sim;
    Icon icon;

    pm = XCreatePixmapFromBitmapData(dpy, win,
       icon_bits, icon_width, icon_height, 0, 1, 1);
    sim = (Server_image)xv_create(0,
       SERVER_IMAGE, SERVER_IMAGE_PIXMAP, pm, NULL);
    icon = xv_create(fr, ICON, XV_WIDTH, icon_width, XV_HEIGHT, icon_height,
       ICON_IMAGE, sim, ICON_MASK_IMAGE, sim, NULL);
    xv_set(fr, FRAME_ICON, icon, NULL);
  }

  defidx[0] = defidx[1] = defidx[2] = defidx[3] = 
				      defidx[BLACK] = BlackPixel(dpy, screen);
  defidx[WHITE] = WhitePixel(dpy, screen);

  if((depth = xv_get(fr, XV_DEPTH)) > 1)
    {
      col.flags = DoRed|DoGreen|DoBlue;

      for(i = 0; i < NUMCOLS; i++)
	{
	  col.pixel = BlackPixel(dpy, screen);
	  col.red   = (defcol[i] & 0xff0000) >> 8;
	  col.green = (defcol[i] & 0x00ff00);
	  col.blue  = (defcol[i] & 0x0000ff) << 8;
	  XAllocColor(dpy, DefaultColormap(dpy, screen), &col);
	  defidx[i] = col.pixel;
	}
    }
  gcv.foreground = defidx[WHITE];
  wingc = XCreateGC(dpy, win, GCForeground,&gcv);
  gcv.foreground = defidx[BLACK];
  bwingc = XCreateGC(dpy, win, GCForeground,&gcv);

  if(!draw_line && depth == 1)
    draw_one = 1;

  xv_main_loop(fr);
}
