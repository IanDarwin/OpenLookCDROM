/* Linux meminfo, using UIT toolkit and /proc
 * Copyright (C) 1993  Kenneth Osterberg
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 */

#include <uit/BaseWindow.h>
#include <uit/ComponentDisplay.h>
#include <uit/TextItem.h>
#include <uit/Notifier.h>
#include <uit/Gauge.h>
#include <uit/NumericInput.h>
#include <uit/UIObject.h>

#include <xview/notify.h>

#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <math.h>

#define VERSION "v1.1"

struct meminfo {
  int meminfofd;
  int total;
  int used;
  int free;
  int shared;
  int buffers;
  int swaptotal;
  int swapused;
  int swapfree;
  int totmegs;
  int swapmegs;
  int bytes_per_tick;
  int swapbytes_per_tick;
};

struct loadinfo {
  int loadavgfd;
  int uptimefd;
  double load1;
  double load5;
  double load15;
  int uptime;
  int updays;
  int uphours;
  int upminutes;
  int load_per_tick;
};

const int WINWID = 560;
const int WINHGT = 224;

const int G_Y = 36;
const int G_H = 124;
const int G_USED_X =  8;
const int G_SHARED_X = G_USED_X + 66;
const int G_BUFFERS_X = G_SHARED_X + 78;
const int G_SWAP_X = G_BUFFERS_X + 104;
const int G_LOAD1_X = G_SWAP_X + 92;
const int G_LOAD5_X = G_LOAD1_X + 66;
const int G_LOAD15_X = G_LOAD5_X + 66;
const int T_VALUE_Y = G_Y + G_H + 24;

struct itimerval timer;
const int PERIODSECS = 30;
int periodic = PERIODSECS + 1;
int freq = 2;
 
/**************************************************/
BaseWindow w("Linux Meminfo  " VERSION);
Gauge g_used("Used"), g_shared("Shared"), g_buffers("Buffers");
Gauge g_swap("Swap");
Gauge g_load1("1 Min"), g_load5("5 Min"), g_load15("15 Min");
TextItem t_usedvalue, t_sharedvalue, t_buffersvalue;
TextItem t_swapvalue;
TextItem t_load1value, t_load5value, t_load15value;
TextItem t_meminfo, t_swapinfo, t_loadinfo;
TextItem t_uptime;
NumericInput n_time("Update freq. ");
/**************************************************/
void freqHandler(UIObject *);
Notify_value update(void);
void updateTextValues(struct meminfo *);
void roundMeg(int, int &, int &);
void periodicUpdate(void);
struct loadinfo *readLoadinfo(void);
struct meminfo *readMeminfo(void);
/**************************************************/
main (int argc, char **argv) {
  char iconfile[128];

  sprintf(iconfile, "%s/include/images/perfmeter.icon",
          getenv("OPENWINHOME"));

  if (chdir("/proc")) {
    perror("chdir() to /proc failed");
    exit(1);
  }

// Create window
  w.initUI(argc, argv);
  w.setWidth(WINWID); w.setHeight(WINHGT);
  w.setDisplayFooter(FALSE);
  w.setResizable(FALSE);
  w.setIcon(iconfile);
  w.setIconLabel("Meminfo");
  w.show(TRUE);

// Create canvas
  ComponentDisplay c(TRUE);
  c.setLocation(0, 0);
  c.setExtendToEdge(WIDTH);
  c.setExtendToEdge(HEIGHT);
  c.setHelp("meminfo:Canvas");
  w.addDisplay(c);

// Create NumericInput for update timer
  n_time.setLocation( 8, WINHGT - 20);
  n_time.setMinValue(0);
  n_time.setMaxValue(60);
  n_time.setValue(2);
  n_time.setInputDisplayLength(2);
  n_time.setNotifyHandler(freqHandler);
  n_time.setHelp("meminfo:UpdateInput");
  c.addComponent(n_time);

// Create TextItem for uptime
  t_uptime.setLocation(G_BUFFERS_X + 48, WINHGT - 20);
  t_uptime.setLabel("Uptime: ");
  t_uptime.setHelp("meminfo:UptimeText");
  c.addComponent(t_uptime);

// Create Memory Gauges

  g_used.setDisplayRange(FALSE);
  g_shared.setDisplayRange(FALSE);
  g_buffers.setDisplayRange(FALSE);
  g_swap.setDisplayRange(FALSE);
  g_load1.setDisplayRange(FALSE);
  g_load5.setDisplayRange(FALSE);
  g_load15.setDisplayRange(FALSE);
  
  g_used.setLocation(G_USED_X, G_Y);
  g_shared.setLocation(G_SHARED_X, G_Y);
  g_buffers.setLocation(G_BUFFERS_X, G_Y);
  g_swap.setLocation(G_SWAP_X, G_Y);
  g_load1.setLocation(G_LOAD1_X, G_Y);
  g_load5.setLocation(G_LOAD5_X, G_Y);
  g_load15.setLocation(G_LOAD15_X, G_Y);

  g_used.setOrientation(VERTICAL);
  g_shared.setOrientation(VERTICAL);
  g_buffers.setOrientation(VERTICAL);
  g_swap.setOrientation(VERTICAL);
  g_load1.setOrientation(VERTICAL);
  g_load5.setOrientation(VERTICAL);
  g_load15.setOrientation(VERTICAL);

  g_used.setGaugeWidth(G_H);
  g_shared.setGaugeWidth(G_H);
  g_buffers.setGaugeWidth(G_H);
  g_swap.setGaugeWidth(G_H);
  g_load1.setGaugeWidth(G_H);
  g_load5.setGaugeWidth(G_H);
  g_load15.setGaugeWidth(G_H);

  g_shared.setMinValue(0);
  g_buffers.setMinValue(0);
  g_swap.setMinValue(0);
  g_load1.setMinValue(0);
  g_load5.setMinValue(0);
  g_load15.setMinValue(0);

  g_used.setMaxValue(256);
  g_shared.setMaxValue(256);
  g_buffers.setMaxValue(256);
  g_swap.setMaxValue(256);
  g_load1.setMaxValue(256);
  g_load5.setMaxValue(256);
  g_load15.setMaxValue(256);

  g_used.setHelp("meminfo:UsedGauge");
  g_shared.setHelp("meminfo:SharedGauge");
  g_buffers.setHelp("meminfo:BuffersGauge");
  g_swap.setHelp("meminfo:SwapGauge");
  g_load1.setHelp("meminfo:Load1Gauge");
  g_load5.setHelp("meminfo:Load5Gauge");
  g_load15.setHelp("meminfo:Load15Gauge");

  struct meminfo *mi = readMeminfo();

  g_used.setTicks(mi->totmegs + 1);
  g_shared.setTicks(mi->totmegs + 1);
  g_buffers.setTicks(mi->totmegs + 1);
  g_swap.setTicks(mi->swapmegs + 1);
  g_load1.setTicks(5);
  g_load5.setTicks(5);
  g_load15.setTicks(5);

  c.addComponent(g_used);
  c.addComponent(g_shared);
  c.addComponent(g_buffers);
  c.addComponent(g_swap);
  c.addComponent(g_load1);
  c.addComponent(g_load5);
  c.addComponent(g_load15);

  t_usedvalue.setLocation(G_USED_X + 24, T_VALUE_Y);
  c.addComponent(t_usedvalue);
  t_sharedvalue.setLocation(G_SHARED_X + 32, T_VALUE_Y);
  c.addComponent(t_sharedvalue);
  t_buffersvalue.setLocation(G_BUFFERS_X + 32, T_VALUE_Y);
  c.addComponent(t_buffersvalue);
  t_swapvalue.setLocation(G_SWAP_X + 24, T_VALUE_Y);
  c.addComponent(t_swapvalue);
  t_load1value.setLocation(G_LOAD1_X + 32, T_VALUE_Y);
  c.addComponent(t_load1value);
  t_load5value.setLocation(G_LOAD5_X + 32, T_VALUE_Y);
  c.addComponent(t_load5value);
  t_load15value.setLocation(G_LOAD15_X + 40, T_VALUE_Y);
  c.addComponent(t_load15value);

  t_usedvalue.setHelp("meminfo:UsedValue");
  t_sharedvalue.setHelp("meminfo:SharedValue");
  t_buffersvalue.setHelp("meminfo:BuffersValue");
  t_swapvalue.setHelp("meminfo:SwapValue");
  t_load1value.setHelp("meminfo:Load1Value");
  t_load5value.setHelp("meminfo:Load5Value");
  t_load15value.setHelp("meminfo:Load15Value");

  t_meminfo.setLocation(8, (G_Y >> 1) - 4);
  t_swapinfo.setLocation(G_BUFFERS_X + 56, (G_Y >> 1) - 4); 
  t_loadinfo.setLocation(G_LOAD1_X + 64, (G_Y >> 1) - 4);

  t_meminfo.setHelp("meminfo:MemInfo");
  t_swapinfo.setHelp("meminfo:SwapInfo");
  t_loadinfo.setHelp("meminfo:LoadInfo");

  c.addComponent(t_meminfo);
  c.addComponent(t_swapinfo);
  c.addComponent(t_loadinfo);

  update();

// Create text labels
  char memlabel[64], swaplabel[64];
  sprintf(memlabel, "Memory   (total %d)", mi->total);
  sprintf(swaplabel, "Swap (total %d)", mi->swaptotal);
  t_meminfo.setLabel(memlabel);
  t_swapinfo.setLabel(swaplabel);
  t_loadinfo.setLabel("Load Averages");

// Create objects so that we have a frame to attach the timer to
  Notifier n;
  n.createXViewObjects ();

  timer.it_value.tv_sec = 1;
  timer.it_value.tv_usec = 0;
  timer.it_interval.tv_sec = 1;
  timer.it_interval.tv_usec = 0;

// Start the timer
  unsigned long frame = (unsigned long)w.getXViewObject();
  if (frame == 0) {
    fprintf(stderr, "Error: XView frame not set for timer init\n");
    exit(1);
  }
  notify_set_itimer_func(frame, update, ITIMER_REAL, &timer, NULL);

// Realize it
  n.start();
  exit(0);
}
/**************************************************/
void freqHandler(UIObject *obj) {
/* Called when user changes NumericInput */

  ((NumericInput *)obj)->getValue(freq);
  if (freq > 0) {
    timer.it_value.tv_sec = freq;
    timer.it_value.tv_usec = 0;
    timer.it_interval.tv_sec = freq;
    timer.it_interval.tv_usec = 0;
  }
  else {
    timer.it_value.tv_sec = 0;
    timer.it_value.tv_usec = 500000L;
    timer.it_interval.tv_sec = 0;
    timer.it_interval.tv_usec = 500000L;
  }
  unsigned long frame = (unsigned long)w.getXViewObject();
  if (frame == 0) {
    fprintf(stderr, "Error: XView frame not set in freqHandler\n");
    exit(1);
  }
  notify_set_itimer_func(frame, update, ITIMER_REAL, &timer, NULL);
} 
/**************************************************/
Notify_value update(void) {
/* Called to refresh the data */

  struct meminfo *m = readMeminfo();
  updateTextValues(m);
  g_used.setValue(m->used / m->bytes_per_tick);
  g_shared.setValue(m->shared / m->bytes_per_tick);
  g_buffers.setValue(m->buffers / m->bytes_per_tick);
  if (m->swapbytes_per_tick != 0)
    g_swap.setValue(m->swapused / m->swapbytes_per_tick);
  else
    g_swap.setValue(0);

  periodic += (freq) ? freq : 1;
  if (periodic > PERIODSECS) {
    periodicUpdate();
    periodic = 0;
  }
  return (notify_value)NOTIFY_DONE;
}
/**************************************************/
void updateTextValues(struct meminfo *m) {
/* Update the TextItems below the gauges which show
 * the numeric values depicted by the gauges */
  char str[16];
  int n, d;

  roundMeg(m->used, n, d);
  sprintf(str, "%d.%d MB", n, d);
  t_usedvalue.setLabel(str);
  roundMeg(m->shared, n, d);
  sprintf(str, "%d.%d MB", n, d);
  t_sharedvalue.setLabel(str);
  roundMeg(m->buffers, n, d);
  sprintf(str, "%d.%d MB", n, d);
  t_buffersvalue.setLabel(str);
  roundMeg(m->swapused, n, d);
  sprintf(str, "%d.%d MB", n, d);
  t_swapvalue.setLabel(str);
}
/**************************************************/
void roundMeg(int val, int &n, int &d) {
/* Turn value to megabyte integer and decimal
 * parts. (Incorrectly) */
  n = val >> 20;
  d = (val & 0xf0000) >> 16;
  switch (d) {
  case 0: case 1:  d = 0; break;
  case 2: case 3:  d = 1; break;
  case 4:          d = 2; break;
  case 5: case 6:  d = 3; break;
  case 7:          d = 4; break;
  case 8: case 9:  d = 5; break;
  case 10: case 11:d = 6; break;
  case 12:         d = 7; break;
  case 13: case 14:d = 8; break;
  case 15:         d = 9; break;
  default:
    fprintf(stderr, "roundMeg() Shouldn't happen\n");
    exit(1);
  }
}
/**************************************************/
void periodicUpdate(void) {
/* Called to update the uptime and loadaverages
 * It is called more seldom than the memory values,
 * because the uptime and loadaverages do not change
 * as often. */
  char upstr[64];
  char loadstr[12];
  struct loadinfo *li;

  li = readLoadinfo();
  sprintf(upstr, "Uptime: %d %s  %d %s  %d %s",
          li->updays, (li->updays == 1) ? "Day" : "Days",
          li->uphours, (li->uphours == 1) ? "Hour" : "Hours",
          li->upminutes, (li->upminutes == 1) ? "Minute" : "Minutes");
  t_uptime.setLabel(upstr);

  g_load1.setValue((int)(li->load_per_tick * li->load1));
  g_load5.setValue((int)(li->load_per_tick * li->load5));
  g_load15.setValue((int)(li->load_per_tick * li->load15));

  sprintf(loadstr, "%1.2f", li->load1);
  t_load1value.setLabel(loadstr);
  sprintf(loadstr, "%1.2f", li->load5);
  t_load5value.setLabel(loadstr);
  sprintf(loadstr, "%1.2f", li->load15);
  t_load15value.setLabel(loadstr);
}
/**************************************************/
struct loadinfo *readLoadinfo(void) {
/* Read /proc/loadavg and /proc/uptime, and return
 * the new values. */
  static struct loadinfo load = { 0, 0 };
  char buf[4097];
  char *tok;
  int cnt;

  if (load.loadavgfd <= 0)
    load.loadavgfd = open("loadavg", O_RDONLY);
  else
    lseek(load.loadavgfd, 0, SEEK_SET);
  if (load.loadavgfd <= 0) {
    fprintf(stderr, "Cannot open /proc/loadavg\n");
    exit(1);
  }
  cnt = read(load.loadavgfd, buf, 4096);
  if (cnt <= 0) {
    fprintf(stderr, "Read failure on /proc/loadavg\n");
    exit(1);
  }
  buf[cnt] = '\0';
  tok = strtok(buf, " \t\r\n");
  if (tok == NULL) {
    fprintf(stderr, "Meminfo: Unknown format on /proc/loadavg\n");
    exit(1);
  }
  load.load1 = atof(tok);
  tok = strtok(NULL, " \t\r\n");
  if (tok == NULL) {
    fprintf(stderr, "Meminfo: Unknown format on /proc/loadavg\n");
    exit(1);
  }
  load.load5 = atof(tok);
  tok = strtok(NULL, " \t\r\n");
  if (tok == NULL) {
    fprintf(stderr, "Meminfo: Unknown format on /proc/loadavg\n");
    exit(1);
  }
  load.load15 = atof(tok);

// Read /proc/uptime 
  if (load.uptimefd <= 0)
    load.uptimefd = open("uptime", O_RDONLY);
  else
    lseek(load.uptimefd, 0, SEEK_SET);
  if (load.uptimefd <= 0) {
    fprintf(stderr, "Cannot open /proc/uptime\n");
    exit(1);
  }
  cnt = read(load.uptimefd, buf, 4096);
  if (cnt <= 0) {
    fprintf(stderr, "Read failure on /proc/uptime\n");
    exit(1);
  }
  buf[cnt] = '\0';
  tok = strtok(buf, " \t\r\n");
  if (tok == NULL) {
    fprintf(stderr, "Meminfo: Unknown format on /proc/uptime\n");
    exit(1);
  }
  load.uptime = atoi(tok);
  load.updays = load.uptime / 86400;
  load.uphours = (load.uptime % 86400) / 3600;
  load.upminutes = (load.uptime % 3600) / 60;
  load.load_per_tick = 64;  // 256 / 4;
  return &load;
}
/**************************************************/
struct meminfo *readMeminfo(void) {
/* Read /proc/meminfo and return the new values */
  static struct meminfo mem = { 0 };
  char buf[4097];
  char *tok, *nextline;
  int cnt;

  if (mem.meminfofd <= 0)
    mem.meminfofd = open("meminfo", O_RDONLY);
  else
    lseek(mem.meminfofd, 0, SEEK_SET);
  if (mem.meminfofd <= 0) {
    fprintf(stderr, "Cannot open /proc/meminfo\n");
    exit(1);
  }
  cnt = read(mem.meminfofd, buf, 4096);
  if (cnt <= 0) {
    perror("Read failure on /proc/meminfo");
    exit(1);
  }
  buf[cnt] = '\0';

// Skip header line
  if ((tok = strtok(buf, "\r\n")) == NULL) {
BadFormat:
    fprintf(stderr, "Unknown format on /proc/meminfo\n");
    exit(1);
  }
  
// Read meminfo
  if ((tok = strtok(NULL, "\r\n")) == NULL)
    goto BadFormat;
  nextline = tok + strlen(tok) + 1;

  tok = strtok(tok, " \t\r\n");
  if ((tok == NULL) || strcmp(tok, "Mem:"))
    goto BadFormat;

  if ((tok = strtok(NULL, " \t\r\n")) != NULL)
    mem.total = atoi(tok);
  else
    goto BadFormat;
  if ((tok = strtok(NULL, " \t\r\n")) != NULL)
    mem.used = atoi(tok);
  else
    goto BadFormat;
  if ((tok = strtok(NULL, " \t\r\n")) != NULL)
    mem.free = atoi(tok);
  else
    goto BadFormat;
  if ((tok = strtok(NULL, " \t\r\n")) != NULL)
    mem.shared = atoi(tok);
  else
    goto BadFormat;
  if ((tok = strtok(NULL, " \t\r\n")) != NULL)
    mem.buffers = atoi(tok);
  else
    goto BadFormat;

// Read swap information
  tok = strtok(nextline, " \t\r\n");
  if ((tok == NULL) || strcmp(tok, "Swap:"))
    goto BadFormat;
  if ((tok = strtok(NULL, " \t\r\n")) != NULL)
    mem.swaptotal = atoi(tok);
  else
    goto BadFormat;
  if ((tok = strtok(NULL, " \t\r\n")) != NULL)
    mem.swapused = atoi(tok);
  else
    goto BadFormat;
  if ((tok = strtok(NULL, " \t\r\n")) != NULL)
    mem.swapfree = atoi(tok);
  else
    goto BadFormat;

  mem.totmegs = (mem.total / (1024 * 1024)) + 1;
  mem.swapmegs = (mem.swaptotal / (1024 * 1024)) + 1;
  mem.bytes_per_tick = mem.total >> 8; 
  mem.swapbytes_per_tick = mem.swaptotal >> 8; 

  return(&mem);
}
/**************************************************/
