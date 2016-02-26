/* static char sccsid[]="@(#)window.h	1.5 10/12/92";*/
#include "sweep.h"
#ifndef _WINDOWS_H_
#define _WINDOWS_H_
#include <X11/Xlib.h>
#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/font.h>
#include <xview/panel.h>
#include <xview/textsw.h>
#include <xview/xv_xrect.h>
#include <xview/svrimage.h>
#include <xview/icon.h>
#include <xview/notify.h>
#include <pwd.h>

short unsigned icon_bits[] = {
#include "sweeper.icon"
};

#ifndef HIGHSCOREFILE
#define HIGHSCOREFILE "sweeper.scores"
#endif

#define VERSION "Sweeper V 1.1"

Frame frame;
Canvas canvas;
Panel pan;
Panel_item time_msg,ngamebutt;
struct itimerval timer;
int sec,bang_shown,no_shown_squares;
Xv_Font font,numberfont,largefont;

void expose();
void repaint_proc();
void cv_event();
void init_windows();
void show_square();
void mark_square();
int  done();
void show_all();
void set_size();
void set_diff();
void new_game();
void start();
Notify_value tick_clock();
void bang();
Notify_value bang_timer();
void stop();
void highscore_butt();
void show_highscore();
int  update_highscore();
void read_highscore();
void write_highscore();
void draw_bomb(/* row,col,pw,dpy,xwin */);
void fill_square(/* row,col,pw,dpy,xwin */);
void draw_number(/* row,col,number,pw,dpy,xwin */);


#endif
