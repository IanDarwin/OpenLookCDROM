/*
Post A Note V3.0
Copyright (c) 1993, Jeffrey W. Bailey
All rights reserved.

Permission is granted to distribute this program in exact, complete
source form, which includes this copyright notice, as long as no fee
other than media and distribution cost is charged.

This program may not be used in whole, or in part, in any other manner
without prior written permission from the author.

This program may not be distributed in modified form without prior
written permission from the author.  In other words, patches may be
distributed, but modified source may not be distributed.

If there are any questions, comments or suggestions, the author may be
contacted at:

    jeff@rd1.interlan.com

    or

    Jeffrey Bailey
    Racal-Datacom, Inc.
    Mail Stop E-110
    1601 N. Harrison Parkway
    Sunrise, FL  33323-2899
*/

#include "pan.h"

extern int errno;
extern char *sys_errlist[];

get_win(np)
    struct Note *np;
    {
    struct FreeWin *fp;

    fp = (struct FreeWin *)LLM_first(&freewin_rt);
    if(fp == NULL) return(0);
    np->frame = fp->frame;
    np->panel = fp->panel;
    np->textsw = fp->textsw;
    np->title = fp->title;
    np->hide = fp->hide;
    np->action = fp->action;
    np->actionmenu = fp->actionmenu;
    np->cdate = fp->cdate;
    np->ctime = fp->ctime;
    np->icon = fp->icon;
#ifdef PAN_DND
    np->drag_obj = fp->drag_obj;
    np->drag_tgt = fp->drag_tgt;
    np->got_itms = fp->got_itms;
    np->sel_itm1 = fp->sel_itm1;
    np->sel_itm2 = fp->sel_itm2;
    np->sel_itm3 = fp->sel_itm3;
#endif
    LLM_delete(&freewin_rt, fp);
    return(1);
    }

put_win(np)
    struct Note *np;
    {
    struct FreeWin *fp;

    fp = (struct FreeWin *)LLM_add(&freewin_rt);
    if(fp == NULL)
        {
        fprintf(stderr, "Internal memory allocation error\n");
        exit(1);
        }
    fp->frame = np->frame;
    fp->panel = np->panel;
    fp->textsw = np->textsw;
    fp->title = np->title;
    fp->hide = np->hide;
    fp->action = np->action;
    fp->actionmenu = np->actionmenu;
    fp->cdate = np->cdate;
    fp->ctime = np->ctime;
    fp->icon = np->icon;
#ifdef PAN_DND
    fp->drag_obj = np->drag_obj;
    fp->drag_tgt = np->drag_tgt;
    fp->got_itms = np->got_itms;
    fp->sel_itm1 = np->sel_itm1;
    fp->sel_itm2 = np->sel_itm2;
    fp->sel_itm3 = np->sel_itm3;
#endif
    xv_set(fp->frame, XV_SHOW, FALSE, NULL);
    if(xv_get(fp->frame, FRAME_CLOSED))
        xv_set(fp->frame, FRAME_CLOSED, FALSE, NULL);
    xv_set(fp->title, PANEL_VALUE, "", NULL);
    }
