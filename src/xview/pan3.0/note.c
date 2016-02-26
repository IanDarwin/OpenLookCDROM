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

#include <xview/font.h>

#ifdef PAN_DND
#include <xview/dragdrop.h>
#endif

#include <X11/X.h>
#include <X11/Xutil.h>

extern int errno;
extern char *sys_errlist[];

extern FILE *errfp;

/*
    Routine called when new note button is selected
*/
newnote(menu, mitem)
    Menu menu;
    Menu_item mitem;
    {
    struct SubDir *sp;
    char *text;

    text = (char *) xv_get(mitem, MENU_STRING);
    sp = (struct SubDir *)LLM_first(&subdir_rt);
    while(sp != NULL)
        {
        if(strcmp(text, sp->subdir) == 0) break;
        sp = (struct SubDir *)LLM_next(&subdir_rt);
        }

    if(sp == NULL)
        {
        notice_prompt(main_frame, NULL,
            NOTICE_MESSAGE_STRINGS,
                "Internal failure - couldn't find subdir entry",
                NULL,
            NOTICE_BUTTON_YES, "Acknowledge",
            NOTICE_NO_BEEPING, noticenobeep,
            NULL);
        return;
        }

    w_newnote(sp, Visible, NULL, NULL, NULL, ERRONDISPLAY);
    }

/*
    Does the grunt work of setting up internals for the frame for
    restorenotes().  Only maps visible notes.
*/
setnote(np)
    struct Note *np;
    {
    int  rl, rt, rw, rh;
    FILE *fp;
    char fname[MAXBUFLEN];
    char att[20];
    char title[MAXTITLELEN + 1];
    char cdate[100];

    makeinfoname(fname, np);
    np->rect.r_left = 0;
    np->rect.r_top = 0;
    np->rect.r_width = DEFWIDTH;
    np->rect.r_height = DEFHEIGHT;
    fp = fopen(fname, "r");
    if(fp != NULL)
        {
        np->state = Visible;

        fgets(title, sizeof(title), fp);
        (void) sscanf(title, "%d %d %d %d %7s", &rl,
            &rt, &rw, &rh, att);
        np->rect.r_left = rl;
        np->rect.r_top = rt;
        np->rect.r_width = rw;
        np->rect.r_height = rh;
        *title = 0;
        fgets(title, sizeof(title), fp);
        trim(title);
        *cdate = 0;
        if(fgets(cdate, sizeof(cdate), fp) != NULL)
            np->crttime = atoi(cdate);


        strcpy(np->ntitle, title);

        if(strncmp(att, "HIDDEN", 6) == 0)
            np->state = Hidden;
        if(strncmp(att, "VISIBLE", 7) == 0)
            np->state = Visible;

        if(np->state != Hidden)
            {
            if(!buildnote(np, 1, FALSE))
                {
                (void) fclose(fp);
                return;
                }
            xv_set(np->title, PANEL_VALUE, np->ntitle, NULL);
            set_frame_title(np, np->ntitle);
            reseticon(np);
            }
        else np->mapped = 0;

        (void) fclose(fp);
        }
    else
        {
        notice_prompt(main_frame, NULL,
            NOTICE_MESSAGE_STRINGS,
                "Couldn't open note geometry file",
                fname,
                sys_errlist[errno],
                NULL,
            NOTICE_BUTTON_YES, "Acknowledge",
            NOTICE_NO_BEEPING, noticenobeep,
            NULL);
        if(!buildnote(np, 1, TRUE)) return;
        np->state = Visible;
        }
    }

/*
    Creates the actual Xview frame and items for a note.  Called by newnote()
    and restorenotes().
*/
buildnote(np, display, adjust)
    struct Note *np;
    int  display;
    int  adjust;
    {
    static Xv_Font ft;
    static int foundfont = 0;
    Rect *rect;
    int  x, hx, y;
    Server_image image, image2;
    static XClassHint chint;
    struct tm *tmptr;
    char title[MAXTITLELEN + 1];
    char fname[MAXBUFLEN];
    char buf [30];

    strcpy(title, NOTITLE);

    if(get_win(np))
        {
        /* if size not specifically set, make sure panel items visible */
        if(adjust)
            {
#ifndef PAN_DND
            rect = (Rect *) xv_get(np->cdate, PANEL_ITEM_RECT);
            hx = rect->r_left + rect->r_width + DEFPANELSPACING;
#else
            rect = (Rect *) xv_get(np->drag_tgt, PANEL_ITEM_RECT);
            hx = rect->r_left + rect->r_width + 2 * DEFPANELSPACING;
#endif
            rect = (Rect *) xv_get(np->ctime, PANEL_ITEM_RECT);
            x = rect->r_left + rect->r_width + DEFPANELSPACING;
            if(hx > x) x = hx;
            np->rect.r_width = x;
            }
        frame_set_rect(np->frame, &np->rect);

        if(np->crttime != 0)
            {
            tmptr = localtime(&np->crttime);
            sprintf(buf, "Created: %02d/%02d/%02d", tmptr->tm_mon + 1,
                tmptr->tm_mday, tmptr->tm_year);
            }
        else strcpy(buf, "No create time");
        xv_set(np->cdate, PANEL_LABEL_STRING, buf, NULL);

        if(np->crttime != 0)
            {
            tmptr = localtime(&np->crttime);
            sprintf(buf, "Time:    %02d:%02d:%02d", tmptr->tm_hour,
                tmptr->tm_min, tmptr->tm_sec);
            }
        else *buf = 0;
        xv_set(np->ctime, PANEL_LABEL_STRING, buf, NULL);

        makename(fname, np);
        xv_set(np->textsw, TEXTSW_FILE, fname, TEXTSW_FIRST, 0, NULL);
        xv_set(np->frame, XV_LABEL, title, NULL);

        xv_set(np->panel, PANEL_CARET_ITEM, np->title, NULL);

        xv_set(np->title, PANEL_CLIENT_DATA, np, NULL);
        xv_set(np->hide, PANEL_CLIENT_DATA, np, NULL);
        xv_set(np->actionmenu, MENU_CLIENT_DATA, np, NULL);

        xv_set(np->frame, XV_SHOW, TRUE, NULL);
        np->mapped = 1;
        return(1);
        }

    np->frame = xv_create(main_frame, FRAME,
                              XV_LABEL, title,
                              FRAME_DONE_PROC, dismissed,
                              NULL);
    if(np->frame == NULL)
        {
        if(display)
            {
            notice_prompt(main_frame, NULL,
                NOTICE_MESSAGE_STRINGS,
                    "Couldn't create a new frame (internal error)",
                    NULL,
                NOTICE_BUTTON_YES, "Acknowledge",
                NOTICE_NO_BEEPING, noticenobeep,
                NULL);
            }
        else
            {
            fprintf(errfp,
                "%s:  Couldn't create a new frame (internal error)\n",
                myname);
            }
        return(0);
        }

    /* Set up the X class since xview doesn't */
    chint.res_name = "pannote";
    chint.res_class = "PanNote";
    XSetClassHint((Display *)xv_get(np->frame, XV_DISPLAY),
        xv_get(np->frame, XV_XID), &chint);

    xv_set(np->frame, WIN_EVENT_PROC, frameexit, WIN_CONSUME_EVENTS,
        LOC_WINEXIT, NULL,
        NULL);

    np->panel = (Panel) xv_create(np->frame, PANEL,
                     WIN_RETAINED, FALSE, 
                     NULL);

    if(np->panel == NULL)
        {
        xv_destroy_safe(np->frame);
        return(0);
        }

    np->title = xv_create(np->panel, PANEL_TEXT,
                     PANEL_LABEL_STRING, "Title:",
                     PANEL_VALUE, "",
                     PANEL_VALUE_DISPLAY_LENGTH, MAXTITLEDISLEN + 1,
                     PANEL_VALUE_STORED_LENGTH, MAXTITLELEN,
                     PANEL_NOTIFY_PROC, newtitle,
                     PANEL_CLIENT_DATA, np,
                     XV_X, xv_col(np->panel, 0),
                     XV_Y, xv_row(np->panel, 0),
                     NULL);
    if(np->title == NULL)
        {
        xv_destroy_safe(np->frame);
        return(0);
        }
    rect = (Rect *) xv_get(np->title, PANEL_ITEM_RECT);
    hx = rect->r_left + rect->r_width + DEFPANELSPACING;

    if(np->crttime != 0)
        {
        tmptr = localtime(&np->crttime);
        sprintf(buf, "Created: %02d/%02d/%02d", tmptr->tm_mon + 1,
            tmptr->tm_mday, tmptr->tm_year);
        }
    else
       {
       strcpy(buf, "No create time");
       }
    np->cdate = xv_create(np->panel, PANEL_MESSAGE,
                 PANEL_LABEL_STRING, buf,
                 XV_X, hx,
                 XV_Y, xv_row(np->panel, 0),
                 NULL);

#ifdef PAN_DND
    rect = (Rect *) xv_get(np->cdate, PANEL_ITEM_RECT);
    x = rect->r_left + rect->r_width + 2 * DEFPANELSPACING;
    np->drag_obj = xv_create(np->panel, DRAGDROP,
                 NULL);
    np->drag_tgt = xv_create(np->panel, PANEL_DROP_TARGET,
                 XV_X, x,
                 XV_Y, xv_row(np->panel, 0),
                 PANEL_DROP_DND, np->drag_obj,
                 PANEL_NOTIFY_PROC, drag_proc,
                 PANEL_DROP_FULL,   TRUE,
                 NULL);
    np->got_itms = 0;
#endif

    np->hide = xv_create(np->panel, PANEL_BUTTON,
                     PANEL_NEXT_ROW, -1,
                     PANEL_LABEL_STRING, "Hide",
                     PANEL_NOTIFY_PROC, hidenote, 
                     PANEL_CLIENT_DATA, np,
                     XV_X, xv_col(np->panel, 0),
                     XV_Y, xv_row(np->panel, 1),
                     NULL);
    if(np->hide == NULL)
        {
        xv_destroy_safe(np->frame);
        return(0);
        }
    rect = (Rect *) xv_get(np->hide, PANEL_ITEM_RECT);
    x = rect->r_left + rect->r_width + DEFPANELSPACING;
    y = rect->r_top;

    np->actionmenu = (Menu) xv_create(NULL, MENU,
                     MENU_NOTIFY_PROC, noteaction,
                     MENU_CLIENT_DATA, np,
                     MENU_STRINGS, "Print", widthtext, "Destroy", NULL,
                     NULL);
    np->action = (Panel_item) xv_create(np->panel, PANEL_BUTTON,
                     PANEL_LABEL_STRING, "Action",
                     PANEL_ITEM_MENU, np->actionmenu,
                     XV_X, x,
                     XV_Y, y,
                     NULL);
    if(np->action == NULL)
        {
        xv_destroy_safe(np->frame);
        return(0);
        }
    rect = (Rect *) xv_get(np->action, PANEL_ITEM_RECT);
    x = rect->r_left + rect->r_width + DEFPANELSPACING;
    if(x > hx)
        {
        hx = x;
        xv_set(np->cdate, XV_X, hx, NULL);
        rect = (Rect *) xv_get(np->cdate, PANEL_ITEM_RECT);
        x = rect->r_left + rect->r_width + 2 * DEFPANELSPACING;
#ifndef PAN_DND
        xv_set(np->drag_tgt, XV_X, x, NULL);
#endif
        }

    if(np->crttime != 0)
        {
        tmptr = localtime(&np->crttime);
        sprintf(buf, "Time:    %02d:%02d:%02d", tmptr->tm_hour,
            tmptr->tm_min, tmptr->tm_sec);
        }
    else *buf = 0;
    np->ctime = xv_create(np->panel, PANEL_MESSAGE,
             PANEL_LABEL_STRING, buf,
             XV_X, hx,
             XV_Y, y,
             NULL);

    window_fit_height(np->panel);

    np->textsw = xv_create(np->frame, TEXTSW,
                     NULL);
    if(np->textsw == NULL)
        {
        xv_destroy_safe(np->frame);
        return(0);
        }

    /* User defined textsw font processing */
    if(*textfont)
        {
        if(foundfont == 0)
            {
            ft = xv_find(main_frame, FONT, FONT_NAME, textfont, NULL);
            if(ft)
                foundfont = 1;
            else
                foundfont = -1;
            }
        if(foundfont == 1)
            {
            xv_set(np->textsw, TEXTSW_FONT, ft, NULL);
            }
        }


    xv_set(xv_get(np->textsw, OPENWIN_NTH_VIEW, 0), 
        WIN_EVENT_PROC, dragdrop,
        WIN_CONSUME_EVENTS,
            ACTION_DRAG_LOAD|ACTION_GO_PAGE_FORWARD|ACTION_GO_PAGE_BACKWARD,
            KBD_DONE,
            NULL,
        WIN_RETAINED, FALSE,
        NULL);

    window_fit(np->textsw);
    window_fit(np->frame);

    /* if size not specifically set, make sure panel items visible */
    if(adjust)
        {
#ifndef PAN_DND
        rect = (Rect *) xv_get(np->cdate, PANEL_ITEM_RECT);
        hx = rect->r_left + rect->r_width + DEFPANELSPACING;
#else
        rect = (Rect *) xv_get(np->drag_tgt, PANEL_ITEM_RECT);
        hx = rect->r_left + rect->r_width + 2 * DEFPANELSPACING;
#endif
        rect = (Rect *) xv_get(np->ctime, PANEL_ITEM_RECT);
        x = rect->r_left + rect->r_width + DEFPANELSPACING;
        if(hx > x) x = hx;
        np->rect.r_width = x;
        }

    frame_set_rect(np->frame, &np->rect);

    image = xv_create(NULL, SERVER_IMAGE, XV_WIDTH, 64, XV_HEIGHT, 64,
                      SERVER_IMAGE_BITS, myicon, NULL);
    if(image == NULL)
        {
        xv_destroy_safe(np->frame);
        return(0);
        }
    image2 = xv_create(NULL, SERVER_IMAGE, XV_WIDTH, 64, XV_HEIGHT, 64,
                      SERVER_IMAGE_BITS, myiconmask, NULL);
    if(image2 == NULL)
        {
        xv_destroy_safe(np->frame);
        return(0);
        }
    np->icon = xv_create(NULL, ICON,
                     ICON_IMAGE, image,
                     ICON_MASK_IMAGE, image2,
                     ICON_LABEL, title,
                     ICON_TRANSPARENT, icontransparent,
                     NULL);
    if(np->icon == NULL)
        {
        xv_destroy_safe(np->frame);
        return(0);
        }
    xv_set(np->frame, FRAME_ICON, np->icon, NULL);

    makename(fname, np);
    xv_set(np->textsw, TEXTSW_FILE, fname, TEXTSW_FIRST, 0, NULL);
    xv_set(np->frame, XV_SHOW, TRUE, NULL);
    np->mapped = 1;

    return(1);
    }

/*
    Sets the icon label of a note to its title.
*/
reseticon(np)
    struct Note *np;
    {
    char title[MAXTITLELEN + 1];

    *title = 0;
    strcpy(title, np->ntitle);
    if(strlen(title) == 0) strcpy(title, NOTITLE);

    xv_set(np->icon, ICON_LABEL, title, NULL);
    }

/*
   Called when the user enters a new title for a note.
*/
newtitle(item, event)
    Panel_item item;
    Event *event;
    {
    Xv_Window window;
    struct Note *np;
    char title[MAXTITLELEN + 1];
    char ntitle[MAXBUFLEN];

    np = (struct Note *)xv_get(item, PANEL_CLIENT_DATA);

    strcpy(title, (char *) xv_get(np->title, PANEL_VALUE));
    trim(title);
    if(strlen(title))
        {
        strcpy(ntitle, title);
        }
    else
        {
        strcpy(ntitle, NOTITLE);
        }
    set_frame_title(np, ntitle);
    strcpy(np->ntitle, ntitle);
    reseticon(np);
    adjust_sorted(np);

    xv_set(item, PANEL_INACTIVE, TRUE, NULL);
    window = (Xv_Window) xv_get(np->textsw, OPENWIN_NTH_VIEW, 0);
    win_set_kbd_focus(window, xv_get(window, XV_XID));
    xv_set(item, PANEL_INACTIVE, FALSE, NULL);
    updateinfo(np, FORCE);
    refresh_popups();
    return((int)PANEL_NONE);
    }

set_frame_title(np, title)
    struct Note *np;
    char *title;
    {
    char t_title[MAXTITLELEN + 1];

    if(fintitle)
        {
        sprintf(t_title, "(%s) %s", np->sp->subdir, title);
        xv_set(np->frame, XV_LABEL, t_title, NULL);
        }
    else
        xv_set(np->frame, XV_LABEL, title, NULL);
    }
