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

extern int errno;
extern char *sys_errlist[];

/*
    Routine to dynamically create a menu of all currently hidden notes
    for the expose from list menu.
*/
Menu gen_exposemenu(mitem, op)
    Menu_item mitem;
    Menu_generate op;
    {
    int  cols;
    int  count = 0;
    int  i;
    static Menu menu = NULL;
    Menu_item mi;
    char *cp;
    char *text;
    struct Note *np;
    struct SubDir *sp;
    char title[2 * MAXTITLELEN];

    if(op == MENU_DISPLAY)
        {
        text = (char *)xv_get(mitem, MENU_STRING);
        if(menu != NULL)
            {
            for(i = xv_get(menu, MENU_NITEMS); i > 0; i--)
                {
                xv_set(menu, MENU_REMOVE, i, NULL);
                xv_destroy(xv_get(menu, MENU_NTH_ITEM, i));
                }
            xv_set(menu, MENU_NCOLS, 1, NULL);
            }
        else
            {
            menu = xv_create(NULL, MENU, NULL);
            }
        sp = (struct SubDir *)LLM_first(&subdir_rt);
        while(sp != NULL)
            {
            if(strcmp(sp->subdir, text) == 0) break;
            sp = (struct SubDir *)LLM_next(&subdir_rt);
            }
        np = (struct Note *)LLM_first(&sp->note_rt);
        /* find first note not visible */
        while(np != NULL && np->state == Visible)
            {
            np = (struct Note *)LLM_next(&sp->note_rt);
            }
        if(np != NULL)
            {
            cp = malloc(strlen(CHOICE_ALL) + 1);
            strcpy(cp, CHOICE_ALL);
            mi = xv_create(NULL, MENUITEM,
                MENU_STRING, cp,
                MENU_NOTIFY_PROC, exposemenu,
                MENU_CLIENT_DATA, 0xFFFFFFFF,
                MENU_RELEASE,
                MENU_RELEASE_IMAGE,
                NULL);
            xv_set(menu, MENU_APPEND_ITEM, mi, NULL);
            count++;
            }
        while(np != NULL)
            {
            if(np->state != Visible)
                {
                *title = 0;
                menu_string_normalize(np->ntitle, title);
                cp = malloc(strlen(title) + 1);
                strcpy(cp, title);
                mi = xv_create(NULL, MENUITEM,
                    MENU_STRING, cp,
                    MENU_NOTIFY_PROC, exposemenu,
                    MENU_CLIENT_DATA, np,
                    MENU_RELEASE,
                    MENU_RELEASE_IMAGE,
                    NULL);
                xv_set(menu, MENU_APPEND_ITEM, mi, NULL);
                count++;
                }
            np = (struct Note *)LLM_next(&sp->note_rt);
            }
        if(count == 0)
            {
            mi = xv_create(NULL, MENUITEM,
                MENU_STRING, "No hidden notes",
                MENU_NOTIFY_PROC, exposemenu,
                MENU_CLIENT_DATA, NULL,
                MENU_RELEASE,
                NULL);
            xv_set(menu, MENU_APPEND_ITEM, mi, NULL);
            }
        else
            {
            cols = menu_columns(count);
            xv_set(menu, MENU_NCOLS, cols, NULL);
            }
        }
    return(menu);
    }

/*
    Handles the Action menu choices.
*/
actionmenu(menu, mitem)
    Menu menu;
    Menu_item mitem;
    {
    char *text;
    Menu_item pullright;
    Menu mpullright;
    char *ptext;
    struct Note *np;

    if(strcmp((text = (char *)xv_get(mitem, MENU_STRING)), CHOICE_QUIT) == 0)
        {
        cleanup(0);
        }
    /* Figure out if we have a print item by climbing the hierarchy */
    mpullright = NULL;
    pullright = (Menu_item) xv_get(menu, MENU_PARENT);
    if(pullright != NULL)
        mpullright = (Menu_item) xv_get(pullright, MENU_PARENT);
    if(mpullright != NULL)
        pullright = (Menu_item) xv_get(mpullright, MENU_PARENT);
    if(pullright != NULL)
        {
        ptext = (char *)xv_get(pullright, MENU_STRING);
        if(strcmp(ptext, CHOICE_PRINT) == 0)
            {
            np = (struct Note *)xv_get(mitem, MENU_CLIENT_DATA);
            if(np == NULL)
                {
                notice_prompt(main_frame, NULL,
                    NOTICE_MESSAGE_STRINGS,
                        "No notes to print",
                        NULL,
                    NOTICE_BUTTON_YES, "Acknowledge",
                    NOTICE_NO_BEEPING, noticenobeep,
                    NULL);
                return;
                }
            w_printnote(np, ERRONDISPLAY);
            }
        }
    }

/*
    Routine to dynamically create a menu of all notes
    for the action print note menu.
*/
Menu gen_prtmenu(mitem, op)
    Menu_item mitem;
    Menu_generate op;
    {
    int  cols;
    int  count = 0;
    int  i;
    static Menu menu = NULL;
    Menu_item mi;
    char *cp;
    char *text;
    struct Note *np;
    struct SubDir *sp;
    char title[2 * MAXTITLELEN];

    if(op == MENU_DISPLAY)
        {
        text = (char *)xv_get(mitem, MENU_STRING);
        if(menu != NULL)
            {
            for(i = xv_get(menu, MENU_NITEMS); i > 0; i--)
                {
                xv_set(menu, MENU_REMOVE, i, NULL);
                xv_destroy(xv_get(menu, MENU_NTH_ITEM, i));
                }
            xv_set(menu, MENU_NCOLS, 1, NULL);
            }
        else
            {
            menu = xv_create(NULL, MENU, NULL);
            }
        sp = (struct SubDir *)LLM_first(&subdir_rt);
        while(sp != NULL)
            {
            if(strcmp(sp->subdir, text) == 0) break;
            sp = (struct SubDir *)LLM_next(&subdir_rt);
            }
        np = (struct Note *)LLM_first(&sp->note_rt);
        while(np != NULL)
            {
            *title = 0;
            menu_string_normalize(np->ntitle, title);
            cp = malloc(strlen(title) + 1);
            strcpy(cp, title);
            mi = xv_create(NULL, MENUITEM,
                MENU_STRING, cp,
                MENU_NOTIFY_PROC, actionmenu,
                MENU_CLIENT_DATA, np,
                MENU_RELEASE,
                MENU_RELEASE_IMAGE,
                NULL);
            xv_set(menu, MENU_APPEND_ITEM, mi, NULL);
            count++;
            np = (struct Note *)LLM_next(&sp->note_rt);
            }
        if(count == 0)
            {
            mi = xv_create(NULL, MENUITEM,
                MENU_STRING, "No notes to print",
                MENU_NOTIFY_PROC, actionmenu,
                MENU_CLIENT_DATA, NULL,
                MENU_RELEASE,
                NULL);
            xv_set(menu, MENU_APPEND_ITEM, mi, NULL);
            }
        else
            {
            cols = menu_columns(count);
            xv_set(menu, MENU_NCOLS, cols, NULL);
            }
        }
    return(menu);
    }

static Font panel_font;
static int  pf_width;
static int  pf_height;
static int  mitem_len;

init_menu_string_normalize()
    {
    pf_width = 0;
    panel_font = xv_get(main_panel, PANEL_FONT);
    if(panel_font)
        {
        pf_width =  xv_get(panel_font, FONT_DEFAULT_CHAR_WIDTH);
        pf_height =  xv_get(panel_font, FONT_DEFAULT_CHAR_HEIGHT);
        }
    mitem_len = pf_width * menutextlen;
    }

menu_string_normalize(src, dst)
    char *src, *dst;
    {
    Font_string_dims dims;
    int  i;
    int  slen;

    if(*src == 0)
        {
        strcpy(dst, NOTITLE);
        return;
        }

    strncpy(dst, src, menutextlen);
    dst[menutextlen] = 0;
    if(pf_width == 0) return;

    slen = strlen(src);

    i = menutextlen;
    xv_get(panel_font, FONT_STRING_DIMS, dst, &dims);
    while(dims.width < mitem_len && i < slen)
        {
        dst[i] = src[i];
        i++;
        dst[i] = 0;
        xv_get(panel_font, FONT_STRING_DIMS, dst, &dims);
        }
    while(dims.width > mitem_len)
        {
        i--;
        dst[i] = 0;
        xv_get(panel_font, FONT_STRING_DIMS, dst, &dims);
        }
    }

menu_columns(count)
    int  count;
    {
    int  cols = 0;
    int  h, rh;

    if(pf_width == 0)
        {
        cols = count / COL_ROLLOVER;
        if(cols == 0) cols = 1;
        if(cols == 1 && count > COL_ROLLOVER) cols = 2;
        if(cols > COL_MAXCOL) cols = COL_MAXCOL;
        }
    else
        {
        rh = mitem_len + (mitem_len / 2);
        h = pf_height * count;
        while(h > rh && cols < COL_MAXCOL)
            {
            cols++;
            h = pf_height * (count / cols);
            }
        }
    return(cols);
    }
