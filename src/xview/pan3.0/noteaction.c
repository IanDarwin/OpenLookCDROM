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
    Routine called when the action button on a note is selected.
*/
noteaction(menu, mitem)
    Menu menu;
    Menu_item mitem;
    {
    Font font;
    Rect rect;
    int  cw, tw;
    struct Note *np;
    int  mod;
    char fname[MAXBUFLEN];
    char item[MAXBUFLEN];

    np = (struct Note *) xv_get(menu, MENU_CLIENT_DATA);
    strcpy(item, (char *)xv_get(mitem, MENU_STRING));
    if(strcmp(item, "Print") == 0)
        {
        w_printnote(np, ERRONDISPLAY);
        }
    if(strcmp(item, widthtext) == 0)
        {
        font = xv_get(np->textsw, TEXTSW_FONT);

        cw = xv_get(font, FONT_DEFAULT_CHAR_WIDTH);

        tw = cw * setwidth;

        frame_get_rect(np->frame, &rect);
        rect.r_width = tw + leftmargin + rightmargin + SCROLLWIDTH;
        frame_set_rect(np->frame, &rect);
        }
    if(strcmp(item, "Destroy") == 0)
        {
        if(confirmdestroy)
            {
            if(notice_prompt(np->frame, NULL,
                         NOTICE_MESSAGE_STRINGS, "Really destroy this note?", NULL,
                         NOTICE_BUTTON_YES, "Yes",
                         NOTICE_BUTTON_NO, "No",
                         NOTICE_NO_BEEPING, noticenobeep,
                         NULL) == NOTICE_NO)
                {
                return;
                }
            }
        mod = xv_get(np->textsw, TEXTSW_MODIFIED);
        if(mod)
            {
            textsw_reset(np->textsw, 0, 0);
            }
        makename(fname, np);
        (void) unlink(fname);
        strcat(fname, "%"); /* Attempt to remove the textsw files */
        (void) unlink(fname);
        makeinfoname(fname, np);
        (void) unlink(fname);
        put_win(np);
        LLM_delete(&np->sp->note_rt, (char *)np);
        refresh_popups();
        }
    }
