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

#include <X11/X.h>
#include <X11/Xutil.h>

extern int errno;
extern char *sys_errlist[];

int cf_resize_proc();

static Panel_item appbutton;

createfolder(menu, mitem)
    Menu menu;
    Menu_item mitem;
    {
    Rect rect, *prect;
    Panel_item pitem;
    int  x, y;
    Panel folder_panel;
    static XClassHint chint;

    w_popupxy(&x, &y, FLDRWIDTH, FLDRHEIGHT, FLDRSPACING);
    folder_mitem = mitem;
    folder_frame = xv_create(main_frame, FRAME_CMD,
                           XV_LABEL, "Create Folder",
                           XV_X, x,
                           XV_Y, y,
                           FRAME_NO_CONFIRM, TRUE,
                           FRAME_DONE_PROC, folderdone,
                           WIN_EVENT_PROC, cf_resize_proc,
                           WIN_CONSUME_EVENTS,
                               WIN_RESIZE,
                               NULL,
                           NULL);
    if(folder_frame == NULL)
        {
        notice_prompt(main_frame, NULL,
            NOTICE_MESSAGE_STRINGS,
                "Unable to create sub-frame (internal error)",
                NULL,
            NOTICE_BUTTON_YES, "Acknowledge",
            NOTICE_NO_BEEPING, noticenobeep,
            NULL);
        return;
        }

    /* Set up the X class since xview doesn't */
    chint.res_name = "pan";
    chint.res_class = "Pan";
    XSetClassHint((Display *)xv_get(folder_frame, XV_DISPLAY),
        xv_get(folder_frame, XV_XID), &chint);

    folder_panel = (Panel) xv_get(folder_frame, FRAME_CMD_PANEL);
    xv_set(folder_panel, WIN_RETAINED, FALSE, NULL);

    folder_item =  xv_create(folder_panel, PANEL_TEXT,
                 PANEL_LABEL_STRING, "New Folder:",
                 PANEL_VALUE, "",
                 PANEL_VALUE_DISPLAY_LENGTH, MAXSUBDIR,
                 PANEL_VALUE_STORED_LENGTH, MAXSUBDIR,
                 PANEL_NOTIFY_PROC, newfolder,
                 XV_Y, xv_row(folder_panel, 0),
                 NULL);
    pitem = xv_create(folder_panel, PANEL_BUTTON,
                     PANEL_LABEL_STRING, "Apply",
                     PANEL_NOTIFY_PROC, newfolder,
                     XV_Y, xv_row(folder_panel, 1),
                     NULL);
    appbutton = pitem;

    window_fit(folder_panel);
    window_fit(folder_frame);

    xv_set(folder_frame, XV_SHOW, TRUE, NULL);

    frame_get_rect(folder_frame, &rect);
    prect = (Rect *) xv_get(pitem, PANEL_ITEM_RECT);
    xv_set(pitem, XV_X, ((rect.r_width / 2) - (prect->r_width / 2)), NULL);


    (void) xv_set(folder_mitem, MENU_INACTIVE, TRUE, NULL);
    }

/*
    Called when the user wants to create a new folder.
    Note: item and event args are not used.
*/
newfolder(item, event)
    Panel_item item;
    Event *event;
    {
    char new_dir[MAXBUFLEN];

    strcpy(new_dir, (char *) xv_get(folder_item, PANEL_VALUE));

    if(xv_get(folder_frame, FRAME_CMD_PUSHPIN_IN) == FALSE)
        {
        xv_destroy_safe(folder_frame);
        (void) xv_set(folder_mitem, MENU_INACTIVE, FALSE, NULL);
        }

    xv_set(folder_frame, FRAME_BUSY, TRUE, NULL);

    w_newfolder(new_dir, ERRONDISPLAY); /* Does all the real work */

    xv_set(folder_frame, FRAME_BUSY, FALSE, NULL);
    }

/*
    Called when the user dismisses the change folder window without applying
    a change.
*/
folderdone()
    {
    xv_destroy_safe(folder_frame);
    (void) xv_set(folder_mitem, MENU_INACTIVE, FALSE, NULL);
    }

/*
    Center the button here even though we don't allow a resize because of
    the intermittent failure of frame_get_rect() immediately after frame
    creation.
*/
cf_resize_proc(frame, event, arg)
    Frame frame;
    Event *event;
    Notify_arg arg;
    {
    Rect rect, *prect;
 
    if(event_id(event) != WIN_RESIZE) return;
    frame_get_rect(frame, &rect);
    rect.r_height -= (topmargin + bottommargin); /* correct for wm title bar */

    prect = (Rect *) xv_get(appbutton, PANEL_ITEM_RECT);
    xv_set(appbutton, XV_X, rect.r_width / 2 - prect->r_width / 2, NULL);
    }
