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

/*
    Handle the window exit event of the mouse pointer.  Used to do auto-saves.
    This trick on a frame is not specifically documented, but seems to work.
    I hope it doesn't break in the future.
*/
frameexit(frame, event, arg)
    Frame frame;
    Event *event;
    Notify_arg arg;
    {
    int  found = 0;
    struct SubDir *sp;
    struct Note *np;

    if(event_id(event) != LOC_WINEXIT) return;
    if(debug_on) fprintf(stderr, "Got LOC_WINEXIT\n");

    sp = (struct SubDir *) LLM_first(&subdir_rt);
    while(sp != NULL && !found)
        {
        np = (struct Note *) LLM_first(&sp->note_rt);
        while(np != NULL && !found)
            {
            if(np->frame == frame && np->mapped)
                {
                update(np);
                updateinfo(np, NOFORCE);
                found = 1;
                }
            np = (struct Note *) LLM_next(&sp->note_rt);
            }
        sp = (struct SubDir *) LLM_next(&subdir_rt);
        }

    }

/*
    Handles a drag & drop event from the file manager into a note, retrieving
    the path & file name and inserting it into the note text.
*/
dragdrop(window, event, arg)
    Xv_Window window;
    Event *event;
    Notify_arg arg;
    {
    int  found = 0;
    struct SubDir *sp;
    struct Note *np;
    Xv_Window twin;
    int  n;
    int  amount;
    char buf[SELN_BUFSIZE];

    if(debug_on)
        {
        if(event_action(event) == ACTION_DRAG_MOVE)
            fprintf(stderr, "Got ACTION_DRAG_MOVE event\n");
        if(event_action(event) == ACTION_DRAG_LOAD)
            fprintf(stderr, "Got ACTION_DRAG_LOAD event\n");
        if(event_action(event) == ACTION_DRAG_COPY)
            fprintf(stderr, "Got ACTION_DRAG_COPY event\n");
        }
    if(event_action(event) == ACTION_DRAG_LOAD)
        {
        sp = (struct SubDir *)LLM_first(&subdir_rt);
        while(sp != NULL && !found)
            {
            np = (struct Note *) LLM_first(&sp->note_rt);
            while(np != NULL && !found)
                {
                if(np->mapped)
                    {
                    twin = (Xv_Window) xv_get(np->textsw, OPENWIN_NTH_VIEW, 0);
                    if(twin == window)
                        {
                        found = 1;
                        n = xv_decode_drop(event, buf, sizeof(buf));
                        buf[sizeof(buf) - 1] = 0;
                        if(debug_on) fprintf(stderr, "buf {%s}\n", buf);
                        if(n > 0)
                            xv_set(np->textsw,
                                TEXTSW_INSERT_FROM_FILE, buf, NULL);
                        }
                    }
                np = (struct Note *) LLM_next(&sp->note_rt);
                }
            sp = (struct SubDir *)LLM_next(&subdir_rt);
            }
        }
    if((event_action(event) == ACTION_GO_PAGE_FORWARD || 
       event_action(event) == ACTION_GO_PAGE_BACKWARD) && event_is_down(event))
        {
        if(debug_on) fprintf(stderr, "Got PAGE event\n");
        sp = (struct SubDir *)LLM_first(&subdir_rt);
        while(sp != NULL && !found)
            {
            np = (struct Note *) LLM_first(&sp->note_rt);
            while(np != NULL && !found)
                {
                if(np->mapped)
                    {
                    twin = (Xv_Window) xv_get(np->textsw, OPENWIN_NTH_VIEW, 0);
                    if(twin == window)
                        {
                        Textsw_index t_ndx;
                        int  top, bottom;
                        int  len;

                        found = 1;
                        textsw_file_lines_visible(np->textsw, &top, &bottom);
                        len = xv_get(np->textsw, TEXTSW_LENGTH);
                        if(debug_on)
                            fprintf(stderr, "len %d\n", len);
                        amount = bottom - top + 1;
                        if(event_action(event) == ACTION_GO_PAGE_BACKWARD)
                            amount = -(amount);
                        if(debug_on)
                            fprintf(stderr, "top %d, bot %d\n", top, bottom);
                        top += amount;
                        if(top < 0) top = 0;
                        t_ndx = textsw_index_for_file_line(np->textsw, top);
                        if(debug_on)
                            fprintf(stderr, "t_ndx %d\n",
                                    t_ndx);
                        if(t_ndx >= 0)
                            {
                            xv_set(np->textsw,
                                   TEXTSW_INSERTION_POINT, t_ndx,
                                   TEXTSW_UPPER_CONTEXT, 0,
                                   NULL);
                            textsw_normalize_view(np->textsw, t_ndx);
                            }
                        }
                    }
                np = (struct Note *) LLM_next(&sp->note_rt);
                }
            sp = (struct SubDir *)LLM_next(&subdir_rt);
            }
        }
    if(event_id(event) == KBD_DONE)
        {
        if(debug_on) fprintf(stderr, "Got KBD_DONE event\n");
        sp = (struct SubDir *)LLM_first(&subdir_rt);
        while(sp != NULL && !found)
            {
            np = (struct Note *) LLM_first(&sp->note_rt);
            while(np != NULL && !found)
                {
                if(np->mapped)
                    {
                    twin = (Xv_Window) xv_get(np->textsw, OPENWIN_NTH_VIEW, 0);
                    if(twin == window)
                        {
                        found = 1;
                        update(np);
                        updateinfo(np, NOFORCE);
                        }
                    }
                np = (struct Note *) LLM_next(&sp->note_rt);
                }
            sp = (struct SubDir *)LLM_next(&subdir_rt);
            }
        }
    }

/*
    Handles child process death notification.  Used with the print note
    action.
*/
Notify_value child_death(me, pid, status, rusage)
    Notify_client me;
    int  pid;
    union wait *status;
    struct rusage *rusage;
    {
    if(WIFEXITED(*status))
        {
        if(debug_on) fprintf(stderr, "Got child death\n");
        cmd_printnote(NULL); /* allow next note to go, if any */
        return(NOTIFY_DONE);
        }
    return(NOTIFY_IGNORED);
    }
