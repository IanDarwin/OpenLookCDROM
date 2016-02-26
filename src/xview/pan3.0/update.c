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
    Routine called when an update of the notes size and position and title
    is desired.  Only write to a file if changes have been made or the
    force flag is set.
*/
updateinfo(np, force)
    struct Note *np;
    int  force;
    {
    int  closed;
    FILE *fp;
    Rect rect;
    char fname[MAXBUFLEN];

    if(!np->mapped) return;
    closed = xv_get(np->frame, FRAME_CLOSED);
    if(!closed)
        {
        frame_get_rect(np->frame, &rect);
        if(force != FORCE)
            {
            if(np->rect.r_left == rect.r_left &&
               np->rect.r_top == rect.r_top &&
               np->rect.r_width == rect.r_width &&
               np->rect.r_height == rect.r_height)
                {
                return;
                }
            }
        memcpy((char *)&np->rect, (char *)&rect, sizeof(rect));
        }
    makeinfoname(fname, np);
    fp = fopen(fname, "w");
    if(fp != NULL)
        {
        if(np->state == Hidden)
            fprintf(fp, "%d\t%d\t%d\t%d\tHIDDEN\n", np->rect.r_left,
                np->rect.r_top, np->rect.r_width, np->rect.r_height);
        else
            fprintf(fp, "%d\t%d\t%d\t%d\tVISIBLE\n", np->rect.r_left,
                np->rect.r_top, np->rect.r_width, np->rect.r_height);
        fprintf(fp, "%s\n", np->ntitle);
        fprintf(fp, "%d\n", np->crttime);
        fclose(fp);
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
        }
    }

/*
    Called when a save of the note text is desired.  Only writes to the file
    if modifications have been made.
*/
update(np)
    struct Note *np;
    {
    int  mod;
    char fname[MAXBUFLEN];

    if(!np->mapped) return(0);
    mod = xv_get(np->textsw, TEXTSW_MODIFIED);
    if(mod)
        {
        xv_set(np->textsw, TEXTSW_CONFIRM_OVERWRITE, FALSE, NULL);
        makename(fname, np);
        textsw_store_file(np->textsw, fname, 0, 0);
        }
    return(mod);
    }

/*
    Make a note file name.
*/
makename(fname, np)
    char *fname;
    struct Note *np;
    {
    struct SubDir *sp;

    sp = np->sp;
    sprintf(fname, "%s/%s/%s", note_dir, sp->subdir, np->basename);
    }

/*
    Make a note information file name.
*/
makeinfoname(fname, np)
    char *fname;
    struct Note *np;
    {
    struct SubDir *sp;

    sp = np->sp;
    sprintf(fname, "%s/%s/%s.info", note_dir, sp->subdir, np->basename);
    }
