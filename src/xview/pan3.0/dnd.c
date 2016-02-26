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

#ifdef PAN_DND

#include "pan.h"
#include <xview/dragdrop.h>

struct Note *drag_find();

static Xv_Server dnd_server;

drag_init(server)
    Xv_Server server;
    {
    dnd_server = server;
    }

drag_proc(item, value, event)
    Xv_opaque item;
    unsigned int value;
    Event *event;
    {
    struct Note *np;
    Selection_requestor sel_req;

    sel_req = xv_get(item, PANEL_DROP_SEL_REQ);
    switch(event_action(event))
        {
        case ACTION_DRAG_MOVE : /* Do nothing for now, textsw handles it */
            break;
        case ACTION_DRAG_COPY : /* Do nothing for now, textsw handles it */
            break;
        case LOC_DRAG :
            np = drag_find(item);
            if(np)
                {
                int txt_len;
                char *text;
                char fname[MAXBUFLEN];
                Atom list[3];

                /* Set up file name */
                makename(fname, np);
                if(!np->got_itms)
                    {
                    np->sel_itm1 = xv_create(np->drag_obj,SELECTION_ITEM,NULL);
                    np->sel_itm2 = xv_create(np->drag_obj,SELECTION_ITEM,NULL);
                    np->sel_itm3 = xv_create(np->drag_obj,SELECTION_ITEM,NULL);
                    np->got_itms = 1;
                    }
                xv_set(np->sel_itm1,
                    SEL_DATA, fname,
                    SEL_FORMAT, 8,
                    SEL_LENGTH, strlen(fname),
                    SEL_TYPE_NAME, "FILE_NAME",
                    SEL_COPY, TRUE,
                    SEL_OWN, TRUE,
                    NULL);
                /* Set up for text transfer */
                txt_len = xv_get(np->textsw, TEXTSW_LENGTH) + 1;
                text = malloc(txt_len);
                if(text == NULL)
                    {
                    fprintf(stderr, "pan:  Memory allocation failure\n");
                    cleanup(1);
                    }
                xv_get(np->textsw, TEXTSW_CONTENTS, 0, text, txt_len);
                xv_set(np->sel_itm2,
                    SEL_DATA, text,
                    SEL_FORMAT, 8,
                    SEL_LENGTH, strlen(text),
                    SEL_TYPE_NAME, "STRING",
                    SEL_COPY, TRUE,
                    SEL_OWN, TRUE,
                    NULL);
                free(text);
                list[0] = (Atom) xv_get(dnd_server, SERVER_ATOM, "TARGETS");
                list[1] = (Atom) xv_get(dnd_server, SERVER_ATOM, "FILE_NAME");
                list[2] = (Atom) xv_get(dnd_server, SERVER_ATOM, "STRING");
                xv_set(np->sel_itm3,
                    SEL_DATA, list,
                    SEL_FORMAT, 32,
                    SEL_LENGTH, 3,
                    SEL_TYPE_NAME, "TARGETS",
                    SEL_COPY, TRUE,
                    SEL_OWN, TRUE,
                    NULL);
                }
            break;
        default :
            printf("unknown event %d\n", event_action(event));
            break;
        }
    return(XV_OK);
    }

struct Note *drag_find(item)
    Xv_opaque item;
    {
    struct SubDir *sp;
    struct Note *np;

    sp = (struct SubDir *) LLM_first(&subdir_rt);
    while(sp != NULL)
        {
        np = (struct Note *) LLM_first(&sp->note_rt);
        while(np != NULL)
            {
            if(np->drag_tgt == item) return(np);
            np = (struct Note *) LLM_next(&sp->note_rt);
            }
        sp = (struct SubDir *) LLM_next(&subdir_rt);
        }
    return(NULL);
    }
#endif
