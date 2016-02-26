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

char *add_sorted(nr, title)
    struct LLM_root *nr;
    char *title;
    {
    struct Note *np;

    np = (struct Note *)LLM_first(nr);
    if(np == NULL)
        {
        return(LLM_add(nr));
        }
    while(np != NULL && strcmp(title, np->ntitle) >= 0)
        {
        np = (struct Note *)LLM_next(nr);
        }
    if(np == NULL)
        {
        return(LLM_add(nr));
        }
    return(LLM_insert(nr, (char *)np));
    }

/* adjust nodes position in list since the title may have changed */
adjust_sorted(np)
    struct Note *np;
    {
    struct LLM_root *nr;
    struct Note *tp;

    nr = &np->sp->note_rt;
    LLM_unlink(nr, (char *)np);
    tp = (struct Note *)LLM_first(nr);
    while(tp != NULL && strcmp(np->ntitle, tp->ntitle) >= 0)
        {
        tp = (struct Note *)LLM_next(nr);
        }
    LLM_link(nr, (char *)tp, (char *)np);
    }
