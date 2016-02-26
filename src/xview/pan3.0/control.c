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

struct Commands
    {
    char *Name;
    int  (*Routine)();
    };

int cmd_debug(), cmd_expose(), cmd_hide(), cmd_movenote(), cmd_newfolder();
int cmd_newnote(), cmd_printnote(), cmd_quit(), cmd_rename(), cmd_veil();

static struct Commands cmds[] = {
    {"debug",     cmd_debug},
    {"expose",    cmd_expose},
    {"hide",      cmd_hide},
    {"move",      cmd_movenote},
    {"newfolder", cmd_newfolder},
    {"newnote",   cmd_newnote},
    {"print",     cmd_printnote},
    {"quit",      cmd_quit},
    {"rename",    cmd_rename},
    {"veil",      cmd_veil},
    {NULL,        NULL}
};

FILE *errfp;

static char ctrllck[MAXBUFLEN];
static char ctrlnam[MAXBUFLEN];
static char errlnam[MAXBUFLEN];

Notify_value check_ctl_file(client, which)
    Notify_client client;
    int  which;
    {
    int  fd;
    struct stat mystat;

    sprintf(ctrllck, "%s/%s", note_dir, CTRLLCK);
    if(stat(ctrllck, &mystat) == 0)
        return(NOTIFY_DONE); /* control file is locked */

    sprintf(ctrlnam, "%s/%s", note_dir, CTRLNAM);
    if(stat(ctrlnam, &mystat) < 0)
        return(NOTIFY_DONE); /* control file doesn't exit */

    if((fd = open(ctrllck, O_CREAT|O_RDWR|O_EXCL, 0644) < 0))
        return(NOTIFY_DONE); /* Couldn't acquire lock */
    (void)close(fd);

    if(logging)
        sprintf(errlnam, "%s/%s", note_dir, ERRLNAM);
    else
        strcpy(errlnam, "/dev/null");

    if((errfp = fopen(errlnam, "a")) == NULL)
        {
        (void) unlink(ctrllck);
        fprintf(stderr, "%s:  Couldn't open %s for append\n", myname, errlnam);
        return(NOTIFY_DONE);
        }

    dispatch(ctrlnam);

    (void) fclose(errfp);
    (void) unlink(ctrlnam);
    (void) unlink(ctrllck);
    return(NOTIFY_DONE);
    }

dispatch(name)
    char *name;
    {
    FILE *fp;
    struct ps_component *lp;
    int  i, found;
    char buf[MAXBUFLEN];

    fp = fopen(name, "r");
    if(fp == NULL)
        {
        fprintf(errfp, "%s:  Couldn't open %s:  %s\n", myname, name,
                sys_errlist[errno]);
        return;
        }

    while(fgets(buf, sizeof(buf), fp) != NULL)
        {
        trim(buf); /* skip comments & blank lines */
        if(*buf == '#' || *buf == 0) continue;

        lp = parse_string(buf, " \t\n", 1, 1, 1);
        if(lp == NULL) continue;

        for(i = 0, found = 0; cmds[i].Name != NULL; i++)
            {
            if(strcmp(lp->ps_text, cmds[i].Name) == 0)
                {
                fprintf(errfp, "%s:  **** executing {%s}\n", myname, buf);
                found = 1;
                (*cmds[i].Routine)(lp->ps_next);
                break;
                }
            }
        free_ps_list(lp);
        if(!found)
            {
            fprintf(errfp, "%s:  Unrecognized command:  %s\n", myname, buf);
            }
        }

    (void) fclose(fp);
    }

cmd_debug(lp)
    struct ps_component *lp;
    {
    if(debug_on) fprintf(stderr, "cmd_debug\n");
    if(lp == NULL ||
       (strcmp(lp->ps_text, "on") != 0 && strcmp(lp->ps_text, "off") != 0))
        {
        fprintf(errfp, "%s:  debug command requires \"on|off\" parameter\n",
            myname);
        return;
        }
    if(strcmp(lp->ps_text, "on") == 0)
        {
        debug_on = 1;
        }
    else
        {
        debug_on = 0;
        }
    }

cmd_expose(lp)
    struct ps_component *lp;
    {
    int  count;
    int  res;
    struct Note **npp;
    struct LLM_root root;
    char title[MAXBUFLEN];
    int  gottitle;
    int  lwindowmax;

    if(debug_on) fprintf(stderr, "cmd_expose\n");
    lwindowmax = windowmax;
    gottitle = 0;
    while(lp != NULL)
        {
        if(lp->ps_next == NULL) /* go by two's, so we need another param */
            {
            fprintf(errfp, "%s:  Incorrect parameters for expose\n",
                myname);
            return;
            }
        if(strcmp(lp->ps_text, "title") == 0)
            {
            lp = lp->ps_next;
            strcpy(title, lp->ps_text);
            gottitle = 1;
            lp = lp->ps_next;
            }
        else if(strcmp(lp->ps_text, "windowmax") == 0)
            {
            lp = lp->ps_next;
            lwindowmax = atoi(lp->ps_text);
            lp = lp->ps_next;
            }
        else
            {
            fprintf(errfp, "%s:  Incorrect parameter {%s} for expose\n",
                myname, lp->ps_text);
            return;
            }
        }
    if(!gottitle)
        {
        fprintf(errfp, "%s:  Missing title for expose\n",
            myname);
        return;
        }
    res = w_matchingnotes(&root, title, 1);
    switch(res)
        {
        case  -1 :
            fprintf(errfp, "%s:  Invalid RE for expose command {%s}\n",
                myname, title);
            return;
            break;
        case  -2 :
            fprintf(errfp, "%s:  Memory allocation failure during expose\n",
                myname);
            LLM_free(&root);
            return;
            break;
        }
    count = 0;
    npp = (struct Note **) LLM_first(&root);
    if(npp == NULL)
        {
        fprintf(errfp, "%s:  No matches found to expose for RE {%s}\n",
            myname, title);
        }
    while(npp != NULL && (lwindowmax == -1 || count < lwindowmax))
        {
        count++;
        w_exposenote(*npp, ERRINLOG);
        npp = (struct Note **) LLM_next(&root);
        }
    LLM_free(&root);

    if(npp != NULL && count == lwindowmax)
        fprintf(errfp, "%s:  expose halted at windowmax setting of %d\n",
                  myname, count);
    if(count)
        fprintf(errfp, "%s:  exposed %d notes for RE {%s}\n",
                  myname, count, title);
    }

cmd_hide(lp)
    struct ps_component *lp;
    {
    int  count;
    int  res;
    struct Note **npp;
    struct LLM_root root;

    if(debug_on) fprintf(stderr, "cmd_hide\n");
    if(lp == NULL || lp->ps_next == NULL)
        {
        fprintf(errfp, "%s:  hide command requires title RE parameter\n",
            myname);
        return;
        }
    if(debug_on)
        fprintf(stderr, "hide {%s} {%s}\n", lp->ps_text,
            lp->ps_next->ps_text);
    if(strcmp(lp->ps_text, "title") != 0)
        {
        fprintf(errfp, "%s:  hide command requires title keyword\n",
            myname);
        return;
        }
    lp = lp->ps_next;
    /*
        This is slightly different than for expose - we will probably get
        matches for notes that are already hidden because of the way
        w_matchingnotes works.  It knows how to a) find all non-visible notes,
        and b) all notes.
    */
    res = w_matchingnotes(&root, lp->ps_text, 0);
    switch(res)
        {
        case  -1 :
            fprintf(errfp, "%s:  Invalid RE for hide command {%s}\n",
                myname, lp->ps_text);
            return;
            break;
        case  -2 :
            fprintf(errfp, "%s:  Memory allocation failure during hide\n",
                myname);
            LLM_free(&root);
            return;
            break;
        }
    count = 0;
    npp = (struct Note **) LLM_first(&root);
    if(npp == NULL)
        {
        fprintf(errfp, "%s:  No matches found to hide for RE {%s}\n",
            myname, lp->ps_text);
        }
    while(npp != NULL)
        {
        /*
            Make sure the note is really visible; note that we do not hide
            veiled notes.
        */
        if((*npp)->state == Visible && (*npp)->mapped)
            {
            count++;
            w_hidenote(*npp);
            }
        npp = (struct Note **) LLM_next(&root);
        }
    LLM_free(&root);
    fprintf(errfp, "%s:  hid %d notes for RE {%s}\n",
            myname, count, lp->ps_text);
    }

cmd_movenote(lp)
    struct ps_component *lp;
    {
    int  gotsrc, gotdst, gottitle, res, count;
    struct SubDir *src_sp, *dst_sp, *sp;
    struct Note **npp;
    struct LLM_root root;
    char src[MAXBUFLEN];
    char dst[MAXBUFLEN];
    char title[MAXBUFLEN];

    if(debug_on) fprintf(stderr, "cmd_movenote\n");
    gotsrc = gotdst = gottitle = 0;
    while(lp != NULL)
        {
        if(lp->ps_next == NULL) /* go by two's, so we need another param */
            {
            fprintf(errfp, "%s:  Incorrect parameters for move\n",
                myname);
            return;
            }
        if(strcmp(lp->ps_text, "source") == 0)
            {
            lp = lp->ps_next;
            strcpy(src, lp->ps_text);
            gotsrc = 1;
            lp = lp->ps_next;
            }
        else if(strcmp(lp->ps_text, "title") == 0)
            {
            lp = lp->ps_next;
            strcpy(title, lp->ps_text);
            gottitle = 1;
            lp = lp->ps_next;
            }
        else if(strcmp(lp->ps_text, "destination") == 0)
            {
            lp = lp->ps_next;
            strcpy(dst, lp->ps_text);
            gotdst = 1;
            lp = lp->ps_next;
            }
        else
            {
            fprintf(errfp, "%s:  Incorrect parameter {%s} for move\n",
                myname, lp->ps_text);
            return;
            }
        }
    if(!gotsrc)
        {
        fprintf(errfp, "%s:  Missing source folder for move\n",
            myname);
        return;
        }
    if(!gotdst)
        {
        fprintf(errfp, "%s:  Missing destination folder for move\n",
            myname);
        return;
        }
    if(!gottitle)
        {
        fprintf(errfp, "%s:  Missing title for move\n",
            myname);
        return;
        }
    src_sp = dst_sp = NULL;
    sp = (struct SubDir *) LLM_first(&subdir_rt);
    while(sp != NULL)
        {
        if(strcmp(src, sp->subdir) == 0) src_sp = sp;
        if(strcmp(dst, sp->subdir) == 0) dst_sp = sp;
        sp = (struct SubDir *) LLM_next(&subdir_rt);
        }
    if(src_sp == NULL)
        {
        fprintf(errfp, "%s:  Bad source folder {%s} for move\n",
            myname, src);
        return;
        }
    if(dst_sp == NULL)
        {
        fprintf(errfp, "%s:  Bad destination folder {%s} for move\n",
            myname, dst);
        return;
        }
    res = w_matchingnotes(&root, title, 0);
    switch(res)
        {
        case  -1 :
            fprintf(errfp, "%s:  Invalid RE for move command {%s}\n",
                myname, title);
            return;
            break;
        case  -2 :
            fprintf(errfp, "%s:  Memory allocation failure during move\n",
                myname);
            LLM_free(&root);
            return;
            break;
        }
    count = 0;
    npp = (struct Note **) LLM_first(&root);
    while(npp != NULL)
        {
        if((*npp)->sp == src_sp) /* Make sure it's in the src folder */
            {
            if(debug_on)
                fprintf(stderr, "Moving {%s} from {%s} to {%s}...\n",
                    (*npp)->ntitle, src, dst);
            res = w_movenote(src_sp, dst_sp, *npp, ERRINLOG);
            if(res == 0)
                {
                if(debug_on) fprintf(stderr, "    ...succeeded\n");
                count++;
                }
            else if(debug_on) fprintf(stderr, "    ...failed\n");
            }
        npp = (struct Note **) LLM_next(&root);
        }
    fprintf(errfp, "%s:  %d notes moved successfully\n", myname, count);
    }

cmd_newfolder(lp)
    struct ps_component *lp;
    {
    if(debug_on) fprintf(stderr, "cmd_newfolder\n");
    if(lp == NULL || lp->ps_next == NULL || strcmp(lp->ps_text, "folder") != 0)
        {
        fprintf(errfp,
            "%s:  newfolder command requires folder name parameter\n", myname);
        return;
        }
    lp = lp->ps_next;
    w_newfolder(lp->ps_text, ERRINLOG);
    }

cmd_newnote(lp)
    struct ps_component *lp;
    {
    int  x, y;
    int  gotrect, gottitle, gotfile, gotfolder;
    struct SubDir *sp;
    NoteState state;
    Rect rect;
    char folder[MAXBUFLEN];
    char title[MAXBUFLEN];
    char file[MAXBUFLEN];

    if(debug_on) fprintf(stderr, "cmd_newnote\n");
    gotrect = gottitle = gotfile = gotfolder = 0;

    /* set up good defaults in case only half is specified */
    w_popupxy(&x, &y, DEFWIDTH, DEFHEIGHT, DEFSPACING);
    rect.r_left = x;
    rect.r_top = y;
    rect.r_width = DEFWIDTH;
    rect.r_height = DEFHEIGHT;

    state = Visible;
    while(lp != NULL)
        {
        if(strcmp(lp->ps_text, "folder") == 0)
            {
            if(lp->ps_next == NULL)
                {
                fprintf(errfp, "%s:  Missing folder name\n", myname);
                return;
                }
            lp = lp->ps_next;
            strcpy(folder, lp->ps_text);
            gotfolder = 1;
            lp = lp->ps_next;
            }
        else if(strcmp(lp->ps_text, "title") == 0)
            {
            if(lp->ps_next == NULL)
                {
                fprintf(errfp, "%s:  Missing title\n", myname);
                return;
                }
            lp = lp->ps_next;
            strcpy(title, lp->ps_text);
            gottitle = 1;
            lp = lp->ps_next;
            }
        else if(strcmp(lp->ps_text, "size") == 0)
            {
            if(lp->ps_next == NULL || lp->ps_next->ps_next == NULL)
                {
                fprintf(errfp, "%s:  Missing width or height\n", myname);
                return;
                }
            gotrect = 1;
            lp = lp->ps_next;
            rect.r_width = atoi(lp->ps_text);
            lp = lp->ps_next;
            rect.r_height = atoi(lp->ps_text);
            lp = lp->ps_next;
            }
        else if(strcmp(lp->ps_text, "location") == 0)
            {
            if(lp->ps_next == NULL || lp->ps_next->ps_next == NULL)
                {
                fprintf(errfp, "%s:  Missing x or y location\n", myname);
                return;
                }
            gotrect = 1;
            lp = lp->ps_next;
            rect.r_left = atoi(lp->ps_text);
            lp = lp->ps_next;
            rect.r_top = atoi(lp->ps_text);
            lp = lp->ps_next;
            }
        else if(strcmp(lp->ps_text, "hidden") == 0)
            {
            state = Hidden;
            lp = lp->ps_next;
            }
        else if(strcmp(lp->ps_text, "visible") == 0)
            {
            state = Visible;
            lp = lp->ps_next;
            }
        else if(strcmp(lp->ps_text, "veiled") == 0)
            {
            state = Veiled;
            lp = lp->ps_next;
            }
        else if(strcmp(lp->ps_text, "file") == 0)
            {
            if(lp->ps_next == NULL)
                {
                fprintf(errfp, "%s:  Missing file name\n", myname);
                return;
                }
            lp = lp->ps_next;
            strcpy(file, lp->ps_text);
            gotfile = 1;
            lp = lp->ps_next;
            }
        else
            {
            fprintf(errfp, "%s:  Invalid parameter {%s}\n",
                myname, lp->ps_text);
            return;
            }
        }
    if(!gotfolder) strcpy(folder, "Miscellaneous");
    sp = (struct SubDir *) LLM_first(&subdir_rt);
    while(sp != NULL)
        {
        if(strcmp(folder, sp->subdir) == 0) break;
        sp = (struct SubDir *) LLM_next(&subdir_rt);
        }
    if(sp == NULL)
        {
        fprintf(errfp, "%s:  Invalid folder {%s}\n",
            myname, folder);
        return;
        }
    w_newnote(sp, state, gotrect ? &rect : NULL, gottitle ? title : NULL,
        gotfile ? file : NULL, ERRINLOG);
    }

/*
    This guy is a half baked state machine for printing notes.

    State 1 = Doing nothing.  Process command and get list of notes to print.
              Start queuing of first note.
    State 2 = Printing and called by child_death.  Kick off next queue command.
    State 3 = Printing and got a real print command.  Add notes to list to
              be printed.
    State 4 = Not printing, called on child death, do nothing.

    The state machine is controlled by the variables:  printing, lp, adding
*/

cmd_printnote(lp)
    struct ps_component *lp;
    {
    int  adding = 0;
    struct Note **tnpp1, **tnpp2;
    struct LLM_root addroot;
    static int printing = 0;
    static int  gotfldr, gottitle, res, count;
    static struct SubDir *fldr_sp, *sp;
    static struct Note **npp;
    static struct LLM_root root;
    static char fldr[MAXBUFLEN];
    static char title[MAXBUFLEN];

    if(debug_on) fprintf(stderr, "cmd_printnote\n");
    if(!printing && lp == NULL) return;
    if(printing && lp != NULL)
        {
        /* need to add to list! */
        adding = 1;
        }
    if(printing && lp == NULL)
        {
        npp = (struct Note **) LLM_next(&root);
        if(npp == NULL)
            {
            printing = 0;
            LLM_free(&root);
            if(debug_on)
                fprintf(stderr, "Printed %d notes\n", count);
            }
        else
            {
            if(debug_on)
                fprintf(stderr, "Printing {%s} ...\n", (*npp)->ntitle);
            w_printnote(*npp, ERRINLOG);
            count++;
            }
        return;
        }
    gotfldr = gottitle = 0;
    while(lp != NULL)
        {
        if(lp->ps_next == NULL) /* go by two's, so we need another param */
            {
            fprintf(errfp, "%s:  Incorrect parameters for print\n",
                myname);
            return;
            }
        if(strcmp(lp->ps_text, "folder") == 0)
            {
            lp = lp->ps_next;
            strcpy(fldr, lp->ps_text);
            gotfldr = 1;
            lp = lp->ps_next;
            }
        else if(strcmp(lp->ps_text, "title") == 0)
            {
            lp = lp->ps_next;
            strcpy(title, lp->ps_text);
            gottitle = 1;
            lp = lp->ps_next;
            }
        else
            {
            fprintf(errfp, "%s:  Incorrect parameter {%s} for print\n",
                myname, lp->ps_text);
            return;
            }
        }
    if(!gotfldr)
        {
        fprintf(errfp, "%s:  Missing source folder for print\n",
            myname);
        return;
        }
    if(!gottitle)
        {
        fprintf(errfp, "%s:  Missing title for print\n",
            myname);
        return;
        }
    fldr_sp = NULL;
    sp = (struct SubDir *) LLM_first(&subdir_rt);
    while(sp != NULL)
        {
        if(strcmp(fldr, sp->subdir) == 0) fldr_sp = sp;
        sp = (struct SubDir *) LLM_next(&subdir_rt);
        }
    if(fldr_sp == NULL)
        {
        fprintf(errfp, "%s:  Bad source folder {%s} for print\n",
            myname, fldr);
        return;
        }
    if(adding)
        {
        res = w_matchingnotes(&addroot, title, 0);
        }
    else
        {
        res = w_matchingnotes(&root, title, 0);
        }
    switch(res)
        {
        case  -1 :
            fprintf(errfp, "%s:  Invalid RE for print command {%s}\n",
                myname, title);
            return;
            break;
        case  -2 :
            fprintf(errfp, "%s:  Memory allocation failure during print\n",
                myname);
            LLM_free(&root);
            return;
            break;
        }
    /* get rid of matching titles not in target folder */
    if(adding)
        {
        tnpp1 = (struct Note **) LLM_first(&addroot);
        while(tnpp1 != NULL)
            {
            if((*tnpp1)->sp != fldr_sp)
                {
                LLM_delete(&addroot, (char *)tnpp1);
                tnpp1 = (struct Note **) LLM_first(&addroot); /* restart */
                }
            else
                {
                tnpp1 = (struct Note **) LLM_next(&addroot);
                }
            }
        /* now append the new list onto the old one we are printing from */
        tnpp1 = (struct Note **) LLM_first(&addroot);
        while(tnpp1 != NULL)
            {
            tnpp2 = (struct Note **) LLM_add(&root);
            if(tnpp2 == NULL) /* Gak! */
                {
                fprintf(errfp,
                    "%s:  Memory allocation failure during print\n",
                    myname);
                LLM_free(&addroot);
                return;
                }
            *tnpp2 = *tnpp1;
            tnpp1 = (struct Note **) LLM_next(&addroot);
            }
        LLM_free(&addroot);
        /* no need to go further, printing should be in process */
        return;
        }
    else
        {
        npp = (struct Note **) LLM_first(&root);
        while(npp != NULL)
            {
            if((*npp)->sp != fldr_sp)
                {
                LLM_delete(&root, (char *)npp);
                npp = (struct Note **) LLM_first(&root); /* restart */
                }
            else
                {
                npp = (struct Note **) LLM_next(&root);
                }
            }
        }
    count = 0;
    printing = 1;
    npp = (struct Note **) LLM_first(&root);
    if(npp == NULL)
        {
        printing = 0;
        LLM_free(&root);
        if(debug_on)
            fprintf(stderr, "Printed %d notes\n", count);
        }
    else
        {
        if(debug_on)
            fprintf(stderr, "Printing {%s} ...\n", (*npp)->ntitle);
        w_printnote(*npp, ERRINLOG);
        count++;
        }
    }

cmd_quit(lp)
    struct ps_component *lp;
    {
    if(debug_on) fprintf(stderr, "cmd_quit\n");
    fprintf(errfp, "%s:  Exiting due to quit command\n", myname);
    (void) fclose(errfp);
    (void) unlink(ctrlnam);
    (void) unlink(ctrllck);
    cleanup(0);
    }

cmd_rename(lp)
    struct ps_component *lp;
    {
    if(debug_on) fprintf(stderr, "cmd_rename\n");
    fprintf(errfp, "%s:  Unimplemented command\n", myname);
    }

cmd_veil(lp)
    struct ps_component *lp;
    {
    int  count;
    int  res;
    struct Note **npp;
    struct LLM_root root;

    if(debug_on) fprintf(stderr, "cmd_veil\n");
    if(lp == NULL || lp->ps_next == NULL)
        {
        fprintf(errfp, "%s:  veil command requires title RE parameter\n",
            myname);
        return;
        }
    if(debug_on)
        fprintf(stderr, "veil {%s} {%s}\n", lp->ps_text,
            lp->ps_next->ps_text);
    if(strcmp(lp->ps_text, "title") != 0)
        {
        fprintf(errfp, "%s:  veil command requires title keyword\n",
            myname);
        return;
        }
    lp = lp->ps_next;
    /*
        This is slightly different than for expose - we will probably get
        matches for notes that are already hidden because of the way
        w_matchingnotes works.  It knows how to a) find all non-visible notes,
        and b) all notes.
    */
    res = w_matchingnotes(&root, lp->ps_text, 0);
    switch(res)
        {
        case  -1 :
            fprintf(errfp, "%s:  Invalid RE for veil command {%s}\n",
                myname, lp->ps_text);
            return;
            break;
        case  -2 :
            fprintf(errfp, "%s:  Memory allocation failure during veil\n",
                myname);
            LLM_free(&root);
            return;
            break;
        }
    count = 0;
    npp = (struct Note **) LLM_first(&root);
    if(npp == NULL)
        {
        fprintf(errfp, "%s:  No matches found to veil for RE {%s}\n",
            myname, lp->ps_text);
        }
    while(npp != NULL)
        {
        /*
            Make sure the note is really visible; note that we do not veil
            hidden notes.
        */
        if((*npp)->state == Visible && (*npp)->mapped)
            {
            count++;
            w_veilnote(*npp);
            }
        npp = (struct Note **) LLM_next(&root);
        }
    LLM_free(&root);
    fprintf(errfp, "%s:  veiled %d notes for RE {%s}\n",
            myname, count, lp->ps_text);
    }
