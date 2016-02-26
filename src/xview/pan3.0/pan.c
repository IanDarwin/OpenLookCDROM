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

#define public

#include "pan.h"
#include "patchlevel.h"
#include <signal.h>

#include <X11/X.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>

extern int errno;
extern char *sys_errlist[];

int  xverror(), xerror();

extern char *optarg;

static char init_search_val[MAXSEARCHLEN];

Notify_value aborted();

main(argc, argv)
    int  argc;
    char *argv[];
    {
    Xv_Server server;
    FILE *fp;
    char *cp;
    int  opt;

    debug_on = 0;
    move_up = 0;
    destroy_up = 0;
    search_up = 0;
    notecount = 0;
    *init_search_val = 0;

    LLM_init(&subdir_rt, sizeof(struct SubDir));
    LLM_init(&freewin_rt, sizeof(struct FreeWin));

    strcpy(myname, argv[0]);
    cp = getenv("HOME");
    if(cp == NULL)
        {
        fprintf(stderr, "%s:  Can't determine value of HOME\n", myname);
        exit(1);
        }

    /* If first time run, set up default dirs */
    sprintf(note_dir, "%s/%s", cp, NOTEDIR);
    (void) mkdir(note_dir, 0700);
    sprintf(note_dir, "%s/%s/Miscellaneous", cp, NOTEDIR);
    (void) mkdir(note_dir, 0700);

    sprintf(note_dir, "%s/%s", cp, NOTEDIR);

    server = xv_init(XV_INIT_ARGC_PTR_ARGV, &argc, argv,
        XV_ERROR_PROC, xverror,
        XV_X_ERROR_PROC, xerror,
        NULL);

    /* Process the X resources database for what we need */
    defaults_load_db(NULL);

    cp = defaults_get_string(resources[RES_IDIR].a, resources[RES_IDIR].b,
         note_dir);
    if(cp != note_dir)
        {
        strcpy(note_dir, cp);
        }

    while((opt = getopt(argc, argv, "d:x")) != -1)
        {
        switch(opt)
            {
            case 'd' : /* Initial directory */
                strcpy(note_dir, optarg);
                break;
            case 'x' : /* Turn on debugs.  Undocumented flag */
                debug_on = 1;
                break;
            }
        }

    if(chdir(note_dir) < 0)
        {
        fprintf(stderr, "%s:  Couldn't chdir to %s\n", myname, note_dir);
        exit(1);
        }

    fp = fopen(PIDNAM, "w");
    if(fp != NULL)
        {
        fprintf(fp, "%d\n", getpid());
        (void) fclose(fp);
        }

    confirmdestroy = (defaults_get_boolean(resources[RES_CDESTROY].a,
        resources[RES_CDESTROY].b, RESDEF_CDESTROY) == TRUE);

    /* Reverse boolean */
    noticenobeep = (defaults_get_boolean(resources[RES_NBEEP].a,
        resources[RES_NBEEP].b, RESDEF_NBEEP) == FALSE);

    icontransparent = (defaults_get_boolean(resources[RES_ITRANSPARENT].a,
        resources[RES_ITRANSPARENT].b, RESDEF_ITRANSPARENT) == TRUE);

    cp = defaults_get_string(resources[RES_FORDER].a, resources[RES_FORDER].b,
         RESDEF_FORDER);
    strcpy(folderorder, cp);

    cp = defaults_get_string(resources[RES_DSEARCH].a, resources[RES_DSEARCH].b,
         RESDEF_DSEARCH);
    strcpy(init_search_val, cp);

    fintitle = (defaults_get_boolean(resources[RES_FINTITLE].a,
        resources[RES_FINTITLE].b, RESDEF_FINTITLE) == TRUE);

    cp = defaults_get_string(resources[RES_TEXTFONT].a,
         resources[RES_TEXTFONT].b, RESDEF_TEXTFONT);
    if(cp != NULL)
        strcpy(textfont, cp);
    else
        *textfont = 0;

    windowmax = defaults_get_integer(resources[RES_WINDOWMAX].a,
        resources[RES_WINDOWMAX].b, RESDEF_WINDOWMAX);

    cp = defaults_get_string(resources[RES_ACTIONDEF].a,
         resources[RES_ACTIONDEF].b, RESDEF_ACTIONDEF);
    strcpy(actiondef, cp);

    menutextlen = defaults_get_integer(resources[RES_MENUTEXTLEN].a,
        resources[RES_MENUTEXTLEN].b, RESDEF_MENUTEXTLEN);
    if(menutextlen < 5) menutextlen = 5;
    if(menutextlen > MAXTITLELEN) menutextlen = MAXTITLELEN;

    setwidth = defaults_get_integer(resources[RES_SETWIDTH].a,
        resources[RES_SETWIDTH].b, RESDEF_SETWIDTH);
    if(setwidth < 0 || setwidth > 200) setwidth = RESDEF_SETWIDTH;
    sprintf(widthtext, "Width %d", setwidth);

    logging = (defaults_get_boolean(resources[RES_LOGGING].a,
        resources[RES_LOGGING].b, RESDEF_LOGGING) == TRUE);

    topmargin = defaults_get_integer(resources[RES_TOPMARGIN].a,
        resources[RES_TOPMARGIN].b, RESDEF_TOPMARGIN);
    if(topmargin < 0) topmargin = 0;

    bottommargin = defaults_get_integer(resources[RES_BOTTOMMARGIN].a,
        resources[RES_BOTTOMMARGIN].b, RESDEF_BOTTOMMARGIN);
    if(bottommargin < 0) bottommargin = 0;

    leftmargin = defaults_get_integer(resources[RES_LEFTMARGIN].a,
        resources[RES_LEFTMARGIN].b, RESDEF_LEFTMARGIN);
    if(leftmargin < 0) leftmargin = 0;

    rightmargin = defaults_get_integer(resources[RES_RIGHTMARGIN].a,
        resources[RES_RIGHTMARGIN].b, RESDEF_RIGHTMARGIN);
    if(rightmargin < 0) rightmargin = 0;

    checkinterval = defaults_get_integer(resources[RES_CHECKINTERVAL].a,
        resources[RES_CHECKINTERVAL].b, RESDEF_CHECKINTERVAL);
    if(checkinterval < 5) checkinterval = 5;

    cp = defaults_get_string(resources[RES_SEARCHMENU].a,
         resources[RES_SEARCHMENU].b, RESDEF_SEARCHMENU);
    strcpy(searchmenu, cp);
    trim(searchmenu);
    if(*searchmenu == 0) strcpy(searchmenu, RESDEF_SEARCHMENU);

    notewidth = defaults_get_integer(resources[RES_NOTEWIDTH].a,
        resources[RES_NOTEWIDTH].b, RESDEF_NOTEWIDTH);
    noteheight = defaults_get_integer(resources[RES_NOTEHEIGHT].a,
        resources[RES_NOTEHEIGHT].b, RESDEF_NOTEHEIGHT);

    setup_display();
#ifdef PAN_DND
    drag_init(server);
#endif
    restorenotes();
    xv_main_loop(main_frame);
    (void) unlink(PIDNAM);
    exit(0);
    /* NOTREACHED */
    }

/*
    Routine to set up main frame and buttons.
*/
setup_display()
    {
    char *cp;
    struct ps_component *lp, *np;
    Menu menu;
    Menu_item mitem;
    Server_image image, image2;
    Icon icon;
    static XClassHint chint;
    struct itimerval itv;
    Rect *rect;
    Panel_item item;
    int  x, num, mnum;

    main_frame = xv_create(NULL, FRAME,
                           XV_LABEL, PAN_VERSION,
                           FRAME_NO_CONFIRM, TRUE,
                           NULL);
    if(main_frame == NULL)
        {
        fprintf(stderr, "%s:  Couldn't create base frame.\n", myname);
        exit(1);
        }

    /* Set up the X class since xview doesn't */
    chint.res_name = "panmain";
    chint.res_class = "PanMain";
    XSetClassHint((Display *)xv_get(main_frame, XV_DISPLAY),
        xv_get(main_frame, XV_XID), &chint);

    notify_interpose_destroy_func(main_frame, aborted);

    main_panel = (Panel) xv_create(main_frame, PANEL, 
                     WIN_RETAINED, FALSE, 
                     NULL);

    main_newnote = xv_create(NULL, MENU,
                     MENU_NOTIFY_PROC, newnote,
                     NULL);
    item = (Panel_item) xv_create(main_panel, PANEL_BUTTON,
                     PANEL_LABEL_STRING, "New Note", 
                     PANEL_ITEM_MENU, main_newnote,
                     XV_X, xv_col(main_panel, 0),
                     XV_Y, xv_row(main_panel, 0),
                     NULL);
    rect = (Rect *) xv_get(item, PANEL_ITEM_RECT);
    x = rect->r_width + DEFPANELSPACING;
    
    item = (Panel_item) xv_create(main_panel, PANEL_BUTTON,
                     PANEL_LABEL_STRING, "Hide Notes",
                     PANEL_NOTIFY_PROC, hideall,
                     XV_X, x,
                     XV_Y, xv_row(main_panel, 0),
                     NULL);
    main_expose = xv_create(NULL, MENU,
                     MENU_NOTIFY_PROC, exposemenu,
                     MENU_STRINGS, CHOICE_ALL, NULL,
                     NULL);
    item = (Panel_item) xv_create(main_panel, PANEL_BUTTON,
                     PANEL_LABEL_STRING, "Expose Notes", 
                     PANEL_ITEM_MENU, main_expose,
                     XV_X, xv_col(main_panel, 0),
                     XV_Y, xv_row(main_panel, 1),
                     NULL);
    rect = (Rect *) xv_get(item, PANEL_ITEM_RECT);
    x = rect->r_width + DEFPANELSPACING;

    main_print = xv_create(NULL, MENU,
                     MENU_NOTIFY_PROC, actionmenu,
                     NULL);
    menu = xv_create(NULL, MENU,
                     MENU_NOTIFY_PROC, actionmenu,
                     MENU_ITEM,
                         MENU_STRING, CHOICE_FOLDER,
                         MENU_NOTIFY_PROC, createfolder,
                         NULL,
                     MENU_ITEM,
                         MENU_STRING, CHOICE_MOVE,
                         MENU_NOTIFY_PROC, movenote,
                         NULL,
                     MENU_ITEM,
                         MENU_STRING, CHOICE_PRINT,
                         MENU_PULLRIGHT, main_print,
                         NULL,
                     MENU_ITEM,
                         MENU_STRING, CHOICE_DESTROY,
                         MENU_NOTIFY_PROC, destroyfolder,
                         NULL,
                     MENU_ITEM,
                         MENU_STRING, CHOICE_QUIT,
                         NULL,
                     NULL);
    mitem = (Menu_item) xv_find(menu, MENUITEM,
                                MENU_STRING, actiondef,
                                XV_AUTO_CREATE, FALSE,
                                NULL);
    if(mitem)
        {
        mnum = xv_get(menu, MENU_NITEMS);
        num = 0;
        while(!num && mnum)
            {
            if(mitem == (Menu_item) xv_get(menu, MENU_NTH_ITEM, mnum))
                num = mnum;
            mnum--;
            }
        xv_set(menu, MENU_DEFAULT, num, NULL);
        }

    item = (Panel_item) xv_create(main_panel, PANEL_BUTTON,
                     PANEL_LABEL_STRING, "Action",
                     PANEL_ITEM_MENU, menu,
                     XV_X, x,
                     XV_Y, xv_row(main_panel, 1),
                     NULL);

    search_item = xv_create(main_panel, PANEL_TEXT,
                     PANEL_LABEL_STRING, "Search:",
                     PANEL_VALUE, init_search_val,
                     PANEL_VALUE_DISPLAY_LENGTH, MAXSEARCHDISLEN + 1,
                     PANEL_VALUE_STORED_LENGTH, MAXSEARCHLEN,
                     PANEL_NOTIFY_PROC, notesearch,
                     XV_X, xv_col(main_panel, 0),
                     XV_Y, xv_row(main_panel, 2),
                     NULL);

    rect = (Rect *) xv_get(search_item, PANEL_ITEM_RECT);
    x = rect->r_width + DEFPANELSPACING;

    menu = xv_create(NULL, MENU,
                     MENU_NOTIFY_PROC, search_menu_proc,
                     NULL);
    lp = parse_string(searchmenu, ", \t\n", 1, 1, 1);
    np = lp;
    while(np != NULL)
        {
        cp = malloc(strlen(np->ps_text) + 1);
        if(cp == NULL)
            {
            fprintf(stderr, "%s:  Couldn't allocate memory.\n", myname);
            exit(1);
            }
        strcpy(cp, np->ps_text);
        mitem = xv_create(NULL, MENUITEM,
            MENU_STRING, cp,
            MENU_RELEASE,
            MENU_RELEASE_IMAGE,
            NULL);
        xv_set(menu, MENU_APPEND_ITEM, mitem, NULL);
        np = np->ps_next;
        }
    free_ps_list(lp);
    search_button = (Panel_item) xv_create(main_panel, PANEL_ABBREV_MENU_BUTTON,
                     PANEL_ITEM_MENU, menu,
                     XV_X, x,
                     XV_Y, xv_row(main_panel, 2),
                     NULL);

    image = xv_create(NULL, SERVER_IMAGE, XV_WIDTH, 64, XV_HEIGHT, 64,
                      SERVER_IMAGE_BITS, mainicon, NULL);
    image2 = xv_create(NULL, SERVER_IMAGE, XV_WIDTH, 64, XV_HEIGHT, 64,
                      SERVER_IMAGE_BITS, mainiconmask, NULL);
    icon = xv_create(main_frame, ICON,
                     ICON_IMAGE, image,
                     ICON_MASK_IMAGE, image2,
                     ICON_LABEL, "Notes",
                     ICON_TRANSPARENT, icontransparent,
                     NULL);
    xv_set(main_frame, FRAME_ICON, icon, NULL);
    window_fit(main_panel);
    window_fit(main_frame);

    itv.it_interval.tv_sec = checkinterval;
    itv.it_interval.tv_usec = 0;
    itv.it_value.tv_sec = checkinterval;
    itv.it_value.tv_usec = 0;
    notify_set_itimer_func(main_frame, check_ctl_file, ITIMER_REAL, &itv, NULL);

    notify_set_signal_func(main_frame, check_ctl_file, SIGUSR1, NOTIFY_SYNC);

    init_menu_string_normalize();
    }

Notify_value aborted(client, status)
    Notify_client client;
    Destroy_status status;
    {
    struct SubDir *sp;
    struct Note *np;
 
    if(status == DESTROY_CHECKING) return(NOTIFY_DONE);

    sp = (struct SubDir *) LLM_first(&subdir_rt);
    while(sp != NULL)
        {
        np = (struct Note *) LLM_first(&sp->note_rt);
        while(np != NULL)
            {
            update(np);
            np = (struct Note *) LLM_next(&sp->note_rt);
            }
        sp = (struct SubDir *) LLM_next(&subdir_rt);
        }

    if(status == DESTROY_CLEANUP)
        return(notify_next_destroy_func(client, status));

    return(NOTIFY_DONE);
    }

/*
    Routine called to safely cleanup and exit pan.  Called by Action Quit menu
    item.
*/
cleanup(errnum)
    int  errnum;
    {
    struct SubDir *sp;
    struct Note *np;

    sp = (struct SubDir *) LLM_first(&subdir_rt);
    while(sp != NULL)
        {
        np = (struct Note *) LLM_first(&sp->note_rt);
        while(np != NULL)
            {
            update(np);
            updateinfo(np, NOFORCE);
            np = (struct Note *) LLM_next(&sp->note_rt);
            }
        sp = (struct SubDir *) LLM_next(&subdir_rt);
        }
    xv_destroy_safe(main_frame);
    (void) unlink(PIDNAM);
    exit(errnum);
    }

/*
    Scan the notes directory and locate all notes.  Create frames of the
    appropriate attributes for any notes found.  Called on startup or
    folder change.
*/
restorenotes()
    {
    int  found;
    char *cp;
    DIR *dp;
    struct dirent *ent;
    struct stat st;
    struct Note *np;
    struct SubDir *sp;
    struct ps_component *list, *tok;
    Menu_item mi;
    char buf[MAXBUFLEN];

    /* parse requested order */
    list = parse_string(folderorder, ", \t", 0, 0, 0);
    if(list != NULL)
        {
        tok = list;
        while(tok != NULL)
            {
            if(stat(tok->ps_text, &st) == 0 && (S_IFMT & st.st_mode) == S_IFDIR)
                {
                sp = (struct SubDir *)LLM_add(&subdir_rt);
                if(sp == NULL)
                    {
                    notice_prompt(main_frame, NULL,
                        NOTICE_MESSAGE_STRINGS,
                            "Insufficient memory - Exiting",
                            sys_errlist[errno],
                            NULL,
                        NOTICE_BUTTON_YES, "Acknowledge",
                        NOTICE_NO_BEEPING, noticenobeep,
                        NULL);
                    cleanup(1);
                    }
                strcpy(sp->subdir, tok->ps_text);
                LLM_init(&sp->note_rt, sizeof(struct Note));
                mi = xv_create(NULL, MENUITEM,
                    MENU_STRING, sp->subdir,
                    MENU_NOTIFY_PROC, newnote,
                    NULL);
                xv_set(main_newnote, MENU_APPEND_ITEM, mi, NULL);
                mi = xv_create(NULL, MENUITEM,
                    MENU_STRING, sp->subdir,
                    MENU_GEN_PULLRIGHT, gen_exposemenu,
                    NULL);
                xv_set(main_expose, MENU_APPEND_ITEM, mi, NULL);
                mi = xv_create(NULL, MENUITEM,
                    MENU_STRING, sp->subdir,
                    MENU_GEN_PULLRIGHT, gen_prtmenu,
                    NULL);
                xv_set(main_print, MENU_APPEND_ITEM, mi, NULL);
                }
            tok = tok->ps_next;
            }
        free_ps_list(list);
        }

    dp = opendir(note_dir);
    if(dp == NULL)
        {
        notice_prompt(main_frame, NULL,
            NOTICE_MESSAGE_STRINGS,
                "Couldn't read directory",
                note_dir,
                sys_errlist[errno],
                NULL,
            NOTICE_BUTTON_YES, "Acknowledge",
            NOTICE_NO_BEEPING, noticenobeep,
            NULL);
        return;
        }
    (void) re_comp("Note_[0-9][0-9]*.*\\.info$");
    while((ent = readdir(dp)) != NULL)
        {
        if(*ent->d_name != '.' && stat(ent->d_name, &st) == 0 &&
           (S_IFMT & st.st_mode) == S_IFDIR)
            {
            found = 0;
            sp = (struct SubDir *)LLM_first(&subdir_rt);
            while(sp != NULL)
                {
                if(strcmp(ent->d_name, sp->subdir) == 0)/* already processed */
                    {
                    found = 1;
                    break;
                    }
                sp = (struct SubDir *)LLM_next(&subdir_rt);
                }
            if(found) continue;
            sp = (struct SubDir *)LLM_add(&subdir_rt);
            if(sp == NULL)
                {
                notice_prompt(main_frame, NULL,
                    NOTICE_MESSAGE_STRINGS,
                        "Insufficient memory - Exiting",
                        sys_errlist[errno],
                        NULL,
                    NOTICE_BUTTON_YES, "Acknowledge",
                    NOTICE_NO_BEEPING, noticenobeep,
                    NULL);
                cleanup(1);
                }
            strcpy(sp->subdir, ent->d_name);
            LLM_init(&sp->note_rt, sizeof(struct Note));
            mi = xv_create(NULL, MENUITEM,
                MENU_STRING, sp->subdir,
                MENU_NOTIFY_PROC, newnote,
                NULL);
            xv_set(main_newnote, MENU_APPEND_ITEM, mi, NULL);
            mi = xv_create(NULL, MENUITEM,
                MENU_STRING, sp->subdir,
                MENU_GEN_PULLRIGHT, gen_exposemenu,
                NULL);
            xv_set(main_expose, MENU_APPEND_ITEM, mi, NULL);
            mi = xv_create(NULL, MENUITEM,
                MENU_STRING, sp->subdir,
                MENU_GEN_PULLRIGHT, gen_prtmenu,
                NULL);
            xv_set(main_print, MENU_APPEND_ITEM, mi, NULL);
            }
        }
    closedir(dp);
    sp = (struct SubDir *)LLM_first(&subdir_rt);
    while(sp != NULL)
        {
        sprintf(buf, "%s/%s", note_dir, sp->subdir);
        dp = opendir(buf);
        if(dp == NULL)
            {
            notice_prompt(main_frame, NULL,
                NOTICE_MESSAGE_STRINGS,
                    "Couldn't read directory",
                    note_dir,
                    sys_errlist[errno],
                    NULL,
                NOTICE_BUTTON_YES, "Acknowledge",
                NOTICE_NO_BEEPING, noticenobeep,
                NULL);
            }
        else
            {
            while((ent = readdir(dp)) != NULL)
                {
                if(re_exec(ent->d_name) == 1)
                    {
                    np = (struct Note *)LLM_add(&sp->note_rt);
                    if(np == NULL)
                        {
                        notice_prompt(main_frame, NULL,
                            NOTICE_MESSAGE_STRINGS,
                                "Insufficient memory - Exiting",
                                note_dir,
                                sys_errlist[errno],
                                NULL,
                            NOTICE_BUTTON_YES, "Acknowledge",
                            NOTICE_NO_BEEPING, noticenobeep,
                            NULL);
                        cleanup(1);
                        }
                    memset((char *)np, 0, sizeof(struct Note));
                    np->sp = sp;
                    strcpy(np->basename, ent->d_name);
                    cp = np->basename;
                    while(*cp)
                        {
                        if(*cp == '.') *cp = 0;
                        cp++;
                        }
                    setnote(np);
                    adjust_sorted(np);
                    }
                }
            closedir(dp);
            }
        sp = (struct SubDir *)LLM_next(&subdir_rt);
        }
    }

/*
   Trims trailing blanks, tabs, newlines, and carriage returns from a string 
*/

trim(start)
    register char *start;
    {
    register int  slen;
    register char *end;

    slen = strlen(start);
    if(slen == 0) return;
    end = start + slen - 1;
    while( (*end == ' ' || *end == '\t' || *end == '\n' || *end == '\r')
           && end >= start) end--;
    end++;
    *end = '\0';
    }

/* Searches for first occurrence of spattern in string */

char *instr(string, spattern)
    char *string;
    char *spattern;
    {
    register char *pos1, *pos2;

    while(*string != '\0')
        {
        pos1 = string;
        pos2 = spattern;
        while(*pos1 == *pos2)
            {
            pos1++;
            pos2++;
            if(*pos2 == '\0') return(string);
            }
        string++;
        }
    return(NULL);
    }

xverror(obj, list)
    Xv_object obj;
    Attr_avlist list;
    {
    fprintf(stderr, "XView error:  %s\n", xv_error_format(obj, list));
    kill(getpid(), SIGBUS);
    }

xerror()
    {
    if(fork() == 0)
        kill(getpid(), SIGBUS);
    return(XV_ERROR);
    }
