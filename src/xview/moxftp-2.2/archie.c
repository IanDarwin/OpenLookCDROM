/*
 * Copyright (c) 1993 The Regents of the University of Texas System.
 * Copyright (c) 1994 The Regents of the University of Texas System.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted provided
 * that the above copyright notice and this paragraph are duplicated in all
 * such forms and that any documentation, advertising materials,  and other
 * materials  related to such  distribution  and use  acknowledge  that the
 * software  was  developed  by the  University of Texas.  The  name of the
 * University may not be  used to endorse or promote  products derived from
 * this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
 * MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */


#ifndef lint
static char copyright[] =
"@(#) Copyright (c) 1993 The Regents of the University of Texas System.\n\
 All rights reserved.\n";
static char rcsid[] =
"$Id: archie.c,v 1.2 1994/05/20 20:10:54 jones Exp $\n";
static char rcspath[] =
"$Source: /mintaka/u0/xx/ss/jones/jones.old/mftp.src/moxftp-2.0/RCS/archie.c,v $\n";
#endif

/* $Log: archie.c,v $
 * Revision 1.2  1994/05/20  20:10:54  jones
 * Declair archieInputId as XtInputId instead of int.
 *
 * Revision 1.1  1994/03/14  18:55:53  jones
 * Initial revision
 *
 */

#include "machine.h"
#include "defs.h"
#include "popen.h"
#include "wc.h"
#include "List.h"
#include "proto/archie.h"

extern XtAppContext  App_context;
extern Widget        Top;

#if defined(XAW)||defined(OPENWINDOW)
#include <X11/Xlib.h>
#define XXsensitive  XtNsensitive
#endif
#if defined(MOTIF)
#include <Xm/Xm.h>
#define XXsensitive  XmNsensitive
#endif

#define SEARCH_CSSS    0x000001 	/* Case Sensitive Substring Search */
#define SEARCH_ESM     0x000002		/* Exact String Match */
#define SEARCH_RES     0x000004		/* Regular experssion search */
#define SEARCH_CISS    0x000008		/* Case Insensitive substring search */
#define NICE_DEFAULT   0x000010		/* Defulat nice 0 */
#define NICE_NICE      0x000020		/* Nice 500 */
#define NICE_NICER     0x000040		/* Nicer 1000 */
#define NICE_VERY      0x000080		/* Very nice 5000 */
#define NICE_EXTERM    0x000100		/* Extermly nice 10000 */
#define NICE_NICEST    0x000200		/* Nicest level 32765 */
#define NUM_95         0x000400		/* Maxium number of hist  95  */
#define NUM_200        0x000800		/* Maxium number of hist  200 */
#define NUM_400        0x001000		/* Maxium number of hist  400 */
#define NUM_800        0x002000		/* Maxium number of hist  800 */
#define SORT_AGE       0x004000		/* Sort by age */
#define SORT_NAME      0x008000		/* Sort by name */
#define SORT_SIZE      0x010000		/* Sort by size */
#define SORT_NORMAL    0x020000		/* normal Sort  */
#define SORT_REVERSE   0x040000		/* reverse Sort */
#define DO_SEARCH      0x100000		/* Do search */
#define SEARCH_TEXT    0x200000		/* Search widget */
#define HOST           0x400000		/* Host */

static archie_search_type = SEARCH_ESM;
static archie_nice_type = NICE_DEFAULT;
static archie_num_type = NUM_95;
static archie_sort_type = SORT_AGE;
static archie_reverse_type = SORT_NORMAL;

static int archie_version = -1;
static Widget default_host = NULL;
static char *default_host_name = NULL;
static XtInputId archieInputId;    
static FILE *ARCHIE = NULL;
static Widget archie_Shell = NULL;
static Widget List_w  = NULL;
static Widget Item_w  = NULL;
static void set_item_title_a();
static void sort_by();
static void archie_once();
static void clear_archie();

static struct {
      int type;
      char *name;
} archie_types[] = {
    {SEARCH_CSSS,   "csss"},
    {SEARCH_ESM,    "esm"},
    {SEARCH_RES,    "res"},
    {SEARCH_CISS,   "ciss"},
    {NICE_DEFAULT,  "nice_default"},
    {NICE_NICE,     "nice_nice"},
    {NICE_NICER,    "nice_nicer"},
    {NICE_VERY,     "nice_very"},
    {NICE_EXTERM,   "nice_exterm"},
    {NICE_NICEST,   "nice_nicest"},
    {NUM_95,	    "95"},
    {NUM_200,	    "200"},
    {NUM_400,	    "400"},
    {NUM_800,	    "800"},
    {SORT_AGE,      "sort_by_age"},
    {SORT_NAME,     "sort_by_name"},
    {SORT_SIZE,     "sort_by_size"},
    {SORT_NORMAL,   "sort_normal"},
    {SORT_REVERSE,  "sort_reverse"},
    {DO_SEARCH,	    "do_search"},
    {SEARCH_TEXT,   "search_text"},
    {HOST,          "host"},
    {0,		     NULL},
}; 

#define MAX_HOSTS 100
static int nhost = 0;
static int host_index = 0;
static char *hosts[MAX_HOSTS];
static char *hosts_title[MAX_HOSTS];
        
#define MAX_ACTIONS 500
static int num_archie_actions = 0;
static struct _archie_actions {
    int type;
    char *name;
    Widget w;
} archie_actions[MAX_ACTIONS];


char **archie_text = NULL;
int  *archie_select = NULL;
int   archie_num = 0;
int   archie_n = 0;



    


void
Check_Archie()
{
    FILE *archie;
    char string[100];
    char *cp, *where;
    char *host;
    int i;

    archie = lpopen("type archie", "r");
    if (fgets(string, sizeof(string), archie) != NULL) {
	if ((cp = bsdstrstr(string, "not found")) != NULL) {
    	     lpclose(archie);
	     return;
	}
    }
    lpclose(archie);
   
    archie = lpopen("archie -v", "r");
    if (fgets(string, sizeof(string), archie) != NULL) {
	if ((cp = bsdstrstr(string, "version 1.")) != NULL) {
		archie_version = atoi(cp+sizeof("version 1.")-1);
	}
    }
    lpclose(archie);
    if (archie_version >= 2) {
        archie = lpopen("archie -L", "r");
	while (fgets(string, sizeof(string), archie) != NULL) {
	    if (bsdstrstr(string, "default Archie server")) {
		for (i=0; i<nhost; i++) {
		    if (bsdstrstr(string, hosts[i])) {
			default_host_name = hosts[i];
			break;
		    }
		}
	    }
	    if(string[0] != '	') continue;
	    cp = INDEX(string, ' ');
	    if (!cp) continue;
	    *cp = '\0';
	    cp++;
	    where = cp;
	    cp = INDEX(where, '\n');
	    if (cp) {
	        *cp = '\0';
	    }
	    if (INDEX(string, '.') != NULL &&
		INDEX(string, ' ') == NULL) {
		add_archie_host(&string[1], where);
	    }
	}
        lpclose(archie);
    }
}

void
Init_Archie()
{

#if defined(XXX)
    XtAppAddActions(App_context, Actions,
                             sizeof(Actions)/sizeof(XtActionsRec));
#endif
    RCALL("archie_hosts"      , archie_hosts_cb);
    RCALL("archie_noop"       , archie_noop_cb);
    RCALL("do_archie"         , do_archie_cb);
    RCALL("abort_archie"      , abort_archie_cb);
    RCALL("Register_archie"   , register_archie_cb);
    RCALL("archie"            , archie_cb);
    RCALL("archie_notify"     , archie_notify_cb);

}

static void
abort_archie_cb(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{
    if (ARCHIE != NULL) {
        XtRemoveInput(archieInputId);
	lpabort(ARCHIE);
        ARCHIE = NULL;
    }
    archie_on();
}

static void
add_archie_host(host, where)
char *host;
char *where;
{
    char *title;

    if (nhost >= MAX_HOSTS) return;
    hosts[nhost] = XtNewString(host);
    title = concat(NULL, host);
    title = concat(title, " ");
    title = concat(title, where);
    hosts_title[nhost] =  title;
    nhost++;
}

static void
add_archie_text(s, selected)
char *s;
int  selected;
{

    if (!archie_text || archie_n >= archie_num) {
	if (archie_text) {
            archie_text = 
		(char **)XtRealloc((char *)archie_text, 
			           2*archie_num*sizeof(char *));
            archie_select = 
		(int *)XtRealloc((char *)archie_select, 
				 2*archie_num*sizeof(int));	
	    archie_num *= 2;
	} else {
	    archie_text = (char **)XtMalloc(3000*sizeof(char *));
 	    archie_select = (int*)XtMalloc(3000*sizeof(int));
	    archie_num = 3000;
	}
    }

    if (s)  {
	archie_text[archie_n] = s;
	if (selected) {
	    archie_select[archie_n] = 1;
	} else {
	    archie_select[archie_n] = 0;
        }
	archie_n++;
 	return;
    }
    archie_text[archie_n] = NULL;
    archie_select[archie_n] = 0;
}

static void
archie_cb(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{
    archie_once();
}

static void
archie_hosts_cb(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{

    if (nhost == 0) {
 	add_archie_host("archie.ans.net", "(USA [NY])");
 	add_archie_host("archie.rutgers.edu", "(USA [NJ])");
 	add_archie_host("archie.sura.net", "(USA [MD])");
	default_host_name = "archie.sura.net";
 	add_archie_host("archie.mcgill.ca","(Canada)");
 	add_archie_host("archie.funet.fi", "(Finland/Mainland Europe)");
 	add_archie_host("archie.au", "Australia)");
 	add_archie_host("archie.doc.ic.ac.uk", "(Great Britain/Ireland)");
 	add_archie_host("archie.wide.ad.jp", "(Japan)");
    }
    for (host_index=0; host_index<nhost; host_index++) {
#if defined(OPENWINDOW)

        WcCreateChildrenCB(w,"*ArchieHost*pane, archiehost", NULL);

#else
        WcCreateChildrenCB(w,"*archiehost archie_host", NULL);
#endif
    }
}

static void
archie_noop_cb(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{
    Arg Insensitive[1];

    XtSetArg(Insensitive[0], XXsensitive, FALSE);
    if (archie_version == -1) XtSetValues(w , Insensitive, 1);
}

static void
archie_notify_cb(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{
    MyXawListReturnStruct *item = (MyXawListReturnStruct*)call_data;
    int i;
    char *host, *file, *cp;

    for (i=0; i<archie_n; i++) {
	if (archie_select[i]) {
	    archie_select[i] = 0;
            MyXawListHighlight(w, i);
	}
    }
	
    if (archie_n > item->list_index && item->list_index >= 0) {
        archie_select[item->list_index] = LIST_SELECT;
        MyXawListHighlight(w, item->list_index);
    } else {
	return;
    }
    host = XtNewString(&archie_text[item->list_index][15]);
    cp = INDEX(host, ' ');
    *cp = '\0';
    file = XtNewString(&archie_text[item->list_index][15+52]);
    cp = INDEX(file, ' ');
    if (cp != NULL) {
	*cp = '\0';
    	cp--;
    } else {
	cp = file + strlen(file) - 1;
    }
    if (cp && *cp != '/') {
	cp = RINDEX(file, '/');
	if (cp != NULL && cp != file) *cp = '\0';
    }
    Archie_Host(host, file);
    XtFree(host);
    XtFree(file);
}

static void
archie_off()
{
    Arg Insensitive[1];
    Arg Sensitive[1];
    Widget w = NULL;

    XtSetArg(Sensitive[0],   XXsensitive, TRUE);
    XtSetArg(Insensitive[0], XXsensitive, FALSE);

    SetWidget(w, "*archie_search", 0);
    XtSetValues(w , Insensitive, 1);
}

static void
archie_on()
{
    Arg Sensitive[1];
    Arg Insensitive[1];
    Widget w = NULL;

    XtSetArg(Insensitive[0], XXsensitive, FALSE);
    XtSetArg(Sensitive[0],   XXsensitive, TRUE);

    SetWidget(w, "*archie_search", 0);
    XtSetValues(w , Sensitive, 1);
}

static void
archie_once()
{
    XWMHints wmhints;

    if (!archie_Shell) {
        archie_Shell = WcFullNameToWidget(Top, "*Shellarchie"); 
        if (!archie_Shell) {
	    WcCreateNamedPopups ( Top, "Shellarchie" );
        }
        archie_Shell = WcFullNameToWidget(Top, "*Shellarchie"); 
	if (!archie_Shell) return;
    }
    XtPopup(archie_Shell, XtGrabNone);
    Set_Icon(archie_Shell, "archie");
    WM_DELETE(archie_Shell, clear_archie, "archie_Shel");
    SetWidget(List_w, "*archie_lw.list", 0);
    if (archie_n == 0) {
        add_archie_text(NULL, 0);
        if (List_w) MyXawListChange(List_w, archie_text, 0, 0, 1, 
		                     archie_select, 0, -1);
	set_item_title_a(0);
    }
}

static void
archie_search(text)
char *text;
{
    char *string;
    char temp[1024];
    int fd;
    int i;
    void read_archie();

    archie_off();
   
    if (text == NULL || text[0] == '\0') return;
    string = concat(NULL, "archie -l ");
    switch(archie_search_type) {
   	case SEARCH_CSSS:
	    string = concat(string, " -c ");
	    break;
   	case SEARCH_ESM:
	    string = concat(string, " -e ");
	    break;
   	case SEARCH_RES:
	    string = concat(string, " -r ");
	    break;
  	case SEARCH_CISS:
	    string = concat(string, " -s ");
	    break;
    }
    switch (archie_nice_type) {
	case NICE_DEFAULT:
	    string = concat(string, " -N0 ");
	    break;
	case NICE_NICE:
	    string = concat(string, " -N500 ");
	    break;
	case NICE_NICER:
	    string = concat(string, " -N1000 ");
	    break;
	case NICE_VERY:
	    string = concat(string, " -N5000 ");
	    break;
	case NICE_EXTERM:
	    string = concat(string, " -N10000 ");
	    break;
	case NICE_NICEST:
	    string = concat(string, " -N32765 ");
	    break;
    }
    switch (archie_num_type) {
    	case NUM_95:
	    string = concat(string, " -m95 ");
	    break;
	case NUM_200:
	    string = concat(string, " -m200 ");
	    break;
	case NUM_400:
	    string = concat(string, " -m400 ");
	    break;
	case NUM_800:
	    string = concat(string, " -m800 ");
	    break;
    }

    if (default_host) {
        for (i=0; i<num_archie_actions; i++) 
      	    if (archie_actions[i].type == HOST &&
  	        archie_actions[i].w == default_host) {
	        string = concat(string, "-h "); 
	        string = concat(string, archie_actions[i].name);
	        string = concat(string, " "); 
		break;
	    }
    }

    string = concat(string, text);
    ARCHIE = lpopen(string, "r");
    if (ARCHIE == NULL) {
        archie_on();
	return;
    }

    for (i=0; i<archie_n; i++) 
	if (archie_text[i])  {
	    XtFree(archie_text[i]);
	    archie_text[i] = NULL;
        }
    archie_n = 0;
    add_archie_text(NULL, 0);
    if (List_w) MyXawListChange(List_w, archie_text, 0, 0, 1, 
		                    archie_select, 0, -1);
    set_item_title_a(0);

    archieInputId = XtAppAddInput(App_context, fileno(ARCHIE),
                                  (XtPointer)XtInputReadMask,
                                  (XtInputCallbackProc)read_archie,
                                  (XtPointer)NULL);

}

static void
clear_archie(w, stuff, xxx)
Widget w;
char   *stuff;
char   *xxx;
{
#if defined(XAW)||defined(OPENWINDOW)
    XtPopdown(archie_Shell);
#endif
}

static void
do_archie_cb(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{
    int i,j;
    Arg Insensitive[1];
    Arg Sensitive[1];
    int temp = -1;
    char *text;
    char *GetText();

    XtSetArg(Sensitive[0],   XXsensitive, TRUE);
    XtSetArg(Insensitive[0], XXsensitive, FALSE);

    for (i=0; i<num_archie_actions; i++) {
        if (archie_actions[i].w == w) {
	    switch(archie_actions[i].type) {
    		case SEARCH_CSSS:
    		case SEARCH_ESM:
    		case SEARCH_RES:
    		case SEARCH_CISS:
		    for (j=0; j<num_archie_actions; j++) 
    		  	if (archie_actions[j].type == archie_search_type) 
			    XtSetValues(archie_actions[j].w , Sensitive, 1);
   		    archie_search_type = archie_actions[i].type;
		    for (j=0; j<num_archie_actions; j++) 
    		      	if (archie_actions[j].type == archie_search_type) 
			    XtSetValues(archie_actions[j].w , Insensitive, 1);
		    return;
    	       	case NUM_95:
	       	case NUM_200:
	       	case NUM_400:
	       	case NUM_800:
		    for (j=0; j<num_archie_actions; j++) 
    		  	if (archie_actions[j].type == archie_num_type) 
			    XtSetValues(archie_actions[j].w , Sensitive, 1);
   		    archie_num_type = archie_actions[i].type;
		    for (j=0; j<num_archie_actions; j++) 
    		      	if (archie_actions[j].type == archie_num_type) 
			    XtSetValues(archie_actions[j].w , Insensitive, 1);
		    return;
		case  NICE_DEFAULT:
		case  NICE_NICE:
		case  NICE_NICER:
		case  NICE_VERY:
		case  NICE_EXTERM:
		case  NICE_NICEST:
		    for (j=0; j<num_archie_actions; j++) 
    		  	if (archie_actions[j].type == archie_nice_type) 
			    XtSetValues(archie_actions[j].w , Sensitive, 1);
   		    archie_nice_type = archie_actions[i].type;
		    for (j=0; j<num_archie_actions; j++) 
    		      	if (archie_actions[j].type == archie_nice_type) 
			    XtSetValues(archie_actions[j].w , Insensitive, 1);
		    return;
    		case SORT_NORMAL:
		case SORT_REVERSE:
		    for (j=0; j<num_archie_actions; j++) 
    		  	if (archie_actions[j].type == archie_reverse_type) 
			    XtSetValues(archie_actions[j].w , Sensitive, 1);
   		    archie_reverse_type = archie_actions[i].type;
		    for (j=0; j<num_archie_actions; j++) 
    		      	if (archie_actions[j].type == archie_reverse_type) 
			    XtSetValues(archie_actions[j].w , Insensitive, 1);
		    sort_by();
		    if (archie_n > 1 && List_w) 
		        MyXawListChange(List_w, archie_text, 0, 0, 1,
                                        archie_select, 0, -1);

		    return;
		case SORT_AGE:
		case SORT_NAME:
		case SORT_SIZE:
		    for (j=0; j<num_archie_actions; j++) 
    		  	if (archie_actions[j].type == archie_sort_type) 
			    XtSetValues(archie_actions[j].w , Sensitive, 1);
   		    archie_sort_type = archie_actions[i].type;
		    for (j=0; j<num_archie_actions; j++) 
    		      	if (archie_actions[j].type == archie_sort_type) 
			    XtSetValues(archie_actions[j].w , Insensitive, 1);
		    sort_by();
		    if (archie_n > 1 && List_w) 
		        MyXawListChange(List_w, archie_text, 0, 0, 1,
                                        archie_select, 0, -1);

		    return;
		case DO_SEARCH:
		    for (j=0; j<num_archie_actions; j++) 
    		      	if (archie_actions[j].type == SEARCH_TEXT)  {
		          	text = GetText(archie_actions[j].w);
				break;
			}
		    archie_search(text);
		    return;
		case HOST:
		    for (j=0; j<num_archie_actions; j++) 
    		      	if (archie_actions[j].type == HOST &&
    		  	    archie_actions[j].w == default_host) 
		             XtSetValues(archie_actions[j].w , Sensitive, 1);
		    default_host = archie_actions[i].w;
		    for (j=0; j<num_archie_actions; j++) 
    		      	if (archie_actions[j].type == HOST &&
    		  	    archie_actions[j].w == default_host) 
			    XtSetValues(archie_actions[j].w , Insensitive, 1);
		    return;
	    }
	}
    }
 
}

static void
read_archie(master, source, id)
XtPointer master;
int       *source;
XtInputId *id;
{
    char temp[1024];
    char host[1024];
    char date[1024];
    char file[1024];
    char buff[1024*4];
    char *text;
    int year, mon, day, hour, min;
    int  size;
  

    if (fgets(temp, sizeof(temp), ARCHIE) != NULL) {
	sscanf(temp, "%s %d %s %s\n", date, &size, host, file);
        sscanf(date, "%4d%2d%2d%2d%2d", &year, &mon, &day, &hour, &min);
	sprintf(buff, "%02d/%02d/%02d %2d:%02d %-40s %10d %s", 
	                year-1900, mon, day, hour, min, host, size,
			file);
        text = XtNewString(buff);
        add_archie_text(text, 0);
    } else {
	XtRemoveInput(archieInputId);
	lpclose(ARCHIE);
	ARCHIE = NULL;
        archie_on();
        add_archie_text(NULL, 0);
	sort_by();
	if (List_w) MyXawListChange(List_w, archie_text, 0, 0, 1, 
		                    archie_select, 0, -1);
	set_item_title_a(archie_n);
   }
}

static void
set_item_title_a(n)
int n;
{
    char string[100];

    SetWidget(Item_w, "*archie_lw.items", 0);
    if (n == 0) {
         string[0] = '\0';
    } else if (n == 1) {
         sprintf(string, "%d item", n);
    } else {
         sprintf(string, "%d items", n);
    }
    SetLabel (Item_w,  string);
}

static void
sort_by()
{
    char *selected = NULL;
    int i;

    if (archie_n > 1) {
	for (i=0; i<archie_n; i++) 
	    if (archie_select[i]) {
		selected = archie_text[i];
		archie_select[i] = 0;
		break;
	    }
        switch(archie_sort_type) {
            case SORT_AGE:
            	qsort((char *)archie_text, archie_n, sizeof(char *), 
		      sort_by_age);
		break;
            case SORT_NAME:
            	qsort((char *)archie_text, archie_n, sizeof(char *), 
		      sort_by_name);
		break;
            case SORT_SIZE:
            	qsort((char *)archie_text, archie_n, sizeof(char *), 
		      sort_by_size);
		break;
	}
        if (selected) {
            for (i=0; i<archie_n; i++) {
	     	if (strcmp(selected, archie_text[i]) == 0) {
		    archie_select[i] = LIST_SELECT;
		    break;
		}
	    }
	}
    }
}

static void
register_archie_cb(w, closure, call_data)
Widget w;
XtPointer closure;
XtPointer call_data;
{
    char *t =  (char *)closure;
    int  type, i;
    Arg Insensitive[1];

    if (num_archie_actions >=MAX_ACTIONS) {
        fprintf(stderr, "Too many archie action widgets\n");
        return;
    }

    XtSetArg(Insensitive[0], XXsensitive, FALSE);

    for (i=0;  archie_types[i].name; i++) {
        if (strcmp(archie_types[i].name, t) == 0) break;
    }

    if(archie_types[i].name == NULL) {
        fprintf(stderr, "Could not find archie action %s\n", t);
        return;
    }


    archie_actions[num_archie_actions].type  = archie_types[i].type;
    archie_actions[num_archie_actions].w     = w;
    if (archie_actions[num_archie_actions].type == archie_search_type) {
	XtSetValues(archie_actions[num_archie_actions].w , Insensitive, 1);
    } else if (archie_actions[num_archie_actions].type == archie_nice_type) {
	XtSetValues(archie_actions[num_archie_actions].w , Insensitive, 1);
    } else if (archie_actions[num_archie_actions].type == archie_sort_type) {
	XtSetValues(archie_actions[num_archie_actions].w , Insensitive, 1);
    } else if (archie_actions[num_archie_actions].type == archie_reverse_type) {
	XtSetValues(archie_actions[num_archie_actions].w , Insensitive, 1);
    }

    if (archie_actions[num_archie_actions].type == HOST) {
	    SetLabel(w, hosts_title[host_index]);
	    if (default_host_name && 
		strcmp(hosts[host_index], default_host_name) == 0) {
	        XtSetValues(archie_actions[num_archie_actions].w , 
			    Insensitive, 1);
	        default_host = w;
	    }
            archie_actions[num_archie_actions].name = hosts[host_index];
    }
    num_archie_actions++;
}

static int
sort_by_age(a,b)
char **a;
char **b;
{
     int value;

     value = strncmp(*a, *b, 8);
     if (value == 0) {
	if ((*a)[9] == ' ' &&
	    (*b)[9] != ' ')
	    value = -11;
	else if ((*a)[9] != ' ' &&
	         (*b)[9] == ' ') 
	    value = 1;
	else 
	    value = strncmp(&(*a)[9], &(*b)[9], 4);
     }

     if (archie_reverse_type == SORT_REVERSE) {
        value = -value;
     }
     return value;
}

static int
sort_by_name(a,b)
char **a;
char **b;
{
    int value;

    value = strcmp((char *)&(*a)[15+52], (char *)&(*b)[15+52]);
     if (archie_reverse_type == SORT_REVERSE) {
	value = -value;
    }
    return value;
}

static int
sort_by_size(a,b)
char **a;
char **b;
{
    int value, ia, ib;

    ia = atoi((char *)&(*a)[15+41]);
    ib = atoi((char *)&(*b)[15+41]);
    value = ia - ib;
    if (archie_reverse_type == SORT_REVERSE) {
	value = -value;
    }
    return value;
}
