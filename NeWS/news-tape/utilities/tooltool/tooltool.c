#include "tooltool.h"

#define MAXITEMS 100
#define TOSTDOUT 1
#define TOFILE 2
#define INDEMO 3

struct item {
    char            itemtype[20];
    char            itemname[10];
    char            itemlabel[50];
    char            itemobject[200];
    char            itemlocation[10];
    int             itemx;
    int             itemy;
};

/*
*   Tooltool 
*
*	To make use of the LiteItem Toolkit
*	more fun.
*			Josh Axelrod
*			2/88 
*
*/

/*ARGSUSED*/
main(argc, argv)
int	argc;
char	**argv;
{
    struct item     items[MAXITEMS];
    char            itemtype[20];
    char            itemname[10];
    char            buf[20];
    int             itemcount = 0;
    int             itemnumber;
    int             x, y;
    int             dumpmode = 0;
    int             item_register();
    char           *item_dump();
    char           *p;
    void            fix_string();
    extern char    *sprintf();


    if (!ps_open_PostScript()) {
	(void) fprintf(stderr, "main: ps_open_PostScript\n");
	exit(1);
    }
    init();
    init_popup();

    while (!psio_error(PostScriptInput)) {

	if (create_item(itemtype)) {

	    if (item_register(items, itemcount, itemtype)) {
		alert_up("Can't Create More Items!");
		continue;
	    }
	} else if (get_delete(itemname)) {

	    if (sscanf(itemname, "item%d", &itemnumber) != 1) {
		alert_up("Can't Delete Item!\n");

	    } else if ((itemnumber > itemcount) ||
		*items[itemnumber].itemname == '\0') {
		alert_up("Can't Delete Item!\n");
	    } else
		*items[itemnumber].itemname = '\0';

	} else if (get_item_info(items[itemcount].itemlabel,
		    items[itemcount].itemlocation,
		items[itemcount].itemobject)) {

	    fix_string(items[itemcount].itemlabel);
	    fix_string(items[itemcount].itemlocation);
	    fix_string(items[itemcount].itemobject);

	    build_item(items[itemcount].itemtype,
		items[itemcount].itemname,
		items[itemcount].itemlabel,
		items[itemcount].itemobject,
		items[itemcount].itemlocation);

	    itemcount++;

	} else if (get_itemxy(itemname, &x, &y)) {

		    
	    if (!strcmp(itemname, "done")) { /* win size on done */

		p = item_dump(items, itemcount, dumpmode, x, y);

		if (p) {
		    if (dumpmode != TOSTDOUT) {
			alert_up(sprintf(buf,
				"Items Dumped to file %s", p));
		    }
		} else
		    alert_up("Problem Dumping Items");

	    } else if (sscanf(itemname, "item%d", &itemnumber) != 1) {
		alert_up("Unknown Item!");
	    } else if ((itemnumber > itemcount) ||
		*items[itemnumber].itemname == '\0') {
		alert_up("Item Already Deleted?");
	    } else {
		items[itemnumber].itemx = x;
		items[itemnumber].itemy = y;
	    }

	} else if (dump_items(&dumpmode)) {
	    send_itemxy();
	} else if (get_done()) {
	    break;
	} else if (psio_eof(PostScriptInput)) {
	    (void) fprintf(stderr, "main: unexpected eof\n");
	    break;
	} else {
	    (void) fprintf(stderr, "main: unknown data from server\n");
	    exit(1);
	}
    }

    ps_close_PostScript();
    exit(0);
}

void
fix_string(p)
register char *p;
{
    register char  *cp;

    cp = p + strlen(p);

    while (--cp > p)
	if (*cp != ' ') {
	    *(++cp) = '\0';
	    break;
	}
    if (cp == p)	/* all blanks case */
	*cp = '\0';

}
int
item_register(items, itemcount, itemtype)
struct item *items;
int itemcount;
char *itemtype;
{
    extern char    *strcpy(), *sprintf();

    if (itemcount == MAXITEMS) {
	(void) fprintf(stderr, "item_register: too many items\n");
	return (-1);
    }
    (void) strcpy(items[itemcount].itemtype, itemtype);
    (void) sprintf(items[itemcount].itemname, "item%d", itemcount);

    return (0);
}

int
addparens(p)
register char *p;
{
    while(p)
	if (*p == ' ') {
	    p++;
	    continue;
	}
	else if (*p == '[' || *p == '/')   
	    return(0);
	else 
	    return(1);

    return(1);
}

char *
item_dump(items, itemcount, dumpmode, width, height)
struct item *items;
int itemcount;
int dumpmode;
int width, height;
{
    register int    i;
    int somework = 0;
    FILE           *fp;
    static char filename[20];
    static int filecount;
    extern char *sprintf();
    int addparens();
   
    static char    *header = "#! /usr/NeWS/bin/psh \n\n\
/createitems {\n/items dictbegin\n";

    static char    *trailer = "dictend def\n} def\n\n\
/win framebuffer /new DefaultWindow send def\n\
{\n\t/PaintClient {1 fillcanvas items paintitems} def\n\
\t/FrameLabel (Tooltool Sample!) def\n\
\t/DestroyClient { currentprocess killprocessgroup } def\n\
} win send\n";

    static char    *trailer2 = "/can /GetCanvas win send def\n\
createitems\n\
/ih items forkitems def\n\
/map win send\n\n";

    for (i = 0; i < itemcount; i++) {
	if (*items[i].itemname) {
	    somework = 1;
	    break;
	}
    }

    if (!somework)
	return ((char *) NULL);

    if (dumpmode == TOSTDOUT)
	fp = stdout;
    else {
	(void) sprintf(filename, "tooltool%d", filecount++);
	if ((fp = fopen(filename, "w")) == (FILE *) NULL)
	    return((char *) NULL);
    }

    if (dumpmode == INDEMO)
        (void) fprintf(fp, "%s", header);

    (void) fprintf(fp, "\n%% Tooltool Current Items %%\n\n");

    for (i = 0; i < itemcount; i++)

	if (*items[i].itemname) {


	    (void) fprintf(fp, "/%-8s", items[i].itemname);

	    (void) fprintf(fp, 
		(addparens(items[i].itemlabel) ? " (%s)" : " %s" ), 
		items[i].itemlabel);

		
	    if (strcmp(items[i].itemtype, "ButtonItem")){

	    (void) fprintf(fp, 
		(addparens(items[i].itemobject) ? " (%s)" : " %s" ), 
		items[i].itemobject);

	    (void) fprintf(fp, " /%-10s", items[i].itemlocation);

	    }

	    (void)fprintf(fp,"\n\t/nullproc can /new %-10s send def\n", 
		items[i].itemtype);

	    (void) fprintf(fp,"%-4d %-4d 0 0 /reshape %-8s send\n\n", 
		items[i].itemx, items[i].itemy, items[i].itemname);
	}

    (void) fprintf(fp, "%% End Tooltool Items %%\n\n");

    if (dumpmode == INDEMO) {
        (void) fprintf(fp, "%s", trailer);
	(void) fprintf(fp, "20 20 %d %d /reshape win send\n",
	    width, height);
	(void) fprintf(fp, "%s", trailer2);
    }

    if (dumpmode == TOSTDOUT)
	    return("stdout");
    else {
	(void) fclose(fp);
	return(filename);
    }
}
