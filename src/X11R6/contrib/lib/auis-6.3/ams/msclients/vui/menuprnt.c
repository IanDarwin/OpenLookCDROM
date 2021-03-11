/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/msclients/vui/RCS/menuprnt.c,v 2.8 1992/12/15 21:23:32 rr2b R6tape $";
#endif



 


#include <vui.h>
#include <vuivers.h>
#include <cuivers.h>
#include <vuimenus.h>
#include <panel.h>
#include <lmenus.h>

extern MENU_TREE main_menu[], mmsg_menu[], folder_menu[], bb_menu[], parms_menu[], 
                 mbody_menu[], bmsg_menu[], bbody_menu[], edit_menu[];

extern MENU_OPTS vui_menus[];

struct MENU_HEAD {
        MENU_TREE *tree;
        char *text;
     } head_list[] = {
	 main_menu,   "Mail Panel",
	 folder_menu, "Folder processing",
	 bb_menu,     "Bulletin Board Reading",
	 parms_menu,  "Session Options",
	 mmsg_menu,   "Mail Captions",
	 mbody_menu,  "Body of Mail message",
	 bmsg_menu,   "BBoard Captions",
   	 bbody_menu,  "Body of Bboard message",
	 edit_menu,   "Message Entry",
	 (MENU_TREE *)NIL, ""  
};

PrintMenu(text, tm, level)
char *text;
MENU_TREE *tm;
int level;
{
    int i, j;
    
    if (level == 0) printf("\n\n***** %s *****\n\n",text);
    for  (i=0; tm[i].this >= 0; i++) {
	for (j=0; j<level*4; j++) printf(" ");
	printf("| %-8s(%.2d) %s\n",vui_menus[tm[i].this].Option,
		tm[i].this, vui_menus[tm[i].this].prompt.pdata);
	if (tm[i].submenu) {
	    printf("\n");
	    PrintMenu(NIL, tm[i].submenu, level+1);
	}
    }
    printf("\n");
}

main()
{
    struct MENU_HEAD *hl;

    printf("\t\tMenu listing for VUI Version %d.%d\n\n",
	    VUI_MAJORVERSION, VUI_MINORVERSION);

    hl=head_list;
    while (hl->tree) {
	PrintMenu(hl->text, hl->tree, 0);
	hl++;
    }
}
