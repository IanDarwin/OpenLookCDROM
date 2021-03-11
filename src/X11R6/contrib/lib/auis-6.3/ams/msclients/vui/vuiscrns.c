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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/msclients/vui/RCS/vuiscrns.c,v 2.11 1992/12/15 21:24:03 rr2b R6tape $";
#endif

#include <andrewos.h>
#include <vui.h>
#include <panel.h>

extern int SetSavethepage(), EntryPageDnKey();
extern int ExpandEntryArea(), ShrinkEntryArea(), SetProtectedFields();

extern PANEL intro[];

PRMPT hdprompt = {0, "Enter the names of people to send the message to" };

PRMPT boprompt = {0, "Enter the text of your message" };


FIELD entry_fields[] = {
/*0:headers:*/ 
    &entry_fields[1], &entry_fields[1], &entry_fields[1],
    &entry_fields[1], &entry_fields[1], (XTENT *) NULL, NORMAL,
    FTYPE_SCR, NIL, &hdprompt, SetSavethepage, (int (*)()) NULL,
    EntryPageDnKey, ExpandEntryArea, ShrinkEntryArea, SetProtectedFields,
/*1:body*/ 
    &entry_fields[0], &entry_fields[0], &entry_fields[0],
    &entry_fields[0], &entry_fields[0], (XTENT *) NULL, NORMAL,
    FTYPE_SCR, NIL, &boprompt, SetSavethepage, (int (*)()) NULL, 
    EntryPageDnKey, ExpandEntryArea, ShrinkEntryArea, (int (*)()) NULL,
    NULL_FIELD
    };

FIELD *FieldArray[] = {
     entry_fields,
     (FIELD *) NULL,
     };

PANEL sess_bp[] = {
     0, 29, HILITE, 0, "Option Settings",
     4,  0, HILITE, 0, "Version:",
     6,  0, HILITE, 0, "Header Filtering:",
     7,  0, HILITE, 0, "Headers:",
     9,  0, HILITE, 0, "Copy of Outgoing:",
    10,  0, HILITE, 0, "Folder for Outgoing:",
    12,  0, HILITE, 0, "Editor:",
    14,  0, HILITE, 0, "Printer:",
    16,  0, HILITE, 0, "Always Purge:",
     NULL_PANEL
   };

PANEL mail_bp[] = {
     0, 31, HILITE, 0, "Folder Processing",
     1,  0, HILITE, 0, "Folder Name",
     1, 61, HILITE, 0, "Subscription Status",
     NULL_PANEL
   };

PANEL bboard_bp[] = {
     0, 30, HILITE, 0, "Bulletin Board Reading",
     1,  0, HILITE, 0, "Bboard Name",
     1, 60, HILITE, 0, "Subscription Status",
     NULL_PANEL
   };

PANEL mess_bp[] = {
     0,  0, HILITE, 0, "The first message is",
     0, 21, NORMAL, 15,"xxxx of yyyy.                ",
     0, 38, HILITE, 0, "In folder:",
     0, 49, NORMAL, 31,"test",
     1,  9, HILITE, 0, "Flags",
     1,  2, HILITE, 0, "Date",
     1, 16, HILITE, 0, "Subject",
     1, 50, HILITE, 0, "From (Length)",
     NULL_PANEL
   };

char entry_bp_data[81]; /* room for putting in the message entry banner */

PANEL entry_bp[] = {
     7, 0, RVIDEO, 80, entry_bp_data,
     NULL_PANEL
   }; 


/* allocate room for version stuff in options screen, will be filled later */

char parm_data_version[80] = {'\0'};
PANEL parm_data[] = {           /* Don't add this to the Panel Array */
     4, 20, NORMAL,60, parm_data_version,
     6, 20, NORMAL, 0, NIL,
     7, 20, NORMAL, 0, NIL,
     9, 20, NORMAL, 0, NIL,
    10, 20, NORMAL, 0, NIL,
    12, 20, NORMAL, 0, NIL,
    14, 20, NORMAL, 0, NIL,
    16, 20, NORMAL, 0, NIL,
     NULL_PANEL
   };

PANEL *PanelArray[] = {
        intro, sess_bp, mail_bp, bboard_bp,
        mess_bp, entry_bp,
        (PANEL *) NIL,
    };

FixUpMenus()
{
    int i;
    char tmpbuf[100];

    /* put the message enrty banner in the entry_bp panel data field */

    sprintf(tmpbuf, " Message Entry (press %s for menu, %s for help) ",
	     ESC_STRING, F1_STRING);
    for (i=0; i<LINE_LENGTH; i++) entry_bp_data[i] = '-';
    strncpy(entry_bp_data+(LINE_LENGTH-strlen(tmpbuf))/2, tmpbuf, strlen(tmpbuf));

    /* fix the intro menu columns */

    for (i=INTRO_START; intro[i].pdata != NIL; i+=2)
	intro[i+1].pcol = intro[i].pcol + strlen(intro[i].pdata)+1;
}
