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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/msclients/vui/RCS/vuimsgs.c,v 1.7 1992/12/15 21:24:03 rr2b R6tape $";
#endif

/* vui error messages */

char *VUIMSG_NOMEM = "Out of Memory...";
char *VUIMSG_MAPFILE = "Could not open/read subscription map file";
char *VUIMSG_READFILE = "Message server could not open/read file";
char *VUIMSG_NOHELP = "There is no help available at this time";
char *VUIMSG_MBCHECK = "Problem checking your mailbox";
char *VUIMSG_MFOLDER = "Sorry; that folder is for your incoming mail";
char *VUIMSG_FCHECK = "Could not check folder";
char *VUIMSG_SUBENTRY = "Could not get/set the subscription entry";
char *VUIMSG_MSGREAD = "The body of the message could not be completely read";
char *VUIMSG_MSSTORE = "Message server could not store file";
char *VUIMSG_AOHEAD = "Ambigious add/omit headers";
char *VUIMSG_NMFOLDER = "No more bboards/folders";
char *VUIMSG_ALLOCPAGE = "Can not allocate another page";
char *VUIMSG_NOMOREMSG = "No more messages";
char *VUIMSG_SNAPSHOT = "Could not get snapshot";
char *VUIMSG_FILLPAGE = "Fill in this page first";
char *VUIMSG_BENTLIST = "You are at the beginning of the list/message";
char *VUIMSG_BMSGFILE = "You are the beginning of this message/file";

/* vui common strings */

#ifndef IBMPC
 char *VUI_HOST	    = "Andrew";
 char *F2_STRING    = "<Esc>2";
 char *PGDN_STRING  = "^v";
 char *PGUP_STRING  = "<Esc>v";
#endif /* IBMPC */

#ifdef IBMPC
 char *VUI_HOST	    = "PC";
 char *F2_STRING    = "F2";
 char *PGDN_STRING  = "PgDn";
 char *PGUP_STRING  = "PgUp";
#endif /* IBMPC */

char *ACK_STRING	    = "Ack-To: ";
char *VOTE_CHOICE_STRING    = "Vote-Choices: ";
char *VOTE_TO_STRING	    = "Vote-To: ";
char *VOTE_ID_STRING	    = "Vote-Request: ";
char *INVITE_STRING	    = "X-Andrew-DirectoryCreation: ";
char *REDIST_STRING	    = "X-Andrew-Redistribution-To: ";
