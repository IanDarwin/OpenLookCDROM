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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/msclients/vui/RCS/vuimenus.c,v 1.9 1992/12/15 21:23:32 rr2b R6tape $";
#endif

#include <andrewos.h>
#include <vui.h>
#include <vuimenus.h>
#include <panel.h>
#include <lmenus.h>

MENU_OPTS vui_menus[] = {

/* #define OPT_D_READ 0 */
"Read",   {0, "Display the list of messages in the highlighted folder"},

/* #define OPT_D_ADD   */
"Create", {0, "Create a new folder"},

/* #define OPT_D_DELETE  */
"Delete", {0, "Delete the highlighted folder"},

/* #define OPT_D_RENAME */
"Rename", {0, "Rename the highlighted folder"},

/* #define OPT_D_MERGE  */
"Merge",  {0, "Move the contents of the highlighted folder into another"},

/* #define OPT_D_INFO  */
"Info",   {0, "Display information about the highlighted folder"},

/* #define OPT_D_SUBSCR */
"Subscr", {0, "Change your subscription for the highlighted folder"},

/* #define OPT_P_PURGE   */
"Purge", {0, "Always purge deleted messages"},

/* #define OPT_D_EXPOSE  */
"Expose", {0, "Choose which bboards to list"},

/* #define OPT_P_PRINTER */
"Printer", {0, "Set or change current printer"},

/* #define OPT_G_MAILF  */
"Folders",{0, "Work with your folders"},

/* #define OPT_C_NEWMAIL */
"NewMail", {0, "Read the newly arrived mail"},

/* #define OPT_S_SINCE */
"Date",  {0, "List all messages since a date you enter"},

/* #define OPT_L_MSSERVE */
"MSserv", {0, "Logout from messageserver(s)"},

/* #define OPT_U_NONE  */
"None",   {0, "Do not subscribe to the highlighted folder"},

/* #define OPT_U_FULL  */
"Full",   {0, "Subscribe to the highlighted folder"},

/* #define OPT_U_ASK   */
"Ask",    {0, "Subscribe, but ask before showing new messages"},

/* #define OPT_U_PRINT */
"Print",  {0, "Subscribe by printing new messages"},

/* #define OPT_L_ALL   */
"All",    {0, "Include all bboards"},

/* #define OPT_L_SUBSCR */
"Subscr", {0, "Include only bboards you are subscribed to"},

/* #define OPT_L_MATCH */
"Match",  {0, "Show only bboards matching a string you enter"},

/* #define OPT_G_BBOARD */
"Bboards",{0, "Read the bulletin boards"},

/* #define OPT_L_PCSERVE */
"PCserv", {0, "Logout from pcserver(s)"},

/* #define OPT_G_PARMS */
"Options",{0, "Set program options"},

/* #define OPT_G_MAIL   */
"Mail",   {0, "Read your mail"},

/* #define OPT_G_NEXTM  */
"Next",   {0, "Read the next message"},

/* #define OPT_G_SEND  */
"Send",   {0, "Send a new message"},

/* #define OPT_EXIT   */
"Exit",   {0, "Leave the program"},

/* #define OPT_LOGOUT */
"Logout", {0, "Exit VUI and logout from server(s)"},

/* #define OPT_L_LOGOUT */
"All", {0, "Logout from all servers"},

/* #define OPT_S_TOPSET */
"Number", {0, "Enter a number to start this list of messages"},

/* #define OPT_MOTD  */
"Notd", {0, "Display note of the day"},

/* #define OPT_P_HEAD */
"Headers",{0, "Control which headers in the message are shown"},

/* #define OPT_P_EDIT  */
"Editor", {0, "Specify the name of the text editor you wish to use"},

/* #define OPT_S_WHEN */
"Whenis", {0, "Check a date specification"},

/* #define OPT_E_WHO  */
"Whois",  {0, "Check an address specification"},

/* #define OPT_P_BLIND  */
"Copy",   {0, "Keep a copy of the outgoing message"},

/* #define OPT_H_KEEP  */
"Keep",   {0, "Give a list of message headers that you want to have displayed"},

/* #define OPT_H_OMIT */
"Omit",   {0, "Give a list of message headers that you want NOT displayed"},

/* #define OPT_H_ALL  */
"All",    {0, "Show all message headers"},

/* #define OPT_C_READ */
"Read",   {0, "Display the contents of the selected message"},

/* #define OPT_C_REPLY */
"Reply",  {0, "Compose a reply to the selected message"},

/* #define OPT_C_FORWARD */
"Forward",{0, "Forward the selected message to someone"},

/* #define OPT_C_PRINT  */
"Print",  {0, "Print the contents of the selected message"},

/* #define OPT_C_DELETE */
"Delete", {0, "Delete the selected message (or recover if deleted)"},

/* #define OPT_C_CLASS   */
"Place",  {0, "Place the selected message in a different folder"},

/* #define OPT_C_SEND */
"Send",   {0, "Compose a new message"},

/* #define OPT_C_FILE  */
"File",   {0, "Save the selected message in a file"},

/* #define OPT_R_SENDER */
"Sender", {0, "Reply to the sender of the selected message"},

/* #define OPT_R_READER  */
"Reader", {0, "Reply to the recipients of the selected message"},

/* #define OPT_R_ALL  */
"All",    {0, "Reply to the sender and recipients of the selected message"},

/* #define OPT_Y_MOVE  */
"Move",   {0, "Put the message in a different folder and delete from current one"},

/* #define OPT_Y_COPY  */
"Copy",   {0, "Put a copy of the selected message in a different folder"},

/* #define OPT_Y_APPEND */
"Append", {0, "Append a copy of the selected message to the end of a different folder"},

/* #define OPT_C_NEXT  */
"Next",   {0, "Go to the next folder. Messages after the * will be kept as new"},

/* #define OPT_E_SEND  */
"Send",   {0, "Submit your message for delivery"},

/* #define OPT_E_EDIT  */
"Edit",   {0, "Edit your message (either where you were or with another editor)"},

/* #define OPT_E_APPEND */
"Append", {0, "Append a file onto the end of the message"},

/* #define OPT_E_CLEAR */ 
"Clear",  {0, "Start the message over"},

/* #define OPT_E_DRAFT */
"Save", {0, "Save a draft copy of this message into a folder"},

/* #define OPT_C_HEADS  */
"Headers",{0, "See all or just relevant headers"},

/* #define OPT_D_SEARCH  */
"Search", {0, "Search message captions"},

/* #define OPT_C_RESTORE */
"Restore", {0, "Create a message using this as a draft"},

/* #define OPT_O_LOCAL */
"Local",  {0, "Print the message on the local printer"},

/* #define OPT_O_REMOTE */
"Remote", {0, "Print the message on the remote printer"},

/* #define OPT_D_LOCAL  */
"Local",  {0, "Copy the message to a file on the local disk"},

/* #define OPT_D_REMOTE */
"Remote", {0, "Copy the message to a file on Vice"},

/* #define OPT_U_ALL  */
"Showall",{0, "Subscribe by showing all messages"},

/* #define OPT_C_DISPOSE */
"Dispose",{0, "Dispose of a message by doing something to it"},

/* #define OPT_S_POST   */
"Post",   {0, "Post a message to the selected bboard"},

/* #define OPT_E_VERIFY  */
"Verify", {0, "Verify the names of the recipients"},

/* #define OPT_C_RESPOND  */
"Respond",{0, "Create and send a message (either a reply, forward, etc.)"},

/* #define OPT_C_RESEND  */
"Resend", {0, "Resend the message to someone"},

/* #define OPT_C_PUNT */
"Punt",   {0, "Skip all these messages (Don't show them again as new)"},

/* #define OPT_E_HERE */
"Return", {0, "Go back to entering text here"},

/* #define OPT_E_PCEDITOR */
"DoEdit", {0, "Load and execute your external editor"},

/* #define OPT_HELP  */
"Help",   {0, "Display general information about the program"},

/* #define OPT_C_SETNEW */
"SetNew", {0, "Consider as new all messages after the highlighted one"},

/* #define OPT_L_PERSONAL */
"Personal",{0, "Include all personal folders"},

/* #define OPT_L_CHANGED */
"Changed",{0, "List bboards you subscribe to that have new messages"},

/* #define OPT_D_BRIEF */
"Brief",  {0, "Display folder type and number of messages"},

/* #define OPT_D_LONG  */
"Long",   {0, "Look for an explanation file for this bboard"},

/* #define OPT_C_CHANGE  */
"Change", {0, "Change the start of this list by number or date"},

/* #define OPT_B_YES   */
"Yes",    {0, "Keep copies of outgoing mail"},

/* #define OPT_B_NO  */
"No",     {0, "Don't keep copies of outgoing mail"},

/* #define OPT_B_BCCTO  */
"Folder", {0, "Specify a folder to receive copies of outgoing mail"},

/* #define OPT_H_DEFAULT  */
"Default",{0, "Restore the default message headers"},

/* #define OPT_E_OPTIONS  */
"Options",{0, "Add special headers to the message"},

/* #define OPT_E_ACK */
"Ack-To", {0, "Request a return receipt"},

/* #define OPT_E_VOTE */
"Vote", {0, "Request a vote"},

/* #define OPT_E_REDIST */
"Redist", {0, "Invite further redistribution"},

/* #define OPT_E_INVITE */
"Invite", {0, "Invite folder subscription"},

/* #define OPT_A_LOCAL */
"Local", {0, "Append local file"},

/* #define OPT_A_REMOTE */
"Remote", {0, "Append remote file"},

/* #define OPT_C_MARK */
"Mark", {0, "Mark messages for a global operation (if marked, unmark it)"},

NIL, {0, NIL},

};

MENU_TREE main_menu[] = {
   (int)OPT_G_MAIL,	 (struct menu_tree_st *) NIL,
   (int)OPT_G_MAILF,  (struct menu_tree_st *) NIL,
   (int)OPT_G_BBOARD, (struct menu_tree_st *) NIL,
   (int)OPT_G_SEND,   (struct menu_tree_st *) NIL,
   (int)OPT_G_PARMS,  (struct menu_tree_st *) NIL,
   (int)OPT_HELP,     (struct menu_tree_st *) NIL,
   (int)OPT_MOTD,     (struct menu_tree_st *) NIL,
   (int)OPT_EXIT,     (struct menu_tree_st *) NIL,
#ifdef IBMPC
   (int)OPT_LOGOUT,   (struct menu_tree_st *) NIL,
#endif
   NULL_MENU
};

MENU_TREE logout_menu[] = {
    (int)OPT_L_LOGOUT, (struct menu_tree_st *)NIL,
    (int)OPT_L_PCSERVE,(struct menu_tree_st *)NIL,
    (int)OPT_L_MSSERVE,(struct menu_tree_st *)NIL,
    NULL_MENU
};

MENU_TREE apndfile_menu[] = {
    (int)OPT_A_LOCAL,	(struct menu_tree_st *)NIL,
    (int)OPT_A_REMOTE,	(struct menu_tree_st *)NIL,
    NULL_MENU
};

MENU_TREE print_menu[] = {
   (int)OPT_O_LOCAL,  (struct menu_tree_st *) NIL,
   (int)OPT_O_REMOTE, (struct menu_tree_st *) NIL,
   NULL_MENU
};

MENU_TREE store_menu[] = {
   (int)OPT_D_LOCAL,  (struct menu_tree_st *) NIL,
   (int)OPT_D_REMOTE, (struct menu_tree_st *) NIL,
   NULL_MENU
};


MENU_TREE d_subscr_menu[] = {
   (int)OPT_U_NONE,   (struct menu_tree_st *) NIL,
   (int)OPT_U_FULL,   (struct menu_tree_st *) NIL,
   (int)OPT_U_ASK,    (struct menu_tree_st *) NIL,
   (int)OPT_U_PRINT,  (struct menu_tree_st *) NIL,
   (int)OPT_U_ALL,    (struct menu_tree_st *) NIL,
   NULL_MENU
};

MENU_TREE d_info_menu[] = {
   (int)OPT_D_BRIEF,  (struct menu_tree_st *) NIL,
   (int)OPT_D_LONG,   (struct menu_tree_st *) NIL,
   NULL_MENU
};

MENU_TREE s_expose_menu[] = {
   (int)OPT_L_CHANGED,(struct menu_tree_st *) NIL,
   (int)OPT_L_SUBSCR, (struct menu_tree_st *) NIL,
   (int)OPT_L_ALL,    (struct menu_tree_st *) NIL,
   (int)OPT_L_MATCH,  (struct menu_tree_st *) NIL,
   (int)OPT_L_PERSONAL,(struct menu_tree_st *) NIL,
   NULL_MENU
};

MENU_TREE f_expose_menu[] = {
   (int)OPT_L_ALL,    (struct menu_tree_st *) NIL,
   (int)OPT_L_CHANGED,(struct menu_tree_st *) NIL,
   (int)OPT_L_SUBSCR, (struct menu_tree_st *) NIL,
   (int)OPT_L_MATCH,  (struct menu_tree_st *) NIL,
   NULL_MENU
};

MENU_TREE folder_menu[] = {
   (int)OPT_D_READ,   (struct menu_tree_st *) NIL,
   (int)OPT_D_ADD,    (struct menu_tree_st *) NIL,
   (int)OPT_D_DELETE, (struct menu_tree_st *) NIL,
   (int)OPT_D_RENAME, (struct menu_tree_st *) NIL,
   (int)OPT_D_MERGE,  (struct menu_tree_st *) NIL,
   (int)OPT_D_INFO,   (struct menu_tree_st *) NIL,
   (int)OPT_D_SUBSCR, d_subscr_menu,
   (int)OPT_D_EXPOSE, f_expose_menu,
   NULL_MENU
};

MENU_TREE bb_menu[] = {
   (int)OPT_D_READ,   (struct menu_tree_st *) NIL,
   (int)OPT_S_POST,   (struct menu_tree_st *) NIL,
   (int)OPT_D_INFO,   d_info_menu,
   (int)OPT_D_SUBSCR, d_subscr_menu,
   (int)OPT_D_EXPOSE, s_expose_menu,
   NULL_MENU
};
 
MENU_TREE t_edit_menu[] = {
   (int)OPT_E_HERE,     (struct menu_tree_st *) NIL,
   (int)OPT_E_PCEDITOR, (struct menu_tree_st *) NIL,
   (int)OPT_P_EDIT,   (struct menu_tree_st *) NIL,
   NULL_MENU
};

MENU_TREE p_head_menu[] = {
   (int)OPT_H_KEEP,   (struct menu_tree_st *) NIL,
   (int)OPT_H_OMIT,   (struct menu_tree_st *) NIL,
   (int)OPT_H_ALL,    (struct menu_tree_st *) NIL,
   (int)OPT_H_DEFAULT,(struct menu_tree_st *) NIL,
   NULL_MENU
};

MENU_TREE b_copy_menu[] = {
   (int)OPT_B_YES,    (struct menu_tree_st *) NIL,
   (int)OPT_B_NO,     (struct menu_tree_st *) NIL,
   NULL_MENU
};

MENU_TREE parms_menu[] = {
   (int)OPT_P_HEAD,   p_head_menu,
   (int)OPT_P_BLIND,  b_copy_menu,
   (int)OPT_B_BCCTO,  (struct menu_tree_st *) NIL,
   (int)OPT_P_EDIT,   (struct menu_tree_st *) NIL,
   (int)OPT_P_PRINTER,(struct menu_tree_st *) NIL,
   (int)OPT_P_PURGE,	 (struct menu_tree_st *) NIL,
   NULL_MENU
};

MENU_TREE reply_menu[] = {
   (int)OPT_R_SENDER, (struct menu_tree_st *) NIL,
   (int)OPT_R_ALL,    (struct menu_tree_st *) NIL,
   (int)OPT_R_READER, (struct menu_tree_st *) NIL,
   NULL_MENU
};

MENU_TREE m_class_menu[] = {
   (int)OPT_Y_MOVE,   (struct menu_tree_st *) NIL,
   (int)OPT_Y_COPY,   (struct menu_tree_st *) NIL,
   (int)OPT_Y_APPEND, (struct menu_tree_st *) NIL,
   NULL_MENU
};

MENU_TREE respond_menu[] = {
   (int)OPT_C_REPLY,  reply_menu,
   (int)OPT_C_FORWARD,(struct menu_tree_st *) NIL,
   (int)OPT_C_SEND,   (struct menu_tree_st *) NIL,
   (int)OPT_C_RESEND, (struct menu_tree_st *) NIL,
   NULL_MENU
};

MENU_TREE dispose_menu[] = {
   (int)OPT_C_DELETE, (struct menu_tree_st *) NIL,
   (int)OPT_C_CLASS,  m_class_menu,
   (int)OPT_C_FILE,   store_menu,
   NULL_MENU
};

MENU_TREE change_menu[] = {
   (int)OPT_S_TOPSET, (struct menu_tree_st *) NIL,
   (int)OPT_S_SINCE,  (struct menu_tree_st *) NIL,
   (int)OPT_D_SEARCH, (struct menu_tree_st *) NIL,
   NULL_MENU
};

MENU_TREE mmsg_menu[] = {
   (int)OPT_C_READ,   (struct menu_tree_st *) NIL,
#ifdef IBMPC
   (int)OPT_C_PRINT,  print_menu,
#else
   (int)OPT_C_PRINT,	(struct menu_tree_st *) NIL,
#endif
   (int)OPT_C_RESPOND,	respond_menu,
   (int)OPT_C_DISPOSE,	dispose_menu,
   (int)OPT_C_RESTORE,	(struct menu_tree_st *) NIL,
   (int)OPT_C_CHANGE,	change_menu,
   (int)OPT_C_NEWMAIL,	(struct menu_tree_st *) NIL,
   (int)OPT_C_MARK,	(struct menu_tree_st *) NIL,
   NULL_MENU
};

MENU_TREE mbody_menu[] = {
   (int)OPT_G_NEXTM,  (struct menu_tree_st *) NIL,
#ifdef IBMPC
   (int)OPT_C_PRINT,  print_menu,
#else
   (int)OPT_C_PRINT,  (struct menu_tree_st *) NIL,
#endif
   (int)OPT_C_RESPOND,respond_menu,
   (int)OPT_C_DISPOSE,dispose_menu,
   (int)OPT_C_RESTORE,(struct menu_tree_st *) NIL,
   (int)OPT_C_HEADS,  (struct menu_tree_st *) NIL,
   (int)OPT_C_MARK,  (struct menu_tree_st *) NIL,
   NULL_MENU
};

MENU_TREE b_class_menu[] = {
   (int)OPT_Y_COPY,   (struct menu_tree_st *) NIL,
   (int)OPT_Y_APPEND, (struct menu_tree_st *) NIL,
   NULL_MENU
};

MENU_TREE b_respond_menu[] = {
   (int)OPT_C_REPLY,  reply_menu,
   (int)OPT_C_FORWARD,(struct menu_tree_st *) NIL,
   (int)OPT_C_SEND,   (struct menu_tree_st *) NIL,
   (int)OPT_C_RESEND, (struct menu_tree_st *) NIL,
   (int)OPT_S_POST,   (struct menu_tree_st *) NIL,
   NULL_MENU
    };

MENU_TREE b_dispose_menu[] = {
   (int)OPT_C_CLASS,  b_class_menu,
   (int)OPT_C_FILE,   store_menu,
   NULL_MENU
};

MENU_TREE fmsg_menu[] = {
   (int)OPT_C_READ,   (struct menu_tree_st *) NIL,
   (int)OPT_C_PUNT,   (struct menu_tree_st *) NIL,
#ifdef IBMPC
   (int)OPT_C_PRINT,  print_menu,
#else
   (int)OPT_C_PRINT,  (struct menu_tree_st *) NIL,
#endif
   (int)OPT_C_RESPOND,respond_menu,
   (int)OPT_C_DISPOSE,dispose_menu,
   (int)OPT_C_SETNEW, (struct menu_tree_st *) NIL,
   (int)OPT_C_CHANGE, change_menu,
   (int)OPT_C_NEXT,   (struct menu_tree_st *) NIL,
   (int)OPT_C_NEWMAIL,(struct menu_tree_st *) NIL,
   (int)OPT_C_MARK,   (struct menu_tree_st *) NIL,
   NULL_MENU
};

MENU_TREE bmsg_menu[] = {
   (int)OPT_C_READ,   (struct menu_tree_st *) NIL,
   (int)OPT_C_PUNT,   (struct menu_tree_st *) NIL,
#ifdef IBMPC
   (int)OPT_C_PRINT,  print_menu,
#else
   (int)OPT_C_PRINT,  (struct menu_tree_st *) NIL,
#endif
   (int)OPT_C_RESPOND,b_respond_menu,
   (int)OPT_C_DISPOSE,b_dispose_menu,
   (int)OPT_C_SETNEW, (struct menu_tree_st *) NIL,
   (int)OPT_C_CHANGE, change_menu,
   (int)OPT_C_NEXT,   (struct menu_tree_st *) NIL,
   (int)OPT_C_MARK,   (struct menu_tree_st *) NIL,
   NULL_MENU
};

MENU_TREE bbody_menu[] = {
   (int)OPT_G_NEXTM,  (struct menu_tree_st *) NIL,
   (int)OPT_C_PUNT,   (struct menu_tree_st *) NIL,
#ifdef IBMPC
   (int)OPT_C_PRINT,  print_menu,
#else
   (int)OPT_C_PRINT,  (struct menu_tree_st *) NIL,
#endif
   (int)OPT_C_RESPOND,b_respond_menu,
   (int)OPT_C_DISPOSE,b_dispose_menu,
   (int)OPT_C_HEADS,  (struct menu_tree_st *) NIL,
   (int)OPT_C_MARK,   (struct menu_tree_st *) NIL,
   NULL_MENU
};

MENU_TREE send_option_menu[] = {
    (int)OPT_E_ACK,	(struct menu_tree_st *) NIL,
    (int)OPT_E_VOTE,	(struct menu_tree_st *) NIL,
    (int)OPT_E_REDIST,	(struct menu_tree_st *) NIL,
    (int)OPT_E_INVITE,	(struct menu_tree_st *) NIL,
    NULL_MENU
};

MENU_TREE edit_menu[] = {
   (int)OPT_E_SEND,   (struct menu_tree_st *) NIL,
   (int)OPT_E_EDIT,   t_edit_menu,
   (int)OPT_E_VERIFY, (struct menu_tree_st *) NIL,
   (int)OPT_E_WHO,    (struct menu_tree_st *) NIL,
   (int)OPT_E_APPEND, apndfile_menu,
   (int)OPT_E_CLEAR,  (struct menu_tree_st *) NIL,
   (int)OPT_P_BLIND,  b_copy_menu,
   (int)OPT_E_DRAFT,  (struct menu_tree_st *) NIL,
   (int)OPT_E_OPTIONS,send_option_menu,
   NULL_MENU
};
