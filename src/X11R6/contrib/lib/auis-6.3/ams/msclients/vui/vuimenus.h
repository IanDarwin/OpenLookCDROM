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


enum VUI_MENU_CODES {
	OPT_D_READ = 0,
	OPT_D_ADD,
	OPT_D_DELETE,
	OPT_D_RENAME,
	OPT_D_MERGE,
	OPT_D_INFO,
	OPT_D_SUBSCR,
	OPT_P_PURGE,	/* UNUSED001 This is unused */
	OPT_D_EXPOSE,
	OPT_P_PRINTER,	/* UNUSED002  This is unused */
	OPT_G_MAILF,
	OPT_C_NEWMAIL,	/* UNUSED003 This is unused */
	OPT_S_SINCE,
	OPT_L_MSSERVE,	/* UNUSED004 This is unused */
	OPT_U_NONE,
	OPT_U_FULL,
	OPT_U_ASK,
	OPT_U_PRINT,
	OPT_L_ALL,
	OPT_L_SUBSCR,
	OPT_L_MATCH,
	OPT_G_BBOARD,
	OPT_L_PCSERVE,	/* UNUSED0045 This is unused */
	OPT_G_PARMS,
	OPT_G_MAIL,
	OPT_G_NEXTM,
	OPT_G_SEND,
	OPT_EXIT,
	OPT_LOGOUT,
	OPT_L_LOGOUT,
	OPT_S_TOPSET,
	OPT_MOTD,
	OPT_P_HEAD,
	OPT_P_EDIT,
	OPT_S_WHEN,
	OPT_E_WHO,
	OPT_P_BLIND,
	OPT_H_KEEP,
	OPT_H_OMIT,
	OPT_H_ALL,
	OPT_C_READ,
	OPT_C_REPLY,
	OPT_C_FORWARD,
	OPT_C_PRINT,
	OPT_C_DELETE,
	OPT_C_CLASS,
	OPT_C_SEND,
	OPT_C_FILE,
	OPT_R_SENDER,
	OPT_R_READER,
	OPT_R_ALL,
	OPT_Y_MOVE,
	OPT_Y_COPY,
	OPT_Y_APPEND,
	OPT_C_NEXT,
	OPT_E_SEND,
	OPT_E_EDIT,
	OPT_E_APPEND,
	OPT_E_CLEAR,
	OPT_E_DRAFT,
	OPT_C_HEADS,
	OPT_D_SEARCH,
	OPT_C_RESTORE,
	OPT_O_LOCAL,
	OPT_O_REMOTE,
	OPT_D_LOCAL,
	OPT_D_REMOTE,
	OPT_U_ALL,
	OPT_C_DISPOSE,
	OPT_S_POST,
	OPT_E_VERIFY,
	OPT_C_RESPOND,
	OPT_C_RESEND,
	OPT_C_PUNT,
	OPT_E_HERE,
	OPT_E_PCEDITOR,
	OPT_HELP,
	OPT_C_SETNEW,
	OPT_L_PERSONAL,
	OPT_L_CHANGED,
	OPT_D_BRIEF,
	OPT_D_LONG,
	OPT_C_CHANGE,
	OPT_B_YES,
	OPT_B_NO,
	OPT_B_BCCTO,
	OPT_H_DEFAULT,
	OPT_E_OPTIONS,
	OPT_E_ACK,
	OPT_E_VOTE,
	OPT_E_REDIST,
	OPT_E_INVITE,
	OPT_A_LOCAL,
	OPT_A_REMOTE,
	OPT_C_MARK
};
