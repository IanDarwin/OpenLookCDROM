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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/msclients/vui/RCS/vuisend.c,v 1.13 1992/12/15 21:24:03 rr2b R6tape $";
#endif

/* 
 *      This module contains the mainline VUI code for sending mail (sendsomemail)
 */

#include <andrewos.h>
#include <ams.h>
#include <vui.h>
#include <vuimenus.h>
#include <panel.h>
#include <lmenus.h>
#include <cmdargs.h>
#include <hdrparse.h>
#include <vuidebug.h>
#include <setjmp.h>

extern char *getenv(), CUI_VersionString[], *GetSUser();

extern PANEL entry_bp[], *PanelArray[];

extern FIELD entry_fields[], *FieldArray[];

extern int GetNUser(), ShowCursor(), FileClear(), ShowParmData(),
           ShowDirData(), ShowMsgData(), ShowBodyData();

extern int CUI_SnapIsRunning, Interactive, BODY_field_len;
extern KEYTAB data_keys[];
extern MKEYTAB dir_keys[], msg_keys[], body_keys[], entry_keys[], f1_keys[];
extern char ENT_filename[],*ENT_body_field, ENT_headerfile[];
extern Boolean ENT_Anyloss, ENT_Savethepage, HeadKeep;

extern long ENT_filelength, ENT_offset, ENT_headlength, ENT_headoffset, SetTopOfPage();

extern long MS_DisambiguateFile(), MS_StorePartialFile(), MS_GetDirInfo(),
            MS_ReInitialize(), MS_GetNewMessageCount(), MS_AppendFileToFolder(),
            CUI_DisambiguateDir();

extern unsigned char bar_row;

extern int (*VUI_RepaintFn)();
extern char *VUI_HelpFile;
extern char VUI_editor[], VUI_bccto[], VUI_printer[];
extern int     BlindStatus;
extern long    mserrcode;

extern MENU_TREE edit_menu[];

jmp_buf entrypanel;

ClearDataEntry ()
{
    RestoreCurrentField(&entry_fields[0]); /* Headers */
    ShowCursor();
    RestoreCurrentField(&entry_fields[1]); /* Body */
    ShowCursor();
    RestoreCurrentField(NIL);
}


DataEntryLoop (panel_bp, current_field, reshow_p, msg)
PANEL panel_bp[];
FIELD *current_field;
Boolean *reshow_p;
char *msg;
{
    int rc;

    debug((1,"DataEntryLoop(%s)\n",msg));

    while (1) {  /* ** Data entry while loop  ** */
        if (*reshow_p) {
            DrawPanel (panel_bp, current_field, PANEL_CLEAR);
	    ShowBar();
	    /* Get the input fields displayed */
	    if (current_field == (FIELD *)NULL)
		ClearDataEntry();
	    else {
		RestoreCurrentField(current_field == &entry_fields[0] ?
				    &entry_fields[1] : &entry_fields[0]);
		ShowCursor();
	    }
	    RestoreCurrentField(current_field);
	    ShowCursor();
            if (msg!=NIL) ReportSuccess(msg);
            *reshow_p=FALSE;
        } 
	else 
	    SetCurrentField(current_field, PANEL_PROMPT);

        rc=setjmp(entrypanel);
        if (rc==-1) {
	    CursorOff();
	    RestoreCurrentField(NIL);
            SaveCurrentEntPage();
            return;
        }
        GetKey(data_keys, TRUE);
    }
}

SendSomeMail (cuid, code)
int cuid, code;
{
    int opt, rc, last_opt = 0, (*saverepaintfn)();
    Boolean reshow = TRUE, quit, reload = FALSE, exitloop = FALSE;
    char *in_file, outfilename[MAXPATHLEN+1], *savehelpfile;
    FIELD *current_field;
    long newlength;

    debug((1,"SendSomeMail(%d, %d)\n",cuid, code));

    current_field=entry_fields; rc = 0;

    if (code==(int)OPT_S_POST) {
	char current_dir[MAXPATHLEN+1], Buffer[MAXPATHLEN+100];
        CUI_GenTmpFileName(outfilename);
        GetCurrentDir(current_dir, NIL, TRUE);
        sprintf(Buffer,"To: %s\nSubject:\nCC:\n\n\n", current_dir);
        rc=strlen(Buffer);
        debug((2,"Putting %d bytes out to start the post.\n", rc));
        mserrcode = MS_StorePartialFile(outfilename, 0L, rc, 0600, TRUE, Buffer);
        in_file=outfilename;
    } else 
	in_file = NIL;

    if (InitEntryData(cuid, code, in_file)) 
	return(MENU_ERROR);

    saverepaintfn = VUI_RepaintFn;
    savehelpfile  = VUI_HelpFile;
    VUI_RepaintFn = ClearDataEntry;
    VUI_HelpFile = "msgentry.hlp";

    if ((code == AMS_REPLY_SENDER) || (code == AMS_REPLY_WIDE) ||
	(code == AMS_REPLY_WIDER)) 
	current_field = &entry_fields[1];
    else if (code == (int)OPT_S_POST)
	current_field = &entry_fields[0];

    while (1) {
        DataEntryLoop(entry_bp, current_field, &reshow, NIL);

        quit=FALSE;
        while (!quit) {  /* ** Menu entry while loop  ** */
            opt=MenuInput(edit_menu, f1_keys, &last_opt);

            if (opt<0) {
                if (!ENT_Anyloss || 
		    GetBooleanFromUser("You will lose what you have entered. OK?", FALSE)) {
                    rc=MENU_DO_OVER;
                    quit = exitloop = TRUE;
                } else continue;
            }

            switch (opt) {
            case OPT_E_WHO:
                WhoIs();
                break;

            case OPT_E_SEND:
                if (!ENT_Anyloss) {
                    ReportError("Please enter your message prior to sending.", ERR_WARNING, FALSE);
		    current_field = &entry_fields[1];
                    quit=TRUE;
                    break;
                }
                if (!SubmitMessage()) {
                    ENT_Anyloss=FALSE;
                } else {
		    quit=TRUE;
		}
		ReloadHeaderFile();
		strcpy(outfilename, ENT_filename);
		reload=TRUE;
                break;

            case OPT_P_EDIT:
                SetEditor();
                break;

            case OPT_E_HERE:
		reload = TRUE;
                quit = TRUE;
                break;

            case OPT_E_PCEDITOR:
		CUI_GenTmpFileName(outfilename);
		newlength = 0L;
                if (CompressFile(ENT_filename, outfilename, &newlength, FALSE, FALSE))
                    break;
                CursorOn();
                if (!EditFile(outfilename, TRUE)) {
                    RealUnlinkViceFile(ENT_filename);
		    ENT_filelength = ENT_offset = 0L;
		    if (DecompressFile(outfilename, ENT_filename, 
				       &ENT_filelength, TRUE, FALSE))
			return(MENU_ERROR);
		    LoadOneEntPage(ENT_filename, ENT_filelength, (ENT_offset = 0L), 
				   ENT_body_field, BODY_field_len, (long *)NULL);
                    ENT_Anyloss = reload = TRUE;
                    DrawPanel (entry_bp, NIL, PANEL_CLEAR);
		    ShowBar();
                } else {
                    RealUnlinkViceFile(outfilename);
                }
                CursorOff();
                break;

            case OPT_E_VERIFY:
		ReportSuccess("Checking recipients. Please stand by...");
		rc = ValidateAddrList();
		if (!rc)  ClearError();
		ReloadHeaderFile();
		reload=TRUE;
		break;

	    case OPT_E_DRAFT:
                {
		char *newdir, ShortName[MAXPATHLEN+1], FullName[MAXPATHLEN+1], dirbuf[81], *fulldirname = FullName;
		Boolean quit = FALSE;
		while (1) {
		    newdir=GetSUser("Enter name of folder to receive draft:", NIL, NIL, dirbuf);
		    if (newdir==NIL) { quit=TRUE; break; }
		    if (*newdir == '?')
			ListFolders();
		    else {
			strcpy(ShortName, newdir);
			if (mserrcode = CUI_DisambiguateDir(ShortName, &fulldirname)) {
			    ReportError("Can't find folder by that name.", ERR_WARNING, TRUE);
			    continue;
			} else break;
		    }
		}
		if (quit) break;
		CUI_GenTmpFileName(outfilename);
		newlength = 0L;
		if ((CompressFile(ENT_headerfile, outfilename, &newlength, FALSE, TRUE)) ||
		    (CompressFile(ENT_filename, outfilename, &newlength, FALSE, FALSE)))
                    break;
		if (mserrcode = MS_AppendFileToFolder(outfilename, fulldirname)) {
		    ReportError("Couldn't append file to folder.", ERR_WARNING, TRUE);
		} else ENT_Anyloss=FALSE;
		RealUnlinkViceFile(outfilename);
		break;
		}

/*            case OPT_E_APPEND: */
	    case OPT_A_LOCAL:
	    case OPT_A_REMOTE:
		{
                char savefilename[80];
                in_file=GetSUser("Enter filename to append:", NIL, NIL, savefilename);
		if (in_file == NIL) break;
		if (!AppendFile(in_file, ENT_filename, &ENT_filelength, opt))
		    ClearError();
		LoadOneEntPage(ENT_filename, ENT_filelength, (ENT_offset = 0L), 
				   ENT_body_field, BODY_field_len, (long *)NULL);
		reload = TRUE;
		ENT_Anyloss = TRUE;
		break;
		}

            case OPT_E_CLEAR:
                if (ENT_Anyloss && !GetBooleanFromUser("You will lose what you have entered. OK?", FALSE)) {
                    break;
                }
		FreeEntData();
		InitEntryData(0, AMS_REPLY_FRESH, NIL);
                reload=TRUE;
                ENT_Anyloss=FALSE;
		current_field = &entry_fields[0]; /* set to headers */
                quit=TRUE;
                break;

            case OPT_B_YES:
                BlindStatus=AMS_SEND_BLINDYES;                  /* V1.3 MAC */
                break;

            case OPT_B_NO:
                BlindStatus=AMS_SEND_BLINDNO;                   /* V1.3 MAC */
                break;

	    case OPT_E_ACK:
	    case OPT_E_VOTE:
	    case OPT_E_REDIST:
	    case OPT_E_INVITE:
		if (reload = AddSpecialHeaders(opt))
		    ReloadHeaderFile();
		break;
            } /* End switch */

            if (reload) {
		ClearDataEntry();
		CursorOff();
                reload = FALSE;
                RestoreCurrentField(NIL);   /* (avoid the dreaded cursor) */
            }
        } /* End menu loop */
	if (exitloop) break;
        ClearPrompt(); /* because menu is not available */
    }

    VUI_RepaintFn = saverepaintfn;
    VUI_HelpFile = savehelpfile;
    SetCurrentField(NIL, PANEL_NOPROMPT);
    FreeEntData();
    return(rc);
}

AppendFile(in_file, out_file, filelength, local_or_remote)
char *in_file, *out_file;
long *filelength;
int   local_or_remote;
{
    char outfilename[MAXPATHLEN+1], new_in_file[MAXPATHLEN+1], new_out_file[MAXPATHLEN+1];
    long newlength, templen;

    if ((local_or_remote == (int)OPT_A_REMOTE) || (!CUI_SnapIsRunning)) {
	if (MS_DisambiguateFile(in_file, new_in_file, AMS_DISAMB_FILEEXISTS)) {
	    ReportError("Can't find file by that name.", ERR_WARNING, FALSE);
	    return(-1);
	}
	strcpy(in_file, new_in_file);
    }

    ReportSuccess("Preparing the file...");
    newlength = 0L;
    if (*filelength == 0) {
	CUI_GenTmpFileName(outfilename);
    } else {
	*outfilename = '\0';
	if (CompressFile(out_file, outfilename, &newlength, FALSE, FALSE))
	    return(-1);
    }
    ReportSuccess("Appending in progress. Please stand by...");
    if (local_or_remote == (int)OPT_A_LOCAL) {
	if (CUI_AppendFileToVice(in_file, outfilename, newlength))
	    return(-1);
    }
    else {
	templen = 0L;
	if ((DecompressFile(in_file, new_in_file, &templen, FALSE, FALSE)) ||
	    (CompressFile(new_in_file, outfilename, &newlength, TRUE, FALSE)))
	    return(-1);
    }

    newlength = 0L;
    if (DecompressFile(outfilename, new_out_file, &newlength, TRUE, FALSE))
	return(-1);

    *filelength = newlength;
    RealUnlinkViceFile(out_file);
    strcpy(out_file, new_out_file);
    return(0);
}
