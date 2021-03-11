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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/hdlnew.c,v 2.61 1993/08/25 20:35:53 susan Exp $";
#endif


                                 

#include <fcntl.h>
#include <andyenv.h>
#include <util.h>
#include <ms.h>
#include <mail.h>
#include <mailconf.h>
#include <eli.h>
#include <pwd.h>
#include <dropoff.h>
#ifdef WHITEPAGES_ENV
#include <wp.h>
#endif                                 /* WHITEPAGES_ENV */
#include <sys/stat.h>
#include <andrewos.h>                  /* sys/file.h */
#include <system.h>

#ifdef sys_vax_11
#define void int                       /* What a grubby hack -- vaxes should
                                        * die. */
#endif                                 /* sys_vax_11 */

static int      SquirreledError = 0;

static eliDataTypes_t OneInt[] = {e_data_integer};
static eliDataTypes_t OneIntOneString[] = {e_data_integer, e_data_string};
static eliDataTypes_t TwoStrings[] = {e_data_string, e_data_string};
static eliDataTypes_t OneStringOneInt[] = {e_data_string, e_data_integer};
static eliDataTypes_t OneStringTwoInts[] = {e_data_string, e_data_integer, e_data_integer};
static eliDataTypes_t TwoStringsOneInt[] = {e_data_string, e_data_string, e_data_integer};
static eliDataTypes_t ThreeInts[] = {e_data_integer, e_data_integer, e_data_integer,};
static eliDataTypes_t OneString[] = {e_data_string};




EliProcessInfo_t EliProcessInfo;
static struct MS_Message *currentMessage = NULL;
static char    *currentMessageFile = NULL;

extern char     home[], MyMailDomain[], Me[];
extern int      homeUsesAMSDelivery, homeUsesUseridPlus;
extern char    *getenv(), *sys_errlist[];

/* No longer takes the second (location string)
 * argument
 */
long            Flames_EliToAMSErr(st)
EliState_t *st;
{
    int             err;

    if (err = EliErr_UnixErr(st)) {
        AMS_RETURN_ERRCODE(err, EIN_ELI, EVIA_FLAMES_HANDLENEW);
    }
    switch (EliErr_ErrCode(st)) {
        case ELI_ERR_OUT_OF_MEM:
            err = ENOMEM;
            break;
        case ELI_ERR_BAD_SYNTAX:
            err = EMSELISYNTAX;
            break;
        case ELI_ERR_UNBOUND:
            err = EMSELIUNBOUND;
            break;
        case ELI_ERR_FN_UNDEF:
            err = EMSELIUNDEF;
            break;
        case ELI_ERR_BAD_PARAM:
            err = EMSELIBADPARAM;
            break;
        case ELI_ERR_BAD_ARGS:
            err = EMSELIBADARGS;
            break;
        case ELI_ERR_NOSYM:
            err = EMSELINOSYM;
            break;
        case ELI_ERR_ARGLISTSIZE:
            err = EMSELIARGLISTSIZE;
            break;
        case ELI_ERR_USERERROR:
            err = EMSELIUSERERROR;
            break;
        case ELI_ERR_SYSERROR:
            err = EMSELISYSERROR;
            break;
        case ELI_ERR_CLIENT:
            if (SquirreledError) {
                SquirreledError = 0;
                return (mserrcode);
            }
            /* drop through */
        default:
            err = EMSUNKNOWN;
            break;
    }

/*
 * We used to call this to display the ELI error location, but now that we have
 * the nifty new error-reporting mechanism, we don't -- bobg, 27-Oct-88
 */

#ifdef NO_NO_NO
    if (eliloc)
        NonfatalBizarreError(eliloc);
#endif                                 /* NO_NO_NO */

    AMS_RETURN_ERRCODE(err, EIN_ELI, EVIA_FLAMES_HANDLENEW);
}

Flames_HandleNewMessage(Msg, FileName, NumDirInsertions, IsMail, MailboxFile, EliErrBuf, EliErrBufLim)
struct MS_Message *Msg;
char           *FileName, *EliErrBuf;
int            *NumDirInsertions, EliErrBufLim;
Boolean         IsMail;
char           *MailboxFile;
{
    static int      flameOn = FALSE;
    static EliState_t stStruct, *st;
    char           *firstfuncname = NULL, *funccall, *badsexp;
    EliSexp_t      *node1, *node2 = NULL;
    Boolean         sawerr = FALSE;
    struct MS_CaptionTemplate CaptionTemplate;
    int             errbuf_remaining = EliErrBufLim;
    long            amsFromEliErr;

    if (!flameOn) {
        st = &stStruct;
        EliInit(st, e_mem_pool);
        if (EliErr_ErrP(st)) {
            AMS_RETURN_ERRCODE(EMSUNKNOWN, EIN_ELI, EVIA_FLAMES_HANDLENEW);
        }

        /* This will call EliPrimDef on flames-specific routines */
        if (Flames_Initialize(st))
            return (mserrcode);

        flameOn = TRUE;
    }
    EliClearErr(st);
    EliTraceStk_Purge(st);
    if (FileName) {
        if (Flames_SlurpFlamesFileIfNecessary(st, FileName, &firstfuncname)) {
            EliClearErr(st);
            EliTraceStk_Purge(st);
            return (mserrcode);
        }
    }
    else {
        if (Flames_ReadDefault(st, MailboxFile, &firstfuncname)) {
            EliClearErr(st);
            EliTraceStk_Purge(st);
            return (mserrcode);
        }
    }
    CaptionTemplate.datetype = DATETYPE_FROMHEADER;
    CaptionTemplate.basictype = BASICTEMPLATE_NORMAL;
    if (BuildDateField(Msg, DATETYPE_FROMHEADER)
/* Took out InventID -- it is already happening in ProcessNewMail -- bobg */
        || BuildReplyField(Msg)
        || BuildAttributesField(Msg)
        || BuildCaption(Msg, &CaptionTemplate, IsMail)) {
        EliClearErr(st);
        EliTraceStk_Purge(st);
        return (mserrcode);
    }
    AMS_SET_ATTRIBUTE(Msg->Snapshot, AMS_ATT_UNSEEN);
    funccall = malloc(30 + strlen(firstfuncname));
    if (!funccall) {
        EliClearErr(st);
        EliTraceStk_Purge(st);
        AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_FLAMES_HANDLENEW);
    }
    sprintf(funccall, "(%s (currentmessage))", firstfuncname);
    if ((!(node1 = eliSGetSexp_trace(st, EliTraceStk(st), funccall)))
        || (!(node2 = eliSexp_GetNew_trace(st, EliTraceStk(st))))) {
        sawerr = TRUE;
    }
    free(funccall);
    currentMessage = Msg;
    currentMessageFile = MailboxFile;
    if (!sawerr) {
        eliEval(st, node1, node2);
        if (EliErr_ErrP(st)) {
            sawerr = TRUE;
        }
    }
    currentMessage = NULL;
    currentMessageFile = NULL;
    if (sawerr) {
        if (MSDebugging & 4096) {
            printf("FLAMES ERROR: %s in %s", EliErrStr(EliErr_ErrCode(st)), EliErr_ErrLoc(st));
            if (EliErr_BadSexpP(st))
                printf(" <%s>\n", EliSPutSexp(EliErr_BadSexp(st)));
            else
                putchar('\n');

            if (EliErr_BacktraceP(st)) {
                printf("BACKTRACE:    (");
                eliDisplayCons(EliErr_Backtrace(st));
                printf(")\n");
            }
        }

/* Nifty new ELI-error reporting mechanism.  We'll assume (reasonably)
 * that EliErrBuf is big enough to hold the following line, and only worry
 * about running out of space in the buffer if there's an s-expression
 * to add to it.
 */

        if (EliErrBufLim && !(*EliErrBuf)) {
            sprintf(EliErrBuf, "FLAMES ERROR: %s in %s", EliErrStr(EliErr_ErrCode(st)), EliErr_ErrLoc(st));

            if (EliErr_UnixErr(st)) {
                errbuf_remaining = EliErrBufLim - strlen(EliErrBuf);
                strcat(EliErrBuf, " {");
                strncat(EliErrBuf, sys_errlist[EliErr_UnixErr(st)], errbuf_remaining - 16);
                strcat(EliErrBuf, "}");
            }
            if (EliErr_BadSexpP(st)) {
                if ((errbuf_remaining = EliErrBufLim - strlen(EliErrBuf)) > 8) {
                    strcat(EliErrBuf, "  <");
                    strncat(EliErrBuf, badsexp = EliSPutSexp(EliErr_BadSexp(st)), errbuf_remaining - 16);
                    if (strlen(badsexp) > (errbuf_remaining - 16))
                        strcat(EliErrBuf, "...");
                    strcat(EliErrBuf, ">");
                }
            }
        }
        amsFromEliErr = Flames_EliToAMSErr(st);
        EliClearErr(st);
        EliTraceStk_Purge(st);
        return (amsFromEliErr);
    }
    if (EliNilP(st, node2)) {
        EliClearErr(st);
        EliTraceStk_Purge(st);
        AMS_RETURN_ERRCODE(EMSORPHANMSG, EIN_DIRSPECPARSE, EVIA_FLAMES_HANDLENEW);
    }
    *NumDirInsertions = 0;
    EliClearErr(st);
    EliTraceStk_Purge(st);
    return (0);
}


Flames_ReadDefault(st, MailboxFile, firstfuncname)
EliState_t     *st;
char *MailboxFile;
char          **firstfuncname;
{
    EliSexp_t      *sexp, *node;
    char            FBuf[1 + MAXPATHLEN], Mailbox[1 + MAXPATHLEN], *s, FlamesInitString[75 + MAXPATHLEN], FuncName[25 + MAXPATHLEN];

    strcpy(Mailbox, MailboxFile);
    s = strrchr(Mailbox, '/');
    if (!s)
        AMS_RETURN_ERRCODE(EMSNOSPECFILE, EIN_PARAMCHECK, EVIA_FLAMES_HANDLENEW);
    *s = '\0';
    if (!NeedToReadFile(Mailbox, firstfuncname)) {
        return (0);
    }
    sprintf(FuncName, "default-%s-prog", Mailbox);
    strcpy(FBuf, Mailbox);
    s = strrchr(FBuf, '/');
    if (!s)
        AMS_RETURN_ERRCODE(EMSNOSPECFILE, EIN_PARAMCHECK, EVIA_FLAMES_HANDLENEW);
    *++s = '\0';
    strcat(FBuf, ".MESSAGES/");
    s = FBuf + strlen(FBuf);           /* points to the null char */
    strcpy(s, "mail");
    if (access(FBuf, W_OK)) {
        strcpy(s, "misc");
        if (access(FBuf, W_OK)) {
            AMS_RETURN_ERRCODE((errno == ENOENT) ? EMSNOSPECFILE : errno, EIN_PARAMCHECK, EVIA_FLAMES_HANDLENEW);
        }
    }
    sprintf(FlamesInitString, "(defun %s (msg) (finalappendmsgtodir msg \"%s\"))", FuncName, FBuf);
    if (!(node = eliSexp_GetNew_trace(st, EliTraceStk(st)))) {
        return (Flames_EliToAMSErr(st));
    }
    if (!(sexp = eliSGetSexp_trace(st, EliTraceStk(st), FlamesInitString))) {
        return (Flames_EliToAMSErr(st));
    }
    eliEval(st, sexp, node);
    if (EliErr_ErrP(st)) {
        return (Flames_EliToAMSErr(st));
    }
    *firstfuncname = malloc(1 + strlen(FuncName));
    if (*firstfuncname) {              /* otherwise we just redefine it next
                                        * time */
        strcpy(*firstfuncname, FuncName);
        RememberRecentRead(Mailbox, *firstfuncname, 1);
    }
    return (0);
}


/* This routine takes a MS-Message struct and turns its headers into
 * an assoc list
 */
void            FLAMES_Prim_GetHeaderList(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    struct MS_Message *Msg;
    char           *HeadersAsList, *startspot;
    char           *argv[1];
    EliSexp_t      *expr;
    int             i;

    if (FLAMES_TranslateArgs(st, arglist, resbuf, 1, 1, argv, OneInt, NULL, "getheaderlist") < 0) {
        return;                        /* errors were flagged */
    }
    Msg = (struct MS_Message *) argv[0];

    for (i = 0; Msg->ParsedStuff->HeadName[i]; ++i) {
        ;
    }
    HeadersAsList = malloc((9 * i) + Msg->HeadSize + 25);
    if (!HeadersAsList) {
        AMS_SET_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_FLAMES_HANDLENEW);
        TellEliAboutAMSError(st, NULL, "FLAMES-PRIMITIVE [GETHEADERLIST (allocating array)]", ENOMEM);
        return;
    }
    strcpy(HeadersAsList, "(");
    for (i = 0; Msg->ParsedStuff->HeadName[i]; ++i) {
        if (Msg->ParsedStuff->HeadBody[i]) {
            strcat(HeadersAsList, "(\"");
            strcat(HeadersAsList, Msg->ParsedStuff->HeadName[i]);
            strcat(HeadersAsList, "\" \"");
            startspot = HeadersAsList + strlen(HeadersAsList);
            strncpy(startspot, Msg->ParsedStuff->HeadBody[i], Msg->ParsedStuff->HeadBodyLen[i]);
            startspot[Msg->ParsedStuff->HeadBodyLen[i]] = '\0';
            strcat(HeadersAsList, "\")");
        }
    }
    strcat(HeadersAsList, ")");
    expr = eliSGetSexp_trace(st, EliTraceStk(st), HeadersAsList);
    if (!expr) {
        return;                        /* error is already flagged */
    }
    free(HeadersAsList);
    EliSexp_SetCons(st, resbuf, EliSexp_GetCons(expr));
    return;
}


int             Flames_SlurpFlamesFileIfNecessary(st, fName, firstfuncname)
EliState_t     *st;
char           *fName;
char          **firstfuncname;
{
    if (NeedToReadFile(fName, firstfuncname)) {
        mserrcode = Flames_SlurpFlamesFile(st, fName, firstfuncname);
        if (!mserrcode) {
            RememberRecentRead(fName, *firstfuncname, 0);
        }
        return (mserrcode);
    }
    else {
        return (0);
    }
}

struct SlurpHistEnt {
    char           *funcname, *filename;
    long            modtime;
    int             permanent;
    struct SlurpHistEnt *Next;
}              *SlurpHistListHead = NULL;

NeedToReadFile(fname, funcname)
char           *fname, **funcname;
{
    struct SlurpHistEnt *tmp = SlurpHistListHead;
    struct stat     stbuf;

    while (tmp) {
        if (!strcmp(fname, tmp->filename)) {
            if (tmp->permanent) {
                debug(4096, ("Definition of %s from %s is permanent.\n", tmp->funcname, tmp->filename));
                return (!(*funcname = tmp->funcname));
            }
            if (stat(fname, &stbuf) < 0) {
                return (1);        /* Make the caller believe we need to
                                     * read the file, then let him crap out
                                     * trying to fetch it from Vice */
            }
            if (stbuf.st_mtime == tmp->modtime) {
                debug(4096, ("Definition of %s from %s appears up-to-date.\n", tmp->funcname, tmp->filename));
                return (!(*funcname = tmp->funcname));
            }
        }
        tmp = tmp->Next;
    }
    return (1);
}

RememberRecentRead(fname, funcname, permanent)
char           *fname, *funcname;
int             permanent;
{

    struct SlurpHistEnt *tmp = SlurpHistListHead, *prev = NULL;
    struct stat     stbuf;
    Boolean         NukeIt;

    while (tmp) {
        NukeIt = FALSE;
        if (!strcmp(fname, tmp->filename)) {
            debug(4096, ("Found old entry for file %s.\n", tmp->filename));
            if (strcmp(funcname, tmp->funcname)) {
                debug(4096, ("Apparently the initial function definition has changed from %s to %s.\n", tmp->funcname, funcname));
            }
            NukeIt = TRUE;
        }
        if (!strcmp(funcname, tmp->funcname)) {
            debug(4096, ("Found old entry for function %s.\n", tmp->funcname));
            if (strcmp(fname, tmp->filename)) {
                char            ErrorText[2000];

                sprintf(ErrorText, "%s is defined in both %s and ", funcname, ap_Shorten(tmp->filename));
                strcat(ErrorText, ap_Shorten(fname));
                strcat(ErrorText, " (inefficient).\n");
                NonfatalBizarreError(ErrorText);
            }
            NukeIt = TRUE;
        }
        if (NukeIt) {
            struct SlurpHistEnt *next = tmp->Next;

            debug(4096, ("Nuking the old entry.\n"));
            if (prev) {
                prev->Next = next;
            }
            else
                if (tmp == SlurpHistListHead) {
                    SlurpHistListHead = next;
                }
            free(tmp->filename);
            free(tmp->funcname);
            free(tmp);
            tmp = next;
        }
        else {
            prev = tmp;
            tmp = tmp->Next;
        }
    }
    stat(fname, &stbuf);
    tmp = (struct SlurpHistEnt *) malloc(1 + sizeof(struct SlurpHistEnt));
    if (tmp) {                         /* Malloc errors here simply result in
                                        * rereading the defuns later. */
        tmp->modtime = stbuf.st_mtime;
        tmp->permanent = permanent;
        tmp->funcname = malloc(1 + strlen(funcname));
        if (tmp->funcname) {
            strcpy(tmp->funcname, funcname);
            tmp->filename = malloc(1 + strlen(fname));
            if (tmp->filename) {
                strcpy(tmp->filename, fname);
                tmp->Next = SlurpHistListHead;
                SlurpHistListHead = tmp;
                debug(4096, ("Created new SlurpHistList entry for function %s in file %s.\n", funcname, fname));
            }
            else {
                free(tmp->funcname);
                free(tmp);
            }
        }
        else {
            free(tmp);
        }
    }
    return;
}

int             Flames_SlurpFlamesFile(st, fName, firstfuncname)
EliState_t     *st;
char           *fName;
char          **firstfuncname;
{
    char            expression[MAXPATHLEN + 10];
    EliSexp_t      *node1, *node2 = NULL;
    int             sawerr = FALSE;

    sprintf(expression, "(read %s)\n", EliUnParseStr(fName));

    if ((!(node1 = eliSGetSexp_trace(st, EliTraceStk(st), expression)))
        || (!(node2 = eliSexp_GetNew_trace(st, EliTraceStk(st))))) {
        sawerr = TRUE;
    }
    if (!sawerr) {
        eliEval(st, node1, node2);
        if (EliErr_ErrP(st)) {
            sawerr = TRUE;
        }
        else {
            if (EliNilP(st, node2)) {
                sawerr = TRUE;
            }
        }
    }
    if (sawerr) {
        if (MSDebugging & 4096) {
            printf("\n*** FLAMES ERROR: %s in %s\n", EliErrStr(EliErr_ErrCode(st)), EliErr_ErrLoc(st));
            if (EliErr_BadSexpP(st)) {
                printf("***  Expression: ");
                EliDisplaySexp(EliErr_BadSexp(st));
                putchar('\n');
            }
        }
        /* Following is a terribly bogus hack.  Just wanted to let you know... */
        if ((EliErr_ErrCode(st) == ELI_ERR_BAD_ARGS)
             && ((!strncmp(EliErr_ErrLoc(st), "ELI-PRIMITIVE [LOAD", 19))
                  || (!strncmp(EliErr_ErrLoc(st), "ELI-PRIMITIVE [READ", 19))))
            AMS_RETURN_ERRCODE(EliErr_UnixErr(st) ? EliErr_UnixErr(st) : EMSUNKNOWN,
                                EIN_READFLAMES, EVIA_FLAMES_HANDLENEW);
        return (Flames_EliToAMSErr(st));
    }
    if (EliSexp_GetType(node2) != e_data_list) {
        EliError(st, ELI_ERR_BAD_ARGS, node2, "FLAMES [Flames_SlurpFlamesFile (READ didn't return a list)]", 0);
        return (Flames_EliToAMSErr(st));
    }
    node1 = EliCons_GetCar(EliSexp_GetCons(node2));
    if (EliSexp_GetType(node1) != e_data_symbol) {
        EliError(st, ELI_ERR_BAD_ARGS, node1, "FLAMES [Flames_SlurpFlamesFile (1st item in file not a DEFUN)]", 0);
        return (Flames_EliToAMSErr(st));
    }
    *firstfuncname = EliStr_GetString(EliSym_GetName(EliSexp_GetSym(node1)));
    return (0);
}

#ifdef NOTUSED
FlamesToEliError(st, resbuf, e1, e2, e3)
EliState_t     *st;
EliSexp_t      *resbuf;
int             e1, e2, e3;
{
    AMS_SET_ERRCODE(e1, e2, e3);
    TellEliAboutAMSError(st, resbuf);
}

#endif                                 /* NOTUSED */

TellEliAboutAMSError(st, resbuf, errortext, unixErr)
EliState_t     *st;
EliSexp_t      *resbuf;
char           *errortext;
int unixErr;
{
    SquirreledError = 1;
    EliError(st, ELI_ERR_CLIENT, resbuf, errortext, unixErr);
}

/* This routine defines LISP primitives for FLAMES */

MyPrimDef(st, name, fn)
EliState_t     *st;
char           *name;
void            (*fn) ();

{
    eliPrimDefCompiled(st, name, fn);
    return (EliErr_ErrP(st));
}

FLAMES_TranslateArgs(st, arglist, resbuf, minargc, maxargc, argv, typev, evalv, fname)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
int             minargc, maxargc;
char          **argv;
eliDataTypes_t  typev[];
int            *evalv;
char           *fname;                 /* for debugging line */

{
    EliSexp_t     **argnodes, *err;
    int             numargs, i;

    argnodes = (EliSexp_t **) malloc((1 + sizeof(EliSexp_t *)) * maxargc);
    if (!argnodes) {
        EliError(st, ELI_ERR_OUT_OF_MEM, NULL, "FLAMES [FLAMES_TranslateArgs (allocating arg array)]", 0);
        return (-1);
    }
    numargs = EliGetListCars(arglist, argnodes, maxargc);
    if (minargc < 0)
        minargc = 0;
    if ((numargs < minargc) || (numargs > maxargc)) {
        free(argnodes);
        if (!(err = eliSexp_GetNew_trace(st, EliTraceStk(st)))) {
            return (-1);
        }
        EliSexp_SetCons(st, err, arglist);
        EliError(st, ELI_ERR_ARGLISTSIZE, err, "FLAMES [FLAMES_TranslateArgs (wrong # of args)]", 0);
        debug(4096, ("FLAMES primitive %s was called with the wrong number of arguments (expected range: %d-%d\n", fname, minargc, maxargc));
        return (-1);
    }
    for (i = 0; i < numargs; ++i) {
        EliSexp_t      *resbuf2;

        if (!(resbuf2 = eliSexp_GetNew_trace(st, EliTraceStk(st)))) {
            free(argnodes);
            return (-1);
        }
        if (!evalv || evalv[i]) {
            eliEval(st, argnodes[i], resbuf2);
            if (EliErr_ErrP(st)) {
                free(argnodes);
                debug(4096, ("FLAMES primitive %s was evaluated with %d arguments but the evaluation of argument #%d failed.\n", fname, numargs, 1 + i));
                return (-1);
            }
        }
        else {
            resbuf2 = argnodes[i];     /* Don't need to worry about the
                                        * reference counts because we use
                                        * foo_getnew_err to put it on errstk */
        }
        if (typev && typev[i] == e_data_none) {
            argv[i] = (char *) resbuf2;
        }
        else {
            if (EliSexp_GetType(resbuf2) != (typev ? typev[i] : e_data_string)) {
                debug(4096, ("Mismatched type codes in argument #%d in call to FLAMES function %s\n", 1 + i, fname));
                free(argnodes);
                EliError(st, ELI_ERR_BAD_ARGS, resbuf2, "FLAMES [FLAMES_TranslateArgs (bad arg type)]", 0);
                return (-1);
            }
            switch (EliSexp_GetType(resbuf2)) {
                case e_data_integer:
                    argv[i] = (char *) EliSexp_GetInt(resbuf2);
                    if ((struct MS_Message *) EliSexp_GetInt(resbuf2) == currentMessage) {
                        debug(4096, ("Argument #%d is an int (the current message)\n", 1 + i));
                    }
                    else {
                        debug(4096, ("Argument #%d is (int) %d\n", 1 + i, argv[i]));
                    }
                    break;
                case e_data_string:
                    argv[i] = EliStr_GetString(EliSexp_GetStr(resbuf2));
                    debug(4096, ("Argument #%d is (char *) %s\n", 1 + i, argv[i]));
                    break;
                case e_data_list:
                    argv[i] = (char *) EliSexp_GetCons(resbuf2);
                    debug(4096, ("Argument #%d is a list.\n", 1 + i));
                    break;
                case e_data_symbol:
                    argv[i] = (char *) EliSexp_GetSym(resbuf2);
                    debug(4096, ("Argument #%d is a symbol.\n", 1 + i));
                    break;
                default:
                    debug(4096, ("Unrecognized type code in evaluating argument %d to FLAMES call %s\n", i, fname));
                    free(argnodes);
                    EliError(st, ELI_ERR_BAD_ARGS, resbuf2, "FLAMES [FLAMES_TranslateArgs (unknown arg type requested)]", 0);
                    return (-1);
            }
        }
    }
    free(argnodes);
    debug(4096, ("Successfully got %d arguments to FLAMES primitive %s\n", numargs, fname));
    return (numargs);
}

PrepareReturnString(st, string, len, resbuf)
EliState_t     *st;
char           *string;
int             len;
EliSexp_t      *resbuf;
{
    EliStr_t       *strtmp;
    char            c = 0;

    if (len) {
        c = string[len];
        string[len] = '\0';
    }
    else {
        string = "";
    }
    if (!(strtmp = EliStringTable_FindOrMake(st, string))) {
        return (-1);
    }
    if (len) {
        string[len] = c;
    }
    EliSexp_SetStr(st, resbuf, strtmp);
    return (0);
}

/* This routine takes a file name, which is the path to a message
 * in an AMS folder, and changes the protection bits on the file to
 * match those on the .MS_MsgDir file in the same folder
 */
static int ReprotectBody(name)
char *name;
{
    char dirName[1 + MAXPATHLEN], *ptr;
    struct stat statbuf;
    int bits;

    strcpy(dirName, name);
    if (ptr = strrchr(dirName, '/')) {
        strcpy(++ptr, MS_DIRNAME);
    }
    else
        return (1);
    if (stat(dirName, &statbuf)) {
        return (1);
    }
    bits = statbuf.st_mode;
    if (chmod(name, bits)) {
        return (1);
    }
    return (0);
}

void            RealAppendMsgToDir(st, arglist, resbuf, AllowRenaming)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
Boolean         AllowRenaming;
{
    char           *dir;
    static char errstr[200];
    struct MS_Message *Msg;
    struct MS_Directory *Dir;
    char            NewFileName[1 + MAXPATHLEN];
    int             wasntRenamed = FALSE, processStat, evalV[2];
    EliSexp_t *sexpBufV[2], *errBuf;
    eliDataTypes_t typeV[2];

    typeV[0] = e_data_integer;
    typeV[1] = e_data_string;
    evalV[0] = evalV[1] = TRUE;
    processStat = EliProcessList(st, arglist, 2, 2, sexpBufV, &errBuf, typeV, evalV);

    if ((processStat == -1) || (processStat == -2)) {
        EliError(st, ELI_ERR_ARGLISTSIZE, errBuf, (AllowRenaming ? "FLAMES-PRIMITIVE [FINALAPPENDMSGTODIR (checking arglist size)]" : "FLAMES-PRIMITIVE [APPENDMSGTODIR (checking arglist size)]"), 0);
        return;
    }
    if (processStat <= -2000)
        return;
    if (processStat <= -1000) {
        sprintf(errstr, "FLAMES-PRIMITIVE [%s (arg %d is of the wrong type)]",
                 AllowRenaming ? "FINALAPPENDMSGTODIR" : "APPENDMSGTODIR",
                 1 - (processStat + 1000));
        EliError(st, ELI_ERR_BAD_ARGS, errBuf, errstr, 0);
        return;
    }

    Msg = (struct MS_Message *) EliSexp_GetInt(sexpBufV[0]);
    dir = EliStr_GetString(EliSexp_GetStr(sexpBufV[1]));

    if (ReadOrFindMSDir(dir, &Dir, MD_APPEND)) {
        TellEliAboutAMSError(st, sexpBufV[1], AllowRenaming ? "FLAMES-PRIMITIVE [FINALAPPENDMSGTODIR (finding dir for appending)]" : "FLAMES-PRIMITIVE [APPENDMSGTODIR (finding dir for appending)]", 0);
        return;
    }

    sprintf(NewFileName, "%s/+%s", Dir->UNIXDir, AMS_ID(Msg->Snapshot));

    if (IsMessageAlreadyThere(Msg, Dir)) {
        if (CacheDirectoryForClosing(Dir, MD_APPEND)) {
            CloseMSDir(Dir, MD_APPEND);
            TellEliAboutAMSError(st, EliCons_GetCar(EliSexp_GetCons(EliCons_GetCdr(arglist))), AllowRenaming ? "FLAMES-PRIMITIVE [FINALAPPENDMSGTODIR (can't cache dir for closing)]" : "FLAMES-PRIMITIVE [APPENDMSGTODIR (can't cache dir for closing)]", 0);
            return;
        }
        else {
            EliSexp_SetSym(st, resbuf, EliTSym(st));
            return;
        }
    }

    /*
     * Now, the "right" thing to do is to write a new file out into the folder
     * as the body file. Unfortunately, there is an irresistable but dangerous
     * performance hack to be had about here.  Under certain circumstances, we
     * can just rename the file from the mailbox into the folder, thus saving
     * some potentially expensive AFS operations. In FLAMES, we do this only
     * if the "finalappendmsgtodir" primitive was used instead of
     * "appendmsgtodir", and only if we know the name of the mailbox file, and
     * only if we haven't changed any headers in the message, and only if the
     * rename itself succeeds (it will fail if it is a cross-volume/device
     * rename).
     */

    if (!AllowRenaming
         || (currentMessage != Msg) || (!currentMessageFile)
         || Msg->WeFiddled
         || (wasntRenamed = RenameEvenInVice(currentMessageFile,
                                              NewFileName))
         || ReprotectBody(NewFileName)) {
	debug(4096, ("Going the long route of writing out the file anew\n"));
	if (WritePureFile(Msg, NewFileName, FALSE, 0664)) {
	    CloseMSDir(Dir, MD_APPEND);
	    TellEliAboutAMSError(st, sexpBufV[1], AllowRenaming ? "FLAMES-PRIMITIVE [FINALAPPENDMSGTODIR (couldn't write body)]" : "FLAMES-PRIMITIVE [APPENDMSGTODIR (couldn't write body)]", 0);
	    return;
	}
    }
    else {
	debug(4096, ("Renamed the file instead of rewriting it.  Fingers crossed.\n"));
    }
    if (AppendMessageToMSDir(Msg, Dir)) {
	CloseMSDir(Dir, MD_APPEND);
	if (wasntRenamed)              /* If it *was* renamed, then the filed
					* copy is the only surviving one and
					* we don't delete it */
	    unlink(NewFileName);
	TellEliAboutAMSError(st,
			     sexpBufV[1],
			     AllowRenaming ?
			     "FLAMES-PRIMITIVE [FINALAPPENDMSGTODIR (couldn't append to dir)]" :
			     "FLAMES-PRIMITIVE [APPENDMSGTODIR (couldn't append to dir)]", 0);
	return;
    }
    if (CacheDirectoryForClosing(Dir, MD_APPEND)) {
	CloseMSDir(Dir, MD_APPEND);
	TellEliAboutAMSError(st,
			     sexpBufV[1],
			     AllowRenaming ?
			     "FLAMES-PRIMITIVE [FINALAPPENDMSGTODIR (can't cache dir for closing)]" :
			     "FLAMES-PRIMITIVE [APPENDMSGTODIR (can't cache dir for closing)]", 0);
    }
    else {
	EliSexp_SetSym(st, resbuf, EliTSym(st));
    }
}

void            FLAMES_Prim_FinalAppendMsgToDir(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    RealAppendMsgToDir(st, arglist, resbuf, TRUE);
}

void            FLAMES_Prim_AppendMsgToDir(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    RealAppendMsgToDir(st, arglist, resbuf, FALSE);
}

/* Called in LISP as: (GetHeaderContents Msg[long] header[string]) */
void            FLAMES_Prim_GetHeaderContents(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    struct MS_Message *Msg;
    char           *headname, *argv[2];
    int             i;
    EliCons_t      *resultlist = (EliCons_t *) 0;
    EliSexp_t      *sexp;

    if (FLAMES_TranslateArgs(st, arglist, resbuf,
                             2, 2, argv, OneIntOneString,
                             NULL, "getheadercontents") < 0) {
        return;
    }
    Msg = (struct MS_Message *) argv[0];
    headname = argv[1];
    LowerStringInPlace(headname, strlen(headname));
    for (i = 0; Msg->ParsedStuff->HeadName[i]; ++i) {
        if ((!strcmp(headname, Msg->ParsedStuff->HeadName[i]))
            && (Msg->ParsedStuff->HeadBodyLen[i] > 0)) {
            if (!(sexp = EliSexp_GetNew(st)))
                return;
            if (PrepareReturnString(st, Msg->ParsedStuff->HeadBody[i],
                                    Msg->ParsedStuff->HeadBodyLen[i], sexp))
                return;
            if (!(resultlist = EliAddToList(st, resultlist, sexp)))
                return;
        }
    }
    if (resultlist)
        EliSexp_SetCons(st, resbuf, resultlist);
    else
        EliSexp_SetSym(st, resbuf, EliNilSym(st));
}

void            FLAMES_Prim_FindFolder(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    char           *inname, *argv[2], NameBuf[1 + MAXPATHLEN], RootName[1 + MAXPATHLEN], *s;
    int             WantsToWrite = FALSE, WillingToCreate = FALSE;
    EliSexp_t *errbuf = EliSexp_GetNew(st);
    EliStr_t *strbuf;

    if (!errbuf)
        return;
    if (FLAMES_TranslateArgs(st, arglist, resbuf, 2, 2, argv, TwoStrings, NULL, "findfolder") < 0) {
        return;                        /* errors were flagged */
    }
    if (!(inname = EliSaveString(argv[0]))) {
        AMS_SET_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_FLAMES_HANDLENEW);
        TellEliAboutAMSError(st, NULL, "FLAMES-PRIMITIVE [FINDFOLDER (allocating folder-name buffer)]", ENOMEM);
        return;
    }
    if (!(strbuf = EliStringTable_FindOrMake(st, inname))) {
        free(inname);
        return;
    }
    EliSexp_SetStr(st, errbuf, strbuf);
    if (FindTreeRoot(inname, RootName, FALSE)) {
        RootName[0] = '\0';
    }
    for (s = inname + 1 + strlen(RootName); *s; ++s) {
        if (*s == '.') {
            *s = '/';                  /* Convert cmu.general to cmu/general,
                                        * etc. */
        }
    }
    switch (argv[1][0]) {
        case 'c':
            WillingToCreate = TRUE;
            /* drop through */
        case 'w':
            WantsToWrite = TRUE;
            break;
        case 'r':
            break;
        default:
            AMS_SET_ERRCODE(EINVAL, EIN_PARAMCHECK, EVIA_FLAMES_HANDLENEW);
            TellEliAboutAMSError(st, EliCons_GetCar(EliSexp_GetCons(EliCons_GetCdr(arglist))), "FLAMES-PRIMITIVE [FINDFOLDER (bad file mode given)]", EINVAL);
            break;
    }
    if (MS_DisambiguateFile(inname, NameBuf, AMS_DISAMB_DIREXISTS)) {
        if (AMS_ERRNO == ENOENT) {
            int             creatable = 0;

            if (WillingToCreate && CheckFolderIsCreatable(inname, NameBuf, &creatable)) {
                TellEliAboutAMSError(st, errbuf, "FLAMES-PRIMITIVE [FINDFOLDER (can't check creatability of folder)]", 0);
            }
            else
                if (creatable) {
                    PrepareReturnString(st, NameBuf, strlen(NameBuf), resbuf);
                }
                else {
                    EliSexp_SetSym(st, resbuf, EliNilSym(st));
                }
        }
        else {
            TellEliAboutAMSError(st, errbuf, "FLAMES-PRIMITIVE [FINDFOLDER (can't disambiguate folder name)]", 0);
        }
        free(inname);
        return;
    }
    if (WantsToWrite) {
        char            FName[1 + MAXPATHLEN];

        sprintf(FName, "%s/%s", NameBuf, MS_DIRNAME);
        if (access(FName, W_OK)) {
            if (errno == EACCES) {
                EliSexp_SetSym(st, resbuf, EliNilSym(st));
            }
            else {
                AMS_SET_ERRCODE(errno, EIN_ACCESS, EVIA_FLAMES_HANDLENEW);
                TellEliAboutAMSError(st, errbuf, "FLAMES-PRIMITIVE [FINDFOLDER (accessing dir)]", errno);
            }
            free(inname);
            return;
        }
    }
    PrepareReturnString(st, NameBuf, strlen(NameBuf), resbuf);
    free(inname);
    return;
}

CheckFolderIsCreatable(inname, NameBuf, creatable)
char           *inname, *NameBuf;
int            *creatable;
{
    struct MS_Directory *Dir;
    char           *s, Scratch[1 + MAXPATHLEN], Root[1 + MAXPATHLEN];

    *creatable = 0;
    strcpy(Scratch, inname);
    s = strrchr(Scratch, '/');
    if (!s) {
        return (0);
    }
    *s++ = '\0';
    if (MS_DisambiguateFile(Scratch, NameBuf, AMS_DISAMB_DIREXISTS)) {
        if (AMS_ERRNO == ENOENT) {
            return (0);
        }
        if (AMS_ERRNO == ENOTDIR) {
            FindTreeRoot(Scratch, Root, FALSE);
            if (!strcmp(Scratch, Root)) {
                if (access(Scratch, W_OK)) {
                    AMS_RETURN_ERRCODE(errno, EIN_ACCESS, EVIA_FLAMES_HANDLENEW);
                }
                *creatable = 1;
                strcpy(NameBuf, inname);
                return (0);
            }
        }
        return (mserrcode);
    }
    if (ReadOrFindMSDir(NameBuf, &Dir, MD_WRITE)) {
        if (AMS_ERRNO == EACCES) {
            return (0);
        }
        return (mserrcode);
    }
    CloseMSDir(Dir, MD_WRITE);         /* ignore errors here */
    *creatable = 1;
    strcat(NameBuf, "/");
    strcat(NameBuf, s);
    return (0);
}



void            FLAMES_Prim_CurrentMessage(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    if (currentMessage) {
        EliSexp_SetInt(st, resbuf, (long) currentMessage);
    }
    else {
        EliSexp_SetSym(st, resbuf, EliNilSym(st));
    }
    return;
}

RestorePartialState(Msg, s1, s2)
struct MS_Message *Msg;
char           *s1, *s2;
{
    strcpy(AMS_DATE(s1), AMS_DATE(s2));
    strcpy(AMS_CAPTION(s1), AMS_CAPTION(s2));
    strcpy(AMS_ID(s1), AMS_ID(s2));
    if (BuildAttributesField(Msg) || BuildReplyField(Msg) || CheckAuthUid(Msg))
        return (mserrcode);
    return (0);
}

void            FLAMES_Prim_AddHeader(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    struct MS_Message *Msg;
    char           *header, *argv[2], SBuf[AMS_SNAPSHOTSIZE + 1];

    if (FLAMES_TranslateArgs(st, arglist, resbuf, 2, 2, argv, OneIntOneString, NULL, "addheader") < 0) {
        return;                        /* errors were flagged */
    }
    Msg = (struct MS_Message *) argv[0];
    header = argv[1];
    bcopy(Msg->Snapshot, SBuf, AMS_SNAPSHOTSIZE);
    if (AddHeader(Msg, header) || RestorePartialState(Msg, Msg->Snapshot, SBuf)) {
        TellEliAboutAMSError(st, EliCons_GetCar(EliSexp_GetCons(EliCons_GetCdr(arglist))), "FLAMES-PRIMITIVE [ADDHEADER (internal error)]", 0);
        return;
    }
    EliSexp_SetSym(st, resbuf, EliTSym(st));
}

void            FLAMES_Prim_DeleteHeader(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    struct MS_Message *Msg;
    char           *header, *argv[2], SBuf[AMS_SNAPSHOTSIZE + 1];

    if (FLAMES_TranslateArgs(st, arglist, resbuf, 2, 2, argv, OneIntOneString, NULL, "deleteheader") < 0) {
        return;                        /* errors were flagged */
    }
    Msg = (struct MS_Message *) argv[0];
    header = argv[1];
    bcopy(Msg->Snapshot, SBuf, AMS_SNAPSHOTSIZE);
    if (DelHeaderByName(Msg, header) || RestorePartialState(Msg, Msg->Snapshot, SBuf)) {
        TellEliAboutAMSError(st, EliCons_GetCar(EliSexp_GetCons(EliCons_GetCdr(arglist))), "FLAMES-PRIMITIVE [DELETEHEADER (internal error)]", 0);
        return;
    }
    EliSexp_SetSym(st, resbuf, EliTSym(st));
}

void            FLAMES_Prim_RejectMessage(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *sexpBufV[4], *errbuf;
    eliDataTypes_t  typeV[4], totype, cctype;
    int             evalV[4], presult;
    static char     errstr[80];

    evalV[0] = evalV[1] = evalV[2] = evalV[3] = TRUE;
    typeV[0] = e_data_integer;         /* msg */
    typeV[1] = e_data_none;            /* to */
    typeV[2] = e_data_none;            /* cc */
    typeV[3] = e_data_string;          /* rejstr */
    presult = EliProcessList(st, arglist, 4, 4,
                             sexpBufV, &errbuf, typeV, evalV);
    if ((presult == -1) || (presult == -2)) {
        EliError(st, ELI_ERR_ARGLISTSIZE, errbuf, "FLAMES-PRIMITIVE [REJECTMESSAGE (checking arglist size)]", 0);
        return;
    }
    if (presult <= -2000)
        return;
    if (presult <= -1000) {
        sprintf(errstr, "FLAMES-PRIMITIVE [REJECTMESSAGE (arg %d is of the wrong type)]",
                1 - (presult + 1000));
        EliError(st, ELI_ERR_BAD_ARGS, errbuf, errstr, 0);
        return;
    }
    totype = EliSexp_GetType(sexpBufV[1]);
    if ((!EliNilP(st, sexpBufV[1])) && (totype != e_data_string)) {
        EliError(st, ELI_ERR_BAD_ARGS, sexpBufV[1], "FLAMES-PRIMITIVE [REJECTMESSAGE (arg 2 is of the wrong type)]", 0);
        return;
    }
    cctype = EliSexp_GetType(sexpBufV[2]);
    if ((!EliNilP(st, sexpBufV[2])) && (cctype != e_data_string)) {
        EliError(st, ELI_ERR_BAD_ARGS, sexpBufV[2], "FLAMES-PRIMITIVE [REJECTMESSAGE (arg 3 is of the wrong type)]", 0);
        return;
    }
    if (RejectMessage((struct MS_Message *) EliSexp_GetInt(sexpBufV[0]),
                      EliStr_GetString(EliSexp_GetStr(sexpBufV[3])),
                       (totype == e_data_string) ?
                       EliStr_GetString(EliSexp_GetStr(sexpBufV[1])) : NULL,
                      (cctype == e_data_string) ?
                      EliStr_GetString(EliSexp_GetStr(sexpBufV[2])) : NULL)) {
        TellEliAboutAMSError(st, NULL, "FLAMES-PRIMITIVE [REJECTMESSAGE (internal error)]", 0);
        return;
    }
    EliSexp_SetSym(st, resbuf, EliTSym(st));
}

void            FLAMES_Prim_ResendMessage(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    struct MS_Message *Msg;
    char           *to, *argv[2];

    if (FLAMES_TranslateArgs(st, arglist, resbuf, 2, 2, argv, OneIntOneString, NULL, "resendmessage") < 0) {
        return;                        /* errors were flagged */
    }
    Msg = (struct MS_Message *) argv[0];
    to = argv[1];
    if (ResendMessageFromMailbox(Msg, to, TRUE)) {
        TellEliAboutAMSError(st, NULL, "FLAMES-PRIMITIVE [RESENDMESSAGE (internal error)]", 0);
        return;
    }
    EliSexp_SetSym(st, resbuf, EliTSym(st));
}

void            FLAMES_Prim_TracelessResendMessage(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    struct MS_Message *Msg;
    char           *to, *argv[2];

    if (FLAMES_TranslateArgs(st, arglist, resbuf, 2, 2, argv, OneIntOneString, NULL, "tracelessresendmessage") < 0) {
        return;                        /* errors were flagged */
    }
    Msg = (struct MS_Message *) argv[0];
    to = argv[1];
    if (ResendMessageFromMailbox(Msg, to, FALSE)) {
        TellEliAboutAMSError(st, NULL, "FLAMES-PRIMITIVE [TRACELESSRESENDMESSAGE (internal error)]", 0);
        return;
    }
    EliSexp_SetSym(st, resbuf, EliTSym(st));
}

void            FLAMES_Prim_SetCaption(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    struct MS_Message *Msg;
    char           *capt, *argv[2];

    if (FLAMES_TranslateArgs(st, arglist, resbuf, 2, 2, argv, OneIntOneString, NULL, "setcaption") < 0) {
        return;                        /* errors were flagged */
    }
    Msg = (struct MS_Message *) argv[0];
    capt = argv[1];
    strncpy(AMS_CAPTION(Msg->Snapshot), capt, AMS_CAPTIONSIZE);
    EliSexp_SetSym(st, resbuf, EliTSym(st));
}

void            FLAMES_Prim_GetCaption(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    struct MS_Message *Msg;
    char           *argv[1];

    if (FLAMES_TranslateArgs(st, arglist, resbuf, 1, 1, argv, OneInt, NULL, "getcaption") < 0) {
        return;                        /* errors were flagged */
    }
    Msg = (struct MS_Message *) argv[0];
    PrepareReturnString(st, AMS_CAPTION(Msg->Snapshot), strlen(AMS_CAPTION(Msg->Snapshot)), resbuf);
}

void            FLAMES_Prim_GetPartialBody(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    struct MS_Message *Msg;
    char           *argv[3], *bodybuf = NULL;
    int             start, len;

    if (FLAMES_TranslateArgs(st, arglist, resbuf, 3, 3, argv, ThreeInts, NULL, "getpartialbody") < 0) {
        return;                        /* errors were flagged */
    }
    Msg = (struct MS_Message *) argv[0];
    start = (int) argv[1];
    len = (int) argv[2];
    if (len == 0) {
        PrepareReturnString(st, "", 0, resbuf);
        return;
    }
    if ((start >= 0) && (len > 0) && ((start + len) <= (Msg->FullSize - Msg->HeadSize))) {
        bodybuf = malloc(1 + len);
        if (!bodybuf) {
            AMS_SET_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_FLAMES_HANDLENEW);
            TellEliAboutAMSError(st, NULL, "FLAMES-PRIMITIVE [GETPARTIALBODY (allocating body buffer)]", ENOMEM);
            return;
        }
    }
    else {
        AMS_SET_ERRCODE(EINVAL, EIN_PARAMCHECK, EVIA_FLAMES_HANDLENEW);
        TellEliAboutAMSError(st, EliCons_GetCdr(arglist), "FLAMES-PRIMITIVE [GETPARTIALBODY (bad file portion specified)]", EINVAL);
        return;
    }
    if (lseek(Msg->OpenFD, Msg->BodyOffsetInFD + start, L_SET) < 0) {
        AMS_SET_ERRCODE(errno, EIN_LSEEK, EVIA_FLAMES_HANDLENEW);
        TellEliAboutAMSError(st, EliCons_GetCdr(arglist), "FLAMES-PRIMITIVE [GETPARTIALBODY (finding requested file portion)]", errno);
        return;
    }
    if (read(Msg->OpenFD, bodybuf, len) != len) {
        AMS_SET_ERRCODE(errno, EIN_READ, EVIA_FLAMES_HANDLENEW);
        TellEliAboutAMSError(st, EliCons_GetCdr(arglist), "FLAMES-PRIMITIVE [GETPARTIALBODY (reading requested file portion)]", errno);
        return;
    }
    PrepareReturnString(st, bodybuf, len, resbuf);
}

void            FLAMES_Prim_CreateFolder(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    char           *argv[1];
    struct MS_Directory *Dir;

    if (FLAMES_TranslateArgs(st, arglist, resbuf, 1, 1, argv, OneString, NULL, "createfolder") < 0) {
        return;                        /* errors were flagged */
    }
    if (CreateNewMSDirectory(argv[0], &Dir, TRUE)) {
        TellEliAboutAMSError(st, EliCons_GetCar(arglist), "FLAMES-PRIMITIVE [CREATEFOLDER (internal error)]", 0);
        return;
    }
    EliSexp_SetSym(st, resbuf, EliTSym(st));
}

#ifdef NOTUSED
void            FLAMES_Prim_System(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    char           *argv[1];

    if (FLAMES_TranslateArgs(st, arglist, resbuf, 1, 1, argv, OneString, NULL, "system") < 0) {
        return;                        /* errors were flagged */
    }
    EliSexp_SetInt(st, resbuf, system(argv[0]));
}

#endif                                 /* NOTUSED */

void            FLAMES_Prim_GetParameter(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    char           *argv[1], *parm;

    if (FLAMES_TranslateArgs(st, arglist, resbuf, 1, 1, argv, OneString, NULL, "getparameter") < 0) {
        return;                        /* errors were flagged */
    }
    parm = argv[0];
    LowerStringInPlace(parm, strlen(parm));
    if (!strcmp(parm, "home")) {
	PrepareReturnString(st, home, strlen(home), resbuf);
    } else if (!strcmp(parm, "uid")) {
	PrepareReturnString(st, Me, strlen(Me), resbuf);
    } else if (!strcmp(parm, "maildomain")) {
	PrepareReturnString(st, MyMailDomain, strlen(MyMailDomain), resbuf);
    } else if (!strcmp(parm, "uidsuffix")) {
	if (homeUsesAMSDelivery > 0 || homeUsesUseridPlus) {
	    PrepareReturnString(st, "+", 1, resbuf);
	} else {
	    PrepareReturnString(st, "", 0, resbuf);
	}
    } else if (!strcmp(parm, "andrewdir")) {
	parm = (char *) AndrewDir(NULL);
	PrepareReturnString(st, parm, strlen(parm), resbuf);
    } else if (!strcmp(parm, "date")) {
	parm = (char *) arpadate();
	if (parm[strlen(parm)-1] == '\n')
	  parm[strlen(parm)-1] = '\0';
	PrepareReturnString(st, parm, strlen(parm), resbuf);
    } else if (!strcmp(parm, "time")) {
        long the_time;
	char timebuf[16];

        if (time(&the_time)<0) {
				/* should the error be
				     EIN_SYSTEM or
				     EIN_UTIMES? */
	  AMS_SET_ERRCODE(errno, EIN_UNKNOWN, EVIA_FLAMES_HANDLENEW);
	  TellEliAboutAMSError(st, NULL, "FLAMES-PRIMITIVE [GETPARAMETER (getting the time)]", errno);
	}
	sprintf(timebuf, "%ld", the_time);
	PrepareReturnString(st, timebuf, strlen(timebuf), resbuf);
    } else {
	EliSexp_SetSym(st, resbuf, EliNilSym(st));
    }
}

void            FLAMES_Prim_Getenv(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    char           *argv[1], *val;

    if (FLAMES_TranslateArgs(st, arglist, resbuf, 1, 1, argv, OneString, NULL, "getenv") < 0) {
        return;                        /* errors were flagged */
    }
    val = getenv(argv[0]);
    if (val) {
        PrepareReturnString(st, val, strlen(val), resbuf);
    } else {
        EliSexp_SetSym(st, resbuf, EliNilSym(st));
    }
}

void            FLAMES_Prim_FileLength(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    char           *argv[1];
    struct stat     stbuf;

    if (FLAMES_TranslateArgs(st, arglist, resbuf, 1, 1, argv, OneString, NULL, "filelength") < 0) {
        return;                        /* errors were flagged */
    }
    if (stat(argv[0], &stbuf)) {
        AMS_SET_ERRCODE(errno, EIN_STAT, EVIA_FLAMES_HANDLENEW);
        TellEliAboutAMSError(st, EliCons_GetCar(arglist), "FLAMES-PRIMITIVE [FILELENGTH (can't stat file)]", errno);
    }
    else {
        EliSexp_SetInt(st, resbuf, stbuf.st_size);
    }
}


void            FLAMES_Prim_ReadFile(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    char           *argv[3], *fname, *buf;
    int             start, len, fd;
    EliSexp_t      *errsexp;

    if (FLAMES_TranslateArgs(st, arglist, resbuf, 3, 3, argv, OneStringTwoInts, NULL, "readfile") < 0) {
        return;                        /* errors were flagged */
    }
    fname = argv[0];
    start = (int) argv[1];
    len = (int) argv[2];
    fd = open(fname, O_RDONLY, 0);
    if (fd < 0) {
        AMS_SET_ERRCODE(errno, EIN_READ, EVIA_FLAMES_HANDLENEW);
        TellEliAboutAMSError(st, EliCons_GetCar(arglist), "FLAMES-PRIMITIVE [READFILE (opening file)]", errno);
        return;
    }
    if (lseek(fd, start, L_SET) < 0) {
        AMS_SET_ERRCODE(errno, EIN_LSEEK, EVIA_FLAMES_HANDLENEW);
        close(fd);
        if (errsexp = EliSexp_GetNew(st))
            EliSexp_SetCons(st, errsexp, arglist);
        TellEliAboutAMSError(st, errsexp, "FLAMES-PRIMITIVE [READFILE (finding requested file portion)]", errno);
        return;
    }
    buf = malloc(1 + len);
    if (!buf) {
        AMS_SET_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_FLAMES_HANDLENEW);
        close(fd);
        TellEliAboutAMSError(st, NULL, "FLAMES-PRIMITIVE [READFILE (allocating result buffer)]", ENOMEM);
        return;
    }
    if (read(fd, buf, len) != len) {
        AMS_SET_ERRCODE(errno, EIN_READ, EVIA_FLAMES_HANDLENEW);
        close(fd);
        free(buf);
        if (errsexp = EliSexp_GetNew(st))
            EliSexp_SetCons(st, errsexp, arglist);
        TellEliAboutAMSError(st, errsexp, "FLAMES-PRIMITIVE [READFILE (reading requested file portion)]", errno);
        return;
    }
    PrepareReturnString(st, buf, len, resbuf);
    free(buf);
    close(fd);
}

void            FLAMES_Prim_WriteFile(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    char           *argv[3], *fname, *buf;
    int             start, len, fd;

    if (FLAMES_TranslateArgs(st, arglist, resbuf, 3, 3, argv, TwoStringsOneInt, NULL, "writefile") < 0) {
        return;                        /* errors were flagged */
    }
    fname = argv[0];
    buf = argv[1];
    start = (int) argv[2];
    len = strlen(buf);
    fd = open(fname, O_WRONLY | O_CREAT, 0664);
    if (fd < 0) {
        AMS_SET_ERRCODE(errno, EIN_READ, EVIA_FLAMES_HANDLENEW);
        TellEliAboutAMSError(st, EliCons_GetCar(arglist), "FLAMES-PRIMITIVE [WRITEFILE (opening file)]", errno);
        return;
    }
    if (lseek(fd, start, L_SET) < 0) {
        AMS_SET_ERRCODE(errno, EIN_LSEEK, EVIA_FLAMES_HANDLENEW);
        close(fd);
        TellEliAboutAMSError(st, NULL, "FLAMES-PRIMITIVE [WRITEFILE (finding requested file portion)]", errno);
        return;
    }
    if (writeall(fd, buf, len) != len) {
        AMS_SET_ERRCODE(errno, EIN_WRITE, EVIA_FLAMES_HANDLENEW);
        close(fd);
        TellEliAboutAMSError(st, NULL, "FLAMES-PRIMITIVE [WRITEFILE (writing data)]", errno);
        return;
    }
    if (vclose(fd)) {
        AMS_SET_ERRCODE(errno, EIN_VCLOSE, EVIA_FLAMES_HANDLENEW);
        TellEliAboutAMSError(st, EliCons_GetCar(arglist), "FLAMES-PRIMITIVE [WRITEFILE (closing file)]", errno);
        return;
    }
    PrepareReturnString(st, buf, len, resbuf);

}

void            FLAMES_Prim_CreateFolderFromMessage(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    char           *argv[2], *dirname, ParentName[1 + MAXPATHLEN], Nick[1 + MAXPATHLEN], *sdum;
    struct MS_Directory *Dir;
    struct MS_Message *Msg;

    if (FLAMES_TranslateArgs(st, arglist, resbuf, 2, 2, argv, OneStringOneInt, NULL, "createfolderfrommessage") < 0) {
        return;                        /* errors were flagged */
    }
    dirname = argv[0];
    Msg = (struct MS_Message *) argv[1];
    if (CreateNewMSDirectory(dirname, &Dir, TRUE)) {
        TellEliAboutAMSError(st, EliCons_GetCar(arglist), "FLAMES-PRIMITIVE [CREATEFOLDERFROMMESSAGE (creating new dir)]", 0);
        return;
    }
    strcpy(ParentName, dirname);
    sdum = strrchr(ParentName, '/');
    if (sdum) {
        *sdum = '\0';
        BuildNickName(dirname, Nick);
        if (AddParentalMessage(Msg, ParentName, Nick, dirname) != 0) {
            TellEliAboutAMSError(st, NULL, "FLAMES-PRIMITIVE [CREATEFOLDERFROMMESSAGE (adding announcement to parent)]", 0);
            return;
        }
    }
    EliSexp_SetSym(st, resbuf, EliTSym(st));
}

void            FLAMES_Prim_BodyLength(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    struct MS_Message *Msg;
    char           *argv[1];

    if (FLAMES_TranslateArgs(st, arglist, resbuf, 1, 1, argv, OneInt, NULL, "bodylength") < 0) {
        return;                        /* errors were flagged */
    }
    Msg = (struct MS_Message *) argv[0];
    EliSexp_SetInt(st, resbuf, Msg->FullSize - Msg->HeadSize);
}

void            FLAMES_Prim_GetAuthSender(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    struct MS_Message *Msg;
    char           *argv[1], *ans;
    struct passwd  *p;

    if (FLAMES_TranslateArgs(st, arglist, resbuf, 1, 1, argv, OneInt, NULL, "getauthsender") < 0) {
        return;                        /* errors were flagged */
    }
    Msg = (struct MS_Message *) argv[0];
    if (Msg->AuthUid <= 0) {
        ans = "<UNAUTHENTICATED>";
/*    } else if (Msg->AuthUid == 0) { */
/*      ans = "root";   */
    }
    else {
        errno = 0;
        p = (struct passwd *) getcpwuid(Msg->AuthUid, Msg->AuthCell ? Msg->AuthCell : WorkstationName);
        if (p) {
            ans = p->pw_name;
        }
        else {
#ifdef WHITEPAGES_ENV
            errno = ConvertWpErrToMSErr(cpw_error, E2BIG, FALSE);
#endif                                 /* WHITEPAGES_ENV */
            if (tfail(errno)) {
                ans = "<TEMPORARILY UNRECOGNIZED USER>";
            }
            else {
                ans = "<UNKNOWN USER>";
            }
        }
    }
    PrepareReturnString(st, ans, strlen(ans), resbuf);
}

void            FLAMES_Prim_ReplyAddr(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    struct MS_Message *Msg;
    char           *argv[2];
    Boolean         ReplyAll = TRUE;

    if (FLAMES_TranslateArgs(st, arglist, resbuf, 2, 2, argv, OneIntOneString, NULL, "replyaddr") < 0) {
        return;                        /* errors were flagged */
    }
    Msg = (struct MS_Message *) argv[0];
    switch (argv[1][0]) {
        case 's':
        case 'S':
            /* reply to sender */
            if (Msg->ReplyTo) {
                PrepareReturnString(st, Msg->ReplyTo, strlen(Msg->ReplyTo), resbuf);
            }
            else {
                EliSexp_SetSym(st, resbuf, EliNilSym(st));
            }
            break;
        case 'r':
        case 'R':
            /* reply to reader */
            ReplyAll = FALSE;
            /* drop through */
        case 'a':
        case 'A':
            /* reply to sender & reader */
            if (BuildWideReply(Msg, ReplyAll)) {
                TellEliAboutAMSError(st, NULL, "FLAMES-PRIMITIVE [REPLYADDR (constructing reply-to-all addresses)]", 0);
                return;
            }
            if (Msg->WideReply) {
                PrepareReturnString(st, Msg->WideReply, strlen(Msg->WideReply), resbuf);
            }
            else {
                EliSexp_SetSym(st, resbuf, EliNilSym(st));
            }
            break;
        default:
            /* error out */
            AMS_SET_ERRCODE(EINVAL, EIN_PARAMCHECK, EVIA_FLAMES_HANDLENEW);
            TellEliAboutAMSError(st, EliCons_GetCar(EliSexp_GetCons(EliCons_GetCdr(arglist))), "FLAMES-PRIMITIVE [REPLYADDR (bad mode specified)]", EINVAL);
            return;
    }
}

void            FLAMES_Prim_UnformatMessage(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    struct MS_Message *Msg;
    char           *argv[1], SBuf[AMS_SNAPSHOTSIZE + 1];

    if (FLAMES_TranslateArgs(st, arglist, resbuf, 1, 1, argv, OneInt, NULL, "unformatmessage") < 0) {
        return;                        /* errors were flagged */
    }
    Msg = (struct MS_Message *) argv[0];
    bcopy(Msg->Snapshot, SBuf, AMS_SNAPSHOTSIZE);
    if (UnformatMessage(Msg) || RestorePartialState(Msg, Msg->Snapshot, SBuf)) {
        TellEliAboutAMSError(st, NULL, "FLAMES-PRIMITIVE [UNFORMATMESSAGE (internal error)]", 0);
        return;
    }
    EliSexp_SetSym(st, resbuf, EliTSym(st));
}

void            FLAMES_Prim_GetAuthSenderCell(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    struct MS_Message *Msg;
    char           *argv[1];

    if (FLAMES_TranslateArgs(st, arglist, resbuf, 1, 1,
                             argv, OneInt, NULL, "getauthsendercell") < 0) {
        return;
    }
    Msg = (struct MS_Message *) argv[0];
    if (Msg->AuthCell) {
        if (PrepareReturnString(st, Msg->AuthCell, strlen(Msg->AuthCell), resbuf)) {
            EliError(st, ELI_ERR_OUT_OF_MEM, NULL, "FLAMES-PRIMITIVE [GETAUTHSENDERCELL (allocating result)]", 0);
            return;
        }
    }
    else
        EliSexp_SetSym(st, resbuf, EliNilSym(st));
}

void FLAMES_Prim_DropoffMessage(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
  char           *argv[2];
  static eliDataTypes_t argtypes[] = {e_data_list, e_data_string};
  EliCons_t      *tolist;
  EliSexp_t      *node, *errnode;
  int             listlen;
  char           *message;
  char          **recipients = NULL;
  int             i;
  char           *addr;
  char            fname[MAXPATHLEN+1];
  int             fd;
  int             err;
  
  
  if (FLAMES_TranslateArgs(st, arglist, resbuf, 2, 2,
			   argv, argtypes, NULL, "dropoffmessage") < 0) {
    return;                        /* errors were flagged */
			   }	/* if (FLAMES_Trans... */
  /* tolist is argv[0] */
  tolist = (EliCons_t *) argv[0];
  /* message is argv[1] */
  message = argv[1];
  
  /* alloc space for recipients, based on length of tolist */
  if ((listlen = EliListLen(tolist)) < 1) {
    if (errnode = EliSexp_GetNew(st))
      EliSexp_SetCons(st, errnode, arglist);
    EliError(st, ELI_ERR_BAD_ARGS, errnode, "FLAMES-PRIMITIVE [DROPOFFMESSAGE (\"to\" list is empty)]", 0);
    return;                        /* error! no addresses to deliver to */
  }
  if ((recipients = (char **) malloc((listlen+1) * sizeof(*recipients))) == NULL) {
    EliError(st, ELI_ERR_OUT_OF_MEM, NULL, "FLAMES-PRIMITIVE [DROPOFFMESSAGE (Couldn't allocate memory for copy of recipients)]", ENOMEM);
    return;                        /* error! malloc failed */
  }
  
  /*
   * for each elt in tolist, check that it is a string, allocate space for
   * it (in recipients), copy the string into recipients
   */
  i = 0;
  do {
    node = EliCons_GetCar(tolist);
    if (EliSexp_GetType(node) != e_data_string) {
      EliError(st, ELI_ERR_BAD_PARAM, node, "FLAMES-PRIMITIVE [DROPOFFMESSAGE (Destination address is not a string)]", 0);
      return;                    /* error! wrong type */
    }   
    addr = EliStr_GetString(EliSexp_GetStr(node));
    
    if ((recipients[i] = (char *) malloc(strlen(addr) + 1)) == NULL) {
      EliError(st, ELI_ERR_OUT_OF_MEM, NULL, "FLAMES-PRIMITIVE [DROPOFFMESSAGE (Couldn't allocate memory for copy of address)]", ENOMEM);
      return;                    /* error! malloc failed */
    }
    strcpy(recipients[i++], addr);
  } while (tolist = EliGetNextCell(tolist));
  recipients[i] = NULL;
  
  /* write the message to a file */
  if (tmpnam(fname) == NULL) {
    EliError(st, ELI_ERR_SYSERROR, NULL, "FLAMES-PRIMITIVE [DROPOFFMESSAGE (Couldn't get a temporary filename)]", 0);
    return;                        /* error! tmpnam failed */
  }
  if ((fd = open(fname, O_WRONLY | O_CREAT, 0664)) < 0) {
    EliError(st, ELI_ERR_SYSERROR, NULL, "FLAMES-PRIMITIVE [DROPOFFMESSAGE (Couldn't open a temporary file)]", errno);
    return;                        /* error! open failed */
  }
  if (writeall(fd, message, strlen(message)) != strlen(message)) {
    AMS_SET_ERRCODE(errno, EIN_WRITE, EVIA_FLAMES_HANDLENEW);
    close(fd);
    TellEliAboutAMSError(st, NULL, "FLAMES-PRIMITIVE [DROPOFFMESSAGE (writing data)]", errno);
    return;
  }

  /* close the file */
  if (vclose(fd)) {
    AMS_SET_ERRCODE(errno, EIN_VCLOSE, EVIA_FLAMES_HANDLENEW);
    TellEliAboutAMSError(st, EliCons_GetCar(arglist), "FLAMES-PRIMITIVE [DROPOFFMESSAGE (closing file)]", errno);
    return;
  }

  /* call dropoff */
  err = dropoff(recipients, fname, NULL, NULL, 0);
  /* return dropoff's error code */
  {
    EliCons_t *retvalue = NULL;
    EliSexp_t *errcode, *errmsg;

    if ((errcode = EliSexp_GetNew(st)) == NULL) {
      Flames_EliToAMSErr(st);
      return;
    }
    EliSexp_SetInt(st, errcode, (long) err);

    if ((errmsg = EliSexp_GetNew(st)) == NULL) {
      Flames_EliToAMSErr(st);
      return;
    }
    EliSexp_SetStr(st, errmsg, EliStringTable_FindOrMake(st, Dropoff_ErrMsg));
    
    if ((retvalue = EliAddToList(st, retvalue, errcode)) == NULL) {
      Flames_EliToAMSErr(st);
      return;
    }
    if ((retvalue = EliAddToList(st, retvalue, errmsg)) == NULL) {
      Flames_EliToAMSErr(st);
      return;
    }
    EliSexp_SetCons(st, resbuf, retvalue);
  }

  /* Remove the temporary file */
  unlink(fname);

  return;
}

void FLAMES_Prim_DropoffFile(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
  char           *argv[2];
  static eliDataTypes_t argtypes[] = {e_data_list, e_data_string};
  EliCons_t      *tolist;
  EliSexp_t      *node, *errnode;
  int             listlen;
  char           *messagefile;
  char          **recipients = NULL;
  int             i;
  char           *addr;
  int             err;
  
  if (FLAMES_TranslateArgs(st, arglist, resbuf, 2, 2,
			   argv, argtypes, NULL, "dropofffile") < 0) {
    return;                        /* errors were flagged */
			   }	/* if (FLAMES_Trans... */
  /* tolist is argv[0] */
  tolist = (EliCons_t *) argv[0];
  /* messagefile is argv[1] */
  messagefile = argv[1];
  
  /* alloc space for recipients, based on length of tolist */
  if ((listlen = EliListLen(tolist)) < 1) {
    if (errnode = EliSexp_GetNew(st))
      EliSexp_SetCons(st, errnode, arglist);
    EliError(st, ELI_ERR_BAD_ARGS, errnode, "FLAMES-PRIMITIVE [DROPOFFFILE (\"to\" list is empty)]", 0);
    return;                        /* error! no addresses to deliver to */
  }
  if ((recipients = (char **) malloc((listlen + 1) * sizeof(*recipients))) == NULL) {
    EliError(st, ELI_ERR_OUT_OF_MEM, NULL, "FLAMES-PRIMITIVE [DROPOFFFILE (Couldn't allocate memory for copy of recipients)]", ENOMEM);
    return;                        /* error! malloc failed */
  }
  
  /*
   * for each elt in tolist, check that it is a string, allocate space for
   * it (in recipients), copy the string into recipients
   */
  i = 0;
  do {
    node = EliCons_GetCar(tolist);
    if (EliSexp_GetType(node) != e_data_string) {
      EliError(st, ELI_ERR_BAD_PARAM, node, "FLAMES-PRIMITIVE [DROPOFFFILE (Destination address is not a string)]", 0);
      return;                    /* error! wrong type */
    }   
    addr = EliStr_GetString(EliSexp_GetStr(node));
    
    if ((recipients[i] = (char *) malloc(strlen(addr) + 1)) == NULL) {
      EliError(st, ELI_ERR_OUT_OF_MEM, NULL, "FLAMES-PRIMITIVE [DROPOFFFILE (Couldn't allocate memory for copy of address)]", ENOMEM);
      return;                    /* error! malloc failed */
    }
    strcpy(recipients[i++], addr);
  } while (tolist = EliGetNextCell(tolist));
  recipients[i] = NULL;
  
  /* call dropoff */
  err = dropoff(recipients, messagefile, NULL, NULL, 0);
  /* return dropoff's error code */
  {
    EliCons_t *retvalue = NULL;
    EliSexp_t *errcode, *errmsg;

    if ((errcode = EliSexp_GetNew(st)) == NULL) {
      Flames_EliToAMSErr(st);
      return;
    }
    EliSexp_SetInt(st, errcode, (long) err);

    if ((errmsg = EliSexp_GetNew(st)) == NULL) {
      Flames_EliToAMSErr(st);
      return;
    }
    EliSexp_SetStr(st, errmsg, EliStringTable_FindOrMake(st, Dropoff_ErrMsg));
    
    if ((retvalue = EliAddToList(st, retvalue, errcode)) == NULL) {
      Flames_EliToAMSErr(st);
      return;
    }
    if ((retvalue = EliAddToList(st, retvalue, errmsg)) == NULL) {
      Flames_EliToAMSErr(st);
      return;
    }
    EliSexp_SetCons(st, resbuf, retvalue);
  }
  return;
}

void            FLAMES_Prim_GenID(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t      *sexpBufV[1], *errbuf;
    eliDataTypes_t  typeV[1];
    int             evalV[1], presult;
    char *gened_id;

    evalV[0] = TRUE;
    typeV[0] = e_data_none;         /* long or short genid flag */
    presult = EliProcessList(st, arglist, 1,  1,
                             sexpBufV, &errbuf, typeV, evalV);
    if ((presult == -1) || (presult == -2)) {
        EliError(st, ELI_ERR_ARGLISTSIZE, errbuf, "FLAMES-PRIMITIVE [GENID (checking arglist size)]", 0);
        return;
    }
    if (presult <= -2000)
        return;

    gened_id = ams_genid(!EliNilP(st, sexpBufV[0]));
    PrepareReturnString(st, gened_id, strlen(gened_id), resbuf);
}

#define NEW_SEXP(x) if (((x) = EliSexp_GetNew(st)) == NULL) {Flames_EliToAMSErr(st);return;}
#define CONS(x,y) if (((x) = EliAddToList(st, (x), (y))) == NULL) {Flames_EliToAMSErr(st);return;}

#ifdef AFS30_ENV
void FLAMES_Prim_GetGroupMembers(st, arglist, resbuf)
EliState_t *st;
EliCons_t *arglist;
EliSexp_t *resbuf;
{
    EliSexp_t *sexpBufV[2], *errbuf, *new_node;
    EliCons_t *return_list = NULL;
    int evalV[2], presult, aq_code;
    char *groupname, *groupcell, *group_members, *saved_head;
      
    evalV[0] = evalV[1] =  TRUE;
    presult = EliProcessList(st, arglist, 1,  2, sexpBufV, &errbuf, TwoStrings, evalV);
    if ((presult == -1) || (presult == -2)) {
      EliError(st, ELI_ERR_ARGLISTSIZE, errbuf, "FLAMES-PRIMITIVE [GETGROUPMEMBERS (checking arglist size)]", 0);
      return;
    }
    if (presult <= -1000) {
      EliError(st, ELI_ERR_BAD_ARGS, errbuf, "FLAMES-PRIMITIVE [GETGROUPMEMBERS (evaluating or type checking arguments)]", 0);
      return;
    }
    if (presult < 0) return;
    
    groupname = EliStr_GetString(EliSexp_GetStr(sexpBufV[0]));
    if (presult == 2) groupcell = EliStr_GetString(EliSexp_GetStr(sexpBufV[1]));
    else groupcell = ThisDomain;

    if ((aq_code=aq_GetGroupMembers(groupname, groupcell, &group_members))==0) {
      NEW_SEXP(new_node);	/* success */
      EliSexp_SetInt(st, new_node, (long) 0L); /* sucess code */
      CONS(return_list, new_node);

      saved_head = group_members;
      while(*group_members) {
	char *next;

	if ((next = strchr(group_members, '\n')) != NULL) {
	  *next = '\0';		/* more elements follow */
	  NEW_SEXP(new_node);
	  EliSexp_SetStr(st, new_node, EliStringTable_FindOrMake(st, group_members));
	  CONS(return_list, new_node);
	  group_members = next + 1;
	} else {
	  NEW_SEXP(new_node);	/* last element */
	  EliSexp_SetStr(st, new_node, EliStringTable_FindOrMake(st, group_members));
	  CONS(return_list, new_node);
	  *group_members = '\0';
	}
      }
      free(saved_head);
    } else {
      NEW_SEXP(new_node);	/* error */
      EliSexp_SetInt(st, new_node, (long) aq_code);
      CONS(return_list, new_node);
      NEW_SEXP(new_node);
      EliSexp_SetStr(st, new_node, EliStringTable_FindOrMake(st, aq_GetLastErrorMessage()));
      CONS(return_list, new_node);
    }
    EliSexp_SetCons(st, resbuf, return_list);
    return;
}

void FLAMES_Prim_UserRightsToDir(st, arglist, resbuf)
EliState_t *st;
EliCons_t *arglist;
EliSexp_t *resbuf;
{
    EliSexp_t *sexpBufV[3], *errbuf, *new_node;
    EliCons_t *return_list = NULL;
    eliDataTypes_t ThreeStrings[3];
    int evalV[3], presult;
    char *directory, *username, *usercell;
    long int aq_rights;
      
    ThreeStrings[0] = e_data_string;
    ThreeStrings[1] = e_data_string;
    ThreeStrings[2] = e_data_string;
    evalV[0] = evalV[1] = evalV[2] = TRUE;
    presult = EliProcessList(st, arglist, 2,  3, sexpBufV, &errbuf, ThreeStrings, evalV);
    if ((presult == -1) || (presult == -2)) {
      EliError(st, ELI_ERR_ARGLISTSIZE, errbuf, "FLAMES-PRIMITIVE [USERRIGHTSTODIR (checking arglist size)]", 0);
      return;
    }
    if (presult <= -1000) {
      EliError(st, ELI_ERR_BAD_ARGS, errbuf, "FLAMES-PRIMITIVE [USERRIGHTSTODIR (evaluating or type checking arguments)]", 0);
      return;
    }
    if (presult < 0) return;
    
    directory = EliStr_GetString(EliSexp_GetStr(sexpBufV[0]));
    username =  EliStr_GetString(EliSexp_GetStr(sexpBufV[1]));
    if (presult == 3) usercell = EliStr_GetString(EliSexp_GetStr(sexpBufV[2]));
    else usercell = ThisDomain;

    if ((aq_rights=aq_UserRightsToDir(username, usercell, directory))>=0) {
      EliSexp_SetInt(st, resbuf, aq_rights); /* success */
    } else {
      NEW_SEXP(new_node);	/* error */
      EliSexp_SetInt(st, new_node, aq_rights);
      CONS(return_list, new_node);
      NEW_SEXP(new_node);
      EliSexp_SetStr(st, new_node, EliStringTable_FindOrMake(st, aq_GetLastErrorMessage()));
      CONS(return_list, new_node);
      EliSexp_SetCons(st, resbuf, return_list);
    }
    return;
}

void FLAMES_Prim_UserAnyRightToDir(st, arglist, resbuf)
EliState_t *st;
EliCons_t *arglist;
EliSexp_t *resbuf;
{
    EliSexp_t *sexpBufV[4], *errbuf, *new_node;
    EliCons_t *return_list = NULL;
    eliDataTypes_t StringIntStringString[4];
    int evalV[4], presult;
    char *directory, *username, *usercell;
    long int asked_rights, aq_result;
      
    StringIntStringString[0] = e_data_string;
    StringIntStringString[1] = e_data_integer;
    StringIntStringString[2] = e_data_string;
    StringIntStringString[3] = e_data_string;
    evalV[0] = evalV[1] = evalV[2] = evalV[3] = TRUE;
    presult = EliProcessList(st, arglist, 3,  4, sexpBufV, &errbuf, StringIntStringString, evalV);
    if ((presult == -1) || (presult == -2)) {
      EliError(st, ELI_ERR_ARGLISTSIZE, errbuf, "FLAMES-PRIMITIVE [USERANYRIGHTTODIR (checking arglist size)]", 0);
      return;
    }
    if (presult <= -1000) {
      EliError(st, ELI_ERR_BAD_ARGS, errbuf, "FLAMES-PRIMITIVE [USERANYRIGHTTODIR (evaluating or type checking arguments)]", 0);
      return;
    }
    if (presult < 0) return;
    
    directory = EliStr_GetString(EliSexp_GetStr(sexpBufV[0]));
    asked_rights = EliSexp_GetInt(sexpBufV[1]);
    username =  EliStr_GetString(EliSexp_GetStr(sexpBufV[2]));
    if (presult == 4) usercell = EliStr_GetString(EliSexp_GetStr(sexpBufV[3]));
    else usercell = ThisDomain;

    if ((aq_result=aq_CheckUserAnyRightToDir(username, usercell, directory, asked_rights))>=0) {
      if (aq_result == 0) {
	EliSexp_SetSym(st, resbuf, EliNilSym(st));
      } else {
	EliSexp_SetSym(st, resbuf, EliTSym(st));
      }
    } else {
      NEW_SEXP(new_node);	/* error */
      EliSexp_SetInt(st, new_node, aq_result);
      CONS(return_list, new_node);
      NEW_SEXP(new_node);
      EliSexp_SetStr(st, new_node, EliStringTable_FindOrMake(st, aq_GetLastErrorMessage()));
      CONS(return_list, new_node);
      EliSexp_SetCons(st, resbuf, return_list);
    }
    return;
}
#endif /* AFS30_ENV */

void            FLAMES_Prim_ValidateAddr(st, arglist, resbuf)
EliState_t     *st;
EliCons_t      *arglist;
EliSexp_t      *resbuf;
{
    EliSexp_t *new_node;
    EliCons_t *return_list = NULL;
    char *argv[1], *parm, *vaddr;
    int ret_code;
    struct passwd *pwent;

    if (FLAMES_TranslateArgs(st, arglist, resbuf, 1, 1, argv, OneString, NULL, "validateaddr") < 0) {
        return;                        /* errors were flagged */
    }
    parm = argv[0];
    if ((pwent = getpwuid(geteuid())) == NULL) {
      EliError(st, ELI_ERR_SYSERROR, NULL, "FLAMES-PRIMITIVE [VALIDATEADDR (Couldn't get user's pw entry)]", 0);
      return;			/* error getting pwent */
    }
    fwdvalid_SetTildeUser(pwent->pw_name);
    if ((ret_code = ValidateFwdAddr(parm,&vaddr)) != 0) {
      NEW_SEXP(new_node);	/* error condition */
      EliSexp_SetInt(st, new_node, (long) ret_code);
      CONS(return_list, new_node);
      NEW_SEXP(new_node);
      EliSexp_SetStr(st, new_node, EliStringTable_FindOrMake(st, fwdvalid_msgbuf));
      CONS(return_list, new_node);
    } else {
      NEW_SEXP(new_node);	/* success */
      EliSexp_SetInt(st, new_node, (long) 0L); /* sucess code */
      CONS(return_list, new_node);
      NEW_SEXP(new_node);
      EliSexp_SetStr(st, new_node, EliStringTable_FindOrMake(st, vaddr));
      CONS(return_list, new_node);
      free(vaddr);
    }
    EliSexp_SetCons(st, resbuf, return_list);
    return;
}

#undef NEW_SEXP
#undef CONS

static struct {
    char           *name;
    void            (*proc) ();
}               inittable[] = {

    {
        "APPENDMSGTODIR", FLAMES_Prim_AppendMsgToDir
    },
    {
        "FINALAPPENDMSGTODIR", FLAMES_Prim_FinalAppendMsgToDir
    },
    {
        "FINDFOLDER", FLAMES_Prim_FindFolder
    },
    {
        "ADDHEADER", FLAMES_Prim_AddHeader
    },
    {
        "DELETEHEADER", FLAMES_Prim_DeleteHeader
    },
    {
        "REJECTMESSAGE", FLAMES_Prim_RejectMessage
    },
    {
        "RESENDMESSAGE", FLAMES_Prim_ResendMessage
    },
    {
        "TRACELESSRESENDMESSAGE", FLAMES_Prim_TracelessResendMessage
    },
    {
        "SETCAPTION", FLAMES_Prim_SetCaption
    },
    {
        "GETCAPTION", FLAMES_Prim_GetCaption
    },
    {
        "GETPARTIALBODY", FLAMES_Prim_GetPartialBody
    },
    {
        "BODYLENGTH", FLAMES_Prim_BodyLength
    },
    {
        "GETAUTHSENDER", FLAMES_Prim_GetAuthSender
    },
    {
        "UNFORMATMESSAGE", FLAMES_Prim_UnformatMessage
    },
    {
        "GETHEADERLIST", FLAMES_Prim_GetHeaderList
    },
    {
        "CREATEFOLDER", FLAMES_Prim_CreateFolder
    },
    {
        "CREATEFOLDERFROMMESSAGE", FLAMES_Prim_CreateFolderFromMessage
    },
    {
        "REPLYADDR", FLAMES_Prim_ReplyAddr
    },
    {
        "GETPARAMETER", FLAMES_Prim_GetParameter
    },
    {
        "GETENV", FLAMES_Prim_Getenv
    },
    {
        "FILELENGTH", FLAMES_Prim_FileLength
    },
    {
        "READFILE", FLAMES_Prim_ReadFile
    },
    {
        "WRITEFILE", FLAMES_Prim_WriteFile
    },
#ifdef NOTUSED
    {
        "SYSTEM", FLAMES_Prim_System
    },
#endif                                 /* NOTUSED */
    {
        "CURRENTMESSAGE", FLAMES_Prim_CurrentMessage
    },
    {
        "GETHEADERCONTENTS", FLAMES_Prim_GetHeaderContents
    },
    {
        "GETAUTHSENDERCELL", FLAMES_Prim_GetAuthSenderCell
    },
    {
        "DROPOFFMESSAGE", FLAMES_Prim_DropoffMessage
    },
    {
        "DROPOFFFILE", FLAMES_Prim_DropoffFile
    },
    {
        "GENID", FLAMES_Prim_GenID
    },
#ifdef AFS30_ENV
    {
        "GETGROUPMEMBERS", FLAMES_Prim_GetGroupMembers
    },
    {
        "USERRIGHTSTODIR", FLAMES_Prim_UserRightsToDir
    },
    {
        "USERANYRIGHTTODIR", FLAMES_Prim_UserAnyRightToDir
    },
#endif /* AFS30_ENV */
    {
        "VALIDATEADDR", FLAMES_Prim_ValidateAddr
    },
    {
        0, 0
    }
};

Flames_Initialize(st)
EliState_t     *st;
{
    int             i;

    eliSetClientLibrary(st, "FLAMESPATH", "flames", AndrewDir("/lib/flames"));
    for (i = 0; inittable[i].name; ++i) {
        if (MyPrimDef(st, inittable[i].name, inittable[i].proc)) {
            return (Flames_EliToAMSErr(st));
        }
    }
    return (0);
}
