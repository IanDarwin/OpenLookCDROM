/*LIBS: -lutil
*/

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/support/RCS/complete.c,v 2.19 1993/09/22 19:33:49 gk5g Exp $";
#endif

/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

#include <andrewos.h> /* sys/types.h */
#include <class.h>
#include <filetype.ih>
#include <keystate.ih>
#include <keymap.ih>
#include <im.ih>
#include <environ.ih>
#include <message.ih>
#include <cursor.ih>
#include <complete.eh>

#include <sys/param.h>
#include <sys/stat.h>

static boolean useCurrentWorkingDirectory = FALSE;
static struct cursor *waitCursor;

#define min(x, y) (((x) < (y)) ? (x) : (y))

static long completion__FindCommon(classID, string1, string2)
    struct classheader *classID;
    char *string1, *string2;
{
    long i = 0;

    while (*string1++ == *string2++ && (*(string1 - 1) != '\0'))
        i++;
    return i;
}

static void completion__CompletionWork(classID, string, data)
    struct classheader *classID;
    char *string;
    struct result *data;
{

    int partialCommon, nameLen;

    partialCommon = completion_FindCommon(string, data->partial);
    if (partialCommon == data->partialLen) { /* Possible to extend complete. */
        nameLen = strlen(string);
        if (partialCommon > data->bestLen) { /* This is a completion */
            data->bestLen = min(data->max, nameLen);
            strncpy(data->best, string, data->bestLen + 1); /* Safe since we left room below... */
            data->code = message_Complete;
        }
        else { /* Merge posibilities which have partial as a common substr. */

            int bestCommon = completion_FindCommon(string, data->best);

            if (bestCommon < data->bestLen) {
                data->bestLen = bestCommon;
                data->best[data->bestLen] = '\0';
                if (bestCommon == nameLen)
                    if (data->code != message_Invalid)
                        data->code = message_CompleteValid;
                    else
                        data->code = message_Complete;
                else
                    data->code = message_Valid;
            }
            else if ((bestCommon == data->bestLen) && ((bestCommon == nameLen) || (data->code == message_Complete)))
                    data->code = message_CompleteValid;
        }
    }
    else {
        if (partialCommon > data->bestLen) { /* Try to getter a longer initial substr. */
            data->bestLen = min(data->max, partialCommon);
            strncpy(data->best, string, data->bestLen);
            data->best[data->bestLen] = '\0'; /* Safe since we left room below... */
        }
        if (partialCommon >= data->bestLen)
            data->code = (partialCommon == strlen(string)) ? message_Complete : message_Invalid;
    }
}

static void FileHelp(partialPath, dummyData, helpTextFunction, helpTextRock)
    char *partialPath;
    long dummyData; /* Just along for the ride. */
    int (*helpTextFunction)();
    long helpTextRock;
{

    int namelen;
    char *slash, dirbuf[MAXPATHLEN], namebuf[MAXNAMLEN];
    DIR *thisDir;
    DIRENT_TYPE *dirEntry;

#ifdef AFS_ENV
    boolean inVICE = FALSE;
#endif /* AFS_ENV */

    struct stat statBuf;

    filetype_CanonicalizeFilename(dirbuf, partialPath, sizeof(dirbuf));
    slash = rindex(dirbuf, '/');
    if (slash == NULL) {
        im_GetDirectory(dirbuf);
        strcat(dirbuf, "/");
        *namebuf = '\0';
    }
    else {
        strcpy(namebuf, slash + 1); /* Skip '/'. */
        slash[1] = '\0';
    }
    namelen = strlen(namebuf);

    im_SetProcessCursor(waitCursor);

    if ((thisDir = opendir(dirbuf)) == NULL) { /* Should try to back up to the first completed directory. */
        (*helpTextFunction)(helpTextRock, message_HelpGenericItem, "Couldn't access directory \"", NULL);
        (*helpTextFunction)(helpTextRock, message_HelpGenericItem, dirbuf, NULL);
        (*helpTextFunction)(helpTextRock, message_HelpGenericItem, "\"\n", NULL);
        im_SetProcessCursor(NULL);
        return;
    }


#ifdef AFS_ENV /* Enable the wonderous VICE hack... */
#if 0
#define VICEMAGICGID 32767

    if ((stat(dirbuf, &statBuf) >= 0) && (statBuf.st_gid == VICEMAGICGID))
        inVICE = TRUE;
#else /* 0 */
    inVICE = IsOnVice(thisDir->dd_fd);
#endif /* 0 */
#endif /* AFS_ENV  */

    while ((dirEntry = readdir(thisDir)) != NULL) {
        if (completion_FindCommon(namebuf, dirEntry->d_name) == namelen) {

            boolean isDirectory = FALSE;
            char fullName[MAXPATHLEN];

#ifdef AFS_ENV
            if (inVICE) {
                    if ((dirEntry->d_ino % 2) == 1)
                        isDirectory = TRUE;
            }
            else
#endif /* AFS_ENV */
            {
                strcpy(fullName, dirbuf); /* dir is guaranteed to end in a /. */
                strcat(fullName, dirEntry->d_name);
                stat(fullName, &statBuf);
                if ((statBuf.st_mode & S_IFMT) == S_IFDIR)
                    isDirectory = TRUE;
            }
            if (*partialPath == '\0') {
                strcpy(fullName, dirbuf); /* dir is guaranteed to end in a /. */
                strcat(fullName, dirEntry->d_name);
		strcat(fullName, "/");
                (*helpTextFunction)(helpTextRock, message_HelpListItem, fullName, NULL);
            }
            else {

                char *item;

                if (isDirectory) {
                    strcpy(fullName, dirEntry->d_name);
		    strcat(fullName,"/");
                    item = fullName;
                }
                else
                    item = dirEntry->d_name;
                (*helpTextFunction)(helpTextRock, message_HelpListItem, item, NULL);
            }
        }
    }
    closedir(thisDir);

    im_SetProcessCursor(NULL);
}

static void completion__FileHelp(classID, partialPath, dummyData, helpTextFunction, helpTextRock)
    struct classheader *classID;
    char *partialPath;
    long dummyData; /* Just along for the ride. */
    int (*helpTextFunction)();
    long helpTextRock;
{
    FileHelp(partialPath, dummyData, helpTextFunction, helpTextRock);
}

static enum message_CompletionCode FileComplete(pathname, directory, buffer, bufferSize)
    char *pathname;
    long directory;
    char *buffer;
    int bufferSize;
{

    int len;
    char *slash, *dir, pathbuf[MAXPATHLEN];
    struct result result;
    char textBuffer[256];
    DIR *thisDir;
    DIRENT_TYPE *dirEntry;
    struct stat statBuf;
    boolean isDirectory;

#ifdef AFS_ENV
    boolean inVICE = FALSE;
#endif /* AFS_ENV */

    im_SetProcessCursor(waitCursor);

    filetype_CanonicalizeFilename(buffer, pathname, bufferSize);
    if (directory && (buffer[len = (strlen(buffer) - 1)] == '/'))
        buffer[len] = '\0';
    slash = rindex(buffer, '/');
    if (slash == NULL) {
        dir = pathbuf;
        im_GetDirectory(dir);
        strcat(dir, "/");
        result.partial = buffer;
    }
    else {
        if (slash[1] == '\0') { /* Special case to handle expanding directory only paths. */

            int returnValue;

            if ((stat(buffer, &statBuf) >= 0) && ((statBuf.st_mode & S_IFMT) == S_IFDIR))
                returnValue = (int)message_CompleteValid;
            else
                returnValue = (int) message_Invalid;
            im_SetProcessCursor(NULL);
            return((enum message_CompletionCode)returnValue);
        }
        strcpy(pathbuf, slash + 1); /* Skip '/'. */
        slash[1] = '\0';
        dir = buffer;
        result.partial = pathbuf;
    }

    if ((thisDir = opendir(dir)) == NULL) { /* Should try to back up to the first completed directory. */
        *buffer = '\0';
        im_SetProcessCursor(NULL);
        return message_Invalid;
    }

#ifdef AFS_ENV /* Enable the wonderous VICE hack... */
#if 0
#define VICEMAGICGID 32767

    if ((stat(dir, &statBuf) >= 0) && (statBuf.st_gid == VICEMAGICGID))
        inVICE = TRUE;
#else /* 0 */
    inVICE = IsOnVice(thisDir->dd_fd);
#endif /* 0 */
#endif /* AFS_ENV  */

    *textBuffer = '\0';
    result.partialLen = strlen(result.partial);
    result.bestLen = 0;
    result.code = message_Invalid;
    result.best = textBuffer;
    result.max = sizeof(textBuffer) - 1; /* Leave extra room for the NUL. */
    result.best[result.max] = '\0';

    while ((dirEntry = readdir(thisDir)) != NULL) {
        if (directory) { /* Total hack to get directory completion to work efficiently. */
            if (completion_FindCommon(dirEntry->d_name, result.partial) == 0)
                continue;
#ifdef AFS_ENV
            if (inVICE && (dirEntry->d_ino % 2) == 0)
#endif /* AFS_ENV */
            {

                char fullName[MAXPATHLEN];


                strcpy(fullName, dir);
                strcat(fullName, dirEntry->d_name);
                if ((stat(fullName, &statBuf) < 0) || !((statBuf.st_mode & S_IFMT) == S_IFDIR))
                    continue;
            }
        }
        completion_CompletionWork(dirEntry->d_name, &result);
    }

    closedir(thisDir);

/* Really ought to check for buffer overflow here. */
    strncpy(buffer, dir, bufferSize);
    strcat(buffer, result.best);
    isDirectory = (stat(buffer, &statBuf) >= 0) && ((statBuf.st_mode & S_IFMT) == S_IFDIR);
    if ((result.code == message_Complete) && (buffer[(len = strlen(buffer)) - 1] != '/') && isDirectory) {
        buffer[len] = '/';
        buffer[len + 1] = '\0';
        im_SetProcessCursor(NULL);
        result.code = message_CompleteValid;
    }
    else if ((result.code == message_CompleteValid) && isDirectory &&
             !directory)
        result.code = message_Valid;
    im_SetProcessCursor(NULL);
    return result.code;
}

static enum message_CompletionCode completion__FileComplete(classID, pathname, directory, buffer, bufferSize)
    struct classheader *classID;
    char *pathname;
    boolean directory;
    char *buffer;
    int bufferSize;
{
    return FileComplete(pathname, (long) directory, buffer, bufferSize);
}

struct fileRock {
    struct view *view;
    long messageLen;
    struct keystate *keystate;
};

static enum keymap_Types FileHack(rock, key, entry, rockP)
    struct fileRock *rock;
    long key;
    struct basicobject **entry;
    long *rockP;
{
    if (key == '/' || key == '~')
        message_DeleteCharacters(rock->view, 0, rock->messageLen);
    keystate_SetOverride(rock->keystate, NULL, 0);
    return keymap_Empty;
}

static int completion__GetFilename(classID, view, prompt, startPath, buffer, bufsiz, directoryP, mustMatch)
    struct classheader *classID;
    struct view *view;
    char *prompt;
    char *startPath;
    char *buffer;
    long bufsiz;
    boolean directoryP;
    boolean mustMatch;
{

    struct fileRock fileRock;
    char *initialString;
    int code;
    int len;

    if (view == NULL) {
        view = (struct view *) im_GetLastUsed();
        if (view == NULL)
            return -1;
    }

    fileRock.view = view;
    fileRock.keystate = keystate_New();
    keystate_SetOverride(fileRock.keystate, FileHack, (long) &fileRock);

    if (startPath == NULL)
        startPath = "";

/* This code makes UseCurrentWorkingDirectory behave reasonably for Read File.
 * Basically, it says if the user has chosen to have all paths default to the
 * current working directory and the initial string is a directory not a file do
 *  not use a default. Otherwise it is a file, in which case we want it to
 * default to that file, or UseCurrentWorkingDirectory is FALSE in which case we
 *  want to behave as we logically should have in the first place.
 * UseCurrentWorkingDirectory is a pain in the neck..
 */
    if (!useCurrentWorkingDirectory || ((len = strlen(startPath)) != 0 && startPath[len - 1] != '/'))
        initialString = startPath;
    else
        initialString = "";

    fileRock.messageLen = strlen(initialString);

    if ((code = message_AskForStringCompleted(view, 40, prompt,
         initialString, buffer, bufsiz, fileRock.keystate, (procedure) FileComplete,
         (procedure) FileHelp, (long) directoryP, mustMatch ? message_MustMatch : 0)) != -1)
        filetype_CanonicalizeFilename(buffer, buffer, bufsiz);
    keystate_Destroy(fileRock.keystate);
    return code;
}

boolean completion__InitializeClass(classID)
    struct classheader *classID;
{

    waitCursor = cursor_Create(NULL);
    cursor_SetStandard(waitCursor, Cursor_Wait);

/* For Sherri Menees and David Nichols... */
    useCurrentWorkingDirectory = environ_GetProfileSwitch("UseCurrentWorkingDirectory", FALSE);

    return TRUE;
}
