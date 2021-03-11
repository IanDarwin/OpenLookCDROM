/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
*Permission to use, copy, modify, and distribute this software and its 
*documentation for any purpose is hereby granted without fee, 
*provided that the above copyright notice appear in all copies and that 
*both that copyright notice, this permission notice, and the following 
*disclaimer appear in supporting documentation, and that the names of 
*IBM, Carnegie Mellon University, and other copyright holders, not be 
*used in advertising or publicity pertaining to distribution of the software 
*without specific, written prior permission.
*
*IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
*DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
*ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
*SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
*BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
*DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
*WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
*ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
*OF THIS SOFTWARE.
* $
*/


 

/* complete.H
 * Class header for message line completion support module.
 */

#include <message.ih>

struct result {
    char *partial;
    int partialLen; /* Length in chars not including terminating NUL */
    char *best;
    int max;
    int bestLen; /* Length in chars not including terminating NUL */
    enum message_CompletionCode code;
};

package completion[complete] {
    classprocedures:
        CompletionWork(char *string, struct result *data);
        GetFilename(struct view *view, char *prompt, char *startPath, char *buffer, long bufsiz, boolean directoryP, boolean mustMatch) returns int;
        FindCommon(char *string1, char *string2) returns long;
        InitializeClass() returns boolean;
        FileHelp(char *partialPath, long dummyData, int (*helpTextFunction) (), long helpTextRock);
        FileComplete(char *pathname, boolean directory, char *buffer, int bufferSize) returns enum message_CompletionCode;
};
