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


 

#define keyrec_VERSION 1

#include <view.ih>

enum keyrec_EventType {
    keyrec_Keystroke,
    keyrec_Menu,
    keyrec_MouseHit
};

union keyrec_KeyValue {
    enum view_MouseAction action;
    long val;
    char *unknown;
};

struct keyitem {
    struct keyitem *next;
    enum keyrec_EventType type;
    struct view *view;
    union keyrec_KeyValue parm[3];
};

class keyrec {
methods:
    Clear();
    StartRecording() returns boolean;	/* Returns FALSE if already recording */
    StopRecording() returns boolean;	/* Returns FALSE if not recording */
    Recording() returns boolean;	/* Returns TRUE if currently recording */
    Copy() returns struct thisobject *;
    RecordEvent(enum keyrec_EventType type, struct view *view, union keyrec_KeyValue value1, union keyrec_KeyValue value2, union keyrec_KeyValue value3);
    StartPlaying() returns boolean;
    NextKey(enum keyrec_EventType *type, struct view **view, union keyrec_KeyValue *value1, union keyrec_KeyValue *value2, union keyrec_KeyValue *value3) returns boolean;
    StopPlaying();
classprocedures:
    InitializeObject(struct keyrec *self) returns boolean;
    FinalizeObject(struct keyrec *self);
data:
    boolean recording;
    struct keyitem *head;
    struct keyitem *tail;
    boolean playing;
    struct keyitem *current;
};
