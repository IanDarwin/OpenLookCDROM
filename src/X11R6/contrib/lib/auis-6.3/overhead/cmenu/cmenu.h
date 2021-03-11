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


#include <cmerror.h>

/* Could have just as easily used TRUE and FALSE for these. */
#define cmenu_Inactive                  0
#define cmenu_Active                    1

#define cmenu_DisallowDuplicates        8
#define cmenu_CreatePane                2
#define cmenu_DeleteEmptyPanes          4

#define cmenu_NoBackground              0
#define cmenu_BackgroundPixel           1
#define cmenu_BackgroundPixmap          2
#define cmenu_NoSaveUnder               3

#ifdef _STDC_
extern struct cmenu *cmenu_Create(Display *display, Window parent,
                         char *defaultEnvironment, void (*freeFunction)());
extern cmenu_Destroy(struct cmenu *menu);
extern int cmenu_AddPane(struct cmenu *menu, char *paneTitle,
                         int panePriority, int flags);
extern int cmenu_DeletePane(struct cmenu *menu, char *paneTitle, int priority);
extern int cmenu_AddSelection(struct cmenu *menu, char *paneTitle,
               int panePriority, char *selectionLabel, int selectionPriority,
               long selectionData, int flags, char *keys);
extern int cmenu_DeleteSelection(struct cmenu *menu, char *paneTitle,
               int panePriority, char *slectionLabel, int selectionPriority,
               int flags);
extern int cmenu_Activate(struct cmenu *menu, XButtonEvent *menuEvent,
               long *data, int backgroundType, long backgroundValue);
#else /* _STDC_ */
extern struct cmenu *cmenu_Create();
extern int cmenu_AddPane();
extern int cmenu_AddSelection();
extern int cmenu_DeletePane();
extern int cmenu_DeleteSelection();
#endif /* _STDC_ */
