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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/overhead/cmenu/RCS/cmmanip.c,v 2.11 1993/08/27 19:28:51 gk5g Exp $";
#endif

#include <andrewos.h>
#include <X11/Xlib.h>
#include <cmintern.h>
#include <scache.h>

/* Need to handle movement of worm item in this code... */

/* Should be in the C library damn it. */
static char *CopyString(string)
    char *string;
{

    char *tempString;

    if (string == NULL)  {
	tempString = (char *) malloc(1);
	*tempString = '\0';
    }
    else  {
	tempString = (char *) malloc(strlen(string) + 1);
	strcpy(tempString, string);
    }
    return(tempString);
}

struct pane *PaneNumToPtr(menu, paneNum)
    struct cmenu *menu;
    int paneNum;
{

    struct pane *panePtr;

    if (paneNum < 0)
        return NULL;

    for (panePtr = menu->panes; panePtr != NULL && paneNum-- > 0; panePtr = panePtr->next)
        ;

    return panePtr;
}

int PanePtrToNum(menu, panePtr)
    struct cmenu *menu;
    struct pane *panePtr;
{

    int paneNum = 0;
    struct pane *thisPane;

    for (thisPane = menu->panes; thisPane != NULL && thisPane != panePtr; thisPane = thisPane->next)
        ++paneNum;

    if (thisPane == NULL)
        return -1;
    else
        return paneNum;
}

struct selection *SelectionNumToPtr(menu, panePtr, selectionNum)
    struct cmenu *menu;
    struct pane *panePtr;
    int selectionNum;
{

    struct selection *selectionPtr;

    if (selectionNum < 0)
        return NULL;

    for (selectionPtr = panePtr->selections; selectionPtr != NULL && selectionNum-- > 0; selectionPtr = selectionPtr->next)
        ;

    return selectionPtr;
}

int SelectionPtrToNum(menu, panePtr, selectionPtr)
    struct cmenu *menu;
    struct pane *panePtr;
    struct selection *selectionPtr;
{

    int selectionNum = 0;
    struct selection *thisSelection;

    for (thisSelection = panePtr->selections; thisSelection != NULL && thisSelection != selectionPtr; thisSelection = thisSelection->next)
        ++selectionNum;

    if (thisSelection == NULL)
        return -1;
    else
        return selectionNum;
}

static int GetPane(menu, paneTitle, panePriority, thisPaneRet, lastPaneRet)
    struct cmenu *menu;
    char *paneTitle;
    int panePriority;
    struct pane **thisPaneRet;
    struct pane **lastPaneRet;
{

    struct pane *thisPane;
    struct pane *lastPane = NULL;

    if (paneTitle != NULL)
        for (thisPane = menu->panes; thisPane != NULL && strcmp(thisPane->label, paneTitle) != 0; thisPane = thisPane->next)
            lastPane = thisPane;
    else
        for (thisPane = menu->panes; thisPane != NULL && thisPane->priority != panePriority; thisPane = thisPane->next)
            lastPane = thisPane;

    if (thisPane == NULL)
        return -1;

    if (thisPaneRet != NULL)
        *thisPaneRet = thisPane;

    if (lastPaneRet != NULL)
        *lastPaneRet = lastPane;

    return 0;
}

int cmenu_AddPane(menu, paneTitle, panePriority, flags)
    struct cmenu *menu;
    char *paneTitle;
    int panePriority;
    int flags;
{

    struct pane *newPane;
    struct pane *thisPane;
    struct pane *lastPane = NULL;

    if ((flags & cmenu_DisallowDuplicates) && GetPane(menu, paneTitle, panePriority, &thisPane, NULL) >= 0)
        cmenu_DeletePane(menu, paneTitle, panePriority);

    if ((newPane = (struct pane *) malloc(sizeof(struct pane))) == NULL)
        return -1;

    newPane->label = scache_Hold(paneTitle);
    newPane->labelWidth = XTextWidth(menu->gMenuData->titleFont, newPane->label, strlen(newPane->label));
    newPane->maxSelectionWidth = 0;
    newPane->maxKeysWidth = 0;
    newPane->selections = NULL;
    newPane->numberOfSelections = 0;
    newPane->priority = panePriority;
    newPane->active = TRUE;

    for (thisPane = menu->panes; thisPane != NULL && ((panePriority == -1) || thisPane->priority <= panePriority); thisPane = thisPane->next)
        lastPane = thisPane;

    if (panePriority == -1)
        if (lastPane != NULL)
            panePriority = lastPane->priority + 1;
        else
            panePriority = 0;

    newPane->next = thisPane;
    if (lastPane != NULL)
        lastPane->next = newPane;
    else
        menu->panes = newPane;

    ++menu->numberOfPanes;

    return 0;
}

static void FreeSelections(menu, pane)
    struct cmenu *menu;
    struct pane *pane;
{

    struct selection *thisSelection;
    struct selection *nextSelection;

    for (thisSelection = pane->selections; thisSelection != NULL; thisSelection = nextSelection) {
        nextSelection = thisSelection->next;
        (*menu->freeFunction)(thisSelection->data);
	scache_Free(thisSelection->label);
	if(thisSelection->keys) scache_Free(thisSelection->keys);
        free(thisSelection);
    }
}

void cmenu_Destroy(menu)
    struct cmenu *menu;
{

    struct pane *thisPane;
    struct pane *nextPane;

    for (thisPane = menu->panes; thisPane != NULL; thisPane = nextPane) {
        nextPane = thisPane->next;
        FreeSelections(menu, thisPane);
	if(thisPane->label) scache_Free(thisPane->label);
        free(thisPane);
    }

    free(menu);
}

int cmenu_DeletePane(menu, paneTitle, panePriority)
    struct cmenu *menu;
    char *paneTitle;
    int panePriority;
{

    struct pane *thisPane;
    struct pane *lastPane;

    if (GetPane(menu, paneTitle, panePriority, &thisPane, &lastPane) < 0)
        return -1;

    if (lastPane != NULL)
        lastPane->next = thisPane->next;
    else
        menu->panes = thisPane->next;

    FreeSelections(menu, thisPane);
    if(thisPane->label) scache_Free(thisPane->label);
    free(thisPane);

    --menu->numberOfPanes;

    return 0;
}

int cmenu_AddSelection(menu, paneTitle, panePriority, selectionLabel, priority, selectionData, flags, keys)
    struct cmenu *menu;
    char *paneTitle;
    int panePriority;
    char *selectionLabel;
    int priority;
    long selectionData;
    int flags;
    char *keys;
{

    struct pane *thisPane;
    struct selection *newSelection = NULL;
    struct selection *thisSelection;
    struct selection *lastSelection = NULL;
    struct selection *tempLastSelection = NULL;
    struct selection *afterSelection = NULL;
    int setLastSelection = FALSE;

    if (GetPane(menu, paneTitle, panePriority, &thisPane, NULL) < 0)
        if (flags & cmenu_CreatePane) {
            if (cmenu_AddPane(menu, paneTitle, panePriority, flags) < 0 || GetPane(menu, paneTitle, panePriority, &thisPane, NULL) < 0)
                return -1;
        }
        else
            return -1;

/* This completely arcane code prevents duplicates... Needs to be fixed BADLY. */
    for (thisSelection = thisPane->selections; thisSelection != NULL; thisSelection = thisSelection->next)
        if ((flags & cmenu_DisallowDuplicates) && strcmp(thisSelection->label, selectionLabel) == 0) {
            if (thisPane->numberOfSelections > 1 && (tempLastSelection == NULL || thisSelection->groupPriority != tempLastSelection->groupPriority) && (thisSelection->next == NULL || thisSelection->groupPriority != thisSelection->next->groupPriority))
                --thisPane->numberOfSelections;
            if (tempLastSelection == NULL)
                thisPane->selections = thisSelection->next;
            else
                tempLastSelection->next = thisSelection->next;
            if (newSelection) {
                (*menu->freeFunction)(newSelection->data);
                scache_Free(newSelection->label);
                if(newSelection->keys) scache_Free(newSelection->keys);
                free(newSelection);
            }
            newSelection = thisSelection;
        }
        else {
            if (!setLastSelection && thisSelection->priority > priority) {
                setLastSelection = TRUE;
                lastSelection = tempLastSelection;
                afterSelection = thisSelection;
            }
            tempLastSelection = thisSelection;
        }

    if (!setLastSelection)
        lastSelection = tempLastSelection;

    if (newSelection == NULL) {
        if ((newSelection = (struct selection *) malloc(sizeof(struct selection))) == NULL)
            return -1;
    }
    else {
        (*menu->freeFunction)(newSelection->data);
        scache_Free(newSelection->label);
        if(newSelection->keys) scache_Free(newSelection->keys);
    }

    newSelection->label = scache_Hold(selectionLabel);
    newSelection->labelLength = strlen(selectionLabel);
    newSelection->labelWidth = XTextWidth(menu->gMenuData->selectionFont, selectionLabel, newSelection->labelLength);
    newSelection->data = selectionData;
    newSelection->priority = priority;
    newSelection->groupPriority = priority / 10;
    newSelection->active = flags&cmenu_Active;
    if(keys) newSelection->keys = scache_Hold(keys);
    else newSelection->keys = NULL;
    newSelection->keysWidth=newSelection->keys ? XTextWidth(menu->gMenuData->keysFont, keys, strlen(keys)):0;
    if (thisPane->selections != NULL && (lastSelection == NULL || newSelection->groupPriority != lastSelection->groupPriority) && (afterSelection == NULL || newSelection->groupPriority != afterSelection->groupPriority))
        ++thisPane->numberOfSelections;

    newSelection->next = afterSelection;
    if (lastSelection != NULL)
        lastSelection->next = newSelection;
    else {
        thisPane->selections = newSelection;
    }
    
    if (newSelection->labelWidth > thisPane->maxSelectionWidth)
        thisPane->maxSelectionWidth = newSelection->labelWidth;

    if (newSelection->keysWidth > thisPane->maxKeysWidth)
	thisPane->maxKeysWidth = newSelection->keysWidth;
    
    ++thisPane->numberOfSelections;

    return 0;
}

static void RecomputeMaxSelectionWidth(pane)
    struct pane *pane;
{

    struct selection *thisSelection;
    int maximum = 0, kmax=0;

    for (thisSelection = pane->selections; thisSelection != NULL; thisSelection = thisSelection->next) {
        if (thisSelection->labelWidth > maximum)
	    maximum = thisSelection->labelWidth;
	if (thisSelection->keysWidth > kmax)
	    kmax = thisSelection->keysWidth;
    }

   pane->maxSelectionWidth = maximum;
   pane->maxKeysWidth = kmax;
}

int cmenu_DeleteSelection(menu, paneTitle, panePriority, selectionLabel, priority, flags)
    struct cmenu *menu;
    char *paneTitle;
    int panePriority;
    char *selectionLabel;
    int priority;
    int flags;
{

    struct pane *thisPane;
    struct selection *thisSelection;
    struct selection *lastSelection = NULL;

    if (GetPane(menu, paneTitle, panePriority, &thisPane, NULL) < 0)
        return -1;

    if (selectionLabel == NULL)
        for (thisSelection = thisPane->selections; thisSelection != NULL && thisSelection->priority != priority; thisSelection = thisSelection->next)
        lastSelection = thisSelection;
    else
        for (thisSelection = thisPane->selections; thisSelection != NULL && strcmp(thisSelection->label, selectionLabel) != 0; thisSelection = thisSelection->next)
        lastSelection = thisSelection;

    if (thisSelection == NULL)
        return -1;

    if (thisPane->numberOfSelections > 1 && (lastSelection == NULL || thisSelection->groupPriority != lastSelection->groupPriority) && (thisSelection->next == NULL || thisSelection->groupPriority != thisSelection->next->groupPriority))
        --thisPane->numberOfSelections;

    if (lastSelection != NULL)
        lastSelection->next = thisSelection->next;
    else
        thisPane->selections = thisSelection->next;

    (*menu->freeFunction)(thisSelection->data);
    scache_Free(thisSelection->label);
    if(thisSelection->keys) scache_Free(thisSelection->keys);
    
    if (thisSelection->labelWidth == thisPane->maxSelectionWidth)
        RecomputeMaxSelectionWidth(thisPane);
    free(thisSelection);

    if (--thisPane->numberOfSelections == 0 && (flags & cmenu_DeleteEmptyPanes))
        cmenu_DeletePane(menu, paneTitle, panePriority);

    return 0;
}

int cmenu_SetActive(menu, paneTitle, panePriority, priority, active)
    struct cmenu *menu;
    char *paneTitle;
    int panePriority;
    int priority;
    int active;
{

    struct pane *thisPane;
    struct selection *thisSelection;

    if (GetPane(menu, paneTitle, panePriority, &thisPane, NULL) < 0)
        return -1;

    if (thisPane == NULL)
        return -1;

    for (thisSelection = thisPane->selections; thisSelection != NULL && thisSelection->priority != priority; thisSelection = thisSelection->next)
        ;

    if (thisSelection == NULL)
        return -1;

    thisSelection->active = active;

    return 0;
}

int cmenu_GetActive(menu, paneTitle, panePriority, priority)
    struct cmenu *menu;
    char *paneTitle;
    int panePriority;
    int priority;
{

    struct pane *thisPane;
    struct selection *thisSelection;

    if (GetPane(menu, paneTitle, panePriority, &thisPane, NULL) < 0)
        return -1;

    for (thisSelection = thisPane->selections; thisSelection != NULL && thisSelection->priority != priority; thisSelection = thisSelection->next)
        ;

    if (thisSelection == NULL)
        return -1;

    return thisSelection->active;
}

