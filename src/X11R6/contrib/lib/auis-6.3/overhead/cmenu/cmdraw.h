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


struct drawingState {
    int stackLeft;
    int stackTop;
    int stackHeight;
    int stackWidth;
    int paneWidth;
    int paneHeight;
    int maxSelections;
    int ySelectionOffset;
    int wormLeft;
    int wormRight;
    int wormTop;
    int wormBottom;
    int paneNum;                /* Index of current pane. */
    struct pane *panePtr;          /* Pointer to current pane. */
    int selectionNum;           /* Index of current selection. */
    struct selection *selectionPtr;/* Pointer to current Selection. */
#ifdef ATTEMPTSAVEUNDERS
    int doSaveUnder;            /* Boolean indicating whether or not to do save under. */
    Pixmap saveUnder;           /* Actual saved bits. */
    int saveUnderX;             /* rectangle of saveunder in parent window. */
    int saveUnderY;
    int saveUnderWidth;
    int saveUnderHeight;
#endif /* ATTEMPTSAVEUNDERS */
};

#define cmenu_Behind     1 /* Draw a card behind another card. */
#define cmenu_BeFront    2 /* Draw a card in fron of th top card. */
#define cmenu_OnTop      3 /* Draw this card as the one the user is selecting on. */
#define cmenu_Expose     4 /* Go from Behind to OnTop efficiently. */
#define cmenu_Hide       5 /* Go from OnTop to Behind efficiently. */

#define BOUNDINGBOXSLOPX 40
#define BOUNDINGBOXSLOPY 40

extern struct pane *SetPaneNum();
extern int SetPanePtr();
extern struct selection *SetSelectionNum();
extern int SetSelectionPtr();
extern void InitializeBoundingBoxWindow();
extern void ShrinkBoundingBoxWindow();
extern void CalculatePaneAndSelection();
extern void ShowAPane();
extern void CreateMenuStack();

#define GetPaneNum(state) ((state)->paneNum)
#define GetPanePtr(state) ((state)->panePtr)
#define GetSelectionNum(state) ((state)->selectionNum)
#define GetSelectionPtr(state) ((state)->selectionPtr)
#define SetPanePtrAndNum(state, ptr, num) ((state)->panePtr = (ptr), (state)->paneNum = (num))
#define SetSelectionPtrAndNum(state, ptr, num) ((state)->selectionPtr = (ptr), (state)->selectionNum = (num))
