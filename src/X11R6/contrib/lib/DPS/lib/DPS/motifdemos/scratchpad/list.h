/* list.h
 *
 * (c) Copyright 1990-1994 Adobe Systems Incorporated.
 * All rights reserved.
 * 
 * Permission to use, copy, modify, distribute, and sublicense this software
 * and its documentation for any purpose and without fee is hereby granted,
 * provided that the above copyright notices appear in all copies and that
 * both those copyright notices and this permission notice appear in
 * supporting documentation and that the name of Adobe Systems Incorporated
 * not be used in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.  No trademark license
 * to use the Adobe trademarks is hereby granted.  If the Adobe trademark
 * "Display PostScript"(tm) is used to describe this software, its
 * functionality or for any other purpose, such use shall be limited to a
 * statement that this software works in conjunction with the Display
 * PostScript system.  Proper trademark attribution to reflect Adobe's
 * ownership of the trademark shall be given whenever any such reference to
 * the Display PostScript system is made.
 * 
 * ADOBE MAKES NO REPRESENTATIONS ABOUT THE SUITABILITY OF THE SOFTWARE FOR
 * ANY PURPOSE.  IT IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED WARRANTY.
 * ADOBE DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NON- INFRINGEMENT OF THIRD PARTY RIGHTS.  IN NO EVENT SHALL ADOBE BE LIABLE
 * TO YOU OR ANY OTHER PARTY FOR ANY SPECIAL, INDIRECT, OR CONSEQUENTIAL
 * DAMAGES OR ANY DAMAGES WHATSOEVER WHETHER IN AN ACTION OF CONTRACT,
 * NEGLIGENCE, STRICT LIABILITY OR ANY OTHER ACTION ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.  ADOBE WILL NOT
 * PROVIDE ANY TRAINING OR OTHER SUPPORT FOR THE SOFTWARE.
 * 
 * Adobe, PostScript, and Display PostScript are trademarks of Adobe Systems
 * Incorporated which may be registered in certain jurisdictions
 * 
 * Author:  Adobe Systems Incorporated
 */

/*     ===== Global Defines =====     */

typedef int boolean;

#define TRUE 1
#define FALSE 0
#define SELECTED_NEW 1
#define SELECTED_MISS 0
#define SELECTED_ALREADY -1


/*     ===== Global Structures =====     */

typedef struct bbox_Struct {
	float llx;
	float lly;
	float urx;
	float ury;
} bboxStruct;


/*     ===== Global Functions =====     */

/*    ==== Initialization Functions ====    */
extern void matrixInit();

/*    ==== Conversion Functions ====    */
extern void convertDevicePoint();
/*     int sourcex, sourcey;
 *     float *destx, *desty;
 */

extern void convertUserPoint();
/*     float sourcex, sourcey;
 *     float *destx, *desty;
 */

/*    ==== Expose Procedures ====    */
extern void redrawElements();
/*     int ulx, uly, lrx, lry;
 */


/*    ==== List Manipulation Procedures ====    */

/*   === Adding Elements ===   */
extern boolean addElement();
/*     char character;
 *     char *fontName;
 *     float fontSize;
 *     extern boolean outline;
 *     bboxStruct *fontBBox;
 *     bboxStruct *blitBBox;
 *     float *xpos, *ypos;
 *     int *devx, *devy;
 */

/*   === Selecting Elements ===   */
extern int constructiveSelectElements();
/*     int ulx, uly, lrx, lry;
 *     bboxStruct *blitBBox;
 */

extern boolean destructiveSelectElements();
/*     int ulx, uly, lrx, lry;
 *     bboxStruct *blitBBox;
 */

extern boolean selectAll();
/*     bboxStruct *blitBBox;
 */

extern boolean unselectAll();
/*     bboxStruct *blitBBox;
 */

/*   === Deleting Elements ===   */
extern int deleteLastElement();
/*     float *returnX, *returnY;
 *     bboxStruct *blitBBox;
 */

extern boolean deleteSelected();
/*     bboxStruct *blitBBox;
 */

extern boolean deleteAll();
/*     bboxStruct *blitBBox;
 */


/*    ==== Selection Manipulation Functions ====    */
extern boolean moveSelectedByPixels();
/*     int xoff, yoff;
 *     bboxStruct *blitBBox;
 */

extern boolean resetSelectedAlignment();
/*     int xpos, ypos;
 *     bboxStruct *blitBBox;
 */

/*    ==== Rotate Mode Functions ====    */
extern boolean anchorRotate();
/*     int xanchor, yanchor;
 */

extern boolean beginRotate();
/*     int xclick, yclick;
 */

extern boolean doRotate();
/*     int xmove, ymove;
 *     bboxStruct *blitBBox;
 */

/*    ==== Scale Mode Functions ====    */
extern boolean anchorScale();
/*     int xanchor, yanchor;
 */

extern boolean beginScale();
/*     int xclick, yclick;
 */

extern boolean doScale();
/*     int xmove, ymove;
 *     bboxStruct *blitBBox;
 */

/*    ==== Slant Mode Functions ====    */
extern boolean anchorSlant();
/*     int xanchor, yanchor;
 */

extern boolean beginSlant();
/*     int xclick, yclick;
 */

extern boolean doSlant();
/*     int xmove, ymove;
 *     bboxStruct *blitBBox;
 */

/*     ===== Save and Load Functions =====     */


extern boolean saveNextElement();
/*     char *letter;
 *     char *fontName;
 *     float *fontSize;
 *     boolean *outline;
 *     float *tmat;
 *     float *xpos, *ypos;
 *     float *offset;
 */

extern void loadNextElement();
/*     char letter;
 *     char *fontName;
 *     float fontSize;
 *     boolean outline;
 *     bboxStruct *fontBBox;
 *     float *tmat;
 *     float xpos, ypos;
 *     float offset;
 */

/*     ===== Change Functions =====     */

extern boolean changeSelectedFont();
/*     char *fontName;
 *     float fontSize;
 *     bboxStruct *fontBBox;
 */

extern void getCurrentFontList();
/*     char ***fontList;
 *     int *numFonts;
 */
