/* list.c
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

#include <stdio.h>
#include <X11/Xos.h>
#include <X11/Intrinsic.h>
#include "math.h"
#include "list.h"
#include "listwraps.h"
#include "xfunctions.h"

/*     ===== Local Types & Definitions =====     */

#define REDRAW_VAL	5.0
#define SCALE_FACTOR	24.0
#define MIN_SCALE	0.000001
#define SCALE_TOLERANCE 5.0
#define SLANT_TOLERANCE 5.0
#define MIN_MARGIN	1.5
#define PI		3.14159265359


/*     ===== Macro Definitions =====     */

#define max(a, b)	((a)>(b) ? (a) : (b))
#define min(a, b)	((a)<(b) ? (a) : (b))
#define absval(a)	((a)>0.0 ? (a) : -1.0*(a))
#define sign(a)		((a)<0.0 ? -1.0 : 1.0)
#define copymatrix(smat, dmat)	\
  {				\
  (dmat)[0] = (smat)[0];	\
  (dmat)[1] = (smat)[1];	\
  (dmat)[2] = (smat)[2];	\
  (dmat)[3] = (smat)[3];	\
  (dmat)[4] = (smat)[4];	\
  (dmat)[5] = (smat)[5];	\
  }

typedef struct parallelogram_Struct {
	float llx;
	float lly;
	float urx;
	float ury;
	float ulx;
	float uly;
	float lrx;
	float lry;
	bboxStruct paraBBox;
} parallelogramStruct;

typedef struct selectStruct {
	struct charStruct *which;
	struct selectStruct *prev;
	struct selectStruct *next;
} *selectPtr;

typedef struct charStruct {
	char letter;
	char fontName[64];
	float fontSize;
	boolean outline;
	float tmat[6];
	float oldmat[6];
	float xpos, ypos;
	float charWidth;
	int spaces;
	float xcnst, ycnst;
	bboxStruct bbox;
	parallelogramStruct hitRgn;
	boolean refresh;
	boolean rectRefresh;
	boolean selected;
	selectPtr selectNode;
	struct charStruct *prev;
	struct charStruct *next;
} *charList;

/*     ===== Local Variables =====     */

static charList beginList = NULL;
static charList endList = NULL;

static selectPtr beginSelect = NULL;
static selectPtr endSelect = NULL;

static float identityMatrix[6] = { 1.0, 0.0, 0.0, 1.0, 0.0, 0.0 };
static float defaultMatrix[6];
static float redrawValue;
static Region *updateRegion = NULL;

static float rotateAnchorX, rotateAnchorY;
static float rotateClickX, rotateClickY;
static float initialAngle;
static float scaleAnchorX, scaleAnchorY;
static float scaleClickX, scaleClickY;
static float slantAnchorX, slantAnchorY;
static float slantClickX, slantClickY;

static char lastFont[64];
static float lastSize = -1.0;
static char printLastFont[64];
static float printLastSize = -1.0;

/*     ===== Local Matrix Operator Functions =====     */

static float maxArray(array, numarray)
     float *array;
     int numarray;
{				
  float maxsofar;
  int inc = numarray-1;	
				
  maxsofar = array[inc];	
  inc--;			
  for(; inc>=0; inc--) {
    if(array[inc] > maxsofar)
      maxsofar = array[inc];
  }
  return maxsofar;
}

static float minArray(array, numarray)
     float *array;
     int numarray;
{				
  float minsofar;
  int inc = numarray-1;	
				
  minsofar = array[inc];	
  inc--;			
  for(; inc>=0; inc--) {
    if(array[inc] < minsofar)
      minsofar = array[inc];
  }
  return minsofar;
}


static void matrixmultiply(concat, matrix, destMatrix)
     float *concat;
     float *matrix;
     float *destMatrix;
{
  destMatrix[0] = (concat[0]*matrix[0]) + (concat[1]*matrix[2]);
  destMatrix[1] = (concat[0]*matrix[1]) + (concat[1]*matrix[3]);
  destMatrix[2] = (concat[2]*matrix[0]) + (concat[3]*matrix[2]);
  destMatrix[3] = (concat[2]*matrix[1]) + (concat[3]*matrix[3]);
  destMatrix[4] = (concat[4]*matrix[0]) + (concat[5]*matrix[2]) + matrix[4];
  destMatrix[5] = (concat[4]*matrix[1]) + (concat[5]*matrix[3]) + matrix[5];
}

static void invertmatrix(smat, dmat)
     float *smat, *dmat;
{
  dmat[0] = 1 / (smat[0] - (smat[1]*smat[2]/smat[3]));
  dmat[1] = 1 / (smat[2] - (smat[0]*smat[3]/smat[1]));
  dmat[2] = 1 / (smat[1] - (smat[0]*smat[3]/smat[2]));
  dmat[3] = 1 / (smat[3] - (smat[1]*smat[2]/smat[0]));
  dmat[4] = -1*smat[4]*dmat[0] - smat[5]*dmat[2];
  dmat[5] = -1*smat[4]*dmat[1] - smat[5]*dmat[3];
}

static void translate(matrix, x, y)
     float *matrix;
     float x, y;
{
  matrix[4] = matrix[0] * x + matrix[2] * y + matrix[4];
  matrix[5] = matrix[1] * x + matrix[3] * y + matrix[5];
}

static void rotate(matrix, radians)
     float *matrix;
     double radians;
{
  float newmatrix[6];

  newmatrix[0] = matrix[0]*(float)cos(radians) + matrix[2]*(float)sin(radians);
  newmatrix[1] = matrix[1]*(float)cos(radians) + matrix[3]*(float)sin(radians);
  newmatrix[2] = matrix[2]*(float)cos(radians) - matrix[0]*(float)sin(radians);
  newmatrix[3] = matrix[3]*(float)cos(radians) - matrix[1]*(float)sin(radians);
  newmatrix[4] = matrix[4];
  newmatrix[5] = matrix[5];
  copymatrix(newmatrix, matrix);
}

static void scale(matrix, scalex, scaley)
     float *matrix;
     float scalex, scaley;
{
  matrix[0] = matrix[0]*scalex;
  matrix[1] = matrix[1]*scalex;
  matrix[2] = matrix[2]*scaley;
  matrix[3] = matrix[3]*scaley;
}

static void slant(matrix, slantx, slanty)
     float *matrix;
     float slantx, slanty;
{
  float a, b, c, d;

  a = matrix[0];
  b = matrix[1];
  c = matrix[2];
  d = matrix[3];

  matrix[0] = a + (c * slanty);
  matrix[1] = b + (d * slanty);
  matrix[2] = (a * slantx) + c;
  matrix[3] = (b * slantx) + d;
}

static void transform(matrix, sourcex, sourcey, destx, desty)
     float *matrix;
     float sourcex, sourcey;
     float *destx, *desty;
{
  *destx = matrix[0]*sourcex + matrix[2]*sourcey + matrix[4];
  *desty = matrix[1]*sourcex + matrix[3]*sourcey + matrix[5];
}

static void itransform(matrix, sourcex, sourcey, destx, desty)
     float *matrix;
     float sourcex, sourcey;
     float *destx, *desty;
{
  float imat[6];

  invertmatrix(matrix, imat);
  *destx = imat[0]*sourcex + imat[2]*sourcey + imat[4];
  *desty = imat[1]*sourcex + imat[3]*sourcey + imat[5];
}

static void idtransform(matrix, sourcex, sourcey, destx, desty)
     float *matrix;
     float sourcex, sourcey;
     float *destx, *desty;
{
  float imat[6];

  invertmatrix(matrix, imat);
  *destx = imat[0]*sourcex + imat[2]*sourcey;
  *desty = imat[1]*sourcex + imat[3]*sourcey;
}

static void initmatrix(matrix)
     float *matrix;
{
  matrix[0] = identityMatrix[0];
  matrix[1] = identityMatrix[1];
  matrix[2] = identityMatrix[2];
  matrix[3] = identityMatrix[3];
  matrix[4] = identityMatrix[4];
  matrix[5] = identityMatrix[5];
}



/*     ===== Bounding Box Helper Functions =====     */

static void copyBBox(destBBox, sourceBBox)
     bboxStruct *destBBox, *sourceBBox;
{
  destBBox->llx = sourceBBox->llx;
  destBBox->lly = sourceBBox->lly;
  destBBox->urx = sourceBBox->urx;
  destBBox->ury = sourceBBox->ury;
}

static void updateRegionBBox(myRgn)
     parallelogramStruct *myRgn;
{
  float values[4];

  values[0] = myRgn->llx;
  values[1] = myRgn->lrx;
  values[2] = myRgn->ulx;
  values[3] = myRgn->urx;
  myRgn->paraBBox.llx = minArray(values, 4);
  myRgn->paraBBox.urx = maxArray(values, 4);

  values[0] = myRgn->lly;
  values[1] = myRgn->lry;
  values[2] = myRgn->uly;
  values[3] = myRgn->ury;
  myRgn->paraBBox.lly = minArray(values, 4);
  myRgn->paraBBox.ury = maxArray(values, 4);
}

static void initBBox(theItem, fontBBox)
     charList theItem;
     bboxStruct *fontBBox;
{
  bboxStruct *destBBox;
  char str[2];
  float margin;

  str[0] = theItem->letter;
  str[1] = '\0';
  destBBox = &(theItem->bbox);

  if((theItem->fontSize == lastSize) && 
     (strcmp(lastFont, theItem->fontName) == 0)) {
    PSWbbox(str, &(destBBox->llx), &(destBBox->lly),
	    &(destBBox->urx), &(destBBox->ury));
  }
  else {
    PSWfontBBox(theItem->fontName, theItem->fontSize, str,
		&(destBBox->llx), &(destBBox->lly),
		&(destBBox->urx), &(destBBox->ury));
    strcpy(lastFont, theItem->fontName);
    lastSize = theItem->fontSize;
  }

  margin = (fontBBox->urx - fontBBox->llx) / (float)SCALE_FACTOR;
  if(margin < (float)MIN_MARGIN)
    margin = (float)MIN_MARGIN;
  destBBox->llx -= margin;
  destBBox->urx += margin;
  destBBox->lly = fontBBox->lly;
  destBBox->ury = fontBBox->ury;
}

static void initRgn(destRgn, sourceBBox, userx, usery)
     parallelogramStruct *destRgn;
     bboxStruct *sourceBBox;
     float userx, usery;
{
  destRgn->llx = sourceBBox->llx + userx;
  destRgn->lly = sourceBBox->lly + usery;
  destRgn->ulx = sourceBBox->llx + userx;
  destRgn->uly = sourceBBox->ury + usery;
  destRgn->lrx = sourceBBox->urx + userx;
  destRgn->lry = sourceBBox->lly + usery;
  destRgn->urx = sourceBBox->urx + userx;
  destRgn->ury = sourceBBox->ury + usery;

  updateRegionBBox(destRgn);
}

static void convertBBoxToDevice(destBBox, sourceBBox)
     bboxStruct *destBBox, *sourceBBox;
{

  transform(defaultMatrix, sourceBBox->llx, sourceBBox->lly,
	    &(destBBox->llx), &(destBBox->lly));
  destBBox->lly += GetCanvasHeight();
  transform(defaultMatrix, sourceBBox->urx, sourceBBox->ury,
	    &(destBBox->urx), &(destBBox->ury));
  destBBox->ury += GetCanvasHeight();
}

static void moveHitRgn(theItem, x, y)
     charList theItem;
     float x, y;
{
  parallelogramStruct *pbox;

  pbox = &(theItem->hitRgn);
  pbox->llx += x;
  pbox->lrx += x;
  pbox->ulx += x;
  pbox->urx += x;
  pbox->lly += y;
  pbox->lry += y;
  pbox->uly += y;
  pbox->ury += y;
  updateRegionBBox(pbox);
}

static void updateRgn(theItem)
     charList theItem;
{
  float tempx, tempy;

  transform(theItem->tmat, theItem->bbox.llx, theItem->bbox.lly,
	     &tempx, &tempy);
  theItem->hitRgn.llx = tempx + theItem->xpos;
  theItem->hitRgn.lly = tempy + theItem->ypos;

  transform(theItem->tmat, theItem->bbox.urx, theItem->bbox.lly,
	     &tempx, &tempy);
  theItem->hitRgn.lrx = tempx + theItem->xpos;
  theItem->hitRgn.lry = tempy + theItem->ypos;
  
  transform(theItem->tmat, theItem->bbox.llx, theItem->bbox.ury,
	     &tempx, &tempy);
  theItem->hitRgn.ulx = tempx + theItem->xpos;
  theItem->hitRgn.uly = tempy + theItem->ypos;
  
  transform(theItem->tmat, theItem->bbox.urx, theItem->bbox.ury,
	     &tempx, &tempy);
  theItem->hitRgn.urx = tempx + theItem->xpos;
  theItem->hitRgn.ury = tempy + theItem->ypos;

  updateRegionBBox(&(theItem->hitRgn));
}

static boolean inRgn(rgn1, rgn2)
     parallelogramStruct *rgn1;
     parallelogramStruct *rgn2;
{
  bboxStruct *bbox1;
  bboxStruct *bbox2;

  bbox1 = &(rgn1->paraBBox);
  bbox2 = &(rgn2->paraBBox);

  if(bbox2->llx > bbox1->urx || bbox2->urx < bbox1->llx)
    return FALSE;
  if(bbox2->lly > bbox1->ury || bbox2->ury < bbox1->lly)
    return FALSE;

  return (checkIntersect(bbox1, rgn2));
}

static boolean inBBox(bbox1, hitRgn)
     bboxStruct *bbox1;
     parallelogramStruct *hitRgn;
{
  float rgnPoints[16];
  int passed;
  bboxStruct *bbox2;

  bbox2 = &(hitRgn->paraBBox);

  if(bbox2->llx > bbox1->urx || bbox2->urx < bbox1->llx)
    return FALSE;
  if(bbox2->lly > bbox1->ury || bbox2->ury < bbox1->lly)
    return FALSE;

  return (checkIntersect(bbox1, hitRgn));
}





/*     ===== Refresh Helper Functions =====     */

static float getSpacing(character, fontName, fontSize)
     char character;
     char *fontName;
     float fontSize;
{
  char str[2];
  float offset;

  str[0] = character;
  str[1] = '\0';

  if((fontSize == lastSize) && 
     (strcmp(lastFont, fontName) == 0)) {
    PSWgetSpacing(defaultMatrix, str, &offset);
  }
  else {
    PSWgetFontSpacing(defaultMatrix, str, fontName, fontSize, &offset);
    strcpy(lastFont, fontName);
    lastSize = fontSize;
  }

  return offset;
}

static void markRefresh(hitRgn)
     parallelogramStruct *hitRgn;
{
  charList currentItem;

  if(beginList == NULL)
    return;

  currentItem = beginList;

  do {
    if(currentItem->refresh != TRUE && 
       inRgn(hitRgn, &(currentItem->hitRgn)))
      currentItem->refresh = TRUE;
    currentItem = currentItem->next;
  } while(currentItem != NULL);
}

static void markRectRefresh(hitRgn)
     parallelogramStruct *hitRgn;
{
  charList currentItem;

  if(beginList == NULL)
    return;

  currentItem = beginList;

  do {
    if(inRgn(hitRgn, &(currentItem->hitRgn))) {
      currentItem->refresh = TRUE;
      currentItem->rectRefresh = TRUE;
    }
    currentItem = currentItem->next;
  } while(currentItem != NULL);
}


static void updateBlitArea(hitRgn)
     parallelogramStruct *hitRgn;
{
  bboxStruct bbox;

  convertBBoxToDevice(&bbox, &(hitRgn->paraBBox));

  updateClipRgn(&bbox, updateRegion);
}




/*     ===== Local Display Functions =====     */

static void printItem(theItem)
     charList theItem;
{
  char str[2];
  float deviceMatrix[6];

  str[0] = theItem->letter;
  str[1] = '\0';
  matrixmultiply(theItem->tmat, defaultMatrix, deviceMatrix);
  if((theItem->fontSize == lastSize) && 
     (strcmp(lastFont, theItem->fontName) == 0)) {
    if(theItem->outline != TRUE)
      PSWprintItem(str, deviceMatrix, theItem->xpos, theItem->ypos);
    else
      PSWprintOutline(str, deviceMatrix, theItem->xpos, theItem->ypos);
  }    
  else {
    strcpy(lastFont, theItem->fontName);
    lastSize = theItem->fontSize;
    if(theItem->outline != TRUE)
      PSWfontPrintItem(str, deviceMatrix, theItem->xpos, theItem->ypos,
		   theItem->fontName, theItem->fontSize);
    else
      PSWfontPrintOutline(str, deviceMatrix, theItem->xpos, theItem->ypos,
		      theItem->fontName, theItem->fontSize);
  }    
    
  if(updateRegion != NULL)
    updateBlitArea(&(theItem->hitRgn));
}

static void sendItem(theItem)
     charList theItem;
{
  char str[2];

  str[0] = theItem->letter;
  str[1] = '\0';
  if((theItem->fontSize == printLastSize) && 
     (strcmp(printLastFont, theItem->fontName) == 0)) {
    if(theItem->outline != TRUE)
      PSWsendItem(str, theItem->tmat, theItem->xpos, theItem->ypos);
    else
      PSWsendOutline(str, theItem->tmat, theItem->xpos, theItem->ypos);
  }    
  else {
    strcpy(printLastFont, theItem->fontName);
    printLastSize = theItem->fontSize;
    if(theItem->outline != TRUE)
      PSWfontSendItem(str, theItem->tmat, theItem->xpos, theItem->ypos,
		   theItem->fontName, theItem->fontSize);
    else
      PSWfontSendOutline(str, theItem->tmat, theItem->xpos, theItem->ypos,
		      theItem->fontName, theItem->fontSize);
  }    
}

static void printRect(theItem)
     charList theItem;
{
  float deviceMatrix[6];

  matrixmultiply(theItem->tmat, defaultMatrix, deviceMatrix);
  PSWprintRect(deviceMatrix, theItem->xpos, theItem->ypos,
	       theItem->bbox.llx, theItem->bbox.lly,
	       theItem->bbox.urx - theItem->bbox.llx,
	       theItem->bbox.ury - theItem->bbox.lly);
  markRefresh(&(theItem->hitRgn));
}

static void eraseRect(theItem)
     charList theItem;
{
  float width, height;
  bboxStruct *bbox;

  bbox = &(theItem->hitRgn.paraBBox);
  width = bbox->urx - bbox->llx;
  height = bbox->ury - bbox->lly;

  if(width < redrawValue) {
    bbox->llx = bbox->llx + width/2 - redrawValue/2;
    bbox->urx = bbox->llx + redrawValue;
    width = redrawValue;
  }
  if(height < redrawValue) {
    bbox->lly = bbox->lly + height/2 - redrawValue/2;
    bbox->ury = bbox->lly + redrawValue;
    height = redrawValue;
  }

  PSWeraseRect(bbox->llx, bbox->lly, width, height);

  markRectRefresh(&(theItem->hitRgn));
  if(updateRegion != NULL)
    updateBlitArea(&(theItem->hitRgn));
}

static void printRefresh()
{
  charList currentItem;

  if(beginList == NULL)
    return;

  currentItem = beginList;

  do {
    if(currentItem->refresh == TRUE) {
      printItem(currentItem);
      currentItem->refresh = FALSE;
      currentItem->rectRefresh = FALSE;
    }
    currentItem = currentItem->next;
  } while(currentItem != NULL);
}  

static void rectRefresh()
{
  selectPtr currentSelect;
  charList currentItem;

  if(beginSelect == NULL)
    return;

  currentSelect = beginSelect;

  do {
    currentItem = currentSelect->which;
    if(currentItem->rectRefresh == TRUE) {
      printRect(currentItem);
      currentItem->rectRefresh = FALSE;
    }
    currentSelect = currentSelect->next;
  } while(currentSelect != NULL);
}  

static void eraseSelected()
{
  selectPtr currentSelect;

  if(beginSelect == NULL)
    return;

  currentSelect = beginSelect;
  do {
    eraseRect(currentSelect->which);
    currentSelect = currentSelect->next;
  } while(currentSelect != NULL);
}  

static void redrawBBox(bbox)
     bboxStruct *bbox;
{
  charList currentItem;

  currentItem = beginList;
  printLastSize = -1;

  do {
    if(inBBox(bbox, &(currentItem->hitRgn)))
      sendItem(currentItem);
    currentItem = currentItem->next;
  } while(currentItem != NULL);
  printLastSize = -1;
}




/*     ===== Local Deletion and Selection Helper Functions =====     */

static void deleteItem(currentItem)
     charList currentItem;
{
  charList prevItem, nextItem;

  prevItem = currentItem->prev;
  nextItem = currentItem->next;
  
  if(prevItem == NULL && nextItem == NULL) {
    beginList = NULL;
    endList = NULL;
  }
  else if(prevItem == NULL) {
    nextItem->prev = NULL;
    beginList = nextItem;
  }
  else if(nextItem == NULL) {
    prevItem->next = NULL;
    endList = prevItem;
  }
  else {
    nextItem->prev = prevItem;
    prevItem->next = nextItem;
  }
  
  eraseRect(currentItem);
  free(currentItem);
}  

static void selectItem(currentItem)
     charList currentItem;
{
  selectPtr temp;

  printRect(currentItem);
  currentItem->selected = TRUE;
  
  if(beginSelect == NULL) {
    beginSelect = (selectPtr)malloc(sizeof(struct selectStruct));
    beginSelect->which = currentItem;
    currentItem->selectNode = beginSelect;
    beginSelect->next = NULL;
    beginSelect->prev = NULL;
  }
  
  else {
    temp = beginSelect;
    beginSelect = (selectPtr)malloc(sizeof(struct selectStruct));
    beginSelect->next = temp;
    temp->prev = beginSelect;
    beginSelect->prev = NULL;
    beginSelect->which = currentItem;
    currentItem->selectNode = beginSelect;
  }
}

static void unselectItem(currentItem)
     charList currentItem;
{
  selectPtr currentSelect, prevSelect, nextSelect;

  currentSelect = currentItem->selectNode;
  prevSelect = currentSelect->prev;
  nextSelect = currentSelect->next;
  
  if(prevSelect == NULL && nextSelect == NULL) {
    beginSelect = NULL;
    endSelect = NULL;
  }
  else if(prevSelect == NULL) {
    nextSelect->prev = NULL;
    beginSelect = nextSelect;
  }
  else if(nextSelect == NULL) {
    prevSelect->next = NULL;
    endSelect = prevSelect;
  }
  else {
    nextSelect->prev = prevSelect;
    prevSelect->next = nextSelect;
  }
  
  eraseRect(currentSelect->which);
  free(currentSelect);
  currentItem->selected = FALSE;
  currentItem->selectNode = NULL;
}





/*     ===== Global Functions =====     */

/*    ==== Initialization Functions ====    */

void matrixInit()
{
  float dummy;

  PSWdefaultMatrix(defaultMatrix);
  itransform(defaultMatrix, REDRAW_VAL, 0.0, &redrawValue, &dummy);
}


/*    ==== Conversion Functions ====    */

void convertDevicePoint(sourcex, sourcey, destx, desty)
     float sourcex, sourcey;
     float *destx, *desty;
{
  sourcey = sourcey - (float)GetCanvasHeight();
  itransform(defaultMatrix, sourcex, sourcey, destx, desty);
}

void convertUserPoint(sourcex, sourcey, destx, desty)
     float sourcex, sourcey;
     float *destx, *desty;
{
  transform(defaultMatrix, (float)sourcex, (float)sourcey, destx, desty);
  *desty += GetCanvasHeight();
}

/*    ==== Expose Procedures ====    */

void redrawElements(ulx, uly, lrx, lry)
     int ulx, uly, lrx, lry;
{
  bboxStruct bbox;

  if(beginList == NULL)
    return;

  convertDevicePoint((float)ulx, (float)lry, &(bbox.llx), &(bbox.lly));
  convertDevicePoint((float)lrx, (float)uly, &(bbox.urx), &(bbox.ury));
  redrawBBox(&bbox);
}


/*    ==== List Manipulation Procedures ====    */

/*   === Adding Elements ===   */

boolean addElement(character, fontName, fontSize, outline, fontBBox,
		blitRgn, xpos, ypos)
     char character;
     char *fontName;
     float fontSize;
     int outline;
     bboxStruct *fontBBox;
     Region *blitRgn;
     float *xpos, *ypos;
{
  float offset;
  bboxStruct *bbox;
  charList last = NULL;
  int tempy;

  offset = getSpacing(character, fontName, fontSize);
  if(character == ' ') {
    *xpos = *xpos + offset;
    if(endList != NULL) {
      endList->charWidth += offset;
      endList->spaces++;
    }
    return FALSE;
  }

  updateRegion = blitRgn;

  if(beginList == NULL) {
    beginList = (charList)malloc(sizeof(struct charStruct));
    endList = beginList;
  }
  else {
    endList->next = (charList)malloc(sizeof(struct charStruct));
    last = endList;
    endList = endList->next;
  }

  endList->charWidth = offset;
  endList->spaces = 0;
  endList->letter = character;
  strcpy(endList->fontName, fontName);
  endList->fontSize = fontSize;
  if(outline)
    endList->outline = TRUE;
  else
    endList->outline = FALSE;
  copymatrix(identityMatrix, endList->tmat);
  endList->xpos = *xpos;
  endList->ypos = *ypos;

  endList->refresh = FALSE;
  endList->rectRefresh = FALSE;
  endList->selected = FALSE;
  endList->selectNode = NULL;
  endList->next = NULL;
  endList->prev = last;

  initBBox(endList, fontBBox);
  bbox = &(endList->bbox);
  initRgn(&(endList->hitRgn), bbox, *xpos, *ypos);
  printItem(endList);
  *xpos = *xpos + offset;

  updateRegion = NULL;
  return TRUE;
}

/*   === Selecting Elements ===   */

int constructiveSelectElements(ulx, uly, lrx, lry, blitRgn)
     int ulx, uly, lrx, lry;
     Region *blitRgn;
{
  charList currentItem;
  boolean gotone = FALSE;
  boolean missedAll = TRUE;
  bboxStruct bbox;

  if(beginList == NULL)
    return SELECTED_MISS;

  updateRegion = blitRgn;
  convertDevicePoint((float)ulx, (float)lry, &(bbox.llx), &(bbox.lly));
  convertDevicePoint((float)lrx, (float)uly, &(bbox.urx), &(bbox.ury));
  
  currentItem = beginList;

  do {
    if(inBBox(&bbox, &(currentItem->hitRgn))) {
      missedAll = FALSE;
      if(currentItem->selected != TRUE) {
	gotone = TRUE;
	selectItem(currentItem);
      }
    }
    currentItem = currentItem->next;
  } while(currentItem != NULL);

  if(gotone == TRUE) {
    printRefresh();
  }

  updateRegion = NULL;
  if(gotone == TRUE)
    return SELECTED_NEW;

  if(missedAll == TRUE)
    return SELECTED_MISS;

  return SELECTED_ALREADY;
}

boolean destructiveSelectElements(ulx, uly, lrx, lry, blitRgn)
     int ulx, uly, lrx, lry;
     Region *blitRgn;
{
  charList currentItem;
  boolean changedOne = FALSE;
  bboxStruct bbox;

  if(beginList == NULL)
    return FALSE;

  updateRegion = blitRgn;
  convertDevicePoint((float)ulx, (float)lry, &(bbox.llx), &(bbox.lly));
  convertDevicePoint((float)lrx, (float)uly, &(bbox.urx), &(bbox.ury));

  currentItem = beginList;

  do {
    if(inBBox(&bbox, &(currentItem->hitRgn))) {
      if(currentItem->selected != TRUE) {
	changedOne = TRUE;
	selectItem(currentItem);
      }
    }

    else {
      if(currentItem->selected == TRUE) {
	changedOne = TRUE;
	unselectItem(currentItem);
      }
    }
    currentItem = currentItem->next;
  } while(currentItem != NULL);
  
  if(changedOne == TRUE) {
    rectRefresh();
    printRefresh();
  }

  updateRegion = NULL;
  return changedOne;
}


boolean selectAll(blitRgn)
     Region *blitRgn;
{
  charList currentItem;
  boolean gotone = FALSE;
  bboxStruct bbox;

  if(beginList == NULL)
    return FALSE;

  updateRegion = blitRgn;
  currentItem = beginList;

  do {
    if(currentItem->selected != TRUE) {
      gotone = TRUE;
      selectItem(currentItem);
    }
    currentItem = currentItem->next;
  } while(currentItem != NULL);

  if(gotone == TRUE) {
    printRefresh();
  }

  updateRegion = NULL;
  return gotone;
}


boolean unselectAll(blitRgn)
     Region *blitRgn;
{
  selectPtr temp;

  if(beginSelect == NULL)
    return FALSE;

  updateRegion = blitRgn;
  do {
    temp = beginSelect;
    beginSelect->which->selected = FALSE;
    beginSelect->which->selectNode = NULL;
    eraseRect(beginSelect->which);
    beginSelect = beginSelect->next;
    free(temp);
  } while(beginSelect != NULL);

  endSelect = NULL;
  printRefresh();
  updateRegion = NULL;
  return TRUE;
}


/*   === Deleting Elements ===   */

int deleteLastElement(returnX, returnY, blitRgn)
     float *returnX, *returnY;
     Region *blitRgn;
{
  charList temp;

  if(endList == NULL)
    return 0;

  if(endList->spaces > 0) {
    endList->spaces--;
    endList->charWidth -= getSpacing(' ', endList->fontName, endList->fontSize);
    *returnX = endList->xpos + endList->charWidth;
    *returnY = endList->ypos;
    return -1;
  }    

  updateRegion = blitRgn;
  *returnX = endList->xpos;
  *returnY = endList->ypos;
  
  eraseRect(endList);

  if(endList->selected == TRUE) {
    unselectItem(endList);
  }

  temp = endList->prev;
  deleteItem(endList);
  endList = temp;

  rectRefresh();
  printRefresh();
  updateRegion = NULL;
  return 1;
}


boolean deleteSelected(blitRgn)
     Region *blitRgn;
{
  selectPtr temp;
  charList currentItem;

  if(beginSelect == NULL)
    return FALSE;

  updateRegion = blitRgn;

  do {
    temp = beginSelect->next;
    currentItem = beginSelect->which;
    unselectItem(currentItem);
    deleteItem(currentItem);
    beginSelect = temp;
  } while(beginSelect != NULL);

  endSelect = NULL;
  printRefresh();
  updateRegion = NULL;
  return TRUE;
}

boolean deleteAll(blitRgn)
     Region *blitRgn;
{
  charList temp;

  if(beginList == NULL)
    return FALSE;

  updateRegion = blitRgn;

  do {
    temp = beginList->next;
    if(beginList->selected == TRUE)
      unselectItem(beginList);
    deleteItem(beginList);
    beginList = temp;
  } while(beginList != NULL);

  endList = NULL;
  updateRegion = NULL;
  return TRUE;
}



/*    ==== Selection Manipulation Functions ====    */

boolean moveSelectedByPixels(xoff, yoff, blitRgn)
     int xoff, yoff;
     Region *blitRgn;
{
  selectPtr currentSelect;
  charList theItem;
  float userx, usery;

  if(beginSelect == NULL)
    return FALSE;

  updateRegion = blitRgn;
  itransform(defaultMatrix, (float)xoff, (float)yoff, &userx, &usery);
  currentSelect = beginSelect;
  eraseSelected();

  do {
    theItem = currentSelect->which;
    theItem->xpos += userx;
    theItem->ypos += usery;
    moveHitRgn(theItem, userx, usery);
    printRect(theItem);
    
    currentSelect = currentSelect->next;
  } while(currentSelect != NULL);

  printRefresh();

  updateRegion = NULL;
  return TRUE;
}

boolean resetSelectedAlignment(xpos, ypos, blitRgn)
     int xpos, ypos;
     Region *blitRgn;
{
  charList currentItem;
  float userx, usery;
  float realWidth;
  float dummy;

  if(beginSelect == NULL)
    return FALSE;

  updateRegion = blitRgn;
  convertDevicePoint((float)xpos, (float)ypos, &userx, &usery);
  eraseSelected();

  currentItem = beginList;
  do {
    if(currentItem->selected) {
      initmatrix(currentItem->tmat);
      currentItem->xpos = userx;
      currentItem->ypos = usery;
      updateRgn(currentItem);
      printRect(currentItem);
      userx += currentItem->charWidth;
    }
    currentItem = currentItem->next;
  } while(currentItem != NULL);

  printRefresh();
  updateRegion = NULL;
  return TRUE;
}



/*    ==== Rotating Functions ====    */

boolean anchorRotate(xanchor, yanchor)
     int xanchor, yanchor;
{
  if(beginSelect == NULL)
    return FALSE;

  convertDevicePoint((float)xanchor, (float)yanchor,
		     &rotateAnchorX, &rotateAnchorY);
  return TRUE;
}

boolean beginRotate(xclick, yclick)
     int xclick, yclick;
{
  charList currentItem;
  selectPtr currentSelect;
  float currentAnchorX, currentAnchorY;

  if(beginSelect == NULL)
    return FALSE;

  convertDevicePoint((float)xclick, (float)yclick, 
		     &rotateClickX, &rotateClickY);

  currentSelect = beginSelect;
  if((rotateClickX - rotateAnchorX) == 0.0 &&
     (rotateClickY - rotateAnchorY) == 0.0)
    initialAngle = 0.0;
  else
    initialAngle = (float)atan2((double)(rotateClickY - rotateAnchorY),
				(double)(rotateClickX - rotateAnchorX));

  do {
    currentItem = currentSelect->which;
    copymatrix(currentItem->tmat, currentItem->oldmat);
    
    currentItem->xcnst = currentItem->xpos - rotateAnchorX;
    currentItem->ycnst = currentItem->ypos - rotateAnchorY;
    currentSelect = currentSelect->next;
  } while(currentSelect != NULL);
  return TRUE;
}

boolean doRotate(xmove, ymove, blitRgn)
     int xmove, ymove;
     Region *blitRgn;
{
  charList currentItem;
  selectPtr currentSelect;
  float rotateMoveX, rotateMoveY;
  float addx, addy;
  float radians;
  float identMatrix[6];
  float temp[6];

  if(beginSelect == NULL)
    return FALSE;

  updateRegion = blitRgn;
  convertDevicePoint((float)xmove, (float)ymove, &rotateMoveX, &rotateMoveY);
  currentSelect = beginSelect;
  eraseSelected();
  if((rotateMoveX - rotateAnchorX) == 0.0 && 
     (rotateMoveY - rotateAnchorY) == 0.0)
    radians = initialAngle;
  else
    radians = (float)atan2((double)(rotateMoveY - rotateAnchorY), 
			   (double)(rotateMoveX - rotateAnchorX))
    		     - initialAngle;

  do {
    copymatrix(identityMatrix, identMatrix);
    currentItem = currentSelect->which;
    copymatrix(currentItem->oldmat, currentItem->tmat);
    rotate(identMatrix, (double)radians);
    transform(identMatrix, currentItem->xcnst, currentItem->ycnst,
	      &addx, &addy);
    currentItem->xpos = rotateAnchorX + addx;
    currentItem->ypos = rotateAnchorY + addy;
    matrixmultiply(currentItem->tmat, identMatrix, temp);
    copymatrix(temp, currentItem->tmat);

    updateRgn(currentItem);
    printRect(currentItem);
    currentSelect = currentSelect->next;
  } while(currentSelect != NULL);

  printRefresh();
  updateRegion = NULL;
  return TRUE;
}



/*    ==== Scaling Functions ====    */

boolean anchorScale(xanchor, yanchor)
     int xanchor, yanchor;
{
  if(beginSelect == NULL)
    return FALSE;

  convertDevicePoint((float)xanchor, (float)yanchor,
		     &scaleAnchorX, &scaleAnchorY);
  return TRUE;
}

boolean beginScale(xclick, yclick)
     int xclick, yclick;
{
  charList currentItem;
  selectPtr currentSelect;
  float userx, usery;

  if(beginSelect == NULL)
    return FALSE;

  convertDevicePoint((float)xclick, (float)yclick, &scaleClickX, &scaleClickY);
  currentSelect = beginSelect;

  if(absval(scaleClickX - scaleAnchorX) < SCALE_TOLERANCE)
    scaleClickX = scaleAnchorX +
      (sign(scaleClickX - scaleAnchorX) * SCALE_TOLERANCE);
  if(absval(scaleClickY - scaleAnchorY) < SCALE_TOLERANCE)
    scaleClickY = scaleAnchorY +
      (sign(scaleClickY - scaleAnchorY) * SCALE_TOLERANCE);

  do {
    currentItem = currentSelect->which;
    copymatrix(currentItem->tmat, currentItem->oldmat);

    currentItem->xcnst = currentItem->xpos - scaleAnchorX;
    currentItem->ycnst = currentItem->ypos - scaleAnchorY;

    currentSelect = currentSelect->next;
  } while(currentSelect != NULL);
  return TRUE;
}

boolean doScale(xmove, ymove, blitRgn)
     int xmove, ymove;
     Region *blitRgn;
{
  charList currentItem;
  selectPtr currentSelect;
  float scaleMoveX, scaleMoveY;
  float transx, transy;
  float scalex, scaley;
  float addx, addy;
  float temp[6];
  float identMatrix[6];

  if(beginSelect == NULL)
    return FALSE;

  updateRegion = blitRgn;
  convertDevicePoint((float)xmove, (float)ymove, &scaleMoveX, &scaleMoveY);
  currentSelect = beginSelect;
  eraseSelected();

  do {
    copymatrix(identityMatrix, identMatrix);
    currentItem = currentSelect->which;
    copymatrix(currentItem->oldmat, currentItem->tmat);
    scalex = (scaleMoveX - scaleAnchorX) / (scaleClickX - scaleAnchorX);
    scaley = (scaleMoveY - scaleAnchorY) / (scaleClickY - scaleAnchorY);

    if(absval(scalex) < MIN_SCALE)
      scalex = MIN_SCALE;
    if(absval(scaley) < MIN_SCALE)
      scaley = MIN_SCALE;

    scale(identMatrix, scalex, scaley);
    transform(identMatrix, currentItem->xcnst, currentItem->ycnst,
	      &addx, &addy);
    currentItem->xpos = scaleAnchorX + addx;
    currentItem->ypos = scaleAnchorY + addy;
    matrixmultiply(currentItem->tmat, identMatrix, temp);
    copymatrix(temp, currentItem->tmat);

    updateRgn(currentItem);
    printRect(currentItem);
    currentSelect = currentSelect->next;
  } while(currentSelect != NULL);

  printRefresh();
  updateRegion = NULL;
  return TRUE;
}



/*    ==== Slanting Functions ====    */

boolean anchorSlant(xanchor, yanchor)
     int xanchor, yanchor;
{
  if(beginSelect == NULL)
    return FALSE;

  convertDevicePoint((float)xanchor, (float)yanchor,
		     &slantAnchorX, &slantAnchorY);
  return TRUE;
}

boolean beginSlant(xclick, yclick)
     int xclick, yclick;
{
  charList currentItem;
  selectPtr currentSelect;

  if(beginSelect == NULL)
    return FALSE;

  convertDevicePoint((float)xclick, (float)yclick, &slantClickX, &slantClickY);
  currentSelect = beginSelect;

  do {
    currentItem = currentSelect->which;
    copymatrix(currentItem->tmat, currentItem->oldmat);
    currentItem->xcnst = currentItem->xpos - slantAnchorX;
    currentItem->ycnst = currentItem->ypos - slantAnchorY;
    currentSelect = currentSelect->next;
  } while(currentSelect != NULL);
  return TRUE;
}

boolean doSlant(xmove, ymove, blitRgn)
     int xmove, ymove;
     Region *blitRgn;
{
  charList currentItem;
  selectPtr currentSelect;
  float slantMoveX, slantMoveY;
  float addx, addy;
  float slantx, slanty;
  float temp[6];
  float identMatrix[6];

  if(beginSelect == NULL)
    return FALSE;

  updateRegion = blitRgn;
  convertDevicePoint((float)xmove, (float)ymove, &slantMoveX, &slantMoveY);
  currentSelect = beginSelect;
  eraseSelected();

  do {
    copymatrix(identityMatrix, identMatrix);
    currentItem = currentSelect->which;
    copymatrix(currentItem->oldmat, currentItem->tmat);
    slantx = (slantMoveX - slantClickX) / (slantClickY - slantAnchorY);
    slanty = (slantMoveY - slantClickY) / (slantClickX - slantAnchorX);

    slant(identMatrix, slantx, slanty);
    transform(identMatrix, currentItem->xcnst, currentItem->ycnst,
	      &addx, &addy);
    currentItem->xpos = slantAnchorX + addx;
    currentItem->ypos = slantAnchorY + addy;
    matrixmultiply(currentItem->tmat, identMatrix, temp);
    copymatrix(temp, currentItem->tmat);

    updateRgn(currentItem);
    printRect(currentItem);
    currentSelect = currentSelect->next;
  } while(currentSelect != NULL);

  printRefresh();
  updateRegion = NULL;
  return TRUE;
}



/*     ===== Save and Load Functions =====     */

boolean saveNextElement(letter, fontName, fontSize, outline,
			tmat, xpos, ypos, offset)
     char *letter;
     char **fontName;
     float *fontSize;
     int *outline;
     float **tmat;
     float *xpos, *ypos;
     float *offset;
{
  static boolean firstTime = TRUE;
  static charList currentItem = NULL;

  if(firstTime == TRUE) {
    currentItem = beginList;
    firstTime = FALSE;
  }
  else
    currentItem = currentItem->next;

  if(currentItem == NULL) {
    firstTime = TRUE;
    return FALSE;
  }

  *letter = currentItem->letter;
  *fontName = currentItem->fontName;
  *fontSize = currentItem->fontSize;
  if(currentItem->outline)
    *outline = 1;
  else
    *outline = 0;
  *tmat = currentItem->tmat;
  *xpos = currentItem->xpos;
  *ypos = currentItem->ypos;
  *offset = currentItem->charWidth;
  return TRUE;
}

void loadNextElement(character, fontName, fontSize, outline, fontBBox,
		     tmat, xpos, ypos, offset)
     char character;
     char *fontName;
     float fontSize;
     boolean outline;
     bboxStruct *fontBBox;
     float *tmat;
     float xpos, ypos;
     float offset;
{
  charList last = NULL;

  if(beginList == NULL) {
    beginList = (charList)malloc(sizeof(struct charStruct));
    endList = beginList;
  }
  else {
    endList->next = (charList)malloc(sizeof(struct charStruct));
    last = endList;
    endList = endList->next;
  }

  endList->charWidth = offset;
  endList->letter = character;
  strcpy(endList->fontName, fontName);
  endList->fontSize = fontSize;
  if(outline)
    endList->outline = TRUE;
  else
    endList->outline = FALSE;
  copymatrix(tmat, endList->tmat);
  endList->xpos = xpos;
  endList->ypos = ypos;

  endList->refresh = FALSE;
  endList->rectRefresh = FALSE;
  endList->selected = FALSE;
  endList->selectNode = NULL;
  endList->next = NULL;
  endList->prev = last;

  initBBox(endList, fontBBox);
  updateRgn(endList);
  printItem(endList);
}




/*     ===== Change Functions =====     */

boolean changeSelectedFont(fontName, fontSize, fontBBox, blitRgn)
     char *fontName;
     float fontSize;
     bboxStruct *fontBBox;
     Region *blitRgn;
{
  float offset, spaceWidth;
  float currentx = -1.0;
  float currenty = -1.0;
  charList currentItem;

  if(beginSelect == NULL)
    return FALSE;

  updateRegion = blitRgn;
  spaceWidth = getSpacing(' ', fontName, fontSize);
  currentItem = beginList;
  eraseSelected();

  do {
    if(currentItem->selected) {
      if(currentItem->ypos != currenty) {
	currentx = currentItem->xpos;
	currenty = currentItem->ypos;
      }
      offset = getSpacing(currentItem->letter, fontName, fontSize);
      currentItem->charWidth = offset + currentItem->spaces * spaceWidth;
      strcpy(currentItem->fontName, fontName);
      currentItem->fontSize = fontSize;
      currentItem->xpos = currentx;
      currentx += offset + currentItem->spaces * spaceWidth;

      initBBox(currentItem, fontBBox);
      updateRgn(currentItem);
      printRect(currentItem);
    }
    currentItem = currentItem->next;
  } while(currentItem != NULL);

  printRefresh();
  updateRegion = NULL;
  return TRUE;
}

void getCurrentFontList(fontList, numFonts)
     char ***fontList;
     int *numFonts;
{
  charList currentItem;
  char *thisFont;
  char **fontPtrs;
  char **myList;
  int i, num, match=0;

  myList = NULL;
  *numFonts = 0;
  num = 0;
  if(beginList == NULL) {
    return;
  }

  fontPtrs = (char **)malloc(sizeof(char *) * 256);
  currentItem = beginList;

  do {
    thisFont = currentItem->fontName;
    if(num == 0) {
      fontPtrs[num] = thisFont;
      num++;
    }
    else {
      for(i=0; i<num; i++) {
	if(strcmp(thisFont, fontPtrs[i]) == 0) {
	  match = 1;
	  break;
	}
      }
      if(!match) {
	fontPtrs[num] = thisFont;
	num++;
      }
      match = 0;
    }
    currentItem = currentItem->next;
  } while(currentItem != NULL);

  *numFonts = num;

  myList = (char **)malloc(num * sizeof(char *));
  for(i=0; i<num; i++) {
    myList[i] = (char *)malloc((strlen(fontPtrs[i])+1) * sizeof(char));
    strcpy(myList[i], fontPtrs[i]);
  }
  free(fontPtrs);
  *fontList = myList;
  return;
}
