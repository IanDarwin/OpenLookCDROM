/* file.c
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
#include "list.h"

extern double atof();
#define BUFFER_LENGTH 128

void saveFile(fileName)
     char *fileName;
{
  FILE *saveFile;
  char buffer[BUFFER_LENGTH];
  char letter;
  char *fontName;
  float fontSize;
  int outline;
  float *tmat;
  float xpos, ypos;
  float offset;

  saveFile = fopen(fileName, "w");

  if(saveFile == NULL) {
    fprintf(stderr, "fopen failed\n");
    return;
  }

  while(saveNextElement(&letter, &fontName, &fontSize, &outline, 
			&tmat, &xpos, &ypos, &offset)) {
    sprintf(buffer, "%c\n", letter);
    fputs(buffer, saveFile);
    
    fputs(fontName, saveFile);
    fputc(' ', saveFile);
    
    sprintf(buffer, "%g\n", fontSize);
    fputs(buffer, saveFile);
    
    sprintf(buffer, "%d\n", outline);
    fputs(buffer, saveFile);
    
    fputc('[', saveFile);
    sprintf(buffer, "%g ", tmat[0]);
    fputs(buffer, saveFile);
    sprintf(buffer, "%g ", tmat[1]);
    fputs(buffer, saveFile);
    sprintf(buffer, "%g ", tmat[2]);
    fputs(buffer, saveFile);
    sprintf(buffer, "%g ", tmat[3]);
    fputs(buffer, saveFile);
    sprintf(buffer, "%g ", tmat[4]);
    fputs(buffer, saveFile);
    sprintf(buffer, "%g", tmat[5]);
    fputs(buffer, saveFile);
    fputc(']', saveFile);
    fputc('\n', saveFile);

    sprintf(buffer, "%g %g\n", xpos, ypos);
    fputs(buffer, saveFile);
    sprintf(buffer, "%g\n", offset);
    fputs(buffer, saveFile);
    fputc('\n', saveFile);
  }    
  fclose(saveFile);
}

void loadFile(fileName)
  char *fileName;
{
  FILE *loadFile;
  char buffer[BUFFER_LENGTH];
  char letter;
  char fontBuffer[BUFFER_LENGTH];
  char *fontName;
  float fontSize;
  int outline;
  float tmat[6];
  float xpos, ypos;
  float offset;
  char *temp;
  bboxStruct bbox;

  loadFile = fopen(fileName, "r");
  if(loadFile == NULL)
    return;

  while(1) {
    if(fgets(buffer, BUFFER_LENGTH, loadFile) == NULL)
      return;
    letter = *buffer;
    
    if(fgets(fontBuffer, BUFFER_LENGTH, loadFile) == NULL)
      return;
    fontName = strtok(fontBuffer, " ");
    if(fontName == NULL) return;
    temp = strtok(NULL, "\n");
    if(temp == NULL) return;
    fontSize = (float)atof(temp);
    if(fontSize == 0.0)
      return;
  
    if(fgets(buffer, BUFFER_LENGTH, loadFile) == NULL)
      return;
    temp = strtok(buffer, "\n");
    if(temp == NULL) return;
    outline = atoi(temp);
    
    if(fgets(buffer, BUFFER_LENGTH, loadFile) == NULL)
      return;
    temp = strtok(buffer+1, " ");
    if(temp == NULL) return;
    tmat[0] = (float)atof(temp);
    temp = strtok(NULL, " ");
    if(temp == NULL) return;
    tmat[1] = (float)atof(temp);
    temp = strtok(NULL, " ");
    if(temp == NULL) return;
    tmat[2] = (float)atof(temp);
    temp = strtok(NULL, " ");
    if(temp == NULL) return;
    tmat[3] = (float)atof(temp);
    temp = strtok(NULL, " ");
    if(temp == NULL) return;
    tmat[4] = (float)atof(temp);
    temp = strtok(NULL, "]");
    if(temp == NULL) return;
    tmat[5] = (float)atof(temp);
    
    if(fgets(buffer, BUFFER_LENGTH, loadFile) == NULL)
      return;
    temp = strtok(buffer, " ");
    if(temp == NULL) return;
    xpos = (float)atof(temp);
    temp = strtok(NULL, "\n");
    if(temp == NULL) return;
    ypos = (float)atof(temp);
    
    if(fgets(buffer, BUFFER_LENGTH, loadFile) == NULL)
      return;
    temp = strtok(buffer, "\n");
    if(temp == NULL) return;
    offset = (float)atof(temp);
    
    fgets(buffer, BUFFER_LENGTH, loadFile);
    
    PSWfontbbox(fontName, fontSize, 
		&(bbox.llx), &(bbox.lly), &(bbox.urx), &(bbox.ury));
    
    loadNextElement(letter, fontName, fontSize, outline, &bbox,
		    tmat, xpos, ypos, offset);
  }
}
