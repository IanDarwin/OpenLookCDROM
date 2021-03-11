/* $Author: rr2b $ */

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


 

/* ************************************************************
 *  Copyright (C) 1989, 1990, 1991
 *  by the Massachusetts Institute of Technology
 *  For full copyright information see:'mit-copyright.h'     *
 *************************************************************/

#include <mit-copyright.h>

#include "eos_structs.h"

package eosfx [eosfx] {
classprocedures:
	PaperClear(Paper *p);
	PaperCopy(Paper *src, Paper *dest);
	LocatePaper(struct paperPositions *list, long x, Paper *paper) returns struct paperPositions *;
	AddPaperText(struct paperPositions **list, Paperlist paper, long x, long len);
	DestroyPositions(struct paperPositions **list);
	OpenCourse(char *course, FX **fxp) returns char *;
	Close(FX **fxp);
	SendFile(char *course, char *filename, Paper *paper, boolean delete)returns char *;
	RetrieveFile (FX *fxp, Paper *paper, char *filename) returns char *;
	Retrieve (FX *fxp, Paper *paper, FILE *fp) returns char *;
	Move(FX *fxp, Paper *src, Paper *dest) returns char *;
	Delete(FX *fxp, Paper *paper) returns char *;
	ListDestroy(Paperlist_res **plist) ;
	List(FX *fxp, Paper *paper, Paperlist_res **ret) returns char *;
	AclList(FX *fxp, char *name, stringlist_res **list) returns char *;
	AclDestroy(stringlist_res **list);
	AclAdd(FX *fxp, char *aclname, char *name) returns char *;
	AclDel(FX *fxp, char *aclname, char *name) returns char *;
	Directory(FX *fxp, stringlist_res **list) returns char *;
	LocalUnique(char *template) returns char *;
	PathTail(char *path) returns char *;
	SaveString (char *s) returns char *;
	PaperCopyContents (Paper *src, Paper *dest);
	PaperFreeContents(Paper *p);
};

