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


 

/*
 * P_R_P_Q_# (C) COPYRIGHT IBM CORPORATION 1987
 * LICENSED MATERIALS - PROPERTY OF IBM
 * REFER TO COPYRIGHT INSTRUCTIONS FORM NUMBER G120-2083
 */
/*
rasterimage.H

	rasterimage object

	is a subclass of pixelimage and adds a few fields utilized by the
	raster package

	Adds the file name finding operation

*/

#define  rasterimage_PROGRAMMERVERSION    1

/* status values for NotifyObservers */
#define rasterimage_DATACHANGED 1

class rasterimage[rastimg] : pixelimage[pixelimg] {

overrides:

	AddObserver(/* struct rasterimage *self, */ struct object *observer);
	RemoveObserver(/* struct rasterimage *self, */ struct object *observer);
	Clone(/*struct rasterimage *self, */) returns struct thisobject *;

methods:

	FindFile(/* struct rasterimage *self, */ char *filename, char *path) returns FILE *;
		/* finds the file, searching in various places.  Opens it and
		 returns the stream.  Saves file name in filename. */
	Defile(/* struct rasterimage *self */);
		/* removes the filename information in preparation for
		 reading in a new file  (Should this be part of resize ? XXX) */

macromethods:

	SetWriteID(/* self, */ wid)	(self->writeID = (wid))
	GetWriteID(/* self */)	(self->writeID)
	SetObjectID(/* self, */ obid)	(self->objectid = (obid))
	GetObjectID(/* self*/ )	(self->objectid)
	MarkPermanent(/* self */)	(self->refcnt++)
	GetFileName(/* self */)	(self->filename)
	GetFilePath(/* self */)	(self->resolutionPath)

classprocedures:

	InitializeObject(/* struct classhdr *ClassID,*/ struct rasterimage *self) returns boolean;
	FinalizeObject(/* struct classhdr *ClassID;*/ struct rasterimage *self);
	Create(long width, long height) returns struct rasterimage *;
		/* creates a rasterimage object with 'bits' for a raster
			of the given width and height */

data:

	long writeID;	/* the writeID when this pixelimage was most recently written */
	long objectid;	/* gives the objectid of the raster that mostly recently 
					wrote this pixelimage as a "bits"  */
	char *filename;		/* file raster is referenced from (or NULL) */
	char *resolutionPath;	/* path originally used to resolve filename */
	long refcnt;		/* XXX */

};

