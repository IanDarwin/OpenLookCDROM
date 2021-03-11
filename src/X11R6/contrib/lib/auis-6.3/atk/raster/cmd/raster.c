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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/raster/cmd/RCS/raster.c,v 2.19 1992/12/15 21:39:11 rr2b R6tape $";
#endif


 

/* raster.c 

	raster Data-object

	Utilizes a rasterimage for actual storage of the image.


*/

#define RASTERVERSION	2

#define MAXFILELINE 255
#define BUFBYTES	600	/* enough for 4792 bits */

#include <stdio.h>
#include <sys/param.h> /* Defines MAXPATHLEN among other things */
#include <rect.h>

#include <class.h>
#include <attribs.h>
#include <raster.eh>
#include <dataobj.ih>
#include <rastimg.ih>
#include <pixelimg.ih>	/* for ClipRange */
#include <rasterio.ih>
#include <plusspc.ih>
#include <oldrf.ih>
#include <xwdio.ih> /* for WriteOtherFormat */
#include <ctype.h>


boolean raster__InitializeObject(ClassID, self)
struct classhdr *ClassID;
register struct raster  *self;
{
    self->pix = NULL;
    self->readOnly = FALSE;
    rectangle_EmptyRect(&self->subraster);
    /* default scaling is one half screen size */
    self->xScale = (raster_UNITSCALE * 12 + 12) / 25;
    self->yScale = (raster_UNITSCALE * 12 + 12) / 25;
    return TRUE;
}

void raster__FinalizeObject(ClassID, self)
struct classhdr *ClassID;
register struct raster  *self;
{
    raster_SetPix(self, NULL);
}

struct raster * raster__Create(ClassID, width, height)
struct classhdr *ClassID;
register long width, height;
{
    register struct raster *self = raster_New();
    raster_Resize(self, width, height);
    return self;
}

void raster__Resize(self, width, height)
register struct raster  *self;
register long width, height;
{
    struct rasterimage *pix = raster_GetPix(self);
    if (pix == NULL) 
	raster_SetPix(self, pix = rasterimage_Create(width, height));
    else {
	rasterimage_Resize(pix, width, height);
	rectangle_SetRectSize(&self->subraster, 0, 0, width, height);
	self->options = 0; }
}

void raster__ObservedChanged(self, pix, status)
register struct raster  *self;
struct rasterimage *pix;
long status;
{
    if (status == observable_OBJECTDESTROYED) {
	/* the observed rasterimage is going away
	 we must not use raster_SetPix because
	 it will tinker the refcnt and try again to 
	 destroy the object */
	self->pix = NULL;
	return; }
    /* inform my own observers that the underlying rasterimage has changed */
    if (rasterimage_GetResized(pix)) {
	rectangle_SetRectSize(&self->subraster, 0, 0, 
			      rasterimage_GetWidth(pix), rasterimage_GetHeight(pix));
	self->options = 0;
	self->xScale = raster_UNITSCALE / 2;
	self->yScale = raster_UNITSCALE / 2;
	/* pasieka's changing this to status. */
	raster_NotifyObservers(self, raster_BOUNDSCHANGED);
	raster_NotifyObservers(self, status); }
    else {
	/* notify observers of self only if the changed rectangle
	    of pix intersects with the subraster of self */
	struct rectangle R;
	R = self->subraster;
	rectangle_IntersectRect(&R, &R, rasterimage_GetChanged(pix));
	if ( ! rectangle_IsEmptyRect(&R))
	    /* pasieka's changing this to status. */
	    raster_NotifyObservers(self, raster_BITSCHANGED);
	    raster_NotifyObservers(self, status); }
}

void raster__SetPix(self, newpix)
struct raster *self;
struct rasterimage *newpix;
{
    struct rasterimage *pix = raster_GetPix(self);
    if (newpix == pix) return;
    if (pix != NULL) 
	rasterimage_RemoveObserver(pix, self);
    self->pix = newpix;
    self->options = 0;
    if (newpix != NULL) {
	rectangle_SetRectSize(&self->subraster, 0, 0, 
			      rasterimage_GetWidth(newpix),
			      rasterimage_GetHeight(newpix));
	rasterimage_AddObserver(newpix, self);
	/* XXX This is a kludge to kludge over the kludge in rasterview's RedrawRaster which creates a rasterimage data object when the view does not have a data object to view. */
	if (pix != NULL) {
	    raster_NotifyObservers(self, raster_BITSCHANGED);
	    raster_NotifyObservers(self, raster_BOUNDSCHANGED); } }
    else
	rectangle_EmptyRect(&self->subraster);
    /* XXX maybe we ought to notify observers of a BOUNDSCAHNGED
      even when newpix is NULL, but that value is used 
      when we are being Destroyed.  */
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *
 *	WRITE routines
 *		entry points:
 *			raster__Write
 *			raster__WriteSubRaster
 *			raster__WriteShare   XXX
 * - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
#ifdef NOTUSED
/*	WriteV1Stream(struct raster *ras, FILE *file, long id);
		write to 'file' the plusspace representation of 'ras' */
	static void
WriteV1Stream(ras, file, id)
	struct raster *ras;
	FILE *file;
	long id;
{
	register long nbytestofile;
	short buf[400];
	long x, y, w, h, yend;
	struct pixelimage *pix = (struct pixelimage *)raster_GetPix(ras);

	rectangle_GetRectSize(&ras->subraster, &x, &y, &w, &h);
	fprintf(file, "\\begindata{%s,%d}\n", class_GetTypeName(ras), id);
	fprintf(file, "%ld %u %d %d %d %d %ld %ld %d", 
			1, 0, 1, 1, 0, 0, w, h, 1);
		/* self->raster_version, self->raster_options, 
		self->raster_compression, self->raster_expansion, 
		self->raster_xoffset, self->raster_yoffset, 
		self->raster_width, self->raster_height, RASTERDEPTH  */

	/* write a "plusspace" version of the file, but only write the 
			selected subraster */
	nbytestofile = (w+7)>>3;
	yend = y + h;
	for ( ; y < yend; y++) {
		pixelimage_GetRow(pix, x, y, w, buf);
		plusspace_WriteRow(file, buf, nbytestofile);
	}

	fprintf(file, "\n\\enddata{%s,%d}\n", class_GetTypeName(ras), id);
}
#endif /* NOTUSED */

/* raster__Write(self, file, writeID, level)
	Writes an ASCII data stream for 'self' to the designated 'file'
	Assumes the write has been done if WriteID int self->pix 
		has not changed from the last write.
	Assigns an object identifier for the data object, writes it in the header,
	Returns the assigned object identifier.
*/
	long
raster__Write(self, file, writeID, level)
	 struct raster *self;
	 FILE *file;
	 long writeID;
	 int level;
{
	register struct rasterimage *pix = raster_GetPix(self);
	long id = raster_UniqueID(self);
	if (pix == NULL) {
		return 0;
	}
	if (self->header.dataobject.writeID != writeID) {
		char *name = class_GetTypeName(self);
		long x, y, width, height;	/* subraster parms */

		self->header.dataobject.writeID = writeID;

/* XXX combine with options a bit saying this image is not shared
(so it doesn't need to be put in the dictionary and retained forever) */

		rectangle_GetRectSize(&self->subraster, &x, &y, &width, &height);
		fprintf(file, "\\begindata{%s,%d}\n", name, id);
		fprintf(file, "%ld %ld %ld %ld ", RASTERVERSION, 
				self->options, self->xScale, self->yScale);
		if (rasterimage_GetWriteID(pix) == writeID) {
			/* write a "refer" line */
			fprintf(file, "%ld %ld %ld %ld\n",
				 x, y, width, height);	/* subraster */
			fprintf(file, "refer %ld\n", rasterimage_GetObjectID(pix));
		}
		else if (rasterimage_GetFileName(pix)) {
			/* write a "file" line */
			char *path = rasterimage_GetFilePath(pix);
			if (path == NULL || *path == '\0')
				path = ".";	/* must write something non-white */
			fprintf(file, "%ld %ld %ld %ld\n",
				 x, y, width, height);	/* subraster */
			fprintf(file, "file %d %s %s \n", id, 
				rasterimage_GetFileName(pix), path);

			rasterimage_SetWriteID(pix, writeID);
			rasterimage_SetObjectID(pix, id);
		}
		else if (pix->refcnt == 1) {
			/* write a "bits" version of the file, but only write the 
				selected subraster */
			register long nbytestofile = (width+7)>>3;
			short buf[400];
			register long yend = y + height;

			fprintf(file, "%ld %ld %ld %ld\n",
				 0, 0, width, height);	/* subraster */
			fprintf(file, "bits %ld %ld %ld\n", id, width, height);

			for ( ; y < yend; y++) {
				rasterimage_GetRow(pix, x, y, width, buf);
				rasterio_WriteRow(file, buf, nbytestofile);
			}

			rasterimage_SetWriteID(pix, writeID);
			rasterimage_SetObjectID(pix, id);
		}
		else {
			/* write a "bits" version of the file */
			register unsigned char *bits, *bitsend;
			register long W = rasterimage_GetRowWidth(pix);
			register long nbytestofile = (rasterimage_GetWidth(pix)+7)>>3;

			fprintf(file, "%ld %ld %ld %ld\n",
				 x, y, width, height);	/* subraster */
			fprintf(file, "bits %ld %ld %ld\n", id,
					rasterimage_GetWidth(pix),
					rasterimage_GetHeight(pix));

			bits = rasterimage_GetBitsPtr(pix);
			bitsend = bits + W * rasterimage_GetHeight(pix);
			for ( ;  bits < bitsend; bits += W)
				rasterio_WriteRow(file, bits, nbytestofile);

			rasterimage_SetWriteID(pix, writeID);
			rasterimage_SetObjectID(pix, id);
		}
		fprintf(file, "\\enddata{%s, %d}\n", name, id);
	} /* end writeID != writeID */
	return(id);
}

/* raster__WriteSubRaster(self, file, objectid, sub)
		Write to 'file' the subraster 'sub'.  
		Use the object identifier 'objectid'.
		Returns the objectid.
*/
	long
raster__WriteSubRaster(self, file, objectid, sub)
	struct raster *self;
	register FILE *file;
	long objectid;
	struct rectangle *sub;
{
	register struct rasterimage *pix = raster_GetPix(self);
	char *name = class_GetTypeName(self);
	struct rectangle R;
	long x, y, width, height;
	register long r;		/* count rows while putting */
	long nbytestofile;		/* bytes to output to for each row */
	unsigned short rowbits[BUFBYTES/2];

	if (pix == NULL) return 0;
	rectangle_IntersectRect(&R, &self->subraster, sub);
	rectangle_GetRectSize(&R, &x, &y, &width, &height);

	fprintf(file, "\\begindata{%s,%d}\n", name, objectid);
	fprintf(file, "%ld %ld %ld %ld %ld %ld %ld %ld\n", RASTERVERSION, 
			self->options, self->xScale, self->yScale,
			 0, 0, width, height);	/* subraster is the whole */
	fprintf(file, "bits %ld %ld %ld\n", objectid, width, height);

	nbytestofile = (width+7)>>3;
	for (r = 0; r < height; r++) {
		rasterimage_GetRow(pix, x, y+r, width, rowbits);
		rasterio_WriteRow(file, (unsigned char *)rowbits, nbytestofile);
	}
	fprintf(file, "\\enddata{%s, %d}\n", name, objectid);
	return objectid;
}

/* raster__WriteShare(self, file, sub)
		write a "share" record for the indicated 'subraster' of 'self'
*/
	void
raster__WriteShare(self, file, sub)
	struct raster *self;
	register FILE *file;
	struct rectangle *sub;
{
	register struct rasterimage *pix = raster_GetPix(self);
	char *name = class_GetTypeName(self);
	struct rectangle R;
	long x, y, width, height;

	if (pix == NULL) return;

	rectangle_IntersectRect(&R, &self->subraster, sub);
	rectangle_GetRectSize(&R, &x, &y, &width, &height);

	fprintf(file, "\\begindata{%s, %d}\n", name, 0);
	fprintf(file, "%ld %ld %ld %ld %ld %ld %ld\n", RASTERVERSION, 
			self->options, self->xScale, self->yScale,
			 0, 0, width, height);	/* subraster is the whole */

	fprintf(file, "share %d 0x%lx \n", getpid(), pix);

	fprintf(file, "\\enddata{%s, %d}\n", name, 0);
}


/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *\
 *
 *	READ routines
 *		Major entry points:
 *			raster__Read 
 *			raster__ReadSubRaster
 *
 *	These routines recognize only the be2 raster formats,
 *	versions 1 and 2 and the original ITC RasterFile
\* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* ReadRasterFile(self, file, id)
	reads a file in RasterFile format
	the leading 0xF1 has already been read.

*/
	static long
ReadRasterFile(self, file, id)
	register struct raster  *self;
	FILE  *file;
	long id;
{
	register struct rasterimage *pix = raster_GetPix(self);
	long retval;

	if (pix == NULL) 
		raster_SetPix(self, pix = rasterimage_New());

	ungetc(0xF1, file);
	retval = oldRF_ReadImage(file, pix);

	/* the next line will not change the pixelimage
		because it already has the right size.
		It clears out the options and subraster in the rasterimage */
	rasterimage_SetRO(self->pix,self->readOnly);
	raster_Resize(self, rasterimage_GetWidth(pix), 
				rasterimage_GetHeight(pix));
	rasterimage_NotifyObservers(pix, rasterimage_DATACHANGED);
	return retval;
}


/* ReadV1Raster(self, file)
	Read a version 1 raster image from stream 'file' and store in 'self'.
	The version field has already been read by the _Read routine above.
*/
/* Format of a compressed raster datastream:   (Version 1)

	 Offset is from AFTER datastream marker

	type	what it is
	----	---- -- --

	long	version
	long	options
	long	compression
	long	expansion
	long	xoffset
	long	yoffset
	long	width
	long	height
	long	depth

*/

/* bits of 'options' value */
#define  V1INVERT	(1<<1)
#define  V1FLIP		(1<<2)
#define  V1FLOP		(1<<3)
#define  V1ROTATE	(1<<4)

/* ReadV1Raster(self, file, id)
	Reads a version 1 BE2 raster image from file to self.

	The subraster, options, and expansion/contraction are set
	fromthe values in the record.
*/
	static long
ReadV1Raster(self, file, id)
	register struct raster  *self;
	FILE  *file;
	long id;
{
	register struct rasterimage *pix = raster_GetPix(self);
	unsigned long options;
	long depth, compression, expansion;
	long xoffset, yoffset,  height, width;
	register long row, W;
	register unsigned char *byteaddr;
	register long nbytesfromfile;
	long retval = 0;

	char s[MAXFILELINE + 2];

	fscanf(file, " %u %ld %ld %ld %ld %ld %ld %hd",  
		&options, &compression, &expansion, &xoffset, 
		&yoffset, &width, &height, &depth);

	if (pix == NULL) {
		raster_SetPix(self, pix = rasterimage_Create(width, height));
	}
	else raster_Resize(self, width, height);

	rectangle_SetRectSize(&self->subraster, xoffset, yoffset, 
			width-xoffset, height-yoffset);
	self->options = options>>1;		/* XXX kludge the V1 options are
					shifted over one from the V2 options */

	/* XXX expansion/compression affect only the print size.  Is this right? */
	if (expansion > 1  && compression > 1) {
		long scale;
		scale = (expansion * raster_UNITSCALE / 2) / compression;
		self->xScale = self->yScale = scale;
	}

	W = rasterimage_GetRowWidth(pix);
	nbytesfromfile = (width+7)>>3;
	byteaddr = rasterimage_GetBitsPtr(pix);
	for (row = 0;   row < height;   row++, byteaddr += W) {
		if (plusspace_ReadRow(file, byteaddr, nbytesfromfile) 
				!= dataobject_NOREADERROR) {
			retval = dataobject_BADFORMAT;
			break;
		}
	}
	
	/* If plusspace_ReadRow ever gets to a backslash, it remains at the read point
		(via the magic of ungetc) */
	while ( ! feof(file) && getc(file) != '\\') {};	/* scan for \enddata */
	fgets(s, MAXFILELINE + 2, file);	/* discard \enddata{...}  */
	rasterimage_SetRO(pix,self->readOnly);
	rasterimage_NotifyObservers(pix, rasterimage_DATACHANGED);
	return retval;
}


/* ReadV1SubRaster(self, file, r)
	Reads from 'file' a data stream containing a V1 raster
	and stores it at upper left of subraster 'r' of 'self'

	NOT IMPLEMENTED     XXX
	It now just skips the image in the file 
*/
	static long
ReadV1SubRaster(self, file, r)
	struct raster *self;
	FILE *file;
	register struct rectangle *r;
{
	char s[MAXFILELINE + 2];
	while (getc(file) != '\\') {}
	fgets(s, MAXFILELINE + 2, file);	/* discard \enddata{...}  */	

	return dataobject_OBJECTCREATIONFAILED;
}

/* raster__Read(self, file, id)
		Read from 'file' a data stream for a raster and
		store the raster in 'self'.  'id' is ignored.

		See file format in rasterspec.d.
		This routine reads the \begindata, if any. Its syntax is not checked.
		This routine reads the \enddata, if any. Its syntax is also not checked.
		If the version number read from the file is 1, 
		then the old Read routine is called.

	XXX should check for read errors
*/
	long
raster__Read(self, file, id)
	struct raster  *self;
	register FILE  *file;
	long  id;			/* !0 if data stream, 0 if direct from file*/
{
	register struct rasterimage *pix = raster_GetPix(self);
	long version, width, height;
	register long row, W;
	register unsigned char *byteaddr;
	register nbytesfromfile;
	long options, xscale, yscale, xoffset, yoffset, subwidth, subheight;
	unsigned char keyword[6];
	long objectid;	/* id read for the incoming pixel image */
	unsigned char filename[256], path[1024];
	FILE *f2;		/* for "file" keyword */
	long result = dataobject_BADFORMAT;

	char s[MAXFILELINE + 2];
	long tc;

	self->xScale = self->yScale = raster_UNITSCALE / 2;

	if (file == NULL) return dataobject_PREMATUREEOF;

	/* check for RasterFile magic number */
	ungetc(tc=getc(file), file);
	if (tc == 0xF1) {
		if (pix != NULL) rasterimage_Defile(pix);
		return ReadRasterFile(self, file, id);
	}
	else if (tc == '\\') {
		/* accept "\begindata{raster,123123123}"
			even though it is a mistake */
		long discardid;
		if (fscanf(file, "\\begindata{raster,%ld", &discardid) != 1
				|| getc(file) != '}' || getc(file) != '\n') 
			return dataobject_NOTBE2DATASTREAM;
	}

	/* XXX check for "\begindata{raster," */

	/* check for version 1 */
	fscanf(file, " %ld ", &version);
	if (version == 1) {
		if (pix != NULL) rasterimage_Defile(pix);
		return ReadV1Raster(self, file, id);
	}

	/* it is a be2 version 2 raster image */

	/* read the rest of the first line of header */
	fscanf(file, " %u %ld %ld %ld %ld %ld %ld",  
		&options, &xscale, &yscale, &xoffset, 
		&yoffset, &subwidth, &subheight);

	/* scan to end of line in case this is actually something beyond V2 */
	while (((tc=getc(file)) != '\n') && (tc != '\\') && (tc != EOF)) {}

	/* read the keyword */
	fscanf(file, " %5s", keyword);

	switch (*keyword) {
	case 'r':	{		/* "refer" type */
		struct rasterimage *addr;
		fscanf(file, " %d ", &objectid);

		addr=NULL;/* XXX Lookup the objectid in a namespc.  set addr  */

		if (FALSE  /* XXX object is in namespace */ )  {
			raster_SetPix(self, pix=addr);
			result = dataobject_NOREADERROR;
		}
		else result = dataobject_OBJECTCREATIONFAILED;
	}	break; 

	case 's':	{            /* "share" type */
		long pid;
		struct rasterimage *addr;
		fscanf(file, " %d %x ", &pid, &addr);
		if (pid == getpid()  && strcmp(class_GetTypeName(addr), 
						"rasterimage")==0) {
			raster_SetPix(self, pix=addr);
			result = dataobject_NOREADERROR;
		}
		else result = dataobject_OBJECTCREATIONFAILED;
	}	break;

	case 'f':	/* "file" type */
		/* XXX creating a pix here is bogus
			remove when fix the filename stuff */
		if (pix == NULL)
			raster_SetPix(self, pix = rasterimage_New());
		rasterimage_Defile(pix);
		fscanf(file, "%d %255s %1023s ", &objectid, filename, path);
		f2 = rasterimage_FindFile(pix, filename, path);
		result = raster_Read(self, f2, 0);

		/* XXX BUG BUG   the raster_Read will Defile(self) */

		rasterimage_SetRO(pix,self->readOnly);
		/* XXX enter rasterimage into namespace with objectid as key
		unless the option bit says it is singly referenced */
	
		rasterimage_NotifyObservers(raster_GetPix(self),
				rasterimage_DATACHANGED);
		break;

	case 'b':	/* "bits" type */
		if (strcmp(keyword, "bits") != 0) {
			result = dataobject_BADFORMAT;
			break;
		}
		/* fix this to not Defile ??? XXX */
		if (pix) rasterimage_Defile(pix);
		fscanf(file, " %d %d %d ", &objectid, &width, &height);

		if (width < 1 || height < 1 || width > 1000000 || height > 1000000) {
			result = dataobject_BADFORMAT;
			break;
		}

		if (pix == NULL)
			raster_SetPix(self, pix = rasterimage_Create(width, height));
		else raster_Resize(self, width, height);
		W = rasterimage_GetRowWidth(pix);
		nbytesfromfile = (width+7)>>3;
		byteaddr = rasterimage_GetBitsPtr(pix);
		result = dataobject_NOREADERROR;
		for (row = 0;   row < height;   row++, byteaddr += W) {
			long c = rasterio_ReadRow(file, byteaddr, nbytesfromfile);
			if (c != '|') {
				result = (c == EOF) 
					? dataobject_PREMATUREEOF
					: dataobject_BADFORMAT;
				break;
			}
		}
	
		/* If ReadRow ever gets to a backslash, it remains at the read point
			(via the magic of ungetc) */
		while (! feof(file) && getc(file) != '\\') {};	/* scan for \enddata */
		fgets(s, MAXFILELINE + 2, file);	/* discard \enddata{...}  */
		if (result == dataobject_NOREADERROR &&
				strncmp(s, "enddata{raster,", 
					strlen("enddata{raster,")) != 0) 
			result = dataobject_MISSINGENDDATAMARKER;

		/* XXX enter rasterimage into dictionary with objectid as key 
		unless bit in options says it is singly referenced */
	
		rasterimage_NotifyObservers(pix, rasterimage_DATACHANGED);

		break;

	}  /* end of switch(*keyword) */

	if (result == dataobject_NOREADERROR) {
	rectangle_SetRectSize(&self->subraster, 
		xoffset, yoffset, subwidth, subheight);
		self->options = options;
		self->xScale = xscale;
		self->yScale = yscale;
		rasterimage_SetRO(pix,self->readOnly);
	}
	return result;
}


/* raster__ReadSubRaster(self, file, r)
		Read from 'file' into the upper left corner of subraster 'r' of 'self' 
		See other comments in raster_Read, above.
*/
	long
raster__ReadSubRaster(self, file, r)
	struct raster *self;
	FILE *file;
	struct rectangle *r;
{
	register struct rasterimage *pix = raster_GetPix(self);
	long version, width, height;
	register long nbytesfromfile;
	unsigned short buffer[BUFBYTES/2];
	register long ylim;
	long x, y, w, h;
	long options, xscale, yscale, xoffset, yoffset, subwidth, subheight;
	unsigned char rastertype [6];
	long objectid, fullwidth, fullheight;
	struct rectangle Dest;
	long result = dataobject_NOREADERROR;

	char s[MAXFILELINE + 2];
	
	fscanf(file, " %ld ", &version);
	if (version == 1) 
		return ReadV1SubRaster(self, file, r);

	fscanf(file, " %ld %ld %ld %ld %ld %ld %ld ",
		&options, &xscale, &yscale, &xoffset, &yoffset, &subwidth, &subheight);
	fscanf(file, " %5s %ld %ld %ld ", rastertype, &objectid, &fullwidth, &fullheight);

	if (strcmp(rastertype, "bits") != 0  
			||  fullwidth != subwidth  
			||  fullheight != subheight)
		/* XXX skip unimplemented operations */
		return 0;

	width = subwidth; 
	height = subheight;

	if (pix == NULL)
		raster_SetPix(self, pix = rasterimage_Create(rectangle_Left(r)+width, 
					rectangle_Top(r)+height));
	
	rectangle_SetRectSize(&Dest, rectangle_Left(r), rectangle_Top(r), width, height);
	rectangle_IntersectRect(&Dest, &Dest, r);
	rectangle_IntersectRect(&Dest, &Dest, &self->subraster);
	rectangle_GetRectSize(&Dest, &x, &y, &w, &h);

	nbytesfromfile = (width+7)>>3;	/* amount to read */
	ylim = y + h;
	for ( ;   y < ylim;   y++) {
		long c = rasterio_ReadRow(file, (unsigned char *)buffer, nbytesfromfile);
		if (c != '|') {
			result = (c == EOF) 
				? dataobject_PREMATUREEOF
				: dataobject_BADFORMAT;
			break;
		}
		rasterimage_SetRow(pix, x, y, w, buffer);
	}
	
	/* If ReadRow ever gets to a backslash, it remains at the read point
		(via the magic of ungetc) */
	while ( ! feof(file) && getc(file) != '\\') {};	/* scan for \enddata */
	fgets(s, MAXFILELINE + 2, file);	/* discard \enddata{...}  */
	rasterimage_SetRO(pix,self->readOnly);
	rasterimage_NotifyObservers(pix, rasterimage_DATACHANGED);
	return result;
}

void 
raster__SetAttributes(self, attributes)
  struct raster	    *self;
  struct attributes *attributes;
{
  struct rasterimage *pix = raster_GetPix(self);

  while(attributes) {
    if(!strcmp(attributes->key, "readonly")) {
      self->readOnly = attributes->value.integer;
      if(pix)
	  rasterimage_SetRO(pix,self->readOnly);
    }
    attributes = attributes->next;
  }
}

static int tmpfilectr = 0;

long raster__WriteOtherFormat(self, file, writeID, level, usagetype, boundary)
struct raster *self;
FILE *file;
long writeID;
int level;
int usagetype;
char *boundary;
{
    FILE *tmpfp;
    char Fnam[1000];

    if (self->header.dataobject.writeID == writeID)  return(self->header.dataobject.id);
    self->header.dataobject.writeID = writeID;
    
    fprintf(file, "\n--%s\nContent-type: image/x-xwd\nContent-Transfer-Encoding: base64\n\n", boundary);
    
    sprintf(Fnam, "/tmp/rastxwd.%d.%d", getpid(), tmpfilectr++);
    tmpfp = fopen(Fnam, "w");
    if (!tmpfp) return(0);
    rasterimage_InvertSubraster(self->pix, &self->subraster); /* ugh */
    xwdio_WriteImage(tmpfp, self->pix, &self->subraster);
    rasterimage_InvertSubraster(self->pix, &self->subraster); /* reverse the ugh */
    fclose(tmpfp);
    tmpfp = fopen(Fnam, "r");
    if (!tmpfp) return(0);
    to64(tmpfp, file);
    fclose(tmpfp);
    unlink(Fnam);
    return(self->header.dataobject.id);
}

boolean raster__ReadOtherFormat(self, file, fmt, encoding, desc)
struct raster *self;
FILE *file;
char *fmt;
char *encoding;
char *desc;
{
    char TmpFile[250];
    FILE *tmpfp = NULL;
    int code;
    struct rasterimage *pix;

    if (strcmp(fmt, "image/xwd")
	 && strcmp(fmt, "image/x-xwd")) return(FALSE);
    /* Need to decode base64 or q-p here */
    if (!strncmp(encoding, "base64", 6)
	 || !strncmp(encoding, "quoted-printable", 16)) {
	sprintf(TmpFile, "/tmp/rastxwd.%d.%d", getpid(), tmpfilectr++);
	tmpfp = fopen(TmpFile, "w");
	if (!tmpfp) return(FALSE);
	if (!strncmp(encoding, "base64", 6)) {
	    from64(file, tmpfp);
	} else {
	    fromqp(file, tmpfp);
	}
	fclose(tmpfp);
	tmpfp = fopen(TmpFile, "r");
	if (!tmpfp) return(FALSE);
	file = tmpfp;
    }

    pix = raster_GetPix(self);
    if (pix == NULL) 
	raster_SetPix(self, pix = rasterimage_New());
    code = xwdio_ReadImage(file, raster_GetPix(self));
    if (tmpfp) {
	fclose(tmpfp);
	unlink(TmpFile); 
    }
    if (code == dataobject_NOREADERROR) {
	rasterimage_InvertSubraster(self->pix, &self->subraster);	
	return(TRUE);
    } else {
	return (FALSE);
    }
}
