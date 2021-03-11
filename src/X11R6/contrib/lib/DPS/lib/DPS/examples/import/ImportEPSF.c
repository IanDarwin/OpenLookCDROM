/*
 * $RCSfile: ImportEPSF.c,v $
 *
 * (c) Copyright 1992-1994 Adobe Systems Incorporated.
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

#include "Import.h"

/*************************************************************
**
** FUNCTION:	freeResourceList
**
** DESCRIPTION:	Free the entries in a resource list
**
** PARAMETERS:	type	Pointer to resource list
**
** RETURN:	None
**
*************************************************************/

void freeResourceList(type)
    ResourceType *type;
{
    ResourceType *t;
    Resource *r, *r1;

    while (type != NULL) {
	XtFree((XtPointer) type->name);
	r = type->list;
	while (r != NULL) {
	    XtFree((XtPointer) r->name);
	    if (r->version != NULL) XtFree((XtPointer) r->version);
	    if (r->revision != NULL) XtFree((XtPointer) r->revision);
	    r1 = r->next;
	    XtFree((XtPointer) r);
	    r = r1;
	}
	t = type->next;
	XtFree((XtPointer) type);
	type = t;
    }
} /* end freeResourceList() */

/*************************************************************
**
** FUNCTION:	addResource
**
** DESCRIPTION:	Add a resource to an element's resource list
**
** PARAMETERS:	e		Element
**		type		Type of resource
**		name		Name of resource
**		supplied	Whether resource is included in file
**
** RETURN:	Possibly new resource entry
**
*************************************************************/

static Resource *addResource(e, type, name, supplied)
    Element *e;
    char *type;
    char *name;
    Boolean supplied;
{
    ResourceType *t;
    Resource *r;

    /*
    ** Try to find the resource type, and if it fails add a new type entry
    */
    for (t = e->resources; t != NULL; t = t->next) {
	if (strcmp(t->name, type) == 0) break;
    }

    if (t == NULL) {
	t = XtNew(ResourceType);
	t->name = XtNewString(type);
	t->list = NULL;
	t->next = e->resources;
	e->resources = t;
    }

    /*
    ** Try to find the resource name.  If successful, update the include
    ** flag and return the resource entry
    */
    for (r = t->list; r != NULL; r = r->next) {
	if (strcmp(r->name, name) == 0) {
	    if (supplied) r->included = True;
	    return r;
	}
    }

    /*
    ** Add a new resource entry
    */
    r = XtNew(Resource);
    r->name = XtNewString(name);
    r->version = r->revision = NULL;
    r->included = supplied;
    r->next = t->list;
    t->list = r;
    return r;
} /* end addResource() */

/*************************************************************
**
** FUNCTION:	addResources
**
** DESCRIPTION:	Parse a buffer for resource names and add them
**		to the resources for an element
**
** PARAMETERS:	e		Element to add resources to
**		type		Type of resource
**		buf 		Buffer holding resource names
**		supplied	Whether resources are included in file
**
** RETURN:	None
**
*************************************************************/

static void addResources(e, type, buf, supplied)
    Element *e;
    char *type;
    char *buf;
    Boolean supplied;
{
    char *ch, savech;
    Resource *r;

    while (1) {
	/*
	** Find the next resource name in the buffer
	*/
	while (*buf == ' ' || *buf == '\t') buf++;
	if (*buf == '\0' || *buf == '\n') return;
	ch = buf;
	while (*ch != ' ' && *ch != '\t' && *ch != '\n' && *ch != '\0') ch++;
	savech = *ch;
	*ch = '\0';

	/*
	** Add it to the resources for the element
	*/
	r = addResource(e, type, buf, supplied);
	*ch = savech;

	/*
	** If the type is ProcSet, the resource name is followed by
	** the version and revision
	*/
	if (strcmp(type, "ProcSet") == 0) {
	    buf = ch;
	    while (*buf == ' ' || *buf == '\t') buf++;	/* skip white space */
	    if (*buf == '\0' || *buf == '\n') return;
	    ch = buf;
	    /* skip version */
	    while (*ch != ' ' && *ch != '\t' &&
		   *ch != '\n' && *ch != '\0') ch++;
	    savech = *ch;
	    *ch = '\0';
	    r->version = XtNewString(buf);
	    *ch = savech;

	    buf = ch;
	    while (*buf == ' ' || *buf == '\t') buf++;	/* skip white space */
	    if (*buf == '\0' || *buf == '\n') return;
	    ch = buf;
	    /* skip revision */
	    while (*ch != ' ' && *ch != '\t' &&
		   *ch != '\n' && *ch != '\0') ch++;
	    savech = *ch;
	    *ch = '\0';
	    r->revision = XtNewString(buf);
	    *ch = savech;
	}
	buf = ch;
    }
} /* end addResources() */

/*
** List of resources known to be resident in the server
*/
static ResourceType *residentResources = NULL;

/*************************************************************
**
** FUNCTION:	markResident
**
** DESCRIPTION:	Mark a resource as being resident in the server
**
** PARAMETERS:	type	Type of resource
**		name	Name of resource
**
** RETURN:	None
**
*************************************************************/

static void markResident(type, name)
    char *type, *name;
{
    ResourceType *t;
    Resource *r;

    /*
    ** Look for resource type in list of resident resources; add
    ** if necessary
    */
    for (t = residentResources; t != NULL; t = t->next) {
	if (strcmp(type, t->name) == 0) break;
    }
    if (t == NULL) {
	t = XtNew(ResourceType);
	t->name = XtNewString(type);
	t->next = residentResources;
	t->list = NULL;
	residentResources = t;
    }

    /*
    ** Add a new resource to the list for the type
    */
    r = XtNew(Resource);
    r->name = XtNewString(name);
    r->version = r->revision = NULL;
    r->next = t->list;
    t->list = r;
} /* end markResident() */

/*************************************************************
**
** FUNCTION:	residentFont
**
** DESCRIPTION:	Checks if a font name is resident in the server
**
** PARAMETERS:	name	Name of font to check
**
** RETURN:	Whether font is resident
**
*************************************************************/

static Boolean residentFont(name)
    char *name;
{
    ResourceType *t;
    Resource *r;
    int resident;

    /*
    ** First check our list for a FontOutline resource with this name
    */
    for (t = residentResources; t != NULL; t = t->next) {
	if (strcmp("FontOutline", t->name) == 0) break;
    }
    if (t != NULL) {
	for (r = t->list; r != NULL; r = r->next) {
	    if (strcmp(r->name, name) == 0) return True;
	}
    }
    /*
    ** We don't know yet whether it's resident, so ask the server.
    ** Store the result in our list.
    */
    PSWCheckFontResident(AppData.imageCtxt, name, &resident);
    if (resident) markResident("FontOutline", name);
    return resident;
} /* end residentFont() */

/*************************************************************
**
** FUNCTION:	residentResource
**
** DESCRIPTION:	Checks whether a resource is resident in the server.
**
** PARAMETERS:	type	Type of resource
**		name	Name of resource
**
** RETURN:	None
**
*************************************************************/

static Boolean residentResource(type, name)
    char *type, *name;
{
    ResourceType *t;
    Resource *r;

    /*
    ** Look in our list.  If it's not there, assume not resident
    */
    for (t = residentResources; t != NULL; t = t->next) {
	if (strcmp(type, t->name) == 0) break;
    }
    if (t == NULL) return False;
    for (r = t->list; r != NULL; r = r->next) {
	if (strcmp(name, r->name) == 0) return True;
    }
    return False;
} /* end residentResource() */

/*************************************************************
**
** FUNCTION:	downloadResource
**
** DESCRIPTION:	Store a resource in the interpreter
**
** PARAMETERS:	type	Type of resource
**		name	Name of resource
**
** RETURN:	Whether resource was successfully downloaded
**
*************************************************************/

static Boolean downloadResource(type, name)
    char *type, *name;
{
    int numFiles;
    char **names, **files;
    FILE *f;
#define BUFLEN 256
    char buf[BUFLEN];

    /*
    ** Try to find the name of a file that defines this resource.
    */
    numFiles = ListPSResourceFiles(NULL, ".", type, name,
				   &names, &files);
    if (numFiles == 0) return False;

    /*
    ** Try to open the resource file
    */
    f = fopen(files[0], "r");
    if (f == NULL) {
	free(names);
	free(files);
	return False;
    }

    /*
    ** Switch to shared VM and write contents of file to the server
    */
    DPSPrintf(AppData.imageCtxt, "\ncurrentshared true setshared\n");
    while (fgets(buf, BUFLEN, f) != NULL) {
	DPSWritePostScript(AppData.imageCtxt, buf, strlen(buf));
    }
    DPSPrintf(AppData.imageCtxt, "\nsetshared\n");
    fclose(f);
    free(names);
    free(files);
    /*
    ** Mark the resource as being resident for future tests
    */
    markResident(type, name);
    return True;
#undef BUFLEN
} /* end downloadResource() */

/*************************************************************
**
** FUNCTION:	downloadResources
**
** DESCRIPTION:	Downloads all the resources in a list to the interpreter
**
** PARAMETERS:	t	List of resources
**
** RETURN:	None
**
*************************************************************/

static void downloadResources(t)
    ResourceType *t;
{
    Resource *r;

    for (/* */; t != NULL; t = t->next) {
	if (strcmp (t->name, "FontOutline") == 0) {
	    /*
	    ** If a font resource, try to download.  But if
	    ** unsuccessful just go on, since font substitution
	    ** will occur
	    */
	    for (r = t->list; r != NULL; r = r->next) {
		if (r->included) continue;
		if (!residentFont(r->name)) {
		    (void) downloadResource(t->name, r->name);
		}
	    }
	} else {
	    /*
	    ** For other types, issue a warning if the download is
	    ** unsuccessful.  File execution will probably fail, but
	    ** continue on in case the file was lying about whether the
	    ** resource was included
	    */
	    for (r = t->list; r != NULL; r = r->next) {
		if (r->included) continue;
		if (!residentResource(t->name, r->name)) {
		    if (!downloadResource(t->name, r->name)) {
			fprintf(stderr, "Warning, resource type %s named %s\n",
				t->name, r->name);
			fprintf(stderr,
			  "  could not be found.  File execution may fail.\n");
		    }
		}
	    }
	}
    }
} /* end downloadResources() */

/*
** The types of resource comments understood by this app.
*/
typedef enum {fonts, neededFonts, suppliedFonts,
	      procSet, neededProcSet, suppliedProcSet, other} CommentType;

typedef struct {
    char *comment;	/* Comment string */
    int len;		/* Length of comment string */
    CommentType type;	/* What this comment string is conveying */
    char *resourceName;	/* The PSres name for this type of resource */
    Boolean supplied;	/* Does the comment imply the resource is included? */
} ResourceComment;

ResourceComment commentTable[] = {
 {"%%DocumentFonts:", 0, fonts, "FontOutline", False},
 {"%%DocumentNeededFonts:", 0, neededFonts, "FontOutline", False},
 {"%%DocumentSuppliedFonts:", 0, suppliedFonts, "FontOutline", True},
 {"%%DocumentProcSets:", 0, procSet, "ProcSet", False},
 {"%%DocumentNeededProcSets:", 0, neededProcSet, "ProcSet", False},
 {"%%DocumentSuppliedProcSets:", 0, suppliedProcSet, "ProcSet", True},
 {NULL, 0, other, NULL, False}
};

/*************************************************************
**
** FUNCTION:	parseFileHeader
**
** DESCRIPTION:	Parse the header of an EPS file for BoundingBox
**		and resource comments
**
** PARAMETERS:	e	Element describing EPS file
**
** RETURN:	Whether a bounding box comment was found
**
*************************************************************/

Boolean parseFileHeader(e)
    Element *e;
{
#define BBOXLEN 14		/* Length of "%%BoundingBox:" */
#define BEGINDOCUMENTLEN 15	/* Length of "%%BeginDocument" */
#define BEGINBINARYLEN 14	/* Length of "%%BeginBinary:" */
#define BUFLEN 257
    char buf[BUFLEN];
    char buf2[BUFLEN];
    Boolean atend = False;	/* Found an (atend) comment */
    Boolean foundBBox = False;
    CommentType lastLine = other;
    float llx, lly, urx, ury;
    int n;
    int nestingLevel = 0;
    unsigned long binaryCount = 0;
    int len;
    ResourceComment *res;

    /*
    ** If this is the first time through, fill in the length fields
    ** in the comment table.
    */
    if (commentTable[0].len == 0) {
	for (res = commentTable; res->comment != NULL; res++) {
	    res->len = strlen(res->comment);
	}
    }

    while (1) {
	/*
	** If EOF, we're done
	*/
	if (fgets(buf, BUFLEN, e->f) == NULL) {
	    goto SUCCESS;
	}	

	len = strlen(buf);

	/*
	** If in binary data ignore line and decrement binary count
	*/
	if (binaryCount != 0) {
	    if (len > binaryCount) binaryCount = 0;
	    else binaryCount -= len;
	    continue;
	}

	/*
	** Line too long?  Give error message and abort
	*/
	if (len == BUFLEN-1 && buf[BUFLEN-1] != '\n') {
	    fprintf(stderr, "Line too long reading file %s\n", e->filename);
	    buf[79] = '\0';
	    fprintf(stderr, "Offending line begins\n%s\n", buf);
	    goto FAILURE;
	}

	/*
	** See if we're beginning binary data
	*/
	if (strncmp(buf, "%%BeginBinary:", BEGINBINARYLEN) == 0) {
	    n = sscanf(buf, "%%%%BeginBinary: %lu", &binaryCount);
	    if (n != 1) binaryCount = 0;	/* Malformed comment */

	/*
	** Check for begin/end document comments.  We ignore anything in
	** nested documents
	*/
	} else if (strncmp(buf, "%%BeginDocument", BEGINDOCUMENTLEN) == 0) {
	    nestingLevel++;

	} else if (strcmp(buf, "%%EndDocument\n") == 0) {
	    nestingLevel--;

	/*
	** If nesting level is 0, look for more comments
	*/
	} else if (nestingLevel == 0) {
	    /*
	    ** If we haven't already hit an (atend), the end of the
	    ** comments is a good place to stop looking
	    */
	    if (!atend && (strcmp(buf, "%%EndComments\n") == 0)) {
		goto SUCCESS;
	    }

	    /*
	    ** If this is a continued line, and the previous comment
	    ** was a resource line, add more resources
	    */
	    if (strncmp(buf, "%%+", 3) == 0) {
		if (lastLine != other) {
		    if (buf[3] != '\0' && buf[3] != '\n') {
			for (res = commentTable; res->type != lastLine;
			     res++) {}
			addResources(e, res->resourceName,
				     buf+3, res->supplied);
		    }
		}
		continue;
	    }

	    /*
	    ** See if this is a resource comment.  If it's (atend), set flag
	    ** to indicate that we have to keep going to the end of the file
	    */
	    for (res = commentTable; res->comment != NULL; res++) {
		if (strncmp(buf, res->comment, res->len) == 0) {
		    /*
		    ** Success; add resources
		    */
		    lastLine = res->type;
		    n = sscanf(buf + res->len, "%s", buf2);
		    if (n == 1 && strcmp(buf2, "(atend)") == 0) {
			atend = True;
		    } else if (n > 0) {
			addResources(e, res->resourceName,
				     buf + res->len, res->supplied);
		    }
		    goto LOOPEND;
		}
	    }

	    /*
	    ** Current line was not a resource comment.  If the next line is
	    ** a continuation, we need to know to ignore it
	    */
	    lastLine = other;

	    /*
	    ** Check for a bounding box comment
	    */
	    if (strncmp(buf, "%%BoundingBox:", BBOXLEN) == 0) {
		n = sscanf(buf, "%%%%BoundingBox: %f %f %f %f",
			       &llx, &lly, &urx, &ury);

		/*
		** If we didn't find four numbers, check for (atend)
		*/
		if (n != 4) {
		    n = sscanf(buf, "%%%%BoundingBox: %7s", buf2);

		    if (n == 1 && strcmp(buf2, "(atend)") == 0) {
			atend = True;
		    } else {
			fprintf(stderr, "Malformed %%%%BoundingBox comment\
in file %s:\n%s\n", e->filename, buf);
			goto FAILURE;
		    }
		} else {
		    foundBBox = True;
		    /*
		    ** Convert bounding box to integers, which they should
		    ** be but sometimes aren't
		    */
		    e->origBBox.ll.x = floor(llx);
		    e->origBBox.ll.y = floor(lly);
		    e->origBBox.ur.x = ceil(urx);
		    e->origBBox.ur.y = ceil(ury);
		}	
	    }
	}
LOOPEND: ;
    }

SUCCESS:
    /*
    ** Seem to be successful, but make sure we found a bounding box comment.
    ** Download any resources required by the file
    */
    if (foundBBox) {
	downloadResources(e->resources);
	return True;
    } else fprintf(stderr,
		   "Missing bounding box comment in file %s\n", e->filename);
FAILURE:
    freeResourceList(e->resources);
    return False;
#undef BUFLEN
#undef BBOXLEN
#undef BEGINDOCUMENTLEN
#undef BEGINBINARYLEN
} /* end parseFileHeader() */

/*************************************************************
**
** FUNCTION:	imageFile
**
** DESCRIPTION:	Execute an EPS file into the imaging context
**
** PARAMETERS:	e	Element to execute
**
** RETURN:	None
**
*************************************************************/

Boolean imageFile(e)
    Element *e;
{
#define BUFSIZE 256
#define EXECLEN 6		/* Length of "\nexec\n" */
    char buf[BUFSIZE];
    static char eobuf[] = "/execSuccess true def\n\
stop\n\
Magic end of data line )))))))))) 99#2 2#99 <xyz> // 7gsad,32h4ghNmndFgj2\n";
    static char restorebuf[] =
	    "\nEPSFsave restore\n";
    int err;

    /*
    ** Reset to beginning of file
    */
    rewind(e->f);

    /*
    ** Prepare to execute PostScript code
    */
    PSWBeginEPSF(AppData.imageCtxt);
    DPSWritePostScript(AppData.imageCtxt, "\nexec\n", EXECLEN);

    /*
    ** Copy file to context
    */
    while (fgets(buf, BUFSIZE, e->f) != NULL) {
	DPSWritePostScript(AppData.imageCtxt, buf, strlen(buf));
    }

    /*
    ** Mark the end of the data stream
    */
    DPSWritePostScript(AppData.imageCtxt, eobuf, strlen(eobuf));

    /*
    ** Check the results of the imaging:  Get the error status and restore the
    ** context
    */
    PSWEndEPSF(AppData.imageCtxt, &err);

    /*
    ** Can't do this is a wrap because of restore semantics
    */
    DPSWritePostScript(AppData.imageCtxt, restorebuf, strlen(restorebuf));

    return err;
#undef EXECLEN
#undef BUFSIZE
} /* end imageFile() */

/*************************************************************
**
** FUNCTION:	computeBBox
**
** DESCRIPTION:	Compute the bounding box for an element taking current
**		scale and translation into account
**
** PARAMETERS:	e	Element
**
** RETURN:	llx	lower-left x coordinate
**		lly	lower-left y coordinate
**		urx	upper-right x coordinate
**		ury	upper-right y coordinate
**
*************************************************************/

void computeBBox(e, llx, lly, urx, ury)
    Element *e;
    int *llx, *lly, *urx, *ury;
{
    BBox b;
    Point ll, lr, ul, ur;
    float r, theta;

    /*
    ** Create a bounding box scaled like the element
    */
    b.ll.x = 0;
    b.ll.y = 0;
    b.ur.x = (e->origBBox.ur.x - e->origBBox.ll.x) * e->sx;
    b.ur.y = (e->origBBox.ur.y - e->origBBox.ll.y) * e->sy;

    /*
    ** For each corner of the box, rotate and translate
    */
    ll.x = e->tx;
    ll.y = e->ty;

    lr.x = (b.ur.x) * cos(DTOR(e->rotation)) + e->tx;
    lr.y = (b.ur.x) * sin(DTOR(e->rotation)) + e->ty;

    ul.x = (b.ur.y) * cos(DTOR(e->rotation + 90.0)) + e->tx;
    ul.y = (b.ur.y) * sin(DTOR(e->rotation + 90.0)) + e->ty;

    if (b.ur.x == 0.0 && b.ur.y == 0.0) {
	ur.x = e->tx;
	ur.y = e->ty;
    } else {
	r = sqrt((b.ur.x) * (b.ur.x) + (b.ur.y) * (b.ur.y));
	theta = atan2((b.ur.y), (b.ur.x));
	ur.x = r * cos(theta + DTOR(e->rotation)) + e->tx;
	ur.y = r * sin(theta + DTOR(e->rotation)) + e->ty;
    }

    /*
    ** Return values are the extremes of the four points
    */
    *llx = floor(ll.x);
    *llx = MIN(*llx, floor(lr.x));
    *llx = MIN(*llx, floor(ul.x));
    *llx = MIN(*llx, floor(ur.x));

    *lly = floor(ll.y);
    *lly = MIN(*lly, floor(lr.y));
    *lly = MIN(*lly, floor(ul.y));
    *lly = MIN(*lly, floor(ur.y));

    *urx = ceil(ll.x);
    *urx = MAX(*urx, ceil(lr.x));
    *urx = MAX(*urx, ceil(ul.x));
    *urx = MAX(*urx, ceil(ur.x));

    *ury = ceil(ll.y);
    *ury = MAX(*ury, ceil(lr.y));
    *ury = MAX(*ury, ceil(ul.y));
    *ury = MAX(*ury, ceil(ur.y));
} /* end computeBBox() */

/*
** Data needed to write an output file into a buffer
*/
typedef struct {
    char *buf;		/* Current output buffer */
    char *currentChar;	/* Where to add more data */
    int bufLen;		/* The length of the buffer so far */
    int bufSize;	/* The allocated size */
} CopyData;

/*************************************************************
**
** FUNCTION:	copyFunc
**
** DESCRIPTION:	Function to copy its data to a buffer.  When
**		passed to writePictureToFile, this makes the output
**		go into the buffer
**
** PARAMETERS:	buf		Null-terminated string to write
**		clientData	Pointer to CopyData structure
**
** RETURN:	None
**
*************************************************************/

static void copyFunc(buf, clientData)
    char *buf;
    char *clientData;
{
    CopyData *d = (CopyData *) clientData;
    int len = strlen(buf);

    len = strlen(buf);
    if (len + d->bufLen >= d->bufSize) {
	d->bufSize += 1000;
	d->buf = XtRealloc(d->buf, d->bufSize);
    }
    strcpy(d->currentChar, buf);
    d->currentChar += len;
    d->bufLen += len;
} /* end copyFunc() */

/*************************************************************
**
** FUNCTION:	convertToEPS
**
** DESCRIPTION:	Convert the element to an EPS format buffer
**
** PARAMETERS:	e		Element to write
**		doPreview	Whether to write a preview bitmap
**
** RETURN:	EPS buffer
**
*************************************************************/

char *convertToEPS(e, doPreview)
    Element *e;
    Boolean doPreview;
{
    int llx, lly, urx, ury;
    int length, width, height, size, lines;
    CopyData d;

    /*
    ** Find the bounding box of the element
    */
    computeBBox(e, &llx, &lly, &urx, &ury);

    /*
    ** Figure out how large to make the buffer
    */
    length = e->length + 1000;
    if (doPreview) {
	width = urx - llx;
	height = ury - lly;
	if (AppData.deepPreview) size = width * 2;
	else size = (width + 7) / 8 * 2;
	lines = size + 249 / 250;
	length += height * lines * (size > 250 ? 253 : size + 3);
    }

    d.buf = (char *) XtMalloc(length);
    d.currentChar = d.buf;
    d.bufLen = 0;
    d.bufSize = length;

    writePictureToFile(copyFunc, (char *) &d, e, doPreview);

    return d.buf;
} /* end convertToEPS() */

/*************************************************************
**
** FUNCTION:	findPictureBBox
**
** DESCRIPTION:	Find the bounding box for the picture.  If singl
**		is non-null, just do that one element
**
** PARAMETERS:	singl	If non-null, just do this element
**
** RETURN:	llx	lower-left x coordinate
**		lly	lower-left y coordinate
**		urx	upper-right x coordinate
**		ury	upper-right y coordinate
**
*************************************************************/

static void findPictureBBox(llx, lly, urx, ury, singl)
    int *llx, *lly, *urx, *ury;
    Element *singl;
{
    Element *e;
    int a, b, c, d;

    if (singl) computeBBox(singl, llx, lly, urx, ury);
    else {
	/*
	** Find bbox for first element
	*/
	computeBBox(AppData.elements, llx, lly, urx, ury);

	/*
	** Update with bboxes of additional elements
	*/
	for (e = AppData.elements->next; e != NULL; e = e->next) {
	    computeBBox(e, &a, &b, &c, &d);
	    if (a < *llx) *llx = a;
	    if (b < *lly) *lly = b;
	    if (c > *urx) *urx = c;
	    if (d > *ury) *ury = d;
	}
    }
} /* end findPictureBBox() */

/*************************************************************
**
** FUNCTION:	executeElement
**
** DESCRIPTION:	Execute an element
**
** PARAMETERS:	e	element to execute
**		llx	x coords of lower left corner of bbox
**		llx	y coords of lower left corner of bbox
**
** RETURN:	None
**
*************************************************************/

static void executeElement(e, llx, lly)
    Element *e;
    int llx, lly;
{
    PSWTransformBeforeEPSF(AppData.imageCtxt, e->tx - llx, e->ty - lly,
			   e->sx, e->sy, e->rotation,
			   -e->origBBox.ll.x, -e->origBBox.ll.y);
    (void) imageFile(e);
} /* end executeElement() */

/*************************************************************
**
** FUNCTION:	addResourcesToList
**
** DESCRIPTION:	Merge an element's resources into a resource list
**
** PARAMETERS:	e	Element to add
**		base	List of resources so far
**
** RETURN:	None
**
*************************************************************/

static void addResourcesToList(e, base)
    Element *e;
    ResourceType **base;
{
    ResourceType *t, *t1;
    Resource *r, *r1;
    
    /*
    ** Go through element's resources and add each one to the combined
    ** list if it's not there already
    */
    for (t = e->resources; t != NULL; t = t->next) {
	for (t1 = *base; t1 != NULL; t1 = t1->next) {
	    if (strcmp(t->name, t1->name) == 0) break;
	}
	if (t1 == NULL) {
	    t1 = XtNew(ResourceType);
	    t1->name = XtNewString(t->name);
	    t1->list = NULL;
	    t1->next = *base;
	    *base = t1;
	}
	for (r = t->list; r != NULL; r = r->next) {
	    if (r->included) continue;
	    for (r1 = t1->list; r1 != NULL; r1 = r1->next) {
		if (strcmp(r->name, r1->name) == 0) break;
	    }
	    if (r1 == NULL) {
		r1 = XtNew(Resource);
		r1->name = XtNewString(r->name);
		r->version = r->revision = NULL;
		r1->included = False;
		r1->next = t1->list;
		t1->list = r1;
	    }
	}
    }
} /* end addResourcesToList() */

/*************************************************************
**
** FUNCTION:	writeResources
**
** DESCRIPTION:	Write comments describing resource requirements
**
** PARAMETERS:	writeFunc	Function to output data
**		data		Client data for writeFunc
**		singl		If non-null, just write resources
**				for this element
**
** RETURN:	None
**
*************************************************************/

static void writeResources(writeFunc, data, singl)
    void (*writeFunc)();
    char *data;
    Element *singl;
{
    ResourceType *t = NULL;
    Resource *r;
    Element *e;
    Boolean first, procSet;
    char *str;
    int i;

    /*
    ** Create resource list.  If singl, just use element's list;
    ** otherwise create a merged list
    */
    if (singl) t = singl->resources;
    else {
	for (e = AppData.elements; e != NULL; e = e->next) {
	    addResourcesToList(e, &t);
	}
    }

    /*
    ** Go through list and write comments
    */
    while (t != NULL) {
	for (i = 0; i < 2; i++) {
	    first = True;

	    /*
	    ** Figure out which string to write to introduce comments
	    */
	    if (strcmp(t->name, "FontOutline") == 0) {
		procSet = False;
		if (i == 0) str = "%%DocumentFonts: ";
		else str = "%%DocumentNeededFonts: ";
	    } else {
		procSet = True;
		if (i == 0) str = "%%DocumentProcSets: ";
		else str = "%%DocumentNeededProcSets: ";
	    }
		
	    /*
	    ** Write the comments for this resource type
	    */
	    for (r = t->list; r != NULL; r = r->next) {
		if (r->included) continue;

		if (first) (*writeFunc)(str, data);
		else (*writeFunc)("%%+ ", data);
		(*writeFunc)(r->name, data);
		if (procSet) {
		    (*writeFunc)(" ", data);
		    (*writeFunc)(r->version, data);
		    (*writeFunc)(" ", data);
		    (*writeFunc)(r->revision, data);
		}
		(*writeFunc)("\n", data);
		first = False;
	    }
	}
	t = t->next;
    }
    /*
    ** If we created a merged list, free it
    */
    if (singl == NULL) freeResourceList(t);
} /* end writeResources() */

/*************************************************************
**
** FUNCTION:	writePreview
**
** DESCRIPTION:	Write preview comments from X image data
**
** PARAMETERS:	im		X image data
**		writeFunc	Function to output data
**		data		Client data for writeFunc
**
** RETURN:	None
**
*************************************************************/

static void writePreview(im, writeFunc, data)
    XImage *im;
    void (*writeFunc)();
    char *data;
{
    int size;
    int lines = 1;
    register int i, j;
    int chars;
    int pixel;
    int accum;
    char buf[257];

    /*
    ** Compute characters per line of preview and number of lines
    */
    if (im->depth == 1) size = (im->width + 7) / 8 * 2;
    else size = im->width * 2;

    lines = (size + 249) / 250;

    /*
    ** Write BeginPreview comment
    */
    sprintf(buf, "%%%%BeginPreview: %d %d %d %d", im->width, im->height,
	    im->depth, lines * im->height);
    (*writeFunc)(buf, data);
    
    /*
    ** If depth is 8, write out two hex digits for each pixel
    */
    if (im->depth == 8) {
	/*
	** Go through lines of image
	*/
	for (i = 0; i < im->height; i++) {
	    (*writeFunc)("\n%", data);
	    chars = 0;
	    /*
	    ** For each line, go through pixels.  Make sure lines don't
	    ** get too long
	    */
	    for (j = 0; j < im->width; j++) {
		if (chars > 250) {
		    (*writeFunc)("\n%", data);
		    chars = 0;
		}
		pixel = XGetPixel(im, j, i);
		sprintf(buf, "%02x", pixel);
		(*writeFunc)(buf, data);
		chars += 2;
	    }
	}
    /*
    ** If depth is 8, combine sets of 8 pixels into an integer and write as
    ** two hex digits
    */
    } else {
	/*
	** Go through lines of image
	*/
	for (i = 0; i < im->height; i++) {
	    (*writeFunc)("\n%", data);
	    chars = 0;
	    accum = 0;
	    /*
	    ** For each line, go through pixels.  Make sure lines don't
	    ** get too long
	    */
	    for (j = 0; j < im->width; j++) {
		if (chars > 250) {
		    (*writeFunc)("\n%", data);
		    chars = 0;
		}
		pixel = XGetPixel(im, j, i);
		accum = (accum << 1) + pixel;
		if (((j + 1) % 8) == 0) {
		    sprintf(buf, "%02x", accum);
		    (*writeFunc)(buf, data);
		    chars += 2;
		    accum = 0;
		}
	    }
	    /*
	    ** If there are leftover pixels, shift left and write
	    */
	    if ((im->width % 8) != 0) {
		accum = accum << 8 - (im->width % 8);
		if (chars > 250) (*writeFunc)("\n", data);
		sprintf(buf, "%02x", accum);
		(*writeFunc)(buf, data);
	    }
	}
    }
    (*writeFunc)("\n%%EndPreview\n", data);
    XDestroyImage(im);
} /* end writePreview() */

/*************************************************************
**
** FUNCTION:	writeElement
**
** DESCRIPTION:	Copy an element to the output file
**
** PARAMETERS:	writeFunc	Function to output data
**		data		Client data for writeFunc
**		e		Element to write
**
** RETURN:	None
**
*************************************************************/

static void writeElement(writeFunc, data, e)
    void (*writeFunc)();
    char *data;
    Element *e;
{
#define BUFSIZE 257
    char linebuf[BUFSIZE];
    BBox *b = &e->origBBox;

    /*
    ** Write the comments to prepare for an EPS file and transform the
    ** coordinate system
    */
    (*writeFunc)("BeginEPSF\n", data);
    sprintf(linebuf, "%g %g translate %g rotate %g %g scale %g %g translate\n",
	    e->tx, e->ty, e->rotation, e->sx, e->sy, -b->ll.x, -b->ll.y);
    (*writeFunc)(linebuf, data);
    sprintf(linebuf, "%g %g moveto %g %g lineto %g %g lineto %g %g lineto\n",
	    b->ll.x, b->ll.y, b->ur.x, b->ll.y,
	    b->ur.x, b->ur.y, b->ll.x, b->ur.y);
    (*writeFunc)(linebuf, data);
    (*writeFunc)("closepath clip newpath\n", data);
    sprintf(linebuf, "%%%%BeginDocument: %s\n", e->filename);
    (*writeFunc)(linebuf, data);
    
    /*
    ** Copy the file to the output
    */
    rewind(e->f);
    while (fgets(linebuf, BUFSIZE, e->f) != NULL) {
	(*writeFunc)(linebuf, data);
    }

    /*
    ** End the document
    */
    (*writeFunc)("\n%%EndDocument\nEndEPSF\n", data);
#undef BUFSIZE
} /* end writeElement() */

/*
** Document prolog for inclusion in output
*/
static char prolog[] = "\
%%BeginProlog\n\
/BeginEPSF {\n\
  /b4_Inc_state save def\n\
  /dict_count countdictstack def\n\
  /op_count count 1 sub def\n\
  userdict begin\n\
  /showpage {} def\n\
  0 setgray 0 setlinecap\n\
  1 setlinewidth 0 setlinejoin\n\
  10 setmiterlimit [] 0 setdash newpath\n\
  /languagelevel where\n\
  {pop languagelevel\n\
  1 ne\n\
    {false setstrokeadjust false setoverprint\n\
    } if\n\
  } if\n\
} bind def\n\
\n\
/EndEPSF {\n\
  count op_count sub {pop} repeat\n\
  countdictstack dict_count sub {end} repeat\n\
  b4_Inc_state restore\n\
} bind def\n\
%%EndProlog\n\
%%Page: 1 1\n\
/pagesave save def\n";

/*************************************************************
**
** FUNCTION:	writeFile
**
** DESCRIPTION:	Write an output file
**
** PARAMETERS:	im		Preview image, if a preview is needed
**		writeFunc	Function to output data
**		data		Client data for writeFunc
**		singl		If non-NULL, just write this element
**		llx lly urx ury	Bounding box of output
**		doPreview	Whether to include a preview bitmap
**
** RETURN:	None
**
*************************************************************/

static void writeFile(im, writeFunc, data, singl,
		      llx, lly, urx, ury, doPreview)
    XImage *im;
    void (*writeFunc)();
    char *data;
    Element *singl;
    int llx, lly, urx, ury;
    Boolean doPreview;
{
    Element *e;
    char buf[257];

    /*
    ** Write file header including bounding box
    */
    (*writeFunc)("%!PS-Adobe-3.0\n%%BoundingBox: ", data);
    sprintf(buf, "%d %d %d %d\n", llx, lly, urx, ury);
    (*writeFunc)(buf, data);

    /*
    ** Write resource requirement to output
    */
    writeResources(writeFunc, data, singl);
    (*writeFunc)("%%Pages: 1\n%%EndComments\n", data);

    /*
    ** If doing a preview, write it
    */
    if (doPreview) writePreview(im, writeFunc, data);

    /*
    ** Write the document prolog
    */
    (*writeFunc)(prolog, data);

    /*
    ** Write the element(s) out
    */
    if (singl) writeElement(writeFunc, data, singl);
    else {
	for (e = AppData.lastElement; e != NULL; e = e->prev) {
	    writeElement(writeFunc, data, e);
	}
    }

    /*
    ** Finish the document
    */
    (*writeFunc)("pagesave restore showpage\n%%EOF\n", data);
} /* end writeFile() */

/*************************************************************
**
** FUNCTION:	writePictureToFile
**
** DESCRIPTION:	Write an output file
**
** PARAMETERS:	writeFunc	Function to output data
**		data		Client data for writeFunc
**		singl		If non-NULL, just write this element
**		doPreview	Whether to include a preview bitmap
**
** RETURN:	None
**
*************************************************************/

void writePictureToFile(writeFunc, data, singl, doPreview)
    void (*writeFunc)();
    char *data;
    Element *singl;
    Boolean doPreview;
{
    int llx, lly, urx, ury;
    Point pt;
    XPoint xpt1, xpt2;
    int width, height;
    Pixmap p;
    Element *e;
    XImage *im;
    Display *dpy = XtDisplay(AppData.drawingArea);

    XDefineCursor(XtDisplay(AppData.drawingArea),
		  XtWindow(AppData.drawingArea), AppData.busyCursor);

    /*
    ** Find the bounding box of the output
    */
    findPictureBBox(&llx, &lly, &urx, &ury, singl);

    /*
    ** If doing a preview, compute preview image size
    */
    if (doPreview) {
	pt.x = llx;
	pt.y = lly;
	convertToX(&xpt1, &pt);
	pt.x = urx;
	pt.y = ury;
	convertToX(&xpt2, &pt);
	width = xpt2.x - xpt1.x + 1;
	height = xpt1.y - xpt2.y + 1;

	/*
	** Allocate a pixmap or bitmap for the preview and set it
	** to be the rendering destination
	*/
	if (AppData.deepPreview && AppData.depth == 8) {
	    p = allocPixmap(dpy, XtWindow(AppData.drawingArea),
			    width, height, 8);
	    XFillRectangle(XtDisplay(AppData.drawingArea), p, AppData.gc,
			   0, 0, width, height);
	    setEPSIPixmapParameters(p, height);
	} else {
	    p = allocPixmap(dpy, XtWindow(AppData.drawingArea),
			    width, height, 1);
	    XFillRectangle(XtDisplay(AppData.drawingArea), p, AppData.bitmapgc,
			   0, 0, width, height);
	    setEPSIBitmapParameters(p, height);
	}

	/*
	** Execute the element(s) to generate preview
	*/
	if (singl) executeElement(singl, llx, lly);
	else {
	    for (e = AppData.lastElement; e != NULL; e = e->prev) {
		executeElement(e, llx, lly);
	    }
	}

	/*
	** Get the image back from the pixmap
	*/
	im = XGetImage(dpy, p, 0, 0, width, height, -1, ZPixmap);
	XFreePixmap(dpy, p);
    }

    /*
    ** Write the output
    */
    writeFile(im, writeFunc, data, singl, llx, lly, urx, ury, doPreview);

    XDefineCursor(XtDisplay(AppData.drawingArea),
		  XtWindow(AppData.drawingArea), None);
} /* end writePictureToFile() */
