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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/text/RCS/smpltext.c,v 2.32 1994/04/15 17:33:37 rr2b Exp $";
#endif

#include <andrewos.h> /* sys/types.h */
#include <class.h>
#include <sys/stat.h>
#include <sys/errno.h>
#include <attribs.h>

#include <dataobj.ih>
#include <dict.ih>
#include <environ.ih>	/* for datastream test only */
#include <mark.ih>

#include <smpltext.eh>

extern int errno;

#define INITIALSTRINGSIZE 100
#define ADDITIONALSIZE 50
#define INITIALKEYWORDSIZE 30

/* These values are used for the read routine */

static char *keyword;		/* Keyword buffer - initialize once when first
				encountering a keyword in the 
				ReadSubString routine */
static long keywordlength;	/* Current length of the keyword buffer */

#define DEFAULTDATASTREAMNUMBER 12
static int DataStreamVersion = 0;


boolean simpletext__InitializeObject(classID, self)
struct classheader *classID;
struct simpletext *self;
{
    self->string = (char *) malloc(INITIALSTRINGSIZE);
    self->lowSize = 0;
    self->gapSize = INITIALSTRINGSIZE;
    self->length = 0;
    self->markList = NULL;
    self->pendingReadOnly = FALSE;
    self->fence = simpletext_CreateMark(self, 0, 0);
    self->objectInsertionAllowed = TRUE;
    mark_SetStyle(self->fence, TRUE, FALSE);

    if(DataStreamVersion == 0){
	char *buf;

	if((buf = environ_GetConfiguration("BE2TextFormat")) !=  NULL && *buf != '\0')
	    DataStreamVersion = atoi(buf);

	if(DataStreamVersion < 10)
	    DataStreamVersion = DEFAULTDATASTREAMNUMBER;
    }

    self->highbitflag=(-1);

    return TRUE;
}

void simpletext__FinalizeObject(classID, self)
struct classheader *classID;
struct simpletext *self;
{
    struct mark *mark;
    
    if (self->markList != NULL)  {
	for (mark = self->markList; mark != NULL; mark = mark_GetNext(mark))
	    mark_SetObjectFree(mark, TRUE);
    }
    mark_Destroy(self->fence);
    if (self->string)
	free(self->string);
}

#define SIZEINCREMENT 200 /* Size to increase pad a "large" space request by. */

static boolean EnsureSize(self, neededGap)
struct simpletext *self;
long neededGap;
{
    long totalSize;
    long newGapSize;
    long count;
    char *s, *t;

    if (neededGap <= self->gapSize)
        return TRUE;

    /* If request more than doubles total buf size,  set length to */
    /* requested size plus some.  Otherwise,  double length.  We */
    /* think this works well for files <50k but may degrade above that.  */

    /* First compute newGapSize necessary to double totalSize. */
    /* If that's not enough, make gap big enough and then some. */

    newGapSize = self->length + 2 * self->gapSize;
    if (neededGap >= newGapSize)
        newGapSize = neededGap + SIZEINCREMENT;

    totalSize = self->length + newGapSize;

    /* Be careful about the realloc */

    s = realloc(self->string, totalSize); 
    if (s == NULL)
        return FALSE;
    self->string = s;

    /* Move the text upwards in memory by the amount necessary */
    /* to increase the gap size to newGapSize. */

    count = self->length - self->lowSize;
    t = self->string + self->length + self->gapSize;
    s = self->string + totalSize;
    bcopy(t-count, s-count, count);
    self->gapSize = newGapSize;
    return TRUE;
}

static void MoveGap(self, pos)
struct simpletext *self;
long pos;
{
    register long amount;
    char *s, *t;

    if (pos > self->lowSize) {
        amount = pos - self->lowSize;
        s = self->string + self->lowSize;
        t = self->string + self->lowSize + self->gapSize;
        self->lowSize += amount;
	bcopy(t, s, amount);
    } else if (pos < self->lowSize) {
        amount = self->lowSize - pos;
        s = self->string + self->lowSize + self->gapSize;
        t = self->string + self->lowSize;
        self->lowSize -= amount;
	bcopy(t-amount, s-amount, amount);
    }
}

/*
This is a define here to a the code to call this routine to set the marks.
but not confuse the reader of the code in __Read to believe that it will call
the LengthChanged method.
*/

#define UpdateMarks(self, pos, size) simpletext__LengthChanged(self, pos, size)

void simpletext__LengthChanged(self, pos, size)
struct simpletext *self;
long pos;
long size;
{
    mark_UpdateMarks(self->markList, pos, size);
}

boolean simpletext__GetReadOnly(self)
    struct simpletext *self;
{
    return (simpletext_GetFence(self) > simpletext_GetLength(self));
}

void simpletext__SetReadOnly(self, readOnly)
    struct simpletext *self;
    boolean readOnly;
{
    if (readOnly)
        simpletext_SetFence(self, simpletext_GetLength(self) + 1);
    else
        simpletext_ClearFence(self);
}

void simpletext__SetAttributes(self, attributes)
    struct simpletext *self;
    struct attributes *attributes;
{

    super_SetAttributes(self, attributes);

    self->pendingReadOnly = FALSE;

    while (attributes) {
        if (strcmp(attributes->key, "filesize") == 0)
            EnsureSize(self, attributes->value.integer - self->length);
        else if (strcmp(attributes->key, "readonly") == 0) {
            self->pendingReadOnly = attributes->value.integer;
            simpletext_SetReadOnly(self, attributes->value.integer);
        }
        attributes = attributes->next;
    }
}

long simpletext__Read(self, file, id)
struct simpletext *self;
FILE *file;
long id;
{
    int oldLength = self->length;

    self->highbitflag=(-1);
    
    simpletext_ClearFence(self);

    /* Code is copied from simpletext_Clear so we can preserve mark state. */
    self->gapSize += self->length;
    self->lowSize = 0;
    self->length = 0;
    self->header.dataobject.id = dataobject_UniqueID(self);
    if (id == 0 && fileno(file) >= 0)  {
	struct stat buf;

	fstat(fileno(file), &buf);

	/* Make sure the file is a regular file. If it isn't stat can't return any useful information about it's size. */

	if ((buf.st_mode & S_IFMT) == S_IFREG) {
	    long readlen;
	    long len;
	    long result;
	    long pos;
	    register unsigned char *p;

	    /* Just in case you are passed a file that has already been read determine the current position, reseek to that position - this should clear out the data stored in the buffer - and then set length to the remaining part of the file. */

	    pos = ftell(file);
	    fseek(file, pos, 0L);
            lseek(fileno(file), pos, 0L);
	    len = buf.st_size - pos;

	    EnsureSize(self, len);
	    readlen = len;
	    /* Start reading in data at the beginning of the gap. */
	    p = (unsigned char *) &self->string[self->lowSize];
	    while (readlen > 0)  {
		result = read(fileno(file), p, readlen);
		if (result == -1)  {
		    if (errno != EINTR)  {
			fprintf(stderr, "Error reading text object - ignoring text\n");
			len = 0;
			break;
		    }
		}
		else if (result == 0)  {
		    len -= readlen;
		    break;
		}
		else  {
		    readlen -= result;
		    p = &p[result];
		}
	    }
	    self->lowSize = len;
	    self->length = len;
	    self->gapSize -= len;
	    fseek(file, len, 0);
	}
	else  {
	    /* The file is pipe or something similar. Just keep reading until EOF. We have to use STDIO because there is no way to re-sync STDIO with the actual stream if we use read. */
	    int count;

	    do {
		/* Make sure there is some space. */
		EnsureSize(self, 1);

		count = fread(self->string + self->lowSize, 1, self->gapSize, file);
		self->length += count;
		self->lowSize += count;
		self->gapSize -= count;
	    } while (count > 0);
	}
    }
    else  {
	simpletext_ReadSubString(self, 0, file, (id > 0));
	UpdateMarks(self, 0, -self->length); /* Bring them all back down. */
    }
    if (self->length < oldLength) /* Bring all marks into range if neccesary. */
	UpdateMarks(self, self->length, self->length - oldLength);
    simpletext_RegionModified(self, 0, self->length);

    if (self->pendingReadOnly) {
	simpletext_SetFence(self, simpletext_GetLength(self) + 1);
	self->pendingReadOnly = FALSE;
    }
    return dataobject_NOREADERROR;
}
static boolean simplewrite(file,p,len)
FILE *file;
char *p;
long len;
{
    /*
      Write out the literal contents of the character buffer.
      write() is used because it is more efficient than fwrite.
      if write() fails, attempt with fwrite (),
	  if it succeeds then continue,
	      if it fails, then when buffer calls ferror, 
		  it will notice that something has gone wrong 
		  */
    long wroteLen;
    while (len > 0)  {
	wroteLen = write(fileno(file), p, len);
	if (wroteLen == -1) {
	    if (errno == EINTR)  continue;
	    if((wroteLen = fwrite(p,1,len,file)) <= 0)	{
		fprintf(stderr, "Error while writing text object.\n");
		return FALSE;
	    }
	    fflush(file);
	}
	len -= wroteLen;
	p = &p[wroteLen];
    }
    return TRUE;
}
long simpletext__Write(self, file, writeID, level)
struct simpletext *self;
FILE *file;
long writeID;
int level;
{
    if (self->header.dataobject.writeID != writeID)  {
	self->header.dataobject.writeID = writeID;
	if (level != 0)  {
	    fprintf(file, "\\begindata{%s,%ld}", class_GetTypeName(self), dataobject_UniqueID(self)); 
	    simpletext_WriteSubString(self, 0, self->length, file, level != 0);
	    fprintf(file, "\\enddata{%s,%ld}", class_GetTypeName(self), dataobject_UniqueID(self));
	    fflush(file);
	}
	else  {
	    fflush(file);
	    if(simplewrite(file,self->string,self->lowSize)) 
		simplewrite(file, &(self->string[self->lowSize + self->gapSize]),self->length - self->lowSize);
	}
    }
    return dataobject_UniqueID(self);
}

void simpletext__Clear(self)
struct simpletext *self;
{
    self->highbitflag=(-1);
    if (self->length == 0) return;
    self->gapSize += self->length;
    self->lowSize = 0;
    simpletext_SetModified(self);
    simpletext_LengthChanged(self, 0, -self->length);
    self->length = 0;
}

boolean simpletext__InsertCharacters(self, pos, str, len)
struct simpletext *self;
long pos;
char *str;
long len;
{
    if (pos >= simpletext_GetFence(self)) {
        simpletext_AlwaysInsertCharacters(self, pos, str, len);
	return TRUE;
    }
    else
        return FALSE;
}

void simpletext__AlwaysInsertCharacters(self, pos, str, len)
struct simpletext *self;
long pos;
char *str;
long len;
{
    register long i;
    register char *s;
    register char *t;

    if (len == 0)
	return;
    
    self->highbitflag=(-1);
    
    if (pos > self->length)
	pos = self->length;

    if (self->gapSize < len)
	EnsureSize(self, len);
    if (pos != self->lowSize)
	MoveGap(self, pos);
    for (i = len, s = str, t = &(self->string[self->lowSize]); i > 0; i--, s++, t++)
	*t = *s;
    self->lowSize += len;
    self->gapSize -= len;
    self->length += len;
    simpletext_SetModified(self);
    simpletext_LengthChanged(self, pos, len);
}

boolean simpletext__DeleteCharacters(self, pos, len)
struct simpletext *self;
long pos;
long len;
{    
    if (pos >= simpletext_GetFence(self)) {
        simpletext_AlwaysDeleteCharacters(self, pos, len);
	return TRUE;
    }
    else
        return FALSE;
}

void simpletext__AlwaysDeleteCharacters(self, pos, len)
struct simpletext *self;
long pos;
long len;
{
    
    self->highbitflag=(-1);
    
    if (len == 0)
        return;
    if (pos + len > self->length)
	len = self->length - pos;

    MoveGap(self, pos);
    self->length -= len;
    self->gapSize += len;
    simpletext_SetModified(self);
    simpletext_LengthChanged(self, pos, -len);
}

boolean simpletext__ReplaceCharacters(self, pos, len, replacementString, replacementLen)
    struct simpletext *self;
    long pos, len;
    char *replacementString;
    long replacementLen;
{

    if (pos >= simpletext_GetFence(self)) {
        simpletext_AlwaysReplaceCharacters(self, pos, len, replacementString, replacementLen);
	return TRUE;
    }
    else
        return FALSE;
}

void simpletext__AlwaysReplaceCharacters(self, pos, len, replacementString, replacementLen)
    struct simpletext *self;
    long pos, len;
    char *replacementString;
    long replacementLen;
{

    int i;
    
    self->highbitflag=(-1);
    

    if (replacementLen > len) {	/* Put gap after characters to replace, leaving space for extras. */
        EnsureSize(self, self->length + replacementLen - len); /* make sure there's enough room */
        MoveGap(self, pos + len);
    }
    else			/* Put gap before extra characters to delete. */
        MoveGap(self, pos + replacementLen);

/* Insert replacement text. */
    for (i = 0; i < replacementLen; i++)
        self->string[i + pos] = *replacementString++;

/* Adjust document sizes appropriately. */
    if (replacementLen > len) {
        self->lowSize += replacementLen - len; /* Eat some of the gap. */
        self->gapSize -= replacementLen - len; /* Make the gap smaller. */
    }
    else
        self->gapSize += len - replacementLen; /* Into the void (gap) goes the text. */
    self->length += replacementLen - len;

/* Hopefully return everything to a normal state. */
    if (replacementLen > len)
        simpletext_LengthChanged(self, pos + len, replacementLen - len);
    else
        simpletext_LengthChanged(self, pos + replacementLen, replacementLen - len);

/* Set modified bits on marks that have been touched. */
    simpletext_RegionModified(self, pos, replacementLen);
    simpletext_SetModified(self);
}

long simpletext__GetLength(self)
struct simpletext *self;
{
    return self->length;
}

long simpletext__GetChar(self, pos)
struct simpletext *self;
long pos;
{
    if (pos >= self->length || pos < 0) return EOF;
    if (pos < self->lowSize) return self->string[pos];
    return self->string[pos+self->gapSize];
}

long simpletext__GetUnsignedChar(self, pos)
struct simpletext *self;
long pos;
{
    if (pos >= self->length || pos < 0) return EOF;
    if (pos < self->lowSize) return ((unsigned char *)(self->string))[pos];
    return ((unsigned char *)(self->string))[pos+self->gapSize];
}

/*
 * GetBuf returns a pointer to a buffer containing any amount of
 * characters up to the number requested.  The amount is placed
 * at lenp.  Returns NULL if pos is out of range.  This call is used
 * for purposes such as writing the buffer to a file in chunks, or
 * searching forwards.
 *
 * GetBufEnd is similar to GetBuf, except backwards:  given an end
 * position, it tries to get a chunk of text to the left of that position
 * as large as it can up to the specified size.  It returns a pointer to
 * one past the end of the chunk.  The returned region extends
 * leftward by the amount stored at lenp.  Returns NULL if endpos is
 * out of range.  This call is used for purposes such as searching
 * backwards.
 *
 * GetGap inserts a gap in a buffer at the given position and of
 * the given length.  It returns a pointer to this contiguous area.  In
 * the event of an error, it returns NULL.  Note that the area is
 * uninitialized and so it is up to the caller to write reasonable data
 * into every byte of the new gap.  This call is used for purposes such
 * as reading a whole file or chunks of a file directly into a buffer.
 */

char *simpletext__GetBuf(self, pos, length, lenp)
struct simpletext *self;
long pos, length;
long *lenp;
{
    if (pos >= 0 && length > 0) {
        long len = self->lowSize - pos;

        if (len > 0) {
            *lenp = MIN(length, len);
            return self->string + pos;
        }

        len = self->length - pos;

        if (len > 0) {
            *lenp = MIN(length, len);
            return self->string + self->gapSize + pos;
        }
    }

    *lenp = 0;
    return NULL;
}

char *simpletext__GetBufEnd(self, endpos, length, lenp)
struct simpletext *self;
long endpos, length, *lenp;
{
    if (endpos >= 0 && length > 0) {
        if (endpos <= self->lowSize) {
            *lenp = MIN(length, endpos);
            return self->string + endpos;
        }

        *lenp = MIN(length, endpos - self->lowSize);
        return self->string + self->gapSize + endpos;
    }

    *lenp = 0;
    return NULL;
}

char *simpletext__GetGap(self, pos, len)
struct simpletext *self;
long pos, len;
{
    if (pos < 0 || pos > self->length || len < 0)
        return NULL;

    /* Make sure the buffer gap is as big as the requested gap. */
    /* Move the buffer's gap to the requested position. */
    /* Transfer the requested size from the gap to the string. */

    if (self->gapSize < len)
        if (EnsureSize(self, len) == FALSE)
            return NULL;

    if (self->lowSize != pos)
        MoveGap(self, pos);

    self->lowSize += len;
    self->gapSize -= len;
    self->length += len;

    return self->string + pos;
}

long simpletext__GetPosForLine(self, line)
struct simpletext *self;
long line;
{
    long len = simpletext_GetLength(self);
    long pos = 0;
    long i;

    i = 1;
    while (i < line) {
	if (pos >= len)
            break;
	if (simpletext_GetChar(self, pos) == '\012')
            i++;
        pos++;
    }
    return pos;
}

long simpletext__GetLineForPos(self, pos)
struct simpletext *self;
long pos;
{
    long line=1;

    while (--pos >= 0)
       if (simpletext_GetChar(self,pos) == '\012')
            line++;

    return line;
}

long simpletext__GetBeginningOfLine(self, pos)
struct simpletext *self;
long pos;
{
    while (pos > 0 && simpletext_GetChar(self, pos-1) != '\n') {
	pos--;
    }
    return pos;
}

long simpletext__GetEndOfLine(self, pos)
struct simpletext *self;
long pos;
{
    long len = simpletext_GetLength(self);

    while (pos < len) {
	if (simpletext_GetChar(self, pos) == '\n') {
	    break;
	}
	pos++;
    }
    return pos;
}


void simpletext__AddInCharacter(self, pos, c)
    struct simpletext *self;
    long pos;
    char c;
{
    self->highbitflag=(-1);
    if (self->gapSize == 0)
	EnsureSize(self, ADDITIONALSIZE);
    if (pos != self->lowSize)
	MoveGap(self, pos);
    self->string[self->lowSize++] = c;
    self->gapSize -= 1;
    self->length += 1;
}

long simpletext__HandleDataObject(self, pos, dop, file)
struct simpletext *self;
long pos;
struct dataobject *dop;
FILE *file;  {
    return 0;
}

long simpletext__HandleKeyWord(self, pos, keyword, file)
struct simpletext *self;
long pos;
char *keyword;
FILE *file;
{
    long len = strlen(keyword);

    self->highbitflag=(-1);
    simpletext_InsertCharacters(self, pos, "\\", 1);
    pos += 1;
    simpletext_InsertCharacters(self, pos, keyword, len);
    pos += len;
    simpletext_InsertCharacters(self, pos, "{", 1);
    return len + 2;
}


long simpletext__HandleCloseBrace(self, pos, file)
struct simpletext *self;
long pos;
FILE *file;
{
    simpletext_InsertCharacters(self, pos, "}", 1);
    return 1;
}

#define TESTEND(C) (C == EOF )
#define GETANDTEST(C,file) ((C = getc(file)) != EOF )

long simpletext__ReadSubString(self, pos, file, quoteCharacters)
struct simpletext *self;
long pos;
FILE *file;
boolean quoteCharacters;
{
    register long c;
    long addedcharacters = 0;
    long keywordpos = 0;
    register int addedspace = -1;
    register long lastadd = -1;
    long endcount = 1;		/* Number of enddata markers to be 
				encountered before completion
 */
    boolean canInsert;
    
    self->highbitflag=(-1);
    
    if(quoteCharacters)
	self->Version = DataStreamVersion;
    else
        self->Version = 0;

    if ((!(canInsert = (pos >= simpletext_GetFence(self)))) && ! quoteCharacters)  {
	fseek(file, 0L, 2);
	return -1;
    }

    simpletext_LengthChanged(self, pos, 1000000000);
    MoveGap(self, pos);    
    while (endcount != 0 && GETANDTEST(c,file))  {
	if (quoteCharacters && (c == '}' || c == '\\'))  {
	    if (c == '}')  {
		if (canInsert)
		    addedcharacters += simpletext_HandleCloseBrace(self, self->lowSize, file);
		continue;
	    }
	    else  { /* handle backslash */
		if (!GETANDTEST(c,file))
		    break;
		if(c == '\n') continue; /* ignore <backslash><newline> pair */
		if (c != '\\' && c != '{' && c != '}')  {
		   /*  Have a keyword here */
		    if (keyword == NULL)  {
			keywordlength = INITIALKEYWORDSIZE;
			keyword = (char *) malloc(INITIALKEYWORDSIZE);
		    }
		    keywordpos = 1;
		    *keyword = c;
		    while (GETANDTEST(c,file))  {
			if (c == '{')  {
			    keyword[keywordpos] = '\0';
			    if (strcmp(keyword, "begindata") == 0)  {
				if (canInsert)  {
				    int rt;

				    rt = simpletext_HandleBegindata(self,pos,file);
				    switch(rt) {
					case -1: /* eof encountered */
					    endcount = 0;
					    c = EOF;
					    break;
					case -2: /* couldn't find object */
					    endcount += 1;
					    break;
					default:
					    if(addedcharacters == lastadd)
						lastadd += rt;
					    addedcharacters += rt;
				    }
				}
				else
				    endcount += 1;
			    }
			    else if (strcmp(keyword, "enddata") == 0)  {
				while (GETANDTEST(c,file) && c != '}');
				if (GETANDTEST(c, file) && c != '\n')
				    ungetc(c, file);
				endcount -= 1;
			    }
			    else if (canInsert){
				long foo;
				foo = simpletext_HandleKeyWord(self, self->lowSize, keyword, file);
				if(foo){
				    if(addedcharacters == lastadd && addedspace == -2)
					lastadd += foo; 
				    addedcharacters += foo;
				}
			    }
			    keywordpos = 0;
			    break;
			}
			else  {
			    if (keywordpos > keywordlength - 2)  {
				keywordlength *= 2;
				keyword = (char *) realloc(keyword, keywordlength); 
			    }
			    keyword[keywordpos++] = c;
			}
		    }
		    if (TESTEND(c) && keywordpos != 0 && canInsert)  {
		/* 	Incomplete keyword - just place in the text */
			keyword[keywordpos] = '\0';
			fprintf(stderr, "Found End of File while reading in the keyword, %s\n", keyword);
			fprintf(stderr, "Placing keyword in as just text.\n");
			simpletext_InsertCharacters(self, self->lowSize, "\\", 1);
			simpletext_InsertCharacters(self, self->lowSize, keyword, strlen(keyword));
			}
		    if(TESTEND(c) ) endcount = 0;
		    continue;
		}
	    }
	}
	if (canInsert) {
	    if (self->gapSize == 0)
		EnsureSize(self, ADDITIONALSIZE);
	    if(self->Version > 11){
		if(c == '\n')  {
		    if(addedcharacters != lastadd){
			if(self->lowSize > 0 && self->string[self->lowSize - 1] != ' ' && self->string[self->lowSize - 1] != '\t'){
			    /* replace first newline , not proceeded with a space, with a space */
			    c = ' ';
			    addedspace = addedcharacters + 1;
			}
			else {
			    addedspace = -1;
			    lastadd = addedcharacters;
			    continue;
			}
		    }
		    else if(addedspace == addedcharacters){
			/* The space inserted above really was a newline */
			self->string[self->lowSize - 1] = '\n';
			addedspace = -2;
			continue;
		    }
		    else addedspace = -2;
		    lastadd = addedcharacters + 1;
		}
	    }
	    self->string[self->lowSize++] = c;
	    self->gapSize -= 1;
	    self->length += 1;
	    addedcharacters += 1;
	}
    }
    if (addedcharacters)  {
	simpletext_SetModified(self);
	simpletext_LengthChanged(self, pos+addedcharacters, addedcharacters);
    }
    simpletext_LengthChanged(self, pos+addedcharacters, -1000000000);
    return addedcharacters;
}

long simpletext__HandleBegindata(self,pos,file)
struct simpletext *self;
long pos;
FILE *file;
{
	char objectname[200];
        int c;
	long objectid;
	char *s;
	struct dataobject *newobject;

	self->highbitflag=(-1);	    
	s = objectname;
	while (GETANDTEST(c,file) && c != ',')
	    *s++ = c;
	if (TESTEND(c))  {
	    fprintf(stderr, "End of File encountered while reading in a begindata marker - ignoring\n");
	    return(-1);
	}
	*s = '\0';
	objectid = 0;
	while (GETANDTEST(c,file) && c != '}')
	    if(c >= '0' && c <= '9')objectid = objectid * 10 + c - '0';
	if (TESTEND(c))  {
	    fprintf(stderr, "End of File encountered while reading in a begindata marker - ignoring\n");
	    return(-1);
	}
	if((c = getc(file))!= '\n')ungetc(c,file);

/* 	Call the New routine for the object */
	if ((newobject = (struct dataobject *) class_NewObject(objectname))==NULL)  {
	    newobject = (struct dataobject *)class_NewObject("unknown");
	}
	if (newobject)  {
	    /* Setup readonly state for object. */
	    if (self->pendingReadOnly) {
		struct attributes readOnlyAttr;

		readOnlyAttr.key = "readonly";
		readOnlyAttr.value.integer = TRUE;
		readOnlyAttr.next = NULL;
		dataobject_SetAttributes(newobject, &readOnlyAttr);
	    }
	    /* 	    Call the read routine for the object */
	    dataobject_Read(newobject, file,objectid);
	    if((c = getc(file))!= '\n')ungetc(c,file);
	    /*     At this point , the object pointer is the new object id */
	    if(simpletext_GetObjectInsertionFlag(self) == FALSE){
		/* ignore non-text object */
		fprintf(stderr,
			"Insertion of objects not allowed, ignoring %s!\n", objectname);
		dataobject_Destroy(newobject);
		return(0l);
	    }
	    else {
		/* 	    Register the object with the dictionary */
		dictionary_Insert(NULL,(char *)objectid,(char *) newobject);
		/* Note: it is assumed that unless a call to Reference is made subsequently it is safe to destroy newobject. */
		dataobject_UnReference(newobject);
		return(simpletext_HandleDataObject(self, self->lowSize, newobject, file));
	    }
	}
	else {
	    fprintf(stderr, "Could not find data object %s - ignoring\n", objectname);
	    return(-2);
	}
}

boolean simpletext__CopyTextExactly(self,pos,srctext,srcpos,len)
    struct simpletext *self;
    long pos;
    struct simpletext *srctext;
    long srcpos;
    long len;
{
	return simpletext_CopyText(self, pos, srctext, srcpos, len);
}
    
boolean simpletext__CopyText(self,pos,srctext,srcpos,len)
    struct simpletext *self;
    long pos;
    struct simpletext *srctext;
    long srcpos;
    long len;
{
    if (pos >= simpletext_GetFence(self)) {
	simpletext_AlwaysCopyText(self,pos,srctext,srcpos,len);
	return TRUE;
    }
    else
        return FALSE;
}

void simpletext__AlwaysCopyTextExactly(self,pos,srctext,srcpos,len)
    struct simpletext *self;
    long pos;
    struct simpletext *srctext;
    long srcpos;
    long len;
{
    simpletext_AlwaysCopyText(self, pos, srctext, srcpos, len);
}
    
void simpletext__AlwaysCopyText(self,pos,srctext,srcpos,len)
    struct simpletext *self;
    long pos;
    struct simpletext *srctext;
    long srcpos;
    long len;
{
    register long i;
    register char *s;
    register char *t;
    register long remlen;

    if (len == 0)
	return;
    
    self->highbitflag=(-1);
    
    if (pos > self->length)
	pos = self->length;

    if(srctext->length < srcpos + len)
	len = srctext->length - srcpos;
    if (self->gapSize < len)
	EnsureSize(self, len);
    if (pos != self->lowSize)
	MoveGap(self, pos);
    
    i = remlen =  len;
    t = &(self->string[self->lowSize]);

    if (srcpos < srctext->lowSize)  {
	if ((remlen = (srcpos + len - srctext->lowSize)) < 0)
	    remlen = 0;
	else
	    i = srctext->lowSize - srcpos;
 	s = &(srctext->string[srcpos]);
 	bcopy(s, t, i);
 	t += i;
	srcpos = srctext->lowSize;
    }
    if (remlen > 0)  {
 	s = &(srctext->string[srcpos + srctext->gapSize]);
 	bcopy(s, t, remlen);
    }
    self->lowSize += len;
    self->gapSize -= len;
    self->length += len;
    simpletext_SetModified(self);
    simpletext_LengthChanged(self, pos, len);
    return;
}

long simpletext__Index(self,pos,c,len)
struct simpletext *self;
long pos;
char c;
long len;
{
    register long remlen = len;
    register char *s, *fs;
    if( len + pos > self->length || pos < 0) return EOF;
     
    if (pos < self->lowSize)  {
	if ((remlen = (pos + len - self->lowSize)) < 0)
	    remlen = 0;
	else
	    len = self->lowSize - pos;
	for (fs = s = &(self->string[pos]); len > 0; s++, len--)  {
	    if(c == *s) return pos + (s - fs) ;
	}
	pos = self->lowSize;
    }
    if (remlen > 0)  {
	for (fs = s = &(self->string[pos + self->gapSize]); remlen > 0; remlen--, s++)  {
	    if(c == *s) return pos + (s - fs);
	}
    }
    return EOF ;
}

int simpletext__Strncmp(self,pos,str,len)
struct simpletext *self;
long pos;
char *str;
long len;
{
    register long remlen = len;
    register char *s;
    if( len + pos > self->length || pos < 0) return EOF;
      
    if (pos < self->lowSize)  {
	if ((remlen = (pos + len - self->lowSize)) < 0)
	    remlen = 0;
	else
	    len = self->lowSize - pos;
	for (s = &(self->string[pos]); len > 0; s++, len--,str++)  {
	    if(*str != *s) return *s - *str;
	}
	pos = self->lowSize;
    }
    if (remlen > 0)  {
	for ( s = &(self->string[pos + self->gapSize]); remlen > 0; remlen--, s++,str++)  {
	    if(*str != *s) return *s - *str;
	}
    }
    return 0 ;
}

#define CharAt(self,pos) ((pos < self->lowSize)? self->string[pos] : self->string[pos+self->gapSize])

int simpletext__Textncmp(self,pos,text,pos2,len)
struct simpletext *self;
long pos;
struct simpletext *text;
long pos2;
long len;
{
    register long remlen = len;
    register char *s;
    if(text == NULL || len + pos > self->length || pos < 0 || pos2 < 0 || len+pos2 > text->length) return EOF;

    if (pos < self->lowSize)  {
	if ((remlen = (pos + len - self->lowSize)) < 0)
	    remlen = 0;
	else
	    len = self->lowSize - pos;
	for (s = &(self->string[pos]); len > 0; s++, len--,pos2++)  {
	    if(CharAt(text,pos2) != *s) return *s - CharAt(text,pos2);
	}
	pos = self->lowSize;
    }
    if (remlen > 0)  {
	for ( s = &(self->string[pos + self->gapSize]); remlen > 0; remlen--, s++,pos2++)  {
	    if(CharAt(text,pos2) != *s) return *s - CharAt(text,pos2);
	}
    }
    return 0 ;
}

void simpletext__WriteSubString(self, pos, len, file, quoteCharacters)
struct simpletext *self;
long pos;
long len;
FILE *file;
boolean quoteCharacters;
{
    long remlen = len;
    char *s;

    if (pos < self->lowSize)  {
	if ((remlen = (pos + len - self->lowSize)) < 0)
	    remlen = 0;
	else
	    len = self->lowSize - pos;
        /* These cases should be unrolled */
	for (s = self->string + pos; len > 0; len--, s++)  {
	    if (quoteCharacters && (*s == '\\' || *s == '{' || *s == '}'))
		putc('\\', file);
            putc(*s, file);
	}
	pos = self->lowSize;
    }
    if (remlen > 0)  {
	for (s = &(self->string[pos + self->gapSize]); remlen > 0; remlen--, s++)  {
	    if (quoteCharacters && (*s == '\\' || *s == '{' || *s == '}'))
		putc('\\', file);
            putc(*s, file);
	}
    }
 }

void simpletext__CopySubString(self, pos, len, buf, quoteCharacters)
struct simpletext *self;
long pos;
long len;
char *buf;
boolean quoteCharacters;
{
    long remlen = len;
    char *s;

    if (pos < self->lowSize)  {
	if ((remlen = (pos + len - self->lowSize)) < 0)
	    remlen = 0;
	else
	    len = self->lowSize - pos;
	for (s = &(self->string[pos]); len > 0; s++, len--)  {
	    if (quoteCharacters && (*s == '\\' || *s == '{' || *s == '}'))
		*buf++ = '\\';
	    *buf++ = *s;
	}
	pos = self->lowSize;
    }
    if (remlen > 0)  {
	for (s = &(self->string[pos + self->gapSize]); remlen > 0; remlen--, s++)  {
	    if (quoteCharacters && (*s == '\\' || *s == '{' || *s == '}'))
		*buf++ = '\\';
	    *buf++ = *s;
	}
    }
    *buf = '\0';
 }

struct mark *simpletext__CreateMark(self, pos, length)
struct simpletext *self;
long pos;
long length;
{
    struct mark *mark;
    
    mark = mark_New();
    mark_SetPos(mark, pos);
    mark_SetLength(mark, length);
    mark_SetNext(mark, self->markList);
    mark_SetObject(mark, self);
    self->markList = mark;
    return mark;
}

void simpletext__RemoveMark(self, mark)
struct simpletext *self;
struct mark *mark;
{
    struct mark *mp;
    struct mark *tp;

    if (mark == NULL) return;
    
    for (mp = self->markList, tp = NULL; mp != NULL && mp != mark; tp = mp, mp = mark_GetNext(mp));
    
    if (mp != NULL)  {
	if (tp == NULL)  {
            /* First element on the list */
	    self->markList = mark_GetNext(mp);
	}
	else  {
	    mark_SetNext(tp, mark_GetNext(mp));
	}
    }
}

void simpletext__RegionModified(self, pos, len)
    struct simpletext *self;
    long pos;
    long len;
{
    struct mark *mp;
    long endpos;

    for (mp = self->markList; mp != NULL; mp = mark_GetNext(mp))  {
	if (pos >= (endpos = mark_GetPos(mp) + mark_GetLength(mp))) {
	    if (pos == endpos)
		mark_SetModified(mp, TRUE);
	}
	else if (pos + len > mark_GetPos(mp))
	    mark_SetModified(mp, TRUE);
    }
}

boolean simpletext__CheckHighBit(self)
struct simpletext *self;
{
    long i=0, len=simpletext_GetLength(self);

    if(self->highbitflag>=0) return self->highbitflag;
    
    self->highbitflag=0;
    for(i=0;i<len;i++) {
	if(simpletext_GetChar(self, i)&0x80) self->highbitflag=1;
    }
    return self->highbitflag;
}
