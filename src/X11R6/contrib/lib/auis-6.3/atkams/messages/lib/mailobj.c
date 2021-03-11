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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atkams/messages/lib/RCS/mailobj.c,v 1.11 1993/07/11 19:55:02 Zarf Exp $";
#endif


 
#include <class.h>
#include <andrewos.h>
#include <mailobj.eh>
#include <envrment.ih>
#include <amsutil.ih>
#include <environ.ih>
#include <ams.h>
#include <ams.ih>
#include <message.ih>
#include <im.ih>
#include <text.ih>
#include <complete.ih>
#include <view.ih>
#include <sys/param.h>
#include <fdphack.h>
#include <ctype.h>
#undef popen /* BOGUS -- should be handled by fdphack */
#undef pclose /* ditto */

static void MetaOutput(fp, self)
FILE *fp;
struct mailobj *self;
{
    char buf[1000];
    int loc = 0;
    struct text *mytext;

    if (!self->t) {
	struct view *v[3], *vp;
	struct dataobject *dob;
	int ct;
	ct = mailobj_ListCurrentViews(self, v, 3);
	if (ct) {
	    vp = v[0];
	    while (vp && !self->t) {
		dob = (struct dataobject *) view_GetDataObject(vp);
		if (dob && class_IsTypeByName(class_GetTypeName(dob), "text")) {
		    self->t = (struct text *) dob;
		    self->env = NULL;
		}
		vp = vp->parent;
	    }
	}
    }
    if (self->t) {
	if (self->env) {
	    loc = environment_Eval(self->env) + self->bytesadded;
	} else {
	    loc = text_GetLength(self->t);
	}
    }
    if (self->bytesadded == 0) {
	loc+=2; /* the extra one is to pass the button itself */
	++self->bytesadded;
	if (self->t) text_AlwaysInsertCharacters(self->t, loc, "\n", 1);
    }
    if (fgets(buf, sizeof(buf), fp) != NULL) {
	self->bytesadded += strlen(buf);
	if (self->t) {
	    text_AlwaysInsertCharacters(self->t, loc, buf, strlen(buf));
	    text_NotifyObservers(self->t, 0);
	} else {
	    fputs(buf, stdout);
	}
	return;
    }
    if (errno != EWOULDBLOCK) {
	int retcode;
	im_RemoveFileHandler(fp);
	self->fp = NULL;
	retcode = pclose(fp);
	if (retcode && (self->bytesadded <= 1)) {
	    char FBuf[1200], Msg[1500];
	    if (ams_GetBooleanFromUser(ams_GetAMS(), "This data cannot be viewed with 'metamail'.  Write it to a file", 1)) {
		FBuf[0] = NULL;
		if (completion_GetFilename(NULL, "Name of file to write data into: ", FBuf, FBuf, sizeof(FBuf), FALSE, FALSE) != -1) {
		    FILE *fp;

		    fp = (FILE *) fopen (FBuf, "w");
		    if (!fp) {
			message_DisplayString(NULL, 10, "ERROR: Could not open file for writing");
		    } else {
			if (self->EncodingCode == ENC_B64) {
			    mailobj_TranslateFrom64(self->RawData, self->RawBytes, fp);
			} else if (self->EncodingCode == ENC_QP) {
			    mailobj_TranslateFromQP(self->RawData, self->RawBytes, fp);
			} else {
			    fwrite(self->RawData, sizeof(char), self->RawBytes, fp);
			}
			fclose(fp);
			sprintf(Msg, "Wrote file %s as requested", FBuf);
			message_DisplayString(NULL, 10, Msg);
			im_ForceUpdate();
		    }
		}
	    }
	} else {
	    message_DisplayString(NULL, 10, "MetaMail command execution completed");
	}
    }
    if (self->t) text_NotifyObservers(self->t, 0);
}

boolean mailobj__InitializeObject(c, self)
struct classheader *c;
struct mailobj *self;
{
    self->ContentType = NULL;
    self->RawData = (unsigned char *) NULL;
    self->t = NULL;
    self->env = NULL;
    self->bytesadded = 0;
    self->RawBytes = 0;
    self->fp = NULL;
    self->EncodingNeeded = self->EncodingCode = ENC_NONE;
    return(TRUE);
}

void mailobj__FinalizeObject(c, self)
struct classheader *c;
struct mailobj *self;
{
    if (self->ContentType) free(self->ContentType);
    if (self->RawData) free(self->RawData);
    if (self->fp) im_RemoveFileHandler(self->fp);
}

void
mailobj__SetTextInsertion(self, t, env)
struct mailobj *self;
struct text *t;
struct environment *env;
{
    self->t = t;
    self->env = env;
}

void
mailobj__ReadAlienMail(self, ContentType, ContentEncoding, fp, StopAtEndData)
struct mailobj *self;
char *ContentType, *ContentEncoding;
FILE *fp;
int StopAtEndData;
{
    int Alloced = 0, Used = 0, len, needsencoding=0;
#define CHUNKSIZE 5000
    char LineBuf[CHUNKSIZE+1], *t;
    unsigned char *s;

    if (self->ContentType) free(self->ContentType);
    self->ContentType = malloc(1+strlen(ContentType));
    /* Strip leading white space */
    while (*ContentType && isspace(*ContentType)) ++ContentType;
    strcpy(self->ContentType, ContentType);
    /* strip trailing white space */
    for (t=self->ContentType+strlen(self->ContentType) - 1;
	  t>=self->ContentType && isspace(*t); ++t) {
	*t = NULL;
    }
    /* strip leading white space */
    if (ContentEncoding) {
	while (*ContentEncoding && isspace(*ContentEncoding)) ++ContentEncoding;
    }
    if (!ContentEncoding) {
	self->EncodingCode = ENC_NONE;
    } else if (!amsutil_lc2strncmp("base64", ContentEncoding, 6)) {
	self->EncodingCode = ENC_B64;
    } else if (!amsutil_lc2strncmp("quoted-printable", ContentEncoding, 16)) {
	self->EncodingCode = ENC_QP;
    } else {
	self->EncodingCode = ENC_NONE;
    }
    Alloced = CHUNKSIZE;
    self->RawData = (unsigned char *) malloc(Alloced);
    s = self->RawData;
    *s = NULL;
    while (TRUE) {
	if (StopAtEndData) {
	    if (!fgets(LineBuf, sizeof(LineBuf), fp)) {
		break;
	    }
	    if (!strncmp(LineBuf, "\\enddata{mailobj", 16)) break;
	    len = strlen(LineBuf);
	} else {
	    len = fread(LineBuf, sizeof(char), sizeof(LineBuf), fp);
	    if (len <= 0) break;
	}
	if ((Used + len) >= Alloced) {
	    Alloced += CHUNKSIZE;
	    self->RawData = (unsigned char *) realloc(self->RawData, Alloced);
	    s = self->RawData + Used;
	}
	bcopy(LineBuf, s, len);
	if (!ContentEncoding) {
	    for (t=LineBuf; t<(LineBuf+len); ++t) {
		if (!isprint(*t) && !(isspace(*t))) ++needsencoding;
	    }
	}
	s += len;
	Used += len;
    }
    self->RawBytes = Used;
    /* We always use at least qp encoding because it is easier than checking for long lines in data read with fread */
    self->EncodingNeeded = ((needsencoding == 0) || ((Used/needsencoding) >= 10)) ? ENC_QP : ENC_B64;
}

static void WriteEncoded();

void
mailobj__RunMetamail(self)
struct mailobj *self;
{
    char TmpFileName[1+MAXPATHLEN], LineBuf[1000], Cmd[1+MAXPATHLEN],
    Msg[50+MAXPATHLEN];
    FILE *fp;

    sprintf(Msg, "Executing MetaMail to display %s", mailobj_GetLabel(self, 0));
    message_DisplayString(NULL, 10, Msg);
    ams_MS_UpdateState(ams_GetAMS()); /* unlock directories */    ams_CUI_GenLocalTmpFileName(ams_GetAMS(), TmpFileName);
    fp = (FILE *) fopen (TmpFileName, "w");
    if (fp) {
	/* Note that ContentType and ContentEncoding already have trailing newlines */
	WriteEncoded(self, fp);
	fclose(fp);
	sprintf(Cmd, "metamail -m messages -z -x -d -q %s 2>&1", TmpFileName); 
	fp = (FILE *) popen(Cmd, "r");
	im_AddFileHandler(fp, MetaOutput, self, 0);
	self->fp = fp;
    }
}

static char basis_hex[] = "0123456789ABCDEF";
static char basis_64[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

static int hexchar(c)
char c;
{
    char *s;
    if (islower(c)) c = toupper(c);
    s = (char *) index(basis_hex, c);
    if (s) return(s-basis_hex);
    return(-1);
}

void
mailobj__TranslateFrom64(c, stuff, len, fp)
struct classheader *c;
char *stuff;
int len;
FILE *fp;
{
    int c1, c2, c3, c4;
    int bytesleft;
    char *s, *end;

    for(s = stuff, end=stuff+len; c1 = *s++; s < end) {
        if (isspace(c1)) continue;
        do {
            c2 = *s++;
        } while (isspace(c2) && (s < end));
        do {
            c3 = *s++;
        } while (isspace(c3) && (s < end));
        do {
            c4 = *s++;
        } while (isspace(c4) && (s < end));
	if (s >= end) {
	    fprintf(stderr, "Bad parse of base64 data!\n");
            return;
        }
        c1 = char64(c1);
        c2 = char64(c2);
	fputc(((c1<<2) | ((c2&0x30)>>4)), fp);
        if (c3 != '=') {
            c3 = char64(c3);
            fputc((((c2&0XF) << 4) | ((c3&0x3C) >> 2)), fp);
            if (c4 != '=') {
                c4 = char64(c4);
		fputc((((c3&0x03) <<6) | c4), fp);
            }
        }
    }
}

static int char64(c)
char c;
{
    char *s = (char *) index(basis_64, c);
    if (s) return(s-basis_64);
    return(-1);
}

void
mailobj__TranslateFromQP(c, stuff, len, fp)
struct classheader *c;
char *stuff;
int len;
FILE *fp;
{
    int c1, c2;
    int bytesleft;
    char *s, *end;

    for(s = stuff, end=stuff+len; c1 = *s++; s < end) {
	if (c1 == '=') {
	    c1 = *s++;
	    if (c1 == '\n') {
		/* ignore it */
	    } else {
		c2 = *s++;
		c1 = hexchar(c1);
		c2 = hexchar(c2);
		fputc(c1<<4 | c2, fp);
	    }
	} else {
	    fputc(c1, fp);
	}
    }
}

long mailobj__Write(self, file, writeID, level)
struct mailobj *self;
FILE *file;
long writeID;
int level;
{
    if (mailobj_GetWriteID(self) != writeID)  {
	mailobj_SetWriteID(self,writeID);
	fprintf(file, "\\begindata{%s,%ld}\n", class_GetTypeName(self),mailobj_GetID(self));
	WriteEncoded(self, file);
	fprintf(file, "\\enddata{%s,%ld}\n", class_GetTypeName(self),mailobj_GetID(self));
    }
    return mailobj_GetID(self);
}

static void fputsquoting(s, fp)
char *s;
FILE *fp;
{
    char *end = s + strlen(s) - 1;
    while (isspace(*end) && end > s) --end;
    if (*s == '\"') {
        putc(*s, fp);
        while (*++s) {
            if (*s == '\"') break; /* MAY TERMINATE EARLY! */
            if (*s == '\\') {
                putc(*s, fp);
                ++s; /* Don't check this next char */
                if (!*s) break;
            }
            putc(*s, fp);
        }
        putc('\"', fp);
    } else {
        putc('\"', fp);
        putc(*s, fp);
        while (*++s) {
            if (*s == '\"' || *s == '\\') {
                putc('\\', fp);
            }
            putc(*s, fp);
        }
        putc('\"', fp);
    }
}
        

static void WriteCtypeNicely(fp, ct)
FILE *fp;
char *ct;
{
    char *semi, *slash, *eq, *s;

    fprintf(fp, "Content-type: ");
    for (s = ct; *s; ++s) {
        if (*s == '\n') *s = ' ';
    }
    semi = (char *) index(ct, ';');
    if (semi) *semi = '\0';
    slash = (char *) index(ct, '/');
    fputs(ct, fp);
    if (!slash) fputs("/unknown", fp);
    while (semi) {
        ct = semi + 1;
        *semi = ';';
        semi = (char *) index(ct, ';');
        if (semi) *semi = '\0';
        eq = (char *) index(ct, '=');
        if (eq) *eq = '\0';
        fputs(";\n\t", fp);
        while (isspace(*ct)) ++ct;
        fputs(ct, fp);
	if (eq) {
	    s = eq;
            fputs("=", fp);
            ++s;
            while (isspace(*s)) ++s;
            fputsquoting(s, fp);
            *eq = '=';
        }
    }
    fputs("\n", fp);
}

static void output64chunk(c1, c2, c3, pads, outfile)
unsigned int c1, c2, c3;
int pads;
FILE *outfile;
{
    putc(basis_64[c1>>2], outfile);
    putc(basis_64[((c1 & 0x3)<< 4) | ((c2 & 0xF0) >> 4)], outfile);
    if (pads == 2) {
        putc('=', outfile);
        putc('=', outfile);
    } else if (pads) {
        putc(basis_64[((c2 & 0xF) << 2) | ((c3 & 0xC0) >>6)], outfile);
        putc('=', outfile);
    } else {
        putc(basis_64[((c2 & 0xF) << 2) | ((c3 & 0xC0) >>6)], outfile);
        putc(basis_64[c3 & 0x3F], outfile);
    }
}

static void WriteEncoded(self, fp)
struct mailobj *self;
FILE *fp;
{
    char *s;
    int CodeToUse, needtoencode;
    if (self->ContentType && self->ContentType[0]) {
	WriteCtypeNicely(fp, self->ContentType);
    }
    s = mailobj_GetLabel(self, 0);
    if (s) {
	char *t = index(s, '\n');
	if (t) *t = NULL;
	fprintf(fp, "Content-Description: %s\n", s);
	if (t) *t = '\n';
    }
    if (self->EncodingCode == ENC_NONE
	 && self->EncodingNeeded != ENC_NONE) {
	CodeToUse = self->EncodingNeeded;
	needtoencode = 1;
    } else {
	CodeToUse = self->EncodingCode;
	needtoencode = 0;
    }
    /* We MUST NOT write encodings for multipart or message, that is illegal MIME */
    if (self->ContentType && 
	 (!amsutil_lc2strncmp("multipart/", self->ContentType, 10) || 
	  !amsutil_lc2strncmp("message/", self->ContentType, 8))) {
	CodeToUse = ENC_NONE;
	needtoencode = 0;
    }
    if (CodeToUse != ENC_NONE) {
	fprintf(fp, "Content-Transfer-Encoding: %s\n", CodeToUse == ENC_B64 ? "base64" : "quoted-printable");
    }
    fputs("\n", fp);
    if (needtoencode) {
	/* Here we need to write it out encoded */
	if (CodeToUse == ENC_QP) {
	    /* NEED TO WRITE OUT IN QP */
         mailobj_ToQP(self->RawData, self->RawBytes, fp);
	} else { /* Use  base64 */
	    int i=0, c1, c2, c3, ct = 0;

	    while (i<self->RawBytes) {
		c1 = self->RawData[i++];
		if (i < self->RawBytes) {
		    c2 = self->RawData[i++];
		    if (i < self->RawBytes) {
			c3 = self->RawData[i++];
			output64chunk(c1, c2, c3, 0, fp);
		    } else {
			output64chunk(c1, c2, 0, 1, fp);
		    }
		} else {
		    output64chunk(c1, 0, 0, 2, fp);
		}
		if (++ct > 16) {
		    putc('\n', fp);
		    ct = 0;
		}
	    }
	}
    } else {
	fwrite(self->RawData, sizeof(char), self->RawBytes, fp);
    }
    fputs("\n", fp);
}

long mailobj__WriteOtherFormat(self, file, writeID, level, usagetype, boundary)
struct mailobj *self;
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
    
    fprintf(file, "\n--%s\n", boundary);
    WriteEncoded(self, file);
    return(self->header.dataobject.id);
}

long mailobj__Read(self, file, id)
struct mailobj *self;
FILE *file;
long id;
{
    char LineBuf[200], Ctype[2000], Cenc[2000], Descrip[2000], c, *ctp;

    Ctype[0] = NULL;
    Cenc[0] = NULL;
    Descrip[0] = NULL;
    while (fgets(LineBuf, sizeof(LineBuf), file)) {
	if (LineBuf[0] == '\n') break;
	if (!amsutil_lc2strncmp("content-type:", LineBuf, 13)) {
	    strcpy(Ctype, LineBuf+13);
	    c = getc(file);
	    ungetc(c, file);
	    while ((c == ' ' || c == '\t') && fgets(LineBuf, sizeof(LineBuf), file)) {
		strcat(Ctype, LineBuf);
		c = getc(file);
		ungetc(c, file);
	    }
	} else if (!amsutil_lc2strncmp("content-transfer-encoding:", LineBuf, 26)) {
	    strcpy(Cenc, LineBuf+26);
	    c = getc(file);
	    ungetc(c, file);
	    while ((c == ' ' || c == '\t') && fgets(LineBuf, sizeof(LineBuf), file)) {
		strcat(Cenc, LineBuf);
                c = getc(file);
		ungetc(c, file);
	    }
	} else if (!amsutil_lc2strncmp("content-description:", LineBuf, 20)) {
	    strcpy(Descrip, LineBuf+20);
	}
    }
    ctp = Ctype[0] ? Ctype : "text/plain";
    while (*ctp && isspace(*ctp)) ++ctp;
    if (Descrip[0]) {
	sprintf(LineBuf, "%s ('%s' format)", Descrip, ctp);
    } else {
	sprintf(LineBuf, "Object of type '%s'", Ctype[0] ? Ctype : "text/plain");
    }
    mailobj_SetLabel(self, 0, LineBuf);
    mailobj_ReadAlienMail(self, Ctype, Cenc, file, TRUE);
} 		

char *mailobj__ViewName(self)
struct mailobj *self;
{
    return "mailobjv";
}


void mailobj__ToQP(classID, s, len, outfile)
struct classheader *classID;
unsigned char *s; /* Lying to avoid high-bit problems */
int len;
FILE *outfile;
{
    int ct=0, prevc=255;
    unsigned char *end=s+len;
    while (s < end) {
        if ((*s < 32 && (*s != '\n' && *s != '\t'))
             || (*s == '=')
             || ((*s >= 127))) {
            putc('=', outfile);
            putc(basis_hex[*s>>4], outfile);
            putc(basis_hex[*s&0xF], outfile);
            ct += 3;
            prevc = 'A'; /* close enough */
        } else if (*s == '\n') {
            if (prevc == ' ' || prevc == '\t') {
                putc('=', outfile); /* soft & hard lines */
                putc(*s, outfile);
            }
            putc(*s, outfile);
            ct = 0;
            prevc = *s;
        } else {
            putc(*s, outfile);
            ++ct;
            prevc = *s;
        }
        if (ct > 72) {
            putc('=', outfile);
            putc('\n', outfile);
            ct = 0;
            prevc = '\n';
	}
	++s;
    }
    if (ct) {
        putc('=', outfile);
        putc('\n', outfile);
    }
}


