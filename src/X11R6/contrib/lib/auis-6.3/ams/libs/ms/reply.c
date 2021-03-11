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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/reply.c,v 2.30 1994/03/31 20:18:19 rr2b Exp $";
#endif

#include <andrewos.h> /* sys/file.h */
#include <stdio.h>
#include <ctype.h>
#include <ms.h>
#include <hdrparse.h>

#define ToLower(c) (isupper(c) ? tolower(c) : (c))

extern FILE *fopen();
extern char *RewriteSubject(), *getprofile(), *StripWhiteEnds();
extern char MeInFull[];
static char *ForwardString = "---------- Forwarded message begins here ----------\n\n";

MS_NameReplyFile(DirName, id, code, FileName)
char *DirName, *id, *FileName; 
	/* The first two char * arguments are passed in, the last returned */
int code; /* passed in */
{
    struct MS_Directory *Dir;
    struct MS_Message *Msg;
    FILE *fp;
    char *Subject= NULL, RawFileName[MAXPATHLEN+1], *CC=NULL, *TmpCC, *InReplyTo = NULL, BE2Format[20], *References = NULL, *Distribution = NULL, BigBuf[5000];
    int len, FormatAsInt, reflen, IsBE2;
    int bytesleft, bytestoread, bytesread;
    char boundary[60];
#ifdef INHIBIT_MIME_GENERATION
    int skiplen;
#endif /* INHIBIT_MIME_GENERATION */


    debug(1, ("MS_NameReplyFile %s %s %d\n", DirName ? DirName : "", id ? id : "", code));
    BE2Format[0] = '\0';
    GenTempName(FileName);
    fp = fopen(FileName, "w");
    if (fp == NULL) {
	AMS_RETURN_ERRCODE(errno, EIN_FOPEN, EVIA_NAMEREPLYFILE);
    }
    if ((Msg = (struct MS_Message *) malloc (sizeof (struct MS_Message))) == NULL) {
	fclose(fp);
	AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_NAMEREPLYFILE);
    }
    bzero(Msg, sizeof(struct MS_Message));
    Msg->OpenFD = -1;
    if (!id || !DirName || !*id || !*DirName) {
	code = AMS_REPLY_FRESH;
    }
    if (code == AMS_REPLY_FRESH) {
	Msg->Snapshot =  malloc(AMS_SNAPSHOTSIZE);
	if (Msg->Snapshot == NULL) {
	    fclose(fp);
	    FreeMessage(Msg, TRUE);
	    AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_NAMEREPLYFILE);
	}
	InventID(Msg);
    } else {
	debug(4, ("Parsing message\n"));
	if (ReadOrFindMSDir(DirName, &Dir, MD_OK) != 0) {
	    fclose(fp);
	    FreeMessage(Msg, TRUE);
	    return(mserrcode);
	}
	QuickGetBodyFileName(Dir->UNIXDir, id, RawFileName);
	if (ReadRawFile(RawFileName, Msg, FALSE)
	    || ParseMessageFromRawBody(Msg)
	    || InventID(Msg)
	    || BuildReplyField(Msg)
	    || BuildWideReply(Msg, code == AMS_REPLY_WIDER ? TRUE : FALSE))
	{
	    FreeMessage(Msg, TRUE);
	    fclose(fp);
	    return(mserrcode); /* already set */
	}
	if (code == AMS_REPLY_FORWARD) {
	    if (UnformatMessage(Msg)) {
		FreeMessage(Msg, TRUE);
		fclose(fp);
		return(mserrcode);
	    }
	}
	len = Msg->ParsedStuff->HeadBodyLen[HP_SUBJECT];
	Subject = malloc(10 + len);
	if (Subject == NULL) {
	    fclose(fp);
	    FreeMessage(Msg, TRUE);
	    AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_NAMEREPLYFILE);
	}
	strcpy(Subject, ((code == AMS_REPLY_FORWARD) || (code == AMS_REPLY_FORWARD_FMT)) ? "Fwd: " : "Re: ");
	if (len > 0) {
	    char *NewSub;

	    strncat(Subject, Msg->ParsedStuff->HeadBody[HP_SUBJECT], len);
	    NewSub = RewriteSubject(Subject);
	    free(Subject);
	    if (!NewSub) {
		fclose(fp);
		FreeMessage(Msg, TRUE);
		AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_NAMEREPLYFILE);
	    }
	    Subject = NewSub;
	}
	len = Msg->ParsedStuff->HeadBodyLen[HP_CC];
	CC = malloc(5 + len);
	if (CC == NULL) {
	    fclose(fp);
	    free(Subject);
	    FreeMessage(Msg, TRUE);
	    AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_NAMEREPLYFILE);
	}
	strncpy(CC, Msg->ParsedStuff->HeadBody[HP_CC], len);
	CC[len] = '\0';
	if (!StripMyselfFromAddressList(CC, &TmpCC)) {
	    free(CC);
	    CC = TmpCC;
	}
	len = Msg->ParsedStuff->HeadBodyLen[HP_DISTRIBUTION];
	if (len) {
	    Distribution = malloc(20+len);
	    if (!Distribution) {
		fclose(fp);
		free(Subject);
		free(CC);
		FreeMessage(Msg, TRUE);
		AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_NAMEREPLYFILE);
	    }
	    strcpy(Distribution, "\nDistribution: ");
	    reflen = strlen(Distribution);
	    strncpy(Distribution+reflen, Msg->ParsedStuff->HeadBody[HP_DISTRIBUTION], len);
	    Distribution[len+reflen] = '\0';
	}
	len = Msg->ParsedStuff->HeadBodyLen[HP_MESSAGEID];
	reflen =  Msg->ParsedStuff->HeadBodyLen[HP_REFERENCES];
	if (len + reflen > 0) {
	    References = malloc(10+len+reflen);
	    if (!References) {
		fclose(fp);
		if (Subject) free(Subject);
		if (Distribution) free(Distribution);
		free(CC);
		FreeMessage(Msg, TRUE);
		AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_NAMEREPLYFILE);
	    }
	    if (reflen) {
		strncpy(References, Msg->ParsedStuff->HeadBody[HP_REFERENCES], reflen);
		References[reflen] = '\0';
	    } else {
		*References = '\0';
	    }
	    if (len) {
		char *s;

		if (reflen) {
		    strcat(References, "\n\t");
		}
		s = References + strlen(References);
		strncpy(s, Msg->ParsedStuff->HeadBody[HP_MESSAGEID], len);
		s[len] = '\0';
	    }
	}
	if (len) {
	    InReplyTo = malloc(5+len);
	    if (InReplyTo == NULL) {
		fclose(fp);
		free(Subject);
		free(CC);
		FreeMessage(Msg, TRUE);
		if (Distribution) free(Distribution);
		if (References) free (References);
		AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_NAMEREPLYFILE);
	    }
	    strncpy(InReplyTo, Msg->ParsedStuff->HeadBody[HP_MESSAGEID], len);
	    InReplyTo[len] = '\0';
	} else {
	    /* No message id, generate my own in-reply-to text & no "references"  */
	    char *s;

	    len = Msg->ParsedStuff->HeadBodyLen[HP_DATE];
	    if (len <= 0) len = 30;
	    len += 50 + (Msg->ReplyTo ? strlen(Msg->ReplyTo) : 0);
	    InReplyTo = malloc(len);
	    if (InReplyTo == NULL) {
		fclose(fp);
		free(Subject);
		free(CC);
		FreeMessage(Msg, TRUE);
		if (Distribution) free(Distribution);
		if (References) free (References);
		AMS_RETURN_ERRCODE(ENOMEM, EIN_MALLOC, EVIA_NAMEREPLYFILE);
	    }
	    sprintf(InReplyTo, "Message from \377%s\377 dated \377", Msg->ReplyTo ? Msg->ReplyTo : "<someone>");
	    len = Msg->ParsedStuff->HeadBodyLen[HP_DATE];
	    if (len) {
		strncat(InReplyTo, Msg->ParsedStuff->HeadBody[HP_DATE], len);
		strcat(InReplyTo, "\377");
	    } else {
		strcat(InReplyTo, "<NO DATE>\377");
	    }
	    for (s = InReplyTo; *s; ++s) {
		switch (*s) {
		    case '\"':
		    case '\n':
		    case '\\':
			*s = ' ';
			break;
		    case '\377':
			*s = '\"';
			break;
		    default:
			break;
		}
	    }
	}
    }
    debug(4, ("Writing out file\n"));
    switch(code) {
	case AMS_REPLY_ACK:
	    EmitHeader(Msg, HP_ACKTO, fp, "To: ");
	    EmitHeader(Msg, HP_SUBJECT, fp, "Subject: Confirming: ");
	    fprintf(fp, "Ack-type: explicit\nAck: ");
	    if (Msg->ParsedStuff->HeadBody[HP_MESSAGEID]) {
		fwriteallchars(Msg->ParsedStuff->HeadBody[HP_MESSAGEID], Msg->ParsedStuff->HeadBodyLen[HP_MESSAGEID], fp);
	    } else {
		fprintf(fp, "<No-Message-ID-In-Ack-Request>");
	    }		
	    fputs("\n\n", fp);
	    fprintf(fp, "This message confirms the receipt by ``%s''\nof a message ", MeInFull);
	    if (Msg->ParsedStuff->HeadBody[HP_FROM]) {
		fputs("from ``", fp);
		fwriteallchars(Msg->ParsedStuff->HeadBody[HP_FROM], Msg->ParsedStuff->HeadBodyLen[HP_FROM], fp);
		fputs("''", fp);
	    } else {
		fputs("with no ``from'' header", fp);
	    }
	    if (Msg->ParsedStuff->HeadBody[HP_SUBJECT]) {
		fprintf(fp, "\non the subject ``");
		fwriteallchars(Msg->ParsedStuff->HeadBody[HP_SUBJECT], Msg->ParsedStuff->HeadBodyLen[HP_SUBJECT], fp);
		fputs("''", fp);
	    } else {
		fprintf(fp, "\nwith no ``subject'' header");
	    }
	    if (Msg->ParsedStuff->HeadBody[HP_DATE]) {
		fprintf(fp, "\ndated ``");
		fwriteallchars(Msg->ParsedStuff->HeadBody[HP_DATE], Msg->ParsedStuff->HeadBodyLen[HP_DATE], fp);
		fputs("''.", fp);
	    } else {
		fprintf(fp, "\nwith no ``date'' header.");
	    }
	    fprintf(fp, "\n\nThis is an ``explicit'' receipt, which means that the user\nsaw the message and agreed to send this confirmation.\n");
	    break;
	case AMS_REPLY_REDRAFT:
	    EmitHeader(Msg, HP_SCRIBEFORMAT, fp, "X-Andrew-ScribeFormat: ");
	    EmitHeader(Msg, HP_CONTENTTYPE, fp, "Content-Type: ");
	    EmitHeader(Msg, HP_UNSUPPORTEDTYPE, fp, "If-Type-Unsupported: ");
	    EmitHeader(Msg, HP_TO, fp, "To: ");
	    EmitHeader(Msg, HP_SUBJECT, fp, "Subject: ");
	    EmitHeader(Msg, HP_CC, fp, "CC: ");
	    EmitHeader(Msg, HP_REPLY_TO, fp, "Reply-To: ");
	    EmitHeader(Msg, HP_SENDER, fp, "Sender: ");
	    EmitHeader(Msg, HP_ENCLOSURE, fp, "Enclosure: ");
	    EmitHeader(Msg, HP_WIDEREPLY, fp, "X-Andrew-WideReply: ");
	    EmitHeader(Msg, HP_DIRECTORYCREATION, fp, "X-Andrew-DirectoryCreation: ");
	    EmitHeader(Msg, HP_VOTEREQUEST, fp, "Vote-Request: ");
	    EmitHeader(Msg, HP_VOTETO, fp, "Vote-To: ");
	    EmitHeader(Msg, HP_VOTECHOICES, fp, "Vote-Choices: ");
	    EmitHeader(Msg, HP_INREPLYTO, fp, "In-Reply-To: ");
	    EmitHeader(Msg, HP_REFERENCES, fp, "References: ");
	    EmitHeader(Msg, HP_DISTRIBUTION, fp, "Distribution: ");
	    fputc('\n', fp);
	    lseek(Msg->OpenFD, Msg->BodyOffsetInFD, L_SET);
	    bytesleft = Msg->FullSize - Msg->HeadSize;
	    while (bytesleft > 0) {
		bytestoread = (bytesleft > (sizeof(BigBuf)-1)) ? (sizeof(BigBuf) - 1) : bytesleft;
		bytesread = read(Msg->OpenFD, BigBuf, bytestoread);
		if ((bytesread <=0)
		    || (fwriteallchars(BigBuf, bytesread, fp) != bytesread)) {
		    fclose(fp);
		    free(Subject);
		    free(CC);
		    FreeMessage(Msg, TRUE);
		    if (Distribution) free(Distribution);
		    if (References) free (References);
		    AMS_RETURN_ERRCODE(errno, EIN_READ, EVIA_NAMEREPLYFILE);
		}
		bytesleft -= bytesread;
	    }
	    break;
	case AMS_REPLY_FRESH:
	    fprintf(fp, "To: \nSubject: \nCC: \n\n");
	    break;
	case AMS_REPLY_FORWARD:
	case AMS_REPLY_FORWARD_FMT:
#ifdef INHIBIT_MIME_GENERATION
	    GetFormatFromMessage(Msg, BE2Format, sizeof(BE2Format), &IsBE2);
	    if (BE2Format[0]) {
		fprintf(fp, "Content-Type: X-BE2; %s\n", BE2Format);
	    }
	    fprintf(fp, "To: \nSubject: %s\nCC: %s%s%s\n\n", Subject, References ? "\nReferences: " : "", References ? References : "", Distribution ? Distribution : "");
	    FormatAsInt = atoi(BE2Format);
	    if (FormatAsInt >= 10) { /* be2 format */
		EmitBE2PrefixAndLSeekPastIt(Msg->OpenFD, fp, &skiplen);
	    } else {
		skiplen = 0;
	    }
	    PrintQuotingFormatting(fp, ForwardString, BE2Format, strlen(ForwardString));
	    if (BE2Format[0]) {
		PrintSpecialStuff(fp, PR_STARTSHRINK, BE2Format);
		PrintSpecialStuff(fp, PR_STARTSHRINK, BE2Format);
	    }
	    PrintFwdHeaders(fp, Msg->RawBits, BE2Format, Msg->HeadSize);
	    if (BE2Format[0]) {
		PrintSpecialStuff(fp, PR_ENDSHRINK, BE2Format);
		PrintSpecialStuff(fp, PR_ENDSHRINK, BE2Format);
	    }
	    bytesleft = Msg->FullSize - Msg->HeadSize - skiplen;
#else /* INHIBIT_MIME_GENERATION */
	    fprintf(fp, "To: \nSubject: %s\nCC: %s%s%s\n", Subject, References ? "\nReferences: " : "", References ? References : "", Distribution ? Distribution : "");
	    sprintf(boundary, "---Forwarded.message.boundary.line.%s", ams_genid(1));
	    fprintf(fp, "Content-type: multipart/mixed; \n\tboundary = %s\n\n", boundary);
	    fprintf(fp, "This is a forwarded message in MIME format.  Please do NOT delete the\nfunny-looking boundary and header lines, or this message may not end up being\nreadable to the recipients.  Place your prefix between the two lines\nthat begin with ==== below.\n");
	    fprintf(fp, "\n--%s\nContent-type: text/plain;  charset=US-ASCII\n\n", boundary); 
	    if (code == AMS_REPLY_FORWARD) { 
		fprintf(fp, "====User Forwarding Prefix goes below this  line:\n\n\n====User Forwarding Prefix ends here\n"); 
	    } 
	    fprintf(fp, "\n--%s\nContent-type: message/rfc822\n\n", boundary);
	    PrintFwdHeaders(fp, Msg->RawBits, 0, Msg->HeadSize);
	    bytesleft = Msg->FullSize - Msg->HeadSize;
#endif /* INHIBIT_MIME_GENERATION */
	    while (bytesleft > 0) {
		bytestoread = (bytesleft > (sizeof(BigBuf)-1)) ? (sizeof(BigBuf) - 1) : bytesleft;
		bytesread = read(Msg->OpenFD, BigBuf, bytestoread);
		if (bytesread <= 0) {
		    if (Subject) free (Subject);
		    if (CC) free (CC);
		    if (InReplyTo) free(InReplyTo);
		    FreeMessage(Msg, TRUE);
		    fclose(fp);
		    if (Distribution) free(Distribution);
		    if (References) free (References);
		    AMS_RETURN_ERRCODE(errno, EIN_READ, EVIA_NAMEREPLYFILE);
		}
#ifdef INHIBIT_MIME_GENERATION
		if (BE2Format[0]) {
		    fwriteallchars(BigBuf, bytesread, fp);
		} else {
		    PrintQuotingFormatting(fp, BigBuf, BE2Format, bytesread);
		}
#else /* INHIBIT_MIME_GENERATION */
		fwriteallchars(BigBuf, bytesread, fp);
#endif /* INHIBIT_MIME_GENERATION */
		bytesleft -= bytesread;
            }
#ifdef INHIBIT_MIME_GENERATION
	    PrintQuotingFormatting(fp, "\n", BE2Format, 1); /* for newline termination */
#else /* INHIBIT_MIME_GENERATION */
	    fprintf(fp, "\n--%s--\n\n", boundary);
#endif /* INHIBIT_MIME_GENERATION */
	    break;
	case AMS_REPLY_WIDE:
	case AMS_REPLY_WIDER:
	    fprintf(fp, "To: %s\nSubject: %s\nCC: %s\nIn-Reply-To: %s%s%s%s\n\n",
		Msg->WideReply ? Msg->WideReply : " ", Subject, CC, InReplyTo, References ? "\nReferences: " : "", References ? References : "", Distribution ? Distribution : "");
	    break;
	case AMS_REPLY_SENDER:
	    fprintf(fp, "To: %s\nSubject: %s\nCC: \nIn-Reply-To: %s%s%s%s\n\n", Msg->ReplyTo ? Msg->ReplyTo : " ", Subject, InReplyTo, References ? "\nReferences: " : "", References ? References : "", Distribution ? Distribution : "");
	    break;
	default:
	    if (Subject) free (Subject);
	    if (CC) free (CC);
	    if (InReplyTo) free(InReplyTo);
	    FreeMessage(Msg, TRUE);
	    fclose(fp);
	    if (Distribution) free(Distribution);
	    if (References) free (References);
	    AMS_RETURN_ERRCODE(EINVAL, EIN_PARAMCHECK, EVIA_NAMEREPLYFILE);
    }
    if (Subject) free (Subject);
    if (CC) free (CC);
    if (InReplyTo) free(InReplyTo);
    if (Distribution) free(Distribution);
    if (References) free (References);
    FreeMessage(Msg, TRUE);
    if (ferror(fp) || feof(fp)) {
	fclose(fp);
	AMS_RETURN_ERRCODE(errno, EIN_FERROR, EVIA_NAMEREPLYFILE);
    }
    if (vfclose(fp) != 0) {
	AMS_RETURN_ERRCODE(errno, EIN_VFCLOSE, EVIA_NAMEREPLYFILE);
    }
    return(0);
}

EmitHeader(Msg, which, fp, head)
struct MS_Message *Msg;
int which;
FILE *fp;
char *head;
{
    int len;

    len = Msg->ParsedStuff->HeadBodyLen[which];
    if (len) {
	fputs(head, fp);
	fwriteallchars(Msg->ParsedStuff->HeadBody[which], len, fp);
	fputc('\n', fp);
    }
}

PrintFwdHeaders(fp, headers, BE2Format, hdrlen)
int fp;
char *headers;
char *BE2Format;
int hdrlen;
{
    char *hdr, *s, *t, *h, *wh;
    int numhdrs, len, match;
    char *weedhdrs;
    char **fwdhdr;
    static int first_time = TRUE;		/* Used to init fwdhdrs */
    static char **fwdhdrs = NULL;		/* Null terminated list of headers prefixes to copy when forwarding mail. */

    if (first_time) {
	/* Get fwdheaders preference. */
	if ((hdr = getprofile("fwdheaders")) != NULL) {
	    /* Want to show only headers with given prefixes. */
	    numhdrs=0;
	    for (t=hdr; t=index(t, ':'); ++t, ++numhdrs) {
		;
	    }
	    fwdhdrs = (char **)malloc((3+numhdrs) * sizeof(char *));
	    t = malloc(15+strlen(hdr));
	    strcpy(t, hdr); /* permanent copy */
	    strcat(t, ":Content-Type");	    /* Must always forward a content-type. */
	    fwdhdrs[0] = t;
	    numhdrs = 1;
	    for (s=t; s=index(s, ':'); ++s, ++numhdrs) {
		*s++ = '\0';
		if (*s) {
		    fwdhdrs[numhdrs] = s;
		} else {
		    --numhdrs;
		}
	    }
	    fwdhdrs[numhdrs] = NULL;
	}
	first_time = FALSE;
    }

    if (fwdhdrs == NULL) {
	/* Forward all headers--this is the default. */
        PrintQuotingFormatting(fp, headers, BE2Format, hdrlen);
    } else {
	/* Copy headers, weeding out those we don't want. */
	weedhdrs = (char *)malloc(hdrlen+1);	/* weedhdrs will be the new header text */
	h = headers;
	wh = weedhdrs;
	len = 0;				/* length of 'headers' scanned so far */
	while (len < hdrlen) {
	    /* Do a case insensitive compare against each fwdhdr */
	    match = FALSE;
	    for (fwdhdr = fwdhdrs; *fwdhdr != NULL; fwdhdr++) {
	        match = TRUE;
	        for (s = (*fwdhdr), t=h; s && *s != '\0'; s++, t++) {
		    if (ToLower(*s) != ToLower(*t)) {
			match = FALSE;
			break;
		    }
		}
		if (match)
		    break;
	    }
	    if (match) {
		while (1) {
		    while (len < hdrlen && *h != '\n') {
			*wh++ = *h++;
			len++;
		    }
		    *wh++ = '\n';
		    h++; len++;
		    /* Check for continuation lines. */
		    if (len >= hdrlen || (*h != ' ' && *h != '\t'))
			break;
		}
	    } else {
		while (len < hdrlen && *h != '\n') {
		    h++;
		    len++;
		}
		h++; len++;
	    }
	}
	if (!BE2Format[0])
	    *wh++ = '\n';
	*wh = '\0';

        PrintQuotingFormatting(fp, weedhdrs, BE2Format, strlen(weedhdrs));
        free(weedhdrs);
    }
}
