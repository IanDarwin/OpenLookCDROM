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


class mailobj : sbutton {
    classprocedures:
      InitializeObject(struct mailobj *self) returns boolean;
      FinalizeObject(struct mailobj *self);
      TranslateFrom64(char *data, int len, FILE *fp);
      TranslateFromQP(char *data, int len, FILE *fp);
      ToQP(char *data, int len, FILE *fp);
     overrides:
      Read (FILE *file, long id) returns long;
      Write (FILE *file, long writeid, int level) returns long;
      WriteOtherFormat(FILE *file, long writeid, int level, int usagetype, char *boundary) returns long;
      ViewName() returns char *;
     methods:
      ReadAlienMail(char *ContentType, char *ContentEncoding, FILE *fp, boolean StopAtEndData);
      RunMetamail();
      SetTextInsertion(struct text *t, struct environment *env);
    data:
      char *ContentType;
      unsigned char *RawData;
      struct text *t;
      struct environment *env;
      int bytesadded, RawBytes;
      int EncodingCode, EncodingNeeded;
      FILE *fp;
};

/*  Codes for EncodingCode variable */
#define ENC_NONE 0
#define ENC_B64 1
#define ENC_QP 2

