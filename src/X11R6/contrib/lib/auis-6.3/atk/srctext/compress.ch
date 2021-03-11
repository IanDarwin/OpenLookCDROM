/* File compress.ch created by R S Kemmetmueller
 
   compress: a dataobject to store a hidden region of text. */
/* Copyright 1992, 1994 Carnegie Mellon University and IBM. All rights reserved.
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

class compress: text {

  classprocedures:
    InitializeClass() returns boolean; 
    InitializeObject(struct compress *self) returns boolean;
    FinalizeObject(struct compress *self);
    Compress(struct text *txt,long pos,long len);
    DecompressAll(struct text *txt);
    DecompressRange(struct text *txt,long pos,long len);
    IsThere(struct text *txt,long pos) returns struct compress *;

  overrides:
    GetLineForPos(long pos) returns long;
    GetModified() returns long;
    GetPosForLine(long line) returns long;

  methods:
    CompressInFront(struct text *txt,long pos,long len);
    CompressInBack(struct text *txt,long pos,long len);
    DecompressBox(struct text *txt);
    PartialDecompress(struct text *txt,long pos,long len);

  macromethods:
    GetLines() ((self)->lines)
    SetLines(long nlines) ( ((self)->lines) = (nlines) )

  data:
    long lines, loc;
};
