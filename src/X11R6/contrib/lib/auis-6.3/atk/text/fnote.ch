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




class fnote: text {
classprocedures:
	CloseAll(struct text *text);
	UpdateAll(struct text *text,long count) returns int;
	CopyAll(struct text *text,struct text *desttext,long count,boolean number) returns int;
	OpenAll(struct text *text);
	InitializeObject(struct fnote *self)returns boolean;
	InitializeClass() returns boolean; 
methods:
	Close(struct text *text);
	CopyNote(struct text *text,struct text * desttext,long count,boolean number) returns long;
	Open(struct text *text);
	IsOpen() returns boolean;
	addenv(struct text *txt,long pos);
	GetLocLength() returns long;
overrides:
	 Read (FILE *file, long id) returns long;
	ViewName() returns char *;
macromethods:
    GetLoc() ((self)->loc)
    SetLoc(long newValue) ( ((self)->loc) = (newValue) )
    GetOwnLoc() ((self)->ownloc)
    SetOwnLoc(long newValue) ( ((self)->ownloc) = (newValue) )
    GetParenttxt() ((self)->parenttext)
    GetEnv() ((self)->env)
    SetEnv(long newValue) ( ((self)->env) = (newValue) )
data:
   long loc,ownloc,notecount;
   struct text *parenttext;
   struct environment *env;
   struct style *vstyle , *hstyle;
   boolean open;
};

