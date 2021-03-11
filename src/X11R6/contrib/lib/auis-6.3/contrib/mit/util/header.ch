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


#define header_HEADER 0
#define header_FOOTER 1

#define header_ltext 0
#define header_ctext 1
#define header_rtext 2
#define header_TEXTS 3

#define ALWAYS_ACTIVE_MODE

class header: dataobject[dataobj] {
classprocedures:
    Create(int type, char *left, char *center, char *right) returns struct thisobject *;
    InitializeObject(struct thisobject *self) returns boolean;
    FinalizeObject(struct thisobject *self);
    InitializeClass() returns boolean;
overrides:

    ObservedChanged(struct observable *t,long value);
    Read (FILE *file, long id) returns long;
    Write (FILE *file, long writeid, int level) returns long;
    ViewName() returns char *;
methods:
    SetHeader(int which, char *str);
data:
    long  where;
    struct text *texts[header_TEXTS];
    boolean active[header_TEXTS];
};
