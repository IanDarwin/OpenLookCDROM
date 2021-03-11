/* Copyright 1992 Carnegie Mellon University, All rights reserved.
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
/* $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/basics/common/RCS/owatch.ch,v 1.2 1992/12/14 20:37:02 rr2b R6tape $ */

struct owatch_data {
    struct observable *obj;
    boolean alive;
    struct owatch_data *next, *prev;
    long refs;
};

class owatch : observable[observe] {
methods:
overrides:      
    ObservedChanged(struct observable *changed, long val);
macros:
Check(struct owatch_data *tocheck) (tocheck?tocheck->alive:FALSE)
classprocedures:
    Create(struct observable *b) returns struct owatch_data *;
    Delete(struct owatch_data *d);
    CheckAndDelete(struct owatch_data *d) returns boolean;
data:
};
