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


 

/* Flags for various things texttroff should or should not do. (each should be a power of 2) */
#define	texttroff_Revert 1	/* if set texttroff will revert to the parents state after processing, otherwise the state will be left however it ended up. */
				  
package texttroff[txttroff] {
classprocedures:
    WriteSomeTroff(struct view *view,struct dataobject *dd,FILE *f,int toplevel,unsigned long revert);
    WriteTroff(struct view *view,struct dataobject *dd,FILE * f,int toplevel);

    /* BeginDoc / EndDoc
	These functions are called by standalone views which wish to use the
	troff environment as is established by text.
	They are called if the "toplevel" (fifth) parameter to xxx_Print is TRUE.
	The text generated before calling EndDoc should end with a newline;
	a .br should have been issued to send the last line to the output.
    */
    BeginDoc(FILE *f);
    EndDoc(FILE *f);


    /* BeginPS / EndPS
	These functions supply the transition for including a postscript rectangle
		in a troff page.  
	BeginPS sets up a postscript coordinate system for an image of size 
		(width x height) in postscript units (1/72 in.) 
		with the origin in the lower left corner.
	At the end of the postscript, the routine EndPS must be called.
	Each line between the two must begin with the two characters:  \!
		(backslash followed by exclamation mark)
*/
    BeginPS(FILE *f, long width, long height);
    EndPS(FILE *f, long width, long height);

};
