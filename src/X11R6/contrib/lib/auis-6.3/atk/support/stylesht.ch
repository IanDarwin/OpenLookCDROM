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


 

/* stylesht.H
 * class header file for stylesht.
 *
 */

#define stylesheet_VERSION 1

class stylesheet[stylesht] : observable[observe] {
methods:
    FreeStyles();			/* clear out all styles in the stylesheet */
    Add(struct style *styleptr);	/* add a style to the stylesheet */
    Delete(struct style *styleptr);	/* delete a style from the stylesheet */
    Find(char *name) returns struct style *;	/* find a style, given it's name */
    GetMenuList(procedure procname, struct classinfo *infotype) returns struct menulist *;
					/* get a menulist for the styles */
    Read(FILE *fileptr, boolean template) returns long;
					/* parse the contents of \\define{} */
    Write(FILE *fileptr);               /* write all externally defined styles */
    SetTemplateName(char *name);	/* save the name of the template being used */
    GetTemplateName() returns char *;	/* retrieve the name of the template being used */
    EnumerateStyles(/* struct stylesheet *self, */ procedure func, long data) 
		returns struct style *;   /* calls func(style, data) for each style in self.  
			The boolean value returned by func is True if the function is
			through enumerating;  EnumerateStyles then returns the last style 
			processed, othewise it returns NULL */
    GetGlobalStyle() returns struct style *;
classprocedures:
    FinalizeObject(struct stylesheet *self);
    InitializeObject(struct stylesheet *self) returns boolean;
data:
    char *templateName;			/* name of the template used, if any */
    long nstyles;			/* number of styles in this stylesheet */
    long maxStyles;			/* number of styles currently allocated */
    long version;			/* version used for updating menulist */
    struct style **styles;		/* array of the styles themselves */
    struct menulist *styleMenu;		/* current menulist for the styles */
};
