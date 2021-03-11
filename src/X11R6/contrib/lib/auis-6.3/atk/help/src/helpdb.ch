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

/* $ACIS$ */

 

/*---------------------------------------------------------------------------*/
/*	MODULE: helpdb.c						     */
/*		Package of routines to do the index-related lookups and	     */
/*		matching algorithms.					     */
/*---------------------------------------------------------------------------*/

package helpdb {
    classprocedures:
        InitializeClass(struct helpdb *self) returns boolean;
	CheckIndex(struct view *v) returns int;
	SetIndex(char *anindex) returns int;
	SetupHelp(struct cache *c, char *aname, int strip) returns int;
	GetHelpDirs() returns struct helpDir *;
	AddSearchDir(char *dirname);
	ReadAliasesFile(char *aname);
	MapAlias(char *name) returns char *;
	Enumerate(void (*proc)(), char *self);
	EnumerateAll(void (*proc)(), char *rock);
	PrintSearchDirs();
};
