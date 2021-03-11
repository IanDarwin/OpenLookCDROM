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


 

/* environ.H
 * Class declarations for environment and preference maodule.
 *
 */

package environ {
    classprocedures:
        Put(char *variable, char *value);
        Delete(char *variable);
        Get(char *variable) returns char *;
        GetProfile(char *preference) returns char *;
        GetProfileSwitch(char *preference, boolean defaultValue) returns boolean;
	GetProfileInt(char *preference, long defaultValue) returns long;
	ProfileEntryExists(char *preference, boolean useDefault) returns boolean;
	GetConfiguration(char *key) returns char *;
	AndrewDir(char *str) returns char *;
	LocalDir(char *str) returns char *;
	ReadConfigureFile(char *filename) returns struct configurelist *;
	GetConfig(struct configurelist *cList, char *key, boolean usedefault) returns char *;
	FreeConfigureList(struct configurelist *cList);
	GetHome(char *user) returns char *;  /* pass in NULL for current user */
	SetProgramName(char *string);	    /* for use by the GetProfile stuff */
	GetProfileFileName() returns char *;
};
