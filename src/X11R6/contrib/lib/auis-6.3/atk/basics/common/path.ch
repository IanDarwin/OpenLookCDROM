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


/*
The path class is for caching information about a pathname --
whether or not it names a directory, and if so, what the 
directories and files are that it contains.

The information returned by
IsDirectory, GetFiles, GetDirs, GetNumFiles and GetNumDirs
is only as recent as the last call to Scan. Scan will stat the
directory, and re-read it only if necessary.
*/

class path[path] : observable[observe] {
  classprocedures:
    InitializeObject(struct path *self) returns boolean;
    FinalizeObject(struct path *self);
    Create(char *filepath) returns struct path *;
    TruncatePath(char *filepath, char *resultPath, long limit, boolean tryHome) returns char *;
    FreeTruncatedPaths();
    InputTruncatedPathCache(FILE *fp);
    OutputTruncatedPathCache(FILE *fp);
    ModifyToParentDirectory(char *filepath, boolean isDirectory) returns boolean;
    UnfoldFileName(char *fromString, char *toString, char *baseName) returns char *;
  methods:
    Scan(boolean statEverything) returns boolean;
    ReleaseFiles(char **filelist);
    ReleaseDirs(char **dirlist);
    Input(FILE *fp);
    Output(FILE *fp);
    IsDirectory() returns boolean;
    GetFiles() returns char **;
    GetDirs() returns char **;
    GetNumFiles() returns long;
    GetNumDirs() returns long;
    GetTruncatedPath() returns char *;
  macromethods:
    GetPath() ((self)->filepath)
  data:
    char *filepath;
    char *truncatedPath;
    time_t changetime;
    boolean haveScanned;
    boolean knowIsDir;
    boolean mayBeWrong; /* if the last scan didn't stat everything */
    boolean isDir;
    char **files;
    char **dirs;
    long numFiles;
    long numDirs;
};
