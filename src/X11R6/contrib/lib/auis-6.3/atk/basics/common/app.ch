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


 


/* Generic application
 */

class application[app] {
    methods:
        ParseArgs(int argc,char **argv) returns boolean;
        ReadInitFile();
        Start() returns boolean;
        Run() returns int;
        Stop();
        Fork() returns boolean;
        PrintVersionNumber();
    macromethods:
        SetFork(do) (self->fork=(do))
        GetFork() (self->fork)

        SetName(str) (self->name=(str))
        GetName() (self->name)

        SetForceLoad(boolean f) ((self)->forceload=(f))
        GetForceLoad() ((self)->forceload)
        SetErrorProc(procedure p) ((self)->errorProc=(p))
        GetErrorProc() ((self)->errorProc)
        SetErrorRock(long r) ((self)->errorRock=(r))
        GetErrorRock() ((self)->errorRock)
        SetMajorVersion(long mv) ((self)->majorversion=(mv))
        GetMajorVersion() ((self)->majorversion)
        SetMinorVersion(long mv) ((self)->minorversion=(mv))
        GetMinorVersion() ((self)->minorversion)
        SetPrintVersionFlag(boolean TorF) ((self)->printversioninfo=(TorF))
	GetPrintVersionFlag() ((self)->printversioninfo)

	SetIconic(boolean val) ((self)->iconic=(val))
	GetIconic() ((self)->iconic)
      
    classprocedures:
        DeleteArgs(char **argv, int num);
        GetATKVersion() returns char *;
        InitializeObject(struct application *self) returns boolean;
        FinalizeObject(struct application *self);
    data:
        boolean fork;
        boolean forceload,readInitFile;
        char *profile; /* class to profile, "" for self, NULL for none */
        char *name;
        void (*errorProc)();
        pointer errorRock;
        char *fgcolor, *bgcolor;
        char *geometry;
        long majorversion, minorversion;
	boolean printversioninfo;
	boolean iconic;
};
