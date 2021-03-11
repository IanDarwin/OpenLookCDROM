/* Copyright 1992 Carnegie Mellon University All rights reserved.
  $Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
 */

#include <andrewos.h>
#include <class.h>
#include <prefeda.eh>
#include <prefs.ih>
#include <pintv.ih>
#include <eza.ih>
#include <environ.ih>
#include <filetype.ih>

static char *fakeargv[]=
{
    "prefed",
    NULL,
    NULL
};

boolean prefedapp__InitializeObject(classID, self)
struct classheader *classID;
struct prefedapp *self;
{
    struct ezapp *e=(struct ezapp *)self;
    e->defaultObject="prefs";
    return TRUE;
}

boolean prefedapp__ParseArgs(self, argc, argv)
struct prefedapp *self;
int argc;
char **argv;
{
    boolean result=super_ParseArgs(self, argc, argv);
    if(((struct ezapp *)self)->files==NULL) {
	fakeargv[1]=environ_GetProfile("PrefEdDefaultFile");
	if(fakeargv[1]==NULL) {
	    fakeargv[1]=environ_GetProfileFileName();
	    if(fakeargv[1] && index(fakeargv[1], 'X')) {
		static char buf[1024], *p;
		strcpy(buf, fakeargv[1]);
		p=rindex(buf, '/');
		if(p==NULL) p=buf;
		else p++;
		strcpy(p, ".preferences");
		fprintf(stderr, "prefed WARNING: prefed would probably corrupt '%s', defaulting to '%s' instead.\nSee help 'prefed' for more information.\n", fakeargv[1], buf);
		fakeargv[1]=buf;
	    }
	}
	return result && super_ParseArgs(self, 2, fakeargv);
    } else return result;
}

void prefedapp__FinalizeObject(classID, self)
struct classheader *classID;
struct prefedapp *self;
{
}

boolean prefedapp__Start(self)
struct prefedapp *self;
{
    /* XXX make sure the default object type is a prefs!
	this will override any settings in an initfile! */
    filetype_AddEntry("*", "prefs", "");
    return super_Start(self);
}
