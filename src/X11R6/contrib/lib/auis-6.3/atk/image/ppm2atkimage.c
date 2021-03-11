/* Copyright 1993 Carnegie Mellon University All rights reserved.
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

#ifdef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/image/RCS/ppm2atkimage.c,v 1.6 1994/02/17 20:45:24 rr2b Exp $";
#endif
#include <andrewos.h> /* strings.h */
#include <stdio.h>
#include <ctype.h>
#include <class.h>
#include <pbm.ih>
#include <image.ih>

/* include the ones utilized, but not by this .c file itself */
#define class_StaticEntriesOnly
#include <observe.ih>
#include <proctbl.ih>
#include <dataobj.ih>
#undef class_StaticEntriesOnly

main(argc, argv)
    int argc;
    char **argv;
{
    long ret, saveQuality = -1;
    struct pbm *self;
    FILE *f = stdin;
    char *saveformat = NULL;
    boolean qualityComing = FALSE;

    if(argc > 1) {
	char *arg, *lastarg = NULL;
	while(argc > 1) {
	    arg = argv[--argc];
	    switch(*arg) {
		case '-':
		    switch(*(arg+1)) {
			    case 'g':
			    case 'G':
				if( strncmp(arg+1, "GIF", 3) == 0 ||
				   strncmp(arg+1, "gif", 3) == 0) 
				    saveformat = "gif";
				break;
			    case 'j':
			    case 'J':
				if( strncmp(arg+1, "jpeg", 3) == 0 ||
				   strncmp(arg+1, "JPEG", 3))
				    saveformat = "jpeg";
				break;
			    case 'q':
				if(*(arg+2) != (char) 0)
				    saveQuality = atoi(arg+2);
				else qualityComing = TRUE;
				break;
			    default:
				break;
		    }
		    break;
		default:
		    if(qualityComing) {
			saveQuality = atoi(arg);
		    }
		    else {
			if((f = fopen(arg, "r")) == NULL) {
			    fprintf(stderr, "ppm2atkimage: couldn't open %s for reading.\n", arg);
			    exit(-1);
			}
		    }
		    break;
	    }
	    lastarg = arg;
	}	
    }

    class_Init(AndrewDir("/dlib/atk"));
    observable_StaticEntry;
    proctable_StaticEntry;
    dataobject_StaticEntry;

    class_Load("pbm");
    if(pbm_Load(self = pbm_New(), NULL, f) == 0) {
	if(saveQuality > 0)
	    pbm_SetJPEGSaveQuality(self, saveQuality);
	if(saveformat)
	    pbm_SetSaveFormatString(self, saveformat);
	if(pbm_Write(self, stdout, pbm_GetID(self), -1) == dataobject_NOREADERROR)
	    exit(0);
    }
    else {
	exit(-1);
    }
}
