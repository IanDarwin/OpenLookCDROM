/*
 * Copyright 1993 by Digital Equipment Corporation, Maynard, Massachusetts.
 * 
 * Permission to use, copy, modify, distribute, and sell this software and its 
 * documentation for any purpose is hereby granted without fee, provided that 
 * the above copyright notice appear in all copies and that both that 
 * copyright notice and this permission notice appear in supporting 
 * documentation, and that the name of Digital not be used in advertising or 
 * publicity pertaining to distribution of the software without specific, 
 * written prior permission.  Digital makes no representations about the 
 * suitability of this software for any purpose.  It is provided "as is" 
 * without express or implied warranty.
 * 
 * DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL 
 * DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN 
 * AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF 
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <AF/AFlib.h>

#ifndef TRUE
#define TRUE	1
#define FALSE	0
#endif

char	*program_name = "aprop";
int 	show_type = TRUE;
int	spy_mode = FALSE;
char	*remove_prop = NULL;

void usage(void)
{
    fprintf(stderr, "%s: usage:\n", program_name);
    fprintf(stderr, "     -d device         Specify audio device\n");
    fprintf(stderr, "     -s server         Specify AudioFile server\n");
    fprintf(stderr, "     -remove prop      Remove a property\n");
    fprintf(stderr, "     -notype           Don't show property types\n");
    fprintf(stderr, "     -spy              Show property changes\n");
    exit(1);
}

/*
  Remove a property.
*/
void remove_property(AFAudioConn *aud, AC ac, char *propname)
{
	AAtom	id;

	id = AFInternAtom(aud, propname, ATrue);
	if(id == ANone) {
		fprintf (stderr, "%s:  no such property \"%s\"\n",
			program_name, propname);
		return;
	}
	AFDeleteProperty(ac, id);
	return;
}

/*
  Show a property.

  For now, we assume that every property is a null-terminated string.  When
  we get more properties, we will probably want to add the "thunk" formatting
  code from xprop.
*/
void show_property(AFAudioConn *aud, AC ac, AAtom prop)
{
	char	*atomname, *typename;
	unsigned char *data;
	AAtom	type;
	unsigned long nitems, left;
	int	format;

	if(AFGetProperty(ac, prop, (long) 0, (long) 100, (ABool) 0, 
		(AAtom) AAnyPropertyType, &type, &format, &nitems, &left, &data) 
		!= ASuccess) {
			fprintf(stderr, "%s: get property failed.\n",
				program_name);
			return;
	}
	atomname = AFGetAtomName(aud, prop);
	if(nitems) {
		if(show_type) {
			typename = AFGetAtomName(aud, type);
			printf("%s(%s) = %s\n", atomname, typename, data);
			AFree(typename);
		} else 
			printf("%s = %s\n", atomname, data);
		AFree(data);
	} else
		printf("%s: not found.\n", atomname);

	AFree(atomname);
}

/*
  Show property change events.
*/
void spy(AFAudioConn *aud, AC ac)
{
	AMask mask;
	AFEvent event;

	mask = APropertyChangeMask;
	AFSelectEvents(ac, mask);
	for(;;) {
		AFNextEvent(aud, &event);
		if(event.type == APropertyEvent)
			show_property(aud, ac, event.aproperty.atom);
	}	
}

/*
  List all of the defined properties for the specified device.
*/
void list_properties(AFAudioConn *aud, AC ac)
{
	int	i, nprops;
	AAtom	*props;

	props = AFListProperties(ac, &nprops);
	for(i=0; i<nprops; i++)
		show_property(aud, ac, props[i]);
	AFree(props);
}

int main(int argc, char **argv)
{
	int i;
	AFAudioConn *aud;	
	AC ac;
	int device = 0;
	char *server = "";

	program_name = argv[0];

/* parse command line options */
    for ( i = 1; i < argc; i++ ) {
	if ( strcmp( argv[i], "-d" ) == 0) {
	    if(++i < argc)
		device = atoi(argv[i]);
	    else {
		fprintf(stderr,"%s: missing device\n", argv[0]);
		exit(1);
	    }
	} else if ( strcmp( argv[i], "-s" ) == 0) {
	    if(++i < argc)
		server = argv[i];
	    else {
		fprintf(stderr,"%s: missing server name\n", argv[0]);
		exit(1);
	    }
	} else if ( strcmp( argv[i], "-remove" ) == 0) {
	    if(++i < argc)
		remove_prop = argv[i];
	    else {
		fprintf(stderr,"%s: missing property to remove\n", argv[0]);
		exit(1);
	    }
	} else if ( strcmp( argv[i], "-spy" ) == 0) {
	    spy_mode = TRUE;
	} else if ( strcmp( argv[i], "-notype" ) == 0) {
	    show_type = FALSE;
	} else if ( strcmp( argv[i], "-help" ) == 0) {
	    usage();
	} else {
	    fprintf(stderr, "%s: unknown option %s\n", argv[0], argv[i]);
	    usage();
	}
    }

	if((aud = AFOpenAudioConn(server)) == NULL) {
		fprintf(stderr, "%s: can't open connection.\n", argv[0]);
		exit(1);
	}
	ac = AFCreateAC(aud, device, 0, NULL);

	if(spy_mode)
		spy(aud, ac);
	else if(remove_prop != NULL)
		remove_property(aud, ac, remove_prop);
	else
		list_properties(aud, ac);

	AFFreeAC(ac);
	AFCloseAudioConn(aud);
	exit(0);
	/*NOTREACHED*/
}
