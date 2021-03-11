/* 
 *  [ ctwm ]
 *
 *  Copyright 1992 Claude Lecommandeur.
 *            
 * Permission to use, copy, modify  and distribute this software  [ctwm] and
 * its documentation for any purpose is hereby granted without fee, provided
 * that the above  copyright notice appear  in all copies and that both that
 * copyright notice and this permission notice appear in supporting documen-
 * tation, and that the name of  Claude Lecommandeur not be used in adverti-
 * sing or  publicity  pertaining to  distribution of  the software  without
 * specific, written prior permission. Claude Lecommandeur make no represen-
 * tations  about the suitability  of this software  for any purpose.  It is
 * provided "as is" without express or implied warranty.
 *
 * Claude Lecommandeur DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL  IMPLIED WARRANTIES OF  MERCHANTABILITY AND FITNESS.  IN NO
 * EVENT SHALL  Claude Lecommandeur  BE LIABLE FOR ANY SPECIAL,  INDIRECT OR
 * CONSEQUENTIAL  DAMAGES OR ANY  DAMAGES WHATSOEVER  RESULTING FROM LOSS OF
 * USE, DATA  OR PROFITS,  WHETHER IN AN ACTION  OF CONTRACT,  NEGLIGENCE OR
 * OTHER  TORTIOUS ACTION,  ARISING OUT OF OR IN  CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Claude Lecommandeur [ lecom@sic.epfl.ch ][ April 1992 ]
 */
/*
 * These routines were extracted from the sound hack for olvwm3.3 by 
 * Andrew "Ender" Scherpbier (turtle@sciences.sdsu.edu)
 * and modified by J.E. Sacco (jsacco @ssl.com)
 */

#include <rplay.h>
#include <string.h>
#include <stdio.h>

char *eventNames[] =
{
    "<EventZero>",
    "<EventOne>",
    "KeyPress",
    "KeyRelease",
    "ButtonPress",
    "ButtonRelease",
    "MotionNotify",
    "EnterNotify",
    "LeaveNotify",
    "FocusIn",
    "FocusOut",
    "KeymapNotify",
    "Expose",
    "GraphicsExpose",
    "NoExpose",
    "VisibilityNotify",
    "CreateNotify",
    "DestroyNotify",
    "UnmapNotify",
    "MapNotify",
    "MapRequest",
    "ReparentNotify",
    "ConfigureNotify",
    "ConfigureRequest",
    "GravityNotify",
    "ResizeRequest",
    "CirculateNotify",
    "CirculateRequest",
    "PropertyNotify",
    "SelectionClear",
    "SelectionRequest",
    "SelectionNotify",
    "ColormapNotify",
    "ClientMessage",
    "MappingNotify",
    "Startup",
    "Shutdown"
};

#define NEVENTS         (sizeof(eventNames) / sizeof(char *))

RPLAY *rp[NEVENTS];

static int need_sound_init = 1;
static int sound_fd = 0;
static int sound_state = 1;
static int startup_sound = NEVENTS -2;
static int exit_sound = NEVENTS -1;
static char hostname[200];

/*
 * initialize
 */
static
sound_init ()
{
    int i;
    FILE *fl;
    char buffer[100];
    char *token;
    char *home;
    char soundfile [256];

    need_sound_init = 0;
    if (sound_fd == 0) {
        if (hostname[0] == '\0') {
		strcpy(hostname, rplay_default_host());
	}
	
        if ((sound_fd = rplay_open (hostname)) < 0)
		rplay_perror ("create");
    }

    /*
     * Destroy any old sounds
     */
    for (i = 0; i < NEVENTS; i++) {
        if (rp[i] != NULL)
	    rplay_destroy (rp[i]);
	rp[i] = NULL;
    }

    /*
     * Now read the file which contains the sounds
     */
    soundfile [0] = '\0';
    if ((home = getenv ("HOME")) != NULL) strcpy (soundfile, home);
    strcat (soundfile, "/.ctwm-sounds");
    fl = fopen (soundfile, "r");
    if (fl == NULL)
	return;
    while (fgets (buffer, 100, fl) != NULL) {
	token = strtok (buffer, ": \t");
	if (token == NULL)
	    continue;
        for (i = 0; i < NEVENTS; i++) {
	    if (strcmp (token, eventNames[i]) == 0) {
	        token = strtok (NULL, " \t\r\n");
	        if (token == NULL || *token == '#' || isspace (*token))
		    continue;
	        rp[i] = rplay_create (RPLAY_PLAY);
	        if (rp[i] == NULL) {
		    rplay_perror ("create");
		    continue;
		}
	        if (rplay_set(rp[i], RPLAY_INSERT, 0, RPLAY_SOUND, token, NULL)
		    < 0)
		    rplay_perror ("rplay");
	    }
	}
    }
    fclose (fl);
}


/*
 * Play sound
 */
play_sound (snd)
int snd;
{
    if (sound_state == 0)
	return;

    if (need_sound_init)
	sound_init ();

    if (rp[snd] == NULL)
	return;
    if (rplay (sound_fd, rp[snd]) < 0)
	rplay_perror ("create");
}

play_startup_sound()
{
    play_sound(startup_sound);
}

play_exit_sound()
{
    play_sound(exit_sound);
}

/*
 * Toggle the sound on/off
 */
toggle_sound ()
{
    sound_state ^= 1;
}


/*
 * Re-read the sounds mapping file
 */
reread_sounds ()
{
    sound_init ();
}

/*
 * Set the SoundHost and force the sound_fd to be re-opened.
 */
set_sound_host(host)
char	*host;
{
	strcpy(hostname, host);
	if (sound_fd != 0)
	{
		rplay_close(sound_fd);
	}
	sound_fd = 0;
}

