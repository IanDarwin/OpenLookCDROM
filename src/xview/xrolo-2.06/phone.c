/*
 * derived from the sun microsystems play.c demo.  Please send bugs,
 * fixes or comments to me directly.
 * 
 * Luis Soltero (luis@rice.edu)
 */

#include <xview/xview.h>
#include <xview/panel.h>
#include <sys/dir.h>
#include "defs.h"

static Frame volume_frame;
static Panel volume_panel;
extern Frame frame;
static Menu_item volume_prefix_item, volume_item,
  volume_use_prefix_item;

static void do_create_phone_attribute_panel();

static in_phone_button=0;
void set_phone_attributes(item, event)
Panel_item item;
Event *event;
{
	if ( in_phone_button ) {
		in_phone_button = 0;
		return;
	}
#ifdef SOUNDS
	do_create_phone_attribute_panel(item, event);
	xv_set(volume_frame, XV_SHOW, TRUE, NULL);
#else
	msg("Using the system speaker to dial only works on SparcStations");
#endif
}

extern Menu_item regex_item;

void send_email_to_selection(item, event)
Panel_item item;
Event *event;
{
	char *get_selection();
	char *cp = get_selection();
	if ( cp == NULL )
	  cp = (char *)xv_get(regex_item, PANEL_VALUE);
	if ( cp == NULL || *cp == '\0' ) {
		msg("No e-mail address");
		return;
	}
	send_email(item, event, cp);
}

void dial_a_number(item, event)
Panel_item item;
Event *event;
{
	char *get_selection();
	char *cp;
	char buff[MAX_SELN_LEN+1]; /* Same size as sel_text in get_selection */
	if ( in_phone_button ) {
		in_phone_button = 0;
		return;
	}
#ifdef SOUNDS
	/* make sure the attribute panel and all default attributes are */
	/* initialized */
	do_create_phone_attribute_panel();
	cp = get_selection();
	if ( cp == NULL )
	  cp = (char *)xv_get(regex_item, PANEL_VALUE);
	if ( cp == NULL || *cp == '\0' ) {
		msg("No number to dial");
		return;
	}
	buff[0] = '\0';
	if ( xv_get(volume_use_prefix_item, PANEL_VALUE) ) {
		strcpy(buff, xv_get(volume_prefix_item, PANEL_VALUE));
	}
	strncat(buff, cp, MAX_SELN_LEN - strlen(buff)); /* Don't overflow */
	if ( !confirm("Dial %s", buff) )
	  return;
	for( cp = buff; *cp != '\0'; cp++ ) {
		dial(*cp);
	}
#else
	msg("Using the system speaker to dial only works on SparcStations");
#endif
}

void phone_button(item, event)
Panel_item item;
Event *event;
{
	if ( event_action(event) == ACTION_MENU )
	  return;

	switch (value_from_mask (event)) {
	  case PLAIN_CLICK:			/* Plain save */
		dial_a_number(item, event);
		break;

	  case SHIFT_CLICK:			/* reload, no save first */
		set_phone_attributes(item, event);
		break;

	  case CTRL_CLICK:
		send_email_to_selection(item, event);
		break;
	}
	in_phone_button = 1;
}

#ifdef SOUNDS

#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <sys/signal.h>

#include <stropts.h>
#include <sys/ioctl.h>

#include <multimedia/libaudio.h>
#include <multimedia/audio_device.h>


#define	MAX_GAIN		(100)		/* maximum gain */

char tones[20][MAXNAMLEN];

#define TONE(x)    tones[x]
#define POUND 10
#define STAR  11
#define GONG  12

#define DEFAULT_VOLUME 50


static void init_tones()
{
	int i;
	for ( i = 0; i < 10; i++ )
	  sprintf(tones[i],"%s/touchtone.%1d.au", SOUNDS, i);
	sprintf(tones[i++], "%s/touchtone.pound.au", SOUNDS);
	sprintf(tones[i++], "%s/touchtone.star.au", SOUNDS);
	sprintf(tones[i++], "%s/drip.au", SOUNDS);
}

static void slider_volume_proc(item, value, event)
Panel_item item;
int value;
Event *event;
{
	volume(value);
	play(TONE(GONG));
}

static void volume_done_proc(item, event)
Panel_item item;
Event *event;
{
	xv_set(volume_frame, XV_SHOW, FALSE, NULL);
}

static void do_create_phone_attribute_panel()
{
	static int inited = 0;
	Menu_item prefix_item;
	if ( inited )
	  return;
	inited++;
		
	volume_frame = xv_create(frame, FRAME,
							 FRAME_LABEL, "Phone Attributes",
							 FRAME_SHOW_LABEL, TRUE,
							 NULL);
	volume_panel = xv_create(volume_frame, PANEL, NULL);
	volume_use_prefix_item = xv_create(volume_panel, PANEL_CHECK_BOX,
							PANEL_CHOOSE_ONE, FALSE,
							PANEL_CHOICE_STRINGS, "Use Dial Prefix:", NULL,
							PANEL_VALUE, defaults_get_integer("xrolo.usedialprefix", 
															  "xrolo.UseDialPrefix", 1), 
							NULL);
	volume_prefix_item = xv_create(volume_panel, PANEL_TEXT, 
								   PANEL_BLINK_CARET, TRUE,
								   PANEL_VALUE_DISPLAY_LENGTH, 24,
								   PANEL_VALUE_STORED_LENGTH, 80,
								   PANEL_VALUE, defaults_get_string("xrolo.dialprefix",
																	"xrolo.DialPrefix", ""),
								   NULL);

	volume_item = xv_create(volume_panel, PANEL_SLIDER,
			  XV_X, xv_col(volume_panel, 0),
			  XV_Y, xv_row(volume_panel, 1),
			  PANEL_LABEL_STRING, "Volume:",
			  PANEL_VALUE, defaults_get_integer("xrolo.defaultvolume",
							   "xrolo.DefaultVolume", DEFAULT_VOLUME),
			  PANEL_MIN_VALUE, 0,
			  PANEL_MAX_VALUE, MAX_GAIN,
			  PANEL_SLIDER_WIDTH, 100,
			  PANEL_SHOW_RANGE, TRUE,
			  PANEL_SHOW_VALUE, TRUE,
			  PANEL_NOTIFY_LEVEL, PANEL_DONE,
			  PANEL_NOTIFY_PROC, slider_volume_proc,
			  NULL);
	xv_create(volume_panel, PANEL_BUTTON,
			  PANEL_LABEL_STRING, "Done",
			  PANEL_NOTIFY_PROC, volume_done_proc,
			  NULL);
	window_fit(volume_panel);
	window_fit(volume_frame);
}

dial(c)
char c;
{
	int tone;
	if ( isdigit(c) ) {
		play_wait(TONE(c - '0'));
		return;
	}
	if ( c == '*'  ) {
		play_wait(TONE(STAR));
		return;
	}
	if ( c == '#' ) {
		play_wait(TONE(POUND));
		return;
	}
	if ( c == ',' ) {
		sleep(2);
		return;
	}
	tone = islower(c) ? toupper(c) : c;
	if ( tone >= 'A' && tone < 'Z') {
		if ( tone >= 'Q' )
		  tone--;
		tone -= 'A';
		tone = (tone / 3) ;
		tone += 2;
		play_wait(TONE(tone));
	}
}

play_wait(cp)
char *cp;
{
	play(cp);
	usleep(50000);
}

/*
 * This defines the tolerable sample rate error as a ratio between the
 * sample rates of the audio data and the audio device.
 */
#define	SAMPLE_RATE_THRESHOLD	(.01)

double		Savevol;		/* saved volume level */
char		*Audio_dev = "/dev/audio";

int		Audio_fd = -1;		/* file descriptor for audio device */
Audio_hdr	Dev_hdr;		/* audio header for device */
Audio_hdr	File_hdr;		/* audio header for file */


init_audio()
{
	int err;
	struct stat	st;
	static inited = 0;

	if ( inited )
	  return(1);

	/* make sure that the volume panel + defaults have been intialized */
	do_create_phone_attribute_panel();
	init_tones();

	/* Validate and open the audio device */
	err = stat(Audio_dev, &st);
	if (err < 0) {
		msg("Play: No audio device");
		return(0);
	}
	if (!S_ISCHR(st.st_mode)) {
		msg("Play: %s is not an audio device", Audio_dev);
		return(0);
	}

	/* Try it quickly, first */
	Audio_fd = open(Audio_dev, O_WRONLY | O_NDELAY);
	if ((Audio_fd < 0) && (errno == EBUSY)) {
		msg("Play: Audio device in use");
		return(0);
	}
	if (Audio_fd < 0) {
		msg("Play: Error opening audio device");
		return(0);
	}

	/* Get the device output encoding configuration */
	if (audio_get_play_config(Audio_fd, &Dev_hdr) != AUDIO_SUCCESS) {
		msg("Play: %s is not an audio device", Audio_dev);
		return(0);
	}
	inited++;
	/* set default volume level */
	volume(xv_get(volume_item, PANEL_VALUE));
	return(1);
}

close_audio()
{
	/*
	 * Though drain is implicit on close(), it's performed here
	 * for the sake of completeness, and to ensure that the volume
	 * is reset after all output is complete.
	 */
	(void) audio_drain(Audio_fd, FALSE);
	(void) audio_set_play_gain(Audio_fd, &Savevol);
	(void) close(Audio_fd);			/* close output */
}

volume(Volume)
unsigned Volume;
{
	double		vol;
	int         err;

	if ( !init_audio() )
	  return;

	if ( Volume > MAX_GAIN ) {
		msg("Volume: Volume must be in the range 0 to %d",
				MAX_GAIN);
		return;
	}
	vol = (double) Volume / (double) MAX_GAIN;
	(void) audio_get_play_gain(Audio_fd, &Savevol);
	err = audio_set_play_gain(Audio_fd, &vol);
	if (err != AUDIO_SUCCESS) {
		msg("Volume: Could not set volume for %s", Audio_dev);
		return;
	}
}

/*
 * Play a list of audio files.
 */
play(Ifile)
char *Ifile;
{
	int		cnt;
	int		ifd;
	int     err;
	static unsigned char buf[1024 * 64];		/* size should depend on sample_rate */

	if ( !init_audio() )
	  return;

/*
fprintf(stderr,"file = %s\n", Ifile);
*/
	if ((ifd = open(Ifile, O_RDONLY, 0)) < 0) {
		msg("Play: Could not open audio file %s", Ifile);
		return;
	}

	/* Check to make sure this is an audio file */
	err = audio_read_filehdr(ifd, &File_hdr, (char *)NULL, 0);

	if (err != AUDIO_SUCCESS) {
		msg("Play: %s is not a valid audio file", Ifile);
		goto closeinput;
	}

	/* Check the device configuration */
	if (audio_cmp_hdr(&Dev_hdr, &File_hdr) != 0) {
		/*
		 * The device does not match the input file.
		 * Wait for any old output to drain, then attempt
		 * to reconfigure the audio device to match the
		 * input data.
		 */
		if (audio_drain(Audio_fd, FALSE) != AUDIO_SUCCESS) {
			msg("Play: AUDIO_DRAIN error");
			goto closeinput;
		}
		if (!reconfig(Ifile))
		  goto closeinput;
	}

	/*
	 * At this point, we're all ready to copy the data.
	 */
	while ((cnt = read(ifd, (char *)buf, sizeof (buf))) >= 0) {
		/* If input EOF, write an eof marker */
		err = write(Audio_fd, (char *)buf, cnt);
		if (err != cnt) {
			msg("Play: output error");
			break;
		}
		if (cnt == 0)
		  break;
	}
	if (cnt < 0) {
		msg("Play: error reading file %s", Ifile);
	}

closeinput:
	(void) close(ifd);		/* close input file */
	audio_drain(Audio_fd, FALSE);
}


/*
 * Try to reconfigure the audio device to match the file encoding.
 * If this fails, we should attempt to make the input data match the
 * device encoding.  For now, we give up on this file.
 *
 * Returns TRUE if successful.  Returns FALSE if not.
 */
reconfig(Ifile)
char *Ifile;
{
	int	err;
	char	mesg[AUDIO_MAX_ENCODE_INFO];

	Dev_hdr = File_hdr;
	err = audio_set_play_config(Audio_fd, &Dev_hdr);

	switch (err) {
	  case AUDIO_SUCCESS:
		return (TRUE);

	  case AUDIO_ERR_NOEFFECT:
		/*
		 * Couldn't change the device.
		 * Check to see if we're nearly compatible.
		 * audio_cmp_hdr() returns >0 if only sample rate difference.
		 */
		if (audio_cmp_hdr(&Dev_hdr, &File_hdr) > 0) {
			double	ratio;
			
			ratio = (double) abs((int)
								 (Dev_hdr.sample_rate - File_hdr.sample_rate)) /
								   (double) File_hdr.sample_rate;
			if (ratio <= SAMPLE_RATE_THRESHOLD) {
				msg("Play: WARNING: %s sampled at %d, playing at %d",
					    Ifile, File_hdr.sample_rate,
					    Dev_hdr.sample_rate);
				return(TRUE);
			}
			msg("Play: %s sample rate %d not available",
					Ifile, File_hdr.sample_rate);
			return (FALSE);
		}
		(void) audio_enc_to_str(&File_hdr, mesg);
		msg("Play: %s encoding not available: %s",Ifile,mesg);
		return (FALSE);

	  default:
		msg("Play: i/o error (set config)");
		abort();
	}
}


#endif
