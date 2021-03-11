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
#include <AF/AFlib.h>

#ifndef TRUE
#define TRUE	1
#define FALSE	0
#endif

AFAudioConn	*aud = NULL;		/* audio connection to server */
char		*server_name = "";	/* audio server name */
int		device = 0;		/* audio device */
int		brief = 0;		/* brief mode */

/*
  AEncodeType strings
*/
char *type_names[] = {
	"MU255",
	"ALAW",
	"LIN16",
	"LIN32",
	"ADPCM32",
	"ADPCM24",
	"??" };

/*
  Open audio connection, if not already open.
*/
void open_server(void)
{
	if(aud)			/* already open? */
		return;

	aud = AFOpenAudioConn(server_name);
	if(aud == NULL) {
		fprintf(stderr, "Can't open connection to audio server\n");
		exit(1);
	}
}

/*
  Close audio server, if open.
*/
void close_server(void)
{
	if(aud)			/* if open? */
		AFCloseAudioConn(aud);
}

/*
  Show a mask state with '*' and '-'
*/
char *mask(int msk, int len)
{
	static char m[32];
	char *p;

	p = m;
	*p++ = '(';
	while(len--) {
		*p++ = (msk & 1) ? '*' : '-';
		msk >>= 1;
	}
	*p++ = ')';
	*p++ = '\0';
	return m;
}

/*
  Show the current device settings for the audio server.
*/
void show_query(void) 
{
  AFDeviceDescriptor	*adev;
  AC			ac;
  int			ndevices;
  int			i;
  char			*ptype, *rtype;
  int			inmin, incur, inmax;
  int			outmin, outcur, outmax;
  AMask			outmask, inmask, tmp;
  ABool			npass, opass;
  
  open_server();
  ndevices = ANumberOfAudioDevices(aud);
  if(!brief) {
    printf("Server %s, device summary:\n\n", AudioConnString(aud));
    printf("        Connections   To                             Gain       Pass\n");
    printf("          #  State   Phone  Type  Rate #Chan    Min  Cur  Max   thru\n");
    printf("         ------------------------------------------------------------\n");
  }
  
  /* iterate through all of the devices */
  for(i=0; i<ndevices; i++) {
    adev = AAudioDeviceDescriptor(aud, i);
    if(adev->playBufType >= UNKNOWN_ENCODETYPE)
      ptype = "?";
    else
      ptype = type_names[adev->playBufType];		
    
    if(adev->recBufType >= UNKNOWN_ENCODETYPE)
      rtype = "?";
    else
      rtype = type_names[adev->recBufType];
    
    ac = AFCreateAC(aud, i, 0, 0);
    incur = AFQueryInputGain(ac, &inmin, &inmax);
    outcur = AFQueryOutputGain(ac, &outmin, &outmax);
    AFEnableInput(ac, 0, &tmp, &inmask);
    AFEnableOutput(ac, 0, &tmp, &outmask);
    AFEnablePassThrough(ac, (ABool)0, &opass, &npass);
    
    printf("%2d   In:  %d   %-7s", i, adev->numberOfInputs, 
	   mask(inmask, adev->numberOfInputs));
	  
    printf("  %d %7s %5d    %d     %3d %3d  %3d %6s\n",
	   adev->inputsFromPhone, rtype,
	   adev->recSampleFreq, adev->recNchannels,
	   inmin, incur, inmax, npass ? "on" : "off");
    
    printf("    Out:  %d   %-7s", adev->numberOfOutputs, 
	   mask(outmask, adev->numberOfOutputs));
    
    printf("  %d %7s %5d    %d     %3d %3d  %3d\n",
	   adev->outputsToPhone, ptype,
	   adev->playSampleFreq, adev->playNchannels,
	   outmin, outcur, outmax);
    
    if(!brief)
      printf("\n");
    
    AFFreeAC(ac);
  }
}

/*
  Enable/disable inputs or outputs.
*/
void enable_io(int dir, int state, int i, int argc, char **argv)
{
	AFDeviceDescriptor	*adev;
	AC			ac;
	int			ind;
	AMask			old,new,maskv;

	if(++i >= argc) {
		fprintf(stderr,"aset: input/output number expected\n");
		exit(1);
	}
	ind = atoi(argv[i]);
	maskv = 1 << ind;
	open_server();
	ac = AFCreateAC(aud, device, 0, 0);
	adev = AAudioDeviceDescriptor(aud, device);

	if(dir) {
		if(ind < 0 || ind > adev->numberOfInputs) {
			fprintf(stderr, "aset: input number %d out of range for device %d\n",
				ind, device);
			exit(1);
		}
		if(state)
			AFEnableInput(ac, maskv, &old, &new);
		else 
			AFDisableInput(ac, maskv, &old, &new);
	} else {
		if(ind < 0 || ind > adev->numberOfOutputs) {
			fprintf(stderr, "aset: output number %d out of range for device %d\n",
				ind, device);
			exit(1);
		}
		if(state)
			AFEnableOutput(ac, maskv, &old, &new);
		else 
			AFDisableOutput(ac, maskv, &old, &new);
	}
	AFFreeAC(ac);
}

/*
  Enable/disable passthrough.
*/
void enable_pass(int state, int i, int argc, char **argv)
{
	AFDeviceDescriptor	*adev;
	AC			ac;
	int			ind;
	ABool			old,new,maskv;

	if(++i >= argc) {
		fprintf(stderr,"aset: input/output number expected\n");
		exit(1);
	}
	ind = atoi(argv[i]);
	maskv = 1 << ind;
	open_server();
	ac = AFCreateAC(aud, device, 0, 0);
	adev = AAudioDeviceDescriptor(aud, device);

	if(ind < 0 || ind > adev->numberOfInputs) {
		fprintf(stderr, "aset: input number %d out of range for device %d\n",
			ind, device);
		exit(1);
	}
	if(state)
		AFEnablePassThrough(ac, maskv, &old, &new);
	else 
		AFDisablePassThrough(ac, maskv, &old, &new);
}

/*
  Set input/output gain.
*/
void set_gain(int dir, int i, int argc, char **argv)
{
	AC	ac;
	int	gain,min,max;

	if(++i >= argc) {
		fprintf(stderr,"aset: gain expected\n");
		exit(1);
	}
	gain = atoi(argv[i]);
	open_server();
	ac = AFCreateAC(aud, device, 0, 0);
	if(dir) {
		AFQueryInputGain(ac, &min, &max);
		if(gain >= min && gain <= max)
			AFSetInputGain(ac, gain);
		else {
			fprintf(stderr, "aset: input gain must be between %d and %d\n",
				min, max);
			exit(1);
		}
	} else {
		AFQueryOutputGain(ac, &min, &max);
		if(gain >= min && gain <= max)
			AFSetOutputGain(ac, gain);
		else {
			fprintf(stderr, "aset: output gain must be between %d and %d\n",
				min, max);
			exit(1);
		}
	}
	AFFreeAC(ac);
}

/*
  Show program usage, and exit with error.
*/
void usage(void)
{
	fprintf(stderr,"usage: aset [-server host:0] option ...\n");
	fprintf(stderr,"   To specify an audio device:\n");
	fprintf(stderr,"      -device #\n");
	fprintf(stderr,"   To enable or disable a device's input:\n");
	fprintf(stderr,"      +input #		-input #\n");
	fprintf(stderr,"   To enable or disable a device's output:\n");
	fprintf(stderr,"      +output #		-output #\n");
	fprintf(stderr,"   To set a device's input gain:\n");
	fprintf(stderr,"      -ingain #\n");
	fprintf(stderr,"   To set a device's output gain:\n");
	fprintf(stderr,"      -outgain #\n");
	fprintf(stderr,"   To enable or disable passthrough:\n");
	fprintf(stderr,"      +pass #		-pass #\n");
	fprintf(stderr,"   For status information: q\n");
	fprintf(stderr,"   To set brief output mode: -b\n");
	exit(1);
}

int main(int argc, char **argv)
{
	int	i;

/* process command line options */
	for(i=1; i<argc; i++) {
		if(!strcmp(argv[i],"q"))
			show_query();
		else if(!strcmp(argv[i],"-server")) {
			close_server();			/* if already open? */
			server_name = argv[++i];
			if(i >= argc) {
				fprintf(stderr, "aset: argument expected after -server\n");
				usage();
			}
		} else if(!strcmp(argv[i],"-device")) {
			device = atoi(argv[++i]);
			if(i >= argc) {
				fprintf(stderr, "aset: argument expected after -device\n");
				usage();
			}
			open_server();
			if(device < 0 || device >= ANumberOfAudioDevices(aud)) {
				fprintf(stderr, "aset: invalid device number %d\n", device);
				exit(1);
			}
		} else if(!strcmp(argv[i],"-b")) {
			brief = TRUE;
		} else if(!strcmp(argv[i],"-input")) {
			enable_io(1, 0, i, argc, argv);
			i++;
		} else if(!strcmp(argv[i],"+input")) {
			enable_io(1, 1, i, argc, argv);
			i++;
		} else if(!strcmp(argv[i],"-output")) {
			enable_io(0, 0, i, argc, argv);
			i++;
		} else if(!strcmp(argv[i],"+output")) {
			enable_io(0, 1, i, argc, argv);
			i++;
		} else if(!strcmp(argv[i],"-ingain")) {
			set_gain(1, i, argc, argv);
			i++;
		} else if(!strcmp(argv[i],"-outgain")) {
			set_gain(0, i, argc, argv);
			i++;
		} else if(!strcmp(argv[i],"+pass")) {
			enable_pass(1, i, argc, argv);
			i++;
		} else if(!strcmp(argv[i],"-pass")) {
			enable_pass(0, i, argc, argv);
			i++;
		} else
			usage();
	}
	close_server();
	exit(0);
	/*NOTREACHED*/
}
