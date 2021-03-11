/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
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

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/music/RCS/play.c,v 1.16 1992/12/15 21:37:21 rr2b R6tape $";
#endif

 


/* play.c
	proctable entry for playing music on RT and RS/6000

	some bits are from pcontrol.c

	Tempo is expressed in the number of quarter notes per minute.
	Note durations are computed in milliseconds.
	The duration sent to speaker is in 1/128 th seconds.
	At a tempo of 120, a quarter note takes 500 milliseconds.
	The Retard value is applied after all other duration computations.

	The global Volume is added to the local volume, according to volTable.
	Volume values are entered in the range 0...9, but are coded down to
	the range 0...3 for the RT.
*/

#include <stdio.h>
#include <system.h>
#include <sys/ioctl.h>

#ifdef IBM032_ENV
#include  <machineio/speakerio.h>
#else
#if defined(_IBMR2)
#include <sys/hft.h>
#include <fcntl.h>
#endif /* _IBMR2 */
#endif

#include <class.h>
#include <proctbl.ih>
#include <play.eh>

#define WHOLENOTE  2000	/* milliseconds in a whole note at tempo 120 */

static double freq[85];		/* frequencies for various notes (0 is rest) */

static int Speaker = 0;	/* file descriptor for speaker */

static long Retard;		/* slow down the music
			Applied after all other note duration computation.
			The note length becomes  length(1+Retard/50) */
static long Volume;		/* global volume */

/* table mapping local vol x  GlobalVolume into an output volume value. */
static long volTable[] = {
			      /* vol code	0  1  2  3  */
	/* Volume code = 0 */	0, 0, 0, 0,
	/* Volume code = 1 */	1, 1, 2, 3,
	/* Volume code = 2 */	2, 2, 3, 3,
	/* Volume code = 3 */	3, 3, 3, 3
};

	boolean
openSpeaker(vol)
	long vol;
{
#if defined(IBM032_ENV)
	Speaker = open("/dev/speaker", 1);
	return Speaker > 0;
#else /* defined(_IBM032) */
#if defined(_IBMR2)
	{
		int fd, n;
		struct hftqstat qs;
		struct hfbuf qsp;
		struct hfrconf volCtl;
		char hft[20];
		ulong volCode[4];

		volCode[0]= HFVOLOFF;
		volCode[1]=HFVOLLOW;
		volCode[2]=HFVOLMED;
		volCode[3]=HFVOLHIGH;
		qsp.hf_bufp=(char *)(&qs);
		qsp.hf_buflen=sizeof(qs);
		volCtl.hf_op=HFCHGVOLUME;
		volCtl.hf_obj=HFVOLOFF;
		
/* find the active virtual terminal. THIS IS A KLUDGE. */

		/* start by opening any hft, say 0 */
		fd = open("/dev/hft/0", 0, 0);
		if (fd < 0) return FALSE;		/* no hft/0 */
		n = ioctl(fd, HFTQSMGR, &qsp);

		/* configure the volume */
		
		vol = (vol+2)/3;	/* 0->0;  1,2,3->1;  4,5,6->2;  7,8,9->3 */
		volCtl.hf_obj = volCode[volTable[4 * ((Volume+2)/3)  + vol]];
		ioctl(fd, HFRCONF, &volCtl);
		close(fd);

		/* find active terminal */
		for (n = qs.hf_numvts - 1; n >= 0; n--)
			if (HFVTACTIVE & qs.hf_vtinfo[n].hf_vtstate)
				break;
		if (n < 0) return FALSE;

		/* open the active terminal */
		sprintf(hft, "/dev/hft/%d", qs.hf_vtinfo[n].hf_vtid);
		Speaker = open(hft, O_WRONLY, 0);
	}
	return Speaker > 0;
#else /* defined(IBMR2) */
	return FALSE;
#endif /* defined(_IBM032) */
#endif /* defined(_IBMR2) */
}

closeSpeaker()
{
#if defined(IBM032_ENV) || defined (_IBMR2)
	close(Speaker);
#endif /* defined(IBM032_ENV) || defined (_IBMR2) */
}

#ifdef IBM032_ENV
/* setbl(b, freq)
	sets the speaker control block bl for frequency freq
*/
	void
setbl(b,freq)
	struct spk_blk *b;
	double freq;
{
	if (freq < 23) {
		b->freqhigh=0;
		b->freqlow=SPKOLOMIN;
	} else if (freq < 46) {
		b->freqhigh=64;
		b->freqlow = (char) ((6000.0 / (float)freq) - 9.31);
	} else if (freq < 91) {
		b->freqhigh=32;
		b->freqlow = (char) ((12000.0 / (float)freq) - 9.37);
	} else if (freq < 182) {
		b->freqhigh=16;
		b->freqlow = (char) ((24000.0 / (float)freq) - 9.48);
	} else if (freq < 363) {
		b->freqhigh=8;
		b->freqlow = (char) ((48000.0 / (float)freq) - 9.71);
	} else if (freq < 725) {
		b->freqhigh=4;
		b->freqlow = (char) ((96000.0 / (float)freq) - 10.18);
	} else if (freq < 1433) {
		b->freqhigh=2;
		b->freqlow = (char) ((192000.0 / (float)freq) - 11.10);
	} else if (freq < 12020) {
		b->freqhigh=1;
		b->freqlow = (char) ((384000.0 / (float)freq) - 12.95);
	} else {
		b->freqhigh=0;
		b->freqlow=SPKOLOMIN;
	}
}
#endif /* IBM032_ENV */


/* play(note, length, tempo, vol)
	plays a single note of the given length and volume
	note is 0..84.  0 is rest;  1..84 are keys (O0C .. O6B)
	length is duration in milliseconds at tempo 120
*/
	void
playonenote(freq, length, tempo, vol)
	double freq;
	long length, tempo, vol;
{
#if defined(IBM032_ENV)
	struct spk_blk  b;

	vol = (vol+2)/3;	/* 0->0;  1,2,3->1;  4,5,6->2;  7,8,9->3 */
	b.volume = volTable[4 * ((Volume+2)/3)  + vol];

	/* convert milliseconds to 1/128 ths:  multiply by 128/1000
		adjust for tempo:  multiply by  120/tempo  */
	b.duration = (length * 128 * 120 / tempo) / 1000;		
	b.duration += b.duration * Retard/50;

#if 0	/* debugging info */
	printf("freq %.2f    vol %d  Volume %d  b.volume %d\n", freq, vol, Volume, b.volume);
	printf("tempo %d  length %d  Retard %d   b.duration %d\n", 
		tempo, length, Retard, b.duration);
#endif /* 0	 */

	setbl(&b, freq);
	write(Speaker, &b, sizeof(b));

#if 0
	/* this code used to be necessary to make it sound good 
	at present it seems to make clicking noises after notes */
	b.volume = 0;	
	b.duration = 1;
	write(Speaker, &b, sizeof(b));
#endif /* 0 */

#else /* defined(_IBMR2) */
#if defined(_IBMR2)
	/* enqueue a note on Speaker */

	struct hfsound sound = {
			HFINTROESC, HFINTROLBR, HFINTROEX,
			0, 0, 0, sizeof(struct hfsound) - 3,
			HFSOUNDCH, HFSOUNDCL,
		2, 0, HFEXECALWAYS, 0, 
		{0, 0},	/* char hf_dur[2];   duration in 1/128 seconds */
		{0, 0}		/* char hf_freq[2];  frequency in hertz */
	};
         union {
		char c[4];
		long d;
	} kludge;
	

	/* convert milliseconds to 1/128 ths:  multiply by 128/1000
		adjust for tempo:  multiply by  120/tempo  */
	kludge.d = (length * 128 * 120 / tempo) / 1000;		
	kludge.d += kludge.d * Retard/50;
	sound.hf_dur[0] = kludge.c[2];
	sound.hf_dur[1] = kludge.c[3];

	kludge.d = freq;		/* converts real to long */
	sound.hf_freq[0] = kludge.c[2];
	sound.hf_freq[1] = kludge.c[3];

	write(Speaker, &sound, sizeof(sound));

	/* try to get a 128'th silence by playing an ultra high frequency */
	sound.hf_dur[0] = 0;
	sound.hf_dur[1] = 1;
	kludge.d = freq;
	sound.hf_freq[0] = 255;
	sound.hf_freq[1] = 255;
	write(Speaker, &sound, sizeof(sound));
#endif /* _IBMR2 */
#endif /* IBM032_ENV */
}


enum interval {I76, I77, I90, I92, I100, I112, I114, I117};

	/* cent: 1.00057778950655485929   1200'th root of 2 */
double Ifactor[] = {
	1.04487715286087075242,		/* cent ^ 76 */
	1.04548087191543268121,		/* cent ^ 77 */
	1.05336103595483586152,		/* cent ^ 90 */
	1.05457862951601300325,		/* cent ^ 92 */
	1.05946309435929526455,		/* cent ^ 100  12'th root of 2 */
	1.06683224294535754535,		/* cent ^ 112 */
	1.06806540804785154947,		/* cent ^ 114 */
	1.06991782890027806748,		/* cent ^ 117 */
};

enum interval *TemperTable;	/* points to one of the following four 
					temper tables */

/* each temper table is a list of the (geometric) intervals between 
   successive keys of the chromatic scale, starting with C. 
An entry in the table is an index into Ifactor, which gives the factor. */

enum interval Pythagorean[] = {
	I90, I114, I90, I90, I114, I90, I114, I90, I114, I90, I90, I114
}; 

enum interval JustIntonation[] = {
	I112, I92, I112, I90, I92, I112, I92, I112, I92, I90, I112, I92
}; 	/* also called Ptolmeaic */

enum interval MeanTone[] = {
	I117, I76, I117, I117, I76, I117, I76, I117, I76, I117, I117, I76
}; 	 /* sum is 1199.  octaves are shorter */

enum interval EqualTemper[] = {
	I100, I100, I100, I100, I100, I100, I100, I100, I100, I100, I100, I100
}; 

/* InitFreq(tbl)
	initialize frequency table according TemperTable
*/
	void
InitFreq(tbl)
	enum interval *tbl;
{
#if 0
	static char *(names[]) = { 
		"","C","C#","D","D#","E","F","F#","G","G#","A","A#","B","C"
	};
#endif /* 0 */

	long note;
	if (tbl == TemperTable) 
		return;
	TemperTable = tbl;
	/* start at A (note 46) = 440 and work in both directions */
	freq[46] = 440.0;
	for (note=47; note <= 84; note++)
		freq[note] = freq[note-1] 
				* Ifactor[(long)TemperTable[(note-2)%12]];
	for (note=45; note>=1; note--)
		freq[note] = freq[note+1] 
				/ Ifactor[(long)TemperTable[(note-1)%12]];
#ifndef _IBMR2
	freq[0] = 0.0;	/* rest */
#else /*  _IBMR2 */
	freq[0] = 12000.0;	/* try using a high tone to get none */
#endif /*  _IBMR2 */

#if 0
	for (note = 0; note <= 84; note++) 
		printf("%2d  %s \t %.2f\n", 
			note, names[(note-1)%12 +1], freq[note]);
#endif /* 0 */
}


	void
play_volume(v)
	long v;
{
	if (v < 0) Volume = 0;
	else if (v > 9) Volume = 9;
	else Volume = v;
}

	void
play__Volume(ClassID, v)
	struct classheader *ClassID;
	long v;
{
	play_volume(v);
}

	void
play_retard(v)
	long v;
{
	if (v < -30) Retard = -30;
	else if (v > 40) Retard = 40;
	else Retard = v;
}

	void
play__Retard(ClassID, v)
	struct classheader *ClassID;
	long v;
{
	play_retard(v);
}


	void
play_tuning(name)
	unsigned char *name;
{
	switch (*name) {
	case 'E':		InitFreq(EqualTemper);	break;
	case 'P':		InitFreq(Pythagorean);	break;
	case 'J':		InitFreq(JustIntonation);	break;
	case 'M':	InitFreq(MeanTone);	break;
	}		
}

/* play__Tuning(name)
	choose tuning from among Pythagorean, Just, Mean, or Equal 
	(only the first letter is tested) 
*/
	void
play__Tuning(ClassID, name)
	struct classheader *ClassID;
	unsigned char *name;
{
	play_tuning(name);
}


	void
play_tone(freq, duration)
	double freq;
	long duration;
{
	long vol = 3;
	if (duration < 0) return;
	if (duration > 10000) 
		/* if more than 10 seconds, give 5 seconds */
		duration = 5000;
	if ( ! openSpeaker(vol))
		return;
	playonenote(freq, duration, 120, vol);
	closeSpeaker();
}

	void
play__Tone(ClassID, freq, duration, volume)
	struct classheader *ClassID;
	double freq;
	long duration, volume;
{
	if (volume < 0) volume = 0;
	else if (volume > 9) volume = 9;
	if (duration < 0) return;
	if (duration > 10000) duration = 5000;	/* if more than 10 seconds, give 5 seconds */
	if ( ! openSpeaker(volume))
		return;
	playonenote(freq, duration, 120, volume);
	closeSpeaker();
}



	static long 
getint(codepp, min, max)
	unsigned char **codepp;
	long min, max;
{
	long v;
	unsigned char c;

	v = 0;
	if (*((*codepp)+1) == '=') {
		/* skip the Basic   =var;   phrase */
		while (TRUE) {
			c = *((*codepp) + 1);
			if (c == '\0'  ||  c == ';') break;
			(*codepp)++;
		}
		return min;
	}
	while (TRUE) {
		c = *((*codepp) + 1);
		if (c == '\0'  ||  c < '0'  ||  c > '9') break;
		v = 10 * v  +  c - '0';
		(*codepp)++;
	}

	if (v < min) return min;
	if (v > max) return max;
	return v;
}

/* here are the possible notes: 
	"C", 1,	"C#", 2,	"C+", 2,
	"D-", 2,	"D", 3,	"D#", 4,	"D+", 4,
	"E-", 4,	"E", 5,
	"F", 6,	"F#", 7,	"F+", 7,
	"G-", 7,	"G", 8,	"G#", 9,	"G+", 9,
	"A-", 9,	"A", 10,	"A#", 11,	"A+", 11,
	"B-", 11,	"B", 12
*/

/* for each letter, this table gives the basic note */
long NoteVal[] = {
	/* A */	10, 
	/* B */	12, 
	/* C */	1, 
	/* D */	3, 
	/* E */	5, 
	/* F */	6, 
	/* G */	8
};


/* getnote(codepp, lenp)
	*codep points to a note letter
	lenp points to a temporary containing its intended length
	check for subsequent  #, +, -, and .    advancing codepp
	adjust note and length
*/
	static long
getnote(codepp, lenp)
	unsigned char **codepp;
	long *lenp;
{
	long note;
	unsigned char c;

	note = NoteVal[**codepp - 'A'];
	c = *((*codepp) + 1);
	if (c == '#'  ||  c == '+') {
		note++;
		(*codepp)++;
	}
	else if (c == '-') {
		note--;
		(*codepp)++;
	}

	while (*((*codepp) + 1) == '.') {
		*lenp  =  *lenp * 3 / 2;
		(*codepp)++;
	}
	return note;
}
	
/* play_notes(s)
	 Play the sequence of notes in the string s.
	The string has codes as defined for Microsoft BASIC:

		Ox		(capital Oh) choose octave
				each octave starts at C and ascends to B
				O3 starts with middle C
				(0 <= x <= 6)  default 4
		<		change to next lower octave		
		>		change to next higher octave

		Lx		length of note is 1/x
				x is any value with 1 <= x <= 64
				default 4
			L1	whole note
			L2	half note
			L4	quarter note
			L8	eighth note
			L16 L32 L64  etc.

		Px		(Pause) rest for length x,
				where x is as for Lx

		C, C#, D, D#, E, F, F#, G, G#, A, A#, B
				play the note in the current octave
				with the current volume and duration.
				C# may be written as C+ or D-
				and similarly for E#, F#, G#, and A# 

		. 		immediately after a note, multiples its duration
				by 3/2 (multiple dots repeat the multiplication)

		Tx		Tempo.  sets the number of quarter notes in a
				minute (32 <= x <= 255)  default 120

		Nx		Note.  plays x'th note  (0 <= x <= 84) 
				n=0 is rest.  Notes 1-12 are in octave 0,
				13-24 in octave 1, ...,  73-84 in octave 6
				middle C is note 37

		Vx		relative volume for subsequent notes
				(0 <= x <= 9)  default 3
				(the RT/PC only distinguishes three levels)

		Mc		ignored (where c is one character)
		Xvariable;	ignored

	spaces newlines tabs and other undefined characters are ignored
	if any x above is of form  =variable;  it is ignored
 */
	void
play_notes(s)
	unsigned char *s;
{
	unsigned char *codep;	/* points to string being sounded */
	long 	length;		/* milliseconds per note */
	long	octave;		/* 0 <= octave <= 6:  3 C is middle C */
	long	vol;		/* local volume   0 <= vol <= 3 */
	long tempo;		/* number of quarter notes per minute */
	long t, tlen;		/* temporaries */
	
	tempo = 120;	
	length = WHOLENOTE / 4;	/* at tempo 120, a quarter note is 500 milliseconds */
	octave = 4;
	vol = 3;

	if ( ! openSpeaker(vol)) 
		return;

	for (codep = s; *codep != '\0'; codep ++) {
		switch (*codep) {

		case 'O':	octave = getint(&codep, 0, 6);	break;

		case '>':	if (octave < 6) octave++;  		break;
		case '<':	if (octave >0 ) octave--;  		break;

		case 'L': length = WHOLENOTE
				 / getint(&codep, 1, 64);	break;

		case 'P': playonenote(freq[0], WHOLENOTE 
					/ getint(&codep, 1, 64), 
				tempo, 0);		break;

		case 'C':  case 'D':  case 'E':  case 'F':  
		case 'G':  case 'A':  case 'B': 
			tlen = length;
			t = getnote(&codep, &tlen);
			playonenote(freq[t + 12 * octave], 
				tlen, tempo, vol);		break;	

		case 'T':	 tempo = getint(&codep, 32, 255);	break;

		case 'N':  playonenote(freq[getint(&codep, 0, 84)], 
				length, tempo, vol);		break;

		case 'V':	 vol = getint(&codep, 0, 9);		
#ifdef _IBMR2
				/* reconfigure with new volume */
				closeSpeaker();
				if ( ! openSpeaker(vol)) 
					return;
#endif
				break;

		case'M':	 codep++;			break;
		case 'X':  while (*(codep+1) && *(codep+1) != ';')
					codep++;		break;
		}
	}
	closeSpeaker();
}

	void
play__Notes(ClassID, s)
	struct classheader *ClassID;
	unsigned char *s;
{
	play_notes(s);
}

	boolean
play__InitializeClass(ClassID)
	struct classheader *ClassID;
{
	proctable_DefineProc("play-volume",play_volume, NULL, NULL,
		"set global volume for playing music   (arg is int)");
	proctable_DefineProc("play-retard",play_retard, NULL, NULL,
		"set retard factor for playing music   (arg is int)");
	proctable_DefineProc("play-tuning",play_tuning, NULL, NULL,
		"set tuning   (arg is \"E\", \"J\", \"M\", or \"P\")");
	proctable_DefineProc("play-tone",play_tone, NULL, NULL,
		"play a single tone (args are {double}freq  &  {long}duration in 128'ths sec");
	proctable_DefineProc("play-notes",play_notes, NULL, NULL,
		"play music   (arg is string of codes)");

	Retard = 0;	/* normal speed */
	Volume = 3;
	InitFreq(EqualTemper);

#if defined(IBM032_ENV) || defined (_IBMR2)
	return TRUE;
#else /* defined(IBM032_ENV) || defined (_IBMR2) */
	return FALSE;
#endif /* defined(IBM032_ENV) || defined (_IBMR2) */
}
