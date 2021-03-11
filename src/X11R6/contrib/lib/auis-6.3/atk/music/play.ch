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


 

/* play.H
	play notes

	At present only the RT has a speaker.
*/

package play {

classprocedures:

	InitializeClass() returns boolean;

	Volume(long v);
		/* set overall volume for playback  (0 <= v <= 9),
		  but RT/PC only distinguishes 3 levels so v is mapped:  0 is off;  
		 1,2,3 are low volume;  4,5,6 are medium;  7,8,9 are high) */

	Retard(long v);
		/* set overall retard for playback  (-30 <= v <= 40)
		note durations are multiplied by (1+Retard/50),
		so negative values increase speed  */

	Tuning(unsigned char *name);
		/* choose scale temper from among Pythagorean, Just, Mean, or Equal 
			(only the first letter of name is tested) */

	Tone(double freq, long duration, long volume);
		/* play the given frequency for the given time at the given volume
		  time is in milliseconds.   0 <= volume <= 9
		  reasonable frequencies range from roughly  80.0 to 4000.0  */

	Notes(unsigned char *s);
		/*  Play the sequence of notes in the string s.
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

		spaces, newlines, tabs, and other undefined characters are ignored,
			when they appear outside the above code sequences
			No whitespace is allowed between a code letter and its operand
		if any x above is of form   =variable;   it is ignored 
			and the minimum value is used
		 */
};
