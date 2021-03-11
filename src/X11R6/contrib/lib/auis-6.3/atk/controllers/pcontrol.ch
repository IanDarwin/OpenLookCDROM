/* user code begins here for HeaderInfo */
/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1989 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/* $Disclaimer: 
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
* $ */

/* user code ends here for HeaderInfo */

class pcontrol {
    classprocedures:
        InitializeClass() returns boolean;
/* user code begins here for classprocedures */
/* user code ends here for classprocedures */
data:
	struct value *kb_2;
	struct pianoV *kb_2View;
	struct value *kb_3;
	struct pianoV *kb_3View;
	struct value *replay;
	struct buttonV *replayView;
	struct value *volume;
	struct fourwayV *volumeView;
	struct value *nt_10;
	struct buttonV *nt_10View;
	struct value *clear;
	struct buttonV *clearView;
	struct value *nt_11;
	struct buttonV *nt_11View;
	struct value *nt_0;
	struct buttonV *nt_0View;
	struct value *nt_1;
	struct buttonV *nt_1View;
	struct value *mode;
	struct onoffV *modeView;
	struct value *nt_2;
	struct buttonV *nt_2View;
	struct value *nt_3;
	struct buttonV *nt_3View;
	struct value *nt_4;
	struct buttonV *nt_4View;
	struct value *nt_5;
	struct buttonV *nt_5View;
	struct value *nt_6;
	struct buttonV *nt_6View;
	struct value *nt_7;
	struct buttonV *nt_7View;
	struct value *nt_8;
	struct buttonV *nt_8View;
	struct value *nt_9;
	struct buttonV *nt_9View;
	struct value *speed;
	struct sliderV *speedView;
	struct value *Read;
	struct buttonV *ReadView;
	struct value *noteout;
	struct stringV *noteoutView;
	struct value *undo;
	struct buttonV *undoView;
	struct text *score;
	struct textview *scoreView;
	struct arbiter *ab1;
	struct arbiterview *ab1View;
	struct lset *ab2;
	struct lsetview *ab2View;
	struct value *rest;
	struct buttonV *restView;
	struct value *kb_0;
	struct pianoV *kb_0View;
	struct value *Save;
	struct buttonV *SaveView;
	struct value *kb_1;
	struct pianoV *kb_1View;
/* user code begins here for classdata */
	struct value *duration;
	long lastnoteval,lastlen;
/* user code ends here for classdata */
	struct view *v;
	struct pcontrol *next;
};
