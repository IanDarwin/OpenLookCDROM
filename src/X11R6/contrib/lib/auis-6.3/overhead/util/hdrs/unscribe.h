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

/*
	Include file for clients of unscribe routines.
*/


struct ScribeState {
    int	statecode;	/* current state */
    int	previous_state;	/* last state, before \ processing */
    int	writecount;

    /* The following are only used by Version 10 and higher */
    int	begindatacount;	/* depth in dataobject tree. */
    int	keywordpos;	/* position in keyword buffer. */
    char *linefrag;	/* Line buffer. */
    char *keyword;	/* Keyword buffer. */
    char *stylebuf;	/* Buffer for {} contents. Used to name views */
    short lmargin;	/* left margin of current line */
    short rmargin;	/* right margin of current line */
    short justify;	/* justification for this line */
    short indent;	/* indentation for this line */
    short specials;	/* count of extra characters needed at end of line */
    short face;		/* face state from previous line */
    short newpara;    /* flag: indicates that the first line of the paragraph is yet to be printed */
    struct StateVector *vector;	/* Stack of style states kept as a linked list */
};

extern int UnScribeInit();
		/* int UnScribeInit(fieldvalue, refstate)
			char *fieldvalue;
			struct ScribeState **refstate;
		Pass it the value of the X-Andrew-ScribeFormat: header
		to see if the package can handle this format.
		Returns a code value >= 0 for OK, < 0 for error.
		Remember this code value to pass to UnScribe.
		In addition, pass the address of a variable to hold the
		UnScribe state between calls.  This variable will be
		initialized (space malloced, too) with the UnScribeInit call.
		Error value of -1 means that the field value wasn't recognized;
		error value of -2 means that a malloc failed.
		*/

extern int UnScribe();
		/* int UnScribe(code, refstate, text, textlen, fileptr)
			int code, textlen;
			struct ScribeState **refstate;
			char *text;
			FILE *fileptr;
		Pass it the initial UnScribeInit return value and the address
		of the integer state variable.  Also pass the address of the
		text to be unscribed, and the number of bytes at that address.
		The unscribed text will be written onto stdio file *fileptr.
		Return values are >= 0 for OK (the number of characters
		written), < 0 for errors.  Error value -1 means
		that errno holds a useful code.	*/
extern int UnScribeFlush();
		/* int UnScribeFlush(code, refstate, fileptr)
			int code;
			struct ScribeState **refstate;
			FILE *fileptr;
		Call this to unbuffer an UnScribe sequence.  If the UnScribe
		routine has buffered any data, this call cues it to un-buffer it.
		Returns zero for OK, non-zero for failures.
		This routine also frees up the refstate.
		*/
extern int UnScribeAbort();
		/* int UnScribeAbort(code, refstate)
			int code;
			struct ScribeState **refstate;
		Call this to undo (abortively) an UnScribeInit without needing
		an active stdio file to which to write buffered data.
		Returns 0 for all OK, non-zero for (presumably ignorable) errors.
		*/
extern int PrintQuotingFormatting();
		/* extern int PrintQuotingFormatting(fp, text, format, len)
			FILE *fp;
			char *text, *format;
			int len;
		Call this to write LEN chars from TEXT to file FP, assuming that
		you want it encoded in format FORMAT (e.g., ``yes'', ``2'', or ``10'').
		Returns the number of characters written, or 0 or -1 for errors (like fwrite).
		*/
extern int PrintMaybeFoldQuotingFormatting();
		/* extern int PrintMaybeFoldQuotingFormatting(fp, text, format, len, DoFold)
			FILE *fp;
			char *text, *format;
			int len, DoFold;
		Call this to write LEN chars from TEXT to file FP, assuming that
		you want it encoded in format FORMAT (e.g., ``yes'', ``2'', or ``10'').
		Returns the number of characters written, or 0 or -1 for errors (like fwrite).
		Make DoFold non-zero to allow line folding.
		*/

