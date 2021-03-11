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


 

/* *************************************************************** 
 errprntf.h:  Header file for Andrew error message standard

   The errprintf routine implements the Andrew official error standard, and represents
	the approved way to generate and format error messages which are 
	destined ultimately for the console program.  This includes any window 
	manager program, since all wm programs now have stderr redirected
	to console, and any demons that are started at boot time, whose stderr
	goes to /dev/console, which console intercepts.

   You should #include "errprntf.h" to make this work.

   The routine errprintf acts just like a printf on stderr, except that you
	replace the "stderr" parameter with four initial parameters:

	application -- should be the name of the program generating the message.
		A zero here will cause the application name to be "UNKNOWN"

	type -- should be either ERR_CRITICAL, ERR_WARNING, ERR_MONITOR, or ERR_DEBUG
		ERR_CRITICAL is for extremely critical conditions (files lost,
		file servers crashing, etc.)  ERR_WARNING is for other error
		conditions that novices must see.  ERR_MONITOR is for informative
		and important messages that sophisticated users will want to know
		about but that need not be shown to novices.  ERR_DEBUG is for
		information that most people won't want to see, but that will be
		useful to enable in the console when you're debugging.  (Note that
		you should not use this facility TOO freely, as it is moderately
		expensive to send unnecessary messages to the console.)
		A zero for "type" is the same as ERR_CRITICAL.

	log -- may be used to cause the message to be logged somewhere.  If log begins
		with a slash ("/"), then the message will be appended to the
		named file.  Otherwise, the message will be mailed to the named
		destination.  A zero here will cause no logging to happen.

	id -- The unique official Andrew ID of this message.  If your message has
		not been assigned a unique ID, this field should be zero.

   The remaining parameters are simply the usual printf control string and arguments.
	Note that errprintf ALWAYS adds a newline to the end of your message, so you
	should probably omit the newline.

   Thus the most typical and painless use of errprintf would be as follows:

	errprintf("myprog", 0, 0, 0, "foo %s bar", s);

   This will cause an error message for the program "myprog" consisting of 
	"foo <contents of string s> bar" to be sent to console with CRITICAL
	priority, no logging, and no official ID.

*************************************************************** */

#define ERR_CRITICAL 0
#define ERR_WARNING 4
#define ERR_MONITOR 6
#define ERR_DEBUG 9
