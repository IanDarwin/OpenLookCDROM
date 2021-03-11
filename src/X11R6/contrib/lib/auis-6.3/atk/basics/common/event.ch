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


 

#define event_VERSION 1

#ifdef EVENT_IMPLEMENTATION
#define event_t t
#define event_proc proc
#define event_procdata procdata
#define event_next next
#define event_id id
#endif

class event  {
    classprocedures:
    Cancel(struct event *);
    Enqueue(long time, procedure proc, char *procdata) returns struct event *;
    ForceNext();
    FirstTime(long currentTime) returns long;
    StartTimer();
    HandleTimer(long currentTime) returns long;
    Now() returns long;
    Allocate() returns struct event *;
    Deallocate(struct event *self);
    data:
      /* WARNING WARNING WARNING: the event data structure is private.
       in fact the event * returned from enqueue event isn't really a pointer!
       The argument to event_Cancel must be a value returned from EnqueueEvent. -rr2b */
    long event_t;		/* time of evenet in microsec >> 6 */
    int (*event_proc)();	/* procedure to call for event */
    char *event_procdata;	/* data to be passed to proc */
    struct event *event_next;
    unsigned long event_id;
};


#define event_TUtoSEC(x)  ((x)/(1000000>>6))
#define event_TUtoUSEC(x) ((x)<<6)
#define event_TUtoMSEC(x) (((x)<<3)/(1000>>3))
#define event_SECtoTU(x)  ((x)*(1000000>>6))
#define event_USECtoTU(x) ((x)>>6)
#define event_MSECtoTU(x) (((x)*(1000>>3))>>3)
#define event_ENDOFTIME 2000000000

