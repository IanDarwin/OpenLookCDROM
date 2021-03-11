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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/snap2/loadserv/RCS/snpfkscb.c,v 2.6 1992/12/15 21:06:36 rr2b R6tape $";
#endif

/*
   a fake version of the ibmpc scb module
*/
#include <scb.h>

/*
  On a pc set this to false right after calling SNAP_ClientInit()
  to inhibid any SNAP_BeginConv from making sessions that login can see.
  If SNAP_login is set to false, do not call SNAP_ClientTerm as it will
  close all (read) cids for all the sessions login has.

  Clearly, this needs to be reorganized some day.
*/
int SNAP_login;

/*
  On a pc this maps a fake cid made up by snapstubs
  (which is really a scb id) into a real snap cid.
  Call this routine on a (fake)cid before calling
  SCB_ClearSession
*/
int SCB_GetCID(acid)
{
    return acid;
}

/*
  on a pc ending a connection still leaves a cid around
  for login to see.  SCB_ClearSession is called after
  SNAP_EndConv() and flushes the session.
  DANGER: The cid passed here is a *real* cid, not the
 fake one retruned by the SNAP_BeginConv glue.
*/
void SCB_ClearSession (cid)
int cid;
{
    return;
}

SNAP_Nop()
{
    return 0;
}

SNAP_GetAnchor(a)
int a;
{
    return 1;
}

SCB_GetLastCid(aname)
{
    return NULLCID;
}
