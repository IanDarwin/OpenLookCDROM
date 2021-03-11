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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/bush/RCS/bushapp.c,v 1.15 1992/12/15 21:29:20 rr2b R6tape $";
#endif

/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	Bush Application

MODULE	bushapp.c

VERSION	1.0

AUTHOR	TC Peters & GW Keim
	Information Technology Center, Carnegie-Mellon University 

DESCRIPTION
	This is the Application layer for the Bush Inset.

    NB: The comment-symbol "===" indicates areas which are:
	    1 - Questionable
	    OR
	    2 - Arbitrary
	    OR
	    3 - Temporary Hacks
    Such curiosities need be resolved prior to Project Completion...


HISTORY
  08/21/85	Created (TCP)
  01/15/89	Convert to ATK (GW Keim)
  05/01/89	Revise Initializing msg to conform with Majro/Minor Version msg (TCP)
  06/06/89	Added call to chdir() to set the current-working-directory to the 
	               path specified on the command line; (GW Keim)
  07/05/89	Use super_ParseArgs (TCP)
  07/28/89	Changed all occurances of AptTree to just Tree; (GW Keim)
  09/07/89	Set V1.0 msg (TCP)

END-SPECIFICATION  ************************************************************/

#include <class.h>
#include <bush.h>
#include <im.ih>
#include <frame.ih>
#include <bush.ih>
#include <bushv.ih>
#include <bushapp.eh>

#define	Bush		((self)->bush)
#define	Bushv		((self)->bushview)
#define	Debug		((self)->debug)
#define imPtr		((self)->im)
#define framePtr	((self)->frame)
#define	ArgC		((self)->argc)
#define	ArgV		((self)->argv)

/* The following defines are for when there are seperate windows for the tree-view, listing-view, and entry-view. */
#define	nodes_object    1
#define	entries_object  2
#define	entry_object    4

boolean
bushapp__InitializeObject( ClassID, self )
  register struct classheader	*ClassID;
  register struct bushapp	*self;
{
  Bush = NULL;
  Bushv = NULL;
  imPtr = NULL;
  framePtr = NULL;
  ArgC = 0;
  ArgV = NULL;
  Debug = 0;
  bushapp_SetMajorVersion(self,1);
  bushapp_SetMinorVersion(self,0);
  return(TRUE);
}

void
bushapp__FinalizeObject( ClassID, self )
  register struct classheader	*ClassID;
  register struct bushapp	*self;
{
  if(imPtr) {
    im_Destroy(imPtr);
    imPtr = NULL;
  }
  if(framePtr) {
    frame_Destroy(framePtr);
    framePtr = NULL;
  }
  if(Bush) {
    bush_Destroy(Bush);
    Bush = NULL;
  }
  if(Bushv) {
    bushv_Destroy(Bushv);
    Bushv = NULL;
  }
}

boolean
bushapp__ParseArgs( self, argc, argv )
  register struct bushapp  *self;
  register int		    argc;
  register char		  **argv;
{
  char **args = argv;

  if(!super_ParseArgs(self,argc,argv))
    return(FALSE);
  ArgV = argv;
  while(args && *args++) ArgC++;
  if(ArgC > 2) {
      printf("usage: bush [directory]\n");
      printf("bush: using '%s' as root directory.\n", ArgV[1]);
  }
  return(TRUE);
}

boolean
bushapp__Start( self )
  register struct bushapp   *self;
{
  register char		    *startDir = NULL;
	
  if(self->argc > 1) startDir = self->argv[1];
  if(!(super_Start(self)) ||
     !(imPtr = im_Create(NULL)) ||
     !(framePtr = frame_New()) ||
     !(Bush = bush_Create(startDir)) ||
     !(Bushv = bushv_Create(nodes_object))) 
	return(FALSE);
  bushv_SetDataObject(Bushv,Bush);
  frame_SetView(framePtr,Bushv);
  im_SetView(imPtr,framePtr);
  frame_PostDefaultHandler(framePtr,"message",
    frame_WantHandler(framePtr,"message"));
  if(bush_TreeRoot(Bush)) 
    im_SetTitle(imPtr,bush_DirName(Bush,bush_TreeRoot(Bush)));
  chdir(bush_DirPath(Bush,bush_TreeRoot(Bush)));
  bushv_WantInputFocus(Bushv,Bushv);
  return(TRUE);
}
