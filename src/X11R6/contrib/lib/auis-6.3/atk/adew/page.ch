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


 

/* The following may be use in place of a specific position  
	in the methods taking position arguments below  */
#define page_CURRENT		 -1
#define page_AFTERCURRENT	 -2
#define page_BEFORECURRENT	 -3
#define page_ATEND 		 -4
#define page_ATBEGINING	 -5

struct page_switchee {
    struct dataobject *d;
    char *viewname;
    char *label;
    struct page_switchee *next; 
};

class page : dataobject [dataobj] {
    classprocedures:
      InitializeObject(struct page *self) returns boolean;
      FinalizeObject(struct page *self);
    overrides:
      Read (FILE *file, long id) returns long;
      Write (FILE *file, long writeid, int level) returns long;
      ViewName() returns char *;
      GetModified() returns long;
    macromethods:
      GetPostMenus() ((self)->PostMenusFlag)
      SetPostMenus(foo) (((self)->PostMenusFlag) = foo)
      GetFirstObject() ((((self)->FirstSwitchee) == NULL) ? NULL: (self)->FirstSwitchee->d)
      GetNowPlaying() ((((self)->NowPlaying) == NULL) ? NULL: (self)->NowPlaying->d)

       /* The following macro methods are private */
      GetFirstSwitchee() ((self)->FirstSwitchee)
      GetNowPlayingSwitchee() ((self)->NowPlaying)
       
    methods:
      AddObject(struct dataobject *d, char *label,
		 char *viewname,long position) returns boolean;
      DeleteObject(struct dataobject *d) returns boolean;
      SetNowPlaying(struct dataobject *d) returns boolean;
      SetNowPlayingByName(char *name) returns boolean;
      GetNowPlayingName() returns char *;
      SetNowPlayingByPosition(long position) returns boolean;
      GetPositionOfObject(struct dataobject *d) returns long;
      GetNameOfObject(struct dataobject *d) returns char *;	
      GetObjectAtPosition(long position) returns struct dataobject *;
      GetObjectByName(char *name) returns struct dataobject *;
      GetObjectCount() returns long;
/* The following methods are private */
      GetSwitcheeName(struct page_switchee *sw) returns char *;
    data:
      struct page_switchee *FirstSwitchee, *NowPlaying;
      boolean PostMenusFlag,executingGetModified;
};
