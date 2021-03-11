/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1989 - All Rights Reserved      *
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


 


#define value_ROCK1 1
#define value_ROCK2 2 
#define value_STRING 3
#define value_STRINGARRAY 4



struct observer
{
  struct basicobject * observer;
  procedure callBack;
  long rock;
};

class value : dataobject[dataobj]  { /* really it would be nice if these two were in the opposite relationship */
 overrides:
  NotifyObservers( long rock );
  Write(FILE *file, long writeID, int level) returns long;
  Read (FILE *file, long id) returns long;
  ViewName() returns char *;
methods:
  AddCallBackObserver( struct basicobject * observer,
	      procedure callBack, long rock );
  RemoveCallBackObserver( struct basicobject * observer );
  RemoveCallBack( struct basicobject * observer,
		 procedure callBack );
  SetValueType(long rock,int type);
  SetStrArrayAndSize(char **rock,long size);
macromethods:
  SetValue(rock) (value_SetValueType(self,(long)rock,value_ROCK1))
  SetArraySize(rock) (value_SetValueType(self,(long)rock,value_ROCK2))
  SetString(rock) (value_SetValueType(self,(long)rock,value_STRING))
  SetStringArray(rock) (value_SetValueType(self,(long)rock,value_STRINGARRAY))
  GetValue() (self->rock1)
  GetArraySize() (self->rock2)
  GetString()  (self->string)
  GetStringArray()  (self->stringarray)
  SetNotify(val) ((self)->notify = val)
  GetUpdateCount() ((self)->updatecount)
classprocedures:

  InitializeClass() returns boolean;

  InitializeObject( struct callBackObservable * self ) returns boolean;
  FinalizeObject( struct callBackObservable * self );
data:
  short maxObservers;			/* number of entries in observers table */
  struct observer *observers;	/* table of observers */
  long rock1,rock2;
  char *string, **stringarray;
  boolean notify;
  long updatecount;
};


#define value_OBJECTDESTROYED -1


/* 
  the call back is invoked
  callBack( self, observed, observed rock, observers rock );
*/
