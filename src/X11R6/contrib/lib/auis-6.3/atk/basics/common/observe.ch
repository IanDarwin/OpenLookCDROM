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


 

#define observable_VERSION 1

class observable[observe] : traced {

methods:
	AddObserver (struct thisobject *observer);
        IsObserver (struct thisobject *observer) returns boolean;
	RemoveObserver (struct thisobject *observer);
	NotifyObservers (long value);
	ObservedChanged (struct observable *changed, long value);

	/* the following methods implement a scheme for "triggers", 
		a set of named messages,  each trigger must first be
		defined with a call to DefineTrigger */
	AddRecipient(/* struct thisobject *self, */ struct atom *trigger,
			struct basicobject *rcvr, void (*func)(), 
			long rock) returns boolean;
		/* when the trigger is Pulled, the 'func' will be called thus:
			func(rcvr, self, rock)  
		    AddRecipient returns FALSE if the trigger is not defined for the class 
		    (each rcvr can be registered only once with each trigger 
			on a given object)
		*/
	DeleteRecipient(/* struct thisobject *self, */ struct atom *trigger,
			struct basicobject *rcvr);
		/* removes the receiver from the list of recipients */
	PullTrigger(/* struct thisobject *self, */ struct atom *trigger);
		/* call all funcs associated with this trigger on this object 
			Only the object itself should call PullTrigger.  */

	/* if a client is calling a number of operations which would pull a trigger 
		too many times, it can disable the trigger temporarily.  
		It must later Enable the trigger.  At that time one call back 
		is autometically made for the trigger if it has 
		been Pulled one or more times in the interim.  */
	DisableTrigger(/* struct thisobject *self, */ struct atom *trigger);
		/* until Enabled, this trigger will no produce call backs 
			Enable MUST be called once 
			for each time Disable has been called. */
	EnableTrigger(/* struct thisobject *self, */ struct atom *trigger);
		/* this trigger will once again produce call backs  */
	DisableCount(/* struct thisobject *self, */ struct atom *trigger) returns long;
		/* returns a non-negative integer giving the
			number of outstanding calls to DisableTrigger
		   if the trigger does not exist, returns -1  */


classprocedures:

	InitializeObject(struct observable *self) returns boolean;
	FinalizeObject(struct observable *self);

	DefineTrigger(struct basicobject *classinstance, struct atom *trigger);
		/* associate the atom as a possible trigger for the class.
			The xxx_InitializeClass routine can use
			xxx_classinfo as the first argument. */
	ListTriggers(struct basicobject *classinstance) returns struct atomlist *;
		/* returns a list of the triggers defined for the class and all super classes.
			The returned value has been newly created and must
			be atomlist_Destroy()ed by the caller.
			As a value for 'classinstance', those objects outside class xxx 
			can use an instance of xxx or 
				((struct xxx *)class_Load("xxx")). */

data:

	short nObservers;			/* number of observers */
	short maxObservers;		/* number of entries in observers table */
	struct observable **observers;	/* table of observers */
	struct triggerhousing *triggers;
};

#define observable_OBJECTDESTROYED -1
#define observable_OBJECTCHANGED 0

