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


#ifndef _type_ch_
#define _type_ch_

/***************************************************************************\
	type.ch

TYPE DATA OBJECT

	Created 6-8-88 by George Baggott

	This object serves as a programmer's interface to the C/Class type system.
	It includes facilities for constructing (both manually and through compiling)
	representations of types, accessing their properties and manipulating memory
	associated with them.  For a user interface to the type system, see prop, a
	type editor object.
 
	      \***************************************************************************/

/*
 * $Log: type.ch,v $
*Revision 1.8  1994/02/27  22:49:18  rr2b
*Fixed #endif to comment out info about which #if it matches.
*COMPILER WARNING
*
*Revision 1.7  1993/05/04  01:23:33  susan
*RCS Tree Split
*
*Revision 1.6.1.1  1993/02/02  02:57:48  rr2b
*new R6tape branch
*
*Revision 1.6  1992/12/15  00:44:33  rr2b
*fixed disclaimerization
*
Revision 1.5  1992/12/14  20:48:33  rr2b
disclaimerization

Revision 1.4  1992/12/09  06:49:18  rr2b
rename index field of info union to ind to avoid
macro expansion of index to strchr on hp
.

Revision 1.3  1991/09/12  19:45:17  bobg
Update copyright notice

Revision 1.2  1989/08/25  14:14:55  gb17
Reset classprocedure added

Revision 1.1  89/08/22  15:30:18  wjh
Initial revision

 */

#include <sym.ih>

/* the type_types structure defined below gives the programmer pointers to the
 * primitive types defined by the C language
 */

struct type_ctypes {
	struct type *charType, *shortType, *intType, *longType;
	struct type *ucharType, *ushortType, *uintType, *ulongType;
	struct type *floatType, *doubleType;
	struct type *voidType;

	struct type *structType,  *unionType,  *enumType;
	struct type *macroType;
	struct type *basicobject;
};

/* operations used to describe the relationship between a type and the "super-type"
 * from which it was constructed.
 */

typedef long operation;

#define type_prim		-1	/* primary operations */
#define type_typedef	-2
#define type_struct		-3
#define type_union		-4
#define type_enum		-5
#define type_field		-6
#define type_const		-7

#define type_ptr		-12	/* secondary operations used to modify primaries */
#define type_array		-13
#define type_funcReturning	-14

#define type_class		-19	/* additional primaries for describing a class */
#define type_package	-20

#define type_macro	-15
#define type_classmacro	-16
#define type_method	-17
#define type_classmethod	-18

#define type_param	-9

/* NOTE: type_array is used to designate a NULL terminated array.  A bounded array is
 * represented by a positive number equal to the size of the array.  For example buf[80]
 * would be represented by ((operation) 80).
 */

#define type_lastAdded	-21	/* special operation to be applied to structs, unions, 
				 * funcs, etc. after last property is created
				 */

/* If a type is a class, it's info union will contain a pointer to a struct classInfo */

struct classInfo {
	sym_ScopeType scope;			/* scope where methods live */
	long noMethods, noClassMethods;		/* table sizes */
	long noMacros, noClassMacros;
	struct type **methods, **classMethods;	/* tables */
	struct type **macros, **classMacros;
	struct type *data;				/* the associated structure */
};

class type : sym {

overrides:

methods:

		Create(char * name, struct type * owner, operation op) returns struct type *;
		/* create a new type by applying a primary type operation, giving a name,
		 * and specifying a type that will possess the newly created type as a property.
		 */

		Lookup(char * name) returns struct type *;
		/* search a type's property list for a type that matches name
		 */

		ApplyOp(operation op);
		/* apply a secondary operation to the type.
		 */

		GetDeclaration() returns char *;
		/* generate the string that would declare the type.
		 */

	/* methods for manipulating the memory associated with a type */

		IsValidAddress(void *addr) returns boolean;
		/* check if addr refers to an area of memory where an
		 * instance of self could be stored.
		 */

		SetValue(void *obj, void *newVal);
		/* copy sizeof(type) bytes from newVal to obj.
		 */

	/* debug methods */

		Print();
		/* print a type to stdout
		 */


macromethods:

	/* GetName() & GetScope(val), inherited from sym */

	/* the inheritance tree */

		GetSuper()		    ((self)->super)
		GetOps()		    ((self)->ops)

		SetSuper(struct type *val)  ((self)->super = val)

	/* the property list */

		GetLast()		    ((self)->last)
		GetPrev()		    ((self)->prev)

		SetLast(struct type *val)   ((self)->last = val)
		SetPrev(struct type *val)   ((self)->prev = val)

	/* other attributes */

		GetSize()		    ((self)->size)
		SetSize(short val)	    ((self)->size = (val))

		GetAlign()		    ((self)->align)
		SetAlign(short val)	    ((self)->align = (val))

		GetReadLevel()		    ((self)->readLevel)
		GetWriteLevel()		    ((self)->writeLevel)

		SetReadLevel(short val)	    ((self)->readLevel = (val))
		SetWriteLevel(short val)    ((self)->writeLevel = (val))

	/* accessing the info union */

		GetOffset()		    ((self)->info.offset)
		GetVal()		    ((self)->info.val)
		GetMacro()		    ((self)->info.macro)
		GetClassInfo(void prop)	    ((self)->info.classInfo->prop)
		GetIndex()		    ((self)->info.ind)

		SetOffset(long newval)	    ((self)->info.offset = newval)
		SetVal(long newval)	    ((self)->info.val = newval)
		SetMacro(char *newval)	    ((self)->info.macro = newval)
		SetClassInfo(void prop, void newval) \
                                            ((self)->info.classInfo->prop = newval)
		SetIndex(long newval)	    ((self)->info.ind = newval)

classprocedures:

	InitializeClass() returns boolean;
	InitializeObject(struct type *self) returns boolean;
	FinalizeObject(struct type *self);

		GetCtypes() returns struct type_ctypes*;
		/* returns the info structure which contains various atomic
		 * types (i.e. integers, structs, etc.)
		 */

		Reset();
		/* undefines and destroys all the types that have been declared */

	/* accessing the type compiler */

		Declare(struct text *text, long loc, long len) returns char*;
		/* compile a text */
		DeclareFromString(char *declaration) returns char*;
		/* compile a string */
		DeclareFromFile(char *filename) returns char*;
		/* compile a file. */
		DeclareClass(char *classname) returns char*;
		/* compiles the .H file associated with classname */

data:

	/* the type inheritance tree */

	struct type *super; 	/* the super-type which ops were applied to
				 * produce the type.
				 */

	operation *ops;		/* A NULL terminated array of operations that describes
				 * the transformation applied to super to produce the type.

			long (*a)[]  - a ptr to an array of longs in the "data:" area
			ops would be:   type_field, type_array, type_ptr

			func(long (*a)[], boolean b) returns char *;    (method)
			for func:  supertype is char, ops are type_method, type_ptr
			GetLast => b,  super is 'boolean'
			GetPrev(GetLast(func)) => a, super is long,  
				ops are type_param, type_array, type_ptr
			GetPrev(GetPrev(GetLast(func))) => func
				this is the termination condition of the list
				note that the required first arg is NOT in the list

				 */

	/* the property list.
	 * by convention, the last type in a property list possesses all the other
	 * types as its properties.  for example, a structure possesses all its fields as properties.
	 */

	struct type *last;		/* the last property in the list */
	struct type *prev;		/* the previous property in the list */

	/* other attributes of the type */

	short size, align;		/* the size and alignment of the memory block where the
				 * type is stored.
				 */

	short readLevel, writeLevel;	/* Used to define the read and write access level
				 * for the type.
				 */

	/* additional info that depends on who possesses the type */

	union {
		long offset;			/* for structs, unions, and parameters */
		long val;			/* for enum constants */
		char *macro;			/* for macro definitions */
		struct classInfo * classInfo;	/* for classes */
		long ind;			/* for methods */
	} info;
};

#endif /* _type_ch_ */
