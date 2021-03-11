/* Copyright 1992 Andrew Toolkit Consortium, Carnegie Mellon University */

class traced {
classprocedures:
	Destroyp(struct traced *self) returns boolean;
	InitializeObject(struct traced *self) returns boolean;
	FinalizeObject(struct traced *self);
macromethods:
	Reference() ((self)->refcount++)
	UnReference() (((self)->refcount>0)?(self)->refcount--:0)
	ReferenceCount() ((self)->refcount)
data:
	unsigned long refcount;
};
