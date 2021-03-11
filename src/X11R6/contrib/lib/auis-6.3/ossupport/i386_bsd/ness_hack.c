/*
 * Some versions of BSD on the 386 don't include the mathlib
 * fuctions below (called by ness).  This code calls ness back
 * as the code calling these routines would do
 */

#define CALL_RunError(f) \
	void f() \
	{ \
		RunError(":unimplemented operation requested (386bsd)", 0); \
	}

CALL_RunError(j0)
CALL_RunError(j1)
CALL_RunError(jn)
CALL_RunError(y0)
CALL_RunError(y1)
CALL_RunError(yn)
CALL_RunError(lgamma)
CALL_RunError(erf)
CALL_RunError(erfc)
