

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
#include <setjmp.h>

typedef char *except_Exception;

typedef boolean (*except_UncaughtExceptionHandler)();

#define except_ANY "except_ANY"

/* Simpler interface to ANY */
#define ANY except_ANY

/* Simpler interfaces to RAISE, ASSERT, and PROPAGATE */
#define RAISE(x, v) except_RAISE(x, v)
#define ASSERT(condition, xid, value) except_ASSERT(condition, xid, value)
#define PROPAGATE() except_PROPAGATE()

/*
   PRIVATE STRUCTURE
	Used to deal with a stack of current handler contexts.
	The head of the stack is held by except_CurrentContext.
 */
typedef struct except_HandlerContext {
    struct except_HandlerContext *nested;
    jmp_buf env;
} except_HandlerContext_t, *except_HandlerContext_p;

/*
   PRIVATE FLAGS
	except_Raised indicates an exception was raised.
	except_Handled indicates the exception was handled
		in the current handler context.
  
   INVARIANT:	Flags may have except_Handled set only if
		except_Raised is also set.
 */
#define except_Raised	0x1
#define except_Handled	0x2

#define TRY \
	{ int _except_flags; \
	  except_HandlerContext_t _except_context; \
	  except_PushContext(&_except_context); \
	  if ((_except_flags = setjmp(_except_context.env)) == 0) {

#define EXCEPT(xid) \
          } else if (except_CheckException(xid, &_except_context, &_except_flags)) {

#define FINALLY \
          } \
	  except_ResetContext(_except_context.nested); \
	  {

#define ENDTRY \
	  } \
	  except_ResetContext(_except_context.nested); \
	  if (_except_flags == except_Raised) { \
	      except_RAISE(NULL, NULL); \
          } \
        }

#define except_ExceptionRaised() (_except_flags & except_Raised)
#define except_ExceptionHandled() (_except_flags & except_Handled)

#define except_ASSERT(condition, xid, value) \
	((condition) || except_RAISE(xid, value))
#define except_PROPAGATE() \
	{ if (except_ExceptionRaised()) except_RAISE(NULL, NULL); }

package except {
    classprocedures:
	GetRaisedException() returns except_Exception;
	SetExceptionValue(char *value);
	GetExceptionValue() returns char *;
	SetUncaughtExceptionHandler(except_UncaughtExceptionHandler h);
	GetUncaughtExceptionHandler() returns except_UncaughtExceptionHandler;
	RAISE(except_Exception x, char *value) returns boolean;

/*
   LOCAL NON-EXPORTED ROUTINES USED BY THE MACROS ABOVE -- USE MACROS INSTEAD!
 */
	PushContext(except_HandlerContext_p context);
	CheckException(except_Exception xid, except_HandlerContext_p context, int *flags) returns boolean;
	ResetContext(except_HandlerContext_p context);
	GetCurrentContext() returns except_HandlerContext_p;
};

/***************************************************************

	|-| Name:	except_begin, except_end, except_for, except_while, except_do

	|-| Abstract:	Bracketing constructs for routines containing TRY
			statements with embedded returns or loop statements
			containing TRY statements with embedded continues or
			breaks.

			TRY statements may be nested.  Embedded return or
			continue or break statements must be replaced by
			except_return, except_continue, or except_break
			(see above).  This does NOT apply to break statements
			for switch statements.

	|-| Parameters:	None

	|-| Results:	None

	|-| Side-effects:	Sets up appropriate state

	|-| Exceptions:	None

	|-| Log:
	    19 Jun 90	mlh	Created.

***************************************************************/

/*
   Use this to replace the open bracket { at the start of a routine
   containing a TRY statement with an embedded except_return statement.
 */
#define except_begin \
	{ except_HandlerContext_p _except_routinecontext = except_GetCurrentContext();

/*
   Use this to replace the close bracket } at the end of a routine to
   match the corresponding except_begin.

   Similarly, use this to match any of except_for, except_while, or except_do
   when used under the circumstances described below.  Note that in this case
   the except_end does not replace a close bracket if an open bracket is used.
 */
#define except_end }

/*
   Use these to replace for, while, or do when such statements contain a
   TRY statement with an embedded except_break or except_continue.  Such use
   must be terminated with a matching except_end.  If the loop requires
   { } bracketing, the except_end does NOT replace the closing } bracket!
 */
#define except_for \
	{ except_HandlerContext_p _except_loopcontext = except_GetCurrentContext(); for
#define except_while \
	{ except_HandlerContext_p _except_loopcontext = except_GetCurrentContext(); while
#define except_do \
	{ except_HandlerContext_p _except_loopcontext = except_GetCurrentContext(); do

/***************************************************************

	|-| Name:	except_break, except_continue, except_return, except_returnvalue

	|-| Abstract:	These are non-local exits from a TRY clause.
			They should NOT be used for loops totally
			embedded within TRY clauses.

			except_break and except_continue for loops that
			contain a TRY statement.

			except_return(value) for returning from a routine
			from within a TRY statement.

	|-| Parameters:
		t	type for return statement value.

		e	value for return statement;
			if value expression may raise an exception
			during its evaluation, better to store
			the result in a local and return the local,
			otherwise the exception will not be handled
			by the handler context established by the
			containing TRY statement.

	|-| Results:	None

	|-| Side-effects:
		As one might expect.

	|-| Exceptions:	See note for parameter e above.

	|-| Log:
	    27 Mar 90	mlh	Created.

***************************************************************/

#define except_break \
	{ except_ResetContext(_except_loopcontext); break; }
#define except_continue \
	{ except_ResetContext(_except_loopcontext); continue; }
#define except_returnvalue(t, e) \
	{ t _rval = e; except_ResetContext(_except_routinecontext); return _rval; }
#define except_return \
	{ except_ResetContext(_except_routinecontext); return; }
