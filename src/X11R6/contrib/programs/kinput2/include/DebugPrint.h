/* $Id: DebugPrint.h,v 1.5 1994/05/16 08:34:19 ishisone Rel $ */

#ifdef DEBUG
extern int	debug_all;

#ifdef DEBUG_VAR
int DEBUG_VAR = 0;
#else /* DEBUG_VAR */
#ifdef EXT_DEBUG_VAR
extern int EXT_DEBUG_VAR;
#define DEBUG_VAR	EXT_DEBUG_VAR
#else /* EXT_DEBUG_VAR */
#define DEBUG_VAR	0
#endif /* EXT_DEBUG_VAR */
#endif /* DEBUG_VAR */

#define DEBUG_CONDITION	(debug_all || DEBUG_VAR)
#define DDEBUG_CONDITION(l) (debug_all >= (l) || DEBUG_VAR >= (l))
#define DPRINT(args)	{ if (DEBUG_CONDITION) printf args; }
#define DDPRINT(level, args) \
	{ if (DDEBUG_CONDITION(level)) printf args; }
#define TRACE(args)	DDPRINT(10, args)

#else /* !DEBUG */

#define DEBUG_CONDITION	0
#define DDEBUG_CONDITION(l) 0
#define DPRINT(args)
#define DDPRINT(level, args)
#define TRACE(args)

#endif
