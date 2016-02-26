/* Copyright (c) 1994-95 Martin Buck  <martin.buck@student.uni-ulm.de>
 * Read COPYING for more information
 */

/* This header file provides an alternative to the standard assert: You can
 * choose wheter or not your program should dump core when an assertion
 * fails. If you don't define ASSERT_DUMP_CORE, your program will print the
 * `failed assertion'-message and call exit(1) without a coredump.
 *
 * The reason for this is that you can lock up your X-server when assert
 * calls abort() at the wrong time (probably during grabs). If you can't
 * login on a serial line or over the network to kill the X-server, you will
 * have to reboot your machine (I once experienced this with another
 * application -- not even XFree's ZapServer-key did work)!
 *
 * There has been a report that in VideoteXt an assertion fails when a
 * station broadcasts a multipage-extension-table. I wasn't able to
 * reproduce this and I don't think that this was the reason for the abort,
 * but I decided to use this alternative assert just to make sure VideoteXt
 * doesn't do bad things[tm] to your machine :-)
 *
 * But if you get `failed assertion'-messages, *please* report them to me!
 */

#include <stdio.h>

#undef assert
#undef __assert

#ifdef NDEBUG
#define assert(ignore) ((void) 0)
#else

#define assert(exp) ((void) ((exp) ? 0 : __assert (#exp, __FILE__, __LINE__)))

#ifdef ASSERT_DUMP_CORE
#define __assert(exp, file, lineno)  \
    (fprintf (stderr, "%s:%u: failed assertion `%s'\n", file, lineno, exp), abort (), 0)
#else
#define __assert(exp, file, lineno)  \
    (fprintf (stderr, "%s:%u: failed assertion `%s'\nTerminating\n", file, lineno, exp), \
    exit (1), 0)
#endif
#endif
