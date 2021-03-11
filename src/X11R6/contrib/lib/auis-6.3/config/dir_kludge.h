/*
 * KLUDGE!
 *
 * Until everything that uses sys/dir.h has been properly
 * POSIXified:
 *
 * #ifdef POSIX_DIR_ENV
 * #include <dirent.h>
 * #else
 * #include <sys/dir.h>
 * #define dirent direct
 * #endif
 *
 * We shall define this file which will PRETEND to
 * to be dirent.h on Solaris2 systems.
 *
 * We do this because we CAN be POSIX compliant, but
 * we don't want to get in the business of using both
 * dir.h and dirent.h at the same time.
 *
 * Bill Cattey
 * MIT Information Systems
 * wdc@mit.edu
 */

#include <dirent.h>
#define direct dirent
