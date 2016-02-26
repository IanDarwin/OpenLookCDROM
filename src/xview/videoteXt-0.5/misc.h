#ifndef _MISC_H
#define _MISC_H

/* Copyright (c) 1994-95 Martin Buck  <martin.buck@student.uni-ulm.de>
 * Read COPYING for more information
 */


#define VTXVERSION "0.5"
#define VTXNAME "videotext"
#define VTXCLASS "Videotext"
#define VTXWINNAME "VideoteXt"

#define REQ_MAJOR 1
#define REQ_MINOR 4


#ifndef MIN
#define MIN(a, b) ((a) < (b) ? (a) : (b))
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#endif
#define SIGN(a) ((a) < 0 ? -1 : ((a) > 0 ? 1 : 0))

#ifndef FALSE
#define FALSE 0
#define TRUE 1
#endif

#endif /* _MISC_H */
