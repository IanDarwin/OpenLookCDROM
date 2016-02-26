/*
 * This file is a product of Sun Microsystems, Inc. and is provided for
 * unrestricted use provided that this legend is included on all tape
 * media and as a part of the software program in whole or part.  Users
 * may copy or modify this file without charge, but are not authorized to
 * license or distribute it to anyone else except as part of a product
 * or program developed by the user.
 *
 * THIS FILE IS PROVIDED AS IS WITH NO WARRANTIES OF ANY KIND INCLUDING THE
 * WARRANTIES OF DESIGN, MERCHANTIBILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE, OR ARISING FROM A COURSE OF DEALING, USAGE OR TRADE PRACTICE.
 *
 * This file is provided with no support and without any obligation on the
 * part of Sun Microsystems, Inc. to assist in its use, correction,
 * modification or enhancement.
 *
 * SUN MICROSYSTEMS, INC. SHALL HAVE NO LIABILITY WITH RESPECT TO THE
 * INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS BY THIS FILE
 * OR ANY PART THEREOF.
 *
 * In no event will Sun Microsystems, Inc. be liable for any lost revenue
 * or profits or other special, indirect and consequential damages, even
 * if Sun has been advised of the possibility of such damages.
 *
 * Sun Microsystems, Inc.
 * 2550 Garcia Avenue
 * Mountain View, California  94043
 *
 * Mark Opperman
 * opcode@sun.com
 * 3 September, 1988
 */

struct connection {
    int client_fd;
    int server_fd;
    int client_nbytes;
    int server_nbytes;
    int firstrow;
    int ngroups;
    int ncols;
    int currentpos;
};

/*
 * Defines encoding used by NeWS.
 */
#define ENC_INT                 0x80
#define ENC_SHORT_STRING        0x90
#define ENC_MANY                0xa0
#define ENC_STRING              0xa0
#define ENC_IEEE_FLOAT          0xa4
#define ENC_IEEE_DOUBLE         0xa5
#define ENC_SYSCOMMON2          0xa6
#define ENC_LUSERCOMMON         0xa7
#define ENC_FREE1               0xab
#define ENC_SYSCOMMON           0xb0
#define ENC_SYSCOMMON_EXT       0xc0
#define ENC_USERCOMMON          0xd0
#define ENC_USERCOMMON_EXT      0xe0
#define ENC_FREE2               0xf0

#define ENC_MASK                0xf0
#define ENC_SUBFLD              0x0f

extern char *systoklst[];
