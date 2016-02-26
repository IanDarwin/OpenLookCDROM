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
 */

/*	@(#)encoding.h 13.1 88/03/05 SMI	*/

/*
 * Copyright (c) 1984 by Sun Microsystems, Inc.
 */

/*
	Definitions for the compressed PostScript encoding.

	encoding.h, Mon Apr  8 14:21:45 1985

		James Gosling,
		Sun Microsystems
 */

#define enc_mask		0360
#define enc_int			0200/* 16 */
#define	enc_short_string	0220/* 16 */
#define	enc_string		0240/* 4 */
#define enc_IEEEfloat		0244/* 1 */
#define enc_IEEEdouble		0245/* 1 */
#define enc_syscommon2		0246/* 1 */
#define enc_lusercommon		0247/* 4 */
/* 9 free */
#define enc_syscommon		0260/* 32 */
#define enc_usercommon		0320/* 32 */
/* 16 free */


/* Special tokens for server->client communication */
#define enc_tag			enc_syscommon
#define enc_nonobject		enc_syscommon+1
#define enc_array		enc_syscommon+2/* 4 */
