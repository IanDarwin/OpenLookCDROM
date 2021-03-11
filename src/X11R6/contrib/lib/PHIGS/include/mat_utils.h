/* $XConsortium: mat_utils.h,v 5.2 94/04/17 20:41:46 rws Exp $ */

/***********************************************************

Copyright (c) 1989, 1990, 1991  X Consortium

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
X CONSORTIUM BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of the X Consortium shall not be
used in advertising or otherwise to promote the sale, use or other dealings
in this Software without prior written authorization from the X Consortium.

Copyright 1989, 1990, 1991 by Sun Microsystems, Inc. 

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the name of Sun Microsystems,
not be used in advertising or publicity pertaining to distribution of 
the software without specific, written prior permission.  

SUN MICROSYSTEMS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, 
INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT 
SHALL SUN MICROSYSTEMS BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL 
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.
******************************************************************/

extern	void	phg_mat_scale();		/* matrix	m;
					   float	x, y, z; */

extern	void	phg_mat_trans();		/* matrix	m;
					   float	x, y, z; */

extern	void	phg_mat_shear_z();		/* matrix	m;
					   float	a, b; */

extern	void	phg_mat_rot_x();		/* matrix	m;
					   float	theta; */
					   
extern	void	phg_mat_rot_y();		/* matrix	m;
					   float	theta; */

extern	void	phg_mat_rot_z();		/* matrix	m;
					   float	theta; */

extern	void	phg_mat_mul_pt();		/* point3d	new_pt;
					   point3d	pt;
					   matrix	mat; */

extern	void	phg_mat_copy();	/* matrix	to, from;  */


extern	void	phg_mat_mul();		/* matrix	result;
					   matrix	mat1;
					   matrix	mat2; */

extern	void	phg_mat_identity ();	/* matrix	mat;  */

extern	float	phg_vector_dot_prod();	/* point3d	a;
					   point3d	b; */
					   
extern	void	phg_vector_cross_prod();	/* point3d	result;
					   point3d	a;
					   point3d	b; */
					   
extern	float	phg_vector_length();	/* point3d	a; */

extern	void	phg_vector_normalize();	/* point3d	a; */

extern	void	phg_mat_print();		/* matrix 	m  */
extern	void	phg_vector_print();		/* point3d	p  */



