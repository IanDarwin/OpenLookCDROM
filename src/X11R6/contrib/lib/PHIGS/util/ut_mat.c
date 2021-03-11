/* $XConsortium: ut_mat.c,v 5.3 94/04/17 20:42:21 rws Exp $ */

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


#include "phg.h"
#include "mat_utils.h"


int
phg_mat3_equal( a, b )
    register	Pfloat	*a, *b;
{
    register	int	i;

    for( i = 0 ; i < 16; i++, a++, b++ ) 
	if ( *a != *b )
	    return 0;

    return 1;
}

void
phg_mat_scale(m, x, y, z)
		Pmatrix3	m;
		Pfloat		x, y, z;
{
    phg_mat_identity(m);
    m[0][0] = x;
    m[1][1] = y;
    m[2][2] = z;
}


void
phg_mat_trans(m, x, y, z)
		Pmatrix3	m;
		Pfloat		x, y, z;
{
    phg_mat_identity(m);
    m[3][0] = x;
    m[3][1] = y;
    m[3][2] = z;
}


void
phg_mat_shear_z(m, a, b)
		Pmatrix3	m;
		Pfloat		a, b;
{
    phg_mat_identity(m);
    m[2][0] = a;
    m[2][1] = b;
}


void
phg_mat_rot_x(m, theta)
		Pmatrix3	m;
		Pfloat		theta;
{
    Pfloat	sin_theta, cos_theta;

    sin_theta = (Pfloat) sin(theta);
    cos_theta = (Pfloat) cos(theta);
    phg_mat_identity(m);

    m[1][1] = m[2][2] = cos_theta;
    m[1][2] = sin_theta;
    m[2][1] = -sin_theta;
}

void
phg_mat_rot_y(m, theta)
		Pmatrix3	m;
		Pfloat		theta;
{
    Pfloat	sin_theta, cos_theta;

    sin_theta = (Pfloat) sin(theta);
    cos_theta = (Pfloat) cos(theta);
    phg_mat_identity(m);

    m[0][0] = m[2][2] = cos_theta;
    m[0][2] = -sin_theta;
    m[2][0] = sin_theta;
}

void
phg_mat_rot_z(m, theta)
		Pmatrix3	m;
		Pfloat		theta;
{
    Pfloat	sin_theta, cos_theta;

    sin_theta = (Pfloat) sin(theta);
    cos_theta = (Pfloat) cos(theta);
    phg_mat_identity(m);

    m[1][1] = m[0][0] = cos_theta;
    m[0][1] = sin_theta;
    m[1][0] = -sin_theta;
}



void
phg_mat_transpose(m)
register	Pmatrix3	m;
{   
    register	int		i, j;
    register	Pfloat		t;
    
    for (i = 1; i < 4; i++) {
	for (j = 0; j < i; j++) {
	    t = m[i][j];
	    m[i][j] = m[j][i];
	    m[j][i] = t;
	}
    }
    
}



void
phg_mat_mul_pt(new_pt, opt, mat)
register	Pfloat		*new_pt;/* the transformed point: x, y, z, w */
		Ppoint3		*opt;	/* the old point: x, y, z */
		Pmatrix3	mat;	/* the xform matrix */
{
    register 	int		i,j;
    register	Pfloat		*col_val;
    register	Pfloat		*pt_val;
		Pfloat		sum;
		Pfloat		pt[4];
		
    pt[0] = opt->x;	/* this is a kludge */
    pt[1] = opt->y;
    pt[2] = opt->z;
    pt[3] = 1.0;
    for(i = 0; i < 4 ; i++) {
	pt_val = pt;
	col_val = &(mat[0][i]) ;	/* get the start of the column */
	sum = 0.0;
	for ( j = 0; j < 4 ; j++) {
	    sum += *pt_val++ * *col_val;
	    col_val += 4;
	}
	*new_pt++ = sum;
    }
}

/*
 * Multiply matrices: m = a * b;
 *
 * This simple-minded explicit form is about 40% faster than
 * nested loops and indexes.
 */

void
phg_mat_mul(m , a, b)
 		Pmatrix3	m;
		Pmatrix3	a;
register	Pmatrix3	b;
{
    register Pfloat	*r;
    register int	i;

    
    if ((m != a) && (m != b)) {
	for ( i = 0; i < 4; i++) {
	    r = a[i];
	    m[i][0] = r[0]*b[0][0] + r[1]*b[1][0] + r[2]*b[2][0] +r[3]*b[3][0];
	    m[i][1] = r[0]*b[0][1] + r[1]*b[1][1] + r[2]*b[2][1] +r[3]*b[3][1];
	    m[i][2] = r[0]*b[0][2] + r[1]*b[1][2] + r[2]*b[2][2] +r[3]*b[3][2];
	    m[i][3] = r[0]*b[0][3] + r[1]*b[1][3] + r[2]*b[2][3] +r[3]*b[3][3];
	}
    }
    else {
			Pmatrix3	t;
	register	Pfloat		*tp, *mp;
	
	for ( i = 0; i < 4; i++) {
	    r = a[i];
	    t[i][0] = r[0]*b[0][0] + r[1]*b[1][0] + r[2]*b[2][0] +r[3]*b[3][0];
	    t[i][1] = r[0]*b[0][1] + r[1]*b[1][1] + r[2]*b[2][1] +r[3]*b[3][1];
	    t[i][2] = r[0]*b[0][2] + r[1]*b[1][2] + r[2]*b[2][2] +r[3]*b[3][2];
	    t[i][3] = r[0]*b[0][3] + r[1]*b[1][3] + r[2]*b[2][3] +r[3]*b[3][3];
	}
	tp = &t[0][0];
	mp = &m[0][0];
	for (i = 0; i < 16; i++)
	    *mp++ = *tp++;
    }
}

void
phg_mat_mul_3x3(m, a, b)
    		Pmatrix		m;		/* OUT result matrix	*/
 	 	Pmatrix		a;		/* matrix a	*/
register	Pmatrix		b;		/* matrix b	*/
{
    register Pfloat	*r;
    register int	i;

    if ((m != a) && (m != b)) {
	for ( i = 0; i < 3; i++) {
	    r = a[i];
	    m[i][0] = r[0]*b[0][0] + r[1]*b[1][0] + r[2]*b[2][0];
	    m[i][1] = r[0]*b[0][1] + r[1]*b[1][1] + r[2]*b[2][1];
	    m[i][2] = r[0]*b[0][2] + r[1]*b[1][2] + r[2]*b[2][2];
	}
    }
    else {
			Pmatrix		t;
	register	Pfloat		*tp, *mp;
	
	for ( i = 0; i < 3; i++) {
	    r = a[i];
	    t[i][0] = r[0]*b[0][0] + r[1]*b[1][0] + r[2]*b[2][0];
	    t[i][1] = r[0]*b[0][1] + r[1]*b[1][1] + r[2]*b[2][1];
	    t[i][2] = r[0]*b[0][2] + r[1]*b[1][2] + r[2]*b[2][2];
	}
	tp = &t[0][0];
	mp = &m[0][0];
	for (i = 0; i < 9; i++)
	    *mp++ = *tp++;
    }
}



void
phg_mat_identity (mat)
register	Pmatrix3	mat;
{
    register	int	i,j;

    for( i = 0 ; i < 4; i++) 
	for( j = 0; j < 4; j++)
	   mat[i][j] = ( (i == j) ? 1.0 : 0.0);
}

void
phg_mat_copy (m1, m2)
register	Pmatrix3	m1, m2;
{
    register	int	i,j;

    for( i = 0 ; i < 4; i++) 
	for( j = 0; j < 4; j++)
	   m1[i][j] = m2[i][j];
}


Pfloat
phg_vector_dot_prod(a, b)
register	Pvec3	*a;
register	Pvec3	*b;
{
   Pfloat		sum;
		
    sum = (a->delta_x * b->delta_x) +
	  (a->delta_y * b->delta_y) +
	  (a->delta_z * b->delta_z) ;
    return(sum);
}

void
phg_vector_cross_prod(a, b, c)
register	Pvec3	*a;
register	Pvec3	*b;
register	Pvec3	*c;
{
    a->delta_x = b->delta_y * c->delta_z - b->delta_z * c->delta_y;
    a->delta_y = b->delta_z * c->delta_x - b->delta_x * c->delta_z;
    a->delta_z = b->delta_x * c->delta_y - b->delta_y * c->delta_x;
}


Pfloat
phg_vector_length(a)
register	Pvec3	*a;
{
    Pfloat	sum;

    sum = (a->delta_x * a->delta_x) +
	  (a->delta_y * a->delta_y) +
	  (a->delta_z * a->delta_z) ;
		
    if (sum != 0.0) {
	sum = sqrt(sum);
	return(sum);
    }
    else
    	return( 0.0);

}

void
phg_vector_normalize(v)
register	Pvec3	*v;
{
    register 	Pfloat		len;

    len = phg_vector_length(v);
    
    if (len != 0.0) {
	v->delta_x /= len;
	v->delta_y /= len;
	v->delta_z /= len;
    }
    else {
	v->delta_x = 0.0;
	v->delta_y = 0.0;
	v->delta_z = 0.0;
    }
}



#ifdef	DEBUG
void
phg_mat_print(m)
register	Pmatrix3	m;
{
    

    register	int	i,j;

    for( i = 0 ; i < 4; i++) {
	for( j = 0; j < 4; j++)
	   (void) fprintf(stderr, "\t% 10.5f",m[i][j]);
	(void) fprintf(stderr, "\n");
    }
}
#endif	/* DEBUG */


/*	
 *	phg_mat_inv - a fairly robust matrix inversion routine
 *
 *	inverts a 4x4 matrix.  
 *
 *	TODO: If the matrix is singular, call a more robust routine (SVD)
 *	to find a solution. See Numerical Recipes in C
 * 
 */


void
phg_mat_inv( a )
    Pmatrix3	a;
{
    short index[4][2], ipivot[4];
    float pivot[4];
    short row, colum;
    float max;
    short i, j, k, l;

    /* need this if we have a fallback to SVD inversion 
    float save[4][4];

    PHG_COPY_MAT(save, a);
    */

    for (j = 0; j < 4; j++)
	ipivot[j] = 0;

    for (i = 0; i < 4; i++) {	/* do matrix inversion */
	max = 0.0;
	for (j = 0; j < 4; j++) {	/* search for pivot element */
	    if (ipivot[j] == 1)
		continue;
	    for (k = 0; k < 4; k++) {
		if (ipivot[k] == 1)
		    continue;
		/* what does this mean? is it annother singular case?
		if (ipivot[k] > 1)
		    TODO:
		*/
		if (fabs(max) < fabs(a[j][k])) {
		    row = j;
		    colum = k;
		    max = a[j][k];
		}
	    }
	}
	if (max == 0.0) {
	    /* input matrix is singular, return the an identity matrix */
	    phg_mat_identity(a);
	   /* TODO: restore matix 'a' and call SVD routine */
	}
	ipivot[colum] += 1;
	if (row != colum) {	/* interchange rows to put */
	    for (l = 0; l < 4; l++) {
		max = a[row][l];
		a[row][l] = a[colum][l];
		a[colum][l] = max;
	    }
	}
	index[i][0] = row;
	index[i][1] = colum;
	pivot[i] = a[colum][colum];
#ifdef DEBUG
	if ((pivot[i] < 1.0e-6) && (pivot[i] > -1.0e-6) ) {
	   /* TODO: restore matix 'a' and call SVD routine */
	   fprintf(stderr,"Matrix is singular: pivot=%f\n", pivot[i]);
	}
#endif
	/* the following isn't needed if we have SVD routine */
	if (pivot[i] == 0.0) {
	   pivot[i] = PHG_ZERO_TOLERANCE;
	}
	a[colum][colum] = 1.0;	/* divide pivot row by pivot element */
	for (l = 0; l < 4; l++)
	    a[colum][l] /= pivot[i];
	for (j = 0; j < 4; j++)
	    if (j != colum) {
		max = a[j][colum];
		a[j][colum] = 0.0;
		for (l = 0; l < 4; l++)
		    a[j][l] -= a[colum][l] * max;
	    }
    }

    for (i = 0; i < 4; i++) {	/* interchange columns */
	l = 4 - 1 - i;
	if (index[l][0] != index[l][1]) {
	    row = index[l][0];
	    colum = index[l][1];
	    for (k = 0; k < 4; k++) {
		max = a[k][row];
		a[k][row] = a[k][colum];
		a[k][colum] = max;
	    }
	}
    }
    /* determinant is d * pivot[0] * pivot[1] * pivot[2] * pivot[3] if needed*/
}

int
phg_tranpt3( p, m, r)
    register Ppoint3	*p;		/* point	*/
    register Pmatrix3	m;		/* transformation matrix	*/
    Ppoint3		*r;		/* OUT transformed point	*/
{
    register float	w;	/* homogeneous coordinate */

    if ( PHG_NEAR_ZERO(
	w = m[3][0]*p->x + m[3][1]*p->y + m[3][2]*p->z + m[3][3])) {
	return 0;
     
    } else {
	w = 1.0 / w;
	r->x = w * (m[0][0] * p->x + m[0][1] * p->y + m[0][2] * p->z + m[0][3]);
	r->y = w * (m[1][0] * p->x + m[1][1] * p->y + m[1][2] * p->z + m[1][3]);
	r->z = w * (m[2][0] * p->x + m[2][1] * p->y + m[2][2] * p->z + m[2][3]);
	return 1;
    }
}

int
phg_tranpts3( m, n, p, r)
    register Pmatrix3	m;		/* transformation matrix */
    register int	n;		/* number of points */
    register Ppoint3	*p;		/* list of points to transform */
    register Ppoint3	*r;		/* OUT transformed points */
{
    register float	w;		/* homogeneous coordinate */
    int			status = 1;

    while ( n--) {
	if ( PHG_NEAR_ZERO(
	    w = m[3][0]*p->x + m[3][1]*p->y + m[3][2]*p->z + m[3][3])) {
	    status = 0;
	    break;
     
	} else {
	    w = 1.0 / w;
	    r->x = w *
		(m[0][0] * p->x + m[0][1] * p->y + m[0][2] * p->z + m[0][3]);
	    r->y = w *
		(m[1][0] * p->x + m[1][1] * p->y + m[1][2] * p->z + m[1][3]);
	    r->z = w *
		(m[2][0] * p->x + m[2][1] * p->y + m[2][2] * p->z + m[2][3]);
	}
	p++; r++;
    }
    return status;
}

/* transfrom pt 4: transform point with variable w*/
int
phg_tranpt4( p, m, r)
    register Ppoint4	*p;		/* vector (x,y,z,w)	*/
    register Pmatrix3	m;		/* transformation matrix	*/
    Ppoint4		*r;		/* OUT transformed vector (x,y,z,w) */
{
    r->x = m[0][0] * p->x + m[0][1] * p->y + m[0][2] * p->z + m[0][3] * p->w;
    r->y = m[1][0] * p->x + m[1][1] * p->y + m[1][2] * p->z + m[1][3] * p->w;
    r->z = m[2][0] * p->x + m[2][1] * p->y + m[2][2] * p->z + m[2][3] * p->w;
    r->w = m[3][0] * p->x + m[3][1] * p->y + m[3][2] * p->z + m[3][3] * p->w;
}

void
phg_tranpts4( m, n, p, r )
    register Pmatrix3	m;		/* transformation matrix */
    register int	n;		/* number of points */
    register Ppoint4	*p;		/* list of points to transform */
    register Ppoint4	*r;		/* OUT transformed points */
{
    while ( n--) {
      r->x = m[0][0] * p->x + m[0][1] * p->y + m[0][2] * p->z + m[0][3] * p->w;
      r->y = m[1][0] * p->x + m[1][1] * p->y + m[1][2] * p->z + m[1][3] * p->w;
      r->z = m[2][0] * p->x + m[2][1] * p->y + m[2][2] * p->z + m[2][3] * p->w;
      r->w = m[3][0] * p->x + m[3][1] * p->y + m[3][2] * p->z + m[3][3] * p->w;
      p++; r++;
    }
}

