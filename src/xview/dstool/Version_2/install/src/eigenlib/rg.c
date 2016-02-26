/*  -------------------------------------------------------------------

This program is the property of:

                             Cornell University 
                        Center for Applied Mathematics 
                              Ithaca, NY 14853

and may be used, modified and distributed freely, subject to the 
following restrictions:

       Any product which incorporates source code from the dstool
       program or utilities, in whole or in part, is distributed
       with a copy of that source code, including this notice. You
       must give the recipients all the rights that you have with
       respect to the use of this software. Modifications of the
       software must carry prominent notices stating who changed
       the files and the date of any change.

DsTool is distributed in the hope that it will be useful, but 
WITHOUT ANY WARRANTY; without even the implied warranty of FITNESS 
FOR A PARTICULAR PURPOSE.  The software is provided as is without 
any obligation on the part of Cornell faculty, staff or students to 
assist in its use, correction, modification or enhancement.

  -----------------------------------------------------------------  */
int
rg(nm,n,a,wr,wi,matz,z,iv1,fv1)

int n,nm,matz;
double **a,*wr,*wi,**z,*fv1;
int *iv1;

{
      int is1,is2,ierr;
  
/*    this subroutine calls the recommended sequence of
      subroutines from the eigensystem subroutine package (eispack)
      to find the eigenvalues and eigenvectors (if desired)
      of a real general matrix.
 
      on input
 
         nm  must be set to the row dimension of the two-dimensional
         array parameters as declared in the calling program
         dimension statement.
 
         n  is the order of the matrix  a.
 
         a  contains the real general matrix.
 
         matz  is an integer variable set equal to zero if
         only eigenvalues are desired.  otherwise it is set to
         any non-zero integer for both eigenvalues and eigenvectors.
 
      on output
 
         wr  and  wi  contain the real and imaginary parts,
         respectively, of the eigenvalues.  complex conjugate
         pairs of eigenvalues appear consecutively with the
         eigenvalue having the positive imaginary part first.
 
         z  contains the real and imaginary parts of the eigenvectors
         if matz is not zero.  if the j-th eigenvalue is real, the
         j-th column of  z  contains its eigenvector.  if the j-th
         eigenvalue is complex with positive imaginary part, the
         j-th and (j+1)-th columns of  z  contain the real and
         imaginary parts of its eigenvector.  the conjugate of this
         vector is the eigenvector for the conjugate eigenvalue.
 
         ierr  is an integer output variable set equal to an error
            completion code described in the documentation for hqr
            and hqr2.  the normal completion code is zero.
 
         iv1  and  fv1  are temporary storage arrays.
 
      this routine is a C-translation of the FORTRAN 77 source code
      written by the mathematics and computer science division,
      argonne national laboratory
      last change :   september 1989.

      mark myers
      Center for Applied Mathematics 
      Cornell University    (607) 255-4195
 
      --------------------------------------------------------- */
 
      if (n <= nm)   {
         balanc(nm,n,a,&is1,&is2,fv1);
         elmhes(nm,n,&is1,&is2,a,iv1);
         if (matz == 0) 
            ierr=hqr(nm,n,&is1,&is2,a,wr,wi);      /* find eigenvalues only */
         else {
            eltran(nm,n,&is1,&is2,a,iv1,z);        /* find both eigenvalues */
            ierr=hqr2(nm,n,&is1,&is2,a,wr,wi,z);   /* and eigenvectors*/
            if (ierr == 0) balbak(nm,n,&is1,&is2,fv1,n,z);  } }
      else
         ierr = 10 * n; 
      return(ierr);
}
