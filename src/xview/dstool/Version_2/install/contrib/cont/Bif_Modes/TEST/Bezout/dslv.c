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
#include <stdio.h>



/* ------------------------------------------------------------------------
     
       this subroutine solves the nvar*nvar linear system
     
                    (  df(x)  )
       (1)          (         ) * y   = y
                    (  e(ipc) )
     
       where df(x) is the jacobian of the given function fx at x, and
       e(ipc) is the ip-th natural basis vector; that is, the last row
       consists of all zeros except for a 1 in column ipc.
     
       the matrix is stored in dense form in the one-dimensional array
       rwork(lrbeg)-rwork(lrlst) where lrlst = lrbeg + nvar*nvar - 1 and
       lrbeg is stored in iwork(15). a data error is caused if lrlst.gt.lrw
     
       the routine stores the pivot-index array in iwork(libeg)-iwork(lilst)
       where lilst = libeg + nvar - 1 and libeg is stored in iwork(13).
       the routine returns with a data error if lilst.gt.liw.
     
       since the matrix of the system (1) is stored in dense form the user
       program df may have the form
     
              subroutine df(nvar,fpar,ipar,x,a,ierr)
              dimension a(nvar,nvar), x(nvar), fpar(1),ipar(1)
              .....
              a(i,j) = df(i)/dx(j) , i=1,...,nvar-1,j=1,...,nvar
              .....
     
       where df(i)/dx(j) denotes the derivative of the i-th component
       of the function fx with respect to the j-th variable.  the user
       need not supply the last row of the matrix of the system (1).
     
       the linpack subroutines dgefa, dgedi, and dgesl are used.
     
       list of variables
     
       dets    the sign of the determinant of the matrix in (1).
       fx      an external routine, the name of the user supplied
               subroutine which computes the nvar-1 dimensional
               function.
     
       df      an external routine, the name of the user supplied
               subroutine which computes the jacobian of the function.
       fpar    a real array for the transmission of parameters to
               dslv and df.
       kerror  error flag
               =  0   normal return
               =  1   data or storage error
               =  2   error in derivative routine df
               =  3   numerically singular matrix detected.
       ipc     the location of the 1 in the last row of the matrix
       ipar    an integer array for transmission of parameters to dslv
               and df.
       iwork   integer work array used to store the pivot array
               (see above)
       liw     dimension of iwork in the calling program
       job     work request switch
               =  0   decompose the matrix, solve the system, and
                      compute the sign of the determinant.
               =  1   solve the system, assuming that the matrix has
                      been previously decomposed.
       nvar    the dimension of the system.
       rwork   real work array used to store the matrix (see above).
       lrw     the dimension of rwork in the calling program.
       x       a real vector of length nvar, the point at which the
               jacobian is to be evaluated.
       y       a real vector of length nvar. on input y contains the
               right hand side of the linear system (1), on return
               with kerror = 0 the solution of the system is given in y.
     
       subprograms called
 
          df, dgedi, dgefa, dgesl
  ----------------------------------------------------------------------   */
double	det[3], wk[5000];

extern struct  Cont_Cntl_Ds            cont_ds;

int
dslv(dets,fx,df,fpar,ipc,ipar,iwork,liw,job,nvar,rwork,lrw,x,y)
int   	(*df)(), (*fx)();
int	ipc,*ipar,*iwork,liw,job,nvar,lrw;
double	*dets,*fpar,*x,*y,*rwork;
{
      double	epsqrt,delm,xsave,delp,**a,**dmatrix(),fabs();
      int	kerror, lpiv, ldf,lilst,lfxp,lfxm,
		jac, lrlst,neqn,ndim,i,ie,j,i1,i2,i3,k;

      int jj,ii;
 
      kerror = 1;
      if((nvar <= 1) || (ipc < 1) || (ipc > nvar)) return(1);		/* check input values of nvar, and ipc.     */
      lpiv = iwork[13];							/* depending on value of job, either set up */
      ldf = iwork[15];							/* augmented jacobian, decompose into l-u   */
      lilst = lpiv + nvar - 1;						/* factors, and get determinant,            */
      lfxp=ldf+nvar*nvar;						/* or use current factored jacobian         */
      lfxm=lfxp+nvar;							/* with new right hand side.		    */
      jac=iwork[9];
      if(jac == 0)
        lrlst=ldf-1+nvar*nvar;
      else
        lrlst=ldf-1+nvar*nvar+2*nvar;

      a = dmatrix(0,nvar,0,nvar);
      clear_mat(a,nvar,nvar);
        
      if((lilst > liw) || (lrlst > lrw))
         {printf("dslv   - need liw= %d have liw= %d \n",lilst,liw);
          printf("dslv   - need lrlst= %d have lrw= %d \n",lrlst,lrw);
	  free_dmatrix(a,0,nvar,0,nvar);
          return(1);
	 }
      if(job != 0) 
         {copy_m_for(a,&rwork[ldf],nvar,nvar);
	  dgesl(a,nvar,nvar,&iwork[lpiv],&y[1],0);
          copy_m_back(a,&rwork[ldf],nvar,nvar);
	  clear_mat(a,nvar,nvar);
          iwork[21] = iwork[21] + 1;
	  free_dmatrix(a,0,nvar,0,nvar);
          return(0);
	 }
 
      neqn=nvar-1;					/* case: job = 0 set up the matrix, decompose it and     */
      epsqrt=rwork[19];					/* get the determinant sign. then use back-substitutions */
      ndim=nvar*nvar;					/* to obtain the solution as in the case job = 1         */
      for(i=1; i<=ndim; i++)
	  rwork[ldf+i-1]=0.0;

      if(jac == 0)
	 {clear_mat(a,nvar,nvar);
	  copy_m_for(a,&rwork[ldf],nvar,nvar);
	  ie = df(nvar,fpar,ipar,x,a);
	  copy_m_back(a,&rwork[ldf],nvar,nvar);
	  clear_mat(a,nvar,nvar);
	  kerror = 2;
	  if(ie != 0)
	     {
	      free_dmatrix(a,0,nvar,0,nvar);
              return(kerror);
             }
          copy_m_for(a,&rwork[ldf],nvar,nvar);
         }
      if(jac == 1 || jac == 2)
          {if(jac == 1)
             {ie = fx(nvar,fpar,ipar,x,&rwork[lfxm-1]);
              delm=0.0;
              iwork[22]=iwork[22]+1;
              if(ie != 0)
		{
	         free_dmatrix(a,0,nvar,0,nvar);
                 return(2); 
                }
             }

           for(j=1; j<=nvar; j++)
              {xsave=x[j];
               delp=epsqrt*(1.0+fabs(xsave));
               x[j]=xsave+delp;
               ie = fx(nvar,fpar,ipar,x,&rwork[lfxp-1]);
               iwork[22]=iwork[22]+1;
               if(ie != 0)
		 {
                  free_dmatrix(a,0,nvar,0,nvar);
                  return(2);
                 }
               if(jac == 2)
                 {delm=-delp;
                  x[j]=xsave+delm;
                  ie = fx(nvar,fpar,ipar,x,&rwork[lfxm-1]);
                  iwork[22]=iwork[22]+1;
                  if(ie != 0)
		    {
                     free_dmatrix(a,0,nvar,0,nvar);
                     return(2);
                    }
                 }
               for(i=1; i<=neqn; i++)
                  {i1=ldf+(j-1)*nvar+i-1;
                   i2=lfxp+i-1;
                   i3=lfxm+i-1;
                   rwork[i1]=(rwork[i2]-rwork[i3])/(delp-delm);}
               x[j]=xsave;
              }
           copy_m_for(a,&rwork[ldf],nvar,nvar);
           a[nvar-1][ipc-1] = 1.0;
          }

      k = ldf+ipc*nvar-1;
      rwork[k] = 1.0;
      a[nvar-1][ipc-1] = 1.0;

/*  
      fprintf(stderr,"\nJacobian in DSLV:\n-----------------\n");		
      for(i=0; i<nvar; i++)
	{
         fprintf(stderr,"\n");
	 for(j=0; j<nvar; j++)
	    fprintf(stderr,"%11.6lf ",a[i][j]);
        }
  
      for(i=0; i<nvar; i++)
	for(j=0; j<nvar; j++) cont_ds.jacobian_wrt_x[i][j] = a[i][j];   
  */
  
      ie = dgefa(a,nvar,nvar,&iwork[lpiv]);			/* decompose matrix    */
      copy_m_back(a,&rwork[ldf],nvar,nvar);
      clear_mat(a,nvar,nvar);
      iwork[20] = iwork[20] + 1;
      if(ie != 0) 
         {printf("dslv   - augmented jacobian is singular \n");
    	  free_dmatrix(a,0,nvar,0,nvar);
          return(3); 
         }
 
      copy_m_for(a,&rwork[ldf],nvar,nvar);
      dgedi(a,nvar,nvar,&iwork[lpiv],&det[1],wk,10); 		/* compute determinant */
      copy_m_back(a,&rwork[ldf],nvar,nvar);
      clear_mat(a,nvar,nvar);
 
      *dets=0.0;						/* record the sign of the determinant */
      if(det[1] > 0.0)
        *dets=1.0;
      else if (det[1] < 0.0)
        *dets=-1.0;
 
      copy_m_for(a,&rwork[ldf],nvar,nvar);
      dgesl(a,nvar,nvar,&iwork[lpiv],&y[1],0);		/* using l-u factored matrix, solve system using */
      copy_m_back(a,&rwork[ldf],nvar,nvar);
      iwork[21] = iwork[21] + 1;			/* forward-backward elimination, and overwrite   */
      free_dmatrix(a,0,nvar,0,nvar);
      return(0);					/* right hand side with solution                 */
}


clear_mat(mat,n,m)
double  **mat;
int     n, m;
{
   int	i, j;
   for(i=0; i<n; i++)
      for(j=0; j<m; j++)
	 mat[i][j] =0.0;
}



copy_m_for(a,v,n,m)
double	**a, *v;
int	n, m;
{
	int	i, j;

	for(j=0; j<m; j++)
	   for(i=0; i<n; i++)
	       a[i][j] = v[j*m+i]; 
}


copy_m_back(a,v,n,m)
double	**a, *v;
int	n, m;
{
	int	i, j;

	for(j=0; j<m; j++)
	   for(i=0; i<n; i++)
	       v[j*m+i] = a[i][j]; 
}

/* ------------------------------------------------------------------------
         dgefa factors a double precision matrix by gaussian elimination.
    
         dgefa is usually called by dgeco, but it can be called
         directly with a saving in time if  rcond  is not needed.
         (time for dgeco) = (1 + 9/n)*(time for dgefa) .
    
         on entry
    
            a       double precision(lda, n)
                    the matrix to be factored.
    
            lda     integer
                    the leading dimension of the array  a .
    
            n       integer
                    the order of the matrix  a .
    
         on return
    
            a       an upper triangular matrix and the multipliers
                    which were used to obtain it.
                    the factorization can be written  a = l*u  where
                    l  is a product of permutation and unit lower
                    triangular matrices and  u  is upper triangular.
    
            ipvt    integer(n)
                    an integer vector of pivot indices.
    
            info    integer
                    = 0  normal value.
                    = k  if  u(k,k) .eq. 0.0 .  this is not an error
                         condition for this subroutine, but it does
                         indicate that dgesl or dgedi will divide by zero
                         if called.  use  rcond  in dgeco for a reliable
                         indication of singularity.
    
         linpack. this version dated 08/14/78 .
         cleve moler, university of new mexico, argonne national lab.
    
         subroutines and functions
    
         blas daxpy,dscal,idamax
    
         internal variables
  ------------------------------------------------------------------------- */
    
int 
dgefa(a,lda,n,ipvt)
int	lda,n,*ipvt;
double	**a;
{
      double	t, *vector1, *vector2;
      int	i,idamax(),j,k,kp1,l,nm1,info;
 
      info = 0;			/* gaussian elimination with partial pivoting */
      nm1 = n - 1;
      if (nm1 < 0)
	 {ipvt[n-1] = n-1;
	  if (a[n-1][n-1] == 0.0) return(n-1); 
	  return(0);}
      vector1 = (double *) calloc(n, sizeof(double));
      vector2 = (double *) calloc(n, sizeof(double));

      for(k=0; k<nm1; k++)
         {kp1 = k + 1;
 	  
	  for(i=0; i<n-k; i++) vector1[i] = a[k+i][k];
          l = idamax(n-k,vector1,1) + k;			/* find l = pivot index */
          ipvt[k] = l;
 
         if (a[l][k] != 0.0)		/* zero pivot implies this column */
            {if (l !=  k) 		/* already triangularized         */
               {t = a[l][k]; 		/* interchange if necessary       */
               a[l][k] = a[k][k];
               a[k][k] = t;}
 
             t = -1.0/a[k][k];		/* compute multipliers */
	     for(i=0; i<n-k-1; i++) vector1[i] = a[k+1+i][k];
             dscal(n-k-1,t,vector1,1);
	     for(i=0; i<n-k-1; i++) a[k+1+i][k]=vector1[i];
 
             for(j=kp1; j<n; j++)	/* row elimination with column indexing */
                {t = a[l][j];
                 if (l != k) 
                    {a[l][j] = a[k][j];
                     a[k][j] = t;}
	         for(i=0; i<n-k-1; i++) vector1[i] = a[k+1+i][k];
	         for(i=0; i<n-k-1; i++) vector2[i] = a[k+1+i][j];
                 daxpy(n-k-1,t,vector1,1,vector2,1); 
	         for(i=0; i<n-k-1; i++) a[k+1+i][k]=vector1[i];
	         for(i=0; i<n-k-1; i++) a[k+1+i][j]=vector2[i];}
            }
         else        
             info = k+1;
         } 
      ipvt[n-1] = n-1;
      if (a[n-1][n-1] == 0.0) return(n-1); 

      free(vector1);
      free(vector2);
      return(info);
}              





/* ---------------------------------------------------------------------
          dgedi computes the determinant and inverse of a matrix
          using the factors computed by dgeco or dgefa.
     
          on entry
     
             a       double precision(lda, n)
                     the output from dgeco or dgefa.
     
             lda     integer
                     the leading dimension of the array  a .
     
             n       integer
                     the order of the matrix  a .
     
             ipvt    integer(n)
                     the pivot vector from dgeco or dgefa.
     
             work    double precision(n)
                     work vector.  contents destroyed.
     
             job     integer
                     = 11   both determinant and inverse.
                     = 01   inverse only.
                     = 10   determinant only.
     
          on return
     
             a       inverse of original matrix if requested.
                     otherwise unchanged.
     
             det     double precision(2)
                     determinant of original matrix if requested.
                     otherwise not referenced.
                     determinant = det(1) * 10.0**det(2)
                     with  1.0 .le. dabs(det(1)) .lt. 10.0
                     or  det(1) .eq. 0.0 .
     
          error condition
     
             a division by zero will occur if the input factor contains
             a zero on the diagonal and the inverse is requested.
             it will not occur if the subroutines are called correctly
             and if dgeco has set rcond .gt. 0.0 or dgefa has set
             info .eq. 0 .
     
          linpack. this version dated 08/14/78 .
          cleve moler, university of new mexico, argonne national lab.
     
          subroutines and functions
     
          blas daxpy,dscal,dswap
          fortran dabs,mod
     
          internal variables
   --------------------------------------------------------------------- */

dgedi(a,lda,n,ipvt,det,work,job)
double	**a, *work,*det;
int	lda,n,*ipvt,job;
{
      double	t,ten=10.0, fabs(), fmod(), *vector1, *vector2;
      int	i,j,k,kb,kp1,l,nm1;

      vector1 = (double *) calloc(n, sizeof(double));
      vector2 = (double *) calloc(n, sizeof(double));
 
      if (job/10  !=  0) 
         {det[0] = 1.0;
          det[1] = 0.0;
          for(i=0; i<n; i++)
             {if (ipvt[i] != i) det[0] = -det[0];
              det[0] = a[i][i]*det[0];
              if (det[0]  ==  0.0) break;     
              while (fabs(det[0]) < 1.0) 
                {det[0] = ten*det[0];
                 det[1] = det[1] - 1.0;}
              while (fabs(det[0]) >= ten) 
                {det[0] = det[0]/ten;
                 det[1] = det[1] + 1.0;}
             }    
         }
 
      if (fabs(fmod(((double) job),10.0)) < 0.1) 		/* compute inverse(u) */
	  {free(vector1);
	   free(vector2);
	   return; }       
      for(k=0; k<n; k++)
         {a[k][k] = 1.0/a[k][k];
          t = -a[k][k];
          dscal(k,t,&a[0][k],1);
          kp1 = k + 1;
          if (n >  kp1) 
             for(j=kp1; j<n; j++)
               {t = a[k][j]; 
                a[k][j] = 0.0; 
		for(i=0; i<k+1; i++) vector1[i] = a[i][k];
		for(i=0; i<k+1; i++) vector2[i] = a[i][j];
                daxpy(k+1,t,vector1,1,vector2,1);
		for(i=0; i<k+1; i++) a[i][k]=vector1[i];
		for(i=0; i<k+1; i++) a[i][j]=vector2[i];}
         }
 
       nm1 = n - 1;						/* form inverse(u)*inverse(l) */
       if (nm1 < 0)
	  {free(vector1);
	   free(vector2);
	   return; }       
       for( kb=0; kb<nm1; kb++)
           {k = n - kb;
            kp1 = k + 1;
            for(i=kp1; i<n; i++)
               {work[i] = a[i][k];
                a[i][k] = 0.0; } 
            for(j=kp1; j<n; j++)
               {t = work[j];
		for(i=0; i<n; i++) vector1[i] = a[i][k];
		for(i=0; i<n; i++) vector2[i] = a[i][j];
                daxpy(n,t,vector2,1,vector1,1);
		for(i=0; i<n; i++) a[i][k]=vector1[i];
		for(i=0; i<n; i++) a[i][j]=vector2[i];}
            l = ipvt[k];
            if (l != k) 
	       {for(i=0; i<n; i++) vector1[i] = a[i][k];
		for(i=0; i<n; i++) vector2[i] = a[i][j];
		dswap(n,vector1,1,vector2,1);
	        for(i=0; i<n; i++) a[i][k]=vector1[i];
		for(i=0; i<n; i++) a[i][j]=vector2[i];}
           } 

       free(vector1);
       free(vector2);
}
    
/* ----------------------------------------------------------------------------
         dgesl solves the double precision system
         a * x = b  or  trans(a) * x = b
         using the factors computed by dgeco or dgefa.
    
         on entry
    
            a       double precision(lda, n)
                    the output from dgeco or dgefa.
    
            lda     integer
                    the leading dimension of the array  a .
    
            n       integer
                    the order of the matrix  a .
    
            ipvt    integer(n)
                    the pivot vector from dgeco or dgefa.
    
            b       double precision(n)
                    the right hand side vector.
    
            job     integer
                    = 0         to solve  a*x = b ,
                    = nonzero   to solve  trans(a)*x = b  where
                                trans(a)  is the transpose.
    
         on return
    
            b       the solution vector  x .
    
         error condition
    
            a division by zero will occur if the input factor contains a
            zero on the diagonal.  technically this indicates singularity
            but it is often caused by improper arguments or improper
            setting of lda .  it will not occur if the subroutines are
            called correctly and if dgeco has set rcond .gt. 0.0
            or dgefa has set info .eq. 0 .
    
         to compute  inverse(a) * c  where  c  is a matrix
         with  p  columns
               call dgeco(a,lda,n,ipvt,rcond,z)
               if (rcond is too small) go to ...
               do 10 j = 1, p
                  call dgesl(a,lda,n,ipvt,c(1,j),0)
            10 continue
    
         linpack. this version dated 08/14/78 .
         cleve moler, university of new mexico, argonne national lab.
    
         subroutines and functions
    
         blas daxpy,ddot
    
         internal variables
  ----------------------------------------------------------------------------- */    

dgesl(a,lda,n,ipvt,b,job)
double	**a, *b;
int	lda,n,*ipvt,job;
{
      double	ddot(),t, *vector;
      int	i,k,kb,l,nm1;

      vector = (double *) calloc(n, sizeof(double));
 
      nm1 = n - 1;
      if (job == 0) 
         {if (nm1 >=  0) 				/* job = 0 , solve  a * x = b  */
          for(k=0; k<nm1; k++) 				/* first solve  l*y = b        */
             {l = ipvt[k];
              t = b[l];
              if (l != k) 
                 {b[l] = b[k];
                  b[k] = t;}
              for(i=0; i<n-k-1; i++) vector[i] = a[k+1+i][k];
              daxpy(n-k-1,t,vector,1,&b[k+1],1);
	      for(i=0; i<n-k-1; i++) a[k+1+i][k]=vector[i];
             }
          for(kb=0; kb<n; kb++)				/* now solve  u*x = y */
            {k = n - kb-1;
             b[k] = b[k]/a[k][k];
             t = -b[k];
	     for(i=0; i<k; i++) vector[i] = a[i][k];
             daxpy(k,t,vector,1,b,1); 
	     for(i=0; i<k; i++) a[i][k]=vector[i];}
          free(vector);
          return;   
         }
      for(k=0; k<n; k++) 			/* job = nonzero, solve  trans(a) * x = b */
         {for(i=0; i<k; i++) vector[i] = a[i][k];
          t = ddot(k,vector,1,b,1);		/* first solve  trans(u)*y = b            */
          b[k] = (b[k] - t)/a[k][k];}

         if (nm1 < 0) return;			/* now solve trans(l)*x = y  */
         for (kb=0; kb<nm1; kb++)
            {k = n - kb-1;
	     for(i=0; i<n-1-k; i++) vector[i] = a[k+1+i][k];
             b[k] = b[k] + ddot(n-k-1,vector,1,&b[k+1],1);
             l = ipvt[k];
             if (l != k) 
               {t = b[l];
                b[l] = b[k];
                b[k] = t;}  
            }         
      free(vector);              
}



/*  ------------------------------------------------------------------------
      copies a vector, x, into another vector, y.
   
    ------------------------------------------------------------------------ */

dcopy(n,dx,incx,dy,incy)
double	*dx, *dy;
int	incx, incy, n;
{
	int	i, ix, iy; 
	if(n < 0) return;
	ix = iy = 0;
 	if(incx < 0) ix = (-n+1)*incx;
	if(incy < 0) iy = (-n+1)*incy;
	for(i=0; i<n; i++) 
	   {dy[iy] = dx[ix];
	    ix += incx;
	    iy += incy; }
	return;
 
}



/*  ------------------------------------------------------------------------
    compute vector dot product

    ------------------------------------------------------------------------ */

double
ddot(n,dx,incx,dy,incy)
double *dx,*dy;
int incx,incy,n;
{
	double	dtemp; 
	int	i,ix,iy;
 
	dtemp = 0.0;				
	if(n < 0) return(0.0);		
	ix = iy = 0;
	if(incx < 0) ix = (-n+1)*incx;
	if(incy < 0) iy = (-n+1)*incy;
	for( i=0; i<n; i++)
	   {dtemp += dx[ix]*dy[iy];
	    ix += incx;
	    iy += incy;}
	return( dtemp );
}           




 
/*  ------------------------------------------------------------------------
      constant times a vector plus a vector.

    ------------------------------------------------------------------------ */
daxpy(n,da,dx,incx,dy,incy)
double	*dx, *dy, da;
int	incx, incy, n;
{
	int	i, ix, iy;

	if(n < 0 || da == 0.0) return;
	
	ix = iy = 0;
	if(incx < 0)ix = (-n+1)*incx;
	if(incy < 0)iy = (-n+1)*incy;
	for(i=0; i<n; i++)
	   {dy[iy] = dy[iy] + da*dx[ix];
	    ix += incx;
	    iy += incy;}
}


/*  ------------------------------------------------------------------------
     interchanges two vectors 
   
    ------------------------------------------------------------------------ */

dswap (n,dx,incx,dy,incy)
double	*dx, *dy;
int	incx, incy, n;
{
	double	dtemp;
	int	i, ix, iy;
 
	if(n < 0) return;

	ix = iy = 0;
	if(incx < 0) ix = (-n+1)*incx;
	if(incy < 0) iy = (-n+1)*incy;
	for( i=0; i<n; i++)
    	   {dtemp = dx[ix];
    	    dx[ix] = dy[iy];
    	    dy[iy] = dtemp;
    	    ix += incx;
    	    iy += incy;}
}



/*  ------------------------------------------------------------------------
    scales a vector by a constant
   
    ------------------------------------------------------------------------ */
dscal(n,da,dx,incx)
double	da, *dx;  
int	incx,n;
{
	int	i, nincx;

	if(n < 0) return;
	nincx = n*incx;
	for(i=0; i<n; i++) 
	   dx[i] *= da;
}


/*
	finds the index of element having max. absolute value.
*/

int
idamax(n,dx,incx)
int	n, incx;
double	*dx;
{
      double	dmax, fabs();
      int	i,ix,result; 
 
      result = -1;
      if( n < 1 ) return(result);
      result = 0;
      if(n == 1) return(result);
 
      ix = 0;
      dmax = fabs(dx[0]);
      ix = ix + incx;
      for(i=1; i<n; i++)
         {if(fabs(dx[ix]) > dmax) 
             {result = i;
              dmax = fabs(dx[ix]);}
          ix = ix + incx;
         }       
      return(result);
}
