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
/*
### matrix memory allocation ###
*/

double **dmatrix(nrl,nrh,ncl,nch)
int nrl,nrh,ncl,nch;
{
	int i;
	double **m;
	void free_dmatrix();

	m = (double **) malloc((unsigned) (nrh - nrl + 1) * sizeof(double *));
	if(!m) {
		system_mess_proc(1,"dmatrix: memory allocation failure!");
		return(0);
	}
	else {
		m -= nrl;
	}

	for(i=nrl;i<=nrh;i++){
		m[i] = (double *) malloc((unsigned) (nch - ncl + 1) * sizeof(double));
		if(!m[i]) {
			system_mess_proc(1,"dmatrix: memory allocation failure!");
			free_dmatrix(m,nrl,i-1,ncl,nch);
			return(0);
		}
		else {
			m[i] -= ncl;
		}
	}
	return(m);
}
