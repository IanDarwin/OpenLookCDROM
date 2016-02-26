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
/* ---------------------------------------------------------------------------
   include this source code in each dynamical system definition file
   to initialize the postmaster

   ------------------------------------------------------------------------ */

  pm(CLEAR, "Model.Varb_Ic",          CLEAR, "Model.Param_Ic",
     CLEAR, "Model.Varb_Dim",         CLEAR, "Model.Param_Dim",
     CLEAR, "Model.Funct_Dim",        CLEAR, "Model.Varb_Names",
     CLEAR, "Model.Param_Names",      CLEAR, "Model.Funct_Names",
     CLEAR, "Model.Param_Min",        CLEAR, "Model.Param_Max",
     CLEAR, "Model.Varb_Min",         CLEAR, "Model.Varb_Max",
     CLEAR, "Model.Funct_Max",        CLEAR, "Model.Funct_Min",
     CLEAR, "Model.Mapping_Flag",     CLEAR, "Model.Inverse_Flag",
     CLEAR, "Model.Jacobian_Flag",    CLEAR, "Manifold.Type",
     CLEAR, "Manifold.Periodic_Varb", CLEAR, "Manifold.Period_Start",
     CLEAR, "Manifold.Period_End", NULL );

  phase_sp_setup( n_varb, manifold_type, 
		 periodic_varb, period_start, period_end );

  varb_setup( n_varb, variable_names, variables, variable_min,
	     variable_max, indep_varb_name, indep_varb_min, indep_varb_max);

  param_func_setup( n_param, n_funct, parameter_names, funct_names, 
		   parameters, parameter_min, parameter_max, 
		   funct_min, funct_max );

  pm(PUT, "Model.Mapping_Flag", mapping_toggle, NULL);             
  pm(PUT, "Model.Inverse_Flag", 
     (mapping_toggle? inverse_toggle : FALSE), NULL); /* NULL for v. fields */
  pm(PUT, "Model.Jacobian_Flag", (jac_name == NULL ? FALSE : TRUE), NULL);

  pm(INIT, "Model.DS_Def", 
     INIT, "Model.DfDx", 
     INIT, "Model.DfDt", 
     INIT, "Model.DfDparam", 
     INIT, "Model.Inverse", 
     INIT, "Model.Aux_Function", NULL );

  pm(PUT, "Model.DS_Def", def_name,
     PUT, "Model.DfDx", jac_name,
     PUT, "Model.DfDt", dfdt_name,
     PUT, "Model.DfDparam", dfdparam_name,
     PUT, "Model.Inverse", 
             (mapping_toggle? inv_name: NULL), /* NULL for vector fields */
     PUT, "Model.Aux_Function", aux_func_name, NULL );

