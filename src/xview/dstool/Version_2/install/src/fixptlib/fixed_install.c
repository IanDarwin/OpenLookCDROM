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
 * fixed_install.c
 *
 * Procedures:
 *   fixed_install()
 *   fixed_reset()
 *
 */
#include <stdio.h>
#include <constants.h>
#include <defaults.h>
#include <pm.h>


static char *FIXED_OBJ_NAME = "Fixed";

static char *FIXED[] = {
  "Fixed.Map_Period", "Fixed.Vf_Period",
  "Fixed.Algorithm", "Fixed.Guess", "Fixed.Mc_Guesses",
  "Fixed.Num_Iters", "Fixed.Found", "Fixed.FD_Step",
  "Fixed.Dups", "Fixed.Var_Conv", "Fixed.Funct_Conv",
  "Fixed.Eigen_Dist", 
  "Fixed.Stab_Points", "Fixed.Stab_Steps",
  "Fixed.Unstab_Points", "Fixed.Unstab_Steps",
  "Fixed.Setting",
  "Fixed.Find_Fixpts", "Fixed.1dman",
  "Fixed.Clear_Points", "Fixed.Clear_Mans",
};

typedef enum {
  MAP_PER = 0, VF_PER, ALGORITHM, GUESS,
  GUESS_NUM, NUM_ITERS, FOUND, FD_STEP,
  DUPS, VAR_CONV, FUNCT_CONV, EIGEN_DISTANCE,
  STABLE_POINTS, STABLE_STEPS, UNSTABLE_POINTS, UNSTABLE_STEPS,
  SETTING, FIND_FIXPTS, ONEDMAN,
  CLEAR_POINTS, CLEAR_MANS
  } FIXED_t;

void
  fixed_install()
{
  void fp_init(), fp_1dman_init(),
    periodic_clear_fixpts_go(), periodic_clear_mans_go();

  pm(CREATE_OBJ, FIXED_OBJ_NAME,
     CREATE_ELEM, FIXED[MAP_PER], INT,
     CREATE_ELEM, FIXED[VF_PER], DBL,
     CREATE_ELEM, FIXED[ALGORITHM], INT,
     CREATE_ELEM, FIXED[GUESS], INT,
     CREATE_ELEM, FIXED[GUESS_NUM], INT,
     CREATE_ELEM, FIXED[NUM_ITERS], INT,
     CREATE_ELEM, FIXED[FOUND], INT,
     CREATE_ELEM, FIXED[FD_STEP], DBL,
     CREATE_ELEM, FIXED[DUPS], DBL,
     CREATE_ELEM, FIXED[VAR_CONV], DBL,
     CREATE_ELEM, FIXED[FUNCT_CONV], DBL,
     CREATE_ELEM, FIXED[EIGEN_DISTANCE], DBL,
     CREATE_ELEM, FIXED[STABLE_POINTS], INT,
     CREATE_ELEM, FIXED[STABLE_STEPS], INT,
     CREATE_ELEM, FIXED[UNSTABLE_POINTS], INT,
     CREATE_ELEM, FIXED[UNSTABLE_STEPS], INT,
     CREATE_ELEM, FIXED[SETTING], INT,
     CREATE_ELEM, FIXED[FIND_FIXPTS], FNCT,
     CREATE_ELEM, FIXED[ONEDMAN], FNCT,
     CREATE_ELEM, FIXED[CLEAR_POINTS], FNCT,
     CREATE_ELEM, FIXED[CLEAR_MANS], FNCT,
     NULL);

  pm(CREATE_ELEM, "Memory.Fixed", MEMRY,
     NULL);

  pm(INIT, FIXED[FIND_FIXPTS],
     INIT, FIXED[ONEDMAN],
     INIT, FIXED[CLEAR_POINTS],
     INIT, FIXED[CLEAR_MANS],
     PUT, FIXED[FIND_FIXPTS], fp_init,
     PUT, FIXED[ONEDMAN], fp_1dman_init,
     PUT, FIXED[CLEAR_POINTS], periodic_clear_fixpts_go,
     PUT, FIXED[CLEAR_MANS], periodic_clear_mans_go,
     PUT, FIXED[SETTING], 0,
     NULL);
}

void
  fixed_reset()
{

  pm(PUT, FIXED[MAP_PER], MAP_PERIOD,
     PUT, FIXED[VF_PER], VF_PERIOD,
     PUT, FIXED[ALGORITHM], FXPT_ALGORITHM,
     PUT, FIXED[GUESS], FXPT_GUESS,
     PUT, FIXED[GUESS_NUM], MC_GUESSES,
     PUT, FIXED[NUM_ITERS], FIXPT_ITERS,
     PUT, FIXED[FOUND], 0,
     PUT, FIXED[FD_STEP], DELTAX,
     PUT, FIXED[DUPS], DUP_DIFF,
     PUT, FIXED[VAR_CONV], VARB_CONV,
     PUT, FIXED[FUNCT_CONV], FUNCTION_CONV,
     PUT, FIXED[EIGEN_DISTANCE], EIGEN_DIST,
     PUT, FIXED[STABLE_POINTS], STAB_POINTS,
     PUT, FIXED[STABLE_STEPS], STAB_STEPS,
     PUT, FIXED[UNSTABLE_POINTS], UNSTAB_POINTS,
     PUT, FIXED[UNSTABLE_STEPS], UNSTAB_STEPS,
     NULL);

  pm(INIT, "Memory.Fixed", FIXPT_MEMORY, NULL);

}
