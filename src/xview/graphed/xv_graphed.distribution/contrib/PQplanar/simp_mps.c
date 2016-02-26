/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*         FILE : simp_mps.c                                     */
/*        AUTOR : Uwe Schnieders                                 */
/*        UPDATE: 23.07.90                                       */
/*****************************************************************/
/*                                                               */
/*                                                               */
/*                                                               */
/*                                                               */
/*                                                               */
/*                                                               */
/*                                                               */
/*                                                               */
/*                                                               */
/*                                                               */
/*****************************************************************/

/*****************************************************************/
/*       INCLUDE                                                 */
/*****************************************************************/

#include "path.h"

#include STDI
#include SGRAPHI
#include SLISTI
#include GRAPHEDI

#include "modula.h"
#include "simptrei.h"
#include <math.h>
#include <xview/xview.h>

static   FILE    *file;

/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

void fprint_mps_real(file,wert)
        FILE  *file;
        REAL   wert;
  BEGIN
    IF wert > 0.0 THEN
      fprintf(file," ");
     ELSE
      fprintf(file,"-");
    ENDIF
    fprintf(file,"%.7f \n",fabs(wert));
  ENDPROC


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

void fprint_mps_name(file,c,nr)
        FILE *file;
        char *c;
        INTEGER nr;
  BEGIN    
    fprintf(file,"%s%07d",c,nr);
  ENDPROC


void simplex_to_MPS(kleiner_gleich,zeilen,spalten)
        INTEGER kleiner_gleich,zeilen,spalten;
  BEGIN
        INTEGER i,j;
        BOOLEAN non_zero_in_spalte;

    IF erfolgreich_fopen(&file,".graphed.sim.mps","w") THEN
      fprintf(file,"ZIELFUNK -1.0 \n");
      fprintf(file," 1 3 1\n");
      fprintf(file,"NAME           winkeloptimierung \n");
      fprintf(file,"ROWS \n");
      FOR_TO(i,1,get_max_U_element(zeilen))
        IF (kleiner_gleich > 0) AND (get_max_U_element(kleiner_gleich) >= i) THEN 
          fprintf(file,"  E ");
         ELSE
          fprintf(file,"  E ");
        ENDIF;
        fprint_mps_name(file,"Z",get_N_element(i));
        fprintf(file,"\n");
      ENDFOR;
      fprintf(file,"  N ZIELFUNK \n");
      fprintf(file,"COLUMNS \n");
      FOR_TO(j,1,get_max_W_element(spalten))
        non_zero_in_spalte = FALSE;
        FOR_TO(i,1,get_max_U_element(zeilen))
          IF f_not_zero(get_matrix_element(i,j)) THEN
            fprintf(file,"    ");
            fprint_mps_name(file,"S",get_M_element(j));
            fprintf(file,"  ");
            fprint_mps_name(file,"Z",get_N_element(i));
            fprintf(file,"  ");
            fprint_mps_real(file,get_matrix_element(i,j));
            non_zero_in_spalte = TRUE;
          ENDIF;
        ENDFOR;
        IF f_not_zero(get_Cost_element(j))  AND non_zero_in_spalte THEN 
          fprintf(file,"    ");
          fprint_mps_name(file,"S",get_M_element(j));
          fprintf(file,"  ZIELFUNK  ");
          fprint_mps_real(file,get_Cost_element(j));
        ENDIF;      
      ENDFOR;

      fprintf(file,"RHS \n");
      FOR_TO(i,1,get_max_U_element(zeilen))
        fprintf(file,"    RHS       ");
        fprint_mps_name(file,"Z",get_N_element(i));
        fprintf(file,"  ");
        fprint_mps_real(file,get_B_element(i));
      ENDFOR;
    
      fprintf(file,"ENDATA \n");

      erfolgreich_fclose(file,".graphed.sim.mps");

    ENDIF;

  ENDPROC;


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

void MPS_basic_to_simplex(zeilen,spalten)
    INTEGER zeilen,spalten;
  BEGIN
        INTEGER i,j,nr;
        REAL    wert;
        char    *dummy = "                                                                                                                                                                 ";

    FOR_TO(j,1,get_max_W_element(spalten))
      set_X_element(get_M_element(j),0.0);
    ENDFOR;

    IF erfolgreich_fopen(&file,".graphed.sim.basic","r") THEN 
      i = getc(file); 
      WHILE NOT(feof(file)) DO
        dummy = "                                                                                                                                                                 ";
        fgets(dummy,70,file);
        sscanf(dummy+12,"%d",&nr);
        wert = atof(dummy+27);
#if TEST
        printf(" ZEILE :%s:\n",dummy);
        printf(" ZEILE :%s:\n",dummy+12);
        printf(" ZEILE :%s:\n",dummy+27);
        printf(" NR: %d  WERT : %f \n",nr,wert);
#endif
        set_X_element(nr,wert);
      ENDDO;
    ENDIF;
    erfolgreich_fclose(file,".graphed.sim.basic");
  ENDPROC


/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

void MPS_cost0_to_simplex(zeilen,spalten)
    INTEGER zeilen,spalten;
  BEGIN
        INTEGER i,nr;
        char    *dummy = "                                                                                                                                                      ";
    
    FOR_TO(i,1,spalten)
        set_cost0_element(i,FALSE);
    ENDFOR;
    IF erfolgreich_fopen(&file,".graphed.sim.lower","r") THEN 
      i = getc(file); 
      WHILE NOT(feof(file)) DO
        dummy = "                                                                                                                                                                 ";
        sscanf(dummy+12,"%d",&nr);
        fgets(dummy,70,file);
#if TEST
        printf(" ZEILE :%s:\n",dummy);
        printf(" ZEILE :%s:\n",dummy+12);
        printf(" NR: %d \n",nr);
#endif
        set_cost0_element(nr,TRUE);
      ENDDO;
    ENDIF;
    erfolgreich_fclose(file,".graphed.sim.lower");

  ENDPROC



/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

void call_system(s)
    char *s;
  BEGIN
    static  int    my_client_object;
    static  Notify_client me = &my_client_object;

    int pid;

    IF (pid = vfork()) == 0 THEN
      (void)execl("/usr/bin/sh","sh","-c",s, (char*)0);
      _exit(127);
    ENDIF
    notify_set_wait3_func(me,notify_default_wait3,pid);
    notify_start();
  END

/*****************************************************************/
/*                                                               */
/*  FUNCTION :                                                   */
/*                                                               */
/*   INPUT   :                                                   */
/*                                                               */
/*   OUTPUT  :                                                   */
/*                                                               */
/*****************************************************************/

void call_MPS()
  BEGIN
 
    printf(" Begin off MPS-SIMPLEX \n");
   
    call_system("simp_server");

    printf(" End   off MPS-SIMPLEX \n");

  ENDPROC

