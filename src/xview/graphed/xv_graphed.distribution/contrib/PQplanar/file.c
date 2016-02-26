/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*         FILE : file.c                                         */
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


BOOLEAN erfolgreich_fopen(file,filename,mode)
        FILE **file;
        char  *filename;
        char  *mode;
  BEGIN
        BOOLEAN result;
        char	*full_filename = "                                                 ";
        char    *home;
 
    home = getenv("HOME");
    IF home != NULL THEN
       sprintf(full_filename,"%s/%s",home,filename);
     ELSE
       printf("ERROR GET ENV HOME :  \n ");     
    ENDIF     
    IF (*file) = fopen(full_filename,mode) THEN 
      result = TRUE;
     ELSE
      result = FALSE;
      printf("ERROR OPEN FILE : %s \n ",full_filename);
    ENDIF;
    RETURN( result );
  ENDPROC


void erfolgreich_fclose(file,filename)
        FILE *file;
        char  *filename;
  BEGIN
    IF fclose(file) THEN 
      printf("ERROR CLOSE FILE : %s \n ",filename);
    ENDIF;
  ENDPROC
