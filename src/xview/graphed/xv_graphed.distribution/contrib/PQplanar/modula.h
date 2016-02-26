/* (C) Universitaet Passau 1986-1991 */
/*****************************************************************/
/*         FILE : modula.h                                       */
/*        AUTOR : Uwe Schnieders                                 */
/*        UPDATE: 23.07.90                                       */
/*****************************************************************/
/*        Modula aehnliche DEFINES um den C-Source-Code leselicher */
/*        zu gestallten .                                        */
/*                                                               */
/*                                                               */
/*                                                               */
/*****************************************************************/


/*****************************************************************/
/*       DEFINES                                                 */
/*****************************************************************/

#define WHILE while (

#define DO    ) {

#define ENDDO  ;}

#define BEGIN {

#define END   ;}

#define ENDPROC  ;}

#define IF    if (

#define THEN  ) {

#define ELSE  ;} else {

#define ENDIF  ;}

#define FOR   for (

#define ENDFOR  ;}

#define FOR_TO(i,v,b)  FOR i=v;i<=b;i++ DO

#define FOR_DOWNTO(i,v,b)  FOR i=v;i>=b;i-- DO

#define CASE  switch(

#define OF    ) { case

#define OTHERWISE  default

#define BREAK      break; case 

#define ENDCASE    }

#define NEW(X) (char*)(X) = malloc(  sizeof( *(X) )  )

#define NOT !

#define AND &&

#define OR  ||

#define MOD %

#define DIV /

#define LOOP for (;;) {

#define REPEAT do {

#define UNTIL(X)  } while(!(X))

#define EXIT      break

#define RETURN  return

#define NIL NULL

#define BOOLEAN  int

#define INTEGER  int

#define REAL double

#define INC(X,Y)  X=(X)+(Y)

#define DEC(X,Y)  X=(X)-(Y)

#define DISPOSE(X) free((char*)X)

/* Added 15/3/91 by Michael Himsolt to eliminate debugging output */

#define printf
