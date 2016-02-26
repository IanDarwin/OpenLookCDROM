/* (C) Universitaet Passau 1986-1991 */
/* GraphEd Source, 1986-1991 by Michael Himsolt	*/
/*****************************************************************/
/*         FILE : modula.h                                       */
/*        AUTOR : Uwe Schnieders 				 */
/*        UPDATE: 27.02.89					 */
/*****************************************************************/

#define WHILE while (

#define DO    ) {

#define END   ;}

#define BEGIN {

#define IF    if (

#define THEN  ) {

#define ELSE  ;} else {

#define FOR   for (

#define CASE  switch(

#define OF    ) { case

#define OTHERWISE  default

#define NEW(X) (char*)(X) = mymalloc(  sizeof( *(X) )  )

#define NOT !

#define AND &&

#define OR  ||

#define MOD %

#define DIV /

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

#define INC(X,Y)  X=X+Y

#define DEC(X,Y)  X=X-Y

#define DISPOSE(X) myfree((char*)X)
