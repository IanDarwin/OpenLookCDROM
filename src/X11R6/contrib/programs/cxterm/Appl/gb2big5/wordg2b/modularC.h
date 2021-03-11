#ifndef __ModularC_h__
#define __ModularC_h__

#define START_IMPLEMENTATION
#define Start_Implementation
#define IS	==
#define IF	if(
#define THEN	){
#define ELIF	}else if(
#define ELSE	}else{
#define ENDI	}
#define WHILE	while(
#define DO	){
#define ENDW	}
#define REPEAT	do{
#define WHEN	}while
#define FOR	for(
#define ENDF	}
#define LOOP	for(;;){
#define ENDL	}
#define SWITCH	switch(
#define OF	){
#define ENDS	}
#define CASE	case
#define DEFAULT	default
#define BREAK	break
#define RETURN	return
#define CONTINUE continue
#define PRIVATE_VAR	static
#define PRIVATE_FUN	static
#define PRIVATE		static
#define EXPORT_VAR
#define EXPORT_FUN
#define EXPORT
#define IMPORT_VAR	extern
#define IMPORT_FUN	extern
#define IMPORT		extern
#define IN
#define OUT
#define INOUT

typedef int	modc_BOOL ;
typedef int	modc_Bool ;
typedef int	modc_Boolean ;

#ifndef TRUE
#define TRUE	1
#define FALSE	0
#endif /* TRUE */

#ifndef Assert

#ifdef DEBUG
/* This Assert() prints (a little) more info than C library assert()
 */
#define Assert(cond) \
	{ IF !(cond) THEN \
		printf( "Assertion failed: file %s, line %d: cond\n", \
			__FILE__, __LINE__); \
		printf( "press <CR> to continue:" ); \
		scanf( "%*c" ); /* instead of abort(); */ \
	ENDI }
#else /* DEBUG */
#define Assert(cond)
#endif /* DEBUG */


#endif /* Assert */

#endif /* __ModularC_h__ */
