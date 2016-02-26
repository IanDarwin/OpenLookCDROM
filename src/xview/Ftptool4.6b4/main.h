
#pragma ident   "@(#)main.h 1.3     93/05/25"

#define	BINARY  0
#define	ASCII   1
#define	TENEX   2
/*
#define	IMAGE   3
#define	EBCDIC  4
 */

#ifdef USE_PROTOTYPES

int main(int argc, char **argv);
void load_xdefaults(void);
void set_xdefaults(void);
void save_xdefaults(void);

#else

int main();
void load_xdefaults();
void set_xdefaults();
void save_xdefaults();

#endif
