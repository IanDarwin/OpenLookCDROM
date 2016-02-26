#include <stdio.h>
extern int yyparse();

FILE *scm, *c;

int scheme = 1;
main(argc, argv)
int argc;
char **argv;
{
    char file_name[256];
    extern FILE *yyin, *yyout;
    char *getenv();

    if (argc != 2) {
	printf("usage: lcs base_name\n");
	exit(-1);
    }
    {
      char *dialect = getenv("LISP_DIALECT");
      if (dialect == NULL) {
	fprintf(stderr,"No LISP-DIALECT; using 'Scheme'\n");
	scheme = 1;
      } else if (!strcmp(dialect,"scheme")) {
	fprintf(stderr,"Using scheme\n");
	scheme = 1;
      } else if (!strcmp(dialect,"cl")) {
	fprintf(stderr,"Using Common Lisp\n");
	scheme = 0;
      } else {
	fprintf(stderr, "Unknown LISP-DIALECT, using Common Lisp\n");
	scheme = 0;
      }
    }
    strcpy(file_name, argv[1]);
    if (!(yyin = fopen(strcat(file_name, ".cps"), "r"))) {
	fprintf(stderr, "Couldn't open file %s.cps.\n", argv[1]);
	exit(-1);
    }
    strcpy(file_name, argv[1]);
    c = fopen(strcat(file_name, ".c"), "w");
    fprintf(c, "#include \"%s_h.h\"\n", argv[1]);
    strcpy(file_name, argv[1]);
    if (scheme)
      scm = fopen(strcat(file_name, "_h.scm"), "w");
    else
      scm = fopen(strcat(file_name, "_h.l"), "w");
    fprintf(scm, "(load \"%s.o\" :foreign-files \'(\"/usr/NeWS/lib/libcps.a\"))\n", argv[1]);
    strcpy(file_name, argv[1]);
    yyout = fopen(strcat(file_name, "_h.cps"), "w");
    yyparse();
    fclose(yyin);
    fclose(scm);
    fclose(c);
    fclose(yyout);
}
