/* input.h: Header for multiple ungetc. */

/* This file should be included after the other headers are included,
 * in every .c file EXCEPT input.c. */

/* Undefine everything we're about to redefine. */
#undef getc
#undef getchar
#undef ungetc
#undef fgetc
#undef fgets

#define getc		input_getc
#define getchar		input_getchar
#define ungetc		input_ungetc
#define fgetc		input_fgetc
#define fgets		input_fgets

#ifdef __STDC__
int input_getc(FILE *fp);
int input_getchar(void);
void input_ungetc(int c, FILE *fp);
int input_fgetc(FILE *fp);
char *input_fgets(char *str, int n, FILE *fp);

long input_number(FILE *fp);
long input_skip_whitespace(FILE *fp);
void input_match(char *s, FILE *fp);
int input_read_up_to(char *s, int n, int marker, FILE *fp);
int input_read_up_to_whitespace(char *s, int n, FILE *fp);
int input_read_up_to_one_of(char *s, int n, char *markers, FILE *fp);
#else
int input_getc();
int input_getchar();
void input_ungetc();
int input_fgetc();
char *input_fgets();

long input_number();
long input_skip_whitespace();
void input_match();
int input_read_up_to();
int input_read_up_to_whitespace();
int input_read_up_to_one_of();
#endif
