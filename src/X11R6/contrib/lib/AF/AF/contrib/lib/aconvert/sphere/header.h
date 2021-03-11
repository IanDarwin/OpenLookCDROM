/********************************************/
/** NIST Speech Header Resources (SPHERE)  **/
/** Release 1.5 (beta)                     **/
/** Stan Janet (stan@jaguar.ncsl.nist.gov) **/
/** October 1990                           **/
/********************************************/

/* File: header.h */

#define TRUE			1
#define FALSE			0

#define MAXFIELDS		8000

#define FPNULL			((FILE *) NULL)
#define CNULL			((char *) NULL)
#define INULL			((int *) NULL)
#define LNULL			((long *) NULL)
#define FNULL			((struct field_t *) NULL)
#define FVNULL			((struct field_t **) NULL)
#define HDRNULL			((struct header_t *) NULL)

#define T_INTEGER		0
#define T_REAL			1
#define T_STRING		2

#define N_STDFIELDS		11

#define ERROR_EXIT_STATUS	1

struct field_t {
	int type;
	char *name;
	char *data;
	int datalen;
	char *comment;
};

struct header_t {
	int fc;
	struct field_t **fv;
};
