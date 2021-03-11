/********************************************/
/** NIST Speech Header Resources (SPHERE)  **/
/** Release 1.5 (beta)                     **/
/** Stan Janet (stan@jaguar.ncsl.nist.gov) **/
/** October 1990                           **/
/********************************************/

/* File: sysparams.h */

#define FUNCTION

#define MIN(x,y)		((x) < (y) ? (x) : (y))
#define MAX(x,y)		((x) > (y) ? (x) : (y))

#define LINESIZE		16384

#define COMMENT_CHAR		';'
#define NISTLABEL		"NIST_1A"
#define ENDSTR			"end_head"
#define HDR_ID_SIZE		8
#define HDR_SIZE_SIZE		8
#define PAD_NEWLINES		24
#define PAD_CHAR		' '
#define PAD_MULT		1024

struct fileheader_fixed {
	char header_id[HDR_ID_SIZE];
	char header_size[HDR_SIZE_SIZE];
};
