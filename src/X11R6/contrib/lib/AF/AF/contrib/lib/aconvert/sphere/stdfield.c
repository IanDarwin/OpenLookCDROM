/********************************************/
/** NIST Speech Header Resources (SPHERE)  **/
/** Release 1.5 (beta)                     **/
/** Stan Janet (stan@jaguar.ncsl.nist.gov) **/
/** October 1990                           **/
/********************************************/

/* LINTLIBRARY */

/* File: stdfield.c */

#include <stdio.h>
#include "header.h"
#include "version.h"

char *std_fields[] = {
	"database_id",
	"database_version",
	"utterance_id",
	"channel_count",
	"sample_count",
	"sample_rate",
	"sample_min",
	"sample_max",
	"sample_n_bytes",
	"sample_byte_format",
	"sample_sig_bits",
	CNULL
};
