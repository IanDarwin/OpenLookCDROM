

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/
#define RQ_PATHLEN 1024

struct getquota_args {
	char *gqa_pathp;
	int gqa_uid;
};
typedef struct getquota_args getquota_args;
bool_t xdr_getquota_args();


struct rquota {
	int rq_bsize;
	bool_t rq_active;
	u_int rq_bhardlimit;
	u_int rq_bsoftlimit;
	u_int rq_curblocks;
	u_int rq_fhardlimit;
	u_int rq_fsoftlimit;
	u_int rq_curfiles;
	u_int rq_btimeleft;
	u_int rq_ftimeleft;
};
typedef struct rquota rquota;
bool_t xdr_rquota();


enum gqr_status {
	Q_OK = 1,
	Q_NOQUOTA = 2,
	Q_EPERM = 3,
};
typedef enum gqr_status gqr_status;
bool_t xdr_gqr_status();


struct getquota_rslt {
	gqr_status status;
	union {
		rquota gqr_rquota;
	} getquota_rslt_u;
};
typedef struct getquota_rslt getquota_rslt;
bool_t xdr_getquota_rslt();


#define RQUOTAPROG ((u_long)11)
#define RQUOTAVERS ((u_long)1)
#define RQUOTAPROC_GETQUOTA ((u_long)1)
extern getquota_rslt *rquotaproc_getquota_1();
#define RQUOTAPROC_GETACTIVEQUOTA ((u_long)2)
extern getquota_rslt *rquotaproc_getactivequota_1();

