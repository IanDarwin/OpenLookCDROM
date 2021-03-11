

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
#define MAXUSERS 100
#define MAXUTLEN 256

struct utmp {
	char *ut_line;
	char *ut_name;
	char *ut_host;
	int ut_time;
};
typedef struct utmp utmp;
bool_t xdr_utmp();


struct utmpidle {
	utmp ui_utmp;
	u_int ui_idle;
};
typedef struct utmpidle utmpidle;
bool_t xdr_utmpidle();


typedef struct {
	u_int utmparr_len;
	utmp *utmparr_val;
} utmparr;
bool_t xdr_utmparr();


typedef struct {
	u_int utmpidlearr_len;
	utmpidle *utmpidlearr_val;
} utmpidlearr;
bool_t xdr_utmpidlearr();


#define RUSERSPROG ((u_long)100002)
#define RUSERSVERS_IDLE ((u_long)1)
#define RUSERSPROC_NUM ((u_long)1)
extern int *rusersproc_num_1();
#define RUSERSPROC_NAMES ((u_long)2)
extern utmpidlearr *rusersproc_names_1();
#define RUSERSPROC_ALLNAMES ((u_long)3)
extern utmpidlearr *rusersproc_allnames_1();
#define RUSERSVERS_ORIG ((u_long)2)
extern int *rusersproc_num_2();
extern utmparr *rusersproc_names_2();
extern utmparr *rusersproc_allnames_2();

